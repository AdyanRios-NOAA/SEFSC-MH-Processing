# 2
# Expand sector ALL to recreational and commercial
# Expand species ALL and species aggregate and group fields so each record has a common name and ITIS code associated with it
# Expansion on species only relevant for management types that are NOT FYI

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Expand records with SECTOR = 'ALL" to be commercial and recreational
mh_sect_expanded <- mh_cleaned %>%
  #CREATE SECTOR USE
  mutate(SECTOR_USE = SECTOR) %>%
  # Rename "ALL" records to 'RECREATIONAL,COMMERCIAL' to separate at the comma
  mutate(SECTOR_USE = case_when(SECTOR_USE == 'ALL' ~ 'RECREATIONAL,COMMERCIAL',
                          TRUE ~ SECTOR_USE)) %>%
  separate_rows(SECTOR_USE)

# Bring in management type detailed (Y/N) table from Google sheets to only expand mtypes we care about
# Read in Google sheets 
detailed_xref <- read_sheet("https://docs.google.com/spreadsheets/d/1PViPVtqkY3q1fWUFGZm1UyIIYrFxt-YDitEUGaHBjsg/edit#gid=1115852389") %>%
  select(-MANAGEMENT_CATEGORY)

# Check species table -- 5 have duplicates still because they are species that were a part of an aggregate, removed, then reinstated
sp_info_use2 <- sp_info_use %>%
  group_by(FMP, SPP_TYPE, SPP_NAME, COMMON_NAME_USE) %>%
  summarise(N = n())

# Break up species table into two tables
  # (1) Single record for species in aggregate or group
  # (2) More than one record for species in aggregate or group
sp_info_use_s <- sp_info_use %>%
  left_join(sp_info_use2, by = c("FMP", "SPP_TYPE", "SPP_NAME", "COMMON_NAME_USE")) %>%
  filter(N == 1) %>% 
  select(-N)
sp_info_use_m <- sp_info_use %>%
  left_join(sp_info_use2, by = c("FMP", "SPP_TYPE", "SPP_NAME", "COMMON_NAME_USE")) %>%
  filter(N == 2) %>% 
  select(-N)

# Prep data for species expansion
mh_sect_expanded <- mh_sect_expanded %>%
  # Add field for detailed (Y/N)
  left_join(detailed_xref, by = "MANAGEMENT_TYPE") %>%
  # Create duplicates of species fields to retain original data format
  mutate(O_COMMON_NAME = COMMON_NAME,
         O_SPECIES_AGGREGATE = SPECIES_AGGREGATE,
         O_SPECIES_GROUP = SPECIES_GROUP) %>%
  # Transpose species fields to only have to join in the expansion on a single field
  pivot_longer(cols = c("COMMON_NAME" = O_COMMON_NAME, "SPECIES_AGGREGATE" = O_SPECIES_AGGREGATE, 
                        "SPECIES_GROUP" = O_SPECIES_GROUP), names_to = "SPP_TYPE", values_to = "SPP_NAME") %>%
  # Remove records where name is null 
  filter(!is.na(SPP_NAME))

# OVER EXPAND MH on species for detailed mtypes where there is a single record for that species in species aggrgate/group table
mh_sp_expanded_y1 <- mh_sect_expanded %>%
  # Filter to remove non-detailed records
  filter(DETAILED == 'YES') %>%
  # Join to species list table by species name type, name,  and FMP
  full_join(., sp_info_use_s, by = c("FMP", "SPP_TYPE", "SPP_NAME")) %>%
  # Remove species group/aggregate name that do not appear in the database or FMP that are not in the data (i.e. HMS)
  filter(!is.na(REGULATION_ID)) %>%
  mutate(COMMON_NAME_USE = case_when(is.na(COMMON_NAME_USE) ~ SPP_NAME,
                                     TRUE ~ COMMON_NAME_USE),
         SPECIES_ITIS_USE = case_when(is.na(SPECIES_ITIS_USE) ~ as.character(SPECIES_ITIS),
                                      TRUE ~ SPECIES_ITIS_USE)) %>%
  # Remove species when expanded if effective date > removed date
  # Remove species when expanded if ineffective date < added date
  filter(EFFECTIVE_DATE < REMOVED_SP_DATE | is.na(REMOVED_SP_DATE) | INEFFECTIVE_DATE > ADDED_SP_DATE) %>%
  # Impute ineffective date with the removed date for records where there is a removed date, but no ineffective date
  mutate(IMP_INEFFECTIVE = case_when(is.na(INEFFECTIVE_DATE) & !is.na(REMOVED_SP_DATE) ~ REMOVED_SP_DATE,
                                     TRUE ~ INEFFECTIVE_DATE),
         IMP_EFFECTIVE = NA) %>%
  # Remove species expansion date variables
  select(-c(SPECIES_ITIS, SCIENTIFIC_NAME))

# OVER EXPAND MH on species for detailed mtypes where there are multiple records for that species in species aggrgate/group table
mh_sp_expanded_y2 <- mh_sect_expanded %>%
  # Filter to remove non-detailed records
  filter(DETAILED == 'YES') %>%
  # Join to species list table by species name type, name,  and FMP
  full_join(., sp_info_use_m, by = c("FMP", "SPP_TYPE", "SPP_NAME")) %>%
  # Remove species group/aggregate name that do not appear in the database or FMP that are not in the data (i.e. HMS)
  filter(!is.na(REGULATION_ID), !is.na(ADDED_SP_DATE)) %>%
  mutate(COMMON_NAME_USE = case_when(is.na(COMMON_NAME_USE) ~ SPP_NAME,
                                     TRUE ~ COMMON_NAME_USE),
         SPECIES_ITIS_USE = case_when(is.na(SPECIES_ITIS_USE) ~ as.character(SPECIES_ITIS),
                                      TRUE ~ SPECIES_ITIS_USE)) %>%
  # Remove species when expanded if effective date > removed date
  # Remove species when expanded if ineffective date < added date
  filter(EFFECTIVE_DATE < REMOVED_SP_DATE | is.na(REMOVED_SP_DATE) | INEFFECTIVE_DATE > ADDED_SP_DATE) %>%
  # Rank duplicate records for same species
  group_by(REGULATION_ID) %>%
  mutate(rnk = order(REMOVED_SP_DATE)) %>%
  ungroup() %>%
  # Remove species when expanded if effective date > added date
  mutate(del = case_when(EFFECTIVE_DATE > ADDED_SP_DATE & EFFECTIVE_DATE > REMOVED_SP_DATE ~ 1,
                         rnk == 2 & INEFFECTIVE_DATE < ADDED_SP_DATE ~ 1,
                         TRUE ~ 0)) %>%
  filter(del == 0) %>%
  # Impute effective date for the duplicate record so that EFFECTIVE_DATE = ADD_SP_DATE, adjusting effective date retains both records and indicates during the gap in timeline the species was not a part of that aggregate/group
  mutate(IMP_EFFECTIVE = case_when(rnk == 2 ~ ADDED_SP_DATE,
                                   TRUE ~ EFFECTIVE_DATE)) %>%
  # Impute ineffective date with the removed date for records where there is a removed date, but no ineffective date
  mutate(IMP_INEFFECTIVE = case_when(is.na(INEFFECTIVE_DATE) & !is.na(REMOVED_SP_DATE) ~ REMOVED_SP_DATE,
                                     INEFFECTIVE_DATE > REMOVED_SP_DATE ~ REMOVED_SP_DATE,
                                     TRUE ~ INEFFECTIVE_DATE)) %>%
  # Remove species expansion date variables
  select(-c(SPECIES_ITIS, SCIENTIFIC_NAME, rnk, del))


# Dataframe of non-detailed records in long form to join with expanded dataframe
mh_sp_expanded_n <- mh_sect_expanded %>%
  # Filter to remove non-detailed records
  filter(DETAILED == 'NO') %>%
  # Get fields to match
  mutate(COMMON_NAME_USE = SPP_NAME,
         SPECIES_ITIS_USE = as.character(SPECIES_ITIS),
         IMP_EFFECTIVE = NA,
         IMP_INEFFECTIVE = NA) %>%
  select(-SPECIES_ITIS)
  
mh_sp_expanded <- mh_sp_expanded_y1 %>%
  bind_rows(mh_sp_expanded_y2, mh_sp_expanded_n)

# Check 
chk <- mh_sp_expanded %>%
  filter(COMMON_NAME_USE == 'GROUPER, GAG', MANAGEMENT_TYPE == 'BAG LIMIT') %>%
  select(REGULATION_ID, SECTOR, O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP, MANAGEMENT_TYPE, SPP_TYPE, SPP_NAME, SPECIES_ITIS_USE, COMMON_NAME_USE, 
         START_YEAR, EFFECTIVE_DATE, INEFFECTIVE_DATE, IMP_EFFECTIVE, IMP_INEFFECTIVE, ADDED_SP_DATE, REMOVED_SP_DATE) %>%
  arrange(EFFECTIVE_DATE)


