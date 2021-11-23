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

# Prep data for species expansion
mh_sect_expanded <- mh_sect_expanded %>%
  # Add field for detailed (Y/N)
  left_join(detailed_xref, by = "MANAGEMENT_TYPE") %>%
  # Transpose species fields to only have to join in the expansion on a single field
  pivot_longer(cols = c(COMMON_NAME, SPECIES_AGGREGATE, SPECIES_GROUP), names_to = "SPECIES_NAME_TYPE", values_to = "NAME") %>%
  # Remove records where name is null 
  filter(!is.na(NAME))

# EXPAND MH on species only when detailed mtype
mh_sp_expanded_y <- mh_sect_expanded %>%
  # Filter to remove non-detailed records
  filter(DETAILED == 'YES') %>%
  # Join to species list table by species name type, name,  and FMP
  full_join(., sp_info_use, by = c("SPECIES_NAME_TYPE", "NAME", "FMP")) %>%
  # Remove species group/aggregate name that do not appear in the database or FMP that are not in the data (i.e. HMS)
  filter(!is.na(REGULATION_ID)) %>%
  mutate(COMMON_NAME_USE = case_when(is.na(COMMON_NAME_USE) ~ NAME,
                                     TRUE ~ COMMON_NAME_USE),
         SPECIES_ITIS_USE = case_when(is.na(SPECIES_ITIS_USE) ~ as.character(SPECIES_ITIS),
                                      TRUE ~ SPECIES_ITIS_USE)) %>%
  # Remove species when expanded if effective date > removed date
  # Remove species when expanded if ineffective date < added date
  filter(EFFECTIVE_DATE < REMOVED_SP_DATE | is.na(REMOVED_SP_DATE) | INEFFECTIVE_DATE > ADDED_SP_DATE) %>%
  # Impute ineffective date with the removed date for records where there is a removed date, but no ineffective date
  mutate(IMP_INEFFECTIVE = case_when(is.na(INEFFECTIVE_DATE) & !is.na(REMOVED_SP_DATE) ~ 1,
                                     TRUE ~ 0),
         INEFFECTIVE_DATE = case_when(is.na(INEFFECTIVE_DATE) & !is.na(REMOVED_SP_DATE) ~ REMOVED_SP_DATE,
                                      TRUE ~ INEFFECTIVE_DATE)) %>%
  # Remove species expansion date variables
  select(-c(ADDED_SP_DATE, REMOVED_SP_DATE, SPECIES_ITIS, SCIENTIFIC_NAME))

# Dataframe of non-detailed records in long form to join with expanded dataframe
mh_sp_expanded_n <- mh_sect_expanded %>%
  # Filter to remove non-detailed records
  filter(DETAILED == 'NO') %>%
  # Get fields to match
  mutate(COMMON_NAME_USE = NAME,
         SPECIES_ITIS_USE = as.character(SPECIES_ITIS)) %>%
  select(-SPECIES_ITIS)
  
mh_sp_expanded <- mh_sp_expanded_y %>%
  bind_rows(mh_sp_expanded_n)




