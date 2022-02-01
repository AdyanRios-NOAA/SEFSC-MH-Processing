# Script 2
# Pre-processing clean up
  # Expand sector ALL to Commercial and Recreational
  # Add "detailed" yes/no field
  # Expand species
  # Translate to new zone names (from Google sheets)
  # Rename mtype "adjustment" to be a flag
  # Create various new variables for processing

#### 1 ####
# Expand records with SECTOR = 'ALL" to be commercial and recreational
mh_sect_expanded <- mh_cleaned %>%
  #CREATE SECTOR USE
  mutate(SECTOR_USE = SECTOR) %>%
  # Rename "ALL" records to 'RECREATIONAL,COMMERCIAL' to separate at the comma
  mutate(SECTOR_USE = case_when(SECTOR_USE == 'ALL' ~ 'RECREATIONAL,COMMERCIAL',
                                TRUE ~ SECTOR_USE)) %>%
  separate_rows(SECTOR_USE)

#### 2 ####
# Bring in management type detailed (Y/N) table from Google sheets to only expand mtypes we care about
# Read in Google sheets 
detailed_xref <- read_sheet("https://docs.google.com/spreadsheets/d/1PViPVtqkY3q1fWUFGZm1UyIIYrFxt-YDitEUGaHBjsg/edit#gid=1115852389") %>%
  select(-MANAGEMENT_CATEGORY)

# Reformat data 
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

#### 3 ####
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
  select(-c(N, FMP_GROUP_ID, SUBGRP_NAME))
sp_info_use_m <- sp_info_use %>%
  left_join(sp_info_use2, by = c("FMP", "SPP_TYPE", "SPP_NAME", "COMMON_NAME_USE")) %>%
  filter(N == 2) %>% 
  select(-c(N, FMP_GROUP_ID, SUBGRP_NAME))

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
  # Remove species when the start date is outside the added and removed dates
  # Remove species when the start date is outside the added and removed dates
  #filter(is.na(REMOVED_SP_DATE) | between(START_DATE, ADDED_SP_DATE, REMOVED_SP_DATE) | between(END_DATE, ADDED_SP_DATE, REMOVED_SP_DATE)) %>%
  # Impute ineffective date with the removed date for records where there is a removed date, but no ineffective date
  # mutate(IMP_INEFFECTIVE = case_when(is.na(INEFFECTIVE_DATE) & !is.na(REMOVED_SP_DATE) ~ REMOVED_SP_DATE,
  #                                    TRUE ~ INEFFECTIVE_DATE),
  #        IMP_EFFECTIVE = NA) %>%
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
  #filter(EFFECTIVE_DATE < REMOVED_SP_DATE | is.na(REMOVED_SP_DATE) | INEFFECTIVE_DATE > ADDED_SP_DATE) %>%
  # Rank duplicate records for same species
  # group_by(REGULATION_ID) %>%
  # mutate(rnk = order(REMOVED_SP_DATE)) %>%
  # ungroup() %>%
  # Remove species when expanded if effective date > added date
  # mutate(del = case_when(EFFECTIVE_DATE > ADDED_SP_DATE & EFFECTIVE_DATE > REMOVED_SP_DATE ~ 1,
  #                        rnk == 2 & INEFFECTIVE_DATE < ADDED_SP_DATE ~ 1,
  #                        TRUE ~ 0)) %>%
  # filter(del == 0) %>%
  # Impute effective date for the duplicate record so that EFFECTIVE_DATE = ADD_SP_DATE, adjusting effective date retains both records and indicates during the gap in timeline the species was not a part of that aggregate/group
  # mutate(IMP_EFFECTIVE = case_when(rnk == 2 ~ ADDED_SP_DATE,
  #                                  TRUE ~ EFFECTIVE_DATE)) %>%
  # # Impute ineffective date with the removed date for records where there is a removed date, but no ineffective date
  # mutate(IMP_INEFFECTIVE = case_when(is.na(INEFFECTIVE_DATE) & !is.na(REMOVED_SP_DATE) ~ REMOVED_SP_DATE,
  #                                    INEFFECTIVE_DATE > REMOVED_SP_DATE ~ REMOVED_SP_DATE,
#                                    TRUE ~ INEFFECTIVE_DATE)) %>%
# Remove species expansion date variables
select(-c(SPECIES_ITIS, SCIENTIFIC_NAME#, rnk, del
))

# Dataframe of non-detailed records in long form to join with expanded dataframe
mh_sp_expanded_n <- mh_sect_expanded %>%
  # Filter to remove non-detailed records
  filter(DETAILED == 'NO') %>%
  # Get fields to match
  mutate(COMMON_NAME_USE = SPP_NAME,
         SPECIES_ITIS_USE = as.character(SPECIES_ITIS)) %>%
  select(-SPECIES_ITIS)

mh_sp_expanded <- mh_sp_expanded_y1 %>%
  bind_rows(mh_sp_expanded_y2, mh_sp_expanded_n)

#### 4 ####
# Areas clean up
# Read in Google sheets for new zone name for some Gulf Reef Fish zone names
area_xref <- read_sheet("https://docs.google.com/spreadsheets/d/1gVFz6UUiN5LU3Fr9NuFSumKGeHvRubgZviJt7R73Gvg/edit#gid=0") %>%
  mutate_all(.funs = toupper) %>%
  # Zone names provided by species and sector which are removed to avoid duplicate names
  select(OLD_ZONE_NAME, NEW_ZONE_NAME) %>%
  unique()

# New zone name variable called ZONE_USE as the standard zone name
# Starting from sector expansion and not species because the species list have duplicates and need to be cleaned up first
mh_setup <- mh_sp_expanded %>%
  left_join(area_xref, by = c("ZONE" = "OLD_ZONE_NAME")) %>%
  rename(ZONE_USE = "NEW_ZONE_NAME") %>%
  # Since the cross reference table is only for Gulf Reef Fish, replace all NAs with the original zone name
  mutate(ZONE_USE = case_when(is.na(ZONE_USE) ~ ZONE,
                              TRUE ~ ZONE_USE)) %>%
  # Remove commas
  mutate(ZONE_USE = gsub(",", "", ZONE_USE))

### 5 ###
# Create new variables
mh_preprocess <- mh_setup %>%
  # Pull out the volume and page number as separate fields from the FR CITATION (currently warning appears because page is NA for "81 FR 33150 B", but once fixed as a bug the warning should go away)
  mutate(vol = as.numeric(sub(" FR.*", "", FR_CITATION)),
         page = as.numeric(sub(".*FR ", "", FR_CITATION)),
         # Add flag when mtype contains adjustment and remove "adjustment" from the mtype name
         ADJUSTMENT = case_when(str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ 1,
                                TRUE ~ 0),
         MANAGEMENT_TYPE_USE = case_when(str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ str_replace(MANAGEMENT_TYPE, " ADJUSTMENT", ""),
                                         TRUE ~ MANAGEMENT_TYPE),
         # Rename reopenings to closures and add open or closed to the value field
         MANAGEMENT_TYPE_USE = case_when(MANAGEMENT_TYPE == "REOPENING" ~ "CLOSURE",
                                         TRUE ~ MANAGEMENT_TYPE),
         VALUE= case_when(MANAGEMENT_TYPE == "CLOSURE" ~ "CLOSE",
                          MANAGEMENT_TYPE == "REOPENING" ~ "OPEN",
                          TRUE ~ VALUE),
         # Replace NA management status as ONCE because dealing with NAs gets tricky sometimes
         MANAGEMENT_STATUS_USE = case_when(is.na(MANAGEMENT_STATUS) ~ 'ONCE',
                                           TRUE ~ MANAGEMENT_STATUS),
         # Create status type as simple, recurring, and complex
         STATUS_TYPE = case_when(MANAGEMENT_STATUS_USE == "ONCE" ~ "SIMPLE",
                                 MANAGEMENT_STATUS_USE %in% c("SEASONAL", "WEEKLY RECURRING", "MONTHLY RECURRING", "DAILY") ~ "RECURRING",
                                 TRUE ~ "COMPLEX"),
         # FLAG REMOVAL AND GENERAL
         REG_REMOVED = case_when(EFFECTIVE_DATE == INEFFECTIVE_DATE ~ 1, TRUE ~ 0),
         GENERAL = case_when(STATUS_TYPE == "SIMPLE" & is.na(VALUE) ~ 1, TRUE ~ 0),
         COMPLEX = case_when(STATUS_TYPE == "COMPLEX" ~ 1, TRUE ~ 0),
         # Create recurring start and end dates (this is to avoid the dual purpose of these fields)
         START_DAY_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_DAY),
         START_MONTH_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_MONTH),
         START_TIME_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_TIME),
         START_DAY_OF_WEEK_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ as.character(START_DAY_OF_WEEK)),
         END_DAY_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_DAY),
         END_MONTH_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_MONTH),
         END_TIME_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_TIME),
         END_DAY_OF_WEEK_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_DAY_OF_WEEK),
         # Start date from start day, month, year fields
         START_DATE = case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                  !is.na(START_DAY) &
                                  !is.na(START_MONTH) &
                                  !is.na(START_YEAR) ~ as.Date(paste(START_MONTH, START_DAY, START_YEAR, sep = "/"), "%m/%d/%Y"),
                                TRUE ~ EFFECTIVE_DATE))

#### 6 ####
# FIND SECTOR FORKS

#SAFE FOR PIVOT TABLE TESTS IN EXCEL
write.csv(mh_preprocess, here('data/preprocessed', paste0("MHpreprocess_", format(Sys.Date(), "%d%b%Y"), ".csv")), row.names = FALSE)

#CREATE A WAY TO TRACK WHICH SECTORS HAVE MULTIPLE SUBSECTORS
sector.match <- c("MANAGEMENT_TYPE_USE", "MANAGEMENT_STATUS_USE",
                  "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
                  "SECTOR_USE",
                  "REGION", "ZONE_USE",
                  "SPP_NAME")

# ASSIGN UNIQUE SECTOR_ID
sector_precluster <- mh_preprocess %>%
  select(one_of(sector.match)) %>%
  distinct() %>%
  mutate(SECTOR_ID = as.numeric(row.names(.))) %>%
  right_join(., mh_preprocess)

# FILTER TO SECTORS WITH MORE THAN 1 SUBSECTOR (ONLY FOR IMPORTANT REGULATION TYPES)
multi_subsector <- sector_precluster %>%
  filter(GENERAL == 0) %>%
  select(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR) %>%
  distinct() %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID) %>%
  mutate(subsector_count = length(SUBSECTOR),
         #FLAG IF ALL IS USED
        subsector_all_used = sum(SUBSECTOR == "ALL")) %>%
  filter(subsector_count > 1) %>%
  arrange(SECTOR_ID, SUBSECTOR) %>%
  data.frame()


multi_subsector_key <- sector_precluster %>%
  select(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR, EFFECTIVE_DATE) %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR) %>%
  summarize(start_use = min(EFFECTIVE_DATE)) %>%
  right_join(., multi_subsector) %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID) %>%
  mutate(date_count = length(unique(start_use))) %>%
  arrange(SECTOR_ID) %>%
  filter(!(subsector_all_used == 0 & date_count == 1)) %>%
  group_by(SECTOR_ID) %>%
  mutate(SUBSECTOR_KEY = paste(unique(SUBSECTOR), sep = ",", collapse=', ')) %>%
  data.frame()

expand_sector_keys = multi_subsector_key %>%
  select(SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY) %>%
  group_by(SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY) %>%
  mutate(SUBSECTOR_N = length(SECTOR_USE)) %>%
  distinct() %>%
  arrange(SUBSECTOR_N) %>%
  data.frame()

expand_sector_keys_recGOMRF <- multi_subsector_key %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO") %>%
  select(SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY, subsector_all_used) %>%
  filter(SECTOR_USE == "RECREATIONAL") %>%
  distinct() %>%
  mutate(column_name = "SECTOR_USE",
         expand_from = case_when(subsector_all_used == 1 ~ "ALL"),
         expand_temp = str_remove(SUBSECTOR_KEY, paste0(expand_from, ", ")),
         expand_to = case_when(str_count(expand_temp, ',') >= 1 ~ expand_temp,
                               TRUE ~ "MANUAL CHECK"))
  
expand_sector_keys_recGOMRF_use <- expand_sector_keys_recGOMRF %>%
  mutate(expand_to = case_when(SECTOR_ID == 513 ~ "FOR-HIRE, PRIVATE",
                               SECTOR_ID == 584 ~ "FOR-HIRE, PRIVATE",
                               TRUE ~ expand_to))

# EXPANSION FUNCTION ####
# ADAPTED TO ALLOW CONDITIONS AND EXPANSION TO BE REGION/FMP/SECTOR/SPECIES SPECIFIC (ETC)
expand_mh <- function(datin, i) {
  data_expand <- datin %>%
    filter(if (!is.na(expansions$SECTOR_ID[i])) SECTOR_ID == expansions$SECTOR_ID[i] else TRUE) %>%
    filter(if (!is.na(expansions$MANAGEMENT_TYPE_USE[i])) MANAGEMENT_TYPE_USE == expansions$MANAGEMENT_TYPE_USE[i] else TRUE) %>%
    filter(if (!is.na(expansions$FMP[i])) FMP == expansions$FMP[i] else TRUE) %>%
    filter(if (!is.na(expansions$REGION[i])) REGION == expansions$REGION[i] else TRUE) %>%
    filter(if (!is.na(expansions$COMMON_NAME_USE[i])) COMMON_NAME_USE == expansions$SPP_NAME[i] else TRUE) %>%
    #filter(if (!is.na(expansions$COMMON_NAME_USE[i])) COMMON_NAME_USE == expansions$COMMON_NAME_USE[i] else TRUE) %>%
    mutate(!!expansions$column_name[i] := case_when(get(expansions$column_name[i]) == expansions$expand_from[i] ~ expansions$expand_to[i],
                                                    get(expansions$column_name[i]) != expansions$expand_from[i] ~ get(expansions$column_name[i]))) %>%
    separate_rows(!!expansions$column_name[i], sep = ", ")

  return(data_expand %>%
           bind_rows(., filter(datin, !REGULATION_ID %in% data_expand$REGULATION_ID)) %>%
           data.frame())
}

# READ IN AND RUN EXPANSIONS ####
expansions_from_csv <- read.csv(here('data/interim', "./MHpreprocess_expansions.csv"), stringsAsFactors = FALSE,
                       fileEncoding = "latin1")
expansions <- bind_rows(expansions_from_csv, expand_sector_keys_recGOMRF_use)
mh_ready <- Reduce(expand_mh, 1:length(expansions$column_name), init = sector_precluster, accumulate = FALSE)
