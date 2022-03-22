# Script 2
# Pre-processing clean up
  # Expand sector ALL to Commercial and Recreational
  # Add "detailed" yes/no field (from google sheets)
  # Translate to new zone names (from Google sheets)
  # Rename mtype "adjustment" to be a flag
  # Create various new variables for processing

#### 1 ####
# Expand records with SECTOR = 'ALL" to be commercial and recreational
# CREATE mh_sect_expanded ####
mh_sect_expanded <- mh_cleaned %>%
  # CREATE SECTOR USE #### 
  # Rename "ALL" records to 'RECREATIONAL,COMMERCIAL'
  mutate(SECTOR_USE = case_when(SECTOR == 'ALL' ~ 'RECREATIONAL,COMMERCIAL',
                                TRUE ~ SECTOR)) %>%
  # EXPAND SECTOR_USE AT THE COMMAS
  separate_rows(SECTOR_USE)

#### 2 ####
# Bring in management type detailed (Y/N) table from Google sheets to only expand mtypes we care about
# Read in Google sheets 
detailed_xref <- read_sheet("https://docs.google.com/spreadsheets/d/1PViPVtqkY3q1fWUFGZm1UyIIYrFxt-YDitEUGaHBjsg/edit#gid=1115852389") %>%
  select(-MANAGEMENT_CATEGORY)

# RUN CROSSCHECKS
# ARE ALL MH MANAGEMENT TYPES IN GOOGLE SHEET (BLANK IF YES)
unique(mh_sect_expanded$MANAGEMENT_TYPE)[unique(mh_sect_expanded$MANAGEMENT_TYPE) %in% detailed_xref$MANAGEMENT_TYPE == FALSE]
# ARE ALL GOOGLE SHEET MANAGEMENT TYPES IN MH (BLANK IF YES)
detailed_xref$MANAGEMENT_TYPE[detailed_xref$MANAGEMENT_TYPE %in% mh_sect_expanded$MANAGEMENT_TYPE == FALSE]

# EDIT mh_sect_expanded ####
# CREATE O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP
# CREATE SPP_TYPE, SP_NAME (consolidates variables above) ####
# Reformat data - get species common name, aggregate name, and group name into single field (never null)
mh_sect_expanded2 <- mh_sect_expanded %>%
  # Add field for detailed (YES/No)
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

# CHECK THAT NUMBER OF REG IDS AND RECORDS REMAINS THE SAME
n_distinct(mh_sect_expanded$REGULATION_ID) == n_distinct(mh_sect_expanded2$REGULATION_ID)
nrow(mh_sect_expanded) == nrow(mh_sect_expanded2)


#### 3 ####
# Areas clean up
# Read in Google sheets for new zone name for some Gulf Reef Fish zone names
area_xref <- read_sheet("https://docs.google.com/spreadsheets/d/1gVFz6UUiN5LU3Fr9NuFSumKGeHvRubgZviJt7R73Gvg/edit#gid=0") %>%
  mutate_all(.funs = toupper) %>%
  # Zone names provided by species and sector which are removed to avoid duplicate names
  select(OLD_ZONE_NAME, NEW_ZONE_NAME) %>%
  unique()

# RUN CROSSCHECKS 
# NOTE: ZONE CLEAN UP ONLY FOR GULF REEF SO FAR
# ARE ALL MH MANAGEMENT TYPES IN GOOGLE SHEET (BLANK IF YES)
unique(mh_sect_expanded2$ZONE)[unique(mh_sect_expanded2$ZONE) %in% area_xref$OLD_ZONE_NAME == FALSE]
# ARE ALL GOOGLE SHEET MANAGEMENT TYPES IN MH (BLANK IF YES)
area_xref$OLD_ZONE_NAME[area_xref$OLD_ZONE_NAME %in% mh_sect_expanded2$ZONE == FALSE]

# CREATE mh_setup ####
# CREATE ZONE USE ####
# New zone name variable called ZONE_USE as the standard zone name
# Starting from sector expansion and not species because the species list have duplicates and need to be cleaned up first
mh_setup <- mh_sect_expanded2 %>%
  left_join(area_xref, by = c("ZONE" = "OLD_ZONE_NAME")) %>%
  rename(ZONE_USE = "NEW_ZONE_NAME") %>%
  # Since the cross reference table is only for Gulf Reef Fish, replace all NAs with the original zone name
  mutate(ZONE_USE = case_when(is.na(ZONE_USE) ~ ZONE,
                              TRUE ~ ZONE_USE)) %>%
  # Remove commas (TRY NOT TO USE COMMAS IN ODM)
  mutate(ZONE_USE = gsub(",", "", ZONE_USE))

### 4 ####
# CREATE mh_preprocess ####
# CREATE vol, page, MANAGEMENT_TYPE_USE, ADJUSTMENT, MANAGEMENT_STATUS_USE , REG_REMOVED####
# CREATE STATUS TYPE, GENERAL, COMPLEX? ####
# Create new variables
mh_preprocess <- mh_setup %>%
  # Pull out the volume and page number as separate fields from the FR CITATION (currently warning appears because page is NA for "81 FR 33150 B", but once fixed as a bug the warning should go away)
  mutate(# VOLUME AND PAGE ARE ESSENTIAL FOR SORTING 
         vol = as.numeric(sub(" FR.*", "", FR_CITATION)),
         page = as.numeric(sub(".*FR ", "", FR_CITATION)),
         # Add flag when mtype contains adjustment and remove "adjustment" from the mtype name
         # Adjustments are never redundant
         ADJUSTMENT = case_when(str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ 1,
                                TRUE ~ 0),
         MANAGEMENT_TYPE_USE = case_when(str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ str_replace(MANAGEMENT_TYPE, " ADJUSTMENT", ""),
                                         TRUE ~ MANAGEMENT_TYPE),
         # Rename reopenings to closures and add open or closed to the value field
         # DIFFERENT FROM INPUT BECAUSE INPUT STAYS TRUE TO FR LANGUAGE 
         MANAGEMENT_TYPE_USE = case_when(MANAGEMENT_TYPE == "REOPENING" ~ "CLOSURE",
                                         TRUE ~ MANAGEMENT_TYPE_USE),
         VALUE= case_when(MANAGEMENT_TYPE == "CLOSURE" ~ "CLOSE",
                          MANAGEMENT_TYPE == "REOPENING" ~ "OPEN",
                          TRUE ~ VALUE),
         # Replace NA management status as ONCE because dealing with NAs gets tricky sometimes
         # BOTH NAs and ONCE ARE MEANT TO BE PROCESSED THE SAME WAY
         MANAGEMENT_STATUS_USE = case_when(is.na(MANAGEMENT_STATUS) ~ 'ONCE',
                                           TRUE ~ MANAGEMENT_STATUS),
         # Create status type as simple, recurring, and complex
         # STATUS TYPE ?
         STATUS_TYPE = case_when(MANAGEMENT_STATUS_USE == "ONCE" ~ "SIMPLE",
                                 MANAGEMENT_STATUS_USE %in% c("SEASONAL", "WEEKLY RECURRING", "MONTHLY RECURRING", "DAILY") ~ "RECURRING",
                                 TRUE ~ "COMPLEX"),
         # FLAG TURN OFF OF REGULATIONS
         REG_REMOVED = case_when(EFFECTIVE_DATE == INEFFECTIVE_DATE ~ 1, TRUE ~ 0),
         # FLAG GENERAL AND COMPLEX?
         GENERAL = case_when(STATUS_TYPE == "SIMPLE" & is.na(VALUE) ~ 1, TRUE ~ 0),
         COMPLEX = case_when(STATUS_TYPE == "COMPLEX" ~ 1, TRUE ~ 0),
         # Create recurring start and end dates (this is to avoid the dual purpose of these fields)
         # REVISIT WHEN WORKING WITH RECURING REGULATIONS
         START_DAY_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_DAY),
         START_MONTH_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_MONTH),
         START_TIME_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_TIME),
         START_DAY_OF_WEEK_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ as.character(START_DAY_OF_WEEK)),
         END_DAY_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_DAY),
         END_MONTH_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_MONTH),
         END_TIME_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_TIME),
         END_DAY_OF_WEEK_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_DAY_OF_WEEK),
         # Start date from start day, month, year fields
         # ONLY WHEN A START DATE IS PROVIDED AND DIFFERENT FROM EFFECTIVE DATE
         START_DATE = case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                  !is.na(START_DAY) &
                                  !is.na(START_MONTH) &
                                  !is.na(START_YEAR) ~ as.Date(paste(START_MONTH, START_DAY, START_YEAR, sep = "/"), "%m/%d/%Y"),
                                TRUE ~ EFFECTIVE_DATE))

#### 5 ####
# FIND SECTOR FORKS

#SAFE FOR PIVOT TABLE TESTS IN EXCEL
write.csv(mh_preprocess, here('data/preprocessed', paste0("MHpreprocess_", format(Sys.Date(), "%d%b%Y"), ".csv")), row.names = FALSE)

#CREATE A WAY TO TRACK WHICH SECTORS HAVE MULTIPLE SUBSECTORS
sector.match <- c("MANAGEMENT_TYPE_USE", "MANAGEMENT_STATUS_USE",
                  "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
                  "SECTOR_USE",
                  "REGION", "ZONE_USE",
                  "SPP_NAME")


# BUILD LIST OF EXISTING sector_precluster FILES
sector_id_files <- dir(here('data/interim/sector_clusters'), full.names = TRUE)

# REAN IN AND COMBINE SECTOR ID FILES LISTED IN sector_id_files
existing_sector_clusters <- sector_id_files %>%
  map(read_csv) %>% 
  reduce(rbind)

# GET STARTING NUMBER OF EXISTING CLUSTERS FOR REFERENCE
clusters_max = max(existing_sector_clusters$SECTOR_ID)

# ASSIGN NUMBERS TO NEW CLUSTERS
new_sector_clusters <- mh_preprocess %>%
  select(one_of(sector.match)) %>%
  distinct() %>%
  anti_join(existing_sector_clusters) %>%
  mutate(SECTOR_ID = (1:n() + clusters_max)[seq_len(nrow(.))])
  
# EXPORT NEW CLUSTERS
if(length(new_sector_clusters$SECTOR_ID) > 0) {
  write_csv(new_sector_clusters, 
            here('data/interim/sector_clusters', paste0("mh_sector_clusters_", format(Sys.Date(), "%d%b%Y"),".csv")))
}

# MERGE OLD AND NEW CLUSTERS
unique_sector_clusters <- rbind(existing_sector_clusters, new_sector_clusters)

# ASSIGN UNIQUE SECTOR_ID
# CREATE SECTOR_ID ####
mh_sector_id <- mh_preprocess %>%
  left_join(unique_sector_clusters, 
            by = c("JURISDICTION", "REGION", "JURISDICTIONAL_WATERS", "FMP", 
                   "SECTOR_USE", "SPP_NAME", "ZONE_USE", "MANAGEMENT_TYPE_USE", 
                   "MANAGEMENT_STATUS_USE"))

# FILTER TO SECTORS WITH MORE THAN 1 SUBSECTOR (ONLY FOR IMPORTANT REGULATION TYPES)
multi_subsector <- mh_sector_id %>%
  #filter(GENERAL == 0) %>%
  filter(DETAILED == "YES") %>%
  select(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR) %>%
  distinct() %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID) %>%
  mutate(subsector_count = length(SUBSECTOR),
         #FLAG IF ALL IS USED
        subsector_all_used = sum(SUBSECTOR == "ALL")) %>%
  filter(subsector_count > 1) %>%
  arrange(SECTOR_ID, SUBSECTOR) %>%
  data.frame()


multi_subsector_key <- mh_sector_id %>%
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
  select(FMP, SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY) %>%
  group_by(FMP, SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY) %>%
  mutate(SUBSECTOR_N = length(SECTOR_USE)) %>%
  distinct() %>%
  arrange(SUBSECTOR_N) %>%
  data.frame()

expand_sector_keys_recGOMRF <- multi_subsector_key %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO") %>%
  select(SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY, subsector_all_used) %>%
  filter(SECTOR_USE == "RECREATIONAL") %>%
  distinct() %>%
  mutate(column_name = "SUBSECTOR",
         expand_from = case_when(subsector_all_used == 1 ~ "ALL"),
         expand_temp = str_remove(SUBSECTOR_KEY, paste0(expand_from, ", ")),
         expand_to = case_when(str_count(expand_temp, ',') >= 1 ~ expand_temp,
                               TRUE ~ "MANUAL CHECK"))
  
expand_sector_keys_recGOMRF_use <- expand_sector_keys_recGOMRF %>%
  mutate(expand_to = case_when(SECTOR_ID == 728 ~ "FOR-HIRE, PRIVATE",
                               SECTOR_ID == 1417 ~ "FOR-HIRE, PRIVATE",
                               TRUE ~ expand_to))

# EXPANSION FUNCTION ####
# ADAPTED TO ALLOW CONDITIONS AND EXPANSION TO BE REGION/FMP/SECTOR/SPECIES SPECIFIC (ETC)
expand_mh <- function(datin, i) {
  data_expand <- datin %>%
    filter(if (!is.na(expansions$SECTOR_ID[i])) SECTOR_ID == expansions$SECTOR_ID[i] else TRUE) %>%
    filter(if (!is.na(expansions$MANAGEMENT_TYPE_USE[i])) MANAGEMENT_TYPE_USE == expansions$MANAGEMENT_TYPE_USE[i] else TRUE) %>%
    filter(if (!is.na(expansions$FMP[i])) FMP == expansions$FMP[i] else TRUE) %>%
    filter(if (!is.na(expansions$REGION[i])) REGION == expansions$REGION[i] else TRUE) %>%
    filter(if (!is.na(expansions$SPP_NAME[i])) SPP_NAME == expansions$SPP_NAME[i] else TRUE) %>%
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
mh_ready <- Reduce(expand_mh, 1:length(expansions$column_name), init = mh_sector_id, accumulate = FALSE)
