# 3
# Pre-processing clean up
  # Translate to new zone names (from Google sheets),
  # Rename mtype "adjustment" to be a flag
  # Create various new variables for processing

# Suppress warning if Output directory already exists
options(warn=-1)

# Areas clean up
# Read in Google sheets for new zone name for some Gulf Reef Fish zone names
area_xref <- read_sheet("https://docs.google.com/spreadsheets/d/1gVFz6UUiN5LU3Fr9NuFSumKGeHvRubgZviJt7R73Gvg/edit#gid=0") %>%
  mutate_all(.funs = toupper) %>%
  # Zone names provided by species and sector which are removed to avoid duplicate names
  select(OLD_ZONE_NAME, NEW_ZONE_NAME) %>%
  unique()

# New zone name variable called ZONE_USE as the standard zone name
# Starting from sector expansion and not species because the species list have duplicates and need to be cleaned up first
mh_preprocess <- mh_sect_expanded %>%
  left_join(area_xref, by = c("ZONE" = "OLD_ZONE_NAME")) %>%
  rename(ZONE_USE = "NEW_ZONE_NAME") %>%
  # Since the cross reference table is only for Gulf Reef Fish, replace all NAs with the original zone name
  mutate(ZONE_USE = case_when(is.na(ZONE_USE) ~ ZONE,
                              TRUE ~ ZONE_USE)) %>%
  # Remove commas
  mutate(ZONE_USE = gsub(",", "", ZONE_USE))

# Create new variables
mh_preprocess <- mh_preprocess %>%
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


# FIND SECTOR FORKS

# Create the Output directory folder where all .csv files will be sent to (this folder will be in the gitignore)
dir.create("./Output")

#SAFE FOR PIVOT TABLE TESTS IN EXCEL
write.csv(mh_preprocess, paste0("./Output/MHpreprocess_", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)

multi_subsector <- mh_preprocess %>%
  filter(GENERAL == 0) %>%
  select(FMP, MANAGEMENT_TYPE_USE, COMMON_NAME, SPECIES_AGGREGATE, SPECIES_GROUP, SECTOR_USE, SUBSECTOR) %>%
  distinct() %>%
  group_by(FMP, MANAGEMENT_TYPE_USE, COMMON_NAME, SPECIES_AGGREGATE, SPECIES_GROUP, SECTOR_USE) %>%
  mutate(subsector_count = length(SUBSECTOR)) %>%
  filter(subsector_count > 1) %>%
  arrange(FMP, MANAGEMENT_TYPE_USE, COMMON_NAME, SPECIES_AGGREGATE, SPECIES_GROUP, SECTOR_USE)

table(multi_subsector$subsector_count)
table(multi_subsector$subsector_count, multi_subsector$SECTOR_USE)

test = filter(multi_subsector, FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO")
table(test$subsector_count)
table(test$subsector_count, test$SECTOR_USE) #add minimum date to see when they happen

# EXPANSION FUNTION ####
# ADAPTED TO ALLOW CONDITIONS AND EXPANSION TO BE REGION/FMP/SECTOR/SPECIES SPECIFIC (ETC)
expand_mh <- function(datin, i) {
  data_expand <- datin %>%
    filter(if (expansions$MANAGEMENT_TYPE_USE[i] != "") MANAGEMENT_TYPE_USE == expansions$MANAGEMENT_TYPE_USE[i] else TRUE) %>%
    filter(if (expansions$FMP[i] != "") FMP == expansions$FMP[i] else TRUE) %>%
    filter(if (expansions$REGION[i] != "") REGION == expansions$REGION[i] else TRUE) %>%
    filter(if (expansions$COMMON_NAME[i] != "") COMMON_NAME == expansions$COMMON_NAME[i] else TRUE) %>%
    #filter(if (expansions$SPECIES_AGGREGATE[i] != "") SPECIES_AGGREGATE == expansions$SPECIES_AGGREGATE[i] else TRUE) %>%
    #filter(if (expansions$SPECIES_GROUP[i] != "") SPECIES_GROUP == expansions$SPECIES_GROUP[i] else TRUE) %>%
    mutate(!!expansions$column_name[i] := case_when(get(expansions$column_name[i]) == expansions$expand_from[i] ~ expansions$expand_to[i],
                                                    get(expansions$column_name[i]) != expansions$expand_from[i] ~ get(expansions$column_name[i]))) %>%
    separate_rows(!!expansions$column_name[i], sep = ", ")

  return(data_expand %>%
           bind_rows(., filter(datin, !REGULATION_ID %in% data_expand$REGULATION_ID)) %>%
           data.frame())
}

# READ IN AND RUN EXPANSIONS ####
expansions <- read.csv("./MHpreprocess_expansions.csv", stringsAsFactors = FALSE,
                       fileEncoding = "latin1")
mh_ready <- Reduce(expand_mh, 1:length(expansions$column_name), init = mh_preprocess, accumulate = FALSE)
