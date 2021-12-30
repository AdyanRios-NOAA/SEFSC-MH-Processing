# 3
# Pre-processing clean up
  # Translate to new zone names (from Google sheets),
  # Rename mtype "adjustment" to be a flag
  # Create various new variables for processing

# Areas clean up
# Read in Google sheets for new zone name for some Gulf Reef Fish zone names
area_xref <- read_sheet("https://docs.google.com/spreadsheets/d/1gVFz6UUiN5LU3Fr9NuFSumKGeHvRubgZviJt7R73Gvg/edit#gid=0") %>%
  mutate_all(.funs = toupper) %>%
  # Zone names provided by species and sector which are removed to avoid duplicate names
  select(OLD_ZONE_NAME, NEW_ZONE_NAME) %>%
  unique()

# New zone name variable called ZONE_USE as the standard zone name
# Starting from sector expansion and not species because the species list have duplicates and need to be cleaned up first
mh_preprocess <- mh_sp_expanded %>%
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

#SAFE FOR PIVOT TABLE TESTS IN EXCEL
write.csv(mh_preprocess, here('data/preprocessed', paste0("MHpreprocess_", format(Sys.Date(), "%d%b%Y"), ".csv")), row.names = FALSE)

#CREATE A WAY TO TRACK WHICH SECTORS HAVE MULTIPLE SUBSECTORS
sector.match <- c("MANAGEMENT_TYPE_USE", "MANAGEMENT_STATUS_USE",
                  "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
                  "SECTOR_USE",
                  "REGION",
                  "SPP_NAME", "COMMON_NAME_USE")

sector_precluster <- mh_preprocess %>%
  select(one_of(sector.match)) %>%
  distinct() %>%
  mutate(SECTOR_ID = as.numeric(row.names(.))) %>%
  right_join(., mh_preprocess)

# ONLY COUNT NUMBER OF SUBSECTORS FOR IMPORTANT REGULATION TYPES
multi_subsector <- sector_precluster %>%
  filter(GENERAL == 0) %>%
  select(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR) %>%
  distinct() %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID) %>%
  mutate(subsector_count = length(SUBSECTOR),
         #FLAG IF ALL IS USED
         all_used = sum(SUBSECTOR == "ALL")) %>%
  filter(subsector_count > 1) %>%
  arrange(SECTOR_ID, SUBSECTOR) %>%
  data.frame()


multi_subsector2 <- sector_precluster %>%
  select(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR, EFFECTIVE_DATE) %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR) %>%
  summarize(start_use = min(EFFECTIVE_DATE)) %>%
  right_join(., multi_subsector) %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID) %>%
  mutate(date_count = length(unique(start_use))) %>%
  arrange(SECTOR_ID) %>%
  filter(!(all_used == 0 & date_count == 1)) %>%
  group_by(SECTOR_ID) %>%
  mutate(SUBSECTOR_KEY = paste(unique(SUBSECTOR), sep = ",", collapse=', ')) %>%
  data.frame()

unique_sector_keys = multi_subsector2 %>%
  select(SECTOR_USE, SUBSECTOR_KEY) %>%
  group_by(SECTOR_USE, SUBSECTOR_KEY) %>%
  mutate(SUBSECTOR_N = length(SECTOR_USE)) %>%
  distinct() %>%
  arrange(SUBSECTOR_N) %>%
  data.frame()

test = multi_subsector2 %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO") %>%
  select(SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY) %>%
  distinct()

# EXPANSION FUNCTION ####
# ADAPTED TO ALLOW CONDITIONS AND EXPANSION TO BE REGION/FMP/SECTOR/SPECIES SPECIFIC (ETC)
expand_mh <- function(datin, i) {
  data_expand <- datin %>%
    filter(if (expansions$MANAGEMENT_TYPE_USE[i] != "") MANAGEMENT_TYPE_USE == expansions$MANAGEMENT_TYPE_USE[i] else TRUE) %>%
    filter(if (expansions$FMP[i] != "") FMP == expansions$FMP[i] else TRUE) %>%
    filter(if (expansions$REGION[i] != "") REGION == expansions$REGION[i] else TRUE) %>%
    filter(if (expansions$SPP_NAME[i] != "") SPP_NAME == expansions$SPP_NAME[i] else TRUE) %>%
    filter(if (expansions$COMMON_NAME_USE[i] != "") COMMON_NAME_USE == expansions$COMMON_NAME_USE[i] else TRUE) %>%
    mutate(!!expansions$column_name[i] := case_when(get(expansions$column_name[i]) == expansions$expand_from[i] ~ expansions$expand_to[i],
                                                    get(expansions$column_name[i]) != expansions$expand_from[i] ~ get(expansions$column_name[i]))) %>%
    separate_rows(!!expansions$column_name[i], sep = ", ")

  return(data_expand %>%
           bind_rows(., filter(datin, !REGULATION_ID %in% data_expand$REGULATION_ID)) %>%
           data.frame())
}

# READ IN AND RUN EXPANSIONS ####
expansions <- read.csv(here('data/interim', "./MHpreprocess_expansions.csv"), stringsAsFactors = FALSE,
                       fileEncoding = "latin1")
mh_ready <- Reduce(expand_mh, 1:length(expansions$column_name), init = mh_preprocess, accumulate = FALSE)
