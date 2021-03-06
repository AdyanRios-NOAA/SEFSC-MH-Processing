# Script 1
# Create new variables
  # Overview:####
  # Expand sector ALL to COMMERCIAL and RECREATIONAL
  # Add "Detailed" YES/NO field (from Google Sheets) to indicate which MANAGEMENT_TYPEs are detailed. 
  # Translate from old ZONE names to new ZONE names (from Google Sheets)
  # Create flag to indicate a MANAGEMENT_TYPE that serves as an "ADJUSTMENT"
  # Create various new variables for processing

#### 1 ####
# Expand records with SECTOR = ALL to be COMMERCIAL and RECREATIONAL ####
# Results in mh_sect_expanded data frame 
mh_sect_expanded <- mh_cleaned %>%
  # CREATE: SECTOR_USE variable  
  # Rename "ALL" records to 'RECREATIONAL,COMMERCIAL'
  mutate(SECTOR_USE = case_when(SECTOR == 'ALL' ~ 'RECREATIONAL,COMMERCIAL',
                                TRUE ~ SECTOR)) %>%
  # Expand SECTOR_USE at the commas
  separate_rows(SECTOR_USE)

#### 2 ####
# Read in Google Sheet with table outlining whether a MANAGEMENT_TYPE is detailed (Y/N) ####
# Only detailed MANAGEMENT_TYPEs will be expanded 
detailed_xref <- read_sheet("https://docs.google.com/spreadsheets/d/1PViPVtqkY3q1fWUFGZm1UyIIYrFxt-YDitEUGaHBjsg/edit#gid=1115852389") %>%
  select(-MANAGEMENT_CATEGORY)

# CHECK: Run crosschecks to assess if all MH MANAGEMENT_TYPEs have been accounted for ####
# Are all MH MANAGEMENT_TYPEs in the Google Sheet table (BLANK if yes)?
unique(mh_sect_expanded$MANAGEMENT_TYPE)[unique(mh_sect_expanded$MANAGEMENT_TYPE) %in% detailed_xref$MANAGEMENT_TYPE == FALSE]
# Are all Google Sheet MANAGEMENT_TYPEs in MH (BLANK if yes)?
detailed_xref$MANAGEMENT_TYPE[detailed_xref$MANAGEMENT_TYPE %in% mh_sect_expanded$MANAGEMENT_TYPE == FALSE]

# CREATE: Species related varaibles ####
# Edit mh_sect_expanded to result in mh_sect_expanded2 data frame
# CREATE: the variables of O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP to retain the original species information from the raw data
# CREATE: the variables of SPP_TYPE and SPP_NAME to be used to consolidate the information from O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP 
# Reformat the data - move species common name, aggregate name, and group name into single field (SPP_NAME) this field should never be null
mh_sect_expanded2 <- mh_sect_expanded %>%
  # CREATE: DETAILED field (YES/NO) to indicate whether or not the MANAGEMENT_TYPE is detailed or not. 
  #Detailed MANAGEMENT_TYPEs are those regulations that are able to be captured more thoroughly within the fields of the database.
  left_join(detailed_xref, by = "MANAGEMENT_TYPE") %>%
  # CREATE: duplicates of species fields to retain original data format
  mutate(O_COMMON_NAME = COMMON_NAME,
         O_SPECIES_AGGREGATE = SPECIES_AGGREGATE,
         O_SPECIES_GROUP = SPECIES_GROUP) %>%
  # CREATE: the variables of SPP_TYPE and SPP_NAME and transpose the species information contained in the 
  # O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP fields into the SPP_TYPE and SPP_NAME fields.
  # Therefore, the specific species information will only expand within a single field (SPP_NAME) 
  pivot_longer(cols = c("COMMON_NAME" = O_COMMON_NAME, "SPECIES_AGGREGATE" = O_SPECIES_AGGREGATE, 
                        "SPECIES_GROUP" = O_SPECIES_GROUP), names_to = "SPP_TYPE", values_to = "SPP_NAME") %>%
  # Remove records where name is null 
  filter(!is.na(SPP_NAME))

# CHECK: that the number of REGULATION_IDs and records remains the same between the mh_sect_expanded and mh_sect_expanded2 data frames ####
n_distinct(mh_sect_expanded$REGULATION_ID) == n_distinct(mh_sect_expanded2$REGULATION_ID)
nrow(mh_sect_expanded) == nrow(mh_sect_expanded2)


#### 3 ####
# Clean up area information (ZONE) ####
# Read in Google Sheets that outlines new ZONE names for some Gulf Reef Fish ZONEs
# These ZONEs were cleaned up for consistency and are outlined by species and sector
area_xref <- read_sheet("https://docs.google.com/spreadsheets/d/1gVFz6UUiN5LU3Fr9NuFSumKGeHvRubgZviJt7R73Gvg/edit#gid=0") %>%
  mutate_all(.funs = toupper) %>%
  # CHECK: Compare NEW_ZONE_NAMEs to the OLD_ZONE_NAMEs provided in the Google Sheets to avoid duplication
  select(OLD_ZONE_NAME, NEW_ZONE_NAME) %>%
  unique()

# CHECK: Run crosschecks to compare ZONE names between mh_sect_expanded and Google Sheet ####
# NOTE: ZONE clean up is only complete for GULF REEF so far
# Are all MH ZONE names in Google Sheet (BLANK if yes)?
unique(mh_sect_expanded2$ZONE)[unique(mh_sect_expanded2$ZONE) %in% area_xref$OLD_ZONE_NAME == FALSE]
# Are all Google Sheet ZONE names in MH (BLANK if yes)?
area_xref$OLD_ZONE_NAME[area_xref$OLD_ZONE_NAME %in% mh_sect_expanded2$ZONE == FALSE]

# Standardize ZONE names ####
# Results in mh_setup data frame
# CREATE: variable of ZONE_USE to incorporate the NEW_ZONE_NAMEs and create standard ZONE names  
# This step is performed after the sector expansion and before the species expansion
# because the species list contains duplicates which are addressed later in the mh_spp_expansion.R script
mh_setup <- mh_sect_expanded2 %>%
  left_join(area_xref, by = c("ZONE" = "OLD_ZONE_NAME")) %>%
  rename(ZONE_USE = "NEW_ZONE_NAME") %>%
  # Since the cross reference table only cleans Gulf Reef Fish ZONES,
  # replace all NAs with the original ZONE name outlined in the raw data
  mutate(ZONE_USE = case_when(is.na(ZONE_USE) ~ ZONE,
                              TRUE ~ ZONE_USE)) %>%
  # Remove commas (TRY NOT TO USE COMMAS IN ODM)
  mutate(ZONE_USE = gsub(",", "", ZONE_USE))

### 4 ####
# CREATE: additional variables ####
# Results in the mh_preprocess data frame
# CREATE: the variables of vol, page, MANAGEMENT_TYPE_USE, ADJUSTMENT, MANAGEMENT_STATUS_USE, REG_REMOVED
# CREATE: the variable of STATUS TYPE which categorizes the MANAGEMENT_STATUS_USE as GENERAL or COMPLEX 
mh_newvar <- mh_setup %>%
  # CREATE: vol and page but pulling out the volume and page number as separate fields from the FR_CITATION 
  # (currently a warning appears because page is NA for "81 FR 33150 B", but once fixed as a bug the warning should go away)
  # Volume and page are essential pieces to include for sorting
  mutate(vol = as.numeric(sub(" FR.*", "", FR_CITATION)),
         page = as.numeric(sub(".*FR ", "", FR_CITATION)),
         # CREATE: ADJUSTMENT variable to flag when he MANAGEMENT_TYPE contains the word "ADJUSTMENT" and remove "ADJUSTMENT" from the MANAGEMENT_TYPE name
         # ADJUSTMENTs are never redundant
         ADJUSTMENT = case_when(str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ 1,
                                MANAGEMENT_TYPE == "REOPENING" & !is.na(INEFFECTIVE_DATE) ~ 1,
                                TRUE ~ 0),
         MANAGEMENT_TYPE_USE = case_when(str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ str_replace(MANAGEMENT_TYPE, " ADJUSTMENT", ""),
                                         TRUE ~ MANAGEMENT_TYPE),
         # Rename REOPENING MANAGEMENT_TYPEs to CLOSURE MANAGEMENT_TYPEs and add OPEN or CLOSED to the VALUE field
         # Although the MANAGEMENT_TYPE will be different from the raw data (since the raw data stays true to the FR Language)
         # this will assist in processing dates to accurately capture the time series of openings/closures in the fishery
         MANAGEMENT_TYPE_USE = case_when(MANAGEMENT_TYPE == "REOPENING" ~ "CLOSURE",
                                         TRUE ~ MANAGEMENT_TYPE_USE),
         VALUE= case_when(MANAGEMENT_TYPE == "CLOSURE" ~ "CLOSE",
                          MANAGEMENT_TYPE == "REOPENING" ~ "OPEN",
                          TRUE ~ VALUE),
         # CREATE: MANAGEMENT_STATUS_USE variable by transposing the MANAGEMENT_STATUS values 
         # Replace NA MANAGEMENT_STATUS_USE as ONCE because processing NAs can be complex to process
         # Both MANAGEMENT_STATUS of NA and ONCE are meant to be processed the same way
         MANAGEMENT_STATUS_USE = case_when(is.na(MANAGEMENT_STATUS) ~ 'ONCE',
                                           TRUE ~ MANAGEMENT_STATUS),
         # CREATE: the variable of STATUS_TYPE with the values of SIMPLE or COMPLEX
         # A STATUS_TYPE of SIMPLE indicates a MANAGEMENT_STATUS_USE of ONCE
         # A STATUS_TYPE of COMPLEX indicates a MANAGEMENT_STATUS_USE that is recurring (SEASONAL, WEEKLY RECURRING, MONTHLY RECURRING, DAILY)
         STATUS_TYPE = case_when(MANAGEMENT_STATUS_USE == "ONCE" ~ "SIMPLE",
                                 MANAGEMENT_STATUS_USE %in% c("SEASONAL", "WEEKLY RECURRING", "MONTHLY RECURRING", "DAILY") ~ "RECURRING",
                                 TRUE ~ "COMPLEX"),
         # CREATE: the variable of REG_REMOVED to indicate when a regulation is "turned off"
         REG_REMOVED = case_when(EFFECTIVE_DATE == INEFFECTIVE_DATE ~ 1, TRUE ~ 0),
         # CREATE: the variables of GENERAL and COMPLEX to flag when a regulation has a 
         # STAUS_TYPE of SIMPLE or COMPLEX, respectively
         GENERAL = case_when(STATUS_TYPE == "SIMPLE" & is.na(VALUE) ~ 1, TRUE ~ 0),
         COMPLEX = case_when(STATUS_TYPE == "COMPLEX" ~ 1, TRUE ~ 0),
         # CREATE: variables to outline recurring start and end dates (this is to avoid the dual purpose of these fields)
         # CREATE: START_DAY_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_DAY),
         # CREATE: START_MONTH_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_MONTH),
         # CREATE: START_TIME_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_TIME),
         # CREATE: START_DAY_OF_WEEK_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ as.character(START_DAY_OF_WEEK)),
         # CREATE: END_DAY_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_DAY),
         # CREATE: END_MONTH_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_MONTH),
         # CREATE: END_TIME_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_TIME),
         # CREATE: END_DAY_OF_WEEK_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_DAY_OF_WEEK),
         # CREATE: START_DATE from the START_DAY, START_MONTH, and START_YEAR fields
         # The START_DATE field is only created when START_DAY, START_MONTH, and START_YEAR are provided and different from EFFECTIVE_DATE 
         # CREATE: END_DATE from the END_DAY, END_MONTH, and END_YEAR fields
         # The END_DATE field is only created when END_DAY, END_MONTH, and END_YEAR are provided and different from INEFFECTIVE_DATE
         START_DATE = case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                  !is.na(START_DAY) &
                                  !is.na(START_MONTH) &
                                  !is.na(START_YEAR) ~ as.Date(paste(START_MONTH, START_DAY, START_YEAR, sep = "/"), "%m/%d/%Y"),
                                TRUE ~ EFFECTIVE_DATE),
         START_DATE = case_when(START_DATE < EFFECTIVE_DATE ~ EFFECTIVE_DATE,
                                TRUE ~ START_DATE),
         END_DATE = case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                !is.na(END_DAY) &
                                !is.na(END_MONTH) &
                                !is.na(END_YEAR) ~ as.Date(paste(END_MONTH, END_DAY, END_YEAR, sep = "/"), "%m/%d/%Y"),
                              TRUE ~ INEFFECTIVE_DATE),
         # When the END_TIME is listed as "12:01:00 AM", the STATUS_TYPE is RECURRING, and the END_DAY is not equal to 1, 
         # the END_DAY should be reverted to one day prior. This will infer that the regulation remained in place through
         # the end of that day and not one minute into the next day.
         END_DAY = case_when(END_TIME == "12:01:00 AM" & STATUS_TYPE == "RECURRING" & END_DAY != 1 ~ END_DAY - 1,
                             TRUE ~ END_DAY),
         # For records meeting the same requirements as above, the END_TIME should be removed since the END_DATE has been
         # reverted to the day prior.
         END_TIME = case_when(END_TIME == "12:01:00 AM" & STATUS_TYPE == "RECURRING" & END_DAY != 1 ~ "11:59:00 PM",
                              TRUE ~ END_TIME),
         # When the START_TIME is equal to "11:59:00 PM", the START_DATE should be pushed ahead by one day since
         # the regulation will be in effect for the entirety of that day.
         START_DATE = case_when(START_TIME == "11:59:00 PM" ~ START_DATE + 1,
                                TRUE ~ START_DATE),
         # For records meeting the requirement above, the START_TIME should be removed since the START_DATE has been pushed
         # to the next day.
         START_TIME = case_when(START_TIME != "11:59:00 PM" ~ START_TIME),
         # For records with an END_TIME of "12:01:00 AM", the END_DATE should be reverted to one day prior.
         # This will infer that the regulation remained in place through the end of that day and not one minute into the next day.
         END_DATE = case_when(END_TIME == "12:01:00 AM" ~ END_DATE - 1,
                                TRUE ~ END_DATE),
         # For records meeting the requirement above, the END_TIME should be removed since the END_DATE has been reverted
         # to the day prior.
         END_TIME = case_when(END_TIME != "12:01:00 AM" ~ END_TIME)) 
