# Data bugs and changes in the database script

# For changes in the database, this includes reclassifying management cateogries and types

# For data bugs, all bugs have been previously addressed or entered into the data bug tracking system
  # Record each data bug ID in each step

library(tidyverse)
library(lubridate)

# Read in data
mh <- read.csv("./notebooks/all_size_limits_mh_download.csv", stringsAsFactors = FALSE)

# Data types of variables
str(mh)

# Changes that are a result of the csv download (the database does not have these issues)
mh_cleaned <- mh %>%
  # Rename regulation ID to match what appears in the database
  rename(REGULATION_ID = REGULATION_ID.) %>%
  # Remove ="..." characters in species ITIS codes
  mutate(SPECIES_ITIS = gsub('="', '', SPECIES_ITIS),
         SPECIES_ITIS = gsub('"', '', SPECIES_ITIS))

# Change all closed areas to closures
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(MANAGEMENT_TYPE == 'CLOSED AREA' ~ 'CLOSURE',
                                     # This means if the previous condition is not met, then MANAGEMENT_TYPE should be the original MANAGEMENT_TYPE
                                     TRUE ~ MANAGEMENT_TYPE))


# Move all definition terms to value and rename management category = OTHER and management type = DEFINITION
mh_cleaned <- mh_cleaned %>%
  mutate(VALUE = case_when(MANAGEMENT_CATEGORY == 'DEFINITION' ~ MANAGEMENT_TYPE,
                           TRUE ~ VALUE),
         MANAGEMENT_TYPE = case_when(MANAGEMENT_CATEGORY == 'DEFINITION' ~ 'DEFINITION',
                                     TRUE ~ MANAGEMENT_TYPE),
         MANAGEMENT_CATEGORY = case_when(MANAGEMENT_CATEGORY == 'DEFINITION' ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY))

# Remove Records
mh_cleaned <- mh_cleaned %>%
  # Bug ID 4955
  filter(REGULATION_ID != 438) %>%
  # Bug ID 4953
  filter(REGULATION_ID != 924) %>%
  # Bug ID 4952
  filter(REGULATION_ID != 648) %>%
  # Bug ID 4951
  filter(REGULATION_ID != 577) %>%
  # Bug ID 4489
  filter(REGULATION_ID != 1480) %>%
  # Bug ID 4457
  filter(REGULATION_ID != 418)

# Bug ID 4460 - Change FR citation from 78 FR 22949 to 78 FR 22950
mh_cleaned <- mh_cleaned %>%
  mutate(FR_CITATION = case_when(REGULATION_ID == 2126 ~ '78 FR 22950',
                                 TRUE ~ FR_CITATION))

# Bug ID 4459 - Change FR citation from 78 FR 22949 to 78 FR 22950 and change mtype to IGNORABLE
mh_cleaned <- mh_cleaned %>%
  mutate(FR_CITATION = case_when(REGULATION_ID == 2398 ~ '78 FR 22950',
                                 TRUE ~ FR_CITATION),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 2398 ~ 'IGNORABLE',
                                     TRUE ~ MANAGEMENT_TYPE),
         FLAG = case_when(REGULATION_ID == 2398 ~ 'NO',
                          TRUE ~ FLAG))

# Bug ID 4461 - Change all effective dates to 1/3/17
mh_cleaned <- mh_cleaned %>%
  mutate(EFFECTIVE_DATE = case_when(FR_CITATION == '81 FR 86971' ~ "01/03/2017",
                                    TRUE ~ EFFECTIVE_DATE))

# Bug ID 4462 - Correction to 81 FR 86971
    # Guidance changed in how corrections to dates are handled, remove all records except for one for this FR which becomes ignorable
mh_cleaned <- mh_cleaned %>%
  # To remove a series of regs related to the same bug ID, use ! in front of the logical statement
  filter(!REGULATION_ID %in% c(672, 673, 674, 675, 676, 838, 839, 840, 841, 842, 843)) %>%
  mutate(FR_SECTION = case_when(REGULATION_ID == 671 ~ '50 CFR 622',
                                TRUE ~ FR_SECTION),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 671 ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 671 ~ 'IGNORABLE',
                                     TRUE ~ MANAGEMENT_TYPE),
         SUBSECTOR = case_when(REGULATION_ID == 671 ~ 'ALL',
                               TRUE ~ SUBSECTOR),
         START_DAY = case_when(REGULATION_ID == 671 ~ 3L,
                               TRUE ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID == 671 ~ 1L,
                                 TRUE ~ START_MONTH),
         # Inverse the statements when you want to remove values because when condition is not met (TRUE ~ NA) is implied
         END_YEAR = case_when(REGULATION_ID != 671 ~ END_YEAR),
         VALUE = case_when(REGULATION_ID != 671 ~ VALUE),
         VALUE_UNITS = case_when(REGULATION_ID != 671 ~ VALUE_UNITS),
         VALUE_TYPE = case_when(REGULATION_ID != 671 ~ VALUE_TYPE),
         VALUE_RATE = case_when(REGULATION_ID != 671 ~ VALUE_RATE))

# Bug ID 4954 - remove the 1,000 hook limit
mh_cleaned <- mh_cleaned %>%
  mutate(VALUE = case_when(REGULATION_ID == 911 ~ '1000',
                           TRUE ~ VALUE),
         INEFFECTIVE_DATE = case_when(REGULATION_ID == 911 ~ EFFECTIVE_DATE,
                                      TRUE ~ INEFFECTIVE_DATE),
         END_DAY = case_when(REGULATION_ID == 911 ~ START_DAY,
                          TRUE ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID == 911 ~ START_MONTH,
                             TRUE ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID == 911 ~ START_YEAR,
                             TRUE ~ END_YEAR))

# Bug ID 5048 - mng. status updated to seasonal
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1025 ~ 'SEASONAL',
                                      TRUE ~ MANAGEMENT_STATUS),
         END_YEAR = case_when(REGULATION_ID != 1025 ~ END_YEAR),
         START_YEAR = case_when(REGULATION_ID !=1025 ~ START_YEAR))

#Bug ID 5049 - mng. status updated to seasonal
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 752 ~ 'SEASONAL',
                                       TRUE ~ MANAGEMENT_STATUS),
         END_YEAR = case_when(REGULATION_ID != 752 ~ END_YEAR),
         START_YEAR = case_when(REGULATION_ID !=752 ~ START_YEAR))

#Bug ID 5047  - update end year and mng status
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1322 ~ 'SEASONAL',
                                       TRUE ~ MANAGEMENT_STATUS),
         START_YEAR = case_when(REGULATION_ID !=1322 ~ START_YEAR))

#Bug ID 5046 - update start day and month
mh_cleaned <- mh_cleaned %>%
  mutate(START_DAY = case_when(REGULATION_ID == 1460 ~ END_DAY,
                               TRUE ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID == 1460 ~ END_MONTH,
                                 TRUE ~ START_MONTH))
#Bug ID 4456 - change accountability measure to NO for 74 FR 17603
mh_cleaned <- mh_cleaned %>%
  mutate(ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 238 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 669 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 668 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 594 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 593 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 475 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 474 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 239 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 476 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 592 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 240 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 420 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 595 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 933 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 237 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 670 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 932 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 667 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 377 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 376 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 375 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 236 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 235 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 471 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 472 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 931 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 1019 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 470 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 234 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 760 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE),
         ACCOUNTABILITY_MEASURE = case_when(REGULATION_ID == 473 ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE))

# Bug ID 4446 - change zone for more accuracy to specific to counties
mh_cleaned <- mh_cleaned %>%
  mutate(ZONE = case_when(REGULATION_ID == 3653 ~ 'FLORIDA COUNTIES - MANATEE, SARASOTA, DESOTO, CHARLOTTE, LEE, COLLIER, MONROE, MIAMI-DADE, AND BROWARD',
                          TRUE ~ ZONE))

#Bug ID 4488 - change flag to NO
mh_cleaned <- mh_cleaned %>%
  mutate(FLAG = case_when(REGULATION_ID == 1479 ~ 'NO', TRUE ~ FLAG))

#BUG ID 4487 - mng status changed to withdrawn and updated to spatial controls:closed area
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1355 ~ 'WITHDRAWN',
                                       TRUE ~ MANAGEMENT_STATUS),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1355 ~ 'SPATIAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1355 ~ 'CLOSED AREA',
                                     TRUE ~ MANAGEMENT_TYPE))

#BUG ID 4727 - change region from ALL to GOM for specificity - CANT FIND FR IN R
mh_cleaned <- mh_cleaned %>%
  mutate(REGION = case_when(REGULATION_ID == 1340 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))
#BUG ID 4726 -  change region from ALL to GOM for specificity - CANT FIND FR IN R
mh_cleaned <- mh_cleaned %>%
  mutate(REGION = case_when(REGULATION_ID == 1341 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID 4597 - change mng cat:type from possession limit to gear limitations:prohibited gear, update values to be blank
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1130 ~ 'GEAR LIMITATIONS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1130 ~ 'PROHIBITED GEAR',
                                     TRUE ~ MANAGEMENT_TYPE),
         VALUE = case_when(REGULATION_ID == 1130 ~ '',
                           TRUE ~ VALUE),
         VALUE_UNITS = case_when(REGULATION_ID == 1130 ~ '',
                                 TRUE ~ VALUE_UNITS),
         VALUE_TYPE = case_when(REGULATION_ID == 1130 ~ '',
                                TRUE ~ VALUE_TYPE),
         VALUE_RATE = case_when(REGULATION_ID == 1130 ~ '',
                                TRUE ~ VALUE_RATE))

#BUG ID 4596 - change mng cat:type from possession limit to effort limits:prohibited species
    #change FR section to 50 CFR 641, update value fields to blank
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 1108 ~ 'PROHIBITED SPECIES',
                                         TRUE ~ MANAGEMENT_TYPE),
         VALUE = case_when(REGULATION_ID == 1108 ~ '',
                           TRUE ~ VALUE),
         VALUE_UNITS = case_when(REGULATION_ID == 1108 ~ '',
                                 TRUE ~ VALUE_UNITS),
         VALUE_TYPE = case_when(REGULATION_ID == 1108 ~ '',
                                TRUE ~ VALUE_TYPE),
         VALUE_RATE = case_when(REGULATION_ID == 1108 ~ '',
                                TRUE ~ VALUE_RATE),
         FR_SECTION = case_when(REGULATION_ID == 1108 ~ '50 CFR 641',
                                TRUE ~ FR_SECTION))

#BUG ID 4592 - change flag to NO b/c flag is captured thru addition of incidental catch record addition in bug id 4588
mh_cleaned <- mh_cleaned %>%
  mutate(FLAG = case_when(REGULATION_ID == 1042 ~ 'NO',
                          TRUE ~ FLAG))

#BUG ID 4727 - update region from ALL to GULF OF MEXICO
mh_cleaned <- mh_cleaned %>%
  mutate(REGION = case_when(REGULATION_ID == 1340 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID 4726 - update region from ALL to GULF OF MEXICO
mh_cleaned <- mh_cleaned %>%
  mutate(REGION = case_when(REGULATION_ID == 1341 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID -1 - Correct missing mng. type/sector/region for reg id 5474
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 5474 ~ 'BAG LIMIT',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 5474 ~ 'RECREATIONAL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 5474 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID -2 - Correct missing mng. type/sector/region for reg id 5473
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 5473 ~ 'CREW BAG LIMIT',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 5473 ~ 'RECREATIONAL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 5473 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID -3 - Correct missing mng. type/sector/region for reg id 5472
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 5472 ~ 'QUOTA',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 5472 ~ 'COMMERCIAL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 5472 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID -4 - Correct missing mng. type/sector/region for reg id 5471
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 5471 ~ 'QUOTA',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 5471 ~ 'RECREATIONAL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 5471 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID -5 - Correct missing mng. type/sector/region for reg id 5427
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 5427 ~ 'MINIMUM SIZE LIMIT',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 5427 ~ 'COMMERCIAL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 5427 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID -6 - Correct missing mng. type/sector/region for reg id 2272
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 2272 ~ 'FISHING SEASON',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 2272 ~ 'ALL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 2272 ~ 'CARIBBEAN',
                            TRUE ~ REGION))

#BUG ID -7 - Correct missing mng. type/sector/region for reg id 2197
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 2197 ~ 'CLOSED AREA',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 2197 ~ 'ALL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 2197 ~ 'CARIBBEAN',
                            TRUE ~ REGION))

#BUG ID -8 - Correct missing mng. type/sector/region for reg id 2440
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 2440 ~ 'IGNORABLE',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 2440 ~ 'ALL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 2440 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))
#Create empty data frame
mh_added = mh_cleaned %>% filter(is.na(REGULATION_ID))

names(mh_added)

#BUG ID 4588 - add catch limits:incidental catch record for 5 red snapper under size limit,
#WORKING BUT NOT ADDING TO MH_ADDED??
mh_added %>% add_row(REGULATION_ID = -998,
                     LAST_UPDATED	= "10/01/2021",
                     JURISDICTION 	= "FEDERAL",
                     ACTION = "FINAL",
                     ACTION_TYPE	= "ORIGINAL FMP",
                     ACCOUNTABILITY_MEASURE	= "NO",
                     FR_CITATION	= "49 FR 39548",
                     FR_SECTION	= "50 FR 641.23",
                     FR_URL	= "https://www.govinfo.gov/app/details/FR-1984-10-09",
                     MANAGEMENT_CATEGORY	= "CATCH LIMITS",
                     MANAGEMENT_TYPE	= "INCIDENTAL CATCH",
                     SECTOR	= "ALL",
                     SUBSECTOR	= "ALL",
                     REGION	= "GULF OF MEXICO",
                     ZONE	= "ALL",
                     JURISDICTIONAL_WATERS	= "EEZ",
                     COMMON_NAME	= "SNAPPER, RED",
                     SPECIES_ITIS	= 168853,
                     FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                     EFFECTIVE_DATE	= "11/08/1984",
                     INEFFECTIVE_DATE	= "",
                     START_DAY	= 8,
                     START_MONTH	= 11,
                     START_YEAR	= 1984,
                     VALUE	= "5",
                     VALUE_UNITS	= "FISH",
                     FLAG = "YES")

#BUG ID 4586 - add rows for other:state related for gag, red snapper, greater amberjack, and gray triggerfish
#WORKING BUT NOT ADDING TO MH_ADDED??
mh_added %>% add_row(REGULATION_ID = -997,
                     LAST_UPDATED	= "10/01/2021",
                     JURISDICTION 	= "FEDERAL",
                     ACTION = "TEMPORARY RULE",
                     ACTION_TYPE	= "INTERIM MEASURES",
                     ACCOUNTABILITY_MEASURE	= "NO",
                     FR_CITATION	= "73 FR 73192",
                     FR_SECTION	= "50 CFR 622.4",
                     FR_URL	= "https://www.federalregister.gov/documents/2008/12/02/E8-28616/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-reef-fish-fishery-of-the-gulf-of-mexico",
                     MANAGEMENT_CATEGORY	= "OTHER",
                     MANAGEMENT_TYPE	= "STATE RELATED",
                     SECTOR	= "ALL",
                     SUBSECTOR	= "ALL",
                     REGION	= "GULF OF MEXICO",
                     ZONE	= "ALL",
                     JURISDICTIONAL_WATERS	= "EEZ",
                     COMMON_NAME	= "GROUPER, GAG",
                     SPECIES_ITIS	= 167759,
                     FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                     MANAGEMENT_STATUS	= "ONCE",
                     EFFECTIVE_DATE	= "01/01/2009",
                     INEFFECTIVE_DATE	= "05/31/2009",
                     START_DAY	= 1,
                     START_MONTH	= 1,
                     START_YEAR	= 2009,
                     END_DAY	= 31,
                     END_MONTH	= 5,
                     END_YEAR	= 2009,
                     FLAG = "YES")

mh_added %>% add_row(REGULATION_ID = -996,
                     LAST_UPDATED	= "10/01/2021",
                     JURISDICTION 	= "FEDERAL",
                     ACTION = "TEMPORARY RULE",
                     ACTION_TYPE	= "INTERIM MEASURES",
                     ACCOUNTABILITY_MEASURE	= "NO",
                     FR_CITATION	= "73 FR 73192",
                     FR_SECTION	= "50 CFR 622.4",
                     FR_URL	= "https://www.federalregister.gov/documents/2008/12/02/E8-28616/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-reef-fish-fishery-of-the-gulf-of-mexico",
                     MANAGEMENT_CATEGORY	= "OTHER",
                     MANAGEMENT_TYPE	= "STATE RELATED",
                     SECTOR	= "ALL",
                     SUBSECTOR	= "ALL",
                     REGION	= "GULF OF MEXICO",
                     ZONE	= "ALL",
                     JURISDICTIONAL_WATERS	= "EEZ",
                     COMMON_NAME	= "SNAPPER, RED",
                     SPECIES_ITIS	= 168853,
                     FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                     MANAGEMENT_STATUS	= "ONCE",
                     EFFECTIVE_DATE	= "01/01/2009",
                     INEFFECTIVE_DATE	= "05/31/2009",
                     START_DAY	= 1,
                     START_MONTH	= 1,
                     START_YEAR	= 2009,
                     END_DAY	= 31,
                     END_MONTH	= 5,
                     END_YEAR	= 2009,
                     FLAG = "YES")

mh_added %>% add_row(REGULATION_ID = -995,
                     LAST_UPDATED	= "10/01/2021",
                     JURISDICTION 	= "FEDERAL",
                     ACTION = "TEMPORARY RULE",
                     ACTION_TYPE	= "INTERIM MEASURES",
                     ACCOUNTABILITY_MEASURE	= "NO",
                     FR_CITATION	= "73 FR 73192",
                     FR_SECTION	= "50 CFR 622.4",
                     FR_URL	= "https://www.federalregister.gov/documents/2008/12/02/E8-28616/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-reef-fish-fishery-of-the-gulf-of-mexico",
                     MANAGEMENT_CATEGORY	= "OTHER",
                     MANAGEMENT_TYPE	= "STATE RELATED",
                     SECTOR	= "ALL",
                     SUBSECTOR	= "ALL",
                     REGION	= "GULF OF MEXICO",
                     ZONE	= "ALL",
                     JURISDICTIONAL_WATERS	= "EEZ",
                     COMMON_NAME	= "AMBERJACK, GREATER",
                     SPECIES_ITIS	= 168689,
                     FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                     MANAGEMENT_STATUS	= "ONCE",
                     EFFECTIVE_DATE	= "01/01/2009",
                     INEFFECTIVE_DATE	= "05/31/2009",
                     START_DAY	= 1,
                     START_MONTH	= 1,
                     START_YEAR	= 2009,
                     END_DAY	= 31,
                     END_MONTH	= 5,
                     END_YEAR	= 2009,
                     FLAG = "YES")

mh_added %>% add_row(REGULATION_ID = -994,
                     LAST_UPDATED	= "10/01/2021",
                     JURISDICTION 	= "FEDERAL",
                     ACTION = "TEMPORARY RULE",
                     ACTION_TYPE	= "INTERIM MEASURES",
                     ACCOUNTABILITY_MEASURE	= "NO",
                     FR_CITATION	= "73 FR 73192",
                     FR_SECTION	= "50 CFR 622.4",
                     FR_URL	= "https://www.federalregister.gov/documents/2008/12/02/E8-28616/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-reef-fish-fishery-of-the-gulf-of-mexico",
                     MANAGEMENT_CATEGORY	= "OTHER",
                     MANAGEMENT_TYPE	= "STATE RELATED",
                     SECTOR	= "ALL",
                     SUBSECTOR	= "ALL",
                     REGION	= "GULF OF MEXICO",
                     ZONE	= "ALL",
                     JURISDICTIONAL_WATERS	= "EEZ",
                     COMMON_NAME	= "TRIGGERFISH, GRAY",
                     SPECIES_ITIS	= 173138,
                     FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                     MANAGEMENT_STATUS	= "ONCE",
                     EFFECTIVE_DATE	= "01/01/2009",
                     INEFFECTIVE_DATE	= "05/31/2009",
                     START_DAY	= 1,
                     START_MONTH	= 1,
                     START_YEAR	= 2009,
                     END_DAY	= 31,
                     END_MONTH	= 5,
                     END_YEAR	= 2009,
                     FLAG = "YES")

#BUG ID 5116 - Add ITQ program est. record to 61 FR 7751
mh_added %>% add_row(REGULATION_ID = -993,
                     LAST_UPDATED	= "10/01/2021",
                     JURISDICTION 	= "FEDERAL",
                     ACTION = "EMERGENCY INTERIM RULE",
                     ACCOUNTABILITY_MEASURE	= "NO",
                     FR_CITATION	= "61 FR 7751",
                     FR_SECTION	= "50 CFR 641.10",
                     FR_URL	= "https://www.federalregister.gov/documents/1996/02/29/96-4432/reef-fish-fishery-of-the-gulf-of-mexico-revised-1996-red-snapper-season",
                     MANAGEMENT_CATEGORY	= "CATCH SHARES",
                     MANAGEMENT_TYPE	= "ITQ PROGRAM ESTABLISHED",
                     SECTOR	= "COMMERCIAL",
                     SUBSECTOR	= "ALL",
                     REGION	= "GULF OF MEXICO",
                     ZONE	= "ALL",
                     JURISDICTIONAL_WATERS	= "EEZ",
                     COMMON_NAME	= "SNAPPER, RED",
                     SPECIES_ITIS	= 168853,
                     FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                     SPECIES_AGGREGATE	= "",
                     SPECIES_GROUP	= "",
                     MANAGEMENT_STATUS	= "ONCE",
                     EFFECTIVE_DATE	= "02/23/1996",
                     INEFFECTIVE_DATE	= "05/29/1996",
                     START_DAY	= 23,
                     START_MONTH	= 2,
                     START_YEAR	= 1996,
                     END_DAY	= 29,
                     END_MONTH	= 5,
                     END_YEAR	= 1996,
                     FLAG = "YES")



# Test bug id changes
test <- mh_added %>%
  filter(FR_CITATION %in% c('73 FR 73192'))
view(test)

# Test bug id changes
test <- mh_added %>%
  filter(REGULATION_ID %in% c(-998))
view(test)

test <- mh_added
View(test)

mh_cleaned <- mh_cleaned %>%
  filter(FR_CITATION %in% c('78 FR 17882'))
view(mh_cleaned)
