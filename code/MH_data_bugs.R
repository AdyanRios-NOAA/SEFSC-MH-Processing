# Data bugs and changes in the database script

# For changes in the database, this includes reclassifying management cateogries and types

# For data bugs, all bugs have been previously addressed or entered into the data bug tracking system
  # Record each data bug ID in each step


# Read in data
mh <- read.csv(here('data/raw', "MH_DOWNLOAD_FEB_22_2022.csv"), stringsAsFactors = FALSE)

# Run only if you want to find the data type of MH variables
# str(mh)

# Changes that are a result of the csv download (the database does not have these issues)
# No longer appears to be an issue when downloading the data to a csv - SFA 9/27/2021
mh_cleaned <- mh %>%
  # Format dates to be mm/dd/yyyy to match added data (this may not be an issue in the future when pull directly from the database)
  mutate(EFFECTIVE_DATE = format(mdy(EFFECTIVE_DATE), "%m/%d/%Y"),
         INEFFECTIVE_DATE = format(mdy(INEFFECTIVE_DATE), "%m/%d/%Y")) %>%
  # Rename regulation ID to match what appears in the database
  #rename(REGULATION_ID = REGULATION_ID.) %>%
  #Remove ="..." characters in species ITIS codes
  mutate(SPECIES_ITIS = gsub('="', '', SPECIES_ITIS),
         SPECIES_ITIS = gsub('"', '', SPECIES_ITIS))

# Remove Records
mh_cleaned <- mh_cleaned %>%
  # Bug ID 5080
  filter(REGULATION_ID != 1160) %>%
  # Bug ID 5081
  filter(REGULATION_ID != 1161) %>%
  # Bug ID 5082
  filter(REGULATION_ID != 1170) %>%
  # Bug ID 4464
  filter(REGULATION_ID != 1222) %>%
  # Bug ID 4464
  filter(REGULATION_ID != 1223) %>%
  # Bug ID 4464
  filter(REGULATION_ID != 1224) %>%
  # BUG ID 4464
  filter(REGULATION_ID != 1225) %>%
  # BUG ID 4464
  filter(REGULATION_ID != 1226) %>%
  # BUG ID 4464
  filter(REGULATION_ID != 1227) %>%
  #Bug ID 4591
  filter(REGULATION_ID != 1341) %>%
  #BUG ID -9 - REMOVING DUPLICATE ERRONEOUS FR (85 FR 50333)
  filter(REGULATION_ID != 207) %>%
  #BUG ID -10 - REMOVING DUPLICATE ERRONEOUS FR (85 FR 50334)
  filter(REGULATION_ID != 206) %>%
  #Bug ID 5326
  filter(REGULATION_ID != 872) %>%
  #Bug ID 5922
  filter(REGULATION_ID != 828) %>%
  #Bug ID 5923
  filter(REGULATION_ID != 829) %>%
  #Bug ID 5924
  filter(REGULATION_ID != 830) %>%
  #Bug ID 5925
  filter(REGULATION_ID != 831) %>%
  #Bug ID 6237
  filter(REGULATION_ID != 809) %>%
  #Bug ID 6276
  filter(REGULATION_ID != 435) %>%
  #Bug ID 6606
  filter(REGULATION_ID != 11866) %>%
  #Bug ID -31
  filter(REGULATION_ID != 360) %>%
  #Bug ID -32
  filter(REGULATION_ID != 359) %>%
  #Bug ID -43
  filter(REGULATION_ID != 644) %>%
  #BUG ID -44
  filter(REGULATION_ID != 645) %>%
  #BUG ID -45
  filter(REGULATION_ID != 646) %>%
  #BUG ID -46
  filter(REGULATION_ID != 647) %>%
  #BUG ID -47
  filter(REGULATION_ID != 247) %>%
  #BUG ID -48
  filter(REGULATION_ID != 248) %>%
  #BUG ID -49
  filter(REGULATION_ID != 249) %>%
  #BUG ID -50
  filter(REGULATION_ID != 250) %>%
  #BUG ID -51
  filter(REGULATION_ID != 970) %>%
  #BUG ID -52
  filter(REGULATION_ID != 971) %>%
  #BUG ID -53
  filter(REGULATION_ID != 972) %>%
  #BUG ID -54
  filter(REGULATION_ID != 973)

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
         # Get data type as integer
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

#Bug ID 5316 - Update MCat:MType to Other:Ignorable for record related to correction
mh_cleaned <- mh_cleaned %>%
  mutate(FR_SECTION = case_when(REGULATION_ID == 298 ~ '50 CFR 622',
                                TRUE ~ FR_SECTION),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 298 ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 298 ~ 'IGNORABLE',
                                     TRUE ~ MANAGEMENT_TYPE),
         MANAGEMENT_STATUS = case_when(REGULATION_ID != 298 ~ MANAGEMENT_STATUS),
         START_DAY = case_when(REGULATION_ID == 298 ~ 26L,
                               TRUE ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID == 298 ~ 5L,
                                 TRUE ~ START_MONTH),
         START_YEAR = case_when(REGULATION_ID == 298 ~ 2010L,
                                TRUE ~ START_YEAR),
         END_DAY = case_when(REGULATION_ID != 298 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 298 ~ END_MONTH))

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

#Bug ID 4694 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1687 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4693 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1575 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4690 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1564 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4687 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1565 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4686 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1570 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4685 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1569 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4679 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 2269 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4678 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 2270 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4677 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 2272 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4676 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 2271 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4546 - update End day and End time for 51 FR 23551 following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(FR_CITATION == '51 FR 23551' ~ 23L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(FR_CITATION == '51 FR 23551' ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4824 - Management Category and Management Type updated from Universal: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1573 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1573 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4823 - Management Category and Management Type updated from Universal: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1574 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1574 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4822 - Management Category and Management Type updated from Universal: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1689 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1689 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4807 - Management Category and Management Type updated from Universal: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1579 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1579 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4806 - Management Category and Management Type updated from Universal: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1697 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1697 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 5077 - Management Category and Management Type updated from Effort Limits: Trip Limit to Effort Limits: Trip Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1146 ~ 'EFFORT LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1146 ~ 'TRIP LIMIT ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 5946 - Management Category and Management Type updated from Effort Limits: Bag Limit to Effort Limits: Bag Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 918 ~ 'EFFORT LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 918 ~ 'BAG LIMIT ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 5536 - Management Category and Management Type updated from Catch Limits: Quota to Catch Limits: Quota Adjustment 
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 436 ~ 'CATCH LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 436 ~ 'QUOTA ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 5537 - Management Category and Management Type updated from Catch Limits: Quota to Catch Limits: Quota Adjustment and adds NO for the flag since the field is blank
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 5472 ~ 'CATCH LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 5472 ~ 'QUOTA ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE),
         FLAG = case_when(REGULATION_ID == 5472 ~ 'NO',
                          TRUE ~ FLAG))

#Bug ID 5906 - Management Category and Management Type updated from Catch Limits: Quota to Catch Limits: Quota Adjustment, start month, start day, and end date information removed following Catch Limit guidance
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 11446 ~ 'CATCH LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 11446 ~ 'QUOTA ADJUSTMENT', 
                                     TRUE ~ MANAGEMENT_TYPE),
         START_DAY = case_when(REGULATION_ID != 11446 ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID != 11446 ~ START_MONTH),
         END_DAY = case_when(REGULATION_ID != 11446 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 11446 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 11446 ~ END_YEAR))
 
#Bug ID 5907 - Management Category and Management Type updated from Catch Limits: Quota to Catch Limits: Quota Adjustment, start month, start day, and end date information removed following Catch Limit guidance
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 11445 ~ 'CATCH LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 11445 ~ 'QUOTA ADJUSTMENT', 
                                     TRUE ~ MANAGEMENT_TYPE),
         START_DAY = case_when(REGULATION_ID != 11445 ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID != 11445 ~ START_MONTH),
         END_DAY = case_when(REGULATION_ID != 11445 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 11445 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 11445 ~ END_YEAR))
 
#Bug 5912 - Management Category and Management Type updated to Catch Limits: Quota Adjustment, start month, start day, and end date information removed following Catch Limit guidance
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 811 ~ 'CATCH LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 811 ~ 'QUOTA ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE),
         START_DAY = case_when(REGULATION_ID != 811 ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID != 811 ~ START_MONTH),
         END_DAY = case_when(REGULATION_ID != 811 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 811 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 811 ~ END_YEAR))

#Bug ID -17 - Management Category and Management Type updated to Catch Limits: Quota Adjustment, start month, start day, and end date information removed following Catch Limit guidance
mh_cleaned <- mh_cleaned %>% 
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 808 ~ 'CATCH LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 808 ~ 'QUOTA ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE),
         START_DAY = case_when(REGULATION_ID != 808 ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID != 808 ~ START_MONTH),
         END_DAY = case_when(REGULATION_ID != 808 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 808 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 808 ~ END_YEAR))
       
#Bug ID 6046 - Start Month and Start Day should be removed following guidance related to Catch Limit Management Types
mh_cleaned <- mh_cleaned %>%
  mutate(START_DAY = case_when(REGULATION_ID != 1105 ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID != 1105 ~ START_MONTH))

#Bug ID 5136 - Updates sector information from ALL: ALL to C: ALL
mh_cleaned <- mh_cleaned %>%
  mutate(SECTOR = case_when(REGULATION_ID == 1109 ~ 'COMMERCIAL',
                            TRUE ~ SECTOR),
         SUBSECTOR = case_when(REGULATION_ID == 1109 ~ 'ALL',
                               TRUE ~ SUBSECTOR))

#Bug ID 4456 - change accountability measure to NO for 74 FR 17603
mh_cleaned <- mh_cleaned %>%
  mutate(ACCOUNTABILITY_MEASURE = case_when(FR_CITATION == '74 FR 17603' ~ 'NO',
                                            TRUE ~ ACCOUNTABILITY_MEASURE))

#BUG ID 4592 - change flag to NO b/c flag is captured thru addition of incidental catch record addition in bug id 4588
mh_cleaned <- mh_cleaned %>%
  mutate(FLAG = case_when(REGULATION_ID == 1042 ~ 'NO',
                          TRUE ~ FLAG))

#Bug ID 4463 - Update flag to NO for Incidental Catch Limit
mh_cleaned <- mh_cleaned %>%
  mutate(FLAG = case_when(REGULATION_ID == 1042 ~ 'NO',
                          TRUE ~ FLAG))

#Bug ID 4464 - Update CFR section to the general 50 CFR 641 - Duplicate Other: FMP ESTABLISHMENT records were removed with other records above
mh_cleaned <- mh_cleaned %>%
  mutate(FR_SECTION = case_when(REGULATION_ID == 1221 ~ '50 CFR 641',
                                TRUE ~ FR_SECTION))

#BUG ID 4726 - update region from ALL to GULF OF MEXICO
mh_cleaned <- mh_cleaned %>%
  mutate(REGION = case_when(REGULATION_ID == 1341 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID -1 - Correct missing mng. type/sector/region for reg id 5474
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 5474 ~ 'BAG LIMIT ADJUSTMENT',
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

#BUG ID -3 and -16 - Correct missing mng. type/sector/region for reg id 5472, Bug ID -16 updates the Management Type to Quota Adjustment and removes the start month and day information following guidance for Catch Limit records
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 5472 ~ 'QUOTA ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 5472 ~ 'COMMERCIAL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 5472 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION),
         START_DAY = case_when(REGULATION_ID != 5472 ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID != 5472 ~ START_MONTH))

#BUG ID -4 and -15- Correct missing mng. type/sector/region for reg id 5471, Bug ID -15 updates the Management Type to Quota Adjustment and removes the start month, day, and end date information for the record since it is a Catch Limit record
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 5471 ~ 'QUOTA ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 5471 ~ 'RECREATIONAL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 5471 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION),
         START_DAY = case_when(REGULATION_ID != 5471 ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID != 5471 ~ START_MONTH),
         END_DAY = case_when(REGULATION_ID != 5471 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 5471 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 5471 ~ END_YEAR))

#BUG ID -5 - Correct missing mng. type/sector/region for reg id 5427
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 5427 ~ 'MINIMUM SIZE LIMIT',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 5427 ~ 'COMMERCIAL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 5427 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID -6 - Correct missing mng. type/sector/region for reg id 2272 - Management Category: Management Type should be Universal: Fishing Season
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2272 ~ 'UNIVERSAL',
                                         TRUE ~ MANAGEMENT_CATEGORY),
        MANAGEMENT_TYPE = case_when(REGULATION_ID == 2272 ~ 'FISHING SEASON',
                                     TRUE ~ MANAGEMENT_TYPE),
         SECTOR = case_when(REGULATION_ID == 2272 ~ 'ALL',
                            TRUE ~ SECTOR),
         REGION = case_when(REGULATION_ID == 2272 ~ 'CARIBBEAN',
                            TRUE ~ REGION))

#BUG ID -7 - Correct missing mng. type/sector/region for reg id 2197
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 2197 ~ 'FISHING SEASON',
                                     TRUE ~ MANAGEMENT_TYPE),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2197 ~ 'UNIVERSAL',
                                         TRUE ~ MANAGEMENT_CATEGORY),
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

#Bug ID 5426 - Update flag to YES to indicate the change to the species included in the grouper aggregate
mh_cleaned <- mh_cleaned %>%
  mutate(FLAG = case_when(REGULATION_ID == 931 ~ 'YES',
                          TRUE ~ FLAG))

#Bug ID 5920 - Start Month, Day, and End Date information should be removed for Catch Limit records since it is not explicitly stated
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 807 ~ "QUOTA ADJUSTMENT",
                                     TRUE ~ MANAGEMENT_TYPE), 
         START_DAY = case_when(REGULATION_ID != 807 ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID != 807 ~ START_MONTH),
         END_DAY = case_when(REGULATION_ID != 807 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 807 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 807 ~ END_YEAR))

#Bug ID 5921 - Start Month, Day, and End Date information should be removed for Catch Limit records since it is not explicitly stated 
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 434 ~ "QUOTA ADJUSTMENT",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         START_DAY = case_when(REGULATION_ID != 434 ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID != 434 ~ START_MONTH),
         END_DAY = case_when(REGULATION_ID != 434 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 434 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 434 ~ END_YEAR))

#Bug ID 5936 - Start Month, Day, and End Date information should be removed for Catch Limit records since it is not explicitly stated 
mh_cleaned <- mh_cleaned %>%
  mutate(START_DAY = case_when(REGULATION_ID != 436 ~ START_DAY),
         START_MONTH = case_when(REGULATION_ID != 436 ~ START_MONTH),
         END_DAY = case_when(REGULATION_ID != 436 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 436 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 436 ~ END_YEAR))

#Bug ID 6026 - update DWH part 1 - zone should be GOM:Area closure related to Deepwater Horizon oil spill, remove amendment and amendment number
mh_cleaned <- mh_cleaned %>%
  mutate(ZONE = case_when(REGULATION_ID == 3068 ~ "AREA CLOSURE RELATED TO DEEPWATER HORIZON OIL SPILL",
                          TRUE ~ ZONE),
         ACTION_TYPE = case_when(REGULATION_ID == 3068 ~ " ",
                                 TRUE ~ ACTION_TYPE),
         AMENDMENT_NUMBER = case_when(REGULATION_ID == 3068 ~ " ",
                                      TRUE ~ AMENDMENT_NUMBER))

#Bug ID 6027 - update DWH part 2 - zone should be SATL:Area closure related to Deepwater Horizon oil spill, remove amendment and amendment number
mh_cleaned <- mh_cleaned %>%
  mutate(ZONE = case_when(REGULATION_ID == 3067 ~ "AREA CLOSURE RELATED TO DEEPWATER HORIZON OIL SPILL",
                          TRUE ~ ZONE),
         ACTION_TYPE = case_when(REGULATION_ID == 3067 ~ " ",
                                 TRUE ~ ACTION_TYPE),
         AMENDMENT_NUMBER = case_when(REGULATION_ID == 3067 ~ " ",
                                      TRUE ~ AMENDMENT_NUMBER))

#Bug ID 6028 - update DWH part 3 - zone should be CAR:Area closure related to Deepwater Horizon oil spill, remove amendment and amendment number
mh_cleaned <- mh_cleaned %>% 
  mutate(ZONE = case_when(REGULATION_ID == 2191 ~ "AREA CLOSURE RELATED TO DEEPWATER HORIZON OIL SPILL",
                          TRUE ~ ZONE),
         ACTION_TYPE = case_when(REGULATION_ID == 2191 ~ " ",
                                 TRUE ~ ACTION_TYPE),
         AMENDMENT_NUMBER = case_when(REGULATION_ID == 2191 ~ " ",
                                      TRUE ~ AMENDMENT_NUMBER))

#Bug ID 6036 - update DWH part 4 - remove amendment and amendment number, change flag to yes
mh_cleaned <- mh_cleaned %>% 
  mutate(ACTION_TYPE = case_when(REGULATION_ID == 11346 ~ " ",
                                 TRUE ~ ACTION_TYPE),
         AMENDMENT_NUMBER = case_when(REGULATION_ID == 11346 ~ " ",
                                      TRUE ~ AMENDMENT_NUMBER),
         FLAG = case_when(REGULATION_ID == 11346 ~ "YES",
                          TRUE ~ FLAG))

#Bug ID 6057 - update Management Category and Management Type due to error during reclassification process
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1356 ~ "TEMPORAL CONTROLS",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1356 ~ "CLOSURE",
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6058 - update Management Category due to error during the reclassification process
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 3383 ~ "UNIVERSAL",
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Bug ID 6059 - update Management Category due to error during the reclassification process 
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 3254 ~ "UNIVERSAL",
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Bug ID 6076 - update Management Category to Other for Definition records that did not reclassify correctly
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4822 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4823 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4831 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4832 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2242 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1241 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2325 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 5220 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2583 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2585 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2588 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2581 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2263 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4238 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4281 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4284 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1210 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4663 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4612 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4126 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4124 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4130 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4122 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 3993 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4009 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4014 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4019 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4020 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4022 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4028 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4039 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4041 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4049 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4053 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4066 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4067 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4070 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4071 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4077 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4080 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4081 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4082 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4083 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2264 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1212 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1211 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1072 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 4319 ~ "OTHER",
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Bug IDs 6086, 6096 - Removing Start month and day information for Catch Limit records
mh_cleaned <- mh_cleaned %>%
  mutate(START_MONTH = case_when(!REGULATION_ID %in% c(1104, 1344, 234, 470, 240, 237, 667, 595, 592, 476, 473, 670, 964, 1343) ~ START_MONTH),
         START_DAY = case_when(!REGULATION_ID %in% c(1104, 1344, 234, 470, 240, 237, 667, 59, 592, 476, 473, 670, 964, 1343) ~ START_DAY))

#Bug ID 6087 - Removing start month, start day, and end date for Catch Limit records
mh_cleaned <- mh_cleaned %>%
  mutate(START_MONTH = case_when(!REGULATION_ID %in% c(11447, 11382, 11381, 11380, 451, 574, 641, 649, 639, 281, 222, 572, 221, 638, 571, 280) ~ START_MONTH),
         START_DAY = case_when(!REGULATION_ID %in% c(11447, 11382, 11381, 11380, 451, 574, 641, 649, 639, 281, 222, 572, 221, 638, 571, 280) ~ START_DAY),
         END_DAY = case_when(!REGULATION_ID %in% c(11447, 11382, 11381, 11380, 451, 574, 641, 649, 639, 281, 222, 572, 221, 638, 571, 280) ~ END_DAY),
         END_MONTH = case_when(!REGULATION_ID %in% c(11447, 11382, 11381, 11380, 451, 574, 641, 649, 639, 281, 222, 572, 221, 638, 571, 280) ~ END_MONTH),
         END_YEAR = case_when(!REGULATION_ID %in% c(11447, 11382, 11381, 11380, 451, 574, 641, 649, 639, 281, 222, 572, 221, 638, 571, 280) ~ END_YEAR))

#Bug IDs 6116 - Removing start month, day, time, and end date information for Catch Limit records
mh_cleaned <- mh_cleaned %>%
  mutate(START_MONTH = case_when(!REGULATION_ID %in% c(11433, 11432, 11390, 11391, 11409, 11410, 11384, 11385) ~ START_MONTH),
         START_DAY = case_when(!REGULATION_ID %in% c(11433, 11432, 11390, 11391, 11409, 11410, 11384, 11385) ~ START_DAY),
         START_TIME = case_when(!REGULATION_ID %in% c(11433, 11432, 11390, 11391, 11409, 11410, 11384, 11385) ~ START_TIME),
         END_DAY = case_when(!REGULATION_ID %in% c(11433, 11432, 11390, 11391, 11409, 11410, 11384, 11385) ~ END_DAY),
         END_MONTH = case_when(!REGULATION_ID %in% c(11433, 11432, 11390, 11391, 11409, 11410, 11384, 11385) ~ END_MONTH),
         END_YEAR = case_when(!REGULATION_ID %in% c(11433, 11432, 11390, 11391, 11409, 11410, 11384, 11385) ~ END_YEAR),
         END_TIME = case_when(!REGULATION_ID %in% c(11433, 11432, 11390, 11391, 11409, 11410, 11384, 11385) ~ END_TIME))

#Bug ID 6186  - Update Management Type to Quota Adjustment and remove start month, start day, and end date information for Catch Limit records
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID %in% c(441, 440, 452, 453, 439, 812, 442, 433, 443, 813) ~ "QUOTA ADJUSTMENT",
                                     TRUE ~ MANAGEMENT_TYPE),
         START_MONTH = case_when(!REGULATION_ID %in% c(441, 440, 452, 453, 439, 812, 442, 433, 443, 813) ~ START_MONTH),
         START_DAY = case_when(!REGULATION_ID %in% c(441, 440, 452, 453, 439, 812, 442, 433, 443, 813) ~ START_DAY),
         END_DAY = case_when(!REGULATION_ID %in% c(441, 440, 452, 453, 439, 812, 442, 433, 443, 813) ~ END_DAY),
         END_MONTH = case_when(!REGULATION_ID %in% c(441, 440, 452, 453, 439, 812, 442, 433, 443, 813) ~ END_MONTH),
         END_YEAR = case_when(!REGULATION_ID %in% c(441, 440, 452, 453, 439, 812, 442, 433, 443, 813) ~ END_YEAR))

#Bug ID -18 - Remove start month, start day, and end date information for Catch Limit records and update flag to YES
mh_cleaned <- mh_cleaned %>%
  mutate(FLAG = case_when(REGULATION_ID == 458 ~ "YES",
                                     TRUE ~ FLAG),
         START_MONTH = case_when(REGULATION_ID != 458 ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID != 458 ~ START_DAY),
         END_DAY = case_when(REGULATION_ID != 458 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 458 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 458 ~ END_YEAR)) 

#Bug ID 6306 - Management Type should be updated to ACL Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID %in% c(11432, 11390, 11409, 11381, 11384, 633, 635, 634, 624, 626, 625, 574, 222, 572, 221, 571, 225) ~ "ACL ADJUSTMENT",
                                         TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6336 - Management Type should be Bag Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 941 ~ "BAG LIMIT ADJUSTMENT",
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6346 - Management Type should be Quota Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 447 ~ "QUOTA ADJUSTMENT",
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6347 - Management Type should be Quota Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 448 ~ "QUOTA ADJUSTMENT",
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6356 - Management Type should be Quota Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 494 ~ "QUOTA ADJUSTMENT",
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6366 - Management Type should be Quota Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 451 ~ "QUOTA ADJUSTMENT",
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6447 - Management Category should be updated to Catch Limits
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1321 ~ "CATCH LIMITS",
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Bug ID 6516 - Management Type should be updated to Trip Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 1435 ~ 'TRIP LIMIT ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6517 - Management Type should be updated to Trip Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 1431 ~ 'TRIP LIMIT ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6518 - Management Type should be updated to Trip Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 1434 ~ 'TRIP LIMIT ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6519 - Management Type should be updated to Trip Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 1436 ~ 'TRIP LIMIT ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6520 - Management Type should be updated to Trip Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 1437 ~ 'TRIP LIMIT ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 6618 - Start Year removed for Fishing Year record
mh_cleaned <- mh_cleaned %>%
  mutate(START_YEAR = case_when(REGULATION_ID != 786 ~ START_YEAR))

#BUG ID -19 - start yr needs to be removed from seasonal quota
mh_cleaned <- mh_cleaned %>%
  mutate(START_YEAR = case_when(REGULATION_ID != 1385 ~ START_YEAR))

#BUG ID -20 - start yr needs to be removed from seasonal quota
mh_cleaned <- mh_cleaned %>%
  mutate(START_YEAR = case_when(REGULATION_ID != 1387 ~ START_YEAR))

#BUG ID -21 - start yr needs to be removed from seasonal quota
mh_cleaned <- mh_cleaned %>%
  mutate(START_YEAR = case_when(REGULATION_ID != 1389 ~ START_YEAR))

#BUG ID -22 - start month and day needs to be removed from yearly quota FOR 2009
mh_cleaned <- mh_cleaned %>%
  mutate(START_MONTH = case_when(REGULATION_ID != 2003 ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID != 2003 ~ START_DAY))

#BUG ID -23 - start month and day needs to be removed from yearly quota FOR 1994
mh_cleaned <- mh_cleaned %>%
  mutate(START_MONTH = case_when(REGULATION_ID != 2009 ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID != 2009 ~ START_DAY))

#BUG ID -24 - start month and day needs to be removed from yearly quota FOR 2006
mh_cleaned <- mh_cleaned %>%
  mutate(START_MONTH = case_when(REGULATION_ID != 2017 ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID != 2017 ~ START_DAY))

#BUG ID -25 - start month and day needs to be removed from yearly quota
mh_cleaned <- mh_cleaned %>%
  mutate(START_MONTH = case_when(REGULATION_ID != 2002 ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID != 2002 ~ START_DAY))

#BUG ID -26 - start month and day needs to be removed from yearly quota
mh_cleaned <- mh_cleaned %>%
  mutate(START_MONTH = case_when(REGULATION_ID != 2012 ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID != 2012 ~ START_DAY))

#BUG ID -27 - start month and day needs to be removed from yearly quota
mh_cleaned <- mh_cleaned %>%
  mutate(START_MONTH = case_when(REGULATION_ID != 2006 ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID != 2006 ~ START_DAY))

#Bug ID -29 - Start time removed since it was incorrectly entered
mh_cleaned <- mh_cleaned %>% 
  mutate(START_TIME = case_when(REGULATION_ID != 385 ~ START_TIME))

#Bug -33 - Update Ineffective Date to reflect the end of the fishing year and update the Manaagement Status to be ONCE
mh_cleaned <- mh_cleaned %>%
  mutate(INEFFECTIVE_DATE = case_when(REGULATION_ID == 778 ~ "12/31/2010",
                                      TRUE ~ INEFFECTIVE_DATE),
         MANAGEMENT_STATUS = case_when(REGULATION_ID == 778 ~ "ONCE",
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug -34 - Update Ineffective Date to reflect the end of the fishing year and update the Manaagement Status to be ONCE
mh_cleaned <- mh_cleaned %>%
  mutate(INEFFECTIVE_DATE = case_when(REGULATION_ID == 753 ~ "12/31/2012",
                                      TRUE ~ INEFFECTIVE_DATE),
         MANAGEMENT_STATUS = case_when(REGULATION_ID == 753 ~ "ONCE",
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug -35 - Update Ineffective Date to reflect the end of the fishing year, remove the end date information
mh_cleaned <- mh_cleaned %>%
  mutate(INEFFECTIVE_DATE = case_when(REGULATION_ID == 712 ~ "12/31/2013",
                                      TRUE ~ INEFFECTIVE_DATE),
         END_DAY = case_when(REGULATION_ID != 712 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 712 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 712 ~ END_YEAR))

#Bug -36 - Update Ineffective Date to reflect the end of the fishing year, remove the end date information
mh_cleaned <- mh_cleaned %>%
  mutate(INEFFECTIVE_DATE = case_when(REGULATION_ID == 711 ~ "12/31/2013",
                                      TRUE ~ INEFFECTIVE_DATE),
         END_DAY = case_when(REGULATION_ID != 711 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 711 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 711 ~ END_YEAR))

#Bug -37 - Update Ineffective Date to reflect the end of the fishing year, remove the end date information
mh_cleaned <- mh_cleaned %>%
  mutate(INEFFECTIVE_DATE = case_when(REGULATION_ID == 710 ~ "12/31/2013",
                                      TRUE ~ INEFFECTIVE_DATE),
         END_DAY = case_when(REGULATION_ID != 710 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 710 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 710 ~ END_YEAR))

#Bug -38 - Update Ineffective Date to reflect the end of the fishing year, remove the end date information
mh_cleaned <- mh_cleaned %>%
  mutate(INEFFECTIVE_DATE = case_when(REGULATION_ID == 709 ~ "12/31/2013",
                                      TRUE ~ INEFFECTIVE_DATE),
         END_DAY = case_when(REGULATION_ID != 709 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 709 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 709 ~ END_YEAR))

#Bug -39 - Update Ineffective Date to reflect the end of the fishing year, remove the end date information
mh_cleaned <- mh_cleaned %>%
  mutate(INEFFECTIVE_DATE = case_when(REGULATION_ID == 708 ~ "12/31/2013",
                                      TRUE ~ INEFFECTIVE_DATE),
         END_DAY = case_when(REGULATION_ID != 708 ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID != 708 ~ END_MONTH),
         END_YEAR = case_when(REGULATION_ID != 708 ~ END_YEAR))

#Bug ID -40 - Update Ineffective Date to reflect the end of the fishing year and update the Manaagement Status to be ONCE
mh_cleaned <- mh_cleaned %>%
  mutate(INEFFECTIVE_DATE = case_when(REGULATION_ID == 773 ~ "12/31/2015",
                                      TRUE ~ INEFFECTIVE_DATE),
         MANAGEMENT_STATUS = case_when(REGULATION_ID == 773 ~ "ONCE",
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID -41 - Update Ineffective Date to reflect the end of the fishing year and update the Manaagement Status to be ONCE
mh_cleaned <- mh_cleaned %>%
  mutate(INEFFECTIVE_DATE = case_when(REGULATION_ID == 774 ~ "12/31/2015",
                                      TRUE ~ INEFFECTIVE_DATE),
         MANAGEMENT_STATUS = case_when(REGULATION_ID == 774 ~ "ONCE",
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID -42 - Update Ineffective Date to reflect the end of the fishing year 
mh_cleaned <- mh_cleaned %>%
  mutate(INEFFECTIVE_DATE = case_when(REGULATION_ID == 178 ~ "12/31/2020",
                                      TRUE ~ INEFFECTIVE_DATE),
         MANAGEMENT_STATUS = case_when(REGULATION_ID == 178 ~ "ONCE",
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID -55 - Flag should be YES for Permit Moratorium records
mh_cleaned <- mh_cleaned %>%
  mutate(FLAG = case_when(REGULATION_ID == 423 ~ "YES",
                          TRUE ~ FLAG))

#Bug ID -56 - Flag should be YES for VMS Related records
mh_cleaned <- mh_cleaned %>%
  mutate(FLAG = case_when(REGULATION_ID == 11456 ~ "YES",
                          TRUE ~ FLAG))

#Bug ID -57 - Flag should be YES for VMS Related records
mh_cleaned <- mh_cleaned %>%
  mutate(FLAG = case_when(REGULATION_ID == 11457 ~ "YES",
                          TRUE ~ FLAG))

#Bug ID -62 - Management Type should be updated to Possession Limit
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 2180 ~ "POSSESSION LIMIT",
                                     TRUE ~ MANAGEMENT_TYPE))
#Bug ID -63 - Management Type should be updated to Possession Limit
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 2181 ~ "POSSESSION LIMIT",
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID -64 - Amendment Number should be 4
mh_cleaned <- mh_cleaned %>%
  mutate(AMENDMENT_NUMBER = case_when(REGULATION_ID == 1592 ~ "4",
                                      TRUE ~ AMENDMENT_NUMBER))

#Bug ID -65 - Management Type should be updated to Minimum Size Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_TYPE = case_when(REGULATION_ID == 792 ~ "MINIMUM SIZE LIMIT ADJUSTMENT",
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID -66 - Start time should be removed since FR Notice stated 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(START_TIME = case_when(REGULATION_ID != 405 ~ START_TIME))

#Bug ID -67 - Start time should be removed since FR Notice stated 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(START_TIME = case_when(REGULATION_ID != 404 ~ START_TIME))

#Bug ID -68 - Start time should be removed since FR Notice stated 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(START_TIME = case_when(REGULATION_ID != 2597 ~ START_TIME))

#Bug ID -69 - Start time should be removed since FR Notice stated 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(START_TIME = case_when(REGULATION_ID != 2598 ~ START_TIME))

#Bug ID -70 - Start time should be removed since FR Notice stated 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(START_TIME = case_when(REGULATION_ID != 407 ~ START_TIME))

#Bug ID -71 - Start time should be removed since FR Notice stated 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(START_TIME = case_when(REGULATION_ID != 165 ~ START_TIME))

#Create empty data frame
mh_added = mh_cleaned %>% filter(is.na(REGULATION_ID))

# Example new record
# mh_added %>% add_row(REGULATION_ID = -,
#                      LAST_UPDATED	= "",
#                      JURISDICTION 	= "",
#                      ACTION = "",
#                      ACTION_TYPE	= "",
#                      AMENDMENT_NUMBER	= "",
#                      ACCOUNTABILITY_MEASURE	= "",
#                      FR_CITATION	= "",
#                      FR_SECTION	= "",
#                      FR_URL	= "",
#                      MANAGEMENT_CATEGORY	= "",
#                      MANAGEMENT_TYPE	= "",
#                      SECTOR	= "",
#                      SUBSECTOR	= "",
#                      REGION	= "",
#                      ZONE	= "",
#                      JURISDICTIONAL_WATERS	= "",
#                      COMMON_NAME	= "",
#                      SPECIES_ITIS	= ,
#                      FMP	= "",
#                      SPECIES_AGGREGATE	= "",
#                      SPECIES_GROUP	= "",
#                      MANAGEMENT_STATUS	= "",
#                      EFFECTIVE_DATE	= "",
#                      INEFFECTIVE_DATE	= "",
#                      START_DAY	= ,
#                      START_MONTH	= ,
#                      START_YEAR	= ,
#                      START_TIME	= "",
#                      START_DAY_OF_WEEK	= "",
#                      END_DAY	= ,
#                      END_MONTH	= ,
#                      END_YEAR	= ,
#                      END_TIME	= "",
#                      END_DAY_OF_WEEK	= "",
#                      VALUE	= "",
#                      VALUE_UNITS	= "",
#                      VALUE_TYPE	= "",
#                      VALUE_RATE	= "",
#                      FLAG = "")


#Bug ID 4966 - Add record for Other: State Related
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -999,
                     LAST_UPDATED = "09/30/2021",
                     JURISDICTION = "FEDERAL",
                     ACTION = "FINAL",
                     ACTION_TYPE = "AMENDMENT",
                     AMENDMENT_NUMBER = "30B",
                     ACCOUNTABILITY_MEASURE = "YES",
                     FR_CITATION = "74 FR 17603",
                     FR_SECTION = "50 CFR 622.4",
                     FR_URL = "https://www.federalregister.gov/documents/2009/04/16/E9-8764/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-reef-fish-fishery-of-the-gulf-of-mexico",
                     MANAGEMENT_CATEGORY = "OTHER",
                     MANAGEMENT_TYPE = "STATE RELATED",
                     SECTOR = "COMMERCIAL",
                     SUBSECTOR = "ALL",
                     REGION = "GULF OF MEXICO",
                     ZONE = "ALL",
                     JURISDICTIONAL_WATERS = "EEZ",
                     COMMON_NAME = "ALL",
                     FMP = "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                     EFFECTIVE_DATE = "05/18/2009",
                     START_DAY = 18,
                     START_MONTH = 5,
                     START_YEAR = 2009,
                     FLAG = "YES")

#BUG ID 4588 - add catch limits:incidental catch record for 5 red snapper under size limit,
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -998,
          LAST_UPDATED	= "10/01/2021",
          JURISDICTION 	= "FEDERAL",
          ACTION = "FINAL",
          ACTION_TYPE	= "ORIGINAL FMP",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "49 FR 39548",
          FR_SECTION	= "50 FR 641.23",
          FR_URL	= "https://www.govinfo.gov/app/details/FR-1984-10-09",
          MANAGEMENT_CATEGORY	= "CATCH LIMITS",
          MANAGEMENT_TYPE	= "INCIDENTAL CATCH LIMIT",
          SECTOR	= "ALL",
          SUBSECTOR	= "ALL",
          REGION	= "GULF OF MEXICO",
          ZONE	= "ALL",
          JURISDICTIONAL_WATERS	= "EEZ",
          COMMON_NAME	= "SNAPPER, RED",
          SPECIES_ITIS	= "168853",
          FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
          EFFECTIVE_DATE	= "11/08/1984",
          START_DAY	= 8,
          START_MONTH	= 11,
          START_YEAR	= 1984,
          VALUE	= "5",
          VALUE_UNITS	= "FISH",
          FLAG = "YES")

#BUG ID 4586 - add rows for other:state related for gag, red snapper, greater amberjack, and gray triggerfish
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -997,
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
          SPECIES_ITIS	= "167759",
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

mh_added <- mh_added %>%
  add_row(REGULATION_ID = -996,
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
          SPECIES_ITIS	= "168853",
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

mh_added <- mh_added %>%
  add_row(REGULATION_ID = -995,
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
          SPECIES_ITIS	= "168689",
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

mh_added <- mh_added %>%
  add_row(REGULATION_ID = -994,
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
          SPECIES_ITIS	= "173138",
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

#BUG ID 5116 and -14 - Add ITQ program est. record to 61 FR 7751, updated Management Category, Management Status, and start date information based on Bug ID -14
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -993,
          LAST_UPDATED	= "10/01/2021",
          JURISDICTION 	= "FEDERAL",
          ACTION = "EMERGENCY INTERIM RULE",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "61 FR 7751",
          FR_SECTION	= "50 CFR 641.10",
          FR_URL	= "https://www.federalregister.gov/documents/1996/02/29/96-4432/reef-fish-fishery-of-the-gulf-of-mexico-revised-1996-red-snapper-season",
          MANAGEMENT_CATEGORY	= "UNIVERSAL",
          MANAGEMENT_TYPE	= "ITQ PROGRAM ESTABLISHED",
          SECTOR	= "COMMERCIAL",
          SUBSECTOR	= "ALL",
          REGION	= "GULF OF MEXICO",
          ZONE	= "ALL",
          JURISDICTIONAL_WATERS	= "EEZ",
          COMMON_NAME	= "SNAPPER, RED",
          SPECIES_ITIS	= "168853",
          FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
          MANAGEMENT_STATUS	= "DELAYED",
          EFFECTIVE_DATE	= "02/23/1996",
          INEFFECTIVE_DATE	= "05/29/1996",
          END_DAY	= 29,
          END_MONTH	= 5,
          END_YEAR	= 1996,
          FLAG = "YES")

#BUG ID 5376 - Add Bag Limit record for 64 FR 57403
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -992,
          LAST_UPDATED	= "01/12/2022",
          JURISDICTION 	= "FEDERAL",
          ACTION = "FINAL",
          ACTION_TYPE = "AMENDMENT",
          AMENDMENT_NUMBER = "16B",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "64 FR 57403",
          FR_SECTION	= "50 CFR 622.39",
          FR_URL	= "https://www.federalregister.gov/documents/1999/10/25/99-27584/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-reef-fish-fishery-of-the-gulf-of-mexico",
          MANAGEMENT_CATEGORY	= "EFFORT LIMITS",
          MANAGEMENT_TYPE	= "BAG LIMIT",
          SECTOR	= "RECREATIONAL",
          SUBSECTOR	= "ALL",
          REGION	= "GULF OF MEXICO",
          ZONE	= "ALL",
          JURISDICTIONAL_WATERS	= "EEZ",
          SPECIES_AGGREGATE = "BAG LIMIT: GROUPERS, COMBINED",
          FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
          EFFECTIVE_DATE = "11/24/1999",
          START_DAY	= 24,
          START_MONTH	=11,
          START_YEAR	= 1999,
          VALUE = "5",
          VALUE_UNITS = "FISH",
          VALUE_TYPE = "COUNT",
          VALUE_RATE = "PER PERSON PER DAY",
          FLAG = "YES")

#BUG ID 5566 - Add Gear Requirements: Trap Specifications record for 48 FR 39463
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -991,
          LAST_UPDATED	= "01/12/2022",
          JURISDICTION 	= "FEDERAL",
          ACTION = "FINAL",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "48 FR 39463",
          FR_SECTION	= "50 CFR 646.22",
          FR_URL	= "https://www.loc.gov/item/fr048170/",
          MANAGEMENT_CATEGORY	= "GEAR REQUIREMENTS",
          MANAGEMENT_TYPE	= "TRAP SPECIFICATIONS",
          SECTOR	= "COMMERCIAL",
          SUBSECTOR	= "FISH TRAPS",
          REGION	= "SOUTH ATLANTIC",
          ZONE	= "ALL",
          JURISDICTIONAL_WATERS	= "EEZ",
          COMMON_NAME	= "ALL",
          FMP	= "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION",
          EFFECTIVE_DATE	= "09/28/1983",
          START_DAY	= 28,
          START_MONTH	= 9,
          START_YEAR	= 1984,
          FLAG = "YES")

#BUG ID 5976 - add other: permit limits and endorsements for fish trap permits,
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -990,
          LAST_UPDATED	= "2/10/2022",
          JURISDICTION 	= "FEDERAL",
          ACTION = "FINAL",
          ACTION_TYPE	= "ORIGINAL FMP",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "49 FR 39548",
          FR_SECTION	= "50 FR 641.4",
          FR_URL	= "https://www.govinfo.gov/app/details/FR-1984-10-09",
          MANAGEMENT_CATEGORY	= "OTHER",
          MANAGEMENT_TYPE	= "PERMIT REQUIREMENTS AND ENDORSEMENTS",
          SECTOR	= "ALL",
          SUBSECTOR	= "FISH TRAPS",
          REGION	= "GULF OF MEXICO",
          ZONE	= "ALL",
          JURISDICTIONAL_WATERS	= "EEZ",
          COMMON_NAME	= "ALL",
          FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
          EFFECTIVE_DATE	= "11/08/1984",
          START_DAY	= 8,
          START_MONTH	= 11,
          START_YEAR	= 1984,
          FLAG = "YES")

#Bug ID 5990 - Add record to capture extension of seasonal may 1 - oct 31 regulation 
mh_added <- mh_added %>%  
  add_row(REGULATION_ID = -989,
          LAST_UPDATED = "2/10/2022",
          JURISDICTION = "FEDERAL",
          ACTION = "FINAL",
          ACTION_TYPE = "AMENDMENT",
          AMENDMENT_NUMBER = "30B",
          FR_CITATION = "74 FR 17603",
          FR_SECTION = "50 CFR 622.34",
          FR_URL = "https://www.federalregister.gov/documents/2009/04/16/E9-8764/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-reef-fish-fishery-of-the-gulf-of-mexico",
          MANAGEMENT_CATEGORY = "EFFORT LIMITS",
          MANAGEMENT_TYPE = "HARVEST METHOD",
          SECTOR = "ALL",
          SUBSECTOR = "ALL",
          REGION = "GULF OF MEXICO",
          ZONE = "MADISON-SWANSON SITES AND STEAMBOAT LUMPS",
          JURISDICTIONAL_WATERS = "EEZ",
          COMMON_NAME = "ALL",
          FMP = "REEF FISH RESOURCES OF THE GULF OF MEXICO",
          EFFECTIVE_DATE = "05/18/2009",
          START_DAY = 18,
          START_MONTH = 5,
          START_YEAR = 2009,
          FLAG = "YES")

#BUG ID 5996 & -28 - add removal of Jan 1 to Feb 1 closure and monthly closure of the 10th to 1st of Feb thru Nov. Bug -29 corrected the Management Status from Monthly to Monthly Recurring for Reg ID -998. Bug ID -30 removed the start time for Reg ID -987
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -988,
          LAST_UPDATED	= "2/10/2022",
          JURISDICTION 	= "FEDERAL",
          ACTION = "FINAL",
          ACTION_TYPE = "AMENDMENT",
          AMENDMENT_NUMBER = "26",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "71 FR 67447",
          FR_SECTION	= "50 CFR 622.34",
          FR_URL	= "https://www.federalregister.gov/documents/2006/11/22/06-9342/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-reef-fish-fishery-of-the-gulf-of-mexico",
          MANAGEMENT_CATEGORY	= "TEMPORAL CONTROLS",
          MANAGEMENT_TYPE	= "CLOSURE",
          SECTOR	= "COMMERCIAL",
          SUBSECTOR	= "ALL",
          REGION	= "GULF OF MEXICO",
          ZONE	= "ALL",
          JURISDICTIONAL_WATERS	= "EEZ",
          COMMON_NAME	= "SNAPPER, RED",
          SPECIES_ITIS	= "168853",
          FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
          MANAGEMENT_STATUS	= "MONTHLY RECURRING",
          EFFECTIVE_DATE	= "01/01/2007",
          INEFFECTIVE_DATE	= "01/01/2007",
          START_DAY	= 10,
          START_MONTH	= 2,
          START_TIME = "12:00:00 PM",
          END_DAY	= 1,
          END_MONTH	= 11,
          END_TIME	= "12:00:00 PM",
          FLAG = "YES")

mh_added <- mh_added %>%
  add_row(REGULATION_ID = -987,
          LAST_UPDATED	= "2/10/2022",
          JURISDICTION 	= "FEDERAL",
          ACTION = "FINAL",
          ACTION_TYPE = "AMENDMENT",
          AMENDMENT_NUMBER = "26",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "71 FR 67447",
          FR_SECTION	= "50 CFR 622.34",
          FR_URL	= "https://www.federalregister.gov/documents/2006/11/22/06-9342/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-reef-fish-fishery-of-the-gulf-of-mexico",
          MANAGEMENT_CATEGORY	= "TEMPORAL CONTROLS",
          MANAGEMENT_TYPE	= "CLOSURE",
          SECTOR	= "COMMERCIAL",
          SUBSECTOR	= "ALL",
          REGION	= "GULF OF MEXICO",
          ZONE	= "ALL",
          JURISDICTIONAL_WATERS	= "EEZ",
          COMMON_NAME	= "SNAPPER, RED",
          SPECIES_ITIS	= "168853",
          FMP	= "REEF FISH RESOURCES OF THE GULF OF MEXICO",
          MANAGEMENT_STATUS	= "SEASONAL",
          EFFECTIVE_DATE	= "01/01/2007",
          INEFFECTIVE_DATE	= "01/01/2007",
          START_DAY	= 1,
          START_MONTH	= 1,
          END_DAY	= 1,
          END_MONTH	= 2,
          END_TIME	= "12:00:00 PM",
          FLAG = "YES")

#Bug ID -58 - Implementation of bag limit that is later removed. Removal has been captured
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -986,
          LAST_UPDATED	= "5/16/2022",
          JURISDICTION 	= "FEDERAL",
          ACTION = "FINAL",
          ACTION_TYPE = "FRAMEWORK AMENDMENT",
          AMENDMENT_NUMBER = "4",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "82 FR 36344",
          FR_SECTION	= "50 CFR 622.382",
          FR_URL	= "https://www.federalregister.gov/documents/2017/08/04/2017-16469/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-coastal-migratory-pelagic-resources-in",
          MANAGEMENT_CATEGORY	= "EFFORT LIMITS",
          MANAGEMENT_TYPE	= "BAG LIMIT",
          SECTOR	= "RECREATIONAL",
          SUBSECTOR	= "ALL",
          REGION	= "MID ATLANTIC",
          ZONE	= "ATLANTIC MIGRATORY GROUP COBIA",
          JURISDICTIONAL_WATERS	= "EEZ",
          COMMON_NAME	= "COBIA",
          SPECIES_ITIS	= "168566",
          FMP	= "COASTAL MIGRATORY PELAGIC RESOURCES",
          EFFECTIVE_DATE	= "09/05/2017",
          START_DAY	= 5,
          START_MONTH	= 9,
          START_YEAR = 2017,
          VALUE = "1",
          VALUE_UNITS = "FISH",
          VALUE_TYPE = "COUNT",
          VALUE_RATE = "PER PERSON PER DAY",
          FLAG = "YES")

#Bug ID -59 - Implementation of bag limit that is later removed. Removal has been captured
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -985,
          LAST_UPDATED	= "5/16/2022",
          JURISDICTION 	= "FEDERAL",
          ACTION = "FINAL",
          ACTION_TYPE = "FRAMEWORK AMENDMENT",
          AMENDMENT_NUMBER = "4",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "82 FR 36344",
          FR_SECTION	= "50 CFR 622.382",
          FR_URL	= "https://www.federalregister.gov/documents/2017/08/04/2017-16469/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-coastal-migratory-pelagic-resources-in",
          MANAGEMENT_CATEGORY	= "EFFORT LIMITS",
          MANAGEMENT_TYPE	= "BAG LIMIT",
          SECTOR	= "RECREATIONAL",
          SUBSECTOR	= "ALL",
          REGION	= "MID ATLANTIC",
          ZONE	= "ATLANTIC MIGRATORY GROUP COBIA",
          JURISDICTIONAL_WATERS	= "EEZ",
          COMMON_NAME	= "COBIA",
          SPECIES_ITIS	= "168566",
          FMP	= "COASTAL MIGRATORY PELAGIC RESOURCES",
          EFFECTIVE_DATE	= "09/05/2017",
          START_DAY	= 5,
          START_MONTH	= 9,
          START_YEAR = 2017,
          VALUE = "6",
          VALUE_UNITS = "FISH",
          VALUE_TYPE = "COUNT",
          VALUE_RATE = "PER VESSEL PER DAY",
          FLAG = "YES")

#Bug ID -60 - Implementation of trip limit that is later removed. Removal has been captured
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -984,
          LAST_UPDATED	= "5/16/2022",
          JURISDICTION 	= "FEDERAL",
          ACTION = "FINAL",
          ACTION_TYPE = "FRAMEWORK AMENDMENT",
          AMENDMENT_NUMBER = "4",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "82 FR 36344",
          FR_SECTION	= "50 CFR 622.385",
          FR_URL	= "https://www.federalregister.gov/documents/2017/08/04/2017-16469/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-coastal-migratory-pelagic-resources-in",
          MANAGEMENT_CATEGORY	= "EFFORT LIMITS",
          MANAGEMENT_TYPE	= "TRIP LIMIT",
          SECTOR	= "COMMERCIAL",
          SUBSECTOR	= "ALL",
          REGION	= "MID ATLANTIC",
          ZONE	= "ATLANTIC MIGRATORY GROUP COBIA",
          JURISDICTIONAL_WATERS	= "EEZ",
          COMMON_NAME	= "COBIA",
          SPECIES_ITIS	= "168566",
          FMP	= "COASTAL MIGRATORY PELAGIC RESOURCES",
          EFFECTIVE_DATE	= "09/05/2017",
          START_DAY	= 5,
          START_MONTH	= 9,
          START_YEAR = 2017,
          VALUE = "2",
          VALUE_UNITS = "FISH",
          VALUE_TYPE = "COUNT",
          VALUE_RATE = "PER PERSON PER DAY",
          FLAG = "YES")

#Bug ID -61 - Implementation of trip limit that is later removed. Removal has been captured
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -983,
          LAST_UPDATED	= "5/16/2022",
          JURISDICTION 	= "FEDERAL",
          ACTION = "FINAL",
          ACTION_TYPE = "FRAMEWORK AMENDMENT",
          AMENDMENT_NUMBER = "4",
          ACCOUNTABILITY_MEASURE	= "NO",
          FR_CITATION	= "82 FR 36344",
          FR_SECTION	= "50 CFR 622.385",
          FR_URL	= "https://www.federalregister.gov/documents/2017/08/04/2017-16469/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-coastal-migratory-pelagic-resources-in",
          MANAGEMENT_CATEGORY	= "EFFORT LIMITS",
          MANAGEMENT_TYPE	= "TRIP LIMIT",
          SECTOR	= "COMMERCIAL",
          SUBSECTOR	= "ALL",
          REGION	= "MID ATLANTIC",
          ZONE	= "ATLANTIC MIGRATORY GROUP COBIA",
          JURISDICTIONAL_WATERS	= "EEZ",
          COMMON_NAME	= "COBIA",
          SPECIES_ITIS	= "168566",
          FMP	= "COASTAL MIGRATORY PELAGIC RESOURCES",
          EFFECTIVE_DATE	= "09/05/2017",
          START_DAY	= 5,
          START_MONTH	= 9,
          START_YEAR = 2017,
          VALUE = "6",
          VALUE_UNITS = "FISH",
          VALUE_TYPE = "COUNT",
          VALUE_RATE = "PER VESSEL PER DAY",
          FLAG = "YES")

mh_cleaned <- bind_rows(mh_added, mh_cleaned)

# Replace all "blank" fields with NA for consistency betwee mh_added and mh_cleaned dataframes
# Also makes it easier to query data when null values are all NA
mh_cleaned <- mh_cleaned %>%
  replace(. == '', NA)

# Format dates from character to be mm/dd/yyyy (this is a product of reading in MH as a csv file, the database should already be in date format)
mh_cleaned <- mh_cleaned %>%
    mutate(EFFECTIVE_DATE = as.Date(EFFECTIVE_DATE, "%m/%d/%Y"),
           INEFFECTIVE_DATE = as.Date(INEFFECTIVE_DATE, "%m/%d/%Y"))

# Remove A or B in FR Citation (exmaple regulation ID = 11514)
mh_cleaned <- mh_cleaned %>%
  mutate(FR_CITATION = str_remove(FR_CITATION, " [AB]"))
