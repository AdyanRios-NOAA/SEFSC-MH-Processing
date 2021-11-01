# Data bugs and changes in the database script

# For changes in the database, this includes reclassifying management cateogries and types

# For data bugs, all bugs have been previously addressed or entered into the data bug tracking system
  # Record each data bug ID in each step


# Read in data
mh <- read.csv("./MHdownload09212021.csv", stringsAsFactors = FALSE)

# Run only if you want to find the data type of MH variables
# str(mh)

# Changes that are a result of the csv download (the database does not have these issues)
# No longer appears to be an issue when downloading the data to a csv - SFA 9/27/2021
mh_cleaned <- mh #%>%
  # Rename regulation ID to match what appears in the database
  #rename(REGULATION_ID = REGULATION_ID.) %>%
  # Remove ="..." characters in species ITIS codes
  #mutate(SPECIES_ITIS = gsub('="', '', SPECIES_ITIS),
  #       SPECIES_ITIS = gsub('"', '', SPECIES_ITIS))

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

#Change all IFQ Program Adapted to Management Category: Other
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'IFQ PROGRAM ADAPTED' ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all ITQ Program Adapted to Management Category: Other
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'ITQ PROGRAM ADAPTED' ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Limited Access System to Management Category: Other
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'LIMITED ACCESS SYSTEM' ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Permit Moratorium to Management Category: Other
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'PERMIT MORATORIUM' ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all IFQ Program Established to Management Category: Universal
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'IFQ PROGRAM ESTABLISHED' ~ 'UNIVERSAL',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all ITQ Program Established to Management Category: Universal
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'ITQ PROGRAM ESTABLISHED' ~ 'UNIVERSAL',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Fishing Season to Management Category: Universal
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'FISHING SEASON' ~ 'UNIVERSAL',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Fishing Year to Management Category: Universal
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'FISHING YEAR' ~ 'UNIVERSAL',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Individual Bluefin Quota (IBQ) Program Established to Management Category: Universal
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'INDIVIDUAL BLUEFIN QUOTA (IBQ) PROGRAM ESTABLISHED' ~ 'UNIVERSAL',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Allowable Species to Management Category: Universal
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'ALLOWABLE SPECIES' ~ 'UNIVERSAL',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Prohibited Species to Management Category: Universal
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'PROHIBITED SPECIES' ~ 'UNIVERSAL',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Harvest Method to Management Category: Harvest Limitations
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'HARVEST METHOD' ~ 'HARVEST LIMITATIONS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Lobsters As Attractants to Management Category: Harvest Limitations
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'LOBSTERS AS ATTRACTANTS' ~ 'HARVEST LIMITATIONS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Bait Restriction to Management Category: Harvest Limitations
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'BAIT RESTRICTION' ~ 'HARVEST LIMITATIONS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Management Category: Gear Limitations to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_CATEGORY == 'GEAR LIMITATIONS' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all BRD Requirement to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'BRD REQUIREMENT' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Dehooking Device to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'DEHOOKING DEVICE' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Descending Device to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'DESCENDING DEVICE' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Green-stick Gear Requirements to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'GREEN_STICK GEAR REQUIREMENTS' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Non-offset Circle Hook Requirements to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'NON-OFFSET CIRCLE HOOK REQUIREMENTS' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Non-offset, Non-Stainless Steel Circle Hooks to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'NON-OFFSET, NON-STAINLESS STEEL CIRCLE HOOKS' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Non-Stainless Steel Circle Hooks to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'NON-STAINLESS STEEL CIRCLE HOOKS' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Non-Stainless Steel Hooks to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'NON-STAINLESS STEEL HOOKS' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Turtle Release Gear to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'TURTLE RELEASE GEAR' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Venting Tool to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'VENTING TOOL' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Weak Hook (Offset Circle Hooks) to Management Category: Gear Requirements
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'WEAK HOOK (OFFSET CIRCLE HOOKS)' ~ 'GEAR REQUIREMENTS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all MSST to Management Category: Catch Limits
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'MSST' ~ 'CATCH LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY))

#Change all Hook size/type/# to Management Category: Effort Limits
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(MANAGEMENT_TYPE == 'HOOK SIZE/TYPE/#' ~ 'EFFORT LIMITS',
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
  filter(REGULATION_ID != 418) %>%
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
  # Bug ID 4590
  filter(REGULATION_ID != 511)%>%
  #Bug ID 4591
  filter(REGULATION_ID != 1341)

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

#Bug ID 5166 - Remove 'B' from FR Citation for 81 FR 33150 B
mh_cleaned <- mh_cleaned %>%
  mutate(FR_CITATION = case_when(REGULATION_ID == 169 ~ '81 FR 33150',
                                 TRUE ~ FR_CITATION))

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

#Bug ID 4949 - update mng. status to once and added start and end year since it is not a seasonal reg
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 748 ~ 'ONCE',
                                       TRUE ~ MANAGEMENT_STATUS),
         START_YEAR = case_when(REGULATION_ID == 748 ~ 2005L,
                                TRUE ~ START_YEAR),
         END_YEAR = case_when(REGULATION_ID == 748 ~ 2005L,
                              TRUE ~ END_YEAR))

#Bug ID 5086 - Update effective and start date following correction guidance (updated by SFA 10/12/2021)
mh_cleaned <- mh_cleaned %>%
  mutate(EFFECTIVE_DATE = case_when(REGULATION_ID == 1106 ~ '10/01/2006',
                                    TRUE ~ EFFECTIVE_DATE),
         START_YEAR = case_when(REGULATION_ID == 1106 ~ 2006L,
                                TRUE ~ START_YEAR),
         START_MONTH = case_when(REGULATION_ID == 1106 ~ 10L,
                                 TRUE ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID == 1106 ~ 1L,
                               TRUE ~ START_DAY))

#Bug ID 4694 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1687 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4693 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1575 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4692 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1576 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4691 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1577 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4690 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1564 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4689 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1571 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4688 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1572 ~'',
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

#Bug ID 4684 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1566 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4683 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1567 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4682 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1568 ~'',
                                       TRUE ~ MANAGEMENT_STATUS))

#Bug ID 4681 - updated mng. status to blank for fishing season regulation
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_STATUS = case_when(REGULATION_ID == 1685 ~'',
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

#Bug ID 4552 - remove end time following guidance to not capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_TIME = case_when(REGULATION_ID == 165 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4551 - remove end time following guidance to not capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_TIME = case_when(REGULATION_ID == 3228 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4549 - remove end time following guidance to not capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_TIME = case_when(REGULATION_ID == 3357 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4547 - Updated End Day and End Time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4293 ~ 30L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4293 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4538 - update End Day, End Month, and End Time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4324 ~ 1L,
                             TRUE ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID == 4324 ~ 4L,
                               TRUE ~ END_MONTH),
         END_TIME = case_when(REGULATION_ID == 4324 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4537 - updated End Day and End Time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4326 ~ 15L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4326 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4536 - updated End Day and End Time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4325 ~ 1L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4325 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4529 - updated End Day and End Time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4371 ~22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4371 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4528 - updated End Day and End Time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4373 ~22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4373 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4527 - updated End Day and End Time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4370 ~ 22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4370 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4526 - update End Day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4369 ~ 22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4369 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4525 - update End Day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4368 ~ 22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4368 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4524 - update End Day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4372 ~ 22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4372 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4523 - update End Day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4374 ~ 22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4374 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4522 - update End Day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4378 ~ 22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4378 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4521 - update End Day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4377 ~ 22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4377 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4520 - update End Day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4376 ~ 22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4376 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4519 - update End Day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4375 ~ 22L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4375 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4518 - update End Day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4379 ~ 28L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4379 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4517 - update End Day, End Month, and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4382 ~ 30L,
                             TRUE ~ END_DAY),
         END_MONTH = case_when(REGULATION_ID == 4382 ~ 11L,
                               TRUE ~ END_MONTH),
         END_TIME = case_when(REGULATION_ID == 4382 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4516 - update End Day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4381 ~ 2L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4381 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4507 - update End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_TIME = case_when(REGULATION_ID == 2462 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4500 - update End day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4383 ~ 23L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4383 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4499 - update End day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4445 ~ 2L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4445 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4498 - update End day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4446 ~ 1L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4446 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4497 - update End day and End time following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(REGULATION_ID == 4447 ~ 2L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(REGULATION_ID == 4447 ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4546 - update End day and End time for 51 FR 23551 following guidance not to capture end time of 2400 hours
mh_cleaned <- mh_cleaned %>%
  mutate(END_DAY = case_when(FR_CITATION == '51 FR 23551' ~ 23L,
                             TRUE ~ END_DAY),
         END_TIME = case_when(FR_CITATION == '51 FR 23551' ~ '',
                              TRUE ~ END_TIME))

#Bug ID 4496 - update Start Year, Month, and Day to blank following guidance to not capture Start Date for withdrawn regulations
mh_cleaned <- mh_cleaned %>%
  mutate(START_YEAR = case_when(REGULATION_ID != 1463 ~ START_YEAR),
         START_MONTH = case_when(REGULATION_ID != 1463 ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID != 1463 ~ START_DAY))

#Bug ID 4487 - update Start Year, Month, and Day to blank following guidance to not capture Start Date for withdrawn regulations
mh_cleaned <- mh_cleaned %>%
  mutate(START_YEAR = case_when(REGULATION_ID != 1355 ~ START_YEAR),
         START_MONTH = case_when(REGULATION_ID != 1355 ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID != 1355 ~ START_DAY))

#Bug ID 4486 - Update effective date, start date, and end date following correction guidance
mh_cleaned <- mh_cleaned %>%
  mutate(EFFECTIVE_DATE = case_when(REGULATION_ID == 1358 ~ '12/20/2010',
                                    TRUE ~ EFFECTIVE_DATE),
         START_YEAR = case_when(REGULATION_ID == 1358 ~ 2010L,
                                TRUE ~ START_YEAR),
         START_MONTH = case_when(REGULATION_ID == 1358 ~ 12L,
                                 TRUE ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID == 1358 ~ 20L,
                               TRUE ~ START_DAY),
         END_YEAR = case_when(REGULATION_ID != 1358 ~ END_YEAR),
         END_MONTH = case_when(REGULATION_ID != 1358 ~ END_MONTH),
         END_DAY = case_when(REGULATION_ID != 1358 ~ END_DAY))

#Bug ID 4950 -  Management Category and Management Type updated from Other: Regulatory Reporting to Other: VMS Related
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1109 ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1109 ~ 'VMS RELATED',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4828 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1566 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1566 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4827 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1567 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1567 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4826 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1568 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1568 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4825 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1685 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1685 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4824 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1573 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1573 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4823 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1574 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1574 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4822 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1689 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1689 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4821 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1571 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1571 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4820 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1572 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1572 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4819 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1688 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1688 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4818 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1576 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1576 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4817 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1577 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1577 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4816 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1690 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1690 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4807 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1579 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1579 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4806 - Management Category and Management Type updated from Temporal Controls: Fishing Season to Temporal Controls: Reopening
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1697 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1697 ~ 'REOPENING',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4458 - Management Category and Management Type updated from Other: Ignorable to Other: Consolidation of Ongoing Regulations
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1351 ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1351 ~ 'CONSOLIDATION OF ONGOING REGULATIONS',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 5076 - Management Category and Management Type updated from Effort Limits: Trip Limit to Effort Limits: Trip Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1148 ~ 'EFFORT LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1148 ~ 'TRIP LIMIT ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 5077 - Management Category and Management Type updated from Effort Limits: Trip Limit to Effort Limits: Trip Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1146 ~ 'EFFORT LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1146 ~ 'TRIP LIMIT ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 5078 - Management Category and Management Type updated from Effort Limits: Bag Limit to Effort Limits: Bag Limit Adjustment
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 919 ~ 'EFFORT LIMITS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 919 ~ 'BAG LIMIT ADJUSTMENT',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4947 - Management Category and Management Type updated from Effort Limits: Prohibited Species to Temporal Controls: Closure
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1242 ~ 'TEMPORAL CONTROLS',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1242 ~ 'CLOSURE',
                                     TRUE ~ MANAGEMENT_TYPE))

#Bug ID 4560 - Management Category and Management Type updated from Other: Electronic Reporting to Other: Permit Requirements and Endorsements, FR section changed to 50 CFR 622.20
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 2421 ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 2421 ~ 'PERMIT REQUIREMENTS AND ENDORSEMENTS',
                                     TRUE ~ MANAGEMENT_TYPE),
         FR_SECTION = case_when(REGULATION_ID == 2421 ~ '50 CFR 622.20',
                                TRUE ~ FR_SECTION))

# Bug ID 4589 - Management Category and Type updated to Other: Ignorable, FR Section updated to general 50 CFR 622, start date changed, value fields left blank - Other record removed above with all removals
# Updated by SFA 10/12/2021 start year, month day should not be character
mh_cleaned <- mh_cleaned %>%
  mutate(MANAGEMENT_CATEGORY = case_when(REGULATION_ID == 1085 ~ 'OTHER',
                                         TRUE ~ MANAGEMENT_CATEGORY),
         MANAGEMENT_TYPE = case_when(REGULATION_ID == 1085 ~ 'IGNORABLE',
                                     TRUE ~ MANAGEMENT_TYPE),
         FR_SECTION = case_when(REGULATION_ID == 1085 ~ '50 CFR 641',
                                TRUE ~ FR_SECTION),
         EFFECTIVE_DATE = case_when(REGULATION_ID == 1085 ~ '08/19/1991',
                                    TRUE ~ EFFECTIVE_DATE),
         START_YEAR = case_when(REGULATION_ID == 1085 ~ 1991L,
                                TRUE ~ START_YEAR),
         START_MONTH = case_when(REGULATION_ID == 1085 ~ 08L,
                                 TRUE ~ START_MONTH),
         START_DAY = case_when(REGULATION_ID == 1085 ~ 19L,
                               TRUE ~ START_DAY),
         VALUE = case_when(REGULATION_ID == 1085 ~'',
                           TRUE ~ VALUE),
         VALUE_UNITS = case_when(REGULATION_ID == 1085 ~ '',
                                 TRUE ~ VALUE_UNITS),
         VALUE_TYPE = case_when(REGULATION_ID == 1085 ~ '',
                                TRUE ~ VALUE_TYPE),
         VALUE_RATE = case_when(REGULATION_ID == 1085 ~ '',
                                TRUE ~ VALUE_RATE))

#Bug ID 4948 - Updated Action Type from blank to Amendment
mh_cleaned <- mh_cleaned %>%
  mutate(ACTION_TYPE = case_when(REGULATION_ID == 1243 ~ 'AMENDMENT',
                                 TRUE ~ ACTION_TYPE))

#Bug ID 4947 - Updated Action Type from blank to Amendment
mh_cleaned <- mh_cleaned %>%
  mutate(ACTION_TYPE = case_when(REGULATION_ID == 1242 ~ 'AMENDMENT',
                                 TRUE ~ ACTION_TYPE))

#Bug ID 4946 - Updated Action Type from blank to Amendment
mh_cleaned <- mh_cleaned %>%
  mutate(ACTION_TYPE = case_when(REGULATION_ID == 1029 ~ 'AMENDMENT',
                                 TRUE ~ ACTION_TYPE))

#Bug ID 4906 - Updated sector and subsector from R: All to R: For-hire
mh_cleaned <- mh_cleaned %>%
  mutate(SECTOR = case_when(REGULATION_ID == 1560 ~ 'RECREATIONAL',
                            TRUE ~ SECTOR),
         SUBSECTOR = case_when(REGULATION_ID == 1560 ~ 'FOR-HIRE',
                               TRUE ~ SUBSECTOR))

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
# Bug ID 4446 - change zone for more accuracy to specific to counties
mh_cleaned <- mh_cleaned %>%
  mutate(ZONE = case_when(REGULATION_ID == 3653 ~ 'FLORIDA COUNTIES - MANATEE, SARASOTA, DESOTO, CHARLOTTE, LEE, COLLIER, MONROE, MIAMI-DADE, AND BROWARD',
                          TRUE ~ ZONE))

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

#Bug ID 4463 - Update flag to NO for Incidental Catch Limit
mh_cleaned <- mh_cleaned %>%
  mutate(FLAG = case_when(REGULATION_ID == 1042 ~ 'NO',
                          TRUE ~ FLAG))

#Bug ID 4464 - Update CFR section to the general 50 CFR 641 - Duplicate Other: FMP ESTABLISHMENT records were removed with other records above
mh_cleaned <- mh_cleaned %>%
  mutate(FR_SECTION = case_when(REGULATION_ID == 1221 ~ '50 CFR 641',
                                TRUE ~ FR_SECTION))

#Bug ID 4490 - Update Value Type to Maximum for Bag Limit Multiplier
mh_cleaned <- mh_cleaned %>%
  mutate(VALUE_TYPE = case_when(REGULATION_ID == 1561 ~ 'MAXIMUM',
                                TRUE ~ VALUE_TYPE))

#Bug ID 4491 - Update Value Type to Maximum for Bag Limit Multiplier
mh_cleaned <- mh_cleaned %>%
  mutate(VALUE_TYPE = case_when(REGULATION_ID == 1558 ~ 'MAXIMUM',
                                TRUE ~ VALUE_TYPE))

#Bug ID 4492 - Update Value Type to Maximum for Bag Limit Multiplier
mh_cleaned <- mh_cleaned %>%
  mutate(VALUE_TYPE = case_when(REGULATION_ID == 1559 ~ 'MAXIMUM',
                                TRUE ~ VALUE_TYPE))

#Bug ID 4493 - Update Value Type to Maximum for Bag Limit Multiplier
mh_cleaned <- mh_cleaned %>%
  mutate(VALUE_TYPE = case_when(REGULATION_ID == 1560 ~ 'MAXIMUM',
                                TRUE ~ VALUE_TYPE))

#Bug ID 4494 - Update Value Type to Maximum for Bag Limit Multiplier
mh_cleaned <- mh_cleaned %>%
  mutate(VALUE_TYPE = case_when(REGULATION_ID == 1416 ~ 'MAXIMUM',
                                TRUE ~ VALUE_TYPE))

#BUG ID 4727 - update region from ALL to GULF OF MEXICO
mh_cleaned <- mh_cleaned %>%
  mutate(REGION = case_when(REGULATION_ID == 1340 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#BUG ID 4726 - update region from ALL to GULF OF MEXICO
mh_cleaned <- mh_cleaned %>%
  mutate(REGION = case_when(REGULATION_ID == 1341 ~ 'GULF OF MEXICO',
                            TRUE ~ REGION))

#Bug ID 4495 - Update Region to GOM from ALL for Ignorable record
mh_cleaned <- mh_cleaned %>%
  mutate(REGION = case_when(REGULATION_ID == 1340 ~ 'GULF OF MEXICO',
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

#Bug ID 5136 - Add record for Other VMS Related for the For-Hire sector
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -1000,
                     LAST_UPDATED = "9/30/2021",
                     JURISDICTION = "FEDERAL",
                     ACTION = "FINAL",
                     ACTION_TYPE = "AMENDMENT",
                     AMENDMENT_NUMBER = "18A",
                     ACCOUNTABILITY_MEASURE = "NO",
                     FR_CITATION = "71 FR 45428",
                     FR_SECTION = "50 CFR 622.9",
                     FR_URL = "https://www.federalregister.gov/documents/2006/08/09/E6-12984/fisheries-of-the-caribbean-gulf-of-mexico-and-south-atlantic-reef-fish-fishery-of-the-gulf-of-mexico",
                     MANAGEMENT_CATEGORY = "OTHER",
                     MANAGEMENT_TYPE = "VMS RELATED",
                     SECTOR = "RECREATIONAL",
                     SUBSECTOR = "FOR-HIRE",
                     REGION = "GULF OF MEXICO",
                     ZONE = "ALL",
                     JURISDICTIONAL_WATERS = "EEZ",
                     COMMON_NAME = "ALL",
                     FMP = "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                     EFFECTIVE_DATE = "12/07/2006",
                     START_DAY = 7,
                     START_MONTH = 12,
                     START_YEAR = 2006,
                     FLAG = "YES")


#Bug ID 4966 - Add record for Other: State Related
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -999,
                     LAST_UPDATED = "09/30/2021",
                     JURISDICTION = "FEDERAL",
                     ACTION = "FINAL",
                     ACTION_TYPE = "AMENDMENT",
                     AMENDMENT_NUMBER = "30B",
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
mh_added <- mh_added %>%
  add_row(REGULATION_ID = -993,
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

mh_cleaned <- bind_rows(mh_added, mh_cleaned)

# Replace all "blank" fields with NA for consistency betwee mh_added and mh_cleaned dataframes
# Also makes it easier to query data when null values are all NA
mh_cleaned <- mh_cleaned %>%
  replace(. == '', NA)
