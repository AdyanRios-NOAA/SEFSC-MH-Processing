# Script 4
# Final processing steps
  # Overview: ####
  # Add END_DATE to sorted records
  # Export the data

# Believe this can all be removed - will check in meeting 5/31/22
# SUMMARY OF STEPS 
# 1 DEFINE VARIABLES THAT DETECT REG CHANGES BASED ON STATUS TYPE
# 2 CHECK FOR CONSECUTIVELY REPEATED REGS
#   2A CREATE EMPTY LIST TO STORE DETECTED REG CHANGES
#   2B SORT BY FR_CITATION WITHIN CLUSTER (CHANGE THIS TO A FUNCTION!!!)
# 3 ADD FLAGS FOR REMOVAL, GENERAL(AKA FYI)/DETAILED, COMPLEX REGS, DEFINE SETTINGS THAT IMPLY A POTENTIALLY REDUNDANT RECORD
# 4 FILTER TO EXPLORE POTENTIALLY REDUNDANT RECORDS
# 5 ADD END DATE TO SORTED RECORDS
# 6 EXPORT DATA

# 1 DEFINE VARIABLES THAT DETECT REG CHANGES BASED ON STATUS TYPE
# SFA added ineffective date to accommodate issue in cluster 657
# IF ANYTHING CHANGES IN ANY OF THESE VARIABLES THEN THAT MEANS A REG CHANGE
# var_track <- c("INEFFECTIVE_DATE", "START_DAY_RECURRING",	'START_MONTH_RECURRING',	#'START_YEAR_RECURRING',
#                'START_TIME_RECURRING',	'START_DAY_OF_WEEK_RECURRING',
#                "END_DAY_RECURRING",	"END_MONTH_RECURRING",	#'END_YEAR_RECURRING',
#                'END_TIME_RECURRING',	'END_DAY_OF_WEEK_RECURRING',
#                "VALUE", "VALUE_UNITS", "VALUE_TYPE", "VALUE_RATE",
#                "INEFFECTIVE_DATE", "FLAG", "MULTI_REG")
# 
# # 2 CHECK FOR CONSECUTIVELY REPEATED REGS 
# 
# # 2A CREATE EMPTY LIST TO STORE DETECTED REG CHANGES  
# 
# mh_detect <- list()
# 
# # 2B SORT BY FR_CITATION WITHIN CLUSTER (CHANGE THIS TO A FUNCTION!!!) 
# 
# for (i in 1:max(mh_prep_use$CLUSTER)) {
# 
#   mh_CLUSTER_i <- mh_prep_use %>%
#     filter(CLUSTER == i) %>%
#     arrange(vol, page, EFFECTIVE_DATE, START_DATE)
# 
#   change_event = rep(1, length(mh_CLUSTER_i$CLUSTER))
# 
#   #START J=2; FIRST EVENT ALWAYS IMPORTANT
# 
#   if(length(mh_CLUSTER_i$CLUSTER) > 1) {
#     for (j in 2:length(mh_CLUSTER_i$CLUSTER)){
#       compare <- mh_CLUSTER_i[(j-1):j,] %>% select(all_of(var_track)) %>%
#         distinct()
#       change_event[j] = nrow(compare) == 2
#     }
#   }
# 
#   mh_detect[[i]] = mh_CLUSTER_i %>%
#     mutate(REG_CHANGE = change_event)
# }
# 
# # Create a blank list for storing all the cluster that will be looked at one by one 
# # For each cluster, filter to just that cluster and sort by effective and start date
#   # Create a new variable to track change events
#   # Start with the assumption that every event is a change event
#   # Only run comparison when there is more than one record in a cluster
#     # For each record (2:total number), compare to record immediately before
#       # The comparison is done by selecting the variables being tracked and filtering to just the two records being compared 
#       # If compare is 2 records long that means the two records are different (change event TRUE)
#       # If compare is 1 record long that means the two records were identical (change event FALSE) 
#   # Merge change event tracker back into the filtered cluster and save inside the list 
#   # Each element of that list is tied to one cluster
# # Flatten list and add flag that implies a redundant record
# 
# # REVISIT the fact that reg_removed, multi_reg, general, and complex, and adjusted are currently considered never redundant
# 
# # 3 FLATTEN LIST ADD FLAG THAT IMPLY A REDUNDANT RECORD
# 
# mh_detect <- mh_detect %>%
#   bind_rows() %>%
#   mutate(REDUNDANT = (REG_CHANGE + REG_REMOVED + MULTI_REG + as.numeric(FLAG == "YES") + as.numeric(MANAGEMENT_TYPE == "IGNORABLE")) == 0)
# 
# # 4 FILTER TO JUST REDUNDANT RECORDS
# 
# mh_redundant <- mh_detect %>%
#   filter(REDUNDANT == 1)
# Believe all above can be removed - check in meeting 5/31/22

#### 1 ####
# Short cut to name used in step 5
# Results in mh_detect data frame 
  mh_detect <- mh_prep_use
  
# Add END_DATE and CHANGE_DATE logic to sorted records####
# Results in mh_sort data frame
# CREATE: END_DATE variable dependent on INEFFECTIVE_DATE of current record, START_DATE of next record,
# and CHANGE_DATE of current record
mh_sort <- mh_detect %>%
  # filter(EFFECTIVE_DATE <= end_timeseries, REDUNDANT == 0) %>%
  # Only process regulations that happen before the terminal date of the processing period
  filter(EFFECTIVE_DATE <= end_timeseries) %>%
  arrange(CLUSTER, desc(START_DATE), desc(vol), desc(page)) %>%
  #group_by(CLUSTER, ADJUSTMENT,) %>%
  group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE, ADJUSTMENT) %>%
  # CREATE: the variable of diff and diff_days to signify the length of time between a 
  # regulation and its subsequent replacement.
  # When START_TIME is "12:01:00 AM" or "12:02:00 AM", diff should days should be lagged by one day.
  # This will infer that the regulation began at the start of the day, not one or two minutes into the 
  # day to properly calculate diff_days.
  mutate(diff = as.numeric(lag(START_DATE) - START_DATE, units = 'days'),
         diff_days = case_when(is.na(lag(START_TIME)) ~ diff - 1,
                               lag(START_TIME) == "12:01:00 AM" ~ diff - 1,
                               lag(START_TIME) == "12:02:00 AM" ~ diff - 1,
                               TRUE ~ diff),
         # When diff_days is not calculated due to there being no subsequent regulation, 
         # the end of the time series should be used as the CHANGE_DATE
         CHANGE_DATE = case_when(is.na(diff_days) ~ end_timeseries,
                                 TRUE ~ START_DATE + diff_days),
         # When diff_days is calculated as -1, the CHANGE_DATE should be lagged by one day
         CHANGE_DATE = case_when(diff_days == -1 ~ lag(CHANGE_DATE),
                                 TRUE ~ CHANGE_DATE),
         # When an END_DATE is provided it should be used to signify the END_DATE
         END_DATE = case_when(!is.na(END_DATE) ~ END_DATE,
                              # is.na(END_YEAR) & !is.na(INEFFECTIVE_DATE) ~ INEFFECTIVE_DATE,
                              TRUE ~ CHANGE_DATE),
         # If the CHANGE_DATE is after the END_DATE and there is an END_DATE provided, then the END_DATE should be used
         # If the CHANGE_DATE is after the INEFFECTIVE_DATE and an INEFFECTIVE_DATE is provided, then the END_DATE should be used
         # Otherwise, the CHANGE_DATE should be used as the END_DATE
         END_DATE = case_when(CHANGE_DATE > END_DATE & !is.na(END_DATE) ~ END_DATE,
                              CHANGE_DATE > INEFFECTIVE_DATE & !is.na(INEFFECTIVE_DATE) ~ END_DATE,
                              TRUE ~ CHANGE_DATE),
         # ROUNDED_START_YEAR = format(round_date(START_DATE, "year"),"%Y"),
         # ROUNDED_END_YEAR = case_when(as.numeric(format(END_DATE, "%m")) >= 7 ~ as.numeric(format(round_date(END_DATE, "year"),"%Y"))-1,
         #                              as.numeric(format(END_DATE, "%m")) < 7 ~ as.numeric(format(round_date(END_DATE, "year"),"%Y"))),
         # CREATE: the variable of NEVER_IMPLEMENTED to signify regulations that were created but never went into effect
         # When the MULT_REG variable is flagged (1), NEVER_IMPLEMENTED should not be flagged (0) meaning the regulation did go into effect
         # When the diff_days variable is less than or equal to -1, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         # When the START_DATE is after the END_DATE, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         NEVER_IMPLEMENTED = case_when(MULTI_REG == 1 ~ 0,
                                       diff_days <= -1 ~ 1,
                                       START_DATE > END_DATE ~ 1,
                                       TRUE ~ 0)) %>%
  #bind_rows(., mh_redundant) %>%
### 2 ####
# Export the data ####
      data.frame()

