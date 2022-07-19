# Script 3
# Fill in dates
  # Overview: ####
  # Order regulations by dates
  # Overwrite END_DATE as needed

#### 1 ####
# Add END_DATE and CHANGE_DATE logic to sorted records####
# Results in mh_dates data frame
# CREATE: END_DATE variable dependent on INEFFECTIVE_DATE of current record, START_DATE of next record,
# and CHANGE_DATE of current record
mh_dates <- mh_statid %>%
  # Only process regulations that happen before the terminal date of the processing period
  filter(EFFECTIVE_DATE <= end_timeseries) %>%
  arrange(CLUSTER, desc(START_DATE), desc(vol), desc(page)) %>%
  group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE) %>%
  # CREATE: the variable of diff and diff_days to signify the length of time between a 
  # regulation and its subsequent replacement.
  # When START_TIME is "12:01:00 AM" or "12:02:00 AM", diff should days should be lagged by one day.
  # This will infer that the regulation began at the start of the day, not one or two minutes into the 
  # day to properly calculate diff_days.
  # The first regulation in our groupings gets NA for diff because no regulation happens after it (it is the most recent and at the top of the list)
  mutate(diff = as.numeric(lag(START_DATE) - START_DATE, units = 'days'),
         diff_days = case_when(is.na(lag(START_TIME)) ~ diff - 1, # if you don't have a start time you need to adjust days by -1, reg started at start of day then ended at start of new reg (the new reg day does not count)
                               lag(START_TIME) == "12:01:00 AM" ~ diff - 1, # same logic as above
                               lag(START_TIME) == "12:02:00 AM" ~ diff - 1, # same logic as above
                               TRUE ~ diff),
         # When diff_days is not calculated due to there being no subsequent regulation, 
         # the end of the time series should be used as the CHANGE_DATE
         CHANGE_DATE = case_when(is.na(diff_days) ~ end_timeseries,
                                 TRUE ~ START_DATE + diff_days), #this just gets at time of subsequent regulation (not if a reg ended then or not)
         # When diff_days is calculated as -1, the CHANGE_DATE should be lagged by one day (this allows us to skip to information from an earlier reg, in a game of telephone across our regs)
         CHANGE_DATE = case_when(diff_days == -1 ~ lag(CHANGE_DATE), # this is different from subtracting one day (this is a resulting difference of -1 day)
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
         # CREATE: the variable of NEVER_IMPLEMENTED to signify regulations that were created but never went into effect
         # When the MULT_REG variable is flagged (1), NEVER_IMPLEMENTED should not be flagged (0) meaning the regulation did go into effect
         # When the diff_days variable is less than or equal to -1, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         # When the START_DATE is after the END_DATE, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         NEVER_IMPLEMENTED = case_when(MULTI_REG == 1 ~ 0,
                                       diff_days <= -1 ~ 1,
                                       START_DATE > END_DATE ~ 1,
                                       TRUE ~ 0)) %>%
        # IDENTIFY AND FLAG START OF EACH CLUSTER
        group_by(CLUSTER) %>%
        mutate(CLUSTER_START = min(EFFECTIVE_DATE)) %>%
        ungroup() %>%
        mutate(FIRST_REG = CLUSTER_START == START_DATE)


check1708 = mh_dates %>% 
  filter(CLUSTER == "1708") %>% 
  select(FIRST_REG, NEVER_IMPLEMENTED, vol, page, ZONE_USE, 
         MANAGEMENT_STATUS_USE, MULTI_REG, 
         CLUSTER, ADJUSTMENT, diff, diff_days, START_DATE, CHANGE_DATE, 
         END_DATE, INEFFECTIVE_DATE)

check991 = mh_dates %>% 
  filter(CLUSTER == "991") %>% 
  select(FIRST_REG, NEVER_IMPLEMENTED, vol, page, ZONE_USE, 
         MANAGEMENT_STATUS_USE, MULTI_REG, 
         CLUSTER, ADJUSTMENT, diff, diff_days, START_DATE, CHANGE_DATE, 
         END_DATE, INEFFECTIVE_DATE)

# Adjustments as FIRST_REG don't need adjusting (they either end or are overwritten)
# Only add reversions after adjustments that are not FIRST_REG

#every single adjustment needs to know the regID of the non-adjusment before it


# how to add duplicate a line in R dataframe
# If you are an ajustment after you comes a copy of the most recent regulation that was not an adjustment
# That new copy needs to have a start date the end date of the adjustment
# pull out the records that need to be duplicated (first we have to variable on the adjustment linking it to its reversions)
# highlight the fact that it is a reversion
# ID and flag records that happen before an adjustment that are not an adjustment

