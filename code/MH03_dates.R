# Script 3
# Fill in dates
  # Overview: ####
  # Order regulations by dates
  # Overwrite END_DATE as needed

#### 1 ####
# Add END_DATE and CHANGE_DATE logic to sorted records####
# Results in mh_dates_prep then mh_dates data frame
# CREATE: END_DATE variable dependent on INEFFECTIVE_DATE of current record, START_DATE of next record,
# and CHANGE_DATE of current record
mh_dates_prep <- mh_statid %>%
  # Only process regulations that happen before the terminal date of the processing period
  filter(EFFECTIVE_DATE <= end_timeseries) %>%
  # IDENTIFY AND FLAG START OF EACH GROUPING
  #ARRANGE AND AND GROUP
  arrange(CLUSTER, desc(START_DATE), desc(vol), desc(page)) %>%
  group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE) %>%
  # IDENTIFY AND FLAG START OF EACH GROUPING
  mutate(GROUP_START = min(START_DATE),
         FIRST_REG = GROUP_START == START_DATE) %>%
  #CREATE LINK BETWEEN ADJUSTMENTS AND REVERSIONS
  mutate(LINK = case_when(ADJUSTMENT == 1 & FIRST_REG == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 1) !=1 ~ lead(REGULATION_ID, 1),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 2) !=1 ~ lead(REGULATION_ID, 2),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 3) !=1 ~ lead(REGULATION_ID, 3),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 4) !=1 ~ lead(REGULATION_ID, 4),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 5) !=1 ~ lead(REGULATION_ID, 5),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 6) !=1 ~ lead(REGULATION_ID, 6),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 1) ==1 & lead(FIRST_REG, 1) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 2) ==1 & lead(FIRST_REG, 2) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 3) ==1 & lead(FIRST_REG, 3) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 4) ==1 & lead(FIRST_REG, 4) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 5) ==1 & lead(FIRST_REG, 5) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 6) ==1 & lead(FIRST_REG, 6) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 7) ==1 & lead(FIRST_REG, 7) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 8) ==1 & lead(FIRST_REG, 8) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 9) ==1 & lead(FIRST_REG, 9) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 10) ==1 & lead(FIRST_REG, 10) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 11) ==1 & lead(FIRST_REG, 11) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 12) ==1 & lead(FIRST_REG, 12) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 13) ==1 & lead(FIRST_REG, 13) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 14) ==1 & lead(FIRST_REG, 14) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 15) ==1 & lead(FIRST_REG, 15) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 16) ==1 & lead(FIRST_REG, 16) ==1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 17) ==1 & lead(FIRST_REG, 17) ==1 ~ 0))

#62 records where VALUE == "OPEN" flagged as adjustments (have ineffective_date)
mh_dates_open <- mh_dates_prep %>% filter(VALUE == "OPEN" & !is.na(INEFFECTIVE_DATE))
summary(as.factor(mh_dates_open$ADJUSTMENT))

#1 Adjustment with first reg (link = 0)
#2 Adjustment not first reg, with real reversion sometime before it (link != 0 )
#3 Adjustment not first reg, with first reg adjustment sometime before it (link = 0)
#4 Non-adjustment first reg
#5 Non-adjustment not first reg

# mh_dates_adj = mh_dates_prep %>% filter(ADJUSTMENT == 1)
# summary(mh_dates_adj$LINK == 0) # Situation 1 + 3 happen 78; Situation 3 happens 164 times
# 
# table(mh_dates_prep$VALUE == "OPEN" & !is.na(mh_dates_prep$INEFFECTIVE_DATE), !is.na(mh_dates_prep$LINK)) #All 62 have links
# table(mh_dates_prep$VALUE == "OPEN" & !is.na(mh_dates_prep$INEFFECTIVE_DATE), abs(mh_dates_prep$LINK) > 0) #113 Links are not associated with re-openings
# table(mh_dates_prep$VALUE == "OPEN" & !is.na(mh_dates_prep$INEFFECTIVE_DATE), mh_dates_prep$LINK == 0) #11
# table(mh_dates_prep$VALUE == "OPEN" & !is.na(mh_dates_prep$INEFFECTIVE_DATE), mh_dates_prep$LINK == 0 & mh_dates_prep$FIRST_REG == TRUE)

mh_reversions = mh_dates_prep %>%
  filter(abs(LINK) > 0) %>% #164 regulations
  group_by(LINK, REGULATION_ID) %>%
  #GIVE EACH reversion A START_DATE2, THE DAY AFTER THE RESPECTIVE ADJUSTMENT
  summarize(REVERSION = TRUE,
         START_DATE_USE = END_DATE + 1, #REVERSION NEW START DATE IS DAY AFTER ADJUMENTS ENDS
         SORT_DATE_USE = START_DATE+ 0.5) %>% # HERE IS WHERE WE FORCE REVERSIONS TO SORT RIGHT AFTER THEIR ADJUSTMENTS
  rename(ADJUSTMENT_ID = REGULATION_ID,
         REGULATION_ID = LINK) %>%
  left_join(mh_dates_prep, by = "REGULATION_ID") %>%
  bind_rows(mh_dates_prep) %>%
  mutate(START_DATE_USE = case_when(is.na(START_DATE_USE) ~ START_DATE,
                                    TRUE ~ START_DATE_USE),
         SORT_DATE_USE = case_when(is.na(SORT_DATE_USE) ~ START_DATE_USE,
                                   TRUE ~ SORT_DATE_USE), 
         REVERSION = case_when(REVERSION == TRUE ~ TRUE,
                               TRUE ~ FALSE))

test = mh_reversions %>% filter(REGULATION_ID == "927")

mh_dates <- mh_reversions %>%
  # IDENTIFY AND FLAG START OF EACH GROUPING
  #ARRANGE AND AND GROUP
  arrange(CLUSTER, desc(SORT_DATE_USE), desc(vol), desc(page)) %>%
  group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE) %>%
  # CREATE: the variable of diff and diff_days to signify the length of time between a 
  # regulation and its subsequent replacement.
  # When START_TIME is "12:01:00 AM" or "12:02:00 AM", diff should days should be lagged by one day.
  # This will infer that the regulation began at the start of the day, not one or two minutes into the 
  # day to properly calculate diff_days.
  # The first regulation in our groupings gets NA for diff because no regulation happens after it (it is the most recent and at the top of the list)
  mutate(diff = as.numeric(lag(START_DATE_USE) - START_DATE_USE, units = 'days'),
         diff_days = case_when(is.na(lag(START_TIME)) ~ diff - 1, # if you don't have a start time you need to adjust days by -1, reg started at start of day then ended at start of new reg (the new reg day does not count)
                               lag(START_TIME) == "12:01:00 AM" ~ diff - 1, # same logic as above
                               lag(START_TIME) == "12:02:00 AM" ~ diff - 1, # same logic as above
                               TRUE ~ diff),
         # When diff_days is not calculated due to there being no subsequent regulation, 
         # the end of the time series should be used as the CHANGE_DATE
         CHANGE_DATE = case_when(is.na(diff_days) ~ end_timeseries,
                                 TRUE ~ START_DATE_USE + diff_days), #this just gets at time of subsequent regulation (not if a reg ended then or not)
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
         # When the START_DATE_USE is after the END_DATE, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         NEVER_IMPLEMENTED = case_when(MULTI_REG == 1 ~ 0,
                                       diff_days <= -1 ~ 1,
                                       START_DATE_USE > END_DATE ~ 1,
                                       TRUE ~ 0))



check1708 = mh_dates %>% 
  filter(CLUSTER == "1708") %>% 
  select(FIRST_REG, NEVER_IMPLEMENTED, vol, page, ZONE_USE, 
         MANAGEMENT_STATUS_USE, MULTI_REG,
         CLUSTER, ADJUSTMENT, diff, diff_days, START_DATE, START_DATE_USE, CHANGE_DATE, 
         END_DATE, INEFFECTIVE_DATE, LINK, REGULATION_ID, REVERSION)

check991 = mh_dates %>% 
  filter(CLUSTER == "991") %>% 
  select(FIRST_REG, vol, page, ZONE_USE, 
         MANAGEMENT_STATUS_USE, MULTI_REG,
         CLUSTER, ADJUSTMENT, diff, diff_days, START_DATE, START_DATE_USE, CHANGE_DATE, 
         END_DATE, INEFFECTIVE_DATE, LINK, REGULATION_ID, REVERSION, ADJUSTMENT_ID, NEVER_IMPLEMENTED)

# Adjustments as FIRST_REG don't need adjusting (they either end or are overwritten)
# Only add reversions after adjustments that are not FIRST_REG

#every single adjustment needs to know the regID of the non-adjusment before it
#we could just give every adjustment a regID of the regulation before it regardless of whether it is an adjustment 
#adjLink


# how to add duplicate a line in R dataframe
# If you are an ajustment after you comes a copy of the most recent regulation that was not an adjustment
# That new copy needs to have a start date the end date of the adjustment
# pull out the records that need to be duplicated (first we have to variable on the adjustment linking it to its reversions)
# highlight the fact that it is a reversion
# ID and flag records that happen before an adjustment that are not an adjustment

check991 = mh_dates %>% 
  filter(CLUSTER == "991") %>% 
  select(FIRST_REG, vol, page, ZONE_USE, 
         MANAGEMENT_STATUS_USE, MULTI_REG, 
         CLUSTER, ADJUSTMENT, LINK, REGULATION_ID)

check874 = mh_dates %>% 
  filter(CLUSTER == "874") %>% 
  select(FIRST_REG, vol, page, ZONE_USE, 
         MANAGEMENT_STATUS_USE, MULTI_REG,
         START_DATE, EFFECTIVE_DATE, GROUP_START,
         CLUSTER, ADJUSTMENT, LINK, REGULATION_ID)
write.csv(check874, "check874.csv")