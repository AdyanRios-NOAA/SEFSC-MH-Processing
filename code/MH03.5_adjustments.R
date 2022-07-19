# Script 4 
# Merge in adjustments (for now just the true adjustments)

# Start to work with mh_dates

# STARTED OUR SECOND LEVEL SORTING
# Basically the same sorting without adjustment

mh_dates2 <- mh_dates %>%
  # Sort using same variables
  # Group using same variables except adjustment
  arrange(CLUSTER, desc(START_DATE), desc(vol), desc(page)) %>%
  group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE) %>%
  #
  mutate(# No longer need diff, because everything has a "true" start day
         diff_days2 = as.numeric((lag(START_DATE) - START_DATE) -1, units = 'days'),
         # When diff_days2 is not calculated due to there being no subsequent regulation, 
         # the end of the time series should be used as the CHANGE_DATE
         CHANGE_DATE2 = case_when(is.na(diff_days2) ~ end_timeseries,
                                 TRUE ~ START_DATE + diff_days2),
         # When diff_days2 is calculated as -1, the CHANGE_DATE should be lagged by one day
         CHANGE_DATE2 = case_when(diff_days2 == -1 ~ lag(CHANGE_DATE2),
                                 TRUE ~ CHANGE_DATE2),
         # When an END_DATE is provided it should be used to signify the END_DATE
         END_DATE2 = case_when(!is.na(END_DATE) ~ END_DATE,
                              # is.na(END_YEAR) & !is.na(INEFFECTIVE_DATE) ~ INEFFECTIVE_DATE,
                              TRUE ~ CHANGE_DATE2),
         # If the CHANGE_DATE is after the END_DATE and there is an END_DATE provided, then the END_DATE should be used
         # If the CHANGE_DATE is after the INEFFECTIVE_DATE and an INEFFECTIVE_DATE is provided, then the END_DATE should be used
         # Otherwise, the CHANGE_DATE should be used as the END_DATE
         END_DATE2 = case_when(CHANGE_DATE2 > END_DATE2 & !is.na(END_DATE2) ~ END_DATE2,
                              CHANGE_DATE2 > INEFFECTIVE_DATE & !is.na(INEFFECTIVE_DATE) ~ END_DATE2,
                              TRUE ~ CHANGE_DATE2),
         # CREATE: the variable of NEVER_IMPLEMENTED to signify regulations that were created but never went into effect
         # When the MULT_REG variable is flagged (1), NEVER_IMPLEMENTED should not be flagged (0) meaning the regulation did go into effect
         # When the diff_days variable is less than or equal to -1, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         # When the START_DATE is after the END_DATE, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         NEVER_IMPLEMENTED = case_when(MULTI_REG == 1 ~ 0,
                                       diff_days <= -1 ~ 1,
                                       START_DATE > END_DATE2 ~ 1,
                                       TRUE ~ 0))

check1708_a = mh_dates %>% 
  filter(CLUSTER == "1708") %>% 
  select(NEVER_IMPLEMENTED, vol, page, ZONE_USE, 
         MANAGEMENT_STATUS_USE, ADJUSTMENT, MULTI_REG, 
         CLUSTER, diff, diff_days, START_DATE, CHANGE_DATE, 
         END_DATE, INEFFECTIVE_DATE)
check1708 = mh_dates3 %>% filter(CLUSTER == "1708")
#resort order of variables to make checks easier

# we need to resort
# only for when it is not at the start: phantom record
# we need to "skip" over (116) consecutive adjustments  and never implemented (0)


# Are there consecutive adjustments
# 116 ARE CONSECUTIVE ADJUMENTS
mh_dates4 <- mh_dates3 %>%
  mutate(new = lead(ADJUSTMENT == 1) & ADJUSTMENT == 1,
         NeverI = lead(NEVER_IMPLEMENTED == 1)  & ADJUSTMENT == 1)
table(mh_dates4$new)
table(mh_dates4$NeverI) # NEVER (can code for it just in case)

check1708 = mh_dates2 %>% filter(CLUSTER == "1708")

# 13 ADJUMENTS HAPPEN AT THE VERY START OF THE CLUSTER
# THESE SEEM LIKE THEY WILL EXECUTE/SORT CORRECTLY BECAUSE OF THEIR 
# ASSOCIATED END DATES GOING BACK TO NO REGULATION OR BEING OVERWRITTEN
mh_dates2 %>%
  filter(FIRST_REG == TRUE,
         ADJUSTMENT == TRUE) %>%
  select(CLUSTER) %>%
  n_distinct()

# 61 ADJUSMENTS NEED TO BE REVERTED TO A EALIER REGULATION
mh_dates2 %>%
  filter(FIRST_REG == FALSE,
         ADJUSTMENT == TRUE) %>%
  select(CLUSTER) %>%
  n_distinct()

# 227 ADJSTMENTS HAVE ADJUSMENTS NOT AT THE START
# 116 ARE CONSECUTIVE ADJUMENTS
mh_dates2 %>%
  filter(FIRST_REG == FALSE,
         ADJUSTMENT == TRUE) %>%
  n_distinct()

# THEY EXIST AMONG 7 MANAGMENT TYPES
mh_dates2 %>%
  filter(FIRST_REG == FALSE,
         ADJUSTMENT == TRUE) %>%
  group_by(MANAGEMENT_TYPE) %>%
  summarize(n = n())

# BAG LIMIT ADJUSTMENT             25  -- value, value type, value rate, value units
# TRIP LIMIT ADJUSTMENT            80  -- value, value type, value rate, value units
# MINIMUM SIZE LIMIT ADJUSTMENT     1  -- value, value type,           , value units

# ACL ADJUSTMENT                   19  -- value, value type, value rate, value units
# QUOTA ADJUSTMENT                 40  -- value, value type, value rate, value units
# TAC ADJUSTMENT                    4  -- value, value type, value rate, value units

# REOPENING                        58  -- value

# Have to make sure that the record before an adjustment is actually active at the time the adjustment goes in
# figure this out with duration calculated someway 
  # (regulation before must not be never implemented, or another adjustment etc)
  # If there is a odd previous we might have to lag, and lag again if there are a few until we 
  # find the regular record which to revert to after the adjustment
# How to trigger copying a row and putting it after the adjustment
# Make sure there are not two subsequent adjustment

# Check to know how many adjustments are currently in MH
table(mh_dates$ADJUSTMENT) #242 Adjustment Records

# Check how many clusters have adjustments
mh_dates %>% 
  filter(ADJUSTMENT == 1) %>%
  select(CLUSTER) %>%
  n_distinct()
#89 Clusters

# Find out which FMPs have adjustments
mh_dates %>% 
  filter(ADJUSTMENT == 1) %>%
  group_by(FMP) %>%
  summarize(count = n_distinct(CLUSTER))

  # COASTAL MIGRATORY PELAGIC RESOURCES                     21
  # DOLPHIN AND WAHOO FISHERY OFF THE ATLANTIC STATES        3
  # REEF FISH RESOURCES OF THE GULF OF MEXICO               31
  # SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION    15

# Find out which FMPs have true adjustments
mh_dates %>% 
  filter(ADJUSTMENT == 1,
         MANAGEMENT_TYPE_USE != "CLOSURE") %>%
  group_by(FMP) %>%
  summarize(count = n_distinct(CLUSTER))

  # COASTAL MIGRATORY PELAGIC RESOURCES                     18
  # REEF FISH RESOURCES OF THE GULF OF MEXICO               23
  # SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION     4



# NOW WE HAVE A WAY TO KNOW IF AN ADJUSTMENT IS AT THE START OF THE CLUSTER
check1708 = mh_dates2 %>% filter(CLUSTER == "1708")

# Multiple starting adjustments within cluster 1708 are ok because they have different zones
# THEY DO REVERT BACK TO NO QUOTA (technically infinite quota)

mh_dates2 %>%
  filter(FIRST_REG == TRUE,
         ADJUSTMENT == TRUE) %>%
  select(CLUSTER) %>%
  n_distinct()
#13 Clusters go back to default (lack of regulation)

mh_dates2 %>%
  filter(FIRST_REG == TRUE,
         ADJUSTMENT == TRUE) %>%
  select(CLUSTER) %>%
  mutate(dup = duplicated(CLUSTER))


table(mh_dates2$FIRST_REG == TRUE, mh_dates2$ADJUSTMENT == TRUE)
# 15 ADJUSTMENTS AT START OF CLUSTER



mh_dates2 %>%
  filter(FIRST_REG == TRUE,
         ADJUSTMENT == TRUE,
         MANAGEMENT_TYPE_USE != "CLOSURE") %>%
  select(CLUSTER) %>%
  n_distinct()
#9 Cluster

mh_dates2 %>%
  filter(FIRST_REG == TRUE,
         ADJUSTMENT == TRUE,
         MANAGEMENT_TYPE_USE != "CLOSURE") %>%
  dim()
# 11 Adjustments at the start of true clusters (6 plain, 2 doubles, 1 permanent)

table(mh_dates2$FIRST_REG == TRUE & mh_dates2$ADJUSTMENT == TRUE,  mh_dates2$MANAGEMENT_TYPE_USE == "CLOSURE")
# 4 CASES OPERATING UNDER CLOSURES

# What are the management types of starting cluster with adjustments
ADJ_ONLY <- mh_dates2 %>% 
  filter(ADJUSTMENT == 1) 
table(ADJ_ONLY$MANAGEMENT_TYPE_USE, ADJ_ONLY$FIRST_REG == TRUE & ADJ_ONLY$ADJUSTMENT == TRUE)
