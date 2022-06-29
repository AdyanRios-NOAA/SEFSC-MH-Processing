# Script 4 
# Merge in adjustments (for now just the true adjustments)

# Start to work with mh_dates

# Check to know how many adjustments are currently in MH
table(mh_dates$ADJUSTMENT) #242 Adjustments

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

# Create a flag for when an adjustment is the first record in a cluster
# First create a new variable for each cluster that has the first date associated with the begining of that cluster

mh_dates2 <- mh_dates %>%
  group_by(CLUSTER) %>%
  mutate(CLUSTER_START = min(EFFECTIVE_DATE)) %>%
  ungroup() %>%
  mutate(FIRST_REG = CLUSTER_START == START_DATE)

# NOW WE HAVE A WAY TO KNOW IF AN ADJUSTMENT IS AT THE START OF THE CLUSTER

mh_dates2 %>%
  filter(FIRST_REG == TRUE,
         ADJUSTMENT == TRUE) %>%
  select(CLUSTER) %>%
  n_distinct()
#13 Cluster

table(mh_dates2$FIRST_REG == TRUE, mh_dates2$ADJUSTMENT == TRUE)
# 15 ADJUSTMENTS AT START OF CLUSTER

# consecutive adjustments might have been given the original start date (?)

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
