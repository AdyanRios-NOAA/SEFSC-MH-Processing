# 3
# Process clustering
  # MATCH CLUSTERS AND ORDER RECORDS

# SUMMARY OF STEPS ####

# 1  GROUP DATA INTO PROCESSING CLUSTERS
#   1A DEFINE MATCH VARIABLES
#   1B COUNT HOW MANY CLUSTERS TO PROCESS
#   1C NOTE WHEN THERE ARE MULTIPLE RECORDS PER CLUSTER PER FR ACTIVE AT THE SAME TIME
# 2 DEFINE VARIABLES THAT DETECT REG CHANGES BASED ON STATUS TYPE
# 3 CHECK FOR CONSECUTIVELY REPEATED REGS
#   4A CREATE EMPTY LIST TO STORE DETECTED REG CHANGES
#   4B SORT BY FR_CITATION WITHIN CLUSTER (CHANGE THIS TO A FUNCTION!!!)
# 4 ADD FLAGS FOR REMOVAL, GENERAL(AKA FYI)/DETAILED, COMPLEX REGS, DEFINE SETTINGS THAT IMPLY A POTENTIALLY REDUNDANT RECORD
# 5 FILTER TO EXPLORE POTENTIALLY REDUNDANT RECORDS
# 6 ADD END DATE TO SORTED RECORDS
# 7 EXPORT DATA


# 1 GROUP DATA INTO PROCESSING CLUSTERS ####

# 1A DEFINE MATCH VARIABLES ####

cluster.match <- c("MANAGEMENT_TYPE_USE",
                   "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
                   "SECTOR_USE", "SUBSECTOR_USE", "REGION",
                   "SPP_NAME")

# READ IN EXISTING CLUSTERS

cluster_files <- dir(here('data/interim/clusters'), full.names = TRUE)

existing_clusters <- cluster_files %>%
  map(read_csv) %>% 
  reduce(rbind)

# GET STARTING NUMBER OF CURRENT CLUSTERS FOR REFERENCE

clusters_max = max(existing_clusters$CLUSTER)

# ASSIGN NUMBERS TO NEW CLUSTERS

new_clusters <- mh_ready %>%
  select(one_of(cluster.match)) %>%
  distinct() %>%
  anti_join(existing_clusters) %>%
  mutate(CLUSTER = (1:n() + clusters_max)[seq_len(nrow(.))])

# EXPORT NEW CLUSTERS
if(length(new_clusters$CLUSTER) > 0) {
  write_csv(new_clusters, 
            here("data/interim/clusters", paste0("mh_unique_clusters_", format(Sys.Date(), "%d%b%Y"),".csv")))
}

# MERGE OLD AND NEW CLUSTERS
unique_clusters <- rbind(existing_clusters, new_clusters)

# # GROUP DATA INTO PROCESSING COLLECTIONS ####
# 
# collection.match <- c("MANAGEMENT_TYPE_USE",
#                       "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
#                       "SECTOR_USE", "SUBSECTOR", "REGION", "ZONE_USE",
#                       "SPP_NAME", "COMMON_NAME_USE")
# 
# # READ IN EXISTING COLLECTIONS
# collection_files <- dir(here('data/interim/collections'), full.names = TRUE)
#   
# existing_collections <- collection_files %>%
#   map(read_csv) %>% 
#   reduce(rbind)
# 
# # GET STARTING NUMBER OF CURRENT COLLECTIONS FOR REFERENCE
# collections_max = max(existing_collections$COLLECTION)
#   
# # ASSIGN NUMBERS TO NEW COLLECTIONS  ####
# 
# new_collections <- mh_ready %>%
#   select(one_of(collection.match)) %>%
#   distinct() %>%
#   anti_join(existing_collections) %>%
#   mutate(COLLECTION = (1:n() + collections_max)[seq_len(nrow(.))])
# 
# # EXPORT NEW COLLECTIONS
# 
# write.csv(new_collections, here("data/interim/collections", paste0("mh_unique_collections_", format(Sys.Date(), "%d%b%Y"),".csv")), row.names = FALSE)


# JOIN IN CLUSTERS AND COLLECTIONS

mh_prep <- mh_ready %>%
  left_join(., unique_clusters, by = cluster.match) %>%
  # left_join(., unique_collections, by = collection.match) %>%
  mutate(REG_CHANGE = 1)

# 1C NOTE WHEN THERE ARE MULTIPLE RECORDS PER CLUSTER PER FR ACTIVE AT THE SAME TIME####

# FIND CASES AND CREATE FLAG
multi_reg <- mh_prep %>%
  group_by(CLUSTER, FR_CITATION) %>%
  summarize(MULTI_REG = as.numeric(duplicated(FR_CITATION))) %>%
  filter(MULTI_REG == 1) %>%
  data.frame() %>%
  distinct()
dim(multi_reg)

# JOIN FLAGGED CASES INTO FULL DATA
mh_prep_use <- mh_prep %>%
  # IDENTIFY REGULATIONS THAT ARE MULTI_REGS (SAME FR WITHIN A CLUSTER)
  left_join(., multi_reg, by = c("FR_CITATION", "CLUSTER")) %>%
  # REPLACE ALL NAs WITH 0
  mutate_at("MULTI_REG", ~replace(., is.na(.), 0)) %>%
  # DOES THIS CLUSTER HAVE ANY INSTANCES OF A MULTI_REG
  mutate(MULTI_REG_CLUSTER = as.numeric(CLUSTER %in% multi_reg$CLUSTER)) 

# INVESTIGATE WHICH MANAGEMENT TYPES AND FMPS TEND TO HAVE MULTI_REGS

# 2 DEFINE VARIABLES THAT DETECT REG CHANGES BASED ON STATUS TYPE ####
# SFA added ineffective date to accommodate issue in cluster 657
# IF ANYTHING CHANGES IN ANY OF THESE VARIABLES THEN THAT MEANS A REG CHANGE
# var_track <- c("INEFFECTIVE_DATE", "START_DAY_RECURRING",	'START_MONTH_RECURRING',	#'START_YEAR_RECURRING',
#                'START_TIME_RECURRING',	'START_DAY_OF_WEEK_RECURRING',
#                "END_DAY_RECURRING",	"END_MONTH_RECURRING",	#'END_YEAR_RECURRING',
#                'END_TIME_RECURRING',	'END_DAY_OF_WEEK_RECURRING',
#                "VALUE", "VALUE_UNITS", "VALUE_TYPE", "VALUE_RATE",
#                "INEFFECTIVE_DATE", "FLAG", "MULTI_REG")
# 
# # 3 CHECK FOR CONSECUTIVELY REPEATED REGS ####
# 
# # 3A CREATE EMPTY LIST TO STORE DETECTED REG CHANGES  ####
# 
# mh_detect <- list()
# 
# # 3B SORT BY FR_CITATION WITHIN CLUSTER (CHANGE THIS TO A FUNCTION!!!) ####
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
# # 4 FLATTEN LIST ADD FLAG THAT IMPLY A REDUNDANT RECORD ####
# 
# mh_detect <- mh_detect %>%
#   bind_rows() %>%
#   mutate(REDUNDANT = (REG_CHANGE + REG_REMOVED + MULTI_REG + as.numeric(FLAG == "YES") + as.numeric(MANAGEMENT_TYPE == "IGNORABLE")) == 0)
# 
# # 5 FILTER TO JUST REDUNDANT RECORDS ####
# 
# mh_redundant <- mh_detect %>%
#   filter(REDUNDANT == 1)

# SHORT CUT TO NAME USED IN NEXT STEP
mh_detect <- mh_prep_use

# TEMP FIX FOR TESTING ADJUSMENT IN CLUSTER 1733
mh_detect <- mh_detect %>%
  mutate(ADJUSTMENT = case_when(REGULATION_ID == 792 ~ 1,
                                TRUE ~ ADJUSTMENT))

# 6 ADD END DATE TO SORTED RECORDS ####
mh_sort <- mh_detect %>%
  # filter(EFFECTIVE_DATE <= end_timeseries, REDUNDANT == 0) %>%
  # ONLY PROCESS REGULATIONS THAT HAPPEN BEFORE THE TERMINAL DATE OF THE PROCESSING PERIOD
  filter(EFFECTIVE_DATE <= end_timeseries) %>%
  arrange(CLUSTER, desc(START_DATE), desc(vol), desc(page)) %>%
  #group_by(CLUSTER, ADJUSTMENT,) %>%
  group_by(CLUSTER, MANAGEMENT_STATUS_USE, ADJUSTMENT) %>%
  mutate(diff = lag(START_DATE) - START_DATE,
         diff_days = as.numeric(diff, units = 'days') - 1,
         CHANGE_DATE = case_when(is.na(diff_days) ~ end_timeseries,
                                 TRUE ~ START_DATE + diff_days),
         CHANGE_DATE = case_when(diff_days == -1 ~ lag(CHANGE_DATE),
                                 TRUE ~ CHANGE_DATE),
         END_DATE = case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                !is.na(END_DAY) &
                                !is.na(END_MONTH) &
                                !is.na(END_YEAR) ~ as.Date(paste(END_MONTH, END_DAY, END_YEAR, sep = "/"), "%m/%d/%Y")),
         END_DATE1 = case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                !is.na(END_DAY) &
                                !is.na(END_MONTH) &
                                !is.na(END_YEAR) ~ as.Date(paste(END_MONTH, END_DAY, END_YEAR, sep = "/"), "%m/%d/%Y")),
         END_DATE = case_when(!is.na(END_DATE) ~ END_DATE,
                              is.na(END_YEAR) & !is.na(INEFFECTIVE_DATE) ~ INEFFECTIVE_DATE,
                              TRUE ~ CHANGE_DATE),
         END_DATE = case_when(CHANGE_DATE > END_DATE & !is.na(END_DATE) ~ END_DATE,
                              CHANGE_DATE > INEFFECTIVE_DATE & !is.na(INEFFECTIVE_DATE) ~ END_DATE,
                              TRUE ~ CHANGE_DATE),
         ROUNDED_START_YEAR = format(round_date(START_DATE, "year"),"%Y"),
         ROUNDED_END_YEAR = case_when(as.numeric(format(END_DATE, "%m")) >= 7 ~ as.numeric(format(round_date(END_DATE, "year"),"%Y"))-1,
                                      as.numeric(format(END_DATE, "%m")) < 7 ~ as.numeric(format(round_date(END_DATE, "year"),"%Y"))),
         NEVER_IMPLEMENTED = case_when(diff_days <= -1 ~ 1,
                                       TRUE ~ 0)) %>%
  #bind_rows(., mh_redundant) %>%
  data.frame()

# INVESTIGATING RECREATIONAL CLOSURES FOR RED SNAPPER
mh_sort %>% filter(SPP_NAME == "SNAPPER, RED",
                   FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                   MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "ALLOWABLE SPECIES", "POSSESSION LIMIT")) %>% 
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID) %>%
  distinct(CLUSTER)

test = filter(mh_sort, CLUSTER %in% c(20, 369, 410,482, 826, 827, 1330))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE,
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE) %>%
arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))

write.csv(test, "redsnapper_closure_clusters.csv")

test2 = filter(test, STATUS_TYPE == "RECURRING", SECTOR_USE == "RECREATIONAL")
select(test2, vol, page, CLUSTER, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE,
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE, END_DATE, SUBSECTOR_USE) %>%
  arrange(SUBSECTOR_USE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))

test = filter(mh_sort, CLUSTER %in% c(20, 369, 410,482, 826, 827, 1330))
test3 = filter(test, STATUS_TYPE == "RECURRING", SECTOR_USE == "COMMERCIAL")%>%
  arrange(MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
select(test3, vol, page, CLUSTER, MANAGEMENT_STATUS_USE,
       diff ,diff_days, EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE, 
       CHANGE_DATE, END_DATE, START_MONTH, START_DAY, END_MONTH, END_DAY) 

test = filter(mh_sort, CLUSTER %in% c(20, 369, 410,482, 826, 827, 1330))
test4 = filter(test, SECTOR_USE == "RECREATIONAL", MANAGEMENT_STATUS_USE  == "ONCE", MANAGEMENT_TYPE_USE == "CLOSURE") %>%
  arrange(SUBSECTOR_USE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
select(test4, vol, page, CLUSTER, MANAGEMENT_TYPE_USE,
       VALUE, diff ,diff_days, EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE, CHANGE_DATE, END_DATE1, END_DATE, SUBSECTOR_USE)[50:64,]






# RECREATIONAL MINIMUM SIZE LIMIT
test = filter(mh_sort, CLUSTER == 1733)
select(test, ID,vol, page, MANAGEMENT_STATUS_USE, SECTOR_USE, ADJUSTMENT, MANAGEMENT_TYPE_USE, END_MONTH, VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, NEVER_IMPLEMENTED) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))

# COMMERCIAL MINIMUM SIZE LIMIT
test = filter(mh_sort, CLUSTER == 1304)
select(test, vol, page, CLUSTER, SECTOR_USE, ADJUSTMENT, MANAGEMENT_TYPE_USE, VALUE, VALUE_RATE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, NEVER_IMPLEMENTED)

# RECREATION BAG LIMIT 1305
test = filter(mh_sort, CLUSTER == 1305)
select(test, vol, page, CLUSTER, SECTOR_USE, ADJUSTMENT, VALUE, VALUE_RATE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, NEVER_IMPLEMENTED)


# 14 - DEALT WITH 14 (BAG LIMIT NEEDS TO BE FOR GOLIATH ALONE 0)
# 518 - 518 is a fishing year (new policy, fishing year does not get start year) 
# 542, 543, 544, 545 - THERE ARE ONLY 21 TIMES WHEN START DATE IS BEFORE EFFECTIVE DATE
  # POTENTIAL NEW RULE - IF START DATE IS BEFORE EFFECTIVE DATE; KEEP EFFECTIVE DATE 
# 1751, 1812 - Keep thinking about how open and closed value affects the sorting


# start day month, year, have their own meaning 
  # (effective date should not be overwritten in this case)
  # write check for start date happening before effective date (that should not be a thing)
  # FOR "FISHING YEAR"(change to reoccurring?)
  # COULD TREAT DATES LIKE VALUES FOR CERTAIN MANAGEMENT TYPES (EX ACL, AND FISHING YEAR)

# CONSIDER ADDING DECIMAL YEAR SUMMARY
# DECIMAL_START_YEAR = decimal_date(as.POSIXlt(START_DATE))
# DECIMAL_END_YEAR = decimal_date(as.POSIXlt(END_DATE))



# ## Export for reviewing results for Gulf Reef Fish (SFA)
# mh_redundant_GOMRF <- mh_detect %>%
#   filter(REDUNDANT == 1, FMP == 'REEF FISH RESOURCES OF THE GULF OF MEXICO')
# mh_gulf_reef_review <- mh_sort2 %>% filter(FMP == 'REEF FISH RESOURCES OF THE GULF OF MEXICO') %>%
#   bind_rows(., mh_redundant_GOMRF) %>%
#   select(., REGION, REGULATION_ID, FR_CITATION, SECTOR_USE, SUBSECTOR, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE, MANAGEMENT_STATUS, ZONE_USE,
#          SPP_NAME, COMMON_NAME_USE, EFFECTIVE_DATE, INEFFECTIVE_DATE,
#          START_DAY_RECURRING, START_MONTH_RECURRING, START_TIME_RECURRING, START_DAY_OF_WEEK_RECURRING,
#          END_DAY_RECURRING, END_MONTH_RECURRING, END_TIME_RECURRING, END_DAY_OF_WEEK_RECURRING,
#          MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, CLUSTER, COLLECTION, GENERAL, COMPLEX,
#          VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE,
#          REG_CHANGE, MULTI_REG, MULTI_REG_CLUSTER, REG_REMOVED, REDUNDANT, NEVER_IMPLEMENTED, diff, diff_days, CHANGE_DATE,
#          vol, page, START_DATE, END_DATE, IMP_START_DATE, IMP_END_DATE) %>%
#   arrange(., CLUSTER, START_DATE, vol, page)
# 
# write.csv(mh_sort2, paste0("./Output/MHprocessed_clean_", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)


# FOLLOW UP: REG OFF, CHECK CLUSTERS
# FOLLOW UP: MULTI REG, CHECK CLUSTERS




