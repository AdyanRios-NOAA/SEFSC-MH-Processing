## MH_PRE_PROCESS.R TESTS ------

##SUBSECTOR FORKS ----
# TEMPORARY TEST WHILE WE PREPARE THE CODE IN MH_PRE_PROCESS.R TO INCLUDE
# ALL FMPS. PROVIDES SUBSECTOR INFORMATION INCLUDING SECTOR ID, 
# START DATE OF USE, AND COUNTS RELATED TO THE SUBSECTOR TO INVESTIGATE 
# SUBSECTOR FORKS AND DETERMINE HOW TO BEST ADDRESS THEM. THE CREATED OUTPUT
# IS USED TO TAKE A LOOK AT THE STRUCTURE OF THE SUBSECTORS FOR THE RESPECTIVE FMP
quick_look <- multi_subsector_key %>%
  filter(FMP == "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION") 

# CREATES THE FIELDS OF EXPAND_FROM, EXPAND_TEMP, AND EXPAND_TO
# TO ADDRESS SITATUATIONS WHERE "ALL" IS ONE OF THE SUBSECTORS.
# SUBSECTORS WITH "MANUAL CHECK" IN THE EXPAND_TO FIELD WILL BE ADDRESSED USING THE 
# NEXT CODE.
expand_sector_keys_recGOMRF <- multi_subsector_key %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO",
         SECTOR_USE == "RECREATIONAL") %>%
  select(SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY, subsector_all_used) %>%
  distinct() %>%
  mutate(column_name = "SUBSECTOR",
         expand_from = case_when(subsector_all_used == 1 ~ "ALL"),
         expand_temp = str_remove(SUBSECTOR_KEY, paste0(expand_from, ", ")),
         expand_to = case_when(str_count(expand_temp, ',') >= 1 ~ expand_temp,
                               TRUE ~ "MANUAL CHECK"))

# CODE TO ADDRESS MANUAL CHECK VALUES WITHIN THE EXPAND_TO FIELD.
# BY REMOVING VARIABLES (ZONE AND MANAGEMENT_STATU) FROM THE CLUSTER MATCH,
# WE REDUCED MANAUL FORKS FOR GOM (WILL NEED MANUAL CHECK FOR OTHER FMPS).
# THIS CODE FIXES ANY MANUAL CHECK VALUES TO BE THE PROPER SUBSECTOR EXPANSION.
 expand_sector_keys_recGOMRF_use <- expand_sector_keys_recGOMRF %>%
   mutate(expand_to = case_when(SECTOR_ID == 1110 ~ "FOR-HIRE, PRIVATE",
                                TRUE ~ expand_to))

# CREATE AN EXPANSIONS DATA FRAME TO LATER JOIN TO MH_SECTOR_ID (IN MH_PRE_PROCESS.R).
# COMBINES THE CODE THAT ADDRESSED "MANUAL CHECK" SUBSECTORS WITH THE ORIGINAL CODE
# THAT CREATED THE SECTOR_ID FIELD.
# NOTES FROM ORIGINAL SCRIPT: THIS IS WHERE WE HAD BROKEN THE CODE WE TURNED OFF 
# THE CODE ABOVEBUT FORGOT THAT THE OBJECT ALREADY EISTED IN OUR ENVIRONMENT
#expansions <- expand_sector_keys_recGOMRF_use
expansions <- expand_sector_keys_recGOMRF
 
## MH_PROCESS.R TESTS ------

## RED GROUPER TESTS----
# INVESTIGATING RECREATIONAL CLOSURES FOR RED GROUPER TO CREATE
# A COMPREHENSIVE AND CLEAR TIME SERIES OF CLOSURES
mh_sort %>% filter(SPP_NAME == "CLOSURE/REOPENING: GROUPERS", 
                   FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                   MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "ALLOWABLE SPECIES", "POSSESSION LIMIT")) %>% 
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID) %>%
  distinct(CLUSTER)


test = filter(mh_sort, CLUSTER %in% c(1419, 1424, 1012, 2102, 2103))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
View(test)

#red grouper clusters - 1419, 1424
#CLOSURE/REOPENING: SHALLOW-WATER GROUPER (SWG) clusters - 1012, 2102
#CLOSURE/REOPENING: GROUPERS clusters - 2103

write.csv(test, "redgrouper_closure_clusters.csv")

# TEST TO INVESTIGATE CLUSTERS RELATED TO CLOSURES TO MAKE SURE THAT 
# THE TIME SERIES IS COMPREHENSIVE AND ACCURATE.
test = filter(mh_sort, CLUSTER %in% c(1419, 1424, 1012, 2102, 2103))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE,
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))

write.csv(test, "redsnapper_closure_clusters.csv")

# TESTS TO INVESTIGATE RECREATIONAL RECURRING CLOSURES BY SECTOR TO MAKE 
# SURE THE TIME SERIES IS COMPREHENSIVE AND ACCURATE. THESE INCLUDE 
# SEASONAL, WEEKLY, AND MONTHLY RECURRING RECORDS.
test2 = filter(test, STATUS_TYPE == "RECURRING", SECTOR_USE == "RECREATIONAL")
select(test2, vol, page, CLUSTER, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE,
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE, END_DATE, SUBSECTOR_USE) %>%
  arrange(SUBSECTOR_USE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))

# TESTS TO INVESTIGATE COMMERCIAL RECURRING CLOSURES BY SECTOR TO MAKE 
# SURE THE TIME SERIES IS COMPREHENSIVE AND ACCURATE. THESE INCLUDE 
# SEASONAL, WEEKLY, AND MONTHLY RECURRING RECORDS.
test = filter(mh_sort, CLUSTER %in% c(20, 369, 410,482, 826, 827, 1330))
test3 = filter(test, STATUS_TYPE == "RECURRING", SECTOR_USE == "COMMERCIAL")%>%
  arrange(MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
select(test3, vol, page, CLUSTER, MANAGEMENT_STATUS_USE,
       diff ,diff_days, EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE, 
       CHANGE_DATE, END_DATE, START_MONTH, START_DAY, END_MONTH, END_DAY) 

## RED SNAPPER ----
## CLOSURE TESTS
# INVESTIGATING RECURRING AND ONE TIME CLOSURES FOR RED SNAPPER TO CREATE
# A COMPREHENSIVE AND CLEAR TIME SERIES OF CLOSURES
mh_sort %>% filter(SPP_NAME == "SNAPPER, RED", 
                   FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO",
                   MANAGEMENT_TYPE_USE %in% c("CLOSURE")) %>% 
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID) %>%
  distinct(CLUSTER)

test_rs1 = filter(mh_sort, CLUSTER %in% c(20, 826, 827))
select(test_rs1, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
View(test_rs1)

#RED SNAPPER CLUSTERS - 20, 826, 827

# TESTS TO INVESTIGATE RECREATIONAL RECURRING CLOSURES BY SECTOR TO MAKE 
# SURE THE TIME SERIES IS COMPREHENSIVE AND ACCURATE. THESE INCLUDE 
# SEASONAL, WEEKLY, AND MONTHLY RECURRING RECORDS.
test_rs2 = filter(test_rs1, STATUS_TYPE == "RECURRING", SECTOR_USE == "RECREATIONAL")
test_rs2_view = select(test_rs2, vol, page, CLUSTER, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE,
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE, END_DATE, SUBSECTOR_USE, START_DAY, START_MONTH, END_DAY, END_MONTH, NEVER_IMPLEMENTED) %>%
  arrange(SUBSECTOR_USE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))

# TESTS TO INVESTIGATE COMMERCIAL RECURRING CLOSURES BY SECTOR TO MAKE 
# SURE THE TIME SERIES IS COMPREHENSIVE AND ACCURATE. THESE INCLUDE 
# SEASONAL, WEEKLY, AND MONTHLY RECURRING RECORDS.
test_rs3 = filter(mh_sort, CLUSTER %in% c(20, 369, 410,482, 826, 827, 1330))
test_rs4 = filter(test_rs3, STATUS_TYPE == "RECURRING", SECTOR_USE == "COMMERCIAL")%>%
  arrange(MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
select(test_rs4, vol, page, CLUSTER, MANAGEMENT_STATUS_USE,
       diff ,diff_days, EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE, 
       CHANGE_DATE, END_DATE, START_MONTH, START_DAY, END_MONTH, END_DAY, END_TIME) 

# TESTS TO INVESTIGATE RECREATIONAL ONE TIME CLOSURES TO MAKE SURE THE 
# TIME SERIES IS COMPREHENSIVE AND ACCURATE.
RS_REC_CLO_ONCE = filter(mh_sort, 
                         SECTOR_USE == "RECREATIONAL", 
                         MANAGEMENT_STATUS_USE  == "ONCE", 
                         MANAGEMENT_TYPE_USE == "CLOSURE",
                         NEVER_IMPLEMENTED == "0",
                         CLUSTER %in% c(20, 369, 410,482, 826, 827, 1330)) %>%
  arrange(SUBSECTOR_USE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, ADJUSTMENT, desc(START_DATE), desc(vol), desc(page))
RS_REC_CLO_ONCE_VIEW = select(RS_REC_CLO_ONCE, FR_CITATION, ZONE_USE, 
            VALUE, NEVER_IMPLEMENTED, ADJUSTMENT, EFFECTIVE_DATE, INEFFECTIVE_DATE, 
            START_DATE, CHANGE_DATE, END_DATE, SUBSECTOR_USE, REGULATION_ID)
view(RS_REC_CLO_ONCE_VIEW)

# TESTS TO INVESTIGATE COMMERCIAL ONE TIME CLOSURES TO MAKE SURE THE 
# TIME SERIES IS COMPREHENSIVE AND ACCURATE.
RS_COM_CLO_ONCE = filter(mh_sort, 
                         SECTOR_USE == "COMMERCIAL", 
                         MANAGEMENT_STATUS_USE  == "ONCE", 
                         MANAGEMENT_TYPE_USE == "CLOSURE",
                         CLUSTER %in% c(20)) %>%
  arrange(SUBSECTOR_USE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, ADJUSTMENT, desc(START_DATE), desc(vol), desc(page))
RS_COM_CLO_ONCE_VIEW = select(RS_COM_CLO_ONCE, FR_CITATION, ZONE_USE, 
                              VALUE, NEVER_IMPLEMENTED, ADJUSTMENT, EFFECTIVE_DATE, INEFFECTIVE_DATE, 
                              START_DATE, CHANGE_DATE, END_DATE, START_TIME, END_TIME, SUBSECTOR_USE, REGULATION_ID)
view(RS_COM_CLO_ONCE_VIEW)

## MINIMUM SIZE LIMIT TESTS 
# TEST TO INVESTIGATE RED SNAPPER RECREATIONAL MINIMUM SIZE LIMIT RECORDS 
# TO MAKE SURE THAT THE TIME SERIES IS COMPREHENSIVE AND ACCURATE.
test_rs_rec_min = filter(mh_sort, CLUSTER == 1350)
select(test_rs_rec_min, vol, page, MANAGEMENT_STATUS_USE, SECTOR_USE, ADJUSTMENT, MANAGEMENT_TYPE_USE, END_MONTH, VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE, END_DATE, NEVER_IMPLEMENTED, CLUSTER) %>%
  arrange(MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))

# TEMPORARY FIX TO CREATE AN ADJUSMENT IN CLUSTER 1350 SO THAT THE DATES 
# IN THE TIME SERIES LINE UP CORRECTLY.
mh_sort <- mh_sort %>%
  mutate(ADJUSTMENT = case_when(REGULATION_ID == 792 ~ 1,
                                TRUE ~ ADJUSTMENT))

# TEST TO INVESTIGATE RED SNAPPER COMMERCIAL MINIMUM SIZE LIMIT RECORDS 
# TO MAKE SURE THAT THE TIME SERIES IS COMPREHENSIVE AND ACCURATE.
test_rs_com_min = filter(mh_sort, CLUSTER == 990)
select(test_rs_com_min, vol, page, CLUSTER, SECTOR_USE, ADJUSTMENT, MANAGEMENT_TYPE_USE, VALUE, VALUE_RATE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, NEVER_IMPLEMENTED)

## BAG LIMIT TESTS 
# TEST TO INVESTIGATE RED SNAPPER RECREATION BAG LIMIT RECORDS
# TO MAKE SURE THAT THE TIME SERIES IS COMPREHENSIVE AND ACCURATE.
test_rs_bag = filter(mh_sort, CLUSTER == 991)
select(test_rs_bag, vol, page, CLUSTER, SECTOR_USE, ADJUSTMENT, VALUE, VALUE_RATE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, NEVER_IMPLEMENTED)

# TEST TO INVESTIGATE RED SNAPPER RECREATIONAL CREW BAG LIMIT RECORDS
# TO MAKE SURE THAT THE TIME SERIES IS COMPREHENSIVE AND ACCURATE.
test_rs_crew = filter(mh_sort, CLUSTER == 992)
select(test_rs_crew, vol, page, CLUSTER, SECTOR_USE, ADJUSTMENT, VALUE, VALUE_RATE, diff, diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE, END_DATE, NEVER_IMPLEMENTED)

## CLUSTER INVESTIGATION ----
# CLUSTER 14 - DEALT WITH 14 (BAG LIMIT NEEDS TO BE FOR GOLIATH ALONE 0)
#   - AGGREGATE GROUPER BAG LIMIT WILL BE REMOVED WITH BUG ID 6606
#     WHICH SHOULD SOLVE THE ISSUE WITH THIS CLUSTER
#
# CLUSTER 518 - 518 is a fishing year (new policy, fishing year does not get start year) 
# 
# CLUSTERS 542, 543, 544, 545 - THERE ARE ONLY 21 TIMES WHEN START DATE IS BEFORE EFFECTIVE DATE
# POTENTIAL NEW RULE - IF START DATE IS BEFORE EFFECTIVE DATE; KEEP EFFECTIVE DATE 
# 
# CLUSTERS 1751, 1812 - Keep thinking about how open and closed value affects the sorting

# start day month, year, have their own meaning 
# (effective date should not be overwritten in this case)
# write check for start date happening before effective date (that should not be a thing)
# FOR "FISHING YEAR"(change to reoccurring?)
# COULD TREAT DATES LIKE VALUES FOR CERTAIN MANAGEMENT TYPES (EX ACL, AND FISHING YEAR)

# CONSIDER ADDING DECIMAL YEAR SUMMARY
# DECIMAL_START_YEAR = decimal_date(as.POSIXlt(START_DATE))
# DECIMAL_END_YEAR = decimal_date(as.POSIXlt(END_DATE))

## INITIAL CODE CREATED TO INVESTIGATE CLUSTERS RELATING TO GULF REEF FISH.
# Export for reviewing results for Gulf Reef Fish (SFA)
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

## GITHUB ISSUES ------
# TO DO: 
# ISSUE #10 - QUOTA RELATED ISSUES
# When only start year or end year is provided, diff days is not calculating correctly. 
# ESPECIALLY IN CASES WHERE MULTIPLE YEARLY ALLOCATION
# We may want to process dates differently for quotas because we are using the date fields differently 
# for these management types. Example cluster 1611 (GOM Reef Fish ACL for red snapper)
# May also need to factor to fishing year

# ISSUE #11 - CLOSURE RELATED ISSUES
# - WHEN THERE IS A REOPENING IN THE MIDDLE OF A CLOSURE WE DO NOT CAPTURE
# THE SECOND PART OF THE CLOSURE AFTER THE REOPENING ENDS - NEED TO USE ADJUSTMENT LOGIC
#   - Cluster 950 - Gulf Reef com red snapper has a couple examples of this. One closure was from 4/15/95 - 12/31/95 
# but then a reopening from 11/1 - 11/2. Our process changes the end date of the closure to 10/31, but we are 
# missing the remaining part of the closure from 11/3 - 12/31) When its all closures its ok, but when reopenings 
# happen is when we need to be careful. Incorporate start and end dates into closure days
#   - CLUSTER 1350 - A seasonal closure is split into 3 time periods of seasonal closures by 83 FR 13426. 
# Of the three new seasonal closures, one is given an incorrect end date while the other two are correct. 
# All three should be 12/31/2020.

## MH_SPP_EXPANSION.R TESTS ------

# GULF REEF FISH RED GROUPER (RG)
test_rg <- mh_final %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO", COMMON_NAME_USE == 'GROUPER, RED')
test_rg2 <- test_rg %>%
  group_by(SECTOR_USE, MANAGEMENT_TYPE_USE) %>%
  summarise(N = n())
view(test_rg2)
# RG Commercial Closures
test_rg3 <- test_rg %>%
  filter(SECTOR_USE == 'COMMERCIAL', MANAGEMENT_TYPE_USE == 'CLOSURE')
# CLUSTER IDs 38  384  385  386  387  785  797  805  806 1012 1424
# CLUSTERS 38-806 are for specific zone/subsector closures for all Gulf Reef fish species
# CLUSTERS 1012 and 1424 are for red grouper and aggregate with RG
test_rg3_clusters <- test_rg3 %>%
  group_by(CLUSTER, SUBSECTOR_USE, SPP_TYPE, SPP_NAME, ZONE_USE) %>%
  summarise(N = n())
view(test_rg3_clusters)
# WORKING FOR 1012 and 1424
test_rg4 <- test_rg3 %>%
  filter(CLUSTER %in% c(1012, 1424)) %>%
  select(vol, page, MANAGEMENT_STATUS_USE, SECTOR_USE, MANAGEMENT_TYPE_USE, SPP_NAME, 
         VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE, END_DATE, 
         NEVER_IMPLEMENTED, START_DATE2, END_DATE2) %>%
  arrange(MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
view(test_rg4)

# EXAMPLE WHERE START DATE ADJUSTED BECAUSE OF SPP AGGREGATE ADDED DATE
mh_final %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO", MANAGEMENT_TYPE_USE == 'CLOSURE',
         SPP_NAME == 'ALL', SECTOR_USE == 'COMMERCIAL', IMP_START_DATE == 1) %>%
  select(CLUSTER) %>%
  distinct()

# CLUSTER 384
# Adjusted start date working (adjusted for 2 species)
# Added a condition where removed spp date == start date to flag those records to remove
test_imps <- mh_final %>%
  filter(CLUSTER == '384') %>%
  #filter(IMP_START_DATE == 1) %>%
  filter(ZONE_USE == 'REEF FISH LONGLINE AND BUOY GEAR RESTRICTED AREA') %>%
  select(vol, page, SPP_NAME, COMMON_NAME_USE, 
         VALUE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE, START_DATE, END_DATE, 
         ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE2, END_DATE2, RM_SPP) %>%
  arrange(desc(START_DATE2), desc(vol), desc(page)) 
view(test_imps)

#CLUSTER 785
# Adjusted start date and end date working correctly as well as spp removal flag
test_imps2 <- mh_final %>%
  filter(CLUSTER == '785') %>%
  #filter(IMP_START_DATE == 1) %>%
  filter(ZONE_USE == 'REEF FISH LONGLINE AND BUOY GEAR RESTRICTED AREA') %>%
  select(vol, page, SPP_NAME, COMMON_NAME_USE, 
         VALUE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE, START_DATE, END_DATE, 
         ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE2, END_DATE2, RM_SPP) %>%
  arrange(desc(START_DATE2), desc(vol), desc(page)) 
view(test_imps2)

#CLUSTER 797
# Adjusted start date and end date working correctly 
test_imps3 <- mh_final %>%
  filter(CLUSTER == '797') %>%
  #filter(IMP_START_DATE == 1) %>%
  filter(ZONE_USE == 'REEF FISH STRESSED AREA') %>%
  select(vol, page, SPP_NAME, COMMON_NAME_USE, 
         VALUE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE, START_DATE, END_DATE, 
         ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE2, END_DATE2, RM_SPP) %>%
  arrange(desc(START_DATE2), desc(vol), desc(page)) 
view(test_imps3)

#CLUSTER 805
# Adjusted start date and end date working correctly 
test_imps4 <- mh_final %>%
  filter(CLUSTER == '805') %>%
  #filter(IMP_START_DATE == 1) %>%
  filter(ZONE_USE == 'REEF FISH STRESSED AREA') %>%
  select(vol, page, SPP_NAME, COMMON_NAME_USE, 
         VALUE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE, START_DATE, END_DATE, 
         ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE2, END_DATE2, RM_SPP) %>%
  arrange(desc(START_DATE2), desc(vol), desc(page)) 
view(test_imps4)

#CLUSTER 806
# Adjusted start date and end date working correctly 
test_imps5 <- mh_final %>%
  filter(CLUSTER == '806') %>%
  #filter(IMP_START_DATE == 1) %>%
  filter(ZONE_USE == 'REEF FISH STRESSED AREA') %>%
  select(vol, page, SPP_NAME, COMMON_NAME_USE, 
         VALUE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE, START_DATE, END_DATE, 
         ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE2, END_DATE2, RM_SPP) %>%
  arrange(desc(START_DATE2), desc(vol), desc(page)) 
view(test_imps5)

# GULF REEF FISH MISTY GROUPER (MG)
test_mg <- mh_final %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO", COMMON_NAME_USE == 'GROUPER, MISTY')
test_mg2 <- test_mg %>%
  group_by(SECTOR_USE, MANAGEMENT_TYPE_USE) %>%
  summarise(N = n())
view(test_mg2)
# MG Commercial Closures
test_mg3 <- test_mg %>%
  filter(SECTOR_USE == 'COMMERCIAL', MANAGEMENT_TYPE_USE == 'CLOSURE')
# CLUSTER IDs 38  384  385  386  387  785  797  805  806 1045
# CLUSTERS 38-806 are for specific zone/subsector closures for all Gulf Reef fish species
# CLUSTERS 1045 is Deepwater Grouper specific for GOM: ALL
test_mg3_clusters <- test_mg3 %>%
  group_by(CLUSTER, SUBSECTOR_USE, SPP_TYPE, SPP_NAME, ZONE_USE) %>%
  summarise(N = n())
view(test_mg3_clusters)
# WORKING FOR 1045
test_mg4 <- test_mg3 %>%
  filter(CLUSTER %in% c(1045)) %>%
  select(vol, page, MANAGEMENT_STATUS_USE, SECTOR_USE, MANAGEMENT_TYPE_USE, SPP_NAME, 
         VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE, END_DATE, 
         NEVER_IMPLEMENTED, START_DATE2, END_DATE2) %>%
  arrange(MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
view(test_mg4)

# EXAMPLE WHERE START DATE ADJUSTED BECAUSE OF SPP AGGREGATE ADDED DATE
mh_final %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO", MANAGEMENT_TYPE_USE == 'BAG LIMIT',
         SPP_NAME == 'ALL', SECTOR_USE == 'RECREATIONAL', IMP_START_DATE == 1) %>%
  select(CLUSTER) %>%
  distinct()

# CLUSTER 999
test_imps6 <- mh_final %>%
  filter(CLUSTER == '999') %>%
  #filter(IMP_START_DATE == 1) %>%
  select(vol, page, SPP_NAME, COMMON_NAME_USE, 
         VALUE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE, START_DATE, END_DATE, 
         ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE2, END_DATE2, RM_SPP) %>%
  arrange(desc(START_DATE2), desc(vol), desc(page)) 
view(test_imps6)

chk_start <- mh_final %>%
  filter(RM_SPP == 0) %>%
  group_by(CLUSTER, MANAGEMENT_TYPE, START_DATE, COMMON_NAME_USE, START_YEAR, VALUE) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) 

# Check (cluster 37 - min size limit hogfish)
chk <- mh_final %>%
  filter(CLUSTER == 37) %>%
  select(CLUSTER, SECTOR_ID, REGULATION_ID, FR_CITATION, SPP_NAME, COMMON_NAME_USE, SECTOR_USE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE, END_DATE, VALUE,
         RM_SPP, REG_CHANGE, REG_REMOVED, MULTI_REG, GENERAL, COMPLEX, ADJUSTMENT)

# Check (cluster 38 - min size limit hogfish)
chk2 <- mh_final %>%
  filter(CLUSTER == 38) %>%
  select(CLUSTER, SECTOR_ID, REGULATION_ID, FR_CITATION, SPP_NAME, COMMON_NAME_USE, SECTOR_USE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE, END_DATE, VALUE,
         RM_SPP, REG_CHANGE, REG_REMOVED, MULTI_REG, GENERAL, COMPLEX, ADJUSTMENT)

# Check (cluster 2537)
chk <- mh_final %>%
  filter(CLUSTER == 2537)

# Check (cluster 31 and 2724) 
chk <- mh_final %>%
  filter(COMMON_NAME_USE == 'GROUPER, GAG', MANAGEMENT_TYPE == 'BAG LIMIT', FMP == 'REEF FISH RESOURCES OF THE GULF OF MEXICO') %>%
  select(CLUSTER, SECTOR_ID, REGULATION_ID, FR_CITATION, SPP_NAME, COMMON_NAME_USE, 
         EFFECTIVE_DATE, INEFFECTIVE_DATE, ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE, END_DATE, VALUE,
         #IMP_START_DATE, IMP_END_DATE, NEVER_IMPLEMENTED, 
         RM_SPP) %>%
  arrange(CLUSTER, EFFECTIVE_DATE)

# To do: check the comment below, look into this
# REMOVE ONE OF THE RECORDS WHEN THERE ARE TWO WITH THE SAME START DATE AND VALUE IN A CLUSTER