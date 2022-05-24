## SATL SNAPPER GROUPER SIMPLICITY ------

## QUANTIFYING SIMPLICITY FOR ALL REGULATIONS
mh_sort5 <- mh_sort %>%
  filter(FMP == "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION")
dim(mh_sort5) # 1427

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (1255/1427 records; 88%)
clus_subsector <- mh_sort5 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (1401/1427 records; 98%)
clus_adj <- mh_sort5 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (767/1427 records; 54%)
clus_multi <- mh_sort5 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (1288/1427 records; 90%)
clus_off <- mh_sort5 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (979/1427 records; 69%)
clus_flag <- mh_sort5 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (1317/1427 records; 92%)
clus_once <- mh_sort5 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (1124/1427 records; 79%)
clus_time <- mh_sort5 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (1427/1427 records; 100%)
clus_never <- mh_sort5 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (1137/1427 records; 80%)
clus_zone <- mh_sort5 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (1325/1427 records; 93%)
clus_species <- mh_sort5 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (1325/1427 records; 93%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort5 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (128/1427; 9%)
simple_all <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER) %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER) %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER) %>%
  filter(!CLUSTER %in% clus_off$CLUSTER) %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER) %>%
  filter(!CLUSTER %in% clus_once$CLUSTER) %>%
  filter(!CLUSTER %in% clus_time$CLUSTER) %>%
  filter(!CLUSTER %in% clus_never$CLUSTER) %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER) %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>% # NOT WORKING AS INTENDED
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_all)


# ALL SIMPLE (887/1427; 62%)
simple_test <- mh_sort5 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER) %>%
  #filter(!CLUSTER %in% clus_adj$CLUSTER) %>% # ADJUSTMENTS
  #filter(!CLUSTER %in% clus_multi$CLUSTER) %>% # MULTI REG
  #filter(!CLUSTER %in% clus_off$CLUSTER) %>% # TURN OFF
  #filter(!CLUSTER %in% clus_flag$CLUSTER) %>% # FLAGS
  filter(!CLUSTER %in% clus_once$CLUSTER) %>%
  #filter(!CLUSTER %in% clus_time$CLUSTER) %>% # TIME
  #filter(!CLUSTER %in% clus_never$CLUSTER) %>% # NEVER IMPLEMENT
  filter(!CLUSTER %in% clus_zone$CLUSTER) %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_test)

## QUANTIFYING SIMPLICITY FOR BAG LIMITS ####
mh_sort6 <- mh_sort %>%
  filter(FMP == "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION") 

mh_sort6 <- mh_sort6 %>%
  filter(MANAGEMENT_TYPE %in% c("BAG LIMIT", "BAG LIMIT ADJUSTMENT"))
dim(mh_sort6) # 34

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (34/34 records; 100%)
clus_subsector <- mh_sort6 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (29/34 records; 41%)
clus_adj <- mh_sort6 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (31/34 records; 91%)
clus_multi <- mh_sort6 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (34/34 records; 100%)
clus_off <- mh_sort6 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (30/34 records; 88%)
clus_flag <- mh_sort6 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (29/34 records; 85%)
clus_once <- mh_sort6 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (34/34 records; 100%)
clus_time <- mh_sort6 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (34/34 records; 100%)
clus_never <- mh_sort6 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (31/34 records; 91%)
clus_zone <- mh_sort6 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (25/34 records; 74%)
clus_species <- mh_sort6 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (25/34 records; 74%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort6 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (12/34; 35%)
simple_all <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER) %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER) %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER) %>%
  filter(!CLUSTER %in% clus_off$CLUSTER) %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER) %>%
  filter(!CLUSTER %in% clus_once$CLUSTER) %>%
  filter(!CLUSTER %in% clus_time$CLUSTER) %>%
  filter(!CLUSTER %in% clus_never$CLUSTER) %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER) %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>% # NOT WORKING AS INTENDED
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_all)


# ALL SIMPLE (17/34; 50%)
simple_test <- mh_sort6 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER) %>%
  #filter(!CLUSTER %in% clus_adj$CLUSTER) %>% # ADJUSTMENTS
  #filter(!CLUSTER %in% clus_multi$CLUSTER) %>% # MULTI REG
  #filter(!CLUSTER %in% clus_off$CLUSTER) %>% # TURN OFF
  #filter(!CLUSTER %in% clus_flag$CLUSTER) %>% # FLAGS
  filter(!CLUSTER %in% clus_once$CLUSTER) %>%
  #filter(!CLUSTER %in% clus_time$CLUSTER) %>% # TIME
  #filter(!CLUSTER %in% clus_never$CLUSTER) %>% # NEVER IMPLEMENT
  filter(!CLUSTER %in% clus_zone$CLUSTER) %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_test)

## QUANTIFYING SIMPLICITY FOR SIZE LIMITS ####
mh_sort7 <- mh_sort %>%
  filter(FMP == "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION") 

mh_sort7 <- mh_sort7 %>%
  filter(MANAGEMENT_TYPE %in% c("MINIMUM SIZE LIMIT", "MAXIMUM SIZE LIMIT"))
dim(mh_sort7) # 124

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (124/124 records; 100%)
clus_subsector <- mh_sort7 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (124/124 records; 100%)
clus_adj <- mh_sort7 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (101/124 records; 82%)
clus_multi <- mh_sort7 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (100/124 records; 81%)
clus_off <- mh_sort7 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (15/124 records; 12%)
clus_flag <- mh_sort7 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (124/124 records; 100%)
clus_once <- mh_sort7 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (124/124 records; 100%)
clus_time <- mh_sort7 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (124/124 records; 100%)
clus_never <- mh_sort7 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (99/124 records; 80%)
clus_zone <- mh_sort7 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (124/124 records; 100%)
clus_species <- mh_sort7 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (124/124 records; 100%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort7 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (1/124; 0.8%)
simple_all <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER) %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER) %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER) %>%
  filter(!CLUSTER %in% clus_off$CLUSTER) %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER) %>%
  filter(!CLUSTER %in% clus_once$CLUSTER) %>%
  filter(!CLUSTER %in% clus_time$CLUSTER) %>%
  filter(!CLUSTER %in% clus_never$CLUSTER) %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER) %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>% # NOT WORKING AS INTENDED
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_all)


# ALL SIMPLE (99/124; 80%)
simple_test <- mh_sort7 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER) %>%
  #filter(!CLUSTER %in% clus_adj$CLUSTER) %>% # ADJUSTMENTS
  #filter(!CLUSTER %in% clus_multi$CLUSTER) %>% # MULTI REG
  #filter(!CLUSTER %in% clus_off$CLUSTER) %>% # TURN OFF
  #filter(!CLUSTER %in% clus_flag$CLUSTER) %>% # FLAGS
  filter(!CLUSTER %in% clus_once$CLUSTER) %>%
  #filter(!CLUSTER %in% clus_time$CLUSTER) %>% # TIME
  #filter(!CLUSTER %in% clus_never$CLUSTER) %>% # NEVER IMPLEMENT
  filter(!CLUSTER %in% clus_zone$CLUSTER) %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_test)

## QUANTIFYING SIMPLICITY FOR CLOSURES ####
mh_sort8 <- mh_sort %>%
  filter(FMP == "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION") 

mh_sort8 <- mh_sort8 %>%
  filter(MANAGEMENT_TYPE %in% c("CLOSURE", "REOPENING"))
dim(mh_sort8) #309

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (284/309 records; 92%)
clus_subsector <- mh_sort8 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (126/309 records; 41%)
clus_adj <- mh_sort8 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (161/309 records; 52%)
clus_multi <- mh_sort8 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (303/309 records; 98%)
clus_off <- mh_sort8 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (215/309 records; 70%)
clus_flag <- mh_sort8 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (249/309 records; 81%)
clus_once <- mh_sort8 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (50/309 records; 16%)
clus_time <- mh_sort8 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (309/309 records; 100%)
clus_never <- mh_sort8 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (184/309 records; 60%)
clus_zone <- mh_sort8 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (278/309 records; 90%)
clus_species <- mh_sort8 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (278/309 records; 90%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort8 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (1/309; 0.3%)
simple_all <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER) %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER) %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER) %>%
  filter(!CLUSTER %in% clus_off$CLUSTER) %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER) %>%
  filter(!CLUSTER %in% clus_once$CLUSTER) %>%
  filter(!CLUSTER %in% clus_time$CLUSTER) %>%
  filter(!CLUSTER %in% clus_never$CLUSTER) %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER) %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>% # NOT WORKING AS INTENDED
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_all)


# ALL SIMPLE (126/309; 41%)
simple_test <- mh_sort8 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER) %>%
  #filter(!CLUSTER %in% clus_adj$CLUSTER) %>% # ADJUSTMENTS
  #filter(!CLUSTER %in% clus_multi$CLUSTER) %>% # MULTI REG
  #filter(!CLUSTER %in% clus_off$CLUSTER) %>% # TURN OFF
  #filter(!CLUSTER %in% clus_flag$CLUSTER) %>% # FLAGS
  filter(!CLUSTER %in% clus_once$CLUSTER) %>%
  #filter(!CLUSTER %in% clus_time$CLUSTER) %>% # TIME
  #filter(!CLUSTER %in% clus_never$CLUSTER) %>% # NEVER IMPLEMENT
  filter(!CLUSTER %in% clus_zone$CLUSTER) %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_test)