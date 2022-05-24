## GULF REEF FISH SIMPLICITY ------

## QUANTIFYING SIMPLICITY FOR ALL REGULATIONS ####
mh_sort1 <- mh_sort %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO")
dim(mh_sort1) # 1411

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (1131/1411 records; 80%)
clus_subsector <- mh_sort1 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (1213/1411 records; 86%)
clus_adj <- mh_sort1 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (633/1411 records; 45%)
clus_multi <- mh_sort1 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (1266/1411 records; 90%)
clus_off <- mh_sort1 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (836/1411 records; 59%)
clus_flag <- mh_sort1 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (1342/1411 records; 95%)
clus_once <- mh_sort1 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (1083/1411 records; 77%)
clus_time <- mh_sort1 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (1282/1411 records; 91%)
clus_never <- mh_sort1 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (1236/1411 records; 88%)
clus_zone <- mh_sort1 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (1241/1411 records; 88%)
clus_species <- mh_sort1 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (1224/1411 records; 87%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort1 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort1 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (135/1411; 10%)
simple_all <- mh_sort1 %>%
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


# ALL SIMPLE (775/1411; 55%)
simple_test <- mh_sort1 %>%
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
mh_sort2 <- mh_sort %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO") 

mh_sort2 <- mh_sort2 %>%
  filter(MANAGEMENT_TYPE %in% c("BAG LIMIT", "BAG LIMIT ADJUSTMENT"))
dim(mh_sort2) # 44

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (44/44 records; 100%)
clus_subsector <- mh_sort2 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (18/44 records; 41%)
clus_adj <- mh_sort2 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (44/44 records; 100%)
clus_multi <- mh_sort2 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (41/44 records; 93%)
clus_off <- mh_sort2 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (15/44 records; 34%)
clus_flag <- mh_sort2 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (44/44 records; 100%)
clus_once <- mh_sort2 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (36/44 records; 82%)
clus_time <- mh_sort2 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (44/44 records; 100%)
clus_never <- mh_sort2 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (41/44 records; 93%)
clus_zone <- mh_sort2 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (34/44 records; 77%)
clus_species <- mh_sort2 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (34/44 records; 77%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort2 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (6/44; 14%)
simple_all <- mh_sort2 %>%
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


# ALL SIMPLE (31/44; 70%)
simple_test <- mh_sort2 %>%
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

## QUANTIFYING SIMPLICITY FOR 'SIZE LIMITS ####
mh_sort3 <- mh_sort %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO") 

mh_sort3 <- mh_sort3 %>%
  filter(MANAGEMENT_TYPE %in% c("MINIMUM SIZE LIMIT", "MAXIMUM SIZE LIMIT"))
dim(mh_sort3) # 96

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (96/96 records; 100%)
clus_subsector <- mh_sort3 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (96/96 records; 100%)
clus_adj <- mh_sort3 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (76/96 records; 79%)
clus_multi <- mh_sort3 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (96/96 records; 100%)
clus_off <- mh_sort3 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (27/96 records; 28%)
clus_flag <- mh_sort3 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (96/96 records; 100%)
clus_once <- mh_sort3 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (96/96 records; 100%)
clus_time <- mh_sort3 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (96/96 records; 100%)
clus_never <- mh_sort3 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (92/96 records; 96%)
clus_zone <- mh_sort3 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (96/96 records; 100%)
clus_species <- mh_sort3 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (96/96 records; 100%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort3 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (23/96; 24%)
simple_all <- mh_sort3 %>%
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


# ALL SIMPLE (92/96; 96%)
simple_test <- mh_sort3 %>%
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
mh_sort4 <- mh_sort %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO") 

mh_sort4 <- mh_sort4 %>%
  filter(MANAGEMENT_TYPE %in% c("CLOSURE", "REOPENING"))
dim(mh_sort4) #322

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (224/322 records; 70%)
clus_subsector <- mh_sort4 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (71/322 records; 22%)
clus_adj <- mh_sort4 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (57/322 records; 18%)
clus_multi <- mh_sort4 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (268/322 records; 83%)
clus_off <- mh_sort4 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (190/322 records; 59%)
clus_flag <- mh_sort4 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (48/322 records; 15%)
clus_once <- mh_sort4 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (33/322 records; 10%)
clus_time <- mh_sort4 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (322/322 records; 100%)
clus_never <- mh_sort4 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (133/322 records; 41%)
clus_zone <- mh_sort4 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (303/322 records; 94%)
clus_species <- mh_sort4 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (257/322 records; 80%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort4 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort4 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (0/322; 0%)
simple_all <- mh_sort4 %>%
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


# ALL SIMPLE (16/322; 5%)
simple_test <- mh_sort4 %>%
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
