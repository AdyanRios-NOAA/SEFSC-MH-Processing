## CAR REEF FISH SIMPLICITY ------

## QUANTIFYING SIMPLICITY FOR ALL REGULATIONS
mh_sort10 <- mh_sort %>%
  filter(FMP == "REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")
dim(mh_sort10) # 416

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (375/416 records; 90%)
clus_subsector <- mh_sort10 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (416/416 records; 100%)
clus_adj <- mh_sort10 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (246/416 records; 59%)
clus_multi <- mh_sort10 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (416/416 records; 100%)
clus_off <- mh_sort10 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (231/416 records; 56%)
clus_flag <- mh_sort10 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (370/416 records; 89%)
clus_once <- mh_sort10 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (321/416 records; 77%)
clus_time <- mh_sort10 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (416/416 records; 100%)
clus_never <- mh_sort10 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (178/416 records; 43%)
clus_zone <- mh_sort10 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (297/416 records; 71%)
clus_species <- mh_sort10 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (297/416 records; 71%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort10 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort10 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (20/416; 5%)
simple_all <- mh_sort10 %>%
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


# ALL SIMPLE (122/416; 29%)
simple_test <- mh_sort10 %>%
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
mh_sort11 <- mh_sort %>%
  filter(FMP == "REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS") 

mh_sort11 <- mh_sort11 %>%
  filter(MANAGEMENT_TYPE %in% c("BAG LIMIT", "BAG LIMIT ADJUSTMENT"))
dim(mh_sort11) #8

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (8/8 records; 100%)
clus_subsector <- mh_sort11 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (8/8 records; 100%)
clus_adj <- mh_sort11 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (0/8 records; 0%)
clus_multi <- mh_sort11 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (8/8 records; 100%)
clus_off <- mh_sort11 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (0/8 records; 0%)
clus_flag <- mh_sort11 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (8/8 records; 100%)
clus_once <- mh_sort11 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (8/8 records; 100%)
clus_time <- mh_sort11 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (8/8 records; 100%)
clus_never <- mh_sort11 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (8/8 records; 91%)
clus_zone <- mh_sort11 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (0/8 records; 0%)
clus_species <- mh_sort11 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (0/8 records; 0%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort11 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort11 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (0/8; 0%)
simple_all <- mh_sort11 %>%
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

# ALL SIMPLE (0/8; 0%)
simple_test <- mh_sort11 %>%
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
mh_sort12 <- mh_sort %>%
  filter(FMP == "REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS") 

mh_sort12 <- mh_sort12 %>%
  filter(MANAGEMENT_TYPE %in% c("MINIMUM SIZE LIMIT", "MAXIMUM SIZE LIMIT"))
dim(mh_sort12) # 52

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (52/52 records; 100%)
clus_subsector <- mh_sort12 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (52/52 records; 100%)
clus_adj <- mh_sort12 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (14/52 records; 27%)
clus_multi <- mh_sort12 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (52/52 records; 100%)
clus_off <- mh_sort12 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (14/52 records; 27%)
clus_flag <- mh_sort12 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (52/52 records; 100%)
clus_once <- mh_sort12 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (52/52 records; 100%)
clus_time <- mh_sort12 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (52/52 records; 100%)
clus_never <- mh_sort12 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (38/52 records; 73%)
clus_zone <- mh_sort12 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (52/52 records; 100%)
clus_species <- mh_sort12 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (52/52 records; 100%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort12 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort12 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (0/52; 0%)
simple_all <- mh_sort12 %>%
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


# ALL SIMPLE (38/52; 73%)
simple_test <- mh_sort12 %>%
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
mh_sort13 <- mh_sort %>%
  filter(FMP == "REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS") 

mh_sort13 <- mh_sort13 %>%
  filter(MANAGEMENT_TYPE %in% c("CLOSURE", "REOPENING"))
dim(mh_sort13) #154

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (133/154 records; 86%)
clus_subsector <- mh_sort13 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (86/154 records; 56%)
clus_adj <- mh_sort13 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (41/154 records; 27%)
clus_multi <- mh_sort13 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (154/154 records; 100%)
clus_off <- mh_sort13 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (62/154 records; 40%)
clus_flag <- mh_sort13 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (104/154 records; 68%)
clus_once <- mh_sort13 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (67/154 records; 44%)
clus_time <- mh_sort13 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (154/154 records; 100%)
clus_never <- mh_sort13 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (24/154 records; 16%)
clus_zone <- mh_sort13 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (139/154 records; 90%)
clus_species <- mh_sort13 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (139/154 records; 90%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort13 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort13 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (0/154; 0%)
simple_all <- mh_sort13 %>%
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


# ALL SIMPLE (0/154; 0%)
simple_test <- mh_sort13 %>%
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
