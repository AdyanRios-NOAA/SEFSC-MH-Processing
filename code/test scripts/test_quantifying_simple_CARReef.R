## CAR REEF FISH SIMPLICITY ------

## QUANTIFYING SIMPLICITY
mh_sort2 <- mh_sort %>%
  filter(FMP == "REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")
dim(mh_sort2) # 416

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (375/416 records; 90%)
clus_subsector <- mh_sort2 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (416/416 records; 100%)
clus_adj <- mh_sort2 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (246/416 records; 59%)
clus_multi <- mh_sort2 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (416/416 records; 100%)
clus_off <- mh_sort2 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (231/416 records; 56%)
clus_flag <- mh_sort2 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (370/416 records; 89%)
clus_once <- mh_sort2 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (321/416 records; 77%)
clus_time <- mh_sort2 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (416/416 records; 100%)
clus_never <- mh_sort2 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (178/416 records; 43%)
clus_zone <- mh_sort2 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (297/416 records; 71%)
clus_species <- mh_sort2 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (297/416 records; 71%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort2 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort2 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (20/416; 5%)
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


# ALL SIMPLE (122/416; 29%)
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
