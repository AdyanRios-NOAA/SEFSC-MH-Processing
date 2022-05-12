## GULF REEF FISH SIMPLICITY ------

## QUANTIFYING SIMPLICITY
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

