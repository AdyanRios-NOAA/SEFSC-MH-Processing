## SATL SNAPPER GROUPER SIMPLICITY ------

## QUANTIFYING SIMPLICITY
mh_sort3 <- mh_sort %>%
  filter(FMP == "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION")
dim(mh_sort3) # 1427

# NO SUBSECTOR
# DO NOT DEAL WITH SUBSECTOR YET (1255/1427 records; 88%)
clus_subsector <- mh_sort3 %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (1401/1427 records; 98%)
clus_adj <- mh_sort3 %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (767/1427 records; 54%)
clus_multi <- mh_sort3 %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (1288/1427 records; 90%)
clus_off <- mh_sort3 %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (979/1427 records; 69%)
clus_flag <- mh_sort3 %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (1317/1427 records; 92%)
clus_once <- mh_sort3 %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (1124/1427 records; 79%)
clus_time <- mh_sort3 %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (1427/1427 records; 100%)
clus_never <- mh_sort3 %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (1137/1427 records; 80%)
clus_zone <- mh_sort3 %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (1325/1427 records; 93%)
clus_species <- mh_sort3 %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (1325/1427 records; 93%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort3 %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort3 %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (128/1427; 9%)
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


# ALL SIMPLE (887/1427; 62%)
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
