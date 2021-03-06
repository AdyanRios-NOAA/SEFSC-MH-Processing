## MH_PRE_PROCESS.R TESTS ------

## QUANTIFYING SIMPLICITY
dim(mh_sort) # 7693

# DO NOT DEAL WITH SUBSECTOR YET (6085/7693 records; 79%)
clus_subsector <- mh_sort %>%
  filter(SUBSECTOR_USE != "ALL")

simple_subsector <- mh_sort %>%
  filter(!CLUSTER %in% clus_subsector$CLUSTER)
dim(simple_subsector)

# NO ADJUSTMENT (5741/7693 records; 74%)
clus_adj <- mh_sort %>%
  filter(ADJUSTMENT == 1)

simple_adj <- mh_sort %>%
  filter(!CLUSTER %in% clus_adj$CLUSTER)
dim(simple_adj)

# NO MULTI_REG (3021/7693 records; 39%)
clus_multi <- mh_sort %>%
  filter(MULTI_REG == 1)

simple_multi <- mh_sort %>%
  filter(!CLUSTER %in% clus_multi$CLUSTER)
dim(simple_multi)

# NO REGULATION TURN OFF (7030/7693 records; 91%)
clus_off <- mh_sort %>%
  filter(REG_REMOVED == 1)

simple_off <- mh_sort %>%
  filter(!CLUSTER %in% clus_off$CLUSTER)
dim(simple_off)

# NO FLAG (3461/7693 records; 45%)
clus_flag <- mh_sort %>%
  filter(FLAG == "YES")

simple_flag <- mh_sort %>%
  filter(!CLUSTER %in% clus_flag$CLUSTER)
dim(simple_flag)

# ONCE (6799/7693 records; 88%)
clus_once <- mh_sort %>%
  filter(MANAGEMENT_STATUS_USE != "ONCE")

simple_once <- mh_sort %>%
  filter(!CLUSTER %in% clus_once$CLUSTER)
dim(simple_once)

# NO TIME (5502/7693 records; 71%)
clus_time <- mh_sort %>%
  filter(!is.na(START_TIME) | !is.na(END_TIME))

simple_time <- mh_sort %>%
  filter(!CLUSTER %in% clus_time$CLUSTER)
dim(simple_time)

# NO NEVER IMPLENTED (4062/7693 records; 52%)
clus_never <- mh_sort %>%
  filter(NEVER_IMPLEMENTED == 1)

simple_never <- mh_sort %>%
  filter(!CLUSTER %in% clus_never$CLUSTER)
dim(simple_never)

# ZONE ALL (4424/7693 records; 58%)
clus_zone <- mh_sort %>%
  filter(ZONE != "ALL")

simple_zone <- mh_sort %>%
  filter(!CLUSTER %in% clus_zone$CLUSTER)
dim(simple_zone)

# SPECIES SPECIFIC (7273/7693 records; 95%)
clus_species <- mh_sort %>%
  filter(SPP_TYPE != "COMMON_NAME")

simple_species <- mh_sort %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_species)

# SPECIES SPECIFIC (7215/7693 records; 94%)
#THIS DOES NOT CAPTURE THAT A SPECIES CAN BE IN AN AGG FOR ONE FMP BUT NOT ANOTHER
clus_agg <- mh_sort %>%
  filter(SPECIES_ITIS %in% agg_info_use$SPECIES_ITIS)

simple_agg <- mh_sort %>%
  filter(!CLUSTER %in% clus_agg$CLUSTER) %>%
  filter(!CLUSTER %in% clus_species$CLUSTER)
dim(simple_agg)

# ALL SIMPLE (539/7693; 7%)
simple_all <- mh_sort %>%
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


# ALL SIMPLE (3221/7693; 42%)
simple_test <- mh_sort %>%
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
