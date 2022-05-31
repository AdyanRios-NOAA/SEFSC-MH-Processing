# 3
# Process clustering
  # MATCH CLUSTERS AND ORDER RECORDS

# SUMMARY OF STEPS ####
# 1  GROUP DATA INTO PROCESSING CLUSTERS 
#   1A DEFINE MATCH VARIABLES
#   1B COUNT HOW MANY CLUSTERS TO PROCESS
#   1C NOTE WHEN THERE ARE MULTIPLE RECORDS PER CLUSTER PER FR ACTIVE AT THE SAME TIME



# 1 Group data into processing CLUSTERS ####

# 1A Define variables which must match for records to be considered part of a CLUSTER ####

cluster.match <- c("MANAGEMENT_TYPE_USE",
                   "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
                   "SECTOR_USE", "SUBSECTOR_USE", "REGION",
                   "SPP_NAME")

# Read in existing CLUSTERS

cluster_files <- dir(here('data/interim/clusters'), full.names = TRUE)

existing_clusters <- cluster_files %>%
  map(read_csv) %>% 
  reduce(rbind)

# Get starting number of current CLUSTERS for reference

clusters_max = max(existing_clusters$CLUSTER)

# Assign numbers to new CLUSTERS

new_clusters <- mh_ready %>%
  select(one_of(cluster.match)) %>%
  distinct() %>%
  anti_join(existing_clusters) %>%
  mutate(CLUSTER = (1:n() + clusters_max)[seq_len(nrow(.))])

# Export new CLUSTERS
if(length(new_clusters$CLUSTER) > 0) {
  write_csv(new_clusters, 
            here("data/interim/clusters", paste0("mh_unique_clusters_", format(Sys.Date(), "%d%b%Y"),".csv")))
}

# Merge old and new CLUSTERS
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


# Join in CLUSTERS and COLLECTIONS

mh_prep <- mh_ready %>%
  left_join(., unique_clusters, by = cluster.match) %>%
  # left_join(., unique_collections, by = collection.match) %>%
  mutate(REG_CHANGE = 1)

# 1C Note when there are multiple records per cluster per FR active at the same time####

# CREATE: find cases when there multiple records per FR_CITATION within the same 
# CLUSTER and create a FLAG to indicate such
multi_reg <- mh_prep %>%
  group_by(CLUSTER, FR_CITATION) %>%
  summarize(MULTI_REG = as.numeric(duplicated(FR_CITATION))) %>%
  filter(MULTI_REG == 1) %>%
  data.frame() %>%
  distinct()
dim(multi_reg)

# Join flagged cases into full data set
# Results in mh_prep_use
mh_prep_use <- mh_prep %>%
  # Identify regulations that are MULTI_REGS (same FR within a CLUSTER)
  left_join(., multi_reg, by = c("FR_CITATION", "CLUSTER")) %>%
  # Replace all NAs with 0
  mutate_at("MULTI_REG", ~replace(., is.na(.), 0)) %>%
  # CHECK: Does this CLUSTER have any instances of a MULTI_REG?
  mutate(MULTI_REG_CLUSTER = as.numeric(CLUSTER %in% multi_reg$CLUSTER)) 


