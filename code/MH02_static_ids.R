# Script 2
# Create Sector and Cluster IDs
  # Overview:####
  # Address SECTOR forks by expanding SUBSECTOR information
    # Define matching variables for sectors
    # Create variables to outline how SUBSECTOR expands
    # Perform expansion by separating SUBSECTOR information into new variable of SUBSECTOR_USE
  # Group data into processing CLUSTERs
    # Define matching variables for CLUSTERs
    # Count how many CLUSTERs are included for processing
    # Note when there are multiple records per cluster per FR_CITATION active at the same time (MULTI_REG)

#### 1 ####
# Address SECTOR forks ####
# Develop means to track which SECTORs have multiple SUBSECTORS and their unique combinations ####
# Use the variables of MANAGEMENT_TYPE_USE, JURISDICTION, JURISDICTIONAL_WATERS, FMP, SECTOR_USE, REGION, and SPP_NAME to develop SECTOR groupings
sector.match <- c("MANAGEMENT_TYPE_USE",
                  "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
                  "SECTOR_USE",
                  "REGION",
                  "SPP_NAME")

# Build list of existing sector_precluster files
# This list is grouped by the variables described above and contains a unique SECTOR_ID for each unique sector grouping
sector_id_files <- dir(here('data/interim/sector_clusters'), full.names = TRUE)

# Read in and combine sector_id_files with existing_sector_clusters
# This step combines any new sector_id_files with old sector groupings outlined in existing_sector_clusters
existing_sector_clusters <- sector_id_files %>%
  map(read_csv) %>% 
  reduce(rbind)

# CHECK: Get starting number of existing sector groupings for reference
clusters_max = max(existing_sector_clusters$SECTOR_ID)

# CREATE: Assign numbers to any new sectors (SECTOR_ID) ####
new_sector_clusters <- mh_newvar %>%
  select(one_of(sector.match)) %>%
  distinct() %>%
  anti_join(existing_sector_clusters) %>%
  mutate(SECTOR_ID = (1:n() + clusters_max)[seq_len(nrow(.))])

# WRITE: export new sector groupings into mh_sector_clusters_ CSV 
if(length(new_sector_clusters$SECTOR_ID) > 0) {
  write_csv(new_sector_clusters, 
            here('data/interim/sector_clusters', paste0("mh_sector_clusters_", format(Sys.Date(), "%d%b%Y"),".csv")))
}

# Merge old and new sector groupings into unique_sector_clusters data frame ####
unique_sector_clusters <- rbind(existing_sector_clusters, new_sector_clusters)

# Join the unique_sector_clusters data frame to the mh_newvar data frame to incorporate sector information in the data set ####
mh_sector_id <- mh_newvar %>%
  left_join(unique_sector_clusters, 
            by = c("JURISDICTION", "REGION", "JURISDICTIONAL_WATERS", "FMP", 
                   "SECTOR_USE", "SPP_NAME", "MANAGEMENT_TYPE_USE"))

# Filter records to include SECTORs with more than one SUBSECTOR for DETAILED MANAGEMENT_TYPEs ####
multi_subsector <- mh_sector_id %>%
  #filter(GENERAL == 0) %>%
  filter(DETAILED == "YES") %>%
  select(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR) %>%
  distinct() %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID) %>%
  mutate(subsector_count = length(SUBSECTOR),
         #Flag if SUBSECTOR "ALL" is used
         subsector_all_used = sum(SUBSECTOR == "ALL")) %>%
  filter(subsector_count > 1) %>%
  arrange(SECTOR_ID, SUBSECTOR) %>%
  data.frame()

# CREATE: the variable SUBSECTOR_KEY to expand SUBSECTOR "ALL" into all components
multi_subsector_key <- mh_sector_id %>%
  select(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR, EFFECTIVE_DATE) %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR) %>%
  # Add the variable of start_use to include the date of the first time each SUBSECTOR is used 
  summarize(start_use = min(EFFECTIVE_DATE)) %>%
  right_join(., multi_subsector, by = c("FMP", "SECTOR_USE", "SECTOR_ID", "SUBSECTOR")) %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID) %>%
  mutate(date_count = length(unique(start_use))) %>%
  arrange(SECTOR_ID) %>%
  # Remove SECTORS that do not use SUBSECTOR "ALL" and everything started on a single date 
  filter(!(subsector_all_used == 0 & date_count == 1)) %>%
  # Concatenate SUBSECTOR names used within SECTOR_ID 
  group_by(SECTOR_ID) %>%
  mutate(SUBSECTOR_KEY = paste(unique(SUBSECTOR), sep = ",", collapse=', ')) %>%
  data.frame()

# CREATE: the variable of SUBSECTOR_N to outline how many SUBSECTORS are included in the expansion
# Currently not being used because we are only looking at GOM Reef Fish
expand_sector_keys = multi_subsector_key %>%
  select(FMP, SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY) %>%
  group_by(FMP, SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY) %>%
  mutate(SUBSECTOR_N = length(SECTOR_USE)) %>%
  distinct() %>%
  arrange(SUBSECTOR_N) %>%
  data.frame()

# CHECK: Temporary look at GOM Reef Fish while we get this to include other FMPs 
quick_look <- multi_subsector_key %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO") 

expand_sector_keys_recGOMRF <- multi_subsector_key %>%
  filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO",
         SECTOR_USE == "RECREATIONAL") %>%
  select(SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY, subsector_all_used) %>%
  distinct() %>%
  #CREATE: the variables of expand_from, expand_temp, and expand_to to track the SUBSECTOR expansion process
  mutate(column_name = "SUBSECTOR",
         expand_from = case_when(subsector_all_used == 1 ~ "ALL"),
         expand_temp = str_remove(SUBSECTOR_KEY, paste0(expand_from, ", ")),
         expand_to = case_when(str_count(expand_temp, ',') >= 1 ~ expand_temp,
                               TRUE ~ "MANUAL CHECK"))

# By removing the variable of ZONE from the sector.match variable,
# we reduced manual forks for GOM Reef Fish (we may need to perform manual checks for other FMPs)
# Example of addressing MANAUAL CHECKS from expand_sector_keys_recGOMRF
expand_sector_keys_recGOMRF_use <- expand_sector_keys_recGOMRF %>%
  mutate(expand_to = case_when(SECTOR_ID == 1110 ~ "FOR-HIRE, PRIVATE",
                               TRUE ~ expand_to))

# Create the expansions data frame to include the expand_sector_keys_recGOMRF_use data frame 
# This will be used to join to mh_sector_id
expansions <- expand_sector_keys_recGOMRF

# Perform the SUBSECTOR expansion function ####
# Join mh_sector_id to the expansions data frame and use the expand_from and expand_to variables to track the expansion process
# Results in the mh_subsect_expanded data frame
mh_subsect_expanded <- left_join(mh_sector_id, expansions, by = c("SECTOR_USE", "SECTOR_ID")) %>%
  mutate(SUBSECTOR_USE = case_when(!is.na(expand_to) & expand_from == SUBSECTOR ~ expand_to,
                                   TRUE ~ SUBSECTOR)) %>%
  # Expand SUBSECTOR_USE at the commas
  separate_rows(SUBSECTOR_USE, sep = ", ")

# Results in the mh_ready data frame 
mh_ready <- mh_subsect_expanded

#### 2 ####
# CREATE: the variable of CLUSTER to assist in processing like MANAGEMENT_TYPEs for a species within the same FMP and REGION.
# CLUSTERs assist in accurately creating a time series of regulation types for a species.
# Define variables which must match for records to be considered part of a CLUSTER ####
cluster.match <- c("MANAGEMENT_TYPE_USE",
                   "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
                   "SECTOR_USE", "SUBSECTOR_USE", "REGION",
                   "SPP_NAME")

# Read in existing CLUSTERs
cluster_files <- dir(here('data/interim/clusters'), full.names = TRUE)

existing_clusters <- cluster_files %>%
  map(read_csv) %>% 
  reduce(rbind)

# CHECK: get starting number of current CLUSTERS for reference
clusters_max = max(existing_clusters$CLUSTER)

# CREATE: Assign numbers to any new CLUSTERs ####
new_clusters <- mh_ready %>%
  select(one_of(cluster.match)) %>%
  distinct() %>%
  anti_join(existing_clusters) %>%
  mutate(CLUSTER = (1:n() + clusters_max)[seq_len(nrow(.))])

# Export new CLUSTERs
if(length(new_clusters$CLUSTER) > 0) {
  write_csv(new_clusters, 
            here("data/interim/clusters", paste0("mh_unique_clusters_", format(Sys.Date(), "%d%b%Y"),".csv")))
}

# Merge old and new CLUSTERS into unique_clusters data frame ####
unique_clusters <- rbind(existing_clusters, new_clusters)

# Join CLUSTERS ####
# Results in the mh_prep data frame
mh_prep <- mh_ready %>%
  left_join(., unique_clusters, by = cluster.match) %>%
  mutate(REG_CHANGE = 1)

# Address CLUSTERs with multiple records from the same FR Notice that are effective at the same time ####
# CREATE: the variable of MULTI_REG to flag cases when there multiple records per FR_CITATION within the same 
# CLUSTER that are effective at the same time
multi_reg <- mh_prep %>%
  group_by(CLUSTER, FR_CITATION) %>%
  summarize(MULTI_REG = as.numeric(duplicated(FR_CITATION))) %>%
  filter(MULTI_REG == 1) %>%
  data.frame() %>%
  distinct()
dim(multi_reg)

# Join flagged MULTI_REG cases with the full data set ####
# Results in mh_statid data frame 
mh_statid <- mh_prep %>%
  # Identify regulations that are MULTI_REGs (same FR_NOTICE within a CLUSTER)
  left_join(., multi_reg, by = c("FR_CITATION", "CLUSTER")) %>%
  # Replace all NAs with 0
  mutate_at("MULTI_REG", ~replace(., is.na(.), 0)) %>%
  # CHECK: Does this CLUSTER have any instances of a MULTI_REG?
  mutate(MULTI_REG_CLUSTER = as.numeric(CLUSTER %in% multi_reg$CLUSTER)) 