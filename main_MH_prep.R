# Main Management History Processing

# Load packages
install.packages("pacman")

pacman::p_load(tidyverse, lubridate, googlesheets4, dotenv, ROracle, keyring)

# For accessing tables as Google sheets, need to authenticate email address
gs4_auth()

# ESTABLISH END OF TIMESERIES 
end_timeseries =  as.Date("2020-12-31", "%Y-%m-%d")

# 0. Load workspace containing the species list dataframe to use in expansion (sp_info_use)
  # Because the data is coming directly from Oralce, the clean_spp_tables.R script does not need to run each time and will only work if you have an Oralce connection
  # Data saved as a workspace rather than a csv to avoid issues in the expansion merge to join to fields that contain NA or null values
  # To also better handle NA or null fields, the species data was transposed so that species_name_type indicates to oringal species field name
  # Name is the species aggregate or species group name or when common name is ALL
  # The MH dataset is also transposed to have a single common name field and no null values
load('./MH_clean_spp_tables.RData')


# 1: Bug related cleaning (these changes will eventually be made in the database)
  # Dataframe result = mh_cleaned
source('./MH_data_bugs.R')


# 2: Sector and species related expansion (aggregates, groups, all species); retain orignal columns, but fills in common name always
  # Dataframe result = mh_expanded
source('./MH_expansion.R')


# 3: Pre-processing clean up (new zone names, renaming "adjusted", creating various new variables)
  # Dataframe result = mh_ready
source('./MH_pre_processing.R')


# 4: Sorting by cluster and fill in dates
  # Dataframe result = mh_sort (before final species expansion cleaning)
  # Dataframe result = mh_sort2 (remove expanded species that are no longer in the group and adjust start and end dates to reflect species added and removed dates)
source('./MH_process.R')

# 5: Post-processing by collection - grouping related mtypes (mtype = cluster)
