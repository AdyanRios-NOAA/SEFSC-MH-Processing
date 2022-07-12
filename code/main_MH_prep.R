# Main Management History Processing

# Load packages
#install.packages("librarian")
librarian::shelf(here, tidyverse, lubridate, googlesheets4)

# SET WD RELATIVE TO PROJECT LOCATION
here::i_am('code/main_MH_prep.R')

# For accessing tables as Google sheets, need to authenticate email address
gs4_auth()
1

# ESTABLISH END OF TIMESERIES 
end_timeseries =  as.Date("2021-12-31", "%Y-%m-%d")

# 0A. Load workspace containing the species list dataframe to use in expansion (sp_info_use)
  # Because the data is coming directly from Oralce, the clean_spp_tables.R script does not need to run each time and will only work if you have an Oralce connection
  # Data saved as a workspace rather than a csv to avoid issues in the expansion merge to join to fields that contain NA or null values
  # To also better handle NA or null fields, the species data was transposed so that species_name_type indicates to oringal species field name
  # Name is the species aggregate or species group name or when common name is ALL
  # The MH dataset is also transposed to have a single common name field and no null values
load(here('data/interim', 'MH_clean_spp_tables.RData'))

# 0B: Bug related cleaning (these changes will eventually be made in the database)
  # Dataframe result = mh_cleaned
# source(here('code', 'MH_data_bugs.R'))
  source(here('Old_MH_data_bugs.R'))

# 1: Create new variables (new zone names, renaming "adjusted" management types, creating various new variables)
  # Dataframe result = mh_newvar
source(here('code', 'MH01_new_variables.R'))

# 2: Create sector and cluster ids
  # Dataframe result = mh_statid
source(here('code', 'MH02_static_ids.R'))

# 3: Fill in dates
  # Dataframe result = mh_dates
source(here('code', 'MH03_dates.R'))

# 4: Species expansion and clean up dates
# Dataframe result = mh_expanded 
source(here('code', 'MH04_spp_expansion.R'))

# 5: Process Adjustments
