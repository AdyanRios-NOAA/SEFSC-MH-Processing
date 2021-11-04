# 2
# Expand sector ALL to recreational and commercial
# Expand species ALL and species aggregate and group fields so each record has a common name and ITIS code associated with it
# Expansion on species only relevant for management types that are NOT FYI

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Expand records with SECTOR = 'ALL" to be commercial and recreational
mh_sect_expanded <- mh_cleaned %>%
  #CREATE SECTOR USE
  mutate(SECTOR_USE = SECTOR) %>%
  # Rename "ALL" records to 'RECREATIONAL,COMMERCIAL' to separate at the comma
  mutate(SECTOR_USE = case_when(SECTOR_USE == 'ALL' ~ 'RECREATIONAL,COMMERCIAL',
                          TRUE ~ SECTOR_USE)) %>%
  separate_rows(SECTOR_USE)

# Bring in management type detailed (Y/N) table from Google sheets to only expand mtypes we care about
# Read in Google sheets 
detailed_xref <- read_sheet("https://docs.google.com/spreadsheets/d/1PViPVtqkY3q1fWUFGZm1UyIIYrFxt-YDitEUGaHBjsg/edit#gid=1115852389") %>%
  select(-MANAGEMENT_CATEGORY)

# Add field for detailed (Y/N)
mh_sect_expanded <- mh_sect_expanded %>%
  left_join(detailed_xref, by = "MANAGEMENT_TYPE")

# Species list (duplicate issue - need to revist once dbase is back up and running)
sp_info_use2 <- sp_info_use %>%
  group_by(SPECIES_NAME_TYPE, NAME, FMP, COMMON_NAME_USE, SPECIES_ITIS_USE, ADDED_SP_DATE, REMOVED_SP_DATE) %>%
  summarise(N = n())

# EXPAND MH on species
mh_sp_expanded <- mh_sect_expanded %>%
  # Transpose species fields to only have to join in the expansion on a single field
  pivot_longer(cols = c(COMMON_NAME, SPECIES_AGGREGATE, SPECIES_GROUP), names_to = "SPECIES_NAME_TYPE", values_to = "NAME") %>%
  # Remove rows where NAME is blank
  filter(NAME != "") %>%
  # Join to species list table by species name type, name,  and FMP
  full_join(., sp_info_use, by = c("SPECIES_NAME_TYPE", "NAME", "FMP")) %>%
  # Remove species group/aggregate name that do not appear in the database or FMP that are not in the data (i.e. HMS)
  filter(!is.na(REGULATION_ID)) %>%
  mutate(COMMON_NAME_USE = case_when(is.na(COMMON_NAME_USE) ~ NAME,
                                     TRUE ~ COMMON_NAME_USE),
         SPECIES_ITIS_USE = case_when(is.na(SPECIES_ITIS_USE) ~ as.character(SPECIES_ITIS),
                                      TRUE ~ SPECIES_ITIS_USE)) %>%
  select(-SPECIES_ITIS)

# Still need to filter to remove records when the species is no longer in the aggregate, group, or FMP list


