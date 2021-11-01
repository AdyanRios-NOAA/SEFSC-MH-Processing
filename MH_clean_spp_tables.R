# Standardize and fill in common name for FMP species, species aggregate and species group tables

# CONNECT TO SECPR SERVER ####
con <- dbConnect(dbDriver("Oracle"),
                 username = keyring::key_list("SECPR")[1,2],
                 password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]),
                 dbname = "SECPR")


# Ask the repo owner for the .env file
load_dot_env("./cleaning and preprocessing/mh_spp.env")
spp_grp_view <- Sys.getenv("FMP_SPECIES_GRP")
spp_agg_view <- Sys.getenv("FMP_SPECIES_AGG")
spp_itis_xref <- Sys.getenv("SEDAT_SPP_ITIS")

# READ IN SPECIES LISTS #
# Join SEDAT species ITIS table to standardize the common name field
    # Example: 169593 says 'longsnout butterflyfish' in V_S_FMP_SPECIES_GRP_DET table and SEDAT has common name as 'BUTTERFLYFISH, LONGSNOUT
fmp_group_info = dbGetQuery(con, paste("select a.fmp_group_id,
                                  a.fmp_group_name,
                                  a.fmp_group_region,
                                  a.subgrp_name,
                                  a.species_itis,
                                  coalesce(b.itis_commonname, a.common_name) as common_name,
                                  coalesce(a.scientific_name, b.itis_scientificname) as scientific_name,
                                  a.added_dt,
                                  a.removed_dt,
                                  a.alias_name
                                  from ", spp_grp_view, " a
                                  left join (select distinct itis_code, itis_commonname, itis_scientificname from ", spp_itis_xref, ") b
                                  on a.species_itis = b.itis_code"))
# There was one record that was missing species ITIS, but the comments section had valid ITIS code
agg_info = dbGetQuery(con, paste("select a.fmp_group_name,
                            a.fmp_species_agg_name,
                            a.species_itis,
                            coalesce(b.itis_commonname, a.common_name) as common_name,
                            coalesce(a.scientific_name, b.itis_scientificname) as scientific_name,
                            a.species_agg_region,
                            a.added_dt,
                            a.FR_citation_for_added,
                            a.removed_dt,
                            a.FR_citation_for_removed,
                            a.relevant_section,
                            a.comments
                            from (select fmp_group_name, fmp_species_agg_name,
                                  case when species_itis is null then '614740' else species_itis end species_itis,
                                  common_name, scientific_name, species_agg_region,
                                  added_dt, FR_citation_for_added, removed_dt, FR_citation_for_removed,
                                  relevant_section, comments
                                  from ", spp_agg_view, ") a
                            left join (select distinct itis_code, itis_commonname, itis_scientificname from ", spp_itis_xref, ") b
                            on a.species_itis = b.itis_code"))

# REFORMAT SPECIES LISTS FOR MERGE TO MH
# Transposed data to eliminate the need for 3 fields and dealing with null values
# species_name_type indicates whether it is a group, aggregate, or common name
# Name is the actual group or aggregate name for expansion or when common name is ALL for expansion

# FMP Species data
fmp_info_use <- fmp_group_info %>%
  rename(ADDED_SP_DATE = ADDED_DT,
         REMOVED_SP_DATE = REMOVED_DT,
         COMMON_NAME_USE = COMMON_NAME,
         SPECIES_ITIS_USE = SPECIES_ITIS,
         FMP = FMP_GROUP_NAME) %>%
  # Set species_name_type to ALL
  mutate(SPECIES_NAME_TYPE = "COMMON_NAME",
         NAME = 'ALL',
         ADDED_SP_DATE = format(as.Date(ADDED_SP_DATE), "%Y-%m-%d"),
         REMOVED_SP_DATE = format(as.Date(REMOVED_SP_DATE), "%Y-%m-%d")) %>%
  select(SPECIES_NAME_TYPE, NAME, FMP, COMMON_NAME_USE, SPECIES_ITIS_USE, ADDED_SP_DATE, REMOVED_SP_DATE) %>%
  # Set common name for species ITIS codes missing common name
  mutate(COMMON_NAME_USE = case_when(SPECIES_ITIS_USE == '614546' ~ 'RAZORFISH, GREEN',
                                     SPECIES_ITIS_USE == '159878' ~ 'SHARK, SAND TIGER',
                                     SPECIES_ITIS_USE == '159821' ~ 'SHARK, SIXGILL',
                                     SPECIES_ITIS_USE == '614513' ~ 'RAZORFISH, PEARLY',
                                     TRUE ~ COMMON_NAME_USE))



# Species group data
grp_info_use <- fmp_group_info %>%
  rename(ADDED_SP_DATE = ADDED_DT,
         REMOVED_SP_DATE = REMOVED_DT,
         COMMON_NAME_USE = COMMON_NAME,
         SPECIES_ITIS_USE = SPECIES_ITIS,
         FMP = FMP_GROUP_NAME) %>%
  # Set species_name_type to species group
  mutate(SPECIES_NAME_TYPE = "SPECIES_GROUP",
         NAME = SUBGRP_NAME,
         ADDED_SP_DATE = format(as.Date(ADDED_SP_DATE), "%Y-%m-%d"),
         REMOVED_SP_DATE = format(as.Date(REMOVED_SP_DATE), "%Y-%m-%d")) %>%
  select(SPECIES_NAME_TYPE, NAME, FMP, COMMON_NAME_USE, SPECIES_ITIS_USE, ADDED_SP_DATE, REMOVED_SP_DATE) %>%
  filter(!is.na(NAME)) %>%
  # Set common name for species ITIS codes missing common name
  mutate(COMMON_NAME_USE = case_when(SPECIES_ITIS_USE == '614546' ~ 'RAZORFISH, GREEN',
                                     SPECIES_ITIS_USE == '159878' ~ 'SHARK, SAND TIGER',
                                     SPECIES_ITIS_USE == '159821' ~ 'SHARK, SIXGILL',
                                     SPECIES_ITIS_USE == '614513' ~ 'RAZORFISH, PEARLY',
                                     TRUE ~ COMMON_NAME_USE))

# Species aggregate data
agg_info_use <- agg_info %>%
  rename(ADDED_SP_DATE = ADDED_DT,
         REMOVED_SP_DATE = REMOVED_DT,
         COMMON_NAME_USE = COMMON_NAME,
         SPECIES_ITIS_USE = SPECIES_ITIS,
         FMP = FMP_GROUP_NAME) %>%
  # Set species_name_type to species aggregate
  mutate(SPECIES_NAME_TYPE = "SPECIES_AGGREGATE",
         NAME = FMP_SPECIES_AGG_NAME,
         ADDED_SP_DATE = format(as.Date(ADDED_SP_DATE), "%Y-%m-%d"),
         REMOVED_SP_DATE = format(as.Date(REMOVED_SP_DATE), "%Y-%m-%d")) %>%
  select(SPECIES_NAME_TYPE, NAME, FMP, COMMON_NAME_USE, SPECIES_ITIS_USE, ADDED_SP_DATE, REMOVED_SP_DATE) %>%
  # Set common name for species ITIS codes missing common name
  mutate(COMMON_NAME_USE = case_when(SPECIES_ITIS_USE == '159821' ~ 'SHARK, SIXGILL',
                                     TRUE ~ COMMON_NAME_USE))


# COMBINE SPECIES LISTS
sp_info_use = bind_rows(fmp_info_use, grp_info_use, agg_info_use) %>%
  mutate(FMP = toupper(FMP))

# Remove connection to Oracle when saving data to workspace
rm(con)

# Save workspace
save.image("./cleaning and preprocessing/MH_clean_spp_tables.RData")



