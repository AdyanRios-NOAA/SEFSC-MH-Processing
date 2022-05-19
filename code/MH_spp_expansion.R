# 4
# CLean up dates

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Check species table -- 5 have duplicates still because they are species that were a part of an aggregate, removed, then reinstated
sp_info_use2 <- sp_info_use %>%
  group_by(FMP, SPP_TYPE, SPP_NAME, COMMON_NAME_USE) %>%
  summarise(N = n())

# Break up species table into two tables
# (1) Single record for species in aggregate or group
# (2) More than one record for species in aggregate or group
sp_info_use_s <- sp_info_use %>%
  left_join(sp_info_use2, by = c("FMP", "SPP_TYPE", "SPP_NAME", "COMMON_NAME_USE")) %>%
  filter(N == 1) %>% 
  select(-c(N, FMP_GROUP_ID, SUBGRP_NAME))
sp_info_use_m <- sp_info_use %>%
  left_join(sp_info_use2, by = c("FMP", "SPP_TYPE", "SPP_NAME", "COMMON_NAME_USE")) %>%
  filter(N == 2) %>% 
  select(-c(N, FMP_GROUP_ID, SUBGRP_NAME))

# OVER EXPAND MH on species for detailed mtypes where there is a single record for that species in species aggrgate/group table
mh_sp_expanded_y1 <- mh_sort %>%
  # Filter to remove non-detailed records
  filter(DETAILED == 'YES') %>%
  # Join to species list table by species name type, name,  and FMP
  full_join(., sp_info_use_s, by = c("FMP", "SPP_TYPE", "SPP_NAME")) %>%
  # Remove species group/aggregate name that do not appear in the database or FMP that are not in the data (i.e. HMS)
  filter(!is.na(REGULATION_ID)) %>%
  mutate(COMMON_NAME_USE = case_when(is.na(COMMON_NAME_USE) ~ SPP_NAME,
                                     TRUE ~ COMMON_NAME_USE),
         SPECIES_ITIS_USE = case_when(is.na(SPECIES_ITIS_USE) ~ as.character(SPECIES_ITIS),
                                      TRUE ~ SPECIES_ITIS_USE)) %>%
  # Remove species when the start date is outside the added and removed dates
  # Remove species when the start date is outside the added and removed dates
  #filter(is.na(REMOVED_SP_DATE) | between(START_DATE, ADDED_SP_DATE, REMOVED_SP_DATE) | between(END_DATE, ADDED_SP_DATE, REMOVED_SP_DATE)) %>%
  # Impute ineffective date with the removed date for records where there is a removed date, but no ineffective date
  # mutate(IMP_INEFFECTIVE = case_when(is.na(INEFFECTIVE_DATE) & !is.na(REMOVED_SP_DATE) ~ REMOVED_SP_DATE,
  #                                    TRUE ~ INEFFECTIVE_DATE),
  #        IMP_EFFECTIVE = NA) %>%
  # Remove species expansion date variables
  select(-c(SPECIES_ITIS, SCIENTIFIC_NAME))

# OVER EXPAND MH on species for detailed mtypes where there are multiple records for that species in species aggrgate/group table
mh_sp_expanded_y2 <- mh_sort %>%
  # Filter to remove non-detailed records
  filter(DETAILED == 'YES') %>%
  # Join to species list table by species name type, name,  and FMP
  full_join(., sp_info_use_m, by = c("FMP", "SPP_TYPE", "SPP_NAME")) %>%
  # Remove species group/aggregate name that do not appear in the database or FMP that are not in the data (i.e. HMS)
  filter(!is.na(REGULATION_ID), !is.na(ADDED_SP_DATE)) %>%
  mutate(COMMON_NAME_USE = case_when(is.na(COMMON_NAME_USE) ~ SPP_NAME,
                                     TRUE ~ COMMON_NAME_USE),
         SPECIES_ITIS_USE = case_when(is.na(SPECIES_ITIS_USE) ~ as.character(SPECIES_ITIS),
                                      TRUE ~ SPECIES_ITIS_USE)) %>%
  # Remove species when expanded if effective date > removed date
  # Remove species when expanded if ineffective date < added date
  #filter(EFFECTIVE_DATE < REMOVED_SP_DATE | is.na(REMOVED_SP_DATE) | INEFFECTIVE_DATE > ADDED_SP_DATE) %>%
  # Rank duplicate records for same species
  # group_by(REGULATION_ID) %>%
  # mutate(rnk = order(REMOVED_SP_DATE)) %>%
  # ungroup() %>%
  # Remove species when expanded if effective date > added date
  # mutate(del = case_when(EFFECTIVE_DATE > ADDED_SP_DATE & EFFECTIVE_DATE > REMOVED_SP_DATE ~ 1,
  #                        rnk == 2 & INEFFECTIVE_DATE < ADDED_SP_DATE ~ 1,
  #                        TRUE ~ 0)) %>%
  # filter(del == 0) %>%
  # Impute effective date for the duplicate record so that EFFECTIVE_DATE = ADD_SP_DATE, adjusting effective date retains both records and indicates during the gap in timeline the species was not a part of that aggregate/group
  # mutate(IMP_EFFECTIVE = case_when(rnk == 2 ~ ADDED_SP_DATE,
  #                                  TRUE ~ EFFECTIVE_DATE)) %>%
  # # Impute ineffective date with the removed date for records where there is a removed date, but no ineffective date
  # mutate(IMP_INEFFECTIVE = case_when(is.na(INEFFECTIVE_DATE) & !is.na(REMOVED_SP_DATE) ~ REMOVED_SP_DATE,
  #                                    INEFFECTIVE_DATE > REMOVED_SP_DATE ~ REMOVED_SP_DATE,
  #                                    TRUE ~ INEFFECTIVE_DATE)) %>%
  # Remove species expansion date variables
  select(-c(SPECIES_ITIS, SCIENTIFIC_NAME#, rnk, del
  ))

# Dataframe of non-detailed records in long form to join with expanded dataframe
mh_sp_expanded_n <- mh_sort %>%
  # Filter to remove non-detailed records
  filter(DETAILED == 'NO') %>%
  # Get fields to match
  mutate(COMMON_NAME_USE = SPP_NAME,
         SPECIES_ITIS_USE = as.character(SPECIES_ITIS)) %>%
  select(-SPECIES_ITIS)

mh_sp_expanded <- mh_sp_expanded_y1 %>%
  bind_rows(mh_sp_expanded_y2, mh_sp_expanded_n)


# REMOVE EXPANDED SPECIES  THAT ARE NO LONGER A PART OF THAT GROUP 
# ADJUST START OR END DATES USING SPECIES ADDED AND REMOVED DATES

chk_spp <- mh_sp_expanded %>%
  mutate(chk_spp = case_when(REMOVED_SP_DATE < START_DATE ~ 1,
                             REMOVED_SP_DATE < END_DATE & REMOVED_SP_DATE > START_DATE ~ 2,
                             ADDED_SP_DATE > START_DATE & END_DATE > ADDED_SP_DATE  ~ 3,
                             ADDED_SP_DATE > START_DATE & END_DATE < ADDED_SP_DATE ~ 4,
                             REMOVED_SP_DATE == START_DATE ~ 5,
                             ADDED_SP_DATE > START_DATE ~ 6,
                             is.na(REMOVED_SP_DATE) ~ 6,
                             ADDED_SP_DATE > START_DATE & REMOVED_SP_DATE < END_DATE ~ 6,
                             ADDED_SP_DATE < START_DATE & END_DATE < REMOVED_SP_DATE ~ 6,
                             ADDED_SP_DATE == START_DATE & END_DATE < REMOVED_SP_DATE ~ 6,
                             ADDED_SP_DATE < START_DATE & REMOVED_SP_DATE == END_DATE ~ 6,
                             ADDED_SP_DATE == START_DATE & REMOVED_SP_DATE == END_DATE ~ 6,
                             TRUE ~ 0),
         chk_reason = case_when(chk_spp == 1 ~ 'spp removed before reg started',
                                chk_spp == 2 ~ 'spp removed before end date',
                                chk_spp == 3 ~ 'spp added after start date but within range',
                                chk_spp == 4 ~ 'spp added after start date and outside range',
                                chk_spp == 5 ~ 'spp removed same day regulation started',
                                chk_spp == 6 ~ 'spp within range',
                                chk_spp == 0 ~ 'not considered')) %>%
  #filter(chk_spp != 0) %>%
  select(CLUSTER, REGULATION_ID, MANAGEMENT_TYPE, MANAGEMENT_TYPE_USE, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPECIES_ITIS_USE, COMMON_NAME_USE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE, END_DATE,
         chk_spp, chk_reason)

# ALOT OF CONDITIONS NOT CONSIDERED AND NEED TO BE INVESTIGATED
chk_spp_sum <- chk_spp %>%
  group_by(chk_spp, chk_reason) %>%
  summarise(N = n()) %>%
  mutate(resolution = case_when(chk_spp == 1 ~ 'remove',
                                chk_spp == 2 ~ 'adjust end date',
                                chk_spp == 3 ~ 'adjust start date',
                                chk_spp == 4 ~ 'remove',
                                chk_spp == 5 ~ 'remove',
                                chk_spp == 6 ~ 'no adjustment',
                                chk_spp == 0 ~ 'manual check'))

mh_final <- mh_sp_expanded %>%
  mutate(RM_SPP = case_when(REMOVED_SP_DATE < START_DATE ~ 1,
                            ADDED_SP_DATE > START_DATE & END_DATE < ADDED_SP_DATE ~ 1,
                            REMOVED_SP_DATE == START_DATE ~ 1,
                            TRUE ~ 0),
         IMP_START_DATE = case_when(ADDED_SP_DATE > START_DATE & END_DATE > ADDED_SP_DATE ~ 1,
                                    TRUE ~ 0),
         START_DATE2 = case_when(IMP_START_DATE == 1 ~ ADDED_SP_DATE,
                                TRUE ~ START_DATE),
         IMP_END_DATE = case_when(REMOVED_SP_DATE < END_DATE & REMOVED_SP_DATE > START_DATE ~ 1,
                                  TRUE ~ 0),
         END_DATE2 = case_when(IMP_END_DATE == 1 ~ REMOVED_SP_DATE,
                              TRUE ~ END_DATE)) 

# REMOVE ONE OF THE RECORDS WHEN THERE ARE TWO WITH THE SAME START DATE AND VALUE IN A CLUSTER

chk_start <- mh_final %>%
  filter(RM_SPP == 0) %>%
  group_by(CLUSTER, MANAGEMENT_TYPE, START_DATE, COMMON_NAME_USE, START_YEAR, VALUE) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) 

# Check (cluster 37 - min size limit hogfish)
chk <- mh_final %>%
  filter(CLUSTER == 37) %>%
  select(CLUSTER, SECTOR_ID, REGULATION_ID, FR_CITATION, SPP_NAME, COMMON_NAME_USE, SECTOR_USE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE, END_DATE, VALUE,
         RM_SPP, REG_CHANGE, REG_REMOVED, MULTI_REG, GENERAL, COMPLEX, ADJUSTMENT, REDUNDANT)

# Check (cluster 38 - min size limit hogfish)
chk2 <- mh_final %>%
  filter(CLUSTER == 38) %>%
  select(CLUSTER, SECTOR_ID, REGULATION_ID, FR_CITATION, SPP_NAME, COMMON_NAME_USE, SECTOR_USE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE, END_DATE, VALUE,
         RM_SPP, REG_CHANGE, REG_REMOVED, MULTI_REG, GENERAL, COMPLEX, ADJUSTMENT, REDUNDANT)

# Check (cluster 2537)
chk <- mh_final %>%
  filter(CLUSTER == 2537)

# Check (cluster 31 and 2724) 
chk <- mh_final %>%
  filter(COMMON_NAME_USE == 'GROUPER, GAG', MANAGEMENT_TYPE == 'BAG LIMIT', FMP == 'REEF FISH RESOURCES OF THE GULF OF MEXICO') %>%
  select(CLUSTER, SECTOR_ID, REGULATION_ID, FR_CITATION, SPP_NAME, COMMON_NAME_USE, 
         EFFECTIVE_DATE, INEFFECTIVE_DATE, ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE, END_DATE, VALUE,
         #IMP_START_DATE, IMP_END_DATE, NEVER_IMPLEMENTED, 
         RM_SPP) %>%
  arrange(CLUSTER, EFFECTIVE_DATE)


# EXPORT DATA ####

#write.csv(mh_final, paste0("./Output/MHprocessed_", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)



