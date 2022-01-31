# 4
# CLean up dates

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)


# REMOVE EXPANDED SPECIES  THAT ARE NO LONGER A PART OF THAT GROUP 
# ADJUST START OR END DATES USING SPECIES ADDED AND REMOVED DATES

chk_spp <- mh_sort %>%
  mutate(chk_spp = case_when(REMOVED_SP_DATE < START_DATE ~ 1,
                             REMOVED_SP_DATE < END_DATE & REMOVED_SP_DATE > START_DATE ~ 2,
                             ADDED_SP_DATE > START_DATE & END_DATE > ADDED_SP_DATE  ~ 3,
                             ADDED_SP_DATE > START_DATE & END_DATE < ADDED_SP_DATE ~ 4,
                             TRUE ~ 0),
         chk_reason = case_when(REMOVED_SP_DATE < START_DATE ~ 'spp removed before reg started',
                                REMOVED_SP_DATE < END_DATE & REMOVED_SP_DATE > START_DATE ~ 'spp removed before end date',
                                ADDED_SP_DATE > START_DATE & END_DATE > ADDED_SP_DATE ~ 'spp added after start date but within range',
                                ADDED_SP_DATE > START_DATE & END_DATE < ADDED_SP_DATE ~ 'spp added after start date and outside range')) %>%
  filter(chk_spp != 0) %>%
  select(CLUSTER, REGULATION_ID, MANAGEMENT_TYPE, MANAGEMENT_TYPE_USE, SECTOR_USE, SUBSECTOR, ZONE_USE, SPECIES_ITIS_USE, COMMON_NAME_USE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE, END_DATE,
         chk_spp, chk_reason)

chk_spp_sum <- chk_spp %>%
  group_by(chk_spp, chk_reason) %>%
  summarise(N = n()) %>%
  mutate(resolution = case_when(chk_spp == 1 ~ 'remove',
                                chk_spp == 2 ~ 'adjust end date',
                                chk_spp == 3 ~ 'adjust start date',
                                chk_spp == 4 ~ 'remove'))

mh_final <- mh_sort %>%
  mutate(RM_SPP = case_when(REMOVED_SP_DATE < START_DATE | (ADDED_SP_DATE > START_DATE & END_DATE < ADDED_SP_DATE) ~ 1,
                            TRUE ~ 0),
         IMP_START_DATE = case_when(ADDED_SP_DATE > START_DATE & END_DATE > ADDED_SP_DATE ~ 1,
                                    TRUE ~ 0),
         START_DATE = case_when(ADDED_SP_DATE > START_DATE & END_DATE > ADDED_SP_DATE ~ ADDED_SP_DATE,
                                TRUE ~ START_DATE),
         IMP_END_DATE = case_when(REMOVED_SP_DATE < END_DATE & REMOVED_SP_DATE > START_DATE ~ 1,
                                  TRUE ~ 0),
         END_DATE = case_when(REMOVED_SP_DATE < END_DATE & REMOVED_SP_DATE > START_DATE ~ REMOVED_SP_DATE,
                              TRUE ~ END_DATE)) 

# Check 
chk <- mh_final %>%
  filter(COMMON_NAME_USE == 'GROUPER, GAG', MANAGEMENT_TYPE == 'BAG LIMIT', FMP == 'REEF FISH RESOURCES OF THE GULF OF MEXICO') %>%
  select(CLUSTER, REGULATION_ID, FR_CITATION, SPP_NAME, COMMON_NAME_USE, 
         EFFECTIVE_DATE, INEFFECTIVE_DATE, ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE, END_DATE, VALUE,
         #IMP_START_DATE, IMP_END_DATE, NEVER_IMPLEMENTED, 
         RM_SPP) %>%
  arrange(EFFECTIVE_DATE)


# EXPORT DATA ####

#write.csv(mh_final, paste0("./Output/MHprocessed_", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)



