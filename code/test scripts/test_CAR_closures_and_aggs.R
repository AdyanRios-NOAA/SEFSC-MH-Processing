##CAR CLOSURES AND AGGREGATES TESTS####

## redo closure assessment with new data tables ####

#veiw all mng types
mh_expanded %>% distinct(MANAGEMENT_TYPE_USE)


#ISOLATING CAR FMP
MH_CARsort <- mh_expanded %>%
  filter(FMP == "REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")

#ALL CLOSURES
MH_CARsort %>% filter(MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "FISHING YEAR", "PROHIBITED SPECIES"))%>%
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID, 
         COMMON_NAME_USE) %>%
  distinct(CLUSTER)

test = filter(MH_CARsort, CLUSTER %in% c(39, 40, 100, 101, 102, 103, 104, 105, 165, 371, 372, 713, 714, 727, 728, 765, 766, 767, 768, 769, 770, 771, 772, 795, 796, 800, 801, 802, 803, 807, 808, 809, 810, 1021, 1022, 1440, 1441, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2046, 2047, 2048, 2049, 2050, 2051, 2052, 2053, 2054, 2055))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME, COMMON_NAME_USE) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
View(test)
write.csv(test, paste0("./Output/CAR test output/CAR_ALL_test", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)

#SEASONAL Recreational closures
MH_CARsort %>% filter(MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "FISHING YEAR", "PROHIBITED SPECIES"),
                      SECTOR_USE == "RECREATIONAL",
                      MANAGEMENT_STATUS_USE == "SEASONAL")%>%
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID, 
         COMMON_NAME_USE) %>%
  distinct(CLUSTER)

test = filter(MH_CARsort, CLUSTER %in% c(713, 1440, 1981, 1983, 1985, 1987, 1989, 1991, 
                                         1993, 1995, 1997, 1999, 2001, 2003))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME, COMMON_NAME_USE) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
View(test)
write.csv(test, paste0("./Output/CAR test output/CAR_SR_test", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)
                       
#ONCE Recreational closures
MH_CARsort %>% filter(MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "FISHING YEAR", "PROHIBITED SPECIES"),
                      SECTOR_USE == "RECREATIONAL",
                      MANAGEMENT_STATUS_USE == "ONCE")%>%
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID, 
         COMMON_NAME_USE) %>%
  distinct(CLUSTER)

test = filter(MH_CARsort, CLUSTER %in% c(39, 102, 104, 371, 713, 727, 765, 767, 769, 771, 796, 
                                         800, 802, 807, 809, 1021, 2046, 2048, 2049, 2050, 2051))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME, COMMON_NAME_USE) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
View(test)
write.csv(test, paste0("./Output/CAR test output/CAR_OR_test", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)

#ONCE Commercial closures
MH_CARsort %>% filter(MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "FISHING YEAR", "PROHIBITED SPECIES"),
                      SECTOR_USE == "COMMERCIAL",
                      MANAGEMENT_STATUS_USE == "ONCE")%>%
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID, 
         COMMON_NAME_USE) %>%
  distinct(CLUSTER)

test = filter(MH_CARsort, CLUSTER %in% c(40, 100, 101, 103, 105, 165, 372, 714, 728, 766, 768, 770, 772, 795, 801, 803, 808, 810, 1022, 2047, 2052, 2053, 2054, 2055))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME, COMMON_NAME_USE) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
View(test)
write.csv(test, paste0("./Output/CAR test output/CAR_OC_test", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)

#SEASONAL Commercial closures
MH_CARsort %>% filter(MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "FISHING YEAR", "PROHIBITED SPECIES"),
                      SECTOR_USE == "COMMERCIAL",
                      MANAGEMENT_STATUS_USE == "SEASONAL")%>%
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID, 
         COMMON_NAME_USE) %>%
  distinct(CLUSTER)

test = filter(MH_CARsort, CLUSTER %in% c(714, 1441, 1982, 1984, 1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000, 2002, 2004))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME, COMMON_NAME_USE) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
View(test)
write.csv(test, paste0("./Output/CAR test output/CAR_SC_test", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)

##NOTES: full species expansion makes very large data set because most closures, esp. one time closures, 
  # are for "ALL SPECIES". For analysis, DWH closures closures were removed and "all species" were condensed
  # to one line for species group

# rerun without species expansion ####

MH_CARsort <- mh_dates %>%
  filter(FMP == "REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")

#SEASONAL Commercial closures
MH_CARsort %>% filter(MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "FISHING YEAR", "PROHIBITED SPECIES"),
                      SECTOR_USE == "COMMERCIAL",
                      MANAGEMENT_STATUS_USE == "SEASONAL")%>%
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID, 
         SPP_NAME, SPP_TYPE) %>%
  distinct(CLUSTER)

test = filter(MH_CARsort, CLUSTER %in% c(714, 1441, 1982, 1984, 1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000, 2002, 2004))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME, SPP_TYPE) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))

write.csv(test, paste0("./Output/CAR test output/CAR_SC_DATES_test", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)

#SEASONAL Recreational closures
MH_CARsort %>% filter(MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "FISHING YEAR", "PROHIBITED SPECIES"),
                      SECTOR_USE == "RECREATIONAL",
                      MANAGEMENT_STATUS_USE == "SEASONAL")%>%
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID, 
         SPP_NAME, SPP_TYPE) %>%
  distinct(CLUSTER)

test = filter(MH_CARsort, CLUSTER %in% c(713, 1440, 1981, 1983, 1985, 1987, 1989, 1991, 
                                         1993, 1995, 1997, 1999, 2001, 2003))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME, SPP_TYPE) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))

write.csv(test, paste0("./Output/CAR test output/CAR_SR_DATES_test", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)

## WHY IS IT GIVING ME ONCE IN SEASONAL FILTER??? 

#ONCE Commercial closures
MH_CARsort %>% filter(MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "FISHING YEAR", "PROHIBITED SPECIES"),
                      SECTOR_USE == "COMMERCIAL",
                      MANAGEMENT_STATUS_USE == "ONCE")%>%
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID, 
         SPP_NAME, SPP_TYPE) %>%
  distinct(CLUSTER)

test = filter(MH_CARsort, CLUSTER %in% c(40, 100, 101, 103, 105, 165, 372, 714, 728, 766, 768, 770, 772, 795, 801, 803, 808, 810, 1022, 2047, 2052, 2053, 2054, 2055))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME, SPP_TYPE) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
View(test)
write.csv(test, paste0("./Output/CAR test output/CAR_OC_DATES_test", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)

#ONCE Recreational closures
MH_CARsort %>% filter(MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON", "FISHING YEAR", "PROHIBITED SPECIES"),
                      SECTOR_USE == "RECREATIONAL",
                      MANAGEMENT_STATUS_USE == "ONCE")%>%
  select(CLUSTER, STATUS_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, SECTOR, SUBSECTOR_USE, ZONE, SECTOR_ID, 
         SPP_NAME, SPP_TYPE) %>%
  distinct(CLUSTER)

test = filter(MH_CARsort, CLUSTER %in% c(39, 102, 104, 371, 713, 727, 765, 767, 769, 771, 796, 
                                         800, 802, 807, 809, 1021, 2046, 2048, 2049, 2050, 2051))
select(test, SECTOR_ID, vol, page, CLUSTER, STATUS_TYPE, MANAGEMENT_STATUS_USE, MANAGEMENT_TYPE_USE, 
       VALUE, diff ,diff_days, EFFECTIVE_DATE, START_DATE, CHANGE_DATE,   END_DATE, SECTOR_USE, SPP_NAME, SPP_TYPE) %>%
  arrange(STATUS_TYPE, MANAGEMENT_STATUS_USE, desc(START_DATE), desc(vol), desc(page))
View(test)
write.csv(test, paste0("./Output/CAR test output/CAR_OR_DATES_test", format(Sys.Date(), "%d%b%Y"), ".csv"), row.names = FALSE)

