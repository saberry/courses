meets = readr::read_csv(unz("dataWrangling/data/powerlifting-database.zip", "meets.csv"))

lifts = readr::read_csv(unz("dataWrangling/data/powerlifting-database.zip", "openpowerlifting.csv"))

meets = meets %>% 
  mutate(townState = paste(MeetTown, MeetState, sep = ", "))

geoCodes = dismo::geocode(unique(meets$townState))

geoCodes = geoCodes %>% 
  dplyr::select(originalPlace, interpretedPlace, longitude, latitude) %>% 
  na.omit()

combined = left_join(lifts, meets, by = "MeetID") %>% 
  left_join(., geoCodes, by = c("townState" = "originalPlace"))

topFive = lifts %>% 
  group_by(Sex, Division, WeightClassKg, Equipment) %>% 
  filter(Place < 5) %>% 
  summarize(meanTotal = mean(TotalKg))

topFiveFederation = combined %>% 
  group_by(Sex, Division, WeightClassKg, Federation) %>% 
  filter(Place < 5) %>% 
  summarize(meanTotal = mean(TotalKg))
