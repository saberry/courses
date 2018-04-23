library(dismo)

restLocation = paste(paste(healthData$BUILDING, 
                     healthData$STREET, sep = " "),
                     healthData$BORO, 
                     "New York", 
                     healthData$ZIPCODE, 
                     "United States", sep = ", ") %>% 
  stringr::str_squish(.)

locations = dismo::geocode(restLocation, oneRecord = TRUE)

locations = locations %>% 
  dplyr::select(originalPlace, longitude, latitude)

healthData = cbind(healthData, locations)

healthData = healthData %>% 
  filter(!(is.na(longitude)))

healthCenter = data.frame(lat = 40.749352, 
                          lon = -73.939059)

distances = lapply(1:nrow(healthData), function(x) {
  geosphere::distVincentyEllipsoid(p1 = c(healthData$longitude[x], healthData$latitude[x]), 
                                   p2 = c(healthCenter$lon, healthCenter$lat))
})

healthDataGrouped = healthData %>% 
  # mutate(dohDistanceMeter = unlist(distances)) %>% 
  tidyr::unite(col = nameLocation, DBA, BUILDING , remove = FALSE) %>% 
  group_by(nameLocation) %>%
  arrange(lubridate::mdy(`GRADE DATE`)) %>% 
  mutate(observation = 1:n())
  
write.csv(healthData, "inf/healthViolationsDistances.csv", row.names = FALSE)
