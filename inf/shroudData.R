test = function(num) {
  
  num = num - sample(1:10, 1)
  
  if(num < 90) {
    num
  } else {
    num = c(num, num - sample(1:10, 1))
    test(num)
  }
}

set.seed(12)

shroudData = data.frame(shroudsProduced = rpois(1095, 5), 
                        employeeCount = sample(15:30, 1095, replace = TRUE), 
                        maxOutsideTempF = as.vector(plyr::raply(3, c(sample(15:32, 91, replace = TRUE), 
                                                                  sample(35:50, 45, replace = TRUE), 
                                                                  sample(50:72, 45, replace = TRUE), 
                                                                  sample(80:95, 92, replace = TRUE), 
                                                                  sample(50:72, 92, replace = TRUE)))), 
                        ceramicMixTons = unlist(replicate(500, c(92, test(95))))[1:1095], 
                        accidentLogged = sample(0:1, 1095, prob = c(.025, .975), replace = TRUE), 
                        kilnMaintNY = rep(c(rep(0, 29), 1), 39)[1:1095], 
                        day = 1:1095
) %>% 
  mutate(shroudsProduced = ifelse(employeeCount > 27, 
                                  shroudsProduced + 3, 
                                  shroudsProduced), 
         shroudsProduced = ifelse(maxOutsideTempF > 82, 
                                  shroudsProduced + 1, 
                                  shroudsProduced))

hist(shroudData$shroudsProduced)

goodfit(shroudData$shroudsProduced, type = "poisson")

summary(lm(shroudsProduced ~ employeeCount, data = shroudData))

write.csv(shroudData, "inf/shroudData.csv", row.names = FALSE)




set.seed(3)

redlights = data.frame(intersection_id = sample(10000:99999, 976, replace = FALSE),
                       camera_installed_ny = sample(0:1, 976, replace = TRUE))

redlights = redlights %>% 
  mutate(citation_count = ifelse(camera_installed_ny == 1, rpois(976, lambda = 4), 
                                 rpois(976, lambda = 10)))


summary(goodfit(redlights$citation_count, type = "poisson"))

summary(redlights)

hist(redlights$citation_count)

redlights %>% 
  group_by(camera_installed_ny) %>% 
  summarise(mean(citation_count))

summary(glm(citation_count ~ as.factor(camera_installed_ny), 
            data = redlights, 
            family = poisson))


write.csv(redlights, "inf/redlights.csv", row.names = FALSE)
