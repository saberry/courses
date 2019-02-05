library(dplyr)

projectData = readxl::read_excel("C://Users/sberry5/Downloads/MARK_70120/MARK 70120/Group Project/projectData.xlsx", sheet = "Data")

projectData2 = projectData

newDat = lapply(4:15, function(x) {
  
  randomSelect = sample(sample(1:2, 10, replace = TRUE), 1)

  res = lapply(1:nrow(projectData2), function(y) {
    if(randomSelect == 1) {
      projectData2[y, x] + sample(runif(100, min = 1000, max = 10000), 1) 
    } else projectData2[y, x] - sample(runif(100, min = 1000, max = 10000), 1)
  })
  
  return(unlist(res))
})

newDat = as.data.frame(newDat)

projectData2[, 4:15] = newDat

projectData %>% 
  select(4:15) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(method = "circle", type = "lower", hc.order = TRUE)

projectData2 %>% 
  select(4:15) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(method = "circle", type = "lower", hc.order = TRUE)

projectData = rbind(projectData, projectData2) %>% 
  mutate(county = rep(c("Cook County", "Suffolk County"), each = 108))

simDatSuffolk = data.frame(year = rep(2009:2017, each = 12), 
                    month = rep(1:12, times = length(2009:2017)), 
                    population_avg_income = sample(x = runif(100000, min = 100000, max = 105000), size = 108), 
                    population_avg_age = sample(x = runif(100000, min = 32, max = 36), size = 108), 
                    county = "Suffolk County")

simDatCook = data.frame(year = rep(2009:2017, each = 12), 
                    month = rep(1:12, times = length(2009:2017)), 
                    population_avg_income = sample(x = runif(100000, min = 95000, max = 100000), size = 108), 
                    population_avg_age = sample(x = runif(100000, min = 33, max = 37), size = 108), 
                    county = "Cook County")

countyData = rbind(simDatSuffolk, simDatCook)

write.csv(countyData, "marketingAnalytics/countyData.csv", row.names = FALSE)

write.csv(projectData, "marketingAnalytics/projectData.csv", row.names = FALSE)
