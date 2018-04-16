library(dplyr)

library(ggplot2)

visualizationData = data.frame(dollar_spent_per_patient = abs(rlogis(127, location = 1000, scale = 100)),
                               patient_days_admitted = sample(1:30, 127, replace = TRUE),
                               department = sample(c("cardiac", "cancer", "general"), 127, 
                                                   prob = c(.2, .2, .6), replace = TRUE)) %>% 
  mutate(dollar_spent_per_patient = ifelse(department != "general", 
                                           dollar_spent_per_patient + 500, 
                                           dollar_spent_per_patient), 
         dollar_spent_per_patient = ifelse(department == "cancer", 
                                           dollar_spent_per_patient + 500, 
                                           dollar_spent_per_patient), 
         dollar_spent_per_patient = ifelse(patient_days_admitted > 15, 
                                           dollar_spent_per_patient + 500, 
                                           dollar_spent_per_patient))

ggplot(visualizationData, aes(patient_days_admitted, dollar_spent_per_patient)) +
  geom_point() +
  theme_minimal()

glmData = data.frame(customer.satisfaction.scores.year = abs(rlogis(529, location = 70, scale = 5)), 
                     training.sessions.attended.year = sample(0:12, 529, replace = TRUE), 
                     csr.id = sample(1000:9999, 529, replace = TRUE)) %>% 
  mutate(customer.satisfaction.scores.year = ifelse(training.sessions.attended.year < 4, 
                                                    customer.satisfaction.scores.year - 10, 
                                                    ifelse(training.sessions.attended.year < 7 & 
                                                             training.sessions.attended.year > 4, 
                                                           customer.satisfaction.scores.year + 5, 
                                                           customer.satisfaction.scores.year + 15)), 
         customer.satisfaction.scores.year = ifelse(customer.satisfaction.scores.year > 100, 
                                                    100, 
                                                    customer.satisfaction.scores.year))

plot(glmData)

summary(lm(customer.satisfaction.scores.year ~ training.sessions.attended.year, 
           data = glmData))

abData = data.frame(PageConfiguration = sample(c("A", "B"), 1059, replace = TRUE), 
                    MinutesOnPage = rnorm(1059, mean = 3, sd = .5)) %>% 
  mutate(MinutesOnPage = ifelse(PageConfiguration == "A", 
                                MinutesOnPage + 3, MinutesOnPage))

abData %>% 
  group_by(PageConfiguration) %>% 
  summarize(mean = mean(MinutesOnPage))

performanceData = data.frame(daily_net_profit_thousand = rnorm(1095, mean = 5, sd = 1.5), 
                             facility_location = rep(c("403 Barr", "710 Oakland", 
                                                      "10 Maple"), 
                                                    each = 365)) %>% 
  mutate(daily_net_profit_thousand = ifelse(facility_location == "403 Barr", 
                                            daily_net_profit_thousand + 3.5, 
                                            daily_net_profit_thousand))

performanceData %>% 
  group_by(facility_location) %>% 
  summarize(mean(daily_net_profit_thousand))

outcomeData = data.frame(storeID = 1:475, 
                         outcomeClosedOpen = sample(0:1, 475, replace = TRUE), 
                         employeeCount = rnorm(475, mean = 10, sd = 2.75), 
                         dailyNetProfitThousands = rlogis(475, location = 4, scale = .25), 
                         quartersWithHealthViolations = sample(0:3, 475, replace = TRUE, 
                                                   prob = c(.6, .2, .15, .05)), 
                         peoplePerSqMile = abs(rlogis(475, location = 200, scale = 15))
) %>% 
  mutate(employeeCount = ifelse(outcomeClosedOpen == 1, 
                                employeeCount + 5, employeeCount), 
         dailyNetProfitThousands = ifelse(outcomeClosedOpen == 1, 
                                          dailyNetProfitThousands + 5, 
                                          dailyNetProfitThousands), 
         quartersWithHealthViolations = ifelse(outcomeClosedOpen == 1, 
                                               sample(0:1), 
                                          quartersWithHealthViolations), 
         peoplePerSqMile = ifelse(outcomeClosedOpen == 1, 
                                  peoplePerSqMile + abs(rlogis(1, location = 100, scale = 15)), 
                                  peoplePerSqMile))

outcomeData %>% 
  group_by(outcomeClosedOpen) %>% 
  select(-storeID) %>% 
  summarize_all(mean)
