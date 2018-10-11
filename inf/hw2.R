library(dplyr)

employment1 = data.frame(daysEmployed = rnorm(462, mean = 220, sd = 50), 
                        salary = rnorm(462, mean = 75000, sd = 10000), 
                        daysAbsent = rnorm(462, mean = 2, sd = 10), 
                        department = sample(c("IT", "CS", "Marketing", "HR"), 
                                            462, replace = TRUE, prob = c(.1, .5, .3, .1)), 
                        education = sample(1:3, 462, replace = TRUE), 
                        gender = sample(1:2, 462, replace = TRUE))

employment1$daysAbsent[which(employment1$daysAbsent < 0)] = 0

employment1$daysEmployed[which(employment1$daysEmployed > 365)] = 365

employment = data.frame(daysEmployed = rnorm(1000, mean = 250, sd = 50), 
                        department = sample(c("IT", "CS", "Marketing", "HR"), 
                                            1000, replace = TRUE, prob = c(.05, .505, .305, .05)), 
                        education = rep(0, 1000))

employment = employment %>% 
  mutate(salary = daysEmployed + 80000 + rnorm(1000, mean = 5000, sd = 10000), 
         daysAbsent = rnorm(1000, mean = 4, sd = 3), 
         gender = sample(1:2, 1000, replace = TRUE, prob = c(.7, .3)))


employment$daysAbsent[which(employment$daysAbsent < 0)] = 0

employment$daysEmployed[which(employment$daysEmployed > 365)] = 365

employment = rbind(employment, employment1)

employment$education[which(employment$salary < quantile(employment$salary)[2])] = 1

employment$education[which(employment$salary > quantile(employment$salary)[2] &
                             employment$salary < quantile(employment$salary)[3])] = 2

employment$education[which(employment$salary > quantile(employment$salary)[3])] = 3

employment$education = ordered(employment$education, levels = 1:3, 
                               labels = c("high school", "bachelor", "post grad"))

employment$gender = factor(employment$gender, levels = 1:2, 
                           labels = c("female", "male"))

employment$salary[employment$gender == "female"] = employment$salary - rnorm(1, mean = 1500, 500)

employment$daysEmployed = round(employment$daysEmployed)

employment$daysAbsent = round(employment$daysAbsent)

write.csv(employment, "inf/hw2Dat.csv", row.names = FALSE)

save(employment, file = "inf/hw2Dat.RData")
