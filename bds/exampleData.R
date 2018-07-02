library(dplyr)

managerIDs = paste(sample(LETTERS[c(22:26)], 18, replace = TRUE),
                   sample(1000:9999, 18, replace = TRUE),
                   sample(LETTERS[c(22:26)], 18, replace = TRUE), sep = "")

managerIDs[6] = "I511L"

poss = lexicon::hash_sentiment_vadar %>% filter(y > .25)

negs = lexicon::hash_sentiment_vadar %>% filter(y < 0)

adjs = lexicon::hash_grady_pos %>% filter(pos == "Adjective")

fearWords = lexicon::nrc_emotions %>% 
  filter(fear == 1, anticipation == 0, surprise == 0, trust == 0) %>% 
  select(term)

negAdjs = adjs$word[adjs$word %in% negs$x]

negAdjs = negAdjs[negAdjs %in% fearWords$term]

posAdjs = adjs$word[adjs$word %in% poss$x]

exampleData = data.frame(jobTasksSDSA = sample(1:7, 1384, replace = TRUE), 
                         jobBalanceSDSA = sample(1:7, 1384, replace = TRUE))

exampleData = exampleData %>% 
  mutate(jobPaySDSA = jobTasksSDSA + ceiling(rnorm(1384, mean = 1, sd = .5)), 
         jobRecognitionSDSA = jobTasksSDSA + ceiling(rnorm(1384, mean = 1, sd = .5)), 
         jobAdvancementSDSA = jobTasksSDSA + ceiling(rnorm(1384, mean = 1, sd = .5)), 
         jobCoworkersSDSA = jobBalanceSDSA + ceiling(rnorm(1384, mean = 1, sd = .5)), 
         jobManagementSDSA = jobBalanceSDSA + ceiling(rnorm(1384, mean = 1, sd = .5))) %>% 
  mutate_all(funs(ifelse(. > 7, 7, .))) %>%
  mutate_all(funs(ifelse(. < 1, 1, .))) %>% 
  mice::ampute(., prop = .15, mech = "MAR", 
               patterns = matrix(c(rep(1, 6), 0), 1, 7)) %>% 
  magrittr::extract2(10) %>%  
  mutate(id = paste(sample(LETTERS, 1384, replace = TRUE),
                    sample(100:999, 1384, replace = TRUE),
                    sample(LETTERS, 1384, replace = TRUE), sep = ""), 
         team = sample(c("R&D", "DSI", "Finance", "Health", "Ethics"), 1384, 
                       prob = c(.325, .05, .325, .15, .15), replace = TRUE), 
         manager = ifelse(team == "R&D", sample(managerIDs[1:5], n(), replace = TRUE), 
                          ifelse(team == "DSI", sample(managerIDs[6:7], n(), prob = c(.2, .8), replace = TRUE), 
                                 ifelse(team == "Finance", sample(managerIDs[8:12], n(), replace = TRUE), 
                                 ifelse(team == "Health", sample(managerIDs[13:15], n(), replace = TRUE), 
                                 sample(managerIDs[16:18], n(), replace = TRUE))))), 
         jobManagementSDSA = ifelse(manager == "I511L", abs(jobManagementSDSA - 2), jobManagementSDSA)) %>% 
  rowwise() %>% 
  mutate(describeManager = ifelse(manager == "I511L", 
                                  list(sample(negAdjs, 5, replace = FALSE)), 
                                  list(sample(posAdjs, 5, replace = FALSE))), 
         genderFM = sample(1:2, 1), 
         degreeBMP = ifelse(genderFM == 1, sample(1:3, 1, prob = c(.5, .35, .15)), 
                            sample(1:3, 1, prob = c(.25, .25, .5))),
         citizenNY = sample(1:2, 1, prob = c(.2, .8)), 
         age = sample(22:55, 1), 
         tenure = sample(0:10, 1), 
         salary = ifelse(degreeBMP == 1, sample(45000:55000, 1), 
                         ifelse(degreeBMP == 2, sample(53000:60000, 1), sample(60000:150000, 1))), 
         salary = ifelse(tenure > 7, salary + sample(15000:25000, 1), salary))

glimpse(exampleData)

library(mgcv)

testGAM = gam(tenure ~ s(salary, k = 5), data = exampleData)

exampleData %>% 
  group_by(team, manager) %>% 
  summarise(n(), mean(jobManagementSDSA, na.rm = TRUE))

exampleData %>% select_if(is.numeric) %>% na.omit() %>%  cor()

exampleData %>% 
  select(id, starts_with("job")) %>% 
  write.csv(., "bds/exerciseData/easy/week04and05.csv")

exampleData %>% 
  select(id, starts_with("job")) %>% 
  jsonlite::write_json(., "bds/exerciseData/week04and05.json")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary) %>% 
  write.csv(., "bds/exerciseData/easy/week06.csv")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary) %>% 
  write.table(., "bds/exerciseData/week06.tab")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure) %>% 
  write.csv(., "bds/exerciseData/easy/week07.csv")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure) %>%
  add_row(., id = "", team = "", manager = "", 
          jobTasksSDSA = "", jobBalanceSDSA = "", jobPaySDSA = "", 
          jobRecognitionSDSA = "", jobAdvancementSDSA = "", jobCoworkersSDSA = "", 
          jobManagementSDSA = "", salary = "", tenure = "", .before = 1) %>% 
  write.table(., "bds/exerciseData/week07.txt", sep = "|")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure, age) %>% 
  write.csv(., "bds/exerciseData/easy/week08.csv")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure, age) %>%
  add_row(., id = "average", jobTasksSDSA = 2.95) %>% 
  write.csv(., "bds/exerciseData/week08.csv")

week10 = exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure, age, 
         genderFM, degreeBMP, citizenNY)

a = rep(NA,dim(week10)[2])

a = rbind(a,a,a)

write.table(a, file = "bds/exerciseData/week10.csv", row.names = FALSE, 
            na = "", col.names = FALSE, sep = ",")

write.table(week10, file = "bds/exerciseData/week10.csv", row.names = FALSE, na = "", 
            append = TRUE, sep = ",")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure, age, 
         genderFM, degreeBMP, citizenNY) %>% 
  write.csv(., "bds/exerciseData/easy/week10.csv")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure, age, 
         genderFM, degreeBMP, citizenNY) %>% 
  write.csv(., "bds/exerciseData/easy/week11.csv")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure, age, 
         genderFM, degreeBMP, citizenNY) %>% 
  mutate(X = sample(1:10000, 1)) %>% 
  write.table(., "bds/exerciseData/week11.txt", sep = "^")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure, age, 
         genderFM, degreeBMP, citizenNY) %>% 
  write.csv(., "bds/exerciseData/easy/week12.csv")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure, age, 
         genderFM, degreeBMP, citizenNY) %>%
  bind_rows(.) %>% 
  write.csv(., "bds/exerciseData/week12.csv")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure, age, 
         genderFM, degreeBMP, citizenNY, describeManager) %>%
  ungroup() %>% 
  mutate(describeManager = as.character(describeManager)) %>% 
  write.csv(., "bds/exerciseData/easy/week13.csv")

exampleData %>% 
  select(id, team, manager, starts_with("job"), salary, tenure, age, 
         genderFM, degreeBMP, citizenNY, describeManager) %>% 
  save(., file = "bds/exerciseData/week13.RData")
