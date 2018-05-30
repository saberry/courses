library(dplyr)

managerIDs = paste(sample(LETTERS[c(22:26)], 18, replace = TRUE),
                   sample(1000:9999, 18, replace = TRUE),
                   sample(LETTERS[c(22:26)], 18, replace = TRUE), sep = "")

managerIDs[6] = "I511L"

poss = lexicon::hash_sentiment_vadar %>% filter(y > 1)

negs = lexicon::hash_sentiment_vadar %>% filter(y < 0)

adjs = lexicon::hash_grady_pos %>% filter(pos == "Adjective")

fearWords = lexicon::nrc_emotions %>% 
  filter(fear == 1, anticipation == 0, surprise == 0, trust == 0) %>% 
  select(term)

negAdjs = adjs$word[adjs$word %in% negs$x]

negAdjs = negAdjs[negAdjs %in% fearWords$term]

posAdjs = adjs$word[adjs$word %in% poss$x]

exampleData = data.frame(jobTasksSDSA = sample(1:5, 1384, replace = TRUE), 
                         jobBalanceSDSA = sample(1:5, 1384, replace = TRUE))

exampleData = exampleData %>% 
  mutate(jobPaySDSA = jobTasksSDSA + ceiling(rnorm(1384, mean = 1, sd = .5)), 
         jobRecognitionSDSA = jobTasksSDSA + ceiling(rnorm(1384, mean = 1, sd = .5)), 
         jobAdvancementSDSA = jobTasksSDSA + ceiling(rnorm(1384, mean = 1, sd = .5)), 
         jobCoworkersSDSA = jobBalanceSDSA + ceiling(rnorm(1384, mean = 1, sd = .5)), 
         jobManagementSDSA = jobBalanceSDSA + ceiling(rnorm(1384, mean = 1, sd = .5))) %>% 
  mutate_all(funs(ifelse(. > 7, 7, .))) %>% 
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
                                  list(sample(posAdjs, 5, replace = FALSE))))


exampleData %>% 
  group_by(team, manager) %>% 
  summarise(n(), mean(jobManagementSDSA, na.rm = TRUE))

exampleData %>% select_if(is.numeric) %>% na.omit() %>%  cor()
