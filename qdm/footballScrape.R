library(dplyr)

library(rvest)

scores = read_html("https://www.sports-reference.com/cfb/years/2018-schedule.html") %>% 
  html_table(., header = FALSE) %>% 
  `[[`(1)

scoresGood = scores %>% 
  select(X6, X7, X8, X9, X10) %>% 
  filter(X6 != "Winner") %>% 
  mutate(X6 = sub("\\([0-9]+\\)", "", X6), 
         X6 = stringr::str_trim(X6), 
         X9 = sub("\\([0-9]+\\)", "", X9), 
         X9 = stringr::str_trim(X9), 
         homeTeam = ifelse(X8 == "@", X9, X6), 
         awayTeam = ifelse(homeTeam == X9, X6, X9), 
         homeScore = ifelse(X8 == "@", X10, X7),
         awayScore = ifelse(homeTeam == X9, X7, X10)) %>% 
  select(homeTeam, awayTeam, homeScore, awayScore)

scoresGood = scoresGood[1:844, ]

writeClipboard(sort(unique(unlist(c(scoresGood$homeTeam, scoresGood$awayTeam)))))

writeClipboard(scoresGood$homeTeam)

writeClipboard(scoresGood$awayTeam)

writeClipboard(scoresGood$homeScore)

writeClipboard(scoresGood$awayScore)