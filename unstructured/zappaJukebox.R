#!/usr/bin/env Rscript

library(rvest)

songList <- c("Peaches en Regalia", "My Guitar Wants To Kill Your Mama", 
              "Wind up Working in a Gas Station", "Cheepnis", "Dog Breath", 
              "Flakes", "Inca Roads", "Montana", "Andy", "Son Of Orange County", 
              "Village Of The Sun", "Joes Garage", "San Berdino", 
              "Echidnas Arf", "Doreen")

songConstructor <- paste("https://www.youtube.com/results?search_query=frank+zappa+", 
                         gsub("\\s", "+", sample(songList, 1)), sep = "") 

videoLink <- read_html(songConstructor) %>% 
  html_node("a[href*='watch']") %>% 
  html_attr('href') %>% 
  paste("https://www.youtube.com", ., sep = "")

commandLineOpen <- if(Sys.info()[["sysname"]] != "Windows") {
  paste("open", videoLink, sep = " ")
} else paste("start chrome", videoLink, sep = " ")

system(commandLineOpen)
