#################################
### Scraping Billboard Hot 50 ###
#################################

library(dplyr)

library(rvest)

# linkConstructor

dateSequence = seq.Date(from = as.Date("1975-12-28"), to = as.Date("2019-12-31"), by = "month")

# dateSequence[length(dateSequence)]

top50Getter = function(date) {
  
  Sys.sleep(1)
  
  out = tryCatch({
    link = paste("https://www.billboard.com/charts/country-songs/", date, sep = "")
    
    linkRead = read_html(link)
    
    number1 = data.frame(song = linkRead %>% 
                           html_nodes(".chart-number-one__title") %>% 
                           html_text(), 
                         artist = linkRead %>% 
                           html_nodes(".chart-number-one__artist") %>% 
                           html_text())
    
    top50 = data.frame(song = linkRead %>% 
                         html_nodes(".chart-list-item__title-text") %>% 
                         html_text(),
                       artist = linkRead %>% 
                         html_nodes(".chart-list-item__artist") %>% 
                         html_text())
    
    top50 = rbind(number1, top50)
    
    top50 = top50 %>% 
      mutate_all(funs(gsub("\n", "", .))) %>% 
      mutate(date = date)
    
    closeAllConnections()
    
    return(top50)
  }, error = function(e) {
    return(date)
  })
  return(out)
}

allTop50 = lapply(dateSequence, function(x) top50Getter(x))

goodBadList <- lapply(allTop50, function(x) is.data.frame(x))

good <- allTop50[which(goodBadList == TRUE)]

goodOut <- data.table::rbindlist(good)

bad <- allTop50[which(goodBadList == FALSE)]

i <- length(bad)

badList <- rep(list(data.frame()), i)

while(i != 0) {
  
  Sys.sleep(1)
  
  badList[[i]] = top50Getter(bad[[i]]) 
  
  if(length(badList[[i]]) > 1) {
    i <- i - 1
  }
  
  print(i)
}

badRerunOut <- data.table::rbindlist(badList)

allTop50 <- rbind(badRerunOut, goodOut)

allTop50 = allTop50 %>% 
  distinct() %>% 
  mutate_all(funs(stringr::str_squish(.)))

save(allTop50, file = "unstructured/data/countryTop50.RData")

## Most common chord ##