##############################
### Scraping Top 50 Lyrics ###
##############################

library(dplyr)

library(httr)

library(rvest)

load("unstructured/data/countryTop50.RData")

id = "S6MtzxA7RXPxQiLNAuATCTv9VQ3k17nuAXQWkt_txCR7N2pZwpoKVde-FPbMRFE9"

secret = "cmBaeo9kWHyFEjuSetApT-pyXmCcKjF9msaZAgQiBXCXq_UJvVnwPNyjvZHKmJXaXc5Dza7hLs3ogtSbOk0GMw"

token = "7sWvcDUwbMnD5C6s4WebTfoKEwbUHLKkqgNkv6ZnOMPpxPTTWpzmbg9Kr2FRE_zX"

monthlyTop10 = allTop50 %>% 
  group_by(date) %>% 
  slice(., 1:10) %>% 
  ungroup() %>% 
  group_by(song) %>% 
  slice(., 1) %>% 
  mutate(searchSong = gsub('[[:punct:]]', "", song, perl = TRUE), 
         searchSong = gsub("\\s", "+", searchSong), 
         searchArtist = stringr::str_extract(artist, "(?<=The\\s)\\w+|\\w+"))

lyricGetter = function(artist, song, date) {
  
  Sys.sleep(1)
  
  out = tryCatch({
    link = paste("api.genius.com/search?q=", song, sep = "")
    
    songSearch = GET(link, add_headers(Authorization = paste("Bearer", token, sep = " ")))
    
    searchResult = content(songSearch)
    
    whichResult = grep(artist, searchResult$response$hits)
    
    whichResultValue = if(length(whichResult) > 0) {
      whichResult[1]
    } else 1
    
    returnedArtist = searchResult$response$hits[[whichResultValue]]$result$primary_artist$name
    
    returnedArtistID = searchResult$response$hits[[whichResultValue]]$result$primary_artist$id
    
    returnedSong = searchResult$response$hits[[whichResultValue]]$result$title
    
    songLink = searchResult$response$hits[[whichResultValue]]$result$url
    
    lyrics = read_html(songLink) %>% 
      html_nodes(".lyrics") %>% 
      html_text()
    
    res = data.frame(returnedArtistName = returnedArtist, 
                     returnedArtistID = returnedArtistID, 
                     returnedSong = returnedSong,
                     artist = artist, 
                     song = song,
                     lyrics = lyrics, 
                     date = date, 
                     warningIndicator = ifelse(length(whichResult) == 0, 1, 0))
    
    return(res)
  }, error = function(e) {
    return(song)
  })
  
  closeAllConnections()
  
  return(out)
}

debugonce(lyricGetter)

lyricGetter(monthlyTop10$searchArtist[5], monthlyTop10$searchSong[5], monthlyTop10$date[5])

allLyrics = mapply(lyricGetter, monthlyTop10$searchArtist, monthlyTop10$searchSong, monthlyTop10$date, 
              SIMPLIFY = FALSE)

goodRes = unlist(lapply(1:length(allLyrics), function(x) is.data.frame(allLyrics[[x]])))

goodRes[which(goodRes == TRUE)]

allLyricsDF = data.table::rbindlist(allLyrics[which(goodRes == TRUE)])