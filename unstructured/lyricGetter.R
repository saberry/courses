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
         searchSong = gsub("\\s", "+", searchSong))

lyricGetter = function(artist, song, date) {
  
  Sys.sleep(1)
  
  out = tryCatch({
    link = paste("api.genius.com/search?q=", song, sep = "")
    
    songSearch = GET(link, add_headers(Authorization = paste("Bearer", token, sep = " ")))
    
    searchResult = jsonlite::fromJSON(content(songSearch, as = "text"))$response$`hits`
    
    searchResult$result$strDistance <- stringdist::stringdist(searchResult$result$primary_artist$name, 
                                                              artist, method = "jw")
    
    whichResult = which.min(searchResult$result$strDistance)
    
    # whichResultValue = if(length(whichResult) > 0) {
    #   whichResult[1]
    # } else 1
    
    returnedArtist = searchResult$result$primary_artist$name[[whichResult]]
    
    returnedArtistID = searchResult$result$primary_artist$id[[whichResult]]
    
    returnedSong = searchResult$result$title[[whichResult]]
    
    songLink = searchResult$result$url[[whichResult]]
    
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
                     stringDistance = searchResult$result$strDistance[whichResult])
    
    return(res)
  }, error = function(e) {
    return(song)
  })
  
  closeAllConnections()
  
  return(out)
}

debugonce(lyricGetter)

lyricGetter(monthlyTop10$artist[1], monthlyTop10$searchSong[1], monthlyTop10$date[1])

lyricGetter(monthlyTop10$artist[5], monthlyTop10$searchSong[5], monthlyTop10$date[5])

lyricGetter(monthlyTop10$artist[7], monthlyTop10$searchSong[7], monthlyTop10$date[7])

tests <- monthlyTop10[1:20, ]

allLyrics = mapply(lyricGetter, monthlyTop10$artist, monthlyTop10$searchSong, monthlyTop10$date, 
              SIMPLIFY = FALSE)

goodRes = unlist(lapply(1:length(allLyrics), function(x) is.data.frame(allLyrics[[x]])))

goodRes[which(goodRes == TRUE)]

allLyricsDF = data.table::rbindlist(allLyrics[which(goodRes == TRUE)])