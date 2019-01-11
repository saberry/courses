####################
### Zappa Lyrics ###
####################

library(dplyr)

library(httr)

library(rvest)

token = "7sWvcDUwbMnD5C6s4WebTfoKEwbUHLKkqgNkv6ZnOMPpxPTTWpzmbg9Kr2FRE_zX"

zappa = rep(list(list()), 100)

for(i in 1:100) {
  link = paste("api.genius.com/artists/41921/songs?q=per_page=20&page=", i, sep = "")
  
  zapContent = GET(link, add_headers(Authorization = paste("Bearer", token, sep = " ")))  
  
  parsedCont = jsonlite::fromJSON(content(zapContent, as = "text"))
  
  result = data.frame(title = parsedCont$response$songs$title,
                      artist = parsedCont$response$songs$primary_artist$name,
                      url = parsedCont$response$songs$url)
  
  zappa[[i]] = result
  
  nextPage = parsedCont$response$next_page
  
  if(is.null(nextPage)) break
}

zappa = data.table::rbindlist(zappa)

zappaClean = zappa %>% 
  filter(artist == "Frank Zappa") %>% 
  mutate(cleanTitle = tolower(gsub("(\\s\\[.*\\])|(\\s\\(.*\\))", "", title))) %>% 
  group_by(cleanTitle) %>% 
  slice(1L)

zappaClean$url = as.character(zappaClean$url)

lyricGetter = function(songLink) {
  
  Sys.sleep(.5)
  
  out = tryCatch({
    lyrics = read_html(songLink) %>% 
      html_nodes(".lyrics") %>% 
      html_text()
    
    res = data.frame(artist = "Frank Zappa", 
                     lyrics = lyrics, stringsAsFactors = FALSE)
    
    return(res)
  }, error = function(e) {
    return(NA)
  })
  
  closeAllConnections()
  
  return(out)
}

debugonce(lyricGetter)

zappaLyrics = lapply(zappaClean$url, function(x) lyricGetter(x))

zappaLyrics = data.table::rbindlist(zappaLyrics)

save(zappaLyrics, file = "unstructured/data/zappaLyrics.RData")
