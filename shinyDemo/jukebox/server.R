library(rvest)

server <- function(input, output) {
  
  zappaSongs <- c("Peaches en Regalia", "My Guitar Wants To Kill Your Mama", 
                  "Wind up Working in a Gas Station", "Cheepnis", "Dog Breath", 
                  "Flakes", "Inca Roads", "Montana", "Andy", "Son Of Orange County", 
                  "Village Of The Sun", "Joes Garage", "San Berdino", 
                  "Echidnas Arf", "Doreen")
  
  countrySongs <- c("buy me a boat", "boys round here")
  
  dataInput <- reactive({
    if(input$Genre == "Country") {
      countrySongs
    } else zappaSongs
  })
  
  embeddedLinkMaker <- reactive({
    songConstructor <- paste("https://www.youtube.com/results?search_query=frank+zappa+", 
                             gsub("\\s", "+", sample(dataInput(), 1)), sep = "") 
    
    videoLink <- read_html(songConstructor) %>% 
      html_node("a[href*='watch']") %>% 
      html_attr('href') %>% 
      paste("https://www.youtube.com", ., sep = "") %>% 
      gsub("watch\\?v\\=", "embed/", .)
    
    closeAllConnections()
    
    songLink <- paste0(videoLink)
    
  })
  
  
  output$frame <- renderUI({
    my_test <- tags$iframe(src=embeddedLinkMaker(), height=600, width=535)
    print(my_test)
    my_test
  })
}
