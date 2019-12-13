install.packages('heatmaply')

library(heatmaply)

library(plotly)

nbaData <- read.csv("https://raw.githubusercontent.com/noahgift/socialpowernba/master/data/nba_2017_players_with_salary_wiki_twitter.csv")


server <- function(input, output) {
  selectedData <- reactive({
    as.matrix(nbaData[, c(input$xcol, input$ycol, input$zcol)])
  })
  
  selectedData2 <- reactive({
    nbaData[, c(input$xcol, input$ycol, input$zcol)]
  })
  
  output$plot1 <- renderPlotly({
    plot_ly(selectedData2(), x = ~get(input$xcol), y = ~get(input$ycol), z = ~get(input$zcol)) %>% add_markers()
  })
  
}

ui <- pageWithSidebar(
  headerPanel('NBA 3D'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(nbaData), 
                selected = names(nbaData)[[5]]),
    selectInput('ycol', 'Y Variable', names(nbaData), 
                selected = names(nbaData)[[6]]),
    selectInput('zcol', 'Z Variable', names(nbaData), 
                selected = names(nbaData)[[7]])
  ),
  mainPanel(
    plotlyOutput('plot1')
  )
)

shinyApp(ui, server)

