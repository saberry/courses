library(shiny)

ui <- fluidPage(titlePanel("Genre Selection"), 
                sidebarLayout(
                  sidebarPanel(
                    fluidRow(
                      column(6, selectInput("Genre", label=h5("Choose a genre"),
                                            choices=c('Country','Zappa'))
                      ))),
                  mainPanel(fluidRow(
                    htmlOutput("frame")
                  )
                  )
                ))