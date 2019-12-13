library(shiny)

ui <- fluidPage(shinythemes::themeSelector(), 
                titlePanel("Predicting a car's transmission type."),
                sidebarLayout(
                  sidebarPanel(p("For each of the following data types, please input
                                  an appropriate value."),
                               selectInput(inputId = "cyl",
                                           label = "How many cylinders does the car have?",
                                           choices = c("4", "6", "8")),
                               numericInput(inputId = "mpg",
                                            label = "What is the mpg for the car?",
                                            value = 25),
                               sliderInput("disp", "What is the dispersion for the car?",
                                           min = 65, max = 480, value = 230),
                               actionButton("submit", ("Submit"))),
                  mainPanel(verbatimTextOutput("summary"), 
                            verbatimTextOutput("recommendation"))))

server <- function(input, output) {
  
  modelCreation <- glm(am ~ as.factor(cyl) + mpg + disp, 
                       data = mtcars, 
                       family = "binomial")
  
  newData <- reactive({
    df <- data.frame(cyl = input$cyl, 
                     mpg = input$mpg, 
                     disp = input$disp,
                     am = "")
  })
  
  out <- eventReactive(input$submit, {
    predictedProbability <- predict(modelCreation, newdata = newData(), type = "response")
    round(predictedProbability, 3)
  })
  
  output$summary <- renderText({
    paste("The probability of being a manual transmission is ", out(), ".", sep = "")
  })
  
  output$recommendation <- renderText({
    if(out() < .5) print("You have a less than 50-50 chance! Don't do it!")
      else if(out() > .5 & out() < .8) print("That is pretty reasonable. Think about it")
        else print("Do it!")
  })
}

shinyApp(ui, server)
