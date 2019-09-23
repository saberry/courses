server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$cyl,
           "4" = 4,
           "6" = 6,
           "8" = 8)
  })
  
  # Generate a summary of the dataset ----
  
  predictionFunction = 0.000238265 + (1.447619532 * input$mpg) + 
    () + ()
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}