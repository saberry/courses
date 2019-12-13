function(input, output) {
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