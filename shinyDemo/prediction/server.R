function(input, output) {
  
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