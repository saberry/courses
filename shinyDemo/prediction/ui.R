fluidPage(shinythemes::themeSelector(), 
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