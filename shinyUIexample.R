ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny Text"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "cyl",
                  label = "How many cylinders does the car have?",
                  choices = c("4", "6", "8")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "mpg",
                   label = "What is the mpg for the car?",
                   value = 25)
    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)
