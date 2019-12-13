pageWithSidebar(
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