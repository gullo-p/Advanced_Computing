shinyUI(
  pageWithSidebar(
    
    headerPanel('Decision boundary for linear discriminant function'),
    
    sidebarPanel(
      numericInput('mean_solapp', 'mean_solvency_approved', 155.2582, min=0, max=3000),
      
      numericInput('mean_soldeni', 'mean_solvency_denied', 102.9975, min=0, max=3000),
      
      numericInput('mean_PIapp', 'mean_PIratio_approved', 4.018225, min = 0, max = 25),
      
      numericInput('mean_PIdeni', 'mean_PIratio_denied',10.0378, min = 0, max = 25),
      
      numericInput('sd_solapp', 'sd_solvency_approved', 19.4534, min=0, max=40),
      
      numericInput('sd_soldeni', 'sd_solvency_denied', 28.52164, min=0, max=40),
      
      numericInput('sd_PIapp', 'sd_PIratio_approved', 1.045781, min = 0, max = 10),
      
      numericInput('sd_PIdeni', 'sd_PIratio_denied', 2.014876, min = 0, max = 10)
    ),
    
    mainPanel(
      plotOutput('plot1'),
      
      tableOutput('contingency_matrix')
    )
  )
)


