# Define UI
ui <- fluidPage(
  titlePanel("Wolbachia Infection Frequency Model"),
  sidebarLayout(
    sidebarPanel(
      numericInput("p0", "Initial Value (p0):", value = 0.4, min = 0, max = 1),
      numericInput("N", "Number of Time Steps (N):", value = 100, min=1,max=1000),
      numericInput("F_val", "Parameter F:", value = 0.5,min=0.0,max=2.0),
      numericInput("mu_val", "Parameter mu:", value = 0.5,min=0.0,max=1.0),
      numericInput("s_h", "Parameter s_h:", value = 0.0,min=0.0,max=1.0)
    ),
    mainPanel(
      plotOutput("dynamicalPlot")
    )
  )
)

# Run the application
#shinyApp(ui = ui, server = server)