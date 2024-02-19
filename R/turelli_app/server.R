# Define server logic
server <- function(input, output) {
  
  output$dynamicalPlot <- renderPlot({
    p <- numeric(input$N + 1)
    p[1] <- input$p0
    
    for (i in 1:input$N) {
      p[i+1] <- infection_frequency_step(p[i],input$F_val,input$mu_val,input$s_h)
      #p[i+1] <- input$F_val * p[i]^2 + input$mu_val * p[i] + input$s_h
    }
    
    #plot(0:input$N, p, type = "b", xlab = "Time Step (t)", ylab = "Value (p)")
    tibble(p=p) |>
      ggplot(aes(x=1:length(p),y=p)) +
      geom_point() +
      ylim(c(0,1)) + 
      labs(x = "Generation", y = "Frequency of infection")
  })
}