#I want to have one graph with two plots.
#For the first plot, i want the plot to be a standard distribution plot with inputs of standard deviation and mean.
#For the second plot, i want the plot to be a t distribution with inputs of standard deviation, mean, and degrees of freedom.
#I want to have the two plots on the same graph.

#I will use the ggplot2 package to create the graph.
library(ggplot2)
library(shiny)


plot_distribution <- function(mean_norm, mean_t, sd, df, samplesize){
  #possible x values for the plot
  x <- seq(-10, 10, length = samplesize)
  #I will create a data frame with the y values for the standard distribution plot.
  y <- dnorm(x = x, mean = mean_norm, sd = sd)
  #I will create a data frame with the y values for the t distribution plot.
  y2 <- dt(x = x, df = df, ncp = mean_t)
  
  #create two data frames, one for each plot.
  df1 <- data.frame(x, y)
  df2 <- data.frame(x, y2)
  
  #create the plots
  library(ggplot2)

  p <- ggplot(df1, aes(x, y)) + 
    geom_line(linetype = "solid", linewidth = 1.25, aes(color = "Standard Distribution")) +
    geom_line(linetype = "longdash", data = df2, aes(x, y2, color = "T Distribution"), linewidth = 1) +
    labs(title = "Standard and T Distribution Plots",
         x = "X",
         y = "P(x)") +
    theme_minimal() +
    scale_color_manual(name = NULL, values = c("Standard Distribution" = "black", "T Distribution" = "red")) +
    theme(legend.position = "right")
  
  # Print the plot
  print(p)
  
  return(p)
}






ui <-fluidPage(
 sliderInput("mean_norm", "Mean for Standard Distribution", min = -10, max = 10, value = 0),
 sliderInput("mean_t", "NCP for T Distribution", min = -5, max = 5, value = 0, step = 0.1),
 sliderInput("sd", "Standard Deviation", min = 0, max = 5, value = 1, step = 0.1),
 sliderInput("df", "Degrees of Freedom", min = 1, max = 100, value = 1),
 sliderInput("samplesize", "Sample Size", min = 0, max = 300, value = 100, step = 10),
 plotOutput("plot")
)




#output the plot and the table
server <- function(input, output) {
  output$plot <- renderPlot({
    plot_distribution(input$mean_norm, input$mean_t, input$sd, input$df, input$samplesize)
  })
}

shinyApp(ui, server)

