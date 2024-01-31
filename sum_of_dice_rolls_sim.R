#This shiny app allows the user chooses a number of dice to roll between 1 and 20, 
#the number of times to roll the dice between 1 and 1000
#then sum the dice rolls and show a graph of the sum of the dice rolls 
#with the mean of the dice rolls and the distribution of the dice rolls

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Dice Roll Simulator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "numDice", 
        "Number of Dice to Roll", 
        min = 0, 
        max = 20, 
        value = 1,
        step = 2),
      sliderInput(
        "numRolls", 
        "Number of Rolls", 
        min = 0, 
        max = 1000, 
        value = 10,
        step = 50)
    ),
    mainPanel(
      plotOutput(
        "diceRolls"
        )
    )
  )
)

server <- function(input, output) {
  output$diceRolls <- renderPlot({
    dice <- sample(1:6, 
                   input$numDice * input$numRolls, 
                   replace = TRUE)
    dice <- matrix(dice, 
                   ncol = input$numDice)
    dice <- rowSums(dice)
    ggplot(data.frame(dice), aes(x = dice)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      geom_vline(aes(xintercept = mean(dice)), color = "red", linetype = "dashed", size = 1) +
      geom_density(aes(y = ..count..), color = "red", size = 1.5, alpha = 0.5) +
      ggtitle("Sum of Dice Rolls") +
      xlab("Sum of Dice Rolls") +
      ylab("Frequency")
  })
}

shinyApp(ui = ui, server = server)