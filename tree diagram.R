library(shiny)
library(igraph)
library(ggraph)


#The main goal is to make a shiny app that creates a tree diagram with the following sets of nodes and edges:
#Nodes: on the first level: a node called S, P(A) and P(notA) on the second level, P(B|A), P(notB|A), P(B|notA), and P(notB|notA) on the third level
#Edges: S to P(A) and P(notA), P(A) to P(B|A), P(A) to P(notB|A), P(notA) to P(B|notA), and P(notA) to P(notB|notA)
#Label the nodes as follows: S = "S", P(A) = "P(A)", P(notA) = "P(notA)", P(B|A) = "P(B|A)", P(notB|A) = "P(notB|A)", P(B|notA) = "P(B|notA)", and P(notB|notA) = "P(notB|notA)"
#Label the edges with the probabilities as follows: S to P(A) = pA, S to P(notA) = 1 - pA, P(A) to P(B|A) = pB_A * pA, P(A) to P(notB|A) = (1 - pB_A) * pA, P(notA) to P(B|notA) = pB_notA * (1 - pA), and P(notA) to P(notB|notA) = (1 - pB_notA) * (1 - pA)
#The user should be able to input the values of pA and use 
#P(A) = pA,
#P(notA) = 1 - pA,
#P(B|A) = pB_A * pA,
#P(notB|A) = (1 - pB_A) * pA,
#P(B|notA) = pB_notA * (1 - pA),
#P(notB|notA) = (1 - pB_notA) * (1 - pA)
#The app should then display the tree diagram with the probabilities

ui <- fluidPage(
  titlePanel("Tree Diagram"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "pA", 
        "P(A)", 
        min = 0, 
        max = 1, 
        value = 0.5,
        step = 0.1),
      sliderInput(
        "pB_A", 
        "P(B|A)", 
        min = 0, 
        max = 1, 
        value = 0.5,
        step = 0.1),
      sliderInput(
        "pB_notA", 
        "P(B|notA)", 
        min = 0, 
        max = 1, 
        value = 0.5,
        step = 0.1)
    ),
    mainPanel(
      plotOutput(
        "treeDiagram"
      )
    )
  )
)

server <- function(input, output) {
  output$treeDiagram <- renderPlot({
    #create a graph
    g <- graph_from_data_frame(data.frame(
      from = c("S", "S", "P(A)", "P(A)", "P(notA)", "P(notA)"),
      to = c("P(A)", 
             "P(notA)", 
             paste0("P(B|A)\n = ", input$pB_A * input$pA), 
             paste0("P(notB|A)\n = ", (1 - input$pB_A) * input$pA), 
             paste0("P(B|notA)\n = ", input$pB_notA * (1 - input$pA)), 
             paste0("P(notB|notA)\n = ", (1 - input$pB_notA) * (1 - input$pA))
             ),
      label = c(input$pA, 1 - input$pA, input$pB_A, 1 - input$pB_A, input$pB_notA, 1 - input$pB_notA)
    ))
    #plot the graph
    ggraph(g, layout = "dendrogram") + 
      geom_node_point() + 
      geom_edge_link(aes(label = label), hjust = 1.25) + 
      geom_node_text(aes(label = name), hjust = 1)
  })
}



shinyApp(ui = ui, server = server)