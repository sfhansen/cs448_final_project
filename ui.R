library(shiny)
library(shinydashboard)

ui = dashboardPage(skin = "black", 
  dashboardHeader(title = "Clickstream Viewer"),
  dashboardSidebar(
    menuItem(
      sliderInput("prob_thresh", label = "Probability Threshold", 
                  min = 0.00001, max = 0.1, value = 0.00001, step = 0.01)),
    menuItem(  
      checkboxInput("show_prob_labels", label = "Toggle Probabilities", 
                      value = FALSE))
  ),
  dashboardBody(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    fluidRow(
      
      tabBox(width = 600, height = 600,
        tabPanel("Markov Chain", plotOutput("graph")),
        tabPanel("Data Table", dataTableOutput("table")),
        tabPanel("Heatmap", plotOutput("heatmap"))
      )
    ),
    
    fluidRow(
      
      box(
        title = "Irreducibility", width = 4, background = "light-blue",
        "This chain is irreducible: all states are reachable from all other states."
      ),
      box(
        title = "Periodicity", width = 4, background = "yellow",
        "This chain is aperiodic: no cycles exist."
      ),
      box(
        title = "Invariant Distribution", width = 4, background = "red",
        "A unique invariant distribution exists."
      )
      
      
      # tags$p(style = "font-size: 10px;"),
      # tags$head(tags$style(HTML('
      # .main-header .logo {
      #   font-family: "Georgia", Times, "Times New Roman", serif;
      #   font-weight: bold;
      #   font-size: 24px;
      # }'))),
      # infoBoxOutput("irreducibleBox"),
      # infoBoxOutput("periodicBox"),
      # infoBoxOutput("invariantBox")
    )
  )
)