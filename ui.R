library(shiny)
library(shinydashboard)
library(networkD3)

ui = dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    menuItem(
      sliderInput("prob_thresh", label = "Transition Probability", 
                  min = 0, max = 0.1, value = 0.1, step = 0.01)
  
      # FIX THIS TO ADD TOGGLE OPTION FOR EDGE LABELS 
      # radioButtons("show_label", label = h3("Display Probabilities"),
      #              choices = list("Yes" = 1, "No" = 2),
      #              selected = 2)
    )
  ),
  dashboardBody(
    fluidRow(
      plotOutput("plot")
    )
  )
)