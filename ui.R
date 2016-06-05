# AUTHORS: Samuel Hansen & Shirbi Ish-Shalom
# This is the UI script for the Clickstream Explorer App

#### Load necessary packages and data ####
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(network)
library(ggplot2)
library(ggnet)
library(purrr)
library(markovchain)
library(sna)
library(ggplot2)
library(DTMCPack)
library(LICORS)
library(networkD3)

# Parameters
node_names <- c(
  "1" = "Frontpage",
  "2" = "News",
  "3" = "Tech",
  "4" = "Local",
  "5" = "Opinion",
  "6" = "On-air",
  "7" = "Misc",
  "8" = "Weather",
  "9" = "Health",
  "10" = "Living",
  "11" = "Business",
  "12" = "Sports",
  "13" = "Summary", 
  "14" = "BBS", 
  "15" = "Travel", 
  "16" = "Msn-News", 
  "17" = "Msn-Sports")

# Read in data 
all_links <- read_rds("links.rds") %>%
  dmap_at(c("link_source", "link_target"), ~ifelse(.x == 0L, 17, .x)) %>%
  dmap_at(c("link_source", "link_target"), as.factor) %>%
  dmap_at(c("link_source", "link_target"), ~plyr::revalue(.x, node_names))

all_nodes <- read_rds("nodes.rds") %>% 
  dmap_at("name", as.factor) %>%
  dmap_at("name", ~plyr::revalue(.x, node_names))

# Initialize UI
ui = dashboardPage(skin = "black", 
  dashboardHeader(title = "Clickstream Explorer"),
  dashboardSidebar(
    menuItem(
      sliderInput("prob_thresh", label = "Probability Threshold", 
                  min = 0.00001, max = 0.1, value = 0.00001, step = 0.01)),
    menuItem(  
      checkboxInput("show_prob_labels", label = "Toggle Probabilities", 
                      value = FALSE)),
    menuItem(  
      selectInput("source_node_choice", label = "Select Start Nodes",
                  choices = c("All", as.vector(all_nodes$name)), selected = "All", multiple = TRUE, 
                  selectize = TRUE))
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
    )
  )
)