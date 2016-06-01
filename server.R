#### Load necessary packages and data ####
library(shiny)
library(shinydashboard)
library(readr)
library(quanteda)
library(LICORS)
library(networkD3)
library(dplyr)
library(network)
library(sna)
library(ggplot2)
library(ggnet)
library(purrr)

all_links = read_csv("links.csv") %>%
  dmap_at(c("link_source", "link_target"), ~ifelse(.x == 0L, 17, .x))
all_nodes = read_csv("nodes.csv")

server = function(input, output) {
  links = all_links
  nodes = all_nodes
  output$prob = renderMenu({
    prob_threshold = input$prob
    links = filter(all_links, link_value >= prob_threshold)
    nodes = filter(all_nodes, name %in% link_source || name %in% link_target)
  })

  output$plot = renderPlot({
    # Filter out edges less than input probability threshold 
    edgelist <- all_links %>%
      filter(link_value >= input$prob_thresh)

    edgelist <- edgelist %>%
      dmap_at("link_value", ~round(.x, digits = 2))
    
    # Construct network object
    net <- network(edgelist, matrix = "edgelist", ignore.eval = FALSE,
                   directed = TRUE, loops = FALSE)
    
    # Construct network plot
    ggnet2(net, label = as.character(seq(1:17)), label.color = "black",
             arrow.size = 12, arrow.gap = 0.025, edge.size = "link_value",
             edge.label = "link_value", edge.label.size = 3)
    
    # FIX THIS CODE TO TOGGLE SHOWING EDGE LABELS on/off
    # if(input$show_labels == 1) {
    # # edgelist <- edgelist %>%
    # #   dmap_at("link_value", ~round(.x, digits = 2))
    # #   ggnet2(net, label = as.character(seq(1:17)), label.color = "black",
    # #          arrow.size = 12, arrow.gap = 0.025, edge.size = "link_value",
    # #          edge.label = "link_value", edge.label.size = 3)
    # 
    # } else {
    #   # ggnet2(net, label = as.character(seq(1:17)), label.color = "black",
    #   #        arrow.size = 12, arrow.gap = 0.025, edge.size = "link_value")
    # }
  })
}