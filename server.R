# AUTHORS: Samuel Hansen & Shirbi Ish-Shalom
# This is the server script for the Clickstream Explorer App

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

transition_matrix <- read_rds("trans_matrix.rds")

# Builds markov chain out of normalized transition matrix
# You can call is.irreducible() and period() on this object
clickstream_markovchain <- new("markovchain", states = as.character(seq(1,17,1)),
                               transitionMatrix = transition_matrix,
                               name = "clickstream_markovchain")
# Unique invariant distribution exists if chain is irreducible and aperiodic
if (is.irreducible(clickstream_markovchain) & period(clickstream_markovchain) == 1){
  invariant_dist <- statdistr(transition_matrix) %>% c() 
  # Otherwise no unique distribution exists, so set all pi to uniformly distributed 
} else {
  invariant_dist <- rep(1/nrow(transition_matrix),nrow(transition_matrix))
}

# Initialize Server
server = function(input, output) {
  observe({
    # Filter out edges less than input probability threshold
    if(is.null(input$source_node_choice) || "All" %in% input$source_node_choice){
      edgelist <- all_links %>%
        filter(link_value >= input$prob_thresh,
               link_source != link_target) %>%
        mutate(weighted_prob = link_value * 5)
      nodes = all_nodes %>%
        filter(name %in% edgelist$link_source,
               name %in% edgelist$link_target)
    } else {
      edgelist <- all_links %>%
        filter(link_value >= input$prob_thresh,
               link_source != link_target, 
               link_source %in% input$source_node_choice) %>%
        mutate(weighted_prob = link_value * 5)
      nodes = all_nodes %>%
        filter(name %in% edgelist$link_source,
               name %in% edgelist$link_target)
    }
    
    # Render Markov graph
    output$graph = renderPlot({
      if(nrow(edgelist) > 1 ) {
        # Display network with probability labels toggeled ON 
        if(input$show_prob_labels == TRUE) {
          
          # Round probabilities to display 2 digits 
          edgelist <- edgelist %>%
            dmap_at("link_value", ~signif(.x, digits = 2))
          
          # Construct network object
          net <- network(edgelist, matrix = "edgelist", ignore.eval = FALSE,
                         directed = TRUE, loops = FALSE) 
          
          # Set invariant distribution as vertex attribute 
          net %v% "invariant_dist" = invariant_dist
          
          # Display network with probability labels 
          ggnet2(net, label = "vertex.names", 
                 label.color = "black", arrow.size = 12, arrow.gap = 0.025, 
                 edge.size = "weighted_prob", edge.label = "link_value", 
                 edge.label.size = 3, size = "invariant_dist", 
                 edge.lty = 1, max_size = 15, node.color = "steelblue3", 
                 edge.color = "gray63") +
            guides(color = FALSE, size = FALSE)
        } else {
          # Construct network object without probability labels 
          net <- network(edgelist, matrix = "edgelist", ignore.eval = FALSE,
                         directed = TRUE, loops = FALSE) 
          
          # Set invariant distribution as vertex attribute 
          net %v% "invariant_dist" = invariant_dist
          
          # Display network without probability labels 
          ggnet2(net, label = "vertex.names", 
                 label.color = "gray34", arrow.size = 12, arrow.gap = 0.025, 
                 edge.size = "weighted_prob", size = "invariant_dist", 
                 edge.lty = 1, max_size = 15, node.color = "steelblue3", 
                 edge.color = "gray67") + 
            guides(color = FALSE, size = FALSE)
        }
      }
    })
    
    # Render heatmap plot 
    output$heatmap = renderPlot({
      all_links %>%
        ggplot(mapping = aes(x = factor(link_target), factor(link_source))) +
        geom_tile(mapping = aes(fill = log10(link_value)), colour = "white") +
        scale_fill_gradient(low = "white", high = "steelblue",
                            guide = guide_legend(title = "Log10\nProbability")) +
        labs(x = "Target Page", y = "Source Page",
             title = "Markov Chain Transition Matrix") +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0))
    })
    
    output$table = renderDataTable({
      # Filter out edges less than input probability threshold 
      edgelist %>%
        plyr::rename(c("link_source" = "Source",
                       "link_target" = "Target",
                       "link_value" = "Transition Probability"))
    }, options = list(lengthMenu = c(10, 20, 50), pageLength = 10))
  })
}