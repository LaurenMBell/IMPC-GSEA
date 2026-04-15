library("devtools")
pak::pak("GSEA-MSigDB/GSEA_R")
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(dplyr)

#most of the layout if based off this file:
#source(system.file('extdata', 'Run.GSEA.R', package = 'GSEA'))

ui <- page_navbar(
  
  title = "GSEA-IMPC | Morgun-Shulzhenko Lab",
  
  sidebar = sidebar(
    title = "Selection menu", 
    width = 350,
    
    fileInput("user_gene_set", 
              label = "Upload your RNK-formatted gene set:"
    ), #closes file input
    
    textOutput("gene_set_check"), 
    
    numericInput("permutations", 
                 label = "Number of permutations: ", 
                 value = 1000, 
                 min = 1, 
                 max = 10000
    )
  ), #closes side bar
  
  card(
    title = "User Selected Parameters", 
    label = "selected_params",
  )
  
) # closes ui

server <- function(input, output) {
  
  output$gene_set_check <- renderText ({
    req(input$user_gene_set)
    
    file_path <- input$user_gene_set$datapath
    
    if (user_set_check(file_path) == TRUE) {
      "No errors detected!!"
    } else {
      "Incorrect format"
    }
    
  }) #closes gene_set_check 
  
}

shinyApp(ui = ui, server = server)



