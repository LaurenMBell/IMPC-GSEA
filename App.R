# 4/16/26 last update

library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(dplyr)

# most of the layout is based off this file:
# source(system.file('extdata', 'Run.GSEA.R', package = 'GSEA'))
source("gene_set_val.R")

pre = TRUE #tick for when parameters are submitted GET RID OF THIS LATER

ui <- page_navbar(
  
  title = "IMPC-GSEA | Morgun-Shulzhenko Lab",

  # PARAMETER + GSEA PAGE ++++++++++++++++++++++++++++++++++++
  nav_panel(
    "GSEA",
    layout_sidebar(
      sidebar = sidebar(
        card_header("User Selected Parameters"),
        
        #=== upload file and make sure it is formatted correctly ====
        fileInput("user_gene_set",
                  label = "Upload your RNK-formatted gene set file:"
                  # ADD DROP HEADER CHECK OPTION
        ),
        textOutput("gene_set_check"),
        
        #=== collapse gene set to gene symbols, must provide CHIP file ====
        checkboxInput("collapsed",
                      label = "Collapse gene set to gene symbols?",
                      value = FALSE),
        conditionalPanel(
          condition = "input.collapsed == true",
          fileInput("chip_file", "Upload CHIP file:"),
          textOutput("CHIP_check")
        ),
        
        #=== number of permutations ====
        numericInput("permutations",
                     label = "Number of permutations: ",
                     value = 10000,
                     min = 1,
                     max = 100000
        ),
        
        #=== null model type ====
        selectInput(
          "null_model",
          "Select method of estimating enrichment score significance:",
          c("Gene label permutation" = "gene_label_permutation",
            "Gamma distribution approximation (blitzGSEA)" = "blitzgsea",
            "Estimation via moments of test statistic (npGSEA)" = "npgsea")
        ), 
        
        actionButton("run", "Run GSEA with selected parameters", 
                     style = "color: white; background-color: #7BC950; border-color: #F7F2F0;")
        
      ),
      
      # main panel — results go here
      div(
        if (pre) {
          p("Waiting for user to select parameters")
        } else {
          p("Running GSEA with selected parameters!")
          # figure out how to do a cool animation here !!
        }
      )
    )
  )
  
) # closes ui
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

server <- function(input, output) {
  
  # ==== check input gene set file formatting =====
  output$gene_set_check <- renderText({
    req(input$user_gene_set)
    
    file_path <- input$user_gene_set$datapath
    
    if (user_set_check(file_path) == TRUE) {
      "No errors detected :)"
    } else {
      "Incorrect format :("
    }
  })
  
  # ==== check input CHIP file formatting =====
  output$CHIP_check <- renderText({
    req(input$chip_file)
    
    file_path <- input$chip_file$datapath
    
    if (user_CHIP_check(file_path) == TRUE) { 
      "No errors detected :0"
    } else {
      "Incorrect format :P"
    }
  })
  
} # closes server

shinyApp(ui = ui, server = server)