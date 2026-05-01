# 4/16/26 last update

library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(dplyr)

source("gene_set_val.R")
source("GSEApreranked.R")
source("blitzGSEA.R") # might have to interace with reticulate
source("npGSEA.R")

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
                  label = "Upload your .RNK-formatted gene set file:"
                  # ADD DROP HEADER CHECK OPTION
        ),
        textOutput("gene_set_check"),
        
        #=== collapse gene set to gene symbols, must provide CHIP file ====
        checkboxInput("collapsed",
                      label = "Collapse gene set to gene symbols?",
                      value = FALSE),
        conditionalPanel(
          condition = "input.collapsed == true",
          fileInput("chip_file", "Upload .CHIP file:"),
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
          c("Gene set permutation" = "gene_set_permutation",
            "Gamma distribution approximation (blitzGSEA)" = "blitzgsea",
            "Estimation via moments of test statistic (npGSEA)" = "npgsea")
        ), 
        
        actionButton("run", "Run GSEA with selected parameters", 
                     style = "color: white; background-color: #7BC950; border-color: #F7F2F0;")
        
      ),
      
      # main panel — results go here
      uiOutput("GSEA_RESULTS")
    )
  )
  
) # closes ui
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

server <- function(input, output) {
  
  # ==== check input gene set file formatting =====
  output$gene_set_check <- renderText({
    req(input$user_gene_set)
    if (user_set_check(input$user_gene_set$datapath)) "Success!" else "Incorrect format"
  })
  
  # ==== check input CHIP file formatting =====
  output$CHIP_check <- renderText({
    req(input$chip_file)
    if (user_CHIP_check(input$chip_file$datapath)) "Success!" else "Incorrect format"
  })
  
  
  # ==== RUNNING GSEA =====
  data("examplePathways")
  data("exampleRanks")
    
  gsea_results <- reactiveVal(NULL)
  has_run <- reactiveVal(FALSE)
    
  # example data, REMOVE LATER
  ranks <- reactiveVal(setNames(exampleRanks, names(exampleRanks)))
    
  observeEvent(input$run, {
    if (input$null_model == "gene_set_permutation") {
      res <- runGSEApreranked(examplePathways, ranks(), nperm = input$permutations)
      message("fgsea returned ", nrow(res), " rows")
      gsea_results(res)
      has_run(TRUE)
     }
  })
    
  output$GSEA_RESULTS <- renderUI({
    if (!has_run()) {
      p("Waiting for user to select parameters and start run.")
    } else {
      tagList(
        tableOutput("gsea_table"),
        plotOutput("gsea_plot")
      )
    }
  })
    
  output$gsea_table <- renderTable({
    req(gsea_results())
    gsea_results() |> arrange(padj) |> head(20) |> select(pathway, pval, padj, NES)
  })
    
  output$gsea_plot <- renderPlot({
    req(gsea_results())
    top_pathway <- gsea_results() |> arrange(padj) |> pull(pathway) |> head(1)
    plots <- plotPathways(top_pathway, examplePathways, ranks())
    req(length(plots) > 0)
    plots[[1]]
  })
    
} # closes server

shinyApp(ui = ui, server = server)