# 4/16/26 last update

library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(dplyr)

source("helpers/gene_set_val.R")
source("helpers/GSEApreranked.R")
source("helpers/blitzGSEA.R") # might have to interface with reticulate
source("helpers/npGSEA.R")

ui <- page_navbar(
  
  title = "IMPC-GSEA | Morgun-Shulzhenko Lab",

  # PARAMETER + GSEA PAGE ++++++++++++++++++++++++++++++++++++
  nav_panel(
    "GSEA",
    layout_sidebar(
      sidebar = sidebar(
        card_header("User Selected Parameters"),
        
        #=== upload file and make sure it is formatted correctly =======
        fileInput("user_gene_set",
                  label = "Upload your .RNK-formatted gene set file:"
                  # ADD DROP HEADER CHECK OPTION
        ),
        textOutput("gene_set_check"),
        
        #==== collapse gene set to gene symbols, must provide CHIP file ========
        #checkboxInput("collapsed",
                      #label = "Collapse gene set to gene symbols?",
                      #value = FALSE),
        #conditionalPanel(
          #condition = "input.collapsed == true",
          #fileInput("chip_file", "Upload .CHIP file:"),
          #textOutput("CHIP_check")
        #),
        
        #===== number of permutations ======
        numericInput("permutations",
                     label = "Number of permutations: ",
                     value = 10000,
                     min = 1,
                     max = 100000
        ),
        
        #=== null model type =======
        selectInput(
          "null_model",
          "Select method of estimating enrichment score significance:",
          c("Gene set permutation (fgsea)" = "gene_set_permutation",
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
    if (user_set_check(input$user_gene_set$datapath)) "Success!" else "Incorrect format :("
  })
  
  # ==== check input CHIP file formatting =====
  output$CHIP_check <- renderText({
    req(input$chip_file)
    if (user_CHIP_check(input$chip_file$datapath)) "Success!" else "Incorrect format :("
  })
  
  
  # ==== RUNNING GSEA =====
  data("examplePathways")
  data("exampleRanks")
  
  gsea_results <- reactiveVal(NULL)
  has_run <- reactiveVal(FALSE)
  
  # example data, REMOVE LATER
  ranks <- reactiveVal(setNames(exampleRanks, names(exampleRanks)))
  
  # ===== run GSEApreranked (with the sample data) =============================
  observeEvent(input$run, {
    if (input$null_model == "gene_set_permutation") {
      res <- runGSEApreranked(examplePathways, ranks(), nperm = input$permutations)
      message("fgsea returned ", nrow(res), " rows")
      gsea_results(res)
      has_run(TRUE)
    }
  })

  #spit out results for GSEApreranked
  output$GSEA_RESULTS <- renderUI({
    if (!has_run()) {
      p("Waiting for user to select parameters and start run.")
    } else {
      tagList(
        tableOutput("gsea_table"),
        plotOutput("gsea_plot"),
        plotOutput("pathways_plot")
      )
    }
  })
  
  output$gsea_table <- renderTable({
    req(gsea_results())
    gsea_results() |> arrange(padj) |> head(20) |> dplyr::select(pathway, pval, padj, NES)
  })
  
  output$gsea_plot <- renderPlot({
    req(gsea_results())
    top_pathway <- gsea_results() |> arrange(padj) |> pull(pathway) |> head(10)
    plots <- lapply(top_pathway, function(pw) plotEnrichment(examplePathways[[pw]], ranks()))
    req(length(plots) > 0)
    plots[[1]]
  })
  
  output$pathways_plot <- renderPlot({
    req(gsea_results())
    res <- gsea_results()
    topPathwaysUp   <- res |> filter(ES > 0) |> arrange(pval) |> head(10) |> pull(pathway)
    topPathwaysDown <- res |> filter(ES < 0) |> arrange(pval) |> head(10) |> pull(pathway)
    topPathways <- c(topPathwaysUp, rev(topPathwaysDown))
    plotGseaTable(examplePathways[topPathways], ranks(), res, gseaParam = 0.5)
  })
  
  # ===== run GSEA (with the sample data) =============================
  
  
} # closes server

shinyApp(ui = ui, server = server)