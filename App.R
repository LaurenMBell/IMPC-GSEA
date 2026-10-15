# 9/22/26 last update

library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(DBI)
library(RSQLite)

source("helpers/gene_set_val.R")
source("helpers/GSEApreranked.R")
#source("helpers/blitzGSEA.R") # might have to interface with reticulate
source("helpers/npGSEA.R")

# https://posit.co/blog/shiny-with-databases
impc_con <- dbConnect(SQLite(), "data/statistical-result.db")

# READ THIS LATER: https://stackoverflow.com/questions/23276491/how-to-implement-a-cleanup-routine-in-r-shiny
onStop(function() {
  dbDisconnect(impc_con)
})

ui <- page_navbar(
  
  title = "IMPC-GSEA | Morgun-Shulzhenko Lab",
  
  # PARAMETER + GSEA PAGE ++++++++++++++++++++++++++++++++++++
  nav_panel(
    "GSEA",
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        card_header("User Selected Parameters"),
        
        ####################################
        selectizeInput("selected_mp_term",
                       "Select phenotype (MP term):",
                       choices = NULL),
        ####################################
        
        #=== upload file and make sure it is formatted correctly =======
        fileInput("user_gene_set",
                  label = "Upload your .RNK-formatted gene set file:"
                  # ADD DROP HEADER CHECK OPTION
        ),
        textOutput("gene_set_check"),
        
        #==== collapse gene set to gene symbols, must provide CHIP file ========
        checkboxInput("collapsed",
                      label = "Collapse gene set to gene symbols?",
                      value = FALSE),
        conditionalPanel(
          condition = "input.collapsed == true",
          fileInput("chip_file", "Upload .CHIP file:"),
          textOutput("CHIP_check")
        ),
        
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
      uiOutput("GSEA_RESULTS_FGSEA")
    )
  )
  
) # closes ui
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

####################################
server <- function(input, output, session) {
  ####################################
  
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
  #ranks <- reactiveVal(setNames(exampleRanks, names(exampleRanks)))
  
  #https://www.rdocumentation.org/packages/DBI/versions/0.5-1/topics/dbGetQuery
  mp_terms <- reactive({
    dbGetQuery(impc_con, "
      SELECT DISTINCT mp_term_id, mp_term_name
      FROM impc_stats
      WHERE mp_term_id IS NOT NULL AND mp_term_id != ''
      ORDER BY mp_term_name
    ")
  })
  
  observe({
    mt <- mp_terms()
    updateSelectizeInput(session, "selected_mp_term",
                         choices = setNames(mt$mp_term_id, mt$mp_term_name),
                         server = TRUE)
  })
  
  ranks <- reactive({
    req(input$selected_mp_term)
    
    df <- dbGetQuery(impc_con, "
      SELECT marker_symbol, effect_size
      FROM impc_stats
      WHERE mp_term_id = ?
        AND effect_size IS NOT NULL AND effect_size != ''
        AND marker_symbol IS NOT NULL AND marker_symbol != ''
    ", params = list(input$selected_mp_term))
    
    df$effect_size <- as.numeric(df$effect_size)
    df <- df[!is.na(df$effect_size), ]
    
    df <- df |>
      dplyr::group_by(marker_symbol) |>
      dplyr::summarise(effect_size = mean(effect_size, na.rm = TRUE)) |>
      dplyr::ungroup()
    
    setNames(df$effect_size, df$marker_symbol)
  })
  
  # ===== run GSEApreranked (WITH SAMPLE DATA) =============================
  observeEvent(input$run, {
    if (input$null_model == "gene_set_permutation") {
      res <- runGSEApreranked(examplePathways, ranks(), nperm = input$permutations)
      message("fgsea returned ", nrow(res), " rows")
      gsea_results(res)
      has_run(TRUE)
    }
    
    if (input$null_model == "blitz_gsea") {
      res <- runGSEApreranked(examplePathways, ranks(), nperm = input$permutations)
      message("blitz_gsea returned ", nrow(res), " rows")
      gsea_results(res)
      has_run(TRUE)
    }
    
    if (input$null_model == "npgsea") {
      res <- runGSEApreranked(examplePathways, ranks(), nperm = input$permutations)
      message("npgsea returned ", nrow(res), " rows")
      gsea_results(res)
      has_run(TRUE)
    }
  })
  
  # results for GSEApreranked
  output$GSEA_RESULTS_FGSEA <- renderUI({
    if (!has_run()) {
      p("Waiting for user to select parameters.")
    } else {
      tagList(
        tableOutput("gsea_table"),
        plotOutput("gsea_plot"),
        plotOutput("pathways_plot")
      )
    }
  })
  
  output$FGSEA_gsea_table <- renderTable({
    req(gsea_results())
    gsea_results() |> arrange(padj) |> head(20) |> dplyr::select(pathway, pval, padj, NES)
  })
  
  output$FGSEA_gsea_plot <- renderPlot({
    req(gsea_results())
    top_pathway <- gsea_results() |> arrange(padj) |> pull(pathway) |> head(10)
    plots <- lapply(top_pathway, function(pw) plotEnrichment(examplePathways[[pw]], ranks()))
    req(length(plots) > 0)
    plots[[1]]
  })
  
  output$FGSEA_pathways_plot <- renderPlot({
    req(gsea_results())
    res <- gsea_results()
    topPathwaysUp   <- res |> filter(ES > 0) |> arrange(pval) |> head(10) |> pull(pathway)
    topPathwaysDown <- res |> filter(ES < 0) |> arrange(pval) |> head(10) |> pull(pathway)
    topPathways <- c(topPathwaysUp, rev(topPathwaysDown))
    plotGseaTable(examplePathways[topPathways], ranks(), res, gseaParam = 0.5)
  })
  
  
  # ===== run GSEA (with the sample data) =============================
  
  
}  #closes server
shinyApp(ui = ui, server = server)


