suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(fgsea)
  library(DBI)
  library(RSQLite)
}))

args <- commandArgs(trailingOnly=TRUE)
gmt_file <- args[1]
print(gmt_file)
sets <- gmtPathways(gmt_file) 
print("sets loaded")

# connect to the db table
impc_con <- dbConnect(SQLite(), "data/statistical-result.db")

# all rows with an effect size
data <- dbGetQuery(impc_con, "
  SELECT parameter_stable_id, marker_accession_id, marker_symbol, effect_size, p_value
  FROM impc_stats
  WHERE effect_size IS NOT NULL")

# param <-> mp term 
#TODO: how to get distinct id pairings but not care about names? how to do that in SQL
mp_term2parameter <- dbGetQuery(impc_con, "
  SELECT DISTINCT mp_term_name, mp_term_id, parameter_stable_id, parameter_name
  FROM impc_stats
  WHERE mp_term_name IS NOT NULL")
print(nrow(mp_term2parameter))

dbDisconnect(impc_con)


# lowest p-value and largest effect size represents all gene-parameter pairs 
#NEED TO CHANGE TO LOWEST P-VALUE + MAX(EFFECT SIZE) !!
data_grouped <- data |> group_by(parameter_stable_id, marker_accession_id, marker_symbol) |> 
  summarise(score = max(effect_size), #flatten each row into max effect size and smallest p
            p = min(p_value, na.rm = TRUE), 
            .groups = "drop")

# stats + mp_term mapping to parameter
data_joined <- inner_join(data_grouped, mp_term2parameter, by = "parameter_stable_id", relationship = "many-to-many")

mp_terms   <- unique(mp_term2parameter$mp_term_name) #all mp_terms to iterate over
results_list <- vector("list", length(mp_terms))
names(results_list) <- mp_terms

for (i in seq_along(mp_terms)) {
  #iterate over each of the mp_terms 
  cterm <- mp_terms[i]
  
  # current mp_term rows 
  cdata <- data_joined |> filter(mp_term_name == cterm)
  
  if (nrow(cdata) == 0) next
  
  # best score/p-value per gene across all parameters associated with the mp_term
  gene_summary <- cdata |>
    group_by(marker_accession_id) |>
    summarise(score = max(as.numeric(score), na.rm = TRUE), 
              p = min(as.numeric(p), na.rm = TRUE), 
              .groups = "drop"
     ) |> filter(!is.na(marker_accession_id) & marker_accession_id != "")
  
  # RANK TERMS
  gene_ranked <- gene_summary |> mutate(p = pmax(p, 1e-300, na.rm = TRUE),
                                        rank_metric = -log10(p) * sign(score))
  
  ranks <- setNames(gene_ranked$rank_metric, gene_ranked$marker_accession_id)
  
  #Error: term: decreased heart rate variability: Not all stats values are finite numbers
  ranks <- ranks[!is.na(names(ranks)) & names(ranks) != ""]
  ranks <- ranks[!duplicated(names(ranks))]
  ranks <- ranks[is.finite(ranks)]
  ranks <- sort(ranks, decreasing = TRUE)
  
  # 10 term baseline for running fgsea with gmt sets
  #https://biostatsquid.com/fgsea-tutorial-gsea/
  if (length(ranks) > 10) {
    fgseaRes <- tryCatch({
      fgsea(pathways = sets,
            stats    = ranks,
            # "For some pathways, in reality P-values are less than 1e-10. 
            #You can set the `eps` argument to zero for better estimation."
            eps      = 0.0, 
            minSize  = 3,
            maxSize  = 500,
            nPermSimple = 1000)
      #what do we want the minimum number of gene overlap to be?
    }, error = function(e) {
      message("Error: term: ", cterm, ": ", e$message)
      NULL
    })
    
    results_list[[i]] <- fgseaRes
  }
}


GSEA_RESULTS <- bind_rows(results_list, .id = "mp_term_query")
GSEA_RESULTS$leadingEdge <- sapply(GSEA_RESULTS$leadingEdge, paste, collapse = ",")

head(GSEA_RESULTS)

write.csv(GSEA_RESULTS, file = "fgsea_results.csv")
#=======================================
#https://cran.r-project.org/web/packages/RSQLite/RSQLite.pdf
#https://posit.co/blog/shiny-with-databases
#https://dbi.r-dbi.org
#https://stackoverflow.com/questions/2151212/how-can-i-read-command-line-parameters-from-an-r-script
#https://support.bioconductor.org/p/74357/
#https://dplyr.tidyverse.org/reference/summarise.html
#https://r4ds.had.co.nz/iteration.html
#https://cnuge.github.io/post/trycatch/
#https://bioinformatics-core-shared-training.github.io/cruk-summer-school-2018/RNASeq2018/html/06_Gene_set_testing.nb.html
#https://stackoverflow.com/questions/78422148/understanding-the-purpose-of-groups-drop-in-dplyrs-summarise-function

