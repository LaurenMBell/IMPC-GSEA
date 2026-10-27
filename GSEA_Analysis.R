suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(fgsea)
  library(DBI)
  library(RSQLite)
}))

args <- commandArgs(trailingOnly=TRUE)
gmt_file <- args[1]
level <- args [2] #1 for mp_term_name, 2 for top_level_mp_term_name
print(gmt_file)
sets <- gmtPathways(gmt_file) 
print("sets loaded")

# connect to the db table
impc_con <- dbConnect(SQLite(), "data/statistical-result.db")

if(level == 1){
  mp_term2parameter <- dbGetQuery(impc_con, "
  SELECT DISTINCT mp_term_name, mp_term_id, parameter_stable_id, parameter_name
    FROM impc_stats
    WHERE mp_term_name IS NOT NULL
    AND TRIM(mp_term_name) != ''
    AND mp_term_id LIKE 'MP:%'")
} else if (level == 2){
  mp_term2parameter <- dbGetQuery(impc_con, "
  SELECT DISTINCT top_level_mp_term_name AS mp_term_name, top_level_mp_term_id AS mp_term_id, parameter_stable_id, parameter_name
    FROM impc_stats
    WHERE top_level_mp_term_name IS NOT NULL
    AND TRIM(top_level_mp_term_name) != ''
    AND top_level_mp_term_id LIKE 'MP:%'")
} else {
  print("Didn't select 1 or 2!!")
}

# all rows with an effect size
data <- dbGetQuery(impc_con, "
  SELECT parameter_stable_id, marker_accession_id, marker_symbol, effect_size, p_value
  FROM impc_stats
  WHERE effect_size IS NOT NULL")

dbDisconnect(impc_con)

# lowest p-value and largest effect size represents all gene-parameter pairs 
#NEED TO CHANGE TO LOWEST P-VALUE + MAX(EFFECT SIZE) FOR EACH ZYGOSITY/GENE/PARAM!!
data_grouped <- data |> group_by(parameter_stable_id, marker_accession_id, marker_symbol) |> 
  summarise(es = max(effect_size), #flatten each row into max effect size and smallest p
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
  
  # best effect size/p-value per gene across all parameters associated with the mp_term
  gene_summary <- cdata |>
    group_by(marker_accession_id) |>
    summarise(es = max(as.numeric(es), na.rm = TRUE), 
              p = min(as.numeric(p), na.rm = TRUE), 
              .groups = "drop"
    ) |> filter(!is.na(marker_accession_id) & marker_accession_id != "")
  
  # RANK TERMS
  gene_ranked <- gene_summary |> mutate(p = pmax(p, 1e-300, na.rm = TRUE),
                                        rank_metric = -log10(p) * sign(es))
  
  # SIGNED P-VAL AS RANKING METRIC: 
  #https://bioinformatics.ccr.cancer.gov/docs/btep-coding-club/CC2025/GSEA/GSEA/
  
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
            minSize  = 3, #what do we want the minimum number of gene overlap to be?
            maxSize  = 500,
            nPermSimple = 1000)
    }, error = function(e) {
      message("Error: term: ", cterm, ": ", e$message)
      NULL
    })
    
    results_list[[i]] <- fgseaRes
  }
}

length(results_list); length(mp_terms)   # equal if no bug
sum(names(results_list) == "" | is.na(names(results_list)))
head(names(results_list))


GSEA_RESULTS <- bind_rows(results_list, .id = "mp_term_query")

#fgsea returns the leadingEdge column as a list (?) -> string for file
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
