#permuting the gene set labels instead of the gene list 
#GSEA preranked: https://bioinformatics-core-shared-training.github.io/cruk-summer-school-2018/RNASeq2018/html/06_Gene_set_testing.nb.html
library(fgsea)
library(data.table)
library(ggplot2)

#runs the fgsea package with built in GMT and user pathways
runGSEApreranked <- function(pathways, ranks, nperm = 1000) {
  set.seed(42)
  fgsea(pathways = pathways,
        stats    = ranks,
        eps      = 0.0,
        minSize  = 15,
        maxSize  = 500,
        nPermSimple = nperm)
}

plotPathways <- function(pathway_names, pathways, ranks) {
  lapply(pathway_names, function(pw) {
    plotEnrichment(pathways[[pw]], ranks) + labs(title = pw)
  })
}
