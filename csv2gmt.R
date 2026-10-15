#csv -> gmt converter (10-15-2026)
#https://support.bioconductor.org/p/74357/
#https://support.bioconductor.org/p/9163834/ RIP biomart

library("org.Mm.eg.db")

args <- commandArgs(trailingOnly=TRUE)
csv_file <- args[1]

mouse_mart <- useMart(
  biomart = "ENSEMBL_MART_ENSEMBL",
  dataset = "mmusculus_gene_ensembl",
  host    = "https://e115.ensembl.org"
)
csv_genes <- read.csv(csv_file)

mouse_mapping <- getBM(
  attributes = c("ensembl_gene_id", "mgi_symbol", "mgi_id", "description"),
  filters    = "ensembl_gene_id",
  values     = csv_genes,
  mart       = mouse_mart
)

print(mouse_mapping)

write.csv(mouse_mapping, sub(csv_file, ".csv", ".gmt"))
