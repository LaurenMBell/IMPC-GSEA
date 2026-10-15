suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(fgsea)
  library(DBI)
  library(RSQLite)
}))

args <- commandArgs(trailingOnly=TRUE)
mp_term <- args[1]
#print(mp_term)

# connect to the table
impc_con <- dbConnect(SQLite(), "data/statistical-result.db")

mp_terms_data <- dbGetQuery(impc_con, "
                  SELECT marker_symbol, zygosity, sex, p_value, effect_size,
                         parameter_stable_id, mp_term_name
                  FROM impc_stats
                  WHERE parameter_stable_id IN (
                    SELECT DISTINCT parameter_stable_id FROM impc_stats WHERE mp_term_name = ?
                  )
                ", params = list(mp_term))

dbDisconnect(impc_con)
show(mp_terms_data)


#=======================================
#https://cran.r-project.org/web/packages/RSQLite/RSQLite.pdf
#https://posit.co/blog/shiny-with-databases
#https://dbi.r-dbi.org
#https://stackoverflow.com/questions/2151212/how-can-i-read-command-line-parameters-from-an-r-script