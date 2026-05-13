# 4/11/26 script for checking user gene sets in the GSEA tool, currently placeholder

user_set_check <- function(infile) {
  #TO DO: check file type, and make sure its a valid 
  df <- read_csv(infile)
  
  if (ncol(df) != 2) { 
    return(FALSE)
  }
  
  #check for tab-delim
  
  return(TRUE)
  
}

user_CHIP_check <- function(infile) {
  #TODO: double check CHIP formatting
  return (TRUE)
}

drop_header <- function(infile) {
  #drop header of input files for RNK or CHIP
  
}
