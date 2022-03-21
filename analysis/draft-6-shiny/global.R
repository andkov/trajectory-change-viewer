# rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 

# ---- load-sources  -----------------------------------
library(magrittr)
# ---- load-packages  -----------------------------------
requireNamespace("readr")


# ---- declare-globals  -----------------------------------

# determine_directory <- function( ) {
#   directoryServerOutside <- "//bbmc-shiny-public/Anonymous/TfcbtPublic"
#   directoryServerInside <- "/var/shinydata/TfcbtPublic"
#   directoryRepo <- "./DataPhiFree"
#   
#   if( file.exists(directoryServerOutside) ) {
#     directoryData <- directoryServerOutside  
#   } else if( file.exists(directoryServerInside) ) {
#     directoryData <- directoryServerInside  
#   } else {
#     directoryData <- directoryRepo
#   }
#   return( directoryData )
# }


path_survey <- function( ) {
  # "./data-public/raw/example-prosthetic-1.rds"
  "../../data-public/raw/example-prosthetic-1.rds"
}
load_survey <- function( ) {
  
  if (!fs::file_exists(path_survey()))
    stop(
      "The the prosthetic-1 dataset cannot be found at the location:\n`",
      path_survey(),
      "`.\nPlease examing the relative directory.  Good Luck!"
    )
  readr::read_rds(path_survey())
}

# browser()

# load_data   -----------------------------------
system.time({
  ds_survey_all <- load_survey()
})
