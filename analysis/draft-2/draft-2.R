rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten

# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R")
source("./scripts/operational-functions.R")
source("./analysis/draft-1/graphing-functions.R")

# ---- load-packages -----------------------------------------------------------
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(explore)   # describe_all
library(labelled)  # formats and look_for
library(tsibble)   # time

# ---- declare-globals ---------------------------------------------------------

# ---- declare-functions -------------------------------------------------------
# custom function for HTML tables
prints_folder <- paste0("./analysis/draft-2/prints/")
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}

# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_rds("./data-public/raw/example-prosthetic-1.rds")

# ---- inspect-data ------------------------------------------------------------
ds0 |> glimpse()
ds0 |> describe_all()
ds0 |> look_for()

# ---- tweak-data --------------------------------------------------------------
ds1 <- 
  ds0 |> 
  mutate(
    year         = lubridate::year(date)
    # ,yearmon     = tsibble::yearmonth(date) # not supported by look_for()
    ,year_fiscal = compute_fiscal_year(date)
    ,quarter = lubridate::quarter(date)
    ,quarter_fiscal = quarter - 1
    ,quarter_fiscal = ifelse(quarter_fiscal==0,4,quarter_fiscal)
    ,year_date = as.Date(paste0(year,"-01-01"))
    ,year_fiscal_date = as.Date(paste0(year_fiscal,"-04-01"))
    ,quarter_date = paste(year,(quarter*3-1),"15", sep="-") %>% as.Date()
    ,quarter_fiscal_date = quarter_date
  )
ds1 |> look_for()
ds1 %>% select(starts_with(c("quarter","year"))) 

# ------ functions-separate ---------------------------------------------------------------
# application of individual functions
l <- 
  ds1 %>% 
  prep_data_trajectory(
    outcome_var    = "employed" # outcome of interest (binary or continuous)
    ,time_var      = "year"     # quarter, year, quarter_fiscal, year_fiscal
    ,count_var     = "id"
    ,color_var     = "gender"
    # ,vfacet_var    = "race"
    # ,hfacet_var    = "age"
    # ,percent_var   = "year"  # TODO: must throw error if illegal
    # ,total_var     = "gender"
  )
l$data 
l$meta$percent_var
# graph production from dto (list) 
g <- 
  l %>% 
  plot_trajectory(
    y_var       = "cell_count" # cell_prop, cell_count
    ,facet      = "grid"      # wrap, grid
    ,scale_mode = "free"      # free, fixed, free_x, free_y
  )
g


# ------ functions-combined ---------------------------------------------------------------
# application as the combined function
g <- 
  ds1 %>% 
  prep_plot_trajectory(
    outcome_var    = "employed"   # outcome of interest (binary or continuous)
    ,y_var         = "cell_prop" # cell_count, cell_prop
    ,time_var      = "year"       # quarter, year, quarter_fiscal, year_fiscal
    ,count_var     = "id"
    # ,color_var   = "gender"
    # ,vfacet_var  = "race"
    # ,hfacet_var  = "gender"
    # ,percent_var = "gender" # gender, race, age
    # ,total_var   = "race" # gender, race, age
    # ,facet       = "grid"
    # ,scale_mode  = "free"
  )
g

# ------ study-dates ---------------------------------------------------------------
# study in using dates on X-asis and applying axis labels

g <- 
  ds1 %>% 
  prep_plot_trajectory(
    outcome_var    = "employed"   # outcome of interest (binary or continuous)
    ,y_var         = "cell_count" # cell_count, cell_prop
    ,time_var      = "quarter_date"       # quarter, year, quarter_fiscal, year_fiscal
    ,count_var     = "id"
    # ,color_var   = "gender"
    # ,vfacet_var  = "race"
    # ,hfacet_var  = "gender"
    # ,percent_var = "gender" # gender, race, age
    # ,total_var   = "race" # gender, race, age
    # ,facet       = "grid"
    # ,scale_mode  = "free"
  )
# g 
g +
  scale_x_date(date_labels = "%y", breaks = "3 months", minor_breaks = "3 months")+
  geom_text(aes(label = lubridate::quarter(quarter_date)),vjust=-1)


# ---- save-to-disk ------------------------------------------------------------
path <- "./analysis/.../report-isolated.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
