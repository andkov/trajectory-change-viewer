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

prints_folder <- paste0("./analysis/draft-1/prints/")
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
  )
ds1 |> look_for()
# ---- table-1 -----------------------------------------------------------------
dt1 <- 
  ds1 |> 
  group_by(
    year, gender, age, race
  ) |> 
  summarize(
    id_count = n()
  )
dt1
# ---- graph-1 -----------------------------------------------------------------
g1 <- 
  dt1 |> 
  ggplot(aes(
    x = year
    ,y = id_count
    ,color = gender
  ))+
  geom_line()+
  geom_point()+
  facet_grid(age ~ race)+
  # facet_wrap(facets = c("age","race"))+
  labs()
g1
# ---- graph-2 -----------------------------------------------------------------
# add totals to each graph
draft_2 <- function(
  d 
  ,time_var
  ,color_var
  ,vfacet_var
  ,hfacet_var 
){
  d <- ds1
  time_var = "year"
  color_var = "gender"
  # color_var = character()
  vfacet_var = "age"
  # vfacet_var = character()
  hfacet_var = "race"
  # hfacet_var = character()
  # hfacet_var = character()
  pct_from = "row" # row, column, total
  
  checkmate::assert_character(color_var , min.len=0, max.len=1, unique=T)
  checkmate::assert_character(hfacet_var, min.len=0, max.len=2, unique=T)
  
  color_symbol <- 
    if (length(color_var) == 0L) {
      NULL
    } else {
      rlang::sym(color_var)
    }
      
 grouping_vars <- c(time_var, color_var, vfacet_var, hfacet_var)
 facet_vars <- c(vfacet_var, hfacet_var)
 # if(pct_from=="row"){
 #   pct_vars <- c()
 # }    
 d1 <- 
    d |> 
    group_by( !!!rlang::syms(grouping_vars)) |> 
    summarize(
      cell_count = n()
    ) |> 
    # group_by(!!!rlang::syms(pct_vars))
   dplyr::ungroup()
d1

  g1 <- 
    d1 |> 
    ggplot(aes(
      x      = !!rlang::sym(time_var)
      ,y     = cell_count
      ,color = !!color_symbol
    ))+
    geom_line() +
    geom_point() +
    labs()

  if( length(facet_vars) == 0L ) {
    # Don't do anything if not faceting variables are specified.
  } else if (length(facet_vars) == 1L) {
    g1 <- g1 + facet_wrap(facet_vars) #,  scales="free_y")
  } else if (length(facet_vars) == 2L) {
    facet_grid_string <- sprintf("%s ~ %s", facet_vars[1], facet_vars[2])
    g1 <- g1 + facet_grid(facet_grid_string)
  } else {
    stop("This graphing function supports only 0, 1, or 2 faceting variables.")
  }

  g1
}


d2 <-
  ds1 |> 
  group_by(
    year, gender, age, race
  ) |> 
  summarize(
    id_count = n()
  )


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
