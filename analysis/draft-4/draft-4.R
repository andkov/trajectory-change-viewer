rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten
# This script started as a copy of `draft-2`
# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R")
source("./scripts/operational-functions.R")
source("./analysis/draft-4/graphing-functions.R")

# ---- load-packages -----------------------------------------------------------
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
# library(stringi)   # strings
library(lubridate) # dates
library(explore)   # describe_all
library(labelled)  # formats and look_for
library(tsibble)   # time
requireNamespace("fs") # https://fs.r-lib.org/articles/function-comparisons.html

# ---- declare-globals ---------------------------------------------------------

# ---- declare-functions -------------------------------------------------------
# custom function for HTML tables
prints_folder <- paste0("./analysis/draft-4/prints/")
if (!fs::dir_exists(prints_folder)) {
  fs::dir_create(prints_folder)
}

# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_rds("./data-public/raw/example-prosthetic-1.rds")

# ---- inspect-data ------------------------------------------------------------
ds0 |> pillar::glimpse()
ds0 |> explore::describe_all()
ds0 |> labelled::look_for()

# Goal: show variation of binary response across a combo of categorical confounders

# ---- tweak-data --------------------------------------------------------------
ds1 <- 
  ds0 |>
  # select(date) %>% # turn ON for inspection, OFF for use
  mutate(
    year                = lubridate::year(date) %>% as.integer(),
    # yearmon             = tsibble::yearmonth, # not supported by look_for()
    year_fiscal         = compute_fiscal_year(date),
    quarter             = lubridate::quarter(date),
    quarter_fiscal      = (quarter - 1),
    quarter_fiscal      = ifelse(quarter_fiscal==0,4,quarter_fiscal)%>% as.integer(),
    year_date           = as.Date(paste0(year,"-01-01")),
    year_fiscal_date    = as.Date(paste0(year_fiscal,"-04-01")),
    quarter_date        = paste(year,(quarter*3-1),"15", sep="-") %>% as.Date(),
    quarter_fiscal_date = quarter_date,
  )
ds1 |> labelled::look_for()
ds1 |> 
  dplyr::distinct() |> 
  dplyr::arrange(date) # inspection of date variables

# ------ functions-separate ---------------------------------------------------------------
# application of individual functions

# ARGUMENT LEGEND
# ### mandatory arguments
# outcome_var          # outcome of interest (binary or continuous)
# time_var             # temporal resolution of x-axis, one dot per quarter, year, quarter_fiscal, year_fiscal
# count_var            # unique row ids used to compute `cell_count` (count = number of uniques)
# ### optional arguments
# color_var   = NULL   # creates multiple lines,     color = levels of this variable
# vfacet_var  = NULL   # creates rows of cells,        row = levels of this variable
# hfacet_var  = NULL   # creates columns of cells,  column = levels of this variable
# total_var   = NULL   # adds "Total" as new level of this v., must be one of used dimensions (color, row, column)
# percent_var = NULL   # "percent from ____" will represent share from this variable,  must be one of used dimensions (color, row, column) or TIMEVAR


l <- 
  ds1 %>% 
  prep_data_trajectory(
    outcome_var    = "employed"  # outcome of interest (binary or continuous)
    ,time_var      = "year"      # quarter, year, quarter_fiscal, year_fiscal
    ,count_var     = "id"        # unique row ids used to compute `cell_count`
    ,color_var     = "gender"    # gender, age, race (categorical variable)
    ,vfacet_var    = "age"       # gender, age, race (categorical variable)
    ,hfacet_var    = "race"      # gender, age, race (categorical variable)
    # ,percent_var   = "race"      # must be one of the used dimensions (color, row, column)
    # ,total_var     = "gender"    # must be one of the used dimensions (color, row, column)
  )
# `prep_data_trajectory()` creates list object `l` passed to `plot_trajectory`
l$data # micro data used for plotting
l$meta # inherited options and arguments stored as vectors
l <- 
  l %>% # created by `prep_data_trajectory()` 
  plot_trajectory(
    y_var       = "cell_prop" # what is put on Y-axis (e.g. cell_prop, cell_count)
    ,facet      = "grid"       # wrap, grid
    ,scale_mode = "free"       # free, fixed, free_x, free_y
  )
l$graph
# g + labs(title = "New Title")


# ------ functions-combined ---------------------------------------------------------------
# application as the combined function
l <- 
  ds1 %>% 
  prep_plot_trajectory(
    outcome_var    = "employed" # outcome of interest (binary or continuous)
    ,y_var         = "cell_prop"# cell_count, cell_prop
    ,time_var      = "quarter_date" # quarter, year, quarter_fiscal, year_fiscal
    ,count_var     = "id" # unique row ids used to compute `cell_count`
    ,color_var     = "gender" # gender, race, age
    ,vfacet_var    = "age" # gender, race, age
    ,hfacet_var    = "race" # gender, race, age
    ,percent_var   = "race" # gender, race, age
    ,total_var     = "gender" # gender, race, age
    ,facet         = "grid" # grid, wrap
    ,scale_mode    = "free" # free, free_y, free_x, fixed
  )
l$graph$graph


# ------ study-dates ---------------------------------------------------------------
# study in using dates on X-asis and applying axis labels
axis_date_format <-  strftime(
  seq.Date(from     = as.Date("2015-01-01")
           , to     = as.Date("2020-12-31")
           , by     = "month")
           , format = "%b\n%Y"
 )
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
g$graph$graph +
  scale_x_date(date_labels = "%b\n%y", breaks = "6 months", minor_breaks = "3 months")+
  # scale_x_date(date_labels = axis_date_format)+
  geom_text(aes(label = lubridate::quarter(quarter_date)),vjust=-1)

# TODO: considering clumping date: https://ouhscbbmc.github.io/OuhscMunge/reference/clump_date.html & https://github.com/OuhscBbmc/OuhscMunge/blob/main/R/dates.R
# strftime(seq.Date(from = as.Date("2015-01-01"), to = as.Date("2020-12-31"), by = "month"), "%b\n%Y")
# > strptime(x="28-2020-04", format="%d-%Y-%m")
# [1] "2020-04-28 CDT"
# > strptime(x="28-2020-Nov", format="%d-%Y-%b")
# [1] "2020-11-28 CST"

# ---- save-to-disk ------------------------------------------------------------
# path <- "./analysis/.../report-isolated.Rmd"
# rmarkdown::render(
#   input = path ,
#   output_format=c(
#     "html_document"
#     # "word_document"
#     # "pdf_document"
#   ),
#   clean=TRUE
# )
