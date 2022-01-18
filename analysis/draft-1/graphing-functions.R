# ----- colors ------------------------------------------------------
# High contrast colorblind safe palette for binary variable:
# https://colorbrewer2.org/#type=diverging&scheme=PuOr&n=3
binary_colors <- c(
  "TRUE"   = "#f1a340" # orange
  ,"FALSE" = "#998ec3" # purple
)

# standard palette to be used across IHACRU reports
# qualitative, 9 categories (max), printer friendly
# http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
# add colors in sequential order
abcol <- c(
  "grey"       = "#5f6a72" # stone   - grey
  ,"magenta"    = "#d40072" # dusk    - magenta
  ,"brown"      = "#ff7900" # sunset  - brown
  ,"yellow"     = "#edb700" # prairie - yellow
  ,"green"      = "#77b800" # pasture - green
  ,"blue"       = "#00aad2" # sky     - blue
)
# ----- functions ------------------------------------------------------

# ---- dev-test-values -----------------

# values to test at different locations
forecast_var_i          = "cell_count" # outcome, "cell_count","cell_prop","age_median"
color_var_i      = "gender"#"gender"
facet_row_var_i  = NULL#"age_group"
facet_col_var_i  = "service_category"
total_cat_var_i  = NULL # new group "Total" will be inserted into this variable, summing across others
percent_var_i    = "gender" # "percent from" will represent share from this category

#
# d0 <-
#     ds1
#   es2 %>%
#   filter(year_fiscal %in% 2015:2021) %>%
#   filter(gender %in% c("Male","Female")) %>%
#   filter(service_category_code !="DRES") %>%
#   # filter(service_id %in% (sample( unique(.$person_oid), 100000))) %>%
#   select(person_oid, service_id,service_category, start_date, year_fiscal, gender, age_at_prog_start, age_group, race, education )

# ---- prep-define ---------------
# d0
prep_data_trajectory <- function(
  d
  ,outcome_var  # = "employed" # variable used for binning into age groups (default = years of age)
  ,time_var         # = "year_fiscal"
  ,color_var     = NULL # = "gender"
  ,facet_row_var = NULL # = "race"
  ,facet_col_var = NULL # = "age"
  ,total_cat_var = NULL # = facet_row_var # new group "Total" will be inserted into this variable, summing across others
  ,percent_var   = NULL # = color_var # "percent from" will represent share from this category
  # prep level specific defaults
  ,count_var      # = "id"
  # ,forecast_var = "cell_count"
){
  # d <- ds1
  # time_var          = "year_fiscal"
  # color_var      = "gender"
  # facet_row_var  = "age"              # rows
  # facet_col_var  = "race" # columns
  # total_cat_var  =  NULL # group "Total" added, summing across others
  # percent_var    = "gender" # "percent from ____" will represent share from this category
  # # prep level specific defaults
  # count_var       = "id" # unique row ids used to compute `cell_count`
  # outcome_var     = "employed" # variable used for binning into age groups (default = years of age)
  #
  # browser()
  (row_name           <- c(color_var, facet_row_var, facet_col_var) %>% unique())
  (most_granular_vars <- c(time_var, row_name)                         %>% unique())
  (total_group_vars   <- setdiff(most_granular_vars, total_cat_var) %>% unique())
  (percent_from_vars  <- setdiff(most_granular_vars, percent_var)   %>% unique())
  
  get_custom_summary <- function(d){
    # in current form, functions supports BINARY and NUMERIC outcomes
    d %>%
      mutate(across(outcome_var %>% all_of(), as.numeric)) %>% 
      summarize(
        cell_count      = n_distinct(!!rlang::sym(count_var))
        ,outcome_median = median(!!rlang::sym(outcome_var),na.rm = T )
        ,outcome_mean   = mean(  !!rlang::sym(outcome_var),na.rm = T )
        ,.groups        = "drop"
      ) %>%
      ungroup() %>%
      mutate(across(where(is.factor), fct_drop)) # only non-empty levels
  }
  
  #### STEP 1 - row counts at most granular level
  d1 <-
    d %>%
    group_by_at(vars(all_of(  most_granular_vars  ))) %>%
    get_custom_summary()

  #### STEP 2  - if Total category is requested, it will be added as more rows
  if(!is.null(total_cat_var)){
    d2 <-
      d %>%
      group_by_at(vars(all_of(  total_group_vars  ))) %>%
      get_custom_summary()%>%
      mutate(
        !!rlang::sym(total_cat_var) := "Total"
      )
    d12 <- dplyr::bind_rows(d1, d2) # adding new rows
  } else {
    d12 <- d1 # if Total is not requested, we leave d1 as is
  }
  #### STEP 3 - Percent computed
  d3 <- 
    d12 %>%
    group_by_at(vars(all_of(  percent_from_vars  ))
    ) %>%
    mutate(
      cell_prop = cell_count/sum(cell_count)
      # ,cell_pct = cell_prop %>% scales::percent(accuracy = 1)
    ) %>%
    ungroup()
  #### STEP 4 - 
  if(!is.null(total_cat_var)){
    d4 <- 
      d3 %>%
      mutate(
        !!rlang::sym(total_cat_var) := as_factor(!!rlang::sym(total_cat_var)) %>%
          fct_relevel("Total", after = Inf)
      )
  }
  d_out <- d4
  # add forecast (optional) - to be developed
  #   d5 <-
  #     d4 %>%
  #     add_prediction_row(
  #       outcome     = forecast_var
  #       ,timevar    = time_var
  #       ,predictors = c("year_fiscal") # c("year_fiscal","year_fiscal_2")
  #       ,row_name   = row_name
  #       ,latest_year_K = 2020
  #     )
  # d5
  # d_out <- d5
  
  ls_out <- list(
    # "data" = d4
    "data" = d_out
    ,"meta" = c(
      "time_var"           = list(time_var)
      # ,"forecast_var"   = list(forecast_var)
      ,"color_var"      = list(color_var)
      ,"facet_row_var"  = list(facet_row_var)
      ,"facet_col_var"  = list(facet_col_var)
      ,"total_cat_var"  = list(total_cat_var) # new group "Total" will be inserted into this variable, summing across others
      ,"percent_var"    = list(percent_var)   # listed as "percent from _____" , will represent share from this category
      ,"count_var"      = list(count_var)     # row ids tallied with group_by
      ,"outcome_var"    = list(outcome_var)   # the outcome of interest
      ,"row_name"       = list(row_name)      #  variables used to add an extrapolation row (forecast)
    )
  )
  return(ls_out)
}

# ---- prep-test -----------------------------
# ldp <-
#   d0 %>%
#   # dsmall %>%
#   prep_data_trajectory(
#      time_var          = "year_fiscal"
#      ,forecast_var         = forecast_var_i # create forecast for this measure
#     ,color_var      = color_var_i
#     ,facet_row_var  = facet_row_var_i
#     ,facet_col_var  = facet_col_var_i
#     ,total_cat_var  = total_cat_var_i # adds "Total" category to this var
#     ,percent_var    = percent_var_i # percent from this
#     # prep level specific defaults
#     ,count_var       = "service_id"
#     ,outcome_var         = "age_at_prog_start" # variable used for binning into age groups (default = years of age)
#   )
# ldp$meta
# ldp$data %>% arrange(gender, service_category, year_fiscal) %>% View()
#
# d <- ldp$data %>%
#   filter(year_fiscal==2020) %>%
#   # filter(gender == "Male") %>%
#   # filter(age_group == "<25") %>%
#   filter(service_category == "Workshop")
# d
# d %>%
#   summarize(should_be_one = sum(cell_prop))



