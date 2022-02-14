# ----- colors -------------------------------------------------------
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

# ---- prep-data-trajectory ---------------
# d0
prep_data_trajectory <- function(
  d
  # mandatory arguments
  ,outcome_var          # outcome of interest (binary or continuous)
  ,time_var             # quarter, year, quarter_fiscal, year_fiscal
  ,count_var            # unique row ids used to compute `cell_count`
  # optional arguments
  ,color_var   = NULL # creates multiple lines,     color = levels of this variable
  ,vfacet_var  = NULL # creates rows of cells,        row = levels of this variable
  ,hfacet_var  = NULL # creates columns of cells,  column = levels of this variable
  ,total_var   = NULL # adds "Total" as new level of this v., must be one of used dimensions (color, row, column)
  ,percent_var = NULL # "percent from ____" will represent share from this variable,  must be one of used dimensions (color, row, column) or TIMEVAR
  # prep level specific defaults
  # ,forecast_var = "cell_count"
){
  # categorical variable: gender, age, race. Replace with NULL to mask out
  # browser()
  # d <- ds1
  # ## mandatory arguments
  # outcome_var = "employed"   # outcome of interest (binary or continuous)
  # time_var    = "year_fiscal"# quarter, year, quarter_fiscal, year_fiscal
  # count_var   = "id"         # unique row ids used to compute `cell_count`
  # ## optional arguments
  # color_var   = NULL         # default = NULL; e.g. gender, age, race 
  # vfacet_var  = NULL         # default = NULL; e.g. gender, age, race 
  # hfacet_var  = NULL         # default = NULL; e.g. gender, age, race 
  # total_var   = NULL         # default = NULL; e.g. gender, age, race 
  # percent_var = NULL         # default = NULL; e.g. color, row, column or TIMEVAR


  (row_name           <- c(color_var, vfacet_var, hfacet_var) %>% unique())
  (most_granular_vars <- c(time_var, row_name)                         %>% unique())
  (total_group_vars   <- setdiff(most_granular_vars, total_var) %>% unique())
  # if no `percent_var` is given, defaults to first non-missing dimension
  if(is.null(percent_var)){percent_var <- (c(row_name,time_var))[1]}
  
  (percent_from_vars  <- setdiff(most_granular_vars, percent_var)   %>% unique())
  
  get_custom_summary <- function(xd){
    # in current form, functions supports BINARY and NUMERIC outcomes
    xd %>%
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
  # browser()
  
  #### STEP 1 - row counts at most granular level
  d1 <-
    d %>%
    group_by_at(vars(all_of(  most_granular_vars  ))) %>%
    get_custom_summary()

  #### STEP 12a  - if Total category is requested, it will be added as more rows
  if(!is.null(total_var)){
    d2 <-
      d %>%
      group_by_at(vars(all_of(  total_group_vars  ))) %>%
      get_custom_summary()%>%
      mutate(
        !!rlang::sym(total_var) := "Total"
      )
    d12 <- dplyr::bind_rows(d1, d2) # adding new rows
  }else{
    d12 <- d1 # if Total is not requested, we leave d1 as is
  }
  #### STEP 3 - Compute percent
  d3 <- 
    d12 %>%
    group_by_at(vars(all_of(  percent_from_vars  ))
    ) %>%
    mutate(
      cell_prop = cell_count/sum(cell_count)
      # ,cell_pct = cell_prop %>% scales::percent(accuracy = 1)
    ) %>%
    ungroup()
  #### STEP 12b - Tweak factors if adding Totals messed it up
  if(!is.null(total_var)){
    d3 <-  # must overwrite because Total is optional
      d3 %>%
      mutate(
        !!rlang::sym(total_var) := as_factor(!!rlang::sym(total_var)) %>%
          fct_relevel("Total", after = Inf)
      )
  }
  d_out <- d3
  # add forecast (optional) - to be developed
  #   d4 <-
  #     d3 %>%
  #     add_prediction_row(
  #       outcome     = forecast_var
  #       ,timevar    = time_var
  #       ,predictors = c("year_fiscal") # c("year_fiscal","year_fiscal_2")
  #       ,row_name   = row_name
  #       ,latest_year_K = 2020
  #     )
  # d4
  # d_out <- d4
  
  ls_out <- list(
    # "data" = d4
    "data" = d_out
    ,"meta" = c(
      "time_var"           = list(time_var)
      # ,"forecast_var"   = list(forecast_var)
      ,"color_var"      = list(color_var)
      ,"vfacet_var"  = list(vfacet_var)
      ,"hfacet_var"  = list(hfacet_var)
      ,"total_var"  = list(total_var) # new group "Total" will be inserted into this variable, summing across others
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
#     ,vfacet_var  = facet_row_var_i
#     ,hfacet_var  = facet_col_var_i
#     ,total_var  = total_cat_var_i # adds "Total" category to this var
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




# ---- plot-trajectory -----------------------
# works with the product of `prep_data_trajectory()`
plot_trajectory <- function(
  l
  ,y_var       = c("cell_prop", "cell_count")
  ,facet       = "grid"
  ,scale_mode  = "free_y"
  ,lab_title   = "Caseload breakdown"
  ,lab_y       = NA_character_
){
  # TODO: check all input values
  checkmate::assert_subset(   y_var, choices = c("cell_prop", "cell_count"), empty.ok = FALSE)
  checkmate::assert_character(y_var, any.missing = FALSE, len = 1, pattern = "^cell_prop|cell_count$", null.ok = FALSE)
  
  # browser()
  # `prep_data_trajectory()` creates list object `l` passed to `plot_trajectory`
  # l$data # micro data used for plotting
  # l$meta # inherited options and arguments stored as vectors
  
  #### VARIABLES AND SYMBOLS ####
  # time_sym      <- rlang::sym( l[["meta"]][["time_var"]]      )
  # y_sym         <- rlang::sym( y_var                          )
  # color_sym     <- rlang::sym( l[["meta"]][["color_var"]]     ) 
  percent_var   <- l[["meta"]][["percent_var"]]
  vfacet_var    <- l[["meta"]][["vfacet_var"]]
  hfacet_var    <- l[["meta"]][["hfacet_var"]]
  
  quo_to_sym <- function(x){
    if (is.null(x)){
      NULL
    } else {
      rlang::sym(x)
    }
  }
  
  y_sym      <- y_var                      %>% quo_to_sym()
  time_sym   <- l[["meta"]][["time_var"]]  %>% quo_to_sym()
  color_sym  <- l[["meta"]][["color_var"]] %>% quo_to_sym()
  vfacet_sym <- vfacet_var                 %>% quo_to_sym()
  hfacet_sym <- hfacet_var                 %>% quo_to_sym()
  
  #### SCAFFOLDING ####
  g <-
    l$data %>%
    {
      ggplot(., aes(
        x      = !!time_sym
        ,y     = !!y_sym
        ,color = !!color_sym
        ,group = !!color_sym
      )) +
        geom_line() +
        geom_point()
    }
  #### GRID or  WRAP ####
  # browser
  if(facet == "grid"){
    g <- 
      g +
      facet_grid(
        rows    = vars( !!vfacet_sym )
        ,cols   = vars( !!hfacet_sym )
        ,scales = scale_mode
      )
  }
  if(facet == "wrap"){
    g <- 
      g +
      facet_wrap(
        facets  = c(vfacet_var, hfacet_var)
        ,ncol   = ifelse(
          is.null(hfacet_var)
          ,1
          ,l$data %>% pull( !!hfacet_sym ) %>% unique() %>% length()
        )
        ,scales = scale_mode
      )
  }
  
  #### ANNOTATIONS ####

  if (y_var == "cell_prop"){
    g <- 
      g + 
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 
    
    lab_y        <- dplyr::coalesce(lab_y, "Percent")
    lab_caption  <- paste0("Sum all levels of (", toupper(percent_var),") to get 100%")
      
  } else if(y_var == "cell_count"){
    g <- 
      g + 
      scale_y_continuous(labels = scales::comma_format())
    
    lab_y        <- dplyr::coalesce(lab_y, "Count")
    lab_caption  <- "Values represent caseload count"
  }
  
  g <- 
    g +
    labs(
      title   = lab_title,
      y       = lab_y,
      caption = lab_caption,
    )
  # browser()
  l[["graph"]] <- g
  return(l)
}

#
# ldp %>%
#   plot_trajectory(
#      y_var       = "cell_prop"
#     ,facet       = "grid" # grid, wrap
#     ,scale_mode = "free_y" # free , free_x , free_y, fixed
#    )

# ----- prep-plot-trajectory --------------------------------------------------


prep_plot_trajectory <- function(
  d
  ,outcome_var    # outcome of interest (binary or continuous)
  ,y_var         # cell_count, cell_prop
  ,time_var      # quarter, year, quarter_fiscal, year_fiscal
  ,count_var     # unique row identifier
  # three optional dimensions 
  ,color_var   = NULL    # color 
  ,vfacet_var  = NULL   # facet rows on this variable
  ,hfacet_var  = NULL # facet columns on this variable
  ,percent_var = NULL    # selected from optional dimensions (not NULL)
  ,total_var   = NULL
  ,facet       = "grid"   # grid, wrap
  ,scale_mode  = "free"      # free, fixed, fixed_y, fixed_x
) {
  # browser()
  # logical tests
  # total must be one of three dimensions
  # if(
  #   (!is.null(total_var))
  #   &
  #   (!any(c(color_var, vfacet_var, hfacet_var))==total_var)
  #   ){
  #   print("`total_var` must be one of the additional dimensions")
  # }
  # `total_var` must be one of the provided dimensions
  optional_dimensions <- c(color_var, vfacet_var, hfacet_var)
  if (!is.null(total_var)){
    checkmate::assert_subset(
      x         = total_var, 
      choices   = optional_dimensions,
      empty.ok  = FALSE
    )
  }

  
  l <-
    d %>% 
    prep_data_trajectory(
      outcome_var    = outcome_var# outcome of interest (binary or continuous)
      ,time_var      = time_var     # quarter, year, quarter_fiscal, year_fiscal
      ,count_var     = count_var
      ,color_var     = color_var
      ,vfacet_var    = vfacet_var
      ,hfacet_var    = hfacet_var
      ,percent_var   = percent_var
      ,total_var     = total_var
    )
  # l$data 
  # l$meta$percent_var
  # graph production from dto (list) 
  # `percent_var` must be one of additional dimensions or time
  row_name <- c(optional_dimensions, time_var)
  checkmate::assert_subset(
    x         = l[["meta"]][["percent_var"]], 
    choices   = row_name,
    empty.ok  = FALSE
  )
  
  g <- 
    l %>% 
    plot_trajectory(
      y_var       = y_var # cell_prop, cell_count
      ,facet      = facet # wrap, grid
      ,scale_mode = scale_mode # free, fixed, free_x, free_y
    )
  # browser()
  # g
  l[["graph"]] <- g
  return(l)
  
}
# how to use
# ds1 %>% 
#   prep_plot_trajectory(
#     outcome_var    = "employed"  # outcome of interest (binary or continuous)
#     ,y_var         = "cell_prop" # cell_count, cell_prop
#     ,time_var      = "year"      # quarter, year, quarter_fiscal, year_fiscal
#     ,count_var     = "id"        # unique row identifier
#     # three optional dimensions 
#     ,color_var  = "gender"    # color 
#     ,vfacet_var = "race"      # facet rows on this variable
#     ,hfacet_var = "age"       # facet columns on this variable
#                                  # sum `percent_var` to get 100% 
#     # ,percent_var   = "gender"  # selected from optional dimensions (not NULL)
#     # ,percent_var   = "race"    # selected from optional dimensions (not NULL)
#     ,percent_var   = "gender"    # selected from optional dimensions (not NULL)
#                                  # adds "Total" as another level of `total_var`
#     # ,total_var = "gender"  # selected from optional dimensions (not NULL)
#     # ,total_var = "age"     # selected from optional dimensions (not NULL)
#     # ,total_var = "race"    # selected from optional dimensions (not NULL)
#     
#     ,facet         = "grid"      # grid, wrap
#     ,scale_mode    = "free"      # free, fixed, fixed_y, fixed_x
#   )

