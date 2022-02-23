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
make_trajectory_data <- function(
  d
  # mandatory arguments
  ,outcome_var          # outcome of interest (binary or continuous)
  ,time_var             # quarter, year, quarter_fiscal, year_fiscal
  ,count_var            # unique row ids used to compute `cell_count`
  ,weight_var
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
  # time_var    = "year"# quarter, year, quarter_fiscal, year_fiscal
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
        sample_size      = n_distinct(!!rlang::sym(count_var)) # count of respondents
        ,cell_count      = sum(!!rlang::sym(weight_var), na.rm = T) # sum of weights, count of population
        ,outcome_median = isotone::weighted.median(y = !!rlang::sym(outcome_var), w = !!rlang::sym(weight_var) )
        ,outcome_mean   =   stats::weighted.mean(  x = !!rlang::sym(outcome_var), w = !!rlang::sym(weight_var),na.rm = T )
        
        # ,outcome_median = median(!!rlang::sym(outcome_var),na.rm = T )
        # ,outcome_mean   = mean(  !!rlang::sym(outcome_var),na.rm = T )
        #
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
#   make_trajectory_data(
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
# works with the product of `make_trajectory_data()`
make_trajectory_plot <- function(
  l
  ,y_var       = c("cell_prop", "cell_count")
  ,facet       = "grid"
  ,scale_mode  = "free_y"
  ,lab_title   = "Outcome: "
  ,lab_y       = NA_character_
  ,keep_na     = TRUE
){
  # TODO: check all input values
  checkmate::assert_subset(   y_var, choices = c("sample_size","cell_prop", "cell_count","outcome_mean","outcome_median"), empty.ok = FALSE)
  checkmate::assert_character(y_var, any.missing = FALSE, len = 1, pattern = "^sample_size|cell_prop|cell_count|outcome_mean|outcome_median$", null.ok = FALSE)
  
  # browser()
  # `make_trajectory_data()` creates list object `l` passed to `make_trajectory_plot`
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
  # browser()
  #### SCAFFOLDING ####
  d1 <- l$data
  if(keep_na == FALSE){
    if (!is.null(color_sym) ){ d1 <- d1 %>% filter(!is.na(!!color_sym))}
    if (!is.null(vfacet_sym)){d1 <- d1 %>% filter(!is.na(!!vfacet_sym))}
    if (!is.null(hfacet_sym)){d1 <- d1 %>% filter(!is.na(!!hfacet_sym))}
  }
  
  
  g <-
    d1 %>%
    {
      ggplot(., aes(
        x      = !!time_sym
        ,y     = !!y_sym
        ,color = !!color_sym
        ,group = !!color_sym
      )) +
        geom_line() +
        geom_point()#+
      # scale_fill_viridis_d(
      #   begin = 0, end = .8, direction = -1, option = "plasma"
      #                               ,guide= guide_legend(reverse=T)
      # )+
      # scale_color_viridis_d(
      #   begin = 0, end = .8, direction = -1, option = "plasma"
      #   ,guide= guide_legend(reverse=T)
      # )
      
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
    
    lab_y        <- dplyr::coalesce(lab_y, "Share of Clients")
    lab_caption  <- paste0("Sum all levels of (", toupper(percent_var),") to get 100%")
    
  } else if(y_var == "cell_count"){
    g <-
      g +
      scale_y_continuous(labels = scales::comma_format())
    
    lab_y        <- dplyr::coalesce(lab_y, "Number of Clients")
    lab_caption  <- "Values represent caseload count"
  } else if(y_var == "outcome_mean"){
    g <-
      g +
      scale_y_continuous(labels = scales::comma_format())
    
    lab_y        <- dplyr::coalesce(lab_y, "Mean")
    lab_caption  <- "Values represent mean of the outcome"
  } else if(y_var == "outcome_median"){
    g <-
      g +
      scale_y_continuous(labels = scales::comma_format())
    
    lab_y        <- dplyr::coalesce(lab_y, "Median")
    lab_caption  <- "Values represent meadian of the outcome"
  } else if(y_var == "sample_size"){
    g <-
      g +
      scale_y_continuous(labels = scales::comma_format())
    
    lab_y        <- dplyr::coalesce(lab_y, "Number of Respondents")
    lab_caption  <- "Values represent counts"
  }
  
  g <-
    g +
    labs(
      title   = paste0(lab_title,toupper(l$meta$outcome_var)),
      y       = lab_y,
      caption = lab_caption,
    )
  # browser()
  l[["graph"]] <- g
  return(l)
}

#
# ldp %>%
#   make_trajectory_plot(
#      y_var       = "cell_prop"
#     ,facet       = "grid" # grid, wrap
#     ,scale_mode = "free_y" # free , free_x , free_y, fixed
#    )

# ----- prep-plot-trajectory --------------------------------------------------



make_trajectory <- function(
  d
  ,outcome_var    # outcome of interest (binary or continuous)
  ,y_var         # cell_count, cell_prop
  ,time_var      # quarter, year, quarter_fiscal, year_fiscal
  ,count_var     # unique row identifier
  ,weight_var = 1L
  # three optional dimensions
  ,color_var   = NULL    # color
  ,vfacet_var  = NULL   # facet rows on this variable
  ,hfacet_var  = NULL # facet columns on this variable
  ,percent_var = NULL    # selected from optional dimensions (not NULL)
  ,total_var   = NULL
  ,facet       = "grid"   # grid, wrap
  ,scale_mode  = "free"      # free, fixed, fixed_y, fixed_x
  ,keep_na     = TRUE
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
    make_trajectory_data(
      outcome_var    = outcome_var# outcome of interest (binary or continuous)
      ,time_var      = time_var     # quarter, year, quarter_fiscal, year_fiscal
      ,count_var     = count_var
      ,color_var     = color_var
      ,weight_var    = weight_var
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
    make_trajectory_plot(
      y_var       = y_var # cell_prop, cell_count
      ,facet      = facet # wrap, grid
      ,scale_mode = scale_mode # free, fixed, free_x, free_y
      ,keep_na = keep_na
    )
  # browser()
  # g
  l[["graph"]] <- g$graph
  return(l)
  
}
# how to use
# ds1 %>%
#   make_trajectory(
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



graph_one_predictor <- function(d,predictor_name){
  # browser()
  # d <- ds4
  # predictor_name = 'gender_binary'
  d1 <-
    d %>%
    # filter(predictor==predictor_name) %>%
    # filter(predictor %in% c(predictor_name,"aggregate")) %>%
    mutate(
      value_level = fct_drop(value_level)
      ,sign_at_05 = p.value <= .05
    ) %>%
    # filter(
    #   outcome == "npr_01_no_fit"
    # ) %>%
    filter(
      !is.na(value_level)
    )
  # d1 %>% glimpse()
  # d1 %>% group_by(outcome) %>% count()
  # browser()
  # d %>% glimpse()
  g <-
    d1 %>%
    {
      ggplot(., aes(x=probability, y = outcome))+
        # geom_point(aes())
        geom_point(shape = 0, color = "black",data = . %>% filter(value_level=="aggregate"), size=3)+
        geom_point(aes(fill=value_level )  , shape = 21, size = 6, alpha = .3,
                   data = . %>% filter(predictor==predictor_name))+
        geom_point(aes(color = value_level), shape = 21, size = 6, alpha = 1,
                   data = . %>% filter(predictor==predictor_name) %>% filter()
        )+
        geom_point(shape = 21, size =1.5, fill = 'black',
                   data = . %>% filter(sign_at_05==TRUE) %>% filter(predictor==predictor_name)
        )+
        geom_text(x=-Inf,hjust=-.09, aes(label=scales::comma(count)),
                  fontface = "italic", alpha = .5,
                  data = . %>% filter(predictor=="aggregate"))+
        # geom_text(aes(label = scales::percent(probability, accuracy =1), color = value_level), hjust=-2.5, size=4, alpha = .6
        #           ,data = . %>% filter(predictor==predictor_name)
        # )+
        annotate(geom="text",label="sample size",x=-Inf,y=Inf,hjust=-.05,vjust=1.2,
                 alpha = .5, fontface = "italic")+
        scale_fill_brewer(type = "qual", palette = "Dark2")+
        scale_color_brewer(type = "qual", palette = "Dark2")+
        # geom_point(aes(fill = sign_at_05), shape = 21, size =2, color = "white")+
        # scale_fill_manual(values = c("TRUE"="grey50"),na.value=NA)+
        # scale_color_manual(values = c("male"="black", "female" = "red"))+
        scale_x_continuous(
          labels = scales::percent_format(accuracy=1)
          ,breaks = seq(0,1,.1)
          ,minor_breaks = seq(0,1,.05)
          ,limits = c(-.05,.7)
        )+
        scale_y_discrete(
          expand = expansion(mult = c(0.05,.1))
        )+
        geom_vline(xintercept = .5, linetype = "dashed", alpha = .1, size =1)+
        labs(
          title = paste0("Factor:",toupper(predictor_name))
          ,x = "Share of clients with positive outcome"
          # ,caption = "Hollow square = agggregate level\nThe first subgroup = reference\n  black dot within cirlce = groups are statistically different at (p < .05) level"
          ,caption = "Hollow square = agggregate level | The first subgroup = reference |  Black dot = subgroup is statistically different at p < .05 level"
          ,color = "Subgroup"
          ,fill = "Subgroup"
          ,y = NULL
        )+
        theme(
          legend.position = "bottom"
        )
    }
  g %>% quick_save(paste0("predictor - ", predictor_name),w=8, h=5)
  return(g)
}
# ds4 %>% graph_one_predictor("age_category")






graph_one_outcome <- function(d, outcome_name){
  # d <- ds4
  # outcome_name = 'applied_benefit'
  d1 <-
    d %>%
    # filter(outcome == "Applied for Benefits") %>%
    filter(outcome == outcomes_of_interest_levels[outcome_name]) %>%
    filter(!is.na(value_level)) %>%
    mutate(
      predictor_level = paste0(predictor, " - ", value_level) %>%
        as_factor()
    ) %>%
    relocate(predictor_level, .before="count") %>%
    mutate(
      value_level = fct_drop(value_level)
      # ,sign_at_05 = ifelse(p.value <= .05, TRUE, FALSE)
      ,sign_at_05 = case_when(
        p.value <= .05 ~ TRUE, TRUE ~ NA
      )
    )
  (min_left <- d1 %>% summarize(min=min(probability, na.rm = T)) %>% pull(min))
  (min_left <- round(min_left - .1, 1))
  (min_left <- ifelse(min_left<0,0,min_left))
  (max_right <- d1 %>% summarize(max=max(probability, na.rm =T)) %>% pull(max))
  (max_right <- round(max_right + .1, 1))
  # d1 %>% glimpse()
  # d1 %>% group_by(sign_at_05) %>% count()
  # browser()
  # d %>% glimpse()
  g <-
    d1 %>%
    {
      ggplot(., aes(x=probability, y = predictor_level))+
        geom_vline(
          xintercept = d1 %>% filter(predictor_level == "aggregate - aggregate") %>% pull(probability)
          , linetype = "dashed", alpha = .5, size =1)+
        geom_point(shape = 21, aes(fill = sign_at_05), size = 3, alpha = .6)+
        geom_text(aes(label = scales::percent(probability, accuracy = 1)), hjust = -0.3, color = "grey80")+
        scale_fill_manual(values = c("TRUE"="red"),na.value=NA)+
        scale_x_continuous(
          labels = scales::percent_format(accuracy=1)
          ,breaks = seq(0,1,.1)
          ,limits = c(min_left,max_right)
        )+
        labs(
          title = paste0("Performance for key demographic groups")
          ,subtitle = paste0("Indicator: ", outcomes_of_interest_levels[outcome_name])
          ,x = "% clients with positive outcome"
          ,caption = "Statistical significance indicates levels of GROUP are different"
          ,color = "Significant at p < .05"
          ,fill = "Significant at p < .05"
        )+
        theme(
          legend.position = "bottom"
        )
    }
  g %>% quick_save(paste0("outcome - ",outcome_name),w=6, h=5)
  return(g)
}

# ds4 %>% graph_one_outcome(names(outcomes_of_interest_levels)[1])
