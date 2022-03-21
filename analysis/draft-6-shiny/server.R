# load-packages  -----------------------------------
library(shiny)
library(magrittr)
# library(ggplot2)
requireNamespace("dplyr", quietly=FALSE)
requireNamespace("DT", quietly=FALSE) # DataTables (a jQuery library)

# declare-globals  -----------------------------------

# Define a server for the Shiny app
shinyServer( function(input, output, session) {
  
  #######################################
  ### Set any session-wide options
  # options(shiny.trace=TRUE)
  
  ### Call source files that contain semi-encapsulated functions.
  
  # load-data  -----------------------------------
  # dsSessionSurvey <- load_session_survey()
  # dsItemProgress <- load_item_progress()
  # ds_therapist <- load_therapist()
  # 
  # # tweak-data  -----------------------------------
  # 
  # # Create the DataTables objects (a jQuery library)  -----------------------------------
  # 
  # Display only the therapists in the selected agencies and call groups (if any are specified)
  # observe({
  #   d_survey <- ds_survey
  # 
  #   if( (is.null(input$employed_codes)) | ("--All--" %in% input$employed_codes) ) {
  #     #Don't filter the pool based on employed_codes if nothing or everything is specified.
  #   } else {
  #     d_survey <- d_survey |> 
  #       dplyr::filter(employed %in% input$employed_codes)
  #   }
  #   # 
  #   # remaining_tags <- d_pool %>%
  #   #   `$`('therapist_tag') %>%
  #   #   unique() %>%
  #   #   sort()
  #   # 
  #   # remaining_tags <- c("--Select a Therapist--", remaining_tags)
  #   # 
  #   # #stillSelected <- isolate(input$call_group_code[input$call_group_code %in% call_group_codes])
  #   # #stillSelected <- isolate(
  #   # #  ifelse(
  #   # #    length(call_group_codes)==0,
  #   # #    input$call_group_code,
  #   # #    input$call_group_code[input$call_group_code %in% call_group_codes]
  #   # #  )
  #   # #)
  #   # 
  #   # updateSelectInput(session, "therapist_tag", choices=remaining_tags)#, selected="--Select a Therapist--")
  # })
  
  ds_survey <- reactive({ 
    d_survey <- ds_survey_all
    
    if( (is.null(input$employed_codes)) | ("--All--" %in% input$employed_codes) ) {
      #Don't filter the pool based on employed_codes if nothing or everything is specified.
    } else {
      d_survey <- d_survey |>
        dplyr::filter(employed %in% input$employed_codes)
    }
    
    return (d_survey)
  })
  # 
  # #Update the clients available for the selected therapist
  # observe({ 
  #   if( is.null(input$therapist_tag) | (length(input$therapist_tag)==0) | (nchar(input$therapist_tag)==0) | (input$therapist_tag=="--Select a Therapist--") ) {
  #     #Don't display any clients if tag is specified.
  #     updateSelectInput(session, "client_number", choices="--Select Therapist First--")
  #   } else {
  #     clients <- dsItemProgress %>%
  #       dplyr::filter(therapist_tag == input$therapist_tag) %>%
  #       `$`('client_number') %>%
  #       unique() %>%
  #       sort()
  #     updateSelectInput(session, "client_number", choices=clients) 
  #   }
  # })
  # 
  
  output$survey_DT <- DT::renderDT({
    d1 <-
      ds_survey() %>%
      as.data.frame() %>%
      DT::datatable(
        class   = 'cell-border stripe'
        ,filter  = "top"
        ,escape = FALSE
        ,options = list(
          pageLength = 10,
          autoWidth  = FALSE
        )
      )
    return(d1)
  }
  # ,escape = FALSE
  )
  
  output$survey_dt <- shiny::renderDataTable({
    # Filter Client Progress data based on selections
    d <- ds_survey()
  

    # if( length(d$description_html) > 0 )
    #   d$description_html <- paste0('<p class="hanging">', d$description_html)
    # 
    # date_pretty <-  strftime(d_session_long$session_date, '%B %d, %Y')
    # d_session_long$session_date <- strftime(d_session_long$session_date, '%m<br/>%d')
    # d_session_long$session_date <- sprintf('<span title="Session %i occured on %s">%s</span>',
    #                                        d_session_long$session_number, date_pretty, d_session_long$session_date)
    # rm(date_pretty)
    # 
    # d_date <- as.data.frame(t(d_session_long[, c("session_date"), drop=F]))
    # colnames(d_date) <- sprintf("session_%02i", d_session_long$session_number)
    # d_date$description_html <- '<p class="accent flush">Session Month<br/>Session Day'
    # d_date$variable_index <- -2L
    # d_date$branch_item <- 0L
    # 
    # d <- plyr::rbind.fill(d, d_date, d_attend)
    # d <- d[order(d$variable_index), ]
    # 
    # #This strips out the "session_" prefix.
    # colnames(d) <- gsub("^session_(\\d{2})$", "\\1", colnames(d))
    # 
    # d$therapist_tag     <- NULL
    # d$client_number     <- NULL
    # d$agency_name       <- NULL
    # d$call_group_code   <- NULL
    # d$item              <- NULL
    # d$description_short <- NULL
    # d$description_long  <- NULL
    # d$variable_index    <- NULL
    # d$therapist_email   <- NULL
    # # browser()
    # 
    # 
    # d <- d %>%
    #   dplyr::select(description_html, dplyr::everything()) %>%  #, branch_item
    #   dplyr::rename_(
    #     '<p class="flush">Session Number' ="description_html"#,
    #     # "B"                               = "branch_item"
    #   )
    # d <- plyr::rename(d, replace=c(
    #   "description_html" = '<p class="flush">Session Number',
    #   "branch_item" = "B"
    # ))
    # columns <- c(setdiff(colnames(d), "B"), "B")

    return( d )
  },
    escape = FALSE,
    options = list(
      language = list(emptyTable="--<em>Please select a valid therapist-client combination above to populate this table.</em>--"),
      aoColumnDefs = list( #http://legacy.datatables.net/usage/columns
        list(sClass ="quasihide", aTargets=-1),
        list(sClass ="smallish", aTargets="_all")
      ),
  
      # columnDefs = list(list(targets = c(3, 4) - 1, searchable = FALSE)),
      searching = FALSE,
      paging    = FALSE,
      sort      = FALSE
      # $("td:eq(0)", nRow).css("font-size", "large");
      # rowCallback = I('
      #   function(nRow, aData) {
      #     // Emphasize rows where the `branch_item` column equals to 1
      #     if (aData[aData.length-1] == "1") {
      #       $("td", nRow).css("background-color", "#aaaaaa");
      #     }
      #   }')
    )
  )
  # 
  # 
  # output$trauma_symptoms <- renderPlot({
  #   dWide <- dsSessionSurvey# [, c("session_date", "trauma_score_caregiver", "trauma_score_child")]   
  #   
  #   if( input$therapist_tag == "--Select a Therapist--" ) {
  #     return(
  #       ggplot(data.frame(x=1, y=1, label="Please select a therapist tag above."), aes(x=x, y=y, label=label)) +
  #         geom_text(size=10, color="gray40") +
  #         theme_bw() +
  #         theme(panel.background  = element_rect(fill=NA, colour = NA)) +
  #         theme(plot.background   = element_rect(fill="gray95", colour = NA)) +
  #         theme(axis.ticks        = element_blank()) +
  #         theme(panel.spacing     = grid::unit(c(0,0,0,0), "lines")) +
  #         theme(axis.text         = element_blank()) +
  #         theme(axis.title        = element_blank()) +
  #         theme(panel.grid        = element_blank()) +
  #         theme(panel.border      = element_blank()) +
  #         theme(plot.margin       = grid::unit(c(0,0,0,0), "lines")) +
  #         theme(legend.position   = "top") +
  #         labs(title=NULL, x="Session Date", y="Trauma Score", colour=NULL, fill=NULL, shape=NULL)
  #     )
  #   } else {
  #     dWide <- dWide[dWide$therapist_tag == input$therapist_tag, ]
  #   }
  #   
  #   if( input$client_number > 0 )
  #     dWide <- dWide[dWide$client_number == input$client_number, ]
  #   
  #   dLong <- reshape2::melt(
  #     dWide, 
  #     id.vars       = c("therapist_tag", "client_number", "session_number", "session_date", "call_group_code", "agency_name"), 
  #     variable.name = "respondent", 
  #     measure.vars  = c("trauma_score_caregiver", "trauma_score_child"),
  #     value.name    = "score"
  #   )
  #   dLong$respondent <- gsub("^trauma_score_(.*)$", "\\1", dLong$respondent)
  #   
  #   dLong <- dLong[!is.na(dLong$score),]
  #   
  #   shape_respondent_dark         <- c("child"=21, "caregiver"=25)
  #   color_respondent_dark         <- c("child"="#1f78b4", "caregiver"="#33a02c") #From paired qualitative palette
  #   color_respondent_light        <- grDevices::adjustcolor( c("child"="#a6cee3", "caregiver"="#b2df8a"), alpha.f = .4)
  #   names(color_respondent_light) <- names(color_respondent_dark)
  #   
  #   if( all(is.na(dLong$score)) ) {
  #     ggplot(data.frame(x=1, y=1, label="There are no trauma scores associated\nwith the patient or caregiver."), aes(x=x, y=y, label=label)) +
  #       geom_text(size=10, color="gray40") +
  #       theme_bw() +
  #       theme(panel.background  = element_rect(fill=NA,colour = NA)) +
  #       theme(plot.background   = element_rect(fill="gray95",colour = NA)) +
  #       theme(axis.ticks        = element_blank()) +
  #       theme(axis.text         = element_blank()) +
  #       theme(axis.title        = element_blank()) +
  #       theme(panel.grid        = element_blank()) +
  #       theme(panel.border      = element_blank()) +
  #       theme(panel.spacing     = grid::unit(c(0,0,0,0), "lines")) +
  #       theme(plot.margin       = grid::unit(c(0,0,0,0), "lines")) +
  #       theme(legend.position   = "top") +
  #       labs(title=NULL, x="Session Date", y="Trauma Score", colour=NULL, fill=NULL, shape=NULL)
  #     
  #   } else {    
  #     ggplot(dLong, aes(x=session_date, y=score, color=respondent, fill=respondent, shape=respondent)) +
  #       geom_point(size=10, na.rm=T) +
  #       geom_line(na.rm=T) +
  #       scale_x_date(labels=scales::date_format("%Y-%m-%d")) +
  #       scale_color_manual(values=color_respondent_dark) +
  #       scale_fill_manual(values=color_respondent_light) +
  #       scale_shape_manual(values=shape_respondent_dark) +  
  #       coord_cartesian(ylim=c(0, 60)) +
  #       theme_bw() +
  #       theme(axis.ticks        = element_blank()) +
  #       theme(panel.spacing     = grid::unit(c(0,0,0,0), "lines")) +
  #       theme(legend.position   = "top") +
  #       labs(title=NULL, x="Session Date", y="Trauma Score", colour=NULL, fill=NULL, shape=NULL)
  #   } #trauma_symptoms plot
  # })
  # 
  output$table_file_info <- renderText({
    paste0(
      "<table>",
      # "<tr><td>Data Path:&nbsp;<td/><td>", determine_directory(), "<td/><tr/>",
      # "<tr><td>Survey Last Modified:&nbsp;<td/><td>", file.info(path_survey)$mtime, "<td/><tr/>",
      # "<tr><td>App Restart Time:&nbsp;<td/><td>", file.info("restart.txt")$mtime, "<td/><tr/>",
      "<table/>"
    )
  })
})