#Starting with http://shiny.rstudio.com/gallery/basic-datatable.html
library(shiny)
library(ggplot2)

# Define the overall UI
shinyUI(fluidPage(theme="bootstrap_lumen.css",
  shiny::tags$head(
    includeCSS("./www/styles.css"), # Include our custom CSS
    tags$style('
      h1 { color:#7D647D; }
      body { background-image:url("images/bg2c.jpg"); }
      .accent { color:#7D647D; }
      .table.dataTable th { background-color:#7D647D; color:white; }
      .table.dataTable tr.odd { background-color:white; }
      .table.dataTable tr.even{ background-color:#eee; }
      .table .session { font-size:80%; padding:0px; text-align:center; }
      .table .smallish { font-size:100%; padding:0px 10px 0px 0px ; }
      .table .alignRight { text-align:right; font-size:80%; padding:0px; }
      .table .semihide { color:#dddddd; padding:0px; }
      .table .quasihide { color:#cccccc; font-size:10%; padding:0px; }
      .hanging { padding-left:3em; text-indent:-2em; margin:0; }
      .flush { padding-left:1em; text-indent:0em; margin:0; }
      .alignLeft { text-align:left; }
      .outcome { text-align:center; }
    ')
  ),
  headerPanel("TF-CBT"),
  HTML(
    '<div id="logo">
      <a href="http://oklahomatfcbt.org/"><img src="images/cropped-OK_TF-CBT_logo_v9_1_1_a_1.png" width="450" height="150" alt="Oklahoma TF-CBT"/></a>
    </div>'
  ),
  fluidRow(
    column(width = 3, 
      selectizeInput(
        inputId="employed_codes", label="Filter by Employment:", width="100%", multiple=TRUE,
        choices=c("--All--", sort(unique(as.character(ds_survey_all$employed))))
      )
    )      
  ), #End fluid row with the agency & call group dropdown boxes
#   fluidRow(
#     column(width = 9, 
#       selectInput(
#         inputId="therapist_tag", label="Select Therapist Tag:", width="100%", #selected="kobl",
#         choices=c("--Select a Therapist--", sort(unique(as.character(dsItemProgress$therapist_tag))))
#       )
#     ),
#     column(width = 3, 
#       selectInput(
#         inputId="client_number", label="Select Therapist's Client:", width="100%", #selected=2,
#         choices=sort(unique(as.character(dsItemProgress$client_number)))
#       )
#     )      
#   ), #End fluid row with the tag & client_number dropdown boxes
  tabsetPanel( type = "tabs",
    tabPanel(
      title = "Table", 
      HTML("Therapy session for the <em>therapist</em> and <em>client</em> selected above.  If you're just exploring, therapist `wfv3` is a good illustration.<br/><br/>"),
      # Create a new row for the table.
      fluidRow(
        shiny::dataTableOutput(outputId = "survey_dt")
      ), #End fluid row with the Group Call table
      HTML('<br/>If you believe any of the data reported to be inaccurate, please email us at <a href="mailto:OKTF-CBT@ouhsc.edu">OKTF-CBT@ouhsc.edu</a> and include any supporting documentation available to you so we might investigate the discrepancy promptly.<br/><br/></a>'),
      
#       HTML('&copy; 2014 <a href="http://oklahomatfcbt.org/" title="Oklahoma TF-CBT" class="accent">Oklahoma TF-CBT</a>'),
#       shiny::icon("code"), #This is a little cheat to get the table icons work
      HTML('Software for data collection and reporting developed by <a href="http://www.ouhsc.edu/BBMC/" class="accent">OUHSC BBMC</a> <a href="https://github.com/OuhscBbmc/" class="accent"><i class="fa fa-github"></i></a>')
    ) #End the (first) tab with the Group Call table
#     tabPanel(
#       title = "Therapist Training", 
#       HTML(
#         "
#           Training summary of each therapist within the call group or agency selected 
#           from the dropdown menu above.  Therapists are displayed in reverse chronological
#           order below, so those most recently trained appear at the top. Therapist 
#           attendance is reported both for their initial call series (a total of 12
#           calls held over 6 months with one consultant at varying times on Mondays
#           and Tuesdays) the Open Call (held every 2nd/4th Tuesday of the month from
#           9-10am with Amanda Mitten) and the Early Childhood Open Call (held every
#           1st/3rd Tuesday of the month from 9-10 with Amanda Mitten).
#           <br/><br/>
#           Each therapist's case status is displayed, options include active case, no case,
#           incomplete case and completed one case (required for successful consultation 
#           completion). Each therapist's consultation progress is also displayed, options
#           included completed, refer to open call, reassign to new group, incomplete-do
#           not reassign, TBD (displayed for those currently in consultation).
#           <br/><br/>
#           If you believe any of the data reported below to be inaccurate, please email
#           us at <a href='mailto:OKTF-CBT@ouhsc.edu'>OKTF-CBT@ouhsc.edu</a> and include 
#           any supporting documentation available to you so we might investigate the
#           discrepancy promptly.
#         "
#       ),
#       dataTableOutput(outputId = "therapist_training")
#     ), #End the (second) tab with the therapist training history
#     tabPanel(
#       title = "Trauma Symptom Tracking", 
#       "Tracking symptom severity over the life of the TF-CBT case. Trainers suggest, at a minimum, administering pre-treatment and post-treatment trauma measures.",
#       plotOutput(outputId='trauma_symptoms', width='95%', height='400px')
#     ), #End the (third) tab with the symptoms
    ,tabPanel(
      title = "Dynamic Table"
      ,fluidRow(
        DT::dataTableOutput(outputId = "survey_DT")
      ),
    ) #End the (third) tab with the debugging details
    ,tabPanel(
      title = "Graph"
      ,fluidRow(
        column(width = 3, # in bootstrap it's a grid of 4X3 (unverified memory) 
               selectizeInput(
                 inputId="var_one", label="Variable 1:", width="100%", multiple=FALSE,
                 choices=c("gender","age","race")
               )
        )      
      )
      ,fluidRow(
        shiny::plotOutput(outputId = "main_plot")
      )
    ) #End the (third) tab with the debugging details
    ,tabPanel(
      title = "Details",
      htmlOutput(outputId='table_file_info')
    ) #End the (third) tab with the debugging details
  ) #End the tabsetPanel
)) #End the fluidPage and shinyUI
