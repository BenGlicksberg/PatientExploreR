####################
# Ben Glicksberg
# Butte Lab / UCSF
# 2018-19
####################
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinyalert))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(timevis))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))

options("currentPath" = paste0(getwd(),'/'))
source("global.R")

##############################################################
############################# UI #############################
##############################################################

ui <- fluidPage(
  #### sets page style
  tags$head(
    tags$style(
      HTML(
        "body{
        height: auto;
        max-width: 1800px;
        margin-left: 250px;
        margin-right:300px;
        }
        .navbar{
        margin-left:250px;
        #margin-right:500px;
        #width:100%;
        max-width:1100px;
        }"
         )
      )
      ),
  navbarPage("PatientExploreR",id="inTabset", ### Navigation bar
             #### HOME tab
             tabPanel("Home",
                  style = "width:100%; margin-left:250px; margin-right:200px",
                      mainPanel(align="center",
                                useShinyjs(),
                                useShinyalert(),
                                fluidPage(theme = shinytheme("paper"),
                                          fluidRow( # intro fluidRow
                                                   tags$h3("PatientExploreR Sandbox Server"),
                                                   tags$p("PatientExploreR interfaces with a relational database of EHR data in the Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM). This application produces patient-level interactive and dynamic reports and visualization of clinical data, without requiring programming skills.",align="left"),
                                                   HTML(paste0(h5("All patient data are synthesized and contain ", tags$b("no Protected Health Information")))),
                                                   tags$br(),
                                                   fluidRow(
                                                      column(width=4, # Help button
                                                        actionButton("gotoHelp","",icon=icon("question-circle","fa-5x"),lib="font-awesome"),
                                                        fluidRow(tags$h5("Help"))),
                                                      column(width=4, # About button
                                                        actionButton("gotoAbout","",icon=icon("info-circle","fa-5x"),lib="font-awesome"),
                                                        fluidRow(tags$h5("About"))),
                                                      column(width=4, #Configuration button
                                                        actionButton("gotoConfiguration","",icon=icon("download","fa-5x"),lib="font-awesome"),
                                                        fluidRow(tags$h5("Configuration")))),         
                                                   tags$br(),
                                                   HTML(paste0(h5("To begin: click ",  tags$u(tags$b("Load Credentials")), "then", tags$u(tags$b("Login"))))),
                                                   tags$hr()
                                          ), # end intro fluidRow
                                          fluidRow(
                                            column(width=5, # Login column
                                                   tags$h4("Please log-in below:"),
                                                   # Credentials section 
                                                   textInput(inputId="sqlid", label="User ID", value = "", width = NULL, placeholder = "User ID"),
                                                   passwordInput(inputId="sqlpass", label="Password", value = "", width = NULL,placeholder = NULL),
                                                   textInput(inputId="sqlhost", label="Host", value = "", width = NULL, placeholder = "Host"),
                                                   textInput(inputId="sqldb", label="Database", value = "", width = NULL, placeholder = "Database"),
                                                   pickerInput(
                                                     inputId = "driver_picker", 
                                                     label = "Driver",  
                                                     choices = c("MySQL","PostgreSQL", "Amazon Redshift", "Microsoft SQL Server", "Microsoft Parallel Data Warehouse", "Google BigQuery"), 
                                                     selected = "MySQL",
                                                     multiple = FALSE),
      
                                                   textInput(inputId="sqlport", label="Port", value = "", width = NULL, placeholder = "Port"),
                                                   
                                                  fluidRow(column(width=6,
                                                                 actionButton(width = 150,
                                                                              inputId = "save_credentials",
                                                                              label = "Save Credentials")),
                                                  column(width=6,
                                                         actionButton(width =150,
                                                                               inputId = "load_credentials",
                                                                               label = "Load Credentials"))),
                                                  HTML("<br>"),
                                                  fluidRow(
                                                    column(width=12,
                                                 directoryInput('directory', label = "", value = getOption("currentPath"))
                                                 # credit: https://github.com/wleepang/shiny-directory-input
                                                    )
                                                  ),  
                                                  HTML("<br>"),
                                                  fluidRow(
                                                    column(width=6,
                                                           disabled(actionButton(width =150,
                                                                                 inputId = "logout",
                                                                                 label = "Logout"
                                                           ))
                                                    ),
                                                    column(width=6,
                                                          actionButton(width = 150,
                                                            inputId = "login",
                                                            label = "Login"
                                                          )))     
                                            ), # end Login column
                                            column(width=7, # utilities column
                                                   shinyjs::hidden(
                                                     div(id="dashboard_open",
                                                  fluidRow(
                                                    shinyjs::hidden(div(id="intro_help_panel",uiOutput("intro_help")))
                                                  ),
                                                  fluidRow( # utilities row
                                                   fluidRow( 
                                                    column(width=2,div(style = "height:20px;"),
                                                    fluidRow(
                                                      actionButton("gotoFinder","",icon=icon("search","fa-3x"),lib="font-awesome")),
                                                      fluidRow(
                                                        tags$h5("Patient Finder")
                                                         )),
                                                    column(width=10,align="left",div(style = "height:20px;"),
                                                           "Search for a patient directly or identify a cohort: query the EHR for a certain patient or find all patients that meet any criteria concept available from the CDM of any modality (e.g., Condition, Procedure).  Cohorts can be futher filtered by demographic features (e.g., age range, self-reported race), visualized, and exported.")
                                                   ), 
                                                   fluidRow(
                                                     column(width=2,div(style = "height:20px;"),
                                                            fluidRow(
                                                              actionButton("gotoReport", "",icon=icon("address-card","fa-3x"),lib="font-awesome")),
                                                            fluidRow(
                                                              tags$h5("Overall Report")
                                                            )),
                                                     column(width=10,align="left",div(style = "height:20px;"),
                                                            "Generate overall report of a selected patient's clinical history: this report will provide a chronological history of all events of all data modalities (e.g., Observations, Medications). Can filter by specific concepts and export.")
                                                     
                                                   ),
                                                   fluidRow(
                                                     column(width=2,div(style = "height:20px;"),
                                                            fluidRow(
                                                              actionButton("gotoInteractive", "",icon=icon("mouse-pointer","fa-3x"),lib="font-awesome")),
                                                            fluidRow(
                                                              tags$h5("Encounter Timeline")
                                                            )),
                                                     column(width=10,align="left",div(style = "height:20px;"),
                                                            "Interact and explore a selected patient's clinical encounter and visit timeline: investigate and visualize clinical events by visit occurrence. Selecting a visit in the interactive timeline will detail all associated clinical events. Can filter by visit (e.g., Outpatient) and admitting/dischanrge types.")
                                                     
                                                   ), 
                                                   fluidRow(
                                                     column(width=2,div(style = "height:20px;"),
                                                            fluidRow(
                                                              actionButton("gotoExplorer", "",icon=icon("wrench","fa-3x"),lib="font-awesome")),
                                                            fluidRow(
                                                              tags$h5("Data Explorer")
                                                            )),
                                                     column(width=10,align="left",div(style = "height:20px;"),
                                                            "Explore patterns of clinical events over time: for a selected patient, can view all data measured for categorical (e.g., Medications, Devices) and numeric (e.g., Measurement, Observation) types over time. Cateogrial variables displayed in a timeline and can be filtered for what is shown. Numeric variables are displayed as a timeseries which the user can interact with. \n
                                                                     Targeted view provides an in-depth graph of one variable at a time while the Multiplex view allows for simulaneous and linked exploration of multiple variables.")
                                                     
                                                   )
                                                  ) # end utilities row
                                                  
                                                  
                                                  
                                            ) # end utilities column
                                            ))  # end shinyjs:hidden
                                          ) #end fluidRow
                                ) # end fluidPage
                      ) # end mainPanel
             ), # end tabpanel Home

             tabPanel("Patient Finder",    
                      ############# FINDER tab
                      style = "width:100%; margin-left:250px; margin-right:200px",
                      mainPanel(
                        div(id="login_message_finder",
                            tags$h4("Please log in to contunue.")
                            ),
                                shinyjs::hidden(
                                  div(id="finder_open",
                                fluidRow(align="center",tags$h4("Patient Finder"),hr(width=100)),
                                fluidRow(
                                  column(width = 5,
                                  tags$p(align = "Left","Search for patients directly or based on clinical criteria (e.g., Condition ICD-10CM code). By selecting 'Criteria', all available ontologies will be displayed per modality which the user can use for searching. This will load demographic information for matching patients to allow for further refining.")
                                  
                                  ),
                                column(width = 3,

                                    radioButtons("finder_type", "Search Mode:",
                                                choices =  c("Search by Patient" = "pt_search",
                                                   "Search by Criteria" = "criteria_search"))

                                ),
                                column(width = 4,
                                       shinyjs::hidden ( # initialized as hidden, but show b/c of server function
                                         div(id="pt_search_open",
                                       fluidRow(      
                                         textInput(inputId = "pt_search_bar_finder",
                                               label="",
                                               placeholder = "Enter Patient ID...")),
                                         fluidRow(disabled(actionButton(inputId = "pt_search_button_finder",
                                                                 label = "Search")))
                                         )) # end shinyjs::show
                                )
   
                                  ), # end top row
                                fluidRow( # main criteria row
                                  shinyjs::hidden ( 
                                    div(id="criteria_search_open",
                                      tags$h4(align="left","Criteria (select from table):"),
                                      fluidRow(
                                          column(width=3,
                                                 pickerInput(
                                                   inputId = "finder_domain_picker", 
                                                   label = "Select Domain", 
                                                   choices = c("Condition","Device","Drug","Measurement","Observation","Procedure"),
                                                   selected = "Condition"
                                                 )  
                                            ),
                                          column(width=3,
                                                 pickerInput(
                                                   inputId = "finder_vocab_picker", 
                                                   label = "Select Vocabulary", 
                                                   choices = "",
                                                   options = list(
                                                     `actions-box` = TRUE, 
                                                     size = 25,
                                                     `selected-text-format` = "count = 1"
                                                   ),
                                                   multiple = TRUE
                                                 ) 
                                           ),
                                          column(width=3,
                                                 pickerInput(
                                                   inputId = "finder_class_picker", 
                                                   label = "Select Concept Class", 
                                                   choices = "",
                                                   options = list(
                                                     `actions-box` = TRUE, 
                                                     size = 25,
                                                     `selected-text-format` = "count = 1"
                                                   ),
                                                   multiple = TRUE
                                                 ) 
                                          ),
                                          column(width = 2,style="padding-top:20px;",
                                                 actionButton(inputId = "criteria_select_all_button_finder",
                                                              label = "Select All")
                                                 ),
                                          column(width = 1,style="padding-top:20px;",
                                                 actionButton(inputId = "criteria_select_none_button_finder",
                                                              label = "None")
                                          )
                                      ), # end picker Row 

                                column(width = 12,
                                       DT::dataTableOutput('finder_term_picker')
                                       )
                                )) # end criteria shinyjs
                                ),# end criteria row
                shinyjs::hidden ( 
                  div(id="criteria_search_text_open",       
                        fluidRow(
                          uiOutput("selected_criteria_options")
                        )
                  )),  # end shinyjs:hidden  # selected criteria_text
                shinyjs::hidden ( 
                  div(id="pts_found_open",       
                      fluidRow(
                        uiOutput("pts_found_display")
                      )
                  ))    # end pts_found shinyjs:hidden

                                )) # end shinyjs:hidden

                      ) # end mainPanel
                      ), # end Finder
             tabPanel("Overall Report",    
                      ############# REPORT tab
                      style = "width:100%; margin-left:250px; margin-right:200px",
                      mainPanel(
                        div(id="login_message_report",
                            tags$h4("Please log in to contunue.")
                        ),
                                shinyjs::hidden(
                                  div(id="report_open",
                                      fluidRow(align="center",uiOutput("overall_title"),hr(width=100)),
                                      fluidRow(
                                        uiOutput("report_pt_info"),
                                        hr()
                                      ), #end fluidRow
                                      fluidRow(
                                        uiOutput("report_pt_filter"),
                                        hr()
                                      ), #end fluidRow
                                      fluidRow(
                                        DT::dataTableOutput("report_table")
                                      ) #end fluidRow
                                  ))# end shinyjs Report
                      ) # end mainPanel
                      ), # end Report
              tabPanel("Encounter Timeline",   
                       ############# TIMELINE tab
                      style = "width:100%; margin-left:250px; margin-right:200px",
                      mainPanel(
                        div(id="login_message_timeline",
                            tags$h4("Please log in to contunue.")
                        ),
                                shinyjs::hidden(
                                  div(id="timeline_open",
                                      
                                      fluidRow(align="center",uiOutput("timeline_title"),hr(width=100))
                                      ,
                                      fluidRow(
                                        uiOutput("timeline_filter_options")
                                      )
                                      ,
                                     shinyjs::hidden(
                                         div(id = "encounter_selected_info_panel",
                                             fluidRow(uiOutput("encounter_info_text")),
                                          fluidRow(
                                            tabsetPanel(id = 'encounter_tables',
                                                        tabPanel("Conditions",
                                                                 tags$br(),
                                                                 DT::dataTableOutput("encounter_conditions_table")),
                                                        tabPanel("Devices",
                                                                 tags$br(),
                                                                 DT::dataTableOutput("encounter_devices_table")),
                                                        tabPanel("Measurements",
                                                                 tags$br(),
                                                                 DT::dataTableOutput("encounter_measurements_table")),
                                                        tabPanel("Medications",
                                                                 tags$br(),
                                                                 DT::dataTableOutput("encounter_medications_table")),
                                                        tabPanel("Observations",
                                                                 tags$br(),
                                                                 DT::dataTableOutput("encounter_observations_table")),
                                                        tabPanel("Procedures",
                                                                 tags$br(),
                                                                 DT::dataTableOutput("encounter_procedures_table"))
                                            )
                                          )


                                       )) #end shinyjs
                                      
 
                                   )) # end shinyjs Timeline  
                                  ) #end mainPanel   
                                      ), # end Timeline
             tabPanel("Data Explorer",    
                      ############# DATA EXPLORER tab
                      div(id="login_message_explorer",
                          tags$h4("Please log in to contunue.")
                      ),
                      style = "width:100%; margin-left:250px; margin-right:200px",
                        mainPanel(
                                  shinyjs::hidden(
                                    div(id="explorer_open",
                                        fluidRow(align="center",uiOutput("explorer_title"),hr(width=100)),
                                        fluidRow(
                                          column(width =3,
                                                 uiOutput("explorer_radio_buttons")
                                                 ),
                                          column(width=9,
                                                 uiOutput("explorer_description")
                                            
                                          )
                                        ),
                                        fluidRow(
                                          hr(),
                                          uiOutput("explorer_data")
                                        )
                                        
                      )) # end shinyjs Explorer
                        ) # end mainPanel      
             ), # end Explorer
             navbarMenu("More",
                        tabPanel("Help", 
                                 style = "width:100%; margin-left:250px; margin-right:200px",
                                 mainPanel(
                                 fluidRow(align="center",tags$h4("Help"),hr(width=100)),
                                 fluidRow(tags$h5("Step-by-step instructions for all pages by section below.")),
                                 fluidRow(tags$p("For further help, please contact Ben Glicksberg at benjamin.glicksberg@ucsf.edu")),
                                 tags$br(),
                                 tabsetPanel(id = 'help_pages',
                                             tabPanel("Home/Login",
                                                      tags$br(),
                                                      fluidPage(
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Home/home1.png", width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Main App Screen"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Tab panel for navigating app"), 
                                                                   tags$li("Go to help page (this page)"), 
                                                                   tags$li("Learn more about our group and the application, data sources, and data format"),
                                                                   tags$li("Instructions for how to download, install, and configure app for your EHR data"),
                                                                   tags$li("Instructions for how to run the sandbox server")
                                                                 )
                                                                 )
                                                        ),        
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5, align = 'center',
                                                                 tags$img(src = "images/Home/home2.png", width = '275px', height = '500px')),
                                                          column(width=7,
                                                                 tags$b("Login Fields"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("All criteria to enable connecting to EHR data"), 
                                                                   tags$li("Username field (not required if not necessary to connect to database)"), 
                                                                   tags$li("Password field (not required if not necessary to connect to database)"),
                                                                   tags$li("Host field (required): For MySQL this is typically a server address, but in other formats (e.g., PostgreSQL), this can be 'server/database'"),
                                                                   tags$li("Database field (required): For MySQL this is the database itself. For PostgreSQL this is the schema."),
                                                                   tags$li("Driver type (required): Relational database structure in which EHR data are stored (see below for more details)"),
                                                                   tags$li("Port for connection (not required)"),
                                                                   tags$li("Saves current credentials that are entered into the credentials .Renviron file (see Configuration page for more details) in the specified path (see #11). Note this button is disabled for the public sandbox webserver."),
                                                                   tags$li("Loads credentials into respective field from credentials .Renviron file (see Configuration page for more details). Will only load fields that adhere to these formatting guidelines. This button will only be enabled if an .Renviron file can be found in the current specified path (see #11)."),
                                                                   tags$li(HTML("Set directory for credentials: Uses <a href= 'https://github.com/wleepang/shiny-directory-input'>DirectoryInput package </a> for selecting directory with credentials file. Credentials are saved in an .Renviron file (see Configuration page for more details). Note this button is disabled for the public sandbox webserver.")),
                                                                   tags$li("Login/Logout buttons")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5, align = 'center',
                                                                 tags$img(src = "images/Home/home3.png", width = '200px', height = '350px')),
                                                          column(width=7,
                                                                 tags$b("Login Fields"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li(HTML("Connections can be made to various relational database formats. For MySQL, connection is made through the <a href = 'https://cran.r-project.org/web/packages/DBI/index.html'> DBI </a> package and <a href = 'https://cran.r-project.org/web/packages/RMySQL/index.html'> RMySQL </a> driver For all others, the OHDSI group's <a href = 'https://github.com/OHDSI/DatabaseConnector'> DatabaseConnector </a>, <a href = 'https://github.com/OHDSI/DatabaseConnectorJars'> DatabaseConnectorJars </a> (drivers), and <a href = 'https://github.com/OHDSI/SqlRender'> SqlRender </a> packages are utilized."))
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5, align = 'center',
                                                                 tags$img(src = "images/Home/home4.png", width = '200px', height = '350px')),
                                                          column(width=7,
                                                                 tags$b("Login Fields"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Loads available credentials from .Renviron file"),
                                                                   tags$li("Click to begin login process. This will check if connection to OMOP server can be made and if the OMOP tables exist. The app will not continue if required tables are missing. A warning message will be appear if tables are empty (but the app will still be able to run). Once these criteria are met, the app will load the data ontology (and save it if it doesn't exist in the specified direcotry) as well as patient demographic information.")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5, align = 'center',
                                                                 tags$img(src = "images/Home/home5.png", width = '275px', height = '300px')),
                                                          column(width=7,
                                                                 tags$b("Section Selections"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li(HTML("Click to get to this Help page.")),
                                                                   tags$li("Patient Finder can be used for cohort querying and basic plots and information. Select a certain patient for other sections"),
                                                                   tags$li("Requires a patient selected from #2. Interactive report of all clinical data generated as well as an automated clinical summary."),
                                                                   tags$li("Requires a patient selected from #2. Intearactive timeline plot of clinical encounters as well as frequency plots."),
                                                                   tags$li("Requires a patient selected from #2. The ability to explore finegrain details about patient clinical variables over time.")
                                                                 )
                                                          )
                                                        )
                                                      ) #end fluidPage
                                                      ), # end Home help tab
                                             tabPanel("Patient Finder",
                                                      tags$br(),
                                                      fluidPage(
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Finder/finder1.png", width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Main Finder Screen"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Search for patients either directly or by clinical criteria (mix-and-match of any type; details below). Searching for a patient will load all relevant clinical and encounter data for subsequent sections.")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Finder/finder2.png", width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Direct Search"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Sample generated patient for use in this example (see About section for more details)."),
                                                                   tags$li("Search for patient: direct search will first check if patient exists and load all data if so.")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Finder/finder3.png", height = '200px', width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Criteria Search"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Can search by all concepts contained in the CDM. Can first filter by Domain."),
                                                                   tags$li("Can also filter by vocabulary"),
                                                                   tags$li("Can also filter by concept class"),
                                                                   tags$li("Concepts can be searched for in the search box"),
                                                                   tags$li("Concepts can be selected by clicking the items in the table. This shows all concepts available based on the criteria above." ),
                                                                   tags$li("Can browse concept table by clicking the Next and Previous buttons" )
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Finder/finder4.png", height = '200px', width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Criteria Search Continued"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Vocabularies (and Concept Classes) are filtered by the previous selection. In this case, ICD10CM, ICD9CM, and SNOMED concepts can be selected as available from the Condition domain.")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Finder/finder5.png", height = '200px', width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Criteria Search Continued"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Specific codes/concepts can be searched for (e.g., ICD10CM: K51) which filters the concept table by relevant fields"),
                                                                   tags$li("The Select All button will automatically select all concepts (in this case n=64) that are filtered based on criteria"),
                                                                   tags$li("The None button unselects all concepts within the filtered list"),
                                                                   tags$li("The criteria filtering limits the concept space by domain (can add different types)")
                                                                 )
                                                          )
                                                        ),
                                                        
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Finder/finder6.png", height = '175px', width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Criteria Search Continued"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("List of all criteria selected from above. Multiple types can be included here "),
                                                                   tags$li("Table of all selected critera by term and vocabulary. Use Next and Previous to browse"),
                                                                   tags$li("Select an item from the Selected Criteria table and click this button to remove item from selected list."),
                                                                   tags$li("Reset Search will remove all selected criteria"),
                                                                   tags$li("Search Type: 'or' will identify a cohort that meet EITHER of ay of the selected criteria; 'and' will require all conditions (i.e., concepts) met."),
                                                                   tags$li(HTML("Search Strategy: Direct will search for the concept directly terms directly (using the _source columns); Mapped (recommended) will first map terms to common ontology (e.g., SNOMED) and find/include all descendent concepts in search  (see <a href = 'https://github.com/BenGlicksberg/ROMOP'> ROMOP package </a> for more detials).")),
                                                                   tags$li("Click to begin searching for cohort based on selection criteria/strategies")
                                                                   )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Finder/finder7.png", height = '350px', width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Criteria Search Cohort"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Table of all selected patients in cohort. Can filter cohort below by any demographic feature (automatically adjusts table and plots; see below for more details)"),
                                                                   tags$li("Table can be sorted by clicking on columns and can show different amounts of entries per page. "),
                                                                   tags$li("Table can be filtered by any demographic filter options by selecting subsets below column name."),
                                                                   tags$li("Lists number of patients in filtered cohort"),
                                                                   tags$li("Navigate the cohort table by these control buttons"),
                                                                   tags$li("Can export/save cohort demographic table into a .csv file for utility in other programs."),
                                                                   tags$li("Show plots of demographic features for cohort. Dynamically changed based on filter options"),
                                                                   tags$li("Other sections of the app require a patient to be selected. Clicking on the seach button for patient in the row begins the search process.")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Finder/finder8.png", height = '200px', width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Selected Cohort Plots"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Hide plots"),
                                                                   tags$li("Dynamic and interactive (hoverable for tooltip text) plots of demographic features for selected cohort. Automatically altered based on filter options. Can be exported by hovering over the plot and clicking the plotly button for 'Download plot as png' ")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Finder/finder9.png", height = '150px', width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Patient Search from Selected Cohort"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Filtering cohort subsets table below. In this example, 'Male' patients are selected only in addition to an age range."),
                                                                   tags$li("The second row is our patient of interest"),
                                                                   tags$li("Click on the Search button in the last column: retrieves all clinical and encounter data for selected patient for use in other sections")
                                                                    )
                                                          )
                                                        )
                                                        ) # end fluidpage
                                                      ), #end Finder help tab
                                             tabPanel("Patient Report",
                                                      tags$br(),
                                                      fluidPage(
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Report/report1.png",width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Main Report Screen"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Demographic background for selected patient (in this case patient id 9000000)"),
                                                                   tags$li("Automatically generated clinical report for selected patient. Inlcudes information such as time in EHR, number of encounters (and by type), and clinical modalitiy entries (and by type).")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Report/report2.png", height = '350px')),
                                                          column(width=7,
                                                                 tags$b("Patient Report"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Table of all clinical concepts recorded for selected patient. Includes formatted data for all domains and organized by Date, Type (i.e., domain), Event (i.e., concept), and Value (if available; e.g. Measurement value)."),
                                                                   tags$li("Browse entries in table"),
                                                                   tags$li("Selet Data Modalities (i.e., domains) to include in report. Selections here will filter items from report table as well as filter options in #4 (i.e., will not display any items for domain if domain not selected)"),
                                                                   tags$li("Filter specific concepts/items per modalitiy to include in report"),
                                                                   tags$li("Can export all clinical data for given patient in .csv file for use in other programs or for further work")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Report/report3.png", height = '350px')),
                                                          column(width=7,
                                                                 tags$b("Patient Report Options"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Select modalities (i.e., domains) to include in report")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Report/report4.png", height = '350px')),
                                                          column(width=7,
                                                                 tags$b("Patient Report Options Continued"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Unselected modalities will not be included in report."),
                                                                   tags$li("Specific items in each modalitiy can be selected/unselected (in this case within Measurements)"),
                                                                   tags$li("In addition to entire modalities, specific items unselected within each will not be included in the report"),
                                                                   tags$li("The export button will save a .csv list of the filtered report")
                                                                 )
                                                          )
                                                        )
                                                      ) #end fluidPage
                                                        ), #end Report help tab
                                             tabPanel("Encounter Timeline",
                                                      tags$br(),
                                                      fluidPage(
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Timeline/timeline1.png",width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Main Timeline Screen"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("All encounter infomration for selected patient (in this case patient id 9000000). Clicking radiobuttonss under Plot Encounters will produce bar plots breakdown for selected type (e.g., Visit Types)"),
                                                                   tags$li(HTML("Select items from these fields will automatically generate and produce an interactive timeline visualization built using the <a href = 'https://github.com/daattali/timevis'>timevis package </a>. "))
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=5,
                                                                 tags$img(src = "images/Timeline/timeline2.png", width = '400px')),
                                                          column(width=7,
                                                                 tags$b("Timeline options"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("All options here are from what is available for the selected patient based on his/her encounter history. Selecting from these list will add all encounters of these types to the timeline.")
                                                                 )
                                                            )
                                                          ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Timeline/timeline3.png", width = '400px')),
                                                          column(width=6,
                                                                 tags$b("Timeline Options Continued"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Inveractive timeline visualization of encounters for selected patient. Can scroll left and right, zoom in and out, and select items by clicking on them. Encounter items are described by their Visit Type text"),
                                                                   tags$li("All Visit Types (e.g., Inpatient Visit) selected in the dropdown are included in the visualization plot"),
                                                                   tags$li("All Admitting Concept Types (e.g., Inpatient Hospital) selected in the dropdown are included in the visualization plot"),
                                                                   tags$li("All Discharge Concept Types (e.g., Home) selected in the dropdown are included in the visualization plot")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Timeline/timeline4.png", width = '400px')),
                                                          column(width=6,
                                                                 tags$b("Timeline Selections"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Buttons to automatically navigate timeline. Fit All Encounters will navigate the timeline to put all items on screen (note this can increase height of plot by a lot). Focus Past Year will focus the timeline to the previous year (e.g., 2017)."),
                                                                   tags$li("All encounter items in the timeline can be selected. Here the third Outpatient Visit is selected for the current patient. Clicking on the timeline item populates information about the encounter below."),
                                                                   tags$li("All information covered in the timeline for the encounter is displayed in the Encounter Information section"),
                                                                   tags$li("All clinical data recorded during the encounter is contained within a table separated by tabs of each domain."),
                                                                   tags$li("These tables contain pre-selected columns pertinent to the selected domain (e.g., 'Condition Status Type' for Condition) "),
                                                                   tags$li("Clicking CSV or Excel buttons on each tab will allow for downloading the table in the affiliated format.")
                                                                 )
                                                          )
                                                        )
                                                        )# end fluidPage
                                                      ), #end Timeline help tab
                                             tabPanel("Data Explorer",
                                                      tags$br(),
                                                      fluidPage(
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer1.png",width = '450px')),
                                                          column(width=6,
                                                                 tags$b("Main Explorer Screen"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("The Data Explorer section allows users to explore trends in categorical and numeric clinical data for a selected patient (in this case patient id 9000000). There are 3 ways in which to explore: i) Targeted: one modality at a time; ii) Multiplex: multiple modalities plotted along the same x-axis (i.e., time); and iii) Multiplex Timeline: multiple modalities plotted on a grouped timeline visualization. ")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer2.png", width = '650px')),
                                                          column(width=6,
                                                                 tags$b("Targeted Explorer: Categorical"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("The Targeted explorer option plots data based on clinical modality at a time (e.g. Conditions or Measurements). For all categorical data (Conditions, Devices, Medications, and Procedures), these are plotted as an interactive timeline. "),
                                                                   tags$li("For the timeline there are two view types possible: Event will display each item as a single time point; Range will plot the item from start-to-end period if that information exists (note these are not always accurate) "),
                                                                   tags$li("Users can select items of each modality from the dropdown to include in the plot. Only concepts recorded for the selected patient are available.")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer3.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Targeted Explorer: Categorical"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Timeline visualization automatically produced containing elements selected in the dropdown list in #2. As with other timelines, this is completely interactive."),
                                                                   tags$li("Only items from the dropdown list are included in the timeline. These items are populated directly from patient-speicific concepts.")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer4.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Targeted Explorer: Categorical"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("If Range option View Type is selected, the concept items in the timeline are displayed based on a range if available "),
                                                                   tags$li("For items without an end date, they are still displayed as an Event (or single time point)"),
                                                                   tags$li("Items with a range (e.g., Ulcerative Colitis) are displayed as a bar across the time period")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer5.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Targeted Explorer: Categorical"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("All items in the timeline can be selected by clicking"),
                                                                   tags$li("Information pertaining to the selected concept are displayed")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer6.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Targeted Explorer: Numeric"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Domains that contain numeric data, specifically Measurements and Observations (although the latter is a mix), are first displayed as a frequency table with the number of recorded events for each item."),
                                                                   tags$li("Items in the frequency table can be selected by clicking to automatically produce a line plot")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer7.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Targeted Explorer: Numeric"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Select an item from the frequency table to view trends over time for that data concept ('C reactive protein [Mass/volume] in Serum or Plasma' selected in this case)"),
                                                                   tags$li("An interactive line plot of all data points for the selected patients in the selected data concept is automatically produced"),
                                                                   tags$li("High/Low values are automatically colored and coded based on the internal range system"),
                                                                   tags$li("Hovering over each data point displays the value")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer8.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Multiplex Explorer Mode"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Multiplex mode: display multiple types of data on the same time scale plot"),
                                                                   tags$li("Categorical data can be selected in which items are displayed as a dot plot"),
                                                                   tags$li("Numerical data can be selected in which terms are displayed as a line plot")
                                                                   
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer9.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Multiplex Explorer Mode Continued"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Numeric data types (Measurements and Observations) can be selected based on what was measured for the selected patient"),
                                                                   tags$li("Categorical data types (all others) can be selected in the same fashion")
                                                                   
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer10.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Multiplex Explorer Mode Continued"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("The interactive multiplex plot is populated with selected items. Users can zoom in by clicking and dragging a section. All other items are then zoomed in at the same scale. Double clicking returns to original scale. Plots can be downloaded by hovering over the image and selecting 'Download plot as png'."),
                                                                   tags$li("All items selected above are displayed in the legend. Categorical data are as dot plots on the top.")
                                                                   
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer11.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Multiplex Timeline Explorer Mode"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("In the Multiplex Timeline Explorer mode, multiple types of data are displayed on the same time scale in a timevis plot."),
                                                                   tags$li("All data types can be selected and included based on data available from the selected patient")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer12.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Multiplex Timeline Explorer Mode Continued"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("Categorical data can be selected like before"),
                                                                   tags$li("Numeric data can be selected as well like before")
                                                                 )
                                                          )
                                                        ),
                                                        tags$br(),
                                                        tags$hr(),
                                                        fluidRow( 
                                                          column(width=6,
                                                                 tags$img(src = "images/Explorer/explorer13.png", width = '425px')),
                                                          column(width=6,
                                                                 tags$b("Multiplex Timeline Explorer Mode Continued"),
                                                                 tags$br(),
                                                                 tags$ol(
                                                                   tags$li("All selected items for the selected patient are displayed in a timevis plot grouped by domain. This is interactive with the same options available as before"),
                                                                   tags$li("Data can be viewed as an Event or Range as before"),
                                                                   tags$li("All data items can be selected by clicking"),
                                                                   tags$li("Information for the selected data item are displayed above the plot")
                                                                 )
                                                          )
                                                        )
                                                        
                                                      ) #end fluidPage
                                             ) #end Explorer help tab
                                 ) #end help_pages Tabset panel
                                 ) #end Help section mainPanel
                                 ), #end Help section tab
                        tabPanel("About",
                                 fluidRow(align="center",tags$h4("About"),hr(width=100)),
                                 tags$br(),
                                 style = "margin-left:250px; max-width:1000px; margin-right:250px",
                                 tags$h5("Manuscript Information"),
                                 tags$p("TBD"),
                                 tags$br(),
                                 tags$h5("Sandbox Server"),
                                 p(HTML("The Centers for Medicare and Medicaid Services (CMS) have released a synthetic clinical dataset <a href='https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF.html'>DE-SynPUF</a> in the public domain with the aim of being reflective of the patient population but containing no protected health information. The OHDSI group has underwent the task of converting these data into the <a href='https://github.com/OHDSI/ETL-CMS'>OMOP CDM format </a>. Users are certainly able to set up this configuration on their own system following the instructions on the GitHub page. We obtained all data files from the <a href='ftp://ftp.ohdsi.org/synpuf'> OHDSI FTP server</a> (accessed June 17th, 2018) and created the CDM (DDL and indexes) according to their  <a href = 'https://github.com/OHDSI/CommonDataModel/tree/master/PostgreSQL'>official instructions</a>, but modified for MySQL. For space considerations, we only uploaded one million rows of each of the data files. The sandbox server is a Rshiny server running as an Elastic Compute Cloud (EC2) instance on Amazon Web Services (AWS) querying a MySQL database server (AWS Aurora MySQL). ")),
                                 tags$br(),
                                 tags$h6("Example Patient"),
                                 p(HTML("As the DE-SynPUF data does not contain patient measurement results, we generated a profile for a patient with Chron's Disease with representative clinical data (e.g., disease codes and lab test results) for illustrative purposes. Users can recreate this example patient using the script <a href = 'https://github.com/BenGlicksberg/PatientExploreR/blob/dev/data/new_pt_insert_commands.txt'> found here </a>. The script is formatted for a MySQL database.")),
                                 tags$h5("Who We Are"),
                                 p(HTML("PatientExploreR was created by Benjamin Glicksberg (ben.glicksberg@gmail.com) while working as a post-doctoral scholar in the lab of <a href = 'http://buttelab.ucsf.edu/'> Dr. Atul Butte </a> at UCSF within the <a href = 'http://bakarinstitute.ucsf.edu/'>Bakar Computaitonal Health Sciences Institute </a>. This project was a collaboration between many individuals (see Manuscript Information) from UCSF, Columbia University, and the Icahn School of Medicine at Mount Sinai.")),
                                 tags$hr()
                                 ),
                        tabPanel("Configuration",
                                 fluidRow(align="center",tags$h4("Configuration"),hr(width=100)),
                                 tags$br(),
                                 style = "margin-left:250px; max-width:1000px; margin-right:250px",  
                                 tags$h5("Source Files"),
                                 p(HTML("<a href = 'https://github.com/BenGlicksberg/PatientExploreR'> GitHub Repository </a>")),
                                 tags$br(),
                                 tags$h5("Requirements"),
                                 tags$ul(
                                   tags$li("Personal Computer or Server with connection to internet"),
                                   tags$li("R"),
                                   tags$li("All required packages (see Install.R)"),
                                   tags$li("Database software (either: MySQL, PostgreSQL, Amazon Redshift, Microsoft SQL Server, Microsoft Parallel Data Warehouse, Google BigQuery"),
                                   tags$li("Access to Electronic Health Record data (reccommended for use with a de-identified version) that is properly formatted to OMOP Common Data Model v5")
                                 ),
                                 tags$h5("Installation"),
                                 tags$ol(
                                   tags$li("Download app from GitHub (see Source Files)"),
                                   tags$li("Navigate to diretory and run Install.R (Rscript Install.R) to install all required packages"),
                                   tags$li("(Optional) Create .Renviron file in directory with database credentials (Note: this can be done in the app itself). See section below for formatting this file."),
                                   tags$li("Open app using either Rstudio (Run App) or from command line: R -e \"shiny::runApp('PatientExploreR.R')\", then navigate to the IP address after \"Listening on…\" using a web browser.")
                                   ),
                                 tags$br(),
                                 tags$h6("Storing credentials"),
                                 HTML("For quick connection, users can quickly load and save their credentials to connect to EHR database within an R environment file (.Renviron). Either this file can be created after the credentials are entered in the input fields (Save Credentials button) which will automatically create this file in the directory of interest. Alternatively, users can create an .Renviron file the project directory in the following format:<br>
                                        driver = ' ' <br>
                                        host = ' ' <br>
                                        username= ' ' <br>
                                        password = ' ' <br>
                                        dbname = ' ' <br>
                                        port = ' '<br>
                                        <br>
                                        Full instructions on these connection parameters can be found from the OHDSI consortium's <a href= 'https://github.com/OHDSI/DatabaseConnector'> Database Connector <\a> GitHub page.")
                        )
             )
    ) # end NavbarPage
) # end UI


##############################################################
########################### SERVER ###########################
##############################################################

server <- function(input, output,session) {

if (!interactive()) sink(stderr(), type = "output") 
  
  
  ############################
  ######### VARIABLES ########
  ############################
  
  logged_in <- reactiveVal(value=FALSE)
  
  finder_term_table <- reactiveValues(df = data.table()) # terms per data ontology
  selected_finder_term_table <- reactiveValues(df = data.table(vocabulary = character(),term = character())) # selected terms based on vocabulary/term
  pts_search_found <-reactiveValues(pt_list = list(),genders=list(),races=list(),statuses=list(),ethnicities=list(),ages=list())
  global_cohort_found <- reactiveValues(df = data.table(person_id=integer(),year_of_birth=integer(), age = numeric(), Gender = character(), Race = character(), Ethnicity = character(), Status =character())) 

  vocab_term <-reactiveValues(l = c())
  
  pt_id_clicked_from_table <-reactiveVal(NULL)
  pt_id_selected <-reactiveVal(NULL)
  enc_id_selected <-reactiveVal(as.character(NULL))
  pt_data_selected <-reactiveValues(l = list())
  pt_data_report <- reactiveValues(df = data.table())
  multiplex_timeline <- reactiveValues(df = data.table())
  
  # check for Renviron file
  if (file.exists(paste0(getOption("currentPath"),".Renviron"))){
    enable("load_credentials")
  }else{
    disable("load_credentials")
  }
  
  
  ############################
  ###### INITIALIZATION ######
  ############################
  
  ### general query function ###
  sqlQuery <- function(query) {
    
    if (input$driver_picker=="MySQL") {
      
      # creating connection object
      drv <- dbDriver("MySQL")
      fullConnectString <- setConnectFunction(input$sqlid, input$sqlpass, input$sqlhost, input$sqldb, input$sqlport)
      con <- eval(parse(text = fullConnectString))
      
      # close db connection after function
      on.exit(DBI::dbDisconnect(con))
      
      # send query
      res <-DBI::dbSendQuery(con, query)
      
      # get elements from results
      result <- DBI::fetch(res, -1)
      
    } else {
    if (input$driver_picker == "PostgreSQL") {
      drv <- "postgresql"
    } else if (input$driver_picker == "Oracle") {
      drv <- "oracle"
    } else if (input$driver_picker == "Amazon Redshift") {
      drv <- "redshift"
    } else if (input$driver_picker == "Microsoft SQL Server") {
      drv <- "sql server"
    } else if (input$driver_picker == "Microsoft Parallel Datawarehouse") {
      drv <- "pdw"
    } else if (input$driver_picker == "Google BigQuery") { 
      drv <- "bigquery"
    }  
      
      # creating connection object using DatabaseConnector
      con <- DatabaseConnector::connect(dbms = drv,
                                        server = input$sqlhost,
                                        user = input$sqlid,
                                        password = input$sqlpass,
                                        schema = input$sqldb,
				                                port = input$sqlport)
      
      # close db connection after function
      on.exit(DatabaseConnector::disconnect(con))
      
      # translate query using SqlRender
      translated_query <- SqlRender::translateSql(query, targetDialect = drv)$sql
      
      # query using DatabaseConnector function
      result <- DatabaseConnector::querySql(con, translated_query)
      
      # coerce columns to lowercase
      colnames(result) <- tolower(colnames(result))
    }
    return(result)
  }
  
  
  enable_tabs <- function(){
    shinyjs::show("dashboard_open", anim = TRUE)
    shinyjs::show("finder_open", anim = TRUE)
    shinyjs::show("report_open", anim = TRUE)
    shinyjs::show("timeline_open", anim = TRUE)
    shinyjs::show("explorer_open", anim = TRUE)
    
    shinyjs::hide("login_message_finder", anim = FALSE)
    shinyjs::hide("login_message_report", anim = FALSE)
    shinyjs::hide("login_message_timeline", anim = FALSE)
    shinyjs::hide("login_message_explorer", anim = FALSE)

  }
  
  disable_tabs <- function(){
    shinyjs::hide("dashboard_open", anim = TRUE)
    shinyjs::hide("finder_open", anim = TRUE)
    shinyjs::hide("report_open", anim = TRUE)
    shinyjs::hide("timeline_open", anim = TRUE)
    shinyjs::hide("explorer_open", anim = TRUE)

    shinyjs::show("login_message_finder", anim = FALSE)
    shinyjs::show("login_message_report", anim = FALSE)
    shinyjs::show("login_message_timeline", anim = FALSE)
    shinyjs::show("login_message_explorer", anim = FALSE)
    
  }
  
  
  #### LOGIN
  

  observeEvent(input$login, {
    shinyjs::disable("login")

    disable("sqlid")
    disable("sqlpass")
    disable("sqlhost")
    disable("sqldb")
    disable("driver_picker")
    disable("sqlport")
    disable("load_credentials")
    disable("save_credentials")
    
    username <- input$sqlid
    password <- input$sqlpass
    host <- input$sqlhost
    dbname <- input$sqldb
    driver <- tolower(input$driver_picker)
    port <- input$sqlport
    
    # require dbname check
    
    ## load driver
    if (driver=="mysql") {
      suppressPackageStartupMessages(library("RMySQL"))
    } else if (driver %in% c("oracle", "postgresql", "redshift", "sql server", "pdw", "bigquery")) {
      suppressPackageStartupMessages(library("DatabaseConnector"))
      suppressPackageStartupMessages(library("SqlRender"))
    }
  
    connection <- checkOMOPconnection(driver, username,password,host,dbname, port)
   
 if(connection==TRUE){ ### if connection can be made
      
      # check OMOP tables
      tbls <- checkOMOPtables(driver, username,password,host,dbname, port)
      missingTables <- tbls$missingTables
      emptyTables <- tbls$emptyTables

      if(length(missingTables)==0){ # if no missing tables
        
        if(length(emptyTables)>0){ # warning if any missing tables
          shinyalert(
            title = "OMOP Database Warning",
            text = paste0("The following tables are empty from the selected database ",dbname, ": \nThe app will still work but data will be missing \n", paste(emptyTables, collapse = "\n")),
            closeOnEsc = TRUE,
            closeOnClickOutside = FALSE,
            html = FALSE,
            type = "warning",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = TRUE
          )
        }
        
    shinyjs::show("intro_help_panel")

  withProgress(message = "Loading data...", min = 0, max = 3, value = 0, { 
    incProgress(1, detail = "Data Ontology...")
   
    if (file.exists(paste0(getOption("currentPath"), "dataOntology.rds")) ) {
      incProgress(1, detail = "Data Ontology found. Loading from .rds file...")
    } else { 
      incProgress(1, detail = "Data Ontology not found. Creating from source...")
      }
    
    dataOntology <<- make_data_ontology()
    
    incProgress(1, detail = "Patient demographics...")
    pts_demographics <<- getDemographics()
  })
   
    logged_in(TRUE)


    shinyjs::enable("logout")
    enable_tabs()

    vs = sort(unique(dataOntology[domain_id == "Condition"]$vocabulary_id))
    cs = sort(unique(dataOntology[domain_id == "Condition" & vocabulary_id == vs[1]]$concept_class_id))
    updatePickerInput(session, inputId = "finder_vocab_picker",choices= vs, selected = vs[1])
    updatePickerInput(session, inputId = "finder_class_picker",choices= cs, selected = cs)
    
    finder_term_table$df = dataOntology[domain_id == "Condition" & vocabulary_id %in% vs[1], c("concept_code","concept_name","domain_id","vocabulary_id","concept_class_id")]  # initialize finder term table
    
      }else{ # any missing tables
        shinyalert(
          title = "OMOP Database Error",
          text = paste0("The following REQUIRED tables are missing from the selected database ",dbname, ": \n", paste(missingTables, collapse = "\n")),
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
        shinyjs::enable("login")
        enable("load_credentials")
        enable("save_credentials")
        enable("sqlid")
        enable("sqlpass")
        enable("sqlhost")
        enable("sqldb")
        enable("driver_picker")
        enable("sqlport")
      }
    
    }else{ # unable to connect
      shinyalert(
        title = "Invalid Credentials",
        text = "Unable to connect to OMOP server with the provided credentials. Please check to make sure they are correct.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      shinyjs::enable("login")
      enable("load_credentials")
      enable("save_credentials")
      enable("sqlid")
      enable("sqlpass")
      enable("sqlhost")
      enable("sqldb")
      enable("driver_picker")
      enable("sqlport")
    } #end if connection
     
    })

  #### LOG OUT

  observeEvent(input$logout, {
    
    shinyjs::hide("criteria_search_open")
    dt_proxy = dataTableProxy("finder_term_picker")
    dt_proxy %>% selectRows(NULL)
    updatePickerInput(session, inputId = "finder_domain_picker",selected = "Condition")
    pts_search_found$pt_list = list()
    pts_search_found$genders = list()
    pts_search_found$races = list()
    pts_search_found$statuses = list()
    pts_search_found$ethnicities = list()
    pts_search_found$ages = list()
    global_cohort_found$df <- data.table(person_id=integer(),year_of_birth=integer(), age = numeric(), Gender = character(), Race = character(), Ethnicity = character(), Status =character())
    
    pt_id_clicked_from_table(NULL)
    pt_id_selected(NULL)
    enc_id_selected(as.character(NULL))
    pt_data_selected$l = list()
    pt_data_report$df = data.table()
    multiplex_timeline$df = data.table()
    
    enable("sqlid")
    enable("sqlpass")
    enable("sqlhost")
    enable("sqldb")
    enable("driver_picker")
    enable("sqlport")
    
    enable("load_credentials")
    enable("save_credentials")
    
    click("reset_filter")
    pt_id_selected(NULL)
    logged_in(FALSE)
    disable_tabs()
    shinyjs::enable("login")
    shinyjs::disable("logout")
    shinyjs::hide("intro_help_panel")


  })
  
  
  ### LOAD CREDENTIALS
  observeEvent(input$load_credentials, {
    # check if .Renviron exists in current directory
    if (file.exists(paste0(getOption("currentPath"),".Renviron"))){
      readRenviron(paste0(getOption("currentPath"), ".Renviron"))  
    }
  
    updateTextInput(session, "sqlid", value=Sys.getenv("username"))
    updateTextInput(session, "sqlpass", value=Sys.getenv("password"))
    updateTextInput(session, "sqlhost", value=Sys.getenv("host"))
    updateTextInput(session, "sqldb", value=Sys.getenv("dbname"))
    updateTextInput(session, "sqlport", value=Sys.getenv("port"))
    
    if("driver" %in% names(Sys.getenv())){
      if (tolower(Sys.getenv("driver"))=="mysql") {
        updatePickerInput(session, "driver_picker", selected= "MySQL")
      } else if (tolower(Sys.getenv("driver"))=="postgresql") {
        updatePickerInput(session, "driver_picker", selected= "PostgreSQL")
      } else if (tolower(Sys.getenv("driver"))=="oracle") {
        updatePickerInput(session, "driver_picker", selected= "Oracle")
      } else if (tolower(Sys.getenv("driver"))=="redshift") {
        updatePickerInput(session, "driver_picker", selected= "Amazon Redshift")
      } else if (tolower(Sys.getenv("driver"))=="sql server") {
        updatePickerInput(session, "driver_picker", selected= "Microsoft SQL Server")
      } else if (tolower(Sys.getenv("driver"))=="pdw") {
        updatePickerInput(session, "driver_picker", selected= "Microsoft Parallel Datawarehouse")
      } else if (tolower(Sys.getenv("driver"))=="bigquery") {
        updatePickerInput(session, "driver_picker", selected= "Google BigQuery")
      }
    }

  })    
    
  ### SAVE CREDENTIALS
  observeEvent(input$save_credentials, {
    #credit: https://github.com/daattali/shinyalert
    shinyalert(
      title = "Save Credentials?",
      text = "Warning: this will create or overwrite the .Renviron file in current directory. Proceed?",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x) { 
        if (x==TRUE) {
          # SAVE CREDENTIALS if OK is selected for Warning shinyalert
          if (input$driver_picker=="MySQL") {
            drv <- "mysql"
          } else if (input$driver_picker == "PostgreSQL") {
            drv <- "postgresql"
          } else if (input$driver_picker == "Oracle") {
            drv <- "oracle"
          } else if (input$driver_picker == "Amazon Redshift") {
            drv <- "redshift"
          } else if (input$driver_picker == "Microsoft SQL Server") {
            drv <- "sql server"
          } else if (input$driver_picker == "Microsoft Parallel Datawarehouse") {
            drv <- "pdw"
          } else if (input$driver_picker == "Google BigQuery") { 
            drv <- "bigquery"
          }  
            write(paste0("driver = '", drv, "'\n",
                         "host = '", input$sqlhost, "'\n",
                         "username = '", input$sqlid, "'\n",
                         "password = '", input$sqlpass, "'\n",
                         "dbname = '", input$sqldb, "'\n",
                         "port = '", input$sqlport, "'\n"),
                  file = paste0(getOption("currentPath"),".Renviron"))
              } 
        
        }
    )
  })

  
  ## shiny-directory-input
  # https://github.com/wleepang/shiny-directory-input
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        
        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'directory'))
        
        # set environemntal variable to path
        options("currentPath" = path)
        
        # update the widget value
        updateDirectoryInput(session, 'directory', value = path)
        
        # check for .Renviron
        if (file.exists(paste0(getOption("currentPath"),".Renviron"))){
          enable("load_credentials")
        }else{
          disable("load_credentials")
        }
        
      }
    }
  )
  
    
  output$intro_help<-renderUI({
    fluidPage(
      icon("question-circle","fa-2x"),"First time user? Check out the ",
      actionLink("gotoHelp2","Help"), "page."
    )
  })
  
  
  ##### HOME BUTTONS
  observeEvent(input$gotoAbout, {
    updateTabsetPanel(session=session,"inTabset",selected = "About")
  })
  
  observeEvent(input$gotoHelp, {
    updateTabsetPanel(session=session,"inTabset",selected = "Help")
  })
  
  observeEvent(input$gotoConfiguration, {
    updateTabsetPanel(session=session,"inTabset",selected = "Configuration")
  })
  
  
  
  observeEvent(input$gotoHelp2, {
    updateTabsetPanel(session=session,"inTabset",selected = "Help")
  })
  
  observeEvent(input$gotoFinder, {
    updateTabsetPanel(session=session,"inTabset",selected = "Patient Finder")
  })
  
  observeEvent(input$gotoReport, {
    updateTabsetPanel(session=session,"inTabset",selected = "Overall Report")
  })
  
  observeEvent(input$gotoInteractive, {
    updateTabsetPanel(session=session,"inTabset",selected = "Encounter Timeline")
  })
  
  observeEvent(input$gotoExplorer, {
    updateTabsetPanel(session=session,"inTabset",selected = "Data Explorer")
  })

  
  ############################
  ########## FINDER ##########
  ############################
  
  observeEvent(input$finder_type,{ 
    
    if(input$finder_type == "criteria_search"){
    shinyjs::hide("pt_search_open")

  if(length(pts_search_found$pt_list)==0){
    shinyjs::show("criteria_search_open")
  }
    shinyjs::show("criteria_search_text_open")
    shinyjs::show("pts_found_open")
    }
    if(input$finder_type == "pt_search"){
      shinyjs::show("pt_search_open")
      shinyjs::hide("criteria_search_open")
      shinyjs::hide("criteria_search_text_open")
      shinyjs::hide("pts_found_open")
    }
    
  })
  
  
  observeEvent(input$pt_search_bar_finder,{
    #enable/disable search button requiring field not empty
    req(logged_in() == TRUE)
    if(input$pt_search_bar_finder != ""){
      enable("pt_search_button_finder")
    }else{
      disable("pt_search_button_finder")
    }
  })
  

  observeEvent(input$pt_search_button_finder, {
    disable("pt_search_button_finder")
    disable("finder_type")
    pt_id = trimws(input$pt_search_bar_finder)

    withProgress(message = "Loading patient data...", min = 0, max = 2, value = 0, {

    incProgress(1, detail = "Searching...")
    pt_found = check_if_pt_exists(pt_id)

    if(pt_found==TRUE){
      pt_id_selected(pt_id) # set reactiveVal pt_id
      incProgress(1, detail = "Patient found; loading data...")

      pt_data_selected$l=get_all_pt_data(pt_id_selected()) #save pt_data globally

      pt_data_report$df = generate_pt_report(pt_data_selected$l) # generate pt_report here
      multiplex_timeline$df <- format_multiplex_timeline(pt_data_report$df) # generate multiplex data from pt_report
      
      showNotification("Patient data loaded.")
      updateTabsetPanel(session=session,"inTabset",selected = "Overall Report")
    }else{
      showNotification("Patient ID not found. ")
    }

    })
    enable("pt_search_button_finder")
    enable("finder_type")
  })
   
  observeEvent(input$finder_domain_picker,{ # update vocab by modality
    req(logged_in() == TRUE)
    
    updatePickerInput(session,inputId ="finder_vocab_picker",choices= sort(unique(dataOntology[domain_id == input$finder_domain_picker]$vocabulary_id)))
    updatePickerInput(session,inputId ="finder_class_picker",choices="")
    
    finder_term_table$df = dataOntology[0, c("concept_code","concept_name","domain_id","vocabulary_id","concept_class_id")]  # initialize finder term table
    
  })

  observeEvent(input$finder_vocab_picker,{ # update terms table by vocabulary
    req(logged_in() == TRUE)
    
    updatePickerInput(session,inputId ="finder_class_picker",choices= sort(unique(dataOntology[domain_id == input$finder_domain_picker & vocabulary_id %in% input$finder_vocab_picker]$concept_class_id)), selected = sort(unique(dataOntology[domain_id == input$finder_domain_picker & vocabulary_id %in% input$finder_vocab_picker]$concept_class_id)))
    
  })

  observeEvent(input$finder_class_picker,{ # update terms table by vocabulary
    req(logged_in() == TRUE)
    
    dat = dataOntology[domain_id == input$finder_domain_picker & vocabulary_id %in% input$finder_vocab_picker & concept_class_id %in% input$finder_class_picker ,c("concept_code","concept_name","domain_id","vocabulary_id","concept_class_id")]
    finder_term_table$df = dat[order(concept_name),]

  })


  # finder term picker table
  output$finder_term_picker <- DT::renderDataTable({
    
    req(!is.null(input$finder_domain_picker) & !is.null(input$finder_vocab_picker) & !is.null(input$finder_class_picker))
    
    finder_table <- finder_term_table$df
    
    DT::datatable(finder_table, options = list(pageLength = 5, autoWidth = FALSE,bLengthChange = FALSE),
                  rownames= FALSE, selection = "single")
  })


  # select all terms for mode/vocab selection
  observeEvent(input$criteria_select_all_button_finder,{
    rows_sel = input$finder_term_picker_rows_all
    toaddstrings<-paste(finder_term_table$df[rows_sel,]$vocabulary_id,finder_term_table$df[rows_sel,]$concept_code,sep=":")
    newaddstrings<- setdiff(toaddstrings, vocab_term$l)
    if(length(newaddstrings>0)){
      
      vocabularies_split <- sapply(strsplit(newaddstrings,":"),'[',1)
      codes_split <- sapply(strsplit(newaddstrings,":"),'[',2)
      
      vocab_term$l = c(vocab_term$l, newaddstrings)
      tmp_vf_tbl = data.table(vocabulary = vocabularies_split, term = codes_split)
      selected_finder_term_table$df = rbind(selected_finder_term_table$df, tmp_vf_tbl)
    }
  })

  # remove all terms for mode/vocab selection
  observeEvent(input$criteria_select_none_button_finder,{
    rows_sel = input$finder_term_picker_rows_all
    toremovestrings<-paste(finder_term_table$df[rows_sel,]$vocabulary_id,finder_term_table$df[rows_sel,]$concept_code,sep=":")
    newremovestrings<- intersect(toremovestrings, vocab_term$l)
    if(length(newremovestrings>0)){

      vocabularies_split <- sapply(strsplit(newremovestrings,":"),'[',1)
      codes_split <- sapply(strsplit(newremovestrings,":"),'[',2)

      vocab_term$l = setdiff(vocab_term$l, newremovestrings)
      tmp_vf_tbl = data.table(vocabulary = vocabularies_split, term = codes_split)
      selected_finder_term_table$df = selected_finder_term_table$df[!tmp_vf_tbl,on=c("vocabulary","term")]
    }
    
  })



  sel <- reactive({!is.null(input$finder_term_picker_rows_selected)})

  observeEvent(input$finder_term_picker_cell_clicked,{

    if(length(input$finder_term_picker_cell_clicked)>0){
      toaddstring = paste(finder_term_table$df[input$finder_term_picker_cell_clicked$row,"vocabulary_id"],finder_term_table$df[input$finder_term_picker_cell_clicked$row,"concept_code"],sep=":")
       if(!toaddstring %in% vocab_term$l){
          vocab_term$l = c(vocab_term$l, toaddstring)
          tmp_vf_tbl = data.table(vocabulary = finder_term_table$df[input$finder_term_picker_cell_clicked$row,]$vocabulary_id, term = finder_term_table$df[input$finder_term_picker_cell_clicked$row,]$concept_code)
          selected_finder_term_table$df = rbind(selected_finder_term_table$df, tmp_vf_tbl)

        }
    }      

  })
  

  sel_selected <- reactive({!is.null(input$vocab_term_selected_table_rows_selected)})
  
  observe({

      if(sel_selected() == TRUE){
        enable("criteria_remove_button")
      }else{
        disable("criteria_remove_button")
      }
    
  })
  

 output$selected_criteria_options<-renderUI({
    search_button = actionButton(inputId = "criteria_search_button_finder",
                     label = strong("Search by Criteria"),width = 200,height = 150)

    if(length(vocab_term$l)==0){
      search_button = disabled(search_button)
    }


   fluidPage(
     hr(),
     tags$h4("Selected Criteria: "),
      fluidRow(
        column(width = 5,
               DT::dataTableOutput("vocab_term_selected_table")),

        column(width = 2,align="center",
               fluidRow(
                      disabled(actionButton(inputId = "criteria_remove_button",
                                   label = "Remove Item",width = 150,height = 150))),
               br(),
               fluidRow(
                            actionButton(inputId = "criteria_search_button_reset",
                                         label = "Reset Search",width = 150,height = 150)
               )
               ),
        column(width = 2,align="center",
               fluidRow(
                 radioButtons("search_func_type", "Search Type:",
                              c("or" = "or",
                                "and" = "and"),inline= TRUE)
               ),
               fluidRow(
                 radioButtons("search_strategy", "Search Strategy:",
                              c("Mapped" = "mapped",
                                "Direct" = "direct"),inline= TRUE)
               )
        ),
     column(width = 3,align="center",
     br(),
     fluidRow(icon("search", "fa-3x") ),
     fluidRow(
       search_button
     )
     ) 

)

)

 })
   
   output$vocab_term_selected_table <- DT::renderDataTable({

     req(nrow(selected_finder_term_table$df)>0)
    
     finder_table <- selected_finder_term_table$df

     DT::datatable(finder_table, options = list(pageLength = 5, autoWidth = FALSE,bLengthChange = FALSE, searching = FALSE),
                   rownames= FALSE, selection = "single")
   })
   

   observeEvent(input$criteria_remove_button,{ # remove term from selected table

    
     if(length(input$vocab_term_selected_table_cell_clicked)>0){
       toremovestring = paste(selected_finder_term_table$df[input$vocab_term_selected_table_cell_clicked$row,"vocabulary"],selected_finder_term_table$df[input$vocab_term_selected_table_cell_clicked$row,"term"],sep=":")
         vocab_term$l = setdiff(vocab_term$l, toremovestring)
       selected_finder_term_table$df=selected_finder_term_table$df[-input$vocab_term_selected_table_cell_clicked$row,]

     }
     
   })
   
  observeEvent(input$criteria_search_button_finder,{
  req(logged_in() == TRUE)
    selected_terms = vocab_term$l
    func_type = input$search_func_type
    search_strat = input$search_strategy

     disable("finder_type")
     disable("criteria_search_button_finder")
     disable("criteria_search_button_reset")
     disable("finder_domain_picker")
     disable("finder_vocab_picker")
     disable("finder_term_picker")
     disable("criteria_select_all_button_finder")
     disable("criteria_select_none_button_finder")
     disable("search_func_type")
     disable("search_strategy")
     disable("show_plots")
     patient_list <- findPatients(selected_terms, func_type, search_strat)

     if(length(patient_list)>0){

     patient_list = as.character(unique(patient_list))
     pts_search_found$pt_list = patient_list
     pts_search_found$genders <- unique(pts_demographics[person_id %in% patient_list]$Gender)
     pts_search_found$races <- unique(pts_demographics[person_id %in% patient_list]$Race) #### <- single races here
     pts_search_found$statuses <- unique(pts_demographics[person_id %in% patient_list]$Status)
     pts_search_found$ethnicities <- unique(pts_demographics[person_id %in% patient_list]$Ethnicity)
     pts_search_found$ages <- c(min(pts_demographics[person_id %in% patient_list]$age),max(pts_demographics[person_id %in% patient_list]$age))
     
   shinyjs::hide("criteria_search_open")
   shinyjs::show("pts_found_open")
     }else{
       showNotification("No patients found for criteria used.")
     }
     
     
   enable("finder_type")
   enable("criteria_search_button_finder")
   enable("criteria_search_button_reset")
   enable("finder_domain_picker")
   enable("finder_vocab_picker")
   enable("finder_term_picker")
   enable("criteria_select_all_button_finder")
   enable("criteria_select_none_button_finder")
   enable("search_func_type")
   enable("search_strategy")
   enable("show_plots")


  })



  observeEvent(input$criteria_search_button_reset,{
    req(logged_in() == TRUE)
    
    disable("search_patient_button2")
    pt_id_clicked_from_table(NULL)
    output$pt_id_table_selected <- renderText({""})
    
    updateActionButton(session, "show_plots", label = "Hide Plots")
    shinyjs::hide("finder_plots")
    shinyjs::hide("pts_found_open")
    shinyjs::show("criteria_search_open") 

   
    vocab_term$l <- list()
  pts_search_found <-reactiveValues(pt_list = list(),genders=list(),races=list(),statuses=list(),ethnicities=list(),ages=list())
  selected_finder_term_table$df <- data.table(vocabulary = character(),term = character())
  global_cohort_found$df <- data.table(person_id=integer(),year_of_birth=integer(), age = numeric(), Gender = character(), Race = character(), Ethnicity = character(), Status =character())
  
    dt_proxy = dataTableProxy("finder_term_picker")
    dt_proxy %>% selectRows(NULL)
    clearSearch(dt_proxy)

    updatePickerInput(session, inputId = "finder_domain_picker",selected = "Condition")

  })

   
  output$pts_found_display<-renderUI({ ### change renderUI to cohort_found ranges
    req(length(pts_search_found$pt_list)>0)

    single_races = unique(pts_demographics[person_id %in% pts_search_found$pt_list]$Race)
    single_races = single_races[!grepl(",",single_races)]

    fluidPage(
      hr(),
      fluidRow(
        column(3,
               downloadButton(
                 outputId = "save_cohort",
                 label = "Export Cohort"
               )
        ),
        column(3,
               actionButton("show_plots", "Show Plots")
        )
      ),
      hr(),
      
      shinyjs::hidden(
        div(id="finder_plots",
      fluidRow(
        plotlyOutput("cohort_plots") %>% withSpinner()
        # credit: https://github.com/andrewsali/shinycssloaders
      )
        )),
      fluidRow(
       DT::dataTableOutput("found_cohort_table")
      )
    )

  })
  


  observeEvent(input$show_plots,{
    toggle("finder_plots")
    if (input$show_plots %% 2 == 1) {
      updateActionButton(session, "show_plots", label = "Hide Plots")
    } else {
      updateActionButton(session, "show_plots", label = "Show Plots")
    }

  })

  
  disable_during_search <-function(){
    disable("finder_type")
    disable("criteria_search_button_finder")
    disable("criteria_search_button_reset")
    disable("save_cohort")
    disable("show_plots")
  }

enable_after_search <-function(){
  enable("finder_type")
  enable("criteria_search_button_finder")
  enable("criteria_search_button_reset")
  enable("save_cohort")
  enable("show_plots")
 }

  
  
  output$cohort_plots <- renderPlotly({
    cohort_found = global_cohort_found$df[input$found_cohort_table_rows_all]
    
    req(nrow(cohort_found)>0)

    ageTitle <- list(
      text = "Age",
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )


    p0 <- plot_ly(cohort_found,x=~age, type = 'histogram') %>% layout(xaxis = list(dtick = 10),showlegend=FALSE, annotations = ageTitle)

    genderTitle <- list(
      text = "Gender",
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )


    genderData <- data.table(cohort_found %>%  dplyr::count(Gender) %>%  mutate(prop = prop.table(n)))

    p1 <- plot_ly(genderData,x=~Gender, y = ~prop, type = 'bar', color = ~Gender, text = ~n) %>%
      layout(xaxis = list(title="",tickangle = 90), showlegend=FALSE, annotations = genderTitle)


    cohort_race = cohort_found[,c("person_id","Race")]

    raceTitle <- list(
      text = "Race",
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )

    raceData <- data.table(cohort_race %>%  dplyr::count(Race) %>%  mutate(prop = prop.table(n)))


    p2 <- plot_ly(raceData, x=~Race, y = ~prop, type = 'bar', color = ~Race, text = ~n) %>%
     layout(xaxis = list(title="",tickangle = 90), showlegend=FALSE, annotations = raceTitle)


    ethnicityTitle <- list(
      text = "Ethnicity",
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )



    ethnicityData <- data.table(cohort_found %>%  dplyr::count(Ethnicity) %>%  mutate(prop = prop.table(n)))

    p3 <- plot_ly(ethnicityData, x=~Ethnicity, y = ~prop, type = 'bar', color = ~Ethnicity, text = ~n)  %>%
      layout(xaxis = list(title="",tickangle = 90), showlegend=FALSE, annotations = ethnicityTitle)

    statusData <- data.table(cohort_found %>%  dplyr::count(Status) %>%  mutate(prop = prop.table(n)))

    statusTitle <- list(
      text = "Status",
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )


    p4 <- plot_ly(statusData, x=~Status, y = ~prop, type = 'bar', color = ~Status, text = ~n) %>%
      layout(xaxis = list(title="",tickangle = 90), showlegend=FALSE, annotations = statusTitle)

    subplot(p0,p1, p2,p3, p4, nrows = 1, margin = 0.02, widths = c(0.3,0.15, 0.3, 0.15, 0.1)) %>% hide_legend() %>% layout(margin = list(b = 200,l=20))

  })


  shinyInput <- function(FUN, len, id, ...) { 
    # credit: https://stackoverflow.com/questions/45739303/r-shiny-handle-action-buttons-in-data-table
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }  


  output$found_cohort_table <- DT::renderDataTable({
    req(length(pts_search_found$pt_list)>0 & logged_in()==TRUE)
    
    pt_list_found = pts_search_found$pt_list

    cohort_found = pts_demographics[person_id %in% pt_list_found]
    cohort_found = cohort_found[,-c("death_date")]

    cohort_found$age = as.integer(cohort_found$age)
    cohort_found$year_of_birth = as.integer(cohort_found$year_of_birth)
    cohort_found$Gender = as.factor(cohort_found$Gender)
    cohort_found$Race = as.factor(cohort_found$Race)
    cohort_found$Ethnicity = as.factor(cohort_found$Ethnicity)
    cohort_found$Status = as.factor(cohort_found$Status)
    
    global_cohort_found$df = cohort_found
    
    
    cohort_found <- cohort_found %>% mutate(Search = shinyInput(actionButton, nrow(cohort_found), 'button_', label = NULL,icon = icon("search", lib = "glyphicon"), onclick = 'Shiny.onInputChange(\"select_pt_button\",  this.id)' ))
    
    datatable(cohort_found,
              filter = "top",
              rownames = FALSE,
              selection = 'single',
              style = "bootstrap",
              escape = FALSE,
              options = list(
                columnDefs = list(list(targets=7, searchable = FALSE))
              )
        )
  })

  
  observeEvent(input$select_pt_button, {
    selectedRow <- as.numeric(strsplit(input$select_pt_button, "_")[[1]][2])
    disable_during_search()
    
    withProgress(message = "Loading patient data...", min = 0, max = 1, value = 0, {
      
      incProgress(1, detail = "Loading data...")
      pt_id_selected(global_cohort_found$df[selectedRow]$person_id) # set reactiveVal pt_id #### <- !! issue here can't use global_chorot_found
      
      pt_data_selected$l=get_all_pt_data(pt_id_selected()) #save pt_data globally
      pt_data_report$df = generate_pt_report(pt_data_selected$l) # generate pt_report here
      
      ### functionalize this
      shinyjs::hide("login_message_explore", anim = FALSE)
      shinyjs::show("explore_info_open", anim = FALSE)
      ######
      
      multiplex_timeline$df <- format_multiplex_timeline(pt_data_report$df) # generate multiplex data from pt_report
      showNotification("Patient data loaded.")
      updateTabsetPanel(session=session,"inTabset",selected = "Overall Report")
    })
    
    enable_after_search()
    
  })
  
  output$save_cohort <- downloadHandler(

    filename = function() {
      "saved_cohort.csv"
    },
    content = function(file) {
      write.csv(global_cohort_found$df[input$found_cohort_table_rows_all], file, row.names = FALSE,quote=FALSE)
    }
  )

  
  ############################
  ########## REPORT ##########
  ############################
  
  output$overall_title <- renderText({
    req(logged_in()==TRUE)
    
    if(!is.null(pt_id_selected())){
      HTML(paste0(h4("Overall Report:  ",pt_id_selected())))
    }else{
      HTML(paste0(h4("Select patient from Finder ","")))
    }
  })
 
  
  
  # Patient Information subheader 
  output$report_pt_info <- renderUI({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    pt_id = pt_id_selected()
    
    # get and filter background
    pt_background = pts_demographics[person_id == pt_id]
    background_info = generate_pt_background(pt_background)
    
    # get and filter pt data
    pt_data = pt_data_selected$l
    
    pt_summary <- generate_pt_summary(pt_data)

    pt_summary1=pt_summary[[1]]
    pt_summary2=pt_summary[[2]]
    
    fluidPage(
      
      fluidRow(
        column(4,align="left",
               tags$h5("Background: "),
               HTML(paste(background_info,"",sep="<br/>"))
        ),
        column(4, align = "left",
               tags$h5("Clinical Summary: "),
               HTML( paste(pt_summary1,"",sep="<br/>"))
        ),
        column(4, align = "left",
               tags$h5(""),
               HTML( paste(pt_summary2,"",sep="<br/>"))
        )
      ))
    
  })  
  
  
  # filter original pt_report 
  filtered_pt_report <- reactive({
    pt_report = pt_data_report$df
    
    pt_report=pt_report[Type %in% input$ReportPicker] # first filter by 
    pt_report=pt_report[!Event %in% setdiff(unique(pt_report[Type == "Observation",]$Event), input$ReportObservationPicker)] # do not include 'unselected' events
    pt_report=pt_report[!Event %in% setdiff(unique(pt_report[Type == "Condition",]$Event), input$ReportConditionPicker)] # do not include 'unselected' events
    pt_report=pt_report[!Event %in% setdiff(unique(pt_report[Type == "Procedure",]$Event), input$ReportProcedurePicker)] # do not include 'unselected' events
    pt_report=pt_report[!Event %in% setdiff(unique(pt_report[Type == "Medication",]$Event), input$ReportMedicationPicker)] # do not include 'unselected' events
    pt_report=pt_report[!Event %in% setdiff(unique(pt_report[Type == "Measurement",]$Event), input$ReportMeasurementPicker)] # do not include 'unselected' events
    pt_report=pt_report[!Event %in% setdiff(unique(pt_report[Type == "Device",]$Event), input$ReportDevicePicker)] # do not include 'unselected' events
  })
  

  observeEvent(input$ReportPicker,{
    
    req(length(pt_data_report$df)>0)

    if(!"Observation" %in% input$ReportPicker){shinyjs::disable("ReportObservationPicker")}else{shinyjs::enable("ReportObservationPicker")}
    if(!"Condition" %in% input$ReportPicker){shinyjs::disable("ReportConditionPicker")}else{shinyjs::enable("ReportConditionPicker")}
    if(!"Procedure" %in% input$ReportPicker){shinyjs::disable("ReportProcedurePicker")}else{shinyjs::enable("ReportProcedurePicker")}
    if(!"Medication" %in% input$ReportPicker){shinyjs::disable("ReportMedicationPicker")}else{shinyjs::enable("ReportMedicationPicker")}
    if(!"Measurement" %in% input$ReportPicker){shinyjs::disable("ReportMeasurementPicker")}else{shinyjs::enable("ReportMeasurementPicker")}
    if(!"Device" %in% input$ReportPicker){shinyjs::disable("ReportDevicePicker")}else{shinyjs::enable("ReportDevicePicker")}
  

  })

  

  
  ### patient report UI
  output$report_pt_filter<- renderUI({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    # retrieve patient report from global variable
    pt_report=pt_data_report$df
    
    fluidPage(
      
      fluidRow( 
        
        column(width=4,
               fluidRow(
               tags$h5("Select data modalities to include:"),
               pickerInput(
                 inputId = "ReportPicker", 
                 label = "Data Modalities", 
                 choices = c("Observation","Condition","Procedure","Medication","Measurement","Device"),
                 selected = unique(pt_report$Type), # intialize all selected
                 options = list(
                   `actions-box` = TRUE, 
                   size = 25,
                   `selected-text-format` = "count > 1"
                 ), 
                 multiple = TRUE
               ) 
               ),
               fluidRow(align= "center",
                 #export button
                 downloadButton(
                   outputId = "export_report",
                   label = "Export Report"
                 )
               )
        ),
        column(width=4,
               fluidRow(  
                 pickerInput( # Observation picker
                 inputId = "ReportObservationPicker", 
                 label = "Observations", 
                 choices = sort(unique(pt_report[Type == "Observation",]$Event)),
                 selected =sort(unique(pt_report[Type == "Observation",]$Event)),
                 options = list(
                   `actions-box` = TRUE, 
                   size = 25,
                   `selected-text-format` = "count > 1"
                 ), 
                 multiple = TRUE
               ) ),
               fluidRow(
                 pickerInput( # Condition picker
                   inputId = "ReportConditionPicker", 
                   label = "Conditions", 
                   choices = sort(unique(pt_report[Type == "Condition",]$Event)),
                   selected =sort(unique(pt_report[Type == "Condition",]$Event)),
                   options = list(
                     `actions-box` = TRUE, 
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ), 
                   multiple = TRUE
                 ) 
               ),
               fluidRow(
                 pickerInput( # Procedure picker
                   inputId = "ReportProcedurePicker", 
                   label = "Procedures", 
                   choices = sort(unique(pt_report[Type == "Procedure",]$Event)),
                   selected =sort(unique(pt_report[Type == "Procedure",]$Event)),
                   options = list(
                     `actions-box` = TRUE, 
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ), 
                   multiple = TRUE
                 ) 
               )
               ),
        column(width = 4, 
               fluidRow(  
                 pickerInput( # Medication picker
                   inputId = "ReportMedicationPicker", 
                   label = "Medications", 
                   choices = sort(unique(pt_report[Type == "Medication",]$Event)),
                   selected =sort(unique(pt_report[Type == "Medication",]$Event)),
                   options = list(
                     `actions-box` = TRUE, 
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ), 
                   multiple = TRUE
                 ) ),
               fluidRow(
                 pickerInput( # Measurement picker
                   inputId = "ReportMeasurementPicker", 
                   label = "Measurements", 
                   choices = sort(unique(pt_report[Type == "Measurement",]$Event)),
                   selected =sort(unique(pt_report[Type == "Measurement",]$Event)),
                   options = list(
                     `actions-box` = TRUE, 
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ), 
                   multiple = TRUE
                 ) 
               ),
               fluidRow(
                 pickerInput( # Device picker
                   inputId = "ReportDevicePicker", 
                   label = "Devices", 
                   choices = sort(unique(pt_report[Type == "Device",]$Event)),
                   selected =sort(unique(pt_report[Type == "Device",]$Event)),
                   options = list(
                     `actions-box` = TRUE, 
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ), 
                   multiple = TRUE
                 ) 
               )
        )
    
      ) # end fluidRow
    ) #end fluidPage
    
    })
  

  
    
  # Report table
  output$report_table <- DT::renderDataTable({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
      
      # retrieve patient report from global variable (filtered report)
      pt_report = filtered_pt_report()
      pt_report = pt_report[order(pt_report$Date),]
      pt_report = pt_report[,c("Date","Type","Event","Value")]
      pt_report$Type = as.character(pt_report$Type)
      pt_report$Event = as.character(pt_report$Event)
      pt_report$Value = as.character(pt_report$Value)

      shiny::validate(need(!is.null(pt_report), message = FALSE))
      datatable(pt_report,
                filter = "top",
                rownames = FALSE,
                selection = 'single',
                style = "bootstrap",
                options = list(
                  columnDefs = list(list(targets= c(1,2,3), searchable = FALSE)),
                  pageLength = 10
                )) 

    
      })
  

  output$export_report <- downloadHandler(

    filename = function() {
      paste(pt_id_selected(), "_report.csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_pt_report()[input$report_table_rows_all], file, row.names = FALSE,quote=FALSE)
    }
  )

  
  ############################
  ######### TIMELINE #########
  ############################
  
  
  # Timeline info and filter subheader 
  output$timeline_filter_options <- renderUI({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()))

    encounters = pt_data_selected$l$Encounters

     fluidPage(
       fluidRow( 
        column(width = 2,offset = 0, style='padding:0px;',

               radioButtons("encounter_plot_type", "Plot Encounters:",
                            choices =  c("None" = "no_enc_selected",
                                         "Visit Types" = "visit_types_selected",
                                         "Admitting Concepts" = "admitting_concepts_selected",
                                         "Discharge Concepts" = "discharge_concepts_selected"
                                         ))
        ),
        column(width = 9,offset = 0, style='padding:0px;',
          uiOutput("pt_encounter_data_panel")
        )
      ),
      fluidRow( # picker input row
        hr(),
          column(width =4,
           pickerInput( # Visit Type
             inputId = "VisitTypePicker",
             label = "Visit Types",
             choices = sort(unique(encounters$visit_concept)),
             selected = NULL,
             options = list(
               `actions-box` = TRUE,
               size = 25,
               `selected-text-format` = "count > 1",
               dropupAuto = FALSE,
               up = FALSE
             ),
             multiple = TRUE
           )
        ),
        column(width =4,
           pickerInput( # Admitting Type
             inputId = "AdmittingTypePicker",
             label = "Admitting Concept Type",
             choices = sort(unique(encounters$admitting_concept)),
             selected =sort(unique(encounters$admitting_concept)),
             options = list(
               `actions-box` = TRUE,
               size = 25,
               `selected-text-format` = "count > 1",
               dropupAuto = FALSE,
               up = FALSE
             ),
             multiple = TRUE
           )
    ),
      column(width =4,
         pickerInput( # Discharge Type
           inputId = "DischargeTypePicker",
           label = "Discharge Concept Type",
           choices = sort(unique(encounters$discharge_concept)),
           selected =sort(unique(encounters$discharge_concept)),
           options = list(
             `actions-box` = TRUE,
             size = 25,
             `selected-text-format` = "count > 1",
             dropupAuto = FALSE,
             up = FALSE
           ),
           multiple = TRUE
         )
    )
    ),# end picker input row
      fluidRow(column(width = 12,
        # credit: https://github.com/daattali/timevis              
        timevisOutput("encounter_timeline")
         )
      ),
      fluidRow(
                 # Timevis Buttons
                 div(id = "interactiveActions",
                     class = "optionsSection",
                     actionButton("fitAllEncounters", "Fit All Encounters"),
                     actionButton("focusPastYear", "Focus Past Year")
                 )

      ),
    fluidRow(
      hr()
    )
     )# end fluidPage
  })
  

  encounter_timeline_data <- reactive({
    
    encounters = pt_data_selected$l$Encounters

    encounters=encounters[!is.na(visit_start_date)] # remove NA dates

    encounters=encounters[visit_concept %in% input$VisitTypePicker]
    encounters=encounters[admitting_concept %in% input$AdmittingTypePicker]
    encounters=encounters[discharge_concept %in% input$DischargeTypePicker]

    encounters

  })


  
  output$encounter_timeline<- renderTimevis({
  # credit: https://github.com/daattali/timevis 
   encounters_timeline = encounter_timeline_data()

    req(nrow(encounters_timeline)>0)

    timevis_encounters <- data.table(id = 1:nrow(encounters_timeline),
                                     content = encounters_timeline$visit_concept,
                                     start = encounters_timeline$visit_start_date,
                                     end = rep(NA,nrow(encounters_timeline)))

    config <- list(
      editable = FALSE,
      multiselect = TRUE
    )


    timevis(timevis_encounters, options = config)

  })

  
#### Timevis buttons
  observeEvent(input$fitAllEncounters, {
    fitWindow("encounter_timeline")
  })

  observeEvent(input$focusPastYear, {
    current_date = as.Date(Sys.time())
    setWindow("encounter_timeline", current_date-365, current_date)
  })

  
### Timevis info
output$timeline_title <- renderText({
    req(logged_in()==TRUE)
    
    if(!is.null(pt_id_selected())){
      HTML(paste0(h4("Encounters Timeline:  ",pt_id_selected())))
    }else{
      HTML(paste0(h4("Select patient from Finder ","")))
    }
    
  })

  
  #### clicking on encounter  
  observeEvent(input$encounter_timeline_selected,{
   req(logged_in()==TRUE)
    tl_id <- input$encounter_timeline_selected
    timevis_encounters = encounter_timeline_data()

	  tl_id = as.integer(tl_id)

    timevis_encounters$id = 1:nrow(timevis_encounters)
  
    enc_id_selected(timevis_encounters[id == tl_id,]$visit_occurrence_id) #update selected encounter id
    req(!is.null(enc_id_selected()))

    shinyjs::show("encounter_selected_info_panel")


    #### Modality-specific tables
    
    output$encounter_conditions_table <- DT::renderDataTable({
      conditions <- pt_data_selected$l$Conditions

      conditions <- conditions[visit_occurrence_id == enc_id_selected()]

      conditions = conditions[,c("condition_concept_name","condition_type","condition_status_type","condition_concept_vocabulary","condition_concept_code","condition_source_vocabulary","condition_source_code","condition_start_date","condition_end_date")]

      datatable(conditions,
                extensions = 'Buttons',
                rownames = FALSE,
                style = "bootstrap",
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('csv', 'excel')
                ))
    })

    output$encounter_devices_table <- DT::renderDataTable({
      devices <- pt_data_selected$l$Devices

      devices <- devices[visit_occurrence_id == enc_id_selected()]

      devices = devices[,c("device_concept_name","device_type","device_exposure_start_date","device_exposure_end_date","device_concept_vocabulary","device_concept_code","device_source_vocabulary","device_source_code")]

      datatable(devices,
                extensions = 'Buttons',
                rownames = FALSE,
                style = "bootstrap",
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('csv', 'excel')
                ))
    })

    output$encounter_measurements_table <- DT::renderDataTable({

      req(logged_in()==TRUE & !is.null(pt_id_selected()))
      measurements <- pt_data_selected$l$Measurements

      measurements <- measurements[visit_occurrence_id == enc_id_selected()]

      measurements = measurements[,c("measurement_concept_name","value_as_number","value_concept", "unit_concept", "measurement_type", "measurement_date","measurement_concept_vocabulary","measurement_concept_code","measurement_source_vocabulary","measurement_source_code")]

      datatable(measurements,
                extensions = 'Buttons',
                rownames = FALSE,
                style = "bootstrap",
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('csv', 'excel')
                ))
    })
    
        output$encounter_medications_table <- DT::renderDataTable({
          medications <- pt_data_selected$l$Medications

          medications <- medications[visit_occurrence_id == enc_id_selected()]

          medications = medications[,c("medication_concept_name","refills","quantity","days_supply","drug_type","route_concept","drug_exposure_start_date","drug_exposure_end_date","medication_concept_vocabulary","medication_concept_code","medication_source_vocabulary","medication_source_code")]

          datatable(medications,
                    extensions = 'Buttons',
                    rownames = FALSE,
                    style = "bootstrap",
                    options = list(
                      dom = 'Bfrtip',
                      buttons = c('csv', 'excel')
                    ))
        })
            output$encounter_observations_table <- DT::renderDataTable({
              observations <- pt_data_selected$l$Observations

              observations <- observations[visit_occurrence_id == enc_id_selected()]

              observations = observations[,c("observation_concept_name","value_as_number","value_as_string","value_concept","unit_source_value","observation_type","observation_concept_vocabulary","observation_concept_code","observation_source_vocabulary","observation_source_code")]

              datatable(observations,
                        extensions = 'Buttons',
                        rownames = FALSE,
                        style = "bootstrap",
                        options = list(
                          dom = 'Bfrtip',
                          buttons = c('csv', 'excel')
                        ))
            })

    output$encounter_procedures_table <- DT::renderDataTable({
      procedures <- pt_data_selected$l$Procedures

      procedures <- procedures[visit_occurrence_id == enc_id_selected()]

      procedures = procedures[,c("procedure_concept_name","procedure_type","procedure_date","procedure_concept_vocabulary","procedure_concept_code","procedure_source_vocabulary","procedure_source_code")]

      datatable(procedures,
                extensions = 'Buttons',
                rownames = FALSE,
                style = "bootstrap",
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('csv', 'excel')
                ))
          })

   })
   
  ######### End modality-specific tables

  output$pt_encounter_data_panel <- renderUI({
   # plot encounter/visit type information (UI for page)
    req(logged_in()==TRUE & !is.null(pt_id_selected()))

    if(input$encounter_plot_type != "no_enc_selected"){
       plotlyOutput("plotly_pt_encounter_data") %>% withSpinner()
    }

  })


  output$encounter_info_text <- renderUI({
    # text information regarding selected encounter
    req(logged_in()==TRUE & !is.null(pt_id_selected()) & !is.null(enc_id_selected()))

    encounters <- pt_data_selected$l$Encounters
    encounters <- encounters[visit_occurrence_id == enc_id_selected()]

    fluidPage(
      fluidRow(tags$h4("Encounter Information: ")),
      fluidRow(
        column(5,
               HTML(paste0("<strong>Visit Date:</strong>  ",encounters$visit_start_date)),
               tags$br(),
               HTML(paste0("<strong>Visit Type:</strong>  ",encounters$visit_concept)),
               tags$br(),
               HTML(paste0("<strong>Visit Admitting Type:</strong>  ",encounters$admitting_concept)),
               tags$br(),
               HTML(paste0("<strong>Visit Discharge Type:</strong>  ",encounters$discharge_concept))
               )
      ),
      tags$br()
      ) #end FluidPage

  })


   output$plotly_pt_encounter_data <- renderPlotly({ # encounters type bar plot
     encounters = pt_data_selected$l$Encounters
     if(input$encounter_plot_type != "no_enc_selected"){
     
          if(input$encounter_plot_type == "visit_types_selected"){
           encounters = encounters[,c("visit_occurrence_id","visit_start_date","visit_concept")]
           encounters = encounters[!duplicated(encounters),]
           dat = data.table(table(encounters$visit_concept))
           plot_title = "Visit Types"
         }
        else if(input$encounter_plot_type == "admitting_concepts_selected"){
          encounters = encounters[,c("visit_occurrence_id","visit_start_date","admitting_concept")]
          encounters = encounters[!duplicated(encounters),]
          dat = data.table(table(encounters$admitting_concept))
          plot_title = "Admitting Concept Types"
           }
       else if(input$encounter_plot_type == "discharge_concepts_selected"){
         encounters = encounters[,c("visit_occurrence_id","visit_start_date","discharge_concept")]
         encounters = encounters[!duplicated(encounters),]
         dat = data.table(table(encounters$discharge_concept))
         plot_title = "Discharge Concept Types"
       }
     
         dat = dat[order(N,decreasing = TRUE)]

     
         xform = list(categoryorder = "array",
                      categoryarray = dat$V1)

        plot_ly(dat,
                x = ~V1,
                y = ~N,
                name = "plot_title",
                type = "bar"
        ) %>% layout(xaxis = xform) %>%
          layout(xaxis = list(title="",tickangle = 90, tickfont = list(size = 8)),
                 margin = list(b = 150, l = 50, r = 20, t= 50))

    }else{
      plotly_empty(type = "scatter",mode="lines+markers")
    }

  })

  
  
  ############################
  ####### DATA EXPLORER ######
  ############################
  
  output$explorer_title <- renderText({
    # header text for explorer
    req(logged_in()==TRUE)
    
    if(!is.null(pt_id_selected())){
      HTML(paste0(h4("Data Explorer:  ",pt_id_selected())))
    }else{
      HTML(paste0(h4("Select patient from Finder ","")))
    }
    
  })
  

  output$explorer_description <- renderUI({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()) & !is.null(input$explorer_type))
  
    if(input$explorer_type == "explorer_targeted"){
      "Explore all clinical events over the patient's history. The user can explore both categorical (Conditions, Medications, Procedures, or Devices) or numeric (Measurement or Observation) data. For categorical data, the events are visualized in an interactive timeline and the user can select which events to show. Further, diseases may be explored at different levels (Disease Name, ICD 9 or 10). For numeric data types, the events (e.g., WBC for Labs) are displayed as a table with # of measurements recorded. The user can select an event of interest which will display as an interactive timeseries plot."
    } else if(input$explorer_type == "explorer_multiplex"){
    "Explore patient's clinical variables over time on the same scale. Can dynamically add variables to assess, including both numeric values and categorical events."
    } else if(input$explorer_type == "explorer_multiplex_timeline"){
      "Explore all types of a patient's clinical variables over time in an interactive time visualization plot grouped by modality."
    }
  
  })
  
  
  output$explorer_radio_buttons <-renderUI({

    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    radioButtons("explorer_type", "Data Explorer Mode:",
                 choices =  c("Targeted" = "explorer_targeted",
                              "Multiplex" = "explorer_multiplex",
                              "Multiplex Timeline" = "explorer_multiplex_timeline"))
    
  })
                                            
  
  output$explorer_data <- renderUI({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()) & !is.null(input$explorer_type))
    
    if(input$explorer_type == "explorer_targeted"){
      
      fluidPage(
        fluidRow(
        
          tabsetPanel(id = 'dataset_type_targeted_explorer',
                    tabPanel("Conditions",
                             tags$br(),
                             uiOutput("conditions_explorer_targeted_panel")),
                    tabPanel("Devices",
                             tags$br(),
                             uiOutput("devices_explorer_targeted_panel")),
                    tabPanel("Measurements",
                             tags$br(),
                             DT::dataTableOutput("measurements_explorer_table")),
                    tabPanel("Medications",
                             tags$br(),
                             uiOutput("medications_explorer_targeted_panel")),
                    tabPanel("Procedures",
                             tags$br(),
                             uiOutput("procedures_explorer_targeted_panel")),
                    tabPanel("Observations",
                             tags$br(),
                             DT::dataTableOutput("observations_explorer_table")) 
                ) # end tabset Panel
        
      ), #end fluidRow
      
      hr(),
      
      uiOutput("timevispanel")
      
      ) # end fluidPage
      
      
    }else if(input$explorer_type == "explorer_multiplex"){
      ## prefilter choices
      
      # filter categorical here removing NA dates
      conditions <- pt_data_selected$l$Conditions
      conditions <- conditions[!is.na(condition_start_date),]

      medications <- pt_data_selected$l$Medications
      medications <- medications[!is.na(drug_exposure_start_date),]

      procedures <- pt_data_selected$l$Procedures
      procedures <- procedures[!is.na(procedure_date),]

      # filter numeric here removing NA dates & non-numeric results
      measurements <- pt_data_selected$l$Measurements
      measurements <- measurements[(!is.na(measurement_date) & !is.na(as.numeric(value_as_number))),]

      devices <- pt_data_selected$l$Devices
      devices <- devices[!is.na(device_exposure_start_date),]

      observations <- pt_data_selected$l$Observations
      observations <- observations[(!is.na(observation_date)),]
      observations <- observations[!(is.na(value_as_number)&is.na(value_as_string))]

      fluidPage(
        fluidRow(
          column(width=5,
                 tags$h4("Categorical",align="center"),
                 pickerInput(
                   inputId = "conditions_explorer_multiplex",
                   label = "Conditions",
                   choices = sort(unique(conditions$condition_concept_name)),
                   selected = c(""),
                   options = list(
                     `actions-box` = TRUE,
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ),
                   multiple = TRUE
                 ),

                 pickerInput(
                   inputId = "medications_explorer_multiplex",
                   label = "Medications",
                   choices = sort(unique(medications$medication_concept_name)),
                   selected = c(""),
                   options = list(
                     `actions-box` = TRUE,
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ),
                   multiple = TRUE
                 ),
                 pickerInput(
                   inputId = "procedures_explorer_multiplex",
                   label = "Procedures",
                   choices = sort(unique(procedures$procedure_concept_name)),
                   selected = c(""),
                   options = list(
                     `actions-box` = TRUE,
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ),
                   multiple = TRUE
                 ),
                 pickerInput(
                   inputId = "devices_explorer_multiplex",
                   label = "Devices",
                   choices = sort(unique(devices$device_concept_name)),
                   selected = c(""),
                   options = list(
                     `actions-box` = TRUE,
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ),
                   multiple = TRUE
                 )
          ), # end column
          column(width = 5,
                 tags$h4("Numeric",align="center"),
                 pickerInput(
                   inputId = "measurements_explorer_multiplex",
                   label = "Measurements",
                   choices = sort(unique(measurements$measurement_concept_name)),
                   selected = c(""),
                   options = list(
                     `actions-box` = TRUE,
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ),
                   multiple = TRUE
                 ),
                 pickerInput(
                   inputId = "observations_explorer_multiplex",
                   label = "Observations",
                   choices = sort(unique(observations$observation_concept_name)),
                   selected = c(""),
                   options = list(
                     `actions-box` = TRUE,
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ),
                   multiple = TRUE
                 )

          )

        ),
        hr(),

        fluidRow(
          uiOutput("multiplot_panel")
        )


      )# end fluidPage
    } else if(input$explorer_type == "explorer_multiplex_timeline"){
      
      ## generate multiplex timeline data frame from report
      multiplex_timeline_data <- multiplex_timeline$df
      
      
      fluidPage(
        fluidRow(tags$h4("Select Variables to include in Timeline:",align="center")),
        fluidRow(
          column(width=5,
                 pickerInput(
                   inputId = "conditions_explorer_multiplex_timeline",
                   label = "Conditions",
                   choices = sort(unique(multiplex_timeline_data[group=="Condition"]$content)),
                   selected = c(""),
                   options = list(
                     `actions-box` = TRUE,
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ),
                   multiple = TRUE
                 ),
                 
                 pickerInput(
                   inputId = "medications_explorer_multiplex_timeline",
                   label = "Medications",
                   choices = sort(unique(multiplex_timeline_data[group=="Medication"]$content)),
                   selected = c(""),
                   options = list(
                     `actions-box` = TRUE,
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ),
                   multiple = TRUE
                 ),
                 pickerInput(
                   inputId = "procedures_explorer_multiplex_timeline",
                   label = "Procedures",
                   choices = sort(unique(multiplex_timeline_data[group=="Procedure"]$content)),
                   selected = c(""),
                   options = list(
                     `actions-box` = TRUE,
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ),
                   multiple = TRUE
                 )
                 
          ), # end column
          column(width = 5,
                 pickerInput(
                   inputId = "devices_explorer_multiplex_timeline",
                   label = "Devices",
                   choices = sort(unique(multiplex_timeline_data[group=="Device"]$content)),
                   selected = c(""),
                   options = list(
                     `actions-box` = TRUE,
                     size = 25,
                     `selected-text-format` = "count > 1"
                   ),
                   multiple = TRUE
                 ),
                 pickerInput(
                   inputId = "measurements_explorer_multiplex_timeline",
                   label = "Measurements",
                   choices = sort(unique(multiplex_timeline_data[group=="Measurement"]$content)),
                 selected = c(""),
                 options = list(
                   `actions-box` = TRUE,
                   size = 25,
                   `selected-text-format` = "count > 1"
                 ),
                 multiple = TRUE
          ),       
          pickerInput(
            inputId = "observations_explorer_multiplex_timeline",
            label = "Observations",
            choices = sort(unique(multiplex_timeline_data[group=="Observation"]$content)),
          selected = c(""),
          options = list(
            `actions-box` = TRUE,
            size = 25,
            `selected-text-format` = "count > 1"
          ),
          multiple = TRUE
          )
          ) #end column  
          ), # end fluidrow
      fluidRow(
        column(width =4,
               tags$h4("View Type:"),
               radioButtons("multiplex_view_type", "View Type:",
                            choices =  c("Event" = "multiplex_view_type_event",
                                         "Range" = "multiplex_view_type_range"),
                            selected = "multiplex_view_type_range")
        ),
        column(width = 6,
               tags$h4("Selected Data Info:"),
               uiOutput("multiplex_timeline_info_panel")
               
               )
      ),
        
        hr(),
        
        fluidRow(
          timevisOutput("multiplex_timevis")               
                  )
        
        
      )# end fluidPage
    } # end multiplex_timeline
  })
  
  

  ################################################################ TARGETED DATA EXPLORER categorical visualization
  
# procedures targeted
    output$procedures_explorer_targeted_panel <- renderUI({

      req(logged_in()==TRUE & !is.null(pt_id_selected()))


      procedures <- pt_data_selected$l$Procedures
      req(nrow(procedures)>0)
      tagList(
        pickerInput(
          inputId = "procedures_explorer_targeted_picker",
          label = "Procedures",
          choices = sort(unique(procedures$procedure_concept_name)),
          selected = "",
          options = list(
            `actions-box` = TRUE,
            size = 25,
            `selected-text-format` = "count > 1"
          ),
          multiple = TRUE
        )

        ,

        timevisOutput("targeted_timeline_explorer_procedures")
      )

    })
  
    
      output$targeted_timeline_explorer_procedures <- renderTimevis({
        req(logged_in()==TRUE & !is.null(pt_id_selected()))


        procedures <- pt_data_selected$l$Procedures
        procedures = procedures[!is.na(procedure_date)]
        req(nrow(procedures)>0)
        
        procedures1 = procedures[,c("procedure_date","procedure_concept_name")] # add other info
        procedures1 = procedures1[!is.na(procedure_date),]
        procedures1 = procedures1[order(procedure_date),]
        procedures1 = procedures1[!duplicated(procedures1),]


        req(input$procedures_explorer_targeted_picker)

        procedures2=procedures1[procedure_concept_name %in% input$procedures_explorer_targeted_picker]
        procedures2$id = 1:nrow(procedures2)

        ### below info on what was clicked

        config <- list(
          editable = FALSE,
          multiselect = TRUE
        )

        colnames(procedures2)=c("start","content","id")

        timevis(procedures2, options = config)
      })

    
    
## medications targeted
  output$medications_explorer_targeted_panel <- renderUI({

    req(logged_in()==TRUE & !is.null(pt_id_selected()))


    medications <- pt_data_selected$l$Medications
    req(nrow(medications)>0)

    tagList(
      
        radioButtons("meds_view_type", "View Type:",
                     choices =  c("Event" = "meds_view_event",
                                  "Range" = "meds_view_range"), inline = TRUE),
        
      
      pickerInput(
        inputId = "meds_explorer_targeted_picker",
        label = "Medications",
        choices = sort(unique(medications$medication_concept_name)),
        selected = "",
        options = list(
          `actions-box` = TRUE,
          size = 25,
          `selected-text-format` = "count > 1"
        ),
        multiple = TRUE
      )

      ,

      timevisOutput("targeted_timeline_explorer_medications"),
      
      uiOutput("meds_info_panel")
    )

  })

  
  meds_targeted_explorer_data <- reactive({
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    

    medications <- pt_data_selected$l$Medications
    medications = medications[!is.na(drug_exposure_start_date)]
    
    req(nrow(medications)>0)
    
    medications1 = medications[,c("drug_exposure_start_date","drug_exposure_end_date","medication_concept_name", "stop_reason","refills","quantity","days_supply","drug_type","route_concept","dose_unit_source_value","sig","medication_concept_code","medication_concept_vocabulary","medication_source_vocabulary","medication_source_code","medication_source_name")] 
    medications1 = medications1[!is.na(drug_exposure_start_date),]
    medications1 = medications1[!duplicated(medications1),]
    
    req(input$meds_explorer_targeted_picker)
    
    medications2=medications1[medication_concept_name %in% input$meds_explorer_targeted_picker]
    medications2$id = 1:nrow(medications2)
    medications2
    
  })
  
  
  
  # Medication info panel 
  output$meds_info_panel <- renderUI({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()) & !is.null(input$targeted_timeline_explorer_medications_selected))
    
    med_id = input$targeted_timeline_explorer_medications_selected
    
    medications=meds_targeted_explorer_data()
    
    medications_selected = medications[id==as.integer(med_id)]
  

    fluidPage(
      fluidRow(
        tags$strong("Visit Ouccurence ID for Medication: ",style="display:inline"),
        tags$p(medications_selected$visit_occurrence_id,style="display:inline"),
        tags$br(),
        tags$strong("Medication Window: ",style="display:inline"), 
        tags$p(medications_selected$drug_exposure_start_date,style="display:inline"),
        tags$p(" to ", style="display:inline"),
        tags$p(medications_selected$drug_exposure_end_date,style="display:inline"),
        tags$br(),
        tags$strong("Medication Type: ",style="display:inline"),
        tags$p(medications_selected$drug_type,style="display:inline"),
        tags$br(),
        tags$strong("Medication Standardized Name: ",style="display:inline"),
        tags$p(medications_selected$medication_concept_name,style="display:inline"),
        tags$br(),
        tags$strong("Medication Standardized Concept Code: ",style="display:inline"),
        tags$p(medications_selected$medication_concept_code,style="display:inline"),
        tags$br(),
        tags$strong("Medication Standardized Concept Vocabulary: ",style="display:inline"),
        tags$p(medications_selected$medication_concept_vocabulary,style="display:inline"),
        tags$br(),
        tags$strong("Medication Standardized Concept Vocabulary: ",style="display:inline"),
        tags$p(medications_selected$medication_concept_vocabulary,style="display:inline"),
        tags$br(),
        tags$strong("Medication Route: ",style="display:inline"),
        tags$p(medications_selected$route_concept,style="display:inline"),
        tags$br(),
        tags$strong("Medication Quantity: ",style="display:inline"),
        tags$p(medications_selected$quantity,style="display:inline"),
        tags$br(),
        tags$strong("Medication Dose Unit: ",style="display:inline"),
        tags$p(medications_selected$dose_unit_source_value,style="display:inline"),
        tags$br(),  
        tags$strong("Medication Days Supply: ",style="display:inline"),
        tags$p(medications_selected$days_supply,style="display:inline"),
        tags$br(),  
        tags$strong("Medication Refills: ",style="display:inline"),
        tags$p(medications_selected$refills,style="display:inline"),
        tags$br(),  
        tags$strong("Medication Stop Reason: ",style="display:inline"),
        tags$p(medications_selected$stop_reason,style="display:inline"),
        tags$br(), 
        tags$strong("Medication Directions (sig): ",style="display:inline"),
        tags$p(medications_selected$sig,style="display:inline"),
        tags$br(),  
        tags$strong("Medication Source Name: ",style="display:inline"),
        tags$p(medications_selected$medication_source_name,style="display:inline"),
        tags$br(),  
        tags$strong("Medication Source Vocabulary: ",style="display:inline"),
        tags$p(medications_selected$medication_source_vocabulary,style="display:inline"),
        tags$br(), 
        tags$strong("Medication Source Code: ",style="display:inline"),
        tags$p(medications_selected$medication_source_code,style="display:inline")

      )
      
        )
    
     
  })
  
  
  output$targeted_timeline_explorer_medications <- renderTimevis({
    req(logged_in()==TRUE & !is.null(pt_id_selected()))

    medications=meds_targeted_explorer_data()
    
    medications = medications[,c("drug_exposure_start_date","drug_exposure_end_date","medication_concept_name","id")] # filter columns here
    

    
    req(nrow(medications)>0)

    config <- list(
      editable = FALSE,
      multiselect = TRUE,
      showToolTips = TRUE # tool tips for range visits
    )
    
    colnames(medications)=c("start","end","content","id")
    medications$title = medications$content
    
    if(input$meds_view_type == "meds_view_event"){ # if Event selected | Range requires no change
      medications$end = NA
    }
    

    
    timevis(medications, options = config)
  })
  
  
  
  ### conditions targeted
  output$conditions_explorer_targeted_panel <- renderUI({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    
    conditions <- pt_data_selected$l$Conditions
    conditions = conditions[!is.na(condition_start_date)]
    req(nrow(conditions)>0)
    
    tagList(
      
      radioButtons("conditions_view_type", "View Type:",
                   choices =  c("Event" = "conditions_view_event",
                                "Range" = "conditions_view_range"),inline=TRUE),

      pickerInput(
        inputId = "conditions_explorer_targeted_picker",
        label = "Conditions",
        choices = sort(unique(conditions$condition_concept_name)),
        selected = "",
        options = list(
          `actions-box` = TRUE,
          size = 25,
          `selected-text-format` = "count > 1"
        ),
        multiple = TRUE
      )
      
      ,
      
      timevisOutput("targeted_timeline_explorer_conditions"),
      
      uiOutput("conditions_info_panel")
    )
    
  })

  conditions_targeted_explorer_data <- reactive({ 
    req(logged_in()==TRUE & !is.null(pt_id_selected()))

    conditions <- pt_data_selected$l$Conditions
    conditions = conditions[!is.na(condition_start_date)]

    req(nrow(conditions)>0)
    conditions1 = conditions[,c("condition_start_date","condition_end_date","condition_concept_name","visit_occurrence_id","condition_source_value","condition_concept_vocabulary","condition_concept_code","condition_source_vocabulary","condition_source_code","condition_status_type")] 
    conditions1 = conditions1[!is.na(condition_start_date),]
    conditions1 = conditions1[!duplicated(conditions1),]

    conditions2=conditions1[condition_concept_name %in% input$conditions_explorer_targeted_picker]
    
    
    
    conditions2$id = 1:nrow(conditions2)
    
    conditions2

  })
  
  
  
  output$targeted_timeline_explorer_conditions <- renderTimevis({
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    conditions=conditions_targeted_explorer_data()

    
    req(nrow(conditions)>0)
    
    conditions = conditions[,c("condition_start_date","condition_end_date","condition_concept_name","id")] # filter columns here

      
      
    config <- list(
      editable = FALSE,
      multiselect = TRUE,
      showToolTips = TRUE # tool tips for range visits
    )
    
    colnames(conditions)=c("start","end","content","id")
    conditions$title = conditions$content
    
    if(input$conditions_view_type == "conditions_view_event"){ # if Event selected | Range requires no change
      conditions$end = NA
    }
    
    
    
    timevis(conditions, options = config)
  })
  
  
  # Conditions info panel
  output$conditions_info_panel <- renderUI({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()) & !is.null(input$targeted_timeline_explorer_conditions_selected))
    
    dx_id = input$targeted_timeline_explorer_conditions_selected
    
    conditions=conditions_targeted_explorer_data()
    
    conditions_selected = conditions[id==as.integer(dx_id)]
    
    fluidPage( 
      tags$strong("Visit Ouccurence ID for Condition: ",style="display:inline"),
      tags$p(conditions_selected$visit_occurrence_id,style="display:inline"),
      tags$br(),
      tags$strong("Condition Window: ",style="display:inline"), 
      tags$p(conditions_selected$condition_start_date,style="display:inline"),
      tags$p(" to ", style="display:inline"),
      tags$p(conditions_selected$condition_end_date,style="display:inline"),
      tags$br(),      
      tags$strong("Condition Status Type: ",style="display:inline"),
      tags$p(conditions_selected$condition_status_type,style="display:inline"),
      tags$br(),
      tags$strong("Condition Standardized Name Selected: ",style="display:inline"),
      tags$p(conditions_selected$condition_concept_name,style="display:inline"),
      tags$br(),
      tags$strong("Condition Standardized Vocabulary: ",style="display:inline"),
      tags$p(conditions_selected$condition_concept_vocabulary,style="display:inline"),
      tags$br(),
      tags$strong("Condition Standardized Vocabulary Code: ",style="display:inline"),
      tags$p(conditions_selected$condition_concept_code,style="display:inline"),
      tags$br(),
      tags$strong("Condition Source Value: ",style="display:inline"),
      tags$p(conditions_selected$condition_source_value,style="display:inline"),
      tags$br(),
      tags$strong("Condition Source Vocabulary: ",style="display:inline"),
      tags$p(conditions_selected$condition_source_vocabulary,style="display:inline"),
      tags$br(),
      tags$strong("Condition Source Vocabulary Code: ",style="display:inline"),
      tags$p(conditions_selected$condition_source_code,style="display:inline")

    )
    
    
  })
  
  ### devices targeted
  output$devices_explorer_targeted_panel <- renderUI({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    
    devices <- pt_data_selected$l$Devices
    devices = devices[!is.na(device_exposure_start_date)]
    req(nrow(devices)>0)
    
    tagList(
      
      radioButtons("devices_view_type", "View Type:",
                   choices =  c("Event" = "devices_view_event",
                                "Range" = "devices_view_range"),inline=TRUE),
      
      pickerInput(
        inputId = "devices_explorer_targeted_picker",
        label = "Devices",
        choices = sort(unique(devices$device_concept_name)),
        selected = "",
        options = list(
          `actions-box` = TRUE,
          size = 25,
          `selected-text-format` = "count > 1"
        ),
        multiple = TRUE
      )
      
      ,
      
      timevisOutput("targeted_timeline_explorer_devices"),
      
      uiOutput("devices_info_panel")
    )
    
  })
  
  
  devices_targeted_explorer_data <- reactive({ 
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    devices <- pt_data_selected$l$Devices
    devices = devices[!is.na(device_exposure_start_date)]
    
    req(nrow(devices)>0)
    devices1 = devices[,c("device_exposure_start_date","device_exposure_end_date","device_concept_name","visit_occurrence_id","device_source_value","device_concept_vocabulary","device_concept_code","device_source_vocabulary","device_source_code","device_source_name", "device_type")] 
    devices1 = devices1[!is.na(device_exposure_start_date),]
    devices1 = devices1[!duplicated(devices1),]
    
    devices2=devices1[device_concept_name %in% input$devices_explorer_targeted_picker]
    
    
    
    devices2$id = 1:nrow(devices2)
    
    devices2
    
  })
  
  
  
  output$targeted_timeline_explorer_devices <- renderTimevis({
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    devices=devices_targeted_explorer_data()
    
    
    req(nrow(devices)>0)
    
    devices = devices[,c("device_exposure_start_date","device_exposure_end_date","device_concept_name","id")] # filter columns here
    
    
    config <- list(
      editable = FALSE,
      multiselect = TRUE,
      showToolTips = TRUE # tool tips for range visits
    )
    
    colnames(devices)=c("start","end","content","id")
    devices$title = devices$content
    
    if(input$devices_view_type == "devices_view_event"){ # if Event selected | Range requires no change
      devices$end = NA
    }
    
    
    
    timevis(devices, options = config)
  })
  
  
  # Devices info panel
  output$devices_info_panel <- renderUI({
    
    req(logged_in()==TRUE & !is.null(pt_id_selected()) & !is.null(input$targeted_timeline_explorer_devices_selected))
    
    d_id = input$targeted_timeline_explorer_devices_selected
    
    devices=devices_targeted_explorer_data()
    
    devices_selected = devices[id==as.integer(d_id)]

    fluidPage( 
      tags$strong("Visit Ouccurence ID for Device: ",style="display:inline"),
      tags$p(devices_selected$visit_occurrence_id,style="display:inline"),
      tags$br(),
      tags$strong("Device Window: ",style="display:inline"), 
      tags$p(devices_selected$device_exposure_start_date,style="display:inline"),
      tags$p(" to ", style="display:inline"),
      tags$p(devices_selected$device_exposure_end_date,style="display:inline"),
      tags$br(),      
      tags$strong("Device Standardized Name Selected: ",style="display:inline"),
      tags$p(devices_selected$device_concept_name,style="display:inline"),
      tags$br(),
      tags$strong("Device Type: ",style="display:inline"),
      tags$p(devices_selected$device_type,style="display:inline"),
      tags$br(),
      tags$strong("Device Standardized Vocabulary: ",style="display:inline"),
      tags$p(devices_selected$device_concept_vocabulary,style="display:inline"),
      tags$br(),
      tags$strong("Device Standardized Vocabulary Code: ",style="display:inline"),
      tags$p(devices_selected$device_concept_code,style="display:inline"),
      tags$br(),
      tags$strong("Device Source Value: ",style="display:inline"),
      tags$p(devices_selected$device_source_value,style="display:inline"),
      tags$br(),
      tags$strong("Device Source Vocabulary: ",style="display:inline"),
      tags$p(devices_selected$device_source_vocabulary,style="display:inline"),
      tags$br(),
      tags$strong("Device Source Vocabulary Code: ",style="display:inline"),
      tags$p(devices_selected$device_source_code,style="display:inline"),
      tags$br(),
      tags$strong("Device Source Name: ",style="display:inline"),
      tags$p(devices_selected$device_source_name,style="display:inline")
      
      
    )
    
  })
  
  
  ################################################################ TARGETED DATA EXPLORER numeric visualization
  
  output$timevispanel <- renderUI({
    if(input$dataset_type_targeted_explorer %in% c("Measurements","Observations")){
      plotlyOutput('timevisnumeric', height = "400px") %>% withSpinner()
    }
  })
  
  
  output$timevisnumeric <- renderPlotly({
    
    if(input$dataset_type_targeted_explorer=="Measurements"){
      
      req(logged_in()==TRUE & !is.null(pt_id_selected()))
      
      measurements <- pt_data_selected$l$Measurements
      
      # require not 0 here
      req(nrow(measurements)>0)

      measurements=measurements[,c("measurement_date","measurement_concept_name","value_as_number","unit_concept","value_concept")]
      i=req(get_measurement_explorer_row())
      if (!is.null(i$value)){
        measurements2=measurements[measurement_concept_name==i$value,]
      
        measurements2$value_as_number=as.numeric(measurements2$value_as_number)
        
        measurements2=measurements2[!is.na(measurements2$value_as_number),]
        measurements2 = measurements2[order(measurement_date),]
        measurements2$value_concept=  as.factor(measurements2$value_concept)

        p <- plot_ly(measurements2,x = ~measurement_date, y = ~value_as_number, type = 'scatter',mode="lines+markers", name = "Normal") %>%
          layout(xaxis = list(title = "Measurement Order Date"), yaxis = list(title = paste(i$value, unique(measurements2$unit_concept),sep=" ")))
        
        if(any(measurements2$value_concept=="High",na.rm=T)){
        p <- add_trace(p, 
                       data = measurements2[value_concept=="High"],
                       mode = 'markers',
                       marker = list(color = "red"),
                       x = ~measurement_date, 
                       y = ~value_as_number,
                       name = "High")
        }
        
        if(any(measurements2$value_concept=="Low",na.rm=T)){
        p <- add_trace(p, 
                       data = measurements2[value_concept=="Low"],
                       mode = 'markers',
                       marker = list(color = "purple"),
                       x = ~measurement_date, 
                       y = ~value_as_number,
                       name = "Low")
        
        }
        ggplotly(p) %>% 
          layout(height = input$plotHeight, autosize=TRUE)
      }
      
    
    } else if(input$dataset_type_targeted_explorer=="Observations"){

    req(logged_in()==TRUE & !is.null(pt_id_selected()))

    observations <- pt_data_selected$l$Observations

    req(nrow(observations)>0)
    
    observations = observations[order(observation_date),]
    
      i=req(get_observation_explorer_row())
      if (!is.null(i$value)){

        observations1=observations[which(observation_concept_name==i$value),]

      if(!is.na(unique(observations1$unit_source_value))){
        yaxis_title = paste(i$value, unique(observations1$unit_source_value),sep=" ")
      }else{
        yaxis_title = i$value
      }
      
      
      ### numeric only
      observation_numeric_rows = which(!is.na(as.numeric(observations1$value_as_number)))
      observation_numeric=observations1[observation_numeric_rows, ]

      ### categorical
      observation_categorical=observations1[!observation_numeric_rows, ]
      
      if(nrow(observation_numeric)==nrow(observations1)){ # only numeric, include lines
        observation_numeric$value_as_number = as.numeric(observation_numeric$value_as_number)


        p <- plot_ly(observation_numeric,x = ~observation_date, y = ~value_as_number, type = 'scatter',mode="lines+markers") %>%
          layout(xaxis = list(title = "Observation Date"), yaxis = list(title = yaxis_title))


      }else{ # categorical, do not include lines
        
        p <- plot_ly(observation_categorical,x = ~observation_date, y = ~value_as_string, type = 'scatter',mode="markers") %>%
          layout(xaxis = list(title = "Observation Date"), yaxis = list(title = yaxis_title))

      }

      ggplotly(p) %>%
        layout(height = input$plotHeight, autosize=TRUE)

      }
      
    }  
    else{
      plotly_empty(type = "scatter",mode="lines+markers")
    }
    
  })
  
  
  ################################################################ TARGETED DATA EXPLORER numeric tables
  
  ########### find cell clicked for numeric frequency tables
  
  get_measurement_explorer_row<-eventReactive(input$measurements_explorer_table_cell_clicked, {  
    info = input$measurements_explorer_table_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 1st column
    if (is.null(info$value)|| info$col!=0) return()
    info
  })
  

  get_observation_explorer_row<-eventReactive(input$observations_explorer_table_cell_clicked, {  
    info = input$observations_explorer_table_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 1st column
    if (is.null(info$value)|| info$col!=0) return()
    info
  })
  ###############
    
  
  output$measurements_explorer_table <- DT::renderDataTable({
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    measurements <- pt_data_selected$l$Measurements
    
    if(nrow(measurements)!=0){


    measurements$value_as_number=as.numeric(measurements$value_as_number)
    measurements=measurements[!is.na(measurements$value_as_number),]
    measurements_table=data.table(table(measurements$measurement_concept_name))
    measurements_table=measurements_table[order(-N),]
    colnames(measurements_table)[1]="Measurement Concept Name"
    }else{
      measurements_table=data.table(measurement_concept_name = character(), N= numeric())
    }
    
    datatable(measurements_table,
              rownames = FALSE,
              selection = 'single',
              style = "bootstrap",
              options = list(
                pageLength = 5
              )) 
    
    
  })
  
  
  
  output$observations_explorer_table <- DT::renderDataTable({
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    observations <- pt_data_selected$l$Observations

    if(nrow(observations)!=0){
    # do not remove Names with non-numeric VALUES
      observations=observations[!(is.na(observations$value_as_number)&is.na(observations$value_as_string)),]
      observations_table=data.table(table(observations$observation_concept_name))
      observations_table=observations_table[order(-N),]
    colnames(observations_table)[1]="Observation_Concept_Name"
    }else{
      observations_table=data.table(Observation_Concept_Name = character(), N= numeric())
    }
    
    datatable(observations_table,
              rownames = FALSE,
              selection = 'single',
              style = "bootstrap",
              options = list(
                pageLength = 5
              )) 
    
    
  })

 
  ################################################################ MULTIPLEX DATA EXPLORER 
  ##### ORIGINAL
  
output$multiplot_panel <- renderUI({ ##
 multiplex_selected <-c(input$conditions_explorer_multiplex,input$medications_explorer_multiplex,input$procedures_explorer_multiplex ,input$measurements_explorer_multiplex,input$devices_explorer_multiplex,input$observations_explorer_multiplex)

  if(length(multiplex_selected)==1){
    plotlyOutput('multiplot', height = "400px") %>% withSpinner()
  }else if(length(multiplex_selected)==2){
    plotlyOutput('multiplot', height = "500px") %>% withSpinner()
  }else if(length(multiplex_selected)==3) {
    plotlyOutput('multiplot', height = "800px") %>% withSpinner()
  }else if(length(multiplex_selected)>3) {
    plotlyOutput('multiplot', height = "1000px") %>% withSpinner()
  }

})

#
  output$multiplot <- renderPlotly({

    numeric_tbl_combined=data.table("Date" = as.POSIXct(character(), format="%Y-%m-%d"), "Name" = character(),"Value"=numeric()) 

    categorical_tbl_combined=data.table("StartDate" = as.POSIXct(character(), format="%Y-%m-%d"),"EndDate" = as.POSIXct(character(), format="%Y-%m-%d"), "Name" = character())  

    ### retrieve and format data
    # conditions
    conditions <- pt_data_selected$l$Conditions
    conditions <- conditions[!is.na(condition_start_date),]
    conditions = conditions[,c("condition_start_date","condition_end_date","condition_concept_name")]
    colnames(conditions) = c("StartDate","EndDate","Name")

    conditions$StartDate = as.POSIXct(conditions$StartDate, format="%Y-%m-%d")
    conditions$EndDate = as.POSIXct(conditions$EndDate, format="%Y-%m-%d") 
    conditions=conditions[!duplicated(conditions),]

    # medications
    medications <- pt_data_selected$l$Medications
    medications <- medications[!is.na(drug_exposure_start_date),]


    medications$StartDate = as.POSIXct(medications$drug_exposure_start_date, format="%Y-%m-%d")
    medications$EndDate = as.POSIXct(medications$drug_exposure_end_date, format="%Y-%m-%d")

    medications = medications[,c("StartDate","EndDate","medication_concept_name")]
    colnames(medications) = c("StartDate","EndDate","Name")
    medications=medications[!duplicated(medications),]

    # procedures
    procedures <- pt_data_selected$l$Procedures
    procedures <- procedures[!is.na(procedure_date),] 

    procedures$StartDate = as.POSIXct(procedures$procedure_date, format="%Y-%m-%d") 
    procedures$EndDate = as.POSIXct(NA)
    procedures = procedures[,c("StartDate","EndDate","procedure_concept_name")]
    colnames(procedures) = c("StartDate","EndDate","Name")
    procedures=procedures[!duplicated(procedures),]

    # measurements
    measurements <- pt_data_selected$l$Measurements
    measurements <- measurements[(!is.na(measurement_date) & !is.na(as.numeric(value_as_number))),] 
    measurements$value_as_number = as.numeric(measurements$value_as_number)
    measurements$date = as.POSIXct(measurements$measurement_date, format="%Y-%m-%d") 
    measurements = measurements[,c("date","measurement_concept_name","value_as_number")]
    colnames(measurements) = c("Date","Name","Value")
    measurements=measurements[!duplicated(measurements),]

    # devices
    devices <- pt_data_selected$l$Devices
    devices <- devices[!is.na(device_exposure_start_date),]
    devices = devices[,c("device_exposure_start_date","device_exposure_end_date","device_concept_name")]
    colnames(devices) = c("StartDate","EndDate","Name")

    devices$StartDate = as.POSIXct(devices$StartDate, format="%Y-%m-%d")
    devices$EndDate = as.POSIXct(devices$EndDate, format="%Y-%m-%d") 
    devices=devices[!duplicated(devices),]

    # observations
    observations <- pt_data_selected$l$Observations
    observations <- observations[!is.na(observation_date),]
    observations <- observations[!(is.na(value_as_number)&is.na(value_as_string)),]
    observations$value_as_number = as.numeric(observations$value_as_number)
    observations$date = as.POSIXct(observations$observation_date, format="%Y-%m-%d") 
#     first pull out numeric rows
   observations_numeric_rows = which(!is.na(as.numeric(observations$value_as_number)))
   observations_numeric=observations[observations_numeric_rows, c("date","observation_concept_name","value_as_number")]
   colnames(observations_numeric)[3]="Value"
   observations_categorical=observations[!observations_numeric_rows,c("date","observation_concept_name","value_as_string")]
   colnames(observations_categorical)[3]="Value"
   observations = rbind(observations_numeric,observations_categorical)
    colnames(observations) = c("Date","Name","Value")
    observations=observations[!duplicated(observations),]

    ##combine selected
    
    if(!is.null(input$conditions_explorer_multiplex)){
    conditions2 = conditions[Name %in% input$conditions_explorer_multiplex]

    categorical_tbl_combined = rbind(categorical_tbl_combined, conditions2)
    }

    if(!is.null(input$medications_explorer_multiplex)){
      medications2 = medications[Name %in% input$medications_explorer_multiplex]

      categorical_tbl_combined = rbind(categorical_tbl_combined, medications2)
    }


    if(!is.null(input$procedures_explorer_multiplex)){
      procedures2 = procedures[Name %in% input$procedures_explorer_multiplex]

     categorical_tbl_combined = rbind(categorical_tbl_combined, procedures2)

      }

    if(!is.null(input$devices_explorer_multiplex)){
      devices2 = devices[Name %in% input$devices_explorer_multiplex]

      categorical_tbl_combined = rbind(categorical_tbl_combined, devices2)

      }

    if(!is.null(input$measurements_explorer_multiplex)){

    measurements2 = measurements[Name %in% input$measurements_explorer_multiplex]

    numeric_tbl_combined = rbind(numeric_tbl_combined, measurements2)
    }

    if(!is.null(input$observations_explorer_multiplex)){
    observations2 = observations[Name %in% input$observations_explorer_multiplex]

    numeric_tbl_combined = rbind(numeric_tbl_combined, observations2)

    }


  if(nrow(numeric_tbl_combined)>0 & nrow(categorical_tbl_combined)==0){ ### numeric only

    numeric_tbl_combined = numeric_tbl_combined %>%
      transform(id = as.integer(factor(Name)))

    numeric_tbl_combined=numeric_tbl_combined[order(numeric_tbl_combined$id,numeric_tbl_combined$Date),]

    p<- numeric_tbl_combined %>%
      plot_ly(x = ~Date, y = ~Value, color = ~Name, colors = "Dark2", yaxis = ~paste0("y", id),type="scatter",mode="lines+markers") %>%
      subplot(nrows = length(unique(numeric_tbl_combined$id)), shareX = TRUE)

    ggplotly(p)

}else if(nrow(numeric_tbl_combined)==0 & nrow(categorical_tbl_combined)>0){ # categorical only

  categorical_tbl_combined = categorical_tbl_combined %>%
    transform(id = as.integer(factor(Name)))

  categorical_tbl_combined=categorical_tbl_combined[order(categorical_tbl_combined$id,categorical_tbl_combined$StartDate),]

  p <- plot_ly(categorical_tbl_combined, colors = "Dark2") %>%
    add_markers(x = ~StartDate, y = ~Name, color = ~Name) %>%
    add_segments(x = ~StartDate, xend = ~EndDate, y = ~Name, yend = ~Name, color = I("gray50"), showlegend = FALSE) %>%
    layout(yaxis = list(title="",showticklabels = FALSE))


  }else if(nrow(numeric_tbl_combined)>0 & nrow(categorical_tbl_combined)>0){ # numeric and categorical

    numeric_tbl_combined = numeric_tbl_combined %>%
      transform(id = as.integer(factor(Name)))

    numeric_tbl_combined=numeric_tbl_combined[order(numeric_tbl_combined$id,numeric_tbl_combined$Date),]

    categorical_tbl_combined = categorical_tbl_combined %>%
      transform(id = as.integer(factor(Name)))

    categorical_tbl_combined=categorical_tbl_combined[order(categorical_tbl_combined$id,categorical_tbl_combined$StartDate),]


    p1<- numeric_tbl_combined %>%
      plot_ly(x = ~Date, y = ~Value, color = ~Name, colors = "Dark2", yaxis = ~paste0("y", id),type="scatter",mode="lines+markers") %>%
      subplot(nrows = length(unique(numeric_tbl_combined$id)), shareX = TRUE)

    p2 <- plot_ly(categorical_tbl_combined, colors = "Spectral") %>%
      add_markers(x = ~StartDate, y = ~Name, color = ~Name) %>%
      add_segments(x = ~StartDate, xend = ~EndDate, y = ~Name, yend = ~Name, color = I("gray50"), showlegend = FALSE) %>%
      layout(yaxis = list(title="",showticklabels = FALSE))

    p<- subplot(p2, p1, nrows = 2, heights = c(1/12, 11/12), shareX = TRUE)






    }else{
    p<-plotly_empty(type = "scatter",mode="lines+markers")
  }

  ggplotly(p)

  }) # end multiplex plotly
  
  
  ###### MULTIPLEX TIMELINE
  
  ### data info
  
  output$multiplex_timeline_info_panel <- renderUI({
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    m_dat_id <- input$multiplex_timevis_selected
    
    if(is.null(m_dat_id)){ # if null present blank values
      m_dat_mode <- ""
      m_dat_concept <- ""
      m_dat_start <-""
      m_dat_end <- ""
      m_dat_val <- ""
      m_to <- ""
    } else {# else load and display values
      
      multiplex_data = multiplex_timeline$df
      
      multiplex_data_selected = multiplex_data[id==as.integer(m_dat_id)]
      
      m_dat_mode <- multiplex_data_selected$group
      m_dat_concept <- multiplex_data_selected$content
      m_dat_start <- multiplex_data_selected$start
      m_dat_end <- multiplex_data_selected$end
      m_dat_val <- multiplex_data_selected$Value
      m_to <- " to "
    }
    
    fluidPage(
      fluidRow(
        tags$strong("Modality: ", style = "display:inline"),
        tags$p(m_dat_mode, style = "display:inline"),
        tags$br(),
        tags$strong("Concept: ", style = "display:inline"),
        tags$p(m_dat_concept, style = "display:inline"),
        tags$br(),
        tags$strong("Window: ", style = "display:inline"),
        tags$p(m_dat_start, style = "display:inline"),
        tags$p(m_to, style = "display:inline"),
        tags$p(m_dat_end, style = "display:inline"),
        tags$br(),
        tags$strong("Value: ", style = "display:inline"),
        tags$p(m_dat_val, style = "display:inline")
      )
    )
  })
  
  ### timevis
  output$multiplex_timevis <- renderTimevis({
    req(logged_in()==TRUE & !is.null(pt_id_selected()))
    
    
    multiplex_data = multiplex_timeline$df
    multiplex_data = multiplex_data[,-"Value"]
    
    # filter multiplex_data
    multiplex_data=multiplex_data[!content %in% setdiff(unique(multiplex_data[group == "Condition",]$content), input$conditions_explorer_multiplex_timeline)] # do not include 'unselected' events
    multiplex_data=multiplex_data[!content %in% setdiff(unique(multiplex_data[group == "Observation",]$content), input$observations_explorer_multiplex_timeline)] # do not include 'unselected' events
    multiplex_data=multiplex_data[!content %in% setdiff(unique(multiplex_data[group == "Procedure",]$content), input$procedures_explorer_multiplex_timeline)] # do not include 'unselected' events
    multiplex_data=multiplex_data[!content %in% setdiff(unique(multiplex_data[group == "Measurement",]$content), input$measurements_explorer_multiplex_timeline)] # do not include 'unselected' events
    multiplex_data=multiplex_data[!content %in% setdiff(unique(multiplex_data[group == "Device",]$content), input$devices_explorer_multiplex_timeline)] # do not include 'unselected' events
    multiplex_data=multiplex_data[!content %in% setdiff(unique(multiplex_data[group == "Medication",]$content), input$medications_explorer_multiplex_timeline)] # do not include 'unselected' events
    

    req(nrow(multiplex_data)>0)
    
    groups <- data.frame(unique(multiplex_data$group),unique(multiplex_data$group))
    colnames(groups) <- c("id","content")
    
    
    
    config <- list(
      editable = FALSE,
      multiselect = TRUE,
      showToolTips = TRUE # tool tips for range visits
    )
    

    
    if(input$multiplex_view_type == "multiplex_view_type_event"){ # if Event selected | Range requires no change
      multiplex_data$end = NA # set all end to NA
     multiplex_data$type = "point" # set all type to point
    }
  
    multiplex_data$title = multiplex_data$content # for tooltip
    
    timevis(multiplex_data, groups = groups, options = config)
  })
  

  } # end server



# Run the application 
shinyApp(ui = ui, server = server)

