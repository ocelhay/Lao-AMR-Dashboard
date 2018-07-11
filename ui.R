fluidPage(
  theme = shinytheme("spacelab"),
  includeCSS("./www/styles.css"),
  
  sidebarPanel(width = 3,
               h2("LOMWRU"),
               h3("AMR Dashboard"),
               div(class = "imgsolidborder4", img(src = "ecoli.jpg")),
               br(),
               
               conditionalPanel(condition = "input.tabs != 'about'",
                                div(class = "border4",
                                    
                                    conditionalPanel(condition = "input.tabs == 'welcome'",
                                                     h3(icon("upload", "fa-1x"), "Upload data"),
                                                     fileInput("file_RData", label = NULL, accept = ".RData", buttonLabel = "Browse..."),
                                                     htmlOutput("data_status")
                                    ),
                                    
                                    conditionalPanel(condition = "input.tabs == 'blood_culture' | input.tabs == 'specimens' | input.tabs == 'organisms' | input.tabs == 'amr'",
                                                     h3(icon("filter", "fa-1x"), "Filter the Dataset:"),
                                                     
                                                     fluidRow(
                                                       column(width = 4, p("Patients Age Range:")),
                                                       column(width = 8, sliderInput("age_patients_selection", label = NULL, min = 0, max = oldest_patient, value = c(0, oldest_patient)))
                                                     ),
                                                     fluidRow(
                                                       column(width = 4, p("Patients Province of Residence:")),
                                                       column(width = 8, pickerInput(inputId = "province_patients_selection", label = NULL, multiple = TRUE,
                                                                                     choices = all_provinces, selected = all_provinces, options = list(
                                                                                       `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                                                       `select-all-text` = "Select All", `none-selected-text` = "None Selected")
                                                       ))
                                                     ),
                                                     hr(),
                                                     fluidRow(
                                                       column(width = 4, p("Specimens Collection Date:")),
                                                       column(width = 8, selectInput("date_range_selection", label = NULL, choices = c("Filter by Year", "Filter by Date Range")),
                                                              conditionalPanel("input.date_range_selection == 'Filter by Year'",
                                                                               checkboxGroupInput("year_selection", label = NULL, choices = all_spec_year, selected = all_spec_year, inline = TRUE)
                                                              ),
                                                              conditionalPanel("input.date_range_selection == 'Filter by Date Range'",
                                                                               br(), br(),
                                                                               dateRangeInput("date_selection", label = NULL)
                                                              ))
                                                     ),
                                                     fluidRow(
                                                       column(width = 4, p("Specimens Collection Location:")),
                                                       column(width = 8, pickerInput(inputId = "spec_method_collection", label = NULL, multiple = TRUE,
                                                                                     choices = all_locations, selected = all_locations, options = list(
                                                                                       `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                                                       `select-all-text` = "Select All", `none-selected-text` = "None Selected")
                                                       )
                                                       ))
                                    ),
                                    conditionalPanel(condition = "input.tabs == 'blood_culture'",
                                                     fluidRow(
                                                       column(width = 4, p("Specimens Collection Method:")),
                                                       column(width = 8, strong("Blood Culture Only"))
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.tabs == 'specimens' | input.tabs == 'organisms' | input.tabs == 'amr'",
                                                     fluidRow(
                                                       column(width = 4, p("Specimens Collection Method:")),
                                                       column(width = 8, pickerInput(inputId = "spec_method_selection", label = NULL, multiple = TRUE,
                                                                                     choices = all_spec_method, selected = all_spec_method, options = list(
                                                                                       `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                                                       `select-all-text` = "Select All", `none-selected-text` = "None Selected")
                                                       ))
                                                     )
                                    ),
                                    
                                    conditionalPanel(condition = "input.tabs == 'specimens' | input.tabs == 'organisms' | input.tabs == 'amr'",
                                                     htmlOutput("filter_text")
                                    ),
                                    conditionalPanel(condition = "input.tabs == 'blood_culture'",
                                                     htmlOutput("filter_text_blood")
                                    )
                                )
               )
  ),
  
  
  
  
  
  mainPanel(width = 9,
            navbarPage(NULL, position = "static-top", id = "tabs", collapsible = TRUE,  windowTitle = "LOMWRU AMR Dashboard",
                       tabPanel("Welcome", value = "welcome",
                                includeMarkdown("./www/disclaimer.md"),
                                includeMarkdown("./www/about_amr.md")
                       ),
                       tabPanel("Blood Culture", value = "blood_culture",
                                fluidRow(
                                  column(2, h2("Sample Growth")),
                                  column(10, div(class = "cent", h2("Specimens Origins")))
                                ),
                                fluidRow(
                                  column(width = 2,
                                         br(),
                                         br(),
                                         plotOutput("growth_blood", height = "250px") %>% withSpinner()
                                  ),
                                  column(width = 5,
                                         plotOutput("province_specimen_blood") %>% withSpinner()
                                  ),
                                  column(width = 5,
                                         plotOutput("hospital_specimen_blood") %>% withSpinner()
                                  )
                                ),
                                h2("Total Organisms"),
                                fluidRow(
                                  column(width = 8,
                                         p("The graph below displays the 25 more commons organisms, report to the table for the complete listing."),
                                         highchartOutput("count_organisms_blood", height = "900px") %>% withSpinner()
                                         # plotOutput("count_organisms_blood", height = "800px") %>% withSpinner(),
                                  ),
                                  column(width = 4,
                                         p("Table of all organisms:"),
                                         DTOutput("table_organisms_blood") %>% withSpinner()
                                  )
                                ),
                                br()
                       ),
                       tabPanel("Specimens", value = "specimens",
                                h2("Total Specimens per Method of Collection"),
                                p("Use the UI elements located on the the sidebar to filter and display, for example, only specimens collected in a specific hospital."),
                                plotOutput("specimens_method", height = "600px") %>% withSpinner()
                       ),
                       tabPanel("Organisms", value = "organisms",
                                h2("Total Isolates per Method of Collection"),
                                plotOutput("isolates_method", height = "600px") %>% withSpinner(),
                                h2("Total Isolates per Organism"),
                                fluidRow(
                                  column(width = 8, 
                                         p("The graph below displays the 25 organisms with the more isolates, report to the table for the complete listing."),
                                         highchartOutput("isolates_organisms", height = "900px") %>% withSpinner()),
                                  column(width = 4, dataTableOutput("table_isolates_organisms") %>% withSpinner())
                                )
                       ),
                       tabPanel("AMR", value = "amr", icon = icon("bug"),
                                tabsetPanel(
                                  tabPanel("A. baumannii",
                                           br(),
                                           fluidRow(
                                             column(width = 6,
                                                    htmlOutput("organism_isolates_ab") %>% withSpinner()),
                                             column(width = 6,
                                                    plotOutput("organism_isolates_year_ab", height = "250px") %>% withSpinner()
                                             )
                                           ),
                                           plotOutput("organism_sir_plot_ab") %>% withSpinner(),
                                           DTOutput("organism_sir_table_ab") %>% withSpinner()
                                           
                                           
                                  ),
                                  tabPanel("E. coli",
                                           br(),
                                           fluidRow(
                                             column(width = 6,
                                                    htmlOutput("organism_isolates_ec") %>% withSpinner()),
                                             column(width = 6,
                                                    plotOutput("organism_isolates_year_ec", height = "250px") %>% withSpinner()
                                             )
                                           ),
                                           plotOutput("organism_sir_plot_ec") %>% withSpinner(),
                                           DTOutput("organism_sir_table_ec") %>% withSpinner(),
                                           h2("(ESBL positive for Escherichia coli)/(Patients with Escherichia coli) for the bugs with ESBL results."),
                                           DTOutput("esbl") %>% withSpinner()
                                  ),
                                  tabPanel("K. pneumoniae",
                                           br(),
                                           fluidRow(
                                             column(width = 6,
                                                    htmlOutput("organism_isolates_kp") %>% withSpinner()),
                                             column(width = 6,
                                                    plotOutput("organism_isolates_year_kp", height = "250px") %>% withSpinner()
                                             )
                                           ),
                                           plotOutput("organism_sir_plot_kp") %>% withSpinner(),
                                           DTOutput("organism_sir_table_kp") %>% withSpinner()
                                  ),
                                  tabPanel("S. aureus",
                                           br(),
                                           fluidRow(
                                             column(width = 6,
                                                    htmlOutput("organism_isolates_sa") %>% withSpinner()),
                                             column(width = 6,
                                                    plotOutput("organism_isolates_year_sa", height = "250px") %>% withSpinner()
                                             )
                                           ),
                                           plotOutput("organism_sir_plot_sa") %>% withSpinner(),
                                           DTOutput("organism_sir_table_sa") %>% withSpinner()
                                  ),
                                  tabPanel("S. pneumoniae",
                                           br(),
                                           fluidRow(
                                             column(width = 6,
                                                    htmlOutput("organism_isolates_sp") %>% withSpinner()),
                                             column(width = 6,
                                                    plotOutput("organism_isolates_year_sp", height = "250px") %>% withSpinner()
                                             )
                                           ),
                                           plotOutput("organism_sir_plot_sp") %>% withSpinner(),
                                           DTOutput("organism_sir_table_sp") %>% withSpinner()
                                  ),
                                  tabPanel("Any Organism",
                                           br(),
                                           pickerInput(inputId = "organism", label = NULL, multiple = FALSE,
                                                       choices = all_org_name, selected = all_org_name
                                           ),
                                           br(),
                                           fluidRow(
                                             column(width = 6,
                                                    htmlOutput("organism_isolates_any") %>% withSpinner()),
                                             column(width = 6,
                                                    plotOutput("organism_isolates_year_any", height = "250px") %>% withSpinner()
                                             )
                                           ),
                                           plotOutput("organism_sir_plot_any") %>% withSpinner(),
                                           DTOutput("organism_sir_table_any") %>% withSpinner()
                                  )
                                )
                       ),
                       tabPanel("About", value = "about",
                                includeMarkdown("./www/about.md")
                       )
            )
  )
)