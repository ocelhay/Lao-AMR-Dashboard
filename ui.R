fluidPage(
  theme = shinytheme("spacelab"),
  includeCSS("./www/styles.css"),
  
  sidebarPanel(width = 3,
               h2("LOMWRU"),
               h3("AMR Dashboard"),
               conditionalPanel(condition = "input.tabs == 'welcome' | input.tabs == 'about'",
                                div(class = "imgsolidborder4", img(src = "ecoli_LOMWRU.png", alt = "Antibiotic susceptibility testing of a multi-drug resistant Escherichia coli isolated from the urine of a 51 year old Lao patient with a perinephric abscess. There are no inhibition zones surrounding any of the antibiotic disks, including meropenem (MEM, 12 o’clock position), a ‘last-line’ antibiotic. Whole-genome sequencing confirmed that this isolate was carrying a NDM-5 carbapenemase. Such infections are likely to become more frequent, given the ability of carbapenemases to spread and the increasing availability of meropenem in Laos.")),
                                htmlOutput('ui_ecoli_legend', inline = TRUE)
                                # p("Antibiotic susceptibility testing of a multi-drug resistant", em("Escherichia coli"), "isolated from the urine of a 51 year old Lao patient with a perinephric abscess. There are no inhibition zones surrounding any of the antibiotic disks, including meropenem (MEM, 12 o’clock position), a ‘last-line’ antibiotic. Whole-genome sequencing confirmed that this isolate was carrying a NDM-5 carbapenemase. Such infections are likely to become more frequent, given the ability of carbapenemases to spread and the increasing availability of meropenem in Laos.")),
               ),
               br(),
               
               conditionalPanel(condition = "input.tabs == 'patients' | input.tabs == 'blood_culture' | input.tabs == 'specimens' | input.tabs == 'organisms' | input.tabs == 'amr'",
                                div(class = "border4",
                                    
                                    htmlOutput("data_status_duplicated"),
                                    
                                    h3(icon("filter", "fa-1x"), "Filter the Dataset:"),
                                    
                                    fluidRow(
                                      column(width = 4, p("Patients Age Range:")),
                                      column(width = 8, sliderInput("age_patients_selection", label = NULL, min = 0, max = 100, value = c(0, 100)))
                                    ),
                                    fluidRow(
                                      column(width = 4, p("Patients Province of Residence:")),
                                      column(width = 8, pickerInput(inputId = "province_patients_selection", label = NULL, multiple = TRUE,
                                                                    choices = NULL, selected = NULL, options = list(
                                                                      `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                                      `select-all-text` = "Select All", `none-selected-text` = "None Selected")
                                      ))
                                    ),
                                    hr(),
                                    fluidRow(
                                      column(width = 4, p("Specimens Collection Date:")),
                                      column(width = 8, selectInput("date_range_selection", label = NULL, choices = c("Filter by Year", "Filter by Date Range")),
                                             conditionalPanel("input.date_range_selection == 'Filter by Year'",
                                                              checkboxGroupInput("year_selection", label = NULL, choices = NULL, selected = NULL, inline = TRUE)
                                             ),
                                             conditionalPanel("input.date_range_selection == 'Filter by Date Range'",
                                                              br(), br(),
                                                              dateRangeInput("date_selection", label = NULL)
                                             ))
                                    ),
                                    fluidRow(
                                      column(width = 4, p("Specimens Collection Location:")),
                                      column(width = 8, pickerInput(inputId = "spec_method_collection", label = NULL, multiple = TRUE,
                                                                    choices = NULL, selected = NULL, options = list(
                                                                      `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                                      `select-all-text` = "Select All", `none-selected-text` = "None Selected")
                                      )
                                      )),
                                    conditionalPanel(condition = "input.tabs == 'blood_culture'",
                                                     fluidRow(
                                                       column(width = 4, p("Specimens Collection Method:")),
                                                       column(width = 8, strong("Blood Culture Only"))
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.tabs == 'patients' | input.tabs == 'specimens' | input.tabs == 'organisms' | input.tabs == 'amr'",
                                                     fluidRow(
                                                       column(width = 4, p("Specimens Collection Method:")),
                                                       column(width = 8, pickerInput(inputId = "spec_method_selection", label = NULL, multiple = TRUE,
                                                                                     choices = NULL, selected = NULL, options = list(
                                                                                       `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                                                       `select-all-text` = "Select All", `none-selected-text` = "None Selected")
                                                       )
                                                       )
                                                     )
                                    ),
                                    
                                    conditionalPanel(condition = "input.tabs == 'patients' | input.tabs == 'specimens' | input.tabs == 'organisms' | input.tabs == 'amr'",
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
                                fluidRow(column(width = 2, h3("Language")),
                                         column(width = 10, h3(icon("upload", "fa-1x"), "Upload data"))),
                                fluidRow(
                                  column(width = 2,
                                         div(class = "large",
                                             radioButtons(inputId = "lang", label = NULL,
                                                          choiceNames = c(paste(emo::ji("Laos"), " Lao "), paste(emo::ji("uk"), " English")),
                                                          choiceValues = c("la", "en"),
                                                          selected = "en", inline = FALSE)
                                         )),
                                  column(width = 6,
                                         htmlOutput("data_status")
                                  ),
                                  column(width = 4,
                                         switchInput(inputId = "mock_data_use", label = "Use Mock Dataset", value = FALSE, inline = FALSE, labelWidth = '200px'),
                                         conditionalPanel(condition = "! input.mock_data_use",
                                                          fileInput("file_RData", label = NULL, accept = ".RData", buttonLabel = "Upload Dataset", placeholder = "accept .RData files"))
                                         
                                  )),
                                # includeMarkdown("./www/about_amr.md"),
                                htmlOutput('ui_about_amr', inline = TRUE),
                                htmlOutput('ui_disclaimer_1', inline = TRUE),
                                htmlOutput('ui_disclaimer_2', inline = TRUE),
                                htmlOutput('ui_disclaimer_3', inline = TRUE),
                                htmlOutput('ui_disclaimer_4', inline = TRUE),
                                br(), br(),
                                htmlOutput('ui_disclaimer_5', inline = TRUE)
                                
                                # includeMarkdown("./www/disclaimer.md")
                       ),
                       tabPanel("Patients", value = "patients",
                                h2("Total Patients per Place of Collection"),
                                plotOutput("patients_nb", height = "600px") %>% withSpinner()
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
                                         p("The graph below displays the 25 most commons organisms, report to the table for the complete listing."),
                                         plotOutput("count_organisms_blood", height = "600px") %>% withSpinner()
                                  ),
                                  column(width = 4,
                                         p("Table of all organisms:"),
                                         DTOutput("table_organisms_blood") %>% withSpinner()
                                  )
                                ),
                                br()
                       ),
                       tabPanel("Specimens", value = "specimens",
                                h2("Total Specimens per Specimen Type"),
                                p("Use filters located on the the sidebar to select and display, for example, only specimens collected in a specific hospital."),
                                plotOutput("specimens_method", height = "600px") %>% withSpinner()
                       ),
                       tabPanel("Organisms", value = "organisms",
                                h2("Total Number of Positive Isolates per Specimen"),
                                em("All organisms labelled 'No growth' have been removed from this section."),
                                plotOutput("isolates_method", height = "600px") %>% withSpinner(),
                                h2("Total Number of Isolates by Organism"),
                                em("All organisms labelled 'No growth' have been removed from this section."),
                                fluidRow(
                                  column(width = 8, 
                                         p("The graph below displays the 25 organisms with the more isolates, report to the table for the complete listing."),
                                         plotOutput("isolates_organisms", height = "600px") %>% withSpinner()),
                                  column(width = 4, dataTableOutput("table_isolates_organisms") %>% withSpinner())
                                )
                       ),
                       tabPanel("AMR", value = "amr", icon = icon("bug"),
                                tabsetPanel(
                                  tabPanel("A. baumannii",
                                           br(),
                                           fluidRow(
                                             htmlOutput("organism_isolates_ab"),
                                             column(width = 8,
                                                    h2("Susceptibility Status"),
                                                    highchartOutput("organism_sir_ab", height = "600px") %>% withSpinner()
                                             ),
                                             column(width = 4,
                                                    br()
                                             )
                                           )
                                  ),
                                  tabPanel("E. coli",
                                           br(),
                                           fluidRow(
                                             htmlOutput("organism_isolates_ec"),
                                             column(width = 6,
                                                    h2("Susceptibility Status"),
                                                    highchartOutput("organism_sir_ec", height = "600px") %>% withSpinner()
                                             ),
                                             column(width = 6,
                                                    h2("ESBL Results per quarter"),
                                                    highchartOutput("esbl_ec", height = "600px") %>% withSpinner(),
                                                    em("2017.3 = Third quarter of 2017")
                                             )
                                           )
                                  ),
                                  tabPanel("K. pneumoniae",
                                           br(),
                                           fluidRow(
                                             htmlOutput("organism_isolates_kp"),
                                             column(width = 6,
                                                    h2("Susceptibility Status"),
                                                    highchartOutput("organism_sir_kp", height = "600px") %>% withSpinner()
                                             ),
                                             column(width = 6,
                                                    h2("ESBL Results per quarter"),
                                                    highchartOutput("esbl_kp", height = "600px") %>% withSpinner(),
                                                    em("2017.3 = Third quarter of 2017")
                                             )
                                           )
                                  ),
                                  tabPanel("S. aureus",
                                           br(),
                                           fluidRow(
                                             htmlOutput("organism_isolates_sa"),
                                             column(width = 6,
                                                    h2("Susceptibility Status"),
                                                    highchartOutput("organism_sir_sa", height = "600px") %>% withSpinner()
                                             ),
                                             column(width = 6,
                                                    h2("Cefoxitin MRSA per quarter"),
                                                    highchartOutput("organism_mrsa_sa", height = "600px") %>% withSpinner(),
                                                    em("2017.3 = Third quarter of 2017")
                                             )
                                           )
                                  ),
                                  tabPanel("S. pneumoniae",
                                           br(),
                                           fluidRow(
                                             htmlOutput("organism_isolates_sp"),
                                             column(width = 8,
                                                    h2("Susceptibility Status"),
                                                    highchartOutput("organism_sir_sp", height = "600px") %>% withSpinner()
                                             ),
                                             column(width = 4,
                                                    br()
                                             )
                                           )
                                  ),
                                  tabPanel("Salmonella Typhi",
                                           br(),
                                           fluidRow(
                                             htmlOutput("organism_isolates_st"),
                                             column(width = 8,
                                                    h2("Susceptibility Status"),
                                                    highchartOutput("organism_sir_st", height = "600px") %>% withSpinner()
                                             ),
                                             column(width = 4,
                                                    br()
                                             )
                                           )
                                  ),
                                  tabPanel("Neisseria gonorrhoeae",
                                           br(),
                                           fluidRow(
                                             htmlOutput("organism_isolates_ng"),
                                             column(width = 8,
                                                    h2("Susceptibility Status"),
                                                    highchartOutput("organism_sir_ng", height = "600px") %>% withSpinner()
                                             ),
                                             column(width = 4,
                                                    br()
                                             )
                                           )
                                  ),
                                  tabPanel("All Organisms",
                                           br(),
                                           pickerInput(inputId = "organism", label = NULL, multiple = FALSE,
                                                       choices = NULL, selected = NULL
                                           ),
                                           br(),
                                           fluidRow(
                                             htmlOutput("organism_isolates_all"),
                                             column(width = 8,
                                                    h2("Susceptibility Status"),
                                                    highchartOutput("organism_sir_all", height = "600px") %>% withSpinner()
                                             ),
                                             column(width = 4,
                                                    br()
                                             )
                                           )
                                  )
                                )
                       ),
                       tabPanel("About", value = "about",
                                a(href = "http://www.tropmedres.ac/home", img(src = "MORU_logo.jpg", height = "100px")),
                                includeMarkdown("./www/about.md")
                       )
            )
  )
)