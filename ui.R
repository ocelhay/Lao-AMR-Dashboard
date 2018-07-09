fluidPage(
  theme = shinytheme("spacelab"),
  includeCSS("./www/styles.css"),
  
  sidebarPanel(width = 3,
               h2("LOMWRU"),
               h3("AMR Dashboard"),
               div(class = "imgsolidborder4",
                   img(src = "ecoli.jpg")
               ),
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
                                                                                       `actions-box` = TRUE,
                                                                                       `deselect-all-text` = "None...",
                                                                                       `select-all-text` = "Select All",
                                                                                       `none-selected-text` = "zero"
                                                                                     )
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
                                                                               dateRangeInput("date_selection", label = NULL)
                                                              ))
                                                     ),
                                                     fluidRow(
                                                       column(width = 4, p("Specimens Collection Location:")),
                                                       column(width = 8, pickerInput(inputId = "spec_method_collection", label = NULL, multiple = TRUE,
                                                                                     choices = all_locations, selected = all_locations, options = list(
                                                                                       `actions-box` = TRUE,
                                                                                       `deselect-all-text` = "None...",
                                                                                       `select-all-text` = "Select All",
                                                                                       `none-selected-text` = "zero"
                                                                                     )
                                                       ))
                                                     )
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
                                                                                       `actions-box` = TRUE,
                                                                                       `deselect-all-text` = "None...",
                                                                                       `select-all-text` = "Select All",
                                                                                       `none-selected-text` = "zero"
                                                                                     )
                                                       ))
                                                     )
                                    ),
                                    
                                    conditionalPanel(condition = "input.tabs == 'specimens' | input.tabs == 'organisms' | input.tabs == 'amr'",
                                                     textOutput("filter_specimens"),
                                                     highchartOutput("filter_visual_ratio")
                                    ),
                                    conditionalPanel(condition = "input.tabs == 'blood_culture'",
                                                     textOutput("filter_specimens_blood")
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
                                         p("The graph below displays only the 20 more commons organisms, report to table for the complete listing."),
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
                                tabsetPanel(
                                  tabPanel("Organisms Plot", value = "organisms_plot",
                                           p("placeholder-dropdown with selection of organism groups"),
                                           
                                           plotOutput("isolates_organism_high") %>% withSpinner(),
                                           
                                           plotOutput("isolates_organism_low") %>% withSpinner(),
                                           
                                           p("placeholder-barplot with nb of isolates per organism")
                                  ),
                                  tabPanel("Species Summary", value = "species_summary",
                                           p("placeholder-dropdown with selection ofspecies"),
                                           p("placeholder-barplot with isolates per age group"),
                                           p("placeholder-barplot with specimen counts per specimen group"),
                                           plotOutput("isolates_spec_method") %>% withSpinner()
                                  )
                                )
                       ),
                       tabPanel("AMR", value = "amr", icon = icon("bug"),
                                tabsetPanel(
                                  tabPanel("A. baumannii",
                                           textOutput("organism_isolates_ab") %>% withSpinner(),
                                           
                                           plotOutput("organism_sir_plot_ab") %>% withSpinner(),
                                           
                                           DTOutput("organism_sir_table_ab") %>% withSpinner(),
                                           
                                           plotOutput("organism_isolates_year_ab") %>% withSpinner(),
                                           
                                           p("placeholder-text with total number of isolates"),
                                           p("placeholder-barplot with SIR per antibiotic"),
                                           p("placeholder-tale with SIR per antibiotic"),
                                           p("placeholder-plot with total isolates per year"),
                                           p("placeholder-plot with evolution of SIR per year")
                                  ),
                                  tabPanel("E. coli",
                                           p("placeholder-text with total number of isolates"),
                                           p("placeholder-barplot with SIR per antibiotic"),
                                           p("placeholder-tale with SIR per antibiotic"),
                                           p("placeholder-plot with total isolates per year"),
                                           p("placeholder-plot with evolution of SIR per year"),
                                           p("placeholder ****NEW****—add the ratio (ESBL positive for Escherichia coli)/(Patients with Escherichia coli) for the bugs with ESBL results.")
                                  ),
                                  tabPanel("K. pneumoniae",
                                           p("placeholder-text with total number of isolates"),
                                           p("placeholder-barplot with SIR per antibiotic"),
                                           p("placeholder-tale with SIR per antibiotic"),
                                           p("placeholder-plot with total isolates per year"),
                                           p("placeholder-plot with evolution of SIR per year")
                                  ),
                                  tabPanel("S. aureus",
                                           p("placeholder-text with total number of isolates"),
                                           p("placeholder-barplot with SIR per antibiotic"),
                                           p("placeholder-tale with SIR per antibiotic"),
                                           p("placeholder-plot with total isolates per year"),
                                           p("placeholder-plot with evolution of SIR per year")
                                  ),
                                  tabPanel("S. pneumoniae",
                                           p("placeholder-text with total number of isolates"),
                                           p("placeholder-barplot with SIR per antibiotic"),
                                           p("placeholder-tale with SIR per antibiotic"),
                                           p("placeholder-plot with total isolates per year"),
                                           p("placeholder-plot with evolution of SIR per year")
                                  ),
                                  tabPanel("Any Organism",
                                           p("placeholder-selection of a species"),
                                           p("placeholder-text with total number of isolates"),
                                           p("placeholder-barplot with SIR per antibiotic"),
                                           p("placeholder-tale with SIR per antibiotic"),
                                           p("placeholder-plot with total isolates per year"),
                                           p("placeholder-plot with evolution of SIR per year")
                                  )
                                )
                       ),
                       tabPanel("About", value = "about",
                                includeMarkdown("./www/about.md")
                       )
            )
  )
)