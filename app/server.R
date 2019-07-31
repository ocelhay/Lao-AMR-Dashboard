shinyServer(
  function(input, output, session) {
    
    # Translator
    i18n <- reactive({
      selected <- input$lang
      if (length(selected) > 0 && selected %in% translator$languages) {
        translator$set_translation_language(selected)
      }
      translator
    })
    
    # Report
    # https://stackoverflow.com/questions/51050306/have-downloadbutton-work-with-observeevent
    feedback_download <- reactiveValues(download_flag = 0)
    
    output$report <- downloadHandler(
      filename = "AMR Report.pdf",
      content = function(file) {
        feedback_download$download_flag <- feedback_download$download_flag + 1
        if(feedback_download$download_flag > 0) {
          showNotification(HTML("Generation of the report typically takes 10 to 30 seconds"), duration = NULL, type = "message", id = "report_generation", session = session)
        }
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("./www/report/report.Rmd", tempReport, overwrite = TRUE)
        rmarkdown::render(tempReport, output_file = file)
        removeNotification(id = "report_generation", session = session)
        showNotification(HTML("Report Generated"), duration = 4, type = "message", id = "report_generated", session = session)
      }
    )
    
    # Elements of the UI
    
    output$ui_ecoli_legend <- renderText({
       HTML(i18n()$t("ecoli_legend"))
      })
    
    output$ui_about_amr <- renderText({
      HTML(i18n()$t("about_amr"))
    })
    
    output$ui_disclaimer_1 <- renderText({
      HTML(i18n()$t("disclaimer_1"))
    })
    
    output$ui_disclaimer_2 <- renderText({
      HTML(i18n()$t("disclaimer_2"))
    })
    
    output$ui_disclaimer_3 <- renderText({
      HTML(i18n()$t("disclaimer_3"))
    })
    
    output$ui_disclaimer_4 <- renderText({
      HTML(i18n()$t("disclaimer_4"))
    })
    
    output$ui_disclaimer_5 <- renderText({
      HTML(i18n()$t("disclaimer_5"))
    })
    
    
    # Initiate reactive values
    data_available <- reactiveVal(FALSE)
    amr <- reactiveVal(NULL)
    source_data <- reactiveVal(NULL)
    date_generation <- reactiveVal(NULL)
    
    # Hide several tabs at the launch of the app
    observeEvent(NULL, {
      hideTab(inputId = "tabs", target = "blood_culture")
      hideTab(inputId = "tabs", target = "patients")
      hideTab(inputId = "tabs", target = "specimens")
      hideTab(inputId = "tabs", target = "organisms")
      hideTab(inputId = "tabs", target = "amr")
    }, ignoreNULL = FALSE)
    
    # Load .Rdata from input and update reactive values.
    observeEvent({
      input$file_RData
      input$mock_data_use},
      {
        if (input$mock_data_use == TRUE){
          load("./www/AMR_mock_data_demo.RData", envir = .GlobalEnv)
        }
        
        if (input$mock_data_use == FALSE){
          # Reinitiate the app
          data_available(FALSE)
          amr(NULL)
          source_data(NULL)
          date_generation(NULL)
          hideTab(inputId = "tabs", target = "blood_culture")
          hideTab(inputId = "tabs", target = "patients")
          hideTab(inputId = "tabs", target = "specimens")
          hideTab(inputId = "tabs", target = "organisms")
          hideTab(inputId = "tabs", target = "amr")

          if (is.null(input$file_RData)) return(NULL)
          inFile <- input$file_RData
          file <- inFile$datapath
          load(file, envir = .GlobalEnv)
        }
        
        # Update reactive values
        data_available(TRUE)
        amr(data$amr)
        source_data(data$source_data)
        date_generation(data$date_generation)
        
        # Show Tabs
        showTab(inputId = "tabs", target = "blood_culture")
        showTab(inputId = "tabs", target = "patients")
        showTab(inputId = "tabs", target = "specimens")
        showTab(inputId = "tabs", target = "organisms")
        showTab(inputId = "tabs", target = "amr")
        
        # Update elements for the UI
        all_provinces <- sort(unique(amr()$province))
        all_locations <- sort(unique(amr()$location))
        all_org_name <- sort(unique(amr()$org_name))
        all_spec_method <- sort(unique(amr()$spec_method))
        all_spec_year <- unique(amr()$spec_year)
        oldest_patient <- max(amr()$age_years, na.rm = TRUE)
        min_collection_date <- min(amr()$spec_date)
        max_collection_date <- max(amr()$spec_date)
        
        updateSliderInput(session = session, "age_patients_selection", max = oldest_patient, value = c(0, oldest_patient))
        updatePickerInput(session = session, "province_patients_selection", choices = all_provinces, selected = all_provinces)
        updateCheckboxGroupInput(session = session, "year_selection", choices = all_spec_year, selected = all_spec_year)
        updateDateRangeInput(session = session, "date_selection", start = min_collection_date, end = max_collection_date)
        updatePickerInput(session = session, "spec_method_collection", choices = all_locations, selected = all_locations)
        updatePickerInput(session = session, "spec_method_selection", choices = all_spec_method, selected = all_spec_method)
        updatePickerInput(session = session, "organism", choices = all_org_name, selected = all_org_name)
    },
    ignoreInit = TRUE)
    
    
    # Create a reactive dataframe applying filters to amr
    amr_filt <- reactive({
      
      if(input$date_range_selection == "Filter by Year"){
        return(
          amr() %>%
            filter(
              age_years >= input$age_patients_selection[1] | is.na(age_years),
              age_years <= input$age_patients_selection[2] | is.na(age_years),
              province %in% input$province_patients_selection | is.na(province),
              spec_year %in% input$year_selection | is.na(spec_year),
              spec_method %in% input$spec_method_selection | is.na(spec_method),
              location %in% input$spec_method_collection | is.na(location)
            )
        )
      }
      
      if(input$date_range_selection == "Filter by Date Range"){
        return(
          amr() %>%
            filter(
              age_years >= input$age_patients_selection[1] | is.na(age_years),
              age_years <= input$age_patients_selection[2] | is.na(age_years),
              province %in% input$province_patients_selection | is.na(province),
              spec_date >= input$date_selection[1] | is.na(spec_date),
              spec_date <= input$date_selection[2] | is.na(spec_date),
              spec_method %in% input$spec_method_selection | is.na(spec_method),
              location %in% input$spec_method_collection | is.na(location)
            )
        )
      }
    })
    
    amr_blood <- reactive({
      amr() %>%
        filter(spec_method == "Blood culture")
    })
    
    
    # Create a reactive dataframe applying filters to amr_blood
    amr_blood_filt <- reactive({
      
      if(input$date_range_selection == "Filter by Year"){
        return(
          amr() %>%
            filter(spec_method == "Blood culture") %>%
            filter(
              age_years >= input$age_patients_selection[1] | is.na(age_years),
              age_years <= input$age_patients_selection[2] | is.na(age_years),
              province %in% input$province_patients_selection | is.na(province),
              spec_year %in% input$year_selection | is.na(spec_year),
              location %in% input$spec_method_collection  | is.na(spec_method)
            )
        )
      }
      
      if(input$date_range_selection == "Filter by Date Range"){
        return(
          amr() %>%
            filter(spec_method == "Blood culture") %>%
            filter(
              age_years >= input$age_patients_selection[1] | is.na(age_years),
              age_years <= input$age_patients_selection[2] | is.na(age_years),
              province %in% input$province_patients_selection | is.na(province),
              spec_date >= input$date_selection[1] | is.na(spec_date),
              spec_date <= input$date_selection[2] | is.na(spec_date),
              location %in% input$spec_method_collection  | is.na(spec_method)
            )
        )
      }
    })
    
    
    
    
    # Render Text on number of specimens
    
    # List of information on the status of data
    output$data_status <- renderText({
      
      ifelse(data_available(),
             paste0(div(class = "info2", h4(icon("info-circle", "fa-1x"), "Data uploaded"), tags$ul( 
               tags$li("Dataset: ", source_data()),
               tags$li("Dataset generated on the ", date_generation()),
               tags$li("Dataset contains ", n_distinct(amr()$patient_id), " patients", " and ", n_distinct(amr()$spec_id)," specimens."))
             )),
             paste0(div(class = "alert", h4(icon("exclamation-triangle", "fa-1x"), "There is no data to display"), "Upload a dataset provided by LOMWRU or 'Use Mock Dataset'."))
      )
    })
    
    output$data_status_duplicated <- renderText({
      
      ifelse(data_available(),
             paste0(div(class = "info2", h4(icon("info-circle", "fa-1x"), "Data uploaded"), tags$ul( 
               tags$li("Dataset: ", source_data()),
               tags$li("Dataset generated on the ", date_generation()),
               tags$li("Dataset contains ", n_distinct(amr()$patient_id), " patients", " and ", n_distinct(amr()$spec_id)," specimens."))
             )),
             paste0(div(class = "alert", h4(icon("exclamation-triangle", "fa-1x"), "There is no data to display,"), br(), "Upload a dataset provided by LOMWRU or 'Use Mock Dataset'."))
      )
    })
    
    
    output$filter_text <- renderText({
      req(data_available())
      
      n_patients_start <- n_distinct(amr()$patient_id)
      n_patients_end <- n_distinct(amr_filt()$patient_id)
      n_specimens_start <- n_distinct(amr()$spec_id)
      n_specimens_end <- n_distinct(amr_filt()$spec_id)
      
      if(n_patients_start != n_patients_end | (n_specimens_start != n_specimens_end)){
        return(
          # paste0(div(class = "info", icon("info-circle", "fa-1x"), strong("Dataset has not been filtered"), tags$ul( 
          #   tags$li("There are ", n_patients_start, " patients."),
          #   tags$li("There are ", n_specimens_start," specimens.")
          # )
          paste0(div(class = "alert", icon("filter", "fa-1x"), strong("Dataset is filtered"), tags$ul(
                       tags$li("There are ", n_patients_end, " of the ", n_patients_start, " patients."),
                       tags$li("There are ", n_specimens_end, " of the ", n_specimens_start, " specimens.")
                     )
          ))
          )
      }
      
      return(NULL)
    })
    
    
    source("www/server_tab_blood.R", local = TRUE)
    
    
    
    # Patient Tab ---------------------------------------------------------------------------------------------------------------
    
    output$patients_nb <- renderPlot({
      req(nrow(amr_filt()) > 0)
      
      amr_filt() %>% 
        group_by(location) %>% summarise(count = n_distinct(patient_id)) %>% ungroup() %>%
        mutate(location = fct_reorder(location, count, .desc = FALSE)) %>%
        ggplot(aes(x = location, weight = count)) + 
        geom_bar() +
        geom_label(aes(y = count, label = count)) +
        coord_flip() +
        labs(x = NULL, y = "Total Patients", x = "Collection Place") +
        theme_minimal(base_size = 16)
    })
    
    
    # Specimens Tab -------------------------------------------------------------------------------------------------------------
    
    output$specimens_method <- renderPlot({
      req(nrow(amr_filt()) > 0)
      
      amr_filt() %>% 
        group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
        count(spec_method) %>% mutate(spec_method = fct_reorder(spec_method, n, .desc = FALSE)) %>%
        ggplot(aes(x = spec_method, weight = n)) + 
        geom_bar() +
        geom_label(aes(y = n, label = n)) +
        coord_flip() +
        labs(x = NULL, y = "Total Specimens", x = "Collection Method") +
        theme_minimal(base_size = 16)
    })
    
    
    # Organisms tab -----------------------------------------------------------
    output$isolates_method <- renderPlot({
      req(nrow(amr_filt()) > 0)
      
      amr_filt() %>% 
        filter(org_name != "No growth") %>%
        count(spec_method) %>% mutate(spec_method = fct_reorder(spec_method, n, .desc = FALSE)) %>%
        ggplot(aes(x = spec_method, weight = n)) + 
        geom_bar() +
        geom_label(aes(y = n, label = n)) +
        coord_flip() +
        labs(x = NULL, y = "Total Isolates", x = "Collection Method") +
        theme_minimal(base_size = 16)
    })
    
    
    output$isolates_organisms <- renderPlot({
      req(nrow(amr_filt()) > 0)
      
      amr_filt() %>%
        filter(org_name != "No growth") %>%
        group_by(org_name) %>% 
        count() %>%
        ungroup() %>%
        arrange(desc(n)) %>%
        head(n = 25) %>%
        mutate(org_name = fct_reorder(org_name, n, .desc = FALSE)) %>%
        ggplot(aes(x = org_name, weight = n)) + 
        geom_bar() +
        geom_label(aes(y = n, label = n)) +
        labs(x = NULL, y = "Total Isolates") +
        coord_flip() +
        theme_minimal(base_size = 15) +
        theme(axis.text = element_text(face = 'italic'))
    })
    
    output$table_isolates_organisms <- renderDT({
      req(nrow(amr_filt()) > 0)
      
      amr_filt() %>%
        filter(org_name != "No growth") %>%
        group_by(org_name) %>% 
        count() %>%
        ungroup() %>%
        arrange(desc(n)) %>%
        transmute(Organisms = paste0('<em>', org_name, '</em>'), Count = n) %>%
        datatable(rownames = FALSE, filter = "none", escape = FALSE, options = list(pageLength = 100, dom = 'ft'))
    })
    
    # AMR Tab -----------------------------------------------------------------
    
    source("www/server_amr_ab.R", local = TRUE)
    source("www/server_amr_ec.R", local = TRUE)
    source("www/server_amr_kp.R", local = TRUE)
    source("www/server_amr_sa.R", local = TRUE)
    source("www/server_amr_sp.R", local = TRUE)
    source("www/server_amr_st.R", local = TRUE)
    source("www/server_amr_ng.R", local = TRUE)
    source("www/server_amr_any.R", local = TRUE)
  }
)
