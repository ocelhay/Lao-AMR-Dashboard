shinyServer(
  function(input, output, session) {
    
    # Initiate reactive values.
    data_available <- reactiveVal(TRUE)
    source_data <- reactiveVal("LOMWRU")
    date_generation <- reactiveVal("2018-07-05")
    
    
    # Create a reactive dataframe applying filters to amr
    amr_filt <- reactive({
      amr %>%
        filter(
          age_years >= input$age_patients_selection[1],
          age_years <= input$age_patients_selection[2],
          province %in% input$province_patients_selection,
          spec_year %in% input$year_selection,
          spec_method %in% input$spec_method_selection,
          location %in% input$spec_method_collection
          )
    })
    
    # Create a reactive dataframe applying filters to amr_blood
    amr_blood_filt <- reactive({
      amr_blood %>%
        filter(
          age_years >= input$age_patients_selection[1],
          age_years <= input$age_patients_selection[2],
          province %in% input$province_patients_selection,
          spec_year %in% input$year_selection,
          # spec_method %in% input$spec_method_selection,  # TODO allow to select across the three different blood?
          location %in% input$spec_method_collection
        )
    })
    
    
    
    
    # Render Text on number of specimens
    
    # List of information on the status of data
    output$data_status <- renderText({
      ifelse(data_available(),
             paste0(div(class = "info", icon("info-circle", "fa-2x"), strong("Data uploaded"), tags$ul( 
               tags$li("source: ", source_data()),
               tags$li("generated on the: ", date_generation()),
               tags$li("the dataset contains XXX patients and YYY specimens."))
             )),
             paste0(div(class = "alert", icon("exclamation-triangle", "fa-2x"), strong("There is no data to display,"), " please upload a dataset."))
      )
    })
    
    output$filter_specimens <- renderText({
      paste0("The dataset filtered contains ",
             n_distinct(amr_filt()$patient_id), " patients and ",
             n_distinct(amr_filt()$spec_id), " specimens.")
    })
    
    output$filter_specimens_blood <- renderText({
      paste0("The dataset filtered to blood cultures contains ",
             n_distinct(amr_blood_filt()$patient_id), " patients and ",
             n_distinct(amr_blood_filt()$spec_id), " specimens.")
    })
    
    # Pyramid with selected over total
    output$filter_visual_ratio <- renderHighchart({
      
      df <- data.frame(name = c("Total Patients", "Patients Displayed", "Total Specimens", "Specimens Displayed"),
                       y = c(n_distinct(amr$patient_id), n_distinct(amr_filt()$patient_id),
                             n_distinct(amr$spec_id), n_distinct(amr_filt()$spec_id)),
                       color = c("#d35400", "#2980b9", "#d35400", "#2980b9"),
                       segmentColor = c("#000004", "#3B0F70", "#000004", "#3B0F70")
      )
                       
      
      highchart() %>%
        hc_chart(type = "column") %>% 
        hc_xAxis(categories = df$name) %>% 
        hc_add_series(df, name = "Patients, Filters", showInLegend = FALSE)
    })
    
    
    output$province_specimen_blood <- renderPlot({
      amr_blood_filt() %>% 
        count(province) %>% mutate(province = fct_reorder(province, n, .desc = TRUE)) %>%
        ggplot(aes(x = province, weight = n)) + 
        geom_bar() +
        geom_label(aes(y = n, label = n)) +
        labs(x = NULL, y = "Specimens", title = "Count of Specimens per Patient Province", subtitle = "Blood Culture Only") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              text = element_text(size = 16))
    })
    
    output$hospital_specimen_blood <- renderPlot({
      amr_blood_filt() %>% 
        count(location) %>% mutate(location = fct_reorder(location, n, .desc = TRUE)) %>%
        ggplot(aes(x = location, weight = n)) + 
        geom_bar() +
        geom_label(aes(y = n, label = n)) +
        labs(x = NULL, y = "Specimens", title = "Count of Specimens per Hospital/Service", subtitle = "Blood Culture Only") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              text = element_text(size = 16))
    })
    
    
    output$growth_blood <- renderPlot({
      amr_blood_filt() %>% 
      group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
      mutate(growth = ifelse(org_name == "No growth", "No Growth", "Growth")) %>%
      count(growth) %>%
      ggplot(aes(x = growth, weight = n, fill = growth)) + 
      geom_bar() +
      geom_label(aes(y = n, label = n)) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = NULL, y = "Samples", title = "Growth in Blood Samples", subtitle = "Blood Culture Only") +
      theme(legend.position = "none", text = element_text(size = 14))
    })
      
    
    output$count_organisms_blood <- renderPlot({
      amr_blood_filt() %>% 
        filter(org_name != "No growth", org_name != "unknown") %>%
        count(org_name) %>% mutate(org_name = fct_reorder(org_name, n, .desc = FALSE)) %>%
        ggplot(aes(x = org_name, weight = n)) + 
        geom_bar() +
        coord_flip() +
        labs(x = NULL, y = "Organisms", title = "Count of Organisms", subtitle = "Blood Culture Only") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              text = element_text(size = 16))
    })
    
    output$table_organisms_blood <- renderDataTable({
      amr_blood_filt() %>% 
        filter(org_name != "No growth", org_name != "unknown") %>%
        count(org_name) %>% mutate(org_name = fct_reorder(org_name, n, .desc = FALSE)) %>%
        transmute(Organisms = org_name, Count = n) %>%
        datatable(rownames = FALSE, filter = "none", options = list(pageLength = 15))
    })
    
    output$specimens_method <- renderPlot({
      amr_filt() %>% 
        group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
        count(spec_method) %>% mutate(spec_method = fct_reorder(spec_method, n, .desc = FALSE)) %>%
        ggplot(aes(x = spec_method, weight = n)) + 
        geom_bar() +
        coord_flip() +
        labs(x = NULL, y = "Specimen", title = "Specimen collection method") +
        theme(text = element_text(size = 16))
    })
    
    output$specimens_method_loc <- renderPlot({
      levels_ord <- amr_filt() %>% 
        group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
        count(spec_method, sort = TRUE) %>%
        pull(spec_method)
      
      amr_filt() %>% 
        group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
        group_by(location, spec_method) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        mutate(spec_method = factor(spec_method, levels = levels_ord)) %>%
        ggplot(aes(x = spec_method, weight = n, fill = location)) + 
        geom_bar() +
        coord_flip() +
        labs(x = NULL, y = "Specimen", title = "Specimen collection method") +
        theme(text = element_text(size = 12))
    })
    
    output$isolates_organism_high <- renderPlot({
      amr_filt() %>% 
        group_by(org_name) %>% 
        summarise(isolates = n()) %>%
        filter(isolates > 9) %>%
        arrange(isolates) %>%
        mutate(org_name = factor(org_name, levels = org_name)) %>%
        ggplot(aes(x = org_name, y = isolates, fill = org_name)) +
        geom_bar(stat = "identity") +
        geom_label(aes(label = isolates)) +
        coord_flip() +
        labs(x = NULL, y = NULL, title = "Isolates per Organism") +
        theme(legend.position = "none", text = element_text(size = 15))
    })
    
    output$isolates_organism_low <- renderPlot({
      amr_filt() %>% 
        group_by(org_name) %>% 
        summarise(isolates = n()) %>%
        filter(isolates < 10) %>%
        arrange(isolates) %>%
        mutate(org_name = factor(org_name, levels = org_name)) %>%
        ggplot(aes(x = org_name, y = isolates, fill = org_name)) +
        geom_bar(stat = "identity") +
        geom_label(aes(label = isolates)) +
        coord_flip() +
        labs(x = NULL, y = NULL, title = "Isolates per Organism") +
        theme(legend.position = "none", text = element_text(size = 15))
    })
    
    output$isolates_spec_method <- renderPlot({
      amr_filt() %>% 
        group_by(spec_method) %>% 
        summarise(isolates = n()) %>%
        arrange(isolates) %>%
        mutate(spec_method = factor(spec_method, levels = spec_method)) %>%
        ggplot(aes(x = spec_method, y = isolates, fill = spec_method)) +
        geom_bar(stat = "identity") +
        geom_label(aes(label = isolates)) +
        coord_flip() +
        labs(x = NULL, y = NULL, title = "Isolates per Specimen Method") +
        theme(legend.position = "none", text = element_text(size = 15))
    })
    
    output$organism_isolates_ab <- renderText({
      organism <- "Acinetobacter baumanii"
      
      n <- amr_filt() %>% 
        filter(org_name == organism) %>% 
        pull(spec_id) %>%
        n_distinct()
      
      paste0("There are a total of ", n, " isolates for ", organism)
      # this includes the row for which antibiotic_name is NA
    })
    
    output$organism_sir_plot_ab <- renderPlot({
      
      organism <- "Acinetobacter baumanii"
      
      total_tested <- amr_filt() %>% 
        filter(org_name == organism, !is.na(antibiotic_name)) %>% 
        count(antibiotic_name) %>%
        rename(total = n)
      
      sir_results <- amr_filt() %>% 
        filter(org_name == organism, !is.na(antibiotic_name)) %>% 
        count(antibiotic_name, resistance) %>%
        left_join(total_tested, by = "antibiotic_name") %>%
        mutate(percent = n / total,
               label = paste0(antibiotic_name, " \n (", total, " tested)"),
               resistance = factor(resistance, levels = c("S", "I", "R")))
      
      ggplot(sir_results, 
             aes(x = label, y = percent, fill = resistance)) +
        geom_bar(stat = "identity", color = "gray10", width = 0.06*length(unique(sir_results$antibiotic_name))) +
        labs(x = NULL, y = "Percent", fill = "Status") +
        scale_fill_manual(values = cols_SIR) +
        theme(panel.spacing = unit(2, "lines"), panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 1),
              legend.position = "top", text = element_text(size = 16)) +
        scale_x_discrete() +
        scale_y_continuous(labels = scales::percent)
    })
    
    output$organism_sir_table_ab <- renderDT({
      organism <- "Acinetobacter baumanii"
      
      total_tested <- amr_filt() %>% 
        filter(org_name == organism, !is.na(antibiotic_name)) %>% 
        count(antibiotic_name) %>%
        rename(total = n)
      
      sir_results <- amr_filt() %>% 
        filter(org_name == organism, !is.na(antibiotic_name)) %>% 
        count(antibiotic_name, resistance) %>%
        left_join(total_tested, by = "antibiotic_name") %>%
        mutate(percent = n / total,
               label = paste0(antibiotic_name, " \n (", total, " tested)"),
               resistance = factor(resistance, levels = c("S", "I", "R")))
      
      left_join(sir_results %>%
                  select(antibiotic_name, resistance, percent) %>%
                  spread(resistance, percent, fill = 0, drop = FALSE) %>%
                  rename(`Pct. S` = S, `Pct. I` = I, `Pct. R` = R),
                sir_results %>%
                  select(antibiotic_name, resistance, total) %>%
                  spread(resistance, total, fill = 0, drop = FALSE),
                by = "antibiotic_name") %>%
        select(`Antibiotic` = antibiotic_name, S, `Pct. S`, I, `Pct. I`, R, `Pct. R`) %>%
        datatable(rownames = FALSE, filter = "none") %>%
        formatPercentage("Pct. S", 2) %>%
        formatPercentage("Pct. I", 2) %>%
        formatPercentage("Pct. R", 2) %>%
        formatStyle("Pct. S", background = styleColorBar(data = c(0, 1), cols_SIR[1])) %>%
        formatStyle("Pct. I", background = styleColorBar(data = c(0, 1), cols_SIR[2])) %>%
        formatStyle("Pct. R", background = styleColorBar(data = c(0, 1), cols_SIR[3]))
    })
    
    output$organism_isolates_year_ab <- renderPlot({
      organism <- "Acinetobacter baumanii"
      
      # should we include the one not tested?
      amr_filt() %>% 
        filter(org_name == organism) %>% 
        group_by(spec_year) %>%
        summarise(n = n_distinct(spec_id)) %>%
        ggplot(aes(x= spec_year, y = n, label = n)) +
        geom_line(aes(group = 1)) +
        geom_label(alpha = 1, fill = "grey") +
        theme_minimal(base_size = 16) +
        theme(axis.text.y = element_blank()) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = NULL, y = NULL, title = "Total Isolates per Year")
    })
  }
)