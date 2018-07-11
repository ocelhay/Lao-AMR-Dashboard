# "Acinetobacter baumanii" ------------------------------------------------

output$organism_isolates_ab <- renderText({
  req(data_available())
  
  organism <- "Acinetobacter baumanii"
  
  n <- amr_filt() %>% 
    filter(org_name == organism) %>% 
    pull(spec_id) %>%
    n_distinct()
  
  paste(h4(paste0("There are a total of ", n, " isolates for ", organism)))
  # this includes the row for which antibiotic_name is NA
})

output$organism_sir_plot_ab <- renderPlot({
  req(data_available())
  
  organism <- "Acinetobacter baumanii"
  
  total_tested <- amr_filt() %>% 
    filter(org_name == organism, !is.na(antibiotic_name)) %>% 
    count(antibiotic_name) %>%
    dplyr::rename(total = n)
  
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
          legend.position = "top", text = element_text(size = 17)) +
    scale_x_discrete() +
    scale_y_continuous(labels = scales::percent)
})

output$organism_sir_table_ab <- renderDT({
  req(data_available())
  
  organism <- "Acinetobacter baumanii"
  
  total_tested <- amr_filt() %>% 
    filter(org_name == organism, !is.na(antibiotic_name)) %>% 
    count(antibiotic_name) %>%
    dplyr::rename(total = n)
  
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
              dplyr::rename(`Pct. S` = S, `Pct. I` = I, `Pct. R` = R),
            sir_results %>%
              select(antibiotic_name, resistance, n) %>%
              spread(resistance, n, fill = 0, drop = FALSE),
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
  req(data_available())
  
  organism <- "Acinetobacter baumanii"
  
  amr_filt() %>% 
    filter(org_name == organism) %>% 
    group_by(spec_year) %>%
    summarise(n = n_distinct(spec_id)) %>%
    mutate(spec_year = as.character(spec_year)) %>%
    ggplot(aes(x = spec_year, weight = n, group = spec_year)) + 
    geom_bar(width = 0.2) +
    geom_label(aes(y = n, label = n)) +
    # scale_fill_brewer(palette = "Set2") +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_minimal(base_size = 18) +
    theme(legend.position = "none", 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
})