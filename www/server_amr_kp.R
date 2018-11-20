
# Klebsiella pneumoniae -----------------------------------------------------------------------------------------------------



output$organism_isolates_kp <- renderText({
  req(data_available())
  organism <- "Klebsiella pneumoniae"
  
  df <- amr_filt() %>% 
    filter(org_name == organism) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for ", organism, ".")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_kp <- renderHighchart({
  req(data_available())
  
  organism <- "Klebsiella pneumoniae"
  
  total_tested <- amr_filt() %>% 
    filter(org_name == organism, !is.na(antibiotic_name)) %>% 
    count(antibiotic_name) %>%
    rename(total_org = n)
  
  sir_results <- amr_filt() %>% 
    filter(org_name == organism, !is.na(antibiotic_name)) %>% 
    count(antibiotic_name, resistance) %>%
    left_join(total_tested, by = "antibiotic_name") %>%
    mutate(percent = round(100*n / total_org, 1),
           resistance = case_when(
             resistance == "S" ~ "Susceptible",
             resistance == "I" ~ "Intermediate",
             resistance == "R" ~ "Resistant",
             TRUE ~ "Unknown")) %>%
    mutate(resistance = factor(resistance, levels = c("Susceptible", "Intermediate", "Resistant", "Unknown"))) %>%
    complete(resistance, nesting(antibiotic_name))
  
  return(
    hchart(sir_results, type = "bar", hcaes(x = "antibiotic_name", y = "percent", group = "resistance")) %>%
      hc_yAxis(title = "", max = 100) %>% hc_xAxis(title = "") %>%
      hc_colors(cols_sir) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "<b>{point.antibiotic_name}</b><br> {point.resistance}: {point.percent}% <br>({point.n} of {point.total_org} tested.)") %>%
      hc_plotOptions(series = list(stacking = 'normal'))
  )
})



# ESBL Status ---------------------------------------------------------------------------------------------------------------

output$esbl_kp <- renderHighchart({
  req(data_available())
  
  organism <- "Klebsiella pneumoniae"
  
  total_tested <- amr_filt() %>% 
    filter(org_name == organism) %>% 
    mutate(spec_quarter = round_date(spec_date, "3 months")) %>%
    group_by(spec_quarter) %>%
    summarise(total2 = n_distinct(spec_id)) %>%
    ungroup()
  
  esbl_results <- amr_filt() %>% 
    filter(org_name == organism) %>% 
    mutate(spec_quarter = round_date(spec_date, "3 months")) %>%
    group_by(spec_quarter, spec_id) %>%
    filter(row_number(spec_id) == 1) %>%
    ungroup() %>%
    group_by(spec_quarter, esbl) %>%
    count() %>%
    ungroup() %>%
    left_join(total_tested, by = "spec_quarter") %>%
    mutate(percent = round(100*n / total2, 1),
           resistance = case_when(
             esbl == "Positive" ~ "ESBL Klebsiella pneumoniae",
             esbl == "Negative" ~ "Susceptible Klebsiella pneumoniae",
             TRUE ~ "Unknown")) %>%
    mutate(resistance = factor(resistance, levels = c("Susceptible Klebsiella pneumoniae", "ESBL Klebsiella pneumoniae", "Unknown"))) %>%
    complete(resistance, nesting(spec_quarter)) %>%
    mutate(spec_quarter = as.character(quarter(spec_quarter, with_year = TRUE)))
  
  return(
    hchart(esbl_results, type = "column", hcaes(x = "spec_quarter", y = "percent", group = "resistance")) %>%
      hc_yAxis(title = "", max = 100) %>% hc_xAxis(title = "") %>%
      hc_colors(cols_sir[c(1, 3, 4)]) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "<b>{point.spec_quarter}</b><br> {point.resistance}: {point.percent}% <br>({point.n} of {point.total2} tested.)") %>%
      hc_plotOptions(series = list(stacking = 'normal'))
  )
})