# Salmonella Typhi ----------------------------------------------------------------------------------------------------------


output$organism_isolates_st <- renderText({
  req(data_available())
  organism <- "Salmonella Typhi"
  
  df <- amr_filt() %>% 
    filter(org_name == organism) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for ", organism, ".")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_st <- renderHighchart({
  req(data_available())
  organism <- "Salmonella Typhi"
  
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
  
  
  hchart(sir_results, type = "bar", hcaes(x = "antibiotic_name", y = "percent", group = "resistance")) %>%
    hc_yAxis(title = "", max = 100) %>% hc_xAxis(title = "") %>%
    hc_colors(cols_sir) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = "<b>{point.antibiotic_name}</b><br> {point.resistance}: {point.percent}% <br>({point.n} of {point.total_org} tested.)") %>%
    hc_plotOptions(series = list(stacking = 'normal'))
})