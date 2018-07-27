# Escherichia coli ----------------------------------------------------------------------------------------------------------

output$organism_isolates_ec <- renderText({
  req(data_available())
  organism <- "Escherichia coli"
  
  df <- amr_filt() %>% 
    filter(org_name == organism) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for ", organism, ".")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_ec <- renderHighchart({
  req(data_available())
  
  organism <- "Escherichia coli"
  
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

output$esbl_ec <- renderHighchart({
  req(data_available())
  organism <- "Escherichia coli"
  
  ifelse(input$esbl_ec_na == TRUE,
         df1 <- amr_filt() %>% 
           filter(org_name == organism, !is.na(antibiotic_name))
         , 
         df1 <- amr_filt() %>% 
           filter(org_name == organism, !is.na(antibiotic_name), esbl != "Unknown")
  )
  
  total_tested <- df1 %>% 
    count(antibiotic_name) %>%
    rename(total_org = n)
  
  df <- df1 %>% 
    group_by(antibiotic_name, esbl) %>%
    count() %>%
    ungroup() %>%
    left_join(total_tested, by = "antibiotic_name") %>%
    mutate(percent = round(100*n / total_org, 1)) %>%
    mutate(esbl = factor(esbl, levels = c("Negative", "Positive", "Unknown"))) %>%
    complete(esbl, nesting(antibiotic_name), fill = list(n = 0))
  
  
  hchart(df, type = "bar", hcaes(x = "antibiotic_name", y = "percent", group = "esbl")) %>%
    hc_yAxis(title = "", max = 100) %>% hc_xAxis(title = "") %>%
    hc_colors(cols_esbl) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = "<b>{point.antibiotic_name}</b><br> {point.esbl}: {point.percent}% ({point.n} of {point.total_org}.)") %>%
    hc_plotOptions(series = list(stacking = 'normal'))
  
})