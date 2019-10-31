# "Shigella species" ------------------------------------------------

output$organism_isolates_shig <- renderText({
  req(data_available())
  
  orga <- data$amr$org_name %>% unique() 
  organism <- orga[orga %>% startsWith("Shig")]
  
  df <- amr_filt() %>% 
    filter(org_name %in% organism) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for ", paste0(organism, collapse = ", "), ".")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_shig <- renderHighchart({
  req(data_available())
  
  # rename all shigella organisms to Shigella spp.
  data <- amr_filt()
  orga <- data$org_name %>% unique()
  organism <- "Shigella spp."
  data$org_name <- str_replace_all(data$org_name, orga[orga %>% startsWith("Shig")], organism)
  
  highchart_sir(data = data, organism = organism)
})


# Ciprofloxacin Status ---------------------------------------------------------------------------------------------------------------

output$ciproflaxin_shig <- renderHighchart({
  req(data_available())
  
  orga <- data$amr$org_name %>% unique() 
  organism <- orga[orga %>% startsWith("Shig")]
  
  highchart_sir_evolution(data = amr_filt(), organism = organism, 
                          antibiotic_vec = "Ciprofloxacin",
                          levels = c("Ciprofloxacin-susceptible", "Ciprofloxacin-intermediate", "Ciprofloxacin-resistant", "Not Tested"))
})

