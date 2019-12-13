# "Acinetobacter species" ------------------------------------------------

output$organism_isolates_ab <- renderText({
  req(data_available())
  
  orga <- data$amr$org_name %>% unique() 
  organism <- orga[orga %>% startsWith("Acinetobacter")]
  
  df <- amr_filt() %>% 
    filter(org_name %in% organism) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for Acinetobacter species.")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_ab <- renderHighchart({
  req(data_available())
  
  # rename all Acinetobacter organisms to Acinetobacter species
  data <- amr_filt()
  # shiny_data <<- data
  # data <- shiny_data
  orga <- data$org_name %>% unique()
  organism <- "Acinetobacter species"
  data$org_name <- replace(data$org_name, which(data$org_name %>% startsWith("Acinetobacter")), organism)
  
  highchart_sir(data = data, organism = organism)
})


# Carbapenem Status ---------------------------------------------------------------------------------------------------------------

output$carbapenem_ab <- renderHighchart({
  req(data_available())
  
  orga <- data$amr$org_name %>% unique() 
  organism <- orga[orga %>% startsWith("Acinetobacter")]
  
  highchart_sir_evolution(data = amr_filt(), organism = organism, 
                          antibiotic_vec = c("Imipenem", "Meropenem"),
                          levels = c("Carbapenem-susceptible", "Carbapenem-intermediate", "Carbapenem-resistant", "Not Tested"))
})

