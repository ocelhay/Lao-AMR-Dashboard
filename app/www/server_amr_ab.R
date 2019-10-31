# "Acinetobacter species" ------------------------------------------------

output$organism_isolates_ab <- renderText({
  req(data_available())
  organism <- "Acinetobacter species"
  
  df <- amr_filt() %>% 
    filter(org_name == organism) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for ", organism, ".")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_ab <- renderHighchart({
  req(data_available())
  
  highchart_sir(data = amr_filt(), organism = "Acinetobacter baumanii")
})


# Carbapenem Status ---------------------------------------------------------------------------------------------------------------

output$carbapenem_ab <- renderHighchart({
  req(data_available())
  
  highchart_sir_evolution(data = amr_filt(), organism = "Acinetobacter species", 
                          antibiotic_vec = c("Imipenem", "Meropenem"),
                          levels = c("Carbapenem-susceptible", "Carbapenem-intermediate", "Carbapenem-resistant", "Not Tested"))
})

