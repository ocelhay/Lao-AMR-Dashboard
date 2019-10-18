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
  
  highchart_sir(data = amr_filt(), organism = "Salmonella Typhi")
})