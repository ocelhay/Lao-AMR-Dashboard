library(DT)
library(highcharter)
library(lubridate)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# This will be replaced in production with the requirement to upload data
load("../App Elements/AMR_data_LOMWRU_Latest.RData")

amr <- data$amr

amr_blood <- amr %>%
  filter(spec_method %in% c("Clotted blood", "EDTA blood", "Haemoculture"))


all_provinces <- sort(unique(amr$province))
all_locations <- sort(unique(amr$location))
all_org_name <- sort(unique(amr$org_name))
all_spec_method <- sort(unique(amr$spec_method))
all_spec_year <- unique(amr$spec_year)

oldest_patient <- max(amr$age_years, na.rm = TRUE)

cols_SIR <- c("S" = "#2166ac", "I" = "#fddbc7", "R" = "#b2182b", "Not Tested" = "light grey")