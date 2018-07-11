library(DT)
library(highcharter)
library(lubridate)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

cols_SIR <- c("S" = "#2166ac", "I" = "#fddbc7", "R" = "#b2182b", "Not Tested" = "light grey")

all_provinces <- ""
all_locations <- ""
all_org_name <- ""
all_spec_method <- ""
all_spec_year <- 999
oldest_patient <- 999