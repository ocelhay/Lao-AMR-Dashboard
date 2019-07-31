# Load packages
library(DT)
library(emo)
library(highcharter)
library(lubridate)
library(shiny)
library(shiny.i18n)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(viridisLite)

# rm(list = ls())

# Colors order: S, I, R, Not Tested
cols_sir <- c("#2166ac", "#fddbc7", "#b2182b", "#969696")
cols_sir_ec_kp <- c("#2166ac", "#fddbc7", "#b2182b", "#2166ac")  # Tamalee: for the E. coli and K. pneumonia ESBLs, for the ESBL results per quarter graph, the grey Unknown section should actually be blue because they are susceptible E.coli/ K. pneumo. 

# Colors order: Negative, Positive, Unknown
cols_esbl <- c("#2166ac", "#b2182b", "#969696")

translator <- Translator$new(translation_csvs_path = "./www/translations/")