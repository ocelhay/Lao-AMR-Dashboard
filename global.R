# Load packages
library(DT)
library(highcharter)
library(lubridate)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(viridisLite)

rm(list = ls())

# Colors order: S, I, R, Not Tested
cols_sir <- c("#2166ac", "#fddbc7", "#b2182b", "#969696")

# Colors order: Negative, Positive, Unknown
cols_esbl <- c("#2166ac", "#b2182b", "#969696")