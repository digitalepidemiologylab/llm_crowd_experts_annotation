# Script information ------------
#' Aim: Source different steps of analysis
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Install packages (public) -------------------------
source("scripts/1_packages.R")

# Get data ------------
source("scripts/2_1_data.R")

# Comparison of annotation's methods -------------
source("scripts/2_2_Comparison.R")

## With full agreement EPFL ------------
source("scripts/2_2_Comparison_full_agreement.R")

## Get datasets for Shiny app (public) -------------
source("scripts/3_shiny_data.R")

# Launch Shiny app (public) -------------
source("shiny_app.R")
shinyApp(ui = ui, server=server)


