# Script information ------------
#' Aim: Source different steps of analysis
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Install packages (public) -------------------------
source("scripts/1_packages.R")

# Get data ------------
source("scripts/2_data.R")

# Comparison of annotation's methods 
## With partial experts' agreement -------------
source("scripts/3_1_Comparison_partial_agreement.R")

## With full experts' agreement -------------
source("scripts/3_2_Comparison_full_agreement.R")

## All tweets -----------------
source("scripts/3_3_Overall_comparison.R")

# Analysis of the non-agreed tweets by 4 annotators ------
source("scripts/3_4_Analysis_no_agreement.R")

# Analysis per each of the 4 annotators -----------
source("scripts/3_5_Comparison_per_annotator.R")

## Get datasets for Shiny app (public) -------------
source("scripts/4_shiny_data.R")

# Launch Shiny app (public) -------------
source("shiny_app.R")
shinyApp(ui = ui, server=server)


