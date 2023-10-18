# Script information ------------
#' Aim: Source different steps of analysis
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Install packages -------------------------
source("scripts/1_packages.R")

# Get data ------------
source("scripts/2_1_data.R")

# Launch Shiny app -------------
source("scripts/annotations/shiny.R")
shinyApp(ui = ui, server=server)

# Clean labelled tweets data ---------------
## Only needed once
source("scripts/2a_clean_labelled_mturk.R")

source("scripts/2b_clean_labelled_tweets.R")

source("scripts/3_epfl_annotated_tweets.R")

# Analysis of annotations from Mturk and GPT (three levels) ------
source("scripts/3_openai_annotation.R")

# Analysis of annotations from Mturk and GPT (two levels) ------
source("scripts/3_openai_annotation_two_labels.R")

# Prompt engineering ---------------------------
source("scripts/4_prompt_engineering.R")

# GPT, EPFL and Mturk -------------
source("scripts/5_comparison.R")

