# Script information ------------
#' Aim: Source different steps of analysis
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Install packages -------------------------
source("scripts/1_packages.R")

# Clean labelled tweets data ---------------
## Only needed once
source("scripts/2_clean_labelled_tweets.R")

# Merge annotations from Mturk and GPT ------
source("scripts/3_openai_annotation.R")