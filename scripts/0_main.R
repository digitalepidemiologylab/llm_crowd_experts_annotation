# Script information ------------
#' Aim: Source different steps of analysis
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Install packages -------------------------
source("scripts/1_packages.R")

# Clean labelled tweets data ---------------
## Only needed once
source("scripts/2a_clean_labelled_mturk.R")

source("scripts/2b_clean_labelled_tweets.R")

# Analysis of annotations from Mturk and GPT (three levels) ------
source("scripts/3_openai_annotation.R")

# Analysis of annotations from Mturk and GPT (two levels) ------
source("scripts/3_openai_annotation_two_labels.R")

# Prompt engineering ---------------------------
source("scripts/4_prompt_engineering.R")
