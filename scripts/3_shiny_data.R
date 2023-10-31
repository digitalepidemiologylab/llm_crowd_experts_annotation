# Script information ------------
#' Aim: Getting data for Shiny app 
#' Author: Laura Espinosa
#' Date created: 31 October 2023
#' Date updated: 31 October 2023


# Reading datasets ------------
## Without full agreement  ----------
message("Getting datasets for Shiny app")
class_all <- read_csv("outputs/confusion_matrix_per_class_all.csv")
class_all_positive <- read_csv("outputs/confusion_matrix_per_class_positive.csv")
class_all_negative <- read_csv("outputs/confusion_matrix_per_class_negative.csv")
class_all_neutral <- read_csv("outputs/confusion_matrix_per_class_neutral.csv")
overall_all <- read_csv("outputs/confusion_matrix_accuracy.csv")

## With full agreement ---------------

# Getting prompts' text ----------
prompts_text <- read_csv("data/prompts_text.csv")
