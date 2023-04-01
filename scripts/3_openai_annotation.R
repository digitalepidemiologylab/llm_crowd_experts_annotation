# Script information ------------
#' Aim: Use Open AI to annotate tweets on vaccine public perception 
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Get Python packages -------------
os <- reticulate::import("os")
openai <- reticulate::import("openai")
