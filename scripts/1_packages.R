# Script information ------------
#' Aim: Install all packages 
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 7 May 2023

# Packages ----------------------
# install/load "pacman" to help installing and loading other packages
message("Installing and loading packages")

while (require("pacman") == FALSE) {
  install.packages("pacman")
  library("pacman")
}

# load packages
p_load(tidyverse, ExPanDaR, reticulate, readxl, janitor, flextable,
       DataExplorer, caret, vroom, data.table, reactable)
