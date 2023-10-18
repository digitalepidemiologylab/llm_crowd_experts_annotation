# Script information ------------
#' Aim: Comparing all annotations
#' Author: Laura Espinosa
#' Date created: 12 October 2023
#' Date updated: 12 October 2023

# Get same id tweets for all datasets -------------


df_all <- df_mturk_clean %>% 
  full_join(gpt, by = "text") #%>% 
  full_join(epfl_df, by = "id_tweets") #%>% 
  mutate(comp_mturk = case_when())

