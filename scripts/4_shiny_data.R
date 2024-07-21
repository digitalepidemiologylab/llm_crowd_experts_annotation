# Script information ------------
#' Aim: Getting data for Shiny app 
#' Author: Laura Espinosa
#' Date created: 31 October 2023
#' Date updated: 31 October 2023

# Getting summary of methods -------------------
## EPFL experts ------------
message('Getting EPFL dataset for Shiny app')

shiny_epfl_partial <- read_csv("data/epfl_annotations_simple_partial_agreement.csv") %>% 
  group_by(stance_epfl) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(`Percentage (%)` = round(n/sum(n) * 100, 1),
         Agreement = "Partial agreement") %>% 
  rename("Stance" = "stance_epfl",
         "Number of tweets" = "n") 

shiny_epfl_full <- read_csv("data/epfl_annotations_simple_full_agreement.csv") %>% 
  group_by(stance_epfl) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(`Percentage (%)` = round(n/sum(n) * 100, 1),
         Agreement = "Full agreement") %>% 
  rename("Stance" = "stance_epfl",
         "Number of tweets" = "n") 

shiny_epfl <- rbind(shiny_epfl_partial, shiny_epfl_full) %>% 
  select(Agreement, Stance, `Number of tweets`,`Percentage (%)`)

## Amazon Mturk workers -----------
message('Getting Amazon Mturk workers dataset for Shiny app')
shiny_mturk_all <- read_csv("data/mturk_annotations_simple.csv") %>% 
  group_by(agree_mturk) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(`Percentage (%)` = round(n/sum(n) * 100, 1),
         Agreement = "All tweets")

shiny_mturk_partial <- read_csv("data/mturk_annotations_simple.csv") %>% 
  filter(partial_agree == 1) %>% 
  group_by(agree_mturk) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(`Percentage (%)` = round(n/sum(n) * 100, 1),
         Agreement = "Partial agreement")

shiny_mturk_full <- read_csv("data/mturk_annotations_simple.csv") %>% 
  filter(full_agree == 1) %>% 
  group_by(agree_mturk) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(`Percentage (%)` = round(n/sum(n) * 100, 1),
         Agreement = "Total agreement")

shiny_mturk <- rbind(shiny_mturk_all, shiny_mturk_partial, shiny_mturk_full) %>% 
  select(Agreement, agree_mturk, n, `Percentage (%)`) %>% 
  rename("Stance" = "agree_mturk",
         "Number of tweets" = "n")

rm(shiny_mturk_all, shiny_mturk_partial, shiny_mturk_full)

## GPT -------
message("Getting GPT dataset for Shiny app")

shiny_gpt <- read_csv("outputs/descriptive_gpt_all.csv") %>% 
  mutate(`Negative (partial agreement)` = round(`Negative (partial agreement)`, 1),
         `Positive (partial agreement)` = round(`Positive (partial agreement)`, 1),
         `Neutral (partial agreement)` = round(`Neutral (partial agreement)`, 1),
         `Negative (full agreement)` = round(`Negative (full agreement)`, 1),
         `Positive (full agreement)` = round(`Positive (full agreement)`, 1),
         `Neutral (full agreement)` = round(`Neutral (full agreement)`, 1),
         Prompt = as.character(Prompt))

## Mistral ----------------
message("Getting Mistral (7B) dataset for Shiny app")
shiny_mistral <- read_csv("outputs/descriptive_mistral_all.csv") %>% 
  mutate(`Negative (partial agreement)` = round(`Negative (partial agreement)`, 1),
         `Positive (partial agreement)` = round(`Positive (partial agreement)`, 1),
         `Neutral (partial agreement)` = round(`Neutral (partial agreement)`, 1),
         `Negative (full agreement)` = round(`Negative (full agreement)`, 1),
         `Positive (full agreement)` = round(`Positive (full agreement)`, 1),
         `Neutral (full agreement)` = round(`Neutral (full agreement)`, 1),
         Prompt = as.character(Prompt))

## Mixtral ----------------
message("Getting Mixtral (8x7B) dataset for Shiny app")
shiny_mixtral <- read_csv("outputs/descriptive_mixtral_all.csv") %>% 
  mutate(`Negative (partial agreement)` = round(`Negative (partial agreement)`, 1),
         `Positive (partial agreement)` = round(`Positive (partial agreement)`, 1),
         `Neutral (partial agreement)` = round(`Neutral (partial agreement)`, 1),
         `Negative (full agreement)` = round(`Negative (full agreement)`, 1),
         `Positive (full agreement)` = round(`Positive (full agreement)`, 1),
         `Neutral (full agreement)` = round(`Neutral (full agreement)`, 1),
         Prompt = as.character(Prompt))

## Llama3 ----------------
message("Getting Llama3 (8B) dataset for Shiny app")
shiny_llama <- read_csv("outputs/descriptive_llama_all.csv") %>% 
  mutate(`Negative (partial agreement)` = round(`Negative (partial agreement)`, 1),
         `Positive (partial agreement)` = round(`Positive (partial agreement)`, 1),
         `Neutral (partial agreement)` = round(`Neutral (partial agreement)`, 1),
         `Negative (full agreement)` = round(`Negative (full agreement)`, 1),
         `Positive (full agreement)` = round(`Positive (full agreement)`, 1),
         `Neutral (full agreement)` = round(`Neutral (full agreement)`, 1),
         Prompt = as.character(Prompt))

## Llama3 (70B) ----------------
message("Getting Llama3 (70B) dataset for Shiny app")
shiny_llama_70b <- read_csv("outputs/descriptive_llama70b_all.csv") %>% 
  mutate(`Negative (partial agreement)` = round(`Negative (partial agreement)`, 1),
         `Positive (partial agreement)` = round(`Positive (partial agreement)`, 1),
         `Neutral (partial agreement)` = round(`Neutral (partial agreement)`, 1),
         `Negative (full agreement)` = round(`Negative (full agreement)`, 1),
         `Positive (full agreement)` = round(`Positive (full agreement)`, 1),
         `Neutral (full agreement)` = round(`Neutral (full agreement)`, 1),
         Prompt = as.character(Prompt))


# Getting performance indicators ------------
## Without full agreement  ----------
message("Getting datasets with partial agreement for Shiny app")
class_all <- read_csv("outputs/confusion_matrix_per_class_all.csv") %>% 
  mutate(Prompt = as.character(Prompt)) %>% 
  arrange(desc(`F1 score`)) 
class_all_positive <- read_csv("outputs/confusion_matrix_per_class_positive.csv") %>% 
  mutate(Prompt = as.character(Prompt)) %>% 
  arrange(desc(`F1 score`)) 
class_all_negative <- read_csv("outputs/confusion_matrix_per_class_negative.csv") %>% 
  mutate(Prompt = as.character(Prompt)) %>% 
  arrange(desc(`F1 score`)) 
class_all_neutral <- read_csv("outputs/confusion_matrix_per_class_neutral.csv") %>% 
  mutate(Prompt = as.character(Prompt)) %>% 
  arrange(desc(`F1 score`)) 
overall_all <- read_csv("outputs/confusion_matrix_accuracy.csv") %>% 
  mutate(Prompt = as.character(Prompt))  %>% 
  arrange(desc(Accuracy)) %>% 
  mutate(`Accuracy (p-value)` = case_when(`Accuracy (p-value)` < 0.0001 ~ as.character("< 0.0001"),
                                          .default = as.character(`Accuracy (p-value)`)))

## With full agreement ---------------
message("Getting datasets with full agreement for Shiny app")
class_all_agree <- read_csv("outputs/confusion_matrix_per_class_all_agree.csv") %>% 
  mutate(Prompt = as.character(Prompt)) %>% 
  arrange(desc(`F1 score`)) 
class_all_positive_agree <- read_csv("outputs/confusion_matrix_per_class_positive_agree.csv") %>% 
  mutate(Prompt = as.character(Prompt)) %>% 
  arrange(desc(`F1 score`))  
class_all_negative_agree <- read_csv("outputs/confusion_matrix_per_class_negative_agree.csv") %>% 
  mutate(Prompt = as.character(Prompt)) %>% 
  arrange(desc(`F1 score`)) 
class_all_neutral_agree <- read_csv("outputs/confusion_matrix_per_class_neutral_agree.csv") %>% 
  mutate(Prompt = as.character(Prompt)) %>% 
  arrange(desc(`F1 score`)) 
overall_all_agree <- read_csv("outputs/confusion_matrix_accuracy_agree.csv") %>% 
  mutate(Prompt = as.character(Prompt)) %>% 
  arrange(desc(Accuracy)) %>% 
  mutate(`Accuracy (p-value)` = case_when(`Accuracy (p-value)` < 0.0001 ~ as.character("< 0.0001"),
                                          .default = as.character(`Accuracy (p-value)`)))

# Getting prompts' text ----------
prompts_text <- read_csv("data/prompts_text.csv")
