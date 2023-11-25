# Script information ------------
#' Aim: Getting data for Shiny app 
#' Author: Laura Espinosa
#' Date created: 31 October 2023
#' Date updated: 31 October 2023


# Reading datasets ------------
## Without full agreement  ----------
message("Getting datasets with partial agreement for Shiny app")
class_all <- read_csv("outputs/confusion_matrix_per_class_all.csv") %>% 
  mutate(Prompt = as.character(Prompt)) %>% 
  arrange(desc(`F1 score`)) %>% 
  mutate(`F1 score` = as.character(`F1 score`),
         Sensitivity = as.character(Sensitivity),
         Specificity = as.character(Specificity),
         PPV = as.character(PPV),
         NPV = as.character(NPV),
         Precision = as.character(Precision),
         Recall = as.character(Recall),
         `Balanced accuracy` = as.character(`Balanced accuracy`))
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
