# Script information ------------
#' Aim: Comparing all annotations
#' Author: Laura Espinosa
#' Date created: 12 October 2023
#' Date updated: 12 October 2023


# Confusion matrix --------
## Get merged datasets ----------
## Get unique prompts ---------------
prompts <- df_all_clean$prompt %>% 
  unique()

## EPFL vs GPT ------------
for (i in prompts) {
  df_con_matrix <- df_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_epfl, sentiment_gpt) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix',i,sep='_'), df_con_matrix)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_gpt <- confusionMatrix(df_con_matrix$sentiment_gpt,
                                   df_con_matrix$stance_epfl, mode = "everything")
  conf_epfl_gpt$prompt <- prompts[i]
  assign(paste('conf_epfl_gpt_all',i,sep='_'),conf_epfl_gpt) 
  # assign(paste('conf_epfl_gpt_all',i,sep='_'),conf_epfl_gpt) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_gpt_all", i, ".csv"))
  
}


## EPFL vs Mturk --------------
df_con_matrix_mturk <- df_all_clean %>% 
    filter(!duplicated(id_tweets)) %>% 
    select(stance_epfl, agree_mturk) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           agree_mturk = factor(agree_mturk, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 

conf_epfl_mturk <- confusionMatrix(df_con_matrix_mturk$agree_mturk,
                                     df_con_matrix_mturk$stance_epfl, mode = "everything")


  
# conf_epfl_mturk %>% 
#     capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mturk.csv"))
  

## EPFL vs Mistral ---------------
for (i in prompts) {
  df_con_matrix_mistral <- df_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_epfl, sentiment_mistral) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_mistral = factor(sentiment_mistral, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mistral',i,sep='_'), df_con_matrix_mistral)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mistral <- confusionMatrix(df_con_matrix_mistral$sentiment_mistral,
                                   df_con_matrix_mistral$stance_epfl, mode = "everything")
  conf_epfl_mistral$prompt <- prompts[i]
  assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs Mixtral ---------------
for (i in prompts) {
  df_con_matrix_mixtral <- df_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_epfl, sentiment_mixtral) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_mixtral = factor(sentiment_mixtral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mixtral',i,sep='_'), df_con_matrix_mixtral)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mixtral <- confusionMatrix(df_con_matrix_mixtral$sentiment_mixtral,
                                       df_con_matrix_mixtral$stance_epfl, mode = "everything")
  conf_epfl_mixtral$prompt <- prompts[i]
  assign(paste('conf_epfl_mixtral_all',i,sep='_'),conf_epfl_mixtral) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs selecting majority class ------------  
df_con_matrix_majority <- df_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_epfl) %>%
  mutate(stance_majority = "neutral", 
         stance_epfl = factor(stance_epfl, ordered = TRUE,
                              levels = c("positive","neutral", "negative")),
         stance_majority = factor(stance_majority, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 


conf_epfl_majority <- confusionMatrix(df_con_matrix_majority$stance_majority,
                                            df_con_matrix_majority$stance_epfl)

# Merging all confusion matrices ------------
## Accuracy -------------
overall_all_fig <- as.data.frame(conf_epfl_mturk$overall) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_0$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_3$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_4$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_6$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_7$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_40$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_41$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_45$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_43$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_44$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_46$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_47$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_48$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_0$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_0$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_5$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_8$overall)) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral_all_0" = "Mistral prompt 0",
                                    "mistral_all_1" = "Mistral prompt 1",
                                    "mistral_all_3" = "Mistral prompt 3",
                                    "mistral_all_4" = "Mistral prompt 4",
                                    "mistral_all_5" = "Mistral prompt 5",
                                    "mistral_all_6" = "Mistral prompt 6",
                                    "mistral_all_7" = "Mistral prompt 7",
                                    "mistral_all_8" = "Mistral prompt 8",
                                    "mixtral_all_0" = "Mixtral prompt 0",
                                    "mixtral_all_1" = "Mixtral prompt 1",
                                    "mixtral_all_3" = "Mixtral prompt 3",
                                    "mixtral_all_4" = "Mixtral prompt 4",
                                    "mixtral_all_5" = "Mixtral prompt 5",
                                    "mixtral_all_6" = "Mixtral prompt 6",
                                    "mixtral_all_7" = "Mixtral prompt 7",
                                    "mixtral_all_8" = "Mixtral prompt 8",
                                    "gpt_all_40" = "GPT 4 prompt 0",
                                    "gpt_all_41" = "GPT 4 prompt 1",
                                    "gpt_all_43" = "GPT 4 prompt 3",
                                    "gpt_all_44" = "GPT 4 prompt 4",
                                    "gpt_all_45" = "GPT 4 prompt 5",
                                    "gpt_all_46" = "GPT 4 prompt 6",
                                    "gpt_all_47" = "GPT 4 prompt 7",
                                    "gpt_all_48" = "GPT 4 prompt 8",
                                    "gpt_all_0" = "GPT 3.5 prompt 0",
                                    "gpt_all_1" = "GPT 3.5 prompt 1",
                                    "gpt_all_3" = "GPT 3.5 prompt 3",
                                    "gpt_all_4" = "GPT 3.5 prompt 4",
                                    "gpt_all_5" = "GPT 3.5 prompt 5",
                                    "gpt_all_6" = "GPT 3.5 prompt 6",
                                    "gpt_all_7" = "GPT 3.5 prompt 7",
                                    "gpt_all_8" = "GPT 3.5 prompt 8",
                                    "mturk" = "Amazon Mturk"))) %>% 
  select(method, Accuracy, AccuracyLower, AccuracyUpper, AccuracyPValue) %>% 
  separate(col = "method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
  mutate(Pvalue = case_when(AccuracyPValue <= 0.05 ~ "<= 0.05",
                            .default = "> 0.05"),
         agreement = "Partial agreement",
         Prompt = replace_na(Prompt, "None"))


overall_all <- overall_all_fig %>% 
  mutate(Accuracy = round(Accuracy, 4),
         AccuracyLower = round(AccuracyLower, 4),
         AccuracyUpper = round(AccuracyUpper, 4),
         AccuracyPValue = round(AccuracyPValue, 6),
         AccuracyCI = paste("(", AccuracyLower, " - ", AccuracyUpper, ")", sep = "")) %>%
  select(Method, Prompt, Accuracy, AccuracyCI, AccuracyPValue) %>% 
    rename("Accuracy (95% CI)" = "AccuracyCI",
           "Accuracy (p-value)" = "AccuracyPValue")
  
overall_all %>%
  write_csv("outputs/confusion_matrix_accuracy.csv")

### Plot accuracy ----------------
accuracy_fig_partial <- overall_all_fig %>% 
  #filter(Method == "GPT 4") %>% 
  filter(!is.na(Prompt)) %>% 
  ggplot(aes(x = Prompt, y = Accuracy)) +
  geom_point(aes(color = Pvalue)) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper,
                    color = Pvalue),
                width = 0.2) +
  # geom_text(aes(label = sprintf("%s", AccuracyPValue), 
  #               y = AccuracyUpper)
  #           #,
  #               # box.padding = 1,
  #               # point.padding = 1,
  #               # segment.color = "transparent",
  #               # max.overlaps = 20
  #               , vjust = -0.4
  #           ) +
  facet_grid(~Method, scales = "free_x") 

accuracy_fig_partial
  
## Per class ---------
class_majority <- as.data.frame(conf_epfl_majority$byClass) %>% 
  mutate(class_tweet = c("positive_majority", "neutral_majority", "negative_majority"))%>% 
  rownames_to_column() %>% 
  select(-rowname) %>%
  separate(., class_tweet, into = c("class", "classifier"), 
           sep = "_") %>% 
  mutate(method = str_replace_all(classifier, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = ""))) %>% 
  select(-classifier) %>% 
  rename("PPV" = "Pos Pred Value",
         "NPV" = "Neg Pred Value",
         "F1 score" = "F1",
         "Method" = "method",
         "Balanced accuracy" = "Balanced Accuracy",
         "Stance" = "class") %>% 
  separate(col = "Method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
  select(Method, Prompt, Stance, "F1 score", Sensitivity, Specificity, PPV, NPV, Precision, Recall, 
         "Balanced accuracy") %>% 
  mutate(`F1 score` = round(`F1 score`, 4),
         Sensitivity = round(Sensitivity, 4),
         Specificity = round(Specificity, 4),
         PPV = round(PPV, 4),
         NPV = round(NPV, 4),
         Precision = round(Precision, 4),
         Recall = round(Recall, 4),
         `Balanced accuracy` = round(`Balanced accuracy`, 4)) %>% 
  select(Method, Prompt, Stance, `F1 score`) %>% 
  mutate(agreement = "Partial agreement") %>% 
  filter(!is.na(`F1 score`))

  
  
class_mturk <- as.data.frame(conf_epfl_mturk$byClass) %>% 
  mutate(class_tweet = c("positive_mturk", "neutral_mturk", "negative_mturk"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt0 <- as.data.frame(conf_epfl_gpt_all_0$byClass) %>% 
  mutate(class_tweet = c("positive_gpt0", "neutral_gpt0", "negative_gpt0"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt1 <- as.data.frame(conf_epfl_gpt_all_1$byClass) %>% 
  mutate(class_tweet = c("positive_gpt1", "neutral_gpt1", "negative_gpt1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt3 <- as.data.frame(conf_epfl_gpt_all_3$byClass) %>% 
  mutate(class_tweet = c("positive_gpt3", "neutral_gpt3", "negative_gpt3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt4 <- as.data.frame(conf_epfl_gpt_all_4$byClass) %>% 
  mutate(class_tweet = c("positive_gpt4", "neutral_gpt4", "negative_gpt4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt5 <- as.data.frame(conf_epfl_gpt_all_5$byClass) %>% 
  mutate(class_tweet = c("positive_gpt5", "neutral_gpt5", "negative_gpt5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt6 <- as.data.frame(conf_epfl_gpt_all_6$byClass) %>% 
  mutate(class_tweet = c("positive_gpt6", "neutral_gpt6", "negative_gpt6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt7 <- as.data.frame(conf_epfl_gpt_all_7$byClass) %>% 
  mutate(class_tweet = c("positive_gpt7", "neutral_gpt7", "negative_gpt7"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt8 <- as.data.frame(conf_epfl_gpt_all_8$byClass) %>% 
  mutate(class_tweet = c("positive_gpt8", "neutral_gpt8", "negative_gpt8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt40 <- as.data.frame(conf_epfl_gpt_all_40$byClass) %>% 
  mutate(class_tweet = c("positive_gpt40", "neutral_gpt40", "negative_gpt40"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt41 <- as.data.frame(conf_epfl_gpt_all_41$byClass) %>% 
  mutate(class_tweet = c("positive_gpt41", "neutral_gpt41", "negative_gpt41")) %>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt45 <- as.data.frame(conf_epfl_gpt_all_45$byClass) %>% 
  mutate(class_tweet = c("positive_gpt45", "neutral_gpt45", "negative_gpt45"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt43 <- as.data.frame(conf_epfl_gpt_all_43$byClass) %>% 
  mutate(class_tweet = c("positive_gpt43", "neutral_gpt43", "negative_gpt43"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt44 <- as.data.frame(conf_epfl_gpt_all_44$byClass) %>% 
  mutate(class_tweet = c("positive_gpt44", "neutral_gpt44", "negative_gpt44"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt46 <- as.data.frame(conf_epfl_gpt_all_46$byClass) %>% 
  mutate(class_tweet = c("positive_gpt46", "neutral_gpt46", "negative_gpt46"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt47 <- as.data.frame(conf_epfl_gpt_all_47$byClass) %>% 
  mutate(class_tweet = c("positive_gpt47", "neutral_gpt47", "negative_gpt47"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt48 <- as.data.frame(conf_epfl_gpt_all_48$byClass) %>% 
  mutate(class_tweet = c("positive_gpt48", "neutral_gpt48", "negative_gpt48"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral0 <- as.data.frame(conf_epfl_mistral_all_0$byClass) %>% 
  mutate(class_tweet = c("positive_mistral0", "neutral_mistral0", "negative_mistral0"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral1 <- as.data.frame(conf_epfl_mistral_all_1$byClass) %>% 
  mutate(class_tweet = c("positive_mistral1", "neutral_mistral1", "negative_mistral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral3 <- as.data.frame(conf_epfl_mistral_all_3$byClass) %>% 
  mutate(class_tweet = c("positive_mistral3", "neutral_mistral3", "negative_mistral3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral4 <- as.data.frame(conf_epfl_mistral_all_4$byClass) %>% 
  mutate(class_tweet = c("positive_mistral4", "neutral_mistral4", "negative_mistral4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral5 <- as.data.frame(conf_epfl_mistral_all_5$byClass) %>% 
  mutate(class_tweet = c("positive_mistral5", "neutral_mistral5", "negative_mistral5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral6 <- as.data.frame(conf_epfl_mistral_all_6$byClass) %>% 
  mutate(class_tweet = c("positive_mistral6", "neutral_mistral6", "negative_mistral6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral7 <- as.data.frame(conf_epfl_mistral_all_7$byClass) %>%
  mutate(class_tweet = c("positive_mistral7", "neutral_mistral7", "negative_mistral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mistral8 <- as.data.frame(conf_epfl_mistral_all_8$byClass) %>% 
  mutate(class_tweet = c("positive_mistral8", "neutral_mistral8", "negative_mistral8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral0 <- as.data.frame(conf_epfl_mixtral_all_0$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral0", "neutral_mixtral0", "negative_mixtral0"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral1 <- as.data.frame(conf_epfl_mixtral_all_1$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral1", "neutral_mixtral1", "negative_mixtral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral3 <- as.data.frame(conf_epfl_mixtral_all_3$byClass) %>%
  mutate(class_tweet = c("positive_mixtral3", "neutral_mixtral3", "negative_mixtral3"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral4 <- as.data.frame(conf_epfl_mixtral_all_4$byClass) %>%
  mutate(class_tweet = c("positive_mixtral4", "neutral_mixtral4", "negative_mixtral4"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral5 <- as.data.frame(conf_epfl_mixtral_all_5$byClass) %>%
  mutate(class_tweet = c("positive_mixtral5", "neutral_mixtral5", "negative_mixtral5"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral6 <- as.data.frame(conf_epfl_mixtral_all_6$byClass) %>%
  mutate(class_tweet = c("positive_mixtral6", "neutral_mixtral6", "negative_mixtral6"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral7 <- as.data.frame(conf_epfl_mixtral_all_7$byClass) %>%
  mutate(class_tweet = c("positive_mixtral7", "neutral_mixtral7", "negative_mixtral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral8 <- as.data.frame(conf_epfl_mixtral_all_8$byClass) %>%
  mutate(class_tweet = c("positive_mixtral8", "neutral_mixtral8", "negative_mixtral8"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_all <- class_gpt0 %>% 
  full_join(class_gpt1) %>% 
  full_join(class_gpt3) %>% 
  full_join(class_gpt4) %>% 
  full_join(class_gpt40) %>% 
  full_join(class_gpt41) %>% 
  full_join(class_gpt45) %>% 
  full_join(class_gpt5) %>% 
  full_join(class_gpt6) %>% 
  full_join(class_gpt7) %>% 
  full_join(class_gpt8) %>% 
  full_join(class_mturk) %>% 
  full_join(class_gpt43) %>% 
  full_join(class_gpt44) %>% 
  full_join(class_gpt46) %>% 
  full_join(class_gpt47) %>% 
  full_join(class_gpt48) %>% 
  full_join(class_mistral0) %>% 
  full_join(class_mistral1) %>% 
  full_join(class_mistral3) %>% 
  full_join(class_mistral4) %>% 
  full_join(class_mistral5) %>% 
  full_join(class_mistral6) %>% 
  full_join(class_mistral7) %>%
  full_join(class_mistral8) %>% 
  full_join(class_mixtral0) %>% 
  full_join(class_mixtral1) %>% 
  full_join(class_mixtral3) %>%
  full_join(class_mixtral4) %>%
  full_join(class_mixtral5) %>%
  full_join(class_mixtral6) %>%
  full_join(class_mixtral7) %>%
  full_join(class_mixtral8) %>%
  separate(., class_tweet, into = c("class", "classifier"), 
           sep = "_") %>% 
  mutate(method = str_replace_all(classifier, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral0" = "Mistral prompt 0",
                                    "mistral1" = "Mistral prompt 1",
                                    "mistral3" = "Mistral prompt 3",
                                    "mistral4" = "Mistral prompt 4",
                                    "mistral5" = "Mistral prompt 5",
                                    "mistral6" = "Mistral prompt 6",
                                    "mistral7" = "Mistral prompt 7",
                                    "mistral8" = "Mistral prompt 8",
                                    "mixtral0" = "Mixtral prompt 0",
                                    "mixtral1" = "Mixtral prompt 1",
                                    "mixtral3" = "Mixtral prompt 3",
                                    "mixtral4" = "Mixtral prompt 4",
                                    "mixtral5" = "Mixtral prompt 5",
                                    "mixtral6" = "Mixtral prompt 6",
                                    "mixtral7" = "Mixtral prompt 7",
                                    "mixtral8" = "Mixtral prompt 8",
                                    "gpt40" = "GPT 4 prompt 0",
                                    "gpt41" = "GPT 4 prompt 1",
                                    "gpt43" = "GPT 4 prompt 3",
                                    "gpt44" = "GPT 4 prompt 4",
                                    "gpt45" = "GPT 4 prompt 5",
                                    "gpt46" = "GPT 4 prompt 6",
                                    "gpt47" = "GPT 4 prompt 7",
                                    "gpt48" = "GPT 4 prompt 8",
                                    "gpt0" = "GPT 3.5 prompt 0",
                                    "gpt1" = "GPT 3.5 prompt 1",
                                    "gpt3" = "GPT 3.5 prompt 3",
                                    "gpt4" = "GPT 3.5 prompt 4",
                                    "gpt5" = "GPT 3.5 prompt 5",
                                    "gpt6" = "GPT 3.5 prompt 6",
                                    "gpt7" = "GPT 3.5 prompt 7",
                                    "gpt8" = "GPT 3.5 prompt 8",
                                    "mturk" = "Amazon Mturk"))) %>% 
  select(-classifier) %>% 
  rename("PPV" = "Pos Pred Value",
         "NPV" = "Neg Pred Value",
         "F1 score" = "F1",
         "Method" = "method",
         "Balanced accuracy" = "Balanced Accuracy",
         "Stance" = "class") %>% 
  separate(col = "Method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
  select(Method, Prompt, Stance, "F1 score", Sensitivity, Specificity, PPV, NPV, Precision, Recall, 
         "Balanced accuracy") %>% 
  mutate(`F1 score` = round(`F1 score`, 4),
         Sensitivity = round(Sensitivity, 4),
         Specificity = round(Specificity, 4),
         PPV = round(PPV, 4),
         NPV = round(NPV, 4),
         Precision = round(Precision, 4),
         Recall = round(Recall, 4),
         `Balanced accuracy` = round(`Balanced accuracy`, 4)) %>% 
  select(Method, Prompt, Stance, `F1 score`, Sensitivity, Specificity) %>% 
  mutate(agreement = "Partial agreement")

class_all %>% 
  write_csv("outputs/confusion_matrix_per_class_all.csv")

class_all_positive <- class_all %>% 
  filter(Stance == "positive") %>% 
  arrange(desc("F1 score"))

class_all_positive %>% 
  write_csv("outputs/confusion_matrix_per_class_positive.csv")

class_all_negative <- class_all %>% 
  filter(Stance == "negative")%>% 
  arrange(desc("F1 score"))

class_all_negative %>% 
  write_csv("outputs/confusion_matrix_per_class_negative.csv")

class_all_neutral <- class_all %>% 
  filter(Stance == "neutral")%>% 
  arrange(desc("F1 score"))

class_all_neutral %>% 
  write_csv("outputs/confusion_matrix_per_class_neutral.csv")

