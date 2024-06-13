# Script information ------------
#' Aim: Comparing each annotator
#' Author: Laura Espinosa
#' Date created: 8 June 2024
#' Date updated: 8 June 2024


# Confusion matrix --------
## Get merged datasets ----------
df_annotator_all <- df_mturk_annot_clean %>% 
  full_join(epfl_df_all, by = "text") %>% 
  full_join(gpt_clean, by = "text") %>%
  full_join(mistral_clean, by = c("text", "prompt")) %>% 
  full_join(mixtral_clean, by = c("text", "prompt")) %>% 
  full_join(df_vader, by = "text") %>% 
  mutate(sent_vader = tolower(sent_vader)) %>% 
  dplyr::filter(!is.na(sent_l)) %>% 
  select(-id_tweets.x, -id_tweets.y) # id_tweets comes from gpt_clean (supposedly same as epfl_df)

df_annotator_all_clean <- df_annotator_all %>% 
  rename("neutral_mturk" = "neutral",
         "positive_mturk" = "positive",
         "negative_mturk" = "negative") %>% 
  select(-text) %>% 
  dplyr::filter(!is.na(stance_epfl)) %>% 
  left_join(select(tweets_id_real, tweet_id, id_tweets),
            by = "id_tweets") 

## Get unique prompts ---------------
prompts <- df_annotator_all_clean$prompt %>% 
  unique()

# Annotator 1 -----------
## EPFL vs GPT ------------
for (i in prompts) {
  df_con_matrix_m <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_m, sentiment_gpt) %>% 
    mutate(stance_m = factor(stance_m, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_m',i,sep='_'), df_con_matrix_m)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_gpt_m <- confusionMatrix(df_con_matrix_m$sentiment_gpt,
                                   df_con_matrix_m$stance_m, mode = "everything")
  conf_epfl_gpt_m$prompt <- prompts[i]
  assign(paste('conf_epfl_gpt_all_m',i,sep='_'),conf_epfl_gpt_m) 
  # assign(paste('conf_epfl_gpt_all',i,sep='_'),conf_epfl_gpt) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_gpt_all", i, ".csv"))
  
}


## EPFL vs Mturk --------------
df_con_matrix_mturk_m <- df_annotator_all_clean %>% 
    filter(!duplicated(id_tweets)) %>% 
    select(stance_m, agree_mturk) %>% 
    mutate(stance_m = factor(stance_m, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           agree_mturk = factor(agree_mturk, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 

conf_epfl_mturk_m <- confusionMatrix(df_con_matrix_mturk_m$agree_mturk,
                                     df_con_matrix_mturk_m$stance_m, mode = "everything")

## EPFL vs vader --------------
df_con_matrix_vader_m <- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_m, sent_vader) %>% 
  mutate(stance_m = factor(stance_m, ordered = TRUE,
                              levels = c("positive","neutral", "negative")),
         sent_vader = factor(sent_vader, ordered = TRUE,
                              levels = c("positive", "neutral", "negative"))) 

conf_epfl_vader_m <- confusionMatrix(df_con_matrix_vader_m$sent_vader,
                                   df_con_matrix_vader_m$stance_m, mode = "everything")


  
## EPFL vs Mistral ---------------
for (i in prompts) {
  df_con_matrix_mistral_m <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_m, sentiment_mistral) %>% 
    mutate(stance_m = factor(stance_m, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_mistral = factor(sentiment_mistral, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mistral_m',i,sep='_'), df_con_matrix_mistral_m)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mistral_m <- confusionMatrix(df_con_matrix_mistral_m$sentiment_mistral,
                                         df_con_matrix_mistral_m$stance_m, mode = "everything")
  conf_epfl_mistral_m$prompt <- prompts[i]
  assign(paste('conf_epfl_mistral_all_m',i,sep='_'),conf_epfl_mistral_m) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs Mixtral ---------------
for (i in prompts) {
  df_con_matrix_mixtral_m <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_m, sentiment_mixtral) %>% 
    mutate(stance_m = factor(stance_m, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_mixtral = factor(sentiment_mixtral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mixtral_m',i,sep='_'), df_con_matrix_mixtral_m)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mixtral_m <- confusionMatrix(df_con_matrix_mixtral_m$sentiment_mixtral,
                                       df_con_matrix_mixtral_m$stance_m, mode = "everything")
  conf_epfl_mixtral_m$prompt <- prompts[i]
  assign(paste('conf_epfl_mixtral_all_m',i,sep='_'),conf_epfl_mixtral_m) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs selecting majority class ------------  
df_con_matrix_majority_m <- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_m) %>%
  mutate(stance_majority = "neutral", 
         stance_m = factor(stance_m, ordered = TRUE,
                              levels = c("positive","neutral", "negative")),
         stance_majority = factor(stance_majority, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 


conf_epfl_majority_m <- confusionMatrix(df_con_matrix_majority_m$stance_majority,
                                      df_con_matrix_majority_m$stance_m)

# Merging all confusion matrices ------------
## Accuracy -------------
overall_all_fig_m <- as.data.frame(conf_epfl_mturk_m$overall) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_3$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_4$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_6$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_7$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_42$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_41$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_45$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_43$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_44$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_46$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_47$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_m_48$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_m_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_m_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_m_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_m_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_m_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_m_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_m_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_m_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_m_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_m_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_m_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_m_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_m_5$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_m_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_m_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_m_8$overall)) %>%
  cbind(as.data.frame(conf_epfl_vader_m$overall)) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral_all_m_2" = "Mistral prompt 2",
                                    "mistral_all_m_1" = "Mistral prompt 1",
                                    "mistral_all_m_3" = "Mistral prompt 3",
                                    "mistral_all_m_4" = "Mistral prompt 4",
                                    "mistral_all_m_5" = "Mistral prompt 5",
                                    "mistral_all_m_6" = "Mistral prompt 6",
                                    "mistral_all_m_7" = "Mistral prompt 7",
                                    "mistral_all_m_8" = "Mistral prompt 8",
                                    "mixtral_all_m_2" = "Mixtral prompt 2",
                                    "mixtral_all_m_1" = "Mixtral prompt 1",
                                    "mixtral_all_m_3" = "Mixtral prompt 3",
                                    "mixtral_all_m_4" = "Mixtral prompt 4",
                                    "mixtral_all_m_5" = "Mixtral prompt 5",
                                    "mixtral_all_m_6" = "Mixtral prompt 6",
                                    "mixtral_all_m_7" = "Mixtral prompt 7",
                                    "mixtral_all_m_8" = "Mixtral prompt 8",
                                    "gpt_all_m_42" = "GPT 4 prompt 2",
                                    "gpt_all_m_41" = "GPT 4 prompt 1",
                                    "gpt_all_m_43" = "GPT 4 prompt 3",
                                    "gpt_all_m_44" = "GPT 4 prompt 4",
                                    "gpt_all_m_45" = "GPT 4 prompt 5",
                                    "gpt_all_m_46" = "GPT 4 prompt 6",
                                    "gpt_all_m_47" = "GPT 4 prompt 7",
                                    "gpt_all_m_48" = "GPT 4 prompt 8",
                                    "gpt_all_m_2" = "GPT 3.5 prompt 2",
                                    "gpt_all_m_1" = "GPT 3.5 prompt 1",
                                    "gpt_all_m_3" = "GPT 3.5 prompt 3",
                                    "gpt_all_m_4" = "GPT 3.5 prompt 4",
                                    "gpt_all_m_5" = "GPT 3.5 prompt 5",
                                    "gpt_all_m_6" = "GPT 3.5 prompt 6",
                                    "gpt_all_m_7" = "GPT 3.5 prompt 7",
                                    "gpt_all_m_8" = "GPT 3.5 prompt 8",
                                    "mturk_m" = "Amazon Mturk",
                                    "vader_m" = "Vader"))) %>% 
  select(method, Accuracy, AccuracyLower, AccuracyUpper, AccuracyPValue) %>% 
  separate(col = "method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
  mutate(Pvalue = case_when(AccuracyPValue <= 0.05 ~ "<= 0.05",
                            .default = "> 0.05"),
         annotator = "Co-author (1)",
         Prompt = replace_na(Prompt, "None"))


overall_all_m <- overall_all_fig_m %>% 
  mutate(Accuracy = round(Accuracy, 4),
         AccuracyLower = round(AccuracyLower, 4),
         AccuracyUpper = round(AccuracyUpper, 4),
         AccuracyPValue = round(AccuracyPValue, 6),
         AccuracyCI = paste("(", AccuracyLower, " - ", AccuracyUpper, ")", sep = "")) %>%
  select(Method, Prompt, Accuracy, AccuracyCI, AccuracyPValue) %>% 
    rename("Accuracy (95% CI)" = "AccuracyCI",
           "Accuracy (p-value)" = "AccuracyPValue")
  
overall_all_m %>%
  write_csv("outputs/confusion_matrix_accuracy_annotator1.csv")

### Plot accuracy ----------------
accuracy_fig_m <- overall_all_fig_m %>% 
  #filter(Method == "GPT 4") %>% 
  filter(!is.na(Prompt)) %>% 
  ggplot(aes(x = Prompt, y = Accuracy)) +
  geom_point(aes(color = Pvalue)) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper,
                    color = Pvalue),
                width = 0.2) +
  facet_grid(~Method, scales = "free_x") 

accuracy_fig_m
  
## Per class ---------
class_majority_m <- as.data.frame(conf_epfl_majority_m$byClass) %>% 
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
  select(Method, Prompt, Stance, `F1 score`, Sensitivity, Specificity) %>% 
  mutate(annotator = "Co-author (1)") 

  
  
class_mturk_m <- as.data.frame(conf_epfl_mturk_m$byClass) %>% 
  mutate(class_tweet = c("positive_mturk", "neutral_mturk", "negative_mturk"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_vader_m <- as.data.frame(conf_epfl_vader_m$byClass) %>% 
  mutate(class_tweet = c("positive_vader", "neutral_vader", "negative_vader"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt2_m <- as.data.frame(conf_epfl_gpt_all_m_2$byClass) %>% 
  mutate(class_tweet = c("positive_gpt2", "neutral_gpt2", "negative_gpt2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt1_m <- as.data.frame(conf_epfl_gpt_all_m_1$byClass) %>% 
  mutate(class_tweet = c("positive_gpt1", "neutral_gpt1", "negative_gpt1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt3_m <- as.data.frame(conf_epfl_gpt_all_m_3$byClass) %>% 
  mutate(class_tweet = c("positive_gpt3", "neutral_gpt3", "negative_gpt3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt4_m <- as.data.frame(conf_epfl_gpt_all_m_4$byClass) %>% 
  mutate(class_tweet = c("positive_gpt4", "neutral_gpt4", "negative_gpt4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt5_m <- as.data.frame(conf_epfl_gpt_all_m_5$byClass) %>% 
  mutate(class_tweet = c("positive_gpt5", "neutral_gpt5", "negative_gpt5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt6_m <- as.data.frame(conf_epfl_gpt_all_m_6$byClass) %>% 
  mutate(class_tweet = c("positive_gpt6", "neutral_gpt6", "negative_gpt6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt7_m <- as.data.frame(conf_epfl_gpt_all_m_7$byClass) %>% 
  mutate(class_tweet = c("positive_gpt7", "neutral_gpt7", "negative_gpt7"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt8_m <- as.data.frame(conf_epfl_gpt_all_m_8$byClass) %>% 
  mutate(class_tweet = c("positive_gpt8", "neutral_gpt8", "negative_gpt8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt42_m <- as.data.frame(conf_epfl_gpt_all_m_42$byClass) %>% 
  mutate(class_tweet = c("positive_gpt42", "neutral_gpt42", "negative_gpt42"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt41_m <- as.data.frame(conf_epfl_gpt_all_m_41$byClass) %>% 
  mutate(class_tweet = c("positive_gpt41", "neutral_gpt41", "negative_gpt41")) %>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt45_m <- as.data.frame(conf_epfl_gpt_all_m_45$byClass) %>% 
  mutate(class_tweet = c("positive_gpt45", "neutral_gpt45", "negative_gpt45"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt43_m <- as.data.frame(conf_epfl_gpt_all_m_43$byClass) %>% 
  mutate(class_tweet = c("positive_gpt43", "neutral_gpt43", "negative_gpt43"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt44_m <- as.data.frame(conf_epfl_gpt_all_m_44$byClass) %>% 
  mutate(class_tweet = c("positive_gpt44", "neutral_gpt44", "negative_gpt44"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt46_m <- as.data.frame(conf_epfl_gpt_all_m_46$byClass) %>% 
  mutate(class_tweet = c("positive_gpt46", "neutral_gpt46", "negative_gpt46"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt47_m <- as.data.frame(conf_epfl_gpt_all_m_47$byClass) %>% 
  mutate(class_tweet = c("positive_gpt47", "neutral_gpt47", "negative_gpt47"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt48_m <- as.data.frame(conf_epfl_gpt_all_m_48$byClass) %>% 
  mutate(class_tweet = c("positive_gpt48", "neutral_gpt48", "negative_gpt48"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral2_m <- as.data.frame(conf_epfl_mistral_all_m_2$byClass) %>% 
  mutate(class_tweet = c("positive_mistral2", "neutral_mistral2", "negative_mistral2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral1_m <- as.data.frame(conf_epfl_mistral_all_m_1$byClass) %>% 
  mutate(class_tweet = c("positive_mistral1", "neutral_mistral1", "negative_mistral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral3_m <- as.data.frame(conf_epfl_mistral_all_m_3$byClass) %>% 
  mutate(class_tweet = c("positive_mistral3", "neutral_mistral3", "negative_mistral3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral4_m <- as.data.frame(conf_epfl_mistral_all_m_4$byClass) %>% 
  mutate(class_tweet = c("positive_mistral4", "neutral_mistral4", "negative_mistral4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral5_m <- as.data.frame(conf_epfl_mistral_all_m_5$byClass) %>% 
  mutate(class_tweet = c("positive_mistral5", "neutral_mistral5", "negative_mistral5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral6_m <- as.data.frame(conf_epfl_mistral_all_m_6$byClass) %>% 
  mutate(class_tweet = c("positive_mistral6", "neutral_mistral6", "negative_mistral6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral7_m <- as.data.frame(conf_epfl_mistral_all_m_7$byClass) %>%
  mutate(class_tweet = c("positive_mistral7", "neutral_mistral7", "negative_mistral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mistral8_m <- as.data.frame(conf_epfl_mistral_all_m_8$byClass) %>% 
  mutate(class_tweet = c("positive_mistral8", "neutral_mistral8", "negative_mistral8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral2_m <- as.data.frame(conf_epfl_mixtral_all_m_2$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral2", "neutral_mixtral2", "negative_mixtral2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral1_m <- as.data.frame(conf_epfl_mixtral_all_m_1$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral1", "neutral_mixtral1", "negative_mixtral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral3_m <- as.data.frame(conf_epfl_mixtral_all_m_3$byClass) %>%
  mutate(class_tweet = c("positive_mixtral3", "neutral_mixtral3", "negative_mixtral3"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral4_m <- as.data.frame(conf_epfl_mixtral_all_m_4$byClass) %>%
  mutate(class_tweet = c("positive_mixtral4", "neutral_mixtral4", "negative_mixtral4"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral5_m <- as.data.frame(conf_epfl_mixtral_all_m_5$byClass) %>%
  mutate(class_tweet = c("positive_mixtral5", "neutral_mixtral5", "negative_mixtral5"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral6_m <- as.data.frame(conf_epfl_mixtral_all_m_6$byClass) %>%
  mutate(class_tweet = c("positive_mixtral6", "neutral_mixtral6", "negative_mixtral6"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral7_m <- as.data.frame(conf_epfl_mixtral_all_m_7$byClass) %>%
  mutate(class_tweet = c("positive_mixtral7", "neutral_mixtral7", "negative_mixtral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral8_m <- as.data.frame(conf_epfl_mixtral_all_m_8$byClass) %>%
  mutate(class_tweet = c("positive_mixtral8", "neutral_mixtral8", "negative_mixtral8"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_all_m <- class_gpt2_m %>% 
  full_join(class_gpt1_m) %>% 
  full_join(class_gpt3_m) %>% 
  full_join(class_gpt4_m) %>% 
  full_join(class_gpt42_m) %>% 
  full_join(class_gpt41_m) %>% 
  full_join(class_gpt45_m) %>% 
  full_join(class_gpt5_m) %>% 
  full_join(class_gpt6_m) %>% 
  full_join(class_gpt7_m) %>% 
  full_join(class_gpt8_m) %>% 
  full_join(class_mturk_m) %>% 
  full_join(class_gpt43_m) %>% 
  full_join(class_gpt44_m) %>% 
  full_join(class_gpt46_m) %>% 
  full_join(class_gpt47_m) %>% 
  full_join(class_gpt48_m) %>% 
  full_join(class_mistral2_m) %>% 
  full_join(class_mistral1_m) %>% 
  full_join(class_mistral3_m) %>% 
  full_join(class_mistral4_m) %>% 
  full_join(class_mistral5_m) %>% 
  full_join(class_mistral6_m) %>% 
  full_join(class_mistral7_m) %>%
  full_join(class_mistral8_m) %>% 
  full_join(class_mixtral2_m) %>% 
  full_join(class_mixtral1_m) %>% 
  full_join(class_mixtral3_m) %>%
  full_join(class_mixtral4_m) %>%
  full_join(class_mixtral5_m) %>%
  full_join(class_mixtral6_m) %>%
  full_join(class_mixtral7_m) %>%
  full_join(class_mixtral8_m) %>%
  full_join(class_vader_m) %>%
  separate(., class_tweet, into = c("class", "classifier"), 
           sep = "_") %>% 
  mutate(method = str_replace_all(classifier, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral2_m" = "Mistral prompt 2",
                                    "mistral1_m" = "Mistral prompt 1",
                                    "mistral3_m" = "Mistral prompt 3",
                                    "mistral4_m" = "Mistral prompt 4",
                                    "mistral5_m" = "Mistral prompt 5",
                                    "mistral6_m" = "Mistral prompt 6",
                                    "mistral7_m" = "Mistral prompt 7",
                                    "mistral8_m" = "Mistral prompt 8",
                                    "mixtral2_m" = "Mixtral prompt 2",
                                    "mixtral1_m" = "Mixtral prompt 1",
                                    "mixtral3_m" = "Mixtral prompt 3",
                                    "mixtral4_m" = "Mixtral prompt 4",
                                    "mixtral5_m" = "Mixtral prompt 5",
                                    "mixtral6_m" = "Mixtral prompt 6",
                                    "mixtral7_m" = "Mixtral prompt 7",
                                    "mixtral8_m" = "Mixtral prompt 8",
                                    "gpt42_m" = "GPT 4 prompt 2",
                                    "gpt41_m" = "GPT 4 prompt 1",
                                    "gpt43_m" = "GPT 4 prompt 3",
                                    "gpt44_m" = "GPT 4 prompt 4",
                                    "gpt45_m" = "GPT 4 prompt 5",
                                    "gpt46_m" = "GPT 4 prompt 6",
                                    "gpt47_m" = "GPT 4 prompt 7",
                                    "gpt48_m" = "GPT 4 prompt 8",
                                    "gpt2_m" = "GPT 3.5 prompt 2",
                                    "gpt1_m" = "GPT 3.5 prompt 1",
                                    "gpt3_m" = "GPT 3.5 prompt 3",
                                    "gpt4_m" = "GPT 3.5 prompt 4",
                                    "gpt5_m" = "GPT 3.5 prompt 5",
                                    "gpt6_m" = "GPT 3.5 prompt 6",
                                    "gpt7_m" = "GPT 3.5 prompt 7",
                                    "gpt8_m" = "GPT 3.5 prompt 8",
                                    "mturk_m" = "Amazon Mturk",
                                    "vader_m" = "Vader"))) %>% 
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
  mutate(annotator = "Co-author (1)")

class_all_m %>% 
  write_csv("outputs/confusion_matrix_per_class_all_annotator1.csv")

class_all_positive_m <- class_all_m %>% 
  filter(Stance == "positive") %>% 
  arrange(desc("F1 score"))

class_all_positive_m %>% 
  write_csv("outputs/confusion_matrix_per_class_positive_annotator1.csv")

class_all_negative_m <- class_all_m %>% 
  filter(Stance == "negative")%>% 
  arrange(desc("F1 score"))

class_all_negative_m %>% 
  write_csv("outputs/confusion_matrix_per_class_negative_annotator1.csv")

class_all_neutral_m <- class_all_m %>% 
  filter(Stance == "neutral")%>% 
  arrange(desc("F1 score"))

class_all_neutral_m %>% 
  write_csv("outputs/confusion_matrix_per_class_neutral_annotator1.csv")


# Annotator 2 -----------
## EPFL vs GPT ------------
for (i in prompts) {
  df_con_matrix_g<- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_g, sentiment_gpt) %>% 
    mutate(stance_g= factor(stance_g, ordered = TRUE,
                            levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_g',i,sep='_'), df_con_matrix_g)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_gpt_g<- confusionMatrix(df_con_matrix_g$sentiment_gpt,
                                    df_con_matrix_g$stance_g, mode = "everything")
  conf_epfl_gpt_g$prompt <- prompts[i]
  assign(paste('conf_epfl_gpt_all_g',i,sep='_'),conf_epfl_gpt_g) 
  # assign(paste('conf_epfl_gpt_all',i,sep='_'),conf_epfl_gpt) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_gpt_all", i, ".csv"))
  
}


## EPFL vs Mturk --------------
df_con_matrix_mturk_g<- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_g, agree_mturk) %>% 
  mutate(stance_g= factor(stance_g, ordered = TRUE,
                          levels = c("positive","neutral", "negative")),
         agree_mturk = factor(agree_mturk, ordered = TRUE,
                              levels = c("positive", "neutral", "negative"))) 

conf_epfl_mturk_g<- confusionMatrix(df_con_matrix_mturk_g$agree_mturk,
                                    df_con_matrix_mturk_g$stance_g, mode = "everything")

## EPFL vs vader --------------
df_con_matrix_vader_g<- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_g, sent_vader) %>% 
  mutate(stance_g= factor(stance_g, ordered = TRUE,
                          levels = c("positive","neutral", "negative")),
         sent_vader = factor(sent_vader, ordered = TRUE,
                             levels = c("positive", "neutral", "negative"))) 

conf_epfl_vader_g<- confusionMatrix(df_con_matrix_vader_g$sent_vader,
                                    df_con_matrix_vader_g$stance_g, mode = "everything")



## EPFL vs Mistral ---------------
for (i in prompts) {
  df_con_matrix_mistral_g<- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_g, sentiment_mistral) %>% 
    mutate(stance_g= factor(stance_g, ordered = TRUE,
                            levels = c("positive","neutral", "negative")),
           sentiment_mistral = factor(sentiment_mistral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mistral_g',i,sep='_'), df_con_matrix_mistral_g)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mistral_g<- confusionMatrix(df_con_matrix_mistral_g$sentiment_mistral,
                                        df_con_matrix_mistral_g$stance_g, mode = "everything")
  conf_epfl_mistral_g$prompt <- prompts[i]
  assign(paste('conf_epfl_mistral_all_g',i,sep='_'),conf_epfl_mistral_g) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs Mixtral ---------------
for (i in prompts) {
  df_con_matrix_mixtral_g<- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_g, sentiment_mixtral) %>% 
    mutate(stance_g= factor(stance_g, ordered = TRUE,
                            levels = c("positive","neutral", "negative")),
           sentiment_mixtral = factor(sentiment_mixtral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mixtral_g',i,sep='_'), df_con_matrix_mixtral_g)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mixtral_g<- confusionMatrix(df_con_matrix_mixtral_g$sentiment_mixtral,
                                        df_con_matrix_mixtral_g$stance_g, mode = "everything")
  conf_epfl_mixtral_g$prompt <- prompts[i]
  assign(paste('conf_epfl_mixtral_all_g',i,sep='_'),conf_epfl_mixtral_g) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs selecting majority class ------------  
df_con_matrix_majority_g<- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_g) %>%
  mutate(stance_majority = "neutral", 
         stance_g= factor(stance_g, ordered = TRUE,
                          levels = c("positive","neutral", "negative")),
         stance_majority = factor(stance_majority, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 


conf_epfl_majority_g<- confusionMatrix(df_con_matrix_majority_g$stance_majority,
                                       df_con_matrix_majority_g$stance_g)

# Merging all confusion matrices ------------
## Accuracy -------------
overall_all_fig_g<- as.data.frame(conf_epfl_mturk_g$overall) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_3$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_4$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_6$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_7$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_42$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_41$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_45$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_43$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_44$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_46$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_47$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_g_48$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_g_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_g_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_g_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_g_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_g_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_g_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_g_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_g_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_g_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_g_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_g_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_g_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_g_5$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_g_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_g_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_g_8$overall)) %>%
  cbind(as.data.frame(conf_epfl_vader_g$overall)) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral_all_g_2" = "Mistral prompt 2",
                                    "mistral_all_g_1" = "Mistral prompt 1",
                                    "mistral_all_g_3" = "Mistral prompt 3",
                                    "mistral_all_g_4" = "Mistral prompt 4",
                                    "mistral_all_g_5" = "Mistral prompt 5",
                                    "mistral_all_g_6" = "Mistral prompt 6",
                                    "mistral_all_g_7" = "Mistral prompt 7",
                                    "mistral_all_g_8" = "Mistral prompt 8",
                                    "mixtral_all_g_2" = "Mixtral prompt 2",
                                    "mixtral_all_g_1" = "Mixtral prompt 1",
                                    "mixtral_all_g_3" = "Mixtral prompt 3",
                                    "mixtral_all_g_4" = "Mixtral prompt 4",
                                    "mixtral_all_g_5" = "Mixtral prompt 5",
                                    "mixtral_all_g_6" = "Mixtral prompt 6",
                                    "mixtral_all_g_7" = "Mixtral prompt 7",
                                    "mixtral_all_g_8" = "Mixtral prompt 8",
                                    "gpt_all_g_42" = "GPT 4 prompt 2",
                                    "gpt_all_g_41" = "GPT 4 prompt 1",
                                    "gpt_all_g_43" = "GPT 4 prompt 3",
                                    "gpt_all_g_44" = "GPT 4 prompt 4",
                                    "gpt_all_g_45" = "GPT 4 prompt 5",
                                    "gpt_all_g_46" = "GPT 4 prompt 6",
                                    "gpt_all_g_47" = "GPT 4 prompt 7",
                                    "gpt_all_g_48" = "GPT 4 prompt 8",
                                    "gpt_all_g_2" = "GPT 3.5 prompt 2",
                                    "gpt_all_g_1" = "GPT 3.5 prompt 1",
                                    "gpt_all_g_3" = "GPT 3.5 prompt 3",
                                    "gpt_all_g_4" = "GPT 3.5 prompt 4",
                                    "gpt_all_g_5" = "GPT 3.5 prompt 5",
                                    "gpt_all_g_6" = "GPT 3.5 prompt 6",
                                    "gpt_all_g_7" = "GPT 3.5 prompt 7",
                                    "gpt_all_g_8" = "GPT 3.5 prompt 8",
                                    "mturk_g" = "Amazon Mturk",
                                    "vader_g" = "Vader"))) %>% 
  select(method, Accuracy, AccuracyLower, AccuracyUpper, AccuracyPValue) %>% 
  separate(col = "method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
  mutate(Pvalue = case_when(AccuracyPValue <= 0.05 ~ "<= 0.05",
                            .default = "> 0.05"),
         annotator = "Annotator",
         Prompt = replace_na(Prompt, "None"))


overall_all_g<- overall_all_fig_g%>% 
  mutate(Accuracy = round(Accuracy, 4),
         AccuracyLower = round(AccuracyLower, 4),
         AccuracyUpper = round(AccuracyUpper, 4),
         AccuracyPValue = round(AccuracyPValue, 6),
         AccuracyCI = paste("(", AccuracyLower, " - ", AccuracyUpper, ")", sep = "")) %>%
  select(Method, Prompt, Accuracy, AccuracyCI, AccuracyPValue) %>% 
  rename("Accuracy (95% CI)" = "AccuracyCI",
         "Accuracy (p-value)" = "AccuracyPValue")

overall_all_g%>%
  write_csv("outputs/confusion_matrix_accuracy_annotator2.csv")

### Plot accuracy ----------------
accuracy_fig_g<- overall_all_fig_g%>% 
  #filter(Method == "GPT 4") %>% 
  filter(!is.na(Prompt)) %>% 
  ggplot(aes(x = Prompt, y = Accuracy)) +
  geom_point(aes(color = Pvalue)) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper,
                    color = Pvalue),
                width = 0.2) +
  facet_grid(~Method, scales = "free_x") 

accuracy_fig_g

## Per class ---------
class_majority_g<- as.data.frame(conf_epfl_majority_g$byClass) %>% 
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
  select(Method, Prompt, Stance, `F1 score`, Sensitivity, Specificity) %>% 
  mutate(annotator = "Annotator") 



class_mturk_g<- as.data.frame(conf_epfl_mturk_g$byClass) %>% 
  mutate(class_tweet = c("positive_mturk", "neutral_mturk", "negative_mturk"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_vader_g<- as.data.frame(conf_epfl_vader_g$byClass) %>% 
  mutate(class_tweet = c("positive_vader", "neutral_vader", "negative_vader"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt2_g<- as.data.frame(conf_epfl_gpt_all_g_2$byClass) %>% 
  mutate(class_tweet = c("positive_gpt2", "neutral_gpt2", "negative_gpt2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt1_g<- as.data.frame(conf_epfl_gpt_all_g_1$byClass) %>% 
  mutate(class_tweet = c("positive_gpt1", "neutral_gpt1", "negative_gpt1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt3_g<- as.data.frame(conf_epfl_gpt_all_g_3$byClass) %>% 
  mutate(class_tweet = c("positive_gpt3", "neutral_gpt3", "negative_gpt3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt4_g<- as.data.frame(conf_epfl_gpt_all_g_4$byClass) %>% 
  mutate(class_tweet = c("positive_gpt4", "neutral_gpt4", "negative_gpt4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt5_g<- as.data.frame(conf_epfl_gpt_all_g_5$byClass) %>% 
  mutate(class_tweet = c("positive_gpt5", "neutral_gpt5", "negative_gpt5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt6_g<- as.data.frame(conf_epfl_gpt_all_g_6$byClass) %>% 
  mutate(class_tweet = c("positive_gpt6", "neutral_gpt6", "negative_gpt6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt7_g<- as.data.frame(conf_epfl_gpt_all_g_7$byClass) %>% 
  mutate(class_tweet = c("positive_gpt7", "neutral_gpt7", "negative_gpt7"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt8_g<- as.data.frame(conf_epfl_gpt_all_g_8$byClass) %>% 
  mutate(class_tweet = c("positive_gpt8", "neutral_gpt8", "negative_gpt8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt42_g<- as.data.frame(conf_epfl_gpt_all_g_42$byClass) %>% 
  mutate(class_tweet = c("positive_gpt42", "neutral_gpt42", "negative_gpt42"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt41_g<- as.data.frame(conf_epfl_gpt_all_g_41$byClass) %>% 
  mutate(class_tweet = c("positive_gpt41", "neutral_gpt41", "negative_gpt41")) %>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt45_g<- as.data.frame(conf_epfl_gpt_all_g_45$byClass) %>% 
  mutate(class_tweet = c("positive_gpt45", "neutral_gpt45", "negative_gpt45"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt43_g<- as.data.frame(conf_epfl_gpt_all_g_43$byClass) %>% 
  mutate(class_tweet = c("positive_gpt43", "neutral_gpt43", "negative_gpt43"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt44_g<- as.data.frame(conf_epfl_gpt_all_g_44$byClass) %>% 
  mutate(class_tweet = c("positive_gpt44", "neutral_gpt44", "negative_gpt44"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt46_g<- as.data.frame(conf_epfl_gpt_all_g_46$byClass) %>% 
  mutate(class_tweet = c("positive_gpt46", "neutral_gpt46", "negative_gpt46"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt47_g<- as.data.frame(conf_epfl_gpt_all_g_47$byClass) %>% 
  mutate(class_tweet = c("positive_gpt47", "neutral_gpt47", "negative_gpt47"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt48_g<- as.data.frame(conf_epfl_gpt_all_g_48$byClass) %>% 
  mutate(class_tweet = c("positive_gpt48", "neutral_gpt48", "negative_gpt48"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral2_g<- as.data.frame(conf_epfl_mistral_all_g_2$byClass) %>% 
  mutate(class_tweet = c("positive_mistral2", "neutral_mistral2", "negative_mistral2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral1_g<- as.data.frame(conf_epfl_mistral_all_g_1$byClass) %>% 
  mutate(class_tweet = c("positive_mistral1", "neutral_mistral1", "negative_mistral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral3_g<- as.data.frame(conf_epfl_mistral_all_g_3$byClass) %>% 
  mutate(class_tweet = c("positive_mistral3", "neutral_mistral3", "negative_mistral3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral4_g<- as.data.frame(conf_epfl_mistral_all_g_4$byClass) %>% 
  mutate(class_tweet = c("positive_mistral4", "neutral_mistral4", "negative_mistral4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral5_g<- as.data.frame(conf_epfl_mistral_all_g_5$byClass) %>% 
  mutate(class_tweet = c("positive_mistral5", "neutral_mistral5", "negative_mistral5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral6_g<- as.data.frame(conf_epfl_mistral_all_g_6$byClass) %>% 
  mutate(class_tweet = c("positive_mistral6", "neutral_mistral6", "negative_mistral6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral7_g<- as.data.frame(conf_epfl_mistral_all_g_7$byClass) %>%
  mutate(class_tweet = c("positive_mistral7", "neutral_mistral7", "negative_mistral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mistral8_g<- as.data.frame(conf_epfl_mistral_all_g_8$byClass) %>% 
  mutate(class_tweet = c("positive_mistral8", "neutral_mistral8", "negative_mistral8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral2_g<- as.data.frame(conf_epfl_mixtral_all_g_2$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral2", "neutral_mixtral2", "negative_mixtral2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral1_g<- as.data.frame(conf_epfl_mixtral_all_g_1$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral1", "neutral_mixtral1", "negative_mixtral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral3_g<- as.data.frame(conf_epfl_mixtral_all_g_3$byClass) %>%
  mutate(class_tweet = c("positive_mixtral3", "neutral_mixtral3", "negative_mixtral3"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral4_g<- as.data.frame(conf_epfl_mixtral_all_g_4$byClass) %>%
  mutate(class_tweet = c("positive_mixtral4", "neutral_mixtral4", "negative_mixtral4"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral5_g<- as.data.frame(conf_epfl_mixtral_all_g_5$byClass) %>%
  mutate(class_tweet = c("positive_mixtral5", "neutral_mixtral5", "negative_mixtral5"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral6_g<- as.data.frame(conf_epfl_mixtral_all_g_6$byClass) %>%
  mutate(class_tweet = c("positive_mixtral6", "neutral_mixtral6", "negative_mixtral6"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral7_g<- as.data.frame(conf_epfl_mixtral_all_g_7$byClass) %>%
  mutate(class_tweet = c("positive_mixtral7", "neutral_mixtral7", "negative_mixtral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral8_g<- as.data.frame(conf_epfl_mixtral_all_g_8$byClass) %>%
  mutate(class_tweet = c("positive_mixtral8", "neutral_mixtral8", "negative_mixtral8"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_all_g<- class_gpt2_g%>% 
  full_join(class_gpt1_g) %>% 
  full_join(class_gpt3_g) %>% 
  full_join(class_gpt4_g) %>% 
  full_join(class_gpt42_g) %>% 
  full_join(class_gpt41_g) %>% 
  full_join(class_gpt45_g) %>% 
  full_join(class_gpt5_g) %>% 
  full_join(class_gpt6_g) %>% 
  full_join(class_gpt7_g) %>% 
  full_join(class_gpt8_g) %>% 
  full_join(class_mturk_g) %>% 
  full_join(class_gpt43_g) %>% 
  full_join(class_gpt44_g) %>% 
  full_join(class_gpt46_g) %>% 
  full_join(class_gpt47_g) %>% 
  full_join(class_gpt48_g) %>% 
  full_join(class_mistral2_g) %>% 
  full_join(class_mistral1_g) %>% 
  full_join(class_mistral3_g) %>% 
  full_join(class_mistral4_g) %>% 
  full_join(class_mistral5_g) %>% 
  full_join(class_mistral6_g) %>% 
  full_join(class_mistral7_g) %>%
  full_join(class_mistral8_g) %>% 
  full_join(class_mixtral2_g) %>% 
  full_join(class_mixtral1_g) %>% 
  full_join(class_mixtral3_g) %>%
  full_join(class_mixtral4_g) %>%
  full_join(class_mixtral5_g) %>%
  full_join(class_mixtral6_g) %>%
  full_join(class_mixtral7_g) %>%
  full_join(class_mixtral8_g) %>%
  full_join(class_vader_g) %>%
  separate(., class_tweet, into = c("class", "classifier"), 
           sep = "_") %>% 
  mutate(method = str_replace_all(classifier, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral2_g" = "Mistral prompt 2",
                                    "mistral1_g" = "Mistral prompt 1",
                                    "mistral3_g" = "Mistral prompt 3",
                                    "mistral4_g" = "Mistral prompt 4",
                                    "mistral5_g" = "Mistral prompt 5",
                                    "mistral6_g" = "Mistral prompt 6",
                                    "mistral7_g" = "Mistral prompt 7",
                                    "mistral8_g" = "Mistral prompt 8",
                                    "mixtral2_g" = "Mixtral prompt 2",
                                    "mixtral1_g" = "Mixtral prompt 1",
                                    "mixtral3_g" = "Mixtral prompt 3",
                                    "mixtral4_g" = "Mixtral prompt 4",
                                    "mixtral5_g" = "Mixtral prompt 5",
                                    "mixtral6_g" = "Mixtral prompt 6",
                                    "mixtral7_g" = "Mixtral prompt 7",
                                    "mixtral8_g" = "Mixtral prompt 8",
                                    "gpt42_g" = "GPT 4 prompt 2",
                                    "gpt41_g" = "GPT 4 prompt 1",
                                    "gpt43_g" = "GPT 4 prompt 3",
                                    "gpt44_g" = "GPT 4 prompt 4",
                                    "gpt45_g" = "GPT 4 prompt 5",
                                    "gpt46_g" = "GPT 4 prompt 6",
                                    "gpt47_g" = "GPT 4 prompt 7",
                                    "gpt48_g" = "GPT 4 prompt 8",
                                    "gpt2_g" = "GPT 3.5 prompt 2",
                                    "gpt1_g" = "GPT 3.5 prompt 1",
                                    "gpt3_g" = "GPT 3.5 prompt 3",
                                    "gpt4_g" = "GPT 3.5 prompt 4",
                                    "gpt5_g" = "GPT 3.5 prompt 5",
                                    "gpt6_g" = "GPT 3.5 prompt 6",
                                    "gpt7_g" = "GPT 3.5 prompt 7",
                                    "gpt8_g" = "GPT 3.5 prompt 8",
                                    "mturk_g" = "Amazon Mturk",
                                    "vader_g" = "Vader"))) %>% 
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
  mutate(annotator = "Annotator")

class_all_g%>% 
  write_csv("outputs/confusion_matrix_per_class_all_annotator2.csv")

class_all_positive_g<- class_all_g%>% 
  filter(Stance == "positive") %>% 
  arrange(desc("F1 score"))

class_all_positive_g%>% 
  write_csv("outputs/confusion_matrix_per_class_positive_annotator2.csv")

class_all_negative_g<- class_all_g%>% 
  filter(Stance == "negative")%>% 
  arrange(desc("F1 score"))

class_all_negative_g%>% 
  write_csv("outputs/confusion_matrix_per_class_negative_annotator2.csv")

class_all_neutral_g <- class_all_g %>% 
  filter(Stance == "neutral")%>% 
  arrange(desc("F1 score"))

class_all_neutral_g %>% 
  write_csv("outputs/confusion_matrix_per_class_neutral_annotator2.csv")

# Annotator 3 -----------
## EPFL vs GPT ------------
for (i in prompts) {
  df_con_matrix_c <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_c, sentiment_gpt) %>% 
    mutate(stance_c = factor(stance_c, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_c',i,sep='_'), df_con_matrix_c)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_gpt_c <- confusionMatrix(df_con_matrix_c$sentiment_gpt,
                                     df_con_matrix_c$stance_c, mode = "everything")
  conf_epfl_gpt_c$prompt <- prompts[i]
  assign(paste('conf_epfl_gpt_all_c',i,sep='_'),conf_epfl_gpt_c) 
  # assign(paste('conf_epfl_gpt_all',i,sep='_'),conf_epfl_gpt) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_gpt_all", i, ".csv"))
  
}


## EPFL vs Mturk --------------
df_con_matrix_mturk_c <- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_c, agree_mturk) %>% 
  mutate(stance_c = factor(stance_c, ordered = TRUE,
                           levels = c("positive","neutral", "negative")),
         agree_mturk = factor(agree_mturk, ordered = TRUE,
                              levels = c("positive", "neutral", "negative"))) 

conf_epfl_mturk_c <- confusionMatrix(df_con_matrix_mturk_c$agree_mturk,
                                     df_con_matrix_mturk_c$stance_c, mode = "everything")

## EPFL vs vader --------------
df_con_matrix_vader_c <- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_c, sent_vader) %>% 
  mutate(stance_c = factor(stance_c, ordered = TRUE,
                           levels = c("positive","neutral", "negative")),
         sent_vader = factor(sent_vader, ordered = TRUE,
                             levels = c("positive", "neutral", "negative"))) 

conf_epfl_vader_c <- confusionMatrix(df_con_matrix_vader_c$sent_vader,
                                     df_con_matrix_vader_c$stance_c, mode = "everything")



## EPFL vs Mistral ---------------
for (i in prompts) {
  df_con_matrix_mistral_c <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_c, sentiment_mistral) %>% 
    mutate(stance_c = factor(stance_c, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_mistral = factor(sentiment_mistral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mistral_c',i,sep='_'), df_con_matrix_mistral_c)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mistral_c <- confusionMatrix(df_con_matrix_mistral_c$sentiment_mistral,
                                         df_con_matrix_mistral_c$stance_c, mode = "everything")
  conf_epfl_mistral_c$prompt <- prompts[i]
  assign(paste('conf_epfl_mistral_all_c',i,sep='_'),conf_epfl_mistral_c) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs Mixtral ---------------
for (i in prompts) {
  df_con_matrix_mixtral_c <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_c, sentiment_mixtral) %>% 
    mutate(stance_c = factor(stance_c, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_mixtral = factor(sentiment_mixtral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mixtral_c',i,sep='_'), df_con_matrix_mixtral_c)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mixtral_c <- confusionMatrix(df_con_matrix_mixtral_c$sentiment_mixtral,
                                         df_con_matrix_mixtral_c$stance_c, mode = "everything")
  conf_epfl_mixtral_c$prompt <- prompts[i]
  assign(paste('conf_epfl_mixtral_all_c',i,sep='_'),conf_epfl_mixtral_c) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs selecting majority class ------------  
df_con_matrix_majority_c <- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_c) %>%
  mutate(stance_majority = "neutral", 
         stance_c = factor(stance_c, ordered = TRUE,
                           levels = c("positive","neutral", "negative")),
         stance_majority = factor(stance_majority, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 


conf_epfl_majority_c <- confusionMatrix(df_con_matrix_majority_c$stance_majority,
                                        df_con_matrix_majority_c$stance_c)

# Merging all confusion matrices ------------
## Accuracy -------------
overall_all_fig_c <- as.data.frame(conf_epfl_mturk_c$overall) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_3$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_4$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_6$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_7$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_42$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_41$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_45$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_43$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_44$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_46$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_47$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_c_48$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_c_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_c_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_c_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_c_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_c_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_c_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_c_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_c_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_c_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_c_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_c_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_c_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_c_5$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_c_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_c_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_c_8$overall)) %>%
  cbind(as.data.frame(conf_epfl_vader_c$overall)) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral_all_c_2" = "Mistral prompt 2",
                                    "mistral_all_c_1" = "Mistral prompt 1",
                                    "mistral_all_c_3" = "Mistral prompt 3",
                                    "mistral_all_c_4" = "Mistral prompt 4",
                                    "mistral_all_c_5" = "Mistral prompt 5",
                                    "mistral_all_c_6" = "Mistral prompt 6",
                                    "mistral_all_c_7" = "Mistral prompt 7",
                                    "mistral_all_c_8" = "Mistral prompt 8",
                                    "mixtral_all_c_2" = "Mixtral prompt 2",
                                    "mixtral_all_c_1" = "Mixtral prompt 1",
                                    "mixtral_all_c_3" = "Mixtral prompt 3",
                                    "mixtral_all_c_4" = "Mixtral prompt 4",
                                    "mixtral_all_c_5" = "Mixtral prompt 5",
                                    "mixtral_all_c_6" = "Mixtral prompt 6",
                                    "mixtral_all_c_7" = "Mixtral prompt 7",
                                    "mixtral_all_c_8" = "Mixtral prompt 8",
                                    "gpt_all_c_42" = "GPT 4 prompt 2",
                                    "gpt_all_c_41" = "GPT 4 prompt 1",
                                    "gpt_all_c_43" = "GPT 4 prompt 3",
                                    "gpt_all_c_44" = "GPT 4 prompt 4",
                                    "gpt_all_c_45" = "GPT 4 prompt 5",
                                    "gpt_all_c_46" = "GPT 4 prompt 6",
                                    "gpt_all_c_47" = "GPT 4 prompt 7",
                                    "gpt_all_c_48" = "GPT 4 prompt 8",
                                    "gpt_all_c_2" = "GPT 3.5 prompt 2",
                                    "gpt_all_c_1" = "GPT 3.5 prompt 1",
                                    "gpt_all_c_3" = "GPT 3.5 prompt 3",
                                    "gpt_all_c_4" = "GPT 3.5 prompt 4",
                                    "gpt_all_c_5" = "GPT 3.5 prompt 5",
                                    "gpt_all_c_6" = "GPT 3.5 prompt 6",
                                    "gpt_all_c_7" = "GPT 3.5 prompt 7",
                                    "gpt_all_c_8" = "GPT 3.5 prompt 8",
                                    "mturk_c" = "Amazon Mturk",
                                    "vader_c" = "Vader"))) %>% 
  select(method, Accuracy, AccuracyLower, AccuracyUpper, AccuracyPValue) %>% 
  separate(col = "method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
  mutate(Pvalue = case_when(AccuracyPValue <= 0.05 ~ "<= 0.05",
                            .default = "> 0.05"),
         annotator = "General public",
         Prompt = replace_na(Prompt, "None"))


overall_all_c <- overall_all_fig_c %>% 
  mutate(Accuracy = round(Accuracy, 4),
         AccuracyLower = round(AccuracyLower, 4),
         AccuracyUpper = round(AccuracyUpper, 4),
         AccuracyPValue = round(AccuracyPValue, 6),
         AccuracyCI = paste("(", AccuracyLower, " - ", AccuracyUpper, ")", sep = "")) %>%
  select(Method, Prompt, Accuracy, AccuracyCI, AccuracyPValue) %>% 
  rename("Accuracy (95% CI)" = "AccuracyCI",
         "Accuracy (p-value)" = "AccuracyPValue")

overall_all_c %>%
  write_csv("outputs/confusion_matrix_accuracy_annotator3.csv")

### Plot accuracy ----------------
accuracy_fig_c <- overall_all_fig_c %>% 
  #filter(Method == "GPT 4") %>% 
  filter(!is.na(Prompt)) %>% 
  ggplot(aes(x = Prompt, y = Accuracy)) +
  geom_point(aes(color = Pvalue)) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper,
                    color = Pvalue),
                width = 0.2) +
  facet_grid(~Method, scales = "free_x") 

accuracy_fig_c

## Per class ---------
class_majority_c <- as.data.frame(conf_epfl_majority_c$byClass) %>% 
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
  select(Method, Prompt, Stance, `F1 score`, Sensitivity, Specificity) %>% 
  mutate(annotator = "General public") 



class_mturk_c <- as.data.frame(conf_epfl_mturk_c$byClass) %>% 
  mutate(class_tweet = c("positive_mturk", "neutral_mturk", "negative_mturk"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_vader_c <- as.data.frame(conf_epfl_vader_c$byClass) %>% 
  mutate(class_tweet = c("positive_vader", "neutral_vader", "negative_vader"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt2_c <- as.data.frame(conf_epfl_gpt_all_c_2$byClass) %>% 
  mutate(class_tweet = c("positive_gpt2", "neutral_gpt2", "negative_gpt2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt1_c <- as.data.frame(conf_epfl_gpt_all_c_1$byClass) %>% 
  mutate(class_tweet = c("positive_gpt1", "neutral_gpt1", "negative_gpt1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt3_c <- as.data.frame(conf_epfl_gpt_all_c_3$byClass) %>% 
  mutate(class_tweet = c("positive_gpt3", "neutral_gpt3", "negative_gpt3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt4_c <- as.data.frame(conf_epfl_gpt_all_c_4$byClass) %>% 
  mutate(class_tweet = c("positive_gpt4", "neutral_gpt4", "negative_gpt4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt5_c <- as.data.frame(conf_epfl_gpt_all_c_5$byClass) %>% 
  mutate(class_tweet = c("positive_gpt5", "neutral_gpt5", "negative_gpt5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt6_c <- as.data.frame(conf_epfl_gpt_all_c_6$byClass) %>% 
  mutate(class_tweet = c("positive_gpt6", "neutral_gpt6", "negative_gpt6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt7_c <- as.data.frame(conf_epfl_gpt_all_c_7$byClass) %>% 
  mutate(class_tweet = c("positive_gpt7", "neutral_gpt7", "negative_gpt7"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt8_c <- as.data.frame(conf_epfl_gpt_all_c_8$byClass) %>% 
  mutate(class_tweet = c("positive_gpt8", "neutral_gpt8", "negative_gpt8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt42_c <- as.data.frame(conf_epfl_gpt_all_c_42$byClass) %>% 
  mutate(class_tweet = c("positive_gpt42", "neutral_gpt42", "negative_gpt42"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt41_c <- as.data.frame(conf_epfl_gpt_all_c_41$byClass) %>% 
  mutate(class_tweet = c("positive_gpt41", "neutral_gpt41", "negative_gpt41")) %>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt45_c <- as.data.frame(conf_epfl_gpt_all_c_45$byClass) %>% 
  mutate(class_tweet = c("positive_gpt45", "neutral_gpt45", "negative_gpt45"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt43_c <- as.data.frame(conf_epfl_gpt_all_c_43$byClass) %>% 
  mutate(class_tweet = c("positive_gpt43", "neutral_gpt43", "negative_gpt43"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt44_c <- as.data.frame(conf_epfl_gpt_all_c_44$byClass) %>% 
  mutate(class_tweet = c("positive_gpt44", "neutral_gpt44", "negative_gpt44"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt46_c <- as.data.frame(conf_epfl_gpt_all_c_46$byClass) %>% 
  mutate(class_tweet = c("positive_gpt46", "neutral_gpt46", "negative_gpt46"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt47_c <- as.data.frame(conf_epfl_gpt_all_c_47$byClass) %>% 
  mutate(class_tweet = c("positive_gpt47", "neutral_gpt47", "negative_gpt47"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt48_c <- as.data.frame(conf_epfl_gpt_all_c_48$byClass) %>% 
  mutate(class_tweet = c("positive_gpt48", "neutral_gpt48", "negative_gpt48"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral2_c <- as.data.frame(conf_epfl_mistral_all_c_2$byClass) %>% 
  mutate(class_tweet = c("positive_mistral2", "neutral_mistral2", "negative_mistral2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral1_c <- as.data.frame(conf_epfl_mistral_all_c_1$byClass) %>% 
  mutate(class_tweet = c("positive_mistral1", "neutral_mistral1", "negative_mistral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral3_c <- as.data.frame(conf_epfl_mistral_all_c_3$byClass) %>% 
  mutate(class_tweet = c("positive_mistral3", "neutral_mistral3", "negative_mistral3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral4_c <- as.data.frame(conf_epfl_mistral_all_c_4$byClass) %>% 
  mutate(class_tweet = c("positive_mistral4", "neutral_mistral4", "negative_mistral4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral5_c <- as.data.frame(conf_epfl_mistral_all_c_5$byClass) %>% 
  mutate(class_tweet = c("positive_mistral5", "neutral_mistral5", "negative_mistral5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral6_c <- as.data.frame(conf_epfl_mistral_all_c_6$byClass) %>% 
  mutate(class_tweet = c("positive_mistral6", "neutral_mistral6", "negative_mistral6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral7_c <- as.data.frame(conf_epfl_mistral_all_c_7$byClass) %>%
  mutate(class_tweet = c("positive_mistral7", "neutral_mistral7", "negative_mistral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mistral8_c <- as.data.frame(conf_epfl_mistral_all_c_8$byClass) %>% 
  mutate(class_tweet = c("positive_mistral8", "neutral_mistral8", "negative_mistral8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral2_c <- as.data.frame(conf_epfl_mixtral_all_c_2$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral2", "neutral_mixtral2", "negative_mixtral2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral1_c <- as.data.frame(conf_epfl_mixtral_all_c_1$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral1", "neutral_mixtral1", "negative_mixtral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral3_c <- as.data.frame(conf_epfl_mixtral_all_c_3$byClass) %>%
  mutate(class_tweet = c("positive_mixtral3", "neutral_mixtral3", "negative_mixtral3"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral4_c <- as.data.frame(conf_epfl_mixtral_all_c_4$byClass) %>%
  mutate(class_tweet = c("positive_mixtral4", "neutral_mixtral4", "negative_mixtral4"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral5_c <- as.data.frame(conf_epfl_mixtral_all_c_5$byClass) %>%
  mutate(class_tweet = c("positive_mixtral5", "neutral_mixtral5", "negative_mixtral5"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral6_c <- as.data.frame(conf_epfl_mixtral_all_c_6$byClass) %>%
  mutate(class_tweet = c("positive_mixtral6", "neutral_mixtral6", "negative_mixtral6"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral7_c <- as.data.frame(conf_epfl_mixtral_all_c_7$byClass) %>%
  mutate(class_tweet = c("positive_mixtral7", "neutral_mixtral7", "negative_mixtral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral8_c <- as.data.frame(conf_epfl_mixtral_all_c_8$byClass) %>%
  mutate(class_tweet = c("positive_mixtral8", "neutral_mixtral8", "negative_mixtral8"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_all_c <- class_gpt2_c %>% 
  full_join(class_gpt1_c) %>% 
  full_join(class_gpt3_c) %>% 
  full_join(class_gpt4_c) %>% 
  full_join(class_gpt42_c) %>% 
  full_join(class_gpt41_c) %>% 
  full_join(class_gpt45_c) %>% 
  full_join(class_gpt5_c) %>% 
  full_join(class_gpt6_c) %>% 
  full_join(class_gpt7_c) %>% 
  full_join(class_gpt8_c) %>% 
  full_join(class_mturk_c) %>% 
  full_join(class_gpt43_c) %>% 
  full_join(class_gpt44_c) %>% 
  full_join(class_gpt46_c) %>% 
  full_join(class_gpt47_c) %>% 
  full_join(class_gpt48_c) %>% 
  full_join(class_mistral2_c) %>% 
  full_join(class_mistral1_c) %>% 
  full_join(class_mistral3_c) %>% 
  full_join(class_mistral4_c) %>% 
  full_join(class_mistral5_c) %>% 
  full_join(class_mistral6_c) %>% 
  full_join(class_mistral7_c) %>%
  full_join(class_mistral8_c) %>% 
  full_join(class_mixtral2_c) %>% 
  full_join(class_mixtral1_c) %>% 
  full_join(class_mixtral3_c) %>%
  full_join(class_mixtral4_c) %>%
  full_join(class_mixtral5_c) %>%
  full_join(class_mixtral6_c) %>%
  full_join(class_mixtral7_c) %>%
  full_join(class_mixtral8_c) %>%
  full_join(class_vader_c) %>%
  separate(., class_tweet, into = c("class", "classifier"), 
           sep = "_") %>% 
  mutate(method = str_replace_all(classifier, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral2_c" = "Mistral prompt 2",
                                    "mistral1_c" = "Mistral prompt 1",
                                    "mistral3_c" = "Mistral prompt 3",
                                    "mistral4_c" = "Mistral prompt 4",
                                    "mistral5_c" = "Mistral prompt 5",
                                    "mistral6_c" = "Mistral prompt 6",
                                    "mistral7_c" = "Mistral prompt 7",
                                    "mistral8_c" = "Mistral prompt 8",
                                    "mixtral2_c" = "Mixtral prompt 2",
                                    "mixtral1_c" = "Mixtral prompt 1",
                                    "mixtral3_c" = "Mixtral prompt 3",
                                    "mixtral4_c" = "Mixtral prompt 4",
                                    "mixtral5_c" = "Mixtral prompt 5",
                                    "mixtral6_c" = "Mixtral prompt 6",
                                    "mixtral7_c" = "Mixtral prompt 7",
                                    "mixtral8_c" = "Mixtral prompt 8",
                                    "gpt42_c" = "GPT 4 prompt 2",
                                    "gpt41_c" = "GPT 4 prompt 1",
                                    "gpt43_c" = "GPT 4 prompt 3",
                                    "gpt44_c" = "GPT 4 prompt 4",
                                    "gpt45_c" = "GPT 4 prompt 5",
                                    "gpt46_c" = "GPT 4 prompt 6",
                                    "gpt47_c" = "GPT 4 prompt 7",
                                    "gpt48_c" = "GPT 4 prompt 8",
                                    "gpt2_c" = "GPT 3.5 prompt 2",
                                    "gpt1_c" = "GPT 3.5 prompt 1",
                                    "gpt3_c" = "GPT 3.5 prompt 3",
                                    "gpt4_c" = "GPT 3.5 prompt 4",
                                    "gpt5_c" = "GPT 3.5 prompt 5",
                                    "gpt6_c" = "GPT 3.5 prompt 6",
                                    "gpt7_c" = "GPT 3.5 prompt 7",
                                    "gpt8_c" = "GPT 3.5 prompt 8",
                                    "mturk_c" = "Amazon Mturk",
                                    "vader_c" = "Vader"))) %>% 
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
  mutate(annotator = "General public")

class_all_c %>% 
  write_csv("outputs/confusion_matrix_per_class_all_annotator3.csv")

class_all_positive_c <- class_all_c %>% 
  filter(Stance == "positive") %>% 
  arrange(desc("F1 score"))

class_all_positive_c %>% 
  write_csv("outputs/confusion_matrix_per_class_positive_annotator3.csv")

class_all_negative_c <- class_all_c %>% 
  filter(Stance == "negative")%>% 
  arrange(desc("F1 score"))

class_all_negative_c %>% 
  write_csv("outputs/confusion_matrix_per_class_negative_annotator3.csv")

class_all_neutral_c <- class_all_c %>% 
  filter(Stance == "neutral")%>% 
  arrange(desc("F1 score"))

class_all_neutral_c %>% 
  write_csv("outputs/confusion_matrix_per_class_neutral_annotator3.csv")

# Annotator 4 -----------
## EPFL vs GPT ------------
for (i in prompts) {
  df_con_matrix_l <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_l, sentiment_gpt) %>% 
    mutate(stance_l = factor(stance_l, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_l',i,sep='_'), df_con_matrix_l)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_gpt_l <- confusionMatrix(df_con_matrix_l$sentiment_gpt,
                                     df_con_matrix_l$stance_l, mode = "everything")
  conf_epfl_gpt_l$prompt <- prompts[i]
  assign(paste('conf_epfl_gpt_all_l',i,sep='_'),conf_epfl_gpt_l) 
  # assign(paste('conf_epfl_gpt_all',i,sep='_'),conf_epfl_gpt) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_gpt_all", i, ".csv"))
  
}


## EPFL vs Mturk --------------
df_con_matrix_mturk_l <- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_l, agree_mturk) %>% 
  mutate(stance_l = factor(stance_l, ordered = TRUE,
                           levels = c("positive","neutral", "negative")),
         agree_mturk = factor(agree_mturk, ordered = TRUE,
                              levels = c("positive", "neutral", "negative"))) 

conf_epfl_mturk_l <- confusionMatrix(df_con_matrix_mturk_l$agree_mturk,
                                     df_con_matrix_mturk_l$stance_l, mode = "everything")

## EPFL vs vader --------------
df_con_matrix_vader_l <- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_l, sent_vader) %>% 
  mutate(stance_l = factor(stance_l, ordered = TRUE,
                           levels = c("positive","neutral", "negative")),
         sent_vader = factor(sent_vader, ordered = TRUE,
                             levels = c("positive", "neutral", "negative"))) 

conf_epfl_vader_l <- confusionMatrix(df_con_matrix_vader_l$sent_vader,
                                     df_con_matrix_vader_l$stance_l, mode = "everything")



## EPFL vs Mistral ---------------
for (i in prompts) {
  df_con_matrix_mistral_l <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_l, sentiment_mistral) %>% 
    mutate(stance_l = factor(stance_l, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_mistral = factor(sentiment_mistral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mistral_l',i,sep='_'), df_con_matrix_mistral_l)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mistral_l <- confusionMatrix(df_con_matrix_mistral_l$sentiment_mistral,
                                         df_con_matrix_mistral_l$stance_l, mode = "everything")
  conf_epfl_mistral_l$prompt <- prompts[i]
  assign(paste('conf_epfl_mistral_all_l',i,sep='_'),conf_epfl_mistral_l) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs Mixtral ---------------
for (i in prompts) {
  df_con_matrix_mixtral_l <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_l, sentiment_mixtral) %>% 
    mutate(stance_l = factor(stance_l, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_mixtral = factor(sentiment_mixtral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mixtral_l',i,sep='_'), df_con_matrix_mixtral_l)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mixtral_l <- confusionMatrix(df_con_matrix_mixtral_l$sentiment_mixtral,
                                         df_con_matrix_mixtral_l$stance_l, mode = "everything")
  conf_epfl_mixtral_l$prompt <- prompts[i]
  assign(paste('conf_epfl_mixtral_all_l',i,sep='_'),conf_epfl_mixtral_l) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs selecting majority class ------------  
df_con_matrix_majority_l <- df_annotator_all_clean %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(stance_l) %>%
  mutate(stance_majority = "neutral", 
         stance_l = factor(stance_l, ordered = TRUE,
                           levels = c("positive","neutral", "negative")),
         stance_majority = factor(stance_majority, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 


conf_epfl_majority_l <- confusionMatrix(df_con_matrix_majority_l$stance_majority,
                                        df_con_matrix_majority_l$stance_l)

# Merging all confusion matrices ------------
## Accuracy -------------
overall_all_fig_l <- as.data.frame(conf_epfl_mturk_l$overall) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_3$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_4$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_6$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_7$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_42$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_41$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_45$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_43$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_44$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_46$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_47$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_l_48$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_l_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_l_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_l_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_l_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_l_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_all_l_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_l_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_all_l_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_l_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_l_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_all_l_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_l_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_l_5$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_l_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_l_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_all_l_8$overall)) %>%
  cbind(as.data.frame(conf_epfl_vader_l$overall)) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral_all_l_2" = "Mistral prompt 2",
                                    "mistral_all_l_1" = "Mistral prompt 1",
                                    "mistral_all_l_3" = "Mistral prompt 3",
                                    "mistral_all_l_4" = "Mistral prompt 4",
                                    "mistral_all_l_5" = "Mistral prompt 5",
                                    "mistral_all_l_6" = "Mistral prompt 6",
                                    "mistral_all_l_7" = "Mistral prompt 7",
                                    "mistral_all_l_8" = "Mistral prompt 8",
                                    "mixtral_all_l_2" = "Mixtral prompt 2",
                                    "mixtral_all_l_1" = "Mixtral prompt 1",
                                    "mixtral_all_l_3" = "Mixtral prompt 3",
                                    "mixtral_all_l_4" = "Mixtral prompt 4",
                                    "mixtral_all_l_5" = "Mixtral prompt 5",
                                    "mixtral_all_l_6" = "Mixtral prompt 6",
                                    "mixtral_all_l_7" = "Mixtral prompt 7",
                                    "mixtral_all_l_8" = "Mixtral prompt 8",
                                    "gpt_all_l_42" = "GPT 4 prompt 2",
                                    "gpt_all_l_41" = "GPT 4 prompt 1",
                                    "gpt_all_l_43" = "GPT 4 prompt 3",
                                    "gpt_all_l_44" = "GPT 4 prompt 4",
                                    "gpt_all_l_45" = "GPT 4 prompt 5",
                                    "gpt_all_l_46" = "GPT 4 prompt 6",
                                    "gpt_all_l_47" = "GPT 4 prompt 7",
                                    "gpt_all_l_48" = "GPT 4 prompt 8",
                                    "gpt_all_l_2" = "GPT 3.5 prompt 2",
                                    "gpt_all_l_1" = "GPT 3.5 prompt 1",
                                    "gpt_all_l_3" = "GPT 3.5 prompt 3",
                                    "gpt_all_l_4" = "GPT 3.5 prompt 4",
                                    "gpt_all_l_5" = "GPT 3.5 prompt 5",
                                    "gpt_all_l_6" = "GPT 3.5 prompt 6",
                                    "gpt_all_l_7" = "GPT 3.5 prompt 7",
                                    "gpt_all_l_8" = "GPT 3.5 prompt 8",
                                    "mturk_l" = "Amazon Mturk",
                                    "vader_l" = "Vader"))) %>% 
  select(method, Accuracy, AccuracyLower, AccuracyUpper, AccuracyPValue) %>% 
  separate(col = "method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
  mutate(Pvalue = case_when(AccuracyPValue <= 0.05 ~ "<= 0.05",
                            .default = "> 0.05"),
         annotator = "Co-author (2)",
         Prompt = replace_na(Prompt, "None"))


overall_all_l <- overall_all_fig_l %>% 
  mutate(Accuracy = round(Accuracy, 4),
         AccuracyLower = round(AccuracyLower, 4),
         AccuracyUpper = round(AccuracyUpper, 4),
         AccuracyPValue = round(AccuracyPValue, 6),
         AccuracyCI = paste("(", AccuracyLower, " - ", AccuracyUpper, ")", sep = "")) %>%
  select(Method, Prompt, Accuracy, AccuracyCI, AccuracyPValue) %>% 
  rename("Accuracy (95% CI)" = "AccuracyCI",
         "Accuracy (p-value)" = "AccuracyPValue")

overall_all_l %>%
  write_csv("outputs/confusion_matrix_accuracy_annotator4.csv")

### Plot accuracy ----------------
accuracy_fig_l <- overall_all_fig_l %>% 
  #filter(Method == "GPT 4") %>% 
  filter(!is.na(Prompt)) %>% 
  ggplot(aes(x = Prompt, y = Accuracy)) +
  geom_point(aes(color = Pvalue)) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper,
                    color = Pvalue),
                width = 0.2) +
  facet_grid(~Method, scales = "free_x") 

accuracy_fig_c

## Per class ---------
class_majority_l <- as.data.frame(conf_epfl_majority_l$byClass) %>% 
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
  select(Method, Prompt, Stance, `F1 score`, Sensitivity, Specificity) %>% 
  mutate(annotator = "Co-author (2)") 



class_mturk_l <- as.data.frame(conf_epfl_mturk_l$byClass) %>% 
  mutate(class_tweet = c("positive_mturk", "neutral_mturk", "negative_mturk"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_vader_l <- as.data.frame(conf_epfl_vader_l$byClass) %>% 
  mutate(class_tweet = c("positive_vader", "neutral_vader", "negative_vader"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt2_l <- as.data.frame(conf_epfl_gpt_all_l_2$byClass) %>% 
  mutate(class_tweet = c("positive_gpt2", "neutral_gpt2", "negative_gpt2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt1_l <- as.data.frame(conf_epfl_gpt_all_l_1$byClass) %>% 
  mutate(class_tweet = c("positive_gpt1", "neutral_gpt1", "negative_gpt1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt3_l <- as.data.frame(conf_epfl_gpt_all_l_3$byClass) %>% 
  mutate(class_tweet = c("positive_gpt3", "neutral_gpt3", "negative_gpt3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt4_l <- as.data.frame(conf_epfl_gpt_all_l_4$byClass) %>% 
  mutate(class_tweet = c("positive_gpt4", "neutral_gpt4", "negative_gpt4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt5_l <- as.data.frame(conf_epfl_gpt_all_l_5$byClass) %>% 
  mutate(class_tweet = c("positive_gpt5", "neutral_gpt5", "negative_gpt5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt6_l <- as.data.frame(conf_epfl_gpt_all_l_6$byClass) %>% 
  mutate(class_tweet = c("positive_gpt6", "neutral_gpt6", "negative_gpt6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt7_l <- as.data.frame(conf_epfl_gpt_all_l_7$byClass) %>% 
  mutate(class_tweet = c("positive_gpt7", "neutral_gpt7", "negative_gpt7"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt8_l <- as.data.frame(conf_epfl_gpt_all_l_8$byClass) %>% 
  mutate(class_tweet = c("positive_gpt8", "neutral_gpt8", "negative_gpt8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt42_l <- as.data.frame(conf_epfl_gpt_all_l_42$byClass) %>% 
  mutate(class_tweet = c("positive_gpt42", "neutral_gpt42", "negative_gpt42"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt41_l <- as.data.frame(conf_epfl_gpt_all_l_41$byClass) %>% 
  mutate(class_tweet = c("positive_gpt41", "neutral_gpt41", "negative_gpt41")) %>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt45_l <- as.data.frame(conf_epfl_gpt_all_l_45$byClass) %>% 
  mutate(class_tweet = c("positive_gpt45", "neutral_gpt45", "negative_gpt45"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt43_l <- as.data.frame(conf_epfl_gpt_all_l_43$byClass) %>% 
  mutate(class_tweet = c("positive_gpt43", "neutral_gpt43", "negative_gpt43"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt44_l <- as.data.frame(conf_epfl_gpt_all_l_44$byClass) %>% 
  mutate(class_tweet = c("positive_gpt44", "neutral_gpt44", "negative_gpt44"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt46_l <- as.data.frame(conf_epfl_gpt_all_l_46$byClass) %>% 
  mutate(class_tweet = c("positive_gpt46", "neutral_gpt46", "negative_gpt46"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt47_l <- as.data.frame(conf_epfl_gpt_all_l_47$byClass) %>% 
  mutate(class_tweet = c("positive_gpt47", "neutral_gpt47", "negative_gpt47"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt48_l <- as.data.frame(conf_epfl_gpt_all_l_48$byClass) %>% 
  mutate(class_tweet = c("positive_gpt48", "neutral_gpt48", "negative_gpt48"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral2_l <- as.data.frame(conf_epfl_mistral_all_l_2$byClass) %>% 
  mutate(class_tweet = c("positive_mistral2", "neutral_mistral2", "negative_mistral2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral1_l <- as.data.frame(conf_epfl_mistral_all_l_1$byClass) %>% 
  mutate(class_tweet = c("positive_mistral1", "neutral_mistral1", "negative_mistral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral3_l <- as.data.frame(conf_epfl_mistral_all_l_3$byClass) %>% 
  mutate(class_tweet = c("positive_mistral3", "neutral_mistral3", "negative_mistral3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral4_l <- as.data.frame(conf_epfl_mistral_all_l_4$byClass) %>% 
  mutate(class_tweet = c("positive_mistral4", "neutral_mistral4", "negative_mistral4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral5_l <- as.data.frame(conf_epfl_mistral_all_l_5$byClass) %>% 
  mutate(class_tweet = c("positive_mistral5", "neutral_mistral5", "negative_mistral5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral6_l <- as.data.frame(conf_epfl_mistral_all_l_6$byClass) %>% 
  mutate(class_tweet = c("positive_mistral6", "neutral_mistral6", "negative_mistral6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mistral7_l <- as.data.frame(conf_epfl_mistral_all_l_7$byClass) %>%
  mutate(class_tweet = c("positive_mistral7", "neutral_mistral7", "negative_mistral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mistral8_l <- as.data.frame(conf_epfl_mistral_all_l_8$byClass) %>% 
  mutate(class_tweet = c("positive_mistral8", "neutral_mistral8", "negative_mistral8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral2_l <- as.data.frame(conf_epfl_mixtral_all_l_2$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral2", "neutral_mixtral2", "negative_mixtral2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral1_l <- as.data.frame(conf_epfl_mixtral_all_l_1$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral1", "neutral_mixtral1", "negative_mixtral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_mixtral3_l <- as.data.frame(conf_epfl_mixtral_all_l_3$byClass) %>%
  mutate(class_tweet = c("positive_mixtral3", "neutral_mixtral3", "negative_mixtral3"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral4_l <- as.data.frame(conf_epfl_mixtral_all_l_4$byClass) %>%
  mutate(class_tweet = c("positive_mixtral4", "neutral_mixtral4", "negative_mixtral4"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral5_l <- as.data.frame(conf_epfl_mixtral_all_l_5$byClass) %>%
  mutate(class_tweet = c("positive_mixtral5", "neutral_mixtral5", "negative_mixtral5"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral6_l <- as.data.frame(conf_epfl_mixtral_all_l_6$byClass) %>%
  mutate(class_tweet = c("positive_mixtral6", "neutral_mixtral6", "negative_mixtral6"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral7_l <- as.data.frame(conf_epfl_mixtral_all_l_7$byClass) %>%
  mutate(class_tweet = c("positive_mixtral7", "neutral_mixtral7", "negative_mixtral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_mixtral8_l <- as.data.frame(conf_epfl_mixtral_all_l_8$byClass) %>%
  mutate(class_tweet = c("positive_mixtral8", "neutral_mixtral8", "negative_mixtral8"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_all_l <- class_gpt2_l %>% 
  full_join(class_gpt1_l) %>% 
  full_join(class_gpt3_l) %>% 
  full_join(class_gpt4_l) %>% 
  full_join(class_gpt42_l) %>% 
  full_join(class_gpt41_l) %>% 
  full_join(class_gpt45_l) %>% 
  full_join(class_gpt5_l) %>% 
  full_join(class_gpt6_l) %>% 
  full_join(class_gpt7_l) %>% 
  full_join(class_gpt8_l) %>% 
  full_join(class_mturk_l) %>% 
  full_join(class_gpt43_l) %>% 
  full_join(class_gpt44_l) %>% 
  full_join(class_gpt46_l) %>% 
  full_join(class_gpt47_l) %>% 
  full_join(class_gpt48_l) %>% 
  full_join(class_mistral2_l) %>% 
  full_join(class_mistral1_l) %>% 
  full_join(class_mistral3_l) %>% 
  full_join(class_mistral4_l) %>% 
  full_join(class_mistral5_l) %>% 
  full_join(class_mistral6_l) %>% 
  full_join(class_mistral7_l) %>%
  full_join(class_mistral8_l) %>% 
  full_join(class_mixtral2_l) %>% 
  full_join(class_mixtral1_l) %>% 
  full_join(class_mixtral3_l) %>%
  full_join(class_mixtral4_l) %>%
  full_join(class_mixtral5_l) %>%
  full_join(class_mixtral6_l) %>%
  full_join(class_mixtral7_l) %>%
  full_join(class_mixtral8_l) %>%
  full_join(class_vader_l) %>%
  separate(., class_tweet, into = c("class", "classifier"), 
           sep = "_") %>% 
  mutate(method = str_replace_all(classifier, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral2_l" = "Mistral prompt 2",
                                    "mistral1_l" = "Mistral prompt 1",
                                    "mistral3_l" = "Mistral prompt 3",
                                    "mistral4_l" = "Mistral prompt 4",
                                    "mistral5_l" = "Mistral prompt 5",
                                    "mistral6_l" = "Mistral prompt 6",
                                    "mistral7_l" = "Mistral prompt 7",
                                    "mistral8_l" = "Mistral prompt 8",
                                    "mixtral2_l" = "Mixtral prompt 2",
                                    "mixtral1_l" = "Mixtral prompt 1",
                                    "mixtral3_l" = "Mixtral prompt 3",
                                    "mixtral4_l" = "Mixtral prompt 4",
                                    "mixtral5_l" = "Mixtral prompt 5",
                                    "mixtral6_l" = "Mixtral prompt 6",
                                    "mixtral7_l" = "Mixtral prompt 7",
                                    "mixtral8_l" = "Mixtral prompt 8",
                                    "gpt42_l" = "GPT 4 prompt 2",
                                    "gpt41_l" = "GPT 4 prompt 1",
                                    "gpt43_l" = "GPT 4 prompt 3",
                                    "gpt44_l" = "GPT 4 prompt 4",
                                    "gpt45_l" = "GPT 4 prompt 5",
                                    "gpt46_l" = "GPT 4 prompt 6",
                                    "gpt47_l" = "GPT 4 prompt 7",
                                    "gpt48_l" = "GPT 4 prompt 8",
                                    "gpt2_l" = "GPT 3.5 prompt 2",
                                    "gpt1_l" = "GPT 3.5 prompt 1",
                                    "gpt3_l" = "GPT 3.5 prompt 3",
                                    "gpt4_l" = "GPT 3.5 prompt 4",
                                    "gpt5_l" = "GPT 3.5 prompt 5",
                                    "gpt6_l" = "GPT 3.5 prompt 6",
                                    "gpt7_l" = "GPT 3.5 prompt 7",
                                    "gpt8_l" = "GPT 3.5 prompt 8",
                                    "mturk_l" = "Amazon Mturk",
                                    "vader_l" = "Vader"))) %>% 
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
  mutate(annotator = "Co-author (2)")

class_all_l %>% 
  write_csv("outputs/confusion_matrix_per_class_all_annotator4.csv")

class_all_positive_l <- class_all_l %>% 
  filter(Stance == "positive") %>% 
  arrange(desc("F1 score"))

class_all_positive_l %>% 
  write_csv("outputs/confusion_matrix_per_class_positive_annotator4.csv")

class_all_negative_l <- class_all_l %>% 
  filter(Stance == "negative")%>% 
  arrange(desc("F1 score"))

class_all_negative_l %>% 
  write_csv("outputs/confusion_matrix_per_class_negative_annotator4.csv")

class_all_neutral_l <- class_all_l %>% 
  filter(Stance == "neutral")%>% 
  arrange(desc("F1 score"))

class_all_neutral_l %>% 
  write_csv("outputs/confusion_matrix_per_class_neutral_annotator4.csv")

# Combined figure for accuracy and CI --------------
## Dataset for CI and accuracy selecting majority class per agreement facet ---------
ci_majority_accuracy_annotators <- data.frame(
  xmin = 0,
  xmax = c(2, 2, 9, 9, 9, 9,
           2, 2, 9, 9, 9, 9,
           2, 2, 9, 9, 9, 9,
           2, 2, 9, 9, 9, 9),
  ymin = c(conf_epfl_majority_g$overall[[3]],
           conf_epfl_majority_g$overall[[3]],
           conf_epfl_majority_g$overall[[3]],
           conf_epfl_majority_g$overall[[3]],
           conf_epfl_majority_g$overall[[3]],
           conf_epfl_majority_g$overall[[3]],
           conf_epfl_majority_m$overall[[3]],
           conf_epfl_majority_m$overall[[3]],
           conf_epfl_majority_m$overall[[3]],
           conf_epfl_majority_m$overall[[3]],
           conf_epfl_majority_m$overall[[3]],
           conf_epfl_majority_m$overall[[3]],
           conf_epfl_majority_l$overall[[3]],
           conf_epfl_majority_l$overall[[3]],
           conf_epfl_majority_l$overall[[3]],
           conf_epfl_majority_l$overall[[3]],
           conf_epfl_majority_l$overall[[3]],
           conf_epfl_majority_l$overall[[3]],
           conf_epfl_majority_c$overall[[3]],
           conf_epfl_majority_c$overall[[3]],
           conf_epfl_majority_c$overall[[3]],
           conf_epfl_majority_c$overall[[3]],
           conf_epfl_majority_c$overall[[3]],
           conf_epfl_majority_c$overall[[3]]),
  ymax = c(conf_epfl_majority_g$overall[[4]],
           conf_epfl_majority_g$overall[[4]],
           conf_epfl_majority_g$overall[[4]],
           conf_epfl_majority_g$overall[[4]],
           conf_epfl_majority_g$overall[[4]],
           conf_epfl_majority_g$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_l$overall[[4]],
           conf_epfl_majority_l$overall[[4]],
           conf_epfl_majority_l$overall[[4]],
           conf_epfl_majority_l$overall[[4]],
           conf_epfl_majority_l$overall[[4]],
           conf_epfl_majority_l$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]]),
  annotator = c("Annotator", "Annotator", "Annotator",
                "Annotator", "Annotator", "Annotator",
                "Co-author (1)", "Co-author (1)",
                "Co-author (1)", "Co-author (1)",
                "Co-author (1)", "Co-author (1)",
                "Co-author (2)", "Co-author (2)",
                "Co-author (2)", "Co-author (2)",
                "Co-author (2)", "Co-author (2)",
                "General public", "General public",
                "General public", "General public",
                "General public", "General public"),
  Method = c("Mturk", "Vader","GPT 3.5", "GPT 4",
             "Mistral", "Mixtral",
             "Mturk", "Vader","GPT 3.5", "GPT 4",
             "Mistral", "Mixtral",
             "Mturk", "Vader","GPT 3.5", "GPT 4",
             "Mistral", "Mixtral",
             "Mturk", "Vader","GPT 3.5", "GPT 4",
             "Mistral", "Mixtral"))

majority_accuracy_annotators <- data.frame(
  hline_accuracy = c(conf_epfl_majority_g$overall[[1]],
                     conf_epfl_majority_m$overall[[1]],
                     conf_epfl_majority_l$overall[[1]],
                     conf_epfl_majority_c$overall[[1]]),
  annotator = c("Annotator", "Co-author (1)", "Co-author (2)", 
                "General public")
)

#### Dataset ---------
overall_all_total_annotators <- overall_all_fig_g %>% 
  rbind(overall_all_fig_m) %>%
  rbind(overall_all_fig_l) %>% 
  rbind(overall_all_fig_c) %>% 
  left_join(majority_accuracy_annotators,
            by = "annotator") %>%
  mutate(Method = case_when(Method == "Amazon Mturk" ~ "Mturk",
                            .default = Method))

#### Figure ----------------
accuracy_fig_all_annotators <- overall_all_total_annotators  %>% 
  ggplot(aes(x = Prompt, y = Accuracy)) +
  geom_point(aes(color = factor(Pvalue), 
                 shape = factor(Pvalue),
                 size = factor(Pvalue))) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper,
                    color = Pvalue),
                width = 0.2) +
  geom_segment(aes(x = -Inf, xend = Inf,
                   y = hline_accuracy,
                   yend = hline_accuracy,
                   linetype = annotator),
               size = 1) +
  geom_rect(data = ci_majority_accuracy_annotators,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = annotator),
            alpha = 0.2, inherit.aes = FALSE) +
  facet_grid(annotator~Method, scales = "free_x") +
  theme_bw() +
  scale_linetype_manual(values = c("Partial agreement" = 2,
                                   "Full agreement" = 3)) +
  scale_color_manual(values = c("<= 0.05" = "darkviolet",
                                "> 0.05" = "black"),
                     labels = c("<= 0.05", 
                                "> 0.05")) +
  scale_shape_manual(values = c("<= 0.05" = "square",
                                "> 0.05" = "circle"),
                     labels = c("<= 0.05", 
                                "> 0.05")) +
  scale_size_manual(values = c("<= 0.05" = 3,
                               "> 0.05" = 2.5),
                    labels = c("<= 0.05", 
                               "> 0.05")) +
  scale_fill_manual(values = c("Partial agreement" = "darkgrey",
                               "Full agreement" = "black"),
                    labels = c("Partial agreement",
                               "Full agreement")) +
  labs(color = "P value",
       shape = "P value",
       size = "P value",
       linetype = "Accuracy for selecting \nmajority class",
       fill = "Confidence interval for \nselecting majority class") +
  theme(text = element_text(size = 14),
        strip.text.x = element_text(size = 14),  
        strip.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        panel.spacing.y = unit(0, "lines"),
        axis.text = element_text(color = "black")) 

accuracy_fig_all_annotators

ggsave("outputs/accuracy_figure.jpeg",
       accuracy_fig_all,
       width = 10, height = 6)
