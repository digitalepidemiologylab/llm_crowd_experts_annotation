# Script information ------------
#' Aim: Comparing all annotations
#' Author: Laura Espinosa
#' Date created: 12 October 2023
#' Date updated: 12 October 2023

# Get all datasets -------------
df_all_agree <- df_mturk_annot_clean %>% 
  full_join(epfl_df, by = "text") %>% 
  full_join(gpt_clean, by = "text") %>% 
  full_join(mistral_clean, by = c("text", "prompt")) %>% 
  full_join(mixtral_clean, by = c("text", "prompt")) %>%
  full_join(df_vader, by = "text") %>% 
  filter(!is.na(sent_l)) %>% 
  select(-id_tweets.x, -id_tweets.y) %>% # id_tweets comes from gpt_clean (supossedly same as epfl_df)
  filter(agree_stance == 1)


## To retrieve the real tweet ids -------------
tweets_id_real <- select(df_mturk_annot_clean, 'text') %>% 
  left_join(select(df_mturk, 'tweet_id', "text"), by = "text") %>% 
  filter(duplicated(text) == FALSE) %>% 
  left_join(select(df_mturk_annot_clean, 'text', 'id_tweets'),
            by = "text") 

# Merge databases ----------------
df_all_agree_clean <- df_all_agree %>% 
  rename("neutral_mturk" = "neutral",
         "positive_mturk" = "positive",
         "negative_mturk" = "negative") %>% 
  select(-text) %>% 
  filter(!is.na(stance_epfl)) %>% 
  left_join(select(tweets_id_real, tweet_id, id_tweets),
            by = "id_tweets")

# Confusion matrix --------
prompts_agree <- df_all_agree_clean$prompt %>% 
  unique()

## EPFL vs GPT ------------
for (i in prompts_agree) {
  df_con_matrix_agree <- df_all_agree_clean %>% 
    filter(prompt == i) %>% 
    select(stance_epfl, sentiment_gpt) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  
  #prompt_loop[i] <- prompts[i]
  conf_epfl_gpt_agree <- confusionMatrix(df_con_matrix_agree$sentiment_gpt,
                                         df_con_matrix_agree$stance_epfl)
  conf_epfl_gpt_agree$prompt <- prompts_agree[i]
  assign(paste('conf_epfl_gpt_agree_all',i,sep='_'),conf_epfl_gpt_agree) #%>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_gpt_agree_all", i, ".csv"))
  
}

## EPFL vs Mistral ---------------
for (i in prompts_agree) {
  df_con_matrix_mistral_agree <- df_all_agree_clean %>% 
    filter(prompt == i) %>% 
    select(stance_epfl, sentiment_mistral) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_mistral = factor(sentiment_mistral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mistral_agree <- confusionMatrix(df_con_matrix_mistral_agree$sentiment_mistral,
                                       df_con_matrix_mistral_agree$stance_epfl, mode = "everything")
  conf_epfl_mistral_agree$prompt <- prompts_agree[i]
   assign(paste('conf_epfl_mistral_agree_all',i,sep='_'),conf_epfl_mistral_agree) #%>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_agree_all", i, ".csv"))
  
}

## EPFL vs Mixtral ---------------
for (i in prompts_agree) {
  df_con_matrix_mixtral_agree <- df_all_agree_clean %>% 
    filter(prompt == i) %>% 
    select(stance_epfl, sentiment_mixtral) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_mixtral = factor(sentiment_mixtral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mixtral_agree <- confusionMatrix(df_con_matrix_mixtral_agree$sentiment_mixtral,
                                             df_con_matrix_mixtral_agree$stance_epfl, mode = "everything")
  conf_epfl_mixtral_agree$prompt <- prompts_agree[i]
  assign(paste('conf_epfl_mixtral_agree_all',i,sep='_'),conf_epfl_mixtral_agree) #%>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_agree_all", i, ".csv"))
  
}

## EPFL vs Mturk --------------
df_con_matrix_mturk_agree <- df_all_agree_clean %>% 
    filter(!duplicated(id_tweets)) %>% 
    select(stance_epfl, agree_mturk) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           agree_mturk = factor(agree_mturk, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  

  conf_epfl_mturk_agree <- confusionMatrix(df_con_matrix_mturk_agree$agree_mturk,
                                     df_con_matrix_mturk_agree$stance_epfl)
  
  # conf_epfl_mturk_agree %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mturk_agree.csv"))

## EPFL vs Vader --------------
  df_con_matrix_vader_agree <- df_all_agree_clean %>% 
    filter(!duplicated(id_tweets)) %>% 
    select(stance_epfl, sent_vader) %>% 
    mutate(sent_vader = tolower(sent_vader)) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sent_vader = factor(sent_vader, ordered = TRUE,
                                levels = c("positive", "neutral", "negative"))) 
  
  
  conf_epfl_vader_agree <- confusionMatrix(df_con_matrix_vader_agree$sent_vader,
                                           df_con_matrix_vader_agree$stance_epfl)
  
  
## EPFL vs selecting majority class ------------  
  df_con_matrix_majority_agree <- df_all_agree_clean %>% 
    filter(!duplicated(id_tweets)) %>% 
    select(stance_epfl) %>%
    mutate(stance_majority = "neutral", 
           stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           stance_majority = factor(stance_majority, ordered = TRUE,
                                levels = c("positive", "neutral", "negative"))) 
  
  
  conf_epfl_majority_agree <- confusionMatrix(df_con_matrix_majority_agree$stance_majority,
                                              df_con_matrix_majority_agree$stance_epfl)
  
  
  
# Merging all confusion matrices ------------
## Accuracy -------------
overall_all_agree_fig <- as.data.frame(conf_epfl_mturk_agree$overall) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_3$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_4$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_6$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_7$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_42$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_41$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_45$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_43$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_44$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_46$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_47$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_48$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_agree_all_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_agree_all_1$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_agree_all_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_agree_all_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_agree_all_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_agree_all_6$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_agree_all_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_agree_all_8$overall)) %>% 
    cbind(as.data.frame(conf_epfl_mixtral_agree_all_2$overall)) %>% 
    cbind(as.data.frame(conf_epfl_mixtral_agree_all_1$overall)) %>%
    cbind(as.data.frame(conf_epfl_mixtral_agree_all_3$overall)) %>%
    cbind(as.data.frame(conf_epfl_mixtral_agree_all_4$overall)) %>%
    cbind(as.data.frame(conf_epfl_mixtral_agree_all_5$overall)) %>%
    cbind(as.data.frame(conf_epfl_mixtral_agree_all_6$overall)) %>%
    cbind(as.data.frame(conf_epfl_mixtral_agree_all_7$overall)) %>%
    cbind(as.data.frame(conf_epfl_mixtral_agree_all_8$overall)) %>%
    cbind(as.data.frame(conf_epfl_vader$overall)) %>%
    t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral_agree_all_2" = "Mistral prompt 2",
                                    "mistral_agree_all_1" = "Mistral prompt 1",
                                    "mistral_agree_all_3" = "Mistral prompt 3",
                                    "mistral_agree_all_4" = "Mistral prompt 4",
                                    "mistral_agree_all_5" = "Mistral prompt 5",
                                    "mistral_agree_all_6" = "Mistral prompt 6",
                                    "mistral_agree_all_7" = "Mistral prompt 7",
                                    "mistral_agree_all_8" = "Mistral prompt 8",
                                    "mixtral_agree_all_2" = "Mixtral prompt 2",
                                    "mixtral_agree_all_1" = "Mixtral prompt 1",
                                    "mixtral_agree_all_3" = "Mixtral prompt 3",
                                    "mixtral_agree_all_4" = "Mixtral prompt 4",
                                    "mixtral_agree_all_5" = "Mixtral prompt 5",
                                    "mixtral_agree_all_6" = "Mixtral prompt 6",
                                    "mixtral_agree_all_7" = "Mixtral prompt 7",
                                    "mixtral_agree_all_8" = "Mixtral prompt 8",
                                    "gpt_agree_all_42" = "GPT 4 prompt 2",
                                    "gpt_agree_all_41" = "GPT 4 prompt 1",
                                    "gpt_agree_all_43" = "GPT 4 prompt 3",
                                    "gpt_agree_all_44" = "GPT 4 prompt 4",
                                    "gpt_agree_all_45" = "GPT 4 prompt 5",
                                    "gpt_agree_all_46" = "GPT 4 prompt 6",
                                    "gpt_agree_all_47" = "GPT 4 prompt 7",
                                    "gpt_agree_all_48" = "GPT 4 prompt 8",
                                    "gpt_agree_all_2" = "GPT 3.5 prompt 2",
                                    "gpt_agree_all_1" = "GPT 3.5 prompt 1",
                                    "gpt_agree_all_3" = "GPT 3.5 prompt 3",
                                    "gpt_agree_all_4" = "GPT 3.5 prompt 4",
                                    "gpt_agree_all_5" = "GPT 3.5 prompt 5",
                                    "gpt_agree_all_6" = "GPT 3.5 prompt 6",
                                    "gpt_agree_all_7" = "GPT 3.5 prompt 7",
                                    "gpt_agree_all_8" = "GPT 3.5 prompt 8",
                                    "mturk_agree" = "Amazon Mturk",
                                    "vader" = "Vader"))) %>% 
    select(method, Accuracy, AccuracyLower, AccuracyUpper, AccuracyPValue) %>% 
    separate(col = "method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
    mutate(Pvalue = case_when(AccuracyPValue <= 0.05 ~ "<= 0.05",
                              .default = "> 0.05"),
           agreement = "Full agreement",
           Prompt = replace_na(Prompt, "None"))

overall_all_agree <- overall_all_agree_fig %>% 
  mutate(Accuracy = round(Accuracy, 4),
         AccuracyLower = round(AccuracyLower, 4),
         AccuracyUpper = round(AccuracyUpper, 4),
         AccuracyPValue = round(AccuracyPValue, 6),
         AccuracyCI = paste("(", AccuracyLower, " - ", AccuracyUpper, ")", sep = "")) %>%
  select(Method, Prompt, Accuracy, AccuracyCI, AccuracyPValue) %>% 
  rename("Accuracy (95% CI)" = "AccuracyCI",
         "Accuracy (p-value)" = "AccuracyPValue")

overall_all_agree %>%
  write_csv("outputs/confusion_matrix_accuracy_agree.csv")

### Figure for accuracy ----------------
accuracy_fig_full <- overall_all_agree_fig %>% 
  #filter(Method == "GPT 4") %>% 
  #filter(!is.na(Prompt)) %>% 
  ggplot(aes(x = Prompt, y = Accuracy)) +
  geom_point(aes(color = Pvalue)) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper,
                    color = Pvalue),
                width = 0.2) +
  facet_grid(~Method, scales = "free_x") 

accuracy_fig_full

## Per class ---------
class_majority_agree <- as.data.frame(conf_epfl_majority_agree$byClass) %>% 
  mutate(class_tweet = c("positive_majority_agree", "neutral_majority_agree", "negative_majority_agree"))%>% 
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
  select(Method, Prompt, Stance, `F1 score`, Sensitivity, Specificity)  %>% 
  mutate(agreement = "Full agreement") 
  

class_mturk_agree <- as.data.frame(conf_epfl_mturk_agree$byClass) %>% 
  mutate(class_tweet = c("positive_mturk", "neutral_mturk", "negative_mturk"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_vader_agree <- as.data.frame(conf_epfl_vader_agree$byClass) %>% 
  mutate(class_tweet = c("positive_vader", "neutral_vader", "negative_vader"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt2 <- as.data.frame(conf_epfl_gpt_agree_all_2$byClass) %>% 
  mutate(class_tweet = c("positive_gpt2", "neutral_gpt2", "negative_gpt2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt1 <- as.data.frame(conf_epfl_gpt_agree_all_1$byClass) %>% 
  mutate(class_tweet = c("positive_gpt1", "neutral_gpt1", "negative_gpt1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt3 <- as.data.frame(conf_epfl_gpt_agree_all_3$byClass) %>% 
  mutate(class_tweet = c("positive_gpt3", "neutral_gpt3", "negative_gpt3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt4 <- as.data.frame(conf_epfl_gpt_agree_all_4$byClass) %>% 
  mutate(class_tweet = c("positive_gpt4", "neutral_gpt4", "negative_gpt4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt5 <- as.data.frame(conf_epfl_gpt_agree_all_5$byClass) %>% 
  mutate(class_tweet = c("positive_gpt5", "neutral_gpt5", "negative_gpt5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt6 <- as.data.frame(conf_epfl_gpt_agree_all_6$byClass) %>% 
  mutate(class_tweet = c("positive_gpt6", "neutral_gpt6", "negative_gpt6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt7 <- as.data.frame(conf_epfl_gpt_agree_all_7$byClass) %>% 
  mutate(class_tweet = c("positive_gpt7", "neutral_gpt7", "negative_gpt7"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt8 <- as.data.frame(conf_epfl_gpt_agree_all_8$byClass) %>% 
  mutate(class_tweet = c("positive_gpt8", "neutral_gpt8", "negative_gpt8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt42 <- as.data.frame(conf_epfl_gpt_agree_all_42$byClass) %>% 
  mutate(class_tweet = c("positive_gpt42", "neutral_gpt42", "negative_gpt42"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt41 <- as.data.frame(conf_epfl_gpt_agree_all_41$byClass) %>% 
  mutate(class_tweet = c("positive_gpt41", "neutral_gpt41", "negative_gpt41")) %>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt45 <- as.data.frame(conf_epfl_gpt_agree_all_45$byClass) %>% 
  mutate(class_tweet = c("positive_gpt45", "neutral_gpt45", "negative_gpt45"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt43 <- as.data.frame(conf_epfl_gpt_agree_all_43$byClass) %>% 
  mutate(class_tweet = c("positive_gpt43", "neutral_gpt43", "negative_gpt43"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt44 <- as.data.frame(conf_epfl_gpt_agree_all_44$byClass) %>% 
  mutate(class_tweet = c("positive_gpt44", "neutral_gpt44", "negative_gpt44"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt46 <- as.data.frame(conf_epfl_gpt_agree_all_46$byClass) %>% 
  mutate(class_tweet = c("positive_gpt46", "neutral_gpt46", "negative_gpt46"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt47 <- as.data.frame(conf_epfl_gpt_agree_all_47$byClass) %>% 
  mutate(class_tweet = c("positive_gpt47", "neutral_gpt47", "negative_gpt47"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt48 <- as.data.frame(conf_epfl_gpt_agree_all_48$byClass) %>% 
  mutate(class_tweet = c("positive_gpt48", "neutral_gpt48", "negative_gpt48"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_mistral2 <- as.data.frame(conf_epfl_mistral_agree_all_2$byClass) %>% 
  mutate(class_tweet = c("positive_mistral2", "neutral_mistral2", "negative_mistral2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_mistral1 <- as.data.frame(conf_epfl_mistral_agree_all_1$byClass) %>% 
  mutate(class_tweet = c("positive_mistral1", "neutral_mistral1", "negative_mistral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_mistral3 <- as.data.frame(conf_epfl_mistral_agree_all_3$byClass) %>% 
  mutate(class_tweet = c("positive_mistral3", "neutral_mistral3", "negative_mistral3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_mistral4 <- as.data.frame(conf_epfl_mistral_agree_all_4$byClass) %>% 
  mutate(class_tweet = c("positive_mistral4", "neutral_mistral4", "negative_mistral4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_mistral5 <- as.data.frame(conf_epfl_mistral_agree_all_5$byClass) %>% 
  mutate(class_tweet = c("positive_mistral5", "neutral_mistral5", "negative_mistral5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_mistral6 <- as.data.frame(conf_epfl_mistral_agree_all_6$byClass) %>% 
  mutate(class_tweet = c("positive_mistral6", "neutral_mistral6", "negative_mistral6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_mistral7 <- as.data.frame(conf_epfl_mistral_agree_all_7$byClass) %>%
  mutate(class_tweet = c("positive_mistral7", "neutral_mistral7", "negative_mistral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_agree_mistral8 <- as.data.frame(conf_epfl_mistral_agree_all_8$byClass) %>% 
  mutate(class_tweet = c("positive_mistral8", "neutral_mistral8", "negative_mistral8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_mixtral2 <- as.data.frame(conf_epfl_mixtral_agree_all_2$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral2", "neutral_mixtral2", "negative_mixtral2"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_mixtral1 <- as.data.frame(conf_epfl_mixtral_agree_all_1$byClass) %>% 
  mutate(class_tweet = c("positive_mixtral1", "neutral_mixtral1", "negative_mixtral1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_mixtral3 <- as.data.frame(conf_epfl_mixtral_agree_all_3$byClass) %>%
  mutate(class_tweet = c("positive_mixtral3", "neutral_mixtral3", "negative_mixtral3"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_agree_mixtral4 <- as.data.frame(conf_epfl_mixtral_agree_all_4$byClass) %>%
  mutate(class_tweet = c("positive_mixtral4", "neutral_mixtral4", "negative_mixtral4"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_agree_mixtral5 <- as.data.frame(conf_epfl_mixtral_agree_all_5$byClass) %>%
  mutate(class_tweet = c("positive_mixtral5", "neutral_mixtral5", "negative_mixtral5"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_agree_mixtral6 <- as.data.frame(conf_epfl_mixtral_agree_all_6$byClass) %>%
  mutate(class_tweet = c("positive_mixtral6", "neutral_mixtral6", "negative_mixtral6"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_agree_mixtral7 <- as.data.frame(conf_epfl_mixtral_agree_all_7$byClass) %>%
  mutate(class_tweet = c("positive_mixtral7", "neutral_mixtral7", "negative_mixtral7"))%>%
  rownames_to_column() %>%
  select(-rowname)

class_agree_mixtral8 <- as.data.frame(conf_epfl_mixtral_agree_all_8$byClass) %>%
  mutate(class_tweet = c("positive_mixtral8", "neutral_mixtral8", "negative_mixtral8"))%>%
  rownames_to_column() %>%
  select(-rowname)


class_all_agree <- class_agree_gpt2 %>% 
  full_join(class_agree_gpt1) %>% 
  full_join(class_agree_gpt3) %>% 
  full_join(class_agree_gpt4) %>% 
  full_join(class_agree_gpt42) %>% 
  full_join(class_agree_gpt41) %>% 
  full_join(class_agree_gpt45) %>% 
  full_join(class_agree_gpt5) %>% 
  full_join(class_agree_gpt6) %>% 
  full_join(class_agree_gpt7) %>% 
  full_join(class_agree_gpt8) %>% 
  full_join(class_mturk_agree) %>%
  full_join(class_agree_gpt43) %>% 
  full_join(class_agree_gpt44) %>% 
  full_join(class_agree_gpt46) %>% 
  full_join(class_agree_gpt47) %>% 
  full_join(class_agree_gpt48) %>% 
  full_join(class_agree_mistral2) %>% 
  full_join(class_agree_mistral1) %>% 
  full_join(class_agree_mistral3) %>% 
  full_join(class_agree_mistral4) %>% 
  full_join(class_agree_mistral5) %>% 
  full_join(class_agree_mistral6) %>% 
  full_join(class_agree_mistral7) %>%
  full_join(class_agree_mistral8) %>%
  full_join(class_agree_mixtral2) %>% 
  full_join(class_agree_mixtral1) %>% 
  full_join(class_agree_mixtral3) %>% 
  full_join(class_agree_mixtral4) %>% 
  full_join(class_agree_mixtral5) %>% 
  full_join(class_agree_mixtral6) %>% 
  full_join(class_agree_mixtral7) %>%
  full_join(class_agree_mixtral8) %>%
  full_join(class_vader_agree) %>%
  separate(., class_tweet, into = c("class", "classifier"), 
           sep = "_") %>% 
  mutate(method = str_replace_all(classifier, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral2" = "Mistral prompt 2",
                                    "mistral1" = "Mistral prompt 1",
                                    "mistral3" = "Mistral prompt 3",
                                    "mistral4" = "Mistral prompt 4",
                                    "mistral5" = "Mistral prompt 5",
                                    "mistral6" = "Mistral prompt 6",
                                    "mistral7" = "Mistral prompt 7",
                                    "mistral8" = "Mistral prompt 8",
                                    "mixtral2" = "Mixtral prompt 2",
                                    "mixtral1" = "Mixtral prompt 1",
                                    "mixtral3" = "Mixtral prompt 3",
                                    "mixtral4" = "Mixtral prompt 4",
                                    "mixtral5" = "Mixtral prompt 5",
                                    "mixtral6" = "Mixtral prompt 6",
                                    "mixtral7" = "Mixtral prompt 7",
                                    "mixtral8" = "Mixtral prompt 8",
                                    "gpt42" = "GPT 4 prompt 2",
                                    "gpt41" = "GPT 4 prompt 1",
                                    "gpt45" = "GPT 4 prompt 5",
                                    "gpt43" = "GPT 4 prompt 3",
                                    "gpt44" = "GPT 4 prompt 4",
                                    "gpt46" = "GPT 4 prompt 6",
                                    "gpt47" = "GPT 4 prompt 7",
                                    "gpt48" = "GPT 4 prompt 8",
                                    "gpt2" = "GPT 3.5 prompt 2",
                                    "gpt1" = "GPT 3.5 prompt 1",
                                    "gpt3" = "GPT 3.5 prompt 3",
                                    "gpt4" = "GPT 3.5 prompt 4",
                                    "gpt5" = "GPT 3.5 prompt 5",
                                    "gpt6" = "GPT 3.5 prompt 6",
                                    "gpt7" = "GPT 3.5 prompt 7",
                                    "gpt8" = "GPT 3.5 prompt 8",
                                    "mturk" = "Amazon Mturk",
                                    "vader" = "Vader"))) %>% 
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
  mutate(agreement = "Full agreement")

class_all_agree %>% 
  write_csv("outputs/confusion_matrix_per_class_all_agree.csv")

class_all_agree_positive <- class_all_agree %>% 
  filter(Stance == "positive") %>% 
  arrange(desc(`F1 score`)) 

class_all_agree_positive %>% 
  write_csv("outputs/confusion_matrix_per_class_positive_agree.csv")

class_all_agree_negative <- class_all_agree %>% 
  filter(Stance == "negative")%>% 
  arrange(desc(`F1 score`))

class_all_agree_negative %>% 
  write_csv("outputs/confusion_matrix_per_class_negative_agree.csv")

class_all_agree_neutral <- class_all_agree %>% 
  filter(Stance == "neutral")%>% 
  arrange(desc(`F1 score`))

class_all_agree_neutral %>% 
  write_csv("outputs/confusion_matrix_per_class_neutral_agree.csv")

# Comparing metrics for partial and full agreement tweets ---------
## Negative class ------------
class_negative_rank <- class_all_negative %>% 
  select(Method, Prompt, `F1 score`) %>% 
  rename("Method (partial tweets)" = "Method",
         "Prompt (partial tweets)" = "Prompt",
         "F1 score (partial tweets)" = "F1 score") %>% 
  arrange(desc(`F1 score (partial tweets)`)) %>% 
  cbind(class_all_agree_negative) %>% 
  select(-Sensitivity, -Specificity) %>% 
  rename("Method (full tweets)" = "Method",
         "Prompt (full tweets)" = "Prompt",
         "F1 score (full tweets)" = "F1 score") %>% 
  mutate(`Rank comparison` = if_else(`Method (partial tweets)` == `Method (full tweets)` &
                                                `Prompt (partial tweets)` == `Prompt (full tweets)`, "Equal", "Different"),
         `Rank comparison` = case_when(is.na(`Rank comparison`) ~ "Equal",
                                       .default = `Rank comparison`)) 

class_negative_rank_method <- class_negative_rank %>% 
  group_by(`Method (full tweets)`, `Rank comparison`) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(`Rank comparison` == "Equal")

class_negative_rank_agreement <- class_negative_rank %>% 
  group_by(`Rank comparison`, Stance) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(percentage = (n / sum(n)*100)) %>% 
  arrange(desc(`Rank comparison`))

class_positive_rank <- class_all_positive %>% 
  select(Method, Prompt, `F1 score`) %>% 
  arrange(desc(`F1 score`)) %>% 
  rename("Method (partial tweets)" = "Method",
         "Prompt (partial tweets)" = "Prompt",
         "F1 score (partial tweets)" = "F1 score") %>% 
  cbind(class_all_agree_positive) %>% 
  select(-Sensitivity, -Specificity) %>% 
  rename("Method (full tweets)" = "Method",
         "Prompt (full tweets)" = "Prompt",
         "F1 score (full tweets)" = "F1 score") %>% 
  mutate(`Rank comparison` = if_else(`Method (partial tweets)` == `Method (full tweets)` &
                                       `Prompt (partial tweets)` == `Prompt (full tweets)`, "Equal", "Different"),
         `Rank comparison` = case_when(is.na(`Rank comparison`) ~ "Equal",
                                       .default = `Rank comparison`)) 

class_positive_rank_method <- class_positive_rank %>% 
  group_by(`Method (full tweets)`, `Rank comparison`) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(`Rank comparison` == "Equal")

class_positive_rank_agreement <- class_positive_rank %>% 
  group_by(`Rank comparison`, Stance) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(percentage = (n / sum(n)*100)) %>% 
  arrange(desc(`Rank comparison`))

class_neutral_rank <- class_all_neutral %>% 
  select(Method, Prompt, `F1 score`) %>%
  arrange(desc(`F1 score`)) %>% 
  rename("Method (partial tweets)" = "Method",
         "Prompt (partial tweets)" = "Prompt",
         "F1 score (partial tweets)" = "F1 score") %>% 
  cbind(class_all_agree_neutral) %>% 
  select(-Sensitivity, -Specificity) %>% 
  rename("Method (full tweets)" = "Method",
         "Prompt (full tweets)" = "Prompt",
         "F1 score (full tweets)" = "F1 score") %>% 
  mutate(`Rank comparison` = if_else(`Method (partial tweets)` == `Method (full tweets)` &
                                       `Prompt (partial tweets)` == `Prompt (full tweets)`, "Equal", "Different"),
         `Rank comparison` = case_when(is.na(`Rank comparison`) ~ "Equal",
                                       .default = `Rank comparison`)) 

class_neutral_rank_method <- class_neutral_rank %>% 
  group_by(`Method (full tweets)`, `Rank comparison`) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(`Rank comparison` == "Equal")

class_neutral_rank_agreement <- class_neutral_rank %>% 
  group_by(`Rank comparison`, Stance) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(percentage = (n / sum(n)*100)) %>% 
  arrange(desc(`Rank comparison`))

