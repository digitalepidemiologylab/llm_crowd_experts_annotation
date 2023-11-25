# Script information ------------
#' Aim: Comparing all annotations
#' Author: Laura Espinosa
#' Date created: 12 October 2023
#' Date updated: 12 October 2023

# Get all datasets -------------
df_all_agree <- df_mturk_annot_clean %>% 
  full_join(epfl_df, by = "text") %>% 
  full_join(gpt_clean, by = "text") %>% 
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
  assign(paste('conf_epfl_gpt_agree_all',i,sep='_'),conf_epfl_gpt_agree) %>% 
    capture.output(., file = paste0("outputs/conf_epfl_gpt_agree_all", i, ".csv"))
  
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
  
  conf_epfl_mturk_agree %>% 
    capture.output(., file = paste0("outputs/conf_epfl_mturk_agree.csv"))
  


## Mturk vs GPT --------------
for (i in prompts) {
  df_con_matrix_mturk_gpt <- df_all_clean %>% 
    filter(prompt == i) %>% 
    select(agree_mturk, sentiment_gpt) %>% 
    mutate(agree_mturk = factor(agree_mturk, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                levels = c("positive", "neutral", "negative"))) 
  
  #prompt_loop[i] <- prompts[i]
  conf_mturk_gpt <- confusionMatrix(df_con_matrix_mturk_gpt$sentiment_gpt,
                                    df_con_matrix_mturk_gpt$agree_mturk)
  conf_mturk_gpt$prompt <- prompts[i]
  assign(paste('conf_mturk_gpt_all',i,sep='_'),conf_epfl_mturk) %>% 
    capture.output(., file = paste0("outputs/conf_mturk_gpt_all", i, ".csv"))
  
}

## GPT vs Mturk --------------------
for (i in prompts) {
  df_con_matrix_mturk_gpt <- df_all_clean %>% 
    filter(prompt == i) %>% 
    select(agree_mturk, sentiment_gpt) %>% 
    mutate(agree_mturk = factor(agree_mturk, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  
  #prompt_loop[i] <- prompts[i]
  conf_gpt_mturk <- confusionMatrix(df_con_matrix_mturk_gpt$agree_mturk,
                                    df_con_matrix_mturk_gpt$sentiment_gpt)
  conf_gpt_mturk$prompt <- prompts[i]
  assign(paste('conf_gpt_mturk_all',i,sep='_'),conf_epfl_mturk) %>% 
    capture.output(., file = paste0("outputs/conf_gpt_mturk_all", i, ".csv"))
  
}

# Comparing the confusion matrices outputs ------------
## EPFL-GPT vs EPFL-Mturk -------------
mcnemar_results <- mcnemar.test(conf_epfl_mturk_agree$table,
                                        conf_epfl_mturk$table)

mcnemar_results

# Merging all confusion matrices ------------
## Accuracy -------------
overall_all_agree <- as.data.frame(conf_epfl_mturk_agree$overall) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_0$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_3$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_4$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_6$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_7$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_40$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_41$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_45$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_43$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_44$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_46$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_47$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_agree_all_48$overall)) %>% 
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "gpt_agree_all_40" = "GPT 4 prompt 0",
                                    "gpt_agree_all_41" = "GPT 4 prompt 1",
                                    "gpt_agree_all_43" = "GPT 4 prompt 3",
                                    "gpt_agree_all_44" = "GPT 4 prompt 4",
                                    "gpt_agree_all_45" = "GPT 4 prompt 5",
                                    "gpt_agree_all_46" = "GPT 4 prompt 6",
                                    "gpt_agree_all_47" = "GPT 4 prompt 7",
                                    "gpt_agree_all_48" = "GPT 4 prompt 8",
                                    "gpt_agree_all_0" = "GPT 3.5 prompt 0",
                                    "gpt_agree_all_1" = "GPT 3.5 prompt 1",
                                    "gpt_agree_all_3" = "GPT 3.5 prompt 3",
                                    "gpt_agree_all_4" = "GPT 3.5 prompt 4",
                                    "gpt_agree_all_5" = "GPT 3.5 prompt 5",
                                    "gpt_agree_all_6" = "GPT 3.5 prompt 6",
                                    "gpt_agree_all_7" = "GPT 3.5 prompt 7",
                                    "gpt_agree_all_8" = "GPT 3.5 prompt 8",
                                    "mturk_agree" = "Amazon Mturk"))) %>% 
  select(method, Accuracy, AccuracyLower, AccuracyUpper, AccuracyPValue) %>% 
  mutate(Accuracy = round(Accuracy, 4),
         AccuracyLower = round(AccuracyLower, 4), 
         AccuracyUpper = round(AccuracyUpper, 4),
         AccuracyPValue = round(AccuracyPValue, 6),
         AccuracyCI = paste("(", AccuracyLower, " - ", AccuracyUpper, ")", sep = "")) %>% 
  separate(col = "method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
  select(Method, Prompt, Accuracy, AccuracyCI, AccuracyPValue) %>% 
  rename("Accuracy (95% CI)" = "AccuracyCI",
         "Accuracy (p-value)" = "AccuracyPValue") 

overall_all_agree %>%
  write_csv("outputs/confusion_matrix_accuracy_agree.csv")

## Per class ---------
class_mturk_agree <- as.data.frame(conf_epfl_mturk_agree$byClass) %>% 
  mutate(class_tweet = c("positive_mturk", "neutral_mturk", "negative_mturk"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_agree_gpt0 <- as.data.frame(conf_epfl_gpt_agree_all_0$byClass) %>% 
  mutate(class_tweet = c("positive_gpt0", "neutral_gpt0", "negative_gpt0"))%>% 
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

class_agree_gpt40 <- as.data.frame(conf_epfl_gpt_agree_all_40$byClass) %>% 
  mutate(class_tweet = c("positive_gpt40", "neutral_gpt40", "negative_gpt40"))%>% 
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

class_all_agree <- class_agree_gpt0 %>% 
  full_join(class_agree_gpt1) %>% 
  full_join(class_agree_gpt3) %>% 
  full_join(class_agree_gpt4) %>% 
  full_join(class_agree_gpt40) %>% 
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
  separate(., class_tweet, into = c("class", "classifier"), 
           sep = "_") %>% 
  mutate(method = str_replace_all(classifier, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "gpt40" = "GPT 4 prompt 0",
                                    "gpt41" = "GPT 4 prompt 1",
                                    "gpt45" = "GPT 4 prompt 5",
                                    "gpt0" = "GPT 3.5 prompt 0",
                                    "gpt1" = "GPT 3.5 prompt 1",
                                    "gpt3" = "GPT 3.5 prompt 3",
                                    "gpt4" = "GPT 3.5 prompt 4",
                                    "gpt5" = "GPT 3.5 prompt 5",
                                    "gpt6" = "GPT 3.5 prompt 6",
                                    "gpt7" = "GPT 3.5 prompt 7",
                                    "gpt8" = "GPT 3.5 prompt 8",
                                    "mturk" = "Amazon Mturk",
                                    "gpt43" = "GPT 4 prompt 3",
                                    "gpt44" = "GPT 4 prompt 4",
                                    "gpt46" = "GPT 4 prompt 6",
                                    "gpt47" = "GPT 4 prompt 7",
                                    "gpt48" = "GPT 4 prompt 8"))) %>% 
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
         `Balanced accuracy` = round(`Balanced accuracy`, 4))

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
