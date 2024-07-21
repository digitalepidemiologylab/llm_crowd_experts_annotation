# Script information ------------
#' Aim: Comparing each annotator
#' Author: Laura Espinosa
#' Date created: 8 June 2024
#' Date updated: 8 June 2024


# Get merged datasets ----------
df_annotator_all <- df_mturk_annot_clean %>% 
  full_join(epfl_df_all, by = "text") %>% 
  full_join(gpt_clean, by = "text") %>%
  full_join(mistral_clean, by = c("text", "prompt")) %>% 
  full_join(mixtral_clean, by = c("text", "prompt")) %>% 
  full_join(llama_clean, by = c("text", "prompt")) %>% 
  full_join(llama70b_clean, by = c("text", "prompt")) %>% 
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

# Figure stance distribution per annotator ----------------
stance_m_grouped <- epfl_df_all %>% 
  group_by(stance_m) %>% 
  tally() %>% 
  mutate(percent = round(n/1000 * 100, digits = 2),
         annotator = "Co-author (1)") %>% 
  dplyr::rename(., stance = stance_m)

stance_c_grouped <- epfl_df_all %>% 
  group_by(stance_c) %>% 
  tally() %>% 
  mutate(percent = round(n/1000 * 100, digits = 2),
         annotator = "General public") %>% 
  dplyr::rename(., stance = stance_c)

stance_l_grouped <- epfl_df_all %>% 
  group_by(stance_l) %>% 
  tally() %>% 
  mutate(percent = round(n/1000 * 100, digits = 2),
         annotator = "Co-author (2)") %>% 
  dplyr::rename(., stance = stance_l)

stance_g_grouped <- epfl_df_all %>% 
  group_by(stance_g) %>% 
  tally() %>% 
  mutate(percent = round(n/1000 * 100, digits = 2),
         annotator = "Annotator") %>% 
  dplyr::rename(., stance = stance_g)

stance_per_annotator <- stance_g_grouped %>% 
  rbind(stance_m_grouped) %>% 
  rbind(stance_l_grouped) %>% 
  rbind(stance_c_grouped) %>% 
  filter(!is.na(stance))

# Confusion matrix --------
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

## EPFL vs Llama3 8B ---------------
for (i in prompts) {
  df_con_matrix_llama_m <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_m, sentiment_llama) %>% 
    mutate(stance_m = factor(stance_m, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_llama = factor(sentiment_llama, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_llama_m',i,sep='_'), df_con_matrix_llama_m)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_llama_m <- confusionMatrix(df_con_matrix_llama_m$sentiment_llama,
                                       df_con_matrix_llama_m$stance_m, mode = "everything")
  conf_epfl_llama_m$prompt <- prompts[i]
  assign(paste('conf_epfl_llama_all_m',i,sep='_'),conf_epfl_llama_m) 
}

## EPFL vs Llama3 70B ---------------
for (i in prompts) {
  df_con_matrix_llama70b_m <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_m, sentiment_llama70b) %>% 
    mutate(stance_m = factor(stance_m, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_llama = factor(sentiment_llama70b, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_llama70b_m',i,sep='_'), df_con_matrix_llama70b_m)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_llama70b_m <- confusionMatrix(df_con_matrix_llama70b_m$sentiment_llama,
                                          df_con_matrix_llama70b_m$stance_m, mode = "everything")
  conf_epfl_llama70b_m$prompt <- prompts[i]
  assign(paste('conf_epfl_llama70b_all_m',i,sep='_'),conf_epfl_llama70b_m) 
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
  cbind(as.data.frame(conf_epfl_llama_all_m_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_m_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_m_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_m_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_m_5$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_m_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_m_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_m_8$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_m_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_m_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_m_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_m_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_m_5$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_m_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_m_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_m_8$overall)) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "llama70b_all_m_2" = "Llama3 \n(70B) prompt 2",
                                    "llama70b_all_m_1" = "Llama3 \n(70B) prompt 1",
                                    "llama70b_all_m_3" = "Llama3 \n(70B) prompt 3",
                                    "llama70b_all_m_4" = "Llama3 \n(70B) prompt 4",
                                    "llama70b_all_m_5" = "Llama3 \n(70B) prompt 5",
                                    "llama70b_all_m_6" = "Llama3 \n(70B) prompt 6",
                                    "llama70b_all_m_7" = "Llama3 \n(70B) prompt 7",
                                    "llama70b_all_m_8" = "Llama3 \n(70B) prompt 8",
                                    
                                    "llama_all_m_2" = "Llama3 \n(8B) prompt 2",
                                    "llama_all_m_1" = "Llama3 \n(8B) prompt 1",
                                    "llama_all_m_3" = "Llama3 \n(8B) prompt 3",
                                    "llama_all_m_4" = "Llama3 \n(8B) prompt 4",
                                    "llama_all_m_5" = "Llama3 \n(8B) prompt 5",
                                    "llama_all_m_6" = "Llama3 \n(8B) prompt 6",
                                    "llama_all_m_7" = "Llama3 \n(8B) prompt 7",
                                    "llama_all_m_8" = "Llama3 \n(8B) prompt 8",
                                    
                                    "mistral_all_m_2" = "Mistral \n(7B) prompt 2",
                                    "mistral_all_m_1" = "Mistral \n(7B) prompt 1",
                                    "mistral_all_m_3" = "Mistral \n(7B) prompt 3",
                                    "mistral_all_m_4" = "Mistral \n(7B) prompt 4",
                                    "mistral_all_m_5" = "Mistral \n(7B) prompt 5",
                                    "mistral_all_m_6" = "Mistral \n(7B) prompt 6",
                                    "mistral_all_m_7" = "Mistral \n(7B) prompt 7",
                                    "mistral_all_m_8" = "Mistral \n(7B) prompt 8",
                                    "mixtral_all_m_2" = "Mixtral \n(8x7B) prompt 2",
                                    "mixtral_all_m_1" = "Mixtral \n(8x7B) prompt 1",
                                    "mixtral_all_m_3" = "Mixtral \n(8x7B) prompt 3",
                                    "mixtral_all_m_4" = "Mixtral \n(8x7B) prompt 4",
                                    "mixtral_all_m_5" = "Mixtral \n(8x7B) prompt 5",
                                    "mixtral_all_m_6" = "Mixtral \n(8x7B) prompt 6",
                                    "mixtral_all_m_7" = "Mixtral \n(8x7B) prompt 7",
                                    "mixtral_all_m_8" = "Mixtral \n(8x7B) prompt 8",
                                    "gpt_all_m_42" = "GPT 4 prompt 2",
                                    "gpt_all_m_41" = "GPT 4 prompt 1",
                                    "gpt_all_m_43" = "GPT 4 prompt 3",
                                    "gpt_all_m_44" = "GPT 4 prompt 4",
                                    "gpt_all_m_45" = "GPT 4 prompt 5",
                                    "gpt_all_m_46" = "GPT 4 prompt 6",
                                    "gpt_all_m_47" = "GPT 4 prompt 7",
                                    "gpt_all_m_48" = "GPT 4 prompt 8",
                                    
                                    "llama_all_m_42" = "Llama3 (8B) prompt 2",
                                    "llama_all_m_41" = "Llama3 (8B) prompt 1",
                                    "llama_all_m_43" = "Llama3 (8B) prompt 3",
                                    "llama_all_m_44" = "Llama3 (8B) prompt 4",
                                    "llama_all_m_45" = "Llama3 (8B) prompt 5",
                                    "llama_all_m_46" = "Llama3 (8B) prompt 6",
                                    "llama_all_m_47" = "Llama3 (8B) prompt 7",
                                    "llama_all_m_48" = "Llama3 (8B) prompt 8",
                                    
                                    "llama70b_all_m_42" = "Llama3 (70B) prompt 2",
                                    "llama70b_all_m_41" = "Llama3 (70B) prompt 1",
                                    "llama70b_all_m_43" = "Llama3 (70B) prompt 3",
                                    "llama70b_all_m_44" = "Llama3 (70B) prompt 4",
                                    "llama70b_all_m_45" = "Llama3 (70B) prompt 5",
                                    "llama70b_all_m_46" = "Llama3 (70B) prompt 6",
                                    "llama70b_all_m_47" = "Llama3 (70B) prompt 7",
                                    "llama70b_all_m_48" = "Llama3 (70B) prompt 8",
                                    
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

## EPFL vs Llama3 8B ---------------
for (i in prompts) {
  df_con_matrix_llama_g <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_g, sentiment_llama) %>% 
    mutate(stance_g = factor(stance_g, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_llama = factor(sentiment_llama, ordered = TRUE,
                                    levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_llama_g',i,sep='_'), df_con_matrix_llama_g)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_llama_g <- confusionMatrix(df_con_matrix_llama_g$sentiment_llama,
                                       df_con_matrix_llama_g$stance_g, mode = "everything")
  conf_epfl_llama_m$prompt <- prompts[i]
  assign(paste('conf_epfl_llama_all_g',i,sep='_'),conf_epfl_llama_g) 
}

## EPFL vs Llama3 70B ---------------
for (i in prompts) {
  df_con_matrix_llama70b_g <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_g, sentiment_llama70b) %>% 
    mutate(stance_g = factor(stance_g, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_llama = factor(sentiment_llama70b, ordered = TRUE,
                                    levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_llama70b_g',i,sep='_'), df_con_matrix_llama70b_g)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_llama70b_g <- confusionMatrix(df_con_matrix_llama70b_g$sentiment_llama,
                                          df_con_matrix_llama70b_g$stance_g, mode = "everything")
  conf_epfl_llama70b_g$prompt <- prompts[i]
  assign(paste('conf_epfl_llama70b_all_g',i,sep='_'),conf_epfl_llama70b_g) 
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
  cbind(as.data.frame(conf_epfl_llama_all_g_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_g_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_g_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_g_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_g_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_g_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_g_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_g_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_g_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_g_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_g_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_g_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_g_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_g_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_g_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_g_7$overall)) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "llama70b_all_g_2" = "Llama3 \n(70B) prompt 2",
                                    "llama70b_all_g_1" = "Llama3 \n(70B) prompt 1",
                                    "llama70b_all_g_3" = "Llama3 \n(70B) prompt 3",
                                    "llama70b_all_g_4" = "Llama3 \n(70B) prompt 4",
                                    "llama70b_all_g_5" = "Llama3 \n(70B) prompt 5",
                                    "llama70b_all_g_6" = "Llama3 \n(70B) prompt 6",
                                    "llama70b_all_g_7" = "Llama3 \n(70B) prompt 7",
                                    "llama70b_all_g_8" = "Llama3 \n(70B) prompt 8",
                                    
                                    "llama_all_g_2" = "Llama3 \n(8B) prompt 2",
                                    "llama_all_g_1" = "Llama3 \n(8B) prompt 1",
                                    "llama_all_g_3" = "Llama3 \n(8B) prompt 3",
                                    "llama_all_g_4" = "Llama3 \n(8B) prompt 4",
                                    "llama_all_g_5" = "Llama3 \n(8B) prompt 5",
                                    "llama_all_g_6" = "Llama3 \n(8B) prompt 6",
                                    "llama_all_g_7" = "Llama3 \n(8B) prompt 7",
                                    "llama_all_g_8" = "Llama3 \n(8B) prompt 8",
                                    
                                    "mistral_all_g_2" = "Mistral \n(7B) prompt 2",
                                    "mistral_all_g_1" = "Mistral \n(7B) prompt 1",
                                    "mistral_all_g_3" = "Mistral \n(7B) prompt 3",
                                    "mistral_all_g_4" = "Mistral \n(7B) prompt 4",
                                    "mistral_all_g_5" = "Mistral \n(7B) prompt 5",
                                    "mistral_all_g_6" = "Mistral \n(7B) prompt 6",
                                    "mistral_all_g_7" = "Mistral \n(7B) prompt 7",
                                    "mistral_all_g_8" = "Mistral \n(7B) prompt 8",
                                    "mixtral_all_g_2" = "Mixtral \n(8x7B) prompt 2",
                                    "mixtral_all_g_1" = "Mixtral \n(8x7B) prompt 1",
                                    "mixtral_all_g_3" = "Mixtral \n(8x7B) prompt 3",
                                    "mixtral_all_g_4" = "Mixtral \n(8x7B) prompt 4",
                                    "mixtral_all_g_5" = "Mixtral \n(8x7B) prompt 5",
                                    "mixtral_all_g_6" = "Mixtral \n(8x7B) prompt 6",
                                    "mixtral_all_g_7" = "Mixtral \n(8x7B) prompt 7",
                                    "mixtral_all_g_8" = "Mixtral \n(8x7B) prompt 8",
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

## EPFL vs Llama3 8B ---------------
for (i in prompts) {
  df_con_matrix_llama_c <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_c, sentiment_llama) %>% 
    mutate(stance_c = factor(stance_c, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_llama = factor(sentiment_llama, ordered = TRUE,
                                    levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_llama_c',i,sep='_'), df_con_matrix_llama_c)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_llama_c <- confusionMatrix(df_con_matrix_llama_c$sentiment_llama,
                                       df_con_matrix_llama_c$stance_c, mode = "everything")
  conf_epfl_llama_c$prompt <- prompts[i]
  assign(paste('conf_epfl_llama_all_c',i,sep='_'),conf_epfl_llama_c) 
}

## EPFL vs Llama3 70B ---------------
for (i in prompts) {
  df_con_matrix_llama70b_c <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_c, sentiment_llama70b) %>% 
    mutate(stance_c = factor(stance_c, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_llama = factor(sentiment_llama70b, ordered = TRUE,
                                    levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_llama70b_c',i,sep='_'), df_con_matrix_llama70b_c)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_llama70b_c <- confusionMatrix(df_con_matrix_llama70b_c$sentiment_llama,
                                          df_con_matrix_llama70b_c$stance_c, mode = "everything")
  conf_epfl_llama70b_c$prompt <- prompts[i]
  assign(paste('conf_epfl_llama70b_all_c',i,sep='_'),conf_epfl_llama70b_c) 
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
  cbind(as.data.frame(conf_epfl_llama_all_c_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_c_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_c_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_c_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_c_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_c_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_c_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_c_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_c_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_c_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_c_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_c_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_c_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_c_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_c_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_c_7$overall)) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "llama70b_all_c_2" = "Llama3 \n(70B) prompt 2",
                                    "llama70b_all_c_1" = "Llama3 \n(70B) prompt 1",
                                    "llama70b_all_c_3" = "Llama3 \n(70B) prompt 3",
                                    "llama70b_all_c_4" = "Llama3 \n(70B) prompt 4",
                                    "llama70b_all_c_5" = "Llama3 \n(70B) prompt 5",
                                    "llama70b_all_c_6" = "Llama3 \n(70B) prompt 6",
                                    "llama70b_all_c_7" = "Llama3 \n(70B) prompt 7",
                                    "llama70b_all_c_8" = "Llama3 \n(70B) prompt 8",
                                    
                                    "llama_all_c_2" = "Llama3 \n(8B) prompt 2",
                                    "llama_all_c_1" = "Llama3 \n(8B) prompt 1",
                                    "llama_all_c_3" = "Llama3 \n(8B) prompt 3",
                                    "llama_all_c_4" = "Llama3 \n(8B) prompt 4",
                                    "llama_all_c_5" = "Llama3 \n(8B) prompt 5",
                                    "llama_all_c_6" = "Llama3 \n(8B) prompt 6",
                                    "llama_all_c_7" = "Llama3 \n(8B) prompt 7",
                                    "llama_all_c_8" = "Llama3 \n(8B) prompt 8",
                                    
                                    "mistral_all_c_2" = "Mistral \n(7B) prompt 2",
                                    "mistral_all_c_1" = "Mistral \n(7B) prompt 1",
                                    "mistral_all_c_3" = "Mistral \n(7B) prompt 3",
                                    "mistral_all_c_4" = "Mistral \n(7B) prompt 4",
                                    "mistral_all_c_5" = "Mistral \n(7B) prompt 5",
                                    "mistral_all_c_6" = "Mistral \n(7B) prompt 6",
                                    "mistral_all_c_7" = "Mistral \n(7B) prompt 7",
                                    "mistral_all_c_8" = "Mistral \n(7B) prompt 8",
                                    "mixtral_all_c_2" = "Mixtral \n(8x7B) prompt 2",
                                    "mixtral_all_c_1" = "Mixtral \n(8x7B) prompt 1",
                                    "mixtral_all_c_3" = "Mixtral \n(8x7B) prompt 3",
                                    "mixtral_all_c_4" = "Mixtral \n(8x7B) prompt 4",
                                    "mixtral_all_c_5" = "Mixtral \n(8x7B) prompt 5",
                                    "mixtral_all_c_6" = "Mixtral \n(8x7B) prompt 6",
                                    "mixtral_all_c_7" = "Mixtral \n(8x7B) prompt 7",
                                    "mixtral_all_c_8" = "Mixtral \n(8x7B) prompt 8",
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

## EPFL vs Llama3 8B ---------------
for (i in prompts) {
  df_con_matrix_llama_l <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_l, sentiment_llama) %>% 
    mutate(stance_l = factor(stance_l, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_llama = factor(sentiment_llama, ordered = TRUE,
                                    levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_llama_l',i,sep='_'), df_con_matrix_llama_l)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_llama_l <- confusionMatrix(df_con_matrix_llama_l$sentiment_llama,
                                       df_con_matrix_llama_l$stance_l, mode = "everything")
  conf_epfl_llama_l$prompt <- prompts[i]
  assign(paste('conf_epfl_llama_all_l',i,sep='_'),conf_epfl_llama_l) 
}

## EPFL vs Llama3 70B ---------------
for (i in prompts) {
  df_con_matrix_llama70b_l <- df_annotator_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_l, sentiment_llama70b) %>% 
    mutate(stance_l = factor(stance_l, ordered = TRUE,
                             levels = c("positive","neutral", "negative")),
           sentiment_llama = factor(sentiment_llama70b, ordered = TRUE,
                                    levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_llama70b_l',i,sep='_'), df_con_matrix_llama70b_l)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_llama70b_l <- confusionMatrix(df_con_matrix_llama70b_l$sentiment_llama,
                                          df_con_matrix_llama70b_l$stance_l, mode = "everything")
  conf_epfl_llama70b_l$prompt <- prompts[i]
  assign(paste('conf_epfl_llama70b_all_l',i,sep='_'),conf_epfl_llama70b_l) 
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
  cbind(as.data.frame(conf_epfl_llama_all_l_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_l_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_l_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_l_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_l_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_l_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_all_l_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_all_l_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_l_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_l_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_l_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_l_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_l_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_l_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_all_l_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_all_l_7$overall)) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "llama70b_all_l_2" = "Llama3 \n(70B) prompt 2",
                                    "llama70b_all_l_1" = "Llama3 \n(70B) prompt 1",
                                    "llama70b_all_l_3" = "Llama3 \n(70B) prompt 3",
                                    "llama70b_all_l_4" = "Llama3 \n(70B) prompt 4",
                                    "llama70b_all_l_5" = "Llama3 \n(70B) prompt 5",
                                    "llama70b_all_l_6" = "Llama3 \n(70B) prompt 6",
                                    "llama70b_all_l_7" = "Llama3 \n(70B) prompt 7",
                                    "llama70b_all_l_8" = "Llama3 \n(70B) prompt 8",
                                    
                                    "llama_all_l_2" = "Llama3 \n(8B) prompt 2",
                                    "llama_all_l_1" = "Llama3 \n(8B) prompt 1",
                                    "llama_all_l_3" = "Llama3 \n(8B) prompt 3",
                                    "llama_all_l_4" = "Llama3 \n(8B) prompt 4",
                                    "llama_all_l_5" = "Llama3 \n(8B) prompt 5",
                                    "llama_all_l_6" = "Llama3 \n(8B) prompt 6",
                                    "llama_all_l_7" = "Llama3 \n(8B) prompt 7",
                                    "llama_all_l_8" = "Llama3 \n(8B) prompt 8",
                                    "mistral_all_l_2" = "Mistral \n(7B) prompt 2",
                                    "mistral_all_l_1" = "Mistral \n(7B) prompt 1",
                                    "mistral_all_l_3" = "Mistral \n(7B) prompt 3",
                                    "mistral_all_l_4" = "Mistral \n(7B) prompt 4",
                                    "mistral_all_l_5" = "Mistral \n(7B) prompt 5",
                                    "mistral_all_l_6" = "Mistral \n(7B) prompt 6",
                                    "mistral_all_l_7" = "Mistral \n(7B) prompt 7",
                                    "mistral_all_l_8" = "Mistral \n(7B) prompt 8",
                                    "mixtral_all_l_2" = "Mixtral \n(8x7B) prompt 2",
                                    "mixtral_all_l_1" = "Mixtral \n(8x7B) prompt 1",
                                    "mixtral_all_l_3" = "Mixtral \n(8x7B) prompt 3",
                                    "mixtral_all_l_4" = "Mixtral \n(8x7B) prompt 4",
                                    "mixtral_all_l_5" = "Mixtral \n(8x7B) prompt 5",
                                    "mixtral_all_l_6" = "Mixtral \n(8x7B) prompt 6",
                                    "mixtral_all_l_7" = "Mixtral \n(8x7B) prompt 7",
                                    "mixtral_all_l_8" = "Mixtral \n(8x7B) prompt 8",
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

accuracy_fig_l


# Combined figure for accuracy and CI --------------
## Dataset for CI and accuracy selecting majority class per agreement facet ---------
ci_majority_accuracy_annotators <- data.frame(
  xmin = 0,
  xmax = c(2, 2, 9, 9, 9, 9,9,9,
           2, 2, 9, 9, 9, 9,9,9,
           2, 2, 9, 9, 9, 9,9,9,
           2, 2, 9, 9, 9, 9,9,9),
  ymin = c(conf_epfl_majority_g$overall[[3]],
           conf_epfl_majority_g$overall[[3]],
           conf_epfl_majority_g$overall[[3]],
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
           conf_epfl_majority_m$overall[[3]],
           conf_epfl_majority_m$overall[[3]],
           0.39,0.39,0.39,0.39,0.39,0.39,0.39,0.39,
           conf_epfl_majority_c$overall[[3]],
           conf_epfl_majority_c$overall[[3]],
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
           conf_epfl_majority_g$overall[[4]],
           conf_epfl_majority_g$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           conf_epfl_majority_m$overall[[4]],
           0.46,0.46,0.46,0.46,0.46,0.46,0.46,0.46,
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]],
           conf_epfl_majority_c$overall[[4]]),
  annotator = c("Annotator", "Annotator", "Annotator",
                "Annotator", "Annotator", "Annotator",
                "Annotator", "Annotator",
                "Co-author (1)", "Co-author (1)",
                "Co-author (1)", "Co-author (1)",
                "Co-author (1)", "Co-author (1)",
                "Co-author (1)", "Co-author (1)",
                "Co-author (2)", "Co-author (2)",
                "Co-author (2)", "Co-author (2)",
                "Co-author (2)", "Co-author (2)",
                "Co-author (2)", "Co-author (2)",
                "General public", "General public",
                "General public", "General public",
                "General public", "General public",
                "General public", "General public"),
  Method = c("Mturk", "Vader","GPT 3.5", "GPT 4",
             "Mistral", "Mixtral", "Llama3 \n(8B)", "Llama3 \n(70B)",
             "Mturk", "Vader","GPT 3.5", "GPT 4",
             "Mistral", "Mixtral", "Llama3 \n(8B)", "Llama3 \n(70B)",
             "Mturk", "Vader","GPT 3.5", "GPT 4",
             "Mistral", "Mixtral", "Llama3 \n(8B)", "Llama3 \n(70B)",
             "Mturk", "Vader","GPT 3.5", "GPT 4",
             "Mistral", "Mixtral", "Llama3 \n(8B)", "Llama3 \n(70B)"))

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

ggsave("outputs/accuracy_figure_annotators.jpeg",
       accuracy_fig_all_annotators,
       width = 12, height = 7)
