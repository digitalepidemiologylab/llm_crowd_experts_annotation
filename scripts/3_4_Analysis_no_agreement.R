# Script information ------------
#' Aim: Comparing all annotations
#' Author: Laura Espinosa
#' Date created: 2 June 2024
#' Date updated: 2 June 2024

# Import data -------------
epfl_df_no_agree <- full_join(epfl_df_1, epfl_df_2, by = "text") %>% 
  full_join(epfl_df_3, by = "text") %>% 
  full_join(epfl_df_4, by = "text") %>% 
  mutate(id_tweets = 1:nrow(epfl_df_1),
         agree_stance = case_when(stance_c == stance_m & stance_m == stance_g & stance_g == stance_l ~ 1,
                                  .default = 0)) %>% 
  select(id_tweets, text, sent_m, sent_c, sent_g, sent_l, 
         stance_m, stance_c, stance_g, stance_l, agree_stance) %>% 
  mutate(neutral_sent = rowSums(.[3:6] == "neutral")/4*100,
         pos_sent = rowSums(.[3:6] == "positive")/4*100,
         neg_sent = rowSums(.[3:6] == "negative")/4*100,
         agree_sent = case_when(neutral_sent >= 75 | pos_sent >= 75 | neg_sent >= 75 ~ 1,
                                .default = 0),
         neutral_stan = rowSums(.[7:10] == "neutral")/4*100,
         pos_stan = rowSums(.[7:10] == "positive")/4*100,
         neg_stan = rowSums(.[7:10] == "negative")/4*100,
         agree_stan = case_when(neutral_stan >= 75 | pos_stan >= 75 | neg_stan >= 75 ~ 1,
                                .default = 0),
         stance_mean = ((pos_stan * 2) + (neg_stan * -2))/ (pos_stan + neg_stan + neutral_stan)) %>% 
  select(id_tweets, text, sent_m, sent_c, sent_g, sent_l, neutral_sent,
         pos_sent, neg_sent, agree_sent, neutral_stan, pos_stan, neg_stan,
         agree_stan, stance_m, stance_c, stance_g, stance_l, agree_stance) %>% 
  filter(agree_stan == 0) %>%
  mutate(agreement = case_when((neutral_stan == 50 & pos_stan == 50) | (neutral_stan == 50 & neg_stan == 50) ~ "two-two",
                               (pos_stan == 50 & neutral_stan == 50) | (pos_stan == 50 & neg_stan == 50) ~ "two-two",
                               (neg_stan == 50 & neutral_stan == 50) | (neg_stan == 50 & pos_stan == 50) ~ "two-two",
                               .default = "two-one-one"),
         stance_predominant = case_when(agreement == "two-one-one" & neutral_stan == 50 ~ "neutral",
                                        agreement == "two-one-one" & pos_stan == 50 ~ "positive",
                                        agreement == "two-one-one" & neg_stan == 50 ~ "negative",
                                        agreement == "two-two" & neutral_stan == 50 & pos_stan == 50 ~ "neutral-positive",
                                        agreement == "two-two" & neutral_stan == 50 & neg_stan == 50 ~ "neutral-negative",
                                        agreement == "two-two" & neg_stan == 50 & pos_stan == 50 ~ "negative-positive",
                                        .default = "negative"))

epfl_df_no_agree_sum <- epfl_df_no_agree %>% 
  group_by(agreement, stance_predominant) %>% 
  tally() %>% 
  arrange(agreement, desc(n))

## Get merged datasets ----------
df_no_agree_all <- df_mturk_annot %>% 
  mutate(id_tweets = 1:nrow(.)) %>% 
  semi_join(epfl_df_all['text']) %>% 
  mutate(neutral_per = neutral/total * 100,
         positive_per = positive/total * 100,
         negative_per = negative/total *100,
         partial_agree = case_when(neutral_per >= 75 | positive_per >= 75 | negative_per >= 75 ~ 1,
                                   .default = 0),
         full_agree = case_when(neutral_per == 100 | positive_per == 100 | negative_per == 100 ~ 1,
                                .default = 0)) %>% 
  full_join(epfl_df_no_agree, by = "text") %>% 
  dplyr::filter(!is.na(stance_predominant)) %>% 
  full_join(gpt_clean, by = "text") %>%
  full_join(mistral_clean, by = c("text", "prompt")) %>% 
  full_join(mixtral_clean, by = c("text", "prompt")) %>% 
  full_join(df_vader, by = "text") %>% 
  full_join(llama_clean, by = c("text", "prompt")) %>% 
  full_join(llama70b_clean, by = c("text", "prompt")) %>% 
  mutate(sent_vader = tolower(sent_vader)) %>% 
  select(-id_tweets.x, -id_tweets.y) # id_tweets comes from gpt_clean (supposedly same as epfl_df)

df_no_agree_all_clean <- df_no_agree_all %>% 
  rename("neutral_mturk" = "neutral",
         "positive_mturk" = "positive",
         "negative_mturk" = "negative") %>% 
  select(-text) %>% 
  dplyr::filter(!is.na(stance_predominant)) %>% 
  left_join(select(tweets_id_real, tweet_id, id_tweets),
            by = "id_tweets") 

df_no_agree_all_clean_2_1_1 <- df_no_agree_all_clean %>% 
  filter(agreement == "two-one-one")


df_no_agree_all_clean_2_2 <- df_no_agree_all_clean %>% 
  filter(agreement == "two-two")

split_stance = str_split(df_no_agree_all_clean_2_2$stance_predominant,
                         "-", simplify = TRUE)

set.seed(42) # for reproducibility of random selection of stance below

sample_stance = function(words) {
  sample(words, 1, prob = c(0.5, 0.5))
}

df_no_agree_all_clean_2_2_new <- df_no_agree_all_clean_2_2 %>% 
  rowwise() %>% 
  mutate(random_stance_predominant = sample_stance(split_stance[row_number(),]))
    
# Get accuracy and other metrics for the two-two tweets ------------
prompts <- df_no_agree_all_clean_2_2_new$prompt %>% 
  unique()

## EPFL vs GPT ------------
for (i in prompts) {
  df_con_matrix_2_2 <- df_no_agree_all_clean_2_2_new %>% 
    filter(prompt == i) %>% 
    select(random_stance_predominant, sentiment_gpt) %>% 
    mutate(random_stance_predominant = factor(random_stance_predominant, ordered = TRUE,
                                       levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_2_2',i,sep='_'), df_con_matrix_2_2)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_gpt_2_2 <- confusionMatrix(df_con_matrix_2_2$sentiment_gpt,
                                         df_con_matrix_2_2$random_stance_predominant, 
                                         mode = "everything")
  conf_epfl_gpt_2_2$prompt <- prompts[i]
  assign(paste('conf_epfl_gpt_2_2_all',i,sep='_'),conf_epfl_gpt_2_2) 
  # assign(paste('conf_epfl_gpt_all',i,sep='_'),conf_epfl_gpt) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_gpt_all", i, ".csv"))
  
}


## EPFL vs Mturk --------------
df_con_matrix_mturk_2_2 <- df_no_agree_all_clean_2_2_new %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(random_stance_predominant, agree_mturk) %>% 
  mutate(random_stance_predominant = factor(random_stance_predominant, ordered = TRUE,
                                     levels = c("positive","neutral", "negative")),
         agree_mturk = factor(agree_mturk, ordered = TRUE,
                              levels = c("positive", "neutral", "negative"))) 

conf_epfl_mturk_2_2 <- confusionMatrix(df_con_matrix_mturk_2_2$agree_mturk,
                                         df_con_matrix_mturk_2_2$random_stance_predominant, mode = "everything")

## EPFL vs vader --------------
df_con_matrix_vader_2_2 <- df_no_agree_all_clean_2_2_new %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(random_stance_predominant, sent_vader) %>% 
  mutate(random_stance_predominant = factor(random_stance_predominant, ordered = TRUE,
                                     levels = c("positive","neutral", "negative")),
         sent_vader = factor(sent_vader, ordered = TRUE,
                             levels = c("positive", "neutral", "negative"))) 

conf_epfl_vader_2_2 <- confusionMatrix(df_con_matrix_vader_2_2$sent_vader,
                                         df_con_matrix_vader_2_2$random_stance_predominant, mode = "everything")



## EPFL vs Mistral ---------------
for (i in prompts) {
  df_con_matrix_mistral_2_2 <- df_no_agree_all_clean_2_2_new %>% 
    filter(prompt == i) %>% 
    select(random_stance_predominant, sentiment_mistral) %>% 
    mutate(random_stance_predominant = factor(random_stance_predominant, ordered = TRUE,
                                       levels = c("positive","neutral", "negative")),
           sentiment_mistral = factor(sentiment_mistral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mistral_2_2',i,sep='_'), df_con_matrix_mistral_2_2)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mistral_2_2 <- confusionMatrix(df_con_matrix_mistral_2_2$sentiment_mistral,
                                             df_con_matrix_mistral_2_2$random_stance_predominant, mode = "everything")
  conf_epfl_mistral_2_2$prompt <- prompts[i]
  assign(paste('conf_epfl_mistral_2_2_all',i,sep='_'),conf_epfl_mistral_2_2) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs Mixtral ---------------
for (i in prompts) {
  df_con_matrix_mixtral_2_2 <- df_no_agree_all_clean_2_2_new %>% 
    filter(prompt == i) %>% 
    select(random_stance_predominant, sentiment_mixtral) %>% 
    mutate(random_stance_predominant = factor(random_stance_predominant, ordered = TRUE,
                                       levels = c("positive","neutral", "negative")),
           sentiment_mixtral = factor(sentiment_mixtral, ordered = TRUE,
                                      levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mixtral_2_2',i,sep='_'), df_con_matrix_mixtral_2_2)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_mixtral_2_2 <- confusionMatrix(df_con_matrix_mixtral_2_2$sentiment_mixtral,
                                             df_con_matrix_mixtral_2_2$random_stance_predominant, mode = "everything")
  conf_epfl_mixtral_2_2$prompt <- prompts[i]
  assign(paste('conf_epfl_mixtral_2_2_all',i,sep='_'),conf_epfl_mixtral_2_2) 
  # assign(paste('conf_epfl_mistral_all',i,sep='_'),conf_epfl_mistral) %>% 
  #   capture.output(., file = paste0("outputs/confusion_matrices/conf_epfl_mistral_all", i, ".csv"))
  
}

## EPFL vs Llama3 ---------------
for (i in prompts) {
  df_con_matrix_llama_2_2 <- df_no_agree_all_clean_2_2_new %>% 
    filter(prompt == i) %>% 
    select(random_stance_predominant, sentiment_llama) %>% 
    mutate(random_stance_predominant = factor(random_stance_predominant, ordered = TRUE,
                                       levels = c("positive","neutral", "negative")),
           sentiment_llama = factor(sentiment_llama, ordered = TRUE,
                                    levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_llama_2_2',i,sep='_'), df_con_matrix_llama_2_2)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_llama_2_2 <- confusionMatrix(df_con_matrix_llama_2_2$sentiment_llama,
                                           df_con_matrix_llama_2_2$random_stance_predominant, mode = "everything")
  conf_epfl_llama_2_2$prompt <- prompts[i]
  assign(paste('conf_epfl_llama_2_2_all',i,sep='_'), conf_epfl_llama_2_2) 
  
}

## EPFL vs Llama3 70b ---------------
for (i in prompts) {
  df_con_matrix_llama70b_2_2 <- df_no_agree_all_clean_2_2_new %>% 
    filter(prompt == i) %>% 
    select(random_stance_predominant, sentiment_llama70b) %>% 
    mutate(random_stance_predominant = factor(random_stance_predominant, ordered = TRUE,
                                              levels = c("positive","neutral", "negative")),
           sentiment_llama70b = factor(sentiment_llama70b, ordered = TRUE,
                                    levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_llama70b_2_2',i,sep='_'), df_con_matrix_llama70b_2_2)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_llama70b_2_2 <- confusionMatrix(df_con_matrix_llama70b_2_2$sentiment_llama70b,
                                         df_con_matrix_llama70b_2_2$random_stance_predominant, mode = "everything")
  conf_epfl_llama70b_2_2$prompt <- prompts[i]
  assign(paste('conf_epfl_llama70b_2_2_all',i,sep='_'), conf_epfl_llama70b_2_2) 
  
}

## EPFL vs selecting majority class ------------  
df_con_matrix_majority_2_2 <- df_no_agree_all_clean_2_2_new %>% 
  filter(!duplicated(id_tweets)) %>% 
  select(random_stance_predominant) %>%
  mutate(stance_majority = "neutral", 
         random_stance_predominant = factor(random_stance_predominant, ordered = TRUE,
                                     levels = c("positive","neutral", "negative")),
         stance_majority = factor(stance_majority, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 


conf_epfl_majority_2_2 <- confusionMatrix(df_con_matrix_majority_2_2$stance_majority,
                                            df_con_matrix_majority_2_2$random_stance_predominant)

## Merging all confusion matrices for 2-2 ------------
### Accuracy -------------
overall_2_2_all_fig <- as.data.frame(conf_epfl_mturk_2_2$overall) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_3$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_4$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_6$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_7$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_42$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_41$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_45$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_43$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_44$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_46$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_47$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_2_2_all_48$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_2_2_all_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_2_2_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_2_2_all_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_2_2_all_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_2_2_all_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mistral_2_2_all_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_2_2_all_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mistral_2_2_all_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_2_2_all_2$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_2_2_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_mixtral_2_2_all_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_2_2_all_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_2_2_all_5$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_2_2_all_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_2_2_all_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_mixtral_2_2_all_8$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_2_2_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama_2_2_all_2$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_2_2_all_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_2_2_all_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_2_2_all_5$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_2_2_all_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_2_2_all_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama_2_2_all_8$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_2_2_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_llama70b_2_2_all_2$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_2_2_all_3$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_2_2_all_4$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_2_2_all_5$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_2_2_all_6$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_2_2_all_7$overall)) %>%
  cbind(as.data.frame(conf_epfl_llama70b_2_2_all_8$overall)) %>%
  cbind(as.data.frame(conf_epfl_vader_2_2$overall)) %>%
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "mistral_2_2_all_2" = "Mistral \n(7B) prompt 2",
                                    "mistral_2_2_all_1" = "Mistral \n(7B) prompt 1",
                                    "mistral_2_2_all_3" = "Mistral \n(7B) prompt 3",
                                    "mistral_2_2_all_4" = "Mistral \n(7B) prompt 4",
                                    "mistral_2_2_all_5" = "Mistral \n(7B) prompt 5",
                                    "mistral_2_2_all_6" = "Mistral \n(7B) prompt 6",
                                    "mistral_2_2_all_7" = "Mistral \n(7B) prompt 7",
                                    "mistral_2_2_all_8" = "Mistral \n(7B) prompt 8",
                                    "mixtral_2_2_all_2" = "Mixtral \n(8x7B) prompt 2",
                                    "mixtral_2_2_all_1" = "Mixtral \n(8x7B) prompt 1",
                                    "mixtral_2_2_all_3" = "Mixtral \n(8x7B) prompt 3",
                                    "mixtral_2_2_all_4" = "Mixtral \n(8x7B) prompt 4",
                                    "mixtral_2_2_all_5" = "Mixtral \n(8x7B) prompt 5",
                                    "mixtral_2_2_all_6" = "Mixtral \n(8x7B) prompt 6",
                                    "mixtral_2_2_all_7" = "Mixtral \n(8x7B) prompt 7",
                                    "mixtral_2_2_all_8" = "Mixtral \n(8x7B) prompt 8",
                                    "gpt_2_2_all_42" = "GPT 4 prompt 2",
                                    "gpt_2_2_all_41" = "GPT 4 prompt 1",
                                    "gpt_2_2_all_43" = "GPT 4 prompt 3",
                                    "gpt_2_2_all_44" = "GPT 4 prompt 4",
                                    "gpt_2_2_all_45" = "GPT 4 prompt 5",
                                    "gpt_2_2_all_46" = "GPT 4 prompt 6",
                                    "gpt_2_2_all_47" = "GPT 4 prompt 7",
                                    "gpt_2_2_all_48" = "GPT 4 prompt 8",
                                    "gpt_2_2_all_2" = "GPT 3.5 prompt 2",
                                    "gpt_2_2_all_1" = "GPT 3.5 prompt 1",
                                    "gpt_2_2_all_3" = "GPT 3.5 prompt 3",
                                    "gpt_2_2_all_4" = "GPT 3.5 prompt 4",
                                    "gpt_2_2_all_5" = "GPT 3.5 prompt 5",
                                    "gpt_2_2_all_6" = "GPT 3.5 prompt 6",
                                    "gpt_2_2_all_7" = "GPT 3.5 prompt 7",
                                    "gpt_2_2_all_8" = "GPT 3.5 prompt 8",
                                    "llama70b_2_2_all_2" = "Llama3 (70B) prompt 2",
                                    "llama70b_2_2_all_1" = "Llama3 (70B) prompt 1",
                                    "llama70b_2_2_all_3" = "Llama3 (70B) prompt 3",
                                    "llama70b_2_2_all_4" = "Llama3 (70B) prompt 4",
                                    "llama70b_2_2_all_5" = "Llama3 (70B) prompt 5",
                                    "llama70b_2_2_all_6" = "Llama3 (70B) prompt 6",
                                    "llama70b_2_2_all_7" = "Llama3 (70B) prompt 7",
                                    "llama70b_2_2_all_8" = "Llama3 (70B) prompt 8",
                                    "llama_2_2_all_2" = "Llama3 (8B) prompt 2",
                                    "llama_2_2_all_1" = "Llama3 (8B) prompt 1",
                                    "llama_2_2_all_3" = "Llama3 (8B) prompt 3",
                                    "llama_2_2_all_4" = "Llama3 (8B) prompt 4",
                                    "llama_2_2_all_5" = "Llama3 (8B) prompt 5",
                                    "llama_2_2_all_6" = "Llama3 (8B) prompt 6",
                                    "llama_2_2_all_7" = "Llama3 (8B) prompt 7",
                                    "llama_2_2_all_8" = "Llama3 (8B) prompt 8",
                                    "mturk_2_2" = "Amazon Mturk",
                                    "vader_2_2" = "Vader"))) %>% 
  select(method, Accuracy, AccuracyLower, AccuracyUpper, AccuracyPValue) %>% 
  separate(col = "method", into = c("Method", "Prompt"), sep = " prompt ") %>% 
  mutate(Pvalue = case_when(AccuracyPValue <= 0.05 ~ "<= 0.05",
                            .default = "> 0.05"),
         agreement = "Partial agreement",
         Prompt = replace_na(Prompt, "None"))


overall_2_2_all <- overall_2_2_all_fig %>% 
  mutate(Accuracy = round(Accuracy, 4),
         AccuracyLower = round(AccuracyLower, 4),
         AccuracyUpper = round(AccuracyUpper, 4),
         AccuracyPValue = round(AccuracyPValue, 6),
         AccuracyCI = paste("(", AccuracyLower, " - ", AccuracyUpper, ")", sep = "")) %>%
  select(Method, Prompt, Accuracy, AccuracyCI, AccuracyPValue) %>% 
  rename("Accuracy (95% CI)" = "AccuracyCI",
         "Accuracy (p-value)" = "AccuracyPValue")

overall_2_2_all %>%
  write_csv("outputs/confusion_matrix_accuracy_2_2.csv")

#### Plot accuracy ----------------
accuracy_fig_2_2 <- overall_2_2_all_fig %>% 
  #filter(Method == "GPT 4") %>% 
  filter(!is.na(Prompt)) %>% 
  ggplot(aes(x = Prompt, y = Accuracy)) +
  geom_point(color = "black", size = 2) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper),
                    color = "black",
                width = 0.2) +
  facet_grid(~Method, scales = "free_x") 

accuracy_fig_2_2

ggsave("outputs/accuracy_figure_no_agreement.jpeg",
       accuracy_fig_all,
       width = 10, height = 6)

