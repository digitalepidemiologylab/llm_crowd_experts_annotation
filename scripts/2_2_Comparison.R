# Script information ------------
#' Aim: Comparing all annotations
#' Author: Laura Espinosa
#' Date created: 12 October 2023
#' Date updated: 12 October 2023

# Get all datasets -------------
df_all <- df_mturk_annot_clean %>% 
  full_join(gpt_clean, by = "id_tweets") %>% 
  full_join(epfl_df, by = "text") 

df_all_available <- df_all %>% 
  group_by(id_tweets.x, prompt) %>% 
  tally() %>% 
  ungroup()
  
id_tweets_unique <- unique(df_all_available$id_tweets.x)
prompts_unique <- unique(df_all_available$prompt) %>% 
  filter(!is.na(.))

df_all_complete <- expand.grid(id_tweets.x = id_tweets_unique, prompt = prompts_unique) %>% 
  filter(!is.na(prompt))

missing_entries <- df_all_available %>% 
  select(id_tweets.x, prompt) %>% 
  setdiff(df_all_complete, .) %>% 
  filter(prompt != 0) %>% 
  group_by(prompt) %>% 
  tally()

## To retrieve the real tweet ids -------------
tweets_id_real <- select(df_mturk_annot_clean, 'text') %>% 
  left_join(select(df_mturk, 'tweet_id', "text"), by = "text") %>% 
  filter(duplicated(text) == FALSE) %>% 
  left_join(select(df_mturk_annot_clean, 'text', 'id_tweets'),
            by = "text") 

# Merge databases ----------------
df_all_clean <- df_all %>% 
  rename("neutral_mturk" = "neutral",
         "positive_mturk" = "positive",
         "negative_mturk" = "negative",
         "id_tweets" = "id_tweets.x") %>% 
  select(-id_tweets.y, -text) %>% 
  filter(!is.na(stance_epfl)) %>% 
  left_join(select(tweets_id_real, tweet_id, id_tweets),
            by = "id_tweets")

# Confusion matrix --------
prompts <- df_all_clean$prompt %>% 
  unique()


for (i in prompts) {
  df_con_matrix <- df_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_epfl, sentiment_gpt) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  
  #prompt_loop[i] <- prompts[i]
  conf_epfl_gpt <- confusionMatrix(df_con_matrix$stance_epfl, 
                                   df_con_matrix$sentiment_gpt)
  conf_epfl_gpt$prompt <- prompts[i]
  assign(paste('conf_epfl_gpt_all',i,sep='_'),conf_epfl_gpt) %>% 
    capture.output(., file = paste0("outputs/conf_epfl_gpt_all", i, ".csv"))
  
}



