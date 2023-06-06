# Script information ------------
#' Aim: Descriptive analysis of annotated English tweets on vaccine public perception 
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Mturk data --------------
## Clean data -----------------------
message("Cleaning labelled tweets")
setwd("data/local/mturk")
files <- fs::dir_ls(glob = "mturk_batch_job_results-*csv")
tweets <- vroom(files)
unique(tweets$question_tag)
tweets_clean <- tweets %>% 
  filter(question_tag == "vax_sentiment" | question_tag == "sentiment") %>% 
  select(-question_tag, -question_id, -answer_id, -id, -full_log, -total_duration_ms, -worker_id, -task_id) %>% 
  mutate(answer_tag = case_when(answer_tag == "very_negative" | answer_tag == "rather_negative" ~ "negative",
                                answer_tag == "very_positive" | answer_tag == "rather_positive" ~ "positive",
                               answer_tag == "neutral" ~ "neutral",
                               TRUE ~ NA_character_)#,
         #id = format(id, scientific = FALSE)
         )
setwd("../../..")
write_csv(tweets_clean, "data/local/labelled_tweets_clean_all_mturk.csv")

## Group data ---------------
message("Group data")

tweets_clean_agg <- tweets_clean %>% 
  filter(!is.na(answer_tag)) %>% 
  group_by(text, answer_tag) %>% 
  tally() %>% 
  ungroup() %>% 
  pivot_wider(id_cols = text,
              names_from = c(answer_tag),
              values_from = n) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(neutral = as.numeric(neutral),
         positive = as.numeric(positive),
         negative = as.numeric(negative),
         total = neutral + negative + positive) %>% 
  filter(total >= 3) %>% 
  filter(!is.na(text))
  #mutate(agree = case_when(neutral/total))


tweets_clean_agg$agree_mturk <- colnames(tweets_clean_agg[,c(2:4)])[apply(tweets_clean_agg[,c(2:4)],1,which.max)]
  
 tweets_clean_agg_1000 <- tweets_clean_agg %>% 
   sample_n(1000)

write_csv(tweets_clean_agg, "data/local/labelled_tweets_clean_all_mturk_agg.csv")
write_csv(tweets_clean_agg_1000, "data/local/labelled_tweets_clean_all_mturk_agg_sample1000.csv")

# Give me one word to select between neutral, positive and negative for the sentiment of this tweet:

# ## Including dates ------------
# df <- read_csv("data/local/annotated_tweets_2.csv")
# 
# df_annotated <- df %>% 
#   filter(id %in% tweets_clean$id)

# GPT -----------
## Clean data -----------------
setwd("data/local")
files_gpt <- fs::dir_ls(glob = "gpt_sentiment_mturk_v*csv")
gpt <- vroom(files_gpt)
setwd("../..")

gpt_clean <- gpt %>% 
  select(id_gpt, sentiment_gpt, run) %>% 
  mutate(id_tweets = id_gpt + 1,
         sentiment_gpt = tolower(sentiment_gpt),
         sentiment_gpt = case_when(str_detect(sentiment_gpt, 'neutral') ~ "neutral",
                                   str_detect(sentiment_gpt, 'positive') ~ "positive",
                                   .default = "negative"))

gpt_clean$sentiment_gpt <- str_replace_all(string = gpt_clean$sentiment_gpt, 
                                           pattern = "\r\n", 
                                           replacement = "") %>%
  tolower()

gpt_clean <- gpt_clean %>% 
  group_by(id_gpt, sentiment_gpt) %>% 
  tally() %>% 
  ungroup() %>% 
  pivot_wider(id_cols = id_gpt,
              names_from = sentiment_gpt,
              values_from = n) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(id_tweets= id_gpt +1,
         neutral = as.numeric(neutral),
         positive = as.numeric(positive),
         negative = as.numeric(negative),
         total = neutral + negative + positive) %>% 
  #filter(total >= 3) %>% 
  filter(!is.na(text))

gpt_clean$agree_gpt <- colnames(gpt_clean[,c(2:4)])[apply(gpt_clean[,c(2:4)],1,which.max)]

write_csv(gpt_clean, "data/local/gpt_sentiment_mturk.csv")
