# Script information ------------
#' Aim: Get datasets for comparison 
#' Author: Laura Espinosa
#' Date created: 5 October 2023
#' Date updated: 12 October 2023

# Get mturk annotations ---------------
message("Getting Mturk annotations")
setwd("data/local/mturk")
files <- fs::dir_ls(glob = "mturk_batch_job_results-*csv")
df_mturk <- vroom(files)
setwd("../../..")

unique(df_mturk$question_tag)
df_mturk_clean <- df_mturk %>% 
  filter(question_tag == "vax_sentiment" | question_tag == "sentiment") %>% 
  select(-question_tag, -question_id, -answer_id, -id, -full_log, -total_duration_ms, -worker_id, -task_id) %>% 
  mutate(answer_tag = case_when(answer_tag == "very_negative" | answer_tag == "rather_negative" | answer_tag == "negative" ~ "negative",
                                answer_tag == "very_positive" | answer_tag == "rather_positive" | answer_tag == "positive" ~ "positive",
                                answer_tag == "neutral" ~ "neutral",
                                TRUE ~ NA_character_)#,
         #id = format(id, scientific = FALSE) 
  ) %>% 
  mutate(id_tweets = 1:nrow(.))


## Mturk dataset aggregated used for GPT ----------
df_mturk_annot <- read_csv("data/local/labelled_tweets_clean_all_mturk_agg.csv")
df_mturk_annot_clean <- df_mturk_annot %>% 
  mutate(id_tweets = 1:nrow(.))

df_mturk_annot_clean_s <- df_mturk_annot_clean %>% 
  select(id_tweets, agree_mturk) %>% 
  write_csv("data/mturk_annotations_simple.csv")  

# Get EPFL annotations ------------------
message("Getting EPFL annotations")
epfl_df_1 <- read_excel("./data/local/epfl_annotated_tweets.xlsx",
                        sheet = "Celine") %>% 
  rename(`sent_c` = `General Sentiment`,
         `stance_c` = `Stance/attitude towards vaccination`)

epfl_df_2 <- read_excel("./data/local/epfl_annotated_tweets.xlsx",
                        sheet = "Marcel") %>% 
  rename(`sent_m` = `General Sentiment`,
         `stance_m` = `Stance/attitude towards vaccination`)

epfl_df_3 <- read_excel("./data/local/epfl_annotated_tweets.xlsx",
                        sheet = "Genevieve") %>% 
  rename(`sent_g` = `General Sentiment`,
         `stance_g` = `Stance/attitude towards vaccination`)

epfl_df_4 <- read_excel("./data/local/epfl_annotated_tweets.xlsx",
                        sheet = "Laura") %>% 
  rename(`sent_l` = `General Sentiment`,
         `stance_l` = `Stance/attitude towards vaccination`)

epfl_df <- full_join(epfl_df_1, epfl_df_2, by = "text") %>% 
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
  mutate(stance_epfl = pmap_chr(select(., starts_with("stance")), ~ {
    x <- c(...)
    if(length(unique(x)) == 1) {
      unique(x)
    } else {
      names(table(x))[which.max(table(x))]
    }
  }))

epfl_df_s <- epfl_df  %>% 
  select(id_tweets, stance_epfl, agree_stance) %>% 
  write_csv("data/epfl_annotations_simple.csv")

# Get ChatGPT annotations -----------------
message("Getting ChatGPT annotations")
setwd("data/local")
files_gpt <- fs::dir_ls(glob = "gpt_sentiment_mturk_prompt*csv")
gpt <- vroom(files_gpt)
setwd("../..")

gpt_clean <- gpt %>% 
  select(id_gpt, sentiment_gpt, prompt) %>% 
  separate(col = sentiment_gpt, into = c("sentiment_gpt", "explanation"),
           sep = 11) %>% 
  mutate(id_tweets = id_gpt + 1,
         sentiment_gpt = tolower(sentiment_gpt),
         sentiment_gpt = case_when(str_detect(sentiment_gpt, 'neutral') ~ "neutral",
                                   str_detect(sentiment_gpt, 'positive') ~ "positive",
                                   .default = "negative")) %>% 
  distinct(id_gpt, prompt, .keep_all = TRUE)

gpt_clean$sentiment_gpt <- str_replace_all(string = gpt_clean$sentiment_gpt, 
                                           pattern = "\r\n", 
                                           replacement = "") %>%
  tolower()

unique(gpt_clean$prompt)

gpt_clean_s <- gpt_clean %>% 
  select(id_tweets, sentiment_gpt, prompt) %>% 
  write_csv("data/gpt_annotations_simple.csv")

## Checking missing tweets per prompt ----------------
# gpt_prompts <- unique(gpt_clean$prompt)
# 
# gpt_missing <- gpt_clean %>% 
#   group_by(id_gpt, prompt) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   select(-n) 
# 
# 
# all_combinations <- expand.grid(id_gpt = unique(gpt_missing$id_gpt),
#                                 prompt = gpt_prompts)
# 
# gpt_missing_merged <- all_combinations %>% 
#   left_join(gpt_missing,
#             by = c("id_gpt", "prompt")) 
# 
# 
# gpt_missing_df <- setdiff(all_combinations, gpt_missing)
# 
