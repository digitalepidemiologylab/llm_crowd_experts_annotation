# Script information ------------
#' Aim: Get datasets for comparison 
#' Author: Laura Espinosa
#' Date created: 5 October 2023
#' Date updated: 12 October 2023

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

epfl_df_all <- full_join(epfl_df_1, epfl_df_2, by = "text") %>% 
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
  filter(!is.na(sent_l)) %>% 
  mutate(stance_epfl = pmap_chr(select(., starts_with("stance")), ~ {
    x <- c(...)
    if(length(unique(x)) == 1) {
      unique(x)
    } else {
      names(table(x))[which.max(table(x))]
    }
  }))

epfl_df_s_all <- epfl_df_all  %>% 
  select(id_tweets, stance_epfl, agree_stance) %>% 
  write_csv("data/epfl_annotations_simple_all.csv")

## Descriptive analysis (all tweets) --------
### Tweets per agreement and class -----------
epfl_df_class_agreement_all <- epfl_df_all %>% 
  #filter(agree_stance == 1 ) %>% 
  group_by(stance_epfl) %>% 
  tally() %>% 
  mutate(percent = n / sum(n) * 100)


## Descriptive analysis (partial agreement) --------
epfl_df <- epfl_df_all %>% 
  filter(agree_stan == 1)


epfl_df_s <- epfl_df  %>% 
  select(id_tweets, stance_epfl, agree_stance, agree_stan) %>% 
  write_csv("data/epfl_annotations_simple_partial_agreement.csv")

### Tweets per agreement and class -----------
epfl_df_class_agreement <- epfl_df %>% 
  filter(agree_stan == 1 ) %>% 
  group_by(stance_epfl) %>% 
  tally() %>% 
  mutate(percent = n / sum(n) * 100)

## Descriptive analysis (full agreement) --------
epfl_df_full <- epfl_df_all %>% 
  filter(agree_stance == 1)


epfl_df_full_s <- epfl_df_full  %>% 
  select(id_tweets, stance_epfl, agree_stance) %>% 
  write_csv("data/epfl_annotations_simple_full_agreement.csv")

### Tweets per agreement and class -----------
epfl_df_class_agreement_full <- epfl_df_full %>% 
  filter(agree_stance == 1 ) %>% 
  group_by(stance_epfl) %>% 
  tally() %>% 
  mutate(percent = n / sum(n) * 100)

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
  mutate(id_tweets = 1:nrow(.)) %>% 
  semi_join(epfl_df['text']) %>% 
  mutate(neutral_per = neutral/total * 100,
         positive_per = positive/total * 100,
         negative_per = negative/total *100,
         partial_agree = case_when(neutral_per >= 75 | positive_per >= 75 | negative_per >= 75 ~ 1,
                                   .default = 0),
         full_agree = case_when(neutral_per == 100 | positive_per == 100 | negative_per == 100 ~ 1,
                                   .default = 0))

df_mturk_annot_clean %>% 
  group_by(partial_agree) %>% 
  tally() %>% 
  mutate(percent = n/sum(n) * 100)

df_mturk_annot_clean %>% 
  group_by(full_agree) %>% 
  tally() %>% 
  mutate(percent = n/sum(n) * 100)


df_mturk_annot_clean_s <- df_mturk_annot_clean %>% 
  select(id_tweets, agree_mturk) %>% 
  write_csv("data/mturk_annotations_simple.csv")  

## Get tweets' dates --------------
mturk_dates <- df_mturk %>%
  select(text, created_at) %>% 
  left_join(df_mturk_annot, by = c("text")) %>% 
  filter(!is.na(agree_mturk)) %>% 
  distinct(text, created_at)

min(mturk_dates$created_at)
max(mturk_dates$created_at)

## Get mturkers ---------
df_mturk %>% 
  semi_join(epfl_df['text']) %>% 
  select(worker_id) %>% 
  unique() %>% 
  count()

## Number of tweets per number of annotations -----------
mturk_number_annotations <- df_mturk_annot_clean %>% 
  group_by(total) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(percent = n / sum(n) * 100)

## Get dates of the Mturk ------------
### Convert each entry to a JSON object
#mturk_job_initialised <- lapply(df_mturk$full_log, fromJSON)

# Extract the value of 'userTimeInitialized' from each JSON object
# mturk_job_initialised_clean <- sapply(mturk_job_initialised, function(obj) obj$userTimeInitialized) %>%
#   as.data.frame() %>%
#   arrange() %>%
#   rename("var1" = ".")

## Get number of Mturkers ---------


# Get ChatGPT annotations -----------------
message("Getting ChatGPT annotations")
setwd("data/local")
files_gpt <- fs::dir_ls(glob = "gpt_sentiment_mturk_prompt*csv")
gpt <- vroom(files_gpt)
setwd("../..")

gpt_clean <- gpt %>% 
  select(text, sentiment_gpt, prompt) %>% 
  separate(col = sentiment_gpt, into = c("sentiment_gpt", "explanation"),
           sep = 11) %>% 
  mutate(sentiment_gpt = tolower(sentiment_gpt),
         # id_tweets = id_gpt + 1,
         sentiment_gpt = case_when(str_detect(sentiment_gpt, 'neutral') ~ "neutral",
                                   str_detect(sentiment_gpt, 'positive') ~ "positive",
                                   .default = "negative")) %>% 
  distinct(text, prompt, .keep_all = TRUE)

gpt_clean$sentiment_gpt <- str_replace_all(string = gpt_clean$sentiment_gpt, 
                                           pattern = "\r\n", 
                                           replacement = "") %>%
  tolower()

unique(gpt_clean$prompt)

gpt_clean <- epfl_df %>% 
  select(id_tweets, text) %>% 
  right_join(gpt_clean, by = "text") 

gpt_clean_s <- gpt_clean %>% 
  select(id_tweets, sentiment_gpt, prompt) %>% 
  write_csv("data/gpt_annotations_simple.csv")

## Descriptive analysis of GPT -----------
gpt_descriptive <- gpt_clean %>% 
  mutate(prompt = str_replace_all(prompt, 
                                  c("40" = "GPT 4 prompt 0",
                                    "41" = "GPT 4 prompt 1",
                                    "45" = "GPT 4 prompt 5",
                                    "43" = "GPT 4 prompt 3",
                                    "44" = "GPT 4 prompt 4",
                                    "46" = "GPT 4 prompt 6",
                                    "47" = "GPT 4 prompt 7",
                                    "48" = "GPT 4 prompt 8",
                                    "^0$" = "GPT 3.5 prompt 0",
                                    "^1$" = "GPT 3.5 prompt 1",
                                    "^3$" = "GPT 3.5 prompt 3",
                                    "^4$" = "GPT 3.5 prompt 4",
                                    "^5$" = "GPT 3.5 prompt 5",
                                    "^6$" = "GPT 3.5 prompt 6",
                                    "^7$" = "GPT 3.5 prompt 7",
                                    "^8$" = "GPT 3.5 prompt 8"))) %>% 
  group_by(prompt, sentiment_gpt) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  separate(prompt, c("GPT", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_gpt",
              id_cols = c("GPT" , "Prompt")) 

gpt_descriptive %>% 
  write_csv("outputs/descriptive_gpt.csv")

# Get all datasets -------------
df_all <- df_mturk_annot_clean %>% 
  full_join(epfl_df, by = "text") %>% 
  full_join(gpt_clean, by = "text") %>% 
  filter(!is.na(sent_l)) %>% 
  select(-id_tweets.x, -id_tweets.y) # id_tweets comes from gpt_clean (supposedly same as epfl_df)

df_all_available <- df_all %>% 
  group_by(id_tweets, prompt) %>% 
  tally() %>% 
  ungroup()

id_tweets_unique <- unique(df_all_available$id_tweets)
prompts_unique <- unique(df_all_available$prompt) #%>% 
#  filter(!is.na(.))

df_all_complete <- expand.grid(id_tweets = id_tweets_unique, prompt = prompts_unique) %>% 
  filter(!is.na(prompt))

missing_entries <- df_all_available %>% 
  select(id_tweets, prompt) %>% 
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
         "negative_mturk" = "negative") %>% 
  select(-text) %>% 
  filter(!is.na(stance_epfl)) %>% 
  left_join(select(tweets_id_real, tweet_id, id_tweets),
            by = "id_tweets") 

df_all_clean %>% 
  write_csv("outputs/all_datasets_for_shiny.csv")

