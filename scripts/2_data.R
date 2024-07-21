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
  dplyr::filter(!is.na(sent_l)) %>% 
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
  dplyr::filter(agree_stan == 1) %>% 
  mutate(agreement = "Partial agreement")


epfl_df_s <- epfl_df  %>% 
  select(id_tweets, stance_epfl, agreement) %>% 
  write_csv("data/epfl_annotations_simple_partial_agreement.csv")

### Tweets per agreement and class -----------
epfl_df_class_agreement <- epfl_df %>% 
  dplyr::filter(agree_stan == 1 ) %>% 
  group_by(stance_epfl) %>% 
  tally() %>% 
  mutate(percent = n / sum(n) * 100,
         agreement = "Partial agreement")

## Descriptive analysis (full agreement) --------
epfl_df_full <- epfl_df_all %>% 
  dplyr::filter(agree_stance == 1) %>% 
  mutate(agreement = "Full agreement")


epfl_df_full_s <- epfl_df_full  %>% 
  select(id_tweets, stance_epfl, agreement) %>% 
  write_csv("data/epfl_annotations_simple_full_agreement.csv")

### Tweets per agreement and class -----------
epfl_df_class_agreement_full <- epfl_df_full %>% 
  dplyr::filter(agree_stance == 1 ) %>% 
  group_by(stance_epfl) %>% 
  tally() %>% 
  mutate(percent = n / sum(n) * 100,
         agreement = "Full agreement")

# Get vader sentiment -------------
df_vader_raw <- read_csv("data/local/vader_sentiment.csv") %>% 
  mutate(sent_vader = case_when(sentiment_score < -0.5 ~ 'Negative',
                                sentiment_score > 0.5 ~ 'Positive',
                                .default = 'Neutral'))

df_vader <- df_vader_raw  %>% 
  select(text, sent_vader) 

## Summary stats --------
df_vader_raw %>% 
  inner_join(epfl_df['text'], by = 'text') %>% 
  split(.$sent_vader) %>% map(summary)

df_vader_raw %>% 
  inner_join(epfl_df['text'], by = 'text') %>% 
  map(summary)


## Descriptive analysis of vader ----------
### Partial agreement from experts --------
df_vader_partial <- df_vader %>% 
  inner_join(epfl_df['text'], by = 'text') %>% 
  group_by(sent_vader) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100,
         Model = "Vader",
         Prompt = 'None') %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sent_vader",
              id_cols = c("Model" , "Prompt")) %>% 
  mutate(agreement = "Partial agreement")

### Full agreement from experts --------
df_vader_full <- df_vader %>% 
  inner_join(epfl_df_full['text'], by = 'text') %>% 
  group_by(sent_vader) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100,
         Model = "Vader",
         Prompt = 'None') %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sent_vader",
              id_cols = c("Model" , "Prompt")) %>% 
  mutate(agreement = "Full agreement")

### All -------------
df_vader_descriptive <- df_vader_full %>% 
  rbind(df_vader_partial)
  
df_vader_descriptive_fig <- df_vader_descriptive %>% 
  pivot_longer(cols = c(starts_with("ne"), starts_with("pos")),
               names_to = "stance",
               values_to = "percentage") %>% 
  select(Model, Prompt, stance, percentage, agreement)

# Get mturk annotations ---------------
message("Getting Mturk annotations")
setwd("data/local/mturk")
files <- fs::dir_ls(glob = "mturk_batch_job_results-*csv")
df_mturk <- vroom(files)
setwd("../../..")

unique(df_mturk$question_tag)
df_mturk_clean <- df_mturk %>%
  dplyr::filter(question_tag == "vax_sentiment" | question_tag == "sentiment") %>%
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
  select(id_tweets, total, neutral, negative, positive, partial_agree, full_agree, agree_mturk) %>% 
  write_csv("data/mturk_annotations_simple.csv")  

## Dataset for figure on stance distribution ------------------
df_mturk_fig <- epfl_df_all %>% 
  select(text, agree_stan, agree_stance) %>% 
  right_join(df_mturk_annot, by = "text") %>% 
  mutate(agreement = case_when(agree_stan == 1 & agree_stance == 0 ~ "Partial agreement",
                               agree_stan == 1 & agree_stance == 1 ~ "Full agreement",
                               .default = "No agreement")) %>% 
  filter(agreement != "No agreement") %>% 
  group_by(agree_mturk, agreement) %>% 
  tally() %>% 
  mutate(Model = "Amazon Mturk",
         Prompt = "None") %>% 
  rename(stance = agree_mturk) %>% 
  arrange(agreement) %>% 
  ungroup() %>% 
  group_by(agreement) %>% 
  mutate(percentage = (n / sum(n)) * 100,
         stance = case_when(stance == "neutral" ~ "Neutral",
                            stance == "positive" ~ "Positive",
                            stance == "negative" ~ "Negative")) %>% 
  select(Model, Prompt, stance, percentage, agreement)


## Get tweets' dates --------------
mturk_dates <- df_mturk %>%
  select(text, created_at) %>% 
  left_join(df_mturk_annot, by = c("text")) %>% 
  dplyr::filter(!is.na(agree_mturk)) %>% 
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
  mutate(sentiment_gpt = tolower(sentiment_gpt),
         # id_tweets = id_gpt + 1,
         sentiment_gpt = str_match(sentiment_gpt, "\\b(neutral|negative|positive)\\b")[,2],
         sentiment_gpt = case_when(is.na(sentiment_gpt) ~ "neutral",
                                   .default = sentiment_gpt)) %>% 
  distinct(text, prompt, .keep_all = TRUE) %>% 
  mutate(prompt = case_when(prompt == 1 ~ 2,
                            prompt == 0 ~ 1,
                            prompt == 41 ~ 42,
                            prompt == 40 ~ 41,
                            .default = prompt))

# gpt_clean$sentiment_gpt <- str_replace_all(string = gpt_clean$sentiment_gpt, 
#                                            pattern = "\r\n", 
#                                            replacement = "") %>%
#   tolower()

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
                                  c("42" = "GPT 4 prompt 2",
                                    "41" = "GPT 4 prompt 1",
                                    "45" = "GPT 4 prompt 5",
                                    "43" = "GPT 4 prompt 3",
                                    "44" = "GPT 4 prompt 4",
                                    "46" = "GPT 4 prompt 6",
                                    "47" = "GPT 4 prompt 7",
                                    "48" = "GPT 4 prompt 8",
                                    "^2$" = "GPT 3.5 prompt 2",
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
  separate(prompt, c("Model", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_gpt",
              id_cols = c("Model" , "Prompt")) 

gpt_descriptive %>% 
  write_csv("outputs/descriptive_gpt.csv")

gpt_descriptive_agree <- gpt_clean %>% 
  mutate(prompt = str_replace_all(prompt, 
                                  c("42" = "GPT 4 prompt 2",
                                    "41" = "GPT 4 prompt 1",
                                    "45" = "GPT 4 prompt 5",
                                    "43" = "GPT 4 prompt 3",
                                    "44" = "GPT 4 prompt 4",
                                    "46" = "GPT 4 prompt 6",
                                    "47" = "GPT 4 prompt 7",
                                    "48" = "GPT 4 prompt 8",
                                    "^2$" = "GPT 3.5 prompt 2",
                                    "^1$" = "GPT 3.5 prompt 1",
                                    "^3$" = "GPT 3.5 prompt 3",
                                    "^4$" = "GPT 3.5 prompt 4",
                                    "^5$" = "GPT 3.5 prompt 5",
                                    "^6$" = "GPT 3.5 prompt 6",
                                    "^7$" = "GPT 3.5 prompt 7",
                                    "^8$" = "GPT 3.5 prompt 8"))) %>% 
  semi_join(epfl_df_full['text']) %>% 
  group_by(prompt, sentiment_gpt) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  separate(prompt, c("Model", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_gpt",
              id_cols = c("Model" , "Prompt")) 

gpt_descriptive_agree %>% 
  write_csv("outputs/descriptive_gpt_agree.csv")

gpt_descriptive_all <- gpt_descriptive %>% 
  left_join(gpt_descriptive_agree, by = c("Model", "Prompt")) %>% 
  rename("Neutral (partial agreement)" = neutral.x,
         "Positive (partial agreement)" = positive.x,
         "Negative (partial agreement)" = negative.x,
         "Neutral (full agreement)" = neutral.y,
         "Positive (full agreement)" = positive.y,
         "Negative (full agreement)" = negative.y)

gpt_descriptive_all %>% 
  write_csv("outputs/descriptive_gpt_all.csv")

# Get Mixtral data ------------------
message("Getting Mixtral annotations")
setwd("data/local")
files_mixtral <- fs::dir_ls(glob = "mixtral_sentiment_prompt*csv")
mixtral <- vroom(files_mixtral)
setwd("../..")

mixtral_clean <- mixtral %>% 
  select(text, sentiment_mixtral, prompt) %>% 
  mutate(sentiment_mixtral_raw = tolower(sentiment_mixtral),
         prompt = case_when(prompt == 1 ~ 2,
                            prompt == 0 ~ 1,
                            .default = prompt),
         # sentiment_mixtral_raw = str_replace_all(sentiment_mixtral_raw,
         #                                         c("neutural" = "neutral",
         #                                         "neutional" = "neutral")
         #                                         ),
         # get the first instance of the three classes
         sentiment_mixtral = str_match(sentiment_mixtral_raw, 
                                       "\\b(neutral|negative|positive)\\b")[,2])

mixtral_clean_s <- epfl_df %>% 
  select(id_tweets, text) %>% 
  right_join(mixtral_clean, by = "text") %>% 
  select(id_tweets, sentiment_mixtral, prompt) %>% 
  distinct(id_tweets, prompt, .keep_all = TRUE) %>% 
  write_csv("data/mixtral_annotations_simple.csv")

## Descriptive analysis of Mixtral -----------
mixtral_descriptive <- mixtral_clean %>% 
  mutate(prompt = str_replace_all(prompt, 
                                  c("^2$" = "Mistral (8x7B) prompt 2",
                                    "^1$" = "Mistral (8x7B) prompt 1"
                                    ,"^3$" = "Mistral (8x7B) prompt 3"
                                    ,"^4$" = "Mistral (8x7B) prompt 4"
                                    ,"^5$" = "Mistral (8x7B) prompt 5"
                                    ,"^6$" = "Mistral (8x7B) prompt 6"
                                    ,"^7$" = "Mistral (8x7B) prompt 7"
                                    ,"^8$" = "Mistral (8x7B) prompt 8"
                                    ))) %>% 
  group_by(prompt, sentiment_mixtral) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  separate(prompt, c("Model", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_mixtral",
              id_cols = c("Model" , "Prompt")) 

mixtral_descriptive %>% 
  write_csv("outputs/descriptive_mixtral.csv")

mixtral_descriptive_agree <- mixtral_clean %>% 
  mutate(prompt = str_replace_all(prompt, 
                                  c("^2$" = "Mistral (8x7B) prompt 2",
                                    "^1$" = "Mistral (8x7B) prompt 1"
                                    ,"^3$" = "Mistral (8x7B) prompt 3"
                                    ,"^4$" = "Mistral (8x7B) prompt 4"
                                    ,"^5$" = "Mistral (8x7B) prompt 5"
                                    ,"^6$" = "Mistral (8x7B) prompt 6"
                                    ,"^7$" = "Mistral (8x7B) prompt 7"
                                    ,"^8$" = "Mistral (8x7B) prompt 8"
                                    ))) %>% 
  semi_join(epfl_df_full['text']) %>% 
  group_by(prompt, sentiment_mixtral) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  separate(prompt, c("Model", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_mixtral",
              id_cols = c("Model" , "Prompt")) 

mixtral_descriptive_agree %>% 
  write_csv("outputs/descriptive_mixtral_agree.csv")

mixtral_descriptive_all <- mixtral_descriptive %>% 
  left_join(mixtral_descriptive_agree, by = c("Model", "Prompt")) %>% 
  rename("Neutral (partial agreement)" = neutral.x,
         "Positive (partial agreement)" = positive.x,
         "Negative (partial agreement)" = negative.x,
         "Neutral (full agreement)" = neutral.y,
         "Positive (full agreement)" = positive.y,
         "Negative (full agreement)" = negative.y)

mixtral_descriptive_all %>% 
  write_csv("outputs/descriptive_mixtral_all.csv")

# Get Mistral data ------------
message("Getting Mistral annotations")
setwd("data/local")
files_mistral <- fs::dir_ls(glob = "mistral_sentiment_prompt*csv")
mistral <- vroom(files_mistral)
setwd("../..")

mistral_clean <- mistral %>% 
  select(text, sentiment_mistral, prompt) %>% 
  mutate(prompt = case_when(prompt == 1 ~ 2,
                            prompt == 0 ~ 1,
                            .default = prompt),
         sentiment_mistral_raw = tolower(sentiment_mistral),
         sentiment_mistral_raw = str_replace_all(sentiment_mistral_raw,
                                                 c("neutural" = "neutral",
                                                   "neutional" = "neutral")),
         # get the first instance of the three classes
         sentiment_mistral = str_match(sentiment_mistral_raw, 
                                       "\\b(neutral|negative|positive)\\b")[,2])

mistral_clean_s <- epfl_df %>% 
  select(id_tweets, text) %>% 
  right_join(mistral_clean, by = "text") %>% 
  select(id_tweets, sentiment_mistral, prompt) %>% 
  write_csv("data/mistral_annotations_simple.csv")

## Descriptive analysis of Mistral -----------
mistral_descriptive <- mistral_clean %>% 
  mutate(prompt = str_replace_all(prompt, 
                                  c("^2$" = "Mistral (7B) prompt 2",
                                    "^1$" = "Mistral (7B) prompt 1",
                                    "^3$" = "Mistral (7B) prompt 3",
                                    "^4$" = "Mistral (7B) prompt 4",
                                    "^5$" = "Mistral (7B) prompt 5",
                                    "^6$" = "Mistral (7B) prompt 6",
                                    "^7$" = "Mistral (7B) prompt 7",
                                    "^8$" = "Mistral (7B) prompt 8"))) %>% 
  group_by(prompt, sentiment_mistral) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  separate(prompt, c("Model", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_mistral",
              id_cols = c("Model" , "Prompt")) 

mistral_descriptive %>% 
  write_csv("outputs/descriptive_mistral.csv")

mistral_descriptive_agree <- mistral_clean %>% 
  mutate(prompt = str_replace_all(prompt, 
                                  c("^2$" = "Mistral (7B) prompt 2",
                                    "^1$" = "Mistral (7B) prompt 1",
                                    "^3$" = "Mistral (7B) prompt 3",
                                    "^4$" = "Mistral (7B) prompt 4",
                                    "^5$" = "Mistral (7B) prompt 5",
                                    "^6$" = "Mistral (7B) prompt 6",
                                    "^7$" = "Mistral (7B) prompt 7",
                                    "^8$" = "Mistral (7B) prompt 8"))) %>% 
  semi_join(epfl_df_full['text']) %>% 
  group_by(prompt, sentiment_mistral) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  separate(prompt, c("Model", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_mistral",
              id_cols = c("Model" , "Prompt")) 

mistral_descriptive_agree %>% 
  write_csv("outputs/descriptive_mistral_agree.csv")

mistral_descriptive_all <- mistral_descriptive %>% 
  left_join(mistral_descriptive_agree, by = c("Model", "Prompt")) %>% 
  rename("Neutral (partial agreement)" = neutral.x,
         "Positive (partial agreement)" = positive.x,
         "Negative (partial agreement)" = negative.x,
         "Neutral (full agreement)" = neutral.y,
         "Positive (full agreement)" = positive.y,
         "Negative (full agreement)" = negative.y)

mistral_descriptive_all %>% 
  write_csv("outputs/descriptive_mistral_all.csv")

# Get Llama 3 8b data ------------
message("Getting Llama 3 8b annotations")
setwd("data/local")
files_llama <- fs::dir_ls(glob = "llama3_sentiment_prompt*csv")
llama <- vroom(files_llama)
setwd("../..")

llama_clean <- llama %>% 
  rename(sentiment_llama = sentiment_llama3) %>% 
  select(text, sentiment_llama, prompt) %>% 
  mutate(prompt = case_when(prompt == 1 ~ 2,
                            prompt == 0 ~ 1,
                            .default = prompt),
         sentiment_llama_raw = tolower(sentiment_llama),
         sentiment_llama_raw = str_replace_all(sentiment_llama_raw,
                                                 c("neutural" = "neutral",
                                                   "neutional" = "neutral")),
         # get the first instance of the three classes
         sentiment_llama = str_match(sentiment_llama_raw, 
                                       "\\b(neutral|negative|positive)\\b")[,2],
         sentiment_llama = case_when(is.na(sentiment_llama) ~ "neutral",
                                     .default = sentiment_llama))

llama_clean_s <- epfl_df %>% 
  select(id_tweets, text) %>% 
  right_join(llama_clean, by = "text") %>% 
  select(id_tweets, sentiment_llama, prompt) %>% 
  distinct(id_tweets, prompt, .keep_all = TRUE) %>% 
  write_csv("data/llama8b_annotations_simple.csv")

## Descriptive analysis of Llama 3 -----------
llama_descriptive <- llama_clean %>% 
  mutate(prompt = str_replace_all(prompt, 
                                  c("^2$" = "Llama3 (8B) prompt 2",
                                    "^1$" = "Llama3 (8B) prompt 1",
                                    "^3$" = "Llama3 (8B) prompt 3",
                                    "^4$" = "Llama3 (8B) prompt 4",
                                    "^5$" = "Llama3 (8B) prompt 5",
                                    "^6$" = "Llama3 (8B) prompt 6",
                                    "^7$" = "Llama3 (8B) prompt 7",
                                    "^8$" = "Llama3 (8B) prompt 8"))) %>% 
  group_by(prompt, sentiment_llama) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  separate(prompt, c("Model", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_llama",
              id_cols = c("Model" , "Prompt")) 

llama_descriptive %>% 
  write_csv("outputs/descriptive_llama.csv")

llama_descriptive_agree <- llama_clean %>% 
  mutate(prompt = str_replace_all(prompt, 
                                  c("^2$" = "Llama3 (8B) prompt 2",
                                    "^1$" = "Llama3 (8B) prompt 1",
                                    "^3$" = "Llama3 (8B) prompt 3",
                                    "^4$" = "Llama3 (8B) prompt 4",
                                    "^5$" = "Llama3 (8B) prompt 5",
                                    "^6$" = "Llama3 (8B) prompt 6",
                                    "^7$" = "Llama3 (8B) prompt 7",
                                    "^8$" = "Llama3 (8B) prompt 8"))) %>% 
  semi_join(epfl_df_full['text']) %>% 
  group_by(prompt, sentiment_llama) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  separate(prompt, c("Model", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_llama",
              id_cols = c("Model" , "Prompt")) 

llama_descriptive_agree %>% 
  write_csv("outputs/descriptive_llama_agree.csv")

llama_descriptive_all <- llama_descriptive %>% 
  left_join(llama_descriptive_agree, by = c("Model", "Prompt")) %>% 
  rename("Neutral (partial agreement)" = neutral.x,
         "Positive (partial agreement)" = positive.x,
         "Negative (partial agreement)" = negative.x,
         "Neutral (full agreement)" = neutral.y,
         "Positive (full agreement)" = positive.y,
         "Negative (full agreement)" = negative.y)

llama_descriptive_all %>% 
  write_csv("outputs/descriptive_llama_all.csv")

# Get Llama 3 70b data ------------
message("Getting Llama 3 70b annotations")
setwd("data/local")
files_llama70b <- fs::dir_ls(glob = "llama3_70b_sentiment_prompt*csv")
llama70b <- vroom(files_llama70b)
setwd("../..")

llama70b_clean <- llama70b %>% 
  rename(sentiment_llama70b = sentiment_llama3) %>% 
  select(text, sentiment_llama70b, prompt) %>% 
  mutate(prompt = case_when(prompt == 1 ~ 2,
                            prompt == 0 ~ 1,
                            .default = prompt),
         sentiment_llama70b_raw = tolower(sentiment_llama70b),
         sentiment_llama70b_raw = str_replace_all(sentiment_llama70b_raw,
                                               c("neutural" = "neutral",
                                                 "neutional" = "neutral")),
         # get the first instance of the three classes
         sentiment_llama70b = str_match(sentiment_llama70b_raw, 
                                     "\\b(neutral|negative|positive)\\b")[,2],
         sentiment_llama70b = case_when(is.na(sentiment_llama70b) ~ "neutral",
                                     .default = sentiment_llama70b))

llama70b_clean_s <- epfl_df %>% 
  select(id_tweets, text) %>% 
  right_join(llama70b_clean, by = "text") %>% 
  select(id_tweets, sentiment_llama70b, prompt) %>% 
  write_csv("data/llama70b_annotations_simple.csv")

## Descriptive analysis of Llama 3 70b -----------
llama70b_descriptive <- llama70b_clean %>% 
  mutate(prompt = str_replace_all(prompt, 
                                  c("^2$" = "Llama3 (70B) prompt 2",
                                    "^1$" = "Llama3 (70B) prompt 1",
                                    "^3$" = "Llama3 (70B) prompt 3",
                                    "^4$" = "Llama3 (70B) prompt 4",
                                    "^5$" = "Llama3 (70B) prompt 5",
                                    "^6$" = "Llama3 (70B) prompt 6",
                                    "^7$" = "Llama3 (70B) prompt 7",
                                    "^8$" = "Llama3 (70B) prompt 8"))) %>% 
  group_by(prompt, sentiment_llama70b) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  separate(prompt, c("Model", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_llama70b",
              id_cols = c("Model" , "Prompt")) 

llama70b_descriptive %>% 
  write_csv("outputs/descriptive_llama70b.csv")

llama70b_descriptive_agree <- llama70b_clean %>% 
  mutate(prompt = str_replace_all(prompt, 
                                  c("^2$" = "Llama3 (70B) prompt 2",
                                     "^1$" = "Llama3 (70B) prompt 1",
                                     "^3$" = "Llama3 (70B) prompt 3",
                                     "^4$" = "Llama3 (70B) prompt 4",
                                     "^5$" = "Llama3 (70B) prompt 5",
                                     "^6$" = "Llama3 (70B) prompt 6",
                                     "^7$" = "Llama3 (70B) prompt 7",
                                     "^8$" = "Llama3 (70B) prompt 8"))) %>% 
  semi_join(epfl_df_full['text']) %>% 
  group_by(prompt, sentiment_llama70b) %>% 
  tally() %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  separate(prompt, c("Model", 'Prompt'), sep = " prompt ")  %>% 
  pivot_wider(values_from = "percentage",
              names_from = "sentiment_llama70b",
              id_cols = c("Model" , "Prompt")) 

llama70b_descriptive_agree %>% 
  write_csv("outputs/descriptive_llama70b_agree.csv")

llama70b_descriptive_all <- llama70b_descriptive %>% 
  left_join(llama70b_descriptive_agree, by = c("Model", "Prompt")) %>% 
  rename("Neutral (partial agreement)" = neutral.x,
         "Positive (partial agreement)" = positive.x,
         "Negative (partial agreement)" = negative.x,
         "Neutral (full agreement)" = neutral.y,
         "Positive (full agreement)" = positive.y,
         "Negative (full agreement)" = negative.y)

llama70b_descriptive_all %>% 
  write_csv("outputs/descriptive_llama70b_all.csv")


# Merge mistral, mixtral, gpt and llama -----------
mistral_mixtral_gpt_llama_descriptive <- gpt_descriptive %>% 
  rbind(mistral_descriptive) %>%
  rbind(mixtral_descriptive) %>% 
  rbind(llama_descriptive) %>% 
  rbind(llama70b_descriptive) %>% 
  rename("Positive" = "positive",
         "Neutral" = "neutral",
         "Negative" = "negative") %>% 
  arrange(Prompt)

mistral_mixtral_gpt_llama_descriptive %>% 
  write_csv("outputs/descriptive_mistral_mixtral_gpt_llama.csv")

mistral_mixtral_gpt_llama_descriptive_agree <- gpt_descriptive_agree %>% 
  rbind(mistral_descriptive_agree) %>% 
  rbind(mixtral_descriptive_agree) %>% 
  rbind(llama_descriptive_agree) %>% 
  rbind(llama70b_descriptive_agree) %>% 
  rename("Positive" = "positive",
         "Neutral" = "neutral",
         "Negative" = "negative") %>% 
  arrange(Prompt)

mistral_mixtral_gpt_llama_descriptive_agree %>% 
  write_csv("outputs/descriptive_mistral_mixtral_gpt_llama_agree.csv")

mistral_mixtral_gpt_llama_descriptive_all <- mistral_mixtral_gpt_llama_descriptive %>% 
  left_join(mistral_mixtral_gpt_llama_descriptive_agree, by = c("Model", "Prompt")) %>% 
  rename("Neutral (partial agreement)" = Neutral.x,
         "Positive (partial agreement)" = Positive.x,
         "Negative (partial agreement)" = Negative.x,
         "Neutral (full agreement)" = Neutral.y,
         "Positive (full agreement)" = Positive.y,
         "Negative (full agreement)" = Negative.y)

mistral_mixtral_gpt_llama_descriptive_all %>% 
  write_csv("outputs/descriptive_mistral_mixtral_gpt_llama_all.csv")

# Figure stance distribution per method and prompt ----------------
mistral_mixtral_gpt_llama_descriptive_all_fig <- mistral_mixtral_gpt_llama_descriptive %>% 
  left_join(mistral_mixtral_gpt_llama_descriptive_agree, by = c("Model", "Prompt")) %>% 
  pivot_longer(cols = c(starts_with("ne"), starts_with("pos")),
               names_to = "stance",
               values_to = "percentage") %>% 
  mutate(agreement = case_when(str_detect(stance,
                                          "x") ~ "Partial agreement",
                               .default = "Full agreement"),
         stance = str_replace_all(stance, ".x", ""),
         stance = str_replace_all(stance, ".y", ""),
         Prompt = paste0("Prompt ", as.character(Prompt))) %>% 
  rbind(df_mturk_fig) %>% 
  rbind(df_vader_descriptive_fig)

## Data for hline per facet -----------
hline_stance_epfl <- epfl_df_class_agreement %>% 
  rbind(epfl_df_class_agreement_full) %>% 
  rename(y_intercept = percent) %>% 
  arrange(desc(agreement), desc(stance_epfl)) %>% 
  select(agreement, y_intercept) %>% 
  group_by(agreement) %>%
  mutate(y_intercept_cum = cumsum(y_intercept)) %>%
  ungroup()

##Figure ----------
stance_distribution_fig <- mistral_mixtral_gpt_llama_descriptive_all_fig %>% 
  #filter(Model != "Llama3" & Model != "Llama3 70b" ) %>% 
  mutate(Prompt = case_when(Prompt == "Prompt 2" ~ "2",
                            Prompt == "Prompt 1" ~ "1",
                            Prompt == "Prompt 3" ~ "3",
                            Prompt == "Prompt 4" ~ "4",
                            Prompt == "Prompt 5" ~ "5",
                            Prompt == "Prompt 6" ~ "6",
                            Prompt == "Prompt 7" ~ "7",
                            Prompt == "Prompt 8" ~ "8",
                            Prompt == "None" ~ "None"),
         Model = case_when(Model == "Amazon Mturk" ~ "Mturk",
                           .default = Model)) %>% 
  #filter(agreement == "Partial") %>% 
  ggplot() +
  geom_bar(aes(x = Prompt, 
               y = percentage, 
               fill = stance),
           position = "stack",
           stat = "identity") +
  geom_hline(data = hline_stance_epfl,
             aes(yintercept = y_intercept_cum),
             color = "black",
             linewidth = 0.8,
             linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", 
             linewidth = 0.5) +
  scale_y_continuous(expand = c(0,0)) +
  facet_grid(agreement ~ Model
             #, switch = "x"
             , scales = "free_x"
             ) +
  scale_linetype_manual("dashed") +
  theme_classic() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, 
                                        color = "white"),
        panel.spacing.x = unit(0.1,"cm"),
        #axis.text = element_text(size = 14),
        text = element_text(size = 14),
        axis.text.x = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 14),  
        strip.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.spacing.y = unit(1, "lines"),
        axis.text = element_text(color = "black")) +
  labs(y = "Percentage",
       fill = "Stance",
       linetype = "Stance distribution \naccording to experts")

stance_distribution_fig

ggsave("outputs/stance_distribution.jpeg", stance_distribution_fig,
       width=12, height=6)



# Get all datasets -------------
df_all <- df_mturk_annot_clean %>% 
  full_join(epfl_df, by = "text") %>% 
  full_join(gpt_clean, by = "text") %>%
  full_join(mistral_clean, by = c("text", "prompt")) %>% 
  full_join(mixtral_clean, by = c("text", "prompt")) %>%
  full_join(llama_clean, by = c("text", "prompt")) %>% 
  full_join(llama70b_clean, by = c("text", "prompt")) %>% 
  full_join(df_vader, by = "text") %>% 
  mutate(sent_vader = tolower(sent_vader)) %>% 
  dplyr::filter(!is.na(sent_l)) %>% 
  select(-id_tweets.x, -id_tweets.y) # id_tweets comes from gpt_clean (supposedly same as epfl_df)

df_all_available <- df_all %>% 
  group_by(id_tweets, prompt) %>% 
  tally() %>% 
  ungroup()

id_tweets_unique <- unique(df_all_available$id_tweets)
prompts_unique <- unique(df_all_available$prompt) #%>% 
#  filter(!is.na(.))

df_all_complete <- expand.grid(id_tweets = id_tweets_unique, prompt = prompts_unique) %>% 
  dplyr::filter(!is.na(prompt))

missing_entries <- df_all_available %>% 
  select(id_tweets, prompt) %>% 
  setdiff(df_all_complete, .) %>% 
  dplyr::filter(prompt != 0) %>% 
  group_by(prompt) %>% 
  tally()

## To retrieve the real tweet ids -------------
tweets_id_real <- select(df_mturk_annot_clean, 'text') %>% 
  left_join(select(df_mturk, 'tweet_id', "text"), by = "text") %>% 
  dplyr::filter(duplicated(text) == FALSE) %>% 
  left_join(select(df_mturk_annot_clean, 'text', 'id_tweets'),
            by = "text") 

# Merge databases ----------------
df_all_clean <- df_all %>% 
  rename("neutral_mturk" = "neutral",
         "positive_mturk" = "positive",
         "negative_mturk" = "negative") %>% 
  select(-text) %>% 
  dplyr::filter(!is.na(stance_epfl)) %>% 
  left_join(select(tweets_id_real, tweet_id, id_tweets),
            by = "id_tweets") 

df_all_clean %>% 
  write_csv("outputs/all_datasets_for_shiny.csv")

