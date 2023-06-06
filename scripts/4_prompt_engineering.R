# Script information ------------
#' Aim: Descriptive analysis of annotated English tweets on vaccine public perception 
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Mturk --------------
tweets_clean_agg <- read_csv("data/local/labelled_tweets_clean_all_mturk_agg.csv") %>% 
  mutate(id_tweets = 1:n()) 

tweets_sample <- read_csv("data/local/labelled_tweets_clean_all_mturk_agg_sample1000.csv") %>% 
  mutate(id_sample = 1:n()) 

## Stratified proportional sample -------------
categories <- tweets_clean_agg %>% 
  group_by(agree_mturk) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(percent = n/nrow(tweets_clean_agg))

neg1000 <- tweets_clean_agg %>% 
  filter(agree_mturk == "negative") %>% 
  sample_n(round(categories$percent[1]/100 * 1000, digits = 0))

neu1000 <- tweets_clean_agg %>% 
  filter(agree_mturk == "neutral") %>% 
  sample_n(round(categories$percent[2]/100 * 1000, digits = 0))

pos1000 <- tweets_clean_agg %>% 
  filter(agree_mturk == "positive") %>% 
  sample_n(round(categories$percent[3]/100 * 1000, digits = 0))

tweets_clean_agg_1000_prop <- rbind(neg1000, neu1000, pos1000)

write_csv(tweets_clean_agg_1000_prop, "data/local/labelled_tweets_clean_all_mturk_agg_sample1000_proportional.csv")

# GPT -----------
## Clean data -----------------
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

df_sample <- gpt_clean %>% 
  left_join(tweets_clean_agg, by = c("id_tweets" )) %>% 
  mutate(comp = case_when(sentiment_gpt == agree_mturk ~ "same",
                          .default = "different")) %>% 
  arrange(comp)

write_csv(df_sample, "data/local/prompt_explanation.csv")

## Get GPT annotations for 1000 proportionate sample -------------
df_prop <- tweets_clean_agg_1000_prop %>% 
  full_join(df_sample[, c("id_tweets", "sentiment_gpt", "prompt", "explanation")], 
            by = "id_tweets")

df_prop_available <- df_prop %>% 
  filter(prompt != 5)

df_prop_missing <- tweets_clean_agg_1000_prop %>% 
  filter(!text %in% df_prop_available$text)

write_csv(df_prop_missing, "data/local/labelled_tweets_clean_all_mturk_agg_sample1000_proportional_missing.csv")
  
  
# Descriptive analysis ------------
## Total number of manual annotations per sentiment ------
total_neg0 <- df_sample %>% 
  filter(agree_mturk == "negative" & prompt == 0) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neut0 <- df_sample %>% 
  filter(agree_mturk == "neutral" & prompt == 0) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_pos0 <- df_sample %>% 
  filter(agree_mturk == "positive" & prompt == 0) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neg1 <- df_sample %>% 
  filter(agree_mturk == "negative" & prompt == 1) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neut1 <- df_sample %>% 
  filter(agree_mturk == "neutral" & prompt == 1) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_pos1 <- df_sample %>% 
  filter(agree_mturk == "positive" & prompt == 1) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neg2 <- df_sample %>% 
  filter(agree_mturk == "negative" & prompt == 2) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neut2 <- df_sample %>% 
  filter(agree_mturk == "neutral" & prompt == 2) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_pos2 <- df_sample %>% 
  filter(agree_mturk == "positive" & prompt == 2) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neg3 <- df_sample %>% 
  filter(agree_mturk == "negative" & prompt == 3) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neut3 <- df_sample %>% 
  filter(agree_mturk == "neutral" & prompt == 3) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_pos3 <- df_sample %>% 
  filter(agree_mturk == "positive" & prompt == 3) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neg4 <- df_sample %>% 
  filter(agree_mturk == "negative" & prompt == 4) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neut4 <- df_sample %>% 
  filter(agree_mturk == "neutral" & prompt == 4) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_pos4 <- df_sample %>% 
  filter(agree_mturk == "positive" & prompt == 4) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neg5 <- df_sample %>% 
  filter(agree_mturk == "negative" & prompt == 5) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neut5 <- df_sample %>% 
  filter(agree_mturk == "neutral" & prompt == 5) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_pos5 <- df_sample %>% 
  filter(agree_mturk == "positive" & prompt == 5) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neg6 <- df_sample %>% 
  filter(agree_mturk == "negative" & prompt == 6) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neut6 <- df_sample %>% 
  filter(agree_mturk == "neutral" & prompt == 6) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_pos6 <- df_sample %>% 
  filter(agree_mturk == "positive" & prompt == 6) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neg7 <- df_sample %>% 
  filter(agree_mturk == "negative" & prompt == 7) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neut7 <- df_sample %>% 
  filter(agree_mturk == "neutral" & prompt == 7) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_pos7 <- df_sample %>% 
  filter(agree_mturk == "positive" & prompt == 7) %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

# Compare annotations --------------
df_sentiment_sample <- df_sample %>% 
  filter(!is.na(id_gpt)) %>% 
  group_by(prompt, agree_mturk, sentiment_gpt, comp) %>% 
  tally() %>% 
  mutate(percent = case_when(agree_mturk == "negative" & prompt == 0 ~ round(n/total_neg0*100, digits = 1),
                             agree_mturk == "neutral" & prompt == 0 ~ round(n/total_neut0*100, digits = 1),
                             agree_mturk == "positive" & prompt == 0 ~ round(n/total_pos0*100, digits = 1),
                             agree_mturk == "negative" & prompt == 1 ~ round(n/total_neg1*100, digits = 1),
                             agree_mturk == "neutral" & prompt == 1 ~ round(n/total_neut1*100, digits = 1),
                             agree_mturk == "positive" & prompt == 1 ~ round(n/total_pos1*100, digits = 1),
                             agree_mturk == "negative" & prompt == 2 ~ round(n/total_neg2*100, digits = 1),
                             agree_mturk == "neutral" & prompt == 2 ~ round(n/total_neut2*100, digits = 1),
                             agree_mturk == "positive" & prompt == 2 ~ round(n/total_pos2*100, digits = 1),
                             agree_mturk == "negative" & prompt == 3 ~ round(n/total_neg3*100, digits = 1),
                             agree_mturk == "neutral" & prompt == 3 ~ round(n/total_neut3*100, digits = 1),
                             agree_mturk == "positive" & prompt == 3 ~ round(n/total_pos3*100, digits = 1),
                             agree_mturk == "negative" & prompt == 4 ~ round(n/total_neg4*100, digits = 1),
                             agree_mturk == "neutral" & prompt == 4 ~ round(n/total_neut4*100, digits = 1),
                             agree_mturk == "positive" & prompt == 4 ~ round(n/total_pos4*100, digits = 1),
                             agree_mturk == "negative" & prompt == 5 ~ round(n/total_neg5*100, digits = 1),
                             agree_mturk == "neutral" & prompt == 5 ~ round(n/total_neut5*100, digits = 1),
                             agree_mturk == "positive" & prompt == 5 ~ round(n/total_pos5*100, digits = 1),
                             agree_mturk == "negative" & prompt == 6 ~ round(n/total_neg6*100, digits = 1),
                             agree_mturk == "neutral" & prompt == 6 ~ round(n/total_neut6*100, digits = 1),
                             agree_mturk == "positive" & prompt == 6 ~ round(n/total_pos6*100, digits = 1),
                             agree_mturk == "negative" & prompt == 7 ~ round(n/total_neg7*100, digits = 1),
                             agree_mturk == "neutral" & prompt == 7 ~ round(n/total_neut7*100, digits = 1),
                             agree_mturk == "positive" & prompt == 7 ~ round(n/total_pos7*100, digits = 1)))


df_sentiment_sample_table <- df_sentiment_sample %>% 
  flextable() %>% 
  flextable::set_header_labels(prompt = 'Version of prompt',
                               agree_mturk = 'Manual annotation',
                               sentiment_gpt = 'GPT classification',
                               comp = 'Comparison',
                               n = 'Total number of tweets',
                               percent = "Percentage (%) of manual annotations") %>% 
  flextable::autofit() %>% 
  flextable::theme_zebra() %>% 
  # flextable::color(i = ~ "Comparison" = "same", 
  #                  j = "Percentage (%) of manual annotations",
  #                  color = "red") %>% 
  flextable::merge_v(j = c(1,2)) %>% 
  flextable::align(j = 4, align = "center") %>% 
  flextable::bg(part = "header", bg = "#65B32E") %>% 
  hline(part="all")

print(df_sentiment_sample_table)

