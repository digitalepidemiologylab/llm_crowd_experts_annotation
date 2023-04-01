# Script information ------------
#' Aim: Use Open AI to annotate tweets on vaccine public perception 
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Get annotations  -------------
## From Mturk --------
tweets_clean <- read_csv("data/local/labelled_tweets_clean.csv") %>% 
  mutate(id_tweets = 1:n())

## From GPT 3.5  ---------
gpt_annot <- readxl::read_excel("data/local/gpt_sentiment_total.xlsx") %>% 
  select(id_gpt, sentiment_gpt) %>% 
  mutate(id_gpt_corrected = id_gpt + 1)  

gpt_annot$sentiment_gpt <- str_replace_all(string = gpt_annot$sentiment_gpt, 
                  pattern = "\r\n", 
                  replacement = "") %>% 
  tolower()

# Merge annotations -------------
annotations <- tweets_clean %>% 
  full_join(gpt_annot, by = c("id_tweets" = "id_gpt_corrected")) %>% 
  mutate(comp = case_when(sentiment_gpt == label_tag ~ "Same",
                   .default = "Different"))

# Compare annotations --------------
comparison <- annotations %>% 
  filter(!is.na(id_gpt)) %>% 
  group_by(label_tag, sentiment_gpt, comp) %>% 
  tally()
