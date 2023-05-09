# Script information ------------
#' Aim: Use Open AI to annotate tweets on vaccine public perception 
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Import data  -------------
## Mturk annotation --------
tweets_clean_pos_neg <- read_csv("data/local/labelled_tweets_clean.csv") %>% 
  mutate(id_tweets = 1:n()) %>% 
  filter(label_tag != "neutral") 

## GPT 3.5  annotations ---------
gpt_annot_pos_neg <- readxl::read_excel("data/local/gpt_sentiment_pos_neg.xlsx") %>% 
  filter(file == "old") %>% 
  select(id_gpt_two, sentiment_gpt_two) %>% 
  mutate(id_tweets = id_gpt_two + 1,
         sentiment = tolower(sentiment_gpt_two),
         sentiment = case_when(str_detect(sentiment_gpt_two, 'positive') ~ "positive",
                                .default = "negative"))  

# gpt_annot_pos_neg$sentiment_gpt_two <- str_replace_all(string = gpt_annot_pos_neg$sentiment_gpt_two, 
#                   pattern = "\r\n", 
#                   replacement = "") %>%
#   tolower()



## Metadata ------------
tweets_2017 <- read_csv("data/local/df_2017_vaccine_EN_annotated.csv") 
tweets_2018 <- read_csv("data/local/df_2018_vaccine_EN_annotated.csv") 
tweets_2019 <- read_csv("data/local/df_2019_vaccine_EN_annotated.csv") 
tweets_2020_1 <- read_csv("data/local/df_2020_1_vaccine_EN_annotated.csv") 
tweets_2020_2 <- read_csv("data/local/df_2020_2_vaccine_EN_annotated.csv") 

tweets_all <- bind_rows(tweets_2017, tweets_2018, tweets_2019, 
                        tweets_2020_1, tweets_2020_2) %>% 
  distinct(text, .keep_all = TRUE)

# Merge all data -------------
df_two <- tweets_clean_pos_neg %>% 
  full_join(gpt_annot_pos_neg, by = "id_tweets") %>% 
  mutate(comp = case_when(sentiment == label_tag ~ "same",
                          .default = "different"),
         comp_num = case_when(sentiment == label_tag ~ 0,
                              .default = 1)) %>% 
  left_join(tweets_all, by = "text") %>% 
  select(-id.x, -id.y, -`...1`) %>% 
  filter(!is.na(sentiment))

plot_missing(df, title = "Missing values",
             group = list("Remove" = 1, "Bad" = 0.8, "Good" = 0),
             missing_only = FALSE, ggtheme = theme_classic())

# Descriptive analysis ------------
## Total number of manual annotations per sentiment ------
total_neg_two <- df_two %>% 
  filter(label_tag == "negative") %>% 
  select(label_tag) %>% 
  tally() %>% 
  as.numeric()

total_pos_two <- df_two %>% 
  filter(label_tag == "positive") %>% 
  select(label_tag) %>% 
  tally() %>% 
  as.numeric()


# Compare annotations --------------
df_sentiment_two <- df_two %>% 
  filter(!is.na(id_gpt_two)) %>% 
  group_by(label_tag, sentiment, comp) %>% 
  tally() %>% 
  mutate(percent = case_when(label_tag == "negative" ~ round(n/total_neg*100, digits = 1),
                             label_tag == "positive" ~ round(n/total_pos*100, digits = 2)))
  

df_sentiment_two_table <- df_sentiment_two %>% 
  flextable() %>% 
  flextable::set_header_labels(label_tag = 'Manual annotation',
                               sentiment_gpt_two = 'GPT classification',
                               comp = 'Comparison',
                               n = 'Total number of tweets',
                               percent = "Percentage (%) of manual annotations") %>% 
  flextable::autofit() %>% 
  flextable::theme_zebra() %>% 
  flextable::merge_v(j = 1) %>% 
  flextable::align(j = 4, align = "center") %>% 
  flextable::bg(part = "header", bg = "#65B32E")  
  
  

df_sentiment_two_table

## next step: compare positive/negative only and provide two options to GPT

# Multinomial logistic regression -------------
model_null <- df %>% 
  mutate(comp_num = case_when(comp == "same" ~ as.numeric(0),
                              comp == "different" ~ as.numeric(1),
                              .default = NA_integer_)) %>% 
  glm(comp_num ~ 1, data = ., 
      family = binomial(link = "logit"))

model_alt <- df %>% 
  mutate(comp_num = case_when(comp == "same" ~ as.numeric(0),
                              comp == "different" ~ as.numeric(1),
                              .default = NA_integer_)) %>% 
  glm(comp_num ~ label_tag + sentiment_gpt + in_reply_to_status_id + 
        in_reply_to_user_id + quoted_status_id + quoted_user_id +retweeted_status_id +
        retweeted_user_id, data = ., 
      family = binomial(link = "logit"))


model_glm <- df %>% 
  mutate(comp_num = case_when(comp == "same" ~ as.numeric(0),
                              comp == "different" ~ as.numeric(1),
                              .default = NA_integer_)) #%>% 

model_glm2 <- glm(comp_num ~ label_tag + sentiment_gpt,
      family = binomial(link = "logit"), data = df)

model_glm2
