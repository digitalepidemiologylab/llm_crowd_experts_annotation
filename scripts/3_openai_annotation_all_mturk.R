# Script information ------------
#' Aim: Use Open AI to annotate tweets on vaccine public perception 
#' Author: Laura Espinosa
#' Date created: 18 March 2023
#' Date updated: 18 March 2023

# Import data  -------------
## Mturk annotation --------
tweets_clean_agg <- read_csv("data/local/labelled_tweets_clean_all_mturk_agg.csv") %>% 
  mutate(id_tweets = 1:n()) 


## GPT 3.5  annotations ---------
gpt_clean <- read_csv("data/local/gpt_sentiment_mturk.csv") 

## Metadata ------------
# tweets_2017 <- read_csv("data/local/df_2017_vaccine_EN_annotated.csv") 
# tweets_2018 <- read_csv("data/local/df_2018_vaccine_EN_annotated.csv") 
# tweets_2019 <- read_csv("data/local/df_2019_vaccine_EN_annotated.csv") 
# tweets_2020_1 <- read_csv("data/local/df_2020_1_vaccine_EN_annotated.csv") 
# tweets_2020_2 <- read_csv("data/local/df_2020_2_vaccine_EN_annotated.csv") 
# 
# tweets_all <- bind_rows(tweets_2017, tweets_2018, tweets_2019, 
#                         tweets_2020_1, tweets_2020_2) %>% 
#   distinct(text, .keep_all = TRUE)

# Merge all data -------------
df <- tweets_clean_agg %>% 
  full_join(gpt_clean, by = "id_tweets") %>% 
  mutate(comp = case_when(agree_gpt == agree_mturk ~ "same",
                          .default = "different")) #%>% 
  # left_join(tweets_all, by = "text") %>% 
  # select(-id.x, -id.y, -`...1`) %>% 
  # filter(!is.na(sentiment_gpt))

plot_missing(df, title = "Missing values",
             group = list("Remove" = 1, "Bad" = 0.8, "Good" = 0),
             missing_only = FALSE, ggtheme = theme_classic())

# Descriptive analysis ------------
## Total number of manual annotations per sentiment ------
total_neg <- df %>% 
  filter(agree_mturk == "negative") %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_neut <- df %>% 
  filter(agree_mturk == "neutral") %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()

total_pos <- df %>% 
  filter(agree_mturk == "positive") %>% 
  select(agree_mturk) %>% 
  tally() %>% 
  as.numeric()


# Compare annotations --------------
df_sentiment_gen <- df %>% 
  filter(!is.na(id_gpt)) %>% 
  group_by(agree_mturk, agree_gpt, comp) %>% 
  tally() %>% 
  mutate(percent = case_when(agree_mturk == "negative" ~ round(n/total_neg*100, digits = 1),
                             agree_mturk == "neutral" ~ round(n/total_neut*100, digits = 1),
                             agree_mturk == "positive" ~ round(n/total_pos*100, digits = 1)))
  

df_sentiment_table <- df_sentiment %>% 
  flextable() %>% 
  flextable::set_header_labels(total.x = 'Total manual annotations',
                               agree_mturk = 'Manual annotation',
                               agree_gpt = 'GPT classification',
                               comp = 'Comparison',
                               n = 'Total number of tweets',
                               percent = "Percentage (%) of manual annotations") %>% 
  flextable::autofit() %>% 
  flextable::theme_zebra() %>% 
  flextable::merge_v(j = 1) %>% 
  flextable::align(j = 4, align = "center") %>% 
  flextable::bg(part = "header", bg = "#65B32E") %>% 
  hline(part="all")
  
  

df_sentiment_table

df_sentiment_reactable <- reactable(df_sentiment,
                                    filterable = TRUE,
                                    groupBy = c("total.x", "agree_mturk"),
                                    columns = list(total.x = colDef('Total manual annotations'),
                                                   agree_mturk = colDef('Manual annotation'),
                                                   agree_gpt = colDef('GPT classification'),
                                                   comp = colDef('Comparison'),
                                                   n = colDef('Total number of tweets',
                                                              align = "center",
                                                              format = colFormat(separators = TRUE)),
                                                   percent = colDef("Percentage (%) of manual annotations",
                                                                    align = "center")),
                                    theme = reactableTheme(headerStyle = list(backgroundColor = "#65B32E",
                                                                              color = "white")))

df_sentiment_reactable

df_sentiment_reactable_gen <- reactable(df_sentiment_gen,
                                        filterable =  TRUE,
                                        groupBy = "agree_mturk",
                                        columns = list(agree_mturk = colDef('Manual annotation'),
                                                       agree_gpt = colDef('GPT classification'),
                                                       comp = colDef('Comparison'),
                                                       n = colDef('Total number of tweets',
                                                                  align = "center",
                                                                  format = colFormat(separators = TRUE)),
                                                       percent = colDef("Percentage (%) of manual annotations",
                                                                        align = "center")),
                                        theme = reactableTheme(headerStyle = list(backgroundColor = "#65B32E",
                                                                                  color = "white")))

df_sentiment_reactable_gen

## Confusion matrix --------------
con_matrix <- caret::confusionMatrix(data = as.factor(df$sentiment_gpt), 
                                     reference = as.factor(df$label_tag),
                                     positive = "positive",
                                     mode = "everything")
print(con_matrix)

# Multinomial logistic regression -------------
# model_null <- df %>% 
#   mutate(comp_num = case_when(comp == "same" ~ as.numeric(0),
#                               comp == "different" ~ as.numeric(1),
#                               .default = NA_integer_)) %>% 
#   glm(comp_num ~ 1, data = ., 
#       family = binomial(link = "logit"))
# 
# model_alt <- df %>% 
#   mutate(comp_num = case_when(comp == "same" ~ as.numeric(0),
#                               comp == "different" ~ as.numeric(1),
#                               .default = NA_integer_)) %>% 
#   glm(comp_num ~ label_tag + sentiment_gpt + in_reply_to_status_id + 
#         in_reply_to_user_id + quoted_status_id + quoted_user_id +retweeted_status_id +
#         retweeted_user_id, data = ., 
#       family = binomial(link = "logit"))
# 
# 
# model_glm <- df %>% 
#   mutate(comp_num = case_when(comp == "same" ~ as.numeric(0),
#                               comp == "different" ~ as.numeric(1),
#                               .default = NA_integer_)) #%>% 
# 
# model_glm2 <- glm(comp_num ~ label_tag + sentiment_gpt,
#       family = binomial(link = "logit"), data = df)
# 
# model_glm2
