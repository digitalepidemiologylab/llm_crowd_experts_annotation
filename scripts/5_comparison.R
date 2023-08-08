# Script information ------------
#' Aim: Comparison of all annotations  
#' Author: Laura Espinosa
#' Date created: 25 July 2023
#' Date updated: 25 July 2023


# Compare EPFL and GPT -------------
epfl_df_clean <- df_sample %>% 
  select(id_tweets, text) %>% 
  right_join(epfl_df, by = "text") %>% 
  filter(!is.na(sent_c)) %>% 
  distinct(text, .keep_all = TRUE) %>% 
  rename(id_doc = id_tweet) %>% 
  arrange(id_tweets) %>% 
  mutate(id_tweets = round(id_tweets, digits = 0))

write_csv(epfl_df_clean, "./data/local/epfl_1000_clean_data.csv")

df_epfl_gpt <- epfl_df_clean %>% 
  left_join(gpt_clean, by = "id_tweets") %>%
  rowwise() %>% 
  mutate(epfl_sent = names(which.max(with(na.omit(vec_count(c_across(starts_with("sent_")), "location")), setNames(count, key)))),
         epfl_stance = names(which.max(with(na.omit(vec_count(c_across(starts_with("stance_")), "location")), setNames(count, key)))),
         comp_epfl = case_when(epfl_stance == sentiment_gpt ~ "same",
                               .default = "different")) %>% 
  select(id_tweets, agree_sent, epfl_sent, agree_stance, epfl_stance, sentiment_gpt, prompt, comp_epfl)



# Descriptive analysis ------------
## Total number of manual annotations per sentiment ------
total_epfl_neg0 <- df_epfl_gpt %>% 
  filter(epfl_stance == "negative" & prompt == 0) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neut0 <- df_epfl_gpt %>% 
  filter(epfl_stance == "neutral" & prompt == 0) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_pos0 <- df_epfl_gpt %>% 
  filter(epfl_stance == "positive" & prompt == 0) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neg1 <- df_epfl_gpt %>% 
  filter(epfl_stance == "negative" & prompt == 1) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neut1 <- df_epfl_gpt %>% 
  filter(epfl_stance == "neutral" & prompt == 1) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_pos1 <- df_epfl_gpt %>% 
  filter(epfl_stance == "positive" & prompt == 1) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neg2 <- df_epfl_gpt %>% 
  filter(epfl_stance == "negative" & prompt == 2) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neut2 <- df_epfl_gpt %>% 
  filter(epfl_stance == "neutral" & prompt == 2) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_pos2 <- df_epfl_gpt %>% 
  filter(epfl_stance == "positive" & prompt == 2) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neg3 <- df_epfl_gpt %>% 
  filter(epfl_stance == "negative" & prompt == 3) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neut3 <- df_epfl_gpt %>% 
  filter(epfl_stance == "neutral" & prompt == 3) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_pos3 <- df_epfl_gpt %>% 
  filter(epfl_stance == "positive" & prompt == 3) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neg4 <- df_epfl_gpt %>% 
  filter(epfl_stance == "negative" & prompt == 4) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neut4 <- df_epfl_gpt %>% 
  filter(epfl_stance == "neutral" & prompt == 4) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_pos4 <- df_epfl_gpt %>% 
  filter(epfl_stance == "positive" & prompt == 4) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neg5 <- df_epfl_gpt %>% 
  filter(epfl_stance == "negative" & prompt == 5) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neut5 <- df_epfl_gpt %>% 
  filter(epfl_stance == "neutral" & prompt == 5) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_pos5 <- df_epfl_gpt %>% 
  filter(epfl_stance == "positive" & prompt == 5) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neg6 <- df_epfl_gpt %>% 
  filter(epfl_stance == "negative" & prompt == 6) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neut6 <- df_epfl_gpt %>% 
  filter(epfl_stance == "neutral" & prompt == 6) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_pos6 <- df_epfl_gpt %>% 
  filter(epfl_stance == "positive" & prompt == 6) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neg7 <- df_epfl_gpt %>% 
  filter(epfl_stance == "negative" & prompt == 7) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neut7 <- df_epfl_gpt %>% 
  filter(epfl_stance == "neutral" & prompt == 7) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_pos7 <- df_epfl_gpt %>% 
  filter(epfl_stance == "positive" & prompt == 7) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neg8 <- df_epfl_gpt %>% 
  filter(epfl_stance == "negative" & prompt == 8) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neut8 <- df_epfl_gpt %>% 
  filter(epfl_stance == "neutral" & prompt == 8) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_pos8 <- df_epfl_gpt %>% 
  filter(epfl_stance == "positive" & prompt == 8) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neg8_35 <- df_epfl_gpt %>% 
  filter(epfl_stance == "negative" & prompt == "835") %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_neut8_35 <- df_epfl_gpt %>% 
  filter(epfl_stance == "neutral" & prompt == 835) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

total_epfl_pos8_35 <- df_epfl_gpt %>% 
  filter(epfl_stance == "positive" & prompt == 835) %>% 
  select(epfl_stance) %>% 
  tally() %>% 
  count() %>% 
  as.numeric()

df_epfl_gpt_comp <- df_epfl_gpt %>% 
  group_by(prompt, epfl_stance, comp_epfl) %>% 
  tally() %>% 
  mutate(percent = case_when(epfl_stance == "negative" & prompt == 0 ~ round(n/total_epfl_neg0*100, digits = 1),
                             epfl_stance == "neutral" & prompt == 0 ~ round(n/total_epfl_neut0*100, digits = 1),
                             epfl_stance == "positive" & prompt == 0 ~ round(n/total_epfl_pos0*100, digits = 1),
                             epfl_stance == "negative" & prompt == 1 ~ round(n/total_epfl_neg1*100, digits = 1),
                             epfl_stance == "neutral" & prompt == 1 ~ round(n/total_epfl_neut1*100, digits = 1),
                             epfl_stance == "positive" & prompt == 1 ~ round(n/total_epfl_pos1*100, digits = 1),
                             epfl_stance == "negative" & prompt == 2 ~ round(n/total_epfl_neg2*100, digits = 1),
                             epfl_stance == "neutral" & prompt == 2 ~ round(n/total_epfl_neut2*100, digits = 1),
                             epfl_stance == "positive" & prompt == 2 ~ round(n/total_epfl_pos2*100, digits = 1),
                             epfl_stance == "negative" & prompt == 3 ~ round(n/total_epfl_neg3*100, digits = 1),
                             epfl_stance == "neutral" & prompt == 3 ~ round(n/total_epfl_neut3*100, digits = 1),
                             epfl_stance == "positive" & prompt == 3 ~ round(n/total_epfl_pos3*100, digits = 1),
                             epfl_stance == "negative" & prompt == 4 ~ round(n/total_epfl_neg4*100, digits = 1),
                             epfl_stance == "neutral" & prompt == 4 ~ round(n/total_epfl_neut4*100, digits = 1),
                             epfl_stance == "positive" & prompt == 4 ~ round(n/total_epfl_pos4*100, digits = 1),
                             epfl_stance == "negative" & prompt == 5 ~ round(n/total_epfl_neg5*100, digits = 1),
                             epfl_stance == "neutral" & prompt == 5 ~ round(n/total_epfl_neut5*100, digits = 1),
                             epfl_stance == "positive" & prompt == 5 ~ round(n/total_epfl_pos5*100, digits = 1),
                             epfl_stance == "negative" & prompt == 6 ~ round(n/total_epfl_neg6*100, digits = 1),
                             epfl_stance == "neutral" & prompt == 6 ~ round(n/total_epfl_neut6*100, digits = 1),
                             epfl_stance == "positive" & prompt == 6 ~ round(n/total_epfl_pos6*100, digits = 1),
                             epfl_stance == "negative" & prompt == 7 ~ round(n/total_epfl_neg7*100, digits = 1),
                             epfl_stance == "neutral" & prompt == 7 ~ round(n/total_epfl_neut7*100, digits = 1),
                             epfl_stance == "positive" & prompt == 7 ~ round(n/total_epfl_pos7*100, digits = 1),
                             epfl_stance == "negative" & prompt == 8 ~ round(n/total_epfl_neg8*100, digits = 1),
                             epfl_stance == "neutral" & prompt == 8 ~ round(n/total_epfl_neut8*100, digits = 1),
                             epfl_stance == "positive" & prompt == 8 ~ round(n/total_epfl_pos8*100, digits = 1),
                             epfl_stance == "negative" & prompt == 835 ~ round(n/total_epfl_neg8_35*100, digits = 1),
                             epfl_stance == "neutral" & prompt == 835 ~ round(n/total_epfl_neut8_35*100, digits = 1),
                             epfl_stance == "positive" & prompt == 835 ~ round(n/total_epfl_pos8_35*100, digits = 1)))  


df_epfl_gpt_comp_table <- df_epfl_gpt_comp %>% 
  flextable() %>% 
  flextable::set_header_labels(prompt = 'Version of prompt',
                               epfl_stance = 'Manual annotation',
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

print(df_epfl_gpt_comp_table)

# Comparison EPFL and mturk --------------
df_epfl_mturk <- df %>% 
  select(id_tweets, label_tag) %>% 
  right_join(df_epfl_gpt, by = "id_tweets") %>% 
  distinct(id_tweets, .keep_all = TRUE) %>% 
  mutate(comp = case_when(label_tag == epfl_stance ~ "same",
                          .default = "different")) %>% 
  filter(!is.na(label_tag)) %>% 
  select(id_tweets, label_tag, epfl_stance, comp)

df_epfl_mturk_tab <- df_epfl_mturk %>% 
  group_by(epfl_stance, label_tag, comp) %>% 
  tally() %>% 
  ungroup() %>% 
  flextable() %>% 
  flextable::set_header_labels(epfl_stance = 'EPFL agreed stance',
                               label_tag = 'Mturk agreed sentiment',
                               comp = 'Comparison',
                               n = 'Total number of tweets') %>% 
  flextable::autofit() %>% 
  flextable::theme_zebra() %>% 
  # flextable::color(i = ~ "Comparison" = "same", 
  #                  j = "Percentage (%) of manual annotations",
  #                  color = "red") %>% 
  flextable::merge_v(j = c(1,2)) %>% 
  flextable::align(j = 4, align = "center") %>% 
  flextable::bg(part = "header", bg = "#65B32E") %>% 
  hline(part="all")  

print(df_epfl_mturk_tab)


# Top words ------------
## Same EPFL and GPT ----------
df_nlp_same <- epfl_df_clean %>% 
  left_join(gpt_clean, by = "id_tweets") %>%
  filter(prompt == 5) %>% 
  rowwise() %>% 
  mutate(epfl_sent = names(which.max(with(na.omit(vec_count(c_across(starts_with("sent_")), "location")), setNames(count, key)))),
         epfl_stance = names(which.max(with(na.omit(vec_count(c_across(starts_with("stance_")), "location")), setNames(count, key)))),
         comp_epfl = case_when(epfl_stance == sentiment_gpt ~ "same",
                               .default = "different")) %>% 
  select(id_tweets, text, agree_sent, epfl_sent, agree_stance, epfl_stance, sentiment_gpt, prompt, comp_epfl) %>% 
  as.data.frame() %>% 
  filter(comp_epfl == "same")

df_nlp_same_clean <- Corpus(VectorSource(df_nlp_same$text)) %>% 
  tm_map(., removePunctuation) %>% 
  tm_map(., content_transformer(tolower)) %>% 
  tm_map(., removeNumbers) %>% 
  tm_map(., stripWhitespace) %>% 
  tm_map(., removeWords, stopwords("english"))

wordcloud(df_nlp_same_clean, scale = c(2,1),
          min.freq = 30)

df_nlp_same_list <- df_nlp_same %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  mutate(pos = 1:nrow(.)) %>% 
  rename("word_same" = "word",
         "n_same" = "n")

## Different EPFL and GPT ----------
df_nlp_diff <- epfl_df_clean %>% 
  left_join(gpt_clean, by = "id_tweets") %>%
  filter(prompt == 5) %>% 
  rowwise() %>% 
  mutate(epfl_sent = names(which.max(with(na.omit(vec_count(c_across(starts_with("sent_")), "location")), setNames(count, key)))),
         epfl_stance = names(which.max(with(na.omit(vec_count(c_across(starts_with("stance_")), "location")), setNames(count, key)))),
         comp_epfl = case_when(epfl_stance == sentiment_gpt ~ "same",
                               .default = "different")) %>% 
  select(id_tweets, text, agree_sent, epfl_sent, agree_stance, epfl_stance, sentiment_gpt, prompt, comp_epfl) %>% 
  as.data.frame() %>% 
  filter(comp_epfl == "different")

df_nlp_diff_clean <- Corpus(VectorSource(df_nlp_diff$text)) %>% 
  tm_map(., removePunctuation) %>% 
  tm_map(., content_transformer(tolower)) %>% 
  tm_map(., removeNumbers) %>% 
  tm_map(., stripWhitespace) %>% 
  tm_map(., removeWords, stopwords("english"))

wordcloud(df_nlp_diff_clean, scale = c(2,1),
          min.freq = 30)

df_nlp_diff_list <- df_nlp_diff %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  mutate(pos = 1:nrow(.)) %>% 
  rename("word_diff" = "word",
         "n_diff" = "n")

## Compare both -----------
df_nlp_all_list <- df_nlp_diff_list %>% 
  left_join(df_nlp_same_list, by = "pos") %>% 
  select(pos, word_diff, n_diff, word_same, n_same)

# Multinomial logistic regression ------------------------------

df_log_epfl <- df_epfl_gpt %>% 
  filter(prompt != 0 & prompt != 2) %>% 
  mutate(comp_epfl = as.factor(comp_epfl),
         comp_epfl = relevel(comp_epfl, ref = "same")) #%>% 
  #select(-explanation, -text)


onehot_enc_epfl <- dummyVars(" ~ .", data=df_log_epfl)
df_log_enc_epfl <- data.frame(predict(onehot_enc_epfl, newdata=df_log_epfl)) %>% 
  select(-comp_epfl.same, -comp_epfl.different) %>% 
  mutate(comp_epfl = df_log_epfl$comp_epfl)

log_reg_epfl <- multinom(comp_epfl ~ .,
                    data = df_log_enc_epfl)

summary(log_reg_epfl)

coef(log_reg_epfl)

log_model_epfl <- glm(comp_epfl ~ agree_sent + epfl_sent+ agree_stance+ epfl_stance+ 
                      sentiment_gpt + prompt,
                 family = binomial(), df_log_epfl)
summary(log_model_epfl)
