# Script information ------------
#' Aim: Comparing all annotations
#' Author: Laura Espinosa
#' Date created: 2 June 2024
#' Date updated: 2 June 2024

# Import data -------------
epfl_df_no_agree <- full_join(epfl_df_1, epfl_df_2, by = "text") %>% 
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
  filter(agree_stan == 0) %>%
  mutate(agreement = case_when((neutral_stan == 50 & pos_stan == 50) | (neutral_stan == 50 & neg_stan == 50) ~ "two-two",
                               (pos_stan == 50 & neutral_stan == 50) | (pos_stan == 50 & neg_stan == 50) ~ "two-two",
                               (neg_stan == 50 & neutral_stan == 50) | (neg_stan == 50 & pos_stan == 50) ~ "two-two",
                               .default = "two-one-one"),
         stance_predominant = case_when(agreement == "two-one-one" & neutral_stan == 50 ~ "neutral",
                                        agreement == "two-one-one" & pos_stan == 50 ~ "positive",
                                        agreement == "two-one-one" & neg_stan == 50 ~ "negative",
                                        agreement == "two-two" & neutral_stan == 50 & pos_stan == 50 ~ "neutral-positive",
                                        agreement == "two-two" & neutral_stan == 50 & neg_stan == 50 ~ "neutral-negative",
                                        agreement == "two-two" & neg_stan == 50 & pos_stan == 50 ~ "negative-positive",
                                        .default = "negative"))

epfl_df_no_agree_sum <- epfl_df_no_agree %>% 
  group_by(agreement, stance_predominant) %>% 
  tally() %>% 
  arrange(agreement, desc(n))
