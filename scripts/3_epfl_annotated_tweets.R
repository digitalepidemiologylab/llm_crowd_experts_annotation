# Script information ------------
#' Aim: Descriptive analysis of annotated English tweets on vaccine public perception by EPFL 
#' Author: Laura Espinosa
#' Date created: 22 July 2023
#' Date updated: 22 July 2023

# Import data ----------------
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
  mutate(id_tweet = 1:nrow(epfl_df_1),
         agree_stance = case_when(stance_c == stance_m & stance_m == stance_g & stance_g == stance_l ~ 1,
                                .default = 0)) %>% 
  select(id_tweet, text, sent_m, sent_c, sent_g, sent_l, 
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
  select(id_tweet, text, sent_m, sent_c, sent_g, sent_l, neutral_sent,
         pos_sent, neg_sent, agree_sent, neutral_stan, pos_stan, neg_stan,
         agree_stan, stance_m, stance_c, stance_g, stance_l, agree_stance, stance_mean)

sum(epfl_df$agree_sent)/sum(!is.na(epfl_df$sent_l)) *100
sum(epfl_df$agree_stan)/sum(!is.na(epfl_df$sent_l)) *100
sum(!is.na(epfl_df$sent_l))
