# Script information ------------
#' Aim: Comparing all annotations
#' Author: Laura Espinosa
#' Date created: 12 October 2023
#' Date updated: 12 October 2023


# Confusion matrix --------
## Get merged datasets ----------


## Get unique prompts ---------------
prompts <- df_all_clean$prompt %>% 
  unique()



## EPFL vs GPT ------------
for (i in prompts) {
  df_con_matrix <- df_all_clean %>% 
    filter(prompt == i) %>% 
    select(stance_epfl, sentiment_gpt) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix',i,sep='_'), df_con_matrix)
  #prompt_loop[i] <- prompts[i]
  conf_epfl_gpt <- confusionMatrix(df_con_matrix$sentiment_gpt,
                                   df_con_matrix$stance_epfl, mode = "everything")
  conf_epfl_gpt$prompt <- prompts[i]
  assign(paste('conf_epfl_gpt_all',i,sep='_'),conf_epfl_gpt) %>% 
    capture.output(., file = paste0("outputs/conf_epfl_gpt_all", i, ".csv"))
  
}


## EPFL vs Mturk --------------
df_con_matrix_mturk <- df_all_clean %>% 
    filter(!duplicated(id_tweets)) %>% 
    select(stance_epfl, agree_mturk) %>% 
    mutate(stance_epfl = factor(stance_epfl, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           agree_mturk = factor(agree_mturk, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 

conf_epfl_mturk <- confusionMatrix(df_con_matrix_mturk$agree_mturk,
                                     df_con_matrix_mturk$stance_epfl, mode = "everything")


  
conf_epfl_mturk %>% 
    capture.output(., file = paste0("outputs/conf_epfl_mturk.csv"))
  


## Mturk vs GPT --------------
for (i in prompts) {
  df_con_matrix_mturk_gpt <- df_all_clean %>% 
    filter(prompt == i) %>% 
    select(agree_mturk, sentiment_gpt) %>% 
    mutate(agree_mturk = factor(agree_mturk, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                levels = c("positive", "neutral", "negative"))) 
  assign(paste('df_con_matrix_mturk_gpt',i,sep='_'),df_con_matrix_mturk_gpt)
  #prompt_loop[i] <- prompts[i]
  conf_mturk_gpt <- confusionMatrix(df_con_matrix_mturk_gpt$sentiment_gpt,
                                    df_con_matrix_mturk_gpt$agree_mturk, mode = "everything")
  conf_mturk_gpt$prompt <- prompts[i]
  assign(paste('conf_mturk_gpt_all',i,sep='_'),conf_epfl_mturk) %>% 
    capture.output(., file = paste0("outputs/conf_mturk_gpt_all", i, ".csv"))
  
}

## GPT vs Mturk --------------------
for (i in prompts) {
  df_con_matrix_mturk_gpt <- df_all_clean %>% 
    filter(prompt == i) %>% 
    select(agree_mturk, sentiment_gpt) %>% 
    mutate(agree_mturk = factor(agree_mturk, ordered = TRUE,
                                levels = c("positive","neutral", "negative")),
           sentiment_gpt = factor(sentiment_gpt, ordered = TRUE,
                                  levels = c("positive", "neutral", "negative"))) 
  
  assign(paste('df_con_matrix_mturk_gpt',i,sep='_'),df_con_matrix_mturk_gpt)
  #prompt_loop[i] <- prompts[i]
  conf_gpt_mturk <- confusionMatrix(df_con_matrix_mturk_gpt$agree_mturk,
                                    df_con_matrix_mturk_gpt$sentiment_gpt, mode = "everything")
  conf_gpt_mturk$prompt <- prompts[i]
  assign(paste('conf_gpt_mturk_all',i,sep='_'),conf_epfl_mturk) %>% 
    capture.output(., file = paste0("outputs/conf_gpt_mturk_all", i, ".csv"))
  
}

# Comparing the confusion matrices outputs ------------
## EPFL-GPT vs EPFL-Mturk -------------
mcnemar_epfl_gpt_mturk <- mcnemar.test(conf_epfl_gpt_all_8$table,
                                        conf_epfl_gpt_all_835$table)

mcnemar_epfl_gpt_mturk

# For the Shiny app ----------
df_list <- ls(pattern = "^df_con_matrix")

# Comparison of results per class ---------------
## EPFL-GPT vs EPFL-Mturk -------------
### GPT 3.5 prompt 5
df_class_epfl_mturk <- conf_epfl_mturk$byClass
df_class_epfl_gpt_5 <- conf_epfl_gpt_all_5$byClass
wilcox_epfl__mturk_gpt5_sensitivity_positive <- wilcox.test(df_class_epfl_gpt_5[[3,3]],
                                                              df_class_epfl_mturk[[3,3]], paired = TRUE)
wilcox_epfl__mturk_gpt5_sensitivity_positive

# Merging all confusion matrices ------------
## Accuracy -------------
overall_all <- as.data.frame(conf_epfl_mturk$overall) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_0$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_1$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_3$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_4$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_5$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_6$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_7$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_8$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_40$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_41$overall)) %>% 
  cbind(as.data.frame(conf_epfl_gpt_all_45$overall)) %>% 
  t() %>% 
  as.data.frame() %>% 
  arrange(desc(Accuracy)) %>% 
  select(-Kappa) %>% 
  rownames_to_column("method") %>% 
  mutate(method = str_replace_all(method, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "gpt_all_40" = "GPT 4 prompt 0",
                                    "gpt_all_41" = "GPT 4 prompt 1",
                                    "gpt_all_45" = "GPT 4 prompt 5",
                                    "gpt_all_0" = "GPT 3.5 prompt 0",
                                    "gpt_all_1" = "GPT 3.5 prompt 1",
                                    "gpt_all_3" = "GPT 3.5 prompt 3",
                                    "gpt_all_4" = "GPT 3.5 prompt 4",
                                    "gpt_all_5" = "GPT 3.5 prompt 5",
                                    "gpt_all_6" = "GPT 3.5 prompt 6",
                                    "gpt_all_7" = "GPT 3.5 prompt 7",
                                    "gpt_all_8" = "GPT 3.5 prompt 8",
                                    "mturk" = "Amazon Mturk")))

overall_all %>%
  write_csv("outputs/confusion_matrix_accuracy.csv")

## Per class ---------
class_mturk <- as.data.frame(conf_epfl_mturk$byClass) %>% 
  mutate(class_tweet = c("positive_mturk", "neutral_mturk", "negative_mturk"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt0 <- as.data.frame(conf_epfl_gpt_all_0$byClass) %>% 
  mutate(class_tweet = c("positive_gpt0", "neutral_gpt0", "negative_gpt0"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt1 <- as.data.frame(conf_epfl_gpt_all_1$byClass) %>% 
  mutate(class_tweet = c("positive_gpt1", "neutral_gpt1", "negative_gpt1"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt3 <- as.data.frame(conf_epfl_gpt_all_3$byClass) %>% 
  mutate(class_tweet = c("positive_gpt3", "neutral_gpt3", "negative_gpt3"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt4 <- as.data.frame(conf_epfl_gpt_all_4$byClass) %>% 
  mutate(class_tweet = c("positive_gpt4", "neutral_gpt4", "negative_gpt4"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt5 <- as.data.frame(conf_epfl_gpt_all_5$byClass) %>% 
  mutate(class_tweet = c("positive_gpt5", "neutral_gpt5", "negative_gpt5"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt6 <- as.data.frame(conf_epfl_gpt_all_6$byClass) %>% 
  mutate(class_tweet = c("positive_gpt6", "neutral_gpt6", "negative_gpt6"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt7 <- as.data.frame(conf_epfl_gpt_all_7$byClass) %>% 
  mutate(class_tweet = c("positive_gpt7", "neutral_gpt7", "negative_gpt7"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt8 <- as.data.frame(conf_epfl_gpt_all_8$byClass) %>% 
  mutate(class_tweet = c("positive_gpt8", "neutral_gpt8", "negative_gpt8"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt40 <- as.data.frame(conf_epfl_gpt_all_40$byClass) %>% 
  mutate(class_tweet = c("positive_gpt40", "neutral_gpt40", "negative_gpt40"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt41 <- as.data.frame(conf_epfl_gpt_all_41$byClass) %>% 
  mutate(class_tweet = c("positive_gpt41", "neutral_gpt41", "negative_gpt41")) %>% 
  rownames_to_column() %>% 
  select(-rowname)

class_gpt45 <- as.data.frame(conf_epfl_gpt_all_45$byClass) %>% 
  mutate(class_tweet = c("positive_gpt45", "neutral_gpt45", "negative_gpt45"))%>% 
  rownames_to_column() %>% 
  select(-rowname)

class_all <- class_gpt0 %>% 
  full_join(class_gpt1) %>% 
  full_join(class_gpt3) %>% 
  full_join(class_gpt4) %>% 
  full_join(class_gpt40) %>% 
  full_join(class_gpt41) %>% 
  full_join(class_gpt45) %>% 
  full_join(class_gpt5) %>% 
  full_join(class_gpt6) %>% 
  full_join(class_gpt7) %>% 
  full_join(class_gpt8) %>% 
  full_join(class_mturk) %>% 
  separate(., class_tweet, into = c("class", "classifier"), 
           sep = "_") %>% 
  mutate(method = str_replace_all(classifier, 
                                  c("conf_epfl_" = "", 
                                    "\\$overall" = "",
                                    "gpt40" = "GPT 4 prompt 0",
                                    "gpt41" = "GPT 4 prompt 1",
                                    "gpt45" = "GPT 4 prompt 5",
                                    "gpt0" = "GPT 3.5 prompt 0",
                                    "gpt1" = "GPT 3.5 prompt 1",
                                    "gpt3" = "GPT 3.5 prompt 3",
                                    "gpt4" = "GPT 3.5 prompt 4",
                                    "gpt5" = "GPT 3.5 prompt 5",
                                    "gpt6" = "GPT 3.5 prompt 6",
                                    "gpt7" = "GPT 3.5 prompt 7",
                                    "gpt8" = "GPT 3.5 prompt 8",
                                    "mturk" = "Amazon Mturk"))) %>% 
  select(-classifier)

class_all %>% 
  write_csv("outputs/confusion_matrix_per_class_all.csv")

class_all_positive <- class_all %>% 
  filter(class == "positive") %>% 
  arrange(desc(F1))

class_all_positive %>% 
  write_csv("outputs/confusion_matrix_per_class_positive.csv")

class_all_negative <- class_all %>% 
  filter(class == "negative")%>% 
  arrange(desc(F1))

class_all_negative %>% 
  write_csv("outputs/confusion_matrix_per_class_negative.csv")

class_all_neutral <- class_all %>% 
  filter(class == "neutral")%>% 
  arrange(desc(F1))

class_all_neutral %>% 
  write_csv("outputs/confusion_matrix_per_class_neutral.csv")
