# Script information ------------
#' Aim: Comparing all annotations and agreements
#' Author: Laura Espinosa
#' Date created: 21 January 2024
#' Date updated: 21 January 2024


# Combined figure for accuracy and CI --------------
## Dataset for CI and accuracy selecting majority class per agreement facet ---------
ci_majority_accuracy <- data.frame(
  xmin = 0,
  xmax = c(2, 2, 9, 9, 9, 9,
           2, 2, 9, 9, 9, 9),
  ymin = c(conf_epfl_majority$overall[[3]],
           conf_epfl_majority$overall[[3]],
           conf_epfl_majority$overall[[3]],
           conf_epfl_majority$overall[[3]],
           conf_epfl_majority$overall[[3]],
           conf_epfl_majority$overall[[3]],
           conf_epfl_majority_agree$overall[[3]],
           conf_epfl_majority_agree$overall[[3]],
           conf_epfl_majority_agree$overall[[3]],
           conf_epfl_majority_agree$overall[[3]],
           conf_epfl_majority_agree$overall[[3]],
           conf_epfl_majority_agree$overall[[3]]),
  ymax = c(conf_epfl_majority$overall[[4]],
           conf_epfl_majority$overall[[4]],
           conf_epfl_majority$overall[[4]],
           conf_epfl_majority$overall[[4]],
           conf_epfl_majority$overall[[4]],
           conf_epfl_majority$overall[[4]],
           conf_epfl_majority_agree$overall[[4]],
           conf_epfl_majority_agree$overall[[4]],
           conf_epfl_majority_agree$overall[[4]],
           conf_epfl_majority_agree$overall[[4]],
           conf_epfl_majority_agree$overall[[4]],
           conf_epfl_majority_agree$overall[[4]]),
  agreement = c("Partial agreement", "Partial agreement", 
                "Partial agreement", "Partial agreement",
                "Partial agreement", "Partial agreement",
                "Full agreement",
                "Full agreement", "Full agreement",
                "Full agreement", "Full agreement", "Full agreement"),
  Method = c("Mturk", "Vader","GPT 3.5", "GPT 4",
             "Mistral", "Mixtral",  "Mturk", "Vader" ,
             "GPT 3.5", "GPT 4", "Mistral", 
             "Mixtral"))

majority_accuracy <- data.frame(
  hline_accuracy = c(conf_epfl_majority$overall[[1]],
                     conf_epfl_majority_agree$overall[[1]]),
  agreement = c("Partial agreement", 
                "Full agreement")
)

#### Dataset ---------
overall_all_total <- overall_all_fig %>% 
  rbind(overall_all_agree_fig) %>% 
  left_join(majority_accuracy,
            by = "agreement") %>%
  mutate(Method = case_when(Method == "Amazon Mturk" ~ "Mturk",
                            .default = Method))

#### Figure ----------------
accuracy_fig_all <- overall_all_total  %>% 
  ggplot(aes(x = Prompt, y = Accuracy)) +
  geom_point(aes(color = factor(Pvalue), 
                 shape = factor(Pvalue),
                 size = factor(Pvalue))) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper,
                    color = Pvalue),
                width = 0.2) +
  geom_segment(aes(x = -Inf, xend = Inf,
                   y = hline_accuracy,
                   yend = hline_accuracy,
                   linetype = agreement),
               size = 1) +
  geom_rect(data = ci_majority_accuracy,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = agreement),
            alpha = 0.2, inherit.aes = FALSE) +
  facet_grid(agreement~Method, scales = "free_x") +
  theme_bw() +
  scale_linetype_manual(values = c("Partial agreement" = 2,
                                   "Full agreement" = 3)) +
  scale_color_manual(values = c("<= 0.05" = "darkviolet",
                                "> 0.05" = "black"),
                     labels = c("<= 0.05", 
                                "> 0.05")) +
  scale_shape_manual(values = c("<= 0.05" = "square",
                                "> 0.05" = "circle"),
                     labels = c("<= 0.05", 
                                "> 0.05")) +
  scale_size_manual(values = c("<= 0.05" = 3,
                               "> 0.05" = 2.5),
                    labels = c("<= 0.05", 
                               "> 0.05")) +
  scale_fill_manual(values = c("Partial agreement" = "darkgrey",
                               "Full agreement" = "black"),
                    labels = c("Partial agreement",
                               "Full agreement")) +
  labs(color = "P value",
       shape = "P value",
       size = "P value",
       linetype = "Accuracy for selecting \nmajority class",
       fill = "Confidence interval for \nselecting majority class") +
  theme(text = element_text(size = 14),
        strip.text.x = element_text(size = 14),  
        strip.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        panel.spacing.y = unit(0, "lines"),
        axis.text = element_text(color = "black")) 

accuracy_fig_all

ggsave("outputs/accuracy_figure.jpeg",
       accuracy_fig_all,
       width = 10, height = 6)


# Combined figure for all metrics ------------
## Merged database ----------
class_total_metrics <- class_all %>% 
  rbind(class_all_agree) %>% 
  mutate(Method = ifelse(Method == "Amazon Mturk", "Mturk",Method),
         Prompt = ifelse(is.na(Prompt), "None", Prompt),
         Stance = case_when(Stance == "negative" ~ "Negative",
                            Stance == "positive" ~ "Positive",
                            Stance == "neutral" ~ "Neutral")) %>% 
  select(Method, Prompt, Stance, agreement, `F1 score`, Sensitivity, Specificity) %>% 
  pivot_longer(cols = c(`F1 score`, Sensitivity, Specificity),
               names_to = "metric",
               values_to = "metric_value")

## Figure -----------
metrics_class_agreement_fig <- class_total_metrics %>% 
  ggplot(aes(x = Prompt)) +
  geom_point(aes(y = metric_value, color = Stance),
             size = 2, shape = 19)  +
  facet_nested(metric~agreement + Method, scales = "free_x") +
  scale_y_continuous(limits = c(0,1.05),
                     expand = c(0, 0),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  #scale_shape_manual(values = c(19, 3, 4)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(vjust = 2, hjust = 0.5),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size = 12),  
        strip.text.y = element_text(size = 12),
        legend.position = "bottom",
        plot.background = element_rect(color = "black", fill="white", size=1),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold")) +
  labs(x = "Model (prompt if applicable)",
       y = "Performance metric's value",
       colour = "Stance towards vaccination", 
       shape = "Performance metric",
       linetype = "F1 score for selecting \n majority class") 

metrics_class_agreement_fig

ggsave("outputs/metrics_class_assessment.jpeg", metrics_class_agreement_fig,
       width = 13, height = 9)

# Comparing ranking of methods by F1 score and class ---------
## Neutral --------
class_all_neutral_rank <- class_all_neutral %>% 
  arrange(desc(`F1 score`)) %>% 
  mutate(rank_neutral_partial = 1:n(),
         Model = paste0(Method, "-", Prompt)) %>% 
  select(Model, rank_neutral_partial) %>% 
  arrange(Model)

class_all_agree_neutral_rank <- class_all_agree_neutral %>% 
  arrange(desc(`F1 score`)) %>% 
  mutate(rank_neutral_full = 1:n(),
         Model = paste0(Method, "-", Prompt)) %>% 
  select(Model, rank_neutral_full) %>% 
  arrange(Model)

class_neutral_rank <- class_all_neutral_rank %>% 
  left_join(class_all_agree_neutral_rank, by = "Model") %>% 
  mutate(comparison = case_when(rank_neutral_partial == rank_neutral_full ~ 1,
                                .default = 0),
         similarity = case_when(rank_neutral_partial == rank_neutral_full - 2 | 
                                  rank_neutral_partial == rank_neutral_full + 2 ~ 1,
                                .default = 0))


## Negative ----------
class_all_negative_rank <- class_all_negative %>% 
  arrange(desc(`F1 score`)) %>% 
  mutate(rank_negative_partial = 1:n(),
         Model = paste0(Method, "-", Prompt)) %>% 
  select(Model, rank_negative_partial) %>% 
  arrange(Model)

class_all_agree_negative_rank <- class_all_agree_negative %>% 
  arrange(desc(`F1 score`)) %>% 
  mutate(rank_negative_full = 1:n(),
         Model = paste0(Method, "-", Prompt)) %>% 
  select(Model, rank_negative_full) %>% 
  arrange(Model)

class_negative_rank <- class_all_negative_rank %>% 
  left_join(class_all_agree_negative_rank, by = "Model") %>% 
  mutate(comparison = case_when(rank_negative_partial == rank_negative_full ~ 1,
                                .default = 0),
         similarity = case_when(rank_negative_partial == rank_negative_full - 2 | 
                                  rank_negative_partial == rank_negative_full + 2 ~ 1,
                                .default = 0))

## Positive ------------
class_all_positive_rank <- class_all_positive %>% 
  arrange(desc(`F1 score`)) %>% 
  mutate(rank_positive_partial = 1:n(),
         Model = paste0(Method, "-", Prompt)) %>% 
  select(Model, rank_positive_partial) %>% 
  arrange(Model)

class_all_agree_positive_rank <- class_all_agree_positive %>% 
  arrange(desc(`F1 score`)) %>% 
  mutate(rank_positive_full = 1:n(),
         Model = paste0(Method, "-", Prompt)) %>% 
  select(Model, rank_positive_full) %>% 
  arrange(Model)

class_positive_rank <- class_all_positive_rank %>% 
  left_join(class_all_agree_positive_rank, by = "Model") %>% 
  mutate(comparison = case_when(rank_positive_partial == rank_positive_full ~ 1,
                                .default = 0),
         similarity = case_when(rank_positive_partial == rank_positive_full - 2 | 
                                  rank_positive_partial == rank_positive_full + 2 ~ 1,
                                .default = 0))
