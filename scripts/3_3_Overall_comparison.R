# Script information ------------
#' Aim: Comparing all annotations and agreements
#' Author: Laura Espinosa
#' Date created: 21 January 2024
#' Date updated: 21 January 2024


# Combined figure for accuracy and CI --------------
## Dataset for CI and accuracy selecting majority class per agreement facet ---------
ci_majority_accuracy <- data.frame(
  xmin = 0,
  xmax = c(2, 9, 9, 9, 9,
           2, 9, 9, 9, 9),
  ymin = c(conf_epfl_majority$overall[[3]],
           conf_epfl_majority$overall[[3]],
           conf_epfl_majority$overall[[3]],
           conf_epfl_majority$overall[[3]],
           conf_epfl_majority$overall[[3]],
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
           conf_epfl_majority_agree$overall[[4]],
           conf_epfl_majority_agree$overall[[4]],
           conf_epfl_majority_agree$overall[[4]],
           conf_epfl_majority_agree$overall[[4]],
           conf_epfl_majority_agree$overall[[4]]),
  agreement = c("Partial agreement", "Partial agreement", 
                "Partial agreement", "Partial agreement",
                "Partial agreement", "Full agreement",
                "Full agreement", "Full agreement",
                "Full agreement", "Full agreement"),
  Method = c("Mturk", "GPT 3.5", "GPT 4",
             "Mistral", "Mixtral", "Mturk", 
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

# Combined figure for F1, sensitivity and specificity ------------------
## Merged database ----------
class_total <- class_all %>% 
  rbind(class_all_agree) %>% 
  mutate(Model = ifelse(Method == "Amazon Mturk", "Mturk", 
                        paste0(Method, " (", Prompt, ")"))) %>% 
  select(Model, Stance, agreement, `F1 score`, Sensitivity, Specificity)

## Individual figures ----------------
### Color palettes ---------
cb_palette <- brewer.pal(3, "Set1")

### Full agreement -----------------
#### Negative ----------------
metrics_full_negative_fig <- class_total %>% 
  filter(agreement == "Full agreement" & Stance == "negative") %>% 
  ggplot(aes(x = reorder(Model, -`F1 score`))) +
  geom_point(aes(y = `F1 score`, 
                 color = "F1 score",
                 shape = "F1 score"),
             size = 2) +
  geom_point(aes(y = Sensitivity, 
             color = "Sensitivity",
             shape = "Sensitivity"), stroke = 1,
             size = 1) +
  geom_point(aes(y = Specificity, 
             color = "Specificity",
             shape = "Specificity"), stroke = 1,
             size = 1) +
  scale_color_manual(values = cb_palette) +
  scale_shape_manual(values = c(19, 3, 4)) +
  scale_y_continuous(limits = c(0,1.05), 
                     expand = c(0, 0),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "right",
        plot.background = element_rect(color = "black", fill=NA, size=1),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  labs(x = "Model (prompt if applicable)",
       title = "Negative (Full agreement)",
       colour = "Metric", shape = "Metric")

metrics_full_negative_fig

#### Neutral ----------------
metrics_full_neutral_fig <- class_total %>% 
  filter(agreement == "Full agreement" & Stance == "neutral") %>% 
  ggplot(aes(x = reorder(Model, -`F1 score`))) +
  geom_point(aes(y = `F1 score`),
             shape = "circle",
             size = 2, color = cb_palette[1]) +
  geom_point(aes(y = Sensitivity), stroke = 1,
             shape = 3, size = 1, color = cb_palette[2]) +
  geom_point(aes(y = Specificity), stroke = 1,
             shape = 4, size = 1, color = cb_palette[3]) +
  scale_y_continuous(limits = c(0,1.05), 
                     expand = c(0,0),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12),
        plot.background = element_rect(color = "black", fill=NA, size=1)) +
  labs(x = "Model (prompt if applicable)",
       title = "Neutral (Full agreement)")

metrics_full_neutral_fig

#### Positive ----------------
metrics_full_positive_fig <- class_total %>% 
  filter(agreement == "Full agreement" & Stance == "positive") %>% 
  ggplot(aes(x = reorder(Model, -`F1 score`))) +
  geom_point(aes(y = `F1 score`),
             shape = "circle",
             size = 2, color = cb_palette[1]) +
  geom_point(aes(y = Sensitivity), stroke = 1,
             shape = 3, size = 1, color = cb_palette[2]) +
  geom_point(aes(y = Specificity), stroke = 1,
             shape = 4, size = 1, color = cb_palette[3]) +
  scale_y_continuous(limits = c(0,1.05), 
                     expand = c(0,0),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12),
        plot.background = element_rect(color = "black", fill=NA, size=1))  +
  labs(x = "Model (prompt if applicable)",
       title = "Positive (Full agreement)")

metrics_full_positive_fig

### Partial agreement -----------------
#### Negative ----------------
metrics_partial_negative_fig <- class_total %>% 
  filter(agreement == "Partial agreement" & Stance == "negative") %>% 
  ggplot(aes(x = reorder(Model, -`F1 score`))) +
  geom_point(aes(y = `F1 score`),
             shape = "circle",
             size = 2, color = cb_palette[1]) +
  geom_point(aes(y = Sensitivity), stroke = 1,
             shape = 3, size = 1, color = cb_palette[2]) +
  geom_point(aes(y = Specificity), stroke = 1,
             shape = 4, size = 1, color = cb_palette[3]) +
  scale_color_manual(values = cb_palette) +
  scale_y_continuous(limits = c(0,1.05), 
                     expand = c(0,0), 
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12),
        plot.background = element_rect(color = "black", fill=NA, size=1))  +
  labs(x = "Model (prompt if applicable)",
       title = "Negative (Partial agreement)")

metrics_partial_negative_fig

#### Neutral ----------------
metrics_partial_neutral_fig <- class_total %>% 
  filter(agreement == "Partial agreement" & Stance == "neutral") %>% 
  ggplot(aes(x = reorder(Model, -`F1 score`))) +
  geom_point(aes(y = `F1 score`),
             shape = "circle",
             size = 2, color = cb_palette[1]) +
  geom_point(aes(y = Sensitivity), stroke = 1,
             shape = 3, size = 1, color = cb_palette[2]) +
  geom_point(aes(y = Specificity), stroke = 1,
             shape = 4, size = 1, color = cb_palette[3]) +
  scale_y_continuous(limits = c(0,1.05), 
                     expand = c(0,0), 
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12),
        plot.background = element_rect(color = "black", fill=NA, size=1))  +
  labs(x = "Model (prompt if applicable)",
       title = "Neutral (Partial agreement)")

metrics_partial_neutral_fig

#### Positive ----------------
metrics_partial_positive_fig <- class_total %>% 
  filter(agreement == "Partial agreement" & Stance == "positive") %>% 
  ggplot(aes(x = reorder(Model, -`F1 score`))) +
  geom_point(aes(y = `F1 score`),
             shape = "circle",
             size = 2, color = cb_palette[1]) +
  geom_point(aes(y = Sensitivity), stroke = 1,
             shape = 3, size = 1, color = cb_palette[2]) +
  geom_point(aes(y = Specificity), stroke = 1,
             shape = 4, size = 1, color = cb_palette[3]) +
  scale_y_continuous(limits = c(0,1.05), 
                     expand = c(0,0), 
                     expand = c(0,0),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12),
        plot.background = element_rect(color = "black", fill=NA, size=1)) +
  labs(x = "Model (prompt if applicable)",
       title = "Positive (Partial agreement)")

metrics_partial_positive_fig

## Combine individual plots into one ---------
metrics_all_fig <- ggarrange(metrics_full_negative_fig,
                             metrics_full_neutral_fig,
                             metrics_full_positive_fig,
                             metrics_partial_negative_fig,
                             metrics_partial_neutral_fig,
                             metrics_partial_positive_fig,
                             common.legend = TRUE, legend = "right",
                             vjust = 1)
metrics_all_fig

# ggsave("outputs/metrics_figure.jpeg", metrics_all_fig,
#        height = 5, width = 10)

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
