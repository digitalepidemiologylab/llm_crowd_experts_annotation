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
  Method = c("Amazon Mturk", "GPT 3.5", "GPT 4",
             "Mistral", "Mixtral", "Amazon Mturk", 
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
            by = "agreement")

#### Figure ----------------
accuracy_fig_all <- overall_all_total %>% 
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
  scale_color_manual(values = c("<= 0.05" = "red",
                                "> 0.05" = "blue"),
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
  theme(text = element_text(size = 16),
        strip.text.x = element_text(size = 16),  
        strip.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.spacing.y = unit(0, "lines"),
        axis.text = element_text(color = "black")) 

accuracy_fig_all

ggsave("outputs/accuracy_figure.jpeg",
       accuracy_fig_all,
       width = 10, height = 6)

# Combined figure for F1, sensitivity and specificity ------------------
## Merged database ----------
class_total <- class_all %>% 
  rbind(class_all_agree)
