## Load packages

packages <- c(
  "tidyverse", "caret", "stargazer", "lme4", "leaps", "gtools",
  "BBmisc", "ggthemes", "arm", "ggsci", "reshape2", "patchwork",
  "hrbrthemes", "cowplot", "showtext"
)

lapply(packages, require, character.only = TRUE)

# Setup -------------------------------------------------------------------

text_size <- 16

# Figure 1A - Fragile Families --------------------------------------------

theme_custom <- theme(panel.grid.major.x = element_line(size = 0.5, linetype = 'dotted',
                                                        colour = "lightgrey"), 
                      panel.grid.minor.x = element_line(size = 0.25, linetype = 'dotted',
                                                        colour = "lightgrey"),
                      strip.placement = "outside", 
                      strip.text.y = element_text(face = "bold", hjust=0.5, vjust=0.5),
                      strip.background=element_rect(fill = NA, color = "black", size = 1.5),
                      panel.spacing.x=unit(0.08, "lines"),
                      panel.spacing.y=unit(0.1, "lines"),
                      panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
                      legend.position = "top",
                      text = element_text(size = 16),
                      axis.text.x = element_text(size = 16),
                      axis.text.y = element_text(size = 16))

## Read data taken directly from the PNAS paper of the FFC.
main_plot_ffc <- read.csv("data/ffc/main_plot.csv")

ffc_df <- main_plot_ffc %>%
  rename(outcomes = on,
         best = point,
         upper = ci.max,
         lower = ci.min,
         bench = benchmark) %>%
  dplyr::select(outcomes, best, upper, lower, bench)

ffc_df$outcomes <- factor(ffc_df$outcomes,
                          levels = ffc_df$outcomes[order(ffc_df$bench, decreasing = T)])

## Generate plot 1.A
fig_1a <- ggplot(ffc_df, aes(y = bench, x = outcomes)) +
  geom_bar(stat = "identity", fill = pal_aaas(palette = "default")(3)[3],
           color = "black", alpha = 0.8) +
  geom_text(label = paste0(round(ffc_df$bench, 2) * 100, "%"), vjust = -1,
            size = 5) +
  theme_cowplot() + ylab("Predictive R-squared relative to null model") + xlab("Outcome") +
  theme(panel.grid.major.y = element_line(size = 0.25, linetype = 'dotted',
                                          colour = "lightgrey"), 
        panel.grid.minor.y = element_line(size = 0.125, linetype = 'dotted',
                                          colour = "lightgrey"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = (text_size - 1)),
        axis.text.y = element_text(size = (text_size - 1)),
        text = element_text(size = text_size)) +
  scale_x_discrete(labels = c("Material\nhardship", "GPA", "Job training",
                              "Eviction", "Grit", "Layoff")) +
  ylim(0, 1) + theme_custom

## 1B - Fragile Families detail of GPA predictions ------------------------

results_melt <- readRDS("data/to_plot/ffc_gpa.rds")

## Generate plot 1.B
fig_1b <- ggplot(results_melt, aes(y = value, x = test, color = variable)) + geom_point(alpha = 0.7) +
  geom_hline(aes(yintercept = nul), linetype = "dashed") + cowplot::theme_cowplot() +
  facet_grid(rows = vars(variable)) + xlim(1, 4) + ylim(1,4) + geom_abline(alpha = 0.7, linetype = "dotted") + theme_custom +
  xlab("Observed GPA") + ylab("Predicted GPA") + theme(legend.position = "none")

## 1C - Mortgage example - Full and Group Predictive Accuracy ------------------------

results <- readRDS("data/to_plot/mortgage.rds")

results_melt <- results %>%
  reshape2::melt() %>%
  mutate(var_name = gsub("_.*", "", variable),
         sub = ifelse(grepl("_b", variable), "Black",
                      ifelse(grepl("_w", variable), "White",
                             ifelse(grepl("_f", variable), "Female",
                                    ifelse(grepl("_m", variable), "Male",
                                           ifelse(grepl("black", variable), "Black",
                                                  ifelse(grepl("white", variable), "White",
                                                         ifelse(grepl("perf", variable), "Both", "All"))))))))

results_melt$var_name = factor(results_melt$var_name, levels = c("m0", "m1", "m2",
                                                                 "m3", "m4", "m5", "joint"))
results_group <- results_melt %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            sd = sd(value),
            p5 = quantile(value, 0.05),
            p95 = quantile(value, 0.95)) %>%
  mutate(var_name = gsub("_.*", "", variable),
         sub = ifelse(grepl("_b", variable), "Black",
                      ifelse(grepl("_w", variable), "White",
                             ifelse(grepl("_f", variable), "Female",
                                    ifelse(grepl("_m", variable), "Male", "All")))),
   
         model_type = ifelse(grepl("joint", var_name), "Separate\nmodels", "One model"))

add_all_mean <- results_group[results_group$sub == "All", c("var_name", "mean")] %>%
  rename(all_mean = mean)

results_group <- results_group %>%
  left_join(add_all_mean, by = "var_name")

results_group$label <- paste0(round(results_group$mean, 3) * 100, "%")

library(scales)

rel_df <- results_group %>% filter(grepl("All|Black", sub)) %>% filter(!grepl("m3|m5", var_name))
rel_df$var_name <- c(rep(c("Null\nModel", "+ Objective\n Scores", "+ Household\nCharacteristics", "+ Race Dummy"), 2),
                     "Separate model\nby race", "Separate model\nby race")

rel_df$var_name <- factor(rel_df$var_name, levels = c("Null\nModel", "+ Objective\n Scores", "+ Household\nCharacteristics", "+ Race Dummy", "Separate model\nby race"))

fig_1c <- ggplot(rel_df, aes(y = mean, x = fct_rev(as.factor(var_name)), fill = sub)) +
  geom_bar(stat = "identity", position = "dodge2", color = "black", alpha = 0.8) +
  geom_text(aes(label = label, y = 1.02, x = var_name, fill = sub), position = position_dodge(width = .9), width = .2) +
  # geom_hline(aes(yintercept = all_mean)) +
  # facet_wrap(~var_name) +
  geom_errorbar(aes(ymin = p5, ymax = p95), position = position_dodge(width = .9), width = .2) +
  coord_flip() + 
  scale_y_continuous(limits=c(0.5, 1.05), oob = scales::rescale_none,
                     labels = scales::percent) +
  cowplot::theme_cowplot() + ggsci::scale_fill_aaas(name = "Sample", labels = c("All applicants", "Non-White applicants")) +
  ylab("OOS-Predictive Accuracy") + theme_custom + facet_grid(rows = vars(model_type), scales = "free_y", space = "free") + xlab("") +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12))


# combine plot ------------------------------------------------------------

(fig_1a + fig_1b + fig_1c) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.title = element_blank(),
      plot.caption = element_text(size = 12)
    )
  )

ggsave("tex/figs/fig1_external_consciousness.pdf", last_plot(),
       width = 14, height = 7)
