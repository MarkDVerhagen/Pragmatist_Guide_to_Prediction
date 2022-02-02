## Load packages

packages <- c(
  "tidyverse", "ggsci", "reshape2", "patchwork",
  "cowplot", "scales", "grid", "gridExtra"
)

lapply(packages, require, character.only = TRUE)

source("./src/functions.R")

# Setup -------------------------------------------------------------------

text_size <- 16

# Figure 1A - Fragile Families --------------------------------------------
font_family = "Helvetica"
theme_custom <- theme(panel.grid.major.x = element_line(size = 0.5, linetype = 'dotted',
                                                        colour = "lightgrey"), 
                      panel.grid.minor.x = element_line(size = 0.25, linetype = 'dotted',
                                                        colour = "lightgrey"),
                      # strip.placement = "outside", 
                     #  strip.text.y = element_text(face = "bold", hjust=0.5, vjust=0.5),
                      strip.background=element_rect(fill = NA, color = "black", size = 1.5),
                      panel.spacing.x=unit(0.08, "lines"),
                      panel.spacing.y=unit(0.1, "lines"),
                      # panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
                      panel.border = element_rect(color = "black"),
                      legend.position = "top",
                      text = element_text(size = 20, family = font_family),
                      axis.text.x = element_text(size = 20, family = font_family),
                      axis.text.y = element_text(size = 20))

## Read data taken directly from the PNAS paper of the FFC.
main_plot_ffc <- read.csv("./data/ffc/main_plot.csv")

ffc_df <- main_plot_ffc %>%
  rename(outcomes = on,
         best = point,
         upper = ci.max,
         lower = ci.min,
         bench = benchmark) %>%
  dplyr::select(outcomes, best, upper, lower, bench)

ffc_df$outcomes <- factor(ffc_df$outcomes,
                          levels = ffc_df$outcomes[order(ffc_df$bench, decreasing = T)])

ffc_df$label <- fix_label(as.character(ffc_df$bench))

## Generate Figure 1.A
fig_1a <- ggplot(ffc_df, aes(y = bench, x = outcomes)) +
  geom_bar(stat = "identity", fill = MetBrewer::met.brewer("Egypt")[2],
           color = "black",
           alpha = 0.8) +
  geom_text(label = fix_label(paste0(round(ffc_df$bench, 3) * 100, "%")), vjust = -1,
            size = 5, family = font_family) +
  geom_rect(aes(ymin = rep(0.41, 6), ymax = rep(0.51, 6),
         xmin = (seq(0.5, 5.5, 1) + 0.03), xmax = (seq(1.5, 6.5, 1) - 0.03)),
         fill =  "white", colour = "black") +
  geom_text(
         label = ffc_df$outcomes,
         size = 4.75, aes(y = 0.46, x = outcomes), family = font_family
  ) +
  theme_cowplot() + ylab("R-squared") + xlab("") +
theme_custom +
  scale_x_discrete(labels = c("Material hardship", "GPA", "Job training",
                              "Eviction", "Grit", "Layoff")) +
  theme_custom +
         scale_y_continuous(labels = c("0%", "10%", "20%", "30%", "40%", "50%"),
         breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), limits = c(0, 0.51)) +
         theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
         )

## 1B - Fragile Families detail of GPA predictions ------------------------

## Read train and test set from FFC reproduction package
df_train <- read.csv("./data/ffc/train.csv")
df_test <- read.csv("./data/ffc/test.csv")

ffc_gpa_full <- readRDS("./data/edit/ffc_gpa_predictions_baseline_full.rds")

ffc_gpa_demog_ols <- ffc_gpa_full[
       ffc_gpa_full$account == "benchmark_ols_demographic",
]

ffc_gpa_full_ols <- ffc_gpa_full[
       ffc_gpa_full$account == "benchmark_ols_full",
]

results <- data.frame(
       test = ffc_gpa_demog_ols$truth,
       nul = mean(ffc_gpa_demog_ols$truth, na.rm = T),
       lm1 = ffc_gpa_demog_ols$prediction,
       lm2 = ffc_gpa_full_ols$prediction,
       challengeId = ffc_gpa_demog_ols$challengeID
)

## Transform predictions for plotting
results_melt <- reshape2::melt(results, id.vars = c("test", "nul", "challengeId")) %>%
       mutate(
              variable = gsub("lm1", "Household", variable),
              variable = gsub("lm2", "Household +\nlagged GPA", variable)
       )

## Save for Figure 2.B
saveRDS(results_melt, "./data/edit/ffc_gpa_predictions_null_baseline.rds")

## Generate Figure 1.B

fig_1b <- ggplot(results_melt, aes(y = value, x = test, fill = variable)) +
  geom_jitter(
         colour = "#333333",
         pch=21,
         size = 3, alpha = 0.8, width = 0.05) + #alpha = 0.2, size = 3, width = 0.04) +
  geom_hline(aes(yintercept = nul), linetype = "dashed") + cowplot::theme_cowplot() +
         theme_custom +
  facet_grid(rows = vars(variable)) + xlim(1, 4) + ylim(1,4) + geom_abline(alpha = 0.7, linetype = "dotted") + theme_custom +
  xlab("Observed GPA") + ylab("Predicted GPA") + theme(legend.position = "none") +
  scale_fill_manual(values = MetBrewer::met.brewer("Ingres")[c(4, 5)])

## New Fig 1C

load("./data/mortgage_temp.rda")

names(nw_df) <- paste0(names(nw_df), "_", "b")
names(w_df) <- paste0(names(w_df), "_", "w")
results <- cbind(overall_df, nw_df, w_df)

results_melt <- results %>%
       reshape2::melt() %>%
       mutate(
              var_name = gsub("_.*", "", variable),
              sub = ifelse(grepl("_b", variable), "Black",
                     ifelse(grepl("_w", variable), "White",
                            ifelse(grepl("black", variable), "Black",
                                   ifelse(grepl("white", variable), "White", "All")
                            )
                     )
              )
       )


results_melt$var_name = factor(results_melt$var_name, levels = c("m0", "m1", "m2",
                                                                 "m3", "m4", "m5"))
results_group <- results_melt %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            sd = sd(value)) %>%
  mutate(p5 = mean - 1.96 * sd,
         p95 = mean + 1.96 * sd) %>%
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

rel_df <- results_group %>% filter(grepl("All|White|Black", sub)) %>% filter(!grepl("m2|m3|m5", var_name))
rel_df$var_name <- c(rep(c("Null\nModel", "+ Objective\nScores", "+ Applicant\nRace"), 3))

## Order for plotting
rel_df$var_name <- factor(rel_df$var_name, levels = c("Null\nModel", "+ Objective\nScores", "+ Applicant\nRace"))
rel_df$label <- fix_label(rel_df$label)

fig_1c <- ggplot(rel_df %>% filter(sub %in% c("White", "Black"))) +
  geom_bar(stat = "identity", position = "dodge2", color = "black", alpha = 0.8, aes(y = mean, x = fct_rev(as.factor(var_name)), fill = sub)) +
  geom_text(aes(label = label, y = 1.0125, x = var_name, fill = sub), size = 5, position = position_dodge(width = .9), width = .2, family = font_family) +
  geom_errorbar(aes(y = mean, x = fct_rev(as.factor(var_name)), ymin = p5, ymax = p95, fill = sub), position = position_dodge(width = .9), width = .2) +
  geom_point(data = rel_df %>% filter(!(sub %in% c("White", "Black"))), aes(y = mean, x = fct_rev(as.factor(var_name)), shape = "Overall"),
             size = 4, width = 2, stroke = 1) +
  geom_text(data = rel_df %>% filter(!(sub %in% c("White", "Black"))),
            aes(label = label, y = mean + 0.075, x = fct_rev(as.factor(var_name)), shape = "Overall"),
            size = 4, width = 2, stroke = 1) +
  coord_flip() + 
  cowplot::theme_cowplot() +
  scale_fill_manual(name = "", values = MetBrewer::met.brewer("VanGogh1")[c(1, 7)], labels = c("White subset", "Non-White subset")) +
  scale_shape_manual(name = "", values = c(8)) +
  ylab("") + theme_custom +
  xlab("Explanatory Variables") +
  theme(axis.text.x = element_text(size = (text_size - 1)),
        axis.text.y = element_text(size = (text_size - 1))
        ) +
  theme(
         legend.text = element_text(size = 15),
         legend.title = element_text(size = 14)
  ) + ylim(0, 1.03) +
  scale_y_continuous(labels = scales::percent)

## Figure 1D
perf <- readRDS("./data/teacher_bias/teacher_bias_perf.rds")

melted <- reshape2::melt(perf %>%
                           dplyr::select(lm_g, lm_gender, lm_ses, lme_g,
                                         op_g, op_gender, op_ses, mop_g))

means <- melted %>% group_by(variable) %>% summarise(mean = mean(value))

desc_df <-  melted %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            sd = sd(value),
            ptile_5 = quantile(value, probs=0.05, na.rm=TRUE),
            ptile_95 = quantile(value, probs=0.95, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(model = ifelse(grepl("lm", variable), "Interval", "Categorical"),
         type = ifelse(grepl("lme_|mop_", variable), "Including random intercept",
                       ifelse(grepl("full", variable), "Full random effects", "No random effects")),
         spec = ifelse(grepl("ses", variable), "+ Parental\nEducation",
                       ifelse(grepl("gender", variable), "+ Sex",
                              ifelse(grepl("school", variable), "+ School\nEffects", "Ability only"))))

desc_df_full <- rbind(desc_df, desc_df[desc_df$variable %in% c("mop_g", "lme_g"), ] %>%
                        mutate(variable = gsub("lme_g", "lm_school", variable),
                               variable = gsub("mop_g", "op_school", variable)) %>%
                        mutate(spec = "+ School\nEffects",
                               type = "No random effects")) %>%
  mutate(ifelse(grepl("op", variable), "Ordered Probit", "Linear Model")) %>%
  mutate(ptile_95 = ifelse(model == "Interval", NA, ptile_95),
         ptile_5 = ifelse(model == "Interval", NA, ptile_5))

desc_df_full$model <- factor(desc_df_full$model, levels = c("Interval", "Categorical"))
desc_df_full$label <- paste0(round(desc_df_full$mean, 3) * 100, "%")
text_size <- 16

plot_1d_df <- desc_df_full %>% filter(!grepl("Full", type), 
                                      type == "No random effects",
                                      !grepl("Sex", spec))

plot_1d_df$spec <- factor(plot_1d_df$spec, levels = unique(plot_1d_df$spec))
plot_1d_df$model <- factor(plot_1d_df$model, levels = c("Categorical", "Interval"))
plot_1d_df$label <- fix_label(plot_1d_df$label)

fig_1d <- ggplot(plot_1d_df) +
  geom_bar(stat = "identity", position = "dodge2", color = "black", alpha = 0.8, aes(x = fct_rev(as.factor(spec)), y = mean, fill = model)) +
  geom_text(aes(label = label, y = 1.0125, x = fct_rev(as.factor(spec)), fill = model),
            size = 5, position = position_dodge(width = .9), width = .2, family = font_family) +
  geom_errorbar(aes(y = mean, x = fct_rev(as.factor(spec)), ymin = ptile_5, ymax = ptile_95, fill = model),
                position = position_dodge(width = .9), width = 0.2) +
  coord_flip() +
  cowplot::theme_cowplot() +
  scale_fill_manual(name = "", values = MetBrewer::met.brewer("Lakota")[c(3, 6)],
  labels = c("Categorical", "Interval")) +
  ylab("Predictive Accuracy") +
  theme_custom + 
  geom_hline(yintercept = 0, linetype = 2, color = "darkgrey") +
  xlab("Explanatory Variables") + 
  theme(axis.text.x = element_text(size = (text_size - 1)),
        axis.text.y = element_text(size = (text_size - 1))) +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 14)) +
         ylim(0, 1.03) +
  scale_y_continuous(labels = scales::percent)

## --- Figure 1E

load("./data/lemieux/results.rda")

fig_1e_df <- melt(resid, id.vars = "year") %>% mutate(variable = gsub("explained", "Explained", variable),
                                                      variable = gsub("resid", "Unexplained", variable),
                                                      year = year + 1900)

fig_1e <- ggplot(fig_1e_df) +
  geom_area(aes(x = year, y = value, fill = variable), color = "black") +
  scale_fill_manual(name = "", values = MetBrewer::met.brewer("Cassatt1", n = 8)[c(2, 7)]) +
  theme_bw() +
  geom_point(data = df_year_r2, aes(x = year, y = r2, shape = "1973 model"), size = 3, fill = "black") +
  geom_point(data = df_year_r2, aes(x = year, y = r2_own, shape = "Year model"), size = 3) +
  scale_shape_manual(name = "", values = c(8, 6)) +
  cowplot::theme_cowplot() +
  theme_custom +
  xlab("Year") +
  scale_y_continuous("Variance / R-squared") +
  theme(axis.text.x = element_text(size = (text_size - 1)),
        axis.text.y = element_text(size = (text_size - 1))
        ) +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 14))

## Combine plots
patchwork <- (fig_1a / fig_1c / fig_1d) | (fig_1b / fig_1e)

ggsave("tex/figs/fig1_external_consciousness.tiff",
       patchwork +
         plot_annotation(tag_levels = list(c('A', 'C', 'D', 'B', 'E')), tag_suffix = '.') &
         theme(#text = element_text("serif"),
               plot.tag = element_text(face = 'bold')),
       width = 16, height = 14
)