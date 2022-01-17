# Load packages

packages <- c("tidyverse", "caret", "stargazer", "lme4", "leaps", "gtools",
              "reshape2", "patchwork", "hrbrthemes", "cowplot", "showtext",
              "BBmisc", "ggthemes", "arm", "ggsci", "scales")
lapply(packages, require, character.only = TRUE)

text_size = 16

# Setup plotting themes --------------------------------------------------------

theme_custom <- theme(
  panel.grid.major.x = element_line(
    size = 0.5, linetype = "dotted",
    colour = "lightgrey"
  ),
  panel.grid.minor.x = element_line(
    size = 0.25, linetype = "dotted",
    colour = "lightgrey"
  ),
  strip.placement = "outside",
  strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
  strip.background = element_rect(fill = NA, color = "black", size = 1.5),
  panel.spacing.x = unit(0.08, "lines"),
  panel.spacing.y = unit(0.1, "lines"),
  panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
  legend.position = "top",
  text = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 16)
)

# Data underlying Figure 3 of PNAS paper of the FFC
main_plot_ffc <- read.csv("data/ffc/main_plot.csv")

# All submission data from the 160 teams
submissions <- read.csv("data/ffc/submissions.csv")

sub_df <- submissions %>%
  group_by(account, outcome) %>%
  summarise(r2_holdout = unique(r2_holdout),
            r2_bench = unique(ybar_train)) %>%
  filter(r2_holdout > -0.2) %>%
  ungroup() %>%
  mutate(type = "Submission") %>%
  left_join(main_plot_ffc %>%
              dplyr::select(outcome, benchmark)) %>%
  mutate(`Improved on\nBenchmark` = ifelse(r2_holdout > benchmark,
                                          "True", "False"))

sub_df$outcome <- factor(
  sub_df$outcome,
  levels = unique(sub_df$outcome[order(sub_df$benchmark, decreasing = T)])
)

fig_2a <- ggplot(sub_df, aes(x = outcome, y = r2_holdout)) +
  geom_jitter(aes(color = `Improved on\nBenchmark`)) +
  geom_point(data = sub_df %>% group_by(outcome) %>%
                 summarise(benchmark = mean(benchmark)),
               aes(x = outcome, y = benchmark), color = "black", shape = 8, size = 5) +
  geom_segment(data = sub_df %>% group_by(outcome) %>%
               summarise(benchmark = mean(benchmark)),
               aes(x = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                   xend = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), y=benchmark,
                   yend=benchmark)) +
  cowplot::theme_cowplot() + theme_custom + scale_color_manual(values = ggsci::pal_aaas()(2)[2:1]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = (text_size - 1)),
        axis.text.y = element_text(size = (text_size - 1)),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11)) +
  scale_x_discrete(labels = c("Material\nhardship", "GPA", "Job training",
                              "Eviction", "Grit", "Layoff")) +
  scale_y_continuous(limits=c(-0.25, 0.25), oob = scales::rescale_none,
                     labels = scales::percent) +
  ylab("Predictive R-squared relative to null model") + xlab("Outcome") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 14))
  

#  Mincerian wage equation ------------------------------------------------

## SIMPLE SIMULATION PLOT

data <- readRDS("data/mincerian/basic_simul_1000.rds")

df <- data %>%
  mutate(lm_1_1 = 1 - (lm_1_1 / lm_1_bench),
         lm_1_2 = 1 - (lm_1_2 / lm_1_bench),
         lm_1_3 = 1 - (lm_1_3 / lm_1_bench),
         lm_2_1 = 1 - (lm_2_1 / lm_2_bench),
         lm_2_2 = 1 - (lm_2_2 / lm_2_bench),
         lm_2_3 = 1 - (lm_2_3 / lm_2_bench),
         lm_3_1 = 1 - (lm_3_1 / lm_3_bench),
         lm_3_2 = 1 - (lm_3_2 / lm_3_bench),
         lm_3_3 = 1 - (lm_3_3 / lm_3_bench),
         gb_1 = 1 - (gb_1 / lm_1_bench),
         gb_2 = 1 - (gb_2 / lm_2_bench),
         gb_3 = 1 - (gb_3 / lm_3_bench)) %>%
  dplyr::select(-lm_1_bench, -lm_2_bench, -lm_3_bench)

df_melt <- df %>%
  reshape2::melt() %>%
  mutate(dataset = ifelse(grepl("lm_1|rf_1|gb_1", variable), "Dataset I",
                          ifelse(grepl("lm_2|rf_2|gb_2", variable), "Dataset II",
                                 ifelse(grepl("lm_3|rf_3|gb_3", variable), "Dataset III",
                                        "Else"))),
         model = ifelse(grepl("\\d{1}_1", variable), "Linear I",
                        ifelse(grepl("\\d{1}_2", variable), "Linear II",
                               ifelse(grepl("\\d{1}_3", variable), "Linear III",
                                      ifelse(grepl("rf", variable), "Random Forest",
                                             ifelse(grepl("gb", variable), "XGBoost",
                                                    "Other"))))))

df_summ <- df_melt %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            sd = sd(value),
            model = unique(model),
            dataset = unique(dataset))

df_plot <- df_summ %>%
  filter(!grepl("rf", variable))

custom_pal <- c(ggsci::pal_aaas()(6)[6],
                RColorBrewer::brewer.pal(3, "Blues"))

df_plot$model <- factor(df_plot$model, levels = c("XGBoost", "Linear I", "Linear II", "Linear III"))

fig_2b <- ggplot(df_plot, aes(y = mean, x = model, group = dataset, fill = model)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(as.character(round(mean, 2) * 100), "%")), vjust=-0.5, size = 4.5) +
  scale_fill_manual(values = custom_pal, name = "Model") +
  ylim(0, 1) +
  scale_y_continuous(limits=c(0.5, 1.05), oob = scales::rescale_none,
                     labels = scales::percent) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5) +
  facet_grid(rows = vars(dataset), scales = "free", space = "free", switch = "y") +
  cowplot::theme_cowplot() + xlab("Model") + ylab("Predictive R-squared relative to null model") +
  theme(panel.grid.major.x = element_line(size = 0.5, linetype = 'dotted',
                                          colour = "lightgrey"), 
        panel.grid.minor.x = element_line(size = 0.25, linetype = 'dotted',
                                          colour = "lightgrey"),
        strip.text.y = element_text(face = "bold", hjust=0.5, vjust=0.5),
        strip.background=element_rect(fill = NA, color = "black", size = 1.5),
        legend.position = "top",
        panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
        legend.text = element_text(size = 9.5),
        legend.title = element_text(size = 9.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = (text_size - 1)),
        axis.text.y = element_text(size = (text_size - 1)),
        axis.title.y = element_text(size = 16.3)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 14))

# Student Tracking --------------------------------------------------------

source("data/teacher_bias/analysis_functions.R")
source("data/teacher_bias/plot_functions.R")

## load coefficients
load("data/teacher_bias/coefs_perfs.rda")


results <- rbind(readRDS("data/teacher_bias/1_50_lm_lme_op_mop_perf.rds"),
                 readRDS("data/teacher_bias/51_250_lm_lme_op_mop_perf.rds"))

results_full <- rbind(readRDS("data/teacher_bias/1_50_mop_full_perf.rds"),
                      readRDS("data/teacher_bias/51_100_mop_full_perf.rds"),
                      readRDS("data/teacher_bias/101_150_mop_full_perf.rds"),
                      readRDS("data/teacher_bias/151_250_mop_full_perf.rds"))

results_df <- as.data.frame(cbind(results, results_full))

perf <- delist_perf(results_df)

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
         spec = ifelse(grepl("ses", variable), "Incl. Parental\nEducation",
                       ifelse(grepl("gender", variable), "Incl. Sex",
                              ifelse(grepl("school", variable), "Incl. School\nEffects", "Ability only"))))

desc_df_full <- rbind(desc_df, desc_df[desc_df$variable %in% c("mop_g", "lme_g"), ] %>%
                        mutate(variable = gsub("lme_g", "lm_school", variable),
                               variable = gsub("mop_g", "op_school", variable)) %>%
                        mutate(spec = "Incl. School\nEffects",
                               type = "No random effects")) %>%
  mutate(ifelse(grepl("op", variable), "Ordered Probit", "Linear Model")) %>%
  mutate(ptile_95 = ifelse(model == "Interval", NA, ptile_95),
         ptile_5 = ifelse(model == "Interval", NA, ptile_5))

desc_df_full$model <- factor(desc_df_full$model, levels = c("Interval", "Categorical"))


fig_2c <- ggplot(desc_df_full %>% filter(!grepl("Full ", type)) %>% filter(type == "No random effects"),
                  aes(x = as.character(reorder(spec, mean)), y = mean, fill = model, group = type)) +
  geom_bar(stat = "identity", position = position_dodge2(), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = ptile_5, ymax = ptile_95), position = position_dodge2(), size = 0.5) +
  scale_fill_manual(values = pal_aaas()(2)[1:2], name = "Model") +
  geom_text(aes(label = paste0(round(mean, 2) * 100, "%")), position = position_dodge2(width = 0.9),
            vjust = -2, size = 4.5) +
  scale_y_continuous(limits=c(0.5, 0.75), oob = scales::rescale_none,
                     labels = scales::percent) +
  scale_x_discrete() +
  cowplot::theme_cowplot() +
  geom_hline(yintercept = 0, linetype = 2, color = "darkgrey") +
  theme_custom + xlab("Explanatory variables") + ylab("Out-of-sample predictive accuracy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = (text_size - 1)),
        axis.text.y = element_text(size = (text_size - 1)),
        legend.text = element_text(size = 10.5),
        legend.title = element_text(size = 10.5)) +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 14))
# combine plot ------------------------------------------------------------

(fig_2a + fig_2b + fig_2c) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.title = element_blank(),
      plot.caption = element_text(size = 12)
    )
  )

ggsave("tex/figs/fig2_benchmarking.pdf", last_plot(),
  width = 14, height = 8
)

