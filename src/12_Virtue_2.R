# Load packages

packages <- c(
  "tidyverse", "ggsci", "reshape2", "patchwork",
  "cowplot", "scales", "grid", "gridExtra"
)
lapply(packages, require, character.only = TRUE)

# Setup plotting themes --------------------------------------------------------
text_size = 16
font_family = "Bookman"
theme_custom <- theme(
  panel.grid.major.x = element_line(
    size = 0.5, linetype = "dotted",
    colour = "lightgrey"
  ),
  panel.grid.minor.x = element_line(
    size = 0.25, linetype = "dotted",
    colour = "lightgrey"
  ),
  # strip.placement = "outside",
  strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
  strip.background = element_rect(fill = NA, color = "black", size = 1.5),
  panel.spacing.x = unit(0.08, "lines"),
  panel.spacing.y = unit(0.1, "lines"),
  # panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
  panel.border = element_rect(color = "black"),
  legend.position = "top",
  text = element_text(size = 20, family = font_family),
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20)
)

# Data underlying Figure 3 of PNAS paper of the FFC

# All submission data from the 160 teams
main_plot_ffc <- read.csv("data/ffc/main_plot.csv")
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
                                           "Improvement", "No improvement"))

sub_df$outcome <- factor(sub_df$outcome,
                         levels = unique(sub_df$outcome[order(sub_df$benchmark, decreasing = T)]))

fig_2a <- ggplot(sub_df) +
  geom_jitter(aes(fill = `Improved on\nBenchmark`, color = `Improved on\nBenchmark`,
  x = outcome, y = r2_holdout),
  colour = "#333333", pch=21,
  size = 3, alpha = 0.9) +
  geom_point(
    data = sub_df %>% group_by(outcome) %>%
      summarise(benchmark = mean(benchmark)),
    aes(x = outcome, y = benchmark), color = "black", shape = 8, size = 5
  ) +
    geom_rect(data = data.frame(ymin = rep(0.26, 6), ymax = rep(0.34, 6),
          xmin = (seq(0.5, 5.5, 1) + 0.03), xmax = (seq(1.5, 6.5, 1) - 0.03)),
          aes(ymax = ymax, ymin = ymin, xmin = xmin, xmax = xmax),
         fill =  "white", colour = "black") +
  geom_text(data = data.frame(label = c("Material\nhardship", "GPA", "Job\ntraining", "Eviction", "Grit", "Layoff"),
                              x =c("materialHardship", "gpa", "jobTraining", "eviction", "grit", "layoff"),
                              family = font_family),
            size = 5, aes(label = label, y = 0.3, x = x), family = font_family) +
  geom_segment(data = sub_df %>% group_by(outcome) %>%
                 summarise(benchmark = mean(benchmark)),
               aes(x = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                   xend = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), y=benchmark,
                   yend=benchmark)) +
  cowplot::theme_cowplot() + theme_custom +
  scale_fill_manual(name = "", values = ggsci::pal_aaas()(2)[1:2]) +
  scale_color_manual(name = "", values = ggsci::pal_aaas()(2)[1:2]) +
  scale_x_discrete(labels = c("Material\nhardship", "GPA", "Job training",
                              "Eviction", "Grit", "Layoff")) +
  scale_y_continuous(limits=c(-0.2, 0.325), oob = scales::rescale_none,
                     labels = scales::percent_format(accuracy = 5L)) +
  ylab("Predictive R-squared") + xlab("") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# New Fig 2B

## Select top submission of GPA outcome
max_gpa_holdout <- max(sub_df$r2_holdout[sub_df$outcome == "gpa"])
gpa_submissions <- sub_df %>%
  filter(outcome == "gpa")
acc <- gpa_submissions$account[gpa_submissions$r2_holdout == max_gpa_holdout]

acc_df <- submissions %>%
  filter(account == acc) %>%  ## Select top account's submissions
  filter(outcome == "gpa") %>%  ## Select GPA only
  filter(!is.na(truth))  ## Omit missing outcomes

## Include linear predictions of baseline model
base_df <- readRDS("data/edit/ffc_gpa_predictions_baseline.rds") %>%
  ungroup() %>%
  select(challengeID, prediction) %>%
  rename(ols_prediction = prediction)
# %>%
#   filter(predictors == "full")


total_df <- acc_df %>%
  left_join(base_df, by = "challengeID") %>%
  select(challengeID, prediction, ols_prediction, truth)

total_df_clean <- total_df[!is.na(total_df$truth), ]

total_melt <- total_df %>%
  reshape2::melt(id.vars = c("challengeID", "truth")) %>%
  filter(!is.na(truth))

fig_2b <- ggplot(total_melt, aes(x = truth, y = value, fill = variable, color = variable)) +
  geom_jitter(width = 0.05, alpha = 0.8,
  colour = "#333333", pch=21,
  size = 3) +
  ylim(1, 4) +
  xlim(1, 4) +
  geom_smooth(se = F) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_hline(yintercept = mean(total_df_clean$truth), linetype = "dashed") +
  scale_fill_manual(name = "", values = MetBrewer::met.brewer("Signac")[c(3, 13)],
  labels = c("Best submission", "OLS benchmark")) +
  scale_color_manual(name = "", values = MetBrewer::met.brewer("Signac")[c(3, 13)],
  labels = c("Best submission", "OLS benchmark")) +
  ylab("Predicted GPA") +
  xlab("Actual GPA") +
  cowplot::theme_cowplot() +
    theme_custom +
    theme(
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 14)
    # ,
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_blank()
  )
    

#  Mincerian wage equation ------------------------------------------------

## SIMPLE SIMULATION PLOT

# data <- readRDS("data/mincerian/simul_1000.rds")

# df <- data %>%
#   mutate(lm_1_1 = 1 - (lm_1_1 / lm_1_bench),
#          lm_1_2 = 1 - (lm_1_2 / lm_1_bench),
#          lm_1_3 = 1 - (lm_1_3 / lm_1_bench),
#          lm_2_1 = 1 - (lm_2_1 / lm_2_bench),
#          lm_2_2 = 1 - (lm_2_2 / lm_2_bench),
#          lm_2_3 = 1 - (lm_2_3 / lm_2_bench),
#          lm_3_1 = 1 - (lm_3_1 / lm_3_bench),
#          lm_3_2 = 1 - (lm_3_2 / lm_3_bench),
#          lm_3_3 = 1 - (lm_3_3 / lm_3_bench),
#          gb_1 = 1 - (gb_1 / lm_1_bench),
#          gb_2 = 1 - (gb_2 / lm_2_bench),
#          gb_3 = 1 - (gb_3 / lm_3_bench)) %>%
#   dplyr::select(-lm_1_bench, -lm_2_bench, -lm_3_bench)

## Using the data from 

df <- readRDS("./data/edit/simul_4models_10.rds") %>%
  filter(!grepl("4", variable))

df_melt <- df %>%
  # reshape2::melt() %>%
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

# df_summ <- df_melt %>%
#   group_by(variable) %>%
#   summarise(mean = mean(value),
#             sd = sd(value),
#             model = unique(model),
#             dataset = unique(dataset))

df_summ <- df_melt

df_plot <- df_summ %>%
  filter(!grepl("rf", variable))

df_plot$model <- factor(df_plot$model, levels = c("XGBoost", "Linear I", "Linear II", "Linear III"))

fig_2c <- ggplot(df_plot, aes(y = mean, x = model, group = dataset, fill = model)) +
  geom_bar(stat = "identity",
  color = "black", alpha = 0.9) +
  geom_text(aes(label = paste0(as.character(round(mean, 2) * 100), "%")), vjust=-0.5, size = 4.5, family = font_family) +
  scale_fill_manual(values = MetBrewer::met.brewer("Egypt"), name = "") +
  # scale_fill_manual(values = MetBrewer::met.brewer("Cassatt2")[c(6, 5, 4, 3)], name = "Model") +
  ylim(0, 1) +
  scale_y_continuous(limits=c(0.5, 1.05), oob = scales::rescale_none,
                     labels = scales::percent_format(accuracy = 5L)) +
  # geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5) +
  facet_wrap(~dataset) +
  cowplot::theme_cowplot() + xlab("Model used for prediction") + ylab("Predictive R-squared") +
  theme_custom +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# combine plot ------------------------------------------------------------

(fig_2a + fig_2b) / (fig_2c) +
  plot_annotation(
    tag_levels = "A", tag_suffix = '.') &
  theme(text = element_text("serif"),
        plot.tag = element_text(face = 'bold'))

ggsave("tex/figs/fig2_benchmarking_new.pdf", last_plot(),
       width = 16, height = 14
)


