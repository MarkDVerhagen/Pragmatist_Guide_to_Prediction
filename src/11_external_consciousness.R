## Load packages

packages <- c(
  "tidyverse",
  "reshape2", "patchwork",
  "cowplot"
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
                      text = element_text(size = 20),
                      axis.text.x = element_text(size = 20),
                      axis.text.y = element_text(size = 20))

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
           color = "black",alpha = 0.8) +
  geom_text(label = paste0(round(ffc_df$bench, 2) * 100, "%"), vjust = -1,
            size = 5) +
  theme_cowplot() + ylab("Predictive R-squared relative to null model") + xlab("Outcome") +
  theme(panel.grid.major.y = element_line(size = 0.25, linetype = 'dotted',
                                          colour = "lightgrey"), 
        panel.grid.minor.y = element_line(size = 0.125, linetype = 'dotted',
                                          colour = "lightgrey"),
        axis.text.x = element_text(angle = 50, hjust = 1, size = (text_size - 1)),
        axis.text.y = element_text(size = (text_size - 1)),
        text = element_text(size = text_size)) +
  scale_x_discrete(labels = c("Material\nhardship", "GPA", "Job training",
                              "Eviction", "Grit", "Layoff")) +
  ylim(0, 1) + theme_custom +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))

## 1B - Fragile Families detail of GPA predictions ------------------------

## Read train and test set from FFC reproduction package
df_train <- read.csv("data/ffc/train.csv")
df_test <- read.csv("data/ffc/test.csv")

## Read full background set from FFC reprooduction package
background_raw <- readRDS("data/ffc/background.rds")

## Transform missingness into NA
background <- background_raw %>%
  mutate(t5c13a_code = ifelse(as.numeric(t5c13a) <= 0, NA, as.numeric(t5c13a)),
         t5c13b_code = ifelse(as.numeric(t5c13b) <= 0, NA, as.numeric(t5c13b)),
         t5c13c_code = ifelse(as.numeric(t5c13c) <= 0, NA, as.numeric(t5c13c))) %>%
  mutate(gpa_age9 = rowMeans(dplyr::select(., c("t5c13a_code", "t5c13b_code", "t5c13c_code")), na.rm = T)) %>%
  filter(!is.na(gpa_age9))

## Generate trainset
df_train_features <- background %>%
  filter(challengeID %in% df_train$challengeID) %>%
  mutate(mother_race = cm1ethrace,
         mother_educ = cm1edu,
         mother_relation = cm1relf,
         age9_gpa_a = t5c13a,
         age9_gpa_b = t5c13b,
         age9_gpa_c = t5c13c)

## Generate testset
df_test_features <- background %>%
  filter(challengeID %in% df_test$challengeID) %>%
  mutate(mother_race = cm1ethrace,
         mother_educ = cm1edu,
         mother_relation = cm1relf,
         age9_gpa_a = t5c13a,
         age9_gpa_b = t5c13b,
         age9_gpa_c = t5c13c) %>%
  filter(mother_educ >= 0) %>%
  left_join(df_test %>% dplyr::select(challengeID, gpa)) %>%
  filter(!is.na(gpa))

## Generate simple versions of train and join train outcome
df_simple_train <- df_train_features %>%
  dplyr::select(challengeID, mother_race, mother_educ, mother_relation, age9_gpa_a,
                age9_gpa_b, age9_gpa_c, gpa_age9) %>%
  left_join(df_train %>% dplyr::select(challengeID, gpa)) %>%
  filter(!is.na(gpa))

lm_nul <- lm(gpa ~ 1, data = df_simple_train)

lm_gpa <- lm(gpa ~ 1 + as.factor(mother_race) + as.factor(mother_educ) + as.factor(mother_relation),
             data = df_simple_train)

lm_gpa2 <- lm(gpa ~ 1 + as.factor(mother_race) + as.factor(mother_educ) + as.factor(mother_relation) + gpa_age9,
              data = df_simple_train)

## Make predictions using the null model, the model with household characteristics, and the full benchmark
results <- data.frame(test = df_test_features$gpa,
                      nul = predict(lm_nul, df_test_features),
                      lm1 = predict(lm_gpa, df_test_features),
                      lm2 = predict(lm_gpa2, df_test_features))

## Transform predictions for plotting
results_melt <- reshape2::melt(results, id.vars = c("test", "nul")) %>%
  mutate(variable = gsub("lm1", "Household", variable),
         variable = gsub("lm2", "Household +\nlagged GPA", variable))

## Generate plot 1.B
fig_1b <- ggplot(results_melt, aes(y = value, x = test, color = variable)) + geom_point(alpha = 0.7) +
  geom_hline(aes(yintercept = nul), linetype = "dashed") + cowplot::theme_cowplot() +
  facet_grid(rows = vars(variable)) + xlim(1, 4) + ylim(1,4) + geom_abline(alpha = 0.7, linetype = "dotted") + theme_custom +
  xlab("Observed GPA") + ylab("Predicted GPA") + theme(legend.position = "none") +
  theme(axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))

## 1C - Mortgage example - Full and Group Predictive Accuracy ------------------------

results <- readRDS("./data/edit/mortgage_1c_results.rds")

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

rel_df <- results_group %>% filter(grepl("All|Black", sub)) %>% filter(!grepl("m3|m5", var_name))
rel_df$var_name <- c(rep(c("Null\nModel", "+ Objective\n Scores", "+ Household\nCharacteristics", "+ Race Dummy"), 2),
                     "Separate model\nby race", "Separate model\nby race")

rel_df$var_name <- factor(rel_df$var_name, levels = c("Null\nModel", "+ Objective\n Scores", "+ Household\nCharacteristics", "+ Race Dummy", "Separate model\nby race"))

fig_1c <- ggplot(rel_df, aes(y = mean, x = fct_rev(as.factor(var_name)), fill = sub)) +
  geom_bar(stat = "identity", position = "dodge2", color = "black", alpha = 0.8) +
  geom_text(aes(label = label, y = 1.03, x = var_name, fill = sub), size = 5, position = position_dodge(width = .9), width = .2) +
  # geom_hline(aes(yintercept = all_mean)) +
  # facet_wrap(~var_name) +
  geom_errorbar(aes(ymin = p5, ymax = p95), position = position_dodge(width = .9), width = .2) +
  coord_flip() + 
  scale_y_continuous(limits=c(0.5, 1.065), oob = scales::rescale_none,
                     labels = scales::percent) +
  cowplot::theme_cowplot() + ggsci::scale_fill_aaas(name = "Sample", labels = c("All", "Non-White only")) +
  ylab("OOS-Predictive Accuracy") + theme_custom + facet_grid(rows = vars(model_type), scales = "free_y", space = "free") + xlab("") +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))


fig_1c

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
  width = 14, height = 7
)

