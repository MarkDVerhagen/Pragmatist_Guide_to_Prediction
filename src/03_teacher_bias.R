library(tidyverse)
source("src/functions.R")

results_df <- readRDS("data/to_plot/performance_simulation.rds")

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

desc_df_full <- desc_df_full %>% filter(!grepl("Full ", type)) %>% filter(type == "No random effects")

saveRDS(desc_df_full, "data/to_plot/teacher_bias.rds")
