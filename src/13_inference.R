packages <- c("tidyverse", "caret", "stargazer", "lme4", "leaps", "gtools",
              "reshape2", "patchwork", "hrbrthemes", "cowplot", "showtext",
              "BBmisc", "ggthemes", "arm", "ggsci")
lapply(packages, require, character.only = TRUE)

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

# Mortgage Discrimination -------------------------------------------------

cross_df <- readRDS("data/edit/motgage_3a_results.rds")
plot_df <- cross_df %>%
  reshape2::melt() %>%
  rename(success = value) %>%
  mutate(failure = 1 - success) %>%
  group_by(variable) %>%
  summarise(p5_success = quantile(success, 0.05),
            p95_success = quantile(success, 0.95),
            p5_failure = quantile(failure, 0.05),
            p95_failure = quantile(failure, 0.95),
            success = mean(success),
            failure = mean(failure)) %>%
  mutate(model = ifelse(grepl("b_b|w_w", variable), "Own", "Other"),
         race = ifelse(grepl("^b", variable), "Black", "White"))

final_plot_df <- rbind(plot_df[, c("variable", "success", "p5_success", "p95_success", "model", "race")] %>%
                         rename(value = success,
                                p5 = p5_success,
                                p95 = p95_success) %>%
                         mutate(x = "Accepted"),
                       plot_df[, c("variable", "failure", "p5_failure", "p95_failure", "model", "race")] %>%
                         rename(value = failure,
                                p5 = p5_failure,
                                p95 = p95_failure) %>%
                         mutate(x = "Denied"))

final_plot_df$x <- factor(final_plot_df$x, levels = c("Accepted", "Denied"))
text_size <- 5.5
black_plot <- ggplot(final_plot_df %>% filter(race == "Black"), aes(x = x, y = value, fill = model)) +
  geom_bar(stat = "summary", position = position_dodge(), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = p5, ymax = p95), width = 0.2, position = position_dodge(width=0.9)) +
  geom_text(aes(label = paste0(round(value, 3) * 100, "%"), y = p95 + 0.1), position = position_dodge(width = 0.9), size=text_size) +
  coord_flip() + ggsci::scale_fill_aaas(name = "Prediction model") + cowplot::theme_cowplot() +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%", ""), breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25), limits = c(0, 1.12)) +
  theme_custom + ggtitle("Non-White applicants") + xlab("") + ylab("Predicted proportion")

white_plot <- ggplot(final_plot_df %>% filter(race == "White") %>% mutate(model = ifelse(model == "Own", "Other", "Own")), aes(x = x, y = value, fill = model)) +
  geom_bar(stat = "summary", position = position_dodge(), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = p5, ymax = p95), width = 0.2, position = position_dodge(width=0.9)) +
  geom_text(aes(label = paste0(round(value, 3) * 100, "%"), y = p95 + 0.1), position = position_dodge(width = 0.9), size=text_size) +
  coord_flip() + scale_fill_manual(name = "Prediction model", values = ggsci::pal_aaas()(2)[1:2]) + cowplot::theme_cowplot() +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%", ""), breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25), limits = c(0, 1.12)) +
  theme_custom + ggtitle("White applicants") + xlab("") + ylab("Predicted proportion")
library(patchwork)


black_plot <- black_plot  +
  scale_fill_manual(values = ggsci::pal_aaas()(6)[5:6], name = "Prediction model", labels = c("Fit to Whites", "Fit to Non-Whites")) +
  xlab("Predicted outcome") + ylab("") +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5))

white_plot <-
  white_plot  +
  scale_fill_manual(values = ggsci::pal_aaas()(6)[5:6], name = "Prediction model", labels = c("Fit to Whites", "Fit to Blacks/Hispanics")) +
  xlab("Predicted outcome") + theme(legend.position = "bottom") + ylab("Proportion predicted to outcome") +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5))

fig_3a <- (black_plot / white_plot) + plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = "I", theme = theme(legend.position = "bottom"))


# Teacher Bias: Cross Gender ---------------------------------------------------

source("src/plot_functions.R")

cross_results_df <- rbind(readRDS("data/teacher_bias/1_25_cross_new.rds"),
                          readRDS("data/teacher_bias/25_250_cross_new.rds"))

cross_results_df <- rbind(readRDS("data/teacher_bias/1_5_cross_new_resample.rds"),
                          readRDS("data/teacher_bias/6_100_cross_new_resample.rds"),
                          readRDS("data/teacher_bias/101_250_cross_new_resample.rds"))

cross_results_df <- readRDS("data/teacher_bias/1_250_cross_within.rds")
# cross_results_df <- readRDS("data/simulations/1_250_cross_within_resample.rds")

names(cross_results_df) <- c("ses_00", "ses_10", "ses_01", "ses_11", "sex_00", "sex_10", "sex_01", "sex_11")

df_res <- lapply(cross_results_df, FUN = function(x) delist_cross(x, col_names = c("I", "II", "III", "IV", "V")))


cross_plot <- function(df1, df2, first = T, color1 = "#EE0000FF", color2 = "#3B4992FF") {
  
  df_own <- df1
  df_other <- df2
  if (mean(df_own %*% c(1, 2, 3, 4, 5)) > mean(df_other %*% c(1, 2, 3, 4, 5))) {
    offset = 1  
  } else {
    offset = -1
  }
  
  
  plot_df <- data.frame(df_own, df_other) %>%
    reshape2::melt() %>%
    group_by(variable) %>%
    summarise(mean = mean(value),
              ptile_5 = quantile(value, probs=0.05, na.rm=TRUE),
              ptile_95 = quantile(value, probs=0.95, na.rm=TRUE)) %>%
    mutate(model = ifelse(grepl("\\.1", variable), "Other model", "Own model"),
           x = gsub("\\.1", "", variable))
  
  ggplot(plot_df, aes(x = x, y = mean, fill =model)) +
    geom_bar(stat = "summary", position = position_dodge(), color = "black") +
    geom_errorbar(aes(ymin = ptile_5, ymax = ptile_95), width = 0.2, position = position_dodge(width=1)) +
    geom_text(aes(label = paste0(round(mean, 3) * 100, "%")), position = position_dodge(width = 1), hjust=-.4, size=text_size) +
    coord_flip() + scale_fill_aaas(name = "Prediction model") + cowplot::theme_cowplot() +
    geom_vline(xintercept = mean(df_own %*% c(1, 2, 3, 4, 5)), linetype="dashed",
               color = color1) +
    annotate("text", x = mean(df_own %*% c(1, 2, 3, 4, 5)) + 0.2 * offset, y = 0.6, color = color1, size=text_size,
             label = as.character(round(mean(df_own %*% c(1, 2, 3, 4, 5)), 3))) +
    geom_vline(xintercept = mean(df_other %*% c(1, 2, 3, 4, 5)), linetype = "dashed",
               color = color2) +
    annotate("text", x = mean(df_other %*% c(1, 2, 3, 4, 5)) - 0.2 * offset, y = 0.6, color = color2, size=text_size,
             label = as.character(round(mean(df_other %*% c(1, 2, 3, 4, 5)), 3))) +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)
    ) +
    ylim(c(0, 0.65)) +
    xlab("Predicted track level") + ylab("Proportion of predictions assigned to track")
}
df1 <- df_res$ses_00
df2 <- df_res$ses_10


ses_0 <- cross_plot(df_res$ses_00, df_res$ses_10) + scale_fill_aaas(name = "Prediction model", labels = c("Fit to low-SES", "Fit to high-SES")) +
  theme(legend.position = "none") + ylab("") + ggtitle("High parental education")
ses_1 <- cross_plot(df_res$ses_01, df_res$ses_11) + scale_fill_aaas(name = "Prediction model", labels = c("Fit to low-SES", "Fit to high-SES")) +
  ggtitle("Low parental education")

sex_0 <- cross_plot(df_res$sex_00, df_res$sex_10, color1 = pal_aaas()(8)[8], color2 = pal_aaas()(8)[7]) +
  scale_fill_manual(values = pal_aaas()(8)[7:8], name = "Prediction model", labels = c("Fit to girls", "Fit to boys")) +
  theme(legend.position = "none") + ylab("") + ggtitle("Boys")
sex_1 <- cross_plot(df_res$sex_01, df_res$sex_11, color1 = pal_aaas()(8)[8], color2 = pal_aaas()(8)[7]) +
  scale_fill_manual(values = pal_aaas()(8)[7:8], name = "Prediction model", labels = c("Fit to girls", "Fit to boys")) +
  ggtitle("Girls")

fig_3b <- ((sex_0 + ggtitle("Boys") + ylab("")) / (sex_1 + ggtitle("Girls") + ylab("") + xlab("")) +
               plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "I", theme = theme(legend.position = "bottom")))
fig_3c <- ((ses_0 + ggtitle("High Parent. Educ.")) / (ses_1 + ggtitle("Low Parent. Educ.") + xlab("")) + plot_layout(ncol = 1, guides = "collect") +
               plot_annotation(tag_levels = "I", theme = theme(legend.position = "bottom")))

# Combine -----------------------------------------------------------------


patchwork <- (black_plot + sex_0 + ses_0) / (white_plot + sex_1 + ses_1)
patchwork[[2]] <- patchwork[[2]] + plot_layout(tag_level = 'new')
patchwork + plot_annotation(tag_levels = c('A'))

ggsave("tex/figs/fig3_group_analysis.pdf", last_plot(), height = 10, width = 16)
ggsave("tex/figs/fig3_group_analysis.jpg", last_plot(), height = 10, width = 16)
