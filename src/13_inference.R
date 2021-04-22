packages <- c(
  "tidyverse", "caret", "stargazer", "lme4", "leaps", "gtools",
  "reshape2", "patchwork", "hrbrthemes", "cowplot", "showtext",
  "BBmisc", "ggthemes", "arm", "ggsci"
)
lapply(packages, require, character.only = TRUE)

source("src/functions.r")

# Mortgage Discrimination -------------------------------------------------


final_plot_df <- readRDS("data/to_plot/mortgage_cross.rds")

black_plot <- ggplot(final_plot_df %>% filter(race == "Black"), aes(x = x, y = value, fill = model)) +
  geom_bar(stat = "summary", position = position_dodge(), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = p5, ymax = p95), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(value, 3) * 100, "%")), position = position_dodge(width = 0.9), hjust = -0.6, size = 4) +
  coord_flip() +
  ggsci::scale_fill_aaas(name = "Prediction model") +
  cowplot::theme_cowplot() +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%", ""), breaks = c(0, 0.25, 0.5, 0.75, 1, 1.2), limits = c(0, 1.12)) +
  theme_custom +
  ggtitle("Non-White applicants") +
  xlab("") +
  ylab("Predicted proportion")

white_plot <- ggplot(final_plot_df %>% filter(race == "White") %>% mutate(model = ifelse(model == "Own", "Other", "Own")), aes(x = x, y = value, fill = model)) +
  geom_bar(stat = "summary", position = position_dodge(), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = p5, ymax = p95), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(value, 3) * 100, "%")), position = position_dodge(width = 0.9), hjust = -0.6, size = 4) +
  coord_flip() +
  scale_fill_manual(name = "Prediction model", values = ggsci::pal_aaas()(2)[1:2]) +
  cowplot::theme_cowplot() +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%", ""), breaks = c(0, 0.25, 0.5, 0.75, 1, 1.2), limits = c(0, 1.12)) +
  theme_custom +
  ggtitle("White applicants") +
  xlab("") +
  ylab("Predicted proportion")

black_plot <- black_plot +
  scale_fill_manual(values = ggsci::pal_aaas()(6)[5:6], name = "Prediction model", labels = c("Fit to Whites", "Fit to Non-Whites")) +
  xlab("Predicted outcome") + ylab("") +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
  )
white_plot <- white_plot +
  scale_fill_manual(values = ggsci::pal_aaas()(6)[5:6], name = "Prediction model", labels = c("Fit to Whites", "Fit to Blacks/Hispanics")) +
  xlab("Predicted outcome") + theme(legend.position = "bottom") + ylab("Proportion predicted to outcome") +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5)
  )

fig_3a <- (black_plot / white_plot) + plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = "I", theme = theme(legend.position = "bottom"))

# Teacher Bias: Cross Gender ---------------------------------------------------

cross_results_df <- readRDS("data/to_plot/teacher_bias_cross.rds")

names(cross_results_df) <- c("ses_00", "ses_10", "ses_01", "ses_11", "sex_00", "sex_10", "sex_01", "sex_11")

df_res <- lapply(cross_results_df, FUN = function(x) delist_cross(x, col_names = c("I", "II", "III", "IV", "V")))

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
patchwork[[2]] <- patchwork[[2]] + plot_layout(tag_level = "new")
patchwork + plot_annotation(tag_levels = c("A"))

ggsave("tex/figs/fig3_group_analysis.pdf", last_plot(), height = 10, width = 16)
