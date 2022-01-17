packages <- c("tidyverse", "patchwork", "cowplot",
              "ggsci")
lapply(packages, require, character.only = TRUE)

source("src/functions.R")

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

# Mortgage Discrimination -------------------------------------------------

load("data/mortgage_temp.rda")
nw_pred_melt <- nw_pred %>%
  reshape2::melt()
w_pred_melt <- w_pred %>%
  reshape2::melt()
total_pred_melt <- rbind(nw_pred_melt, w_pred_melt) %>%
  mutate(group = ifelse(variable %in% c("w_w", "w_nw"), "White", "Non-White"),
         variable = ifelse(variable %in% c("w_w", "nw_nw"), "Original Data", "Intervened Data"))

mean_total <- total_pred_melt %>%
  group_by(variable, group) %>%
  summarise(value = mean(value))

total_pred_melt$variable <- factor(total_pred_melt$variable,
levels = c("Original Data", "Intervened Data")
)
mean_total$variable <- factor(mean_total$variable,
levels = c("Original Data", "Intervened Data")
)

prob_plot <- ggplot(total_pred_melt) +
  geom_jitter(aes(y = value, x = variable, color = variable, fill = variable), width = 0.42,
  colour = "#333333", pch=21,
  size = 3, alpha = 0.9) +
  geom_violin(aes(y = value, x = variable, fill = variable), alpha = 0.8,
  color = "black"
  ) +
    geom_point(data = mean_total, aes(y = value, x = variable), shape = 8, size = 5) +
      geom_segment(data = mean_total, aes(
        x = c(1.5, 1.5, 0.5, 0.5), xend = c(2.5, 2.5, 1.5, 1.5), y = value,
        yend = value, group = group
      )) +
      geom_text(data = mean_total, aes(
        label = fix_label(paste0(round(value * 100, 1), "%")), group = group, x = variable, y = 1.02
      ), size = 4.5, family = font_family
      ) +
    facet_wrap(~group) +
      cowplot::theme_cowplot() +
      theme_custom +
      ylab("Predicted probability of success") +
      xlab("") +
      scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%"),
      breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1.02)) +
      scale_color_manual(name = "", values = ggsci::pal_aaas("default")(4)[3:4]) +
      scale_fill_manual(name = "", values = ggsci::pal_aaas("default")(4)[3:4]) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
        # axis.text.y = element_text(size = 10)
      ) + guides(fill = guide_legend(override.aes = list(shape = NA, alpha = NA)))

plot_df <- do_df %>%
  reshape2::melt() %>%
  rename(success = value) %>%
  mutate(failure = 1 - success) %>%
  group_by(variable) %>%
  summarise(
    # p5_success = quantile(success, 0.05),
    # p95_success = quantile(success, 0.95),
    p5_failure = quantile(failure, 0.05),
    p95_failure = quantile(failure, 0.95),
    sd_success = sd(success),
    success = mean(success),
    failure = mean(failure)
  ) %>%
  mutate(
    p5_success = success - 1.96 * sd_success,
    p95_success = success + 1.96 * sd_success
  ) %>%
  mutate(
      model = ifelse(variable %in% c("nw_nw", "w_w"), "Original Data", "Intervened Data"),
      race = ifelse(grepl("^nw", variable), "Non-White", "White")
  )

final_plot_df <- rbind(plot_df[,
c("variable", "success", "p5_success", "p95_success", "model", "race")
] %>%
                         rename(value = success,
                                p5 = p5_success,
                                p95 = p95_success) %>%
                         mutate(x = "Accepted"),
                       plot_df[, c("variable", "failure", "p5_failure",
                       "p95_failure", "model", "race")] %>%
                         rename(value = failure,
                                p5 = p5_failure,
                                p95 = p95_failure) %>%
                         mutate(x = "Denied"))

final_plot_df$x <- factor(final_plot_df$x, levels = c("Accepted", "Denied"))
final_plot_df$label <- paste0(round(final_plot_df$value, 3) * 100, "%")
final_plot_df$label <- fix_label(final_plot_df$label)
final_plot_df$model <- factor(final_plot_df$model,
  levels = c("Original Data", "Intervened Data")
)
text_size_small <- 4.5

outcome_plot <- ggplot(
  final_plot_df %>% filter(x == "Accepted"),
  aes(x = 1, y = value, fill = model)
) +
  geom_bar(stat = "identity", position = position_dodge(),
  color = "black",
  alpha = 0.9) +
  geom_errorbar(aes(ymin = p5, ymax = p95), width = 0.2,
  position = position_dodge(width=0.9)) +
  geom_text(aes(label = label, y = p95 + 0.02),
    size = text_size_small, family = font_family,
    position = position_dodge(width = 0.9)
  ) +
    facet_wrap(~race) +
  # coord_flip() +
  cowplot::theme_cowplot() +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%"),
  breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1.02)) +
  theme_custom + scale_fill_manual(name = "", values = ggsci::pal_aaas("default")(4)[3:4]) +
    ylab("Percentage predicted succesfull") +
    xlab("") +
    theme(
      # axis.text.y = element_text(size = 10),
      axis.ticks.x = element_blank(), axis.text.x = element_blank()
    ) +
    guides(fill = guide_legend(override.aes = list(shape = NA, alpha = NA)))

## -- Figure 3C

cross_results_df <- readRDS("data/teacher_bias/1_250_cross_within.rds")
# cross_results_df <- readRDS("data/simulations/1_250_cross_within_resample.rds")

names(cross_results_df) <- c("ses_00", "ses_10", "ses_01", "ses_11", "sex_00", "sex_10", "sex_01", "sex_11")

df_res <- lapply(cross_results_df, FUN = function(x) delist_cross(x, col_names = c("I", "II", "III", "IV", "V")))


cross_plot <- function(df1, df2, theme_custom, first = T, color1 = "#EE0000FF", color2 = "#3B4992FF", font_family = "Bookman") {
  text_size = 5.5
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
    geom_bar(stat = "summary", position = position_dodge(),
    color = "black"
    ) +
    geom_errorbar(aes(ymin = ptile_5, ymax = ptile_95), width = 0.2, position = position_dodge(width=1)) +
    geom_text(aes(label = paste0(round(mean, 3) * 100, "%")), position = position_dodge(width = 1), hjust=-.4, size=text_size, family = font_family) +
    coord_flip() + scale_fill_aaas(name = "Prediction model") + cowplot::theme_cowplot() +
    geom_vline(xintercept = mean(df_own %*% c(1, 2, 3, 4, 5)), linetype="dashed",
               color = color1) +
    annotate("text",
      x = mean(df_own %*% c(1, 2, 3, 4, 5)) + 0.2 * offset, y = 0.6, color = color1, size = text_size,
      label = as.character(round(mean(df_own %*% c(1, 2, 3, 4, 5)), 3)),
      family = font_family
    ) +
      theme_custom +
    geom_vline(xintercept = mean(df_other %*% c(1, 2, 3, 4, 5)), linetype = "dashed",
               color = color2) +
    annotate("text", x = mean(df_other %*% c(1, 2, 3, 4, 5)) - 0.2 * offset, y = 0.6, color = color2, size=text_size,
             label = as.character(round(mean(df_other %*% c(1, 2, 3, 4, 5)), 3))) +
    theme(legend.position = "bottom",
          panel.border = element_rect(color = "black")
          # text = element_text(size = 14),
          # axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.65)) +
    xlab("Predicted track level") + ylab("Proportion of predictions assigned to track")
}


ses_0 <- cross_plot(df_res$ses_00, df_res$ses_10, theme_custom = theme_custom) + scale_fill_aaas(name = "", labels = c("Predicted as Low Parent. Educ.", "Predicted as High Parent. Educ.")) +
  theme(legend.position = "none") + ylab("")
ses_1 <- cross_plot(df_res$ses_01, df_res$ses_11, theme_custom = theme_custom) + scale_fill_aaas(name = "", labels = c("Predicted as Low Parent. Educ.", "Predicted as High Parent. Educ."))

plot_3b <- ((ses_0 + ggtitle("High Parental Education students")) / (ses_1 + ggtitle("Low Parental Education students")) + plot_layout(ncol = 1, guides = "collect") +
               plot_annotation(theme = theme(legend.position = "bottom")))

# Combine -----------------------------------------------------------------

plot_3a <- (prob_plot + plot_layout(guides = "collect") & theme(legend.position = "bottom")) +
  ((outcome_plot) + theme(legend.position = "none"))

patchwork <- plot_3a | plot_3b
patchwork +
plot_layout(widths = c(5, 3)) +
plot_annotation(
  tag_levels = list(c('A.', ' ', 'B.', ' '))) &
         theme(text = element_text("serif"),
               plot.tag = element_text(face = 'bold'))

ggsave("tex/figs/fig3_group_analysis_new.pdf", last_plot(), height = 10, width = 16)
# ggsave("tex/figs/fig3_group_analysis.jpg", last_plot(), height = 10, width = 16)

