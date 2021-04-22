# Load packages

packages <- c(
  "tidyverse", "caret", "stargazer", "lme4", "leaps", "gtools",
  "reshape2", "patchwork", "hrbrthemes", "cowplot", "showtext",
  "BBmisc", "ggthemes", "arm", "ggsci", "scales"
)
lapply(packages, require, character.only = TRUE)

text_size <- 16

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

theme_custom_y <- theme_custom + theme(
  panel.grid.major.y = element_line(
    size = 0.5, linetype = "dotted",
    colour = "lightgrey"
  ),
  panel.grid.minor.y = element_line(
    size = 0.25, linetype = "dotted",
    colour = "lightgrey"
  )
)

# Data underlying Figure 3 of PNAS paper of the FFC
sub_df <- readRDS('data/to_plot/ffc_all_submissions.rds')

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
  cowplot::theme_cowplot() + theme_custom_y + scale_color_manual(values = ggsci::pal_aaas()(2)[2:1]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = (text_size - 1)),
        axis.text.y = element_text(size = (text_size - 1)),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11)) +
  scale_x_discrete(labels = c("Material\nhardship", "GPA", "Job training",
                              "Eviction", "Grit", "Layoff")) +
  scale_y_continuous(limits=c(-0.25, 0.25), oob = scales::rescale_none,
                     labels = scales::percent) +
  ylab("Predictive R-squared relative to null model") + xlab("Outcome")  

#  Mincerian wage equation ------------------------------------------------

## SIMPLE SIMULATION PLOT

df_plot <- readRDS("data/to_plot/mincerian.rds")

custom_pal <- c(ggsci::pal_aaas()(6)[6],
                RColorBrewer::brewer.pal(3, "Blues"))

fig_2b <- ggplot(df_plot, aes(y = mean, x = model, group = dataset, fill = model)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(as.character(round(mean, 2) * 100), "%")), vjust=-0.5) +
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
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))

# Student Tracking --------------------------------------------------------

desc_df <- readRDS("data/to_plot/teacher_bias.rds")

fig_2c <- ggplot(
  desc_df,
  aes(x = as.character(reorder(spec, mean)), y = mean, fill = model, group = type)
) +
  geom_bar(stat = "identity", position = position_dodge2(), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = ptile_5, ymax = ptile_95), position = position_dodge2(), size = 0.5) +
  scale_fill_manual(values = pal_aaas()(2)[1:2], name = "Model") +
  geom_text(aes(label = paste0(round(mean, 2) * 100, "%")),
    position = position_dodge2(width = 0.9),
    vjust = -2
  ) +
  scale_y_continuous(
    limits = c(0.5, 0.75), oob = scales::rescale_none,
    labels = scales::percent
  ) +
  scale_x_discrete() +
  cowplot::theme_cowplot() +
  geom_hline(yintercept = 0, linetype = 2, color = "darkgrey") +
  theme_custom +
  xlab("Explanatory variables") +
  ylab("Out-of-sample predictive accuracy") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = (text_size - 1)),
    axis.text.y = element_text(size = (text_size - 1)),
    legend.text = element_text(size = 10.5),
    legend.title = element_text(size = 10.5)
  )
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
       width = 14, height = 8)

