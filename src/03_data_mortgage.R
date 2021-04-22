library(foreign)
library(tidyverse)
library(xgboost)
library(randomForest)
library(lme4)
source("src/functions.R")

data <- read.dta("data/mortgage/mortgage.dta") %>%
    mutate(accept = accept * 100) %>%
    filter(!is.na(female),
           !is.na(accept),
           !is.na(married),
           !is.na(PI_ratio))

all_ids <- 1 : dim(data)[1]
train_prop <- .70
n_bootstrap <- 100
sensitivity <- 50
n <- dim(data)[1]
banal_perfs <- c()
model_perfs <- c()
female0_perfs <- c()
female1_perfs <- c()
insample <- c()

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

data$accept_bin <- ifelse(data$accept > 0 , 1, 0)

overall_perf <- list()

names_overall <- c("m0", "m1", "m2", "m3", "m4", "m5")
names_black <- paste0(names_overall, "_b")
names_white <- paste0(names_overall, "_w")
names_female <- paste0(names_overall, "_f")
names_male <- paste0(names_overall, "_m")

overall_df <- c()
black_df <- c()
white_df <- c()
female_df <- c()
male_df <- c()
joint_perf <- c()
joint_white <- c()
joint_black <- c()

cross_df <- data.frame(matrix(ncol = 4, nrow = 0))
names(cross_df) <- c("b_b", "b_w", "w_b", "w_w")

for (i in 1:n_bootstrap) {
    set.seed(i)
    train_ids <- sample(1:n, train_prop * n)
    test_ids <- all_ids[!(all_ids %in% train_ids)]

    train_df <- data[train_ids, ]
    test_df <- data[test_ids, ]

    black_train_df <- train_df %>%
        filter(black == 1)
    black_test_df <- test_df %>%
        filter(black == 1)

    white_train_df <- train_df %>%
        filter(black == 0)
    white_test_df <- test_df %>%
        filter(black == 0)

    model_0 <- glm(accept_bin ~ 1, data = train_df, family = "binomial")
    model_1 <- glm(accept_bin ~ 1 + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value +
        denied_PMI, data = train_df, family = "binomial")
    model_2 <- glm(accept_bin ~ 1 + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value +
        denied_PMI, data = train_df, family = "binomial")
    model_3 <- glm(accept_bin ~ 1 + female + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value +
        denied_PMI, data = train_df, family = "binomial")
    model_4 <- glm(accept_bin ~ 1 + black + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value +
        denied_PMI, data = train_df, family = "binomial")
    model_5 <- glm(accept_bin ~ 1 + female + black + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value +
        denied_PMI, data = train_df, family = "binomial")

    model_2_b <- glm(accept_bin ~ 1 + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value +
        denied_PMI, data = black_train_df, family = "binomial")

    model_2_w <- glm(accept_bin ~ 1 + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value +
        denied_PMI, data = white_train_df, family = "binomial")

    b_b <- mean(round(predict(model_2_b, black_test_df, type = "response")))
    b_w <- mean(round(predict(model_2_w, black_test_df, type = "response")))
    w_b <- mean(round(predict(model_2_b, white_test_df, type = "response")))
    w_w <- mean(round(predict(model_2_w, white_test_df, type = "response")))

    cross_df <- rbind(cross_df, c(b_b, b_w, w_b, w_w))
    names(cross_df) <- c("b_b", "b_w", "w_b", "w_w")

    joint_perf <- c(joint_perf, (b_b * mean(test_df$black)) + (w_w * (1 - mean(test_df$black))))
    joint_white <- c(joint_white, w_w)
    joint_black <- c(joint_black, b_b)

    models <- list(model_0, model_1, model_2, model_3, model_4, model_5)

    overall_perf <- lapply(models, FUN = function(x) mean(round(predict(x, test_df, type = "response")) == test_df$accept_bin))
    black_perf <- lapply(models, FUN = function(x) mean((round(predict(x, test_df, type = "response")) == test_df$accept_bin)[test_df$black == 1]))
    white_perf <- lapply(models, FUN = function(x) mean((round(predict(x, test_df, type = "response")) == test_df$accept_bin)[test_df$black == 0]))
    female_perf <- lapply(models, FUN = function(x) mean((round(predict(x, test_df, type = "response")) == test_df$accept_bin)[test_df$female == 1]))
    male_perf <- lapply(models, FUN = function(x) mean((round(predict(x, test_df, type = "response")) == test_df$accept_bin)[test_df$female == 0]))

    overall_df <- rbind(overall_df, unlist(overall_perf))
    black_df <- rbind(black_df, unlist(black_perf))
    white_df <- rbind(white_df, unlist(white_perf))
    female_df <- rbind(female_df, unlist(female_perf))
    male_df <- rbind(male_df, unlist(male_perf))
}


overall_df <- as.data.frame(overall_df)
black_df <- as.data.frame(black_df)
white_df <- as.data.frame(white_df)
female_df <- as.data.frame(female_df)
male_df <- as.data.frame(male_df)

names(overall_df) <- names_overall
names(black_df) <- names_black
names(white_df) <- names_white
names(female_df) <- names_female
names(male_df) <- names_male

results <- cbind(overall_df, black_df, white_df, female_df, male_df)
results$joint_perf <- joint_perf
results$joint_white <- joint_white
results$joint_black <- joint_black

saveRDS(results, "data/to_plot/mortgage.rds")