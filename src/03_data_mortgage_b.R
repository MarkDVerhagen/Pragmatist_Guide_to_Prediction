library(foreign)
library(tidyverse)
library(scales)

source("./src/functions.R")

## Drop missing
data <- read.dta("./data/mortgage/mortgage.dta") %>%
    mutate(accept = accept * 100) %>%
    filter(!is.na(female),
           !is.na(accept),
           !is.na(married),
           !is.na(PI_ratio))

all_ids <- 1 : dim(data)[1]
train_prop <- .70
n_repetition <- 100
sensitivity <- 50
n <- dim(data)[1]
cv_k <- 5

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
nw_df <- c()
w_df <- c()

black_df <- c()
white_df <- c()
female_df <- c()
male_df <- c()
joint_perf <- c()
joint_white <- c()
joint_black <- c()

cross_df <- data.frame(matrix(ncol = 4, nrow = 0))
names(cross_df) <- c("nw_nw", "nw_w", "w_nw", "w_w")

calc_rmse <- function(model, data) {
        true_outcome <- data %>% as.data.frame() %>% pull(accept_bin)
        return(mean((round(predict(model, data, type = "response")) -
        mean(true_outcome))^2))
    }

calc_perf <- function(model, data, w = F, nw = F) {
    data <- data %>% as.data.frame()
    if (nw) {
        true_outcome <- data %>% filter(black == 1) %>% pull(accept_bin)    
        if (length(true_outcome) == 0) {
            return(NA)
        }
        return(mean(round(predict(model, data[data$black == 1, ], type = "response")) == true_outcome))
    } else {
        true_outcome <- data %>% as.data.frame() %>% pull(accept_bin)
        return(mean(round(predict(model, data, type = "response")) == true_outcome))
    }
}

calc_perf2 <- function(model, data, w = F, female = F) {
    data <- data %>% as.data.frame()
    if (female) {
        true_outcome <- data %>% filter(female == 1) %>% pull(accept_bin)    
        if (length(true_outcome) == 0) {
            return(NA)
        }
        return(mean(round(predict(model, data[data$female == 1, ], type = "response")) == true_outcome))
    } else {
        true_outcome <- data %>% as.data.frame() %>% pull(accept_bin)
        return(mean(round(predict(model, data, type = "response")) == true_outcome))
    }
}

calc_rmse <- function(model, data, w = F, nw = F) {
    data <- data %>% as.data.frame()
    if (nw) {
        true_outcome <- data %>% filter(black == 1) %>% pull(accept_bin)    
        if (length(true_outcome) == 0) {
            return(NA)
        }
        return(mean((predict(model, data[data$black == 1, ], type = "response") - true_outcome)^2))
    } else {
        true_outcome <- data %>% as.data.frame() %>% pull(accept_bin)
        return(mean((predict(model, data, type = "response") - true_outcome)^2))
    }
}

model_0 <- function(data) {
    glm(accept_bin ~ 1, data = data, family = "binomial")
}
model_1 <- function(data) {
    glm(accept_bin ~ 1 + housing_expense_ratio +
    bad_history + PI_ratio + loan_to_value +
    denied_PMI, data = data, family = "binomial")
}
model_2 <- function(data) {
    glm(accept_bin ~ 1 + self_employed + married + housing_expense_ratio +
    bad_history + PI_ratio + loan_to_value +
    denied_PMI, data = data, family = "binomial")
}
model_3 <- function(data) {
    glm(accept_bin ~ 1 + female + self_employed + married + housing_expense_ratio +
    bad_history + PI_ratio + loan_to_value +
    denied_PMI, data = data, family = "binomial")
}
model_4 <- function(data) {
    glm(accept_bin ~ 1 + black + self_employed + married + housing_expense_ratio +
    bad_history + PI_ratio + loan_to_value +
    denied_PMI, data = data, family = "binomial")
}
model_5 <- function(data) {
    glm(accept_bin ~ 1 + female + black + self_employed + married + housing_expense_ratio +
    bad_history + PI_ratio + loan_to_value +
    denied_PMI, data = data, family = "binomial")
}
model_cross <- function(data) {
    glm(accept_bin ~ 1 + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value + female +
        denied_PMI, data = data, family = "binomial")
}
model_cross2 <- function(data) {
    glm(accept_bin ~ 1 + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value + black +
        denied_PMI, data = data, family = "binomial")
}

cv_frame <- cv_frame_nw

gen_cv_frame <- function(cv_frame, cv_frame_1, cv_frame_2, nw = F, w = F) {
    l0 <- map(cv_frame$train, ~model_0(.))
    l1 <- map(cv_frame$train, ~model_1(.))
    l2 <- map(cv_frame$train, ~model_2(.))
    l3 <- map(cv_frame$train, ~model_3(.))
    l4 <- map(cv_frame$train, ~model_4(.))
    l5 <- map(cv_frame$train, ~model_5(.))
    l_cross <- map(cv_frame$train, ~model_cross(.))

    if (nw) {
        cv_frame <- cv_frame_1
    } else if(w) {
        cv_frame <- cv_frame_2
    } else {
        cv_frame <- cv_frame
    }
    mean(unlist(map2(l0, cv_frame$test, ~calc_perf(.x, .y))))
    mean(unlist(map2(l5, cv_frame$test, ~calc_perf(.x, .y))))
    mean(unlist(map2(l5, cv_frame$test, ~calc_perf(.x, .y))))
    return(data.frame(
        m0 = unlist(map2(l0, cv_frame$test, ~calc_perf(.x, .y))),
        m1 = unlist(map2(l1, cv_frame$test, ~calc_perf(.x, .y))),
        m2 = unlist(map2(l2, cv_frame$test, ~calc_perf(.x, .y))),
        m3 = unlist(map2(l3, cv_frame$test, ~calc_perf(.x, .y))),
        m4 = unlist(map2(l4, cv_frame$test, ~calc_perf(.x, .y))),
        m5 = unlist(map2(l5, cv_frame$test, ~calc_perf(.x, .y, nw = T))))
    )
    }

cv_frame_female <- modelr::crossv_kfold(data[data$female == 1, ] , k = 10)
l5 <- map(cv_frame_all$train, ~model_5(.))
l_cross <- map(cv_frame_female$train, ~model_cross2(.))

mean(unlist(map2(l_cross, cv_frame_female$test, ~calc_perf2(.x, .y))))
mean(unlist(map2(l5, cv_frame_all$test, ~calc_perf2(.x, .y, female = T))), na.rm = T)

mean(unlist(map2(l_cross, cv_frame_nw$test, ~calc_rmse(.x, .y))))
mean(unlist(map2(l5, cv_frame_all$test, ~calc_rmse(.x, .y, nw = T))), na.rm = T)

predict(l_cross[[2]], cv_frame_nw$test[[2]] %>% as.data.frame(), type = "response")

cv_frame_nw$test[[3]] %>% as.data.frame()

lapply(cv_frame_nw$test, FUN = function(x) {as.data.frame(x) %>% pull(black)})
mean(m5)

m5 <- unlist(map2(l5, cv_frame$test, ~calc_perf(.x, .y, nw = T)))

mean(m5, na.rm = T)

calc_perf(l5[[1]], cv_frame$test[[1]], nw = T)

cv_frame <- cv_frame_all
cv_frame_1 <- modelr::crossv_kfold(data[data$black == 1, ] , k = cv_k)
cv_frame_2 <- modelr::crossv_kfold(data[data$black == 0, ], k = cv_k)

l_w[[5]]
l_nw[[5]]
dim(data[data$female == 1,])

gen_cross_frame <- function(cv_frame_1, cv_frame2) {
    l_nw <- map(cv_frame_1$train, ~model_cross(.))
    l_w <- map(cv_frame_2$train, ~model_cross(.))
    return(data.frame(
        ## Dataset first, model second
        nw_nw = unlist(map2(l_nw, cv_frame_1$test, ~calc_perf(.x, .y))),
        nw_w = unlist(map2(l_w, cv_frame_2$test, ~calc_perf(.x, .y))),
        w_w = unlist(map2(l_w, cv_frame_2$test, ~calc_perf(.x, .y))),
        w_nw = unlist(map2(l_nw, cv_frame_2$test, ~calc_perf(.x, .y))) 
    ))

}

colMeans(cross_df)
colMeans(nw_df)

model_overall <- glm(accept_bin ~ 1 + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value +
        denied_PMI, data = data, family = "binomial")

model_2_b <- glm(accept_bin ~ 1 + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value +
        denied_PMI, data = data[data$black == 1, ], family = "binomial")

ggplot(data.frame(pred = predict(model_2_b, data[data$black == 1, ], type = "response")), aes(x = pred)) +
    geom_histogram()

model_2_w <- glm(accept_bin ~ 1 + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value +
        denied_PMI, data = data[data$black == 0,], family = "binomial")

model_full <- glm(accept_bin ~ 1 + self_employed + married + housing_expense_ratio +
        bad_history + PI_ratio + loan_to_value + female + black +
        denied_PMI, data = data, family = "binomial")

ggplot(data.frame(pred = predict(model_2_w, data[data$black == 0, ], type = "response"),
                  actual = data$accept_bin[data$black == 0]),
    aes(x = pred)) +
    geom_histogram()

ggplot(data.frame(pred = predict(model_2_b, data[data$black == 1, ], type = "response"),
                  actual = data$accept_bin[data$black == 1]),
    aes(x = pred)) +
    geom_density() + theme_bw()

ggplot(data.frame(pred = predict(model_full, data, type = "response"),
                  actual = data$accept_bin,
                  black = data$black),
    aes(x = pred, color = as.factor(black), fill = as.factor(black))) +
    geom_density(alpha = 0.6) + theme_bw()


checks <- data.frame(pred = predict(model_full, data, type = "response"),
                     actual = data$accept_bin,
                     black = data$black)
checks$pred_bin <- round(round(checks$pred, 2) * 10)

head(checks)

checks %>%
    group_by(pred_bin, black) %>%
    summarise(mean_actual = mean(actual)) %>%
    View()

model_full %>%
    summary()

ggplot(data.frame(pred = predict(model_full, data, type = "response"),
                  actual = data$accept_bin,
                  black = data$black),
    aes(y = black, x = pred, color = as.factor(actual), fill = as.factor(actual))) +
    geom_jitter(alpha = 0.6) + theme_bw() + ylab("Race")
    #  + scale_y_continuous(labels = c(0, 1), values = c(0, 1))

mean(round(predict(model_2_b, data[data$black == 1, ], type = "response")) == data$accept_bin[data$black == 1])
mean(round(predict(model_overall, data[data$black == 1, ], type = "response")) == data$accept_bin[data$black == 1])

mean((predict(model_2_b, data[data$black == 1, ], type = "response") - data$accept_bin[data$black == 1])^2)
mean((predict(model_overall, data[data$black == 1, ], type = "response") - data$accept_bin[data$black == 1])^2)

    # b_b <- mean(round(predict(model_2_b, black_test_df, type = "response")))
    # b_w <- mean(round(predict(model_2_w, black_test_df, type = "response")))
    # w_b <- mean(round(predict(model_2_b, white_test_df, type = "response")))
    # w_w <- mean(round(predict(model_2_w, white_test_df, type = "response")))

for (i in 1:n_repetition) {
    set.seed(i)
    cv_frame_all <- modelr::crossv_kfold(data, k = cv_k)

    cv_frame_nw <- modelr::crossv_kfold(data[data$black == 1, ] , k = 333)
    cv_frame_w <- modelr::crossv_kfold(data[data$black == 0, ], k = cv_k)

    overall_df <- rbind(overall_df, gen_cv_frame(cv_frame_all))
    nw_df <- rbind(nw_df, gen_cv_frame(cv_frame_nw))
    w_df <- rbind(w_df, gen_cv_frame(cv_frame_w))
    cross_df <- rbind(cross_df, gen_cross_frame(cv_frame_nw, cv_frame_w))
    # black_train_df <- train_df %>%
    #     filter(black == 1)
    # black_test_df <- test_df %>%
    #     filter(black == 1)

    # white_train_df <- train_df %>%
    #     filter(black == 0)
    # white_test_df <- test_df %>%
    #     filter(black == 0)

    
    

    # cross_df <- rbind(cross_df, c(b_b, b_w, w_b, w_w))
    # names(cross_df) <- c("b_b", "b_w", "w_b", "w_w")

    # joint_perf <- c(joint_perf, (b_b * mean(test_df$black)) + (w_w * (1 - mean(test_df$black))))
    # joint_white <- c(joint_white, w_w)
    # joint_black <- c(joint_black, b_b)

    # models <- list(model_0, model_1, model_2, model_3, model_4, model_5)

    # overall_perf <- lapply(models, FUN = function(x) mean(round(predict(x, test_df, type = "response")) == test_df$accept_bin))
    # black_perf <- lapply(models, FUN = function(x) mean((round(predict(x, test_df, type = "response")) == test_df$accept_bin)[test_df$black == 1]))
    # white_perf <- lapply(models, FUN = function(x) mean((round(predict(x, test_df, type = "response")) == test_df$accept_bin)[test_df$black == 0]))
    # female_perf <- lapply(models, FUN = function(x) mean((round(predict(x, test_df, type = "response")) == test_df$accept_bin)[test_df$female == 1]))
    # male_perf <- lapply(models, FUN = function(x) mean((round(predict(x, test_df, type = "response")) == test_df$accept_bin)[test_df$female == 0]))

    # overall_df <- rbind(overall_df, unlist(overall_perf))
    # black_df <- rbind(black_df, unlist(black_perf))
    # white_df <- rbind(white_df, unlist(white_perf))
    # female_df <- rbind(female_df, unlist(female_perf))
    # male_df <- rbind(male_df, unlist(male_perf))
    
}

colMeans(cross_df)

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

results <- readRDS("data/to_plot/mortgage.rds")

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
           model_type = ifelse(grepl("joint", var_name), "Separate models", "One model"))

add_all_mean <- results_group[results_group$sub == "All", c("var_name", "mean")] %>%
    rename(all_mean = mean)

results_group <- results_group %>%
    left_join(add_all_mean, by = "var_name")

results_group$label <- paste0(round(results_group$mean, 3) * 100, "%")
    


rel_df <- results_group %>% filter(grepl("All|Black", sub)) %>% filter(!grepl("m3|m5", var_name))
rel_df$var_name <- c(rep(c("+ Race Dummy", "+ Household\nCharacteristics", "+ Objective\n Scores", "Null\nModel"), 2),
                     "Separate model\nby race", "Separate model\nby race")

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

plot_df <- final_plot_df %>%
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

saveRDS(final_plot_df, "data/to_plot/mortgage_cross.rds")
