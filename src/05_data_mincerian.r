### 05_data_mincerian.R
## Author: Mark Verhagen
##
## Description: Code to generate simulation results for the
## mincerian wage example
##
## Data inputs:
## - data/mincerian/GSS2018.dta
##
## Data outputs:
## - data/mincerian/perf_holdout.rds
###

## Load libraries
library(foreign)
library(xgboost)
library(tidyverse)
library(caret)
library(randomForest)

# source("functions.r")
# setup
n_obs <- 50000
set.seed(1704)

# read data from 2018 GSS

data <- read.dta("./data/mincerian/GSS2018.dta")

# transform data
df <- data %>%
    filter(
        age >= 18,
        age <= 65,
        !is.na(educ)
    ) %>%
    mutate(age_cat = ifelse(age < 25, "18_24",
        ifelse(age < 35, "25_34",
            ifelse(age < 50, "35_49", "50_65")
        )
    ))

# make larger sample
educ_age_props <- count(df, educ, age_cat, wt = wtss) %>%
    mutate(
        prop = n / sum(n),
        cum_prop = cumsum(prop)
    )

# sample actual ages
new_sample <- sample_n(educ_age_props, size = n_obs, weight = n, replace = T)

new_sample$age <- ifelse(new_sample$age_cat == "18_24",
    round(runif(dim(new_sample)[1], 18, 24)),
    ifelse(new_sample$age_cat == "25_34",
        round(runif(dim(new_sample)[1], 25, 34)),
        ifelse(new_sample$age_cat == "35_49",
            round(runif(dim(new_sample)[1], 35, 49)),
            round(runif(dim(new_sample)[1], 50, 65))
        )
    )
)

# generate independent variables
analysis_df <- new_sample %>%
    mutate(
        S = as.numeric(educ),
        X = ifelse(educ >= 10, age - educ - 4, age - 16),
        X = ifelse(X < 0, 0, X),
        X_sq = X^2
    ) %>%
    rowwise() %>%
    mutate(
        S_0_8 = min(S, 8),
        S_9_10 = min(2, max(S - 8, 0)),
        S_11_12 = min(2, max(S - 10, 0)),
        S_13_14 = min(2, max(S - 12, 0)),
        S_15p = max(S - 14, 0)
    ) %>%
    ungroup()

# simulate log wage based on linear additive models
simul_df <- analysis_df %>%
    mutate(
        epsilon = rnorm(dim(analysis_df)[1], 0, 0.12),
        ln_y_I = 4.5 + 0.125 * S + 0.09 * X + rnorm(dim(analysis_df)[1], 0, 0.12),
        ln_y_II = 4.5 + 0.125 * S + 0.09 * X - 0.001 * X^2 + rnorm(dim(analysis_df)[1], 0, 0.07),
        ln_y_III = 4.5 + 0.02 * S_0_8 + 0.03 * S_9_10 + 0.30 * S_11_12 +
            0.06 * S_13_14 + 0.06 * S_15p + 0.09 * X - 0.001 * X^2 + rnorm(dim(analysis_df)[1], 0, 0.07)
    ) %>%
    mutate(id = 1:dim(analysis_df))

# write.csv(simul_df, "data/edit/analysis_df.csv")

train_set <- simul_df %>%
    sample_frac(0.8)
test_set <- simul_df %>%
    filter(!(id %in% train_set$id))

# functional forms
form_0 <- as.formula("ln_y ~ 1")
form_1 <- as.formula("ln_y ~ 1 + S + X")
form_2 <- as.formula("ln_y ~ 1 + S + X + X_sq")
form_3 <- as.formula("ln_y ~ 1 + S_0_8 + S_9_10 + S_11_12 + S_13_14 + S_15p + X + X_sq")

formulas <- list(form_0, form_1, form_2, form_3)
data_sets <- list(
    train_set %>% mutate(ln_y = ln_y_I),
    train_set %>% mutate(ln_y = ln_y_II),
    train_set %>% mutate(ln_y = ln_y_III)
)

lm_1 <- LM_fits(formulas, train_set, test_set, "ln_y_I")[[2]]
lm_2 <- LM_fits(formulas, train_set, test_set, "ln_y_II")[[2]]
lm_3 <- LM_fits(formulas, train_set, test_set, "ln_y_III")[[2]]

gb_1 <- fit_xgb(train_set = train_set, test_set = test_set, y_var = "ln_y_I")
gb_2 <- fit_xgb(train_set = train_set, test_set = test_set, y_var = "ln_y_II")
gb_3 <- fit_xgb(train_set = train_set, test_set = test_set, y_var = "ln_y_III")

df_results <-
    list(
        lm_1[[1]], lm_1[[2]], lm_1[[3]], lm_1[[4]],
        lm_2[[1]], lm_2[[2]], lm_2[[3]], lm_2[[4]],
        lm_3[[1]], lm_3[[2]], lm_3[[3]], lm_3[[4]],
        gb_1, gb_2, gb_3
    ) %>%
    as.data.frame()

names(df_results) <- c(
    "lm_1_bench", "lm_1_1", "lm_1_2", "lm_1_3",
    "lm_2_bench", "lm_2_1", "lm_2_2", "lm_2_3",
    "lm_3_bench", "lm_3_1", "lm_3_2", "lm_3_3",
    "gb_1", "gb_2", "gb_3"
)

df_results <- df_results %>%
    mutate(
        lm_1_1 = 1 - (lm_1_1 / lm_1_bench),
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
        gb_3 = 1 - (gb_3 / lm_3_bench)
    ) %>%
    dplyr::select(-lm_1_bench, -lm_2_bench, -lm_3_bench)

saveRDS(df_results, "data/mincerian/perf_holdout.rds")
