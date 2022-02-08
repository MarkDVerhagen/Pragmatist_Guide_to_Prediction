### 02_data_mortgage.R
## Author: Mark Verhagen
##
## Description: Code to generate results for the
## mortgage example
##
## Data inputs:
## - data/mortgage/mortgage.dta
##
## Data outputs:
## - data/mortgage/mortgage_results.rda
###

library(foreign)
library(tidyverse)
library(assertthat)

source("./src/functions.R")

## Drop missing
data <- read.dta("./data/mortgage/mortgage.dta") %>%
    mutate(accept = accept * 100) %>%
    filter(
        !is.na(female),
        !is.na(accept),
        !is.na(married),
        !is.na(PI_ratio)
    )

n_repetition <- 100
n <- dim(data)[1]
cv_k <- 5
data$accept_bin <- ifelse(data$accept > 0, 1, 0)

## Setup empty dataframe
overall_df <- nw_df <- w_df <- do_df <- do_df_prob <- c()

for (i in 1:n_repetition) {
    set.seed(i)
    cv_frame_all <- modelr::crossv_kfold(group_by(data, black), k = cv_k)

    # cv_frame_nw <- modelr::crossv_kfold(data[data$black == 1, ] , k = cv_k)
    # cv_frame_w <- modelr::crossv_kfold(data[data$black == 0, ], k = cv_k)

    overall_df <- rbind(overall_df, gen_cv_frame(cv_frame_all))
    nw_df <- rbind(nw_df, gen_cv_frame(cv_frame_all, nw = T))
    w_df <- rbind(w_df, gen_cv_frame(cv_frame_all, w = T))
    # cross_df <- rbind(cross_df, gen_cross_frame(cv_frame_nw, cv_frame_w))
    do_df_prob <- rbind(do_df_prob, gen_do_frame(cv_frame_all, prob = T))
    do_df <- rbind(do_df, gen_do_frame(cv_frame_all, prob = F))
}

set.seed(1704)
models <- map(cv_frame_all$train, ~ model_4(.))

w_pred <- data.table::rbindlist(map2(
    models, cv_frame_all$test,
    ~ calc_prob_single(.x, .y, prob = prob, w = T)
))

nw_pred <- data.table::rbindlist(map2(
    models, cv_frame_all$test,
    ~ calc_prob_single(.x, .y, prob = prob, w = F)
))

save(nw_df, w_df, overall_df, do_df, do_df_prob, w_pred, nw_pred,
    file = "data/mortgage/mortgage_results.rda"
)