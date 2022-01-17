### Model Complexity Mincerian wage example
## Use: Evaluate flexible and specified functional forms OOS
## Author: Mark Verhagen

## Load libraries
library(doParallel)
library(tidyverse)
library(MASS)
library(Metrics)
library(caret)
library(xgboost)

simul_df <- readRDS("mincerian/data/edit/analysis_df.csv")

## Run simulation

n_simul <- 200
cores <- detectCores()
cl <- makeCluster(cores[1] - 2)
registerDoParallel(cl)

results <- foreach(
    i = 1:n_simul, .combine = rbind,
    .packages = c(
        "MASS", "Metrics", "caret", "xgboost",
        "tidyverse"
    )
) %dopar% {
    set.seed(i)
    # make train set
    train_set <- simul_df %>%
        sample_frac(0.8)
    test_set <- simul_df %>%
        filter(!(id %in% train_set$id))
    stopifnot(dim(train_set)[1] + dim(test_set)[1] == dim(simul_df)[1])
    # run models: all four linear functional forms on all four datasets
    lm_1 <- lm_fits(formulas, train_set, test_set, "ln_y_I")[[2]]
    lm_2 <- lm_fits(formulas, train_set, test_set, "ln_y_II")[[2]]
    lm_3 <- lm_fits(formulas, train_set, test_set, "ln_y_III")[[2]]
    lm_4 <- lm_fits(formulas, train_set, test_set, "ln_y_IV")[[2]]

    # run models: GB model on all four datasets
    gb_1 <- fit_xgb(
        train_set = train_set, test_set = test_set,
        y_var = "ln_y_I"
    )
    gb_2 <- fit_xgb(
        train_set = train_set, test_set = test_set,
        y_var = "ln_y_II"
    )
    gb_3 <- fit_xgb(
        train_set = train_set, test_set = test_set,
        y_var = "ln_y_III"
    )
    gb_4 <- fit_xgb(
        train_set = train_set, test_set = test_set, y_var = "ln_y_IV",
        features = c("S", "X", "gender")
    )

    list(
        lm_1[[1]], lm_1[[2]], lm_1[[3]], lm_1[[4]], lm_1[[5]],
        lm_2[[1]], lm_2[[2]], lm_2[[3]], lm_2[[4]], lm_2[[5]],
        lm_3[[1]], lm_3[[2]], lm_3[[3]], lm_3[[4]], lm_3[[5]],
        lm_4[[1]], lm_4[[2]], lm_4[[3]], lm_4[[4]], lm_4[[5]],
        gb_1, gb_2, gb_3, gb_4
    )
}

df_results <- as.data.frame(results)
names(df_results) <- c(
    "lm_1_bench", "lm_1_1", "lm_1_2", "lm_1_3", "lm_1_4",
    "lm_2_bench", "lm_2_1", "lm_2_2", "lm_2_3", "lm_2_4",
    "lm_3_bench", "lm_3_1", "lm_3_2", "lm_3_3", "lm_3_4",
    "lm_4_bench", "lm_4_1", "lm_4_2", "lm_4_3", "lm_4_4",
    "gb_1", "gb_2", "gb_3", "gb_4"
)

i <- names(df_results)[[1]]

for (i in names(df_results)) {
    df_results[[i]] <- as.numeric(df_results[[i]])
}

saveRDS(df_results, "data/final/simul_4models_100.rds")