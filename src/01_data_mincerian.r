## Script to simulate wage and educational data for the Prediction paper

## Load libraries
library(foreign)
library(xgboost)
library(tidyverse)
library(caret)
library(randomForest)
library(doParallel)

source("src/functions.r")

# read data from 2018 GSS to use as sampling distribution
data <- read.dta("data/mincerian/GSS2018.dta")
n_obs <- 50000
set.seed(1704)

# transform data to only include working age individuals and
# include broad bins to sample from
df <- data %>%
  filter(age >= 18,
         age <= 65,
         !is.na(educ)) %>%
  mutate(age_cat = ifelse(age < 25, "18_24",
      ifelse(age < 35, "25_34",
          ifelse(age < 50, "35_49", "50_65")
      )
  ))

# generate a sampling frame based on education and age category
# not using each age separately to ensure reasonable distributions
educ_age_props <- count(df, educ, age_cat, wt = wtss) %>%
    mutate(
        prop = n / sum(n),
        cum_prop = cumsum(prop)
    )

# sample from the educ x age categories and draw actual ages uniformly
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

# generate independent variables for regression analysis
analysis_df <- new_sample %>%
    mutate(
        S = as.numeric(educ),
        X = age - educ - 6,
        X_sq = X^2
    ) %>%
  rowwise() %>%
  mutate(S_0_8 = min(S, 8),
         S_9_10 = min(2, max(S - 8, 0)),
         S_11_12 = min(2, max(S - 10, 0)),
         S_13_14 = min(2, max(S - 12, 0)),
         S_15p = max(S - 14, 0)) %>%
  ungroup()

# randomly assign rows to male or female for gender illustration
analysis_df$gender <- ifelse(runif(dim(analysis_df)[1], 0, 1) > 0.5, 1, 0)


# simulate log wage based on linear additive models based on recent research
simul_df <- analysis_df %>%
    mutate(
        epsilon = rnorm(dim(analysis_df)[1], 0, 0.1),
        ln_y_I = 4.5 + 0.125 * S + 0.09 * X + epsilon,
        ln_y_II = 4.5 + 0.125 * S + 0.09 * X - 0.001 * X^2 + epsilon,
        ln_y_III = 4.5 + 0.02 * S_0_8 + 0.03 * S_9_10 + 0.30 * S_11_12 +
            0.06 * S_13_14 + 0.06 * S_15p + 0.09 * X - 0.001 * X^2 + epsilon
        ) %>%
    mutate(id = 1:dim(analysis_df)[1])

# define functional forms to estimate
form_0 <- as.formula("ln_y ~ 1")
form_1 <- as.formula("ln_y ~ 1 + S + X")
form_2 <- as.formula("ln_y ~ 1 + S + X + X_sq")
form_3 <- as.formula("ln_y ~ 1 + S_0_8 + S_9_10 + S_11_12 + S_13_14 + S_15p + X + X_sq")

# save list of functional forms
formulas <- list(form_0, form_1, form_2, form_3)

# run simulation to generate n_simul datasets and analyze OOS performance

n_simul <- 1000
cores <- detectCores()
cl <- makeCluster(cores[1] - 2)
registerDoParallel(cl)

results <- foreach(i = 1 : n_simul, .combine = rbind,
                   .packages = c("MASS", "Metrics", "caret", "xgboost",
                   "tidyverse")) %dopar% {
  # set seed equal to loop to ensure ability to re-engineer a specific draw's data
  set.seed(i)
  # make train set
  train_set <- simul_df %>%
    sample_frac(0.7)
  test_set <- simul_df %>%
    filter(!(id %in% train_set$id))
  stopifnot(dim(train_set)[1] + dim(test_set)[1] == dim(simul_df)[1])  
  
  # run models: all four linear functional forms on all four datasets
  lm_1 <- LM_fits(formulas, train_set, test_set, "ln_y_I")[[2]]
  lm_2 <- LM_fits(formulas, train_set, test_set, "ln_y_II")[[2]]
  lm_3 <- LM_fits(formulas, train_set, test_set, "ln_y_III")[[2]]

  # run models: GB model on all four datasets
  gb_1 <- fit_xgb(train_set = train_set, test_set = test_set, y_var = "ln_y_I")
  gb_2 <- fit_xgb(train_set = train_set, test_set = test_set, y_var = "ln_y_II")
  gb_3 <- fit_xgb(train_set = train_set, test_set = test_set, y_var = "ln_y_III")

  list(
      lm_1[[1]], lm_1[[2]], lm_1[[3]], lm_1[[4]],
      lm_2[[1]], lm_2[[2]], lm_2[[3]], lm_2[[4]],
      lm_3[[1]], lm_3[[2]], lm_3[[3]], lm_3[[4]],
      gb_1, gb_2, gb_3
  )
}

df_results <- as.data.frame(results)
names(df_results) <- c(
    "lm_1_bench", "lm_1_1", "lm_1_2", "lm_1_3",
    "lm_2_bench", "lm_2_1", "lm_2_2", "lm_2_3",
    "lm_3_bench", "lm_3_1", "lm_3_2", "lm_3_3",
    "gb_1", "gb_2", "gb_3"
)

i <- names(df_results)[[1]]

for (i in names(df_results)) {
    df_results[[i]] <- as.numeric(df_results[[i]])
}

df_results <- readRDS("data/to_plot/mincerian.rds")
df <- df_results %>%
  mutate(lm_1_1 = 1 - (lm_1_1 / lm_1_bench),
         lm_1_2 = 1 - (lm_1_2 / lm_1_bench),
         lm_1_3 = 1 - (lm_1_3 / lm_1_bench),
         lm_2_1 = 1 - (lm_2_1 / lm_2_bench),
         lm_2_2 = 1 - (lm_2_2 / lm_2_bench),
         lm_2_3 = 1 - (lm_2_3 / lm_2_bench),
         lm_3_1 = 1 - (lm_3_1 / lm_3_bench),
         lm_3_2 = 1 - (lm_3_2 / lm_3_bench),
         lm_3_3 = 1 - (lm_3_3 / lm_3_bench),
         gb_1 = 1 - (gb_1 / lm_1_bench),
         gb_2 = 1 - (gb_3 / lm_2_bench),
         gb_3 = 1 - (gb_3 / lm_3_bench)) %>%
  dplyr::select(-lm_1_bench, -lm_2_bench, -lm_3_bench)

df_melt <- df %>%
  reshape2::melt() %>%
  mutate(dataset = ifelse(grepl("lm_1|gb_1", variable), "Dataset I",
                          ifelse(grepl("lm_2|gb_2", variable), "Dataset II",
                                 ifelse(grepl("lm_3|gb_3", variable), "Dataset III",
                                        "Else"))),
         model = ifelse(grepl("\\d{1}_1", variable), "Linear I",
                        ifelse(grepl("\\d{1}_2", variable), "Linear II",
                               ifelse(grepl("\\d{1}_3", variable), "Linear III",
                                      ifelse(grepl("gb", variable), "XGBoost",
                                            "Other")))))

df_plot <- df_melt %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            sd = sd(value),
            model = unique(model),
            dataset = unique(dataset))

df_plot$model <- factor(df_plot$model, levels = c("XGBoost", "Linear I", "Linear II", "Linear III"))

saveRDS(df_plot, "data/to_plot/mincerian.rds")
