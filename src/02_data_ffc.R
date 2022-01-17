#####################################
## Plots for:                      ##
## Measuring the Predictability of ##
## Life Outcomes with a Scientific ##
## Mass Collaboration              ##
#####################################

## Code by Ian Lundberg
## ilundberg at princeton dot edu

#####################
## Set seed for    ##
## reproducibility ##
#####################
set.seed(08544)

############################
## Load required packages ##
############################
library(tidyverse)
library(magrittr)
library(haven)
library(forcats)
library(reshape2)
library(foreach)
library(readstata13)
library(Amelia)
library(ranger)

# Set directory information
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")
results.dir <- file.path(here(), "results")

# Set ggplot2 theme
theme_set(theme_bw())

###############
## Load data ##
###############

background <- readstata13::read.dta13("data/ffc/background.dta", convert.factors = F)
train <- read_csv("data/ffc/train.csv")
test <- read_csv("data/ffc/test.csv")
outcomes <- colnames(train)[-1]
submissions <- read_csv("data/ffc/submissions.csv")

###############################
## Benchmarks for supplement ##
###############################

d <- background %>%
  mutate(cm1relf = ifelse(cm1relf == 1, "Married",
                          ifelse(cm1relf == 2, "Cohabiting",
                                 ifelse(cm1relf >= 3, "Other",NA))),
         cm1ethrace = ifelse(cm1ethrace %in% c(1,4), "White/other",
                             ifelse(cm1ethrace == 2, "Black",
                                    ifelse(cm1ethrace == 3, "Hispanic", NA))),
         cm1edu = factor(ifelse(cm1edu >= 1, cm1edu, NA),
                         labels = c("Less than high school",
                                    "High school",
                                    "Some college",
                                    "College")),
         ## For prior measure of GPA, use the teacher report of skills
         ## in language and literacy,
         ## in science and social studies,
         ## and in math,
         ## all coded 1 = far below average to 5 = far above average
         gpa9 = 1/3 * (ifelse(t5c13a > 0, t5c13a, NA) +
                         ifelse(t5c13b > 0, t5c13b, NA) +
                         ifelse(t5c13c > 0, t5c13c, NA)),
         ## For grit, use teacher reports of:
         ## Child persists in completing tasks
         ## Child fails to finish things he or she starts (reverse coded)
         ## Child does not follow through on instructions and fails to finish homework
         ## EXCLUDE Child pays attention well
         ## EXCLUDE Child ignores peer distractions when doing class work
         ## EXCLUDE Child has a short attention span
         ## EXCLUDE Child has distractibility or attention span problem
         grit9 = 1/3 * (ifelse(t5b2b > 0, t5b2b, NA) +
                          ifelse(t5b4y >= 0, 4 - t5b4y, NA) +
                          ifelse(t5b4z >= 0, 4 - t5b4z, NA)),
         materialHardship9 = ifelse(
           m5a2 %in% c(1,2),
           ## Mother's material hardship
           1 / 10 * (
             ifelse(m5f23a > 0, m5f23a == 1, NA) +
               ifelse(m5f23b > 0, m5f23b == 1, NA) +
               ifelse(m5f23c > 0, m5f23c == 1, NA) +
               ifelse(m5f23d > 0, m5f23d == 1, NA) +
               ifelse(m5f23e > 0, m5f23e == 1, NA) +
               ifelse(m5f23f > 0, m5f23f == 1, NA) +
               ifelse(m5f23g > 0, m5f23g == 1, NA) +
               ifelse(m5f23h > 0, m5f23h == 1, NA) +
               ifelse(m5f23i > 0, m5f23i == 1, NA) +
               ifelse(m5f23j > 0, m5f23j == 1, NA)
           ),
           ifelse(f5a2 %in% c(1,2),
                  ## Father's material hardship
                  1 / 10 * (
                    ifelse(f5f23a > 0, f5f23a == 1, NA) +
                      ifelse(f5f23b > 0, f5f23b == 1, NA) +
                      ifelse(f5f23c > 0, f5f23c == 1, NA) +
                      ifelse(f5f23d > 0, f5f23d == 1, NA) +
                      ifelse(f5f23e > 0, f5f23e == 1, NA) +
                      ifelse(f5f23f > 0, f5f23f == 1, NA) +
                      ifelse(f5f23g > 0, f5f23g == 1, NA) +
                      ifelse(f5f23h > 0, f5f23h == 1, NA) +
                      ifelse(f5f23i > 0, f5f23i == 1, NA) +
                      ifelse(f5f23j > 0, f5f23j == 1, NA)
                  ),
                  ## PCG material hardship
                  1 / 10 * (
                    ifelse(n5g1a > 0, n5g1a == 1, NA) +
                      ifelse(n5g1b > 0, n5g1b == 1, NA) +
                      ifelse(n5g1c > 0, n5g1c == 1, NA) +
                      ifelse(n5g1d > 0, n5g1d == 1, NA) +
                      ifelse(n5g1e > 0, n5g1e == 1, NA) +
                      ifelse(n5g1f > 0, n5g1f == 1, NA) +
                      ifelse(n5g1g > 0, n5g1g == 1, NA) +
                      ifelse(n5g1h > 0, n5g1h == 1, NA) +
                      ifelse(n5g1i > 0, n5g1i == 1, NA) +
                      ifelse(n5g1j > 0, n5g1j == 1, NA)
                  ))
         ),
         eviction9 = ifelse(m5a2 %in% c(1,2),
                            ifelse(m5f23d <= 0, NA, m5f23d == 1),
                            ifelse(f5a2 %in% c(1,2),
                                   ifelse(f5f23d <= 0, NA, f5f23d == 1),
                                   NA)),
         ## Use whether did work for pay the week of the age 9 interview
         layoff9 = ifelse(m5a2 %in% c(1,2),
                          ifelse(m5i4 > 0, m5i4 == 2, NA),
                          ifelse(f5a2 %in% c(1,2),
                                 ifelse(f5i4 > 0, f5i4 == 2, NA),
                                 NA)),
         jobTraining9 = ifelse(m5a2 %in% c(1,2),
                               ifelse(m5i3b > 0, m5i3b == 1, NA),
                               ifelse(f5a2 %in% c(1,2),
                                      ifelse(f5i3b > 0, f5i3b == 1, NA),
                                      NA))) %>%
  dplyr::select(challengeID, cm1ethrace, cm1relf, cm1edu,
         gpa9, grit9, materialHardship9, eviction9, layoff9, jobTraining9) %>%
  left_join(train, by = "challengeID")

## For one row that is missing everything, fill in that race is white/other
## so we can impute everything from there. This case is likely missing
## in the test set and has no training outcomes, so this won't matter.

d[apply(d[,-1],1,function(x) all(is.na(x))),"cm1ethrace"] <- "White/other"

####################
## Fit benchmarks ##
####################

## Function to make OLS predictions
get.benchmark.predictions <- function(outcome, model = "full", data = d) {
  if(model == "full") {
    thisFormula <- formula(paste0(outcome,
                                  " ~ cm1ethrace + cm1relf + cm1edu + ",
                                  outcome,"9"))
    imputed <- amelia(data %>% dplyr::select(challengeID, cm1ethrace, cm1relf, cm1edu, contains(outcome)),
                      m = 1,
                      noms = c("cm1ethrace","cm1relf"),
                      ords = "cm1edu",
                      idvars = "challengeID")$imputations$imp1
  } else if (model == "lagged") {
    thisFormula <- formula(paste0(outcome,
                                  " ~ ",
                                  outcome,"9"))
    imputed <- amelia(data %>% dplyr::select(challengeID, contains(outcome)),
                      m = 1,
                      idvars = "challengeID")$imputations$imp1
  } else if (model == "demographic") {
    imputed <- amelia(data %>% 
                        dplyr::select(challengeID, cm1ethrace, cm1relf, cm1edu, contains(outcome)) %>%
                        dplyr::select(-contains(paste0(outcome,9))),
                      m = 1,
                      noms = c("cm1ethrace","cm1relf"),
                      ords = "cm1edu",
                      idvars = "challengeID")$imputations$imp1
    thisFormula <- formula(paste0(outcome,
                                  " ~ cm1ethrace + cm1relf + cm1edu"))
  }
  
  # Identify the rows that are missing all predictors
  # This happens if there were no variables with valid values,
  # giving Amelia no data with which to impute.
  missing_all_predictors <- apply(get_all_vars(thisFormula, data = imputed), 1, function(x) all(is.na(x[-1])))
  # Create holders for the predicted values from the models
  ols.yhat <- logit.yhat <- rf.yhat <- rep(NA, nrow(imputed))
  
  # If missing all predictors, impute the grand mean
  ols.yhat[missing_all_predictors] <- 
    logit.yhat[missing_all_predictors] <- 
    rf.yhat[missing_all_predictors] <- 
    mean(imputed[,outcome], na.rm = T)
  
  # Fit models to impute predictions when predictors are available
  
  # OLS
  ols <- lm(formula = thisFormula,
            data = imputed[!is.na(data[,outcome]),])
  ols.yhat[!missing_all_predictors] <- predict(ols, newdata = imputed[!missing_all_predictors,])
  
  # Logit for binary outcomes
  if (length(unique(na.omit(data[,outcome]))) == 2) {
    logit <- glm(formula = thisFormula,
                 family = binomial(link = "logit"),
                 data = imputed[!is.na(data[,outcome]),])
    logit.yhat[!missing_all_predictors] <- predict(logit, newdata = imputed[!missing_all_predictors,], type = "response") ## Post correction of PNAS paper
  } else {
    # If not binary, make all logit predictions NA
    logit.yhat <- NA
  }
  
  # Random forest
  rf <- ranger(thisFormula,
               data = imputed[!is.na(data[,outcome]),])
  rf.yhat[!missing_all_predictors] <- predict(rf, data = imputed[!missing_all_predictors,])$predictions
  
  # Combine into one data frame
  # and truncate to observable range
  all_predictions <- data.frame(outcome = outcome,
                                challengeID = imputed$challengeID,
                                ols = ols.yhat,
                                logit = logit.yhat,
                                rf = rf.yhat) %>%
    mutate(ols = case_when(outcome %in% c("grit","gpa") & ols < 1 ~ 1,
                           outcome %in% c("grit","gpa") & ols > 4 ~ 4,
                           outcome %in% c("grit","gpa") ~ ols,
                           ols < 0 ~ 0,
                           ols > 1 ~ 1,
                           T ~ ols),
           logit = case_when(logit < 0 ~ 0,
                             logit > 1 ~ 1,
                             T ~ as.numeric(logit)),
           rf = case_when(outcome %in% c("grit","gpa") & rf < 1 ~ 1,
                          outcome %in% c("grit","gpa") & rf > 4 ~ 4,
                          outcome %in% c("grit","gpa") ~ rf,
                          rf < 0 ~ 0,
                          rf > 1 ~ 1,
                          T ~ rf))
  return(all_predictions)
}

gpa_benchmark_predictions <- benchmarks %>%
  filter(outcome == "gpa") %>%
  saveRDS("data/edit/ffc_gpa_predictions_baseline.rds")

# Get benchmarks on all outcomes
benchmarks <- foreach(thisOutcome = outcomes, .combine = "rbind") %do% {
  foreach(predictor_set = c("full","demographic","lagged"), .combine = "rbind") %do% {
    get.benchmark.predictions(thisOutcome, model = predictor_set) %>%
      mutate(predictors = predictor_set)
  }
}

# Output a version stored like submissions.csv
# Matt will use this in his figures
benchmarks_long <- benchmarks %>%
  dplyr::select(challengeID, outcome, ols, logit, rf, predictors) %>%
  melt(id = c("challengeID", "outcome", "predictors"),
       variable.name = "account",
       value.name = "prediction") %>%
  mutate(account = paste("benchmark", account, predictors, sep = "_")) %>%
  dplyr::select(-predictors) %>%
  # Add information so that this is formatted like submissions.csv
  right_join(
    test %>%
      melt(id = "challengeID", variable.name = "outcome", value.name = "truth") %>%
      dplyr::select(challengeID, outcome, truth),
    by = c("challengeID","outcome")
  ) %>%
  left_join(
    train %>%
      melt(id = "challengeID", variable.name = "outcome") %>%
      group_by(outcome) %>%
      summarize(ybar_train = mean(value, na.rm = T)),
    by = c("outcome")
  ) %>%
  group_by(outcome, account) %>%
  mutate(r2_holdout = 1 - mean((truth - prediction) ^ 2, na.rm = T) / mean((truth - ybar_train) ^ 2, na.rm = T),
         beatingBaseline = r2_holdout > 0,
         outcome_name = case_when(outcome == "materialHardship" ~ "A. Material\nhardship",
                                  outcome == "gpa" ~ "B. GPA",
                                  outcome == "grit" ~ "C. Grit",
                                  outcome == "eviction" ~ "D. Eviction",
                                  outcome == "jobTraining" ~ "E. Job\ntraining",
                                  outcome == "layoff" ~ "F. Layoff")) %>%
  dplyr::select(outcome, outcome_name, account, challengeID, prediction, truth, ybar_train, r2_holdout, beatingBaseline) %>%
  arrange(outcome_name, account, challengeID)

gpa_benchmark <- benchmarks_long %>%
  filter(outcome == "gpa") %>%
  filter(account == "benchmark_ols_full")

gpa_benchmark %>%
  # filter(outcome == "gpa") %>%
  saveRDS("data/edit/ffc_gpa_predictions_baseline.rds")

gpa_benchmark <- benchmarks_long %>%
  filter(outcome == "gpa") %>%
  saveRDS("data/edit/ffc_gpa_predictions_baseline_full.rds")

write_csv(
    benchmarks_long,
    path = file.path(data.dir, "intermediate_files", "benchmarks_long.csv")
)

estimates_with_intervals <- foreach(outcome_case = outcomes, .combine = "rbind") %do% {
  squared_errors <- submissions %>% 
    bind_rows(benchmarks_long) %>%
    bind_rows(submissions %>%
                group_by(outcome, challengeID) %>%
                filter((1:n()) == 1) %>%
                group_by(outcome) %>%
                mutate(prediction = ybar_train,
                       account = "baseline",
                       r2_holdout = 0,
                       beatingBaseline = F)) %>%
    filter(outcome == outcome_case) %>%
    # Remove cases where all predictions are within 10^-4 of the baseline
    group_by(account) %>%
    mutate(predicts_baseline = all(abs(prediction - ybar_train) < 10^-4)) %>%
    filter(!predicts_baseline | account == "baseline") %>%
    filter(!is.na(truth)) %>%
    mutate(sq_error = (truth - prediction) ^ 2) %>%
    dplyr::select(challengeID, account, sq_error) %>%
    spread(key = account, value = sq_error) %>%
    dplyr::select(-challengeID)
  
  # Move baseline to the front and benchmarks to the back
  squared_errors <- squared_errors[,c("baseline",
                                      colnames(squared_errors[!(grepl("baseline|benchmark",colnames(squared_errors)))]),
                                      colnames(squared_errors[(grepl("benchmark",colnames(squared_errors)))]))]
  
  # Calculate point estimates
  r2_point <- 1 - colMeans(squared_errors) / colMeans(squared_errors[,"baseline"])
  
  # Calculate point estimates on 10000 bootstrap samples
  r2_boot <- foreach(i = 1:10000, .combine = "rbind") %do% {
    chosen <- sample(1:nrow(squared_errors), replace = T)
    1 - colMeans(squared_errors[chosen,]) / colMeans(squared_errors[chosen,"baseline"])
  }
  
  # Identify the columns that are submissions or baseline
  notBenchmark_indicators <- !grepl("benchmark",colnames(squared_errors))
  # Identify the columns that are submissions
  submission_indicators <- !grepl("baseline|benchmark",colnames(squared_errors))
  
  # Identify submission columns for which all of the squared errors are the same
  # as a previously seen submission column or the baseline
  same <- rep(NA, ncol(squared_errors))
  # For all columns that are not submissions, treat as not duplicates
  same[!submission_indicators] <- F
  # Loop through columns that are submissions and indicate duplicates
  for(i in which(submission_indicators)) {
    this_column <- squared_errors[,i]
    previous_columns <- squared_errors[,1:(i-1)]
    if (i == 2) {
      same[i] <- all(abs(this_column - previous_columns) < 10^-4)
    } else {
      same[i] <- any(apply(previous_columns, 2, function(x) all(abs(this_column - x) < 10^-4)))
    }
  }
  
  # Calculate the maximum R^2
  r2_point_max <- max(r2_point[submission_indicators & !same])
  # Calculate point estimates for comparisons to benchmarks
  if (outcome_case %in% c("gpa","grit","materialHardship")) {
    benchmark <- "benchmark_ols_full"
  } else {
    benchmark <- "benchmark_logit_full"
  }
  point_difference <- r2_point_max - r2_point[benchmark]
  point_performanceMultiplying <- r2_point_max / r2_point[benchmark]
  point_gapClosing <- (r2_point_max - r2_point[benchmark]) / (1 - r2_point[benchmark])
  
  # BOOTSTRAP CIs
  # First: CI for every submission and every benchmark
  ci_all_bootstrap <- apply(r2_boot, 2, function(x) sort(x)[c(250,9750)])
  # Take the best submission in each bootstrap draw
  best_score_draws <- apply(r2_boot[,submission_indicators], 1, max)
  # Make a CI for the maximum R^2
  ci_max_bootstrap <- sort(best_score_draws)[c(250,9750)]
  # Make CI for comparisons to benchmark
  # The bootstrap is the only way we construct these CIs
  ci_difference_bootstrap <- sort(best_score_draws - r2_boot[,benchmark])[c(250,9750)]
  ci_performanceMultiplying_bootstrap <- sort(best_score_draws / r2_boot[,benchmark])[c(250,9750)]
  ci_gapClosing_bootstrap <- sort((best_score_draws - r2_boot[,benchmark]) / (1 - r2_boot[,benchmark]))[c(250,9750)]
  
  # Prepare to return
  rownames(ci_all_bootstrap) <- 
    names(ci_max_bootstrap) <-
    names(ci_difference_bootstrap) <-
    names(ci_performanceMultiplying_bootstrap) <-
    names(ci_gapClosing_bootstrap) <-
    c("ci.min", "ci.max")
  
  return(
    data.frame(t(ci_all_bootstrap),
               point = r2_point,
               account = names(r2_point),
               method = "bootstrap") %>%
      bind_rows(data.frame(t(ci_max_bootstrap), point = r2_point_max, account = "max", method = "bootstrap")) %>%
      bind_rows(data.frame(t(ci_difference_bootstrap), point = point_difference, account = "difference", method = "bootstrap")) %>%
      bind_rows(data.frame(t(ci_performanceMultiplying_bootstrap), point = point_performanceMultiplying, account = "performanceMultiplying", method = "bootstrap")) %>%
      bind_rows(data.frame(t(ci_gapClosing_bootstrap), point = point_gapClosing, account = "gapClosing", method = "bootstrap")) %>%
      mutate(outcome = outcome_case)
  )
} %>%
  mutate(outcome_name = case_when(outcome == "materialHardship" ~ "Material\nhardship",
                                  outcome == "gpa" ~ "GPA",
                                  outcome == "grit" ~ "Grit",
                                  outcome == "eviction" ~ "Eviction",
                                  outcome == "jobTraining" ~ "Job\ntraining",
                                  outcome == "layoff" ~ "Layoff"),
         outcome_name = fct_relevel(outcome_name, "Material\nhardship", "GPA", "Grit",
                                    "Eviction", "Job\ntraining", "Layoff"))
write_csv(estimates_with_intervals,
          path = file.path(data.dir, "intermediate_files", "estimates_with_intervals.csv"))

#######################################
# MAIN PERFORMANCE PLOT OF MAIN PAPER #
#######################################
estimates_with_intervals %>%
  filter((account == "max" & method == "bootstrap")) %>%
  left_join(estimates_with_intervals %>%
              filter((account == "benchmark_ols_full" & outcome %in% c("gpa","grit","materialHardship")) |
                       (account == "benchmark_logit_full" & outcome %in% c("eviction","layoff","jobTraining"))) %>%
              rename(benchmark = point) %>%
              dplyr::select(outcome, benchmark, on=outcome_name),
            by = "outcome") %>%
  mutate(outcome_name = gsub(".) ","",outcome_name),
         outcome_name = fct_reorder(outcome_name,-point)) %>%
  write.csv("data/ffc/main_plot.csv")

############################################
## Generate individual predictions for GPA #
############################################
## Code by Mark Verhagen

## Read train and test set from FFC reproduction package
df_train <- train
df_test <- test

## Read full background set from FFC reprooduction package
background_new <- background

## Transform missingness into NA
background_new <- background_new %>%
  mutate(t5c13a_code = ifelse(as.numeric(t5c13a) <= 0, NA, as.numeric(t5c13a)),
         t5c13b_code = ifelse(as.numeric(t5c13b) <= 0, NA, as.numeric(t5c13b)),
         t5c13c_code = ifelse(as.numeric(t5c13c) <= 0, NA, as.numeric(t5c13c))) %>%
  mutate(gpa_age9 = rowMeans(dplyr::select(., c("t5c13a_code", "t5c13b_code", "t5c13c_code")), na.rm = T)) %>%
  filter(!is.na(gpa_age9))

## Generate trainset
df_train_features <- background_new %>%
  filter(challengeID %in% df_train$challengeID) %>%
  mutate(mother_race = cm1ethrace,
         mother_educ = cm1edu,
         mother_relation = cm1relf,
         age9_gpa_a = t5c13a,
         age9_gpa_b = t5c13b,
         age9_gpa_c = t5c13c)

## Generate testset
df_test_features <- background_new %>%
  filter(challengeID %in% df_test$challengeID) %>%
  mutate(mother_race = cm1ethrace,
         mother_educ = cm1edu,
         mother_relation = cm1relf,
         age9_gpa_a = t5c13a,
         age9_gpa_b = t5c13b,
         age9_gpa_c = t5c13c) %>%
  filter(mother_educ >= 0) %>%
  left_join(df_test %>% dplyr::select(challengeID, gpa)) %>%
  filter(!is.na(gpa))

## Generate simple versions of train and join train outcome
df_simple_train <- df_train_features %>%
  dplyr::select(challengeID, mother_race, mother_educ, mother_relation, age9_gpa_a,
                age9_gpa_b, age9_gpa_c, gpa_age9) %>%
  left_join(df_train %>% dplyr::select(challengeID, gpa)) %>%
  filter(!is.na(gpa))

lm_nul <- lm(gpa ~ 1, data = df_simple_train)

lm_gpa <- lm(gpa ~ 1 + as.factor(mother_race) + as.factor(mother_educ) + as.factor(mother_relation),
             data = df_simple_train)

lm_gpa2 <- lm(gpa ~ 1 + as.factor(mother_race) + as.factor(mother_educ) + as.factor(mother_relation) + gpa_age9,
              data = df_simple_train)

## Make predictions using the null model, the model with household characteristics, and the full benchmark
results <- data.frame(test = df_test_features$gpa,
                      nul = predict(lm_nul, df_test_features),
                      lm1 = predict(lm_gpa, df_test_features),
                      lm2 = predict(lm_gpa2, df_test_features))

## Transform predictions for plotting
results_melt <- reshape2::melt(results, id.vars = c("test", "nul")) %>%
  mutate(variable = gsub("lm1", "Household", variable),
         variable = gsub("lm2", "Household +\nlagged GPA", variable))

saveRDS(results_melt, "data/to_plot/ffc_gpa.rds")


########################################
## Plot each team's predictive accuracy #
########################################
## Code by Mark Verhagen

submissions <- read.csv("data/ffc/submissions.csv")
main_plot_ffc <- read.csv("data/ffc/main_plot.csv")

## Use new benchmarks (post correction of PNAS paper)
main_plot_ffc_new <- main_plot_ffc %>%
  dplyr::select(outcome, benchmark)

sub_df <- submissions %>%
  group_by(account, outcome) %>%
  summarise(r2_holdout = unique(r2_holdout),
            r2_bench = unique(ybar_train)) %>%
  filter(r2_holdout > -0.2) %>%
  ungroup() %>%
  mutate(type = "Submission") %>%
  left_join(main_plot_ffc %>%
              dplyr::select(outcome, benchmark)) %>%
  mutate(`Improved on\nBenchmark` = ifelse(r2_holdout > benchmark,
                                          "True", "False"))

sub_df$outcome <- factor(sub_df$outcome,
  levels = unique(sub_df$outcome[order(sub_df$benchmark, decreasing = T)])
)

saveRDS(sub_df, "data/to_plot/ffc_all_submissions.rds")
