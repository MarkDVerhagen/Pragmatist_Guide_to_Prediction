library(caret)

## 01_data_mincerian.R

fit_xgb <- function(train_set, test_set, y_var = "ln_y_I",
features = c("S", "X")) {
  train_outcome <- train_set[[y_var]]
  train_data <- xgb.DMatrix(
      data = as.matrix(train_set %>% dplyr::select(all_of(features))),
      label = train_outcome
  )
  test_outcome <- test_set[[y_var]]
  test_data <- xgb.DMatrix(
      data = as.matrix(test_set %>% dplyr::select(all_of(features))),
      label = test_outcome
  )
  nround    <- 50 # number of XGBoost rounds
  xg_model <- xgb.train(data = train_data,
                        nrounds = nround)
  return(caret::RMSE(test_set[[y_var]], predict(xg_model, test_data)))
}

LM_fits <- function(formulas, train, test, var = "ln_y_I") {
    test$ln_y <- test[[var]]
    train$ln_y <- train[[var]]
    fits <- lapply(formulas, FUN = function(x) {
        lm(x, data = train)
    })
    perfs <- lapply(fits, FUN = function(x) {
        caret::RMSE(test$ln_y, predict(x, test_set))
    })
    return(list(fits, perfs))
}

## 03_data_mortgage.R

model_fit <- function(data) {
    return(lm(accept ~ 1 + female + black + housing_expense_ratio +
             self_employed + married + bad_history + PI_ratio + loan_to_value +
             denied_PMI, data = data))
}

make_binary <- function(x, sensitivity = 50) {
    return(ifelse(x > sensitivity, 100, 0))
}


predict_models <- function(df_test, df_train, fits,
                           outcomes = list("y_lin", "y_sq", "y_root")) {
    predict_fun <- lapply(outcomes, FUN = function(x) {
            predict(fits[[x]], df_test) - df_test[[x]]})
    predicts_mean <- lapply(outcomes, FUN = function(x) df_test[[x]] -
                            mean(df_train[[x]]))
    return(list(predict_fun, predicts_mean))
}

## 04_teacher_bias.R

delist_perf <- function(df, col_names = c(
                            "lm_g", "lm_gender", "lm_ses", "lme_g",
                            "lme_gender", "lme_ses", "op_g", "op_gender",
                            "op_ses", "mop_g", "mop_gender", "mop_ses",
                            "full_g", "full_gender", "full_ses"
                        )) {
    list_df <- c()
    first <- T

    for (i in 1:dim(df)[1]) {
        new <- as.data.frame(t(unlist(df[i, ])))
        names(new) <- col_names
        if (first) {
            list_df <- new
            first <- F
        } else {
            list_df <- rbind(list_df, new)
        }
    }
    return(list_df)
}
