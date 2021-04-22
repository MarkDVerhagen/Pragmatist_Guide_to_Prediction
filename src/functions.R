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

## 13_inference.R

delist_cross <- function(df, col_names) {
  list_df <- c()
  first <- T
  i <- 1
  for (i in 1 : length(df)) {
    new <- c(t(df[i]))[[1]]
    names(new) <- col_names
    if (first) {
      list_df <- new 
      first <- F
    } else {
      list_df <- rbind(list_df, new)
    }
  }
  rownames(list_df) <- NULL
  list_df <- list_df / rowSums(list_df)
  return(list_df)
}

ap_cuts <- function(mod, test) {
  linspace <- as.matrix(test[names(mod$coefficients)]) %*% mod$coefficients
  linspace <- linspace[order(linspace)]
  
  zetas <- mod$zeta
  df <- data.frame(
    p1 = pnorm(zetas[1] - linspace, 0, 1),
    p2 = pnorm(zetas[2] - linspace, 0, 1) - pnorm(zetas[1] - linspace, 0, 1),
    p3 = pnorm(zetas[3] - linspace, 0, 1) - pnorm(zetas[2] - linspace, 0, 1),
    p4 = pnorm(zetas[4] - linspace, 0, 1) - pnorm(zetas[3] - linspace, 0, 1),
    p5 = 1 - pnorm(zetas[4] - linspace, 0, 1))
  
  final_df <- data.frame(fixed = linspace,
                         prediction = apply(df, 1, function(x) which.max(x)))
  cuts <- final_df %>%
    arrange(fixed) %>%
    filter(!duplicated(prediction),
           prediction != 1)
  return(cuts$fixed)  
}

generate_ranef_df <- function(object_lm, overall_intercept) {
  ranef_se <- arm::se.ranef(object_lm)
  ranef_df <- data.frame(school = 1 : length(coef(object_lm)$s_id[, 1]),
                         intercept = coef(object_lm)$s_id[, 1],
                         se = ranef_se$s_id,
                         s_id = rownames(ranef_se$s_id))
  names(ranef_df) <- c("School", "Intercept", "SE", "s_id")
  
  ranef_df <- ranef_df %>%
    mutate(lb_intercept = -2 * SE + Intercept,
           ub_intercept = 2 * SE + Intercept)
  
  ranef_df <- ranef_df[order(ranef_df$Intercept), ] %>%
    mutate(Class = 1 : dim(ranef_df)[1],
           Sig = ifelse((lb_intercept > overall_intercept) | (ub_intercept < overall_intercept),
                        "Excl. intercept", "Incl. intercept"))
  return(ranef_df)
}

normalize_op <- function(df_op, df_lm, maths = T) {
  
  if (maths) {
    ratio <- (df_lm$Estimate[rownames(df_lm) == "mean_Maths"]) /
      (df_op$Estimate[df_op$labels == "mean_Maths"])
    
    return(df_op %>%
             filter(!grepl("\\|", labels)) %>%
             mutate(Estimate = Estimate * ratio,
                    `Std. Error` = `Std. Error` * ratio))
  } else {
    diff <- (df$Estimate[df$labels == "4|5"] -
               df$Estimate[df$labels == "1|2"]) / 3
    return(df %>%
             filter(!grepl("\\|", labels)) %>%
             mutate(Estimate = Estimate / diff,
                    `Std. Error` = `Std. Error` / diff))
  }
}

cross_plot <- function(df1, df2, first = T, color1 = "#EE0000FF", color2 = "#3B4992FF") {
  
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
    geom_bar(stat = "summary", position = position_dodge(), color = "black") +
    geom_errorbar(aes(ymin = ptile_5, ymax = ptile_95), width = 0.2, position = position_dodge(width=1)) +
    geom_text(aes(label = paste0(round(mean, 3) * 100, "%")), position = position_dodge(width = 1), hjust=-.4, size=4) +
    coord_flip() + scale_fill_aaas(name = "Prediction model") + cowplot::theme_cowplot() +
    geom_vline(xintercept = mean(df_own %*% c(1, 2, 3, 4, 5)), linetype="dashed",
               color = color1) +
    annotate("text", x = mean(df_own %*% c(1, 2, 3, 4, 5)) + 0.2 * offset, y = 0.6, color = color1, size = 4,
             label = as.character(round(mean(df_own %*% c(1, 2, 3, 4, 5)), 3))) +
    geom_vline(xintercept = mean(df_other %*% c(1, 2, 3, 4, 5)), linetype = "dashed",
               color = color2) +
    annotate("text", x = mean(df_other %*% c(1, 2, 3, 4, 5)) - 0.2 * offset, y = 0.6, color = color2, size = 4,
             label = as.character(round(mean(df_other %*% c(1, 2, 3, 4, 5)), 3))) +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)
    ) +
    ylim(c(0, 0.65)) +
    xlab("Predicted track level") + ylab("Proportion of predictions assigned to track")
}