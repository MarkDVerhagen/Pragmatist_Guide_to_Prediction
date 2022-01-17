library(caret)

## General plotting functions

fix_label <- function(col) {
  ## Add digits for rounded numbers
  return(ifelse(!grepl("\\.", col),
    gsub("%", "\\.0%", col), col
  ))
}

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
  nround <- 50 # number of XGBoost rounds
  xg_model <- xgb.train(
    data = train_data,
    nrounds = nround
  )
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

calc_perf <- function(model, data, w = F, nw = F) {
  data <- data %>% as.data.frame()
  if (nw) {
    true_outcome <- data %>%
      filter(black == 1) %>%
      pull(accept_bin)
    if (length(true_outcome) == 0) {
      return(NA)
    }
    return(mean(round(predict(model, data[data$black == 1, ],
      type = "response"
    )) == true_outcome))
  } else if (w) {
    true_outcome <- data %>%
      filter(black == 0) %>%
      pull(accept_bin)
    if (length(true_outcome) == 0) {
      return(NA)
    }
    return(mean(round(predict(model, data[data$black == 0, ],
      type = "response"
    )) == true_outcome))
  } else {
    true_outcome <- data %>%
      as.data.frame() %>%
      pull(accept_bin)
    return(mean(round(predict(model, data,
      type = "response"
    )) == true_outcome))
  }
}


gen_do_frame <- function(cv_frame, prob = T) {
  l4 <- map(cv_frame$train, ~ model_4(.))
  prob_est <- map2(
    l4, cv_frame$test,
    ~ calc_prob(.x, .y, prob = prob)
  )
  return(data.table::rbindlist(prob_est))
}


calc_prob <- function(model, data, prob = T) {
  ## Calculate predicted probability and
  ## when intervening on race variable

  original_data <- data %>%
    as.data.frame()

  ## Whites
  w_data <- original_data %>%
    filter(black == 0)
  w_do_data <- w_data %>%
    filter(black == 0) %>%
    mutate(black = 1)
  w_w <- predict(model, w_data, type = "response")
  w_nw <- predict(model, w_do_data, type = "response")

  ## Non-whites
  nw_data <- original_data %>%
    filter(black == 1)
  nw_do_data <- nw_data %>%
    filter(black == 1) %>%
    mutate(black = 0)
  nw_nw <- predict(model, nw_data, type = "response")
  nw_w <- predict(model, nw_do_data, type = "response")


  if (prob) {
    return(data.frame(
      w_w = mean(w_w),
      w_nw = mean(w_nw),
      nw_nw = mean(nw_nw),
      nw_w = mean(nw_w)
    ))
  } else {
    return(data.frame(
      w_w = mean(round(w_w)),
      w_nw = mean(round(w_nw)),
      nw_nw = mean(round(nw_nw)),
      nw_w = mean(round(nw_w))
    ))
  }
}


calc_prob_single <- function(model, data, prob = T, w = T) {
  ## Calculate predicted probability and
  ## when intervening on race variable

  original_data <- data %>%
    as.data.frame()

  ## Whites
  w_data <- original_data %>%
    filter(black == 0)
  w_do_data <- w_data %>%
    filter(black == 0) %>%
    mutate(black = 1)
  w_w <- predict(model, w_data, type = "response")
  w_nw <- predict(model, w_do_data, type = "response")

  ## Non-whites
  nw_data <- original_data %>%
    filter(black == 1)
  nw_do_data <- nw_data %>%
    filter(black == 1) %>%
    mutate(black = 0)
  nw_nw <- predict(model, nw_data, type = "response")
  nw_w <- predict(model, nw_do_data, type = "response")

  if (w) {
    return(data.frame(
      w_w = w_w,
      w_nw = w_nw
    ))
  } else {
    return(data.frame(
      nw_nw = nw_nw,
      nw_w = nw_w
    ))
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
  glm(accept_bin ~ 1 + self_employed + married +
    housing_expense_ratio + bad_history + PI_ratio + loan_to_value +
    denied_PMI, data = data, family = "binomial")
}


model_3 <- function(data) {
  glm(accept_bin ~ 1 + female + self_employed + married +
    housing_expense_ratio + bad_history + PI_ratio + loan_to_value +
    denied_PMI, data = data, family = "binomial")
}


model_4 <- function(data) {
  glm(accept_bin ~ 1 + black + self_employed + married +
    housing_expense_ratio + bad_history + PI_ratio + loan_to_value +
    denied_PMI, data = data, family = "binomial")
}


model_5 <- function(data) {
  glm(accept_bin ~ 1 + female + self_employed + married +
    housing_expense_ratio + bad_history + PI_ratio + loan_to_value +
    denied_PMI + black, data = data, family = "binomial")
}


gen_cv_frame <- function(cv_frame, cv_frame_1, cv_frame_2, nw = F, w = F) {
  l0 <- map(cv_frame$train, ~ model_0(.))
  l1 <- map(cv_frame$train, ~ model_1(.))
  l2 <- map(cv_frame$train, ~ model_2(.))
  l3 <- map(cv_frame$train, ~ model_3(.))
  l4 <- map(cv_frame$train, ~ model_4(.))
  l5 <- map(cv_frame$train, ~ model_5(.))

  return(data.frame(
    m0 = unlist(map2(l0, cv_frame$test, ~ calc_perf(.x, .y, nw, w))),
    m1 = unlist(map2(l1, cv_frame$test, ~ calc_perf(.x, .y, nw, w))),
    m2 = unlist(map2(l2, cv_frame$test, ~ calc_perf(.x, .y, nw, w))),
    m3 = unlist(map2(l3, cv_frame$test, ~ calc_perf(.x, .y, nw, w))),
    m4 = unlist(map2(l4, cv_frame$test, ~ calc_perf(.x, .y, nw, w))),
    m5 = unlist(map2(l5, cv_frame$test, ~ calc_perf(.x, .y, nw, w)))
  ))
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
  for (i in 1:length(df)) {
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
    p5 = 1 - pnorm(zetas[4] - linspace, 0, 1)
  )

  final_df <- data.frame(
    fixed = linspace,
    prediction = apply(df, 1, function(x) which.max(x))
  )
  cuts <- final_df %>%
    arrange(fixed) %>%
    filter(
      !duplicated(prediction),
      prediction != 1
    )
  return(cuts$fixed)
}

generate_ranef_df <- function(object_lm, overall_intercept) {
  ranef_se <- arm::se.ranef(object_lm)
  ranef_df <- data.frame(
    school = 1:length(coef(object_lm)$s_id[, 1]),
    intercept = coef(object_lm)$s_id[, 1],
    se = ranef_se$s_id,
    s_id = rownames(ranef_se$s_id)
  )
  names(ranef_df) <- c("School", "Intercept", "SE", "s_id")

  ranef_df <- ranef_df %>%
    mutate(
      lb_intercept = -2 * SE + Intercept,
      ub_intercept = 2 * SE + Intercept
    )

  ranef_df <- ranef_df[order(ranef_df$Intercept), ] %>%
    mutate(
      Class = 1:dim(ranef_df)[1],
      Sig = ifelse((lb_intercept > overall_intercept) | (ub_intercept < overall_intercept),
        "Excl. intercept", "Incl. intercept"
      )
    )
  return(ranef_df)
}

normalize_op <- function(df_op, df_lm, maths = T) {
  if (maths) {
    ratio <- (df_lm$Estimate[rownames(df_lm) == "mean_Maths"]) /
      (df_op$Estimate[df_op$labels == "mean_Maths"])

    return(df_op %>%
      filter(!grepl("\\|", labels)) %>%
      mutate(
        Estimate = Estimate * ratio,
        `Std. Error` = `Std. Error` * ratio
      ))
  } else {
    diff <- (df$Estimate[df$labels == "4|5"] -
      df$Estimate[df$labels == "1|2"]) / 3
    return(df %>%
      filter(!grepl("\\|", labels)) %>%
      mutate(
        Estimate = Estimate / diff,
        `Std. Error` = `Std. Error` / diff
      ))
  }
}

cross_plot <- function(df1, df2, first = T, color1 = "#EE0000FF", color2 = "#3B4992FF") {
  df_own <- df1
  df_other <- df2
  if (mean(df_own %*% c(1, 2, 3, 4, 5)) > mean(df_other %*% c(1, 2, 3, 4, 5))) {
    offset <- 1
  } else {
    offset <- -1
  }


  plot_df <- data.frame(df_own, df_other) %>%
    reshape2::melt() %>%
    group_by(variable) %>%
    summarise(
      mean = mean(value),
      ptile_5 = quantile(value, probs = 0.05, na.rm = TRUE),
      ptile_95 = quantile(value, probs = 0.95, na.rm = TRUE)
    ) %>%
    mutate(
      model = ifelse(grepl("\\.1", variable), "Other model", "Own model"),
      x = gsub("\\.1", "", variable)
    )

  ggplot(plot_df, aes(x = x, y = mean, fill = model)) +
    geom_bar(stat = "summary", position = position_dodge(), color = "black") +
    geom_errorbar(aes(ymin = ptile_5, ymax = ptile_95), width = 0.2, position = position_dodge(width = 1)) +
    geom_text(aes(label = paste0(round(mean, 3) * 100, "%")), position = position_dodge(width = 1), hjust = -.4, size = 4) +
    coord_flip() +
    scale_fill_aaas(name = "Prediction model") +
    cowplot::theme_cowplot() +
    geom_vline(
      xintercept = mean(df_own %*% c(1, 2, 3, 4, 5)), linetype = "dashed",
      color = color1
    ) +
    annotate("text",
      x = mean(df_own %*% c(1, 2, 3, 4, 5)) + 0.2 * offset, y = 0.6, color = color1, size = 4,
      label = as.character(round(mean(df_own %*% c(1, 2, 3, 4, 5)), 3))
    ) +
    geom_vline(
      xintercept = mean(df_other %*% c(1, 2, 3, 4, 5)), linetype = "dashed",
      color = color2
    ) +
    annotate("text",
      x = mean(df_other %*% c(1, 2, 3, 4, 5)) - 0.2 * offset, y = 0.6, color = color2, size = 4,
      label = as.character(round(mean(df_other %*% c(1, 2, 3, 4, 5)), 3))
    ) +
    theme(
      legend.position = "bottom",
      text = element_text(size = 14),
      axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)
    ) +
    ylim(c(0, 0.65)) +
    xlab("Predicted track level") +
    ylab("Proportion of predictions assigned to track")
}

## 13_Virtue_3.R

cross_plot <- function(df1, df2, theme_custom, first = T,
color1 = "#EE0000FF", color2 = "#3B4992FF",
font_family = "Bookman") {
  text_size = 5.5
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
    geom_bar(stat = "summary", position = position_dodge(),
    color = "black"
    ) +
    geom_errorbar(aes(ymin = ptile_5, ymax = ptile_95), width = 0.2, position = position_dodge(width=1)) +
    geom_text(aes(label = paste0(round(mean, 3) * 100, "%")), position = position_dodge(width = 1), hjust=-.4, size=text_size, family = font_family) +
    coord_flip() + scale_fill_aaas(name = "Prediction model") + cowplot::theme_cowplot() +
    geom_vline(xintercept = mean(df_own %*% c(1, 2, 3, 4, 5)), linetype="dashed",
               color = color1) +
    annotate("text",
      x = mean(df_own %*% c(1, 2, 3, 4, 5)) + 0.2 * offset, y = 0.6, color = color1, size = text_size,
      label = as.character(round(mean(df_own %*% c(1, 2, 3, 4, 5)), 3)),
      family = font_family
    ) +
      theme_custom +
    geom_vline(xintercept = mean(df_other %*% c(1, 2, 3, 4, 5)), linetype = "dashed",
               color = color2) +
    annotate("text", x = mean(df_other %*% c(1, 2, 3, 4, 5)) - 0.2 * offset, y = 0.6, color = color2, size=text_size,
             label = as.character(round(mean(df_other %*% c(1, 2, 3, 4, 5)), 3))) +
    theme(legend.position = "bottom",
          panel.border = element_rect(color = "black")
          # text = element_text(size = 14),
          # axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.65)) +
    xlab("Predicted track level") + ylab("Proportion of predictions assigned to track")
}