library(purrr)
# library(dplyr)


construct_fmla <- function(roi, degree, motion_estimate = NULL) {
  fmla_string <- paste0(c(sprintf("%s ~ poly(age, degree = %s, raw = TRUE)", roi, degree),
                          motion_estimate), 
                        collapse = "+")
  fmla <- as.formula(fmla_string)
  fmla
}


# get_aic_val <- function(fit_summary) {
#   aic_val <- 
#   aic_val
# }


compare_models <- function(fit_summaries) {
  
  # TODO: Compare with base null model?
  
  age_coefs <- list("poly(age, degree = 1, raw = TRUE)", 
                    "poly(age, degree = 2, raw = TRUE)2",
                    "poly(age, degree = 3, raw = TRUE)3")
  
  p_vals <- map2(fit_summaries, age_coefs, summary)
  
  aic_values <- AIC(fit_summaries[[1]], fit_summaries[[2]], fit_summaries[[3]])

  if (p3 < .05 & aic_values$AIC[3] < aic_values$AIC[2] & aic_values$AIC[3] < aic_values$AIC[1]) {
    model_order <- 3
  } else if (p2 < .05 & aic_values$AIC[2] < aic_values$AIC[1]) {
    model_order <- 2
  } else
    model_order <- 1

  model_order
}


get_best_model <- function(model_fits) {
  
  fit_summaries <- map(model_fits, summary)
  
  model_comparison <- AIC(model_fits[[1]], model_fits[[2]], model_fits[[3]])  # TODO: Is there a way to unpack the fits list?
  
  best_model <- compare_models(fit_summaries)
  best_model
}


# Helper function to get the age of peak cortical thickness for a single ROI
get_peak_age <- function(roi, get_best_model, df, motion_estimate = NULL, ...) {
  
  # Construct the model formulas
  degrees <- list(1, 2, 3)
  fmlas <- map2(list(roi, roi, roi), degrees, construct_fmla)  # TODO; Remove ROI duplication
  
  model_fits <- map(fmlas, function(fmla) lm(fmla, data = df))

  # Get the model that best fits the data for the ROI
  best_model <- get_best_model(model_fits)
  
  # If the best-fitting model is first-order linear, then just output a sentinel value ...
  if (best_model == 1) {
    peak_age <- -999
  # ... otherwise construct the appropriate model formula
  } else
    best_fmla <- fmlas[[best_model]]
  
  # Fit the appropriate model
  best_fit <- lm(best_fmla, data = df)
  
  age_range <- range(df$age)
  age <- seq(age_range[1], age_range[2], by = 0.01)
  n_ages <- length(age)
  sex <- rep(sex, length = n_ages)
  site <- rep(site, length = n_ages)
  diagnosis <- rep(diagnosis, length = n_ages)
  
  new_df <- data.frame(age = age,
                       sex = sex,
                       diagnosis = diagnosis,
                       site = site)
  
  best_model_preds <- predict(best_fit, new_df)
  
  peak_age_idx <- which.max(best_model_preds)
  peak_age <- new_df$ages[peak_age_idx]
  peak_age
}


#-------------------------------------------------------------------------
# TODO: Put all this in its own script
#
# peak_ages_no_motion <- map(roi_list, get_peak_age)
# 
# roi_list <- names(pardoe_df)[16:80]
# 
# best_models_no_motion <- map(roi_list, get_best_model)
# 
# peak_ages_no_motion <- map(roi_list, get_peak_ages)
# peak_ages_motion_rmsd <- map(roi_list, get_peak_ages, "rmsd")