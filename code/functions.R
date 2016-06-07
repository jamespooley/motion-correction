library(purrr)  # I use map all over the place; could also just use lapply from base


construct_fmla <- function(roi, degree, motion_estimate = NULL) {
  fmla_string <- paste0(c(sprintf("%s ~ poly(age, degree = %s, raw = TRUE)", roi, degree),
                          motion_estimate),
                        collapse = "+")
  fmla <- as.formula(fmla_string)
  fmla
}


# TODO: Compare with null model
get_best <- function(p_vals, aic_vals) {
  
  # TODO: Is there a function to unpack a list like in Python?
  p1 <- p_vals[[1]]
  p2 <- p_vals[[2]]
  p3 <- p_vals[[3]]
  
  aic1 <- aic_vals[[1]]
  aic2 <- aic_vals[[2]]
  aic3 <- aic_vals[[3]]
  
  if ((p3 < .05) & (aic3 < aic2) & (aic3 < aic2)) {
    model_order <- 3
  } else if ((p2 < .05) & (aic2 < aic1)) {
    model_order <- 2
  } else
    model_order <- 1
  
  model_order
}


compare_models <- function(model_fits) {
  # TODO: Compare with base null model?
  
  aic_vals <- map(model_fits, extractAIC)  # TODO: Check that this is a list of three numbers
  
  age_coefs <- list("poly(age, degree = 1, raw = TRUE)", 
                    "poly(age, degree = 2, raw = TRUE)2",
                    "poly(age, degree = 3, raw = TRUE)3")
  fit_summaries <- map(model_fits, summary)
  p_vals <- map2(fit_summaries, age_coefs, summary)

  best_model <- get_order(p_vals, aic_vals)
  best_model
}


# TODO: This is kind of pointless; combine with get_peak_age function?
get_best_model <- function(model_fits) {
  best_model <- compare_models(model_fits)
  best_model
}


# Helper function to get the age of peak cortical thickness for a single ROI
# TODO: Still a lot of duplication going on here and don't like how the new data frame
# construction is just happening out in the open
get_peak_age <- function(roi, get_best_model, df, motion_estimate = NULL, ...) {
  
  # Construct the model formulas for the given ROI
  degrees <- list(1, 2, 3)
  fmlas <- map2(list(roi, roi, roi), degrees, construct_fmla)  # TODO; Remove ROI duplication
  # Something like this: fmlas <- map(degrees, construct_fmla, roi)?
  
  # Fit a linear, quadratic, and cubic model to the data of a given ROI
  model_fits <- map(fmlas, function(fmla) lm(fmla, data = df))

  # Get the model that best fits the data for the ROI
  best_model <- get_best_model(model_fits)
  
  # TODO: All this is a bit crap, so find a better way 
  # If the best-fitting model is first-order linear, then just output a sentinel value ...
  if (best_model == 1) {
    peak_age <- -999
    return(peak_age)
  # ... otherwise construct the appropriate model formula
  } else
    best_fmla <- fmlas[[best_model]]
  
  # Fit the appropriate model
  best_fit <- lm(best_fmla, data = df)
  
  # TODO: Wrap this in function
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
  
  # Something like this: new_df <- construct_df(age, sex, diagnosis, site)?
  
  best_model_preds <- predict(best_fit, new_df)
  
  peak_age_idx <- which.max(best_model_preds)
  peak_age <- new_df$ages[peak_age_idx]
  peak_age
}