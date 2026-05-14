library(tidyverse)
library(broom)
source("R/new_prices.R")

analysis_date <- as.Date("2026-05-14")

prepare_model_data <- function(listings) {
  listings |>
    filter(model_sample, !is.na(price), !is.na(km), !is.na(model_year), !is.na(seats), is_electric) |>
    enrich_new_prices() |>
    filter(!is.na(estimated_new_price), !is.na(depreciation), depreciation > -50000) |>
    mutate(
      ad_age_years = pmax(as.numeric(format(analysis_date, "%Y")) - model_year, 0),
      log_km = log1p(km),
      battery_kwh_model = coalesce(battery_kwh, median(battery_kwh, na.rm = TRUE)),
      battery_75 = battery_kwh_model >= 70,
      seats = as.integer(seats),
      county = replace_na(county, "Ukjent"),
      length_variant = replace_na(length_variant, "Ukjent"),
      trim_family = replace_na(trim_family, "base"),
      vehicle_model = fct_lump_min(factor(vehicle_model), min = 2, other_level = "Annen søskenmodell")
    ) |>
    drop_na(depreciation, ad_age_years, log_km, estimated_new_price, battery_kwh_model, vehicle_model)
}

fit_candidate_models <- function(model_data) {
  list(
    age_km = lm(depreciation ~ ad_age_years + log_km, data = model_data),
    age_km_newprice = lm(depreciation ~ ad_age_years + log_km + estimated_new_price, data = model_data),
    age_km_newprice_model = lm(depreciation ~ ad_age_years + log_km + estimated_new_price + vehicle_model, data = model_data),
    age_km_newprice_battery_model = lm(depreciation ~ ad_age_years + log_km + estimated_new_price + battery_75 + vehicle_model, data = model_data)
  )
}

loocv_for_formula <- function(formula, model_data) {
  preds <- purrr::map_dfr(seq_len(nrow(model_data)), function(i) {
    train <- model_data[-i, , drop = FALSE]
    test <- model_data[i, , drop = FALSE]
    tryCatch({
      fit <- lm(formula, data = train)
      tibble(
        row = i,
        url_id = test$url_id,
        depreciation = test$depreciation,
        predicted_cv = as.numeric(predict(fit, newdata = test)),
        cv_ok = TRUE
      )
    }, error = function(e) {
      tibble(row = i, url_id = test$url_id, depreciation = test$depreciation, predicted_cv = NA_real_, cv_ok = FALSE)
    })
  }) |>
    mutate(cv_residual = depreciation - predicted_cv)

  tibble(
    loocv_n_ok = sum(preds$cv_ok, na.rm = TRUE),
    loocv_n_failed = sum(!preds$cv_ok, na.rm = TRUE),
    loocv_rmse = sqrt(mean(preds$cv_residual^2, na.rm = TRUE)),
    loocv_mae = mean(abs(preds$cv_residual), na.rm = TRUE),
    loocv_median_abs_error = median(abs(preds$cv_residual), na.rm = TRUE)
  )
}

compare_models <- function(model_data) {
  tibble(
    model = c("age_km", "age_km_newprice", "age_km_newprice_model", "age_km_newprice_battery_model"),
    formula = list(
      depreciation ~ ad_age_years + log_km,
      depreciation ~ ad_age_years + log_km + estimated_new_price,
      depreciation ~ ad_age_years + log_km + estimated_new_price + vehicle_model,
      depreciation ~ ad_age_years + log_km + estimated_new_price + battery_75 + vehicle_model
    )
  ) |>
    mutate(
      fit = map(formula, lm, data = model_data),
      glance = map(fit, broom::glance),
      cv = map(formula, loocv_for_formula, model_data = model_data)
    ) |>
    unnest_wider(glance, names_sep = "_") |>
    unnest_wider(cv)
}

augment_model <- function(fit, model_data) {
  broom::augment(fit, data = model_data) |>
    mutate(
      predicted_depreciation = .fitted,
      predicted_price = estimated_new_price - predicted_depreciation,
      depreciation_residual = depreciation - predicted_depreciation,
      price_residual = price - predicted_price,
      depreciation_residual_pct = depreciation_residual / estimated_new_price,
      student_residual = rstudent(fit),
      cooks_d = cooks.distance(fit),
      leverage = hatvalues(fit),
      underpriced_rank = min_rank(desc(depreciation_residual))
    )
}

model_metrics <- function(modelled, fit) {
  tibble(
    n = nrow(modelled),
    r_squared = broom::glance(fit)$r.squared,
    adj_r_squared = broom::glance(fit)$adj.r.squared,
    rmse = sqrt(mean(modelled$depreciation_residual^2, na.rm = TRUE)),
    mae = mean(abs(modelled$depreciation_residual), na.rm = TRUE),
    median_abs_error = median(abs(modelled$depreciation_residual), na.rm = TRUE),
    mean_abs_pct_error = mean(abs(modelled$depreciation_residual_pct), na.rm = TRUE),
    max_abs_error = max(abs(modelled$depreciation_residual), na.rm = TRUE)
  )
}
