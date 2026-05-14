library(tidyverse)
library(broom)

analysis_date <- as.Date("2026-05-14")

prepare_model_data <- function(listings) {
  data <- listings |>
    filter(model_sample, !is.na(price), !is.na(km), !is.na(model_year), !is.na(seats), is_electric) |>
    mutate(
      ad_age_years = pmax(as.numeric(format(analysis_date, "%Y")) - model_year, 0),
      log_km = log1p(km),
      log_price = log(price),
      battery_kwh_model = coalesce(battery_kwh, median(battery_kwh, na.rm = TRUE)),
      battery_75 = battery_kwh_model >= 70,
      seats = as.integer(seats),
      county = replace_na(county, "Ukjent"),
      length_variant = replace_na(length_variant, "Ukjent"),
      vehicle_model = fct_lump_min(factor(vehicle_model), min = 2, other_level = "Annen søskenmodell")
    ) |>
    drop_na(price, log_price, ad_age_years, log_km, battery_kwh_model, vehicle_model)

  data
}

fit_candidate_models <- function(model_data) {
  list(
    age_km = lm(log_price ~ ad_age_years + log_km, data = model_data),
    age_km_battery = lm(log_price ~ ad_age_years + log_km + battery_75, data = model_data),
    age_km_battery_model = lm(log_price ~ ad_age_years + log_km + battery_75 + vehicle_model, data = model_data)
  )
}

loocv_for_formula <- function(formula, model_data) {
  preds <- purrr::map_dfr(seq_len(nrow(model_data)), function(i) {
    train <- model_data[-i, , drop = FALSE]
    test <- model_data[i, , drop = FALSE]
    out <- tryCatch({
      fit <- lm(formula, data = train)
      tibble(
        row = i,
        url_id = test$url_id,
        price = test$price,
        predicted_cv = exp(as.numeric(predict(fit, newdata = test))),
        cv_ok = TRUE
      )
    }, error = function(e) {
      tibble(row = i, url_id = test$url_id, price = test$price, predicted_cv = NA_real_, cv_ok = FALSE)
    })
    out
  }) |>
    mutate(cv_residual = price - predicted_cv)

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
    model = c("age_km", "age_km_battery", "age_km_battery_model"),
    formula = list(
      log_price ~ ad_age_years + log_km,
      log_price ~ ad_age_years + log_km + battery_75,
      log_price ~ ad_age_years + log_km + battery_75 + vehicle_model
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

select_model <- function(model_data) {
  cmp <- compare_models(model_data)
  chosen_name <- cmp |>
    arrange(loocv_mae, glance_AIC) |>
    slice(1) |>
    pull(model)
  fit_candidate_models(model_data)[[chosen_name]]
}

augment_model <- function(fit, model_data) {
  aug <- broom::augment(fit, data = model_data) |>
    mutate(
      predicted = exp(.fitted),
      residual = price - predicted,
      pct_residual = residual / predicted,
      abs_residual = abs(residual),
      student_residual = rstudent(fit),
      cooks_d = cooks.distance(fit),
      leverage = hatvalues(fit),
      underpriced_rank = min_rank(residual)
    )
  aug
}

model_metrics <- function(modelled, fit) {
  tibble(
    n = nrow(modelled),
    r_squared = broom::glance(fit)$r.squared,
    adj_r_squared = broom::glance(fit)$adj.r.squared,
    rmse = sqrt(mean(modelled$residual^2, na.rm = TRUE)),
    mae = mean(abs(modelled$residual), na.rm = TRUE),
    median_abs_error = median(abs(modelled$residual), na.rm = TRUE),
    mean_abs_pct_error = mean(abs(modelled$pct_residual), na.rm = TRUE),
    max_abs_error = max(abs(modelled$residual), na.rm = TRUE)
  )
}
