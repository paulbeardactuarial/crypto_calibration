# =================================================================
# ====================== extract_rss() ============================
# =================================================================
extract_rss <- function(empirical_quantiles,
                        fitted_quantiles) {
  sqrt(sum((fitted_quantiles - empirical_quantiles)^2))
}


# =================================================================
# ==================== extract_loglik() ===========================
# =================================================================

extract_loglik <- function(model) {
  UseMethod("extract_loglik", model)
}
extract_loglik.fitdist <- function(model) {
  model |> purrr::pluck("loglik")
}
extract_loglik.hyperbFit <- function(model) {
  model |> purrr::pluck("maxLik")
}
extract_loglik.vgFit <- function(model) {
  model |> purrr::pluck("maxLik")
}


# =================================================================
# ==================== extract_aic() ===========================
# =================================================================

extract_aic <- function(model) {
  UseMethod("extract_aic", model)
}
extract_aic.fitdist <- function(model) {
  model |> purrr::pluck("aic")
}
extract_aic.hyperbFit <- function(model) {
  loglik <- model |> purrr::pluck("maxLik")
  k <- 4 # because there are 4 parameters
  -2 * loglik + 2 * k
}
extract_aic.vgFit <- function(model) {
  loglik <- model |> purrr::pluck("maxLik")
  k <- 4 # because there are 4 parameters
  -2 * loglik + 2 * k
}

# =================================================================
# ==================== extract_bic() ===========================
# =================================================================

extract_bic <- function(model) {
  UseMethod("extract_bic", model)
}
extract_bic.fitdist <- function(model) {
  model |> purrr::pluck("bic")
}
extract_bic.hyperbFit <- function(model) {
  loglik <- model |> purrr::pluck("maxLik")
  k <- 4 # because there are 4 parameters
  n <- model |>
    unlist() |>
    names() |>
    stringr::str_detect("obs\\d") |>
    sum()
  -2 * loglik + log(n) * k
}
extract_bic.vgFit <- function(model) {
  loglik <- model |> purrr::pluck("maxLik")
  k <- 4 # because there are 4 parameters
  n <- model |>
    unlist() |>
    names() |>
    stringr::str_detect("obs\\d") |>
    sum()
  -2 * loglik + log(n) * k
}
