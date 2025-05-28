
# ---------------------------------------------------------------------------
# -------------------------fit_distribution()----------------------------
# ---------------------------------------------------------------------------

fit_distribution <- function(distribution, data) {
  fitted_distr_object <- switch(distribution,
                     "vg" = VarianceGamma::vgFit(data, startMethod = "Nelder-Mead", startValues = "SL", method = "Nelder-Mead"),
                     "hyperb" = GeneralizedHyperbolic::hyperbFit(data),
                     "emg" = fitdistrplus::fitdist(data,
                                     distr = distribution,
                                     method = "mle",
                                     start = list(
                                       mu = mean(data),
                                       sigma = sd(data),
                                       lambda = 5
                                     ),
                                     lower = c(NA, 0, 0)
                     ),
                     "t.scaled" = {
                       # specify t.scaled fitting to have 3 attempts with different df starting values
                       # usually df=3 works, however sometimes optimal df value is very high and df=3 fails
                       df_attempts <- c(3, 100, 10000)
                       fitted_distr_object <- NULL
                       attempt.no <- 1
                       
                       while (attempt.no <= 3) {
                         try_result <- tryCatch(
                           {
                             fitted_distr_object <- fitdistrplus::fitdist(data,
                                                 distr = distribution,
                                                 method = "mle",
                                                 start = list(
                                                   df = df_attempts[attempt.no],
                                                   mean = mean(data),
                                                   sd = sd(data)
                                                 ),
                                                 lower = c(1, NA, 0)
                             )
                           },
                           error = function(e) {
                             if (grepl("code 7", e$message)) {
                               message("Error code 7 encountered. Retrying with new starting values...")
                               return(NULL)
                             } else {
                               stop(e)
                             }
                           }
                         )
                         
                         if (!is.null(try_result)) {
                           break
                         }
                         
                         attempt.no <- attempt.no + 1
                       }
                       
                       if (is.null(fitted_distr_object)) {
                         message("Maximum number of attempts reached. Could not fit distribution.")
                       }
                       fitted_distr_object
                     },
                     fitdistrplus::fitdist(data, distr = distribution, method = "mle")
  )
  
  return(fitted_distr_object)
}




# -------------------------------------------------------------------
# ------------------------collect_params()----------------------------
# -------------------------------------------------------------------

collect_params <- function(distribution,
                          model,
                          max.params = 4) {
  
  #get parameters of variance-gamma or hyperbolic distributions
  if (distribution %in% c("vg", "hyperb")) {
    
    parameters <- data.frame(Parameter = names(model$param), Value = model$param)
    
  } else {
    
    #...or get the parameters of all other distributions
    parameters <- data.frame(Parameter = names(model$estimate), Value = model$estimate)
    
  }
  
  #set out data frame of parameters
  parameters <- data.frame(
    Number = 1:max.params,
    Parameter = c(as.character(parameters$Parameter), rep(NA, max.params - length(parameters$Parameter))),
    Value = c(parameters$Value, rep(NA, max.params - length(parameters$Value)))
  )
  
  return(parameters)
}



# -------------------------------------------------------------------
# -------------------------collect_moments()--------------------------
# -------------------------------------------------------------------

collect_moments <- function(distribution,
                            model) {
  if (distribution == "vg") {
    moments <- c(
      VarianceGamma::vgMean(param = model$param),
      VarianceGamma::vgVar(param = model$param),
      VarianceGamma::vgSkew(param = model$param),
      VarianceGamma::vgKurt(param = model$param)
    )
  } else if (distribution == "hyperb") {
    moments <- c(
      GeneralizedHyperbolic::hyperbMean(param = model$param),
      GeneralizedHyperbolic::hyperbVar(param = model$param),
      GeneralizedHyperbolic::hyperbSkew(param = model$param),
      GeneralizedHyperbolic::hyperbKurt(param = model$param)
    )
  } else {
    moments <- NULL
  }
  return(moments)
}


# ---------------------------------------------------------------------------
# -------------------------extract_fit_quantiles()----------------------------
# ---------------------------------------------------------------------------

extract_fit_quantiles <- function(distribution, model, percentiles) {
  
  if(length(model) == 1 & is.list(model[[1]])) {model <- model[[1]]}
  if(is.list(percentiles)) {percentiles <- c(unlist(percentiles))}
  
  quantiles <- switch(distribution,
                      
                      # variance-gamma
                      "vg" = VarianceGamma::qvg(percentiles, param = model$param),
                      
                      # hyperbolic
                      "hyperb" = GeneralizedHyperbolic::qhyperb(percentiles, param = model$param),
                      
                      # all other distributions
                      quantile(model, probs = percentiles) |> 
                        purrr::pluck("quantiles") |> 
                        unlist()
  )
  
  return(quantiles |> unname())
}


# ---------------------------------------------------------------------------
# -------------------------extract_data_percentiles()------------------------
# ---------------------------------------------------------------------------

extract_data_percentiles <- function(data) {
  x <- length(data)
  set_percentiles <- seq(from = 1 / (2 * x), to = 1 - (1 / (2 * x)), by = 1 / x)
  data_percentiles <- set_percentiles[rank(data, ties.method = "first")]
  return(data_percentiles)
}




# ---------------------------------------------------------------------------
# -------------------------extract_fitting_stats()------------------------
# ---------------------------------------------------------------------------

# WARNING THIS IS NOT FUNCTIONING CORRECTLY

extract_fitting_stats <- function(empirical_quantiles,
                      fitted_quantiles,
                      distribution,
                      model,
                      parameters) {
  
  # populate fitting.stats dataframe
  fs <- data.frame(Statistic = c("RSS", "MLE", "AIC", "BIC"), Value = c(0, 0, 0, 0))
  
  # RSS
  fs$Value[1] <- sqrt(sum((fitted_quantiles - empirical_quantiles)^2))
  
  # MLE
  fs$Value[2] <- ifelse(distribution %in% c("vg", "hyperb"),
                        model$maxLik,
                        model$loglik
  )
  
  # AIC
  fs$Value[3] <- 2 * sum(!is.na(parameters$Value)) - 2 * fs$Value[2]
  
  # BIC
  fs$Value[4] <- log(length(empirical_quantiles)) * sum(!is.na(parameters$Value)) -2 * fs$Value[2]
  
  
  return(fs)
}









