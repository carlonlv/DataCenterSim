#' @include sim_class.R generics.R
NULL

#' Validity Checker for ari11_sim Object
#'
#' @param object A ar1_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_ari11_sim <- function(object) {
  errors <- character()
  res_dist_choices <- c("norm", "skew_norm")
  if (length(object@res_dist) != 1 | is.na(object@res_dist) |  all(object@res_dist != res_dist_choices)) {
    msg <- paste0("train_policy must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (object@reg_num != 2) {
    msg <- paste0("reg_num must be fixed to 2 for ari11 sim model.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}

#' @rdname sim-class
#' @param res_dist The distribution of residual.
#' @param reg_num The number of past regressive observations needed to forecast next observation.
#' @export ari11_sim
ari11_sim <- setClass("ari11_sim",
                    slots = list(res_dist = "character", reg_num = "numeric"),
                    contains = "sim",
                    prototype = list(name = "AR1",
                                     res_dist = "norm",
                                     reg_num = 2),
                    validity = check_valid_ari11_sim)


#' @describeIn train_model Train ARI11 Model specific to ari11_sim object.
setMethod("train_model",
          signature(object = "ari11_sim", trainset_max = "numeric", trainset_avg = "numeric"),
          function(object, trainset_max, trainset_avg) {
            new_trainset_max <- convert_frequency_dataset(trainset_max, object@window_size, "max")
            new_trainset_avg <- convert_frequency_dataset(trainset_avg, object@window_size, "avg")
            if (object@response == "max") {
              new_trainset_max_diff <- diff(new_trainset_max)
              mean_x <- mean(new_trainset_max_diff)
              x_dot <- new_trainset_max_diff - mean_x
              phi <- suppressWarnings(tryCatch({
                ts_model <- stats::arima(x = new_trainset_max_diff, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "Nelder-Mead")
                as.numeric(ts_model$coef[1])
              }, warning = function(w) {
                phi_num <- sample_moment_lag(x_dot, k = 1, r = 1, s = 1)
                phi_den <- sample_moment_lag(x_dot, k = 0, r = 1, s = 1)
                phi <- phi_num / phi_den
              }, error = function(cond) {
                phi_num <- sample_moment_lag(x_dot, k = 1, r = 1, s = 1)
                phi_den <- sample_moment_lag(x_dot, k = 0, r = 1, s = 1)
                phi <- phi_num / phi_den
              }))
            } else {
              new_trainset_avg_diff <- diff(new_trainset_avg)
              mean_x <- mean(new_trainset_avg_diff)
              x_dot <- new_trainset_avg_diff - mean_x
              phi <- suppressWarnings(tryCatch({
                ts_model <- stats::arima(x = new_trainset_avg_diff, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "Nelder-Mead")
                as.numeric(ts_model$coef[1])
              }, warning = function(w) {
                phi_num <- sample_moment_lag(x_dot, k = 1, r = 1, s = 1)
                phi_den <- sample_moment_lag(x_dot, k = 0, r = 1, s = 1)
                phi <- phi_num / phi_den
              }, error = function(cond) {
                phi_num <- sample_moment_lag(x_dot, k = 1, r = 1, s = 1)
                phi_den <- sample_moment_lag(x_dot, k = 0, r = 1, s = 1)
                phi <- phi_num / phi_den
              }))
            }

            fitted_x <- phi * x_dot[-length(x_dot)]
            res <- x_dot[-1] - fitted_x

            if (object@res_dist == "norm") {
              # mu parameter
              mu <- mean_x * (1 - phi)
              # sigma parameter
              sigma2 <- stats::var(res)

              trained_result <- list("phi" = phi, "mu" = mu, "sigma2" = sigma2)
            } else {
              skew_res <- sample_moment_lag(res, k = 0, r = 3, s = 0) / (sample_moment_lag(res, k = 0, r = 2, s = 0) ^ (3/2))
              abs_skew_res <- min(abs(skew_res), 0.99)

              # alpha parameter
              delta <- sign(skew_res) * sqrt((pi / 2) * (abs_skew_res^(2/3)) / ((abs_skew_res ^ (2/3)) + (2 - 0.5 * pi) ^ (2/3)))
              alpha <- delta / sqrt(1 - delta ^ 2)

              # omega parameter
              omega2 <- sample_moment_lag(res, k = 0, r = 2, s = 0) / (1 - 2 / pi * delta ^ (2))
              omega <- sqrt(omega2)

              # xi parameter
              xi <- mean_x * (1 - phi) - sqrt(pi / 2) * omega * delta

              trained_result <- list("phi" = phi, "xi" = xi, "omega" = omega, "alpha" = alpha)
            }
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained ARI Model.
setMethod("do_prediction",
          signature(object = "ari11_sim", trained_result = "list", last_obs_max = "numeric" , last_obs_avg = "numeric", level = "numeric"),
          function(object, trained_result, last_obs_max, last_obs_avg, level) {
            # caclulate probability
            last_obs_max_diff <- last_obs_max[2] - last_obs_max[1]
            last_obs_avg_diff <- last_obs_avg[2] - last_obs_avg[1]
            if (object@response == "max") {
              if (object@res_dist == "norm") {
                mu <- trained_result$mu + trained_result$phi * last_obs_max_diff
                mu <- last_obs_max[2] + mu
              } else {
                xi <- trained_result$xi + trained_result$phi * last_obs_max_diff
                xi <- last_obs_max[2] + xi
              }
            } else {
              if (object@res_dist == "norm") {
                mu <- trained_result$mu + trained_result$phi * last_obs_avg_diff
                mu <- last_obs_avg[2] + mu
              } else {
                xi <- trained_result$xi + trained_result$phi * last_obs_avg_diff
                xi <- last_obs_avg[2] + xi
              }
            }
            if (object@res_dist == "norm") {
              sd <- sqrt(trained_result$sigma2)
              prob <- NULL
              if (!is.na(level)) {
                prob <- 1 - stats::pnorm(q = level, mean = mu, sd = sd)
              }
              predicted_result <- list("prob" = as.numeric(prob), "mean" = mu, "sd" = sd)
            } else {
              omega <- trained_result$omega
              alpha <- trained_result$alpha
              prob <- NULL
              if (!is.na(level)) {
                prob <- 1 - sn::psn(x = level, xi = xi, omega = omega, alpha = alpha)
              }
              predicted_result <- list("prob" = as.numeric(prob), "xi" = xi, "omega" = omega, "alpha" = alpha)
            }
            return(predicted_result)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained ARI Model.
setMethod("compute_pi_up",
          signature(object = "ari11_sim", predicted_result = "list"),
          function(object, predicted_result) {
            if (object@res_dist == "norm") {
              mu <- predicted_result$mean
              sd <- predicted_result$sd
              upper_bounds <- min(stats::qnorm(1 - object@cut_off_prob, mean = mu, sd = sd), 100)
            } else {
              xi <- predicted_result$xi
              omega <- predicted_result$omega
              alpha <- predicted_result$alpha
              upper_bounds <- min(sn::qsn(1 - object@cut_off_prob, xi = xi, omega = omega, alpha = alpha), 100)
            }
            return(upper_bounds)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_numeric_slots
#' @export
setMethod("get_numeric_slots",
          signature(object = "ari11_sim"),
          function(object) {
            numeric_slots <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "tolerance1", "tolerance2")
            numeric_lst <- list()
            for (i in numeric_slots) {
              numeric_lst[[i]] <- methods::slot(object, i)
            }
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_character_slots
#' @export
setMethod("get_character_slots",
          signature(object = "ari11_sim"),
          function(object) {
            character_slots <- c("name", "type", "train_policy", "schedule_policy", "adjust_policy", "response", "res_dist")
            character_lst <- list()
            for (i in character_slots) {
              character_lst[[i]] <- methods::slot(object, i)
            }
            return(character_lst)
          })
