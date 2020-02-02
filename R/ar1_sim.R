#' @include sim_class.R generics.R
NULL


#' Validity Checker for ar1_sim Object
#'
#' @param object A ar1_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_ar1_sim <- function(object) {
  errors <- character()
  res_dist_choices <- c("norm", "skew_norm")
  if (length(object@res_dist) != 1 | is.na(object@res_dist) |  all(object@res_dist != res_dist_choices)) {
    msg <- paste0("train_policy must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @name ar1_sim-class
#' @export ar1_sim
ar1_sim <- setClass("ar1_sim",
                    slots = list(res_dist = "character"),
                    contains = "sim",
                    prototype = list(name = "AR1",
                                     res_dist = "norm"),
                    validity = check_valid_ar1_sim)

#' @rdname sim_process-class
ar1_sim_process <- setClass("ar1_sim_process",
                            slots = list(trained_model = "list", predict_result = "list"),
                            prototype = list(trained_model = list(), predict_result = list()),
                            contains = "ar1_sim")

#' @rdname sim_result-class
ar1_sim_result <- setClass("ar1_sim_result",
                           slots = list(result = "data.frame", summ = "list"),
                           contains = "ar1_sim")


#' @describeIn train_model Train AR1 Model specific to ar1_sim object.
setMethod("train_model",
          signature(object = "ar1_sim", trainset_max = "numeric", trainset_avg = "numeric"),
          function(object, trainset_max, trainset_avg) {
            new_trainset_max <- convert_frequency_dataset(trainset_max, object@window_size, "max")
            new_trainset_avg <- convert_frequency_dataset(trainset_avg, object@window_size, "avg")
            if (object@response == "max") {
              mean_x <- mean(new_trainset_max)
              x_dot <- new_trainset_max - mean_x

              phi <- suppressWarnings(tryCatch({
                ts_model <- stats::arima(x = new_trainset_max, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "Nelder-Mead")
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
              mean_x <- mean(new_trainset_avg)
              x_dot <- new_trainset_avg - mean_x
              phi <- suppressWarnings(tryCatch({
                ts_model <- stats::arima(x = new_trainset_avg, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "Nelder-Mead")
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
            return(ar1_sim_process(object, trained_model = trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "ar1_sim_process", last_obs_max = "numeric", last_obs_avg = "numeric", level = "numeric"),
          function(object, last_obs_max, last_obs_avg, level) {
            # caclulate probability
            if (object@response == "max") {
              if (object@res_dist == "norm") {
                mu <- object@trained_model$mu + object@trained_model$phi * last_obs_max
              } else {
                xi <- object@trained_model$xi + object@trained_model$phi * last_obs_max
              }
            } else {
              if (object@res_dist == "norm") {
                mu <- object@trained_model$mu + object@trained_model$phi * last_obs_avg
              } else {
                xi <- object@trained_model$xi + object@trained_model$phi * last_obs_avg
              }
            }
            if (object@res_dist == "norm") {
              sd <- sqrt(object@trained_model$sigma2)
              prob <- NULL
              if (!is.na(level)) {
                prob <- 1 - stats::pnorm(q = level, mean = mu, sd = sd)
              }
              predict_result <- list("prob" = as.numeric(prob), "mean" = mu, "sd" = sd)
            } else {
              omega <- object@trained_model$omega
              alpha <- object@trained_model$alpha
              prob <- NULL
              if (!is.na(level)) {
                prob <- 1 - sn::psn(x = level, xi = xi, omega = omega, alpha = alpha)
              }
              predict_result <- list("prob" = as.numeric(prob), "xi" = xi, "omega" = omega, "alpha" = alpha)
            }
            object@predict_result <- predict_result
            return(object)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained AR1 Model.
setMethod("compute_pi_up",
          signature(object = "ar1_sim_process"),
          function(object) {
            if (object@res_dist == "norm") {
              mu <- object@predict_result$mean
              sd <- object@predict_result$sd
              upper_bounds <- min(stats::qnorm(1 - object@cut_off_prob, mean = mu, sd = sd), 100)
            } else {
              xi <- object@predict_result$xi
              omega <- object@predict_result$omega
              alpha <- object@predict_result$alpha
              upper_bounds <- min(sn::qsn(1 - object@cut_off_prob, xi = xi, omega = omega, alpha = alpha), 100)
            }
            return(upper_bounds)
          })


#' @describeIn get_sim_save Generate ar1_sim_result object from simulation.
setMethod("get_sim_save",
          signature(object = "ar1_sim", evaluation = "data.frame", write_result = "logical"),
          function(object, evaluation, write_result) {
            gn_result <- generate_result(object, evaluation, write_result)
            return(ar1_sim_result(object, result = gn_result$result, summ = gn_result$summ))
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_numeric_slots
#' @export
setMethod("get_numeric_slots",
          signature(object = "ar1_sim"),
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
          signature(object = "ar1_sim"),
          function(object) {
            numeric_slots <- c("name", "type", "train_policy", "schedule_policy", "adjust_policy", "response", "res_dist")
            numeric_lst <- list()
            for (i in numeric_slots) {
              numeric_lst[[i]] <- methods::slot(object, i)
            }
            return(numeric_lst)
          })


#' @export
setAs("ar1_sim", "data.frame",
      function(from) {
        numeric_lst <- get_numeric_slots(from)
        result_numeric <- as.data.frame(numeric_lst)
        return(result_numeric)
      })


#' @export
setAs("ar1_sim_result", "data.frame",
      function(from) {
        summ <- from@summ
        numeric_lst <- get_numeric_slots(from)
        result_numeric <- as.data.frame(numeric_lst)
        result_summ <- as.data.frame(summ)
        return(cbind(result_numeric, result_summ))
      })
