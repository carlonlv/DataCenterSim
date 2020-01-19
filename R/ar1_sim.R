#' @include sim_class.R generics.R
NULL

#' @rdname sim-class
#' @name ar1_sim-class
#' @export ar1_sim
ar1_sim <- setClass("ar1_sim",
                    contains = "sim",
                    prototype = list(name = "AR1"))

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
              ts_model <- suppressWarnings(tryCatch({
                stats::arima(x = new_trainset_max, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "Nelder-Mead")
              }, warning = function(w) {
                stats::arima(x = new_trainset_max, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "BFGS")
              }, error = function(cond) {
                stats::arima(x = new_trainset_max, order = c(1,0,0), include.mean = TRUE, method = "CSS", optim.control = list(maxit = 2000), optim.method = "CG")
              }))
            } else {
              ts_model <- suppressWarnings(tryCatch({
                stats::arima(x = new_trainset_avg, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "Nelder-Mead")
              }, warning = function(w) {
                stats::arima(x = new_trainset_avg, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "BFGS")
              }, error = function(cond) {
                stats::arima(x = new_trainset_avg, order = c(1,0,0), include.mean = TRUE, method = "CSS", optim.control = list(maxit = 2000), optim.method = "CG")
              }))
            }
            trained_result <- list("coeffs" = as.numeric(ts_model$coef[1]), "means" = as.numeric(ts_model$coef[2]), "vars" = ts_model$sigma2)
            return(methods::new("ar1_sim_process", object, trained_model = trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "ar1_sim_process", last_obs_max = "numeric", last_obs_avg = "numeric", predict_size = "numeric", level = "numeric"),
          function(object, last_obs_max, last_obs_avg, predict_size, level) {
            calculate_var_cov_matrix <- function(var, l, phi) {
              dm = abs(outer(1:l,1:l,"-"))
              var_cov <- matrix(var[outer(1:l,1:l,"pmin")],l,l)*phi^dm
              return(var_cov)
            }
            phi <- object@trained_result$coeffs
            mean <- object@trained_result$means
            variance <- object@trained_result$vars

            # Construct mean
            if (object@response == "max") {
              mu <- rep(last_obs_max, predict_size)
            } else {
              mu <- rep(last_obs_avg, predict_size)
            }
            mu <- mu * phi^(1:predict_size) + (1 - phi^(1:predict_size)) * mean

            # Construct Var-cov matrix
            var <- cumsum((phi^2)^(0:(predict_size - 1)))*variance
            varcov <- calculate_var_cov_matrix(var, predict_size, phi)

            # caclulate probability
            prob <- NULL
            if (!is.null(level)) {
              prob <- 1 - mvtnorm::pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
            }
            predict_result <- list("prob" = as.numeric(prob), "mu" = mu, "varcov" = varcov)
            object@predict_result <- predict_result
            return(object)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained AR1 Model.
setMethod("compute_pi_up",
          signature(object = "ar1_sim_process"),
          function(object) {
            mu <- object@predict_result$mu
            varcov <- object@predict_result$varcov
            upper_bounds <- rep(NA, length(mu))
            for (i in 1:length(mu)) {
              upper_bounds[i] <- min(mu[i] + stats::qnorm((1 - object@cut_off_prob)) * sqrt(varcov[i,i]), 100)
            }
            return(max(upper_bounds))
          })


#' @describeIn get_sim_save Generate ar1_sim_result object from simulation.
setMethod("get_sim_save",
          signature(object = "ar1_sim", evaluation = "data.frame", write_result = "logical"),
          function(object, evaluation, write_result) {
            gn_result <- generate_result(object, evaluation, write_result)
            return(methods::new("ar1_sim_result", object, result = gn_result$result, summ = gn_result$summ))
          })


#' @export
setAs("ar1_sim_result", "data.frame",
      function(from) {
        summ <- from@summ
        numeric_lst <- get_numeric_slots(from)
        result_numric <- as.data.frame(numeric_lst)
        result_summ <- as.data.frame(summ)
        return(cbind(result_numric, result_summ))
      })
