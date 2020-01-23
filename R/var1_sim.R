#' @include sim_class.R generics.R
NULL

#' @rdname sim-class
#' @name var1_sim-class
#' @export var1_sim
var1_sim <- setClass("var1_sim",
                    contains = "sim",
                    prototype = list(name = "VAR1"))

#' @rdname sim_process-class
var1_sim_process <- setClass("var1_sim_process",
                            slots = list(trained_model = "list", predict_result = "list"),
                            prototype = list(trained_model = list(), predict_result = list()),
                            contains = "var1_sim")

#' @rdname sim_result-class
var1_sim_result <- setClass("var1_sim_result",
                           slots = list(result = "data.frame", summ = "list"),
                           contains = "var1_sim")


#' @describeIn train_model Train VAR Model specific to var1_sim object.
setMethod("train_model",
          signature(object = "var1_sim", trainset_max = "numeric", trainset_avg = "numeric"),
          function(object, trainset_max, trainset_avg) {
            new_trainset_max <- convert_frequency_dataset(trainset_max, object@window_size, "max")
            new_trainset_avg <- convert_frequency_dataset(trainset_avg, object@window_size, "avg")
            if (object@response == "max") {
              uni_data_matrix <- matrix(nrow = length(new_trainset_max), ncol = 2)
              uni_data_matrix[,1] <- new_trainset_max
              uni_data_matrix[,2] <- new_trainset_avg
              trained_result <- MTS::VAR(uni_data_matrix, p = 1, include.mean = TRUE, output = FALSE)
            } else {
              uni_data_matrix <- matrix(nrow = length(new_trainset_avg), ncol = 2)
              uni_data_matrix[,1] <- new_trainset_avg
              uni_data_matrix[,2] <- new_trainset_max
              trained_result <- MTS::VAR(uni_data_matrix, p = 1, include.mean = TRUE, output = FALSE)
            }
            return(var1_sim_process(object, trained_model = trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained VAR Model.
setMethod("do_prediction",
          signature(object = "var1_sim_process", last_obs_max = "numeric", last_obs_avg = "numeric", predict_size = "numeric", level = "numeric"),
          function(object, last_obs_max, last_obs_avg, predict_size, level) {

            if (object@response == "max") {
              mu <- matrix(c(last_obs_max,last_obs_avg), ncol = 1)
            } else {
              mu <- matrix(c(last_obs_avg,last_obs_max), ncol = 1)
            }
            intercept <- object@trained_model$Ph0
            ar_coef <- object@trained_model$Phi
            sample_var <- object@trained_model$Sigma

            for (h in 1:predict_size) {
              mu <- matrix(intercept, nrow = 2, ncol = 1) + ar_coef %*% mu
            }

            if (predict_size == 1) {
              varcov <- sample_var
            } else {
              forecast_var <- list()
              forecast_var[[1]] <- sample_var
              for (h in 2:predict_size) {
                temp_coef <- matrixcalc::matrix.power(ar_coef, h - 1)
                forecast_var[[h]] <- forecast_var[[h - 1]] + temp_coef %*% sample_var %*% t(temp_coef)
              }
              varcov <- forecast_var[[predict_size]]
            }
            # caclulate probability
            prob <- NULL
            if (!is.na(level)) {
              prob <- 1 - mvtnorm::pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
            }
            predict_result <- list("prob" = as.numeric(prob), "mu" = mu, "varcov" = varcov)
            object@predict_result <- predict_result
            return(object)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained AR1 Model.
setMethod("compute_pi_up",
          signature(object = "var1_sim_process"),
          function(object) {
            mu <- object@predict_result$mu
            varcov <- object@predict_result$varcov
            upper_bounds <- rep(NA, ncol(mu))
            for (i in 1:ncol(mu)) {
              upper_bounds[i] <- min(mu[1, i] + stats::qnorm(1 - object@cut_off_prob) * sqrt(varcov[i, i]), 100)
            }
            return(max(upper_bounds))
          })


#' @describeIn get_sim_save Generate ar1_sim_result object from simulation.
setMethod("get_sim_save",
          signature(object = "var1_sim", evaluation = "data.frame", write_result = "logical"),
          function(object, evaluation, write_result) {
            gn_result <- generate_result(object, evaluation, write_result)
            return(var1_sim_result(object, result = gn_result$result, summ = gn_result$summ))
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_numeric_slots
#' @export
setMethod("get_numeric_slots",
          signature(object = "var1_sim"),
          function(object) {
            numeric_slots <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "tolerance")
            numeric_lst <- list()
            for (i in numeric_slots) {
              numeric_lst[[i]] <- methods::slot(object, i)
            }
            return(numeric_lst)
          })


#' @export
setAs("var1_sim", "data.frame",
      function(from) {
        numeric_lst <- get_numeric_slots(from)
        result_numeric <- as.data.frame(numeric_lst)
        return(result_numeric)
      })


#' @export
setAs("var1_sim_result", "data.frame",
      function(from) {
        summ <- from@summ
        numeric_lst <- get_numeric_slots(from)
        result_numeric <- as.data.frame(numeric_lst)
        result_summ <- as.data.frame(summ)
        return(cbind(result_numeric, result_summ))
      })
