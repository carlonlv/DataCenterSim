#' @include sim_class.R generics.R
NULL

#' Validity Checker for var1_sim Object
#'
#' @param object A var1_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_var1_sim <- function(object) {
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
#' @name var1_sim-class
#' @export var1_sim
var1_sim <- setClass("var1_sim",
                     slots = list(res_dist = "character"),
                     contains = "sim",
                     prototype = list(name = "VAR1",
                                      res_dist = "norm"),
                     validity = check_valid_var1_sim)

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
          signature(object = "var1_sim_process", last_obs_max = "numeric", last_obs_avg = "numeric", level = "numeric"),
          function(object, last_obs_max, last_obs_avg, level) {

            if (object@response == "max") {
              mu <- matrix(c(last_obs_max,last_obs_avg), ncol = 1)
            } else {
              mu <- matrix(c(last_obs_avg,last_obs_max), ncol = 1)
            }
            intercept <- object@trained_model$Ph0
            ar_coef <- object@trained_model$Phi
            sample_var <- object@trained_model$Sigma

            mu <- matrix(intercept, nrow = 2, ncol = 1) + ar_coef %*% mu

            varcov <- sample_var

            # caclulate probability
            prob <- NULL
            if (!is.na(level)) {
              prob <- 1 - stats::pnorm(level, mean = mu[1, 1], sd = varcov[1, 1])
            }
            predict_result <- list("prob" = as.numeric(prob), "mu" = mu[1, 1], "sd" = sqrt(varcov[1, 1]))
            object@predict_result <- predict_result
            return(object)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained AR1 Model.
setMethod("compute_pi_up",
          signature(object = "var1_sim_process"),
          function(object) {
            mu <- object@predict_result$mu
            var <- object@predict_result$var
            upper_bounds <- min(stats::qnorm(1 - object@cut_off_prob, mean = mu, sd = sqrt(var)))
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
            numeric_slots <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "tolerance1", "tolerance2")
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
