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
  if (object@reg_num != 1) {
    msg <- paste0("reg_num must be fixed to 1 for var1 sim model.")
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
#' @export var1_sim
var1_sim <- setClass("var1_sim",
                     slots = list(res_dist = "character", reg_num = "numeric"),
                     contains = "sim",
                     prototype = list(name = "VAR1",
                                      res_dist = "norm",
                                      reg_num = 1),
                     validity = check_valid_var1_sim)


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
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained VAR Model.
setMethod("do_prediction",
          signature(object = "var1_sim", trained_result = "list", last_obs_max = "numeric", last_obs_avg = "numeric", level = "numeric"),
          function(object, trained_result, last_obs_max, last_obs_avg, level) {
            if (object@response == "max") {
              mu <- matrix(c(last_obs_max,last_obs_avg), ncol = 1)
            } else {
              mu <- matrix(c(last_obs_avg,last_obs_max), ncol = 1)
            }
            intercept <- trained_result$Ph0
            ar_coef <- trained_result$Phi
            sample_var <- trained_result$Sigma

            mu <- matrix(intercept, nrow = 2, ncol = 1) + ar_coef %*% mu

            varcov <- sample_var
            # caclulate probability
            prob <- NULL
            if (!is.na(level)) {
              prob <- 1 - stats::pnorm(level, mean = mu[1, 1], sd = varcov[1, 1])
            }
            predicted_result <- list("prob" = as.numeric(prob), "mu" = as.numeric(mu[1, 1]), "sd" = as.numeric(sqrt(varcov[1, 1])))
            return(predicted_result)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained AR1 Model.
setMethod("compute_pi_up",
          signature(object = "var1_sim", predicted_result = "list"),
          function(object, predicted_result) {
            mu <- predicted_result$mu
            sd <- predicted_result$sd
            upper_bounds <- min(stats::qnorm(1 - object@cut_off_prob, mean = mu, sd = sd))
            return(max(upper_bounds))
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


#' @return A list containing all character parameter informations.
#' @rdname get_character_slots
#' @export
setMethod("get_character_slots",
          signature(object = "var1_sim"),
          function(object) {
            character_slots <- c("name", "type", "train_policy", "schedule_policy", "adjust_policy", "response", "res_dist")
            character_lst <- list()
            for (i in character_slots) {
              character_lst[[i]] <- methods::slot(object, i)
            }
            return(character_lst)
          })

