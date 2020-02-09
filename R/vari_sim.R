#' @include sim_class.R generics.R
NULL

#' Validity Checker for vari_sim Object
#'
#' @param object A vari_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_vari_sim <- function(object) {
  errors <- character()
  res_dist_choices <- c("norm", "skew_norm")
  if (length(object@res_dist) != 1 | is.na(object@res_dist) |  all(object@res_dist != res_dist_choices)) {
    msg <- paste0("train_policy must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (object@reg_num != 1) {
    msg <- paste0("reg_num must be fixed to 1 for vari sim model.")
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
#' @export vari_sim
vari_sim <- setClass("vari_sim",
                     slots = list(res_dist = "character", reg_num = "numeric"),
                     contains = "sim",
                     prototype = list(name = "vari",
                                      res_dist = "norm",
                                      reg_num = 1),
                     validity = check_valid_vari_sim)


#' @describeIn train_model Train VAR Model specific to vari_sim object.
setMethod("train_model",
          signature(object = "vari_sim", trainset_max = "numeric", trainset_avg = "numeric"),
          function(object, trainset_max, trainset_avg) {
            new_trainset_max <- convert_frequency_dataset(trainset_max, object@window_size, "max")
            new_trainset_avg <- convert_frequency_dataset(trainset_avg, object@window_size, "avg")
            if (object@response == "max") {
              new_trainset_max_diff <- diff(new_trainset_max)
              new_trainset_avg_diff <- diff(new_trainset_avg)
              uni_data_matrix <- matrix(nrow = length(new_trainset_max_diff), ncol = 2)
              uni_data_matrix[,1] <- new_trainset_max_diff
              uni_data_matrix[,2] <- new_trainset_avg_diff
              trained_result <- MTS::VAR(uni_data_matrix, p = 1, include.mean = TRUE, output = FALSE)
            } else {
              new_trainset_max_diff <- diff(new_trainset_max)
              new_trainset_avg_diff <- diff(new_trainset_avg)
              uni_data_matrix <- matrix(nrow = length(new_trainset_avg_diff), ncol = 2)
              uni_data_matrix[,1] <- new_trainset_avg_diff
              uni_data_matrix[,2] <- new_trainset_max_diff
              trained_result <- MTS::VAR(uni_data_matrix, p = 1, include.mean = TRUE, output = FALSE)
            }
#            if (object@res_dist != "norm"){
#              ar_coef <- as.numeric(trained_result$Phi)
#              mean_x <- mean(new_trainset_max_diff)
#              mean_y <- mean(new_trainset_avg_diff)
#              x_dot <- new_trainset_max_diff - mean_x
#              y_dot <- new_trainset_avg_diff - mean_y
#              fitted_x <- ar_coef[1] * x_dot[-length(x_dot)] + ar_coef[2] * y_dot[-length(y_dot)]
#              res <- x_dot[-1] - fitted_x
#              skew_res <- sample_moment_lag(res, k = 0, r = 3, s = 0) / (sample_moment_lag(res, k = 0, r = 2, s = 0) ^ (3/2))
#              abs_skew_res <- min(abs(skew_res), 0.99)

              # alpha parameter
#              delta <- sign(skew_res) * sqrt((pi / 2) * (abs_skew_res^(2/3)) / ((abs_skew_res ^ (2/3)) + (2 - 0.5 * pi) ^ (2/3)))
#              alpha <- delta / sqrt(1 - delta ^ 2)

              # omega parameter
#              omega2 <- sample_moment_lag(res, k = 0, r = 2, s = 0) / (1 - 2 / pi * delta ^ (2))
#              omega <- sqrt(omega2)

              # xi parameter
#              xi <- mean_x * (1 - phi) - sqrt(pi / 2) * omega * delta

#              trained_result <- list("phi" = phi, "xi" = xi, "omega" = omega, "alpha" = alpha)
#            }
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained VAR Model.
setMethod("do_prediction",
          signature(object = "vari_sim", trained_result = "list", last_obs_max = "numeric", last_obs_avg = "numeric", level = "numeric"),
          function(object, trained_result, last_obs_max, last_obs_avg, level) {
            last_obs_max_diff <- last_obs_max[2] - last_obs_max[1]
            last_obs_avg_diff <- last_obs_avg[2] - last_obs_avg[1]
            if (object@response == "max") {
              mu <- matrix(c(last_obs_max_diff,last_obs_avg_diff), ncol = 1)
            } else {
              mu <- matrix(c(last_obs_avg_diff,last_obs_max_diff), ncol = 1)
            }
            intercept <- trained_result$Ph0
            ar_coef <- trained_result$Phi
            sample_var <- trained_result$Sigma

            mu <- matrix(intercept, nrow = 2, ncol = 1) + ar_coef %*% mu
            mu <- mu + matrix(c(last_obs_max[2],last_obs_avg[2]), nrow = 2, ncol = 1)

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
          signature(object = "vari_sim", predicted_result = "list"),
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
          signature(object = "vari_sim"),
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
          signature(object = "vari_sim"),
          function(object) {
            character_slots <- c("name", "type", "train_policy", "schedule_policy", "adjust_policy", "response", "res_dist")
            character_lst <- list()
            for (i in character_slots) {
              character_lst[[i]] <- methods::slot(object, i)
            }
            return(character_lst)
          })

