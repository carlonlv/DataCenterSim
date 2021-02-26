#' @include sim_class.R generics.R
NULL


#' Validity Checker for lm_sim Object
#'
#' @param object A lm_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_lm_sim <- function(object) {
  errors <- character()
  res_dist_choices <- c("normal", "skew_norm")
  window_type_choices <- c("max", "avg")
  if (length(object@res_dist) != 1 | is.na(object@res_dist) | all(object@res_dist != res_dist_choices)) {
    msg <- paste0("res_dist must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (is.na(object@window_size_for_reg) | object@window_size_for_reg %% 1 != 0) {
    msg <- paste0("window_size_for_reg must be an integer.")
    errors <- c(errors, msg)
  }
  if (is.na(object@window_type_for_reg) | all(object@window_type_for_reg != window_type_choices)) {
    msg <- paste0("window_type_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param res_dist A character representing the distribution of residual, \code{"normal"} for normal distribution, \code{"skew_norm"} for skewed normal distribution, or \code{"empirical"} for empirical distribution. Default value is \code{"normal"}.
#' @param window_size_for_reg A numeric value representing the window size to be aggregated.
#' @param window_type_for_reg A character value indicating how the windowing operation is executed. The value must be one of \code{c("max", "avg")}.
#' @param train_args A list representing additional call passed into the training function, \code{stats::lm}. Default value is \code{list()}.
#' @export lm_sim
lm_sim <- setClass("lm_sim",
                      slots = list(res_dist = "character",
                                   window_size_for_reg = "numeric",
                                   window_type_for_reg = "character",
                                   train_args = "list"),
                      contains = "sim",
                      prototype = list(name = "ARIMA",
                                       res_dist = "normal",
                                       window_size_for_reg = NA_real_,
                                       window_type_for_reg = NA_character_,
                                       train_args = list()),
                      validity = check_valid_lm_sim)


#' @describeIn train_model Train LM Model specific to lm_sim object.
setMethod("train_model",
          signature(object = "lm_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- convert_frequency_dataset(setNames(train_x[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):length(train_x)], rownames(train_x)[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):length(train_x)]), object@window_size, object@response, keep.names = TRUE, right.aligned = TRUE)
            new_train_xreg <- convert_frequency_dataset_overlapping(setNames(train_xreg[1:(length(train_x) - object@window_size * object@extrap_step)], rownames(train_xreg)[1:(length(train_x) - object@window_size * object@extrap_step)]), object@window_size_for_reg, object@window_type_for_reg, keep.names = TRUE, jump = object@window_size)

            args.method <- list("data" = data.frame("new_train_x" = new_train_x, "new_train_xreg" = new_train_xreg))
            for (i in names(object@train_args)) {
              args.method[[i]] <- object@train_args[[i]]
            }

            trained_result <- do.call(stats::lm, c(list("formula" = stats::as.formula("new_train_x~new_train_xreg")), args.method))
            trained_result$call$x <- train_x
            trained_result$call$xreg <- train_xreg

            if (object@res_dist == "skew_norm") {
              res <- stats::residuals(trained_result)
              skew_res <- sample_moment_lag(res, k = 0, r = 3, s = 0) / (sample_moment_lag(res, k = 0, r = 2, s = 0) ^ (3/2))
              abs_skew_res <- min(abs(skew_res), 0.99)

              # alpha
              delta <- sign(skew_res) * sqrt((pi / 2) * (abs_skew_res^(2/3)) / ((abs_skew_res ^ (2/3)) + (2 - 0.5 * pi) ^ (2/3)))
              alpha <- delta / sqrt(1 - delta ^ 2)

              # omega
              omega2 <- sample_moment_lag(res, k = 0, r = 2, s = 0) / (1 - 2 / pi * delta ^ (2))
              omega <- sqrt(omega2)

              # xi
              xi <- 0 - sqrt(pi / 2) * omega * delta

              trained_result <- c(trained_result, list("xi" = xi, "omega" = omega, "alpha" = alpha))
            }
            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained LM Model.
setMethod("do_prediction",
          signature(object = "lm_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            new_test_xreg <- c(trained_result$call$xreg, test_xreg[,1])

            new_test_xreg <- convert_frequency_dataset_overlapping(new_test_xreg[(length(new_test_xreg) - object@window_size * object@extrap_step - object@window_size * (object@extrap_step - 1) - max(object@window_size_for_reg, object@window_size) + 1):(length(new_test_xreg) - object@window_size * object@extrap_step)], object@window_size_for_reg, object@window_type_for_reg, keep.names = TRUE, jump = object@window_size)

            predict_result <- stats::predict(trained_result, newdata = data.frame("new_train_xreg" = new_test_xreg), interval = "prediction", level = level / 100)

            expected <- as.numeric(predict_result[,"fit"])
            pi_up <- as.numeric(predict_result[, "upr"])

            if (object@res_dist == "skew_norm") {
              xi <- trained_result$xi + as.numeric(predict_result$mean)
              omega <- trained_result$omega
              alpha <- trained_result$alpha

              expected <- xi + sqrt(2 / pi) * omega * (alpha / sqrt(1 + alpha ^ 2))
              pi_up <- max(sn::qsn(level / 100, xi = xi, omega = omega, alpha = alpha))

              predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "pi_up"] <- pi_up
              predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "expected"] <- expected
            }

            predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "pi_up"] <- pi_up
            predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "expected"] <- expected
            return(predict_info)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "lm_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "lm_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            character_lst[["res_dist"]] <- methods::slot(object, "res_dist")
            character_lst[["window_size_for_reg"]] <- methods::slot(object, "window_size_for_reg")
            character_lst[["window_type_for_reg"]] <- methods::slot(object, "window_type_for_reg")
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "lm_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["train_args"]] <- methods::slot(object, "train_args")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "lm_sim",
      function(from) {
        object <- methods::new("lm_sim")
        for (i in names(from)) {
          if (i %in% methods::slotNames(object)) {
            if (methods::is(from[, i], "character")) {
              if (length(strsplit(from[, i], ",")[[1]]) == 1) {
                methods::slot(object, i) <- from[, i]
              } else {
                methods::slot(object, i) <- as.numeric(strsplit(from[, i], ",")[[1]])
              }
            } else {
              methods::slot(object, i) <- from[, i]
            }
          }
        }
        return(object)
      })
