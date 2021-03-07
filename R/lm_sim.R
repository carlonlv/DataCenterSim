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
  if (length(object@window_type_for_reg) < 1 | any(is.na(object@window_type_for_reg)) | all(object@window_type_for_reg != window_type_choices)) {
    msg <- paste0("window_type_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (length(object@window_size_for_reg) < 1 | any(is.na(object@window_size_for_reg)) | object@window_size_for_reg %% 1 != 0) {
    msg <- paste0("window_size_for_reg must be an integer.")
    errors <- c(errors, msg)
  }
  if (length(object@res_dist) != 1 | is.na(object@res_dist) | all(object@res_dist != res_dist_choices)) {
    msg <- paste0("res_dist must be one of ", paste(res_dist_choices, collapse = " "), ".")
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
                      prototype = list(name = "LM",
                                       res_dist = "normal",
                                       window_size_for_reg = 12,
                                       window_type_for_reg = "max",
                                       train_args = list()),
                      validity = check_valid_lm_sim)


#' @describeIn train_model Train LM Model specific to lm_sim object.
setMethod("train_model",
          signature(object = "lm_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- convert_frequency_dataset(stats::setNames(train_x[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x),1], rownames(train_x)[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):length(train_x)]),
                                                     object@window_size,
                                                     object@response,
                                                     keep.names = TRUE,
                                                     right.aligned = TRUE)
            new_train_xreg <- convert_frequency_dataset_overlapping(stats::setNames(train_xreg[1:(nrow(train_xreg) - object@window_size * object@extrap_step),1], rownames(train_xreg)[1:(nrow(train_xreg) - object@window_size * object@extrap_step)]),
                                                                    object@window_size_for_reg,
                                                                    object@window_type_for_reg,
                                                                    keep.names = TRUE,
                                                                    jump = object@window_size,
                                                                    right.aligned = TRUE,
                                                                    length.out = length(new_train_x))


            args.method <- list("data" = data.frame("new_train_x" = new_train_x, "new_train_xreg" = new_train_xreg))
            for (i in names(object@train_args)) {
              args.method[[i]] <- object@train_args[[i]]
            }

            trained_result <- do.call(stats::lm, c(list("formula" = stats::as.formula("new_train_x~new_train_xreg")), args.method))
            trained_result$call$x <- new_train_x
            trained_result$call$xreg <- new_train_xreg
            trained_result$call$orig_x <- train_x
            trained_result$call$orig_xreg <- train_xreg

            if (object@res_dist == "skew_norm") {
              trained_result <- c(trained_result, skew_norm_param_estimation(trained_result$residuals))
            }
            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained LM Model.
setMethod("do_prediction",
          signature(object = "lm_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- 1 - object@cut_off_prob * 2

            new_test_xreg <- c(trained_result$call$orig_xreg[,1], test_xreg[,1])
            new_test_xreg <- convert_frequency_dataset_overlapping(new_test_xreg,
                                                                   object@window_size_for_reg,
                                                                   object@window_type_for_reg,
                                                                   keep.names = TRUE,
                                                                   jump = object@window_size,
                                                                   right.aligned = TRUE,
                                                                   length.out = object@extrap_step)

            pi_up <- matrix(nrow = object@extrap_step, ncol = 0)
            for (i in sort(level)) {
              predict_result <- stats::predict(trained_result, newdata = data.frame("new_train_xreg" = new_test_xreg), interval = "prediction", level = i, se.fit = TRUE)
              pi_up <- cbind(pi_up, as.matrix(predict_result$fit[,"upr"]))
            }
            colnames(pi_up) <- paste0("Quantile_", sort(1 - object@cut_off_prob))

            expected <- data.frame("expected" = as.numeric(predict_result$fit[,"fit"]))
            predicted_params <- data.frame("mean" = as.numeric(predict_result$fit[,"fit"]), "sd" = (pi_up[,1] - expected[,1]) / stats::qnorm(sort(1 - object@cut_off_prob)[1]))

            if (object@res_dist == "skew_norm") {
              skewnorm_prediction_result <- skew_norm_param_prediction(object, trained_result, as.numeric(predict_result$fit[,"fit"]), level)

              expected <- skewnorm_prediction_result$expected
              pi_up <- skewnorm_prediction_result$pi_up
              predicted_params <- skewnorm_prediction_result$predicted_params
            }

            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @describeIn train_model Train LM Model specific to lm_sim object.
setMethod("train_model",
          signature(object = "lm_sim", train_x = "matrix", train_xreg = "list", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- convert_frequency_dataset(stats::setNames(train_x[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x),1], rownames(train_x)[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):length(train_x)]),
                                                     object@window_size,
                                                     object@response,
                                                     keep.names = TRUE,
                                                     right.aligned = TRUE)
            new_train_xreg <- do.call(cbind, lapply(1:length(train_xreg), function(reg) {
              temp_reg <- train_xreg[[reg]]
              as.matrix(convert_frequency_dataset_overlapping(stats::setNames(temp_reg[1:(nrow(temp_reg) - object@window_size * object@extrap_step),1], rownames(temp_reg)[1:(nrow(temp_reg) - object@window_size * object@extrap_step)]),
                                                              object@window_size_for_reg[reg],
                                                              object@window_type_for_reg[reg],
                                                              keep.names = TRUE,
                                                              jump = object@window_size,
                                                              right.aligned = TRUE,
                                                              length.out = length(new_train_x)))
            }))
            colnames(new_train_xreg) <- names(train_xreg)

            args.method <- list("data" = stats::setNames(as.data.frame(cbind(new_train_x, new_train_xreg)),
                                                         c("new_train_x", colnames(new_train_xreg))))
            args.method$data <- c
            for (i in names(object@train_args)) {
              args.method[[i]] <- object@train_args[[i]]
            }

            trained_result <- do.call(stats::lm, c(list("formula" = stats::as.formula("new_train_x~new_train_xreg")), args.method))
            trained_result$call$x <- new_train_x
            trained_result$call$xreg <- new_train_xreg
            trained_result$call$orig_x <- train_x
            trained_result$call$orig_xreg <- train_xreg

            if (object@res_dist == "skew_norm") {
              trained_result <- c(trained_result, skew_norm_param_estimation(trained_result$residuals))
            }
            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained LM Model.
setMethod("do_prediction",
          signature(object = "lm_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "list"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- 1 - object@cut_off_prob * 2

            new_test_xreg <- do.call(cbind, lapply(1:length(test_xreg), function(reg) {
              temp_xreg <- c(trained_result$call$orig_xreg[[reg]][,1], test_xreg[[reg]][,1])
              convert_frequency_dataset_overlapping(temp_xreg,
                                                    object@window_size_for_reg[reg],
                                                    object@window_type_for_reg[reg],
                                                    keep.names = TRUE,
                                                    jump = object@window_size,
                                                    right.aligned = TRUE,
                                                    length.out = object@extrap_step)
            }))
            colnames(new_test_xreg) <- colnames(trained_result$call$xreg)

            pi_up <- matrix(nrow = object@extrap_step, ncol = 0)
            for (i in sort(level)) {
              predict_result <- stats::predict(trained_result, newdata = as.data.frame(new_test_xreg), interval = "prediction", level = i, se.fit = TRUE)
              pi_up <- cbind(pi_up, as.matrix(predict_result$fit[,"upr"]))
            }
            colnames(pi_up) <- paste0("Quantile_", sort(1 - object@cut_off_prob))

            expected <- data.frame("expected" = as.numeric(predict_result$fit[,"fit"]))
            predicted_params <- data.frame("mean" = as.numeric(predict_result$fit[,"fit"]), "sd" = (pi_up[,1] - expected[,1]) / stats::qnorm(sort(1 - object@cut_off_prob)[1]))

            if (object@res_dist == "skew_norm") {
              skewnorm_prediction_result <- skew_norm_param_prediction(object, trained_result, as.numeric(predict_result$fit[,"fit"]), level)

              expected <- skewnorm_prediction_result$expected
              pi_up <- skewnorm_prediction_result$pi_up
              predicted_params <- skewnorm_prediction_result$predicted_params
            }

            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
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
            hidden_lst[["window_size_for_reg"]] <- methods::slot(object, "window_size_for_reg")
            hidden_lst[["window_type_for_reg"]] <- methods::slot(object, "window_type_for_reg")
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
