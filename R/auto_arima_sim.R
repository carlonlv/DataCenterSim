#' @include sim_class.R generics.R model_helper.R
NULL


#' Validity Checker for auto_arima_sim Object
#'
#' @param object A auto_arima_sim object.
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_auto_arima_sim <- function(object) {
  errors <- character()
  window_type_choices <- c("max", "avg")
  res_dist_choices <- c("normal", "skew_norm", "empirical")
  outlier_type_choices <- c("AO", "IO", "LS", "None", "All")
  outlier_prediction_choices <- c("None", "Categorical", "Categorical-Dirichlet")
  if (any(is.na(object@window_type_for_reg)) | all(object@window_type_for_reg != window_type_choices)) {
    msg <- paste0("window_type_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (length(object@res_dist) != 1 | is.na(object@res_dist) | all(object@res_dist != res_dist_choices)) {
    msg <- paste0("res_dist must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@outlier_type) != 1 | is.na(object@outlier_type) |  all(object@outlier_type != outlier_type_choices)) {
    msg <- paste0("outlier_type must be one of ", paste(outlier_type_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@outlier_cval) != 1 | any(object@outlier_cval < 3, na.rm = TRUE) | any(object@outlier_cval > 4, na.rm = TRUE)) {
    msg <- paste0("outlier_cval must be a numeric value within 3 and 4, inclusively, or NA.")
    errors <- c(errors, msg)
  }
  if (length(object@outlier_prediction) != 1 | is.na(object@outlier_prediction) | all(object@outlier_prediction != outlier_prediction_choices)) {
    msg <- paste0("outlier_prediction must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@outlier_prediction_update_param) != 1 | is.na(object@outlier_prediction_update_param)) {
    msg <- paste0("outlier_prediction_update_param must be length one logical value.")
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
#' @param outlier_type A character representing the type of outlier it will be treated, it can be None for not checking outliers, AO as additive outliers, IO as innovative outliers, LS as level shift, or All for taking account of all outlier types. Default value is \code{"None"}.
#' @param outlier_cval A numeric value representing the critical value to determine the significance of each type of outlier. If NA_real_ is supplied, then it uses defaults: If n ≤ 50 then cval is set equal to 3.0; If n ≥ 450 then cval is set equal to 4.0; otherwise cval is set equal to 3 + 0.0025 * (n - 50). Default value is \code{NA_real_}.
#' @param outlier_prediction A character representing the distribution of occurences of outliers, and the prior distribution for probability of occurences. Current choices are "None", "Categorical" for predictions based on MLE, "Categorical-Dirichlet" for Bayesian prediction and prior distribution.
#' @param outlier_prediction_prior A numeric vector representing the starting value of the hyperparameters of prior distirbution.
#' @param outlier_prediction_update_param A logical value representing whether to update the value of the hyperparameters of prior distribution or MLE estimations of parameters depending on \code{outlier_prediction}.
#' @param train_args A list representing additional call passed into the training function, \code{forecast::auto.arima}. Default value is \code{list("order" = c(1, 0, 0))}.
#' @export auto_arima_sim
auto_arima_sim <- setClass("auto_arima_sim",
                      slots = list(window_size_for_reg = "numeric",
                                   window_type_for_reg = "character",
                                   res_dist = "character",
                                   outlier_type = "character",
                                   outlier_cval = "numeric",
                                   outlier_prediction = "character",
                                   outlier_prediction_prior = "numeric",
                                   outlier_prediction_update_param = "logical",
                                   train_args = "list"),
                      contains = "sim",
                      prototype = list(window_size_for_reg = NA_real_,
                                       window_type_for_reg = NA_character_,
                                       name = "ARIMA",
                                       res_dist = "normal",
                                       outlier_type = "None",
                                       outlier_cval = NA_real_,
                                       outlier_prediction = "None",
                                       outlier_prediction_prior = NA_real_,
                                       outlier_prediction_update_param = TRUE,
                                       train_args = list("order" = c(1, 0, 0))),
                      validity = check_valid_auto_arima_sim)


#' @describeIn train_model Train ARMA Model specific to auto_arima_sim object.
setMethod("train_model",
          signature(object = "auto_arima_sim", train_x = "matrix", train_xreg = "NULL", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(train_x, object@window_size, object@response, keep.names = TRUE))

            args.tsmethod <- list("include.mean" = TRUE, "method" = ifelse(object@res_dist == "normal", "ML", "CSS"), "optim.method" = "Nelder-Mead", "optim.control" = list(maxit = 5000))
            for (i in names(object@train_args)) {
              args.tsmethod[[i]] <- object@train_args[[i]]
            }

            if (object@outlier_type == "None") {
              trained_result <- do.call(forecast::auto.arima, c(list("y" = new_train_x), args.tsmethod))
              trained_result$call$x <- new_train_x
              trained_result$call$orig_x <- train_x
            } else {
              if (is.na(object@outlier_cval)) {
                cval <- ifelse(length(new_train_x) <= 50, 3, ifelse(length(new_train_x) >= 450, 4, 3 + 0.0025 * (length(new_train_x) - 50)))
              } else {
                cval <- object@outlier_cval
              }

              if (object@outlier_type == "All") {
                ol_type <- c("AO", "IO", "TC")
              } else {
                ol_type <- object@outlier_type
              }

              trained_result <- tryCatch({
                tso_model <- tsoutliers::tso(y = new_train_x, types = ol_type, cval = cval, maxit = 4, tsmethod = "auto.arima", args.tsmethod = args.tsmethod, maxit.oloop = 20, maxit.iloop = 10)
                tso_model$fit$call$orig_x <- train_x
                param_mle <- estimate_outliers(object, ol_type, new_train_x, tso_model, trained_model)
                c(tso_model$fit, list("param_mle" = param_mle))
              }, error = function(e) {
                ts_model <- do.call(forecast::auto.arima, c(list("y" = new_train_x), args.tsmethod))
                ts_model$call$x <- new_train_x
                ts_model$call$orig_x <- train_x
                ts_model
              })
            }

            if (object@res_dist == "skew_norm") {
              trained_result <- c(trained_result, skew_norm_param_estimation(trained_result$residuals))
            }
            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "auto_arima_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "NULL"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (object@res_dist == "empirical") {
              bootstrap <- TRUE
            } else {
              bootstrap <- FALSE
            }

            if (nrow(predict_info) == 0) {
              if (is.null(trained_result$call$xreg)) {
                target_model <- forecast::auto.arima(y = trained_result$call$x, model = trained_result)
              } else {
                target_model <- forecast::auto.arima(y = trained_result$call$x, xreg = trained_result$call$xreg, model = trained_result)
              }
            } else {
              new_x <- c(trained_result$call$x, predict_info$actual)
              res <- stats::ts(c(trained_result$residuals, predict_info$residuals))

              if (object@outlier_type != "None") {
                new_x <- find_and_remove_outliers_testing_set(object, trained_result, new_x, res)
              }

              prev_xreg <- trained_result$call$xreg
              if (is.null(prev_xreg)) {
                # Outliers are not found.
                target_model <- forecast::Arima(new_x, model = trained_result)
              } else {
                # Outliers are considered and found
                new_xreg <- matrix(0, nrow = length(new_x) - length(trained_result$call$x), ncol = ncol(prev_xreg))
                colnames(new_xreg) <- colnames(prev_xreg)
                new_xreg <- rbind(prev_xreg, new_xreg)
                target_model <- forecast::Arima(new_x, xreg = new_xreg, model = trained_result)
              }
            }

            if (is.null(trained_result$call$xreg)) {
              # Outliers are not considered or outliers are not found
              predict_result <- forecast::forecast(target_model, h = object@extrap_step, bootstrap = bootstrap, npaths = length(trained_result$call$x), level = level)
            } else {
              dxreg <- matrix(0, nrow = object@extrap_step, ncol = ncol(trained_result$call$xreg))
              colnames(dxreg) <- colnames(trained_result$call$xreg)
              predict_result <- forecast::forecast(target_model, xreg = dxreg, h = object@extrap_step, bootstrap = bootstrap, npaths = length(trained_result$call$x), level = level)
            }

            expected <- stats::setNames(as.data.frame(as.numeric(predict_result$mean)), "expected")
            pi_up <- stats::setNames(as.data.frame(predict_result$upper), paste0("Quantile_", sort(1 - object@cut_off_prob)))
            predicted_params <- data.frame("mean" = as.numeric(predict_result$mean), "sd" = (pi_up[,1] - expected[,1]) / stats::qnorm(sort(1 - object@cut_off_prob)[1]))

            if (object@res_dist == "skew_norm") {
              skewnorm_prediction_result <- skew_norm_param_prediction(object, trained_result, as.numeric(predict_result$mean), level)

              expected <- skewnorm_prediction_result$expected
              pi_up <- skewnorm_prediction_result$pi_up
              predicted_params <- skewnorm_prediction_result$predicted_params
            }

            if (object@res_dist == "normal" & object@outlier_type != "None" & object@outlier_prediction != "None" & !is.null(trained_result$param_mle)) {
              outlier_prediction_result <- prediction_including_outlier_effect(object, trained_result, pi_up, expected, level)

              expected <- outlier_prediction_result$expected
              pi_up <- outlier_prediction_result$pi_up
              predicted_params <- outlier_prediction_result$predicted_params
            }

            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @describeIn train_model Train ARMA Model specific to auto_arima_sim object.
setMethod("train_model",
          signature(object = "auto_arima_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(stats::setNames(train_x[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x),1], rownames(train_x)[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):length(train_x)]),
                                                               object@window_size,
                                                               object@response,
                                                               keep.names = TRUE,
                                                               right.aligned = TRUE))
            new_train_xreg <- as.matrix(convert_frequency_dataset_overlapping(stats::setNames(train_xreg[1:(nrow(train_xreg) - object@window_size * object@extrap_step),1], rownames(train_xreg)[1:(nrow(train_xreg) - object@window_size * object@extrap_step)]),
                                                                              object@window_size_for_reg,
                                                                              object@window_type_for_reg,
                                                                              keep.names = TRUE,
                                                                              jump = object@window_size,
                                                                              right.aligned = TRUE,
                                                                              length.out = length(new_train_x)))
            colnames(new_train_xreg) <- "xreg"

            args.tsmethod <- list("include.mean" = TRUE, "method" = ifelse(object@res_dist == "normal", "ML", "CSS"), "optim.method" = "Nelder-Mead", "optim.control" = list(maxit = 5000))
            for (i in names(object@train_args)) {
              args.tsmethod[[i]] <- object@train_args[[i]]
            }

            if (object@outlier_type == "None") {
              trained_result <- do.call(forecast::auto.arima, c(list("y" = new_train_x, "xreg" = new_train_xreg), args.tsmethod))
              trained_result$call$x <- new_train_x
              trained_result$call$xreg <- new_train_xreg
              trained_result$call$orig_x <- train_x
              trained_result$call$orig_xreg <- train_xreg
            } else {
              if (is.na(object@outlier_cval)) {
                cval <- ifelse(length(new_train_x) <= 50, 3, ifelse(length(new_train_x) >= 450, 4, 3 + 0.0025 * (length(new_train_x) - 50)))
              } else {
                cval <- object@outlier_cval
              }

              if (object@outlier_type == "All") {
                ol_type <- c("AO", "IO", "TC")
              } else {
                ol_type <- object@outlier_type
              }

              trained_result <- tryCatch({
                tso_model <- tsoutliers::tso(y = new_train_x, xreg = new_train_xreg, types = ol_type, cval = cval, maxit = 4, tsmethod = "auto.arima", args.tsmethod = args.tsmethod, maxit.oloop = 20, maxit.iloop = 10)
                tso_model$fit$call$orig_x <- train_x
                tso_model$fit$call$orig_xreg <- train_xreg
                param_mle <- estimate_outliers(object, ol_type, new_train_x, tso_model, trained_model)
                c(tso_model$fit, list("param_mle" = param_mle))
              }, error = function(e) {
                ts_model <- do.call(forecast::auto.arima, c(list("y" = new_train_x, "xreg" = new_train_xreg), args.tsmethod))
                ts_model$call$x <- new_train_x
                ts_model$call$xreg <- new_train_xreg
                ts_model$call$orig_x <- train_x
                ts_model$call$orig_xreg <- train_xreg
                ts_model
              })
            }

            if (object@res_dist == "skew_norm") {
              trained_result <- c(trained_result, skew_norm_param_estimation(trained_result$residuals))
            }
            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "auto_arima_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (object@res_dist == "empirical") {
              bootstrap <- TRUE
            } else {
              bootstrap <- FALSE
            }

            if (nrow(predict_info) == 0) {
              target_model <- forecast::Arima(y = trained_result$call$x, xreg = trained_result$call$xreg, model = trained_result)
            } else {
              new_x <- c(trained_result$call$x, predict_info$actual)
              res <- stats::ts(c(trained_result$residuals, predict_info$residuals))

              if (object@outlier_type != "None") {
                new_x <- find_and_remove_outliers_testing_set(object, trained_result, new_x, res)
              }

              prev_xreg <- trained_result$call$xreg

              new_xreg <- c(trained_result$call$orig_xreg[,1], test_xreg[,1])
              new_xreg <- as.matrix(convert_frequency_dataset_overlapping(new_xreg[1:(length(new_xreg) - object@window_size * object@extrap_step)],
                                                                          object@window_size_for_reg,
                                                                          object@window_type_for_reg,
                                                                          keep.names = TRUE,
                                                                          jump = object@window_size,
                                                                          right.aligned = TRUE,
                                                                          length.out = length(predict_info$actual)))
              if (ncol(prev_xreg) > 1) {
                # Outliers are considered and found.
                new_ol <- matrix(0, nrow = nrow(new_xreg), ncol = ncol(prev_xreg) - 1)
                new_xreg <- cbind(new_xreg, new_ol)
              }
              colnames(new_xreg) <- colnames(prev_xreg)
              new_xreg <- rbind(prev_xreg, new_xreg)
              target_model <- forecast::Arima(new_x, xreg = new_xreg, model = trained_result)
            }

            dxreg <- c(trained_result$call$orig_xreg[,1], test_xreg[,1])
            dxreg <- as.matrix(convert_frequency_dataset_overlapping(dxreg,
                                                                     object@window_size_for_reg,
                                                                     object@window_type_for_reg,
                                                                     keep.names = TRUE,
                                                                     jump = object@window_size,
                                                                     right.aligned = TRUE,
                                                                     length.out = object@extrap_step))
            if (ncol(dxreg) < ncol(trained_result$call$xreg)) {
              dxreg <- cbind(dxreg, matrix(0, nrow = object@extrap_step, ncol = (ncol(trained_result$call$xreg) - ncol(dxreg))))
            }
            colnames(dxreg) <- colnames(trained_result$call$xreg)
            predict_result <- forecast::forecast(target_model, xreg = dxreg, h = object@extrap_step, bootstrap = bootstrap, npaths = length(trained_result$call$x), level = level)

            expected <- stats::setNames(as.data.frame(as.numeric(predict_result$mean)), "expected")
            pi_up <- stats::setNames(as.data.frame(predict_result$upper), paste0("Quantile_", sort(1 - object@cut_off_prob)))
            predicted_params <- data.frame("mean" = as.numeric(predict_result$mean), "sd" = (pi_up[,1] - expected[,1]) / stats::qnorm(sort(1 - object@cut_off_prob)[1]))

            if (object@res_dist == "skew_norm") {
              skewnorm_prediction_result <- skew_norm_param_prediction(object, trained_result, as.numeric(predict_result$mean), level)

              expected <- skewnorm_prediction_result$expected
              pi_up <- skewnorm_prediction_result$pi_up
              predicted_params <- skewnorm_prediction_result$predicted_params
            }

            if (object@res_dist == "normal" & object@outlier_type != "None" & object@outlier_prediction != "None" & !is.null(trained_result$param_mle)) {
              outlier_prediction_result <- prediction_including_outlier_effect(object, trained_result, pi_up, expected, level)

              expected <- outlier_prediction_result$expected
              pi_up <- outlier_prediction_result$pi_up
              predicted_params <- outlier_prediction_result$predicted_params
            }

            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @describeIn train_model Train ARMA Model specific to auto_arima_sim object.
setMethod("train_model",
          signature(object = "auto_arima_sim", train_x = "matrix", train_xreg = "list", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(stats::setNames(train_x[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x),1], rownames(train_x)[(max(object@window_size_for_reg, object@window_size) + (object@extrap_step - 1) * object@window_size + 1):nrow(train_x)]),
                                                               object@window_size,
                                                               object@response,
                                                               keep.names = TRUE,
                                                               right.aligned = TRUE))
            new_train_xreg <- do.call(cbind, lapply(1:length(train_xreg), function(reg) {
              temp_reg <- train_xreg[[reg]]
              as.matrix(convert_frequency_dataset_overlapping(stats::setNames(temp_reg[1:(nrow(temp_reg) - object@window_size * object@extrap_step),1], rownames(temp_reg)[1:(nrow(temp_reg) - object@window_size * object@extrap_step)]),
                                                              ifelse(length(object@window_size_for_reg) == 1, object@window_size_for_reg, object@window_size_for_reg[reg]),
                                                              ifelse(length(object@window_type_for_reg) == 1, object@window_type_for_reg, object@window_type_for_reg[reg]),
                                                              keep.names = TRUE,
                                                              jump = object@window_size,
                                                              right.aligned = TRUE,
                                                              length.out = length(new_train_x)))
            }))
            colnames(new_train_xreg) <- names(train_xreg)

            args.tsmethod <- list("include.mean" = TRUE, "method" = ifelse(object@res_dist == "normal", "ML", "CSS"), "optim.method" = "Nelder-Mead", "optim.control" = list(maxit = 5000))
            for (i in names(object@train_args)) {
              args.tsmethod[[i]] <- object@train_args[[i]]
            }

            if (object@outlier_type == "None") {
              trained_result <- do.call(forecast::auto.arima, c(list("y" = new_train_x, "xreg" = new_train_xreg), args.tsmethod))
              trained_result$call$x <- new_train_x
              trained_result$call$xreg <- new_train_xreg
              trained_result$call$orig_x <- train_x
              trained_result$call$orig_xreg <- train_xreg
            } else {
              if (is.na(object@outlier_cval)) {
                cval <- ifelse(length(new_train_x) <= 50, 3, ifelse(length(new_train_x) >= 450, 4, 3 + 0.0025 * (length(new_train_x) - 50)))
              } else {
                cval <- object@outlier_cval
              }

              if (object@outlier_type == "All") {
                ol_type <- c("AO", "IO", "TC")
              } else {
                ol_type <- object@outlier_type
              }

              trained_result <- tryCatch({
                tso_model <- tsoutliers::tso(y = new_train_x, xreg = new_train_xreg, types = ol_type, cval = cval, maxit = 4, tsmethod = "auto.arima", args.tsmethod = args.tsmethod, maxit.oloop = 20, maxit.iloop = 10)
                tso_model$call$orig_x <- train_x
                tso_model$call$orig_xreg <- train_xreg
                param_mle <- estimate_outliers(object, ol_type, new_train_x, tso_model, trained_model)
                c(tso_model$fit, list("param_mle" = param_mle))
              }, error = function(e) {
                ts_model <- do.call(forecast::auto.arima, c(list("y" = new_train_x, "xreg" = new_train_xreg), args.tsmethod))
                ts_model$call$x <- new_train_x
                ts_model$call$xreg <- new_train_xreg
                ts_model$call$orig_x <- train_x
                ts_model$call$orig_xreg <- train_xreg
                ts_model
              })
            }

            if (object@res_dist == "skew_norm") {
              trained_result <- c(trained_result, skew_norm_param_estimation(trained_result$residuals))
            }
            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "auto_arima_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "list"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (object@res_dist == "empirical") {
              bootstrap <- TRUE
            } else {
              bootstrap <- FALSE
            }

            if (nrow(predict_info) == 0) {
              target_model <- forecast::Arima(y = trained_result$call$x, xreg = trained_result$call$xreg, model = trained_result)
            } else {
              new_x <- c(trained_result$call$x, predict_info$actual)
              res <- stats::ts(c(trained_result$residuals, predict_info$residuals))

              if (object@outlier_type != "None") {
                new_x <- find_and_remove_outliers_testing_set(object, trained_result, new_x, res)
              }

              prev_xreg <- trained_result$call$xreg

              new_xreg <- do.call(cbind, lapply(1:length(test_xreg), function(reg) {
                temp_xreg <- c(trained_result$call$orig_xreg[[reg]][,1], test_xreg[[reg]][,1])
                convert_frequency_dataset_overlapping(temp_xreg[1:(length(temp_xreg) - object@window_size * object@extrap_step)],
                                                      ifelse(length(object@window_size_for_reg) == 1, object@window_size_for_reg, object@window_size_for_reg[reg]),
                                                      ifelse(length(object@window_type_for_reg) == 1, object@window_type_for_reg, object@window_type_for_reg[reg]),
                                                      keep.names = TRUE,
                                                      jump = object@window_size,
                                                      right.aligned = TRUE,
                                                      length.out = length(predict_info$actual))
              }))

              if (ncol(prev_xreg) > ncol(new_xreg)) {
                # Outliers are considered and found.
                new_ol <- matrix(0, nrow = nrow(new_xreg), ncol = ncol(prev_xreg) - ncol(new_xreg))
                new_xreg <- cbind(new_xreg, new_ol)
              }
              colnames(new_xreg) <- colnames(prev_xreg)
              new_xreg <- rbind(prev_xreg, new_xreg)
              target_model <- forecast::Arima(new_x, xreg = new_xreg, model = trained_result)
            }

            dxreg <- do.call(cbind, lapply(1:length(test_xreg), function(reg) {
              temp_xreg <- c(trained_result$call$orig_xreg[[reg]][,1], test_xreg[[reg]][,1])
              convert_frequency_dataset_overlapping(temp_xreg,
                                                    ifelse(length(object@window_size_for_reg) == 1, object@window_size_for_reg, object@window_size_for_reg[reg]),
                                                    ifelse(length(object@window_type_for_reg) == 1, object@window_type_for_reg, object@window_type_for_reg[reg]),
                                                    keep.names = TRUE,
                                                    jump = object@window_size,
                                                    right.aligned = TRUE,
                                                    length.out = object@extrap_step)
            }))
            if (ncol(dxreg) < ncol(trained_result$call$xreg)) {
              dxreg <- cbind(dxreg, matrix(0, nrow = object@extrap_step, ncol = (ncol(trained_result$call$xreg) - ncol(test_xreg))))
            }
            colnames(dxreg) <- colnames(trained_result$call$xreg)
            predict_result <- forecast::forecast(target_model, xreg = dxreg, h = object@extrap_step, bootstrap = bootstrap, npaths = length(trained_result$call$x), level = level)

            expected <- stats::setNames(as.data.frame(as.numeric(predict_result$mean)), "expected")
            pi_up <- stats::setNames(as.data.frame(predict_result$upper), paste0("Quantile_", sort(1 - object@cut_off_prob)))
            predicted_params <- data.frame("mean" = as.numeric(predict_result$mean), "sd" = (pi_up[,1] - expected[,1]) / stats::qnorm(sort(1 - object@cut_off_prob)[1]))

            if (object@res_dist == "skew_norm") {
              skewnorm_prediction_result <- skew_norm_param_prediction(object, trained_result, as.numeric(predict_result$mean), level)

              expected <- skewnorm_prediction_result$expected
              pi_up <- skewnorm_prediction_result$pi_up
              predicted_params <- skewnorm_prediction_result$predicted_params
            }

            if (object@res_dist == "normal" & object@outlier_type != "None" & object@outlier_prediction != "None" & !is.null(trained_result$param_mle)) {
              outlier_prediction_result <- prediction_including_outlier_effect(object, trained_result, pi_up, expected, level)

              expected <- outlier_prediction_result$expected
              pi_up <- outlier_prediction_result$pi_up
              predicted_params <- outlier_prediction_result$predicted_params
            }

            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @return A list containing all numeric parameter information.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "auto_arima_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["outlier_cval"]] <- methods::slot(object, "outlier_cval")
            return(numeric_lst)
          })


#' @return A list containing all character parameter information.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "auto_arima_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            character_lst[["res_dist"]] <- methods::slot(object, "res_dist")
            character_lst[["outlier_type"]] <- methods::slot(object, "outlier_type")
            character_lst[["outlier_prediction"]] <- methods::slot(object, "outlier_prediction")
            character_lst[["outlier_prediction_update_param"]] <- methods::slot(object, "outlier_prediction_update_param")
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "auto_arima_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["train_args"]] <- methods::slot(object, "train_args")
            hidden_lst[["outlier_prediction_prior"]] <- methods::slot(object, "outlier_prediction_prior")
            hidden_lst[["window_size_for_reg"]] <- methods::slot(object, "window_size_for_reg")
            hidden_lst[["window_type_for_reg"]] <- methods::slot(object, "window_type_for_reg")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "auto_arima_sim",
      function(from) {
        object <- methods::new("auto_arima_sim")
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
