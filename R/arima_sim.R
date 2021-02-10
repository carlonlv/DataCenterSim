#' @include sim_class.R generics.R
NULL


#' Validity Checker for arima_sim Object
#'
#' @param object A arima_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_arima_sim <- function(object) {
  errors <- character()
  res_dist_choices <- c("normal", "skew_norm", "empirical")
  outlier_type_choices <- c("AO", "IO", "LS", "None", "All")
  outlier_prediction_choices <- c("None", "Categorical", "Categorical-Dirichlet")
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
#' @param train_args A list representing additional call passed into the training function, \code{forecast::Arima}. Default value is \code{list("order" = c(1, 0, 0))}.
#' @export arima_sim
arima_sim <- setClass("arima_sim",
                    slots = list(res_dist = "character",
                                 outlier_type = "character",
                                 outlier_cval = "numeric",
                                 outlier_prediction = "character",
                                 outlier_prediction_prior = "numeric",
                                 outlier_prediction_update_param = "logical",
                                 train_args = "list"),
                    contains = "sim",
                    prototype = list(name = "ARIMA",
                                     res_dist = "normal",
                                     outlier_type = "None",
                                     outlier_cval = NA_real_,
                                     outlier_prediction = "None",
                                     outlier_prediction_prior = NA_real_,
                                     outlier_prediction_update_param = TRUE,
                                     train_args = list("order" = c(1, 0, 0))),
                    validity = check_valid_arima_sim)


#' @describeIn train_model Train ARMA Model specific to arima_sim object.
setMethod("train_model",
          signature(object = "arima_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(train_x, object@window_size, object@response, keep.names = TRUE))

            if (length(train_xreg) == 0) {
              new_train_xreg <- NULL
            } else {
              new_train_xreg <- as.matrix(convert_frequency_dataset(train_xreg, object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)], keep.names = TRUE))
              colnames(new_train_xreg) <- "xreg"
            }

            if (is.na(object@outlier_cval)) {
              cval <- ifelse(length(new_train_x) <= 50, 3, ifelse(length(new_train_x) >= 450, 4, 3 + 0.0025 * (length(new_train_x) - 50)))
            } else {
              cval <- object@outlier_cval
            }

            args.tsmethod <- list("include.mean" = TRUE, "method" = ifelse(object@res_dist == "normal", "ML", "CSS"), "optim.method" = "Nelder-Mead", "optim.control" = list(maxit = 5000))
            for (i in names(object@train_args)) {
              args.tsmethod[[i]] <- object@train_args[[i]]
            }

            if (object@outlier_type == "None") {
              trained_result <- do.call(forecast::Arima, c(list("y" = new_train_x, "xreg" = new_train_xreg), args.tsmethod))
              if (length(new_train_xreg) != 0) {
                trained_result$call$xreg <- new_train_xreg
              } else {
                trained_result$call$xreg <- NULL
              }
              trained_result$call$x <- new_train_x
            } else {
              if (object@outlier_type == "All") {
                ol_type <- c("AO", "IO", "TC")
              } else {
                ol_type <- object@outlier_type
              }

              trained_result <- tryCatch({
                tso_model <- tsoutliers::tso(y = new_train_x, xreg = new_train_xreg, types = ol_type, cval = cval, maxit = 2, tsmethod = "arima", args.tsmethod = args.tsmethod, maxit.oloop = 12, maxit.iloop = 6)
                if (object@outlier_prediction == "Categorical") {
                  param_mle <- do.call(rbind,  lapply(ol_type, function(i) {
                    target_ol_info <- tso_model$outlier[tso_model$outlier$type == i,]
                    param <- nrow(target_ol_info) / length(new_train_x)
                    total <- length(new_train_x)
                    if (object@outlier_prediction_update_param & length(trained_model) > 0) {
                      param <- stats::weighted.mean(
                        x = c(param, ifelse(is.null(trained_model[[1]]$param_mle),
                                            0,
                                            trained_model[[1]]$param_mle$param[which(i == ol_type)])),
                        w = c(length(new_train_x), ifelse(is.null(trained_model[[1]]$param_mle),
                                                          0,
                                                          trained_model[[1]]$param_mle$total[which(i == ol_type)])),
                        na.rm = TRUE
                      )
                      total <- length(new_train_x) + ifelse(is.null(trained_model[[1]]$param_mle),
                                                            0,
                                                            trained_model[[1]]$param_mle$total[which(i == c(ol_type, "NO"))])
                    }
                    effect_mean <- ifelse(length(target_ol_info$coefhat) == 0, 0, mean(target_ol_info$coefhat))
                    effect_var <- ifelse(length(target_ol_info$coefhat) == 0 | length(target_ol_info$coefhat) == 1, 0, stats::var(target_ol_info$coefhat))
                    data.frame("param" = param, "total" = total, "effect_mean" = effect_mean, "effect_var" = effect_var)
                  }))
                  NO_param <- 1 - sum(param_mle$param)
                  param_mle <- rbind(param_mle,
                                     c("param" = NO_param, "total" = length(new_train_x), "effect_mean" = 0, "effect_var" = 0))
                } else if (object@outlier_prediction == "Categorical-Dirichlet") {
                  param_mle <- do.call(rbind,  lapply(ol_type, function(i) {
                    target_ol_info <- tso_model$outlier[tso_model$outlier$type == i,]
                    count <- nrow(target_ol_info)

                    if (is.na(object@outlier_prediction_prior)) {
                      outlier_prediction_prior <- 2
                    } else {
                      outlier_prediction_prior <- object@outlier_prediction_prior[which(i == c(ol_type, "NO"))]
                    }
                    if (object@outlier_prediction_update_param & length(trained_model) > 0) {
                      outlier_prediction_prior <- ifelse(
                        is.null(trained_model[[1]]$param_mle),
                        outlier_prediction_prior,
                        trained_model[[1]]$param_mle$outlier_prediction_prior[which(i == c(ol_type, "NO"))] + count
                      )
                    }
                    effect_mean <- ifelse(length(target_ol_info$coefhat) == 0, 0, mean(target_ol_info$coefhat))
                    effect_var <- ifelse(length(target_ol_info$coefhat) == 0 | length(target_ol_info$coefhat) == 1, 0, stats::var(target_ol_info$coefhat))
                    data.frame("count" = count, "outlier_prediction_prior" = outlier_prediction_prior, "effect_mean" = effect_mean, "effect_var" = effect_var)
                  }))
                  NO_count <- length(new_train_x) - sum(param_mle$count)
                  if (is.na(object@outlier_prediction_prior)) {
                    NO_outlier_prediction_prior <- 58
                  } else {
                    NO_outlier_prediction_prior <- object@outlier_prediction_prior[length(c(ol_type, "NO"))]
                  }
                  if (object@outlier_prediction_update_param & length(trained_model) > 0) {
                    NO_outlier_prediction_prior <- ifelse(
                      is.null(trained_model[[1]]$param_mle),
                      NO_outlier_prediction_prior,
                      trained_model[[1]]$param_mle$outlier_prediction_prior[length(c(ol_type, "NO"))]
                    ) + NO_count
                  }
                  param_mle <- rbind(param_mle,
                                     c("count" = NO_count, "outlier_prediction_prior" = NO_outlier_prediction_prior, "effect_mean" = 0, "effect_var" = 0))
                } else {
                  param_mle <- NULL
                }

                c(tso_model$fit, list("param_mle" = param_mle))
              }, error = function(e) {
                ts_model <- do.call(forecast::Arima, c(list("y" = new_train_x, "xreg" = new_train_xreg), args.tsmethod))
                if (length(new_train_xreg) != 0) {
                  ts_model$call$xreg <- new_train_xreg
                } else {
                  ts_model$call$xreg <- NULL
                }
                ts_model$call$x <- new_train_x
                ts_model
              })
            }

            if (object@res_dist == "skew_norm") {
              res <- trained_result$residuals
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


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "arima_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (object@res_dist == "empirical") {
              bootstrap <- TRUE
            } else {
              bootstrap <- FALSE
            }

            if (object@outlier_type == "All") {
              ol_type <- c("AO", "IO", "TC")
            } else {
              ol_type <- object@outlier_type
            }

            if (nrow(predict_info) == object@extrap_step) {
              if (is.null(trained_result$call$xreg)) {
                target_model <- forecast::Arima(y = trained_result$call$x, model = trained_result)
              } else {
                target_model <- forecast::Arima(y = trained_result$call$x, xreg = trained_result$call$xreg, model = trained_result)
              }
            } else {
              prev_x <- trained_result$call$x
              new_x <- predict_info$actual[-((nrow(predict_info) - object@extrap_step + 1):nrow(predict_info))]
              new_x <- c(prev_x, new_x)

              res <- stats::ts(c(trained_result$residuals, predict_info$residuals[-((nrow(predict_info) - object@extrap_step + 1):nrow(predict_info))]))

              if (is.na(object@outlier_cval)) {
                cval <- ifelse(length(res) <= 50, 3, ifelse(length(res) >= 450, 4, 3 + 0.0025 * (length(res) - 50)))
              } else {
                cval <- object@outlier_cval
              }

              if (object@outlier_type != "None") {
                # Find outliers from past residuals and remove their effect.
                pars <- tsoutliers::coefs2poly(trained_result)
                ol <- tsoutliers::locate.outliers.iloop(resid = res, pars = pars, cval = cval, types = ol_type, maxit = 20)

                if (nrow(ol) > 0) {
                  non_ol <- tsoutliers::find.consecutive.outliers(ol, object@outlier_type)
                  non_ol <- c(non_ol, which(ol$ind <= length(trained_result$residuals)))
                  non_ol <- unique(non_ol)
                  if (length(non_ol) > 0) {
                    ol <- ol[-non_ol,]
                  }
                  if (nrow(ol) > 0) {
                    new_x[ol$ind - length(trained_result$residuals)] <- new_x[ol$ind - length(trained_result$residuals)] - ol$coefhat
                  }
                }
              }

              prev_xreg <- trained_result$call$xreg
              if (is.null(prev_xreg)) {
                # Outliers are not considered or outliers are not found, and no external regressor is considered.
                target_model <- forecast::Arima(new_x, model = trained_result)
              } else {
                # Outliers are considered and found, or external regressor is considered.
                if (length(test_xreg) == 0) {
                  # No external regressor is considered.
                  new_xreg <- matrix(nrow = length(new_x) - length(prev_x), ncol = 0)
                } else {
                  # External regressor is considered.
                  new_xreg <- as.matrix(convert_frequency_dataset(test_xreg[-c((nrow(test_xreg) - object@window_size * object@extrap_step + 1):nrow(test_xreg)), 1], object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)]))
                }
                if (ncol(new_xreg) < ncol(prev_xreg)) {
                  # Outliers are considered and found.
                  new_ol <- matrix(0, nrow = nrow(new_xreg), ncol = ncol(prev_xreg) - ncol(new_xreg))
                  new_xreg <- cbind(new_xreg, new_ol)
                }
                colnames(new_xreg) <- colnames(prev_xreg)
                new_xreg <- rbind(prev_xreg, new_xreg)
                target_model <- forecast::Arima(new_x, xreg = new_xreg, model = trained_result)
              }
            }

            if (is.null(trained_result$call$xreg)) {
              # Outliers are not considered or outliers are not found, and no external regressor is considered.
              predict_result <- forecast::forecast(target_model, h = object@extrap_step, bootstrap = bootstrap, npaths = length(trained_result$call$x), level = level)
            } else {
              if (length(test_xreg) == 0) {
                # No external regressor is considered.
                dxreg <- matrix(0, nrow = object@extrap_step, ncol = ncol(trained_result$call$xreg))
              } else {
                # External regressor is considered.
                dxreg <- as.matrix(convert_frequency_dataset(test_xreg[(nrow(test_xreg) - object@window_size * object@extrap_step + 1):nrow(test_xreg)], object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)]))
                if (ncol(dxreg) < ncol(trained_result$call$xreg)) {
                  dxreg <- cbind(dxreg, matrix(0, nrow = object@extrap_step, ncol = (ncol(trained_result$call$xreg) - ncol(test_xreg))))
                }
              }
              colnames(dxreg) <- colnames(trained_result$call$xreg)
              predict_result <- forecast::forecast(target_model, xreg = dxreg, h = object@extrap_step, bootstrap = bootstrap, npaths = length(trained_result$call$x), level = level)
            }

            expected <- as.numeric(predict_result$mean)
            pi_up <- as.numeric(predict_result$upper)

            if (object@res_dist == "skew_norm") {
              xi <- trained_result$xi + as.numeric(predict_result$mean)
              omega <- trained_result$omega
              alpha <- trained_result$alpha

              expected <- xi + sqrt(2 / pi) * omega * (alpha / sqrt(1 + alpha ^ 2))
              pi_up <- max(sn::qsn(level / 100, xi = xi, omega = omega, alpha = alpha))

              predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "pi_up"] <- pi_up
              predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "expected"] <- expected
            } else {
              if (object@res_dist == "normal" & object@outlier_type != "None" & object@outlier_prediction != "None" & !is.null(trained_result$param_mle)) {
                ol_occurence <- list()
                if (object@outlier_prediction == "Categorical") {
                  ## Probability of occurences
                  for (i in 1:(nrow(trained_result$param_mle) - 1)) {
                    ol_occurence[[ol_type[i]]] <- trained_result$param_mle[i, "param"]
                  }
                  ol_occurence[["NO"]] <- trained_result$param_mle[nrow(trained_result$param_mle), "param"]
                  ol_occurence <- data.frame(ol_occurence)
                  ol_occurence <- do.call(rbind, replicate(object@extrap_step, ol_occurence, simplify = FALSE))
                } else if (object@outlier_prediction == "Categorical-Dirichlet") {
                  ## Probability of occurences
                  if (object@extrap_step > 1) {
                    stop("Multiple extrapolation step for Categorical-Dirichlet is not implemented.")
                  }
                  for (i in 1:(nrow(trained_result$param_mle) - 1)) {
                    ol_occurence[[ol_type[i]]] <- (trained_result$param_mle$count[i] + trained_result$param_mle$outlier_prediction_prior[i]) / sum(trained_result$param_mle$count, trained_result$param_mle$outlier_prediction_prior)
                  }
                  ol_occurence[["NO"]] <- (trained_result$param_mle$count[nrow(trained_result$param_mle)] + trained_result$param_mle$outlier_prediction_prior[nrow(trained_result$param_mle)]) / sum(trained_result$param_mle$count, trained_result$param_mle$outlier_prediction_prior)
                  ol_occurence <- data.frame(ol_occurence)
                }

                predict_var <- ((pi_up - expected) / stats::qnorm(level / 100))^2
                pi_up <- sapply(1:object@extrap_step, function(h) {
                  suppressWarnings(KScorrect::qmixnorm(level / 100,
                                                       mean = expected[h] + trained_result$param_mle$effect_mean,
                                                       sd = sqrt(predict_var[h] + trained_result$param_mle$effect_var),
                                                       pro = ol_occurence[h,]))
                })
                expected <- sapply(1:object@extrap_step, function(h) {
                  expected[h] + sum(trained_result$param_mle$effect_mean * ol_occurence[h,])
                })
              }
            }

            predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "pi_up"] <- pi_up
            predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "expected"] <- expected
            return(predict_info)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "arima_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["outlier_cval"]] <- methods::slot(object, "outlier_cval")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "arima_sim"),
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
          signature(object = "arima_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["train_args"]] <- methods::slot(object, "train_args")
            hidden_lst[["outlier_prediction_prior"]] <- methods::slot(object, "outlier_prediction_prior")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "arima_sim",
      function(from) {
        object <- methods::new("arima_sim")
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
