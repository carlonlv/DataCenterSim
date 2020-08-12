#' @include sim_class.R generics.R
NULL


#' Validity Checker for auto_arima_sim Object
#'
#' @param object A auto_arima_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_auto_arima_sim <- function(object) {
  errors <- character()
  res_dist_choices <- c("normal", "empirical")
  outlier_type_choices <- c("AO", "IO", "LS", "None", "All")
  if (length(object@res_dist) != 1 | is.na(object@res_dist) |  all(object@res_dist != res_dist_choices)) {
    msg <- paste0("res_dist must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@outlier_type) != 1 | is.na(object@outlier_type) |  all(object@outlier_type != outlier_type_choices)) {
    msg <- paste0("outlier_type must be one of ", paste(outlier_type_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (any(object@outlier_cval < 3, na.rm = TRUE) | any(object@outlier_cval > 4, na.rm = TRUE)) {
    msg <- paste0("outlier_cval must only consist numeric values within 3 and 4, inclusively, or NA.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param res_dist The distribution of residual, \code{"normal"} for normal distribution or \code{"empirical"} for empirical distribution. Default value is \code{"normal"}.
#' @param outlier_type The type of outlier it will be treated, it can be None for not checking outliers, AO as additive outliers, IO as innovative outliers, LS as level shift, or All for taking account of all outlier types. Default value is \code{"None"}.
#' @param outlier_cval The critical value to determine the significance of each type of outlier. If NA_real_ is supplied, then it uses defaults: If n ≤ 50 then cval is set equal to 3.0; If n ≥ 450 then cval is set equal to 4.0; otherwise cval is set equal to 3 + 0.0025 * (n - 50).
#' @export auto_arima_sim
auto_arima_sim <- setClass("auto_arima_sim",
                      slots = list(res_dist = "character",
                                   outlier_type = "character",
                                   outlier_cval = "numeric",
                                   train_args = "list"),
                      contains = "sim",
                      prototype = list(name = "AUTO_ARIMA",
                                       res_dist = "normal",
                                       outlier_type = "None",
                                       outlier_cval = NA_real_,
                                       train_args = list("max.p" = 3, "max.q" = 2, "max.d" = 1, "max.P" = 1, "max.Q" = 1, "max.D" = 1)),
                      validity = check_valid_auto_arima_sim)


#' @describeIn train_model Train ARMA Model specific to auto_arima_sim object.
setMethod("train_model",
          signature(object = "auto_arima_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
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

            args.tsmethod <- c(object@train_args, list("method" = ifelse(object@res_dist == "norm", "CSS-ML", "CSS"), "optim.method" = "BFGS", "optim.control" = list(maxit = 5000)))
            if (object@outlier_type == "None") {
              trained_result <- do.call(forecast::auto.arima, c(list("y" = new_train_x, "xreg" = new_train_xreg), args.tsmethod))
              if (!(length(new_train_xreg) == 0)) {
                trained_result$call$xreg <- as.matrix(new_train_xreg)
              } else {
                trained_result$call$xreg <- NULL
              }
              trained_result$call$x <- new_train_x
            } else if (object@outlier_type == "All") {
              trained_result <- tryCatch({
                tso_model <- tsoutliers::tso(y = new_train_x, xreg = new_train_xreg, types = c("AO", "IO", "TC"), cval = cval, maxit = 2, tsmethod = "auto.arima", args.tsmethod = args.tsmethod, maxit.oloop = 12, maxit.iloop = 6)
                tso_model$fit
              }, error = function(e) {
                do.call(forecast::auto.arima, c(list("y" = new_train_x, "xreg" = new_train_xreg), args.tsmethod))
              })
            } else {
              trained_result <- tryCatch({
                tso_model <- tsoutliers::tso(y = new_train_x, xreg = new_train_xreg, types = object@outlier_type, cval = cval, maxit = 2, tsmethod = "auto.arima", args.tsmethod = args.tsmethod, maxit.oloop = 12, maxit.iloop = 6)
                tso_model$fit
              }, error = function(e) {
                do.call(forecast::auto.arima, c(list("y" = new_train_x, "xreg" = new_train_xreg), args.tsmethod))
              })
            }
            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "auto_arima_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (object@res_dist == "normal") {
              bootstrap <- FALSE
            } else {
              bootstrap <- TRUE
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
              names(new_x) <- predict_info$time[-((nrow(predict_info) - object@extrap_step + 1):nrow(predict_info))]
              new_x <- c(prev_x, new_x)

              res <- stats::ts(c(trained_result$residuals, predict_info$residuals[-((nrow(predict_info) - object@extrap_step + 1):nrow(predict_info))]))
              pars <- tsoutliers::coefs2poly(trained_result)

              if (is.na(object@outlier_cval)) {
                cval <- ifelse(length(res) <= 50, 3, ifelse(length(res) >= 450, 4, 3 + 0.0025 * (length(res) - 50)))
              } else {
                cval <- object@outlier_cval
              }

              if (object@outlier_type != "None") {
                # Find outliers from past residuals and remove their effect.
                if (object@outlier_type == "All") {
                  ol_type <- c("AO", "IO", "LS")
                } else {
                  ol_type <- object@outlier_type
                }

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
                  new_xreg <- matrix(nrow = nrow(prev_xreg), ncol = 0)
                } else {
                  # External regressor is considered.
                  new_xreg <- matrix(convert_frequency_dataset(test_xreg[-c((nrow(test_xreg) - object@window_size * object@extrap_step + 1):nrow(test_xreg))], object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)]), ncol = 1, byrow = TRUE)
                }
                if (ncol(new_xreg) < ncol(prev_xreg)) {
                  # Outliers are considered and found.
                  new_ol <- matrix(0, nrow = nrow(new_xreg), ncol = ncol(prev_xreg) - ncol(new_xreg))
                  new_xreg <- cbind(new_xreg, new_ol)
                }
                rownames(new_xreg) <- predict_info$time[-((nrow(predict_info) - object@extrap_step + 1):nrow(predict_info))]
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
                dxreg <- matrix(convert_frequency_dataset(test_xreg[(nrow(test_xreg) - object@window_size * object@extrap_step + 1):nrow(test_xreg)], object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)]), ncol = 1, byrow = TRUE)
                if (ncol(dxreg) < ncol(trained_result$call$xreg)) {
                  dxreg <- cbind(dxreg, matrix(0, nrow = object@extrap_step, ncol = (ncol(trained_result$call$xreg) - ncol(test_xreg))))
                }
              }
              colnames(dxreg) <- colnames(trained_result$call$xreg)
              predict_result <- forecast::forecast(target_model, xreg = dxreg, h = object@extrap_step, bootstrap = bootstrap, npaths = length(trained_result$call$x), level = level)
            }

            expected <- as.numeric(predict_result$mean)
            pi_up <- max(as.numeric(predict_result$upper))

            predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "pi_up"] <- pi_up
            predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "expected"] <- expected
            return(predict_info)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "auto_arima_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["outlier_cval"]] <- methods::slot(object, "outlier_cval")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "auto_arima_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            character_lst[["res_dist"]] <- methods::slot(object, "res_dist")
            character_lst[["outlier_type"]] <- methods::slot(object, "outlier_type")
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
