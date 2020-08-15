#' @include sim_class.R generics.R
NULL


#' Validity Checker for nn_sim Object
#'
#' @param object A nn_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_nn_sim <- function(object) {
  errors <- character()
  if (length(object@p) != 1 | any(object@p %% 1 != 0, na.rm = TRUE) | any(object@p <= 0, na.rm = TRUE)) {
    msg <- paste0("p must be a positive numeric integer or NA_real_.")
    errors <- c(errors, msg)
  }
  if (length(object@P) != 1 | is.na(object@P) | object@P %% 1 != 0 | object@P <= 0) {
    msg <- paste0("P must be a positive numeric integer.")
    errors <- c(errors, msg)
  }
  if (length(object@size) != 1 | any(object@size %% 1 != 0, na.rm = TRUE) | any(object@size <= 0, na.rm = TRUE)) {
    msg <- paste0("size must be a positive numeric integer or NA_real_.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param p A numeric integer value representing the number of lags for the input series of neural network. If \code{NA_real_} is supplied, optimal number of lags according to the AIC will be selected. It will be passed into \code{forecast::nnetar} as \code{p}. Default value is \code{1}.
#' @param P A numeric integer value representing the number of seasonal lags for the input series of neural network. It will be passed into \code{forecast::nnetar} as \code{P}. Default value is \code{0}.
#' @param size A numeric integer value representing the number of parameters in the hidden layer of the neural network. If \code{NA_real_} is supplied, half of the number of input nodes plus 1 will be used. Default value is \code{NA_real_}.
#' @param train_args A list representing additional call passed into the training function, \code{forecast::nnetar}. Default value is \code{list("repeats" = 50)}.
#' @export nn_sim
nn_sim <- setClass("nn_sim",
                           slots = list(p = "numeric",
                                        P = "numeric",
                                        size = "numeric",
                                        train_args = "list"),
                           contains = "sim",
                           prototype = list(name = "NN",
                                            p = NA_real_,
                                            P = 0,
                                            size = NA_real_,
                                            train_args = list("repeats" = 50)),
                           validity = check_valid_nn_sim)


#' @describeIn train_model Train NN Model specific to nn_sim object.
setMethod("train_model",
          signature(object = "nn_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- stats::ts(convert_frequency_dataset(train_x, object@window_size, object@response, keep.names = TRUE))

            if (length(train_xreg) == 0) {
              new_train_xreg <- NULL
            } else {
              new_train_xreg <- as.matrix(convert_frequency_dataset(train_xreg, object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)], keep.names = TRUE))
              colnames(new_train_xreg) <- "xreg"
            }

            if (is.na(object@p) & is.na(object@size)) {
              args.tsmethod <- list()
            } else if (is.na(object@p)) {
              args.tsmethod <- list("size" = object@size)
            } else if (is.na(object@size)) {
              args.tsmethod <- list("p" = object@p)
            } else {
              args.tsmethod <- list("p" = object@p, "size" = object@size)
            }
            args.tsmethod <- c(args.tsmethod, object@train_args, list("y" = new_train_x, "xreg" = new_train_xreg, "P" = object@P))
            trained_result <- do.call(forecast::nnetar, args.tsmethod)

            if (length(new_train_xreg) != 0) {
              trained_result$call$xreg <- new_train_xreg
            } else {
              trained_result$call$xreg <- NULL
            }
            trained_result$call$x <- new_train_x

            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "nn_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (nrow(predict_info) == object@extrap_step) {
              if (is.null(trained_result$call$xreg)) {
                target_model <- forecast::nnetar(y = trained_result$call$x, model = trained_result)
              } else {
                target_model <- forecast::nnetar(y = trained_result$call$x, xreg = trained_result$call$xreg, model = trained_result)
              }
            } else {
              prev_x <- trained_result$call$x
              new_x <- predict_info$actual[-((nrow(predict_info) - object@extrap_step + 1):nrow(predict_info))]
              new_x <- c(prev_x, new_x)

              prev_xreg <- trained_result$call$xreg
              if (is.null(prev_xreg)) {
                target_model <- forecast::nnetar(y = new_x, model = trained_result)
              } else {
                new_xreg <- as.matrix(convert_frequency_dataset(test_xreg[-c((nrow(test_xreg) - object@window_size * object@extrap_step + 1):nrow(test_xreg)),], object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)]))
                new_xreg <- rbind(prev_xreg, new_xreg)
                target_model <- forecast::nnetar(y = new_x, xreg = new_xreg, model = trained_result)
              }
            }

            if (length(test_xreg) == 0) {
              predict_result <- forecast::forecast(target_model, PI = TRUE, h = object@extrap_step, npaths = 2 * length(trained_result$call$x), level = level)
            } else {
              dxreg <- as.matrix(convert_frequency_dataset(test_xreg[(nrow(test_xreg) - object@window_size * object@extrap_step + 1):nrow(test_xreg), 1], object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)]))
              colnames(dxreg) <- colnames(trained_result$call$xreg)
              predict_result <- forecast::forecast(target_model, PI = TRUE, xreg = dxreg, h = object@extrap_step, npaths = 2 * length(trained_result$call$x), level = level)
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
          signature(object = "nn_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["p"]] <- methods::slot(object, "p")
            numeric_lst[["P"]] <- methods::slot(object, "P")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "nn_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "nn_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["train_args"]] <- methods::slot(object, "train_args")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "nn_sim",
      function(from) {
        object <- methods::new("nn_sim")
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
