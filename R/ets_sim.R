#' @include sim_class.R generics.R model_helper.R
NULL


#' Validity Checker for ets_sim Object
#'
#' @param object A ets_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_ets_sim <- function(object) {
  errors <- character()
  window_type_choices <- c("max", "avg")
  res_dist_choices <- c("empirical", "discretized", "normal")
  if (any(is.na(object@window_type_for_reg)) | all(object@window_type_for_reg != window_type_choices)) {
    msg <- paste0("window_type_for_reg must be one of ", paste(window_type_choices, collapse = " "))
    errors <- c(errors, msg)
  }
  if (length(object@res_dist) != 1 | is.na(object@res_dist) | all(object@res_dist != res_dist_choices)) {
    msg <- paste0("res_dist must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@model) != 1 | stringr::str_detect(object@model, "^[AMZ]{1}[ANMZ]{2}$")) {
    msg <- paste0("model must be one of form ^[AMZ]{1}[ANMZ]{2}$.")
    errors <- c(errors, msg)
  }
  if (length(object@state_num) != 1 | (!is.na(object@state_num) & (object@state_num < 0 | object@state_num %% 1 != 0)) | (is.na(object@state_num) & object@granularity == 0)) {
    msg <- paste0("state_num must be NA or positive integer, if granularity is 0, then state_num cannot be NA.")
    errors <- c(errors, msg)
  }
  if (length(object@freq) != 1 (substring(object@model, 3, 3) != "N" & is.na(object@freq) & (object@freq < 0 | object@freq %% 1 != 0))) {
    msg <- paste0("freq must be positive integer if 3rd character is not N.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param model A character matching \code{"^[AMZ]{1}[ANMZ]{2}$"}.
#' @param freq A numeric value representing the number of observations per unit of time.
#' @param state_num A numeric number that represents the number of states in residual discretization.
#' @param res_dist A character representing the distribution of residual, \code{"empirical"} for empirical distribution, or \code{"discretized"} for discretized over \code{"state_num"} number of states equally partitioned from 0 to 100. Default value is \code{"empirical"}.
#' @param train_args A list representing additional call passed into the training function, \code{forecast::nnetar}. Default value is \code{list("repeats" = 50)}.
#' @export ets_sim
ets_sim <- setClass("ets_sim",
                           slots = list(model = "character",
                                        state_num = "numeric",
                                        freq = "numeric",
                                        res_dist = "character",
                                        train_args = "list",
                                        pred_args = "list"),
                           contains = "sim",
                           prototype = list(name = "ETS",
                                            model = "ZZZ",
                                            state_num = NA_real_,
                                            freq = NA_real_,
                                            res_dist = "normal",
                                            train_args = list(),
                                            pred_args = list()),
                           validity = check_valid_ets_sim)


#' @describeIn train_model Train Structual Model specific to ets_sim object.
setMethod("train_model",
          signature(object = "ets_sim", train_x = "matrix", train_xreg = "NULL", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            if (!is.na(object@freq)) {
              new_train_x <- stats::ts(convert_frequency_dataset(train_x, object@window_size, object@response, keep.names = TRUE), frequency = object@freq)
            } else {
              new_train_x <- stats::ts(convert_frequency_dataset(train_x, object@window_size, object@response, keep.names = TRUE))
            }

            args.tsmethod <- list()
            for (i in object@train_args) {
              args.tsmethod[[i]] <- object@train_args[[i]]
            }
            args.tsmethod <- c(args.tsmethod, list("y" = new_train_x, "model" = object@model, "additive.only" = TRUE))
            trained_result <- do.call(forecast::ets, args.tsmethod)

            trained_result$call$x <- new_train_x
            trained_result$call$orig_x <- train_x

            return(list(trained_result))
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "ets_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "NULL"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            trained_result <- trained_result[[1]]
            level <- (1 - object@cut_off_prob * 2) * 100

            if (nrow(predict_info) == 0) {
              target_model <- trained_result
            } else {
              prev_x <- trained_result$call$x
              new_x <- predict_info$actual
              new_x <- stats::ts(c(prev_x, new_x), start = stats::start(prev_x), frequency = stats::frequency(prev_x))

              target_model <- forecast::ets(new_x, model = trained_result, use.initial.values = TRUE)
            }

            args.tsmethod <- list()
            for (i in object@pred_args) {
              args.tsmethod[[i]] <- object@pred_args[[i]]
            }
            args.tsmethod <- c(args.tsmethod, list("object" = trained_result, "h" = object@extrap_step, "level" = level, "bootstrap" = ifelse(object@res_dist == "empirical", TRUE, FALSE)))

            predict_result <- do.call(forecast::forecast, args.tsmethod)

            expected <- stats::setNames(as.data.frame(as.numeric(predict_result$mean)), "expected")
            pi_up <- stats::setNames(as.data.frame(predict_result$upper), paste0("Quantile_", 0.5 + as.numeric(sub("%", "", colnames(as.data.frame(predict_result$upper)))) / 100 / 2))
            predicted_params <- data.frame("mean" = as.numeric(predict_result$mean), "sd." = (pi_up[,1] - expected[,1]) / stats::qnorm(sort(1 - object@cut_off_prob)[1]))

            if (object@res_dist == "discretized") {
              compute_pi_up <- function(prob, to_states) {
                current_state <- 1
                current_prob <- 0
                while (current_state <= length(to_states)) {
                  current_prob <- current_prob + to_states[current_state]
                  if (current_prob < prob) {
                    current_state <- current_state + 1
                  } else {
                    break
                  }
                }
                pi_up <- current_state * (100 / length(to_states))
                return(pi_up)
              }
              predicted_params <- discretized_from_normal_param_prediction(object, expected, pi_up)

              pi_up <- matrix(0, nrow = object@extrap_step, ncol = length(1 - object@cut_off_prob))
              for (j in 1:nrow(pi_up)) {
                pi_up[j,] <- sapply(sort(1 - object@cut_off_prob), function(i) {
                  compute_pi_up(i, predicted_params[j,])
                })
              }
              pi_up <- as.data.frame(pi_up)
              colnames(pi_up) <- paste0("Quantile_", sort(1 - object@cut_off_prob))

              expected <- data.frame("expected" = sapply(1:object@extrap_step, function(i) {
                find_expectation_state_based_dist(predicted_params[i, grep("prob_dist.", colnames(predicted_params))])
              }))
            }
            return(list("predicted_quantiles" = cbind(expected, pi_up), "predicted_params" = predicted_params))
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "ets_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["state_num"]] <- methods::slot(object, "state_num")
            numeric_lst[["freq"]] <- methods::slot(object, "freq")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "ets_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            character_lst[["model"]] <- methods::slot(object, "model")
            character_lst[["res_dist"]] <- methods::slot(object, "res_dist")
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "ets_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["train_args"]] <- methods::slot(object, "train_args")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "ets_sim",
      function(from) {
        object <- methods::new("ets_sim")
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
