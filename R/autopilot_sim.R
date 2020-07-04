#' @include sim_class.R generics.R
NULL


#' Validity Checker for autopilot_sim Object
#'
#' @param object A autopilot_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_autopilot_sim <- function(object) {
  errors <- character()
  statistics_choices <- c("peak", "weighted_avg", "j-quantile")
  if (length(object@statistics) != 1 | is.na(object@statistics) |  all(object@statistics != statistics_choices)) {
    msg <- paste0("statistics must be one of ", paste(statistics_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (length(object@n) != 1 | object@n <= 0 | object@n %% 1 != 0) {
    msg <- paste0("n must be a postive numeric integer or NA.")
    errors <- c(errors, msg)
  }
  if (length(object@half_life) != 1 | object@half_life <= 0 | object@half_life %% 1 != 0) {
    msg <- paste0("half_life must be a postive numeric integer or NA.")
    errors <- c(errors, msg)
  }
  if ((length(object@breaks) == 1 & object@breaks <= 0) | length(object@breaks) == 0) {
    msg <- paste0("breaks must be a numeric vector spanning the range of x or a postive value.")
    errors <- c(errors, msg)
  }
  if (length(object@cut_off_weight) != 1 | object@cut_off_weight > 1 | object@cut_off_weight < 0) {
    msg <- paste0("cut_off_weight must be a positive numeric value that is smaller than 1.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param statistics A character representing the type of statistics used to compute the recommendation at a specific time, can be either \code{"peak"}, \code{"weighted_avg"} or \code{"j-quantile"}. Default value is \code{"peak"}.
#' @param n A numeric integer representing the number of windows to find maximum over, used only when \code{"statistics"} is assigned to be \code{"peak"}. Default value is \code{288}.
#' @param half_life A numerc integer representing the number of windows for the weight to drop to half, used when \code{"statistics"} is assigned to be \code{"weighted_avg"} or \code{"j-quantile"}. Default value is \code{144}.
#' @param breaks A numeric integer or vector representing the number of breaks for each histogram in each window or the break points for \code{x}. Used when \code{"statistics"} is assigned to be \code{"weighted_avg"} or \code{"j-quantile"}, passed into \code{hist}. Default value is \code{10}.
#' @param cut_off_weight A numeric value that is close to zero, representing the smallest weight possible, lower which the weight will be considered as zero. Used when \code{"statistics"} is assigned to be \code{"weighted_avg"} or \code{"j-quantile"}. Default value is \code{0.001}.
#' @export autopilot_sim
autopilot_sim <- setClass("autopilot_sim",
                           slots = list(statistics = "character",
                                        n = "numeric",
                                        half_life = "numeric",
                                        breaks = "numeric",
                                        cut_off_weight = "numeric"),
                           contains = "sim",
                           prototype = list(name = "AUTOPILOT",
                                            statistics = "peak",
                                            n = 288,
                                            half_life = 144,
                                            breaks = 10,
                                            cut_off_weight = 0.001),
                           validity = check_valid_autopilot_sim)


#' @describeIn train_model Train model for autopilot recommender.
setMethod("train_model",
          signature(object = "autopilot_sim", ts_num = "numeric", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, ts_num, train_x, train_xreg, trained_model) {
            if (length(object@breaks) == 1) {
              breaks <- seq(from = 0, to = 100, length.out = object@breaks + 1)
            } else {
              breaks <- object@breaks
            }

            max_len <- floor(log2(object@cut_off_weight) * (-object@half_life))
            if (length(trained_model) == 0) {
              trained_result <- lapply(seq(from = nrow(train_x), by = -object@window_size, length.out = min(nrow(train_x) %/% object@window_size, max_len)), function(s) {
                graphics::hist(train_x[(s - object@window_size + 1):s,], breaks = breaks, plot = FALSE)
              })
            } else {
              hist_x <- lapply(seq(from = nrow(train_x), by = -object@window_size, length.out = object@update_freq), function(s) {
                graphics::hist(train_x[(s - object@window_size + 1):s,], breaks = breaks, plot = FALSE)
              })

              forget_num <- length(trained_model) + length(hist_x) - max_len
              if (forget_num > 0) {
                trained_model <- trained_model[-c((length(trained_model) - forget_num + 1):length(trained_model))]
              }
              trained_result <- append(trained_model, hist_x, 0)
            }
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on selected past statistics.
setMethod("do_prediction",
          signature(object = "autopilot_sim", trained_result = "list", predict_info = "data.frame", ts_num = "numeric", test_x = "matrix", test_xreg = "data.frame"),
          function(object, trained_result, predict_info, ts_num, test_x, test_xreg) {
            if (length(object@breaks) == 1) {
              breaks <- seq(from = 0, to = 100, length.out = object@breaks + 1)
            } else {
              breaks <- object@breaks
            }

            ## Histogram Aggregation
            weight <- (1 / 2) ** (seq(from = 0, by = 1, length.out = length(trained_result)) / object@half_life)

            if (object@statistics == "peak") {
              pi_up <- max(sapply(trained_result[1:object@n], function(h) {
                max(h$breaks[-1][h$counts > 0])
              }))
            } else if (object@statistics == "weighted_avg") {
              pi_up <- stats::weighted.mean(sapply(trained_result, function(h) {
                stats::weighted.mean(h$breaks[-1], h$counts)
              }), weight)
            } else {
              agg_count <- sapply(1:(length(breaks) - 1), function(b_index) {
                sum(weight * sapply(trained_result, function(h) {
                  h$counts[b_index]
                }))
              })
              agg_freq <- agg_count / sum(agg_count)

              compute_pi_up <- function(prob, agg_freq, breaks) {
                current_state <- 1
                current_prob <- 0
                while (current_state <= length(agg_freq)) {
                  current_prob <- current_prob + agg_freq[current_state]
                  if (current_prob < prob) {
                    current_state <- current_state + 1
                  } else {
                    break
                  }
                }
                pi_up <- breaks[current_state]
                return(pi_up)
              }
              pi_up <- compute_pi_up(1 - object@cut_off_prob, agg_freq, breaks[-1])
            }

            predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "pi_up"] <- pi_up
            predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "expected"] <- NA
            return(predict_info)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "autopilot_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["n"]] <- methods::slot(object, "n")
            numeric_lst[["half_life"]] <- methods::slot(object, "half_life")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "autopilot_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            character_lst[["statistics"]] <- methods::slot(object, "statistics")
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "autopilot_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["breaks"]] <- methods::slot(object, "breaks")
            hidden_lst[["cut_off_weight"]] <- methods::slot(object, "cut_off_weight")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "autopilot_sim",
      function(from) {
        object <- methods::new("autopilot_sim")
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