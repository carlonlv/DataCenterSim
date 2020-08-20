#' @include sim_class.R generics.R
NULL


#' Validity Checker for var_sim Object
#'
#' @param object A var_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_var_sim <- function(object) {
  errors <- character()
  if (object@p %% 1 != 0 & object@p < 0) {
    msg <- paste0("p must be a non-negative integer.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param p A numeric value representing the autoregressive order for VAR model. Default value is \code{1}.
#' @export var_sim
var_sim <- setClass("var_sim",
                     slots = list(diff_order = "numeric", p = "numeric"),
                     contains = "sim",
                     prototype = list(name = "VAR",
                                      p = 1),
                     validity = check_valid_var_sim)


#' @describeIn train_model Train VAR Model specific to var_sim object.
setMethod("train_model",
          signature(object = "var_sim", train_x = "matrix", train_xreg = "matrix", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            new_train_x <- convert_frequency_dataset(train_x, object@window_size, object@response)
            new_train_xreg <- convert_frequency_dataset(train_xreg, object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)])

            uni_data_matrix <- matrix(nrow = length(new_train_x), ncol = 2)
            uni_data_matrix[,1] <- new_train_x
            uni_data_matrix[,2] <- new_train_xreg
            colnames(uni_data_matrix) <- c("max", "avg")
            rownames(uni_data_matrix) <- names(new_train_x)

            trained_result <- MTS::VAR(uni_data_matrix, p = object@p, include.mean = TRUE, output = FALSE)
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained VAR Model.
setMethod("do_prediction",
          signature(object = "var_sim", trained_result = "list", predict_info = "data.frame", test_x = "matrix", test_xreg = "matrix"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            level <- 1 - object@cut_off_prob * 2
            if (nrow(predict_info) == object@extrap_step) {
              trained_result <- trained_result
            } else {
              prev_data <- trained_result$data

              new_data <- matrix(nrow = nrow(predict_info) - object@extrap_step, ncol = 2)

              new_data[,1] <- convert_frequency_dataset(test_x, object@window_size, object@response)
              new_data[,2] <- convert_frequency_dataset(test_xreg[-c((nrow(test_xreg) - object@window_size * object@extrap_step + 1):nrow(test_xreg))], object@window_size, c("max", "avg")[-which(c("max", "avg") == object@response)])
              trained_result$data <- rbind(prev_data, new_data)
            }
            predict_result <- MTS::VARpred(trained_result, h = object@extrap_step, Out.level = FALSE, output = FALSE)

            if (object@extrap_step > 1) {
              expected <- as.numeric(predict_result$pred[,1])
            } else {
              expected <- as.numeric(predict_result$pred[1])
            }

            pi_up <- max(stats::qnorm(level, mean = expected, sd = predict_result$se.err[,1]))

            predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "pi_up"] <- pi_up
            predict_info[(nrow(predict_info) - object@extrap_step + 1):nrow(predict_info), "expected"] <- expected
            return(predict_info)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "var_sim"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "var_sim"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            character_lst[["p"]] <- methods::slot(object, "p")
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "var_sim"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "var_sim",
      function(from) {
        object <- methods::new("var_sim")
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
