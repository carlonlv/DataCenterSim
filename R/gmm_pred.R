#' @include pred_class.R generics.R
NULL


#' Validity Checker for gmm_pred Object
#'
#' @param object A gmm_pred object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_gmm_pred <- function(object) {
  errors <- character()
  if (object@max_cluter < 1 | object@max_cluster %% 1 != 0) {
    msg <- paste0("outlier_cval must be only a positive integer.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname pred-class
#' @param max_cluter A numeric value representing the maximum number of clusters to be selected by information criterion.
#' @param train_args A list representing additional call passed into the training function.
#' @export gmm_pred
gmm_pred <- setClass("gmm_pred",
                      slots = list(max_cluter = "numeric",
                                   train_args = "list"),
                      contains = "pred",
                      prototype = list(name = "GMM",
                                       max_cluter = 10,
                                       train_args = list()),
                      validity = check_valid_gmm_pred)


#' @describeIn train_model Train ARMA Model specific to gmm_pred object.
setMethod("train_model",
          signature(object = "gmm_pred", train_x = "numeric", train_xreg = "numeric"),
          function(object, train_x, train_xreg) {
            trained_result <- list()
            ## TODO: put trained model into the list
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained GMM clustering Model.
setMethod("do_prediction",
          signature(object = "gmm_pred", trained_result = "list", predict_info = "data.frame", xreg = "data.frame"),
          function(object, trained_result, predict_info, xreg) {
            trained_result <- trained_result[[1]]
            ## TODO: do prediction and store prediction upper bound and cluster number
            predict_info[nrow(predict_info), "cluster_info"] <- NA
            return(predict_info)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "gmm_pred"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["max_cluter"]] <- methods::slot(object, "max_cluter")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "gmm_pred"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "gmm_pred"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["train_args"]] <- methods::slot(object, "train_args")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "gmm_pred",
      function(from) {
        object <- methods::new("gmm_pred")
        for (i in names(from)) {
          if (i %in% methods::slotNames(object)) {
            methods::slot(object, i) <- from[, i]
          }
        }
        return(object)
      })
