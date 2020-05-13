#' @include pred_class.R generics.R
NULL


#' Validity Checker for surtree_pred Object
#'
#' @param object A surtree_pred object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_surtree_pred <- function(object) {
  errors <- character()

  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname pred-class
#' @param min_obs A numeric value representing the minimum number of observations in each leaf of the tree.
#' @param train_args A list representing additional call passed into the training function.
#' @export surtree_pred
surtree_pred <- setClass("surtree_pred",
                     slots = list(min_obs = "numeric",
                                  train_args = "list"),
                     contains = "pred",
                     prototype = list(name = "SURTREE",
                                      min_obs = 200,
                                      train_args = list()),
                     validity = check_valid_surtree_pred)


#' @describeIn train_model Train ARMA Model specific to surtree_pred object.
setMethod("train_model",
          signature(object = "surtree_pred", train_x = "numeric", train_xreg = "numeric"),
          function(object, train_x, train_xreg) {
            trained_result <- list()
            ## TODO: put trained model into the list
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained GMM clustering Model.
setMethod("do_prediction",
          signature(object = "surtree_pred", trained_result = "list", predict_info = "data.frame"),
          function(object, trained_result, predict_info) {
            trained_result <- trained_result[[1]]
            ## TODO: do prediction and store prediction upper bound and cluster number
            predict_info[nrow(predict_info), "pi_up"] <- NA
            predict_info[nrow(predict_info), "cluster_info"] <- NA
            return(predict_info)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "surtree_pred"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            numeric_lst[["min_obs"]] <- methods::slot(object, "max_cluter")
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "surtree_pred"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "surtree_pred"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["train_args"]] <- methods::slot(object, "train_args")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "surtree_pred",
      function(from) {
        object <- methods::new("surtree_pred")
        for (i in names(from)) {
          if (i %in% methods::slotNames(object)) {
            methods::slot(object, i) <- from[, i]
          }
        }
        return(object)
      })
