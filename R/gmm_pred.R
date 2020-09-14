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
          signature(object = "gmm_pred", train_x = "numeric", train_xreg = "data.frame", trained_model = "list"),
          function(object, train_x, train_xreg, trained_model) {
            training_data <- cbind(train_xreg[,which(colnames(train_xreg) != "job_ID")], "task_duration" = train_x)
            training_data$task_duration <- discretization(object@bins, training_data$task_duration)

            args.methods <- list("G" = 1:object@max_cluter)
            for (i in names(object@train_args)) {
              args.methods[[i]] <- object@train_args[[i]]
            }

            model <- do.call(mclust::Mclust, c(list("data" = training_data[, which(colnames(training_data) != "task_duration")]), args.methods))
            Get_Training_ProbVec <- function(model, training_data, breakpoints){
              bins <- length(breakpoints) - 1
              GMM_training_clusters <- model$classification
              probvec_GMM <- list()
              for (i in 1:length(sort(unique(GMM_training_clusters)))) {
                datai <- training_data$task_duration[GMM_training_clusters == sort(unique(GMM_training_clusters))[i]]
                hist1 <- hist(datai,breaks = breakpoints,plot = F)
                probvec_GMM[[i]] <- hist1$counts/sum(hist1$counts)
              }
              probvec_GMM
            }
            prob_vec <- Get_Training_ProbVec(model, training_data, object@bins)
            return(list("model" = model, "prob" = prob_vec))
          })


#' @describeIn do_prediction Do prediction based on trained GMM clustering Model.
setMethod("do_prediction",
          signature(object = "gmm_pred", trained_result = "list", predict_info = "data.frame", test_x = "numeric", test_xreg = "data.frame"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            model <- trained_result$model
            GMM_test <- predict(model, test_xreg[,which(colnames(test_xreg) != "job_ID")])
            GMM_test_clusters <- GMM_test$classification
            for (i in 1:length(sort(unique(trained_result$model$classification)))) {
              GMM_test_clusters[GMM_test$classification == sort(unique(trained_result$model$classification))[i]] <- i
            }
            predict_info[nrow(predict_info), "cluster_info"] <- GMM_test_clusters
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
