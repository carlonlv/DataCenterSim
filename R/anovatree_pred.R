#' @include pred_class.R generics.R
NULL


#' Validity Checker for anovatree_pred Object
#'
#' @param object A anovatree_pred object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_anovatree_pred <- function(object) {
  errors <- character()
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname pred-class
#' @param train_args A list representing additional call passed into the training function.
#' @export anovatree_pred
anovatree_pred <- setClass("anovatree_pred",
                         slots = list(train_args = "list"),
                         contains = "pred",
                         prototype = list(name = "ANOVATREE",
                                          train_args = list("control" = rpart::rpart.control(minbucket = 200))),
                         validity = check_valid_anovatree_pred)


#' @describeIn train_model Train ARMA Model specific to anovatree_pred object.
setMethod("train_model",
          signature(object = "anovatree_pred", train_x = "numeric", train_xreg = "data.frame", trained_model = "list"),
            function(object, train_x, train_xreg, trained_model) {
              training_data <- cbind(train_xreg[,which(colnames(train_xreg) != "job_ID")], "task_duration" = train_x)
              training_data$task_duration <- discretization(object@bins,training_data$task_duration)
              trained_result <- list()

              args.methods <- list()
              for (i in names(object@train_args)) {
                args.methods[[i]] <- object@train_args[[i]]
              }

              form <- as.formula(paste("task_duration ~ ", paste(colnames(train_xreg)[-which(colnames(train_xreg) == "job_ID")], collapse = "+")))
              model <- do.call(rpart::rpart, c(list("formula" = form, "data" = training_data, "method" = "anova"), args.methods))
              model <- partykit::as.party(model)

              Get_Training_ProbVec <- function(model,training_data,breakpoints){
                probvec_Tree <- list()
                cluster1 <- as.numeric(predict(model, training_data, type = "node"))
                for (i in 1:length(unique(cluster1))) {
                  datai <- training_data$task_duration[cluster1 == sort(unique(cluster1))[i]]
                  hist1 <- hist(datai, breaks = breakpoints, plot = F)
                  probvec_Tree[[i]] <- hist1$counts/sum(hist1$counts)
                }
                names(probvec_Tree) <- sort(unique(cluster1))
                probvec_Tree
              }
              prob_vec <- Get_Training_ProbVec(model, training_data, object@bins)
              trained_result$nodes <- as.numeric(names(prob_vec))
              names(prob_vec) <- 1:length(sort(unique(as.numeric(names(prob_vec)))))
              trained_result$model <- model
              trained_result$prob <- prob_vec
              return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained ANOVA tree clustering Model.
setMethod("do_prediction",
          signature(object = "anovatree_pred", trained_result = "list", predict_info = "data.frame", test_x = "numeric", test_xreg = "data.frame"),
          function(object, trained_result, predict_info, test_x, test_xreg) {
            model <- trained_result$model
            nodes <- trained_result$nodes
            test_clusters <- predict(model, test_xreg[,which(colnames(test_xreg) != "job_ID")], type = "node")
            test_clusters2 <- which(nodes %in% test_clusters)
            predict_info[nrow(predict_info), "cluster_info"] <- test_clusters2
            return(predict_info)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "anovatree_pred"),
          function(object) {
            numeric_lst <- methods::callNextMethod(object)
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "anovatree_pred"),
          function(object) {
            character_lst <- methods::callNextMethod(object)
            return(character_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "anovatree_pred"),
          function(object) {
            hidden_lst <- methods::callNextMethod(object)
            hidden_lst[["train_args"]] <- methods::slot(object, "train_args")
            return(hidden_lst)
          })


#' @export
setAs("data.frame", "anovatree_pred",
      function(from) {
        object <- methods::new("anovatree_pred")
        for (i in names(from)) {
          if (i %in% methods::slotNames(object)) {
            methods::slot(object, i) <- from[, i]
          }
        }
        return(object)
      })
