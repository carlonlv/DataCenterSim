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
#' @param min_obs A numeric value representing the minimum number of observations in each leaf of the tree.
#' @param train_args A list representing additional call passed into the training function.
#' @export anovatree_pred
anovatree_pred <- setClass("anovatree_pred",
                         slots = list(min_obs = "numeric",
                                      train_args = "list"),
                         contains = "pred",
                         prototype = list(name = "ANOVATREE",
                                          min_obs = 200,
                                          train_args = list()),
                         validity = check_valid_anovatree_pred)


#' @describeIn train_model Train ARMA Model specific to anovatree_pred object.
setMethod("train_model",
          signature(object = "anovatree_pred", ts_num = "ANY", train_x = "numeric", train_xreg = "data.frame"),
            function(object, ts_num, train_x, train_xreg) {
              training_data <- cbind(train_xreg, "task_duration" = train_x)
              training_data$task_duration <- discretization(object@bins,training_data$task_duration)
              trained_result <- list()
              ## TODO: put trained model into the list
              anova_tree <- function(training_data,minsize = 500){
                fit <- rpart::rpart(task_duration ~ scheduling_class + priority + requestCPU + requestRAM + requestLocal_disk_space, data = training_data, method = "anova",control = rpart::rpart.control(minbucket = minsize))
                tree_classify <- partykit::as.party(fit)
                tree_classify
              }
              model <- anova_tree(training_data)
              Get_Training_ProbVec <- function(model,training_data,breakpoints){
                probvec_Tree <- list()
                cluster1 <- as.numeric(predict(model, training_data[,c("scheduling_class", "priority", "requestCPU", "requestRAM", "requestLocal_disk_space")], type = "node"))
                for (i in 1:length(unique(cluster1))) {
                  datai <- training_data$task_duration[cluster1 == sort(unique(cluster1))[i]]
                  hist1 <- hist(datai,breaks = breakpoints,plot = F)
                  probvec_Tree[[i]] <- hist1$counts/sum(hist1$counts)
                }
                names(probvec_Tree) <- sort(unique(cluster1))
                probvec_Tree
              }
              prob_vec <- Get_Training_ProbVec(model,training_data,object@bins)
              trained_result$nodes <- as.numeric(names(prob_vec))
              names(prob_vec) <- 1:length(sort(unique(as.numeric(names(prob_vec)))))
              trained_result$model <- model
              trained_result$prob <- prob_vec
              return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained GMM clustering Model.
setMethod("do_prediction",
          signature(object = "anovatree_pred", trained_result = "list", predict_info = "data.frame", xreg = "data.frame"),
          function(object, trained_result, predict_info, xreg) {
            model <- trained_result$model
            nodes <- trained_result$nodes
            test_clusters <- predict(model, xreg[,c("scheduling_class", "priority", "requestCPU", "requestRAM", "requestLocal_disk_space")], type = "node")
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
            numeric_lst[["min_obs"]] <- methods::slot(object, "min_obs")
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
