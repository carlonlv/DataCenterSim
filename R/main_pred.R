de#' Predict Runtime of Jobs On Given Test Set
#'
#' Sequantially predict running time of jobs on provided test set.
#'
#' @param object A S4 pred object.
#' @param trained_result A trained object depending on the model used for training.
#' @param test_x A numeric vector representing the test set, containing maximum and average of actual observations.
#' @param test_xreg A dataframe representing the dataset that target dataset depends on for scheduling and evaluations.
#' @param predict_info A dataframe containing all the past predicted information.
#' @param switch_status A list containing all the information about current switches and identifiers.
#' @return A dataframe containing the past predicted information and the current predicted information.
#' @keywords internal
predict_pred <- function(object, trained_result, test_x, test_xreg, predict_info, switch_status) {
  test_predict_info <- data.frame("train_iter" = numeric(0),
                                  "test_iter" = numeric(0),
                                  "predict_iter" = numeric(0),
                                  "job_id" = numeric(0),
                                  "cluster_info" = numeric(0),
                                  "actual" = numeric(0),
                                  stringsAsFactors = FALSE)

  predict_iter <- 0
  current_end <- 1
  while (current_end <= length(test_x)) {
    test_predict_info[nrow(test_predict_info) + 1,] <- c(switch_status$train_iter, switch_status$test_iter, predict_iter + 1, test_xreg[current_end, "job_ID"], rep(NA, ncol(test_predict_info) - 4))

    predict_iter <- predict_iter + 1
    test_predict_info <- do_prediction(object, trained_result, test_predict_info, test_xreg[current_end,])

    actual_obs <- test_x[current_end]
    test_predict_info[nrow(test_predict_info), "actual"] <- discretization(object@bins, actual_obs)
    current_end <- current_end + 1
  }

  switch_status <- list("train_iter" = switch_status$train_iter, "test_iter" = switch_status$test_iter + 1)
  return(list("switch_status" = switch_status,  "predict_info" = test_predict_info))
}


#' Simulation of Scheduling Jobs Based On Predictions
#'
#' Sequantially training and testing by scheduling jobs based on predictions on a single trace using AR1 Model.
#'
#' @param object The corresponding trace/column in \code{dataset}.
#' @param x A numeric vector of length n representing the target dataset for predictions and evaluations.
#' @param xreg A matrix of length n by m representing the regressors.
#' @return A list containing the resulting prediction informations.
#' @keywords internal
predicting_pred <- function(object, x, xreg) {
  predict_info <- data.frame("train_iter" = numeric(0),
                             "test_iter" = numeric(0),
                             "predict_iter" = numeric(0),
                             "job_id" = numeric(0),
                             "cluster_info" = numeric(0),
                             "actual" = numeric(0),
                             stringsAsFactors = FALSE)

  current <- 1
  last_time_update <- length(x) - object@update_freq - object@train_size + 1

  trained_model <- list()

  train_sig <- TRUE

  train_iter <- 1
  while (current <= last_time_update) {
    if (train_sig) {
      train_start <- current
      train_end <- current + object@train_size - 1

      train_x <- x[train_start:train_end]
      train_xreg <- xreg[train_start:train_end,]

      trained_model <- c(trained_model, train_model(object, train_x, train_xreg))
      switch_status <- list("train_iter" = train_iter, "test_iter" = 0)
      train_iter <- train_iter + 1
    }

    ## Get test set
    test_start <- current + object@train_size
    test_end <- current + object@train_size + object@update_freq - 1
    test_x <- x[test_start:test_end]
    test_xreg <- xreg[test_start:test_end,]

    ## Test Model
    score_switch_info <- predict_pred(object, trained_model, test_x, test_xreg, predict_info, switch_status)
    switch_status <- score_switch_info[["switch_status"]]
    predict_info <- score_switch_info[["predict_info"]]

    ## Make scheduling decisions
    if (object@train_policy == "offline") {
      train_sig <- FALSE
    } else {
      train_sig <- TRUE
    }

    ## Update Step
    current <- current + object@update_freq
  }

  return(c(trained_model, predict_info))
}


#' Predictions of Clustering Jobs Based On Predictions.
#'
#' Sequantially training and testing by predicting the runtime of a job.
#'
#' @param epoch_setting A dataframe representing a specific parameter setting.
#' @param x A matrix of size n by m representing the target dataset for scheduling and evaluations.
#' @param xreg A matrix of length n by m representing the dataset that target dataset depends on for scheduling and evaluations.
#' @return A list of S4 pred result object.
#' @export
run_pred <- function(epoch_setting, x, xreg) {
  name_epoch_setting <- dplyr::group_by_at(epoch_setting, "name")
  score_all_lst <- dplyr::group_map(name_epoch_setting,
                                    function(other, name) {
                                      defau <- methods::new(paste0(tolower(as.character(name)), "_pred"))
                                      char_defau <- names(get_representation(defau, "char_raw"))
                                      char_epoch_setting <- dplyr::group_by_at(epoch_setting, c("name", colnames(other)[which(colnames(other) %in% char_defau)]))
                                      score_char_lst <- dplyr::group_map(char_epoch_setting,
                                                                         function(other, char) {
                                                                           param_uni_lst <- methods::as(cbind(char, other), "pred")
                                                                           score_param_lst <- lapply(param_uni_lst, predicting_pred, x, xreg)
                                                                           return(score_param_lst)
                                                                         })
                                      return(score_char_lst)
                                    })
  return(score_all_lst)
}
