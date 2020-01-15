#' @include sim_class.R generics.R
NULL


#' @rdname sim-class
ar1_sim <- setClass("ar1_sim",
                    contains = "sim")


ar1_sim_process <- setClass("ar1_sim_process",
                            slots = list(trained_model = "list", predict_result = "list"),
                            prototype = list(trained_model = list(), predict_result = list()),
                            contains = "ar1_sim")


ar1_sim_result <- setClass("ar1_sim_result",
                           slots = list(result = "data.frame", summ = "list"),
                           contains = "ar1_sim")


setMethod("train_model",
          signature(object = "ar1_sim", trainset_max = "numeric", trainset_avg = "numeric"),
          function(object, trainset_max, trainset_avg) {
            if (object@response == "max") {
              ts_model <- suppressWarnings(tryCatch({
                stats::arima(x = trainset_max, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "Nelder-Mead")
              }, warning = function(w) {
                stats::arima(x = trainset_max, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "BFGS")
              }, error = function(cond) {
                stats::arima(x = trainset_max, order = c(1,0,0), include.mean = TRUE, method = "CSS", optim.control = list(maxit = 2000), optim.method = "CG")
              }))
            } else {
              ts_model <- suppressWarnings(tryCatch({
                stats::arima(x = trainset_avg, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "Nelder-Mead")
              }, warning = function(w) {
                stats::arima(x = trainset_avg, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 2000), optim.method = "BFGS")
              }, error = function(cond) {
                stats::arima(x = trainset_avg, order = c(1,0,0), include.mean = TRUE, method = "CSS", optim.control = list(maxit = 2000), optim.method = "CG")
              }))
            }
            trained_result <- list("coeffs" = as.numeric(ts_model$coef[1]), "means" = as.numeric(ts_model$coef[2]), "vars" = ts_model$sigma2)
            return(methods::new("ar1_sim_process", object, trained_model = trained_result))
          })


setMethod("do_prediction",
          signature(object = "ar1_sim_process", last_obs_max = "numeric", last_obs_avg = "numeric", predict_size = "numeric", level = "numeric"),
          function(object, last_obs_max, last_obs_avg, predict_size, level) {
            calculate_var_cov_matrix <- function(var, l, phi) {
              dm = abs(outer(1:l,1:l,"-"))
              var_cov <- matrix(var[outer(1:l,1:l,"pmin")],l,l)*phi^dm
              return(var_cov)
            }
            phi <- object@trained_result$coeffs
            mean <- object@trained_result$means
            variance <- object@trained_result$vars

            # Construct mean
            if (object@response == "max") {
              mu <- rep(last_obs_max, predict_size)
            } else {
              mu <- rep(last_obs_avg, predict_size)
            }
            mu <- mu * phi^(1:predict_size) + (1 - phi^(1:predict_size)) * mean

            # Construct Var-cov matrix
            var <- cumsum((phi^2)^(0:(predict_size - 1)))*variance
            varcov <- calculate_var_cov_matrix(var, predict_size, phi)

            # caclulate probability
            prob <- NULL
            if (!is.null(level)) {
              prob <- 1 - mvtnorm::pmvnorm(lower = rep(0, predict_size), upper = rep(level, predict_size), mean = mu, sigma = varcov)
            }
            predict_result <- list("prob" = as.numeric(prob), "mu" = mu, "varcov" = varcov)
            object@predict_result <- predict_result
            return(object)
          })


setMethod("compute_pi_up",
          signature(object = "ar1_sim_process"),
          function(object) {
            mu <- object@predict_result$mu
            varcov <- object@predict_result$varcov
            upper_bounds <- rep(NA, length(mu))
            for (i in 1:length(mu)) {
              upper_bounds[i] <- min(mu[i] + stats::qnorm((1 - object@cut_off_prob)) * sqrt(varcov[i,i]), 100)
            }
            return(max(upper_bounds))
          })


setMethod("generate_result",
          signature(object = "ar1_sim", evaluation = "data.frame", write_result = "logical"),
          function(object, evaluation, write_result) {
            overall_result <- find_overall_evaluation(evaluation$sur_num, evaluation$sur_den, evaluation$util_num, evaluation$util_den)

            print(paste("Avg Survival Rate:", overall_result$avg_score1))
            print(paste("Agg Survival Rate:", overall_result$agg_score1))
            print(paste("Avg Utilization Rate:", overall_result$avg_score2))
            print(paste("Agg Utilization Rate:", overall_result$agg_score2))

            if (write_result) {
              file_name <- paste("AR1", "Sim:", object@type, "Train:", object@training_policy, "Schedule:", object@schedule_policy, "Adjust:", object@adjust_policy)
              fp <- fs::path(paste0(object@result_loc, file_name), ext = "csv")
              param <- c(object@window_size, object@cut_off_prob, object@granularity, object@train_size, object@update_freq, object@tolerance)
              new_row <- data.frame()
              new_row <- rbind(new_row, c(param, overall_result$avg_score1, overall_result$agg_score1, overall_result$avg_score2, overall_result$agg_score2))
              colnames(new_row) <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "tolerance", "avg_correct_scheduled_rate", "agg_correct_scheduled_rate", "avg_correct_unscheduled_rate", "agg_correct_unscheduled_rate")
              if (!fs::file_exists(fp)) {
                fs::file_create(fp)
                utils::write.table(new_row, file = fp, append = FALSE, quote = FALSE, col.names = TRUE, row.names = FALSE, sep = ",")
              } else {
                utils::write.table(new_row, file = fp, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = ",")
              }
            }
            return(methods::new("ar1_sim_result", object, result = evaluation, summ = overall_result))
          })
