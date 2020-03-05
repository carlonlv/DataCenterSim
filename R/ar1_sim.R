#' @include sim_class.R generics.R
NULL


#' Validity Checker for ar1_sim Object
#'
#' @param object A ar1_sim object
#' @return \code{TRUE} if the input sim object is valid, vector of error messages otherwise.
#' @keywords internal
check_valid_ar1_sim <- function(object) {
  errors <- character()
  res_dist_choices <- c("norm", "skew_norm")
  outlier_type_choices <- c("AO", "IO", "LS", "None", "All")
  if (length(object@res_dist) != 1 | is.na(object@res_dist) |  all(object@res_dist != res_dist_choices)) {
    msg <- paste0("train_policy must be one of ", paste(res_dist_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (object@reg_num != 1) {
    msg <- paste0("reg_num must be fixed to 1 for ar1 sim model.")
    errors <- c(errors, msg)
  }
  if (length(object@outlier_type) != 1 | is.na(object@outlier_type) |  all(object@outlier_type != outlier_type_choices)) {
    msg <- paste0("outlier_type must be one of ", paste(outlier_type_choices, collapse = " "), ".")
    errors <- c(errors, msg)
  }
  if (any(object@outlier_cval < 3, na.rm = TRUE) | any(object@outlier_cval > 4, na.rm = TRUE)) {
    msg <- paste0("outlier_cval must only consist numeric values within 3 and 4, inclusively, or NA.")
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}


#' @rdname sim-class
#' @param res_dist The distribution of residual.
#' @param reg_num The number of past regressive observations needed to forecast next observation.
#' @param outlier_type The type of outlier it will be treated, it can be None for not checking outliers, AO as additive outliers, IO as innovative outliers, LS as level shift, or All for taking account of all outlier types.
#' @param outlier_cval The critical value to determine the significance of each type of outlier. If NA_real_ is supplied, then it uses defaults: If n ≤ 50 then cval is set equal to 3.0; If n ≥ 450 then cval is set equal to 4.0; otherwise cval is set equal to 3 + 0.0025 * (n - 50).
#' @export ar1_sim
ar1_sim <- setClass("ar1_sim",
                    slots = list(res_dist = "character",
                                 reg_num = "numeric",
                                 outlier_type = "character",
                                 outlier_cval = "numeric"),
                    contains = "sim",
                    prototype = list(name = "AR1",
                                     res_dist = "norm",
                                     reg_num = 1,
                                     outlier_type = "AO",
                                     outlier_cval = NA_real_),
                    validity = check_valid_ar1_sim)


#' @describeIn train_model Train AR1 Model specific to ar1_sim object.
setMethod("train_model",
          signature(object = "ar1_sim", trainset_max = "numeric", trainset_avg = "numeric"),
          function(object, trainset_max, trainset_avg) {
            new_trainset_max <- stats::ts(convert_frequency_dataset(trainset_max, object@window_size, "max"))
            new_trainset_avg <- stats::ts(convert_frequency_dataset(trainset_avg, object@window_size, "avg"))

            if (object@response == "max") {
              if (is.na(object@outlier_cval)) {
                cval <- ifelse(length(new_trainset_max) <= 50, 3, ifelse(length(new_trainset_max) >= 450, 4, 3 + 0.0025 * (length(new_trainset_max) - 50)))
              } else {
                cval <- object@outlier_cval
              }

              ts_model <- suppressWarnings(tryCatch({
                if (object@outlier_type == "None") {
                  ts_model <- stats::arima(x = new_trainset_max, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 5000))
                } else if (object@outlier_type == "All") {
                  ts_model <- tsoutliers::tso(y = new_trainset_max, types = c("AO", "IO", "TC"), cval = cval, maxit = 2, tsmethod = "arima", args.tsmethod = list("order" = c(1,0,0), "include.mean" = TRUE, "method" = "CSS-ML", "optim.control" = list(maxit = 5000)), maxit.oloop = 20, maxit.iloop = 20)
                  ts_model <- ts_model$fit
                } else {
                  ts_model <- tsoutliers::tso(y = new_trainset_max, types = object@outlier_type, cval = cval, maxit = 2, tsmethod = "arima", args.tsmethod = list("order" = c(1,0,0), "include.mean" = TRUE, "method" = "CSS-ML", "optim.control" = list(maxit = 5000)), maxit.oloop = 20, maxit.iloop = 20)
                  ts_model <- ts_model$fit
                }
                phi <- as.numeric(ts_model$coef["ar1"])
                intercept <- as.numeric(ts_model$coef["intercept"]) * (1 - phi)
                list("intercept" = intercept, "phi" = phi, "residuals" = ts_model$residuals, "sigma2" = ts_model$sigma2)
              }, warning = function(w) {
                if (object@outlier_type == "None") {
                  ts_model <- stats::arima(x = new_trainset_max, order = c(1,0,0), include.mean = TRUE, method = "CSS", optim.control = list(maxit = 5000))
                } else if (object@outlier_type == "All") {
                  ts_model <- tsoutliers::tso(y = new_trainset_max, types = c("AO", "IO", "TC"), cval = cval, maxit = 1, tsmethod = "arima", args.tsmethod = list("order" = c(1,0,0), "include.mean" = TRUE, "method" = "CSS-ML", "optim.control" = list(maxit = 5000)), maxit.oloop = 10, maxit.iloop = 10)
                  ts_model <- ts_model$fit
                } else {
                  ts_model <- tsoutliers::tso(y = new_trainset_max, types = object@outlier_type, cval = cval, maxit = 1, tsmethod = "arima", args.tsmethod = list("order" = c(1,0,0), "include.mean" = TRUE, "method" = "CSS-ML", "optim.control" = list(maxit = 5000)), maxit.oloop = 10, maxit.iloop = 10)
                  ts_model <- ts_model$fit
                }
                phi <- as.numeric(ts_model$coef["ar1"])
                intercept <- as.numeric(ts_model$coef["intercept"]) * (1 - phi)
                list("intercept" = intercept, "phi" = phi, "residuals" = ts_model$residuals, "sigma2" = ts_model$sigma2)
              }, error = function(cond) {
                x_dot <- new_trainset_max - mean(new_trainset_max)
                if (object@outlier_type == "None") {
                  x_dot_trimmed <- x_dot
                } else {
                  x_dot_trimmed <- x_dot[stats::quantile(abs(x_dot), 0.125) < abs(x_dot) & abs(x_dot) < stats::quantile(abs(x_dot), 0.875)]
                }
                phi_num <- sample_moment_lag(x_dot_trimmed, k = 1, r = 1, s = 1)
                phi_den <- sample_moment_lag(x_dot_trimmed, k = 0, r = 1, s = 1)
                phi <- phi_num / phi_den
                if (object@outlier_type == "None") {
                  intercept <- mean(new_trainset_max) * (1 - phi)
                } else {
                  intercept <- mean(new_trainset_max, trim = 0.125) * (1 - phi)
                }
                fitted_x <- phi * x_dot[-length(x_dot)]
                res <- x_dot[-1] - fitted_x
                if (object@outlier_type == "None") {
                  res_trimmed <- res
                } else {
                  res_trimmed <- res[stats::quantile(abs(res), 0.125) < abs(res) & abs(res) < stats::quantile(abs(res), 0.875)]
                }
                sigma2 <- sample_moment_lag(res_trimmed, k = 0, r = 1, s = 1)
                list("intercept" = intercept, "phi" = phi, "residuals" = c(rep(0, object@reg_num), res), "sigma2" = sigma2)
              }))
            } else {
              if (is.na(object@outlier_cval)) {
                cval <- ifelse(length(new_trainset_avg) <= 50, 3, ifelse(length(new_trainset_avg) >= 450, 4, 3 + 0.0025 * (length(new_trainset_avg) - 50)))
              } else {
                cval <- object@outlier_cval
              }

              ts_model <- suppressWarnings(tryCatch({
                if (object@outlier_type == "None") {
                  ts_model <- stats::arima(x = new_trainset_avg, order = c(1,0,0), include.mean = TRUE, method = "CSS-ML", optim.control = list(maxit = 5000))
                } else if (object@outlier_type == "All") {
                  ts_model <- tsoutliers::tso(y = new_trainset_avg, types = c("AO", "IO", "TC"), cval = cval, maxit = 2, tsmethod = "arima", args.tsmethod = list("order" = c(1,0,0), "include.mean" = TRUE, "method" = "CSS-ML", "optim.control" = list(maxit = 5000)), maxit.oloop = 20, maxit.iloop = 20)
                  ts_model <- ts_model$fit
                } else {
                  ts_model <- tsoutliers::tso(y = new_trainset_avg, types = object@outlier_type, cval = cval, maxit = 2, tsmethod = "arima", args.tsmethod = list("order" = c(1,0,0), "include.mean" = TRUE, "method" = "CSS-ML", "optim.control" = list(maxit = 5000)), maxit.oloop = 20, maxit.iloop = 20)
                  ts_model <- ts_model$fit
                }
                phi <- as.numeric(ts_model$coef["ar1"])
                intercept <- as.numeric(ts_model$coef["intercept"]) * (1 - phi)
                list("intercept" = intercept, "phi" = phi, "residuals" = ts_model$residuals, "sigma2" = ts_model$sigma2)
              }, warning = function(w) {
                if (object@outlier_type == "None") {
                  ts_model <- stats::arima(x = new_trainset_avg, order = c(1,0,0), include.mean = TRUE, method = "CSS", optim.control = list(maxit = 5000))
                } else if (object@outlier_type == "All") {
                  ts_model <- tsoutliers::tso(y = new_trainset_avg, types = c("AO", "IO", "TC"), cval = cval, maxit = 1, tsmethod = "arima", args.tsmethod = list("order" = c(1,0,0), "include.mean" = TRUE, "method" = "CSS-ML", "optim.control" = list(maxit = 5000)), maxit.oloop = 10, maxit.iloop = 10)
                  ts_model <- ts_model$fit
                } else {
                  ts_model <- tsoutliers::tso(y = new_trainset_avg, types = object@outlier_type, cval = cval, maxit = 1, tsmethod = "arima", args.tsmethod = list("order" = c(1,0,0), "include.mean" = TRUE, "method" = "CSS-ML", "optim.control" = list(maxit = 5000)), maxit.oloop = 10, maxit.iloop = 10)
                  ts_model <- ts_model$fit
                }
                phi <- as.numeric(ts_model$coef["ar1"])
                intercept <- as.numeric(ts_model$coef["intercept"]) * (1 - phi)
                list("intercept" = intercept, "phi" = phi, "residuals" = ts_model$residuals, "sigma2" = ts_model$sigma2)
              }, error = function(cond) {
                x_dot <- new_trainset_avg - mean(new_trainset_avg)
                if (object@outlier_type == "None") {
                  x_dot_trimmed <- x_dot
                } else {
                  x_dot_trimmed <- x_dot[stats::quantile(abs(x_dot), 0.125) < abs(x_dot) & abs(x_dot) < stats::quantile(abs(x_dot), 0.875)]
                }
                phi_num <- sample_moment_lag(x_dot_trimmed, k = 1, r = 1, s = 1)
                phi_den <- sample_moment_lag(x_dot_trimmed, k = 0, r = 1, s = 1)
                phi <- phi_num / phi_den
                if (object@outlier_type == "None") {
                  intercept <- mean(new_trainset_avg) * (1 - phi)
                } else {
                  intercept <- mean(new_trainset_avg, trim = 0.125) * (1 - phi)
                }
                fitted_x <- phi * x_dot[-length(x_dot)]
                res <- x_dot[-1] - fitted_x
                if (object@outlier_type == "None") {
                  res_trimmed <- res
                } else {
                  res_trimmed <- res[stats::quantile(abs(res), 0.125) < abs(res) & abs(res) < stats::quantile(abs(res), 0.875)]
                }
                sigma2 <- sample_moment_lag(res_trimmed, k = 0, r = 1, s = 1)
                list("intercept" = intercept, "phi" = phi, "residuals" = c(rep(0, object@reg_num), res), "sigma2" = sigma2)
              }))
            }

            if (object@res_dist == "norm") {
              # phi parameter
              phi <- ts_model$phi

              # mu parameter
              mu <- ts_model$intercept

              # sigma parameter
              sigma <- sqrt(ts_model$sigma2)

              trained_result <- list("phi" = phi, "mu" = mu, "sigma" = sigma, "residuals" = ts_model$residuals, "intercept" = ts_model$intercept)
            } else {
              skew_res <- sample_moment_lag(ts_model$residuals, k = 0, r = 3, s = 0) / (sample_moment_lag(ts_model$residuals, k = 0, r = 2, s = 0) ^ (3/2))
              abs_skew_res <- min(abs(skew_res), 0.99)

              # phi parameter
              phi <- ts_model$phi

              # alpha parameter
              delta <- sign(skew_res) * sqrt((pi / 2) * (abs_skew_res^(2/3)) / ((abs_skew_res ^ (2/3)) + (2 - 0.5 * pi) ^ (2/3)))
              alpha <- delta / sqrt(1 - delta ^ 2)

              # omega parameter
              omega2 <- sample_moment_lag(ts_model$residuals, k = 0, r = 2, s = 0) / (1 - 2 / pi * delta ^ (2))
              omega <- sqrt(omega2)

              # xi parameter
              xi <- ts_model$intercept - sqrt(pi / 2) * omega * delta

              trained_result <- list("phi" = phi, "xi" = xi, "omega" = omega, "alpha" = alpha, "residuals" = ts_model$residuals, "intercept" = ts_model$intercept)
            }
            return(trained_result)
          })


#' @describeIn do_prediction Do prediction based on trained AR1 Model.
setMethod("do_prediction",
          signature(object = "ar1_sim", trained_result = "list", last_obs_max = "numeric", last_obs_avg = "numeric", last_res = "numeric", level = "numeric"),
          function(object, trained_result, last_obs_max, last_obs_avg, last_res, level) {
            if (object@response == "max") {
              last_obs <- last_obs_max
            } else {
              last_obs <- last_obs_avg
            }
            res <- stats::ts(c(trained_result$residuals, last_res))

            if (!is.na(last_res)) {
              pars <- list("arcoefs" = trained_result$phi, "macoefs" = numeric(0))
              if (is.na(object@outlier_cval)) {
                cval <- ifelse(length(res) <= 50, 3, ifelse(length(res) >= 450, 4, 3 + 0.0025 * (length(res) - 50)))
              } else {
                cval <- object@outlier_cval
              }
              ol <- tsoutliers::locate.outliers.iloop(resid = res, pars = pars, cval = cval, types = object@outlier_type, maxit = 20)
              if (nrow(ol) > 0) {
                cons_ol <- tsoutliers::find.consecutive.outliers(ol, object@outlier_type)
                if (length(cons_ol) > 0) {
                  ol <- ol[-cons_ol,]
                }
                if (length(res) %in% ol$ind) {
                  ol <- ol[which(ol$ind == length(res)),]
                  last_obs <- last_obs - ol$coefhat
                }
              }
            }

            if (object@res_dist == "norm") {
              mu <- trained_result$mu + trained_result$phi * last_obs
              sd <- trained_result$sigma
              prob <- NULL
              if (!is.na(level)) {
                prob <- 1 - stats::pnorm(q = level, mean = mu, sd = sd)
              }
              predicted_result <- list("prob" = as.numeric(prob), "mean" = mu, "sd" = sd, "expected" = min(max(mu, 0), 100))
            } else {
              xi <- trained_result$xi + trained_result$phi * last_obs
              omega <- trained_result$omega
              alpha <- trained_result$alpha
              prob <- NULL
              if (!is.na(level)) {
                prob <- 1 - sn::psn(x = level, xi = xi, omega = omega, alpha = alpha)
              }
              predicted_result <- list("prob" = as.numeric(prob), "xi" = xi, "omega" = omega, "alpha" = alpha, "expected" = min(max(xi, 0), 100))
            }
            return(predicted_result)
          })


#' @describeIn compute_pi_up Compute prediction interval Upper Bound based on trained AR1 Model.
setMethod("compute_pi_up",
          signature(object = "ar1_sim", predicted_result = "list"),
          function(object, predicted_result) {
            if (object@res_dist == "norm") {
              mu <- predicted_result$mean
              sd <- predicted_result$sd
              upper_bounds <- min(stats::qnorm(1 - object@cut_off_prob, mean = mu, sd = sd), 100)
            } else {
              xi <- predicted_result$xi
              omega <- predicted_result$omega
              alpha <- predicted_result$alpha
              upper_bounds <- min(sn::qsn(1 - object@cut_off_prob, xi = xi, omega = omega, alpha = alpha), 100)
            }
            return(upper_bounds)
          })


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "ar1_sim"),
          function(object) {
            numeric_slots <- c("cut_off_prob", "granularity", "train_size", "update_freq", "tolerance1", "tolerance2", "outlier_cval")
            numeric_lst <- list()
            for (i in numeric_slots) {
              numeric_lst[[i]] <- methods::slot(object, i)
            }
            return(numeric_lst)
          })


#' @return A list containing all character parameter informations.
#' @rdname get_characteristic_slots
#' @export
setMethod("get_characteristic_slots",
          signature(object = "ar1_sim"),
          function(object) {
            character_slots <- c("name", "type", "window_size", "train_policy", "schedule_policy", "adjust_policy", "response", "res_dist", "outlier_type")
            character_lst <- list()
            for (i in character_slots) {
              character_lst[[i]] <- methods::slot(object, i)
            }
            return(character_lst)
          })


#' @rdname plot_sim_tracewise
#' @export
setMethod("plot_sim_tracewise",
          signature(object = "ar1_sim", index = "numeric", trace_name = "character", trainset = "data.frame", testset = "data.frame", prev_score = "data.frame", last_score = "numeric", decision = "list"),
          function(object, index, trace_name, trainset, testset, prev_score, last_score, decision) {
            if (object@response == "max") {
              target_dataset <- c(utils::tail(trainset$trainset_max, 4 * nrow(testset)), testset$testset_max)
            } else {
              target_dataset <- c(utils::tail(trainset$trainset_avg, 4 * nrow(testset)), testset$testset_avg)
            }

            train_or_test <- c(rep("train", 4 * nrow(testset)), rep("test", nrow(testset)))
            t <- as.numeric(c(utils::tail(rownames(trainset), 4 * nrow(testset)), rownames(testset))) / 60

            train_decision <- decision$train_decision
            test_decision <- decision$test_decision

            train_sig <- train_decision$train_sig
            iter <- train_decision$iter
            trained_result <- train_decision$trained_result
            res <- trained_result$residuals

            pi_up <- c(rep(NA_real_, 4 * nrow(testset)), test_decision$pi_up)
            adjust_switch <- c(rep(NA_integer_, 4 * nrow(testset)), test_decision$adjust_switch)
            decision_opt <- c(rep(NA_integer_, 4 * nrow(testset)), test_decision$decision_opt)

            msg1 <- paste("Current batch has performance of", paste(last_score, collapse = " "))
            if (nrow(prev_score) == 0) {
              msg2 <- paste("No historical score to compare with on score1.")
              msg3 <- paste("No historical score to compare with on score2.")
            } else {
              msg2 <- paste("Current batch has performance not failing", sum(last_score[1] >= prev_score$prev_score1, na.rm = TRUE) / nrow(prev_score), "on score 1.")
              msg3 <- paste("Current batch has performance not failing", sum(last_score[2] >= prev_score$prev_score2, na.rm = TRUE) / nrow(prev_score), "on score 2.")
            }
            msg4 <- paste("Based on tolerance level of", object@tolerance1, object@tolerance2, "the training signal is", train_sig, "for next iteration.")
            wn_test <- stats::Box.test(res, lag = round(sqrt(length(res))), type = "Ljung-Box", fitdf = 2)
            msg5 <- paste("The White Noise Test for Residuals has p-value of", round(wn_test$p.value, 3), "for one-tailed test.")

            # Time Series Plot
            result <- data.frame("target_dataset" = target_dataset, "time" = t, "train_or_test" = train_or_test, "pi_up" = pi_up, "adjust_switch" = adjust_switch, "decision_opt" = decision_opt)
            ts_plt <- ggplot2::ggplot(result, aes(x = result$time, y = result$target_dataset)) +
              ggplot2::geom_line(aes(color = factor(result$train_or_test))) +
              ggplot2::geom_line(aes(y = result$pi_up, color = as.factor(result$adjust_switch)), na.rm = TRUE) +
              ggplot2::geom_line(aes(y = result$decision_opt), color = "darkcyan", na.rm = TRUE) +
              ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "purple") +
              ggplot2::xlab("Time (minutes)") +
              ggplot2::ylab("Cpu (percent)") +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ggtitle(paste("Diagnostic Plot of", trace_name, "at Iteration", iter)) +
              ggplot2::annotate("text", x = -Inf, y = Inf, vjust = c(2, 3.25, 4.5, 5.75, 7), hjust = 0, label = c(msg1, msg2, msg3, msg4, msg5))

            # Density Plot of Residuals
            residual <- data.frame("x" = as.numeric(res))
            dens_res <- ggplot2::ggplot(residual, aes(x = residual$x)) +
              ggplot2::geom_density(fill = "red", alpha = 0.5)
            if (object@res_dist == "norm") {
              mu <- trained_result$mu - trained_result$intercept
              sd <- trained_result$sigma
              dens_res <- dens_res +
                ggplot2::stat_function(fun = stats::dnorm, n = length(res), args = list("mean" = mu, "sd" = sd), color = "blue")
            } else {
              xi <- trained_result$xi - trained_result$intercept
              omega <- trained_result$omega
              alpha <- trained_result$alpha
              dens_res <- dens_res +
                ggplot2::stat_function(fun = sn::dsn, n = length(res), args = list("xi" = xi, "omega" = omega, "alpha" = alpha), color = "blue")
            }
            dens_res <- dens_res +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ylab("density of residuals") +
              ggplot2::xlab("residuals")

            # Time Series Plot of Residuals
            t <- seq(to = as.numeric(rownames(trainset)[nrow(trainset)]), from = as.numeric(rownames(trainset)[nrow(trainset)]) - (length(res) - 1) * 300, by = 300) / 60
            residual <- data.frame("x" = as.numeric(res), "time" = t)
            ts_res <- ggplot2::ggplot(residual, aes(x = residual$time, y = residual$x)) +
              ggplot2::geom_line(color = "green") +
              ggplot2::theme(legend.position = "none") +
              ggplot2::ylab("residuals") +
              ggplot2::xlab("time")

            plt <- gridExtra::arrangeGrob(ts_plt, dens_res, ts_res, ncol = 2, nrow = 2, layout_matrix = rbind(c(1,1), c(2,3)))

            file_name <- paste(unlist(get_characteristic_slots(object)), collapse = " ")
            fp <- fs::path(paste0(object@result_loc, "tracewise_plots/", file_name, " index ", index, " trace ", trace_name, " iter ", iter), ext = "png")
            ggplot2::ggsave(fp, plot = plt, width = 12, height = 7)
          })
