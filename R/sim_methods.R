#' @include sim_class.R
NULL

#' @rdname type
#' @return A character representing type of simulation.
#' @export
setMethod("type",
          signature(object = "sim"),
          function(object){
            return(object@type)
          })

#' @rdname type
#' @export
setReplaceMethod("type",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@type <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname window_size
#' @return A numeric vector representing window size of simulation.
#' @export
setMethod("window_size",
          signature(object = "sim"),
          function(object){
            return(object@window_size)
          })

#' @rdname window_size
#' @export
setReplaceMethod("window_size",
          signature(object = "sim", value = "numeric"),
          function(object, value) {
            object@window_size <- value
            methods::validObject(object)
            return(object)
          })


#' @rdname cut_off_prob
#' @return A numeric vector representing cut off probability of simulation.
#' @export
setMethod("cut_off_prob",
          signature(object = "sim"),
          function(object){
            return(object@cut_off_prob)
          })

#' @rdname cut_off_prob
#' @export
setReplaceMethod("cut_off_prob",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@cut_off_prob <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname granularity
#' @return A numeric vector representing granularity of simulation.
#' @export
setMethod("granularity",
          signature(object = "sim"),
          function(object){
            return(object@granularity)
          })

#' @rdname granularity
#' @export
setReplaceMethod("granularity",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@granularity <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname train_size
#' @return A numeric vector representing training size of simulation.
#' @export
setMethod("train_size",
          signature(object = "sim"),
          function(object){
            return(object@train_size)
          })

#' @rdname train_size
#' @export
setReplaceMethod("train_size",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@train_size <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname update_freq
#' @return A numeric vector representing update frequency of simulation.
#' @export
setMethod("update_freq",
          signature(object = "sim"),
          function(object){
            return(object@update_freq)
          })

#' @rdname update_freq
#' @export
setReplaceMethod("update_freq",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@update_freq <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname tolerance
#' @return A numeric vector representing tolerance1 of score1.
#' @export
setMethod("tolerance1",
          signature(object = "sim"),
          function(object){
            return(object@tolerance1)
          })

#' @rdname tolerance
#' @export
setReplaceMethod("tolerance1",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@tolerance1 <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname tolerance
#' @return A numeric vector representing tolerance2 of score2.
#' @export
setMethod("tolerance2",
          signature(object = "sim"),
          function(object){
            return(object@tolerance2)
          })

#' @rdname tolerance
#' @export
setReplaceMethod("tolerance2",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@tolerance2 <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname response
#' @return A character representing response of simulation.
#' @export
setMethod("response",
          signature(object = "sim"),
          function(object){
            return(object@response)
          })

#' @rdname response
#' @export
setReplaceMethod("response",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@response <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname train_policy
#' @return A character representing training policy of simulation.
#' @export
setMethod("train_policy",
          signature(object = "sim"),
          function(object){
            return(object@train_policy)
          })

#' @rdname train_policy
#' @export
setReplaceMethod("train_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@train_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname schedule_policy
#' @return A character vector representing schedule policy of simulation.
#' @export
setMethod("schedule_policy",
          signature(object = "sim"),
          function(object){
            return(object@schedule_policy)
          })

#' @rdname schedule_policy
#' @export
setReplaceMethod("schedule_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@schedule_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname adjust_policy
#' @return A numeric vector representing adjust policy of simulation.
#' @export
setMethod("adjust_policy",
          signature(object = "sim"),
          function(object){
            return(object@adjust_policy)
          })

#' @rdname adjust_policy
#' @export
setReplaceMethod("adjust_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@adjust_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname result_loc
#' @return A character vector representing result location of simulation.
#' @export
setMethod("result_loc",
          signature(object = "sim"),
          function(object){
            return(object@result_loc)
          })

#' @rdname result_loc
#' @export
setReplaceMethod("result_loc",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@result_loc <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @return A list containing all the sim objects with uni-length slots
#' @rdname split_to_uni
#' @export
setMethod("split_to_uni",
          signature(object = "sim"),
          function(object) {
            methods::validObject(object)
            result <- list()
            numeric_lst <- get_numeric_slots(object)
            character_slots <- setdiff(methods::slotNames(object), names(numeric_lst))
            cmb <- expand.grid(numeric_lst)
            counter <- 1
            for (j in 1:nrow(cmb)) {
              info <- cmb[j,]
              uni <- methods::new(class(object))
              error <- FALSE
              for (k in names(numeric_lst)) {
                tryCatch({
                  methods::slot(uni, k, check = TRUE) <- as.numeric(info[k])
                }, error = function(cond) {
                  error <- TRUE
                })
                if (error) {
                  break
                }
              }
              if (!error) {
                for (l in character_slots) {
                  methods::slot(uni, l) <- slot(object, l)
                }
                result[[counter]] <- uni
                counter <- counter + 1
              }
            }
            return(result)
          })


#' @return A plot object
#' @rdname plot_sim
#' @export
setMethod("plot_sim",
          signature(object = "sim"),
          function(object) {
            file_name <- paste(object@name, "Sim:", object@type, "Train:", object@train_policy, "Schedule:", object@schedule_policy, "Adjust:", object@adjust_policy)
            fp <- fs::path(paste0(object@result_loc, file_name), ext = "csv")
            result <- utils::read.csv(fp)

            result$window_size_update_freq <- paste(result$window_size, result$update_freq)
            if (object@type == "scheduling") {
              if (object@train_policy == "dynamic") {
                result$tolerance <- paste(result$tolerance1, result$tolerance2)
                plt <- ggplot2::ggplot(result, ggplot2::aes(x = result$agg_correct_scheduled_rate, y = result$agg_correct_unscheduled_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(shape = result$window_size_update_freq, alpha = as.factor(result$granularity), fill = as.factor(result$train_size), color = as.factor(result$tolerance))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(result$cut_off_prob)), type = "norm") +
                  ggplot2::ylab("Correct Scheduled Rate") +
                  ggplot2::xlab("Correct Unscheduled Rate") +
                  ggplot2::scale_color_brewer(name = "tolerance", palette = "Set1", guide = ggplot2::guide_legend(ncol = 2))
              } else {
                plt <- ggplot2::ggplot(result, ggplot2::aes(x = result$agg_correct_scheduled_rate, y = result$agg_correct_unscheduled_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(shape = result$window_size_update_freq, alpha = as.factor(result$granularity), fill = as.factor(result$train_size))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(result$cut_off_prob)), type = "norm") +
                  ggplot2::ylab("Correct Scheduled Rate") +
                  ggplot2::xlab("Correct Unscheduled Rate")
              }
            } else {
              if (object@train_policy == "dynamic") {
                result$tolerance <- paste(result$tolerance1, result$tolerance2)
                plt <- ggplot2::ggplot(result, ggplot2::aes(x = result$agg_survival_rate, y = result$agg_utilization_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(shape = result$window_size_update_freq, alpha = as.factor(result$granularity), fill = as.factor(result$train_size), color = as.factor(result$tolerance))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(result$cut_off_prob)), type = "norm") +
                  ggplot2::geom_vline(xintercept = 0.99, linetype = "dashed", color = "red") +
                  ggplot2::ylab("Utilization") +
                  ggplot2::xlab("Survival Rate") +
                  ggplot2::scale_color_brewer(name = "tolerance", palette = "Set1", guide = ggplot2::guide_legend(ncol = 2))
              } else {
                plt <- ggplot2::ggplot(result, ggplot2::aes(x = result$agg_survival_rate, y = result$agg_utilization_rate)) +
                  ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(shape = result$window_size_update_freq, alpha = as.factor(result$granularity), fill = as.factor(result$train_size))) +
                  ggplot2::stat_ellipse(aes(linetype = as.factor(result$cut_off_prob)), type = "norm") +
                  ggplot2::geom_vline(xintercept = 0.99, linetype = "dashed", color = "red") +
                  ggplot2::ylab("Utilization") +
                  ggplot2::xlab("Survival Rate")
              }
            }
            plt <- plt +
              ggplot2::scale_linetype(name = "cut_off_prob", guide = ggplot2::guide_legend(ncol = 2)) +
              ggplot2::scale_shape_manual(name = "window_size by update_freq", values = 21:25, guide = ggplot2::guide_legend(ncol = 2)) +
              ggplot2::scale_alpha(name = "granularity", guide = ggplot2::guide_legend(ncol = 2)) +
              ggplot2::scale_fill_brewer(name = "train_size", palette = "Set3") +
              ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21), ncol = 2)) +
              ggplot2::ggtitle(paste("Model Performance of", file_name))
            ggplot2::ggsave(fs::path(paste0(object@result_loc, file_name), ext = "png"), width = 5, height = 5)
            return(plt)
          })
