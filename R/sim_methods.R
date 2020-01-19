#' @include sim_class.R
NULL

#' @rdname type
#' @return A character representing type of simulation.
setMethod("type",
          signature(object = "sim"),
          function(object){
            return(object@type)
          })

#' @rdname type
setReplaceMethod("type",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@type <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname window_size
#' @return A numeric vector representing window size of simulation.
setMethod("window_size",
          signature(object = "sim"),
          function(object){
            return(object@window_size)
          })

#' @rdname window_size
setReplaceMethod("window_size",
          signature(object = "sim", value = "numeric"),
          function(object, value) {
            object@window_size <- value
            methods::validObject(object)
            return(object)
          })


#' @rdname cut_off_prob
#' @return A numeric vector representing cut off probability of simulation.
setMethod("cut_off_prob",
          signature(object = "sim"),
          function(object){
            return(object@cut_off_prob)
          })

#' @rdname cut_off_prob
setReplaceMethod("cut_off_prob",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@cut_off_prob <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname granularity
#' @return A numeric vector representing granularity of simulation.
setMethod("granularity",
          signature(object = "sim"),
          function(object){
            return(object@granularity)
          })

#' @rdname granularity
setReplaceMethod("granularity",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@granularity <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname train_size
#' @return A numeric vector representing training size of simulation.
setMethod("train_size",
          signature(object = "sim"),
          function(object){
            return(object@train_size)
          })

#' @rdname train_size
setReplaceMethod("train_size",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@train_size <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname update_freq
#' @return A numeric vector representing update frequency of simulation.
setMethod("update_freq",
          signature(object = "sim"),
          function(object){
            return(object@update_freq)
          })

#' @rdname update_freq
setReplaceMethod("update_freq",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@update_freq <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname tolerance
#' @return A numeric vector representing tolerance of simulation.
setMethod("tolerance",
          signature(object = "sim"),
          function(object){
            return(object@tolerance)
          })

#' @rdname tolerance
setReplaceMethod("tolerance",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@tolerance <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname response
#' @return A character representing response of simulation.
setMethod("response",
          signature(object = "sim"),
          function(object){
            return(object@response)
          })

#' @rdname response
setReplaceMethod("response",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@response <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname train_policy
#' @return A character representing training policy of simulation.
setMethod("train_policy",
          signature(object = "sim"),
          function(object){
            return(object@train_policy)
          })

#' @rdname train_policy
setReplaceMethod("train_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@train_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname schedule_policy
#' @return A character vector representing schedule policy of simulation.
setMethod("schedule_policy",
          signature(object = "sim"),
          function(object){
            return(object@schedule_policy)
          })

#' @rdname schedule_policy
setReplaceMethod("schedule_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@schedule_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname adjust_policy
#' @return A numeric vector representing adjust policy of simulation.
setMethod("adjust_policy",
          signature(object = "sim"),
          function(object){
            return(object@adjust_policy)
          })

#' @rdname adjust_policy
setReplaceMethod("adjust_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@adjust_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @rdname result_loc
#' @return A character vector representing result location of simulation.
setMethod("result_loc",
          signature(object = "sim"),
          function(object){
            return(object@result_loc)
          })

#' @rdname result_loc
setReplaceMethod("result_loc",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@result_loc <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @return A list containing all numeric parameter informations.
#' @rdname get_numeric_slots
setMethod("get_numeric_slots",
          signature(object = "sim"),
          function(object) {
            numeric_slots <- c("window_size", "cut_off_prob", "granularity", "train_size", "update_freq", "tolerance")
            numeric_lst <- list()
            for (i in numeric_slots) {
              numeric_lst[[i]] <- methods::slot(object, i)
            }
            return(numeric_lst)
          })


#' @return A list containing all the sim objects with uni-length slots
#' @rdname split_to_uni
setMethod("split_to_uni",
          signature(object = "sim"),
          function(object) {
            methods::validObject(object)
            result <- list()
            numeric_lst <- get_numeric_slots(sim)
            character_slots <- methods::slotNames(object)[-names(numeric_lst)]
            cmb <- expand.grid(numeric_lst)
            counter <- 1
            for (j in 1:nrow(cmb)) {
              info <- cmb[j,]
              uni <- methods::new(class(object))
              error <- FALSE
              for (k in names(numeric_lst)) {
                tryCatch({
                  methods::slot(uni, k, check = TRUE) <- info[k]
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


setMethod("plot_sim",
          signature(object = "sim"),
          function(object) {
            file_name <- paste(object@name, "Sim:", object@type, "Train:", object@training_policy, "Schedule:", object@schedule_policy, "Adjust:", object@adjust_policy)
            fp <- fs::path(paste0(object@result_loc, file_name), ext = "csv")
            if (object@type == "scheduling") {
              coln <- c(names(get_numeric_slots(object)), "avg_correct_scheduled_rate", "agg_correct_scheduled_rate", "avg_correct_unscheduled_rate", "agg_correct_unscheduled_rate")
              result <- utils::read.table(fp, quote = "", col.names = coln, row.names = NULL, sep = ",")
              plt <- ggplot2::ggplot(result, ggplot2::aes(x = result$agg_correct_scheduled_rate, y = result$agg_correct_unscheduled_rate)) +
                ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(size = result$window_size, shape = result$update_freq, alpha = result$granularity, colour = result$train_size, fill = result$tolerance))
            } else {
              coln <- c(names(get_numeric_slots(object)), "avg_survival_rate", "agg_survival_rate", "avg_utilization_rate", "agg_utilization_rate")
              result <- utils::read.table(fp, quote = "", col.names = coln, row.names = NULL, sep = ",")
              plt <- ggplot2::ggplot(result, ggplot2::aes(x = result$agg_survival_rate, y = result$agg_utilization_rate)) +
                ggplot2::geom_point(na.rm = TRUE, ggplot2::aes(size = result$window_size, shape = result$update_freq, alpha = result$granularity, colour = result$train_size, fill = result$tolerance))
            }
            plt <- plt +
              ggplot2::scale_shape_manual(values = 21:25) +
              ggplot2::scale_colour_manual(values = c("black", "grey", "white")) +
              ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21))) +
              ggplot2::geom_vline(xintercept = 0.99, linetype = "dashed", color = "red") +
              ggplot2::ylab("Utilization") +
              ggplot2::xlab("Survival Rate") +
              ggplot2::ggtitle(paste("Model Performance of", file_name))
            return(plt)
          })
