#' @include sim_class.R generics.R
NULL

#' @name type
#' @rdname type
setMethod("type",
          signature(object = "sim"),
          function(object){
            return(object@type)
          })

#' @name type<-
#' @rdname type
setReplaceMethod("type",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@type <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @name window_size
#' @rdname window_size
setMethod("window_size",
          signature(object = "sim"),
          function(object){
            return(object@window_size)
          })

#' @name window_size<-
#' @rdname window_size
setReplaceMethod("window_size",
          signature(object = "sim", value = "numeric"),
          function(object, value) {
            object@window_size <- value
            methods::validObject(object)
            return(object)
          })


#' @name cut_off_prob
#' @rdname cut_off_prob
setMethod("cut_off_prob",
          signature(object = "sim"),
          function(object){
            return(object@cut_off_prob)
          })

#' @name cut_off_prob<-
#' @rdname cut_off_prob
setReplaceMethod("cut_off_prob",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@cut_off_prob <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @name granularity
#' @rdname granularity
setMethod("granularity",
          signature(object = "sim"),
          function(object){
            return(object@granularity)
          })

#' @name granularity<-
#' @rdname granularity
setReplaceMethod("granularity",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@granularity <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @name train_size
#' @rdname train_size
setMethod("train_size",
          signature(object = "sim"),
          function(object){
            return(object@train_size)
          })

#' @name train_size<-
#' @rdname train_size
setReplaceMethod("train_size",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@train_size <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @name update_freq
#' @rdname update_freq
setMethod("update_freq",
          signature(object = "sim"),
          function(object){
            return(object@update_freq)
          })

#' @name update_freq<-
#' @rdname update_freq
setReplaceMethod("update_freq",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@update_freq <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @name tolerance
#' @rdname tolerance
setMethod("tolerance",
          signature(object = "sim"),
          function(object){
            return(object@tolerance)
          })

#' @name tolerance<-
#' @rdname tolerance
setReplaceMethod("tolerance",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@tolerance <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @name response
#' @rdname response
setMethod("response",
          signature(object = "sim"),
          function(object){
            return(object@response)
          })

#' @name response<-
#' @rdname response
setReplaceMethod("response",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@response <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @name train_policy
#' @rdname train_policy
setMethod("train_policy",
          signature(object = "sim"),
          function(object){
            return(object@train_policy)
          })

#' @name train_policy<-
#' @rdname train_policy
setReplaceMethod("train_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@train_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @name schedule_policy
#' @rdname schedule_policy
setMethod("schedule_policy",
          signature(object = "sim"),
          function(object){
            return(object@schedule_policy)
          })

#' @name schedule_policy<-
#' @rdname schedule_policy
setReplaceMethod("schedule_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@schedule_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @name adjust_policy
#' @rdname adjust_policy
setMethod("adjust_policy",
          signature(object = "sim"),
          function(object){
            return(object@adjust_policy)
          })

#' @name adjust_policy<-
#' @rdname adjust_policy
setReplaceMethod("adjust_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@adjust_policy <- value
                   methods::validObject(object)
                   return(object)
                 })


#' @name result_loc
#' @rdname result_loc
setMethod("result_loc",
          signature(object = "sim"),
          function(object){
            return(object@result_loc)
          })

#' @name result_loc<-
#' @rdname result_loc
setReplaceMethod("result_loc",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@result_loc <- value
                   methods::validObject(object)
                   return(object)
                 })


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
                  break
                })
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
