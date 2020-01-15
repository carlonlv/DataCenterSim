#' @include sim_class.R generics.R
NULL


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
