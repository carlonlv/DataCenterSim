#' @include sim.R generics.R


setMethod("window_size",
          signature(object = "sim"),
          function(object){
            return(object@window_size)
          })


setReplaceMethod("window_size",
          signature(object = "sim", value = "numeric"),
          function(object, value) {
            object@window_size <- value
            validObject(object)
            return(object)
          })


setMethod("cut_off_prob",
          signature(object = "sim"),
          function(object){
            return(object@cut_off_prob)
          })


setReplaceMethod("cut_off_prob",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@cut_off_prob <- value
                   validObject(object)
                   return(object)
                 })

setMethod("granularity",
          signature(object = "sim"),
          function(object){
            return(object@granularity)
          })

setReplaceMethod("granularity",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@granularity <- value
                   validObject(object)
                   return(object)
                 })

setMethod("train_size",
          signature(object = "sim"),
          function(object){
            return(object@train_size)
          })


setReplaceMethod("train_size",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@train_size <- value
                   validObject(object)
                   return(object)
                 })


setMethod("update_freq",
          signature(object = "sim"),
          function(object){
            return(object@update_freq)
          })


setReplaceMethod("update_freq",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@update_freq <- value
                   validObject(object)
                   return(object)
                 })


setMethod("tolerance",
          signature(object = "sim"),
          function(object){
            return(object@tolerance)
          })


setReplaceMethod("tolerance",
                 signature(object = "sim", value = "numeric"),
                 function(object, value) {
                   object@tolerance <- value
                   validObject(object)
                   return(object)
                 })


setMethod("train_policy",
          signature(object = "sim"),
          function(object){
            return(object@train_policy)
          })

setReplaceMethod("train_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@train_policy <- value
                   validObject(object)
                   return(object)
                 })


setMethod("schedule_policy",
          signature(object = "sim"),
          function(object){
            return(object@schedule_policy)
          })


setReplaceMethod("schedule_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@schedule_policy <- value
                   validObject(object)
                   return(object)
                 })


setMethod("adjust_policy",
          signature(object = "sim"),
          function(object){
            return(object@adjust_policy)
          })


setReplaceMethod("adjust_policy",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@adjust_policy <- value
                   validObject(object)
                   return(object)
                 })

setMethod("result_loc",
          signature(object = "sim"),
          function(object){
            return(object@result_loc)
          })


setReplaceMethod("result_loc",
                 signature(object = "sim", value = "character"),
                 function(object, value) {
                   object@result_loc <- value
                   validObject(object)
                   return(object)
                 })
