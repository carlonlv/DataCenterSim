#' @include sim.R

setGeneric("window_size", function(object) standardGeneric("window_size"))
setGeneric("window_size<-", function(object, value) standardGeneric("window_size<-"))

setGeneric("cut_off_prob", function(object) standardGeneric("cut_off_prob"))
setGeneric("cut_off_prob<-", function(object, value) standardGeneric("cut_off_prob<-"))

setGeneric("granularity", function(object) standardGeneric("granularity"))
setGeneric("granularity<-", function(object, value) standardGeneric("granularity<-"))

setGeneric("train_size", function(object) standardGeneric("train_size"))
setGeneric("train_size<-", function(object, value) standardGeneric("train_size<-"))

setGeneric("update_freq", function(object) standardGeneric("update_freq"))
setGeneric("update_freq<-", function(object, value) standardGeneric("update_freq<-"))

setGeneric("train_policy", function(object) standardGeneric("train_policy"))
setGeneric("train_policy<-", function(object, value) standardGeneric("train_policy<-"))

setGeneric("schedule_policy", function(object) standardGeneric("schedule_policy"))
setGeneric("schedule_policy<-", function(object, value) standardGeneric("schedule_policy<-"))

setGeneric("adjust_policy", function(object) standardGeneric("adjust_policy"))
setGeneric("adjust_policy<-", function(object, value) standardGeneric("adjust_policy<-"))

setGeneric("result_loc", function(object) standardGeneric("result_loc"))
setGeneric("result_loc<-", function(object, value) standardGeneric("result_loc<-"))

setGeneric("mode", function(object) standardGeneric("object"))
setGeneric("mode<-", function(object, value) standardGeneric("mode<-"))

setGeneric("state_num", function(object) standardGeneric("state_num"))
setGeneric("state_num<-", function(object, value) standardGeneric("state_num<-"))

setGeneric("train_model", function(object) standardGeneric("train_model"))

setGeneric("do_prediction", function(object) standardGeneric("do_prediction"))

setGeneric("compute_pi_up", function(object) standardGeneric("compute_pi_up"))
