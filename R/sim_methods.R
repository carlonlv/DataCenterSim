#' @include sim_class.R
NULL


#' @return A list containing all numeric parameter informations.
#' @rdname get_param_slots
#' @export
setMethod("get_param_slots",
          signature(object = "sim"),
          function(object) {
            numeric_slots <- c("granularity", "train_size", "update_freq", "react_speed", "schedule_setting")
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
          signature(object = "sim"),
          function(object) {
            character_slots <- c("name", "window_size", "extrap_step", "response", "target", "train_policy")
            character_lst <- list()
            for (i in character_slots) {
              character_lst[[i]] <- methods::slot(object, i)
            }
            return(character_lst)
          })


#' @return A list containing all hidden parameter informations.
#' @rdname get_hidden_slots
#' @export
setMethod("get_hidden_slots",
          signature(object = "sim"),
          function(object) {
            return(list("cut_off_prob" = methods::slot(object, "cut_off_prob")))
          })


#' @export
setAs("data.frame", "sim",
      function(from) {
        if (nrow(from) == 1) {
          return(list(methods::as(from, paste0(tolower(from[, "class"]), "_sim"))))
        } else {
          return(lapply(1:nrow(from), function(rownum) {methods::as(from[rownum,], "sim")[[1]]}))
        }
      })


#' @export
setAs("sim", "data.frame",
      function(from) {
        char_lst <- get_characteristic_slots(from)
        char_df <- stats::setNames(data.frame(matrix(ncol = length(char_lst), nrow = 1)), names(char_lst))
        for (i in names(char_lst)) {
          if (length(char_lst[[i]]) == 1) {
            char_df[1, i] <- char_lst[[i]]
          } else {
            char_df[1, i] <- paste(char_lst[[i]], collapse = ",")
          }
        }
        param_lst <- get_param_slots(from)
        param_df <- stats::setNames(data.frame(matrix(ncol = length(param_lst), nrow = 1)), names(param_lst))
        for (i in names(param_lst)) {
          if (length(param_lst[[i]]) == 1) {
            param_df[1, i] <- param_lst[[i]]
          } else {
            param_df[1, i] <- paste(param_lst[[i]], collapse = ",")
          }
        }
        return(cbind(char_df, param_df))
        })


#' @rdname get_representation
#' @export
setMethod("get_representation",
          signature(object = "sim", type = "character"),
          function(object, type) {
            df <- methods::as(object, "data.frame")

            char_col <- names(unlist(get_characteristic_slots(object)))
            param_col <- colnames(df)[-which(colnames(df) %in% char_col)]

            char_name <- df[, char_col]
            char_name <- stats::setNames(as.character(char_name), colnames(char_name))
            param_name <- df[, param_col]
            param_name <- stats::setNames(as.character(param_name), colnames(param_name))
            if (type == "char_raw") {
              return(char_name)
            } else if (type == "char_con") {
              return(paste(names(char_name), char_name, sep = "-", collapse = ","))
            } else if (type == "param_raw") {
              return(param_name)
            } else if (type == "param_con") {
              return(paste(names(param_name), param_name, sep = "-", collapse = ","))
            } else {
              stop("Type must be one of char_raw, char_con, param_raw, param_con.")
            }
          })

