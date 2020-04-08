#' 100 Samples of Azure Public Dataset V1
#'
#' A dataframe conataining only maxes of traces from Microsoft Azure Data Center, with each column is one trace from one Virtual Machine.
#' The row name represents the time stamp of each obervation, the difference between observation is 300 seconds, the total length of each trace is almost 28 days.
#' The column name represents the hashed name of each virtual machine.
#' The value of each entry represents the maximum CPU value within the time granularity of 5 minutes.
#'
#' @format A data frame with 8000 rows and 100 columns.
#' @source \url{https://github.com/Azure/AzurePublicDataset/blob/master/AzurePublicDatasetV1.md}
"microsoft_max_100"

#' 100 Samples of Azure Public Dataset V1
#'
#' A dataframe conataining only avges of traces from Microsoft Azure Data Center, with each column is one trace from one Virtual Machine.
#' The row name represents the time stamp of each obervation, the difference between observation is 300 seconds, the total length of each trace is almost 28 days.
#' The column name represents the hashed name of each virtual machine.
#' The value of each entry represents the maximum CPU value within the time granularity of 5 minutes.
#'
#' @format A data frame with 8000 rows and 100 columns.
#' @source \url{https://github.com/Azure/AzurePublicDataset/blob/master/AzurePublicDatasetV1.md}
"microsoft_avg_100"

#' 78 Samples of Periodic Azure Public Dataset V1
#'
#' A dataframe conataining only maxes of traces from Microsoft Azure Data Center, with each column is one trace from one Virtual Machine.
#' The row name represents the time stamp of each obervation, the difference between observation is 300 seconds, the total length of each trace is almost 28 days.
#' The column name represents the hashed name of each virtual machine.
#' The value of each entry represents the maximum CPU value within the time granularity of 5 minutes.
#' The naming of each trace consists hashed trace name and periodicity in hours, separared by a white space.
#'
#' @format A data frame with 8400 rows and 78 columns.
#' @source \url{https://github.com/Azure/AzurePublicDataset/blob/master/AzurePublicDatasetV1.md}
"periodic_max_78"

#' 78 Samples of Periodic Azure Public Dataset V1
#'
#' A dataframe conataining only avges of traces from Microsoft Azure Data Center, with each column is one trace from one Virtual Machine.
#' The row name represents the time stamp of each obervation, the difference between observation is 300 seconds, the total length of each trace is almost 28 days.
#' The column name represents the hashed name of each virtual machine.
#' The value of each entry represents the maximum CPU value within the time granularity of 5 minutes.
#' The naming of each trace consists hashed trace name and periodicity in hours, separared by a white space.
#'
#' @format A data frame with 8400 rows and 78 columns.
#' @source \url{https://github.com/Azure/AzurePublicDataset/blob/master/AzurePublicDatasetV1.md}
"periodic_avg_78"

#' 128 Numbers of Parameter Settings
#'
#' A dataframe of parameter settings combining \code{window_size = c(12, 36)}, \code{cut_off_prob = c(0.005, 0.1)}, \code{granularity = c(100/32, 0)},
#' \code{train_size = c(150, 300)}, \code{model_num = c(1, 3)}, \code{update_freq = c(3, 6)}, \code{react_speed = list(c(1,1), c(1,3))}.
#' In order to pass into the \code{run_sim} argument, correct \code{name} column must be supplied.
#'
#' @format A dataframe with 128 rows and 10 columns:
#' Check \link{sim} for descriptions of the columns.
"param_setting1"
