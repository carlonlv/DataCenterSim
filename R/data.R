#' 100 Samples of Azure Public Dataset V1
#'
#' A dataframe conataining only maxes of traces from Microsoft Azure Data Center, with each column is one trace from one Virtual Machine.
#' The row name represents the time stamp of each obervation, the difference between observation is 300 seconds, the total length of each trace is almost 28 days.
#' The column name represents the hashed name of each virtual machine.
#' The value of each entry represents the maximum CPU value within the time granularity of 5 minutes.
#'
#' @format A data frame with 8000 rows and 100 columns:
#' @source \url{https://github.com/Azure/AzurePublicDataset/blob/master/AzurePublicDatasetV1.md}
"microsoft_max_100"

#' 100 Samples of Azure Public Dataset V1
#'
#' A dataframe conataining only avges of traces from Microsoft Azure Data Center, with each column is one trace from one Virtual Machine.
#' The row name represents the time stamp of each obervation, the difference between observation is 300 seconds, the total length of each trace is almost 28 days.
#' The column name represents the hashed name of each virtual machine.
#' The value of each entry represents the maximum CPU value within the time granularity of 5 minutes.
#'
#' @format A data frame with 8000 rows and 100 columns:
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
#' @format A data frame with 8400 rows and 78 columns:
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
#' @format A data frame with 8400 rows and 78 columns:
#' @source \url{https://github.com/Azure/AzurePublicDataset/blob/master/AzurePublicDatasetV1.md}
"periodic_avg_78"
