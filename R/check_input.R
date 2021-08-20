#' Check input data
#'
#' This function is used to check and verify the input data given as input.
#' The package needs a univariate time series as input. This function keeps
#' the first 2 columns, first is renamed as time and second is renamed as value.
#' If the optional `time` and `value` arguments are provided then they are used
#' to determine the relevant columns in the data.
#'
#' @param df A data frame containing the input data. If it contains more than
#' two columns then specify the names of time and value columns using the
#' `time` and `value` arguments.
#' @param dt_format Format of timestamps used in the data. It uses lubridate
#' formats as mentioned
#' [here](https://lubridate.tidyverse.org/reference/parse_date_time.html#details).
#' @param time The name of column in provided data to be used as time column.
#' @param value The name of column in provided data, to be used as
#' value(observations) column.
#'
#' @return
#' Data containing 2 columns, time and value. Time column is converted to
#' POSIX object and value to numeric.
#'
#' @importFrom lubridate parse_date_time
#' @importFrom data.table as.data.table ":="
#'

check_input <- function(df, dt_format, time, value) {

  tab <- as.data.table(df)

  if (xor(is.null(time), is.null(value))) {
    stop("Either the time or value argument was passed.
         Both the arguments time and value should be provided.")
  } else if (is.null(time) && is.null(value)) {
    if (ncol(df) > 2) {
      print(
        paste0(
          "Input contains more than 2 columns. Discarding rows 3 to ",
          ncol(df), "."
        )
      )
      tab <- tab[, 1:2]
    }
  } else {
    tab <- tab[, c(time, value), with = F]
  }

  names(tab) <- c("time", "value")

  if (any(is.na(tab$time))) {
    stop("The time column contains NA.")
  }

  tab[, "time" := parse_date_time(time, orders = dt_format)]
  tab[, "value" := as.numeric(value)]

  # TODO:
  # if (any(is.na(tab$time))) {
  #   # print(paste("Time column contains NA, or some timestamps failed to parse. If timestamps have multiple formats specify all of them."))
  #   stop("Time column contains NA, or some timestamps failed to parse. If timestamps have multiple formats specify all of them.")
  #   TODO: Find out the rows with NA and print them.
  #   TODO: In such case what should be returned???
  # }

  return(tab)
}
