#' This function is used to check and verify the input data to the package.
#' The package need a univariate time series as input.
#' This function keeps the first 2 columns, 1st is renamed as time and other is renamed as value.
#'
#' @param df Input data.
#' @param dt_format The format of the time column in the input data.
#' @param time Name of the column to use as time.
#' @param value Name of column to use as value(observations).
#'
#' @return Data containing 2 columns, time and value. Time column is POSIX object and value is double
check_input <- function(df, dt_format, time, value) {

  # setDT(df)
  # tab <- copy(df)

  tab <- as.data.table(df)

  if(xor(is.null(time), is.null(value))) {
    stop("Either the time or value argument was passed. Both the arguments time and value should be provided.")
  } else if(is.null(time) && is.null(value)) {
    if (ncol(df) > 2) {
      print(paste0("Input contains more than 2 columns. Discarding rows 3 to ", ncol(df), "."))
      tab <- df[, 1:2]
    }
  } else {
    tab <- df[, c(time, value), with = F]
  }

  names(tab) <- c("time", "value")

  if(any(is.na(tab$time))) {
    stop("The time column contains NA.")
  }

  tab[, "time" := lubridate::parse_date_time(time, orders = dt_format)]
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
