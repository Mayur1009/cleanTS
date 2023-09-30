#' Clean univariate time-series data
#'
#' `cleanTS()`is the main function of the package which creates a cleanTS
#' object. It performs all the different data cleaning tasks, such as
#' converting the timestamps to proper format, imputation of missing values,
#' handling outliers, etc. It is a wrapper function that calls all the other
#' internal functions to performs different data cleaning tasks.
#'
#' The first task is to check the input time series data for structural and
#' data type-related errors. Since the functions need univariate time series
#' data, the input data is checked for the number of columns. By default, the
#' first column is considered to be the time column, and the second column to
#' be the observations. Alternatively, if the time and value arguments are
#' given, then those columns are used. The time column is converted to a POSIX
#' object. The value column is converted to a numeric type. The column names
#' are also changed to time and value. All the data is converted to a
#' \emph{data.table} object. This data is then passed to other functions to
#' check for missing and duplicate timestamps. If duplicate timestamps are
#' found, then the observation values are checked. If the observations are the
#' same, then only one copy of that observation is kept. But if the observations
#' are different, then it is not possible to find the correct one, so the
#' observation is set to NA. This data is the passed to a function for finding
#' and handling missing observations. The methods given in the imp_methods
#' argument are compared and selected. The MCAR and MAR values are handled
#' seperately. After the best methods are found, imputation is performed using
#' those methods. The user can also pass user-defined functions for comparison.
#' The user-defined function should follow the structure as the default
#' functions. It should take a numeric vector containing missing values as
#' input, and return a numeric vector of the same length without missing values
#' as output. Once the missing values are handled the data is checked for
#' outliers. If the replace_outliers parameter is set to TRUE in the cleanTS()
#' function, then the outliers are replaced by NA and imputed using the
#' procedure mentioned for imputing missing values. Then it creates a cleanTS
#' object which contains the cleaned data, missing timestamps, duplicate
#' timestamps, imputation methods, MCAR imputation error, MAR imputation error,
#' outliers, and if the outliers are replaced then imputation errors for those
#' imputations are also included. The \emph{cleanTS} object is returned by the
#' function.
#'
#' @param data A data frame containing the input data. By default, it considers
#' that the first column to contain the timestamps and the second column
#' contains the observations.If that is not the case or if it contains more than
#' two columns then specify the names of time and value columns using the
#' `time` and `value` arguments.
#' @param date_format Format of timestamps used in the data. It uses lubridate
#'  formats as mentioned
#' [here](https://lubridate.tidyverse.org/reference/parse_date_time.html#details).
#'  More than one formats can be using a vectors of strings.
#' @param imp_methods The imputation methods to be used.
#' @param time Optional, the name of column in provided data to be used as
#'  time column.
#' @param value Optional, the name of column in provided data, to be used as
#'  value column.
#' @param replace_outliers Boolean, if `TRUE` then the outliers found will be
#'  removed and imputed using the given imputation methods.
#'
#' @return A `cleanTS` object which contains:
#'  * Cleaned data
#'  * Missing timestamps
#'  * Duplicate timestamps
#'  * Imputation errors
#'  * Outliers
#'  * Outlier imputation errors
#'
#' @examples
#' \dontrun{
#'   # Convert sunspots.month to dataframe
#'   data <- timetk::tk_tbl(sunspot.month)
#'   print(data)
#'
#'   # Randomly insert missing values to simulate missing value imputation
#'   set.seed(10)
#'   ind <- sample(nrow(data), 100)
#'   data$value[ind] <- NA
#'
#'   # Perform cleaning
#'   cts <- cleanTS(data, date_format = "my", time = "index", value = "value")
#'   print(cts)
#' }
#' @export
cleanTS <- function(data, date_format,
                    imp_methods = c("na_interpolation", "na_locf",
                                    "na_ma", "na_kalman"),
                    time = NULL, value = NULL,
                    replace_outliers = TRUE) {
  is_outlier <- NULL

  repo <- list()

  df <- check_input(data, date_format, time, value)

  l <- missing_timestamps(df)
  repo$missing_ts <- l$missing_ts

  l <- duplicate_timestamps(l$ts)
  repo$duplicates <- l$duplicates

  imp <- impute(l$ts, imp_methods)

  clean_data <- l$ts
  if (nrow(imp$imp_best) > 0) {
    clean_data[is.na(value)] <- imp$imp_best[, c("time", "value")]
  }

  out_list <- detect_outliers(clean_data, replace_outliers, imp_methods)
  outliers <- out_list$outliers
  if (replace_outliers & nrow(outliers) > 0) {
    clean_data[time %in% outliers$time, "value"] <- outliers$value
  }

  clean_data <- imp$imp_best[clean_data,
    on = c("time", "value")
  ][
    ,
    "is_outlier" :=
      ifelse(time %in% outliers$time, T, F)
  ]

  clean_data[is_outlier == TRUE, c("method_used")] <- outliers[, c("method_used")]

  res <- structure(
    list(
      "clean_data" = clean_data,
      "missing_ts" = repo$missing_ts,
      "duplicate_ts" = repo$duplicates,
      "imp_methods" = imp_methods,
      "mcar_err" = imp$mcar_err,
      "mar_err" = imp$mar_err,
      "outliers" = out_list$outliers,
      "outlier_mcar_err" = out_list$outlier_mcar_err,
      "outlier_mar_err" = out_list$outlier_mar_err
    ),
    class = "cleanTS"
  )

  return(res)
}

#' Print a *cleanTS* object
#'
#' Print method for `cleanTS` class.
#'
#' @param x cleanTS object
#' @param ... Other arguments
#'
#' @return Does not return any value.
#'
#' @examples
#' \dontrun{
#' # Using the same data as in `cleanTS()` function example.
#' cts <- cleanTS(data, "my")
#' print(cts)
#' }
#'
#' @importFrom tibble as_tibble
#'
#' @export
#'
#' @method print cleanTS
print.cleanTS <- function(x, ...) {
  l <- list(
    "clean_data" = as_tibble(x$clean_data),
    "missing_ts" = x$missing_ts,
    "duplicate_ts" = x$duplicate_ts,
    "imp_methods" = paste(x$imp_methods, collapse = ", "),
    "mcar_err" = as_tibble(x$mcar_err),
    "mar_err" = as_tibble(x$mar_err),
    "outliers" = as_tibble(x$outliers),
    "outlier_mcar_err" = as_tibble(x$outlier_mcar_err),
    "outlier_mar_err" = as_tibble(x$outlier_mar_err)
  )
  print(l)
}
