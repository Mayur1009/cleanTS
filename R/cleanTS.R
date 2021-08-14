#' Clean univariate time-series data
#'
#' `cleanTS()`is the main function of the package which creates a cleanTS
#' object. It performs all the different data cleaning tasks, such as
#' converting the timestamps to proper format, imputation of missing values,
#' handling outliers, etc.
#'
#' @param data A data frame containg the input data. By default, it considers
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
#' # Convert sunspots.month to dataframe
#' data <- timetk::tk_tbl(sunspot.month)
#' print(data)
#'
#' # Randomly insert missing values to simulate missing value imputation
#' set.seed(10)
#' ind <- sample(nrow(data), 100)
#' data$value[ind] <- NA
#'
#' # Perform cleaning
#' cts <- cleanTS(data, date_format = "my", time = "index", value = "value")
#' print(cts)
#' @export
cleanTS <- function(data, date_format,
                    imp_methods = c("na_interpolation", "na_locf",
                                    "na_ma", "na_kalman"),
                    time = NULL, value = NULL,
                    replace_outliers = T) {
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

  clean_data[is_outlier == T, c("method_used")] <- outliers[, c("method_used")]

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
