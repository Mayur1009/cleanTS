#' Generate a report.
#'
#' `gen.report()` generates a report of the entire process and the changes made
#' to the original data.
#'
#' @param obj A *cleanTS* object.
#'
#'
#' @return Does not return any value.
#' @examples
#' # Convert sunspots.month to dataframe
#' data <- timetk::tk_tbl(sunspot.month)
#'
#' # Randomly insert missing values to simulate missing value imputation
#' set.seed(10)
#' ind <- sample(nrow(data), 100)
#' data$value[ind] <- NA
#'
#' # Perform cleaning
#' cts <- cleanTS(data, date_format = "my", time = "index", value = "value")
#'
#' gen.report(cts)
#'
#' @export
gen.report <- function(obj) {
  is_outlier <- missing_type <- NULL


  n.total <- nrow(obj$clean_data)
  n.miss_ts <- length(obj$missing_ts)
  n.dup_ts <- length(obj$duplicates)
  n.outliers <- nrow(obj$outliers)
  per.outliers <- 100 * n.outliers / n.total

  missing_data <- obj$clean_data[is_outlier == F & !(is.na(missing_type))]
  missing_mcar <- missing_data[missing_type == "mcar", c("time", "value", "method_used")]
  missing_mar <- missing_data[missing_type == "mar",  c("time", "value", "method_used")]

  n.missing <- nrow(missing_data)
  per.missing <- 100 * n.missing / n.total
  n.mcar <- nrow(missing_mcar)
  per.mcar <- 100 * n.mcar / n.total
  n.mar <- nrow(missing_mar)
  per.mar <- 100 * n.mar / n.total


  cat("\n# Summary of cleaned data: \n")
  print(summary(obj$clean_data$value))

  cat("\n# Missing timestamps: ", n.miss_ts, "\n")
  if (n.miss_ts > 0) {
    print(obj$missing_ts)
  } else {
    cat("\nNo missing timestamps found.\n")
  }

  cat("\n# Duplicate timestamps: ", n.dup_ts, "\n")
  if (n.dup_ts > 0) {
    print(obj$duplicates)
  } else {
    cat("\nNo duplicate timestamps found.\n")
  }

  cat("\n# Missing Values: ", n.missing, paste0("(", per.missing, "%)"))

  cat("\n\n## MCAR: ", n.mcar, paste0("(", per.mcar, "%)"))
  if (n.mcar > 0) {
    cat("\n MCAR Errors:\n")
    print(obj$mcar_err)
    cat("\n")
    print(missing_mcar)
  } else {
    cat("\nNo MCAR found.\n")
  }

  cat("\n\n## MAR: ", n.mar, paste0("(", per.mar, "%)"))
  if (n.mar > 0) {
    cat("\n MAR Errors:\n")
    print(obj$mar_err)
    print(missing_mar)
  } else {
    cat("\nNo MAR found.\n")
  }

  cat("\n# Outliers: ", n.outliers, "\n")
  if (n.outliers > 0) {
    print(obj$outliers)
    cat("## Imputation errors while replacing outliers:\n")
    cat("### MCAR errors:\n")
    print(obj$outlier_mcar_err)
    cat("### MAR errors:\n")
    print(obj$outlier_mar_err)
  } else {
    cat("\nNo outliers found.\n")
  }
}
