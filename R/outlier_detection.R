#' Find outliers in the data
#'
#' This function detects outliers/anomalies in the data. If the
#' `replace_outlier` argument is set to `TRUE`, then the outliers are removed
#' and imputated using the provided imputation methods.
#'
#' @param dt A data.table.
#' @param replace_outlier Boolean, defaults to `TRUE`. Specify if the outliers
#' are to be removed and imputated.
#' @param imp_methods The imputation methods to be used.
#'
#' @return The outliers found in the data. If the outliers are replaced,
#' then the imputation errors are also returned.
#'
#' @importFrom data.table setnames

detect_outliers <- function(dt, replace_outlier, imp_methods) {
  time <- anomaly <- is_outlier <- value <- remainder <- NULL

  tbl <- tibbletime::as_tbl_time(dt, index = time)

  ano <- as.data.table(
    anomalize::time_recompose(
      anomalize::anomalize(
        data = anomalize::time_decompose(
          data = tbl,
          target = value,
          message = F
        ),
        target = remainder
      )
    )
  )
  # ano <- tbl %>%
  #   anomalize::time_decompose(.data$value, message = F) %>%
  #   anomalize::anomalize(.data$remainder) %>%
  #   anomalize::time_recompose() %>%
  #   as.data.table
  setnames(ano, c("observed"), c("value"))
  ano[, "is_outlier" := ifelse(anomaly == "Yes", T, F)]
  df <- ano[is_outlier == T, c("time", "value")]

  if (!replace_outlier | nrow(df) == 0) {
    return(
      list(
        "outliers" = df[, "method_used" := NA][, "orig_value" := value],
        "outlier_mcar_err" = data.frame(),
        "outlier_mar_err" = data.frame()
      )
    )
  }

  ret <- ano[, c("time", "value", "is_outlier")]
  ret[, "orig_value" := value][is_outlier == T, "value" := NA]
  l <- impute(ret[, c("time", "value")], imp_methods)
  ret[is_outlier == T, "value" := l$imp_best$value][is_outlier == T, "method_used" := l$imp_best$method_used]
  ret <- ret[is_outlier == T, c("time", "value", "orig_value", "method_used")]
  return(
    list(
      "outliers" = ret,
      "outlier_mcar_err" = l$mcar_err,
      "outlier_mar_err" = l$mar_err
    )
  )
}