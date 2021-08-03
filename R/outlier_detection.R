#' This function detects outliers in the data.
#' @param dt Input data
#' @param replace_outlier Should the detected outliers be removed and imputated using the given imputation methods.
#' @param imp_methods Imputation methods
#'
#' @import magrittr
#'
#'

detect_outliers <- function(dt, replace_outlier, imp_methods) {
  time <- anomaly <- is_outlier <- value <- NULL

  tbl <- tibbletime::as_tbl_time(dt, index = time)
  ano <- tbl %>%
    anomalize::time_decompose(.data$value, message = F) %>%
    anomalize::anomalize(.data$remainder) %>%
    anomalize::time_recompose() %>%
    as.data.table
  setnames(ano, c("observed"), c("value"))
  ano[, "is_outlier" := ifelse(anomaly == "Yes", T, F)]
  df <- ano[is_outlier == T, c("time", "value")]

  if(!replace_outlier | nrow(df) == 0) {
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
