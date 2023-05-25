#' Find outliers in the data
#'
#' This function detects outliers/anomalies in the data. If the
#' `replace_outlier` argument is set to `TRUE`, then the outliers are removed
#' and imputed using the provided imputation methods.
#'
#' @param dt A data.table.
#' @param replace_outlier Boolean, defaults to `TRUE`. Specify if the outliers
#' are to be removed and imputed.
#' @param imp_methods The imputation methods to be used.
#'
#' @return The outliers found in the data. If the outliers are replaced,
#' then the imputation errors are also returned.
#'

#### Before anomalize got removed
# @importFrom data.table setnames
# @importFrom anomalize anomalize time_decompose time_recompose
####

#' @importFrom stats mad median
detect_outliers <- function(dt, replace_outlier, imp_methods) {
  time <- anomaly <- is_outlier <- value <- remainder <- NULL

  # tbl <- tibbletime::as_tbl_time(dt, index = time)

  # ano <- as.data.table(
  #   time_recompose(
  #     data = anomalize(
  #       data = time_decompose(
  #         data = tbl,
  #         target = value,
  #         message = F
  #       ),
  #       target = remainder
  #     )
  #   )
  # )
  # setnames(ano, c("observed"), c("value"))

  ano <- copy(dt)
  # Implementing Hample filter for outlier detection
  lb <- median(dt$value) - 3 * mad(dt$value)
  ub <- median(dt$value) + 3 * mad(dt$value)
  ano[, "is_outlier" := ifelse(value < lb | value > ub, T, F)]

  df <- ano[is_outlier == TRUE, c("time", "value")]

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
  ret[, "orig_value" := value][is_outlier == TRUE, "value" := NA]
  l <- impute(ret[, c("time", "value")], imp_methods)
  ret[is_outlier == TRUE, "value" := l$imp_best$value][is_outlier == TRUE, "method_used" := l$imp_best$method_used]
  ret <- ret[is_outlier == TRUE, c("time", "value", "orig_value", "method_used")]
  return(
    list(
      "outliers" = ret,
      "outlier_mcar_err" = l$mcar_err,
      "outlier_mar_err" = l$mar_err
    )
  )
}
