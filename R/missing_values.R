#' Handle missing values in the data
#'
#' This function handles missing values in the data. It compares various
#' imputation methods and finds the best one for imputation.
#'
#'
#' @param dt A data.table.
#' @param methods The imputation methods to be used.
#'
#' @return A data.table with missing data imputed, and the imputation errors.
#'

impute <- function(dt, methods) {
  values <- imp <- value <- type <- method_used <- time <- missing_type <- NULL

  orig_dt <- df <- copy(dt)

  # Add Index column
  df[, i := seq_len(nrow(df))]

  # If no NAs
  if (!any(is.na(df$value))) {
    res <- list(
      "imp_best" = data.table(time = double(), value = numeric()),
      "mcar_err" = data.frame(),
      "mar_err" = data.frame()
    )
    return(res)
  }

  # Break df in blocks of NA's
  t <- rle(as.numeric(is.na(df$value)))
  blocks <-
    data.table(
      values = t$values,
      lengths = t$lengths,
      from = c(1, cumsum(t$lengths)[-length(t$lengths)] + 1),
      to = cumsum(t$lengths)
    )

  # Seperate MAR and MCAR
  mar <- blocks[values == 1 & lengths > 1]
  mcar <- blocks[values == 1 & lengths == 1]

  # Calculate percentage of MAR, MCAR
  permar <- 100 * sum(mar$lengths) / nrow(df)
  permcar <- 100 * nrow(mcar) / nrow(df)

  # Calculate imputation error for MCAR and MAR
  mcar_err <- mar_err <- data.frame()
  best_mcar_name <- best_mar_name <- NULL
  imputed <- df$value

  if (permcar > 0) {
    mcar_err <- data.frame(
      imputeTestbench::impute_errors(
        df$value,
        smps = "mcar",
        missPercentFrom = permcar,
        missPercentTo = permcar,
        methods = methods
      )
    )[-(1:2)]
    best_mcar_name <- names(which.min(mcar_err))
    best_mcar_fun <- eval(parse(text = best_mcar_name))
    imputed <- best_mcar_fun(df$value)

    # Make MAR missing values NA
    for (i in seq_len(nrow(mar))) {
      imputed[mar$from[i]:mar$to[i]] <- NA
    }
  }

  if (permar > 0) {
    mar_err <- data.frame(
        imputeTestbench::impute_errors(
        df$value,
        smps = "mar",
        missPercentFrom = permar,
        missPercentTo = permar,
        methods = methods
      )
    )[-(1:2)]
    best_mar_name <- names(which.min(mar_err))
    best_mar_fun <- eval(parse(text = best_mar_name))
    imputed <- best_mar_fun(imputed)
  }

  # Filter NAs, add columns for method used and type(mar, mcar)
  nas <-
    df[
      , imp := imputed
    ][
      is.na(value)
    ][
      , "missing_type" := ifelse(i %in% mcar$from, "mcar", "mar")
    ][
      , "method_used" := ifelse(missing_type == "mcar", best_mcar_name, best_mar_name)
    ][
      , "value" := imp
    ][
      , c("time", "value", "missing_type", "method_used")
    ]

  res <- list(
    "imp_best" = nas,
    "mcar_err" = mcar_err,
    "mar_err" = mar_err
  )
  return(res)
}

#' @importFrom imputeTS na_interpolation
#' @export
imputeTS::na_interpolation

#' @importFrom imputeTS na_kalman
#' @export
imputeTS::na_kalman

#' @importFrom imputeTS na_locf
#' @export
imputeTS::na_locf

#' @importFrom imputeTS na_ma
#' @export
imputeTS::na_ma

#' @importFrom imputeTS na_mean
#' @export
imputeTS::na_mean
