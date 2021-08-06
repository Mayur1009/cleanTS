#' Missing timestamps
#'
#' This function finds and inserts the missing timestamps in the time columns
#' of the data. The observations for the inserted timestamps are filled with
#' `NA`.
#'
#' @param dt Input data
#'
#' @return A list of data.table with inserted missing timestamps and the
#' missing timestamps.

missing_timestamps <- function(dt) {
  incomp <- copy(dt)

  # Insert missing timestamps
  all_ts <- data.table(time = seq(min(incomp$time), max(incomp$time), by = min(diff(incomp$time[1:2])))) # TODO: need change in diff()
  comp <- incomp[all_ts, on = "time"] # Right join
  missing_ts <- all_ts[!incomp, on = "time"] # anti join
  return(list("ts" = comp, "missing_ts" = missing_ts$time))
}
