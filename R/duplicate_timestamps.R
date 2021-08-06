#' Duplicate Timestamps
#'
#' This function finds and removes the duplicate timestamps in the time columns
#' of the data.
#'
#'
#' @param dt Input data
#'
#' @return A list of data.table without duplicate timestamps and the
#' duplicate timestamps.
#'
#'
duplicate_timestamps <- function(dt) {
  time <- N <- NULL

  ret <- copy(dt)
  dupts <- ret[, .N, by = "time"][N > 1]$time
  if (length(dupts) > 0) {
    vals <- sapply(
      dupts,
      function(i) {
        x <- ret[time == i]$value
        ifelse(length(unique(x)) == 1, x[1], NA)
      }
    )
    t <- data.table(time = dupts, value = vals)
    ret <- rbind(ret[!(time %in% dupts)], t)
  }
  res <- list(
    "ts" = ret[order(time)],
    "duplicates" = dupts
  )
  return(res)
}
