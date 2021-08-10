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
#'
#' @importFrom lubridate period
#' @importFrom data.table copy data.table

missing_timestamps <- function(dt) {

  incomp <- copy(dt)

  # Time difference between 1st and 2nd timestamps
  d <- find_dif(incomp$time[1], incomp$time[2])

  # Check time diff between random timestamps in the data.
  for (i in sample((nrow(dt) - 1), 10)) {
    dif <- find_dif(incomp$time[i], incomp$time[i + 1])

    # If the time diff is less for this sample, then use it
    if (period(dif) < period(d)) {
      d = dif
    }
  }

  # Insert missing timestamps
  all_ts <- data.table(
    time = seq(
      min(incomp$time),
      max(incomp$time),
      by = d
    )
  )

  comp <- incomp[all_ts, on = "time"] # Right join
  missing_ts <- all_ts[!incomp, on = "time"] # anti join
  return(list("ts" = comp, "missing_ts" = missing_ts$time))
}

#' Helper function to find the time difference between two given timestamps.
#'
#' @param time1 POSIXt or Date object.
#' @param time2 POSIXt or Date object.
#'
#' @return String, specifying the time interval between `time1` and `time2`.
#' It contains a integer and the unit, for e.g., *5 weeks*, *6 months*,
#' *14 hours*, etc.
#'
#' @importFrom lubridate interval period
#'
find_dif <- function(time1, time2) {
  i <- interval(time1, time2)
  for (f in c("years", "months", "weeks", "days", "hours", "minutes",
              "seconds")) {
    t <- i %/% period(f)
    if (t > 0) {
      return(paste(t, f))
    }
  }
  return(NULL)
}

