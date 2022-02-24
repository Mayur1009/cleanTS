#' Merge Multiple CSV files
#'
#' `mergecsv()` takes a folder containing CSV files and merges them into a
#' single *data.table*. It is assumed that the first column of all the CSVs
#' contains the timestamps.
#'
#' All these files are read and the first column is parsed to a proper DateTime
#' object using the formats given in the formats argu- ment. Then these
#' dataframes are merged using the timestamp column as a common column. The
#' merged data frame returned by the function contains the first column as the
#' timestamps.
#'
#' @param path Path to the folder.
#' @param formats Datetime formats.
#'
#' @return Merged `data.table`.
#'
#' @importFrom stringr str_c str_ends
#' @importFrom data.table fread merge.data.table
#' @importFrom lubridate parse_date_time
#'
#' @export
#'

mergecsv <- function(path, formats) {
  time <- NULL
  if (!dir.exists(path)) {
    stop("Path not found.")
  }

  # TODO: Is this same for windows??
  if (!str_ends(path, "/")) {
    path <- str_c(path, "/")
  }

  all_files <- dir(path, recursive = F)
  csv_files <- str_c(path, all_files[str_ends(all_files, ".csv")])

  dts <- lapply(
    csv_files,
    function(x) {
      dt <- fread(x)
      names(dt)[1] <- "time"
      dt[, "time" := parse_date_time(time, orders = formats)]
    }
  )

  mergedDT <- Reduce(function(x, y) merge(x, y, all = TRUE), dts)

  mergedDT
}
