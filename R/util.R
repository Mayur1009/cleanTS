prepare_plot_data <- function(obj, interval) {
  state <- X <- value <- color <- time <- is_outlier <- NULL


  orig_data <- copy(obj$clean_data)
  orig_data[is_outlier == T, c("value", "missing_type", "method_used") := list(obj$outliers$orig_value, NA, NA)]

  imp_all <- impute_all(orig_data, obj$imp_methods)

  pdf_l <- copy(orig_data)
  pdf_l <- pdf_l[, "state" := cut(time, breaks = interval, labels = F, start.on.monday = F)][, "X" := seq_len(length(value)), by = "state"][, c("time", "value", "state", "X", "is_outlier")]

  dif <- (pdf_l[2, "time"] - pdf_l[1, "time"])$time

  if (is.character(interval)) {
    spt <- strsplit(interval, " ")[[1]]
    time2 <- pdf_l[state == 2 & X == 1]$time
    exptime1 <- time2 - lubridate::period(num = as.numeric(spt[1]), units = spt[2])
    offset <- length(seq(from = exptime1, to = pdf_l[1, ]$time, by = dif))
    pdf_l[state == 1, X := X + offset]
  }

  pdf_p <- imp_all[pdf_l, on = "time"][is_outlier == T, c("original_outlier", "replaced_outlier") := list(obj$outliers$orig_value, obj$outliers$value)]
  pdf_p <- melt(pdf_p, id.vars = c("time", "X", "is_outlier", "state"), variable.name = "color")[!is.na(value) & color != "value"]
  pdf_p[, "shape" := ifelse(color == "original_outlier" | color == "replaced_outlier", "outlier", "missing_value")]

  n.state1 <- nrow(pdf_l[state == 1])
  last_state <- max(pdf_l$state)
  n.laststate <- nrow(pdf_l[state == last_state])

  if (n.state1 == 1) {
    pdf_l <- pdf_l[state != 1]
  }
  if (n.laststate == 1) {
    pdf_l <- pdf_l[state != last_state]
  }

  pdf_l <- pdf_l[, c("time", "value", "state", "X")]
  pdf_p <- pdf_p[, c("time", "value", "state", "X", "color", "shape")]
  setcolorder(pdf_l, c("time", "X", "value", "state"))
  setcolorder(pdf_p, c("time", "X", "value", "state", "color", "shape"))

  return(list("pdf_l" = pdf_l, "pdf_p" = pdf_p))
}



impute_all <- function(dt, methods) {
  missing_type <- is_outlier <- value <- NULL
  df <- copy(dt)
  df[!(is.na(missing_type)) & is_outlier == F, value := NA]
  imp_all <- df[, c("time", "value")]
  imp_all_names <- names(imp_all)
  # TODO: Can this for loop be avoided??
  for (m in methods) {
    fun <- eval(parse(text = m))
    imp_all <- cbind(imp_all, fun(df$value))
    imp_all_names <- c(imp_all_names, m)
  }
  names(imp_all) <- imp_all_names
  imp_all <- imp_all[is.na(value), imp_all_names[-2], with = F]
  imp_all
}
