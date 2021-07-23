#' Generate animated plot.
#' @param obj Object of cleanTS
#' @param interval Viewing interval
#'
#' @import ggplot2
#' @import transformr
#' @import gifski
#' @import glue
#' @import stringr
#'
#' @export

animate_interval <- function(obj, interval) {
  . <- X <- value <- color <- shape <- state <- time <- NULL


  plot_lst <- prepare_plot_data(obj, interval)
  pdf_l <- plot_lst$pdf_l
  pdf_p <- plot_lst$pdf_p

  frame.lenghts <- pdf_l[, .N, by = "state"]
  max.X <- max(frame.lenghts$N)
  max.value <- max(pdf_l$value)
  min.value <- min(pdf_l$value)
  last_state <- max(frame.lenghts$state)

  dif <- (pdf_l[2, "time"] - pdf_l[1, "time"])$time

  # clean_data <- copy(obj$clean_data)
  clean_data <- obj$clean_data[pdf_l, on = .(time, value)]
  clean_data[, "state" := pdf_l$state]
  state_lst <- split(clean_data, clean_data$state)
  caption_lst <- sapply(state_lst, function(x) capgen(x, obj$missing_ts, obj$duplicate_ts))
  # caption <- report(clean_data)

  labx <- ifelse(
    is.character(interval),
    interval,
    as.character(paste(interval, units(dif)))
  )
  p <- ggplot() +
    geom_line(
      data = pdf_l,
      mapping = aes(x = X, y = value),
      alpha = 1,
      na.rm = T
    ) +
    geom_point(
      data = pdf_l,
      mapping = aes(x = X, y = value),
      alpha = 0.75,
      na.rm = T
      # color = "#404040"
    ) +
    geom_point(
      data = pdf_p,
      mapping = aes(x = X, y = value, color = color, shape = shape),
      alpha = 1,
      stroke = 2,
      size = 3,
      na.rm = T
    ) +
    scale_shape_manual(values = c("missing_value" = 1, "outlier" = 4)) +
    coord_cartesian(xlim = c(0, max.X), ylim = c(min.value, max.value))

  anim <- p +
    theme_bw() +
    labs(x = labx, y = "value", caption = "State No: {next_state}<br>{caption_lst[next_state]}") +
    theme(text = element_text(size = 12), legend.text = element_text(size = 12), plot.caption = ggtext::element_markdown(hjust = 0, size = 12, lineheight = 1.2)) +
    gganimate::transition_states(state, transition_length = 1, state_length = 3) +
    gganimate::shadow_mark(alpha = alpha / 10) +
    # shadow_wake(10/last_state, wrap = F, size = NULL) +
    gganimate::exit_fade()

  return(list("animation" = anim, "nstates" = last_state))
}

# Caption generator (HTML and markdown)
capgen <- function(data, miss_ts, dup_ts) {
  tc <- textConnection("str", "w")
  sink(tc)
  reportHelper(data, miss_ts, dup_ts)
  sink()
  close(tc)
  # str[1] <- glue("<pre>{str[1]}</pre>")
  # str <- str_replace_all(str, " ", "<span> </span>")
  str <- glue_collapse(str, sep = "<br>")
  str
}


reportHelper <- function(data, miss_ts, dup_ts) {
  is_outlier <- missing_type <- time <- NULL
  outliers <- data[is_outlier == T]
  missing  <- data[!is.na(missing_type) & is_outlier == F]
  miss_ts  <- data[time %in% miss_ts]
  dup_ts   <- data[time %in% dup_ts]

  n.total    <- nrow(data)
  n.miss_ts  <- nrow(miss_ts)
  n.dup_ts   <- nrow(dup_ts)
  n.outliers <- nrow(outliers)
  n.missing  <- nrow(missing)
  n.mcar     <- nrow(missing[missing_type == "mcar"])
  n.mar      <- nrow(missing[missing_type == "mar"])

  per.miss_ts  <- 100 * n.miss_ts/n.total
  per.dup_ts   <- 100 * n.dup_ts/n.total
  per.outliers <- 100 * n.outliers/n.total
  per.missing  <- 100 * n.missing/n.total
  per.mcar     <- 100 * n.mcar/n.total
  per.mar      <- 100 * n.mar/n.total

  cat("**Summary:** \n")
  print(summary(data$value))

  cat("\n**Missing timestamps:** ", n.miss_ts)
  cat("\n**Duplicate timestamps:** ", n.dup_ts)
  cat("\n**Missing Values:** ", n.missing, paste0("(", round(per.missing, 3), "%)"))
  cat("\n**Outliers:** ", n.outliers)
}

#' Generate animation
#'
#' @param anim List of gganim and nstates
#' @param nframes No. of frames
#' @param duration Duration of animation
#' @param ... Extra animation
#'
#' @export
gen.animation <- function(anim, nframes = 2*anim$nstates, duration = anim$nstate, ...) {
  animate(anim$animation, nframes = nframes, duration = duration, ...)
}




#' @importFrom gganimate animate
#' @export
gganimate::animate

#' @importFrom gganimate anim_save
#' @export
gganimate::anim_save
