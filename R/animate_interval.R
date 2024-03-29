#' Generate animated plot
#'
#' `animate_interval()` creates an animated plot using a `cleanTS` object
#' and a interval.
#'
#' First, the data is split according to the `interval` argument passed to the
#' function. If it is a numeric value, the cleaned data is split into dataframes
#' containing `interval` observations. It can also be a string, like
#' **1 week**, **3 months**, **14 days**, etc. In this case, the data
#' is split according to the `interval` given, using the timestamp column. Then
#' an animation is created using the spliited data, with the help of `gganimate`
#' package. The `animate_interval()` function returns a list containing
#' the `gganim` object used to generate the animation and the number of
#' states in the data. The animation can be generated using the
#' `gen.animation()` function and saved using the `anim_save()`
#' function. The plots in the animation also contain a short summary, containing
#' the statistical information and the number of missing values, outliers,
#' missing timestamps, and duplicate timestamps in the data shown in that frame
#' of animation.
#'
#' @param obj A *cleanTS* object.
#' @param interval A numeric or character, specifying the viewing interval.
#'
#' @return
#' A list containing:
#' * animation: A `gganim` object.
#' * nstates: The number of states in the animation.
#'
#' @examples
#' \dontrun{
#'   # Create a `gganim` using `animate_interval()` function
#'   a <- animate_interval(cts, "10 year")
#'
#'   # cts -> `cleanTS` object created using `cleanTS()`.
#' }
#'
#' @import ggplot2
#' @import transformr
#' @import glue
#' @import stringr
#' @importFrom data.table .N
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
    as.character(paste(interval))
    # as.character(paste(interval, units(dif)))
    # TODO: units(dif) will not work. think of an alternative.
  )
  p <- ggplot() +
    geom_line(
      data = pdf_l,
      mapping = aes(x = X, y = value),
      alpha = 1,
      na.rm = TRUE
    ) +
    geom_point(
      data = pdf_l,
      mapping = aes(x = X, y = value),
      alpha = 0.75,
      na.rm = TRUE
      # color = "#404040"
    ) +
    geom_point(
      data = pdf_p,
      mapping = aes(x = X, y = value, color = color, shape = shape),
      alpha = 1,
      stroke = 2,
      size = 3,
      na.rm = TRUE
    ) +
    scale_shape_manual(values = c("missing_value" = 1, "outlier" = 4)) +
    coord_cartesian(xlim = c(0, max.X), ylim = c(min.value, max.value))

  anim <- p +
    theme_bw() +
    labs(
      x = labx,
      y = "value",
      caption = "State No: {next_state}<br>{caption_lst[next_state]}"
    ) +
    theme(
      text = element_text(size = 12),
      legend.text = element_text(size = 12),
      plot.caption = ggtext::element_markdown(
        hjust = 0,
        size = 12,
        lineheight = 1.2
      )
    )


  anim <- anim +
    gganimate::transition_states(
      state,
      transition_length = 1,
      state_length = 3
    ) +
    gganimate::shadow_mark(alpha = alpha / 10)
    # shadow_wake(10/last_state, wrap = F, size = NULL) +
    #gganimate::exit_fade()

  return(list("animation" = anim, "nstates" = last_state))
}

# Caption generator (HTML and markdown)
capgen <- function(data, miss_ts, dup_ts) {
  tc <- textConnection("str", "w", local = TRUE)
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
  outliers <- data[is_outlier == TRUE]
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
#' This function takes the list outputted by `animate_interval()` and generates
#' a GIF animation. It is a simple wrapper around the `gganimate::animate()`
#' function with some defaults. The generated GIF can be saved using the
#' `anim_save()` function. By default, in the animate() function only 50 states
#' in the data are shown. So, to avoid this gen.animation() defines the default
#' value for the number of frames. Also, the duration argument has a default
#' value equal to the number of states, making the animation slower. More
#' arguments can be passed, which are then passed to animate(), like, height,
#' width, fps, renderer, etc.
#'
#' @param anim List outputted by the `animate_interval()` function containing
#' a `gganim` object and  the number of states in the animation.
#' @param nframes Number of frames. Defaults to double the number of states
#' in the animation.
#' @param duration The duration of animation. Defaults to the number of states
#' in the animation.
#' @param ... Extra arguments passed to `gganimate::animate()`.
#'
#' @return Does not return any value.
#'
#' @examples
#' \dontrun{
#'   a <- animate_interval(cts, "10 year")
#'
#'   # Generate animation using `gen.animation()`
#'   if(interactive()){
#'     gen.animation(a, height = 700, width = 900)
#'   }
#'
#'   # Save animation using `anim_save()`
#'   anim_save("filename.gif")
#' }
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
