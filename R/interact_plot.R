#' Create interactive plot
#'
#' Interactive plot is similar to the animated plot, but gives the used some
#' control over the animation. It runs a shinyApp instead of creating a GIF.
#'
#' The problem with an animated plot is that the user does not have any control
#' over the animation. There is not play or pause functionality so that the
#' user can observe any desired frame. This can be achieved by adding
#' interactivity to the plot. The `interact_plot()` function creates and runs a
#' shiny widget locally on the machine. It takes the cleanTS object and splits
#' the cleaned data according to the `interval` argument, similar to the
#' `animate interval()` function. It then creates a shiny widget which shows the
#' plot for the current state and gives a slider used to change the state.
#' Unlike `animate_interval()` it provides a global report containing information
#' about complete data, and a state report giving information about the current
#' state shown in the plot.
#'
#' @param obj A *cleanTS* object.
#' @param interval A numeric or character, specifying the viewing interval.
#'
#' @return Does not return any value.
#'
#' @examples
#' \dontrun{
#' # Using the same data used in `cleanTS()` function example.
#' interact_plot(cts, interval = "1 week")
#' }
#'
#' @import shiny
#' @import ggplot2
#' @importFrom data.table setcolorder rbindlist
#'
#' @export
interact_plot <- function(obj, interval) {
  state <- X <- value <- color <- shape <- time <- NULL

  plot_lst <- prepare_plot_data(obj, interval)
  pdf_l <- plot_lst$pdf_l
  pdf_p <- plot_lst$pdf_p

  frame.lenghts <- pdf_l[, .N, by = "state"]
  max.X <- max(frame.lenghts$N)
  max.value <- max(pdf_l$value)
  min.value <- min(pdf_l$value)
  last_state <- max(frame.lenghts$state)

  dif <- (pdf_l[2, "time"] - pdf_l[1, "time"])$time
  labx <- ifelse(
    is.character(interval),
    interval,
    as.character(paste(interval, units(dif)))
  )

  n.mth <- length(obj$imp_methods)
  dummy_data <-
    data.table(
      time = as.POSIXct(c(NA, NA, rep(NA, n.mth))),
      value = as.numeric(c(NA, NA, rep(NA, n.mth))),
      X = as.integer(c(NA, NA, rep(NA, n.mth))),
      color = as.factor(c("original_outlier", "replaced_outlier", obj$imp_methods)),
      shape = c("outlier", "outlier", rep("missing_value", n.mth))
    )

  # TODO: Make gen.* functions self dependent. Also try to merge gen.report() and gen.framereport().
  gen.frameplot <- function(plot_l, plot_p) {
    ggplot() +
      geom_line(
        data = plot_l,
        mapping = aes(x = X, y = value),
        na.rm = TRUE
      ) +
      geom_point(
        data = plot_l,
        mapping = aes(x = X, y = value),
        na.rm = TRUE
      ) +
      geom_point(
        data = plot_p,
        mapping = aes(x = X, y = value, color = color, shape = shape),
        size = 2,
        stroke = 2,
        na.rm = TRUE
      ) +
      scale_shape_manual(values = c("missing_value" = 1, "outlier" = 4)) +
      coord_cartesian(xlim = c(0, max.X), ylim = c(min.value, max.value)) +
      theme_bw() +
      labs(x = labx)
  }

  gen.framereport <- function(obj, plot_l, plot_p) {

    miss_ts <- plot_l[time %in% obj$missing_ts]$time
    dupl_ts <- plot_l[time %in% obj$duplicate_ts]$time
    miss_val <- obj$clean_data[time %in% plot_p[shape == "missing_value"]$time]
    out_val <- obj$outliers[time %in% plot_p$time]

    n.total <- nrow(plot_l)
    n.miss_ts <- length(miss_ts)
    n.dupl_ts <- length(dupl_ts)
    n.miss_val <- nrow(miss_val)
    n.out_val <- nrow(out_val)


    cat("Summary of frame:\n")
    print(summary(plot_l$value))

    cat("\n# Missing timestamps: ", n.miss_ts, "\n")
    if (n.miss_ts > 0) {
      print(miss_ts)
    } else {
      cat("\nNo missing timestamps.\n")
    }

    cat("\n# Duplicate timestamps: ", n.dupl_ts, "\n")
    if (n.dupl_ts > 0) {
      print(dupl_ts)
    } else {
      cat("\nNo duplicate timestamps.\n")
    }

    cat("\n# Missing Values: ", n.miss_val, "\n")
    if (n.miss_val > 0) {
      print(miss_val)
    } else {
      cat("\nNo missing values.\n")
    }

    cat("\n# Outliers: ", n.out_val, "\n")
    if (n.out_val > 0) {
      print(out_val)
    } else {
      cat("\nNo outliers.\n")
    }

  }


  app <- shinyApp(
    ui = fluidPage(
      # titlePanel("Interactive Plot"),
      fluidRow(
        column(9, plotOutput("plt")),
        column(
          3,
          wellPanel(
            sliderInput(
              inputId = "frame",
              label = "Frame",
              min = 1,
              max = last_state,
              value = 1,
              step = 1,
              animate = TRUE,
            ),
            actionButton("left", label = "Prev", icon = icon("backward")),
            actionButton("right", label = "Next", icon = icon("forward")),
          ),
        )
      ),
      fluidRow(
        column(6, verbatimTextOutput("frameReport")),
        column(6, verbatimTextOutput("globalReport")),
      )
    ),

    server = function(input, output) {
      observeEvent(input$left, {
        updateSliderInput(inputId = "frame", value = input$frame - 1)
      })
      observeEvent(input$right, {
        updateSliderInput(inputId = "frame", value = input$frame + 1)
      })

      plot_l <- reactive({
        # pdf_l %>% filter(state == input$frame)
        pdf_l[state == input$frame]
      })

      plot_p <- reactive({
        dummy_data[, "state" := input$frame]
        setcolorder(dummy_data, c("time", "X", "value", "state", "color", "shape"))
        rbindlist(list(pdf_p[state == input$frame], dummy_data))
      })



      output$plt <- renderPlot(gen.frameplot(plot_l = plot_l(), plot_p = plot_p()))

      output$frameReport <- renderPrint(gen.framereport(obj = obj, plot_l = plot_l(), plot_p = plot_p()))

      output$globalReport <- renderPrint(gen.report(obj))

    },
  )
  # runApp(app, launch.browser = .rs.invokeShinyPaneViewer)
  app
}
