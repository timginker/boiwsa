#' Summary function
#'
#' S3 method for objects of class "boiwsa". Prints the regression summary output.
#'
#' @importFrom stats summary.lm
#'
#' @param object An object of class \code{boiwsa}.
#' @param ... Additional arguments (currently not used).
#' @export
summary.boiwsa=function(object,...){

  summary.lm(object$m)


}

#' Generic summary function
#'
#' This is the generic summary function.
#'
#' @param object An object to summarize.
#' @param ... Additional arguments (currently not used).
#' @export
summary <- function(object, ...) {
  UseMethod("summary")
}

#' Print method for boiwsa objects
#'
#' S3 method for objects of class \code{boiwsa}. Prints a short model summary
#' including the number of trigonometric terms and the position of outliers.
#'
#' @param x Result of \code{boiwsa}.
#' @param ... Additional arguments (currently not used).
#' @export
print.boiwsa <- function(x, ...) {
  cat("\n", 'number of yearly cycle variables: ', x$my.k_l[1], "\n",
      'number of monthly cycle variables: ', x$my.k_l[2], "\n",
      'list of additive outliers: ', as.character(x$ao.list))
}


#' Generic print function
#'
#' This is the generic print function.
#'
#' @param x An object to print.
#' @param ... Additional arguments (currently not used).
#' @export
print <- function(x, ...) {
  UseMethod("print")
}


#' Plot
#'
#' S3 method for objects of class "boiwsa".
#' Produces a ggplot object of seasonally decomposed time series.
#'
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#'
#' @param x  Result of boiwsa
#' @param ...  Additional arguments (currently not used).
#'
#' @export
#'
plot.boiwsa=function(x,...){

  if(!is.null(x$sa)){

  # plot of original and seasonally adjusted series
  plt1 <- ggplot2::ggplot() +
    ggplot2::ggtitle("Original (blue) and Seasonally adjusted (green)") +
    ggplot2::geom_line(ggplot2::aes(x = x$dates, y = x$x, color = "orig")) +
    ggplot2::geom_line(ggplot2::aes(x = x$dates, y = x$sa, color = "sa")) +
    ggplot2::theme_bw() +
    ggplot2::xlab(" ") +
    ggplot2::ylab("") +  # Removed empty space in y-axis label
    ggplot2::scale_color_manual(name = "",
                       values = c("orig" = 'royalblue', "sa" = "green"),
                       labels = c("Original", "Seasonally adjusted")) +
    ggplot2::theme(legend.position = "None",
          legend.text = ggplot2::element_text(size = 10))

  # Plot of seasonal factors
  sf <- ggplot2::ggplot() +
    ggplot2::ggtitle("Seasonal") +
    ggplot2::geom_line(ggplot2::aes(x = x$dates, y = x$seasonal.factors, color = "sf")) +
    ggplot2::xlab(" ") +
    ggplot2::ylab("") +  # Removed empty space in y-axis label
    ggplot2::scale_color_manual(name = "",
                       values = c("sf" = 'royalblue'),
                       labels = c("Seasonal Factors")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
          legend.position = "None")

  # Plot of the trend component
  tr1 <- ggplot2::ggplot() +
    ggplot2::ggtitle("Trend") +
    ggplot2::geom_line(ggplot2::aes(x = x$dates, y = x$trend, color = "tr")) +
    ggplot2::xlab(" ") +
    ggplot2::ylab("") +  # Removed empty space in y-axis label
    ggplot2::scale_color_manual(name = "",
                       values = c("tr" = 'blue'),
                       labels = c("Trend")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
          legend.position = "None")

  # Arrange the plots in a grid
  return(gridExtra::grid.arrange(plt1, tr1, sf, nrow = 3))}else{

    message("Series should not be a candidate for seasonal adjustment because automatic selection found k=l=0")

  }

}


#' Predict
#'
#' S3 method for 'boiwsa' class. Returns forecasts and other information using a combination of nonseasonal
#' auto.arima and estimates from boiwsa.
#'
#' @param object An object of class \code{boiwsa}.
#' @param ... Additional arguments:
#'   \itemize{
#'     \item \code{n.ahead}: Number of periods for forecasting (required).
#'     \item \code{level}: Confidence level for prediction intervals. By default is set to c(80, 95).
#'     \item \code{new_H}: Matrix with future holiday- and trading day factors.
#'     \item \code{arima.options}: List of \code{forecast::Arima} arguments for custom modeling.
#'   }
#'
#' @return A list containing the forecast values and ARIMA fit.
#'
#' @export
#' @import forecast
#' @importFrom lubridate days
predict.boiwsa <- function(object, ...) {
  # Extract additional arguments
  args <- list(...)

  # Required argument
  if (!"n.ahead" %in% names(args)) {
    stop("Argument 'n.ahead' is required.")
  }
  n.ahead <- args$n.ahead

  # Optional arguments with defaults
  level <- if ("level" %in% names(args)) args$level else c(80, 95)
  new_H <- args$new_H
  arima.options <- args$arima.options

  # Fitting auto.arima to seasonally and outlier-adjusted variables
  if (length(object$ao.list) > 0) {
    y_est <- object$sa - object$out.factors
  } else {
    y_est <- object$sa
  }

  if (is.null(arima.options)) {
    fit <- forecast::auto.arima(y_est, seasonal = FALSE)
  } else {
    fit <- do.call(forecast::Arima, c(list(y = y_est), arima.options))
  }

  # Forecasting 'sa' series n.ahead periods forward
  fct <- forecast::forecast(fit, h = n.ahead, level = level)

  # Generating new dates
  new_dates <- seq.Date(
    from = as.Date(object$dates[length(object$dates)]) + lubridate::days(7),
    by = "weeks",
    length.out = n.ahead
  )

  # Creating Fourier variables
  seas_vars_fct <- boiwsa::fourier_vars(
    k = object$my.k_l[1],
    l = object$my.k_l[2],
    dates = new_dates
  )

  # Forecasting seasonal factors
  seas_factors_fct <- as.matrix(seas_vars_fct) %*% as.matrix(object$beta[1:(sum(object$my.k_l) * 2)])

  # Adjusting for additional factors if provided
  if (is.null(new_H)) {
    point_fct <- as.numeric(fct$mean) + seas_factors_fct
  } else {
    add_factors <- as.matrix(new_H) %*% as.matrix(
      object$beta[(sum(object$my.k_l) * 2 + 1):(sum(object$my.k_l) * 2 + ncol(new_H))]
    )
    point_fct <- as.numeric(fct$mean) + seas_factors_fct + add_factors
  }

  # Calculating confidence interval bounds
  bound_L1 <- point_fct - (fct$mean - fct$lower[, 1])
  bound_L2 <- point_fct - (fct$mean - fct$lower[, 2])
  bound_U1 <- point_fct + (fct$upper[, 1] - fct$mean)
  bound_U2 <- point_fct + (fct$upper[, 2] - fct$mean)

  # Creating the forecast data frame
  fct_fin <- data.frame(
    dates = new_dates,
    mean = point_fct,
    lower1 = bound_L1,
    lower2 = bound_L2,
    upper1 = bound_U1,
    upper2 = bound_U2
  )

  # Renaming columns for clarity
  colnames(fct_fin)[3] <- paste0("lower ", level[1], "%")
  colnames(fct_fin)[4] <- paste0("lower ", level[2], "%")
  colnames(fct_fin)[5] <- paste0("upper ", level[1], "%")
  colnames(fct_fin)[6] <- paste0("upper ", level[2], "%")

  # Returning the results
  return(list(forecast = fct_fin, fit = fit))
}

#' Visualize seasonal patterns in weekly data
#'
#' Plot detrended observations by week within the month and ISO week within
#' the year. For a boiwsa object, use its seasonally adjusted series.
#'
#' @param dates Observation dates (a Date vector, POSIXt vector, or character
#'   vector convertible to Date), or an object of class boiwsa.
#' @param y Numeric vector of observations. Omit when dates is a boiwsa object.
#' @param ylab Vertical axis label. NULL selects a label based on the input.
#' @param base_size Base font size in points; must be greater than two.
#'
#' @details
#' Observations with missing dates or non-finite values are removed, and the
#' remaining observations are sorted by date. Duplicate dates are not allowed.
#' A trend estimated by stats::supsmu() is subtracted from the series, using
#' elapsed calendar time as the smoothing coordinate.
#'
#' Within-month groups represent days 1--7, 8--14, 15--21, 22--28, and 29--31
#' of the observation date. Within-year groups use ISO weeks 1--53. The upper
#' axis shows approximate month positions based on the middle of each month
#' in 2025. Outlier points are hidden but remain in the boxplot calculations.
#'
#' This is a descriptive diagnostic, not a formal test for residual
#' seasonality. Moving holidays need separate assessment.
#'
#' @return A patchwork object containing two ggplot panels.
#' @importFrom rlang .data
#' @import patchwork
#' @export
#' @examples
#' plot_weekly_patterns(gasoline.data$date, gasoline.data$y)
#' \dontrun{
#' res <- boiwsa(x = gasoline.data$y, dates = gasoline.data$date)
#' plot_weekly_patterns(res)
#' }
plot_weekly_patterns <- function(dates, y = NULL, ylab = NULL,
                                 base_size = 12) {
  adjusted <- inherits(dates, "boiwsa")
  if (adjusted) {
    if (!is.null(y)) {
      stop("Omit 'y' when supplying a boiwsa object.", call. = FALSE)
    }
    y <- dates$sa
    dates <- dates$dates
    if (is.null(y) || is.null(dates)) {
      stop("The boiwsa object must contain 'sa' and 'dates'.", call. = FALSE)
    }
  }
  if (!is.numeric(y) || !is.null(dim(y))) {
    stop("'y' must be a numeric vector.", call. = FALSE)
  }
  if (!(inherits(dates, "Date") || inherits(dates, "POSIXt") ||
        is.character(dates))) {
    stop("'dates' must be Date, POSIXt, or character dates.", call. = FALSE)
  }
  dates <- tryCatch(as.Date(dates), error = function(e) {
    stop("'dates' could not be converted to Date.", call. = FALSE)
  })
  if (length(dates) != length(y)) {
    stop("'dates' and 'y' must have the same length.", call. = FALSE)
  }
  if (!is.numeric(base_size) || length(base_size) != 1L ||
      !is.finite(base_size) || base_size <= 2) {
    stop("'base_size' must be a finite number greater than two.", call. = FALSE)
  }
  if (is.null(ylab)) {
    ylab <- if (adjusted) "Detrended seasonally adjusted series" else
      "Detrended series"
  }

  keep <- is.finite(as.numeric(dates)) & is.finite(y)
  if (any(!keep)) {
    warning(sum(!keep), " observation(s) with invalid dates or values removed.",
            call. = FALSE)
  }
  df <- data.frame(date = dates[keep], y = as.numeric(y[keep]))
  df <- df[order(df$date), , drop = FALSE]
  if (nrow(df) < 5L) {
    stop("At least five valid observations are required.", call. = FALSE)
  }
  if (anyDuplicated(df$date)) {
    stop("Observation dates must be unique.", call. = FALSE)
  }

  time <- as.numeric(df$date - min(df$date))
  df$detrended <- df$y - stats::supsmu(time, df$y)$y
  df$wn <- ceiling(as.integer(format(df$date, "%d")) / 7)
  df$wy <- as.integer(format(df$date, "%V"))
  month_dates <- as.Date(sprintf("2025-%02d-15", 1:12))
  month_weeks <- as.integer(format(month_dates, "%V"))

  journal_theme <- ggplot2::theme_classic(
    base_size = base_size, base_family = "serif"
  ) + ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = base_size + 1, face = "plain",
      margin = ggplot2::margin(b = 12)
    ),
    axis.title = ggplot2::element_text(size = base_size),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 9)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 9)),
    axis.text = ggplot2::element_text(color = "black", size = base_size - 1),
    axis.line = ggplot2::element_line(linewidth = 0.35),
    axis.ticks = ggplot2::element_line(linewidth = 0.35),
    axis.ticks.x.top = ggplot2::element_blank(),
    axis.line.x.top = ggplot2::element_blank(),
    axis.text.x.top = ggplot2::element_text(
      size = base_size - 2, margin = ggplot2::margin(b = 6)
    ),
    plot.margin = ggplot2::margin(10, 12, 10, 10)
  )

  panel <- function(variable, title, xlab) {
    ggplot2::ggplot(df, ggplot2::aes(
      x = .data[[variable]], y = .data$detrended, group = .data[[variable]]
    )) +
      ggplot2::geom_hline(
        yintercept = 0, color = "grey65", linewidth = 0.35,
        linetype = "dashed"
      ) +
      ggplot2::geom_boxplot(
        width = 0.65, fill = "grey90", color = "grey20",
        linewidth = 0.35, outlier.shape = NA
      ) +
      ggplot2::labs(title = title, x = xlab, y = ylab) + journal_theme
  }

  p_month <- panel("wn", "A. Within-month pattern", "Week number in a month") +
    ggplot2::scale_x_continuous(breaks = 1:5, limits = c(0.5, 5.5))
  p_year <- panel("wy", "B. Within-year pattern", "Week number in a year") +
    ggplot2::scale_x_continuous(
      breaks = c(1, seq(4, 52, 4)),
      limits = c(0.5, max(52, df$wy) + 0.5),
      sec.axis = ggplot2::dup_axis(
        breaks = month_weeks, labels = month.abb, name = NULL
      )
    ) + ggplot2::labs(y = NULL)

  # Use the same quartile/whisker convention as the displayed boxplots.
  whiskers <- function(p) {
    boxes <- ggplot2::ggplot_build(p)$data[[2L]]
    c(boxes$ymin, boxes$ymax)
  }
  limits <- range(c(0, whiskers(p_month), whiskers(p_year)))
  padding <- max(diff(limits) * 0.08, 1e-8)
  limits <- limits + c(-padding, padding)
  p_month <- p_month + ggplot2::coord_cartesian(ylim = limits, expand = FALSE)
  p_year <- p_year + ggplot2::coord_cartesian(ylim = limits, expand = FALSE)
  patchwork::wrap_plots(p_month, p_year, nrow = 1)
}
