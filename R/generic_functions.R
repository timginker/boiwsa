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
