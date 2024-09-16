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
