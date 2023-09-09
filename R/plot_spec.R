#' Original and SA data AR spectrum
#'
#' AR spectrum of the (detrended) original and seasonally adjusted data. Computed using [`stats::spec.ar()`] with order set to 60.
#'
#' @importFrom graphics abline legend lines
#' @importFrom stats spec.ar
#'
#' @param x boiwsa results
#'
#' @return AR spectrum plot
#' @export
#'
#' @examples
#' \dontrun{
#' # Seasonal adjustment of weekly US gasoline production
#' res=boiwsa(x=gasoline.data$y,dates=gasoline.data$date)
#' plot_spec(res)}
#'
plot_spec=function(x){


  spec0=spec.ar((x$x-x$trend),order = 60,plot = F,n.freq=500)
  spec1=spec.ar((x$sa-x$trend),order = 60,plot = F)

  #par(mfrow = c(1, 1))
  plot(spec1$freq,spec0$spec,type="l",ylab="",xlab="frequency",lwd=2,col="darkgrey",main="Original and SA data AR spectrum")
  lines(spec1$freq,spec1$spec,col="royalblue",lwd=1.5)
  abline(v=(1:2)/4.34, col="red",lwd=1.5,lty=2)
  abline(v=(1:3)/52.1775, col="black",lwd=1.5,lty=3)
  legend(
    "topright",
    legend = c("monthly peaks", "yearly peaks","original","SA"),
    lwd = c(2,2,2,2),
    lty=c(2,3,1,1),
    col = c("red", "black","darkgrey","royalblue"),
    bty = "n"
  )




}
