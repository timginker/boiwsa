#' Original and SA data AR spectrum
#'
#' AR spectrum of the (detrended) original and seasonally adjusted data. Computed using [`stats::spec.ar()`] with order set to 60.
#'
#' @importFrom graphics abline legend lines
#' @importFrom stats spec.ar
#' @import ggplot2
#'
#' @param x boiwsa results
#'
#' @return AR spectrum plot
#' @export
#'
#' @examples
#' \donttest{
#' # Not run
#' # Seasonal adjustment of weekly US gasoline production
#' res=boiwsa(x=gasoline.data$y,dates=gasoline.data$date)
#' plot_spec(res)}
#'
plot_spec=function(x){


  spec0=spec.ar((x$x-x$trend),order = 60,plot = F,n.freq=500)
  spec1=spec.ar((x$sa-x$trend),order = 60,plot = F)

  ggplot2::ggplot()+
    ggplot2::geom_line(aes(x=spec0$freq,y=spec0$spec,color="orig"))+
    ggplot2::geom_line(aes(x=spec0$freq,y=spec1$spec,color="sa"))+
    ggplot2::geom_vline(xintercept = 1:2/4.34,linetype="dashed")+
    ggplot2::geom_text(aes(x=1:2/4.34, label="\n Intra-monthly cycle peaks", y=0.5*max(spec0$spec)), colour="black", angle=90)+
    ggplot2::geom_vline(xintercept = (1:3)/52.1775,linetype="dashed")+
    ggplot2::geom_text(aes(x=3/52.1775, label="\n First three intra-yearly cycle peaks", y=0.5*max(spec0$spec)), colour="black", angle=90)+
    ggplot2::scale_color_manual(name = "",
                                values = c( "orig"='#31a354', "sa"="#3182bd"),
                                labels = c("Original", "Seasonally adjusted"))+
    ggplot2::theme_bw()+
    ggplot2::guides(y="none")+
    ggplot2::theme(legend.position="bottom")+
    ggplot2::theme(legend.text = element_text(size = 11))+
    ggplot2::ylab(" ")+
    ggplot2::xlab("Frequency")


}
