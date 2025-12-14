#' Generate a simple working-day trading-day regressor
#'
#' Constructs a weekly trading-day regressor by counting the number of full
#' working days within each weekly period and centering the resulting series by
#' subtracting its sample mean. Daily working-day information is supplied via
#' \code{df.td} and mapped to the weekly dates provided in \code{dates}.
#'
#' @import lubridate
#' @import dplyr
#' @importFrom tidyr fill
#' @importFrom rlang .data
#'
#' @param dates A vector of class \code{"Date"} corresponding to the weekly observation dates.
#' @param df.td A data frame containing daily working-day information with two columns:
#'   \code{date} (class \code{"Date"}) and \code{WORKING_DAY_PART}. Full working days
#'   should be coded as \code{1}; all other values are treated as non-working days.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{date}{Weekly dates corresponding to \code{dates}.}
#'   \item{td}{Centered weekly count of full working days.}
#' }
#' The returned object can be merged into a matrix of holiday/trading-day
#' regressors supplied to \code{boiwsa()} via the \code{H} argument.
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' data(dates_il)
#' data(gasoline.data)
#'
#' dates_il%>%
#'   dplyr::select(DATE_VALUE,ISR_WORKING_DAY_PART)%>%
#'   `colnames<-`(c("date","WORKING_DAY_PART"))%>%
#'   dplyr::mutate(date=as.Date(date))->df.td
#'
#' td=simple_td(dates = gasoline.data$date,df.td = df.td)
#'
#'
#'
#'
simple_td <- function(dates,df.td) {

  start.date=min(dates)-lubridate::days(6)
  end.date=max(dates)

  df0=data.frame(date=seq.Date(from=start.date,to=end.date,by="day"),daily=seq.Date(from=start.date,to=end.date,by="day"))
  df1=data.frame(date=dates,weekly=dates)

  df2=merge(df0,df1,by="date",all = T)

  df2%>%
    tidyr::fill("weekly",.direction = "up")->df2


  df3=merge(df2,df.td,by="date",all=T)%>%
    dplyr::filter(date>=start.date,date<=end.date)


  df3$t=(df3$WORKING_DAY_PART==1)*1



  df3%>%
    dplyr::select("weekly","t")%>%
    dplyr::group_by(.data$weekly)%>%
    dplyr::summarise(t=sum(t))->df3


  m.t=df3%>%
    dplyr::select(t)%>%
    dplyr::summarise(across(everything(), mean))%>%as.numeric()

  df3$t=df3$t-m.t

  colnames(df3)=c("date","td")

  return(df3)
}
