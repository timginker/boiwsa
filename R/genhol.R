#' Generate a moving-holiday regressor for weekly data
#'
#' Generates moving-holiday regressors for weekly data based on supplied holiday
#' occurrence dates using the Easter formula described in
#' Table 2 of Findley et al. (1998). The function can be used to construct
#' regressors for U.S. holidays such as Easter, Labor Day, and Thanksgiving, as
#' well as for Israeli holidays such as Rosh Hashanah and Pesach. The resulting
#' weekly holiday regressors are calendar-centered to avoid bias.
#'
#'
#' @import lubridate
#' @import dplyr
#' @importFrom tidyr fill
#' @importFrom rlang .data
#'
#' @param dates A vector of class \code{"Date"} corresponding to the weekly
#'   observation dates.
#' @param holiday.dates A vector of class \code{"Date"} giving the holiday
#'   occurrence dates (e.g., Easter, Labor Day, Thanksgiving, Rosh Hashanah,
#'   Pesach). Dates outside the range of \code{dates} are ignored.
#' @param start Integer. Number of days before each holiday date to include in the
#'   moving-holiday window. Negative values may be used to shift the start of the
#'   window to dates after the holiday.
#' @param end Integer. Number of days after each holiday date to include in the
#'   moving-holiday window. Negative values may be used to shift the end of the
#'   window to dates before the holiday.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{date}{Weekly dates corresponding to \code{dates}.}
#'   \item{moving_holiday}{Calendar-centered moving-holiday regressor at weekly
#'   frequency.}
#' }
#' The returned object can be merged into a matrix of holiday or trading-day
#' regressors supplied to \code{boiwsa()} via the \code{H} argument.
#'
#' @references Findley, D.F., Monsell, B.C., Bell, W.R., Otto, M.C. and B.C Chen (1998). New capabilities and methods of the X-12-ARIMA seasonal-adjustment program. Journal of Business & Economic Statistics, 16(2), pp.127-152.
#' @export
#' @examples
#'
#' # Moving-holiday regressor for Israeli Rosh Hashanah
#' data(gasoline.data)
#' data(holiday_dates_il) # dates of Israeli Rosh Hashanah and Pesach
#' movehol=genhol(gasoline.data$date,holiday.dates = holiday_dates_il$rosh)
#'
#'
genhol=function(dates,holiday.dates,start=7,end=7){

  start.date=min(dates)-lubridate::days(6)
  end.date=max(dates)

  df0=data.frame(date=seq.Date(from=start.date,to=end.date,by="day"),daily=seq.Date(from=start.date,to=end.date,by="day"))
  df1=data.frame(date=dates,weekly=dates)

  df2=merge(df0,df1,by="date",all = T)

  df2%>%tidyr::fill("weekly",.direction = "up")->df2

  df2$hag=0

  for (i in 1:length(holiday.dates)) {

    hdate=holiday.dates[i]

    df2[(df2$date<=hdate+lubridate::days(end))&(df2$date>=hdate-lubridate::days(start)),"hag"]=1
  }



  df2%>%
    dplyr::select("weekly","hag")%>%
    dplyr::group_by(.data$weekly)%>%
    dplyr::summarise(t=sum(.data$hag))->df3


  df3[df3$t!=0,"t"]=df3[df3$t!=0,"t"]/(start+end+1)

  m.t=df3%>%
    dplyr::filter(t!=0)%>%
    dplyr::select(t)%>%
    dplyr::summarise(across(everything(), mean))%>%
    as.numeric()

  df3[df3$t!=0,"t"]=df3[df3$t!=0,"t"]-m.t

  colnames(df3)=c("date","moving_holiday")


  return(df3)

}
