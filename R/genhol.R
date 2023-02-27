#' Generate Holiday Regression Variables
#'
#'
#'
#'
#' @import lubridate
#' @import dplyr
#' @importFrom tidyr fill
#'
#' @param dates a vector of class "Date", containing the data dates
#' @param holiday.dates a vector of class "Date", containing the occurrences of the holiday. It can be generated with as.Date().
#' @param start integer, shifts the start point of the holiday. Use negative values if start is before the specified date.
#' @param end integer, shifts end point of the holiday. Use negative values if end is before the specified date.
#'
#' @return an matrix with holiday variables that can be used as a user defined variable in boiwsa().
#' @export
#' @examples
#'
#' # Creating moving holiday variable for Israeli Rosh Hashanah
#' data(gasoline.data)
#' movehol=genhol(gasoline.data$date,holiday.dates = holiday_dates_il$rosh)
#'
#'
genhol=function(dates,holiday.dates,start=7,end=7){

  start.date=min(dates)-lubridate::days(6)
  end.date=max(dates)

  df0=data.frame(date=seq.Date(from=start.date,to=end.date,by="day"),daily=seq.Date(from=start.date,to=end.date,by="day"))
  df1=data.frame(date=dates,weekly=dates)

  df2=merge(df0,df1,by="date",all = T)

  df2%>%tidyr::fill(weekly,.direction = "up")->df2

  df2%>%dplyr::mutate(hag=0)->df2

  for (i in 1:length(holiday.dates)) {

    hdate=holiday.dates[i]

    df2[(df2$date<=hdate+lubridate::days(end))&(df2$date>=hdate-lubridate::days(start)),"hag"]=1
  }



  df2%>%
    dplyr::select(weekly,hag)%>%
    dplyr::group_by(weekly)%>%
    dplyr::summarise(t=sum(hag))->df3


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
