#' Generate simple trading day variable
#'
#' @param dates a vector of class "Date", containing the data dates
#' @param df.td dataframe with working days. See, for example, dates_il
#'
#' @return matrix with trading day variables
#' @export
#'
simple_td <- function(dates,df.td) {

  start.date=min(dates)-lubridate::days(6)
  end.date=max(dates)

  df0=data.frame(date=seq.Date(from=start.date,to=end.date,by="day"),daily=seq.Date(from=start.date,to=end.date,by="day"))
  df1=data.frame(date=dates,weekly=dates)

  df2=merge(df0,df1,by="date",all = T)

  df2%>%
    tidyr::fill(weekly,.direction = "up")->df2


  df3=merge(df2,df.td,by="date",all=T)%>%
    dplyr::filter(date>=start.date,date<=end.date)


  df3$t=(df3$ISR_WORKING_DAY_PART==1)*1



  df3%>%
    dplyr::select(weekly,t)%>%
    dplyr::group_by(weekly)%>%
    dplyr::summarise(t=sum(t))->df3


  m.t=df3%>%
    dplyr::select(t)%>%
    dplyr::summarise(across(everything(), mean))%>%as.numeric()

  df3$t=df3$t-m.t

  colnames(df3)=c("date","td")

  return(df3)
}
