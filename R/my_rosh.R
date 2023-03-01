#' Internal function for a specific series
#'
#' @param dates a vector of class "Date", containing the data dates
#' @param holiday.dates a vector of class "Date", containing the occurrences of the holiday. It can be generated with as.Date().
#' @param start -11 for rosh, 3 for pesach
#' @param end 12 for rosh, -1 for pesach
#'
#' @return rosh holiday variable
#' @export
#'

my_rosh=function(dates,holiday.dates,start=-11,end=12){


  start.date=min(dates)-lubridate::days(6)
  end.date=max(dates)

  df0=data.frame(date=seq.Date(from=start.date,to=end.date,by="day"),daily=seq.Date(from=start.date,to=end.date,by="day"))
  df1=data.frame(date=dates,weekly=dates)

  df2=merge(df0,df1,by="date",all = T)

  df2%>%tidyr::fill(weekly,.direction = "up")->df2

  df2%>%
    dplyr::mutate(hag=0)->df2

  for (i in 1:length(holiday.dates)) {

    hdate=holiday.dates[i]

    df2[(df2$date<=hdate+lubridate::days(end))&(df2$date>=hdate-lubridate::days(start)),"hag"]=1
  }



  df2%>%
    dplyr::select(weekly,hag)%>%
    dplyr::group_by(weekly)%>%
    dplyr::summarise(t=sum(hag))->df3

  df3[df3$t>0,"t"]=1

  df3%>%
    dplyr::mutate(up=lag(t),up2=lag(t,2))->df3

  df3[is.na(df3)]=0

  df3[,c("t","up","up2")]=scale(df3[,c("t","up","up2")],scale=F)

  colnames(df3)=c("date","down","up","up2")

  return(df3)

}
