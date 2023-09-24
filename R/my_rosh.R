#' Internal function for a specific application
#'
#' Creates a dummy moving holiday variable for the weekly number of initial registrations at the Employment Service in Israel.
#'
#'
#' @import lubridate
#' @import dplyr
#' @importFrom tidyr fill
#' @importFrom rlang .data
#'
#' @param dates a vector of class "Date", containing the data dates
#' @param holiday.dates a vector of class "Date", containing the occurrences of the holiday. It can be generated with as.Date().
#' @param start -11 for rosh, 3 for pesach
#' @param end 12 for rosh, -1 for pesach
#'
#' @return rosh holiday variable
#' @export
#'
#' @examples
#'
#' # Creating moving holiday dummy variable for Israeli Rosh Hashanah
#' data(gasoline.data)
#' data(holiday_dates_il) # dates of Israeli Rosh Hashanah and Pesach
#' movehol=my_rosh(gasoline.data$date,holiday.dates = holiday_dates_il$rosh)
#'

my_rosh=function(dates,holiday.dates,start=-11,end=12){


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

  df3[df3$t>0,"t"]=1

  df3%>%
    dplyr::mutate(up=lag(t),up2=lag(t,2))->df3

  df3[is.na(df3)]=0

  df3[,c("t","up","up2")]=scale(df3[,c("t","up","up2")],scale=F)

  colnames(df3)=c("date","down","up","up2")

  return(df3)

}
