#' Create additive outlier variables
#'
#' Creates a matrix with additive outlier variables. Uses the original data dates and the user specified outlier dates.
#'
#' @param dates Vector of dates in a date format
#' @param out.list Vector of outlier dates in a date format
#'
#' @return AO matrix with outlier variables
#' @export
#'
#' @examples
#'
#' # create a sequence of dates
#' dates=seq.Date(from=as.Date("2023-01-02"),by="weeks",length.out = 100)
#' # create a vector of outlier dates
#' my_ao_dates=as.Date(c("2023-01-02","2023-01-03"))
#' # create a matrix of AO variables
#' my_ao(dates = dates,out.list = my_ao_dates)
#' # as you can see there is only one column corresponding to 2023-01-02,
#' # the second date is ignored because it is not present in the dates vector
#'
my_ao=function(dates,out.list) {

  # checking that the dates in out.list are in the data, and removing them if not

  out.list=out.list[out.list%in%dates]

  if (length(out.list)>0) {

    AO=matrix(0,nrow = length(dates), ncol=length(out.list))

    for (i in 1:ncol(AO)) {

      AO[dates==out.list[i],i]=1

    }

    colnames(AO)=paste0("AO ",out.list)

  }else{AO=NULL}




  return(AO)

}
