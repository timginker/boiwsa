#' Create additive outlier indicator variables
#'
#' Constructs a matrix of additive outlier (AO) indicator variables based on a
#' set of user-specified outlier dates. For each outlier date that coincides with
#' an observation date, a binary indicator equal to one is created at the
#' corresponding position and zero elsewhere. Outlier dates not present in the
#' observation dates are silently ignored.
#'
#' @param dates A vector of class \code{"Date"} corresponding to the observation dates.
#' @param out.list A vector of class \code{"Date"} specifying candidate additive
#'   outlier dates.
#'
#' @return A numeric matrix with \code{length(dates)} rows and one column per
#' outlier date present in \code{dates}. Column names are of the form
#' \code{"AO <date>"}. If none of the supplied outlier dates coincide with
#' \code{dates}, \code{NULL} is returned.
#'
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
