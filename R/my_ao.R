#' Create additive outlier variables
#'
#' @param dates Vector of dates in a date format
#' @param out.list Vector of outlier dates in a date format
#'
#' @return AO matrix with outlier variables
#' @export
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
