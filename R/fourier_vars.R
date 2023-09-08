#' Create fourier predictors
#'
#' Creates sine and cosine variables to capture intramonthly and intrayearly cycles.
#'
#' @importFrom Hmisc yearDays
#' @importFrom lubridate days_in_month day yday
#'
#' @param k Number of yearly cycle fourier terms
#' @param l Number of monthly cycle fourier terms
#' @param dates Vector of dates in a date format
#'
#' @return Matrix with fourier variables
#' @export
#'
#' @examples
#'
#' # create a vector of dates
#' dates=seq.Date(from=as.Date("2023-01-02"),by="weeks",length.out = 100)
#' # Create a matrix with 20 yearly and 6 monthly pairs of sine and cosine variables
#' X=fourier_vars(k=20,l=6,dates=dates)
#'
fourier_vars=function(k=1,l=1,dates){


  if (l>0) {

    X=matrix(NA_real_,nrow = length(dates),ncol=2*l)


    Nm=as.numeric(lubridate::days_in_month(dates)) # number of days in a moth
    mt=lubridate::day(dates) # day in a month

    for (i in 1:l) {

      X[,i]=sin(2*pi*i*mt/Nm)

      X[,l+i]=cos(2*pi*i*mt/Nm)

    }


    Xm=X

    colnames(Xm)=c(paste0("S(",1:l,"/Nm",")"),paste0("C(",1:l,"/Nm",")"))
  }else{

    Xm=NULL
  }




  if (k>0) {
    # creating yearly cycle variables

    yt=lubridate::yday(dates)
    Ny=Hmisc::yearDays(dates)



    X=matrix(NA_real_,nrow = length(dates),ncol=2*k)



    for (i in 1:k) {

      X[,i]=sin(2*pi*i*yt/Ny)

      X[,k+i]=cos(2*pi*i*yt/Ny)

    }

    colnames(X)=c(paste0("S(",1:k,"/Ny",")"),paste0("C(",1:k,"/Ny",")"))

  }else{

    X=NULL
  }





  cbind(X,Xm)->X



  return(X)


}
