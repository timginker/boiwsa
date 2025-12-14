#' Create Fourier (trigonometric) regressors for weekly data
#'
#' Constructs sine and cosine regressors to capture seasonal variation at
#' intrayearly and intramonthly frequencies in weekly time series. The Fourier
#' terms are defined using the day-of-year and day-of-month corresponding to each
#' observation date, allowing the seasonal frequencies to adapt to varying month
#' and year lengths.
#'
#' @importFrom Hmisc yearDays
#' @importFrom lubridate days_in_month day yday
#'
#' @param k Integer. Number of yearly-cycle Fourier harmonics (pairs of sine and
#'   cosine terms) to include.
#' @param l Integer. Number of monthly-cycle Fourier harmonics (pairs of sine and
#'   cosine terms) to include.
#' @param dates A vector of class \code{"Date"} corresponding to the observation dates.
#'
#' @return A numeric matrix with \code{length(dates)} rows and \code{2 * (k + l)}
#' columns containing the Fourier regressors. Columns are ordered with yearly
#' terms first, followed by monthly terms. If both \code{k = 0} and \code{l = 0},
#' \code{NULL} is returned.
#'
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
