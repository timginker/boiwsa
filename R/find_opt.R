#' Find optimal number of fourier variables
#'
#' Searches through the model space to identify the best number of trigonometric variables, with the lowest AIC, AICc or BIC value.
#'
#' @importFrom stats supsmu lm AIC BIC
#' @importFrom MuMIn AICc
#' @import lubridate
#'
#' @param x Numeric vector. Time series to seasonally adjust
#' @param dates a vector of class "Date", containing the data dates
#' @param H (optional) Matrix with holiday and trading day variables
#' @param AO (optional) Matrix with additive outlier variables
#' @param method Decomposition method: "additive" or "multiplicative". By default uses the additive method
#' @param l.max maximal number of the monthly cycle variables to search for
#' @param k.max maximal number of the yearly cycle variables to search for
#' @param by step size in the search
#'
#' @return list with the optimal number of (yearly and monthly) fourier variables according to AIC, AICc and BIC
#' @export
#'
#' @examples
#'
#' data(gasoline.data)
#'
#' res=find_opt(x=gasoline.data$y,dates=gasoline.data$date)
#' print(res)
#'

find_opt=function(x,dates,H=NULL,AO=NULL,method="additive",l.max=24,k.max=42,by=6){


  if (method=="multiplicative") {
    x=log(x)
  }

  if(length(x)<2*(k.max+l.max+ifelse(is.null(H),0,ncol(H))+ifelse(is.null(AO),0,ncol(AO)))){

    stop("There is not enough observations to search through the given model space")
  }

  trend.init=stats::supsmu(1:length(x),x)$y

  y=x-trend.init

  aic0=matrix(NA,nrow=length(seq(by,k.max,by)),ncol=length(seq(by,l.max,by)))
  aicc0=matrix(NA,nrow=length(seq(by,k.max,by)),ncol=length(seq(by,l.max,by)))
  bic0=matrix(NA,nrow=length(seq(by,k.max,by)),ncol=length(seq(by,l.max,by)))


  # function to create fourier variables
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



  # searching through model space
  for (i in 1:length(seq(by,k.max,by))) {

    for (j in 1:length(seq(by,l.max,by))) {

      X=fourier_vars(k=(i-1)*by,l=(j-1)*by,dates)

      X=cbind(X,H,AO)

      if(is.null(X)){
        m=stats::lm(y~-1)
      }else{m=stats::lm(y~X-1)}




      aic0[i,j]=stats::AIC(m)
      aicc0[i,j]=MuMIn::AICc(m)
      bic0[i,j]=stats::BIC(m)

    }


  }


  opt.aic=(which(aic0 == min(aic0), arr.ind = TRUE)-1)*by
  opt.aicc=(which(aicc0 == min(aicc0), arr.ind = TRUE)-1)*by
  opt.bic=(which(bic0 == min(bic0), arr.ind = TRUE)-1)*by

  return(list(opt.aic=opt.aic,opt.aicc=opt.aicc,opt.bic=opt.bic))

}
