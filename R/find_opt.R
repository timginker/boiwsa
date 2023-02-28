#' Find optimal number of fourier variables
#'
#' Searches through the model space to identify the best number of trigonometric variables, with the lowest AIC, AICc or BIC value.
#'
#' @param x Numeric vector. Time series to seasonally adjust
#' @param dates a vector of class "Date", containing the data dates
#' @param H (optional) Matrix with holiday and trading day variables
#' @param AO (optional) Matrix with additive outlier variables
#' @param method Decomposition method: "additive" or "multiplicative". By default uses the additive method
#'
#' @return list with the optimal number of fourier terms according to AIC, AICc and BIC
#' @export
#'

find_opt=function(x,dates,H=NULL,AO=NULL,method="additive"){


  if (method=="multiplicative") {
    x=log(x)
  }

  trend.init=stats::supsmu(1:length(x),x)$y

  y=x-trend.init

  aic0=matrix(NA,nrow=length(seq(6,42,6)),ncol=length(seq(6,42,6)))
  aicc0=matrix(NA,nrow=length(seq(6,42,6)),ncol=length(seq(6,42,6)))
  bic0=matrix(NA,nrow=length(seq(6,42,6)),ncol=length(seq(6,42,6)))


  for (i in 1:length(seq(6,42,6))) {

    for (j in 1:length(seq(6,42,6))) {

      X=fourier_vars(k=(i-1)*6,l=(j-1)*6,dates)

      X=cbind(X,H,AO)

      if(is.null(X)){
        m=stats::lm(y~-1)
      }else{m=stats::lm(y~X-1)}




      aic0[i,j]=stats::AIC(m)
      aicc0[i,j]=MuMIn::AICc(m)
      bic0[i,j]=stats::BIC(m)

    }


  }


  opt.aic=(which(aic0 == min(aic0), arr.ind = TRUE)-1)*6
  opt.aicc=(which(aicc0 == min(aicc0), arr.ind = TRUE)-1)*6
  opt.bic=(which(bic0 == min(bic0), arr.ind = TRUE)-1)*6

  return(list(opt.aic=opt.aic,opt.aicc=opt.aicc,opt.bic=opt.bic))

}
