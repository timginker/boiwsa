#' Find optimal number of fourier variables
#'
#' @param y Numeric vector with a detrended dependent variable
#' @param dates Vector with dates in a date format
#' @param H Matrix with holiday and trading day variables
#' @param AO Matrix with additive outlier variables
#'
#' @return list with the optimal number of fourier terms according to AIC, AICc and BIC
#' @export
#'

find_opt=function(y,dates,H=NULL,AO=NULL){

  # y - detrended dependent variable
  # H - holiday and trading day effects (as matrix)
  # AO - additive outlier variables (as matrix)


  source("./Codes/fourier_vars.R")

  aic0=matrix(NA,nrow=length(seq(6,42,6)),ncol=length(seq(6,42,6)))
  aicc0=matrix(NA,nrow=length(seq(6,42,6)),ncol=length(seq(6,42,6)))
  bic0=matrix(NA,nrow=length(seq(6,42,6)),ncol=length(seq(6,42,6)))


  for (i in 1:length(seq(6,42,6))) {

    for (j in 1:length(seq(6,42,6))) {

      X=fourier_vars(k=(i-1)*6,l=(j-1)*6,dates)

      X=cbind(X,H,AO)

      if(is.null(X)){
        m=lm(y~-1)
      }else{m=lm(y~X-1)}




      aic0[i,j]=AIC(m)
      aicc0[i,j]=MuMIn::AICc(m)
      bic0[i,j]=BIC(m)

    }


  }


  opt.aic=(which(aic0 == min(aic0), arr.ind = TRUE)-1)*6 # optimal number of terms
  opt.aicc=(which(aicc0 == min(aicc0), arr.ind = TRUE)-1)*6
  opt.bic=(which(bic0 == min(bic0), arr.ind = TRUE)-1)*6

  return(list(opt.aic=opt.aic,opt.aicc=opt.aicc,opt.bic=opt.bic))

}
