#' Seasonal adjustment of weekly data
#'
#' @import lubridate
#' @import zoo
#' @importFrom Hmisc yearDays
#' @import MuMIn
#' @import tidyverse
#'
#' @param x Input time series as a numeric vector
#' @param dates a vector of class "Date", containing the data dates
#' @param r Defines the rate of decay of the weights. Should be between zero and one. By default is set to 0.8.
#' @param auto.ao.seacrh Boolean. Search for additive outliers
#' @param ao.list Vector with user specified additive outliers in a date format
#' @param my.k_l Numeric vector defining the number of yearly and monthly trigonometric variables. If NULL, is found automatically using the information criteria
#' @param H Matrix with holiday- and trading day factors
#' @param ic Information criterion used in the automatic search for the number of trigonometric regressors. There are thee options: aic, aicc and bic. By default uses aicc
#' @param method Decomposition type: additive or multiplicative
#'
#' @return {sa} {Seasonally adjusted series}
#' @return {my.k_l} {Number of trigonometric variables used to model the seasonal pattern}
#' @return {sf} {Estimated seasonal effects}
#' @return {hol.factors} {Estimated holiday effects}
#' @return {out.factors} {Estimated outlier effects}
#' @return {beta} {Regression coefficients for the last year}
#' @return {m} {lm object. Unweighted OLS regression on the full sample}
#' @author Tim Ginker
#' @export
#' @examples
#'  # Seasonal adjustment of weekly US gasoline production
#'
#'  data("gasoline.data")
#'  res=boiwsa(x=gasoline.data$y,dates=gasoline.data$date) # Seasonal adjustment using automatic model selection

boiwsa=function(x,
                dates,
                r=0.8,
                auto.ao.seacrh=T,
                out.tolerance=3.8,
                ao.list=NULL,
                my.k_l=NULL,
                H=NULL,
                ic="aicc",
                method="additive"){
  ############
  # Arguments:
  ############
  # x - time series to seasonally adjust. object of class vector
  # dates - vector of dates in a date format
  # r - weight decay parameter
  # ao.list - vector of outlier dates in a date format
  # my.k_l - vector with two numbers for fourier variables, for yearly and monthly seasonality, respectively
  # H - matrix of holiday and trading day variables
  ########
  # Value:
  ########
  # sa - seasonally adjusted series
  # my.k_l
  # sf - seasonal factors
  # hol.factors - holiday and trading day factors
  # out.factors - outlier factors
  # beta - vector of regression coefficients for the last year
  # m - lm object



  #----------------------------------------------#
  my_ao=function(dates,out.list) {

    # checking that the dates in out.list are in the data, and removing them if not

    out.list=out.list[out.list%in%dates]

    if (length(out.list)>0) {

      AO=matrix(0,nrow = length(dates), ncol=length(out.list))

      for (i in 1:ncol(AO)) {

        AO[dates==out.list[i],i]=1

      }

      colnames(AO)=paste0("AO ",lubridate::as_date(out.list))

    }else{AO=NULL}




    return(AO)

  }


  #----------------------------------------------#


  find_opt=function(y,dates,H=NULL,AO=NULL){

    # y - detrended dependent variable
    # H - holiday and trading day effects (as matrix)
    # AO - additive outlier variables (as matrix)


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


  #----------------------------------------------#

  fourier_vars=function(k=1,l=1,dates){

    # packages: Hmisc, lubridate


    # k- number of yearly cycle fourier terms
    # l - number of monthly cycle fourier terms
    # dates - a vector of dates in a date format

    # creating monthly cycle variables

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


  #----------------------------------------------#

  find_outliers=function(y,dates,out.tolerance=out.tolerance,my.AO.list=NULL,H=NULL,my.k_l=NULL){

    # y -detrended variable
    # out.tolerance - t-stat threshold
    # predefined additive outlier variables
    # my.k_l - number of yearly and monthly fourier variables


    if (is.null(my.k_l)) {

      if (is.null(my.AO.list)) {
        AO=NULL
      }else{

        AO=my_ao(dates=dates,out.list =my.AO.list )

      }

      opt=find_opt(y = y, dates = dates,H = H, AO = AO)

      my.k_l=opt$opt.aicc

    }

    X=fourier_vars(k=my.k_l[1],l=my.k_l[2],dates = dates)


    Xs=cbind(X,H,AO)

    err=y-Xs%*%solve(t(Xs)%*%Xs)%*%t(Xs)%*%y

    sig_R=1.49*median(abs(err))



    f.sel.pos=NULL

    out.search.points=(1:length(dates))[!dates%in%my.AO.list]

    run=T

    while (run) {

      Ts=NULL

      for (t in out.search.points) {

        AOt=rep(0,length(dates))

        AOt[t]=1

        Xst=cbind(Xs,AOt)

        Tt=(solve(t(Xst)%*%Xst)%*%t(Xst)%*%y)[ncol(Xst)]/(diag(solve((t(Xst)%*%Xst))*sig_R^2)[ncol(Xst)]^0.5)

        Ts=c(Ts,abs(Tt))

      }


      if (max(Ts)>=out.tolerance) {

        AOt=rep(0,length(dates))

        AOt[out.search.points[which.max(Ts)]]=1

        f.sel.pos=c(f.sel.pos,out.search.points[which.max(Ts)])

        out.search.points=out.search.points[-which.max(Ts)]



        Xs=cbind(Xs,AOt)

      }





      if (max(Ts)<out.tolerance) {
        run=F
      }


    }


    # Backward deletion


    if(length(f.sel.pos)>0){run=T}



    while (run) {


      f.sel.ao.dates=dates[f.sel.pos]


      AObd=my_ao(dates=dates,out.list=c(my.AO.list,f.sel.ao.dates))


      Xst=cbind(X,H,AObd)

      err=y-Xst%*%solve(t(Xst)%*%Xst)%*%t(Xst)%*%y

      sig_R=1.49*median(abs(err))

      Tt=abs((solve(t(Xst)%*%Xst)%*%t(Xst)%*%y)/(diag(solve((t(Xst)%*%Xst))*sig_R^2)^0.5))[(ncol(Xst)-length(f.sel.ao.dates)+1):ncol(Xst)]


      if(min(Tt)<out.tolerance){

        f.sel.ao.dates=f.sel.ao.dates[-which.min(Tt)]

      }else{

        run=F
      }

      if(length(f.sel.ao.dates)==0){

        run=F
      }


    }


    f.sel.ao.dates=f.sel.ao.dates[order(f.sel.ao.dates)]

    return(list(ao=f.sel.ao.dates,my.k_l=my.k_l))



  }

  #----------------------------------------------#


  if (method=="multiplicative") {
    x=log(x)
  }


  #############################
  # First RUN
  #############################


  # computing initial trend estimate with Friedman's SuperSmoother

  trend.init=stats::supsmu(1:length(x),x)$y

  y=x-trend.init

  # looking for additive outliers

  if(auto.ao.seacrh){

    auto.ao=find_outliers(y=y,dates=dates,H = H,my.AO.list = ao.list)

  }else{

    auto.ao=NULL

  }

  if (length(auto.ao$ao)>0) {

    ao.list=c(ao.list,auto.ao$ao)
  }



  # First run of the SA procedure


  # creating outlier variables

  if (is.null(ao.list)) {
    AO=NULL

    nc.ao=0

  }else{

    AO=my_ao(dates=dates,out.list = ao.list)
    nc.ao=ncol(AO)
  }


  if (is.null(my.k_l)) {

    opt=find_opt(y = y, dates = dates,H = H, AO = AO)

    if (ic=="aicc") {
      my.k_l=opt$opt.aicc
    }

    if (ic=="aic") {
      my.k_l=opt$opt.aic
    }

    if (ic=="bic") {
      my.k_l=opt$opt.bic
    }


  }

  X=fourier_vars(k=my.k_l[1],l=my.k_l[2],dates = dates)

  Xs=cbind(X,H,AO)

  # Creating weights

  my.years=unique(lubridate::year(dates))


  Wi=array(0,dim=c(length(dates),length(dates),length(my.years)))

  w.i=rep(0,length(my.years))

  for (i in 1:length(my.years)) {

    for (j in 1:length(my.years)) {

      w.i[j]=r^(abs(j-i))



    }

    w=NULL

    for (k in 1:length(my.years)) {

      w=c(w,rep(w.i[k],sum(year(dates)==my.years[k])))

    }



    diag(Wi[,,i])=w/sum(w)

  }

  #

  sf=rep(0,length(dates))
  hol.factors=rep(0,length(dates))
  out.factors=rep(0,length(dates))


  for (i in 1:length(my.years)) {

    beta=solve(t(Xs)%*%Wi[,,i]%*%Xs)%*%t(Xs)%*%Wi[,,i]%*%y

    n.i=(lubridate::year(dates)==my.years[i])

    sf[n.i]=(Xs[,1:(length(beta)-nc.ao)]%*%beta[1:(length(beta)-nc.ao)])[n.i]

    if (!is.null(H)) {

      hol.factors[n.i]=(Xs[,(ncol(X)+1):(ncol(X)+ncol(H))]%*%beta[(ncol(X)+1):(ncol(X)+ncol(H))])[n.i]

    }

    if(nc.ao>0){

      if (!is.null(H)) {

        out.factors[n.i]=(Xs[,(ncol(X)+ncol(H)+1):ncol(Xs)]%*%beta[(ncol(X)+ncol(H)+1):ncol(Xs)])[n.i]

      }else{

        out.factors[n.i]=(as.matrix(Xs[,(ncol(X)+1):ncol(Xs)])%*%as.matrix(beta[(ncol(X)+1):ncol(Xs)]))[n.i]

      }




    }else{

      out.factors=NULL
    }


  }


  if (!is.null(out.factors)) {
    seas.out.adj=x-sf-out.factors
  }else{

    seas.out.adj=x-sf

  }

  ####################
  # Second RUN
  ####################


  trend.init=supsmu(1:length(x),seas.out.adj)$y

  y=x-trend.init


  # creating outlier variables

  if (is.null(ao.list)) {
    AO=NULL

    nc.ao=0

  }else{

    AO=my_ao(dates=dates,out.list = ao.list)
    nc.ao=ncol(AO)
  }


  if (is.null(my.k_l)) {

    opt=find_opt(y = y, dates = dates,H = H, AO = AO)

    if (ic=="aicc") {
      my.k_l=opt$opt.aicc
    }

    if (ic=="aic") {
      my.k_l=opt$opt.aic
    }

    if (ic=="bic") {
      my.k_l=opt$opt.bic
    }


  }

  X=fourier_vars(k=my.k_l[1],l=my.k_l[2],dates = dates)

  Xs=cbind(X,H,AO)



  my.years=unique(lubridate::year(dates))


  Wi=array(0,dim=c(length(dates),length(dates),length(my.years)))

  w.i=rep(0,length(my.years))

  for (i in 1:length(my.years)) {

    for (j in 1:length(my.years)) {

      w.i[j]=r^(abs(j-i))



    }

    w=NULL

    for (k in 1:length(my.years)) {

      w=c(w,rep(w.i[k],sum(lubridate::year(dates)==my.years[k])))

    }



    diag(Wi[,,i])=w/sum(w)

  }


  sf=rep(0,length(dates))
  hol.factors=rep(0,length(dates))
  out.factors=rep(0,length(dates))


  for (i in 1:length(my.years)) {

    beta=solve(t(Xs)%*%Wi[,,i]%*%Xs)%*%t(Xs)%*%Wi[,,i]%*%y

    n.i=(lubridate::year(dates)==my.years[i])

    sf[n.i]=(Xs[,1:(length(beta)-nc.ao)]%*%beta[1:(length(beta)-nc.ao)])[n.i]

    if (!is.null(H)) {

      hol.factors[n.i]=(Xs[,(ncol(X)+1):(ncol(X)+ncol(H))]%*%beta[(ncol(X)+1):(ncol(X)+ncol(H))])[n.i]

    }

    if(nc.ao>0){

      if (!is.null(H)) {

        out.factors[n.i]=(Xs[,(ncol(X)+ncol(H)+1):ncol(Xs)]%*%beta[(ncol(X)+ncol(H)+1):ncol(Xs)])[n.i]

      }else{

        out.factors[n.i]=(as.matrix(Xs[,(ncol(X)+1):ncol(Xs)])%*%as.matrix(beta[(ncol(X)+1):ncol(Xs)]))[n.i]
      }




    }else{

      out.factors=NULL
    }


  }


  if (!is.null(out.factors)) {
    seas.out.adj=x-sf-out.factors
  }else{

    seas.out.adj=x-sf

  }


  trend.fin=supsmu(1:length(x),seas.out.adj)$y


  # computing final seasonal adjusted series

  sa=x-sf

  if(method=="multiplicative"){

    sa=exp(sa)

    trend.fin=exp(trend.fin)

    sf=exp(sf)


  }

  m=lm(y~Xs-1)

  #my.k_l=as.data.frame(my.k_l)

  #colnames(my.k_l)=c("yearly variables","monthly variables")




  return(list(sa=sa,
              my.k_l=my.k_l,
              seasonal.factors=sf,
              hol.factors=hol.factors,
              out.factors=out.factors,
              trend=trend.fin,
              beta=beta,
              m=m,
              x=x,
              ao.list=lubridate::as_date(ao.list)))





}
