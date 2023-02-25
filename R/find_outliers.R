#' Find additive outliers
#'
#' @param y Numeric vector with a detrended dependent variable
#' @param dates Vector with dates in a date format
#' @param out.tolerance t-stat threshold for outliers (see Findley et al., 1998)
#' @param my.AO.list Vector with user defined additive outlier variables
#' @param H Matrix with holiday and trading day variables
#' @param my.k_l Vector with the number of fourier terms to capture the yeraly and monthly cycle. If NULL, would perform automatic search
#'
#' @return my.k_l
#' @return ao list of AO dates
#' @export
#'

find_outliers=function(y,dates,out.tolerance=3.8,my.AO.list=NULL,H=NULL,my.k_l=NULL){

  if (is.null(my.k_l)) {

    if (is.null(my.AO.list)) {
      AO=NULL
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
