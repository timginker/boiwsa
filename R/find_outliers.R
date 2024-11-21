#' Find additive outliers
#'
#' Searches for additive outliers using the method described in Appendix C of Findley et al. (1998).
#' If the number of trigonometric variables is not specified will search automatically through the model space to identify the best number of trigonometric variables, with the lowest AIC, AICc or BIC value.
#'
#' @importFrom stats AIC BIC lm median supsmu
#' @import lubridate
#'
#' @param x Numeric vector. Time series to seasonally adjust
#' @param dates a vector of class "Date", containing the data dates
#' @param out.tolerance t-stat threshold for outliers (see Findley et al., 1998)
#' @param my.AO.list (optional) Vector with user defined additive outlier variables
#' @param H (optional) Matrix with holiday and trading day variables
#' @param my.k_l (optional) Vector with the number of fourier terms to capture the yearly and monthly cycle. If NULL, would perform automatic search using AICc criterion
#' @param method Decomposition method: "additive" or "multiplicative". By default uses the additive method
#'
#' @return my.k_l
#' @return ao list of AO dates
#' @references Findley, D.F., Monsell, B.C., Bell, W.R., Otto, M.C. and B.C Chen (1998). New capabilities and methods of the X-12-ARIMA seasonal-adjustment program. Journal of Business & Economic Statistics, 16(2), pp.127-152.
#' @export
#'
#' @examples
#'
#' \donttest{
#' #Not run:
#' # Searching for additive outliers in Gasoline data
#' data(gasoline.data)
#' ao_list=find_outliers(x=gasoline.data$y,dates = gasoline.data$date)}
#'

find_outliers=function(x,dates,out.tolerance=3.8,my.AO.list=NULL,H=NULL,my.k_l=NULL,method="additive"){


  #----------------------------------------------#

  rankUpdateInverse <- function(X_inv, X_t, v) {
    u1 <- X_t %*% v
    u2 <- X_inv %*% u1
    d <- as.numeric(1 / (t(v) %*% v - t(u1) %*% u2))
    u3 <- d * u2
    F11_inv <- X_inv + d * u2 %*% t(u2)
    XtX_inv <- rbind(cbind(F11_inv, -u3), c(-u3, d))
    return(XtX_inv)
  }

  # create AO variables matrix

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
  # function to find optimal number of trigonometric variables

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
        aicc0[i,j]=stats::AIC(m)+2*length(m$coefficients)*(length(m$coefficients)+1)/(length(m$residuals)-length(m$coefficients)+1)
        bic0[i,j]=stats::BIC(m)

      }


    }


    opt.aic=(which(aic0 == min(aic0), arr.ind = TRUE)-1)*by
    opt.aicc=(which(aicc0 == min(aicc0), arr.ind = TRUE)-1)*by
    opt.bic=(which(bic0 == min(bic0), arr.ind = TRUE)-1)*by

    return(list(opt.aic=opt.aic,opt.aicc=opt.aicc,opt.bic=opt.bic))

  }




  if (method=="multiplicative") {
    x=log(x)
  }

  trend.init=stats::supsmu(1:length(x),x)$y

  y=x-trend.init


  # if my.k_l is not specified
  if (is.null(my.k_l)) {
    # if there are no prespceified AOs set AO=NULL
    if (is.null(my.AO.list)) {
      AO=NULL
    }else{
    #if there are prespecifed AO dates, create AO variables
      AO=my_ao(dates=dates,out.list =my.AO.list )

    }

    # search for optimal number of Fourier variables
    opt=find_opt(x = y, dates = dates,H = H, AO = AO)
    # set my.k_l based on AICc
    my.k_l=opt$opt.aicc

  }

  # RUN only if there are a positive number of Fourier variables
  if(sum(my.k_l)>0){

    X=fourier_vars(k=my.k_l[1],l=my.k_l[2],dates = dates)


    Xs=cbind(X,H,AO)

    err=y-Xs%*%solve(t(Xs)%*%Xs)%*%t(Xs)%*%y

    sig_R=1.49*stats::median(abs(err))



    f.sel.pos=NULL

    out.search.points=(1:length(dates))[!dates%in%my.AO.list]

    run=TRUE

    Xs_t <- t(Xs)
    while (run) {
      Ts <- numeric(length(out.search.points))
      ts_idx <- 1
      Xst2_inv <- solve(crossprod(Xs))
      Xst_y <- t(Xs) %*% y
      for (t in out.search.points) {

        AOt=rep(0,length(dates))

        AOt[t]=1

        Xst2_inv_t <- rankUpdateInverse(Xst2_inv, Xs_t, AOt)
        Xst_y_t <- rbind(Xst_y, t(AOt) %*% y)
        Tt <- (Xst2_inv_t %*% Xst_y_t)[ncol(Xs) + 1] / (diag(Xst2_inv_t * sig_R^2)[ncol(Xs) + 1]^0.5)
        Ts[ts_idx] <- abs(Tt)
        ts_idx <- ts_idx + 1
      }


      if (max(Ts)>=out.tolerance) {

        AOt=rep(0,length(dates))

        AOt[out.search.points[which.max(Ts)]]=1

        f.sel.pos=c(f.sel.pos,out.search.points[which.max(Ts)])

        out.search.points=out.search.points[-which.max(Ts)]

        Xs <- cbind(Xs, AOt)
        Xs_t <- t(Xs)
      }





      if (max(Ts)<out.tolerance) {
        run=FALSE
      }


    }


    # Backward deletion


    if(length(f.sel.pos)>0){

      run=TRUE

      f.sel.ao.dates=dates[f.sel.pos]

    }else{

      f.sel.ao.dates=NULL

    }



    while (run) {





      AObd=my_ao(dates=dates,out.list=lubridate::as_date(c(my.AO.list,f.sel.ao.dates)))


      Xst=cbind(X,H,AObd)

      err=y-Xst%*%solve(t(Xst)%*%Xst)%*%t(Xst)%*%y

      sig_R=1.49*stats::median(abs(err))

      Tt=abs((solve(t(Xst)%*%Xst)%*%t(Xst)%*%y)/(diag(solve((t(Xst)%*%Xst))*sig_R^2)^0.5))[(ncol(Xst)-length(f.sel.ao.dates)+1):ncol(Xst)]


      if(min(Tt)<out.tolerance){

        f.sel.ao.dates=f.sel.ao.dates[-which.min(Tt)]

      }else{

        run=FALSE
      }

      if(length(f.sel.ao.dates)==0){

        run=FALSE
      }


    }

    if(length(f.sel.ao.dates)==0){

      f.sel.ao.dates=NULL
    }else{

      f.sel.ao.dates=f.sel.ao.dates[order(f.sel.ao.dates)]
    }



    return(list(ao=f.sel.ao.dates,my.k_l=my.k_l))


  }else{

    return(list(ao=NULL,my.k_l=my.k_l))
  }




}
