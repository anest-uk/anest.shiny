pcaest <- 
  function(# local version with tantheta[1]!=0 derived from symmetry in 2/3 plane (see last lines)
    x = data.table(date, retmat),
    iscale = c('cov', 'cor'),
    method = c( 'ML','unbiased'),
    signmethod=c('ordered','reference'),
    rcref='WC-3', #reference series name
    refpol=c(1,-1,-1), #polarity associated
    rollsum=1, #apply rollsum - not used
    verbose=T,
    center=T,
    pcawin=x[,range(date)],
    rotwin=c(x[,sort(date)[2]],pcawin[2]), #default to eliminate first period (SNR motive)
    rotate=T, #always T
    krot=2:3,
    doplot=F
  ){
    iscale <- match.arg(iscale)
    method <- match.arg(method)
    signmethod <- match.arg(signmethod)
    pcawin <- sort(round(as.Date(pcawin)))
    stopifnot(is.Date(pcawin)&all(pcawin%in%x[,date])&length(pcawin)==2)
    rotwin <- sort(round(as.Date(rotwin)))
    stopifnot(is.Date(rotwin)&all(rotwin%in%x[,date])&length(rotwin)==2)
    ipcawin <- setNames(match(pcawin,x[,date]),pcawin)
    irotwin <- setNames(match(rotwin,x[,date]),rotwin)
    nbar <- ncol(x) - 1
    x0 <- rollapply(x[,-'date'],width=rollsum,FUN=sum,partial=T,align='right')
    if(rollsum!=1&verbose) {print(paste0('rollsum=',rollsum,' in pcaest'))}
    x1 <- cov.wt(x0[ipcawin[1]:ipcawin[2],],
                 method = method,
                 center = center,
                 cor = T)
    x2 <- x1[[iscale]]
    x3 <- eigen(x = x2)
    dimnames(x3$vectors) <- dimnames(x1$cov) 
    thresh <- sqrt(.Machine$double.eps)
    if(any(x3$values<thresh)) {
      kbar <- max(which(x3$values>thresh))
      x3$values <- x3$values[1:kbar]
      x3$vectors <- x3$vectors[,1:kbar,drop=F]
    } else {
      kbar <- ncol(x2)
    }
    if(signmethod=='ordered') {
      print('ordered method')
      signfinder <-
        function(evec, pola = c(1, -1, -1)[1:min(3,length(evec))]) {
          #sign by regression on 1st 3 even zero phase cos(x.centred); pola is tgt for last
          n <- length(evec) - 1
          x <-
            data.table(
              y = evec,
              f0 = rep(1, n + 1),
              f1 = -cos(pi * (0:n) / n),
              f3 = cos(-2 * pi * (0:n) / n)
            )
          x1 <- summary(lm(y ~ . - 1, x))$coefficients
          if(any(is.na(x1[,2]))){
            x2 <- sign(sum(x1[, 1] * pola))
          } else {
            x2 <- sign(sum(x1[, 3] * pola))#sum of t-stats (scale-invariant)
          }
          x2
        }
      x4 <- unlist(lapply(data.table(x3$vectors), signfinder))
      if(any(is.na(x4))) {x4 <- rep(1,length(x4))}
    } else {
      stopifnot(rcref%in%rownames(x3$vectors))
      iref <- match(rcref,rownames(x3$vectors))
      jref <- seq_along(refpol)
      x4 <- c(refpol*sign(x3$vectors[iref,jref]),rep(1,ncol(x3$vectors)-length(jref)))
    }
    x3$vectors <- sweep(x3$vectors,
                        STAT = x4,
                        MAR = 2,
                        FUN = `/`)
    x4 <- list(
      x = x,
      xrs = x0, #rs 'rollsum applied'
      date = x[, date],
      sigma=sqrt(diag(x1$cov)),
      xx = x2,
      eig = x3,
      tantheta = 
        c(.13 #base rotation for 240502 run
          -0.01008349, #adjustment applied 240503
          rep(0,nbar-1)
          ),
      g = rep(1, nbar),
      par = list(
        method = method,
        iscale = iscale,
        xsect = '',
        rollsum=rollsum,
        kbar=kbar,
        ipcawin=ipcawin,
        irotwin=irotwin
      )
    )
    if(rotate) { #solve theta to minimise range(z(k=2,3)), update tantheta
      x4 <- pcarot0(x4)
    }
    if(doplot) {plot(cumsum(pcaz(x4))[,1:3],scr=1,col=1:3)}
    x4
  }
