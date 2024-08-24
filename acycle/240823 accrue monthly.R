#accrue estdt into monthly arith returns
f0823 <- 
  function(
    x0=z321
  ) {
    x1 <- 
      z321$ses$estdt%>%
      .[order(nx,date0)]
    x2 <-
      x1%>%
      .[,.(ddate=seq.Date(from=date0[1],to=date1[.N],by=1))]
    x3 <- 
      x2[,.(mdate=seq.Date(from=min(ddate)+1,to=max(ddate)+1,by='m')-1)]
    x4 <- x1[,sort(unique(nx))]
    x5 <- list(NULL)
    for(i in seq_along(x4)) {
      x5[[i]] <- 
        x1[nx==x4[i]][x2,on=c(date0='ddate'),roll=Inf]%>%
        .[,.(date0,nx,x=cumsum(c(0,xdot.daily[-1])))]%>% #start cumulation day 1, not day 0
        .[x3,on=c(date0='mdate')]%>%
        .[,.(date=date0[-1],nx[-1],xdot=diff(x))]
    }
    x6 <- zoo::zoo(x5[[8]][,.(r=exp(xdot)-1)],x5[[i]][,date])
    x7 <- PerformanceAnalytics::table.CalendarReturns(x6,digits=2)
    x7
  }