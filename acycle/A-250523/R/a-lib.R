#accessors

#geo
ageo <- #geo accessor ----
  function(
    x=f250509ed$geo
  ){
    copy(x)%>%
      .[,.(rc9,nx,lab)]%>%
      sco(.,F)
  }
#ageo()

#estdt
aestdt1 <- #estdt accessor ----
  function(
    x=res$rsi,
    adddatum=T,
    keepj=T
  ){
    x0 <- as.Date('1994-12-31')
    x1 <- 
      x[date!=x0]%>%
      .[order(nx,date),.(xdotd,date,nx)]%>%
      .[,.(
      xdotd,date,
      days=as.integer(diff(c(x0,date))),
      ii=1:.N
      ),
      by=nx
      ]%>%
      .[,xdot:=days*xdotd,by=nx]%>%
      .[,x:=cumsum(xdot),by=nx]%>%
      .[]
    if(keepj) {#retain additional columns
      j1 <- setdiff(names(x),names(x1))
      x2 <- x[,c(j1,'nx','date'),with=F]
      x3 <- x1[x2,on=c(nx='nx',date='date')]
    } else {
      x3 <- x1
    }
    if(adddatum) {#add initial datum row
      x4 <- 
        x3[,.SD[1,],nx]%>%
        .[,date:=x0]%>%
        .[,c('xdotd','days','xdot','x','ii'):=0]%>%
        rbind(.,x3)%>%
        .[order(nx,date)]
    } else {
      x4 <- x3
    }
    x4[]
  }
#aestdt1()

aestdt2 <- #date accessor: list of 2 vectors ----
  function(
    x=res$lab[res$rsi,on=c(nx='nx')],
    adddatum=T
  ){
    x0 <- as.Date('1994-12-31')
    x1 <- 
      x[date!=x0]%>%
      .[substr(lab,8,9)=='AN',sort(unique(date))]
    x2 <- 
      x[date!=x0]%>%
      .[substr(lab,8,9)!='AN',sort(unique(date))]
    if(adddatum==T) {
      x1 <- c(x0,x1)
      x2 <- c(x0,x2)
    }
    x3 <- 
      list(
        AN=x1, #yearends
        BA=x2  #DRC
        )
    x3
  }
#aestdt2()

akss <- # kfoldsse accessor ----
  function(
      x = f250509ed$kfoldsse,
      agg = T) {
    x1 <-
      copy(x) %>%
      .[, rsqraw := 1 - sser / sstr] %>%
      .[order(nx)]
    x2 <-
      copy(x1) %>%
      .[, .(
        ssei = sum(ssei),
        toti = sum(toti),
        ssek = sum(ssek),
        sser = sum(sser),
        sstr = sum(sstr),
        n = sum(n)
      ),
      by = nx
      ] %>%
      .[, rsqraw := 1 - sser / sstr] %>%
      .[order(nx)]
    x3 <-
      list(
        sco(x1, F),
        sco(x2, F)
      )
    x3
  }
#akss()

apva <- #pva accessor ----
  function(
      x = z110) {
    copy(x)[
      ,
      .(
        nid,
        m2,
        pv,
        pt,
        ppm2,
        rcx
      )
    ]%>%
      sco(.,F)
  }
#apva()


