#accessors
print('a-lib.R execution')
#geo
ageo <- #geo accessor ----
function(
    x=resS
){
  #geo <- copy(x1$geo)
  if(isTRUE(all.equal(sort(unique(names(x$geo))),sort(c('lab','nx','rc9'))))) {
    x1 <- 
      x$geo
  } else {
    x1 <- 
      x$geo%>%
      .[x$lab,on=c(nx='nx')]
  }
  sco(x1,F)
}
#ageo()

#estdt
aestdt1 <- #estdt accessor ----
function(
    x=resS,
    adddatum=T,
    keepj=T
){
  x0 <- as.Date('1994-12-31')
  x1 <- 
    x$rsi[date!=x0]%>%
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
    j1 <- setdiff(names(x$rsi),names(x1))
    x2 <- x$rsi[,c(j1,'nx','date'),with=F]
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
  sco(x4,F)
}
#aestdt1()

aestdt2 <- #date accessor: list of 2 vectors ----
function(
    x=resS,
    adddatum=T
){
  x0 <- x$lab[resS$rsi,on=c(nx='nx')]
  x1 <- as.Date('1994-12-31')
  x2 <- 
    x0[date!=x1]%>%
    .[substr(lab,8,9)=='AN',sort(unique(date))]
  x3 <- 
    x0[date!=x1]%>%
    .[substr(lab,8,9)!='AN',sort(unique(date))]
  if(adddatum==T) {
    x2 <- c(x1,x2)
    x3 <- c(x1,x3)
  }
  x4 <- 
    list(
      AN=x2, #yearends
      BA=x3  #DRC
    )
  x4
}
#aestdt2()

akss <- # kfoldsse accessor ----
function(
    x = resS,
    agg = T) {
  x0 <- x$kss
  x1 <-
    copy(x0) %>%
    .[, rsqraw := 1 - ssra / ssta] %>%
    .[order(nx)]
  x2 <-
    copy(x1) %>%
    .[, .(
      ssri = sum(ssri), #ssri      
      ssti = sum(ssti), #ssti      
      ssrk = sum(ssrk), #ssrk      
      ssra = sum(ssra), #ssta     
      ssta = sum(ssta), #ssra      
      n = sum(n)
    ),
    by = nx
    ] %>%
    .[, rsqraw := 1 - ssra / ssta] %>%
    .[order(nx)]
  x3 <-
    list(
      rc6=sco(x1, F),
      nx=sco(x2, F)
    )
  x3
}
#akss()
# akss <- # kfoldsse accessor ----
#   function(
    #       x = f250509ed$kfoldsse,
#       agg = T) {
#     x1 <-
#       copy(x) %>%
#       .[, rsqraw := 1 - sser / sstr] %>%
#       .[order(nx)]
#     x2 <-
#       copy(x1) %>%
#       .[, .(
#         ssei = sum(ssei),
#         toti = sum(toti),
#         ssek = sum(ssek),
#         sser = sum(sser),
#         sstr = sum(sstr),
#         n = sum(n)
#       ),
#       by = nx
#       ] %>%
#       .[, rsqraw := 1 - sser / sstr] %>%
#       .[order(nx)]
#     x3 <-
#       list(
#         sco(x1, F),
#         sco(x2, F)
#       )
#     x3
#   }

apva <- #pva accessor ----
function(
    x = resS
) {
  copy(x$pva)[
    ,
    .(
      nid,
      m2,
      pv,
      ppm2=pv/m2,
      rc6
    )
  ]%>%
    sco(.,F)
}
#apva()

apol <- #pol accessor ----
function(
    x = datS
) {
  x$pol
}

aresn <- function( #access (nx)
  nxx=ageo(resx)[grep('^L',lab)][1,nx],
  resx=resS
) {
  x1 <- copy(resx)
  x1$geo <- resx$geo[nx%in%nxx]
  x1$lab <- resx$lab[nx%in%nxx]
  x1$rsi <- resx$rsi[nx%in%nxx]
  x1$kss <- resx$kss[nx%in%nxx]
  if('f250618b'%in%names(x1)) {
    x1$f250618b <- resx$f250618b[nx%in%nxx]
  }
  x1
}
#aresn()

areso <- function( #access optimum local(rcx) where x can be 3 or 6
  rcxtx=resS$geo[,unique(rc6)[1:3]],
  resx=resS
) {
  resx$f250618b[grep(grepstring(rcxtx),rc6),nx]%>%
    aresn(resx=resx,nxx=.)
}

aestdt3 <- #daily x
  function(
    #nx=C4131x()[,unique(nx)], #C4131x no longer exists
    nx=CC4231x()[,unique(nx)], #not sure if this is the right default
    resx=resS
  ) {
    x0 <- aresn(nx=nx,resx=resx)
    x1 <- aestdt1(x=x0)
    x2 <- # Step 1: generate full date sequences for each nx
      x1[, .(date = seq(min(date), max(date), by = "day")), by = nx]
    x3 <- # Step 2: rolling join, roll=-Inf, to bring in the most recent xdotd before or on date
      x1[x2, on = .(nx, date), roll = -Inf]
    x3%>% # Step 3: compute cumsum(xdotd) by nx
      .[, x := cumsum(fifelse(is.na(xdotd), 0, xdotd)), by = nx]
    x3[,.(date,ii,nx,xdotd,x)]
  }

