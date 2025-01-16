f121D <- function( #winding, probably
  estdtX =estdtlG,dfnxX  =dfnxG, #----
  drangeX=range(dfnxxX),
  typeX  =typeC, #L
  tbinX  =tbinC, 
  dfnxxX =dfnxX[-1,tbinC+1,with=F]%>%setnames(.,'x')%>%.[,sort(unique(x))],#current tbin
  d2X    =dfnxX[-1,tbinC+2,with=F]%>%setnames(.,'x')%>%.[,sort(unique(x))] #annual dates t>0
) {
  d1 <- #daily
    seq.Date(from=drangeX[1],to=drangeX[2],by='d')
  x1 <-
    estdtX%>% #local
    .[.(date=d1),on=c(date='date'),roll=-Inf,j=.(date,xdotd)]%>%
    .[,.(ii=1:.N,date,x=cumsum(xdotd))]%>%
    .[.(date2=d2X),on=c(date='date2')]%>%
    .[,.(date,x,xdot=c(x[1],diff(x)),ii=1:.N)]%>%
    .[,.(ii,date,xdot,x)]%>%
    .[,.(date,xdot)]%>%
    .[date==as.Date('2009-02-28'),let(date,as.Date('2008-12-31'))]%>%
    .[,.(decade=substr(date,1,3),yr=substr(date,4,4),xdot=round(xdot,3))]%>%
    dcast(.,decade~yr,value.var='xdot')%>%
    .[,decade:=c(1990,2000,2010,2020)]
  for(i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]),'',as.character(round(x1[[i]],3)))
  x2 <- gt::gt(x1)%>%gt::tab_footnote(
    footnote=f241108a(typeX,tbinX)[[1]]
  )%>%gt::tab_footnote(
    footnote=f241108a(typeX,tbinX)[[2]]
  )
  x2
}

f122D <- function( #122 characteristics----
                   rc6tX=rc6tG,
                   rssaX=rssaG,
                   rsscuX=rsscuG,
                   z110X=z110G
){
  rsscux <- copy(rsscuX)[,lab:='CU000']#R()
  f122 <- #combine rss and P characteristics
    function(rssx,z110X) {
      x0 <- 
        z110X[rssx,on=c(rcx='rc6')]%>%
        .[,.(
          frac=round(sum(nid)/z110X[nchar(rcx)==6,sum(nid)],nfig3),
          nid=sum(nid),
          ppm2max=round(max(ppm2),nfig2),
          ppm2min=round(min(ppm2),nfig2),
          p=round(sum(pv)/sum(m2),nfig2)
        ),
        lab
        ]%>%
        .[rssx[,.(R2rsi=1-sum(ssek)/sum(sstr)),lab],on=c(lab='lab')]%>%
        .[,.(
          lab=substr(lab,1,4),
          frac,
          R2rsi=round(R2rsi,3),
          pnum=p,
          p=prettyNum(round(p,nfig3), big.mark=","),
          p.cus=paste0(prettyNum(round(ppm2min,nfig2), big.mark=","),'-',prettyNum(round(ppm2max,nfig2), big.mark=","))
        )]
    }
  x0 <- f122(rssx=rsscux,z110X=z110X)
  x1 <- f122(rssx=rssaX,z110X=z110X)
  x2 <- 
    rbind(x1,x0)[order(-pnum)][,-'pnum']
  print(x2)
  x <- 
    x2%>%
    gt::gt(.)%>%
    cols_label(
      lab = gt::html('Area-band'),
      frac = gt::html('Fraction<br>properties'),
      R2rsi = gt::html("RSI R<sup>2</sup>"),
      p = gt::html("Aggregate"),
      p.cus=gt::html("Range")
    )%>%
    tab_spanner(
      label = gt::html("£/m<sup>2</sup>"),
      columns = c(p.cus, p)
    )%>%gt::tab_footnote(
      footnote=f241108a(typeC,tbinC)[[1]]
    )%>%gt::tab_footnote(
      footnote=f241108a(typeC,tbinC)[[2]]
    )
  x122G <<- copy(x)
  x
}
#f122D()
f131D <- function( #131 summary----
                   estdtxX=estdtxG,tslideX=tslideG )
{
  x <- 
    estdtxX%>%
    .[ii>=tslideX]%>%
    dcast(.,ii~lab,value.var='xdot')%>%
    .[,-'ii']%>%
    as.matrix(.)%>%
    zoo(.,estdtxX[,sort(unique(date))])%>%
    table.Stats(.,digits=3)%>%
    data.table(.,keep.rownames = T)%>%
    `[`(.,i=-c(1,2,7,11,12,13))%>%
    gt::gt(.)%>%
    cols_label(
      rn = gt::html('Log return<br>summary')
    )%>%gt::tab_footnote(
      footnote=f241108a(typeC,tbinC)[[1]]
    )%>%gt::tab_footnote(
      footnote=f241108a(typeC,tbinC)[[2]]
    )
  x
}

f132 <- function( #trade summary
  geox=geoqG,
  steprip='smallrip/',
  estdtlx=estdtlG, #only used for its date(ii) relation
  tmin=20
) {#tmin=input$tslider
  x0 <-
    geox[,grepstring(rc6)]%>%
    coread2(.,steprip)%>% #or rc6tC
    .[,.(N=.N,mean=round(mean(as.numeric(retsa)),4)),.(buy=substr(as.Date(buydate),1,4),sell=substr(as.Date(selldate),1,4))]%>%
    .[(buy>=estdtlx[ii>=tmin,substr(min(as.character(date)),1,4)])]
  x1 <- 
    x0%>%
    dcast(.,
          buy~sell,
          value.var='mean' #the value is unique so any aggregator function is ok
    )
  for(i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]),'',as.character(round(x1[[i]],3)))
  x2 <-
    x0%>%
    dcast(.,
          buy~sell,
          value.var='N'
    )
  for(i in 2:length(x2)) x2[[i]] <- ifelse(is.na(x2[[i]]),'',x2[[i]])
  x3 <- list(x1,x2)
  x3
}

f132D <- function( #132 trade summary(2)----
                   tslideX=tslideG,
                   geoqX=geoqG,
                   geocuX=geocuG,
                   estdtlX=estdtlG
)
{
  steprip='smallrip/'
  tminx <- tslideX
  x1 <- f132(
    geox=geoqX,#geoqR()
    steprip=steprip,
    estdtlx=estdtlX,#estdtlR()
    tmin=tminx#tmin=input$tslider
  )
  x2 <- f132(
    geox=geocuX[,.(rc6=rc9)],#geoqR()
    steprip=steprip,
    estdtlx=estdtlX,#estdtlR()
    tmin=tminx#tmin=input$tslider
  )
  x <- list(
    local=x1,
    custom=x2
  )
  x[['local']][[1]] <- 
    x[['local']][[1]]%>%
    gt::gt(.)%>%
    tab_header(.,title = 'Local - Return')%>%
    opt_align_table_header(., align = "left")%>%
    tab_options(heading.title.font.size =14)%>%
    tab_spanner(
      label = gt::html("sell"),
      columns = 2:ncol(x[['local']][[1]])
    )
  x[['local']][[2]] <- 
    x[['local']][[2]]%>%
    gt::gt(.)%>%
    tab_header(., title='Local - Number')%>%
    opt_align_table_header(., align = "left")%>%
    tab_options(heading.title.font.size =14)%>%
    tab_spanner(
      label = gt::html("sell"),
      columns = 2:ncol(x[['local']][[2]])
    )
  x[['custom']][[1]] <- 
    x[['custom']][[1]]%>%
    gt::gt(.)%>%
    tab_header(., title='Custom - Return')%>%
    opt_align_table_header(., align = "left")%>%
    tab_options(heading.title.font.size =14)%>%
    tab_spanner(
      label = gt::html("sell"),
      columns = 2:ncol(x[['custom']][[1]])
    )
  x[['custom']][[2]] <- 
    x[['custom']][[2]]%>%
    gt::gt(.)%>%
    tab_header(., title='Custom - Number')%>%
    opt_align_table_header(., align = "left")%>%
    tab_options(heading.title.font.size =14)%>%
    tab_spanner(
      label = gt::html("sell"),
      columns = 2:ncol(x[['custom']][[2]])
    )
  x132G <<- copy(x)
  if(verbose) print('exit x132D')
  x
}
f211D <- function( #triang recap
  estdtlX=estdtlG, #single
  geoqX=geoqG, #footnote only 'this qtile'
  dfnxxX=dfnxxG, #single
  typeX=typeC
) {
  if(verbose) print('enter x211D')
  x1 <-
    fread('f241122ad.csv')%>%
    .[geoqX[,.(rc6,lab)],on=c(rc6='rc6')]%>%
    .[,.(cum=sum(cum)),.(lab,nh,date)]%>%
    .[data.table(date=dfnxxX[-1],i=1:(length(dfnxxX)-1)),on=c(date='date')]%>% #dfnG is all dates, all frequencies
    dcast(.,date+i+lab~nh,value.var='cum')%>%#
    .[order(date),.(date,t=i,lab,NF,NH,UF,UH)]
  x2 <-
    estdtlX%>%
    .[,.(t=c(0,ii),days=c(NA,days),date=c(date[1]-days[1],date),xdot=c(NA,xdot),x=c(0,x))]%>%
    x1[.,on=c(t='t')]%>%
    .[1,let(NF,0)]%>%
    .[1,let(NH,0)]%>%
    .[1,let(UF,0)]%>%
    .[1,let(UH,0)]%>%
    .[,.(t,date=i.date,days,xdot,x,
         NF=c(0,diff(NF)),
         NH=c(0,diff(NH)),
         UF=c(0,diff(UF)),
         UH=c(0,diff(UH)),
         tot=c(0,diff(NF+NH+UF+UH))
    )]%>%
    .[-1,.(
      t,
      date,
      days,
      return=round(xdot,sf),
      cumreturn=round(x,sf),
      newhouse=round(NH/tot,sf),
      usedhouse=round(UH/tot,sf),
      newflat=round(NF/tot,sf),
      usedflat=round(UF/tot,sf),
      total=round(tot),
      perday=round(tot/days,1)
    )]
  x3 <- #districts footnote
    geoqX[
      ,paste0('Districts: ',paste0(sort(irregpcode(rc6)),collapse=', '))]
  x <-
    gt::gt(x2)%>%gt::tab_footnote(
      footnote=f241108a(typeX,tbinC)[[1]]
    )%>%gt::tab_footnote(
      footnote=f241108a(typeX,tbinC)[[2]],
      locations = NULL,
      placement = c("auto", "right", "left")
    )%>%gt::tab_header(
      title=x3
    )%>%
    cols_label(
      date = gt::html('end date'),
      cumreturn = gt::html('cumulative'),
      newhouse = gt::html('new house'),
      usedhouse = gt::html('used house'),
      newflat = gt::html('new flat'),
      usedflat = gt::html('used flat'),
      perday = gt::html('per day'),
      total = gt::html('total')
    )%>%
    tab_spanner(
      label = gt::html("Period"),
      columns = c(date,days)
    )%>%
    tab_spanner(
      label = gt::html("Log price"),
      columns = c(return,cumreturn)
    )%>%
    tab_spanner(
      label = gt::html("Fraction"),
      columns = c(newhouse, usedhouse,newflat,usedflat)
    )%>%
    tab_spanner(
      label = gt::html("Count"),
      columns = c(total,perday)
    )%>%
    tab_spanner(
      label = gt::html("Sales Breakdown"),
      columns = c(newhouse, usedhouse,newflat,usedflat,total,perday)
    )%>%
    tab_options(
      heading.align = 'left',
      heading.title.font.size = 12
    )
  
  x211G <<- copy(x)
  x
}
#  f211D()
f311D <- function(geo0X=geo0G,z110X=z110G,rc6tX=rc6tG)                   #311 constituents----
{
  if(verbose) print('enter 311')
  x1 <- 
    geo0X[,.(rc3,rc6,qtile)]%>%
    z110X[.,on=c(rcx='rc6')]%>%
    .[,.(rc3,rc6=rcx,nid,ppm2=round(ppm2),quantile=paste0('local-',qtile))]
  x <- 
    DT::datatable(
      x1,
      options = list(
        search = list(search = rc6tX), 
        columnDefs = list(list(className = 'dt-center', targets = 1:4,searchable = F, targets = 3:5)),
        paging=T,
        pageLength=100,
        initComplete = JS(
          "function(settings, json) {",
          "$('body').css({'font-family': 'Calibri'});",
          "}"
        )
      ),
      rownames=F
    )%>%
    DT::formatStyle( 0, target= 'row', lineHeight='70%')
  x311G <<- copy(x)
  x
}
