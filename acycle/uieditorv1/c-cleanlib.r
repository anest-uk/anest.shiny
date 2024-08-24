accrue2 <-
function(  #for vector of dates, accrue days from buydate to selldate
    pdate=round(seq(from=fur[,min(buydate)],to=fur[,max(selldate)],length.out=10)),
    fur = f221029bd #has Date fields buydate,selldate
  ) {
    x1 <- pdate <- round(sort(unique(pdate)))
    x1[length(x1)] <- fur[,max(selldate)] #always accrue all dates at the end
    x2 <- structure(outer(x1, fur[, selldate], `-`)
                    , class = 'numeric')
    x2[] <- pmax(0, x2[]) #numeric matrix
    x3 <-   structure(outer(x1, fur[, buydate], `-`)
                      , class = 'numeric')
    x3[] <- pmax(0, x3[])
    structure(
      cbind(
        t(x3[1, , drop = F]), 
        t(diff(x3 - x2))
      ), #accrue all days up to pdate[1]
      dimnames=list(
        fur[, idhash.selldate], 
        as.character(pdate[])
      )
    )
  }
chkpcode <-
function(
    pc='EC2R 8AH'
    ) {
  nch <- sapply(pc,nchar)
  stopifnot(all(nch<=8)) #right length
  stopifnot(unlist(lapply(gregexpr(patt=' ',pc),length))==1)#max one space
  x <- strsplit(pc,split=' ')
  if(length(x[[1]])==1) {
    stopifnot(all(unlist(gregexpr(pc,patt='^[A-Z,a-z]'))==1))#1-part always starts with alpha cap
  }
  if(length(x[[1]])==2) {
    pcin <- lapply(x,'[[',2)
    if(!all(unlist(gregexpr(pcin,patt='^[0-9]'))==1)) browser()#2-part always starts with number
    stopifnot(all(unlist(gregexpr(pcin,patt='^[0-9]'))==1))
  }
  TRUE
}
cobalt <-
function(){
c(
  blue='#0082F4',
  green='#35CA05',
  onch='#ED9304',
  punk='#FF628C',
  midnight='002140')
}
coread <-
function( #read data from csv in subdir
    rcx='AL-', #postcodes to read
    step='nac', #subdir name
    colClasses=NULL, #e.g. colClasses=c(deed_date='character'),nrow=10
    nrows=Inf
) {
  x1 <- dir(step)
  if(length(x1)==0) {stop(paste0('step: ',step,' no files found'))}
  x2 <- grep(grepstring(rcx,caret=T),x1)
  x3 <- x1[x2]
  ext <- tolower(unique(unlist(lapply(strsplit(x3,split='\\.'),`[`,i=2))))
  stopifnot(length(ext)==1) #csv or rdata
  x4 <- list(NULL)
  i <- 1
  for(i in seq_along(x3)) {
    fp <- paste0(step,'/',x3[i])
    if(ext=='rdata') {
      load(file=fp)
    } else if(ext=='csv') {
      x <- fread(file=fp,colClasses=colClasses,nrows=nrows)
      if(is.data.table(x)&is.null(colClasses)) { #undo autorecognition
        for(i2 in seq_along(x)) {
          x[[i2]] <- as.character(x[[i2]])
        }
      }
    } else {
      stop(paste0('file not found step=',step,'rcx=',paste0(rcx,collapse=',')))
    }
    x4[[i]] <- x
  }
  rbindlist(x4)
}
f221209a <-
function( #wrapper for accrue2
    geo=f221230ad$geo,
    dfn=f230215c(), #yearend series
    fur=f221029bd,
    applygeo=T
  ) {
    dfn <- sort(unique(dfn))
    if(applygeo) {
      x0 <- fur[geo,on=names(geo)[grep('^rc.',names(geo))]%>%setNames(.,.),allow=T]
    } else {
      x0 <- fur
    }
    x1 <- accrue2( #accrue
      fur = x0,
      pdate = dfn[-1])%>%
      data.table(.,keep.rownames=T)%>%
      .[
        x0[,.(idhash.selldate,rc9,retsa)],
        on=c(rn='idhash.selldate')
      ]%>%
      setnames(.,old='rn',new='idhash.selldate')
    x1
  }
f230312a <-
function(  #solve single nx -> estdt with no pra
    nxx=geo[,min(nx)],
    stepdfn='ver001\\06dfn',
    stepgeo='ver001\\04geo',
    steprip='ver001\\03rip', 
    dfn=coread(rcx='xxx',
               step=stepdfn,
               colClasses=c(dfn='Date'),
               nrows=Inf)%>%
      .[,dfn],    
    geo=coread(rcx='xxx',
               step=stepgeo,
               colClasses=list(integer='nx'), 
               nrows=Inf),
    outthresh=.1
  ) {
    x1 <- #rip read
      coread(
        rcx=geo[nx==nxx][,rc9],
        step=steprip,
        colClasses=list(numeric=c('retsa'),Date=c('buydate','selldate'))#'retraw',
      )
    x2 <- f221209a(
        geo=geo[nx==nxx], 
        fur=x1,
        dfn=dfn,
        applygeo=F
      )
    x4 <- lm(
      retsa~.-1,
      x2[,!c('idhash.selldate','rc9')] #all
    )
    x5 <- residuals(x4)
    x6a <- x2[,.(idhash.selldate,rc6=substr(rc9,1,6),res=x5)] #per rc6
    x6b <- x6a[,.(lo=quantile(res,outthresh/2),hi=quantile(res,(1-outthresh/2))),rc6] #apply thresholds
    x6c <- x6b[x6a,on=c(rc6='rc6')][(lo<res&res<hi),.(rc6,idhash.selldate)] #select inlier
    x6d <- x2[x6c[,.(idhash.selldate)],on=c(idhash.selldate='idhash.selldate')]
    x6e <- x6d[,!c('idhash.selldate','rc9')]
    x6 <- lm(
      retsa~.-1,
      x6e)%>%
      .[['coefficients']]%>%
      data.table(
        xdotd=as.numeric(.),
        date=as.Date(substr(names(.),2,11)))%>% 
      .[,days:=as.numeric(diff(c(min(dfn),date)))]%>%
      .[,xdot:=as.numeric(xdotd*days)]%>%
      .[,x:=cumsum(xdot)]%>%
      .[,.(nx=nxx,rsqraw=summary(x4)$r.squared,date,xdotd,days,xdot,x,lab=geo[nx==nxx][1,lab])]%>%
      .[,ii:=1:.N,lab]%>%
      .[,col:=as.factor(lab)]
    x6
  }
f230703c <-
function(  #NUTS2 names
  ) {
    nname <-
      structure(list(
        X1 = c('L','K','J','I','H','G','F','E','D','C'), 
        X2 = 
          c('Wales','South West','South East','London','East of England',
            'West Midlands','East Midlands','Yorkshire and Humber', 'North West',
            'North East'),
        X3 = c('Wales','Southwest','Southeast','London', 
               'East','W Midlands','E Midlands','Yorkshire', 
               'Northwest','Northeast'),
        nx=c(4,7,9,10,8,5,6,3,2,1)
      ), class = 'data.frame', 
      row.names = c(NA, 
                    -10L))%>%
      data.table(.)%>%setnames(.,c('code','name','abbrev','nx'))%>%
      .[order(-nx)]
    nname
  }
f231204a <-
function(#generate table 4, 'geo comparison' combining P, RSI, LFM, CIRC
  ipanel=3,  #note this function *breaks* the universal rule: it has global references xnnn which are not passed
  cardinal=c('TS-','L--','S--','M--','LS-','B--','BS-','AL-','N--','WC-') 
    ) {
  rsi=list(z221,z321,z421)[[ipanel]] #---global 
  prj=list(z223,z323,z423)[[ipanel]] #---global
  bwe=list(z224,z324,z424)[[ipanel]] #---global
  pva=z110 #-----------------------------global
  stat=rsi$ses$stat
  geo=rsi$geo
  x0 <- #expand rc3 parts of geo into rc6
    pva[nchar(rcx)==6]%>%
    .[,rc3:=substr(rcx,1,3)]%>%
    .[rc3%in%geo[,rc9],.(rc6=rcx,rc3)]%>%
    geo[.,on=c(rc9='rc3')]%>%
    .[,.(rc9=rc6,nx,lab)]%>%
    rbind(.,geo[nchar(rc9)==6])
  cname=rbind(
    data.table(panel=1,nx=1:10,lab=gsub('-','',cardinal)),
    data.table(panel=2,nx=1:10,lab=paste0('np',1:10)),
    data.table(panel=3,nx=1:10,lab=f230703c()[order(nx),abbrev])
  )%>%
    .[panel==ipanel,.(ipanel,nx,lab)]
  x1 <- 
    stat[type=='all',.(nx,rsq)]%>%
    .[prj[,.(nx,rbarsqprj,aprj,ase=aprj/atprj)],on=c(nx='nx')]%>%
    .[bwe[,.(nx,b1,tbw=atan2(b3w,b2w),dtbw=c(NA,diff(atan2(b3w,b2w))))],on=c(nx='nx')]%>%
    .[pva[x0,on=c(rcx='rc9')][,.(nid=sum(nid),ppm2min=min(ppm2),ppm2max=max(ppm2),ppm2=sum(pv)/sum(m2)),nx],on=c(nx='nx')]%>%
    .[,.(nx,ppm2min,ppm2max,ppm2,fid=nid/sum(nid),r2rsi=rsq,rbar2prj=rbarsqprj,b1,tbw,dtbw,aprj,ase)]%>%
    cname[.,on=c(nx='nx')]%>%
    .[order(-nx)]
  x1
}
f240810a <-
function( #leaflet special/custom function for index app, copied from anest.shiny/acycle/applib.R
    zoomx=6.5,
    x3a=pxosrdo2dd,#rc6 polygons 
    pva=z110,
    rcx=regpcode(pxosrdo2dd$name[grep('^NG',pxosrdo2dd$name)]),#c('NG-7--','NG-2--')#,
    targetrcx=rcx[1],
    minzoom=6,
    maxzoom=11,
    palx=leaflet::colorNumeric(palette=cobalt()[c(2,4)],domain=0:1)
  ){
    gsx <- grepstring(rcx,caret=T,dollar=T)
    width=1000*.7
    height=1000*.7
    w1=1
    l1=.08
    addlegend=F
    uproj3 = #desired longlat
      "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"%>%
      CRS(.)
    x3 <- #data slot is pc
      x3a[grep(gsx,regpcode(x3a@data$name)),] #selected polygons named irreg
    x4 <- #add rc6 and vv
      data.table(x3@data)%>%
      .[,rc6:=regpcode(name)]%>%
      .[,.(name,rc6,col=as.numeric(rc6==targetrcx))]
    x3@data <- data.frame(x4)
    x5 <- pva[grep(gsx,rcx),.(rcx=sort(rcx))]
    labels <- sprintf(
      paste0("<strong>%s</strong><br/>%g"),
      x5[,irregpcode(rcx)],pva[x5,round(ppm2,-1),on=c(rcx='rcx')]
    ) %>%
      lapply(htmltools::HTML)
    x7 <- leaflet(
      x3,
      width=width,
      height=height,
      options=leafletOptions(
        zoomControl=F,
        minZoom=minzoom,
        maxZoom=maxzoom,
        zoomDelta=2
      )
    )
    x8 <- x7 %>% 
      addPolygons(
        stroke=T,
        fillColor = ~palx(col),
        smoothFactor = 0, #remove white slivers
        weight = 1,
        opacity = 1,
        color = "transparent",#"steelblue",
        dashArray = "2",
        fillOpacity = .5,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto")
      )%>%
      addProviderTiles(providers$CartoDB.Positron)
    x8
  }
f240823a <-
function( #winding performance table
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
    x4 <- 
      x1[,sort(unique(nx))]
    x5 <- 
      list(NULL)
    for(i in seq_along(x4)) {
      x5[[i]] <- 
        x1[nx==x4[i]][x2,on=c(date0='ddate'),roll=Inf]%>%
        .[,.(date0,nx,x=cumsum(c(0,xdot.daily[-1])))]%>% #start cumulation day 1, not day 0
        .[x3,on=c(date0='mdate')]%>%
        .[,.(date=date0[-1],nx[-1],xdot=diff(x))]
    }
    x6 <- 
      zoo::zoo(x5[[8]][,.(r=exp(xdot)-1)],x5[[i]][,date])
    x7 <- 
      PerformanceAnalytics::table.CalendarReturns(x6,digits=2)
    x7
  }
grepstring <-
function(#grep for any in x 
    x=regpcode(metro()), #character vector
    dollar=F,
    caret=T
    ) {
  if(caret) x <- paste0('^',x)
  if(dollar) x <- paste0(x,'$')
  paste(x,collapse='|') #OR function does the work
}
irregpcode <-
function( #convert regular (area,district,sector,unit) 12 char to 'normal' postcode
    x
    ) {
  x1 <- substr(x,1,pmin(6,nchar(x)))
  x2 <- substr(x,pmin(7,nchar(x)),nchar(x))
  gsub(patt=' $',rep='',x=paste(gsub(patt='\\-',rep='',x=x1),gsub(patt='\\-',rep='',x=x2)))
}
pad1 <-
function(x) {
  n1 <- nchar(x)
  x[n1==1] <- paste0(x[n1==1],paste(collapse ='',rep(rcs(),2)))
  x[n1==2] <- paste0(x[n1==2],rcs())
  x
}
parsepcode <-
function( #parse a vector of 'irregular' (normal) postcode
    pc=c('AL1 1AD','AL1 1BD','AL1 1CD')
    ) {
    x <- lapply(pc,ppc)%>%
      lapply(.,data.table)%>%
      lapply(.,t)%>%
      Reduce(rbind,.)%>%
      data.frame(.)%>%
      lapply(.,unlist)%>%
      suppressWarnings(.)
    x <- lapply(x,`names<-`,NULL)
    names(x) <- names(ppc(pc[1]))
    x
  }
ppc <-
function(pc='EC2R 8AH') {
  if(nchar(pc)<2) return(list(area=ifelse(grepl('[A-Z,a-z]',pc),paste0(toupper(pc),'--'),''),district='',sector='',unit=''))
  chkpcode(pc)
  pc <- toupper(pc)
  gg <- gregexpr(patt=' ',pc)
  x <- strsplit(pc,split=' ')
  out <- unlist(lapply(x,'[[',1))
  nout <- nchar(out)
  inum <- as.numeric(regexpr("[0-9]",out))
  area <- pc
  sector <- unit <- district <- rep('',length(pc))
  area[inum==2] <- substr(out[inum==2],1,1)
  area[inum==3] <- substr(out[inum==3],1,2)
  district[inum==2] <- substring(out[inum==2],2)
  district[inum==3] <- substring(out[inum==3],3)
  if(any(lapply(x,length)>1)) { #inbound code exists
    stopifnot(all(lapply(x,length)==2)) #exists for all
    inb <- unlist(lapply(x,'[[',2))
    nin <- nchar(inb)
    sector <- substr(inb,1,1)
    unit <- substring(inb,2,nin)
  }
  list(area=area,district=district,sector=sector,unit=unit)
}
rcs <-
function(){'-'}
regpcode <-
function(#parse irregular postcode to regular 12-char (area,district,sector,unit)
    rawcode=c('AL1 1AD','AL1 1BD','AL1 1CD'),x=parsepcode(rawcode)) {
    rawcode <- gsub(patt='  ',rep=' ',rawcode)
    Reduce(paste0,lapply(x,pad1))
  }
rmifgl <-
function( #remove if global
  x #character=names of non-function objects in .GlobalEnv
  ) {
  for(i in seq_along(x)) {
    if(
      exists(x[i],envir=globalenv())
      &&
      mode(get(x[i],envir=globalenv()))!='function'
    ) {
      rm(list=x[i],envir=globalenv())
      }
  }
}
