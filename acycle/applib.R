 steprip  <- '../v153/now/ver001/03rip'#'now\\ver001\\03rip'
  
  dfnx <-
    structure(c(9130, 9628, 9835, 10012, 10150, 10405, 10687, 10822,
                10939, 10997, 11546, 11792, 11923, 12016, 12136, 12207, 12275,
                12354, 12435, 12525, 12576, 12721, 12918, 13335, 13541, 13651,
                13878, 14303, 14410, 14639, 15050, 15251, 15537, 15967, 16201,
                16316, 16524, 16725, 16815, 16954, 17083, 17356, 17624, 18266,
                18801, 19111, 19813), class = "Date") #2
  geo0 = 
    c("AL-", "B--", "BA-", "BB-", "BD-", "BH-", "BL-", "BN-", "BR-", 
      "BS-", "CA-", "CB-", "CF-", "CH-", "CM-", "CO-", "CR-", "CT-", 
      "CV-", "CW-", "DA-", "DE-", "DH-", "DL-", "DN-", "DT-", "DY-", 
      "E--", "EC-", "EN-", "EX-", "FY-", "GL-", "GU-", "HA-", "HD-", 
      "HG-", "HP-", "HR-", "HU-", "HX-", "IG-", "IP-", "KT-", "L--", 
      "LA-", "LD-", "LE-", "LL-", "LN-", "LS-", "LU-", "M--", "ME-", 
      "MK-", "N--", "NE-", "NG-", "NN-", "NP-", "NR-", "NW-", "OL-", 
      "OX-", "PE-", "PL-", "PO-", "PR-", "RG-", "RH-", "RM-", "S--", 
      "SA-", "SE-", "SG-", "SK-", "SL-", "SM-", "SN-", "SO-", "SP-", 
      "SR-", "SS-", "ST-", "SW-", "SY-", "TA-", "TF-", "TN-", "TQ-", 
      "TR-", "TS-", "TW-", "UB-", "W--", "WA-", "WC-", "WD-", "WF-", 
      "WN-", "WR-", "WS-", "WV-", "YO-")%>% 
    data.table(rc9=.,nx=seq_along(.),lab=.)

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
    steprip=steprip, 
    #steprii='ver001\\08rii',#optional write: inliers id
    #riiwrite=F, #flag to write inlier id
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
        colClasses=list(numeric=c('retraw','retsa'),Date=c('buydate','selldate'))
      )
    x2 <- f221209a(
        geo=geo[nx==nxx], 
        fur=x1,
        dfn=dfn,
        applygeo=F
      )
    # x2 <- #xy 
    #   x2a[,!c('idhash.selldate','rc9')]
    
    x4 <- lm(
      retsa~.-1,
      x2[,!c('idhash.selldate','rc9')] #all
    )
    x5 <- residuals(x4)
    
    
    #browser()
    #this line replaced 231209 for the next 4, stratified sampling by rc6
    #x6 <- x2[!(x4<quantile(x4,q1/2)|x4>quantile(x4,1-(q1/2)))] #select inlier
    x6a <- x2[,.(idhash.selldate,rc6=substr(rc9,1,6),res=x5)] #per rc6
    x6b <- x6a[,.(lo=quantile(res,outthresh/2),hi=quantile(res,(1-outthresh/2))),rc6] #apply thresholds
    x6c <- x6b[x6a,on=c(rc6='rc6')][(lo<res&res<hi),.(rc6,idhash.selldate)] #select inlier
    x6d <- x2[x6c[,.(idhash.selldate)],on=c(idhash.selldate='idhash.selldate')]
    x6e <- x6d[,!c('idhash.selldate','rc9')]
    #browser()
    # if(riiwrite) {
    #   cowrite(x0=x6d,dirnam=steprii,newdir=F,format='csv',key='rc9')
    # }
    
    
    x6 <- lm(
      retsa~.-1,
      #x2[!(x5<quantile(x5,outthresh/2)|x5>quantile(x5,1-(outthresh/2)))])%>%
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

 f240810a <- 
   function(
    zoomx=6.5,
    x3a=pxosrdo2dd,#rc6 polygons 
    rcx=c('NG-7--','NG-2--'),
    minzoom=6
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
      .[,.(name,rc6)]
    x3@data <- data.frame(x4)
    x7 <- leaflet(
      x3,
      width=width,
      height=height,
      options=leafletOptions(
        zoomControl=F,
        minZoom=minzoom,
        maxZoom=16,
        zoomDelta=2
      )
    )
    x8 <- x7 %>% addPolygons(
      stroke=T,
      smoothFactor = 0, #remove white slivers
      weight = 1,
      opacity = 1,
      color = "steelblue",
      dashArray = "2",
      fillOpacity = .5,
      fillColor='transparent'
    )
    x8%>%
      addProviderTiles(providers$CartoDB.Positron)
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

 
 cobalt <- function(){
c(
  blue='#0082F4',
  green='#35CA05',
  onch='#ED9304',
  punk='#FF628C',
  midnight='002140')
 }
 
 regpcode <-
function(#parse irregular postcode to regular 12-char (area,district,sector,unit)
    rawcode=c('AL1 1AD','AL1 1BD','AL1 1CD'),x=parsepcode(rawcode)) {
    rawcode <- gsub(patt='  ',rep=' ',rawcode)
    Reduce(paste0,lapply(x,pad1))
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
chkpcode <-
function(pc='EC2R 8AH') {
  #composed of correct chars
  #grepl(patt='[^a-zA-Z0-9]/',x=pc,perl=TRUE)
  #right length
  nch <- sapply(pc,nchar)
  stopifnot(all(nch<=8))
  #max one space
  stopifnot(unlist(lapply(gregexpr(patt=' ',pc),length))==1)
  #is either 1-part or 2-part
  x <- strsplit(pc,split=' ')
  #stopifnot(all(lapply(x,length)==1)||all(lapply(x,length)==2))
  #1-part always starts with alpha cap
  if(length(x[[1]])==1) {
    stopifnot(all(unlist(gregexpr(pc,patt='^[A-Z,a-z]'))==1))
  }
  #2-part always starts with number
  if(length(x[[1]])==2) {
    pcin <- lapply(x,'[[',2)
    if(!all(unlist(gregexpr(pcin,patt='^[0-9]'))==1)) browser()
    stopifnot(all(unlist(gregexpr(pcin,patt='^[0-9]'))==1))
  }
  TRUE
}
rcs <-
function(){'-'}
regpcode <-
function(#parse irregular postcode to regular 12-char (area,district,sector,unit)
    rawcode=c('AL1 1AD','AL1 1BD','AL1 1CD'),x=parsepcode(rawcode)) {
    rawcode <- gsub(patt='  ',rep=' ',rawcode)
    Reduce(paste0,lapply(x,pad1))
}


