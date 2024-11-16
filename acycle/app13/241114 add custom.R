
nxcuR <- 
  eventReactive( 
    geocuR(),
    {
      geocuR()[,.(nx,rc3,qtile,lab)]%>%
        unique(.)
    }
  )

rsicuR <- #custom rsi
  eventReactive( 
    list(
      geocuR(),
      estdtaR() #for dates
    ),
    {
      geox <- geocuR()
      dfnx <- estdtaG[,sort(unique(date))]
      x <- 
        f240710a(  #returns estdt, kfoldsse, all
          nxx=0,
          stepripx='app241115/03rip', 
          dfn=dfnx,    #R
          geo=geox, #R
          outthresh=.1,
          kfold=5,
          sectorwise=T, 
          usepra=F, 
          newused=c('.','N','U'),
          houseflat=c('.','H','F') 
        )
      rsicuG <<- copy(x)
      x
    }
  )
estdtcuR <- 
  eventReactive( 
    list(
      rsicuR()
    ),
    {
      x <- rsicuR()$estdt
      estdtcuG <<- copy(x)
      x
    }
  )

rsscuR <- 
  eventReactive( 
    list(
      rsicuR()
    ),
    {
      x <- rsicuR()$rss
      rsscuG <<- copy(x)
      x
    }
  )



rc6tC <- 'NG-7--'
geo0 <- data.table(gx=0,itrim=itriC,tbin=tbinC,nx=0,lab='CUS000',rc9=rc6tC,des='CUS0',typex='C')

dateR <- 
  reactive(
    estdt[tbin==2,.(sort(unique(date)))]
  )

geo0R <- 
  eventReactive(
    eventExpr=rc6tR(),
    valueExpr=
      data.table(
        gx=0,
        itrim=itriC,
        tbin=tbinC,
        nx=0,
        lab='CUS000',
        rc9=pc6tR(),
        des='CUS0',
        typex='C'
      )
  )

nx0R <- 
  eventReactive( 
    geo0R(),
    {
      geo0R()[,.(nx,rc3,qtile,lab)]%>%
        unique(.)
    }
  )

rsi0R <- 
  eventReactive( 
    list(
      geo0R(),
      pcaR()
    ),
    {
      x <- f230312x(  #solve single nx -> estdt with no pra
        nxx=1,
        steprip='03rip\\',
        dfn=dateR(),
        geo=geo0R()
      )
      x
    }
  )



geoplus[type=='L'][nx<13]

geo
f240710a(  #solve single nx -> estdt with no pra
    nxx=0,
    stepripx='app241115/03rip', 
    dfn=estdtaG[,sort(unique(date))],    
    geo=geocuG,
    outthresh=.1,
    kfold=5,
    sectorwise=T, #flag to split folds on complete sectors<<<<<<<<<<<<< T
    usepra=F, #optional<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< F
    newused=c('.','N','U'),
    houseflat=c('.','H','F') #typex field added to rip 240826, values UH/NH/UF/NF for new/house
  )
coread('E--14-',stepripx)
