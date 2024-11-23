library(broom)
library(bslib)
library(car) #linear hypothesis test
library(colorspace)
library(data.table)
library(devtools)
library(DT)
library(ggplot2)    
library(ggrepel)   
library(grid)
library(gt)
library(leaflet)
library(lubridate)
library(magrittr)   
library(PerformanceAnalytics)
library(scales)
library(shinyWidgets)
library(sp)
library(zoo)
library(shiny)
library(plotly)
library(gridlayout)
source('c-cleanlib.R')          #2
source('rctree.R')

#load('app241121/.RData') #see app7   #3
load('.RData') #see app7   #3
load('pxosrdo2dd.RData')   #4

#--------------------------------ui
gridheight="630px"
gridheight2="830px"
colx <<- cobalt()[c(4,2,1)]
sf <<- 3
pgmc <<- 'grey50'

#--------------------------------Pseudo=Control----2----
hoflC   <<- c('house','flat','all')[3]#,
itriC   <<- c('.0'=1,'.1'=2,'.5'=3)[2]#, #Trim ---
neusC   <<- c('new','used','all')[3]#,
rc3coC  <<- c('B--','E--','AL-')#,  #comp
rc6cuC  <<- c('E--14-','E--1W-')#, #custom
tbinC   <<- c(lo=1,hi=2,an=3)[2]#,  #lo hi an ---
typeC   <<- c('A','L','N','C')[2]#, #All Local National ---
typerC  <<- typeC
nfig2   <<- -1 #for ppm2
nfig3   <<- 4 #for frac
verbose <<- T

zerorefC =F#, #set reference asset NULL
showtradetriangle=F

#---ui   section
ui <-      #ui----
grid_page( ###grid page ----
           layout = c(
             "header  header",
             "sidebar area2 "
           ),
           row_sizes = c(
             "100px",
             "1fr"
           ),
           col_sizes = c(
             "250px",
             "1fr"
           ),
           gap_size = "1rem",
           grid_card(
             area = "sidebar",
             card_header("Settings"),
             card_body(
               textInput(
                 inputId = "rc6tC",
                 label = "District",
                 value = irregpcode(rc6cuC[1])
               ),
               sliderInput(
                 inputId = "tslider",
                 label = "Datum period",
                 min=0,
                 max=45,
                 value=27
               ),
               treeInput( #districts
                 inputId = "rctreeC",
                 label = "Custom selection",
                 choices = create_tree(f240824b(unique(substr(dir('smallrip/'),1,3)))),
                 selected = rc6cuC, 
                 returnValue = "text",
                 closeDepth = 0
               )
               
             )
           ),#grid card end
           grid_card_text(
             area = "header",
             content = "Local and custom index",
             alignment = "start",
             is_title = FALSE
           ),
           grid_card(
             area = "area2",
             card_body(
               tabsetPanel(#----
                           nav_panel(title = "Time-series summary",#----
                                     
                                     grid_container(
                                       layout = c(
                                         "x111     xtimeseries    ",
                                         "Winding characteristics",
                                         "summary tradesummary   "
                                       ),
                                       row_sizes = c(
                                         "1fr",
                                         ".7fr",
                                         "1fr"
                                       ),
                                       col_sizes = c(
                                         "1fr",
                                         "1fr"
                                       ),
                                       gap_size = "10px",
                                       grid_card( #1,1-------------card
                                         area = "x111", 
                                         full_screen = TRUE,
                                         card_header(
                                           "Postcode area map"
                                         ),
                                         card_body(#
                                           leaflet::leafletOutput('x111'),
                                           height=gridheight
                                         )
                                       ),
                                       grid_card(
                                         area = "xtimeseries", #1.1 -------x112
                                         full_screen = TRUE,
                                         card_header(
                                           "Indices"
                                         ),
                                         card_body(#--------------x112
                                           plotOutput('x112'),
                                           height=gridheight
                                         )
                                         
                                       ),
                                       grid_card(
                                         area = "Winding",
                                         full_screen = TRUE,
                                         card_header(
                                           "Price return summary"
                                         ),
                                         card_body(#--------------x121
                                           gt::gt_output('x121a'),
                                           gt::gt_output('x121b'),
                                           height=gridheight
                                         )
                                         
                                       ),
                                       grid_card(
                                         area = "characteristics",
                                         full_screen = TRUE,
                                         card_header(
                                           "Index characteristics"
                                         ),
                                         card_body(#--------------x122
                                           gt::gt_output('x122')#,
                                           #height=gridheight
                                         )
                                       ),
                                       grid_card(
                                         area = "summary",
                                         full_screen = TRUE,
                                         card_header(
                                           "Summary"
                                         ),
                                         card_body(#--------------x131
                                           gt::gt_output('x131'),
                                           height=gridheight
                                         )
                                       )
                                       ,
                                       grid_card(
                                         area = "tradesummary",
                                         full_screen = TRUE,
                                         card_header(
                                           "Trade recap"
                                         ),
                                         card_body(#--------------x132
                                           gt::gt_output('x132a'),
                                           gt::gt_output('x132b'),
                                           gt::gt_output('x132c'),
                                           gt::gt_output('x132d'),
                                           height=gridheight2
                                         )
                                       )
                                       #   )
                                       # )
                                     )
                           ),
                           nav_panel(title = "Listing",#----
                                     
                                     grid_container(
                                       layout = c(
                                         "locallist  customlist"
                                       ),
                                       row_sizes = c(
                                         "1fr"
                                       ),
                                       col_sizes = c(
                                         "1fr",
                                         "1fr"
                                       ),
                                       grid_card(
                                         area='locallist',
                                         full_screen = TRUE,
                                         card_header(
                                           "Local"
                                         ),
                                         card_body(#--------------x211
                                           gt::gt_output('x211')
                                         )
                                       ),
                                       grid_card(
                                         area='customlist',
                                         full_screen = TRUE,
                                         card_header(
                                           "Custom"
                                         ),
                                         card_body(#--------------x211cu
                                           gt::gt_output('x211cu')
                                         )
                                       )
                                     )
                           ),
                           nav_panel(title = "Constituents",#----
                                     card(
                                       full_screen = TRUE,
                                       card_header(
                                         "Constituent districts"
                                       ),
                                       card_body(#--------------x311
                                         DT::DTOutput('x311')
                                       ),
                                       height=gridheight2
                                     )
                           ),
                           nav_panel(title = "Accuracy", #----
                                     card(
                                       full_screen = TRUE,
                                       card_header("RMS error sensitivity to key parameters"),
                                       card_body(
                                         grid_container(
                                           layout = c(
                                             "timesamplinglocal      timesamplingcustom",
                                             "outlierrejectionlocal  outlierrejectioncustom",
                                             "crossvalidationlocal   crossvalidationcustom"#,
                                           ),
                                           row_sizes = c(
                                             "1fr",
                                             "1fr",
                                             "1fr"#,
                                           ),
                                           col_sizes = c(
                                             "1fr",
                                             "1fr"
                                           ),
                                           gap_size = "10px",
                                           grid_card(
                                             area = "timesamplinglocal",
                                             full_screen = TRUE,
                                             card_header(
                                               "Time Sampling"
                                             ),
                                             card_body(#--------------x411
                                               gt::gt_output('x411')
                                             )
                                           ),
                                           grid_card(
                                             area = "timesamplingcustom",
                                             full_screen = TRUE,
                                             card_header(
                                               "."
                                             ),
                                             card_body(#--------------x411cu
                                               gt::gt_output('x411cu')
                                             )
                                           ),
                                           grid_card(
                                             area = "outlierrejectionlocal",
                                             full_screen = TRUE,
                                             card_header(
                                               "Outlier Rejection"
                                             ),
                                             card_body(#--------------x421
                                               gt::gt_output('x421')
                                             )
                                           ),
                                           
                                           grid_card(
                                             area = "outlierrejectioncustom",
                                             full_screen = TRUE,
                                             card_header(
                                               "."
                                             ),
                                             card_body(#--------------x411cu
                                               gt::gt_output('x421cu')
                                             )
                                           ),
                                           grid_card(
                                             area = "crossvalidationlocal",
                                             full_screen = TRUE,
                                             card_header(
                                               "Cross Validation"
                                             ),
                                             card_body(#--------------x431
                                               gt::gt_output('x431')
                                             )
                                           ),
                                           grid_card(
                                             area = "crossvalidationcustom",
                                             full_screen = TRUE,
                                             card_header(
                                               "Cross Validation"
                                             ),
                                             card_body(#--------------x431cu
                                               gt::gt_output('x431cu')
                                             )
                                           )
                                         )
                                       )
                                     )
                           ),
                           nav_panel(title = "Notes",#----
                                     
                                     card(
                                       full_screen = TRUE,
                                       card_body(
                                         htmltools::includeMarkdown("notes.Rmd")
                                       )
                                     )
                           )
               )
             )
           )
)###----
#---server   section
server <-  #server----
function(input, output) {
  #---global   section----
  x00R <- reactive({
    x <- copy(f241021ad)
    x00G <<- copy(x)
    x
  })
  geoplusR <- reactive({
    x <- copy(x00R()$geoplus)[,let(lab,des)]
    geoplusG <<- copy(x)
    x
  })
  estdtR <- reactive({
    x <- copy(x00R()$estdt)[,.(nx,ii,date,xdotd,days,xdot,x)]
    estdtG <<- copy(x)
    x
  })
  rssR  <- reactive({
    x <- copy(x00R()$rss)
    rssG <<- copy(x)
    x
  })
  pxosrdo2ddR <- reactive({
    x <- copy(pxosrdo2dd)
    pxosrdo2ddG <<- copy(x)
    x
  })
  z110R <- reactive({
    x <- copy(z110)
    z110G <<- copy(x)
    x
  })
  x101R <- reactive({
    x <- copy(x101) #initial dates
    x101G <<- copy(x)
    x
  })
  geo0R <- reactive({
    x <- 
      geoplusR()%>%
      .[type=='L']%>%
      .[itrim==itriC]%>%
      .[tbin==tbinC]%>%
      .[,.(
        nx,
        gx,
        lab=des,
        rc6=rc9,
        rc3=substr(rc9,1,3),
        qtile=as.numeric(substr(des,4,4)))
      ]%>%
      z110R()[.,on=c(rcx='rc6')]%>%
      .[,.(nx,gx,lab,rc3,rc6=rcx,qtile)]
    geo0G <<- copy(x)
    x
  })
  dfnR <- reactive({ #not used?
    x <- data.table(date=c(as.Date('1994-12-31'),estdtR()[,sort(unique(date))]))[,let(i,(0:(.N-1)))]
    dfnG <<- copy(x)
    x
  })
  dfnxR <- reactive({ #4-col table with NA
    x <- 
      dcast(x00R()$estdt[,.(tbin,date)]%>%unique(.)%>%.[order(tbin,date)],date~tbin,value.var='date')%>% #lo, hi, an
      rbind(as.data.table(as.list(rep(as.Date('1994-12-31'),4))),.,use.names=F)%>%
      setnames(.,c('date','tbin1','tbin2','tbin3'))
    dfnxG <<- copy(x)
    x
  })
  dfnxxR <- reactive({ #vector of current date
    x <-
      dfnxR()[,paste0('tbin',tbinC),with=F]%>%setnames(.,'x')%>%.[,sort(unique(x))]
    dfnxxG <<- copy(x)
    x
  })
  
  tslideR <- reactive({
    x <- input$tslider
    tslideG <<- copy(x)
    x
  })
  
  
  dfnF <- #date extractor using globals
    function(nn=c('tbinC','dfnxG')) {
      x <- dfnxR()[,paste0('tbin',tbinC),with=F]%>%setnames(.,'x')%>%.[,sort(unique(x))]
      x #vector of Date 0:tbar
    }
  
  #------------------------reactive + global
  #---target   section----
  geotR <-     #---target geo compute   ----
  eventReactive( 
    rc6tR(),
    {
      if(verbose) print('enter geotR')
      x <- 
        geoaR()%>%
        .[rc6==rc6tR()]
      geotG <<- copy(x)
      x
    }
  )
  
  rc6tR <-     #---target rc6 reformat  ----
  eventReactive( 
    input$rc6tC,
    {
      if(verbose) print('enter rc6tR')
      x <- 
        regpcode(input$rc6tC)
      rc6tG <<- copy(x)
      x
    }
  )
  #---custom   section----
  
  rc6cuR <-    #---custom rc6 control   ----
  eventReactive( 
    list(rc6tR(),input$rctreeC), #+control
    {
      if(verbose) print('enter rc6cuR')
      x <- sort(unique(c(rc6tR(),input$rctreeC)))
      print(x)
      rc6cuG <<- copy(x)
      x
    }
  )
  geocuR <-    #---custom geo compute   -----
  eventReactive( 
    rc6cuR(),
    {
      if(verbose) print('enter geocuR')
      x <- 
        data.table(rc9=rc6cuR(),nx=0,lab='CU00') 
      geocuG <<- copy(x)
      x
    }
  )
  nxcuR <-     #---custom nx compute    ----
  eventReactive( 
    geocuR(),
    {
      if(verbose) print('enter nxcuR')
      x <- 
        geocuR()[,.(nx,rc3,qtile,lab)]%>%
        unique(.)
      nxcuG <<- copy(x)
      x
    }
  )
  rsicuR <-    #---custom rsi compute   ----
  eventReactive( 
    list(
      geocuR(),
      estdtaR(), #for dates
      dfnxxR()
    ),
    {
      if(verbose) print('enter rsicuR')
      geox <- geocuR()
      dfnx <- dfnxxR() #source of truth
      x <- 
        f241119a(  #returns estdt, kfoldsse, all
          nxx=0,
          steprip2='smallrip/',  #smaller format
          dfn=dfnx,    #R
          geo=geox, #R
          outthresh=.1,
          kfold=5,
          sectorwise=T, 
          usepra=F, 
          newused=c('.'),
          houseflat=c('.') 
        )
      rsicuG <<- copy(x)
      x
    }
  )
  estdtcuR <-  #---custom estdt select  ----
  eventReactive( 
    list(
      rsicuR()
    ),
    {
      if(verbose) print('enter estdtcuR')
      x <- rsicuR()$estdt
      estdtcuG <<- copy(x)
      x
    }
  )
  rsscuR <-    #---custom rss select    ----
  eventReactive( 
    list(
      rsicuR()
    ),
    {
      if(verbose) print('enter rsscuR')
      x <- cbind(rsicuR()$kfoldsse,rsicuR()$all)
      rsscuG <<- copy(x)
      x
    }
  )
  #---qtile    section----
  geoqR <-     #---qtile geo select     ----
  eventReactive( 
    list(geoaR(),geotR()#,
    ),
    {
      if(verbose) print('enter geoqR')
      x <- geoaR()%>% 
        .[geotR()[,.(qtile)],
          on=c(qtile='qtile')]
      geoqG <<- copy(x)
      x
    }
  )
  nxqR <-      #---qtile nx compute     ----
  eventReactive( 
    geoqR(),
    {
      if(verbose) print('enter nxqR')
      x<- 
        geoqR()[,.(nx,rc3,qtile,lab)]%>%
        unique(.)
      nxqG <<- copy(x)
      x
    }
  )
  estdtlR <-   #---local estdt compute  ----
  eventReactive( 
    nxqR(),
    {
      if(verbose) print('enter estdtlR')
      x <- 
        estdtR()[nxqR(),on=c(nx='nx')]%>%
        .[,.(nx,date,ii,lab,rc3,qtile,xdotd,days,xdot,x)]
      estdtlG <<- copy(x)
      x
    }
  )
  #---area     section----
  geoaR <-     #---area geo compute     ----
  eventReactive( 
    rc6tR(),
    {
      if(verbose) print('enter geoaR')
      #browser()
      x <- geo0R()%>%
        .[rc3==substr(rc6tR(),1,3)] 
      geoaG <<- copy(x)
      x
    }
  )
  nxaR <-      #---area nx select       ----
  eventReactive( 
    geoaR(),
    {
      if(verbose) print('enter nxaR')
      x <- 
        geoaR()[,.(nx,rc3,qtile,lab)]%>%
        unique(.)
      nxaG <<- copy(x)
      x
    }
  )
  estdtaR <-   #---area estdt compute   ----
  eventReactive( 
    nxaR(),
    {
      if(verbose) print('enter estdtaR')
      x <- 
        estdtR()[nxaR(),on=c(nx='nx')]%>%
        .[,.(nx,date,ii,lab,rc3,qtile,xdotd,days,xdot,x)]
      estdtaG <<- copy(x)
      x
    }
  )
  rssaR <-     #---area rss compute     ----
  eventReactive( 
    nxaR(),
    {
      if(verbose) print('enter rssaR')
      x <- 
        rssR()[nxaR(),on=c(nx='nx')]
      rssaG <<- copy(x)
      x
    }
  )
  #---combo    section                  ----
  estdtxR <- #----112 x(t)              ----
  eventReactive(
    list(estdtcuR(),estdtaR())
    ,
    {
      x <- 
        rbind(
          estdtcuR()[,.(nx,date,xdotd,days,xdot,x,lab,ii,qtile=0,rc3=geocuR()[,substr(rc9,1,3)])],
          estdtaR() [,.(nx,date,xdotd,days,xdot,x,lab,ii,qtile,rc3)]
        )[,qq:=as.factor(qtile)]
      estdtxG <<- copy(x)
      x
    }
  )
  
  #---utility  section----
  ylimR <-     #ylim                    ----
  eventReactive( 
    estdtxR(),
    {
      x <- 
        estdtxR()[,range(x)]*1.1
      ylimG <<- copy(x)
      x
    }
  )
  #---display  section----
  #---Time-series summary 6 displays----
  
  f111D <- function(
    rc6tX=rc6tG,
    rc6cuX=rc6cuG,
    geoaX=geoaG,
    pxosrdo2ddX=pxosrdo2ddG,
    z110X=z110G,
    colX=colx #punk green blue
  ) {
    x <-
      geoaX%>%
      .[,.(
        rc6,
        col=lighten(colx,.7)[.BY[[1]]],
        qtile, #shade tiles light
        lab
      ),by=.(qtile)]%>%
      .[
        rc6==rc6tX, #with target district darker
        col:=colX[.BY[[1]]],
        by=.(qtile)
      ]%>%
      f240810b( #->leaflet, colours for areas-to-shade in column 'col'
        .,
        x2=pxosrdo2ddX,
        pva=z110X,
        minzoom=9
      )%>%
      addPolygons( #outline custom districts
        data=pxosrdo2ddX[which(pxosrdo2ddX@data$name%in%irregpcode(rc6cuX)),],
        fill=F,
        color="orange",
        weight=1,
        opacity=1
      )
    x
  }
  #f111D()
  x111D <- eventReactive(list(rc6tR(),rc6cuR(),geoaR(),pxosrdo2ddR(),z110R()),       #111 map ----
                         {
                           if(verbose) print('enter x111D')
                           x <-   f111D(
                             rc6tX=rc6tR(),
                             rc6cuX=rc6cuR(),
                             geoaX=geoaR(),
                             pxosrdo2ddX=pxosrdo2ddR(),
                             z110X=z110R(),
                             colX=colx #punk green blue
                           )
                           x111G <<- copy(x)
                           x
                         }
  )
  
  f112D <- function(
    tslideX=tslideG,
    estdtxX=estdtxG,
    ylimX=ylimG
  )
  {
    x2c <- estdtxX%>%
      .[,.SD[,.(ii,date,lab,x=x-ifelse(tslideX==0,0,x[tslideX]))],.(qtile)]%>%
      .[,.SD[,.(ii,date,lab,x)],.(qtile)]%>%
      .[,qq:=as.factor(qtile)]%>%
      .[,labx:=ifelse(date==max(date),lab,NA)]
    x0 <- setNames(cobalt()[c('punk','green','blue')],as.character(1:3))
    x3 <- estdtxX[,.SD[,.(ifelse(tslideX==0,0,x[tslideX]))],.(qtile)][,mean(V1)]#base value for rebase level
    x2 <-
      estdtxX[,.SD[,.(ii,date,lab,x=x-ifelse(tslideX==0,0,x[tslideX]))],.(qtile)]%>%
      .[,qq:=as.factor(qtile)]%>%
      .[,labx:=ifelse(date==max(date),lab,NA)]
    x <- x2%>%
      ggplot(.,aes(date,x,color=qq,label=labx))+
      geom_hline(yintercept=0,linewidth=.4,linetype = "dotted",color='grey40')+
      geom_line()+
      geom_point(size=.3)+
      geom_text_repel()+
      ylim(ylimX-x3)+
      xlab('')+
      ylab(bquote(Delta~P~log~price~change))+
      theme_bw() +
      theme(
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size=.2,linetype = 'dotted',color=pgmc),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size=16,face='plain'),
        axis.line.y.left=element_line(size=.1),
        axis.line.x.bottom=element_line(size=.1),
        legend.position='none')+
      scale_color_manual(values=x0)+
      scale_x_date(
        breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
        date_labels = "%Y",
        limits=c(as.Date(c('1994-12-31','2027-12-31')))
      )
    x
  }
  #f112D()
  
  x112D <- eventReactive(list(input$tslider,estdtxR(),ylimR()),#112 x(t)----
                         {
                           if(verbose) print('enter x112D')
                           x <- 
                             f112D(tslideX=tslideR(),
                                   estdtxX=estdtxR(),
                                   ylimX=ylimR())
                           x112G <<- copy(x)
                           x
                         }
  )
  
  f121D <- function(estdtX =estdtlG,dfnxX  =dfnxG, #----
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
  
  x121D <- eventReactive(list(estdtlR(),estdtcuR(),dfnxxR()),  #121 winding----
                         {
                           if(verbose) print('enter x121D')
                           x2 <- f121D(estdt=estdtlR(),
                                       dfnxX=dfnxR())
                           x4 <- f121D(estdt=estdtcuR(),
                                       dfnxX=dfnxR())
                           x <- list(x2,x4)
                           x121G <<- copy(x)
                           x
                         }
  )
  
  
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
            p=prettyNum(round(p,nfig3), big.mark=","),
            p.cus=paste0(prettyNum(round(ppm2min,nfig2), big.mark=","),'-',prettyNum(round(ppm2max,nfig2), big.mark=","))
          )]
      }
    x0 <- f122(rssx=rsscux,z110X=z110X)
    x1 <- f122(rssx=rssaX,z110X=z110X)
    x2 <- 
      rbind(x1,x0)[order(-p)][]
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
  
  x122D <- eventReactive(list(rc6tR(),rssaR(),rsscuR(),z110R()), #122 characteristics----
                         {
                           if(verbose) print('enter x122D')
                           x <- f122D(
                             rc6tX=rc6tR(),
                             rssaX=rssaR(),
                             rsscuX=rsscuR(),
                             z110X=z110R()
                           )
                           x122G <<- copy(x)
                           x
                         }
  )
  
  f131D <- function(#131 summary----
                    estdtxX=estdtxG,
                    tslideX=tslideG
  )
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
  
  x131D <- eventReactive(list(tslideR(),estdtxR()),#131 summary----
                         {
                           if(verbose) print('enter x131D')
                           x <-  f131D(
                             estdtxX=estdtxR(),
                             tslideX=tslideR()
                           )
                           x131G <<- copy(x)
                           x
                         }
  )
  
  f132 <- function(
    geox=geoqG,
    steprip='smallrip/',
    estdtlx=estdtlG, #oE14nly used for its date(ii) relation
    tmin=20
  ) {#tmin=input$tslider
    x0 <-
      geox[,rc6]%>%
      coread2(.,steprip)%>% #or rc6tC
      .[,.(N=.N,mean=round(mean(as.numeric(retsa)),4)),.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))]%>%
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
  
  x132D <- eventReactive(list(tslideR(),geoqR(),estdtlR()),#132 trade summary(2)----
                         {
                           if(verbose) print('enter x132D')
                           x <- f132D(
                             tslideX=tslideR(),
                             geoqX=geoqR(),
                             geocuX=geocuR(),
                             estdtlX=estdtlR()
                           )
                           x132G <<- copy(x)
                           if(verbose) print('exit x132D')
                           x
                         }
  )
  
  #------------------custom accuracy
  x411D <- eventReactive(list(rc6cuR(),geoqR()),        #2x11 accuracy----tbin----
                         {
                           if(verbose) print('enter x411G')
                           pc6tx <- rc6tR()
                           x1 <-
                             data.table(tbin=1:3,freq=c('lo','hi','an'))
                           x2 <- 
                             rssG%>% #use global no filters
                             .[geoqR(),on=c(rc6='rc6')]%>%
                             .[type=='L']%>%
                             .[itrim==itriC]%>%
                             .[,.(n=sum(n),ssek=sum(ssek)),.(tbin,rc6)]
                           x3 <-
                             rbind(
                               x2[,.(span='index.average',mse=round(sqrt(sum(ssek)/sum(n)),4)),tbin],
                               x2[rc6==pc6tx,.(span=pc6tx,mse=round(sqrt(sum(ssek)/sum(n)),4)),tbin]
                             )%>%
                             dcast(.,tbin~span,value.var='mse')%>%
                             x1[.,on=c(tbin='tbin')]%>%
                             .[,-'tbin']
                           x <- 
                             gt::gt(x3)%>%
                             gt::tab_footnote(
                               footnote=f241108a(typeC,tbinC)[[1]]
                             )
                           x411G <<- copy(x)
                           x
                         }
  )
  
  x421D <- eventReactive(list(rc6cuR(),geoqR()),        #221 accuracy----trim----
                         {
                           if(verbose) print('enter x421D')
                           pc6tx <- rc6tR()
                           x1 <-
                             data.table(itrim=1:3,threshold=c('0.0','0.1','0.5'))
                           x2 <-
                             rssG%>%
                             .[geoqR(),on=c(rc6='rc6')]%>%
                             .[type=='L']%>%
                             .[tbin==tbinC]%>%
                             .[,.(n=sum(n),ssek=sum(ssek)),.(itrim,rc6)]
                           x3 <- rbind(
                             x2[,.(span='index.average',mse=round(sqrt(sum(ssek)/sum(n)),4)),itrim],
                             x2[rc6==pc6tx,.(span=pc6tx,mse=round(sqrt(sum(ssek)/sum(n)),4)),itrim]
                           )%>%
                             dcast(.,itrim~span,value.var='mse')%>%
                             x1[.,on=c(itrim='itrim')]%>%
                             .[,-'itrim']
                           x <- 
                             gt::gt(x3)%>%gt::tab_footnote(
                               footnote=f241108a(typeC,tbinC)[[1]]
                             )%>%gt::tab_footnote(
                               footnote=f241108a(typeC,tbinC)[[2]]
                             )
                           x421G <<- copy(x)
                           x
                         }
  )
  
  
  x431D <- eventReactive(list(rc6cuR(),geoqR()),        #231 accuracy----in/out----
                         {
                           if(verbose) print('enter x431D')
                           pc6tx <- rc6tR()
                           x1 <-
                             rssG%>%
                             .[geoqR(),on=c(rc6='rc6')]%>%
                             .[type=='L']%>%
                             .[tbin==tbinC]%>%
                             .[itrim==itriC]%>%
                             .[,.(n=sum(n),ssek=sum(ssek),ssei=sum(ssei)),.(itrim,rc6)]
                           x2 <-
                             rbind(
                               x1[,.(outsamp=round(sqrt(sum(ssek)/sum(n)),4),insamp=round(sqrt(sum(ssei)/sum(n)),4))],
                               x1[rc6==pc6tx,.(outsamp=round(sqrt(sum(ssek)/sum(n)),4),insamp=round(sqrt(sum(ssei)/sum(n)),4))]
                             )%>%
                             as.matrix(.)%>%t(.)%>%as.data.table(.,keep.rownames=T)
                           setnames(x2,c('domain','index.average',rc6tR())[1:ncol(x2)])
                           if(ncol(x2)==3) x2 <- x2[,c(1,3,2)]
                           x <- 
                             gt::gt(x2)%>%gt::tab_footnote(
                               footnote=f241108a(typeC,tbinC)[[1]]
                             )%>%gt::tab_footnote(
                               footnote=f241108a(typeC,tbinC)[[2]]
                             )
                           x431G <<- copy(x)
                           x
                         }
  )
  
  
  x411cuD <- eventReactive(list(rc6cuR(),rssaR()),      #2x11cu accuracy--custom--tbin----
                           {
                             if(verbose) print('enter x411Gcu')
                             pc6tx <- rc6tR()
                             x1 <-
                               data.table(tbin=1:3,freq=c('lo','hi','an'))
                             x2 <- 
                               rsscuR()%>% #use global no filters
                               .[geocuR(),on=c(rc6='rc9')]%>%
                               .[,.(n,ssek,tbin=tbinC,rc6)]
                             #.[,.(n=sum(n),ssek=sum(ssek)),.(tbin=tbinC,rc6)] 
                             x3 <-
                               rbind(
                                 x2[,.(span='index.average',mse=round(sqrt(sum(ssek)/sum(n)),4)),tbin],
                                 x2[rc6==pc6tx,.(span=pc6tx,mse=round(sqrt(sum(ssek)/sum(n)),4)),tbin]
                               )%>%
                               dcast(.,tbin~span,value.var='mse')%>%
                               x1[.,on=c(tbin='tbin')]%>%
                               .[,-'tbin']
                             x <- 
                               gt::gt(x3)%>%
                               gt::tab_footnote(footnote=f241108a(tc='C',tbinC)[[1]])%>%
                               gt::tab_footnote(footnote=paste0('only freq=hi is computed for custom'))
                             x411cuG <<- copy(x)
                             x
                           }
  )
  
  
  x421cuD <- eventReactive(list(rc6cuR(),rssaR()),      #221cu accuracy----trim----
                           {
                             if(verbose) print('enter x421D')
                             pc6tx <- rc6tR()
                             x1 <-
                               data.table(itrim=1:3,threshold=c('0.0','0.1','0.5'))
                             x2 <-
                               rsscuR()%>%
                               .[geocuR(),on=c(rc6='rc9')]%>%
                               .[,.(n,ssek,itrim=itriC,rc6)]
                             #.[,.(n=sum(n),ssek=sum(ssek)),.(itrim=itriC,rc6)]
                             x3 <- rbind(
                               x2[,.(span='index.average',mse=round(sqrt(sum(ssek)/sum(n)),4)),itrim],
                               x2[rc6==pc6tx,.(span=pc6tx,mse=round(sqrt(sum(ssek)/sum(n)),4)),itrim]
                             )%>%
                               dcast(.,itrim~span,value.var='mse')%>%
                               x1[.,on=c(itrim='itrim')]%>%
                               .[,-'itrim']
                             x <- 
                               gt::gt(x3)%>%
                               gt::tab_footnote(footnote=f241108a(tc='C',tbinC)[[1]])%>%
                               gt::tab_footnote(footnote=paste0('only threshold=0.1 is computed for custom'))
                             x431cuG <<- copy(x)
                             x
                           }
  )
  
  x431cuD <- eventReactive(list(rc6cuR(),rssaR()),      #231cu accuracy----in/out----
                           {
                             if(verbose) print('enter x431cuD')
                             pc6tx <- rc6tR()
                             x1 <-
                               rsscuR()%>%
                               .[geocuR(),on=c(rc6='rc9')]%>%
                               .[,.(n,ssek,ssei,itrim=itriC,rc6)]
                             #.[,.(n=sum(n),ssek=sum(ssek),ssei=sum(ssei)),.(itrim=itriC,rc6)]
                             x2 <-
                               rbind(
                                 x1[,.(outsamp=round(sqrt(sum(ssek)/sum(n)),4),insamp=round(sqrt(sum(ssei)/sum(n)),4))],
                                 x1[rc6==pc6tx,.(outsamp=round(sqrt(sum(ssek)/sum(n)),4),insamp=round(sqrt(sum(ssei)/sum(n)),4))]
                               )%>%
                               as.matrix(.)%>%t(.)%>%as.data.table(.,keep.rownames=T)
                             setnames(x2,c('domain','index.average',rc6tR())[1:ncol(x2)])
                             if(ncol(x2)==3) x2 <- x2[,c(1,3,2)]
                             x <- 
                               gt::gt(x2)%>%
                               gt::tab_footnote(footnote=f241108a(tc='C',tbinC)[[1]])
                             x431cuG <<- copy(x)
                             x
                           }
  )
  #211 listing----
  f211D <- function(
    estdtlX=estdtlG, #single
    geoqX=geoqG, #footnote only 'this qtile'
    dfnxxX=dfnxxG #single
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
        footnote=f241108a(typeC,tbinC)[[1]]
      )%>%gt::tab_footnote(
        footnote=f241108a(typeC,tbinC)[[2]]
      )%>%gt::tab_footnote(
        footnote=x3,
        locations = NULL,
        placement = c("auto", "right", "left")
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
      )
    
    x211G <<- copy(x)
    x
  }
#  f211D()
  
  x211D <- eventReactive(list(
    estdtlR(),
    geoqR(),
    dfnxxR()
  ),       #211 listing----
  {
    if(verbose) print('enter x211D')
    x <- f211D(
      estdtlX=estdtlR(),
      geoqX=geoqR(), 
      dfnxxX=dfnxxR()
    )
    x211G <<- copy(x)
    x
  }
  )
  x211cuD <- eventReactive(list(estdtlR(),geoqR()),     #311cu listing----
                           {
                             if(verbose) print('enter x211D')
                             geox <- copy(geocuR())[,let(rc6,rc9)] #used for aggregation and label
                             x0 <-
                               geo0R()[z110R(),on=c(rc6='rcx'),nomatch=NULL]%>%
                               .[,.(ppm2=sum(pv)/sum(m2)),.(gx,nx)]%>%
                               .[1,-c('ppm2')]
                             x1 <-
                               fread('f241117ad.csv')%>%
                               .[geox[,.(rc6,lab)],on=c(rc6='rc6')]%>% #cu here
                               .[,.(cum=sum(cum)),.(nh,date,lab)]%>%
                               .[data.table(date=dfnxxR()[-1],i=(1:length(dfnxxR())-1)),on=c(date='date')]%>%
                               dcast(.,date+i+lab~nh,value.var='cum')%>%
                               .[order(date),.(date,t=i,lab,NF,NH,UF,UH)]
                             x2 <-
                               estdtcuR()%>%
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
                               geox[
                                 ,paste0('Districts: ',paste0(sort(irregpcode(rc6)),collapse=', '))]
                             x <- 
                               gt::gt(x2)%>%gt::tab_footnote(
                                 footnote=f241108a('C',tbinC)[[1]]
                               )%>%gt::tab_footnote(
                                 footnote=f241108a('C',tbinC)[[2]]
                               )%>%gt::tab_footnote(
                                 footnote=x3,
                                 locations = NULL,
                                 placement = c("auto", "right", "left")
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
                               )
                             x211cuG <<- copy(x)
                             x
                           }
  )
  
  x311D <- eventReactive(list(geo0R()),                   #411 constituents----
                         {
                           if(verbose) print('enter 411')
                           x1 <- 
                             geo0R()[,.(rc3,rc6,qtile)]%>%
                             z110R()[.,on=c(rcx='rc6')]%>%
                             .[,.(rc3,rc6=rcx,nid,ppm2=round(ppm2),quantile=paste0('local-',qtile))]
                           x <- 
                             DT::datatable(
                               x1,
                               options = list(
                                 columnDefs = list(list(className = 'dt-center', targets = 1:4)),
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
  )
  
  #---render section------------
  output$x111 <- renderLeaflet(x111D())
  output$x112 <- renderPlot(x112D())
  output$x121a <- gt::render_gt(x121D()[[1]])
  output$x121b <- gt::render_gt(x121D()[[2]])
  output$x122 <- gt::render_gt(x122D())
  output$x131 <- gt::render_gt(x131D())
  output$x132a <- gt::render_gt(x132D()[['local']][[1]])
  output$x132b <- gt::render_gt(x132D()[['local']][[2]])
  output$x132c <- gt::render_gt(x132D()[['custom']][[1]])
  output$x132d <- gt::render_gt(x132D()[['custom']][[2]])
  output$x411 <- gt::render_gt(x411D())
  output$x421 <- gt::render_gt(x421D())
  output$x431 <- gt::render_gt(x431D())
  output$x411cu <- gt::render_gt(x411cuD())
  output$x421cu <- gt::render_gt(x421cuD())
  output$x431cu <- gt::render_gt(x431cuD())
  output$x211 <- gt::render_gt(x211D())
  output$x211cu <- gt::render_gt(x211cuD())
  output$x311 <- DT::renderDT(x311D())
  
  
}

shinyApp(ui, server)


