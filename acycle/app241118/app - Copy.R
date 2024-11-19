library(broom)
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
library(sp)
library(zoo)
library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
source('c-cleanlib.R')          #2

load('.RData') #see app7   #3
load('pxosrdo2dd.RData')   #4

#--------------------------------ui
gridheight="630px"
gridheight2="830px"
colx <- cobalt()[c(4,2,1)]
#--------------------------------Pseudo=Control----2----
hoflC   = c('house','flat','all')[3]#,
itriC   = c('.0'=1,'.1'=2,'.5'=3)[2]#, #Trim ---
neusC   = c('new','used','all')[3]#,
rc3coC  =  c('B--','E--','AL-')#,  #comp
rc6cuC  = c('E--14-','E--1W-')#, #custom
tbinC   = c(lo=1,hi=2,an=3)[2]#,  #lo hi an ---
typeC   = c('A','L','N','C')[2]#, #All Local National ---
typerC  = typeC#,
zerorefC =F#, #set reference asset NULL
showtradetriangle=F
nfig2 <- -1 #for ppm2
nfig3 <- 4 #for frac
verbose <- T
if(verbose) print('enter ')

ui <-      #ui----
grid_page(
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
        value = "E14"
      ),
      sliderInput(
        inputId = "tslider",
        label = "Datum period",
        min=0,
        max=45,
        value=27
      )
    )
  ),#grid card end
  grid_card_text(
    area = "header",
    content = "Local index",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "area2",
    card_body(
      tabsetPanel(
        nav_panel(
          title = "Time-series summary",
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
        nav_panel(
          title = "Listing",
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
              card_body(#--------------x311
                gt::gt_output('x311')
              )
            ),
            grid_card(
              area='customlist',
              full_screen = TRUE,
              card_header(
                "Custom"
              ),
              card_body(#--------------x311cu
                gt::gt_output('x311cu')
              )
            )
          )
        ),
        nav_panel(
          title = "Constituents",
          card(
            full_screen = TRUE,
            card_header(
              "Constituent districts"
            ),
            card_body(#--------------x411
              DT::DTOutput('x411')
            ),
            height=gridheight2
          )
        ),
        nav_panel(
          title = "Accuracy",
          card(
            full_screen = TRUE,
            card_header("Header"),
            card_body(
              grid_container(
                layout = c(
                  "timesampling      ",
                  "outlierrejection  ",
                  "crossvalidation   "#,
                  #"geographicgrouping"
                ),
                row_sizes = c(
                  "1fr",
                  "1fr",
                  "1fr"#,
                  #"1fr"
                ),
                col_sizes = c(
                  "1fr"
                ),
                gap_size = "10px",
                grid_card(
                  area = "timesampling",
                  full_screen = TRUE,
                  card_header(
                    "Time Sampling"
                  ),
                  card_body(#--------------x211
                    gt::gt_output('x211')
                  )
                  
                ),
                grid_card(
                  area = "outlierrejection",
                  full_screen = TRUE,
                  card_header(
                    "Outlier Rejection"
                  ),
                  card_body(#--------------x221
                    gt::gt_output('x221')
                  )
                ),
                grid_card(
                  area = "crossvalidation",
                  full_screen = TRUE,
                  card_header(
                    "Cross Validation"
                  ),
                  card_body(#--------------x231
                    gt::gt_output('x231')
                  )
                )
                
              )
            )
          )
        ),
        nav_panel(
          title = "Notes",
          card(
            full_screen = TRUE,
            card_body(
              htmltools::includeMarkdown("notes.Rmd")
            )
          )
        )#----end nav-panel
      )
    )
  )
)#ui grid_page ends
server <-  #server----
function(input, output) {
  #---global   section----
  x00G <<- copy(f241021ad)
  geoplusG <<- copy(x00G$geoplus)[,let(lab,des)]
  estdtG <<- copy(x00G$estdt)[,.(nx,ii,date,xdotd,days,xdot,x)]
  rssG <<- copy(x00G$rss)
  pxosrdo2ddG <<- copy(pxosrdo2dd)
  z110G <<- copy(z110)
  x101G <<- copy(x101)
  geo0G <<- #augmented geo
    geoplusG%>%
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
    z110G[.,on=c(rcx='rc6')]%>%
    .[,.(nx,gx,lab,rc3,rc6=rcx,qtile)]
  dfnG <<- #assume constant
    data.table(date=c(as.Date('1994-12-31'),estdtG[,sort(unique(date))]))[,let(i,(0:(.N-1)))]

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
    rc6tR(), #+control
    {
      if(verbose) print('enter rc6cuR')
      x <- rc6tR()
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
        data.table(rc9=rc6cuR(),nx=0,lab='CU00') #rc6cuG -> rc6cuR()
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
      estdtaR() #for dates
    ),
    {
      if(verbose) print('enter rsicuR')
      geox <- geocuR()
      dfnx <- estdtaR()[,sort(unique(date))]
      #geox <- rbind(geox,geox[,rc9:='E--1--'])
      #geox[,nx:=1]
      #browser()
      x <- 
        f240710a(  #returns estdt, kfoldsse, all
          nxx=0,
          stepripx='03rip/', 
          dfn=sort(unique(c(min(x101G),dfnx))),    #R
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
      rsicuR(),
      rc6tR() #added.... debugging...
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
         #rsscuR(),estdtcuR() #these added here just to provide dependency and hence drive global assignment of custom
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
  estdtlR <-   #---local estdt compute ----
  eventReactive( 
    nxqR(),
    {
      if(verbose) print('enter estdtlR')
      x <- 
        estdtG[nxqR(),on=c(nx='nx')]%>%
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
      x <- geo0G%>%
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
        estdtG[nxaR(),on=c(nx='nx')]%>%
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
        rssG[nxaR(),on=c(nx='nx')]
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
      ylimG <- copy(x)
      x
    }
  )
  #---display  section----
  x111D <- eventReactive(list(rc6tR(),rc6cuR()),#map ----
                         {
                           if(verbose) print('enter x111D')
                           x <- 
                             geoaR()%>% 
                             .[,.(
                               rc6,
                               col=lighten(colx,.7)[.BY[[1]]],
                               qtile, #shade tiles light
                               lab
                             ),by=.(qtile)]%>%
                             .[
                               rc6==rc6tR(), #with target district darker
                               col:=colx[.BY[[1]]],
                               by=.(qtile)
                             ]%>%
                             f240810b( #->leaflet, colours for areas-to-shade in column 'col'
                               .,
                               x2=pxosrdo2ddG,
                               pva=z110G,
                               minzoom=9
                             )%>%
                             addPolygons( #outline custom districts
                               data=pxosrdo2ddG[which(pxosrdo2ddG@data$name%in%irregpcode(rc6cuR())),], 
                               fill=F, 
                               color="orange",
                               weight=1,
                               opacity=1
                             )
                           x111G <<- copy(x)
                           x
                         }
  )
  output$x111 <- 
    renderLeaflet(
      x111D()
    )
  
  
  x112D <- eventReactive(list(input$tslider,estdtxR()),#x(t)----
                         {
                           if(verbose) print('enter x112D')
                           x2c <- estdtxR()%>%
                             .[,.SD[,.(ii,date,lab,x=x-ifelse(input$tslider==0,0,x[input$tslider]))],.(qtile)]%>%
                             .[,.SD[,.(ii,date,lab,x)],.(qtile)]%>%
                             .[,qq:=as.factor(qtile)]%>%
                             .[,labx:=ifelse(date==max(date),lab,NA)]
                           x0 <- setNames(cobalt()[c('punk','green','blue')],as.character(1:3))
                           x3 <- estdtxR()[,.SD[,.(ifelse(input$tslider==0,0,x[input$tslider]))],.(qtile)][,mean(V1)]#base value for rebase level
                           x2 <-
                             estdtxR()[,.SD[,.(ii,date,lab,x=x-ifelse(input$tslider==0,0,x[input$tslider]))],.(qtile)]%>%
                             .[,qq:=as.factor(qtile)]%>%
                             .[,labx:=ifelse(date==max(date),lab,NA)]
                           x <- x2%>%
                             ggplot(.,aes(date,x,color=qq,label=labx))+
                             geom_hline(yintercept=0,linewidth=.4,linetype = "dotted",color='grey40')+
                             geom_line()+
                             geom_point(size=.3)+
                             geom_text_repel()+
                             ylim(ylimR()-x3)+
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
                           x112G <<- copy(x)
                           #}
                           x
                         }
  )
  output$x112 <- 
    renderPlot(
      x112D()
    )
  
  f121 <- function(
    estdt=estdtlR(),
    tslidex=input$tslider,
    drange=range(x101G)
  ) {
    d1 <- #daily
      seq.Date(from=drange[1],to=drange[2],by='d')
    d2 <- #resample: yearend + final date
      x101G%>%
      ifelse(.==as.Date('2009-02-28'),as.Date('2008-12-31'),.)%>%
      as.Date(.)%>%
      .[-1]
    x1 <-
      estdt%>% #local
      .[.(date=d1),on=c(date='date'),roll=-Inf,j=.(date,xdotd)]%>%
      .[,.(ii=1:.N,date,x=cumsum(xdotd))]%>%
      .[.(date2=d2),on=c(date='date2')]%>%
      .[,.(date,x,xdot=c(x[1],diff(x)),ii=1:.N)]%>%
      .[,.(ii,date,xdot,x)]%>%
      .[,.(date,xdot)]%>%
      .[date==as.Date('2009-02-28'),let(date,as.Date('2008-12-31'))]%>%
      .[,.(decade=substr(date,1,3),yr=substr(date,4,4),xdot=round(xdot,3))]%>%
      dcast(.,decade~yr,value.var='xdot')%>%
      .[,decade:=c(1990,2000,2010,2020)]
    for(i in 2:length(x1)) x1[[i]] <- ifelse(is.na(x1[[i]]),'',as.character(round(x1[[i]],3)))
    x1
  }
  
  x121D <- eventReactive(list(input$tslider,estdtlR()),#winding----
                         {
                           if(verbose) print('enter x121D')
                           x1 <- f121()
                           x2 <- gt::gt(x1)%>%gt::tab_footnote(
                             footnote=f241108a(typeC,tbinC)[[1]]
                           )%>%gt::tab_footnote(
                             footnote=f241108a(typeC,tbinC)[[2]]
                           )
                           x3 <- f121(estdt=estdtcuR())
                           x4 <- gt::gt(x3)%>%gt::tab_footnote(
                             footnote=f241108a('Custom',tbinC)[[1]]
                           )%>%gt::tab_footnote(
                             footnote=f241108a('Custom',tbinC)[[2]]
                           )
                           x <- list(x2,x4)
                           x121G <<- copy(x)
                           x
                         }
  )
  output$x121a <- 
    gt::render_gt(x121D()[[1]])
  output$x121b <- 
    gt::render_gt(x121D()[[2]])
  
  x122D <- eventReactive(list(rc6tR(),rssaR(),rsscuR),#characteristics----
                         {
                           if(verbose) print('enter x122D')
                           rssax <- rssaR()
                           #rc6tx <- rc6tG#R()
                           rsscux <- copy(rsscuR())[,lab:='CU000']#R()
                           x0 <- #custom
                             z110G[rsscux,on=c(rcx='rc6')]%>%
                             .[,.(
                               frac=round(sum(nid)/z110G[nchar(rcx)==6,sum(nid)],nfig3),
                               nid=sum(nid),
                               ppm2max=round(max(ppm2),nfig2),
                               ppm2min=round(min(ppm2),nfig2),
                               p=round(sum(pv)/sum(m2),nfig2)
                             ),
                             lab
                             ]%>%
                             .[rsscux[,.(R2rsi=1-sum(ssek)/sum(sstr)),lab],on=c(lab='lab')]%>%
                             .[,.(
                               lab=substr(lab,1,4),#='Custom',
                               frac,
                               R2rsi=round(R2rsi,3),
                               p=prettyNum(round(p,nfig3), big.mark=","),
                               p.cus=paste0(prettyNum(round(ppm2min,nfig2), big.mark=","),'-',prettyNum(round(ppm2max,nfig2), big.mark=","))
                             )]
                           
                           x1 <- #local
                             z110G[rssax,on=c(rcx='rc6')]%>%
                             .[,.(
                               frac=round(sum(nid)/z110G[nchar(rcx)==6,sum(nid)],nfig3),
                               nid=sum(nid),
                               ppm2max=round(max(ppm2),nfig2),
                               ppm2min=round(min(ppm2),nfig2),
                               p=round(sum(pv)/sum(m2),nfig2)
                             ),
                             lab
                             ]%>%
                             .[rssax[,.(R2rsi=1-sum(ssek)/sum(sstr)),lab],on=c(lab='lab')]%>%
                             .[order(-p)]%>%
                             .[,.(
                               lab,
                               frac,
                               R2rsi=round(R2rsi,3),
                               p=prettyNum(round(p,nfig3), big.mark=","),
                               p.cus=paste0(prettyNum(round(ppm2min,nfig2), big.mark=","),'-',prettyNum(round(ppm2max,nfig2), big.mark=","))
                             )]
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
  )
  output$x122 <- 
    gt::render_gt(
      x122D()
    )
  
  
  x131D <- eventReactive(list(input$tslider,estdtxR()),#summary----
                         {
                           if(verbose) print('enter x131D')
                           x <- 
                             estdtxR()%>%
                             .[ii>=input$tslider]%>%
                             dcast(.,ii~lab,value.var='xdot')%>%
                             .[,-'ii']%>%
                             as.matrix(.)%>%
                             zoo(.,estdtxR()[,sort(unique(date))])%>%
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
                           x131G <<- copy(x)
                           x
                         }
  )
  output$x131 <- 
    gt::render_gt(
      x131D()
    )
  
  
  
  
  f132 <- function(
    geox=geoqG,
    steprip='03rip/',
    estdtlx=estdtlG, #only used for its date(ii) relation
    tmin=20
  ) {#tmin=input$tslider
    x0 <-
      geox[,rc6]%>%
      coread(.,steprip)%>% #or rc6tC
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
    print(x3)
    x3
  }
  x132D <- eventReactive(list(input$tslider,geoqR(),estdtlR()),#trade summary(2)----
                         {
                           tminx <- input$tslider
                           if(verbose) print('enter x132D')
                           x1 <- f132(
                             geox=geoqR(),#geoqR()
                             steprip='03rip/',
                             estdtlx=estdtlR(),#estdtlR()
                             tmin=tminx#tmin=input$tslider
                           )
                           x2 <- f132(
                             geox=geocuR()[,.(rc6=rc9)],#geoqR()
                             steprip='03rip/',
                             estdtlx=estdtlR(),#estdtlR()
                             tmin=tminx#tmin=input$tslider
                           )
                           x <- list(
                             local=x1,
                             custom=x2
                           )
                            #setnames(x[['local']][[1]],old='buy',new='buy\\sell')
                           x[['local']][[1]] <- 
                             x[['local']][[1]]%>%
                             gt::gt(.)%>%
                             tab_header(.,title = 'Local - Return', subtitle=NULL, preheader = NULL)%>%
                             opt_align_table_header(., align = "left")%>%
                             tab_options(heading.title.font.size =14)%>%
                             tab_spanner(
                               label = gt::html("sell"),
                               columns = 2:ncol(x[['local']][[1]])
                             )
                           x[['local']][[2]] <- 
                             x[['local']][[2]]%>%
                             gt::gt(.)%>%
                             tab_header(., title='Local - Number', subtitle = NULL, preheader = NULL)%>%
                            opt_align_table_header(., align = "left")%>%
                             tab_options(heading.title.font.size =14)%>%
                             tab_spanner(
                               label = gt::html("sell"),
                               columns = 2:ncol(x[['local']][[2]])
                             )
                           x[['custom']][[1]] <- 
                             x[['custom']][[1]]%>%
                             gt::gt(.)%>%
                             tab_header(., title='Custom - Return', subtitle = NULL, preheader = NULL)%>%
                            opt_align_table_header(., align = "left")%>%
                             tab_options(heading.title.font.size =14)%>%
                             tab_spanner(
                               label = gt::html("sell"),
                               columns = 2:ncol(x[['custom']][[1]])
                             )
                           x[['custom']][[2]] <- 
                             x[['custom']][[2]]%>%
                             gt::gt(.)%>%
                             tab_header(., title='Custom - Number', subtitle = NULL, preheader = NULL)%>%
                            opt_align_table_header(., align = "left")%>%
                             tab_options(heading.title.font.size =14)%>%
                             tab_spanner(
                               label = gt::html("sell"),
                               columns = 2:ncol(x[['custom']][[2]])
                             )
                           x132G <<- copy(x)
                           x
                         }
  )
  
  output$x132a <- 
    gt::render_gt(x132D()[['local']][[1]])
  output$x132b <- 
    gt::render_gt(x132D()[['local']][[2]])
  output$x132c <- 
    gt::render_gt(x132D()[['custom']][[1]])
  output$x132d <- 
    gt::render_gt(x132D()[['custom']][[2]])
  
  
  x211D <- eventReactive(list(rc6tR(),rssaR()),#accuracy----tbin----
                         {
                           if(verbose) print('enter x211G')
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
                           x211G <<- copy(x)
                           x
                         }
  )
  output$x211 <- 
    gt::render_gt(x211D())
  
  
  x221D <- eventReactive(list(rc6tR(),rssaR()),#accuracy----trim----
                         {
                           if(verbose) print('enter x221D')
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
                           x221G <<- copy(x)
                           x
                         }
  )
  output$x221 <- 
    gt::render_gt(x221D())
  
  
  x231D <- eventReactive(list(rc6tR(),rssaR()),#accuracy----in/out----
                         {
                           if(verbose) print('enter x231D')
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
                           x231G <<- copy(x)
                           x
                         }
  )
  output$x231 <-
    gt::render_gt(x231D())

  
   x311D <- eventReactive(list(estdtlR(),geoqR()), #listing----
                         {
                           if(verbose) print('enter x311D')
                           x0 <-
                             geo0G[z110G,on=c(rc6='rcx'),nomatch=NULL]%>%
                             .[,.(ppm2=sum(pv)/sum(m2)),.(gx,nx)]%>%
                             .[1,-c('ppm2')]
                           x1 <-
                             fread('f241117ad.csv')%>%
                             .[geoqG[,.(rc6,lab)],on=c(rc6='rc6')]%>%
                             .[,.(cum=sum(cum)),.(nh,date,lab)]%>%
                             .[dfnG[i>0],on=c(date='date')]%>%
                             dcast(.,date+i+lab~nh,value.var='cum')%>%
                             .[order(date),.(date,t=i,lab,NF,NH,UF,UH)]
                           x2 <-
                             estdtlR()%>%
                             .[,.(t=c(0,ii),days=c(NA,days),date=c(date[1]-days[1],date),xdot=c(NA,xdot),x=c(0,x))]%>%
                             x1[.,on=c(t='t')]%>%
                             .[,.(t,date=i.date,days,xdot,x,NF,NH,UF,UH,tot=NF+NH+UF+UH)]%>%
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
                               total=round(tot/1000,1),
                               perday=round(tot/days)
                             )]
                           x3 <- #districts footnote
                             geoqR()[
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
                             tab_spanner(
                               label = gt::html("Sales Breakdown"),
                               columns = c(newhouse, usedhouse,newflat,usedflat,total,perday)
                             )%>%
                             cols_label(
                               total = gt::html('total(000)')
                             )

                           x311G <<- copy(x)
                           x
                         }
  )
  output$x311 <-
    gt::render_gt(
      x311D()
    )
  x311cuD <- eventReactive(list(estdtlR(),geoqR()), #listing----
                         {
                           if(verbose) print('enter x311D')
                           geox <- copy(geocuR())[,let(rc6,rc9)] #used for aggregation and label
                           #browser()
                           x0 <-
                             geo0G[z110G,on=c(rc6='rcx'),nomatch=NULL]%>%
                             .[,.(ppm2=sum(pv)/sum(m2)),.(gx,nx)]%>%
                             .[1,-c('ppm2')]
                           x1 <-
                             fread('f241117ad.csv')%>%
                             .[geox[,.(rc6,lab)],on=c(rc6='rc6')]%>% #cu here
                             .[,.(cum=sum(cum)),.(nh,date,lab)]%>%
                             .[dfnG[i>0],on=c(date='date')]%>%
                             dcast(.,date+i+lab~nh,value.var='cum')%>%
                             .[order(date),.(date,t=i,lab,NF,NH,UF,UH)]
                           x2 <-
                             estdtcuR()%>%
                             .[,.(t=c(0,ii),days=c(NA,days),date=c(date[1]-days[1],date),xdot=c(NA,xdot),x=c(0,x))]%>%
                             x1[.,on=c(t='t')]%>%
                             .[,.(t,date=i.date,days,xdot,x,NF,NH,UF,UH,tot=NF+NH+UF+UH)]%>%
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
                               total=round(tot/1000,1),
                               perday=round(tot/days)
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
                             tab_spanner(
                               label = gt::html("Sales Breakdown"),
                               columns = c(newhouse, usedhouse,newflat,usedflat,total,perday)
                             )%>%
                             cols_label(
                               total = gt::html('total(000)')
                             )
                           
                           x311cuG <<- copy(x)
                           x
                         }
  )
  output$x311cu <- 
    gt::render_gt(
      x311cuD()
    )
  
  x411D <- eventReactive(list(geo0G),  #constituents----
                         {
                           if(verbose) print('enter 411')
                           x1 <- 
                             geo0G[,.(rc3,rc6,qtile)]%>%
                             z110G[.,on=c(rcx='rc6')]%>%
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
                           x411G <<- copy(x)
                           x
                         }
  )
  output$x411 <- 
    DT::renderDT(
      x411D()
    )
}

shinyApp(ui, server)


