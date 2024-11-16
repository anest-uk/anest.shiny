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
#--------------------------------Pseudo=Control----2----
hoflC   = c('house','flat','all')[3]#,
itriC   = c('.0'=1,'.1'=2,'.5'=3)[2]#, #Trim ---
neusC   = c('new','used','all')[3]#,
rc3coC  =  c('B--','E--','AL-')#,  #comp
rc6cuC  = c('E--7--')#, #custom
tbinC   = c(lo=1,hi=2,an=3)[2]#,  #lo hi an ---
typeC   = c('A','L','N','C')[2]#, #All Local National ---
typerC  = typeC#,
zerorefC =F#, #set reference asset NULL
showtradetriangle=F
nfig2 <- -1 #for ppm2
nfig3 <- 4 #for frac

ui <- grid_page(
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
                gt::gt_output('x121')#,
                #height=gridheight
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
                height=gridheight
              )
            )
            #   )
            # )
          )
        ),
        nav_panel(
          title = "Listing",
          card(
            full_screen = TRUE,
            card_header(
              "Index time series"
            ),
            card_body(#--------------x311
              gt::gt_output('x311')
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
)

server <- function(input, output) {
   aaa <- 0
  #------------------------global
  x00 <<-  copy(f241021ad)
  #x00$geoplus <<- x00$geoplus[,let(lab,des)]
  #x00$estdt <<- x00$estdt[,.(nx,ii,date,xdotd,days,xdot,x)]
  
  #assign global
  geoplusg <<- copy(x00$geoplus)[,let(lab,des)]
  estdtg <<- copy(x00$estdt)[,.(nx,ii,date,xdotd,days,xdot,x)]
  rss <<- copy(x00$rss)
  pxosrdo2ddg <<- pxosrdo2dd
  z110g <<- z110
  x101g <<- x101
  geo0g <<- #augmented geo
    geoplusg%>%
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
    z110[.,on=c(rcx='rc6')]%>%
    .[,.(nx,gx,lab,rc3,rc6=rcx,qtile)]
  
  #------------------------------geo
  #---target: this district
  geotR <-
    eventReactive( 
      rc6tR(),
      {
        geoaR()%>%
          .[rc6==rc6tR()]
      }
    )
  #----area: districts in this area
  geoaR <- 
    eventReactive( 
      rc6tR(),
      {
        geo0g%>%
          .[rc3==substr(rc6tR(),1,3)] 
      }
    )
  #---qtile: districts in this area-qtile
  geoqR <-
    eventReactive( 
      list(geoaR(),geotR()),
      {
        geoaR()%>% 
          .[geotR()[,.(qtile)],
            on=c(qtile='qtile')]
      }
    )
  #---rtile: districts in this area-rtile where an rtile is any set of districts numbered nx in the geo
  #the rtiles coexist with the rest of geo and all other tables so nx defines one such rtile; nx is +ve for 'all' and 'nat' zero for 'cus' and -ve for others
  #we start with custom nx=0 which has a defined geo always
  #georR is therefore always the target district alone for now
  #georR = target
  #nxrR = 0
  #rsiR =  rsi(geoR)
  #estdtrR estdt(geo0R)
  #rssrR   rss(geo)
  
  #------------------------------nx
  #---qtile
  nxqR <- 
    eventReactive( 
      geoqR(),
      {
        geoqR()[,.(nx,rc3,qtile,lab)]%>%
          unique(.)
      }
    )
  #---area
  nxaR <- 
    eventReactive( 
      geoaR(),
      {
        geoaR()[,.(nx,rc3,qtile,lab)]%>%
          unique(.)
      }
    )
  #------------------------------estdt(nx)
  #---qtile
  estdttR <-
    eventReactive( 
      nxqR(),
      {
        
        estdtg[nxqR(),on=c(nx='nx')]%>%
          .[,.(nx,date,ii,lab,rc3,qtile,xdotd,days,xdot,x)]
      }
    )
  #---area
  estdtaR <- 
    eventReactive( 
      nxaR(),
      {
        estdtg[nxaR(),on=c(nx='nx')]%>%
          .[,.(nx,date,ii,lab,rc3,qtile,xdotd,days,xdot,x)]
      }
    )
  #------------------------------rss(nx)
  rssaR <- #area
    eventReactive( 
      nxaR(),
      {
        rss[nxaR(),on=c(nx='nx')]
      }
    )
  
  #---------------------------utility
  ylimR <-
    eventReactive( 
      estdtaR(),
      {
        estdtaR()[,range(x)]*1.1
      }
    )
  rc6tR <-
    eventReactive( 
      input$rc6tC,
      {
        regpcode(input$rc6tC)
      }
    )
#-----------------------------------------------------------------==end reactive
  
  #---1.1.1 map---------------------------------------------------------------------
  x111R <-          
    eventReactive( 
      rc6tR(),
      {
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
            x2=pxosrdo2ddg,
            pva=z110g,
            minzoom=9
          )
      }
    )
  output$x111 <- 
    renderLeaflet(
      x111R()
    )
  #----1.1.2 x(t)-------------------------------------------------------------------
  x112 <- eventReactive(
    list(input$tslider,rc6tR(),estdtaR()),
    {
      print('update x112')
      print(estdtaR())
      
      x0 <- setNames(cobalt()[c('punk','green','blue')],as.character(1:3))
      x3 <- estdtaR()[,.SD[,.(ifelse(input$tslider==0,0,x[input$tslider]))],.(qtile)][,mean(V1)]
      x2 <-
        estdtaR()[,.SD[,.(ii,date,lab,x=x-ifelse(input$tslider==0,0,x[input$tslider]))],.(qtile)]%>%
        .[,qq:=as.factor(qtile)]%>%
        .[,labx:=ifelse(date==max(date),lab,NA)]
      x4 <- x2%>%
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
      x4
    }
  )
  
  output$x112 <- 
    renderPlot(
      x112()
    )
  
  #----1.2.1 winding
  x121 <- eventReactive(
    list(input$tslider,rc6tR(),x101g),
    {
      tslidex <- input$tslider
      d1 <- #daily
        seq.Date(from=min(x101g),to=max(x101g),by='d')
      d2 <- #yearend + final date
        x101g%>%
        ifelse(.==as.Date('2009-02-28'),as.Date('2008-12-31'),.)%>%
        as.Date(.)%>%
        .[-1]
      x1 <-
        estdttR()%>%
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
      x2 <- gt::gt(x1)%>%gt::tab_footnote(
        footnote=f241108a(typeC,tbinC)[[1]]
      )%>%gt::tab_footnote(
        footnote=f241108a(typeC,tbinC)[[2]]
      )
      x2
    }
  )
  
  output$x121 <- 
    render_gt(
      x121()
    )
  
  #----1.2.2 characteristics
  x122 <- eventReactive(
    list(rc6tR(),rssaR()),
    {
      x1 <- 
        z110g[rssaR(),on=c(rcx='rc6')]%>%
        .[,.(
          frac=round(sum(nid)/z110g[nchar(rcx)==6,sum(nid)],nfig3),
          nid=sum(nid),
          ppm2max=round(max(ppm2),nfig2),
          ppm2min=round(min(ppm2),nfig2),
          p=round(sum(pv)/sum(m2),nfig2)
        ),
        lab
        ]%>%
        .[rssaR()[,.(R2rsi=1-sum(ssek)/sum(sstr)),lab],on=c(lab='lab')]%>%
        .[order(-p)]%>%
        .[,.(
          lab,
          frac,
          R2rsi=round(R2rsi,3),
          p=prettyNum(round(p,nfig3), big.mark=","),
          p.cus=paste0(prettyNum(round(ppm2min,nfig2), big.mark=","),'-',prettyNum(round(ppm2max,nfig2), big.mark=","))
        )]%>%
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
      x1
    }
  )
  output$x122 <- 
    render_gt(
      x122()
    )
  
  #----1.3.1 summary
  x131 <- eventReactive(
    list(input$tslider,estdtaR()),
    {
      x1 <- 
        estdtaR()%>%
        .[ii>=input$tslider]%>%
        dcast(.,ii~lab,value.var='xdot')%>%
        .[,-'ii']%>%
        as.matrix(.)%>%
        zoo(.,estdtaR()[,sort(unique(date))])%>%
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
      x1
    }
  )
  output$x131 <- 
    render_gt(
      x131()
    )
  
  #----1.3.2 trade summary (2 tables)
  x132 <- eventReactive(
    list(input$tslider,geoqR(),estdttR()),
    {
      steprip <- '03rip/'
      x0 <-
        geoqR()[,rc6]%>%
        coread(.,steprip)%>% #or rc6tC
        .[,.(N=.N,mean=round(mean(as.numeric(retsa)),4)),.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))]%>%
        .[(buy>=estdttR()[ii>=input$tslider,substr(min(as.character(date)),1,4)])]
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
      x3 <-
        gt::gt(x1)%>%gt::tab_footnote(
          footnote=f241108a(typeC,tbinC)[[1]]
        )%>%gt::tab_footnote(
          footnote=f241108a(typeC,tbinC)[[2]]
        )
      x4 <-
        gt::gt(x2)%>%gt::tab_footnote(
          footnote=f241108a(typeC,tbinC)[[1]]
        )%>%gt::tab_footnote(
          footnote=f241108a(typeC,tbinC)[[2]]
        )
      list(x3,x4)
    }
  )
  output$x132a <- render_gt(x132()[[1]])
  output$x132b <- render_gt(x132()[[2]])
  
  #----2.1.1  accuracy  tbin
  x211 <- eventReactive(
    list(rc6tR(),geoaR()),
    {
      pc6tx <- rc6tR()
      x1 <-
        data.table(tbin=1:3,freq=c('lo','hi','an'))
      x2 <- rss%>%
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
      x4 <- 
        gt::gt(x3)%>%
        gt::tab_footnote(
          footnote=f241108a(typeC,tbinC)[[1]]
        )
      x4
    }
  )
  output$x211 <- render_gt(x211())
  
  #----2.2.1  accuracy  trim
  x221 <- eventReactive(
    list(rc6tR(),geoaR()),
    {
      pc6tx <- rc6tR()
      x1 <-
        data.table(itrim=1:3,threshold=c('0.0','0.1','0.5'))
      x2 <-
        rss%>%
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
      x4 <- 
        gt::gt(x3)%>%gt::tab_footnote(
          footnote=f241108a(typeC,tbinC)[[1]]
        )%>%gt::tab_footnote(
          footnote=f241108a(typeC,tbinC)[[2]]
        )
      x4
    }
  )
  output$x221 <- 
    render_gt(
      x221()
    )
  
  #----2.3.1  accuracy  in/out
  x231 <- eventReactive(
    list(rc6tR(),geoaR()),
    {
      pc6tx <- rc6tR()
      x1 <-
        rss%>%
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
      x4 <- 
        gt::gt(x2)%>%gt::tab_footnote(
          footnote=f241108a(typeC,tbinC)[[1]]
        )%>%gt::tab_footnote(
          footnote=f241108a(typeC,tbinC)[[2]]
        )
      x4
    }
  )
  output$x231 <- 
    render_gt(
      x231()
    )
  
  #----3.1.1 listing
  x311 <- eventReactive(
    list(estdttR(),geoqR()), #geo0g is not reactive
    {
      x0 <-
        geo0g[z110g,on=c(rc6='rcx'),nomatch=NULL]%>%
        .[,.(ppm2=sum(pv)/sum(m2)),.(gx,nx)]%>%
        .[1,-c('ppm2')]
      x1 <-
        fread('f241024ad.csv')%>%
        .[order(tbin,gx,date)]%>%.[,let(t,1:.N),.(tbin,gx)]%>%
        .[tbin==tbinC]%>%
        .[geotR(),on=c(gx='gx')]%>%
        .[order(date),.(date,t,lab,NF,NH,UF,UH)]
      x2 <-
        estdttR()%>%
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
      x4 <- 
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
        )
      x4
    }
  )
  output$x311 <- 
    render_gt(
      x311()
    )
  
  #----4.1.1 constituents
  x411 <- eventReactive(
    list(geo0g), #geo0g is not reactive
    {
      x1 <- 
        geo0g[,.(rc3,rc6,qtile)]%>%
        z110g[.,on=c(rcx='rc6')]%>%
        .[,.(rc3,rc6=rcx,nid,ppm2=round(ppm2),quantile=paste0('local-',qtile))]
      x2 <- 
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
      x2
    }
  )
  output$x411 <- 
    DT::renderDT(
      x411()
    )
}

shinyApp(ui, server)


