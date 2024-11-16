library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
# library(shinyuieditor)
# setwd('c:/users/giles/anest.repo/anest.shiny/acycle/app10')
# launch_editor('.')
pgmt='dotted'
pgmc='grey50'
pgms=.2
lightenx <- .7
gridheight="630px"
gridheight2="830px"


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
      #'e.g. E--3--',
      # radioButtons(
      #   inputId = "timebin",
      #   label = "Time sampling",
      #   choices = list("High" = "H", "Low" = "L", "Annual" = "A"),
      #   width = "100%"
      # ),
      # radioButtons(
      #   inputId = "trim",
      #   label = "Outlier rejection",
      #   choices = list("None" = "1", "Low" = "2", "High" = "3"),
      #   width = "100%"
      # ),
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
              "map     xtimeseries    ",
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
              area = "map", #1.1 -------name
              full_screen = TRUE,
              card_header(#-------------header
                "Postcode area map"
              ),
              card_body(#--------------output
                leaflet::leafletOutput('map'),
                height=gridheight
              )
            ),
            grid_card(
              area = "xtimeseries", #1.1 -------name
              full_screen = TRUE,
              card_header(
                "Indices"
              ),
              card_body(#--------------output
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
              card_body(#--------------output
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
              card_body(#--------------output
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
              card_body(#--------------output
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
              card_body(#--------------output
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
            card_body(#--------------output
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
            card_body(#--------------output
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
                  card_body(#--------------output
                    gt::gt_output('x211')
                  )
                  
                ),
                grid_card(
                  area = "outlierrejection",
                  full_screen = TRUE,
                  card_header(
                    "Outlier Rejection"
                  ),
                  card_body(#--------------output
                    gt::gt_output('x221')
                  )
                ),
                grid_card(
                  area = "crossvalidation",
                  full_screen = TRUE,
                  card_header(
                    "Cross Validation"
                  ),
                  card_body(#--------------output
                    gt::gt_output('x231')
                  )
                )#,
                # grid_card(
                #   area = "geographicgrouping",
                #   full_screen = TRUE,
                #   card_header(
                #     "Geographic grouping"
                #   )
                # )
              )
            )
          )
        ),
        nav_panel(
          title = "Notes",
          card(
            full_screen = TRUE,
            #card_header("Notes"),
            card_body(
              tags$div(class = "header", checked = NA,
                       tags$p("This is an internal development version"),
                       tags$p("These informal notes will be updated as development proceeds"),
                       tags$p("The app is not self-explanatory at present and needs proper introduction"),
                       tags$p("Geographic scope:"),
                       tags$p("These are the 'local indices'"),
                       tags$p("The current scope includes the local model, pre-calculated, on 104 postcode areas"),
                       tags$p("Most areas have been optimally split into 3 price-bands, smaller ones into 2 or just 1, depending on datapoints available"),
                       tags$p("The indices total 286 and span the country exactly: every property belongs to exactly 1 index"),
                       tags$p("The controls are (1) select district, AKA 'outward postcode', or 'outcode' (2) a 'base date' which is used to on the first page, displays 'indices' 'summary' and 'trade recap'"),
                       tags$p("Trade recap is available for area examples (BA, B, E, AL) only at present"),
                       tags$p("The time-binning is as before dynamic bins currently 46 in total"),
                       tags$p("Accuracy is reported as 'mean square error' i.e. the error in the repeat sales regression"),
                       tags$p("Limitations/scope:"),
                       tags$p(" -labelling is abbreviated and needs explanation"),
                       tags$p(" -units are delta log price and are decimal"),
                       tags$p(" -only one time-binning of 3 is displayed"),
                       tags$p(" -only local geo-bins are displayed"),
                       tags$p(" -sensitivity to outlier removal is reported but the resulting indices not shown"),
                       tags$p(" -custom index is not implemented / not shown"),
                       tags$p(" -upload and measure acccuracy of 3rd party index is not available"),
                       tags$p(" -download index is not available"),
                       tags$p(" -numerical results are not checked in detail"),
                       tags$p("The cycle model is not shown"),
                       tags$p(""),
                       tags$p("")
              )
            )
          )
        )#----end nav-panel
      )
    )
  )
)


server <- function(input, output) {
  
  
  #these need to be inline in the app, not in a separate file  
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
  
  
  source('c-cleanlib.R')          #2
  load('.RData') #see app7   #3
  load('pxosrdo2dd.RData')   #4
  pxosrdo2ddR <- reactive(pxosrdo2dd)
  z110R <- reactive(z110)
  x101R <- reactive(x101)
  
  #x101 <<- copy(x101)
  #--------------------------------Control-----------------------------------2----
      colx <- cobalt()[c(4,2,1)]
  
  #actual controls, also
  rc6tX   = 'AL-2--'#, #target
  
  #f241105a <- #assign 'Controls' ----
  #function(
    hoflC   = c('house','flat','all')[3]#,
  itriC   = c('.0'=1,'.1'=2,'.5'=3)[2]#, #Trim ---
  neusC   = c('new','used','all')[3]#,
  #rc3coC  =  c('SW-','SE-','TN-','PE-','CT-','S--','LU-','KT-','L--','CA-'),  #comp
  rc3coC  =  c('B--','E--','AL-')#,  #comp
  rc6cuC  = c('E--7--')#, #custom
  rc6rC   = 'AL-1--'#, #reference
  rcumC   = c('ret','cum')[1]#,
  tbinC   = c(lo=1,hi=2,an=3)[2]#,  #lo hi an ---
  tstartC = 22#, #for tables
  typeC   = c('A','L','N','C')[2]#, #All Local National ---
  typerC  = typeC#,
  zerorefC =F#, #set reference asset NULL
  showtradetriangle=F
  x00 <<-  copy(f241021ad)
  geoplus <<- copy(x00$geoplus)
  estdt <<- copy(x00$estdt)
  rss <<- copy(x00$rss)
  # showtradetriangle <<- showtradetriangle
  #}rc6tc
  #f241105a()
  #------------------------------------Reactive
  geo0R <- #full geo (itriC tbinC rc6tC)    #many - but no reactives
    geoplus[type=='L'][itrim==itriC][tbin==tbinC][,.(nx,gx,lab=des,rc6=rc9,rc3=substr(rc9,1,3),ltile=as.numeric(substr(des,4,4)))]%>%
    z110[.,on=c(rcx='rc6')]%>%.[,.(nx,gx,lab,rc3,rc6=rcx,ltile)]
  
  output$map <- renderLeaflet(#national map                         1.1   ----
                              x111()
  )
  
  pc6tR <- #convert irregular input to regular rc6
    eventReactive( 
      input$rc6tC,
      {
        regpcode(input$rc6tC)
      }
    )
  
  
  geoaR <- #area geo  (itriC tbinC rc6tC)  all rc6 in area
    eventReactive( 
      pc6tR(),
      {
        print('update geoaR')
        print(pc6tR())
        rc6tloc <- pc6tR()
        print(paste0('in geoaR'))
        print(rc6tloc)
        geo0R%>%
          .[rc3==substr(rc6tloc,1,3)]       #select single area 'a'
      }
    )
  
  geodR <- #district geo  (itriC tbinC rc6tC) 
    eventReactive( 
      pc6tR(),
      {
        rc6tloc <- pc6tR()
        print(paste0('in geodR'))
        print(rc6tloc)
        geoaR()%>%
          .[rc6==rc6tloc]                   #select single district 'd' is target
      }
    )
  
  geotR <-                                  
    eventReactive( 
      list(geoaR(),geodR()),
      {
        geoaR()[geodR()[,.(ltile)],on=c(ltile='ltile')] #select single tile 'ltile' from area, all tile-peers of target
      }
    )
  
  nxaR <- #area peers (itriC tbinC rc6tC)    peer nx within area 
    eventReactive( 
      geoaR(),
      {
        geoaR()[,.(nx,rc3,ltile,lab)]%>%
          unique(.)
      }
    )
  
  nxtR <- #target tertile(itriC tbinC rc6tC)  peer geo for target
    eventReactive( 
      geotR(),
      {
        geotR()[,.(nx,rc3,ltile,lab)]%>%
          unique(.)
      }
    )
  
  estdtaR <- #area peers (itriC tbinC rc6tC)  
    eventReactive( 
      nxaR(),
      {
        print(paste0('in estdtaR'))
        print(pc6tR())
        estdt[nxaR(),on=c(nx='nx')]%>%       #select estdt for area 'a' 
          .[,.(nx,date,ii,lab=i.lab,rc3,ltile,xdotd,days,xdot,x)] #estdtR[,.N,lab][,diff(range(N))]%>%`==`(x=.,y=0)%>%stopifnot(.)
      }
    )
  
  estdttR <- #target tertile (itriC tbinC rc6tC)  many
    eventReactive( 
      nxtR(),     #select nx for target 't'
      {
        estdt[nxtR(),on=c(nx='nx')]%>%
          .[,.(nx,date,ii,lab=i.lab,rc3,ltile,xdotd,days,xdot,x)]
      }
    )
  
  rssaR <- #area peers (itriC tbinC rc6tC)       122 
    eventReactive( 
      nxaR(),
      {
        rss[nxaR(),on=c(nx='nx')]#  select rss for area 'a'
      }
    )
  
  ylimR <-
    eventReactive( 
      estdtaR(),
      {
        estdtaR()[,range(x)]*1.1
      }
    )
  
  #---1.1.1 map---------------------------------------------------------------------
  #prep for render
  x111 <-         
    eventReactive( 
      pc6tR(),
      {
        print('update x111')
        x1 <- #area geo  (itriC tbinC rc6tC)      #many
          geo0R%>%
          .[rc3==substr(pc6tR(),1,3)]
        x1%>% #lab=rc3-q and both parts are used here
          #  .[rc3==rc3xR]%>% #rc3 match
          .[,.(
            rc6,
            col=lighten(colx,lightenx)[.BY[[1]]],
            ltile,
            lab #include label
          ),by=.(ltile)]%>%
          .[
            rc6==pc6tR(), #target district
            col:=colx[.BY[[1]]],
            by=.(ltile)
          ]%>%#[as.numeric(substr(lab,4,4))]  #overwrite target: colour without lightening
          .[]%>%
          f240810b(.,x2=pxosrdo2ddR(),pva=z110R(),minzoom=9) #fields rc6,col
      }
    )
  output$map <- 
    renderLeaflet(#national map                         1.1   ----
                  x111()
    )
  
  #----1.1.2 x(t)-------------------------------------------------------------------
  
  x112 <- eventReactive(
    list(input$tslider,pc6tR(),estdtaR()),
    {
      print('update x112')
      print(estdtaR())
      tstartC <- as.integer(input$tslider)
      
      x0 <- setNames(cobalt()[c('punk','green','blue')],as.character(1:3))
      x3 <- estdtaR()[,.SD[,.(ifelse(tstartC==0,0,x[tstartC]))],.(ltile)][,mean(V1)]
      x2 <-
        #estdtaR%>%
        estdtaR()[,.SD[,.(ii,date,lab,x=x-ifelse(tstartC==0,0,x[tstartC]))],.(ltile)]%>%
        .[,qq:=as.factor(ltile)]%>%
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
          panel.grid.major = element_line(size=pgms,linetype = pgmt,color=pgmc),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text=element_text(size=16,face='plain'),
          axis.line.y.left=element_line(size=.1),
          axis.line.x.bottom=element_line(size=.1),
          legend.position='none')+
        scale_color_manual(values=x0)+#cobalt()[2:4]  as.character(cobalt())
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
    list(input$tslider,pc6tR(),x101R()),
    {
      tslidex <- input$tslider
      d1 <- #daily
        seq.Date(from=min(x101R()),to=max(x101R()),by='d')
      d2 <- #yearend + final date
        x101R()%>%
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
  
  nfig1 <- 3 #for T2
  nfig2 <- -1 #for ppm2
  nfig3 <- 4 #for frac
  x122 <- eventReactive(
    list(pc6tR(),rssaR()),
    {
      x1 <- 
        z110R()[rssaR(),on=c(rcx='rc6')]%>%
        .[,.(
          frac=round(sum(nid)/z110R()[nchar(rcx)==6,sum(nid)],nfig3),
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
  # x122 <- x1
  output$x122 <- 
    render_gt(
      x122()
    )
  
  #----1.3.1 summary
  x131 <- eventReactive(
    list(input$tslider,estdtaR()),
    {
      tstartC <- input$tslider
      x1 <- 
        estdtaR()%>%
        .[ii>=tstartC]%>%
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
    list(input$tslider,geotR(),estdttR()),
    {
      tstartC <- input$tslider
      steprip <- '03rip/'
      x0 <-
        geotR()[,rc6]%>%
        coread(.,steprip)%>% #or rc6tC
        .[,.(N=.N,mean=round(mean(as.numeric(retsa)),4)),.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))]%>%
        .[(buy>=estdttR()[ii>=tstartC,substr(min(as.character(date)),1,4)])]
      x1 <- 
        x0%>%
        dcast(.,
              buy~sell,
              value.var='mean'
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
    list(pc6tR(),geoaR()),
    {
      pc6tx <- pc6tR()
      x1 <-
        data.table(tbin=1:3,freq=c('lo','hi','an'))
      x2 <- rss%>%
        .[geotR(),on=c(rc6='rc6')]%>%
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
        )#%>%
      # gt::tab_footnote(
      #   footnote=f241108a(typeC,tbinC)[[2]]
      # )
      x4
    }
  )
  #x211 <- x4
  output$x211 <- render_gt(x211())
  
  #----2.2.1  accuracy  trim
  x221 <- eventReactive(
    list(pc6tR(),geoaR()),
    {
      pc6tx <- pc6tR()
      x1 <-
        data.table(itrim=1:3,threshold=c('0.0','0.1','0.5'))
      x2 <-
        rss%>%
        .[geotR(),on=c(rc6='rc6')]%>%
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
  #x221 <- x4
  output$x221 <- 
    render_gt(
      x221()
    )
  
  #----2.3.1  accuracy  in/out
  
  x231 <- eventReactive(
    list(pc6tR(),geoaR()),
    {
      pc6tx <- pc6tR()
      x1 <-
        rss%>%
        .[geotR(),on=c(rc6='rc6')]%>%
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
      setnames(x2,c('domain','index.average',pc6tR())[1:ncol(x2)])
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
  #x231 <- x4
  output$x231 <- 
    render_gt(
      x231()
    )
  
  #----3.1.1 listing
  x311 <- eventReactive(
    list(estdttR(),geotR()), #geo0R is not reactive
    {
      x0 <-
        geo0R[z110R(),on=c(rc6='rcx'),nomatch=NULL]%>%
        .[,.(ppm2=sum(pv)/sum(m2)),.(gx,nx)]%>%
        .[1,-c('ppm2')]
      x1 <-
        fread('f241024ad.csv')%>%
        .[order(tbin,gx,date)]%>%.[,let(t,1:.N),.(tbin,gx)]%>%
        .[tbin==tbinC]%>%
        .[geodR(),on=c(gx='gx')]%>%
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
        geotR()[
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
  #x311 <- x4
  output$x311 <- 
    render_gt(
      x311()
    )
  
  
  #----4.1.1 constituents
  x411 <- eventReactive(
    list(geo0R), #geo0R is not reactive
    {
      x1 <- 
        geo0R[,.(rc3,rc6,ltile)]%>%
        z110R()[.,on=c(rcx='rc6')]%>%
        .[,.(rc3,rc6=rcx,nid,ppm2=round(ppm2),quantile=paste0('local-',ltile))]
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
  #x411 <- x2
  output$x411 <- 
    DT::renderDT(
      x411()
    )
}

shinyApp(ui, server)


