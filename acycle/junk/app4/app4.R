#---------------------CRAN package for gui
#source('CRANlibload.R') #causes failure once deployed
library(magic)

library(broom)
library(bslib)
library(car) #linear hypothesis test
library(colorspace)
library(data.table)
library(devtools)
library(ggplot2)    
library(ggrepel)   
library(grid)
library(gridlayout)
library(gt)
library(gtExtras)
library(htmltools)
library(leaflet)
library(lubridate)
library(magrittr)   
library(PerformanceAnalytics)   
library(plotly)
library(scales)
library(shiny)
library(shinyWidgets)
library(sp)
library(zoo)


#----------------------------------shared across sessions
pgmt='dotted'
pgmc='grey50'
pgms=.2

load('t4dump.Rdata',envir=globalenv())

dfnx <- seq.Date(from=as.Date('1994-12-31'),to=as.Date('2024-12-31'),by='y')
#---------------------function lib
source('c-cleanlib.R')
source('rctree.R') #f240824b() : rctree
source('headerscript.R') #geo and dfn
rcx <<- c('SW-','AL-','M--')

#---------------------function: map colours
palette=cobalt()[c(2,4)]
pal <- leaflet::colorNumeric(palette=cobalt()[c(2,4)],domain=0:1)


ui <- grid_page(
      tags$head(tags$link(rel="shortcut icon", href="fav.ico")),
  

  layout = c(
    "header  header  ",
    "sidebar plot "
  ),
  row_sizes = c(
    "50px",
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
      em(""),
      textInput(
        inputId = "tgtrc6",
        label = "Target district",
        value = "SW3"
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Repeat Sales Index",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "plot",
    card_header("Estimates"),
    card_body(
      tabsetPanel(
        nav_panel(
          title = "National",
          grid_container(
            layout = c(
              "estdtnatp leafletnat",
              "perfnatt0 tab4natt ",
              "perfnatt2 perfnatt3 ",
              "perfnatt1 ."
            ),
            row_sizes = c(
              "1fr",
              "1fr",
              "1fr",
              "1fr"
            ),
            col_sizes = c(
              ".37fr",
              ".37fr"
            ),
            gap_size = "10px",
            grid_card(
              area="leafletnat",
              leafletOutput('geonatl'),
              max_height="500px"
            ),
            grid_card(
              area = "estdtnatp",
              plotOutput('estdtnatp'),
              max_height="500px"
            ),
            grid_card(
              area = "perfnatt0",
              'RSI log price change',
              div(
                gt_output('perfnatt0')
              ),
              max_height="400px"
            ),
            grid_card(
              area = "perfnatt1",
              'transaction count',
              div(
                gt_output('perfnatt1')
              ),
              max_height="500px"
            ),
            grid_card(
              area = "perfnatt2",
              'bought\\sold average log price change',
              
              div(
                gt_output('perfnatt2')
              ),
              max_height="500px"
            ),
            grid_card( #charac table
              area='tab4natt',
              'price-bin characteristics',
              div(
                gt_output('tab4natt'),
                max_height="400px"
              )
            ),
            grid_card( #charac table
              area='perfnatt3',
              'time-series summary',
              div(
                gt_output('perfnatt3'),
                max_height="500px"
              )
            )
            
            
            # ,
            #     card_header(
            #       "table 4"
            #     )
            #   )
            # ),
            # grid_card(
            #   area = "binchanat",
            #   full_screen = TRUE,
            #   card_header(
            #     "P-bins
            #                                     "
            #   )
            # )
            #)
          )
        ),
        nav_panel(
          title = "Custom",
          grid_container(
            layout = c(
              "custom_control xchartcus leafletcus",
              "custom_control . binchacus"
            ),
            row_sizes = c(
              "1fr",
              "1fr"
            ),
            col_sizes = c(
              ".2fr",#"0.36fr",
              ".37fr",
              ".37fr"
            ),
            # row_sizes = c(
            #   "1fr",
            #   ".4fr"
            # ),
            # col_sizes = c(
            #   "1fr",#"0.36fr",
            #   "1.37fr",
            #   "1.27fr"
            # ),
            gap_size = "10px",
            grid_card(
              area = "custom_control",
              
              card_body(
                'Index estimation',
                # checkboxGroupInput(
                #   inputId = "builtform",
                #   label = "Built form",
                #   choices = list("flat" = "a", "house" = "b")
                # ),
                # checkboxGroupInput(
                #   inputId = "Used",
                #   label = "Used",
                #   choices = list("Used" = "a", "New" = "b")
                # ),
                radioButtons(
                  inputId = "Type",
                  label = "Type",
                  choices = list("House" = "H", "Flat" = "F", "All" = "."),
                  width = "100%",
                  inline=T,
                  selected='.'
                ),
                radioButtons(
                  inputId = "Used",
                  label = "Used",
                  choices = list("Used" = "U", "New" = "N", "All" = "."),
                  width = "100%",
                  inline=T,
                  selected='.'
                ),
                actionButton(inputId = "go.custom.b", label = "Update index", 
                             style="color: #222222; background-color: #ffffff; border-color: #2e6da4; padding:4px; font-size:75%; width:50%"),
                
                
                #downloadButton("downloadData", "Download")
                actionButton(inputId = "downloadData", label = "Download index", #,icon("paper-plane"
                             style="color: #222222; background-color: #ffffff; border-color: #2e6da4; padding:4px; font-size:75%; width:50%")
                ,
                checkboxInput(inputId = "fixedscale",'fixed y-scale',T)
                ,
                #style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                # radioButtons(
                #   inputId = "timebinning",
                #   label = "Time bins",
                #   choices = list("annual" = "a", "semi-annual" = "b", "DRC" = "value3"),
                #   width = "100%"
                # ),
                treeInput( #districts
                  inputId = "customtree",
                  label = "Select districts:",
                  choices = create_tree(f240824b(unique(substr(dir('03rip/'),1,3)))),
                  selected = "SW-2--",
                  returnValue = "text",
                  closeDepth = 1
                )
                
              )
            ),
            grid_card(
              area = "xchartcus",
              plotOutput("rsi"),
              max_height="500px"
            ),
            grid_card(
              area="leafletcus",
              leafletOutput('geocusl'),
              max_height="500px"
            ),
            grid_card(
              area = "binchacus",
              'All properties',
              gt_output('binchacus'),
              max_height="400px"
              #full_screen = TRUE,
              #card_header(
              #  "P-bins
              #  "
            )
          )
        ),
        nav_panel(
          title = "Local",
          grid_container(
            layout = c(
              "xchartloc leafletloc",
              "permatloc binchaloc "
            ),
            row_sizes = c(
              "1fr",
              "1fr"
            ),
            col_sizes = c(
              ".37fr",
              ".37fr"
            ),
            gap_size = "10px",
            grid_card_plot(area = "xchartloc"),
            grid_card_plot(area = "leafletloc"),
            grid_card(
              area = "permatloc",
              full_screen = TRUE,
              card_header("Delta log price table")
            ),
            grid_card(
              area = "binchaloc",
              full_screen = TRUE,
              card_header("P-bins")
            )
          )
        )
        
      )
    )
  )
)

server <- function(input, output) {
  steprip <- '03rip/'
  x.nat.t4 <-
    f231204a(2)%>%
    .[,.(
      p.bin=paste0(
        formatC(round(ppm2min,-1), format="f", big.mark=",", digits=0),'-',
        formatC(round(ppm2max,-1), format="f", big.mark=",", digits=0)
      ),
      p=formatC(round(ppm2,-1), format="f", big.mark=",", digits=0),
      np=nx,
      frac=round(fid,4),
      R2rsi=round(r2rsi,3),
      beta=round(b1/mean(b1),2)
    )]
  
  
  
  

    
  output$perfnatt3 <-  #stats
    render_gt(
      #nxx <- z321a$geo[rc9==regpcode(input$tgtrc6),nx],
      #nxx <- 1
      #x1 <- dcast(coread(geon[nx==z321a$geo[rc9==regpcode(input$tgtrc6),nx],rc9],steprip)[,.N,.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))],buy~sell,value.var='N')
      #x2 <-  dcast(coread(geon[nx==z321a$geo[rc9==regpcode(input$tgtrc6),nx],rc9],steprip)[,.(r=round(mean(as.numeric(retsa)),3)),.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))],buy~sell,value.var='r')
      zoo(z321$pan[,-'date'],z321$pan[,date])%>%
        table.Stats(.,digits=3)%>%
        data.table(.,keep.rownames = T)%>%
        `[`(.,i=-c(2,7))%>%
        setnames(.,c('.',paste0('np=',1:10)))
      #x3[]
      # f240823a(z321a,nx=z321a$geo[rc9==regpcode(input$tgtrc6),nx])%>%
      #   .[]%>%
      #   gt::gt(. ,rownames_to_stub = T)
      
    ) 
  
  output$perfnatt0 <-  #gt winding
    render_gt(
      #nxx <- z321a$geo[rc9==regpcode(input$tgtrc6),nx],
      #nxx <- 1
      #x1 <- dcast(coread(geon[nx==z321a$geo[rc9==regpcode(input$tgtrc6),nx],rc9],steprip)[,.N,.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))],buy~sell,value.var='N')
      #x2 <-  dcast(coread(geon[nx==z321a$geo[rc9==regpcode(input$tgtrc6),nx],rc9],steprip)[,.(r=round(mean(as.numeric(retsa)),3)),.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))],buy~sell,value.var='r')
      x3 <- z321a$pan[,date:=as.character(date)][,c(1,z321a$geo[rc9==regpcode(input$tgtrc6),nx]+1),with=F][date=='2009-02-28',date:='2008-12-31']%>%
        setnames(.,c('date','xdot'))%>%
        .[,.(decade=substr(date,1,3),yr=substr(date,4,4),xdot=round(xdot,3))]%>%
        dcast(.,decade~yr,value.var='xdot')%>%
        .[,decade:=c(1990,2000,2010,2020)]
      #x3[]
      # f240823a(z321a,nx=z321a$geo[rc9==regpcode(input$tgtrc6),nx])%>%
      #   .[]%>%
      #   gt::gt(. ,rownames_to_stub = T)
      
    ) 
  
  output$perfnatt1 <-  #gt winding
    render_gt(
      #nxx <- z321a$geo[rc9==regpcode(input$tgtrc6),nx],
      #nxx <- 1
      x1 <- dcast(coread(geon[nx==z321a$geo[rc9==regpcode(input$tgtrc6),nx],rc9],steprip)[,.N,.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))],buy~sell,value.var='N')
      #x2 <- dcast(coread(geon[nx==z321a$geo[rc9==regpcode(input$tgtrc6),nx],rc9],steprip)[,.(r=mean(as.numeric(retsa))),.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))],buy~sell,value.var='r')
      #x3 <- z321a$pan[,date:=as.character(date)][,c(1,nxx+1),with=F][date=='2009-02-28',date:='2008-12-31']%>%setnames(.,c('date','xdot'))%>%.[,.(decade=substr(date,1,3),yr=substr(date,4,4),xdot)]%>%dcast(.,decade~yr,value.var='xdot')%>%.[,decade:=c(1990,2000,2010,2020)]
      #x3[]
      # f240823a(z321a,nx=z321a$geo[rc9==regpcode(input$tgtrc6),nx])%>%
      #   .[]%>%
      #   gt::gt(. ,rownames_to_stub = T)
      
    )
  output$perfnatt2 <-  #gt winding
    render_gt(
      #nxx <- z321a$geo[rc9==regpcode(input$tgtrc6),nx],
      #nxx <- 1
      #x1 <- dcast(coread(geon[nx==z321a$geo[rc9==regpcode(input$tgtrc6),nx],rc9],steprip)[,.N,.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))],buy~sell,value.var='N')
      x2 <-  dcast(coread(geon[nx==z321a$geo[rc9==regpcode(input$tgtrc6),nx],rc9],steprip)[,.(r=round(mean(as.numeric(retsa)),3)),.(buy=substr(buydate,1,4),sell=substr(selldate,1,4))],buy~sell,value.var='r')
      #x3 <- z321a$pan[,date:=as.character(date)][,c(1,nxx+1),with=F][date=='2009-02-28',date:='2008-12-31']%>%setnames(.,c('date','xdot'))%>%.[,.(decade=substr(date,1,3),yr=substr(date,4,4),xdot)]%>%dcast(.,decade~yr,value.var='xdot')%>%.[,decade:=c(1990,2000,2010,2020)]
      #x3[]
      # f240823a(z321a,nx=z321a$geo[rc9==regpcode(input$tgtrc6),nx])%>%
      #   .[]%>%
      #   gt::gt(. ,rownames_to_stub = T)
      
    ) 
  
  output$tab4natt <-  #gt characteristics table 4
    render_gt(
      gt::gt(x.nat.t4)%>%
        cols_label(
          frac = html('Fraction<br>properties'),
          R2rsi = html("RSI R<sup>2</sup>"),
          p = html("Aggregate"),
          p.bin=html("Range")
        )%>%
        tab_spanner(
          label = html("Bin £/m<sup>2</sup>"),
          columns = c(p.bin, p)
        )%>%
        gt_highlight_rows(
          .,
          columns = gt::everything(),
          rows = z321a$geo[rc9==regpcode(input$tgtrc6),11-nx], #reversed order
          fill = cobalt()['green'], #"#80bcd8"
          alpha = 0.1, #v pale
          font_weight = "normal",
          #font_color = "#000000",
          #bold_target_only = FALSE,
          target_col = c()
        )
      
    )
  Restdtnatp <-     eventReactive(
    input$tgtrc6,
    {
      x <- z321a$ses$estdt[nx==z321a$geo[rc9==regpcode(input$tgtrc6),nx]]%>%
        ggplot(.,aes(date1,x))+
        geom_line()+
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
        scale_x_date(
          breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
          date_labels = "%Y",
          limits=c(as.Date(c('1994-12-31','2027-12-31')))
        )
      
      x
    }
  )
  
  output$estdtnatp <- #ggplot x
    renderPlot(
      Restdtnatp()
    )
  
  output$geonatl <- #leaflet np
    renderLeaflet(
      z321a$geo[nx==z321a$geo[rc9==regpcode(input$tgtrc6),nx],rc9]%>%
        f240810a(rcx=.,x3a=pxosrdo2dd,target=regpcode(input$tgtrc6),pva=z110,palx=pal,maxzoom=12)
    )
  
  output$binchacus <-  #pva 
    render_gt(
      z110[rcx%in%input$customtree[which(nchar(input$customtree)==6)],.(pc=irregpcode(rcx),ppm2=round(ppm2,-1),nid,m2bar=round(m2/nid),pvbar=round(pv/(1000*nid)))]%>%
        gt::gt(.)%>%
        fmt_number(columns = 1:4,sep_mark = ",",decimals=0) %>% 
        cols_label(
          pc = '',
          nid = html("Tracked properties"),
          ppm2 = html("£/m<sup>2</sup>"),
          m2bar = html("Floor area (m<sup>2</sup>)"),
          pvbar = html("Present value (£000)")
        )%>%
        tab_spanner(
          label = html("Average"),
          columns = c(m2bar, pvbar)
        )%>%
        tab_options(
          table.align = "left"
        )
    )
  
  
  output$geocusl <- #leaflet cust
    renderLeaflet(
      input$customtree[which(nchar(input$customtree)==6)]%>%
        f240810a(rcx=.,x3a=pxosrdo2dd,target=regpcode(input$tgtrc6),pva=z110,palx=pal,maxzoom=12)
    )
  
  #--------------------------------------------------custom
  
  Rselectedrc <- #rc
    eventReactive(
      input$go.custom.b,
      {
        #print(input$customtree[which(nchar(input$customtree)==6)])
        input$customtree[which(nchar(input$customtree)==6)]
      }
    )
  
  Rgeo <- #Rselectedrc -> geo [->Rrsi]
    eventReactive(
      input$go.custom.b,
      {
        data.table(
          rc9=Rselectedrc(),
          nx=1,
          lab='lab001'
        )
      }
    )
  
  Rrsi0 <- #Rgeo -> RSI 
    eventReactive(
      eventExpr=
        list(
          input$go.custom.b,
          input$Used, #generate reactivity in table
          input$Type,
          input$customtree
        ),
      {
        x0 <- list(
          input$go.custom.b,
          input$Used,
          input$Type,
          input$customtree
        )
        x <- f230312x(  #solve single nx -> estdt with no pra
          nxx=1,
          steprip='03rip/',
          dfn=dfnx,
          geo=Rgeo(),
          houseflat=input$Type,
          newused=input$Used
        )
        if(any(x[,is.na(x)])) {xna <<- x; print('na found');x <- x[is.na(xdot),xdot:=0][,x:=cumsum(xdot)][xdot==0,x:=NA];print(x)}
        print(input$customtree)
        x
      }
    )
  
  Rrsi <- 
    eventReactive(
      eventExpr=
        list(
          input$go.custom.b#,
          # input$Used,    generate no reactivity
          # input$Type,
          # input$customtree
        )
      ,
      {
        x <- Rrsi0()
        # x0 <- input$Used
        # x1 <- input$Type
        x1 <- 
          ggplot(
          x,
          aes(date,x)
        )+
          geom_line()+
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
          scale_x_date(
            breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
            date_labels = "%Y",
            limits=c(as.Date(c('1994-12-31','2027-12-31')))
          )
        if(input$fixedscale==T){
          x1 <- x1+ylim(c(-.1,3))
        }
        x1
      }
    )
  
  
  output$geo <-
    render_gt(
      Rgeo()[1:3]
    )
  
  output$selected_var <- #render the string
    renderText({
      Rselectedrc()
    })
  
  output$rsi <- #solve rsi
    renderPlot({
      Rrsi()#+
      #geom_line()%>%
      #print(.)
    })
}

#-------------------------------------------------------------------------------Run
shinyApp(ui = ui, server = server)


