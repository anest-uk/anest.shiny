#----------------------------------shared across sessions
pgmt='dotted'
pgmc='grey50'
pgms=.2

load('t4dump.Rdata',envir=globalenv())

#---------------------CRAN package for gui
library(bslib)
library(htmltools)
library(leaflet)
library(sp)
library(gridlayout)
library(shinyWidgets)
library(shiny)

#---------------------function lib
source('c-cleanlib.R')
source('rctree.R') #for f240824b()
rcx <<- c('SW-','AL-','M--')

#---------------------function: map colours
pal <- leaflet::colorNumeric(palette=cobalt()[c(2,4)],domain=0:1)

#-------------------------------------------------------------------------------ui
ui <- page_sidebar(
  title = "Index",
  sidebar = sidebar(
    textInput("tgtrc6",label='Target RC6',value='SW15'),
    width=140
  ),
  navset_card_underline(
    nav_panel(
      title = "National",
      grid_page(
        layout = c(
          "space1-nat  header-nat     space3-nat     space4-nat",
          "spacel-nat  estdt-nat-p    geo-nat-l  spacer-nat",
          "spacel-nat  estdt-nat-t tab4-nat-t  spacer-nat"
        ),
        row_sizes = c(
          "50px",
          "1.5fr",
          "1.5fr"
        ),
        col_sizes = c(
          ".02fr",".3fr",".3fr",".02fr"
        ),
        gap_size = "2rem"
        ,
        grid_card_text(
          area = "header-nat",
          content = html("National £/m<sup>2</sup>"),
          alignment = "start",
          is_title = FALSE
        ),
        grid_card(area='space1-nat'),
        grid_card(area='space2-nat'),
        grid_card(area='space3-nat'),
        grid_card(area='space4-nat'),
        grid_card(area='spacel-nat'),
        grid_card(area='spacer-nat'),
        
        grid_card( #x(t) line
          area='estdt-nat-p',
          p("Index"),
          plotOutput('estdt.nat.p')
        )
        ,
        
        grid_card( #leaflet
          area='geo-nat-l',
          p("Districts"),
          leafletOutput('geo.nat.l')
        ),
        
        grid_card( #winding perf table
          area='estdt-nat-t',
          p("RSI estimation"),
          div(
            gt_output('perf.nat.t'),
            style = "font-size:65%")
        ),
        grid_card( #charac table
          area='tab4-nat-t',
          p("Bin/index characteristics"),
          div(
            gt_output('tab4.nat.t'),
            style = "font-size:65%")
        )
      )
    ),
    
    
    nav_panel(
      title = "Local",
      grid_page(
        layout = c(
          "space1-loc  header-loc     space3-loc     space4-loc"#,
          #"spacel-loc  estdt-loc-p    geo-loc-l  spacer-loc",
          #"spacel-loc  estdt-loc-t tab4-loc-t  spacer-loc"
        ),
        row_sizes = c(
          "50px"#,
          #"1.5fr",
          #"1.5fr"
        ),
        col_sizes = c(
          ".02fr",".3fr",".3fr",".02fr"
        ),
        gap_size = "2rem"
        ,
        grid_card_text(
          area = "header-loc",
          content = html("Local £/m<sup>2</sup>"),
          alignment = "start",
          is_title = FALSE
        ),
        grid_card(area='space1-loc'),
        grid_card(area='space2-loc'),
        grid_card(area='space3-loc'),
        grid_card(area='space4-loc'),
        grid_card(area='spacel-loc'),
        grid_card(area='spacer-loc')#,
        # grid_card( #x(t) line
        #   area='estdt-loc-p',
        #   p("Index")#,
        #   #plotOutput('estdt.nat.p')
        # )
        # ,
        # grid_card( #leaflet
        #   area='geo-loc-l',
        #   p("Districts")#,
        #   #leafletOutput('geo.nat.l')
        # ),
        # grid_card( #winding perf table
        #   area='estdt-loc-t',
        #   p("RSI estimation")#,
        #   # div(
        #   #   gt_output('perf.nat.t'),
        #   #   style = "font-size:65%"
        #   #   )
        # ),
        # grid_card( #charac table
        #   area='tab4-loc-t',
        #   p("Bin/index characteristics")#,
        #   # div(
        #   #   gt_output('tab4.nat.t'),
        #   #   style = "font-size:65%"
        #   #   )
        # )
      )
    ),
    # nav_panel(
    #   title = "Local",
    #   p("graphic"),
    #   card(p("graphic")),
    #   p("map"),
    #   card(p("map")),
    #   p("table")
    # ),
    nav_panel(
      title = "Custom",
      sidebarLayout(
        sidebarPanel(
          actionButton("go.custom.b", "Estimate"), #go
          h5(''),
          
          treeInput( #districts
            inputId = "ID1",
            label = "Select districts:",
            choices = create_tree(f240824b(rcx)),
            selected = "St. Albans-AL-",#"London-SW-",
            returnValue = "text",
            closeDepth = 0
          )
        ),
        
        mainPanel(
          p("map"),
          textOutput("selected_var"),
          plotOutput("rsi"),
          gt_output("geo")
        )
        
      )
    )
  )
)

#-------------------------------------------------------------------------------server
server <- function(input, output) {
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
  
  output$perf.nat.t <-  #gt estdt
    render_gt(
      f240823a(z321)%>%
        .[]%>%
        gt(. ,rownames_to_stub = T)
      
    )
  output$tab4.nat.t <-  #gt estdt
    render_gt(
      gt(x.nat.t4)%>%
        cols_label(
          frac = html('Fraction<br>properties'),
          R2rsi = html("RSI R<sup>2</sup>"),
          p = html("Aggregate"),
          p.bin=html("Range")
        )%>%
        tab_spanner(
          label = html("Bin £/m<sup>2</sup>"),
          columns = c(p.bin, p)
        ) 
    )
  output$estdt.nat.t <-  #gt estdt
    render_gt(
      z321$ses$estdt[nx==z321$geo[rc9==regpcode(input$tgtrc6),nx]]%>%
        .[,.(np=nx,date=as.Date(date1),days,xdot=round(xdot,4),xdotse=round(xdotse,4),x=round(x,4),xse=round(xse,4))]#,
    )
  output$estdt.nat.p <- #ggplot x
    renderPlot(
      z321$ses$estdt[nx==z321$geo[rc9==regpcode(input$tgtrc6),nx]]%>%
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
    )
  
  output$geo.nat.l <- #leaflet np
    renderLeaflet(
      z321$geo[nx==z321$geo[rc9==regpcode(input$tgtrc6),nx],rc9]%>%
        f240810a(rcx=.,x3a=pxosrdo2dd,target=regpcode(input$tgtrc6),pva=z110,palx=pal,maxzoom=12)
    )
  #--------------------------------------------------custom
  
  Rselectedrc <- #rc
    eventReactive(
      input$go.custom.b, 
      {
        input$ID1[which(nchar(input$ID1)==6)] 
      }
    )
  
  Rrdt <- #returns
    eventReactive(
      input$go.custom.b, 
      {
        Rselectedrow()
        coread(Rselectedrc(),'03rip/')[]
      }
    )
  
  Rgeo <- #geo
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
  
  Rrsi <- 
    eventReactive(
      input$go.custom.b, 
      {
        x <- f230312a(  #solve single nx -> estdt with no pra
          nxx=1,
          steprip='03rip/',
          dfn=dfnx,
          geo=Rgeo()
        )
        rsi.g <<- x
        ggplot(
          x,
          aes(ii,x)
        )+
          geom_line()
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
      Rrsi()+
        geom_line()%>%
        print(.)
    })
}

#-------------------------------------------------------------------------------Run
shinyApp(ui = ui, server = server)
