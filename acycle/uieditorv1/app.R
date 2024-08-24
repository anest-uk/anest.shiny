#----------------------------------shared across sessions
pgmt='dotted'
pgmc='grey50'
pgms=.2

load('t4dump.Rdata',envir=globalenv())

#---------------------CRAN package for gui
library(htmltools)
library(leaflet)
library(sp)
library(shinyWidgets)

#---------------------function lib
source('c-cleanlib.R')
source('rctree.R') #for f240824b()
rcx <<- c('SW-','AL-','M--')

#---------------------function: map colours
pal <- leaflet::colorNumeric(palette=cobalt()[c(2,4)],domain=0:1)

#from uieditor
library(shiny)
library(plotly)
library(gridlayout)
library(bslib)


ui <- grid_page(
  layout = c(
    "header  header  ",
    "sidebar plot "
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
    content = "RSI",
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
              # "estdtnatp leafletnat",
              # "permatnat binchanat "
              "estdtnatp leafletnat",
              "perfnatt . ",
              ". . "
            ),
            row_sizes = c(
              ".5fr",
              ".4fr",
              "1fr"
            ),
            col_sizes = c(
              ".3fr",
              ".3fr"
            ),
            gap_size = "10px",
            grid_card(
              area="leafletnat",
              leafletOutput('geo.nat.l')
            ),#,
            grid_card(
              area = "estdtnatp",
              plotOutput('estdt.nat.p')
            ),
            grid_card(
              area = "perfnatt",
              div(
                gt_output('perfnatt'),
                style = "font-size:25%"
              )
            ),
            card_header(
              "Delta log price table"
            )
          )
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
            "1fr",
            "1fr"
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
      ),
      nav_panel(
        title = "Custom",
        grid_container(
          layout = c(
            "custom_control xchartcus leafletcus",
            ".              permatcus binchacus "
          ),
          row_sizes = c(
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "0.36fr",
            "1.37fr",
            "1.27fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "custom_control",
            card_body(
              checkboxGroupInput(
                inputId = "bultform",
                label = "Built form",
                choices = list("flat" = "a", "house" = "b")
              ),
              checkboxGroupInput(
                inputId = "Used",
                label = "Used",
                choices = list("Used" = "a", "New" = "b")
              ),
              radioButtons(
                inputId = "timebinning",
                label = "Time bins",
                choices = list("annual" = "a", "semi-annual" = "b", "DRC" = "value3"),
                width = "100%"
              ),
              checkboxInput(
                inputId = "treeselect",
                label = "Postcodes",
                value = FALSE
              )
            )
          ),
          grid_card_plot(area = "xchartcus"),
          grid_card_plot(area = "leafletcus"),
          grid_card(
            area = "permatcus",
            full_screen = TRUE,
            card_header("Delta log price table")
          ),
          grid_card(
            area = "binchacus",
            full_screen = TRUE,
            card_header(
              "P-bins
                "
            )
          )
        )
      )
    )
  )
)
)


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
  
  output$perfnatt <-  #gt estdt
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
        ) ,
      height=pct(10)
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