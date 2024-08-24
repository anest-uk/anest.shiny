#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#----------------------------------shared across sessions
setwd("C:/Users/Giles/anest.repo/anest.shiny/acycle")
pgmt='dotted'
pgmc='grey80'
pgms=.2

#---------------------CRAN package
library(bslib)
library(data.table)
library(ggplot2)
library(gridlayout)
library(leaflet)
library(magrittr)
library(shiny)
library(shinyWidgets)
library(sp)

#---------------------function lib
source('applib.R')

#---------------------data
vbl <- #saved in rdata with own name
  c(
    'z421',
    'z321',
    'pxosrdo2dd'
    )
rmifgl(vbl)
for(i in seq_along(vbl)) {load(paste0(vbl[i],'.Rdata'))}



#-------------------------------------------------------------------------------ui
ui <- page_sidebar(
  title = "Index",
  sidebar = sidebar(
    textInput("tgtrc6",label='Target RC6',value='WV-1--'),
    width=140
  ),
  
  navset_card_underline(
    nav_panel(
      title = "National", 
      grid_page(
        layout = c(
          "header  header header",
          "graphic table map"
        ),
        row_sizes = c(
          "1fr"  
        ),
        col_sizes = c(
          ".8fr"
        ),
        gap_size = "4rem",
        
        grid_card(
          p("graphic"),
          plotOutput('estdt.nat.p')
        ),
        grid_card(
          p("table"),
          div(
            tableOutput('estdt.nat.t')
          , style = "font-size:65%")
        )
        ,
        grid_card(
          p("map"),
          leafletOutput('geo.nat.l')
        )
      )
    ),
    nav_panel(
      title = "Local", 
      p("graphic"),
      card(p("graphic")),
      p("map"),
      card(p("map")),
      p("table")
    ),
    nav_panel(title = "Custom", 
              sidebarLayout(
                sidebarPanel(
                  card(
                    sliderInput("bins",
                                "Placeholder",
                                min = 1,
                                max = 50,
                                value = 30)
                  ),
                  width=2
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  p("map")
                )#,
                # tableOutput('table')
              )
    )
  )
)



#-------------------------------------------------------------------------------server
server <- function(input, output) {
  #----------------------------------per-session
  #national estdt
  output$estdt.nat.t <- 
    renderTable(
      z321$ses$estdt[nx==z321$geo[rc9==input$tgtrc6,nx]][,.(nx,date=as.Date(date1),days,xdot,xdotse,x,xse)]
    )
  #national timeseries
  output$estdt.nat.p <- renderPlot(
    z321$ses$estdt[nx==z321$geo[rc9==input$tgtrc6,nx]]%>%
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
        #text=element_text(size=7,face='plain'),
        axis.line.y.left=element_line(size=.1),
        axis.line.x.bottom=element_line(size=.1),
        #axis.text=element_text(size=6,face = "plain"),
        legend.position='none')+
      scale_x_date(
        breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
        date_labels = "%Y",
        limits=c(as.Date(c('1994-12-31','2027-12-31')))
      )
  )
  
  output$geo.nat.l <- renderLeaflet(
    z321$geo[nx==z321$geo[rc9==input$tgtrc6,nx],rc9]%>%
      f240810a(rcx=.,x3a=pxosrdo2dd)
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
