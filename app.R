#----------------------------------shared across sessions
# setwd("C:/Users/Giles/anest.repo/anest.shiny/acycle")
# load(file='z110.Rdata')
pgmt <- "dotted"
pgmc <- "grey80"
pgms <- .2

#---------------------CRAN package
library(bslib)
# library(data.table)
# library(ggplot2)
# library(gt)
# library(htmltools)
# #library(kableExtra)
# library(leaflet)
# library(magrittr)
# library(scales)
# library(sp)

library(gridlayout)
library(shinyWidgets)
library(shiny)

#---------------------function lib
# source('applib.R')
# pal <- leaflet::colorNumeric(palette=cobalt()[c(2,4)],domain=0:1)

#-------------------------------------------------------------------------------ui
ui <- page_sidebar(
  title = "Index",
  sidebar = sidebar(
    textInput("tgtrc6", label = "Target RC6", value = "WV-1--"),
    width = 140
  ),
  navset_card_underline(
    # nav_panel(
    #   title = "National",
    #   grid_page(
    #     layout = c(
    #       "header  header  header header header",
    #       "spacel graphic table map spacer"
    #     ),
    #     row_sizes = c(
    #       "1fr"
    #     ),
    #     col_sizes = c(
    #       ".03fr",".7fr",".7fr",".7fr",".03fr"
    #     ),
    #     gap_size = "2rem"
    #     ,
    #     grid_card(p('spacel'))
    #     ,
    #     grid_card(
    #       p("graphic"),
    #       plotOutput('estdt.nat.p')
    #     )
    #     ,
    #     grid_card(
    #       p("map"),
    #       leafletOutput('geo.nat.l')
    #     ),
    #     grid_card(
    #       p("table"),
    #       div(
    #         gt_output('estdt.nat.t'),
    #         style = "font-size:65%")
    #     )
    #     ,
    #     grid_card(p('spacer'))
    #   )
    # ),
    # nav_panel(
    #   title = "Local",
    #   p("graphic"),
    #   card(p("graphic")),
    #   p("map"),
    #   card(p("map")),
    #   p("table")
    # ),
    # nav_panel(title = "Custom",
    #           sidebarLayout(
    #             sidebarPanel(
    #               card(
    #                 sliderInput("bins",
    #                             "Placeholder",
    #                             min = 1,
    #                             max = 50,
    #                             value = 30)
    #               ),
    #               width=2
    #             ),
    #
    #             mainPanel(
    #               p("map")
    #             )
    #           )
    # )
  )
)

#-------------------------------------------------------------------------------server
server <- function(input, output) {
  #----------------------------------per-session
  #---------------------data
  # vbl <- #saved in rdata with own name
  #   c(
  #     'z110',
  #     'z421',
  #     'z321',
  #     'pxosrdo2dd'
  #   )
  # rmifgl(vbl)
  # for(i in seq_along(vbl)) {load(paste0(vbl[i],'.Rdata'))}
  #
  #
  # #national estdt
  # output$estdt.nat.t <-  #gt estdt
  #   render_gt(
  #     z321$ses$estdt[nx==z321$geo[rc9==input$tgtrc6,nx]]%>%
  #       .[,.(nx,date=as.Date(date1),days,xdot=round(xdot,4),xdotse=round(xdotse,4),x=round(x,4),xse=round(xse,4))]#,
  #   )
  # output$estdt.nat.p <- #ggplot x
  #   renderPlot(
  #     z321$ses$estdt[nx==z321$geo[rc9==input$tgtrc6,nx]]%>%
  #       ggplot(.,aes(date1,x))+
  #       geom_line()+
  #       xlab('')+
  #       ylab(bquote(Delta~P~log~price~change))+
  #       theme_bw() +
  #       theme(
  #         axis.line = element_line(colour = "black"),
  #         panel.grid.major = element_line(size=pgms,linetype = pgmt,color=pgmc),
  #         panel.grid.minor = element_blank(),
  #         panel.border = element_blank(),
  #         panel.background = element_blank(),
  #         #text=element_text(size=7,face='plain'),
  #         axis.line.y.left=element_line(size=.1),
  #         axis.line.x.bottom=element_line(size=.1),
  #         #axis.text=element_text(size=6,face = "plain"),
  #         legend.position='none')+
  #       scale_x_date(
  #         breaks = as.Date(c('1995-01-01','2000-01-01','2010-01-01','2020-01-01','2024-01-01')),
  #         date_labels = "%Y",
  #         limits=c(as.Date(c('1994-12-31','2027-12-31')))
  #       )
  #   )
  #
  # output$geo.nat.l <- #leaflet np
  #   renderLeaflet(
  #     z321$geo[nx==z321$geo[rc9==input$tgtrc6,nx],rc9]%>%
  #       f240810a(rcx=.,x3a=pxosrdo2dd,target=input$tgtrc6,pva=z110,palx=pal)
  #   )
}

#-------------------------------------------------------------------------------Run
shinyApp(ui = ui, server = server)
