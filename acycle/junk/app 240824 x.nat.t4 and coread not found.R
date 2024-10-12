#----------------------------------shared across sessions
#setwd("C:/Users/Giles/anest.repo/anest.shiny/acycle")
pgmt='dotted'
pgmc='grey50'
pgms=.2

#---------------------CRAN package
library(bslib)
library(data.table)
library(ggplot2)
library(gt)
library(htmltools)
#library(kableExtra)
library(leaflet)
library(magrittr)
library(scales)
library(sp)

library(gridlayout)
library(shinyWidgets)
library(shiny)

#print(z221)
#---------------------function lib
source('applib.R')
source('rctree.R')
rctree <- #selected
  data.table(x)%>%
  .[rc3%in%c('SW-','AL-','M--'),]%>%
  .[.N:1,c('lab','rc6')]#%>%
  #as.data.frame(.)

#---------------------function: map colours
pal <- leaflet::colorNumeric(palette=cobalt()[c(2,4)],domain=0:1)

#-------------------------------------------------------------------------------ui
ui <- page_sidebar(
  title = "Index",
  sidebar = sidebar(
    textInput("tgtrc6",label='Target RC6',value='SW15'),
    width=140
  ),
  
  #   navset_card_underline(
  #   nav_panel(
  #     title = "National",
  #     grid_page(
  #       layout = c(
  #         "space1  header     space3     space4",
  #         "spacel  x-nat-p    geo-nat-l  spacer",
  #         "spacel  perf-nat-t bin-nat-t  spacer"
  #       ),
  #       row_sizes = c(
  #         "50px",
  #         "1.5fr",
  #         "1.5fr"
  #       ),
  #       col_sizes = c(
  #         ".02fr",".3fr",".3fr",".02fr"
  #       ),
  #       gap_size = "2rem"
  #       ,
  #       grid_card_text(
  #         area = "header",
  #         content = html("National £/m<sup>2</sup>"),
  #         alignment = "start",
  #         is_title = FALSE
  #       ),
  #       # grid_card_text(
  #       #   area = "desc",
  #       #   content = html("[how bins constructed]"),
  #       #   alignment = "start",
  #       #   is_title = FALSE
  #       # )
  #       # ,
  #       grid_card(area='space1'),
  #       grid_card(area='space2'),
  #       grid_card(area='space3'),
  #       grid_card(area='space4'),
  #       grid_card(area='spacel'),
  #       grid_card(area='spacer'),
  #       grid_card(
  #         area='x-nat-p',
  #         p("Index"),
  #         plotOutput('estdt.nat.p')
  #       )
  #       ,
  #       grid_card(
  #         area='geo-nat-l',
  #         p("Districts"),
  #         leafletOutput('geo.nat.l')
  #       )
  #       ,
  #       grid_card(
  #         area='bin-nat-t',
  #         p("Bin/index characteristics"),
  #         div(
  #           gt_output('bin.nat.t'),
  #           style = "font-size:65%")
  #       ),
  #       grid_card(
  #         area='perf-nat-t',
  #         p("Price change"),
  #         div(
  #           gt_output('perf.nat.t'),
  #           style = "font-size:65%")
  #       )
  #       
  #     )
  #   ),
  #   nav_panel(
  #     title = "Local",
  #     p("graphic"),
  #     card(p("graphic")),
  #     p("map"),
  #     card(p("map")),
  #     p("table")
  #   ),
  #   nav_panel(
  #     title = "Custom",
  #     sidebarLayout(
  #       sidebarPanel(
  #         downloadButton("downloadData", "Download"),
  #         h5(''),
  #         actionButton("go.custom.b", "Estimate"), #go 
  #         h5(''),
  #         
  #         treeInput( #districts
  #           inputId = "custom.tree",
  #           label = "Select districts:",
  #           choices = create_tree(rctree),
  #           #selected = "London-SW-",
  #           returnValue = "text",
  #           closeDepth = 0
  #         )
  #       ),
  #       
  #       mainPanel(
  #         p("map"),
  #         textOutput("selected_var"),
  #         plotOutput("rsi"),
  #         gt_output("geo")
  #       )
  #       
  #     )
  #   )
  # )
  # 
  
  
  
  navset_card_underline(
    nav_panel(
      title = "National",
      grid_page(
        layout = c(
          "space1  header     space3     space4",
          "spacel  estdt-nat-p    geo-nat-l  spacer",
          "spacel  estdt-nat-t tab4-nat-t  spacer"
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
          area = "header",
          content = html("National £/m<sup>2</sup>"),
          alignment = "start",
          is_title = FALSE
        ),
        grid_card(area='space1'),
        grid_card(area='space2'),
        grid_card(area='space3'),
        grid_card(area='space4'),
        grid_card(area='spacel'),
        grid_card(area='spacer'),
        
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
      p("graphic"),
      card(p("graphic")),
      p("map"),
      card(p("map")),
      p("table")
    ),
    nav_panel(
      title = "Custom",
      sidebarLayout(
        sidebarPanel(
          actionButton("go.custom.b", "Estimate"), #go
          h5(''),
          
          treeInput( #districts
            inputId = "ID1",
            label = "Select districts:",
            choices = create_tree(rctree),
            selected = "London-NW-",
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
  # ----------------------------------per-session
  # ---------------------data
  # vbl <- #saved in rdata with own name
  #   c(
  #     'z110',
  #     'z421',
  #     'z321',
  #     't4dump',
  #     'pxosrdo2dd'
  #   )
  # rmifgl(vbl)
  #for(i in seq_along(vbl)) {load(paste0(vbl[i],'.Rdata'))}
  # load('t4dump.Rdata')
  #load('C:/Users/Giles/anest.repo/anest.shiny/acycle/t4dump.Rdata')
  load('t4dump.Rdata',envir=globalenv())
  nat.t4 <- 
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
      #R2prj=round(rbar2prj,3),
      #beta1=round(b1,3),
      #tstata=round(aprj/ase,1)
    )]
  
  output$perf.nat.t <-  #gt estdt
    render_gt(
      g240823a(z321)%>%
        .[]%>%
        gt(. ,rownames_to_stub = T)
      
    )
  #national estdt
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
        )    #%>%
      # data_color(
      #   .,
      #   rows=match(
      #     input$pcd,
      #     z321$geo[which(z321$geo[rc9==regpcode(input$pcd),nx]==nx),rc9]
      #     ),
      #   palette=cobalt())
      
      #setnames(.,c('n','£/m2'))
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
          #axis.text=element_text(size=6,face = "plain"),
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
      input$go.custom.b, { #ok
        #print(z321$geo[rc9==regpcode(input$pcd),nx])
        #print(input$pcd%in%z321$geo[z321$geo[rc9==regpcode(input$pcd),nx]==nx,rc9])
        print('**********************************')
        print(input$ID1)
        input$ID1[which(nchar(input$ID1)==6)] 
      }
    )
  
  # Rselectedrow <- #rc
  #   eventReactive(input$go.custom.b, { #on button, return nx(rc),-> returns
  #     #print(Rselectedrc())
  #     x <- match(
  #       z321$geo[rc9==Rselectedrc(),nx]
  #     )
  #     print(x)
  #     x
  #   })
  
  Rrdt <- #returns
    eventReactive(input$go.custom.b, {
      Rselectedrow()
      coread(Rselectedrc(),'03rip/')[]
    })
  
  Rgeo <- #geo
    eventReactive(input$go.custom.b, {
      data.table(
        rc9=Rselectedrc(),
        nx=1,
        lab='lab001'
      )
    })
  
  Rrsi <- 
    eventReactive(input$go.custom.b, {
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
    })
  
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
