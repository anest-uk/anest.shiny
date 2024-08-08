library(shiny)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(magrittr)
library(bslib) #bootstrap
library(gt)

source('rctree.R') #postcode tree 'x'
#-----

#source('153 DINO SLOW PREAMBLE.R')
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

rctree <- #selected
  data.table(x)%>%
  .[rc3%in%c('NW-','AL-','M--'),]%>%
  .[.N:1,c('lab','rc6')]

ui <- page_sidebar(
  tags$head(tags$style(".sidebar-menu li { margin-bottom: 100px; }")),
  title = "Custom index 240806",
  textOutput("selected_var"),
  plotOutput("rsi"),
  gt_output("geo"),
  
  sidebar = sidebar( #sidebar elements
    
    downloadButton("downloadData", "Download"),
    h5(''),
    actionButton("go", "Estimate"), #go 
    h5(''),
    
    treeInput( #districts
      inputId = "ID1",
      label = "Select districts:",
      choices = create_tree(rctree),
      selected = "London-NW-",
      returnValue = "text",
      closeDepth = 0
    )
  )
  
)


server <- function(input, output, session) {
  source('c-cleanlib.R')
  source('rctree.R') #postcode tree 'x'
  source('applib.R') #extras for rsi
  
  output$files <- renderTable(input$upload)
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rsi.g, file)
    }
  )
  Rselectedrc <- #rc
    eventReactive(input$go, {
      input$ID1[nchar(input$ID1)==6]
    })
  Rrdt <- #returns
    eventReactive(input$go, {
      coread(Rselectedrc(),'03rip/')[]
    })
  Rgeo <- #solve rsi
    eventReactive(input$go, {
      data.table(
        rc9=Rselectedrc(),
        nx=1,
        lab='lab001'
      )
    })
  Rrsi <- 
    eventReactive(input$go, {
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

shinyApp(ui, server)
