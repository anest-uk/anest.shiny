# source('CRANlibload.R')
# library(shiny)
# library(shinyWidgets)
# library(plotly)
# library(gridlayout)
# library(bslib)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(magrittr)
library(bslib) #bootstrap
library(gt)
source('rctree.R')

# rctree <- #selected
#   x%>%
#   #.[rc3%in%c('NW-','AL-','M--'),]%>%
#   .[rc3%in%unique(substr(dir('03rip/'),1,3)),]%>%
#   .[.N:1,c('region','lab','rc6')]


ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar plot  "
  ),
  row_sizes = c(
    "55px",
    "1fr"
  ),
  col_sizes = c(
    "285px",
    "1fr"
  ),
  gap_size = "1.6000000000000005rem",
  grid_card(
    area = "sidebar",
    card_header("Settings"),
    card_body(
      downloadButton("downloadData", "Download"),
      h5(''),
      actionButton(inputId = "go", label = "Estimate"),
      h5(''),
      treeInput( #districts
                                    inputId = "ID1",
                                    label = "Select districts:",
                                    choices = create_tree(rctree),
                                    selected = "London-NW-",
                                    returnValue = "text",
                                    closeDepth = 0
                                  ),
    )
  ),
  grid_card_text(
    area = "header",
    content = "Index estimation",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "plot",
    #card_header(h1(strong("Index"))),
    card_body(
      textOutput(outputId = "selected_var"),
      plotOutput(outputId = "rsi"),
      gt_output("geo")
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
      # x <- data.table(ii=1:10,x=1:10)
      ggplot(
        x,
        aes(ii,x)
      )+
        geom_line()
      #print(x)
      #x
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
  

