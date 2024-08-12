library(gridlayout)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(magrittr)
library(bslib)
library(gt)

  source('c-cleanlib.R')
  source('rctree.R') #postcode tree 'x'
  source('applib.R') #extras for rsi

source('rctree.R') #postcode tree 'x'


ui <- page_navbar(
  title = "Chick Weights",
  selected = "Line Plots",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  nav_panel(
    title = "Line Plots",
    grid_container(
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "250px",
        "1fr"
      ),
      gap_size = "10px",
      layout = c(
        "num_chicks linePlots"
      ),
      grid_card(
        area = "num_chicks",
        card_header("Settings"),
        card_body(
        )
      ),
      grid_card_plot(area = "linePlots")
    )
  ),
  nav_panel(
    title = "Distributions",
    grid_container(
      row_sizes = c(
        "165px",
        "1fr"
      ),
      col_sizes = c(
        "1fr"
      ),
      gap_size = "10px",
      layout = c(
        "facetOption",
        "dists"
      ),
      grid_card_plot(area = "dists"),
      grid_card(
        area = "facetOption",
        card_header("Distribution Plot Options"),
        card_body(
                    title = "Custom index 240806",
          textOutput(outputId = "selected_var"),
          plotOutput(outputId = "rsi"),
          gt_output("geo"),

          radioButtons(
            inputId = "distFacet",
            label = "Facet distribution by",
            choices = list("Diet Option" = "Diet", "Measure Time" = "Time")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
   
  
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
  

