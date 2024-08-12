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

rctree <- x[1:100,]

ui <- page_navbar(
  title = "Index",
  selected = "Custom",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  sidebar = sidebar(
    title = "Sidebar Title",
    textInput(
      inputId = "target_rc6",
      label = "Target",
      value = ""
    )
  ),
  # nav_panel(#------------------navpanel
  #   title = "Custom",
  #   grid_container(
  #     row_sizes = c(
  #       "1fr"
  #     ),
  #     col_sizes = c(
  #       "250px",
  #       "1fr"
  #     ),
  #     gap_size = "10px",
  #     layout = c(
  #       "peerselect rsi"
  #     ),
  #     grid_card(
  #       area = "peerselect",
  #       card_header("Settings"),
  #       card_body(
  #         actionButton(inputId = "go", label = "Estimate"),
  #         treeInput( #districts
  #           inputId = "ID1",
  #           label = "Select districts:",
  #           choices = create_tree(rctree),
  #           selected = "Wolverhampton-WV-",
  #           returnValue = "text",
  #           closeDepth = 0
  #         ),
  #         downloadButton("downloadData", "Download")
  #       )
  #     ),
  #     grid_card_plot(
  #       area = "rsi"
  #       )
  #   )
  # ),#------------------navpanel
  
  nav_panel(
    title = "National",
    grid_container(
      layout = c(
        #"area0",
        "area1"#,
        # "area2",
        # "area3"
      ),
      row_sizes = c(
        # "1fr",
        # "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr"
      ),
      gap_size = "10px",
      # grid_card(
      #   area = "area0",
      #   card_body(plotOutput(outputId = "plot")),
        
        
      grid_card(
        area = "area1",
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "250px",
        "1fr"
      ),
      gap_size = "10px",
      layout = c(
        "area1a rsi"
      ),
      grid_card(
        area = "area1a",
        card_header("Settings"),
        card_body(
          actionButton(inputId = "go", label = "Estimate"),
          treeInput( #districts
            inputId = "ID1",
            label = "Select districts:",
            choices = create_tree(rctree),
            selected = "Wolverhampton-WV-",
            returnValue = "text",
            closeDepth = 0
          ),
          downloadButton("downloadData", "Download")
        )
      ),
      grid_card_plot(
        area = "rsi"
        )
    )
        #,
        
      #),
      # grid_card(
      #   area = "area2",
      #   full_screen = TRUE,
      #   card_header(
      #     "Leaflet
      #     "
      #   )
      # ),
      # grid_card(
      #   area = "area3",
      #   full_screen = TRUE,
      #   card_header("Table")
      # )
    )
  ),
  nav_panel(
    title = "Local",
    grid_container(
      layout = c(
        "area0",
        "area1",
        "area2"
      ),
      row_sizes = c(
        "1.01fr",
        "1fr",
        "0.99fr"
      ),
      col_sizes = c(
        "1fr"
      ),
      gap_size = "10px",
      grid_card_plot(area = "area0"),
      grid_card(
        area = "area1",
        full_screen = TRUE,
        card_header(
          "Leaflet
          "
        )
      ),
      grid_card(
        area = "area2",
        full_screen = TRUE,
        card_header(
          "Table
          "
        )
      )
    )
  ),
  nav_panel(title = "Regional")
)

server <- function(input, output) {
  #--------------------------------------
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


