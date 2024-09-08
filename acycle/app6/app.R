library(shiny)
library(plotly)
library(gridlayout)
library(bslib)


ui <- grid_page(
  layout = c(
    ".     .    ",
    "area1 area0"
  ),
  row_sizes = c(
    "85px",
    "1fr"
  ),
  col_sizes = c(
    "135px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "area0",
    card_body(
      tabsetPanel(
        nav_panel(title = "Empty Tab"),
        nav_panel(title = "Empty Tab"),
        nav_panel(title = "Empty Tab")
      )
    )
  ),
  grid_card(
    area = "area1",
    card_body(
      textInput(
        inputId = "myTextInput",
        label = "Text Input",
        value = ""
      )
    )
  )
)


server <- function(input, output) {
   
  output$plot <- renderPlotly({
    plot_ly(
      diamonds[diamonds$cut == input$cut,], 
      x = ~carat
    ) |> 
    add_histogram() 
  })
  
}

shinyApp(ui, server)
  

