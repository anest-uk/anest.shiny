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
  ),
  grid_card_text(
    area = "header",
    content = "Diamonds!",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "plot",
    card_header("Interactive Plot"),
    card_body(
      plotlyOutput(
        outputId = "plot",
        width = "100%",
        height = "100%"
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
  

