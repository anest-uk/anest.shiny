server <- function(input, output) {
  dataInput <- reactive({
    getSymbols(input$symb)
  })
  dataProcess <- reactive({
    process(dataInput())
  })
}