server_constituents <- function(input, output, session, common) {

  constituentsData <- reactive({
    req(common$dataR())
    common$dataR()
  })

  output$constituentsTable <- renderTable({
    constituentsData()
  })
}