server_listing <- function(input, output, session, common) {

  listingData <- reactive({
    req(common$dataR())
    common$dataR()
  })

  output$listingTable <- renderTable({
    listingData()
  })
}