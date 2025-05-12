server_common <- function(input, output, session) {

  # Example shared reactive
  dataR <- reactive({
    # replace with actual data
    NULL
  })

  list(
    dataR = dataR
  )
}