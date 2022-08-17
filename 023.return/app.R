library(shiny)
library(data.table)
library(DT)


# Define UI for dataset viewer app ----
ui <- fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  # App title ----#"width:104px; height:77px"
  titlePanel(
    title=htmltools::div(
      htmltools::p(),
      htmltools::img(src="Transparent Logo No Slogan.png",style="width:72px; height:52px"),
      htmltools::p(),
      htmltools::p(),
      "repeat sales by identifier"
    ),
    windowTitle = "repeat.sale"
  ),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
       # Input: Selector for id ----
      textInput(inputId = "id.public",
                  label = "id.public",
                  value = "0050482308a45e86" #fread('rid/00.csv',nr=1)[1,substr(idtr,1,nchar(idtr)-11)]
                ),
      h6("
         Note:
         "),
      h6("
         This browser is an audit tool to look up repeat returns from idhash, a unique property identifier.
         "),
      h6("
         This browser can be used in conjunction with the id-lookup-from-address app here: https://anest-uk.shinyapps.io/id-lookup/
         "),
      h6("
         Presence of a return does not mean the transactions are present in the analysis dataset after screening.
         "),
      h6("
         Log return is seasonally adjusted.
         "),
      h6("
         Source: Land Registry Price Paid Data
         "),
      h6("
         Driven by R and shinyapps.io
         "),
      h6("
         Beta test
         ")
    ),

    # Main panel for displaying outputs ----
    # mainPanel(
    #
    #   # Output: HTML table with requested number of observations ----
    #   tableOutput("view")
    #
    # )
    mainPanel(

      div(dataTableOutput("view"), style = "font-family:consolas")

    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  # Return the requested dataset ----
  datasetInput <- reactive({
    x1 <- input$id.public
    x2 <- paste0('rid/',substr(x1,1,2),'.csv') #fpath
    print(x1)
    print(x2)
    if(file.exists(x2)) {
      x3 <- fread(x2)
      x4 <- x3[grep(paste0('^',input$id.public),idtr)]
      x5 <- head(x4[,.(idtr,rc6,days,deseas)][order(idtr)],50)
    } else {
      x5 <- NULL
    }
    if(is.null(x5)|nrow(x5)<1) {
      x6 <- NULL
    } else {
      x6 <- x5[,.(
        n=1:.N,
        idhash.saledate=idtr,
        postcode.district=rc6,
        days.held=days,
        log.return=deseas
      )]
    }
    x6
  })

  output$view <- renderDT({datasetInput()},options = list(dom = 'ft'))
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

#library(rsconnect)
#deployApp('08/05/023',appName='anestrs1')
