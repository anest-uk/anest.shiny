library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
# library(shinyuieditor)
# setwd('c:/users/giles/anest.repo/anest.shiny/acycle/app10')
# launch_editor('.')

ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar area2 "
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
      textInput(
        inputId = "myTextInput",
        label = "District",
        value = ""
      ),
      radioButtons(
        inputId = "timebin",
        label = "Time sampling",
        choices = list("High" = "H", "Low" = "L", "Annual" = "A"),
        width = "100%"
      ),
      radioButtons(
        inputId = "trim",
        label = "Outlier rejection",
        choices = list("None" = "1", "Low" = "2", "High" = "3"),
        width = "100%"
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Local index",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "area2",
    card_body(
      tabsetPanel(
        nav_panel(
          title = "Summary",
          grid_container(
            layout = c(
              "map     xtimeseries    ",
              "Winding characteristics",
              "Summary Tradesummary   "
            ),
            row_sizes = c(
              "1fr",
              "1fr",
              "1fr"
            ),
            col_sizes = c(
              "1fr",
              "1fr"
            ),
            gap_size = "10px",
            grid_card(
              area = "map",
              full_screen = TRUE,
              card_header(
                "Area map
                                "
              )
            ),
            grid_card(
              area = "xtimeseries",
              full_screen = TRUE,
              card_header(
                "Indices
                                "
              )
            ),
            grid_card(
              area = "Winding",
              full_screen = TRUE,
              card_header(
                "Price return summary
                                "
              )
            ),
            grid_card(
              area = "characteristics",
              full_screen = TRUE,
              card_header(
                "Index characteristics
                                "
              )
            ),
            grid_card(
              area = "Summary",
              full_screen = TRUE,
              card_header(
                "Summary
                                "
              )
            ),
            grid_card(
              area = "Tradesummary",
              full_screen = TRUE,
              card_header("Trade summary")
            )
          )
        ),
        nav_panel(
          title = "Listing",
          card(
            full_screen = TRUE,
            card_header(
              "Index time series
              "
            )
          )
        ),
        nav_panel(
          title = "Constituents",
          card(
            full_screen = TRUE,
            card_header(
              "Constituent districts
              
              
              "
            )
          )
        ),
        nav_panel(
          title = "Accuracy",
          card(
            full_screen = TRUE,
            card_header("Header"),
            card_body(
              grid_container(
                layout = c(
                  "timesampling      ",
                  "outlierrejection  ",
                  "crossvalidation   ",
                  "geographicgrouping"
                ),
                row_sizes = c(
                  "1fr",
                  "1fr",
                  "1fr",
                  "1fr"
                ),
                col_sizes = c(
                  "1fr"
                ),
                gap_size = "10px",
                grid_card(
                  area = "timesampling",
                  full_screen = TRUE,
                  card_header(
                    "Tme Sampling
                    "
                  )
                ),
                grid_card(
                  area = "outlierrejection",
                  full_screen = TRUE,
                  card_header(
                    "Outlier rejection
                    "
                  )
                ),
                grid_card(
                  area = "crossvalidation",
                  full_screen = TRUE,
                  card_header(
                    "Cross validation
                    "
                  )
                ),
                grid_card(
                  area = "geographicgrouping",
                  full_screen = TRUE,
                  card_header(
                    "Geographic grouping
                    "
                  )
                )
              )
            )
          )
        )
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
  

