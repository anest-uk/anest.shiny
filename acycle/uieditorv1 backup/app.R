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
      em(""),
      textInput(
        inputId = "Target_district",
        label = "Target district",
        value = "SW3"
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "RSI",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "plot",
    card_header("Interactive Plot"),
    card_body(
      tabsetPanel(
        nav_panel(
          title = "National",
          grid_container(
            layout = c(
              "xchartnat leafletnat",
              "permatnat binchanat "
            ),
            row_sizes = c(
              "1fr",
              "1fr"
            ),
            col_sizes = c(
              "1.01fr",
              "0.99fr"
            ),
            gap_size = "10px",
            grid_card_plot(area = "xchartnat"),
            grid_card_plot(area = "leafletnat"),
            grid_card(
              area = "permatnat",
              full_screen = TRUE,
              card_header(
                "Delta log price table
                                                "
              )
            ),
            grid_card(
              area = "binchanat",
              full_screen = TRUE,
              card_header(
                "P-bins
                                                "
              )
            )
          )
        ),
        nav_panel(
          title = "Local",
          grid_container(
            layout = c(
              "xchartloc leafletloc",
              "permatloc binchaloc "
            ),
            row_sizes = c(
              "1fr",
              "1fr"
            ),
            col_sizes = c(
              "1fr",
              "1fr"
            ),
            gap_size = "10px",
            grid_card_plot(area = "xchartloc"),
            grid_card_plot(area = "leafletloc"),
            grid_card(
              area = "permatloc",
              full_screen = TRUE,
              card_header("Delta log price table")
            ),
            grid_card(
              area = "binchaloc",
              full_screen = TRUE,
              card_header("P-bins")
            )
          )
        ),
        nav_panel(
          title = "Custom",
          grid_container(
            layout = c(
              "custom_control xchartcus leafletcus",
              ".              permatcus binchacus "
            ),
            row_sizes = c(
              "1fr",
              "1fr"
            ),
            col_sizes = c(
              "0.36fr",
              "1.37fr",
              "1.27fr"
            ),
            gap_size = "10px",
            grid_card(
              area = "custom_control",
              card_body(
                checkboxGroupInput(
                  inputId = "bultform",
                  label = "Built form",
                  choices = list("flat" = "a", "house" = "b")
                ),
                checkboxGroupInput(
                  inputId = "Used",
                  label = "Used",
                  choices = list("Used" = "a", "New" = "b")
                ),
                radioButtons(
                  inputId = "timebinning",
                  label = "Time bins",
                  choices = list("annual" = "a", "semi-annual" = "b", "DRC" = "value3"),
                  width = "100%"
                ),
                checkboxInput(
                  inputId = "treeselect",
                  label = "Postcodes",
                  value = FALSE
                )
              )
            ),
            grid_card_plot(area = "xchartcus"),
            grid_card_plot(area = "leafletcus"),
            grid_card(
              area = "permatcus",
              full_screen = TRUE,
              card_header("Delta log price table")
            ),
            grid_card(
              area = "binchacus",
              full_screen = TRUE,
              card_header(
                "P-bins
                "
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
  

