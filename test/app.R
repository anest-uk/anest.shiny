library(shiny)
library(plotly)
library(gridlayout)
library(bslib)


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
      selectInput(
        inputId = "cut",
        label = "Cut of Diamond",
        choices = list(
          "Fair" = "Fair",
          "Good" = "Good",
          "Very_Good" = "Very Good",
          "Premium" = "Premium",
          "Ideal" = "Ideal"
        ),
        selected = "Ideal",
        width = "100%"
      ),
      em("Select the cut of the diamond you want to view the carat size distribution for.")
    )
  ),
  grid_card_text(
    area = "header",
    content = "Diamonds!",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "area2",
    card_body(
      grid_container(
        layout = c(
          "area0"
        ),
        row_sizes = c(
          "1fr"
        ),
        col_sizes = c(
          "0.27fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "area0",
          card_body(
            grid_container(
              layout = c(
                ". area0"
              ),
              row_sizes = c(
                "1.41fr"
              ),
              col_sizes = c(
                "1fr",
                "1fr"
              ),
              gap_size = "10px",
              grid_card(
                area = "area0",
                card_body(
                  grid_container(
                    layout = c(
                      "area0 .",
                      "area1 ."
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
                    grid_card(
                      area = "area0",
                      card_body(plotOutput(outputId = "plot"))
                    ),
                    grid_card_plot(area = "area1")
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
   
  
  
}

shinyApp(ui, server)
  

