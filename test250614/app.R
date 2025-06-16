library(shiny)
library(gt)
library(dplyr)
library(purrr)

x <- data.frame(
  light = c("#FF9FB5", "#F7B84F", "#84E26A", "#84E26A", "#66B2F7", "#BBA3F9"),
  code = c("1.3", "1.2", "1.1", "2.3", "2.2", "3.3"),
  row = 1:6
)

ui <- fluidPage(
  gt_output("table")
)

server <- function(input, output, session) {
  output$table <- render_gt({
    x %>%
      mutate(color_disk = paste0(
        "<div style='height:15px; width:15px; border-radius:50%; background:", light, "; margin:auto;'></div>"
      )) %>%
      gt() %>%
      text_transform(
        locations = cells_body(columns = color_disk),
        fn = function(x) map(x, gt::html)
      ) %>%
      cols_label(color_disk = "Color")
  })
}

shinyApp(ui, server)
