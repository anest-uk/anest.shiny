ui_main <- function() {
  grid_page(
    layout = c(
      "header",
      "sidebar area2"
    ),
    row_sizes = c("100px", "1fr"),
    col_sizes = c("250px", "1fr"),
    gap_size = "10px",

    grid_card(area = "header", "Header area"),
    grid_card(area = "sidebar", "Sidebar area"),

    grid_card(area = "area2", 
      page_navbar(
        fillable = FALSE,
        position = "fixed-top",
        ui_listing(),
        ui_constituents(),
        ui_newpage()
      )
    )
  )
}