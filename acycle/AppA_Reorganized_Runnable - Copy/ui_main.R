ui_main <- function() {
  grid_page(
    layout = c(
      "action  header",
      "sidebar  area2 "
    ),
    row_sizes = c("200px", "1fr"),
    col_sizes = c("250px", "1fr"),
    gap_size = ".1rem",
    ui_card_action(),
    ui_card_sidebar(),
    ui_card_header(),
    ui_area2()
  )
}