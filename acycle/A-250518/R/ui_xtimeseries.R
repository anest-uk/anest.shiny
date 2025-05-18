ui_navpanel_xtss <- function(id = NULL) {
  nav_panel(
    title = "x Time-series summary", #---------.----
    grid_container(
      layout = c(
        "x111     xtimeseries    "
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "x111",
        full_screen = TRUE,
        card_header(
          "Postcode area map"
        ),
        card_body(
        )
      ),
      grid_card(
        area = "xtimeseries",
        full_screen = TRUE,
        card_header(
          "Indices"
        ),
        card_body(
        )
      )
    )
  )
}
