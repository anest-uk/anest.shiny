ui_navpanel_lis1 <- function(
    id = NULL) {
  nav_panel(
    title = "lis1",
    grid_container(
      layout = c(
        "locallist1  customlist1"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      grid_card(
        area = "locallist1",
        full_screen = TRUE,
        card_header(
          "Local"
        ),
        card_body( #-------------- listing table 211
          gt::gt_output("O211x")
        )
      ),
      grid_card(
        area = "customlist1",
        full_screen = TRUE,
        card_header(
          "Custom"
        ),
        card_body( #-------------- listing table 211cu
          gt::gt_output("O212x")
        )
      )
    )
  )
}
