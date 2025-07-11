ui_navpanel_lis1 <- function(
    id = NULL) {
  nav_panel(
    title = "lis1",
    grid_container(
      layout = c(
        "timebins  locallist1  customlist1"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        ".3fr",
        "1fr",
        "1fr"
      ),
      grid_card(
        area = "timebins",
        full_screen = TRUE,
        card_header(
          "Time bins"
        ),
        card_body( #-------------- timebin table
          gt::gt_output("O211a")
        )
      ),
      grid_card(
        area = "locallist1",
        full_screen = TRUE,
        card_header(
          "Local index"
        ),
        card_body( #-------------- local index table
          gt::gt_output("O211b")
        )
      ),
      grid_card(
        area = "customlist1",
        full_screen = TRUE,
        card_header(
          "Custom index"
        ),
        card_body( #-------------- custom index table
          gt::gt_output("O211c")
        )
      )
    )
  )
}
