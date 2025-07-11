#-------------------------------------------------4200 listing
ui4200 <- 
  function(
    id = NULL) {
  nav_panel(
    title = "gp4200",
    grid_container(
      layout = c(
        "a4211a  a4211b  a4211c"
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
        area = "a4211a",
        full_screen = TRUE,
        card_header(
          "Time bins"
        ),
        card_body( #-------------- timebin table
          gt::gt_output("O4211a")
        )
      ),
      grid_card(
        area = "a4211b",
        full_screen = TRUE,
        card_header(
          "Local index"
        ),
        card_body( #-------------- local index table
          gt::gt_output("O4211b")
        )
      ),
      grid_card(
        area = "a4211c",
        full_screen = TRUE,
        card_header(
          "Custom index"
        ),
        card_body( #-------------- custom index table
          gt::gt_output("O4211c")
        )
      )
    )
  )
}
