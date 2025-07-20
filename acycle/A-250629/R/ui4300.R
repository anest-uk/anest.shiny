#-------------------------------------------------4200 listing
ui4300 <- 
  function(
    id = NULL) {
  nav_panel(
    title = "4300",
    grid_container(
      layout = c(
        "a4211  a4212  a4213"
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
        area = "a4211",
        full_screen = TRUE,
        card_header(
          "Time bins"
        ),
        card_body( #-------------- timebin table
          gt::gt_output("O4211")
        )
      ),
      grid_card(
        area = "a4212",
        full_screen = TRUE,
        card_header(
          "Local index"
        ),
        card_body( #-------------- local index table
          gt::gt_output("O4212")
        )
      ),
      grid_card(
        area = "a4213",
        full_screen = TRUE,
        card_header(
          "Custom index"
        ),
        card_body( #-------------- custom index table
          gt::gt_output("O4213")
        )
      )
    )
  )
}
