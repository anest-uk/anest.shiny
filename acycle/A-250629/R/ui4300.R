#-------------------------------------------------4200 listing
ui4300 <-
  function(
      id = NULL) {
    nav_panel(
      title = "4300",
      grid_container(
        layout = c(
          "AA4311  AA4312"
        ),
        row_sizes = c(
          "1fr"
        ),
        col_sizes = c(
          #".3fr",
          "1fr",
          "1fr"
        ),
        # grid_card(
        #   area = "AA4311",
        #   full_screen = TRUE,
        #   card_header(
        #     "Time bins"
        #   ),
        #   card_body( #-------------- timebin table
        #     gt::gt_output("OO4311")
        #   )
        # ),
        grid_card(
          area = "AA4311",
          full_screen = TRUE,
          card_header(
            "Local index"
          ),
          card_body( #-------------- local index table
            gt::gt_output("OO4311")
          )
        ),
        grid_card(
          area = "AA4312",
          full_screen = TRUE,
          card_header(
            "Custom index"
          ),
          card_body( #-------------- custom index table
            gt::gt_output("OO4312")
          )
        )
      )
    )
  }
