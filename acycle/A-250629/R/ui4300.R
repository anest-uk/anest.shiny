#-------------------------------------------------4200 listing
ui4300 <-
  function(
      id = NULL) {
    nav_panel(
      title = "4300",
      grid_container(
        layout = c(
          "AA4311 ."
        ),
        row_sizes = c(
          "1fr"
        ),
        col_sizes = c(
          # ".3fr",
          "1fr" ,
          "1fr"
        ),
        grid_card(
          area = "AA4311",
          full_screen = TRUE,
          # card_header(
          #   "Local index"
          # ),
          # card_body( #-------------- local index table
            bslib::navset_tab( # navset_tab within grid_card
              nav_panel(
                title = "local",
                gt::gt_output("OO4311")
              ), # nav_panel
              nav_panel(
                title = "custom",
                gt::gt_output("OO4312")
              ) # nav_panel
            ) # navset_tab
          # ) # card_body
        ) # grid_card
      )
    )
  }
