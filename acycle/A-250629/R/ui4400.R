#-------------------------------------------------4200 listing
ui4400 <-
  function(
      id = NULL) {
    nav_panel(
      title = "4400",
      grid_container(
        layout = c(
          "AA4411 ."
        ),
        row_sizes = c(
          "1fr"
        ),
        col_sizes = c(
          "1fr",
          "1fr"
        ),
        grid_card(
          area = "AA4411",
          full_screen = TRUE,
          gt::gt_output("OO4411a"),
          gt::gt_output("OO4411b") #summary
        ) # grid_card
      ) # grid_container
    ) # nav_panel
  }
