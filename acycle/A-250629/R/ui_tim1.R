ui_navpanel_tim1 <- function(id = NULL) {
  nav_panel(
    title = "tim1", #---------.----
    grid_container(
      layout = c(
        "O111x    O112x ",
        "O121x    O122x ",
        "O131x    O132x "
      ),
      row_sizes = c(
        "1fr",
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "O111x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          leafletOutput("O111x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "O112x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          plotOutput("O112x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "O121x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          gt::gt_output("O121xa"),
          gt::gt_output("O121xb"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "O122x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          gt::gt_output("O122x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "O131x",
        full_screen = TRUE,
        card_header(
          "O131x"
        ),
        card_body(
          gt::gt_output("O131x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "O132x",
        full_screen = TRUE,
        card_header(
          "O132x"
        ),
        card_body()
      ) # gridcard
    ) # gridcontainer
  ) # navpanel
}
