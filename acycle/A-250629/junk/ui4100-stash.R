#-------------------------------------------------4100 timeseries
ui4100 <- function(id = NULL) {
  nav_panel(
    title = "gp4100", #---------.----
    grid_container(
      layout = c(
        "A4111x    A4112x ",
        "A4121x    A4122x ",
        "A4131x    A4132x "
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
        area = "A4111x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          leafletOutput("O4111x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4112x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          plotOutput("O4112x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4121x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          gt::gt_output("O4121xa"),
          gt::gt_output("O4121xb"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4122x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          gt::gt_output("O4122x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4131x",
        full_screen = TRUE,
        card_header(
          "A4131x"
        ),
        card_body(
          gt::gt_output("O4131x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4132x",
        full_screen = TRUE,
        card_header(
          "A4132x"
        ),
        card_body(
          gt::gt_output("O4132xa"),
          gt::gt_output("O4132xb"),
          gt::gt_output("O4132xc"),
          gt::gt_output("O4132xd"),
          height = gridheight2
        )
      ) # gridcard
    ) # gridcontainer
  ) # navpanel
}
