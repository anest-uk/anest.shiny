#-------------------------------------------------4100 timeseries
ui4100 <- function(id = NULL) {
  nav_panel(
    title = "gp4100", #---------.----
    grid_container(
      layout = c(
        "A4211x    A4212x ",
        "A4221x    A4222x ",
        "A4231x    A4232x "
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
        area = "A4211x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          leafletOutput("O4211x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4212x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          plotOutput("O4212x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4221x",
        full_screen = TRUE,
        card_body(
          id = "table_tabs",
            bslib::navset_tab(#navset_tab within grid_card
              nav_panel(
                title="custom",
                gt::gt_output("O4221xa")
              ),
              nav_panel(
                title="local",
                gt::gt_output("O4221xb")
              )
            )#navs_tab
          #)#nav_panel
        ),#card_body
        height = gridheight
      )#gridcard
      ,
      grid_card(
        area = "A4222x",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          gt::gt_output("O4222x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4231x",
        full_screen = TRUE,
        card_header(
          "A4231x"
        ),
        card_body(
          gt::gt_output("O4231x"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4232x",
        full_screen = TRUE,
        card_header(
          "A4232x"
        ),
        card_body(
          gt::gt_output("O4232xa"),
          gt::gt_output("O4232xb"),
          gt::gt_output("O4232xc"),
          gt::gt_output("O4232xd"),
          height = gridheight2
        )
      ) # gridcard
    ) # gridcontainer
  ) # navpanel
}
