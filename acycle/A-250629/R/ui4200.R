#-------------------------------------------------4100 timeseries
ui4200 <- function(id = NULL) {
  nav_panel(
    title = "4200", #---------.----
    grid_container(
      # layout = c(
      #   "A4212    A4222 ",
      #   "A4221    A4222x ",
      #   "A4211    A4231 "
      # ),
      layout = c(
        "A4211    A4212 ",
        "A4221    A4222 ",
        "A4231    . "
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
        area = "A4211",
        full_screen = TRUE,
        card_header(
          "A4211"
        ),
        card_body(
          gt::gt_output("OO4211"),
          height = gridheight
        )
      ) # gridcard
      ,      
      grid_card(
        area = "A4212",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          leafletOutput("OO4212"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4222",
        full_screen = TRUE,
        # card_header(
        #   "."
        # ),
        card_body(
          plotOutput("OO4222"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4221",
        full_screen = TRUE,
        card_body(
          id = "table_tabs",
            bslib::navset_tab(#navset_tab within grid_card
              nav_panel(
                title="custom",
                gt::gt_output("OO4221a")
              ),
              nav_panel(
                title="local",
                gt::gt_output("OO4221b")
              )
            )#navs_tab
          #)#nav_panel
        ),#card_body
        height = gridheight
      )#gridcard
      ,
      # grid_card(
      #   area = "A4222x",
      #   full_screen = TRUE,
      #   # card_header(
      #   #   "."
      #   # ),
      #   card_body(
      #     gt::gt_output("OO4222x"),
      #     height = gridheight
      #   )
      # ) # gridcard
      # ,

      grid_card(
        area = "A4231",
        full_screen = TRUE,
        card_header(
          "A4231"
        ),
        card_body(
          gt::gt_output("OO4231a"),
          gt::gt_output("OO4231b"),
          gt::gt_output("OO4231c"),
          gt::gt_output("OO4231d"),
          height = gridheight2
        )
      ) # gridcard
    ) # gridcontainer
  ) # navpanel
}
