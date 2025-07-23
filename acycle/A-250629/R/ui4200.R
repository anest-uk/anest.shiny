#-------------------------------------------------4100 timeseries
ui4200 <- function(id = NULL) {
  nav_panel(
    title = "4200", #---------.----
    grid_container(
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
        card_body(
          leafletOutput("OO4212"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "A4222",
        full_screen = TRUE,
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
          id = "A4221tabs",
          bslib::navset_tab( # navset_tab within grid_card
            nav_panel(
              title = "custom",
              gt::gt_output("OO4221a")
            ),
            nav_panel(
              title = "local",
              gt::gt_output("OO4221b")
            )
          ) # navs_tab
        ), # card_body
        height = gridheight
      ) # gridcard
      ,
      grid_card(
        area = "A4231",
        full_screen = TRUE,
        card_header(
          "A4231"
        ),
        
        
        card_body(
          id = "A4231tabs",
          bslib::navset_tab( # custom/local
            nav_panel(#outer nav_panel 1
              title = "custom",
              bslib::navset_pill_list( # navset_tab
                widths = c(2,12),
                #placement='start',
                nav_panel(
                  title = "return",
                  gt::gt_output("OO4231c")
                ),
                nav_panel(
                  title = "count",
                  gt::gt_output("OO4231d")
                )
              ) # inner navset_tab 1
            ), # outer nav_panel 1
            nav_panel(#outer nav_panel 2
              title = "local",
              bslib::navset_pill_list( # navset_tab
                widths = c(2,12),
                nav_panel(
                  title = "return",
                  gt::gt_output("OO4231a")
                ),
                nav_panel(
                  title = "count",
                  gt::gt_output("OO4231b")
                )
              ) # inner navset_tab 2
            ) # outer nav_panel 2
          ) # outer navset_tab
        ) # , # card_body
        
        
        
        # card_body(
        #   gt::gt_output("OO4231a"),
        #   gt::gt_output("OO4231b"),
        #   gt::gt_output("OO4231c"),
        #   gt::gt_output("OO4231d"),
        #   height = gridheight2
        # ) #card_body
      ) # gridcard
    ) # gridcontainer
  ) # navpanel
}



#       ,
#       grid_card(
#         area = "A4231",
#         full_screen = TRUE,
#         card_header(
#           "A4231"
#         ),
#         card_body(
#           gt::gt_output("OO4231a"),
#           gt::gt_output("OO4231b"),
#           gt::gt_output("OO4231c"),
#           gt::gt_output("OO4231d"),
#           height = gridheight2
#         )
#       ) # gridcard
#     ) # gridcontainer
#   ) # navpanel
# }
