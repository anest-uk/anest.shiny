ui_navpanel_tim1 <- function(id = NULL) {
  nav_panel(
    title = "tim1", #---------.----
    grid_container(
      layout = c(
        "tim111    tim112 ",
        "tim121    tim122 ",
        "tim131    tim132 "
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
        area = "tim111",
        full_screen = TRUE,
        card_header(
          "tim111"
        ),
        card_body(
          leafletOutput("tim111"),
          height = gridheight
        )
      ) # gridcard
      ,
      grid_card(
        area = "tim112",
        full_screen = TRUE,
        card_header(
          "tim112"
        ),
        card_body(
          plotOutput("tim112")
        )
      ) # gridcard
      ,
      grid_card(
        area = "tim121",
        full_screen = TRUE,
        card_header(
          "tim121"
        ),
        card_body()
      ) # gridcard
      ,
      grid_card(
        area = "tim122",
        full_screen = TRUE,
        card_header(
          "tim122"
        ),
        card_body()
      ) # gridcard
      ,
      grid_card(
        area = "tim131",
        full_screen = TRUE,
        card_header(
          "tim131"
        ),
        card_body()
      ) # gridcard
      ,
      grid_card(
        area = "tim132",
        full_screen = TRUE,
        card_header(
          "tim132"
        ),
        card_body()
      ) # gridcard
    ) # gridcontainer
  ) # navpanel
}
