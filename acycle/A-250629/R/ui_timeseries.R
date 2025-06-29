ui_navpanel_tss <- function(id = NULL) {
  nav_panel(
    title = "Time-series summary", #---------.----
    grid_container(
      layout = c(
        "O111     O112    ",
        "O121     O122",
        "O131     O132   "
      ),
      row_sizes = c(
        "1fr",
        ".7fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "O111",
        full_screen = TRUE,
        card_header(
          "Postcode area map"
        ),
        card_body(
          leaflet::leafletOutput("O111"), #--.----
          height = gridheight
        )
      ),
      grid_card(
        area = "O112",
        full_screen = TRUE,
        card_header(
          "Indices"
        ),
        card_body(
          plotOutput("O112"), #--------------.----
          height = gridheight
        )
      ),
      grid_card(
        area = "O121",
        full_screen = TRUE,
        card_header(
          "Price return summary"
        ),
        card_body( #-----------------------121----
          gt::gt_output("O121a"),
          gt::gt_output("O121b"),
          height = gridheight
        )
      ),
      grid_card(
        area = "O122",
        full_screen = TRUE,
        card_header(
          "Index characteristics"
        ),
        card_body( # characteristics table x122----
          gt::gt_output("O122")
        )
      ),
      grid_card(
        area = "O131",
        full_screen = TRUE,
        card_header(
          "Summary"
        ),
        card_body( #-return summary table x131----
          gt::gt_output("O131"),
          height = gridheight
        )
      ),
      grid_card(
        area = "O132",
        full_screen = TRUE,
        card_header(
          "Trade recap"
        ),
        card_body( #----trade recap table x132----
          gt::gt_output("O132a"),
          gt::gt_output("O132b"),
          gt::gt_output("O132c"),
          gt::gt_output("O132d"),
          height = gridheight2
        )
      )
    )
  )
}
