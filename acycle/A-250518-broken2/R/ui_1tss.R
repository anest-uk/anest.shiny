ui_navpanel_tss <- function(id = NULL) {
  nav_panel(
    title = "tss", #1tss
    card(
      full_screen = TRUE,
      card_header(
        "tss"
      ),
      card_body(
          leaflet::leafletOutput("tss12"), #--.----
          height = gridheight
      ),
      height = gridheight3
    )
  )
}
