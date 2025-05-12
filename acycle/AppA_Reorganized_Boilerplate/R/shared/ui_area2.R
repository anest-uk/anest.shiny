ui_area2 <- function(id=NULL) {
  grid_card(
    area = "area2",
    card_body(
      page_navbar(
        fillable=FALSE,
        position='fixed-top',
        ui_navpanel_tss(),
        ui_navpanel_listing(),
        ui_navpanel_constituents(),
        ui_navpanel_accuracy(),
        ui_navpanel_notes()
      )
    )
  )
}