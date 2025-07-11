ui_navpanel_con1 <- function(id = NULL) {
  nav_panel(
    title = "con1", #--------------------------------Constituents
    card(
      full_screen = TRUE,
      card_header(
        "Constituent districts"
      ),
      card_body( #-------------- constituent table 311
        DT::DTOutput("O311x"),
        height = gridheight3
      ),
      height = gridheight3
    )
  )
}
