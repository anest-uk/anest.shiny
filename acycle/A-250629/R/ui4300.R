ui4300 <- function(id = NULL) {
  nav_panel(
    title = "con1", #--------------------------------Constituents
    card(
      full_screen = TRUE,
      card_header(
        "Constituent districts"
      ),
      card_body( #-------------- constituent table 311
        DT::DTOutput("O4311x"),
        height = gridheight3
      ),
      height = gridheight3
    )
  )
}
