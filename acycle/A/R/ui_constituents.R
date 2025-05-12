

ui_navpanel_constituents <- function(id=NULL) {
  nav_panel(title = "Constituents",#--------------------------------Constituents
            card(
              full_screen = TRUE,
              card_header(
                "Constituent districts"
              ),
              card_body(#-------------- constituent table 311
                DT::DTOutput('x311'),
                height=gridheight3
              ),
              height=gridheight3
            )
  )}
