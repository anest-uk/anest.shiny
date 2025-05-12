

ui_navpanel_notes <- function(id=NULL) {
  nav_panel(title = "Notes",#----------------------------------------------Notes
            
            card(
              full_screen = TRUE,
              card_body(
                htmltools::includeMarkdown("docs/notes.Rmd")
              )
            )
  )
}
