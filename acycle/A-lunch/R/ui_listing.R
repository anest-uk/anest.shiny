ui_navpanel_listing <- function(# 211 = listing ----
    id=NULL) {
  nav_panel(title = "Listing", 
            
            grid_container(
              layout = c(
                "locallist  customlist"
              ),
              row_sizes = c(
                "1fr"
              ),
              col_sizes = c(
                "1fr",
                "1fr"
              ),
              grid_card(
                area='locallist',
                full_screen = TRUE,
                card_header(
                  "Local"
                ),
                card_body(#-------------- listing table 211
                  gt::gt_output('x211')
                )
              ),
              grid_card(
                area='customlist',
                full_screen = TRUE,
                card_header(
                  "Custom"
                ),
                card_body(#-------------- listing table 211cu
                  gt::gt_output('x211cu')
                )
              )
            )
  )}
