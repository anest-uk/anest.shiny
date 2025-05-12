ui_navpanel_tss <- function(id=NULL){
  nav_panel(
    title = "Time-series summary",#--------------------------Time-series summary
    grid_container(
      layout = c(
        "x111     xtimeseries    ",
        "Winding characteristics",
        "summary tradesummary   "
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
        area = "x111", 
        full_screen = TRUE,
        card_header(
          "Postcode area map"
        ),
        card_body(
          leaflet::leafletOutput('x111'),#-----------leaflet x111
          height=gridheight
        )
      ),
      grid_card(
        area = "xtimeseries",
        full_screen = TRUE,
        card_header(
          "Indices"
        ),
        card_body(
          
          
    verbatimTextOutput("debug_ts"),  # match outputId exactly<<<<<<<<<<<<<<<<<<<<<<<<<<<<debug
 
          plotOutput('x112'), #----------------------plot x112
          height=gridheight
        )
        
      ),
      grid_card(
        area = "Winding",
        full_screen = TRUE,
        card_header(
          "Price return summary"
        ),
        card_body(#----------------------------------winding table 121
          gt::gt_output('x121a'),
          gt::gt_output('x121b'),
          height=gridheight
        )
        
      ),
      grid_card(
        area = "characteristics",
        full_screen = TRUE,
        card_header(
          "Index characteristics"
        ),
        card_body(#---------------------------------characteristics table x122
          gt::gt_output('x122')
        )
      ),
      grid_card(
        area = "summary",
        full_screen = TRUE,
        card_header(
          "Summary"
        ),
        card_body(#--------------------------------return summary table x131
          gt::gt_output('x131'),
          height=gridheight
        )
      )
      ,
      grid_card(
        area = "tradesummary",
        full_screen = TRUE,
        card_header(
          "Trade recap"
        ),
        card_body(#--------------------------------trade recap table x132
          gt::gt_output('x132a'),
          gt::gt_output('x132b'),
          gt::gt_output('x132c'),
          gt::gt_output('x132d'),
          height=gridheight2
        )
      )
    )
  )
}
