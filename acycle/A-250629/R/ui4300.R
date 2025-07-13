#-------------------------------------------------4300 constituents

#experimental
# ui4300 <-
#   function(id = NULL) {
#     #B - works fine
#     nav_panel(
#       title = "gp4300", #--------------------------------Constituents
#       
#       bslib::navs_tab(
#         id = "table_tabs",
#         nav_panel(
#           title="Table A",
#           DT::DTOutput("O4311x")
#         ),
#         nav_panel(
#           title="Table B",
#           DT::DTOutput("O4311x")
#         )
#       )
#     )#nav_panel
#   }
# 
# 

ui4300 <-
  function(id = NULL) {
    #B - works fine
    nav_panel(
      title = "gp4300", #--------------------------------Constituents
      card(
        full_screen = TRUE,
        card_header(
          "Constituent districts"
        ),
        card_body( #-------------- constituent table 311
          DT::DTOutput("O4311x"),
          height = gridheight3
        )#card_body
      )#card
    )#nav_panel
  }

    


