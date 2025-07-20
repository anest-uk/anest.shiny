# ui4300 <- function(id = NULL) {
#   nav_panel(
#      title = "gp4300",
#     grid_container(
#       layout = c("A ."),
#       col_sizes = c("1fr", "1fr"),
#       row_sizes = c("1fr"),
#       gap_size = "10px",
#       grid_card(
#         area = "A",
#         card_header("Test"),
#         card_body(
#           p("Hi")  # ✅ wrap the string
#         )
#       )
#     )
#   )
# }

# helloworld

ui4100 <- function(id = NULL) {
  print('enter ui4300')
  nav_panel(
    title = "4100", #---------.----
    grid_container(
      layout = c(
        "AA4411        AA4112",
        "AA4121        ."
      ),
      row_sizes = c("1fr"),
      col_sizes = c("1fr", "1fr"),
      gap_size = "10px",
      
      grid_card(
        area = "AA4411",
        card_header(
          "Constituent districts"
        ),
        card_body( #-------------- local table 311
          gt::gt_output("OO4111"),
          height = gridheight4,
          uiOutput("OO4121"), #'identifies as' text
          gt::gt_output("O4311b")
        ) # card_body
      ), # grid_card
      
      grid_card(
        area = "AA4121",
        card_header(
          "Custom districts"
        ),
        card_body( #-------------- custom table 
          gt::gt_output("OO4131")
        ) # card_body
      ), # grid_card
      
      grid_card(
        area = "AA4112",
        card_header(
          uiOutput("O4300x")
        ),
        card_body( # leaflet
          leafletOutput("OO4112"),
          height = gridheight
        ) # card_body
        
      ) # card
      
    ) # gridcontainer
  ) # navpanel
}
