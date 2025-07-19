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

ui4300 <- function(id = NULL) {
  print('enter ui4300')
  nav_panel(
    title = "gp4300", #---------.----
    grid_container(
      layout = c(
        "A4311x        A4312x",
        "A4331x        ."
      ),
      row_sizes = c("1fr"),
      col_sizes = c("1fr", "1fr"),
      gap_size = "10px",
      
      grid_card(
        area = "A4311x",
        card_header(
          "Constituent districts"
        ),
        card_body( #-------------- local table 311
          gt::gt_output("O4311x"),
          height = gridheight4,
          uiOutput("O4321x"), #'identifies as' text
          gt::gt_output("O4311b")
        ) # card_body
      ), # grid_card
      
      grid_card(
        area = "A4331x",
        card_header(
          "Custom districts"
        ),
        card_body( #-------------- custom table 
          gt::gt_output("O4331x")
        ) # card_body
      ), # grid_card
      
      grid_card(
        area = "A4312x",
        card_header(
          uiOutput("O4300x")
        ),
        card_body( # leaflet
          leafletOutput("O4312x"),
          height = gridheight
        ) # card_body
        
      ) # card
      
    ) # gridcontainer
  ) # navpanel
}
