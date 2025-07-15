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
  nav_panel(
    title = "gp4300", #---------.----
    grid_container(
      layout = c(
        "A4310x        ."#,
        #"A4321x        ."
      ),
      # layout = c("a ."),
      row_sizes = c("1fr"),
      col_sizes = c("1fr", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "A4310x",
        card_header(
          "Constituent districts"
        ),
        card_body( #-------------- constituent table 311
          gt::gt_output("O4311x"),
          height = gridheight4,
          uiOutput("O4321x")
          #textOutput("O4321x")
        ) # card_body
      )#,
      # grid_card(
      #   area = "A4321x",
      #   card_body( # message/meaning
      #     # textOutput("O4321x")
      #   ) # card_body
      # ) # card
    ) # gridcontainer
  ) # navpanel
}
