ui1000 <- function(id = NULL) {
  grid_card(
    area = "a1000", #-----------------------------action button 1000
    card_header("Custom"),
    card_body(
      div(
        style = "padding:4px",
        span(
          textOutput("defrc6"),
          style = "padding:0px;color:black;font-size:12px"
        ),
        span(
          textOutput("selrc6"),
          style = "padding:0px;color:black;font-size:12px"
        ),
        span(
          textOutput("comrc6"),
          style = "padding:0px;color:black;font-size:12px"
        ),
        conditionalPanel(
          condition = "output.comrc6forjstest==output.selrc6forjstest",
          span(
            textOutput("cuseqcom"),
            style = "color:black;font-size:12px"
          ),
        ),
        conditionalPanel(
          condition = "output.comrc6forjstest!=output.selrc6forjstest",
          span(
            textOutput("cusnecom"),
            style = "color:red;font-size:12px"
          ),
        ),
        actionButton("docusabC", "Recompute custom"),
      ),
    )
  )
}
