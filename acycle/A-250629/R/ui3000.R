ui3000 <- function(id = NULL) {
  grid_card(
    area = "A3000", #-----------------------------sidebar 3000
    card_header("Selection"),
    card_body(
      card(
        treeInput( # district---target
          inputId = "rc6tC",
          label = "Target district",
          choices = create_tree(
            f240824b(rev(sort(unique(
              substr(dir(stepripG), 1, 3)
            ))))[(order(regionx, lab))]
          ),
          selected = (rc6cuC[1]),
          returnValue = "text",
          closeDepth = 0
        )
        ,
        textOutput("O3002x"),
        fill=FALSE
      ),
      card(
        sliderInput(
          inputId = "tslider",
          label = "Datum t",
          min = 0,
          max = 45,
          value = 34
        ),
        #textOutput("datet0"),
        textOutput("O3001x"),
        fill=FALSE
      ),
      card(
        treeInput( # district---custom
          inputId = "rctreeC",
          label = "Custom selection",
          choices = create_tree(f240824b(rev(sort(unique(
            substr(dir(stepripG), 1, 3)
          ))))[(order(regionx, lab))]),
          selected = rc6cuC,
          returnValue = "text",
          closeDepth = 0
        ),
        fill=FALSE
      ),
      span(
        textOutput("selrc6forjstest"),
        style = "color:white"
      ),
      span(
        textOutput("comrc6forjstest"),
        style = "color:white"
      )
    )
  )
}
