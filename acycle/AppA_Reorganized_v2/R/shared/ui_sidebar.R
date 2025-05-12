ui_card_sidebar <- function(id=NULL) {
  grid_card(
    area = "sidebar", #--------------------------------------------------sidebar
    card_header("Selection"),
    card_body(
      treeInput( #district---target
        inputId = "rc6tC",
        label = "Target district",
        choices = create_tree(
          f240824b(rev(sort(unique(
            substr(dir(stepripG),1,3)
          ))))[(order(regionx,lab))]),
        selected = (rc6cuC[1]), 
        returnValue = "text",
        closeDepth = 0
      )
      ,
      sliderInput(
        inputId = "tslider",
        label = "Datum period",
        min=0,
        max=45,
        value=34
      )
      ,
      treeInput( #district---custom
        inputId = "rctreeC",
        label = "Custom selection",
        choices = create_tree(f240824b(rev(sort(unique(
          substr(dir(stepripG),1,3)
        ))))[(order(regionx,lab))]),
        selected = rc6cuC, 
        returnValue = "text",
        closeDepth = 0
      )
      ,
      span(
        textOutput('selrc6forjstest'), 
        style="color:white"
      )
      ,
      span(
        textOutput('comrc6forjstest'), 
        style="color:white"
      ) 
    )
  )
}
