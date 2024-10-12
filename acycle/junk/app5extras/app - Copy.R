library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(DT)
#shinyuieditor::launch_editor('acycle/app5nbts')
#title = div(img(src = "cbrelogo2.png", height = 35),'....example only'),
#title = div(img(src = "A.png", height = 55),'cycle'),
#title = "Acycle",
#title = div(img(src = "A.png", height = 55)),
#tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),

ui <- page_navbar(
  footer = tags$div("powered by Acycle"),
  title = "Acycle",
  selected = "Index",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  nav_panel(
    title = "Index",
    tabsetPanel(
      nav_panel(
        title = "Custom",
        tabsetPanel(
          nav_panel(
            title = "Settings",
            grid_container(
              layout = c(
                "area0 area3 area6",
                "area1 area4 area7",
                "area2 area5 area8"
              ),
              row_sizes = c(
                "1fr",
                "1fr",
                "1fr"
              ),
              col_sizes = c(
                "1fr",
                "1fr",
                "1fr"
              ),
              gap_size = "10px",
              grid_card(
                area = "area0",
                card_body(
                  radioButtons(
                    inputId = "myRadioButtons",
                    label = "Time interval",
                    choices = list(
                      "Annual" = "a",
                      "Quarterly" = "b",
                      "Monthly" = "value3",
                      "Custom" = "value4"
                    ),
                    width = "100%"
                  )
                )
              ),
              grid_card(
                area = "area1",
                card_body(
                  radioButtons(
                    inputId = "myRadioButtons",
                    label = "Type",
                    choices = list("House" = "a", "Flat" = "b", "All" = "value3"),
                    width = "100%"
                  )
                )
              ),
              grid_card(
                area = "area2",
                card_body(
                  radioButtons(
                    inputId = "myRadioButtons",
                    label = "Age",
                    choices = list("New" = "a", "Used" = "b", "All" = "value3"),
                    width = "100%"
                  )
                )
              ),
              grid_card(
                area = "area3",
                card_body(
                  radioButtons(
                    inputId = "myRadioButtons",
                    label = "Geo definition",
                    choices = list("Tree picker" = "a", "Custom file" = "b"),
                    width = "100%"
                  ),
                  textInput(
                    inputId = "myTextInput",
                    label = "Target district",
                    value = ""
                  )
                )
              ),
              grid_card(
                area = "area4",
                full_screen = TRUE,
                card_header(
                  "[tree picker]
                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                                                                                              "
                )
              ),
              grid_card(
                area = "area5",
                full_screen = TRUE,
                card_header(
                  "Custom file upload
                                                                                                                                                                                                                                                                                                                                                                                                                              "
                )
              ),
              grid_card(
                area = "area6",
                card_body(
                  radioButtons(
                    inputId = "myRadioButtons",
                    label = "Outlier reject %",
                    choices = list("0" = "b", "10" = "a", "30" = "value3"),
                    width = "100%"
                  )
                )
              ),
              grid_card(
                area = "area7",
                card_body(
                  radioButtons(
                    inputId = "myRadioButtons",
                    label = "Cross-validation",
                    choices = list("none" = "a", "5-fold" = "b"),
                    width = "100%"
                  )
                )
              ),
              grid_card(
                area = "area8",
                card_body(
                  actionButton(inputId = "myButton", label = "Compute now")
                )
              )
            )
          ),
          nav_panel(
            title = "Index",
            grid_container(
              layout = c(
                "area0 area1",
                "area2 area3",
                "area4 area5"
              ),
              row_sizes = c(
                "1fr",
                "1fr",
                "1fr"
              ),
              col_sizes = c(
                "1fr",
                "1fr"
              ),
              gap_size = "10px",
              grid_card(
                area = "area0",
                full_screen = TRUE,
                card_header("Index time-series"),
                card_body(DTOutput(outputId = "myTable", width = "100%"))
              ),
              grid_card(
                area = "area1",
                full_screen = TRUE,
                card_header("Map")
              ),
              grid_card(
                area = "area2",
                full_screen = TRUE,
                card_header("Index return (winding table)")
              ),
              grid_card(
                area = "area3",
                full_screen = TRUE,
                card_header(
                  "Geo-bin characteristics table
                                                                                                                                                                                                                                                                                                                                                                        "
                ),
                card_body(DTOutput(outputId = "myTable", width = "100%"))
              ),
              grid_card(
                area = "area4",
                full_screen = TRUE,
                card_header("Time-series summary"),
                card_body(DTOutput(outputId = "myTable", width = "100%"))
              ),
              grid_card(
                area = "area5",
                full_screen = TRUE,
                card_header("Header")
              )
            )
          )
        )
      ),
      nav_panel(
        title = "National",
        grid_container(
          layout = c(
            "           ",
            "area2 area3",
            "area4 .    "
          ),
          row_sizes = c(
            "1fr",
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "1fr",
            "1fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "",
            full_screen = TRUE,
            card_header(
              "Index time-series
                                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                              "
            ),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          ),
          grid_card(
            area = "area1",
            full_screen = TRUE,
            card_header("Map")
          ),
          grid_card(
            area = "area2",
            full_screen = TRUE,
            card_header(
              "Index return (winding table)
                                                                                                                                                                                                                                                                                                                                                "
            )
          ),
          grid_card(
            area = "area3",
            full_screen = TRUE,
            card_header(
              "Geo-bin characteristics table
                                                                                                                                                                                                                                                                                                                                                "
            ),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          ),
          grid_card(
            area = "area4",
            full_screen = TRUE,
            card_header("Time-series summary"),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          )
        )
      ),
      nav_panel(
        title = "Local",
        grid_container(
          layout = c(
            "area0 area1",
            "area2 area3",
            "area4 .    "
          ),
          row_sizes = c(
            "1fr",
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "1fr",
            "1fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            full_screen = TRUE,
            card_header(
              "Index time-series
                                                                                                                                                                                                                                                                                        "
            ),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          ),
          grid_card(
            area = "area1",
            full_screen = TRUE,
            card_header("Map")
          ),
          grid_card(
            area = "area2",
            full_screen = TRUE,
            card_header("Index return (winding table)")
          ),
          grid_card(
            area = "area3",
            full_screen = TRUE,
            card_header(
              "Geo-bin characteristics table
                                                                                                                                                                                                                                                                                        "
            ),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          ),
          grid_card(
            area = "area4",
            full_screen = TRUE,
            card_header("Time-series summary"),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          )
        )
      ),
      nav_panel(
        title = "Accuracy",
        grid_container(
          layout = c(
            "area7 area1 area2",
            "area4 .     .    ",
            "area5 .     .    ",
            "area6 .     .    ",
            ".     .     .    "
          ),
          row_sizes = c(
            "0.93fr",
            "0.49fr",
            "0.45fr",
            "0.46fr",
            "2.67fr"
          ),
          col_sizes = c(
            "0.9fr",
            "1.1fr",
            "1fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area1",
            card_body(
              "R2",
              grid_container(
                layout = c(
                  "area0 area1 area2"
                ),
                gap_size = "10px",
                col_sizes = c(
                  "1fr",
                  "1fr",
                  "1fr"
                ),
                row_sizes = c(
                  "0.7000000000000001fr"
                ),
                grid_card(area = "area0", card_body("All")),
                grid_card(
                  area = "area1",
                  card_body(
                    "Outlier trim
                                                                                                                                                                                                                                                                                                                                                                                                                "
                  )
                ),
                grid_card(area = "area2", card_body("k-fold"))
              )
            )
          ),
          grid_card(
            area = "area2",
            card_body(
              "MSE
                                                                                                                                                                                                                                                                                        ",
              grid_container(
                layout = c(
                  "area0 area1 area2"
                ),
                gap_size = "10px",
                col_sizes = c(
                  "1fr",
                  "1fr",
                  "1fr"
                ),
                row_sizes = c(
                  "0.8fr"
                ),
                grid_card(area = "area0", card_body("All")),
                grid_card(area = "area1", card_body("Outlier trim")),
                grid_card(
                  area = "area2",
                  card_body(
                    "k-fold
                                                                                                                                                                                                                                                                                                                                                                                                                "
                  )
                )
              )
            )
          ),
          grid_card(
            area = "area4",
            card_body(
              "Custom
                                                                                                                                                                                                                                                                                        "
            )
          ),
          grid_card(area = "area5", card_body("National")),
          grid_card(area = "area6", card_body("Local")),
          grid_card(
            area = "area7",
            card_body("this is just a table!")
          )
        )
      ),
      nav_panel(
        title = "Download",
        grid_container(
          layout = c(
            "area0",
            "area1",
            "area2",
            ".    "
          ),
          row_sizes = c(
            "1fr",
            "1fr",
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "1fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "area0",
            card_body(
              card(
                full_screen = TRUE,
                card_header(
                  "Custom
                                                                                                                                                                                                                                                                                                                                    "
                ),
                card_body(
                  grid_container(
                    layout = c(
                      "area0 area1 ."
                    ),
                    row_sizes = c(
                      "1fr"
                    ),
                    col_sizes = c(
                      "0.5fr",
                      "0.5fr",
                      "2fr"
                    ),
                    gap_size = "10px",
                    grid_card(
                      area = "area0",
                      full_screen = TRUE,
                      card_header("csv"),
                      card_body(
                        actionButton(inputId = "myButton", label = "Data")
                      )
                    ),
                    grid_card(
                      area = "area1",
                      full_screen = TRUE,
                      card_header("html"),
                      card_body(
                        actionButton(inputId = "myButton", label = "Report")
                      )
                    )
                  )
                )
              )
            )
          ),
          grid_card(
            area = "area1",
            card_body(
              card(
                full_screen = TRUE,
                card_header("National"),
                card_body(
                  grid_container(
                    layout = c(
                      "area0 area1 ."
                    ),
                    row_sizes = c(
                      "1fr"
                    ),
                    col_sizes = c(
                      "0.5fr",
                      "0.5fr",
                      "2fr"
                    ),
                    gap_size = "10px",
                    grid_card(
                      area = "area0",
                      full_screen = TRUE,
                      card_header("csv"),
                      card_body(
                        actionButton(inputId = "myButton", label = "Data")
                      )
                    ),
                    grid_card(
                      area = "area1",
                      full_screen = TRUE,
                      card_header("html"),
                      card_body(
                        actionButton(inputId = "myButton", label = "Report")
                      )
                    )
                  )
                )
              )
            )
          ),
          grid_card(
            area = "area2",
            card_body(
              card(
                full_screen = TRUE,
                card_header("Local"),
                card_body(
                  grid_container(
                    layout = c(
                      "area0 area1 ."
                    ),
                    row_sizes = c(
                      "1fr"
                    ),
                    col_sizes = c(
                      "0.5fr",
                      "0.5fr",
                      "2fr"
                    ),
                    gap_size = "10px",
                    grid_card(
                      area = "area0",
                      full_screen = TRUE,
                      card_header("csv"),
                      card_body(
                        actionButton(inputId = "myButton", label = "Data")
                      )
                    ),
                    grid_card(
                      area = "area1",
                      full_screen = TRUE,
                      card_header("html"),
                      card_body(
                        actionButton(inputId = "myButton", label = "Report")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Cycle",
    grid_container(
      layout = c(
        "area0 area1"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "0.28fr",
        "1.72fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        full_screen = TRUE,
        card_header(
          "Settings
                                                                                                                                                                                                                                                                                                                                                                                  "
        ),
        card_body(
          textInput(
            inputId = "myTextInput",
            label = "Reference/benchmark",
            value = "SW3"
          ),
          sliderInput(
            inputId = "inputId",
            label = "Date",
            min = 1995,
            max = 2025,
            value = 2024,
            width = "100%",
            step = 1
          ),
          checkboxGroupInput(
            inputId = "myCheckboxGroup",
            label = "Local index (treeselect)",
            choices = list("choice a" = "a", "choice b" = "b")
          ),
          checkboxGroupInput(
            inputId = "myCheckboxGroup",
            label = "National index ",
            choices = list(
              "1" = "a",
              "2" = "b",
              "3" = "value3",
              "4" = "value4",
              "5" = "value5",
              "6" = "value6",
              "7" = "value7",
              "8" = "value8",
              "9" = "value9",
              "10" = "value10"
            )
          ),
          checkboxGroupInput(
            inputId = "myCheckboxGroup",
            label = "Custom",
            choices = list(" " = "a")
          )
        )
      ),
      grid_card(
        area = "area1",
        card_body(
          tabsetPanel(
            nav_panel(
              title = "Value/Momentum",
              grid_container(
                layout = c(
                  "area0 area1",
                  "area2 area3"
                ),
                row_sizes = c(
                  "0.97fr",
                  "1.03fr"
                ),
                col_sizes = c(
                  "1fr",
                  "1fr"
                ),
                gap_size = "10px",
                grid_card(
                  area = "area0",
                  full_screen = TRUE,
                  card_header(
                    "C/c = Value/Momentum = Dog/star
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                  ),
                  card_footer(
                    DTOutput(outputId = "myTable", width = "100%")
                  )
                ),
                grid_card(
                  area = "area1",
                  full_screen = TRUE,
                  card_header(
                    "c/return/y-axis/momentum  barchart
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                  )
                ),
                grid_card(
                  area = "area2",
                  full_screen = TRUE,
                  card_header(
                    "C/index/x-axis/value-flipped  barchart
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                  )
                ),
                grid_card(
                  area = "area3",
                  full_screen = TRUE,
                  card_header(
                    "Scatter c/total or c/(c+s
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                  ),
                  card_body(DTOutput(outputId = "myTable", width = "100%"))
                )
              )
            ),
            nav_panel(
              title = "Factor/beta",
              grid_container(
                layout = c(
                  "area0 area1",
                  "area2 area3"
                ),
                row_sizes = c(
                  "1fr",
                  "1fr"
                ),
                col_sizes = c(
                  "1fr",
                  "1fr"
                ),
                gap_size = "10px",
                grid_card(
                  area = "area0",
                  full_screen = TRUE,
                  card_header(
                    "z23 b23 points
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                  ),
                  card_footer(
                    DTOutput(outputId = "myTable", width = "100%")
                  )
                ),
                grid_card(
                  area = "area1",
                  full_screen = TRUE,
                  card_header(
                    "x23 timeseries
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "
                  )
                ),
                grid_card(
                  area = "area2",
                  full_screen = TRUE,
                  card_header("z23 b23-centred")
                ),
                grid_card(
                  area = "area3",
                  full_screen = TRUE,
                  card_header(
                    "xwave and xcycle
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "
                  ),
                  card_body(DTOutput(outputId = "myTable", width = "100%"))
                )
              )
            ),
            nav_panel(
              title = "Cross-section",
              grid_container(
                layout = c(
                  "area0",
                  "area1"
                ),
                row_sizes = c(
                  "1fr",
                  "1fr"
                ),
                col_sizes = c(
                  "1fr"
                ),
                gap_size = "10px",
                grid_card(
                  area = "area0",
                  full_screen = TRUE,
                  card_header(
                    "Index/level
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                  ),
                  card_body(
                    grid_container(
                      layout = c(
                        "area0 area4 area1 area2 area3"
                      ),
                      row_sizes = c(
                        "1fr"
                      ),
                      col_sizes = c(
                        "1fr",
                        "1fr",
                        "1fr",
                        "1fr",
                        "1fr"
                      ),
                      gap_size = "10px",
                      grid_card(
                        area = "area0",
                        full_screen = TRUE,
                        card_header(
                          "X Index total
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "
                        ),
                        card_body(DTOutput(outputId = "myTable", width = "100%"))
                      ),
                      grid_card(
                        area = "area1",
                        full_screen = TRUE,
                        card_header(
                          "C cyclical
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "
                        )
                      ),
                      grid_card(
                        area = "area2",
                        full_screen = TRUE,
                        card_header(
                          "S systematic
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "
                        )
                      ),
                      grid_card(
                        area = "area3",
                        full_screen = TRUE,
                        card_header("R residual")
                      ),
                      grid_card(
                        area = "area4",
                        full_screen = TRUE,
                        card_header(
                          "M market
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "
                        )
                      )
                    )
                  )
                ),
                grid_card(
                  area = "area1",
                  card_body(
                    card(
                      full_screen = TRUE,
                      card_header("Return/change"),
                      card_body(
                        grid_container(
                          layout = c(
                            "area0 area1 area2 area3 area4"
                          ),
                          row_sizes = c(
                            "1fr"
                          ),
                          col_sizes = c(
                            "1fr",
                            "1fr",
                            "1fr",
                            "1fr",
                            "1fr"
                          ),
                          gap_size = "10px",
                          grid_card(
                            area = "area0",
                            full_screen = TRUE,
                            card_header(
                              "x return total
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                            )
                          ),
                          grid_card(
                            area = "area1",
                            full_screen = TRUE,
                            card_header(
                              "m market
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                            )
                          ),
                          grid_card(
                            area = "area2",
                            full_screen = TRUE,
                            card_header(
                              "c cyclical
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                            )
                          ),
                          grid_card(
                            area = "area3",
                            full_screen = TRUE,
                            card_header(
                              "s systematic
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                            )
                          ),
                          grid_card(
                            area = "area4",
                            full_screen = TRUE,
                            card_header(
                              "r residual
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                            ),
                            card_body(DTOutput(outputId = "myTable", width = "100%"))
                          )
                        )
                      ),
                      card_body()
                    )
                  )
                )
              )
            ),
            nav_panel(
              title = "Time-series",
              grid_container(
                layout = c(
                  "area0",
                  "area1"
                ),
                row_sizes = c(
                  "1fr",
                  "1fr"
                ),
                col_sizes = c(
                  "1fr"
                ),
                gap_size = "10px",
                grid_card(
                  area = "area0",
                  full_screen = TRUE,
                  card_header(
                    "Index/level
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                  ),
                  card_body(
                    grid_container(
                      layout = c(
                        "area0 area1 area2 area3 area4"
                      ),
                      row_sizes = c(
                        "1fr"
                      ),
                      col_sizes = c(
                        "1fr",
                        "1fr",
                        "1fr",
                        "1fr",
                        "1fr"
                      ),
                      gap_size = "10px",
                      grid_card(
                        area = "area0",
                        full_screen = TRUE,
                        card_header("X "),
                        card_body(DTOutput(outputId = "myTable", width = "100%"))
                      ),
                      grid_card(
                        area = "area1",
                        full_screen = TRUE,
                        card_header("M")
                      ),
                      grid_card(
                        area = "area2",
                        full_screen = TRUE,
                        card_header("C")
                      ),
                      grid_card(
                        area = "area3",
                        full_screen = TRUE,
                        card_header("S")
                      ),
                      grid_card(
                        area = "area4",
                        full_screen = TRUE,
                        card_header("R")
                      )
                    )
                  )
                ),
                grid_card(
                  area = "area1",
                  full_screen = TRUE,
                  card_header(
                    "Retrun/change
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                  ),
                  card_body(
                    grid_container(
                      layout = c(
                        "area0 area1 area2 area3 area4"
                      ),
                      row_sizes = c(
                        "1fr"
                      ),
                      col_sizes = c(
                        "1fr",
                        "1fr",
                        "1fr",
                        "1fr",
                        "1fr"
                      ),
                      gap_size = "10px",
                      grid_card(
                        area = "area0",
                        full_screen = TRUE,
                        card_header("x")
                      ),
                      grid_card(
                        area = "area1",
                        full_screen = TRUE,
                        card_header("m")
                      ),
                      grid_card(
                        area = "area2",
                        full_screen = TRUE,
                        card_header("c")
                      ),
                      grid_card(
                        area = "area3",
                        full_screen = TRUE,
                        card_header("s")
                      ),
                      grid_card(
                        area = "area4",
                        full_screen = TRUE,
                        card_header("r"),
                        card_body(DTOutput(outputId = "myTable", width = "100%"))
                      )
                    )
                  )
                )
              )
            ),
            nav_panel(
              title = "Download",
              grid_container(
                layout = c(
                  "area0",
                  "area1",
                  "area2"
                ),
                gap_size = "10px",
                col_sizes = c(
                  "1fr"
                ),
                row_sizes = c(
                  "1fr",
                  "1fr",
                  "1fr"
                ),
                grid_card(
                  area = "area0",
                  full_screen = TRUE,
                  card_header(
                    "Factor
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "
                  ),
                  card_body(
                    grid_container(
                      layout = c(
                        "area0 area1 ."
                      ),
                      row_sizes = c(
                        "1fr"
                      ),
                      col_sizes = c(
                        "0.5fr",
                        "0.5fr",
                        "2fr"
                      ),
                      gap_size = "10px",
                      grid_card(
                        area = "area0",
                        full_screen = TRUE,
                        card_header("csv"),
                        card_body(
                          actionButton(inputId = "button1", label = "Data")
                        )
                      ),
                      grid_card(
                        area = "area1",
                        full_screen = TRUE,
                        card_header("html"),
                        card_body(
                          actionButton(inputId = "myButton", label = "Report")
                        )
                      )
                    )
                  )
                ),
                grid_card(
                  area = "area1",
                  full_screen = TRUE,
                  card_header("Beta"),
                  card_body(
                    grid_container(
                      layout = c(
                        "area0 area1 ."
                      ),
                      row_sizes = c(
                        "1fr"
                      ),
                      col_sizes = c(
                        "0.5fr",
                        "0.5fr",
                        "2fr"
                      ),
                      gap_size = "10px",
                      grid_card(
                        area = "area0",
                        full_screen = TRUE,
                        card_header("csv"),
                        card_body(
                          actionButton(inputId = "myButton", label = "Data")
                        )
                      ),
                      grid_card(
                        area = "area1",
                        full_screen = TRUE,
                        card_header("html"),
                        card_body(
                          actionButton(inputId = "myButton", label = "Report")
                        )
                      )
                    )
                  )
                ),
                grid_card(
                  area = "area2",
                  full_screen = TRUE,
                  card_header(
                    "Covariance
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "
                  ),
                  card_body(
                    grid_container(
                      layout = c(
                        "area0 area1 ."
                      ),
                      row_sizes = c(
                        "1fr"
                      ),
                      col_sizes = c(
                        "0.5fr",
                        "0.5fr",
                        "2fr"
                      ),
                      gap_size = "10px",
                      grid_card(
                        area = "area0",
                        full_screen = TRUE,
                        card_header("csv"),
                        card_body(
                          actionButton(inputId = "myButton", label = "Data")
                        )
                      ),
                      grid_card(
                        area = "area1",
                        full_screen = TRUE,
                        card_header("html"),
                        card_body(
                          actionButton(inputId = "myButton", label = "Report")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Data",
    grid_container(
      layout = c(
        "area2 area6  area11",
        "area1 area7  area12",
        "area3 area8  area13",
        "area4 area9  area14",
        "area5 area10 area15"
      ),
      row_sizes = c(
        "1fr",
        "1fr",
        "1fr",
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "0.5fr",
        "1.5fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area1",
        full_screen = TRUE,
        card_header("PPD-id"),
        card_body(
          actionButton(inputId = "myButton", label = "Download")
        )
      ),
      grid_card(
        area = "area2",
        full_screen = TRUE,
        card_header("EPC-id"),
        card_body(
          actionButton(inputId = "myButton", label = "Download")
        )
      ),
      grid_card(
        area = "area3",
        full_screen = TRUE,
        card_header(
          "Seasonal
                                                                                                                                                                                                                                                                    "
        ),
        card_body(
          actionButton(inputId = "myButton", label = "Download")
        )
      ),
      grid_card(
        area = "area4",
        full_screen = TRUE,
        card_header(
          "Sale
                                                                                                                                                                                                                                                                    "
        ),
        card_body(
          actionButton(inputId = "myButton", label = "Download")
        )
      ),
      grid_card(
        area = "area5",
        full_screen = TRUE,
        card_header(
          "Repeat
                                                                                                                                                                                                                                                                    "
        ),
        card_body(
          actionButton(inputId = "myButton", label = "Download")
        )
      ),
      grid_card(
        area = "area6",
        full_screen = TRUE,
        card_header(
          "Sample
                                                                                                                                                                                                                                                          "
        )
      ),
      grid_card(
        area = "area7",
        full_screen = TRUE,
        card_header("Sample")
      ),
      grid_card(
        area = "area8",
        full_screen = TRUE,
        card_header("Sample")
      ),
      grid_card(
        area = "area9",
        full_screen = TRUE,
        card_header("Sample")
      ),
      grid_card(
        area = "area10",
        full_screen = TRUE,
        card_header("Sample")
      ),
      grid_card(
        area = "area11",
        full_screen = TRUE,
        card_header(
          "Fields
                                                                                                                                                                                                                                                          "
        )
      ),
      grid_card(
        area = "area12",
        full_screen = TRUE,
        card_header("Fields")
      ),
      grid_card(
        area = "area13",
        full_screen = TRUE,
        card_header("Fields")
      ),
      grid_card(
        area = "area14",
        full_screen = TRUE,
        card_header("Fields")
      ),
      grid_card(
        area = "area15",
        full_screen = TRUE,
        card_header("Fields")
      )
    )
  )
)


server <- function(input, output) {
   
  output$linePlots <- renderPlot({
    obs_to_include <- as.integer(ChickWeight$Chick) <= input$numChicks
    chicks <- ChickWeight[obs_to_include, ]
  
    ggplot(
      chicks,
      aes(
        x = Time,
        y = weight,
        group = Chick
      )
    ) +
      geom_line(alpha = 0.5) +
      ggtitle("Chick weights over time")
  })
  
  output$dists <- renderPlot({
    ggplot(
      ChickWeight,
      aes(x = weight)
    ) +
      facet_wrap(input$distFacet) +
      geom_density(fill = "#fa551b", color = "#ee6331") +
      ggtitle("Distribution of weights by diet")
  })
  
}

shinyApp(ui, server)
  

