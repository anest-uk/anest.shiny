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
                "incuseti incusege incuseou",
                "incusety incusetr incusecr",
                "incuseag incusecu incuseco"
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
                area = "incuseti",
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
                area = "incusety",
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
                area = "incuseag",
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
                area = "incusege",
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
                area = "incusetr",
                full_screen = TRUE,
                card_header(
                  "[tree picker]
                                                                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "
                )
              ),
              grid_card(
                area = "incusecu",
                full_screen = TRUE,
                card_header(
                  "Custom file upload
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "
                )
              ),
              grid_card(
                area = "incuseou",
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
                area = "incusecr",
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
                area = "incuseco",
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
                "incuinti incuinma",
                "incuinwi incuinch",
                "incuinsu incuinx"
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
                area = "incuinti",
                full_screen = TRUE,
                card_header("Index time-series"),
                card_body(DTOutput(outputId = "myTable", width = "100%"))
              ),
              grid_card(
                area = "incuinma",
                full_screen = TRUE,
                card_header("Map")
              ),
              grid_card(
                area = "incuinwi",
                full_screen = TRUE,
                card_header("Index return (winding table)")
              ),
              grid_card(
                area = "incuinch",
                full_screen = TRUE,
                card_header(
                  "Geo-bin characteristics table
                                                                                                                                                                                                                                                                                                                                                                                                            "
                ),
                card_body(DTOutput(outputId = "myTable", width = "100%"))
              ),
              grid_card(
                area = "incuinsu",
                full_screen = TRUE,
                card_header("Time-series summary"),
                card_body(DTOutput(outputId = "myTable", width = "100%"))
              ),
              grid_card(
                area = "incuinx",
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
            "innati innama",
            "innawi innach",
            "innasu .    "
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
            area = "innati",
            full_screen = TRUE,
            card_header("Index time-series"),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          ),
          grid_card(
            area = "innama",
            full_screen = TRUE,
            card_header("Map")
          ),
          grid_card(
            area = "innawi",
            full_screen = TRUE,
            card_header(
              "Index return (winding table)
                                                                                                                                                                                                                                                                                                                                                                            "
            )
          ),
          grid_card(
            area = "innach",
            full_screen = TRUE,
            card_header(
              "Geo-bin characteristics table
                                                                                                                                                                                                                                                                                                                                                                            "
            ),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          ),
          grid_card(
            area = "innasu",
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
            "inloti inloma",
            "inlowi inloch",
            "inlosu .    "
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
            area = "inloti",
            full_screen = TRUE,
            card_header(
              "Index time-series
                                                                                                                                                                                                                                                                                                                    "
            ),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          ),
          grid_card(
            area = "inloma",
            full_screen = TRUE,
            card_header("Map")
          ),
          grid_card(
            area = "inlowi",
            full_screen = TRUE,
            card_header("Index return (winding table)")
          ),
          grid_card(
            area = "inloch",
            full_screen = TRUE,
            card_header(
              "Geo-bin characteristics table
                                                                                                                                                                                                                                                                                                                    "
            ),
            card_body(DTOutput(outputId = "myTable", width = "100%"))
          ),
          grid_card(
            area = "inlosu",
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
            "inacth area1 area2",
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
            area = "inacth",
            card_body("this is just a table!")
          )
        )
      ),
      nav_panel(
        title = "Download",
        grid_container(
          layout = c(
            "indocu",
            "indona",
            "indolo",
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
            area = "indocu",
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
            area = "indona",
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
            area = "indolo",
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
        "cyse cyta"
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
        area = "cyse",
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
        area = "cyta",
        card_body(
          tabsetPanel(
            nav_panel(
              title = "Value/Momentum",
              grid_container(
                layout = c(
                  "cyvmcc cyvmcr",
                  "cyvmci cyvmct"
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
                  area = "cyvmcc",
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
                  area = "cyvmcr",
                  full_screen = TRUE,
                  card_header(
                    "c/return/y-axis/momentum  barchart
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "
                  )
                ),
                grid_card(
                  area = "cyvmci",
                  full_screen = TRUE,
                  card_header(
                    "C/index/x-axis/value-flipped  barchart
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "
                  )
                ),
                grid_card(
                  area = "cyvmct",
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
                  "cyfbar cyfbxt",
                  "cyfbce cyfbwc"
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
                  area = "cyfbar",
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
                  area = "cyfbxt",
                  full_screen = TRUE,
                  card_header(
                    "x23 timeseries
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "
                  )
                ),
                grid_card(
                  area = "cyfbce",
                  full_screen = TRUE,
                  card_header("z23 b23-centred")
                ),
                grid_card(
                  area = "cyfbwc",
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
                        "cycrix cycrim cycric cycris cycrir"
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
                        area = "cycrix",
                        full_screen = TRUE,
                        card_header(
                          "X Index total
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "
                        ),
                        card_body(DTOutput(outputId = "myTable", width = "100%"))
                      ),
                      grid_card(
                        area = "cycric",
                        full_screen = TRUE,
                        card_header(
                          "C cyclical
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "
                        )
                      ),
                      grid_card(
                        area = "cycris",
                        full_screen = TRUE,
                        card_header(
                          "S systematic
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "
                        )
                      ),
                      grid_card(
                        area = "cycrir",
                        full_screen = TRUE,
                        card_header("R residual")
                      ),
                      grid_card(
                        area = "cycrim",
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
                            "cycrrx cycrrm cycrrc cycrrs cycrr"
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
                            area = "cycrrx",
                            full_screen = TRUE,
                            card_header(
                              "x return total
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                            )
                          ),
                          grid_card(
                            area = "cycrrm",
                            full_screen = TRUE,
                            card_header(
                              "m market
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                            )
                          ),
                          grid_card(
                            area = "cycrrc",
                            full_screen = TRUE,
                            card_header(
                              "c cyclical
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                            )
                          ),
                          grid_card(
                            area = "cycrrs",
                            full_screen = TRUE,
                            card_header(
                              "s systematic
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
                            )
                          ),
                          grid_card(
                            area = "cycrr",
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
                        "cytsix cytsim cytsic cytsis cytsir"
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
                        area = "cytsix",
                        full_screen = TRUE,
                        card_header("X "),
                        card_body(DTOutput(outputId = "myTable", width = "100%"))
                      ),
                      grid_card(
                        area = "cytsim",
                        full_screen = TRUE,
                        card_header("M")
                      ),
                      grid_card(
                        area = "cytsic",
                        full_screen = TRUE,
                        card_header("C")
                      ),
                      grid_card(
                        area = "cytsis",
                        full_screen = TRUE,
                        card_header("S")
                      ),
                      grid_card(
                        area = "cytsir",
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
                        "cytsrx cytsrm cytsrc cytsrs cytsrr"
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
                        area = "cytsrx",
                        full_screen = TRUE,
                        card_header("x")
                      ),
                      grid_card(
                        area = "cytsrm",
                        full_screen = TRUE,
                        card_header("m")
                      ),
                      grid_card(
                        area = "cytsrc",
                        full_screen = TRUE,
                        card_header("c")
                      ),
                      grid_card(
                        area = "cytsrs",
                        full_screen = TRUE,
                        card_header("s")
                      ),
                      grid_card(
                        area = "cytsrr",
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
                  "cydofa",
                  "cydobe",
                  "cydoco"
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
                  area = "cydofa",
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
                  area = "cydobe",
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
                  area = "cydoco",
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
        "daipbu daipsu  daipfi",
        "dappbu dappsa  dappfi",
        "dasebu dasesa  dasefi",
        "dasabu dasasa  dasafi",
        "darebu daresa darefi"
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
        area = "dappbu",
        full_screen = TRUE,
        card_header("PPD-id"),
        card_body(
          actionButton(inputId = "myButton", label = "Download")
        )
      ),
      grid_card(
        area = "daipbu",
        full_screen = TRUE,
        card_header("EPC-id"),
        card_body(
          actionButton(inputId = "myButton", label = "Download")
        )
      ),
      grid_card(
        area = "dasebu",
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
        area = "dasabu",
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
        area = "darebu",
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
        area = "daipsu",
        full_screen = TRUE,
        card_header(
          "Sample
                                                                                                                                                                                                                                                                              "
        )
      ),
      grid_card(
        area = "dappsa",
        full_screen = TRUE,
        card_header("Sample")
      ),
      grid_card(
        area = "dasesa",
        full_screen = TRUE,
        card_header("Sample")
      ),
      grid_card(
        area = "dasasa",
        full_screen = TRUE,
        card_header("Sample")
      ),
      grid_card(
        area = "daresa",
        full_screen = TRUE,
        card_header("Sample")
      ),
      grid_card(
        area = "daipfi",
        full_screen = TRUE,
        card_header(
          "Fields
                                                                                                                                                                                                                                                                              "
        )
      ),
      grid_card(
        area = "dappfi",
        full_screen = TRUE,
        card_header("Fields")
      ),
      grid_card(
        area = "dasefi",
        full_screen = TRUE,
        card_header("Fields")
      ),
      grid_card(
        area = "dasafi",
        full_screen = TRUE,
        card_header("Fields")
      ),
      grid_card(
        area = "darefi",
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
  

