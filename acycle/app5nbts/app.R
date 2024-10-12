library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(DT)


ui <- page_navbar(
  title = "Acycle",
  selected = "Index",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  nav_panel(
    title = "Index",
    tabsetPanel(
      nav_panel(
        title = "Custom",
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
            )
          ),
          grid_card(
            area = "area1",
            card_body(
              grid_container(
                layout = c(
                  "area0 area1",
                  "area2 area3",
                  ".     .    "
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
                  card_header("Header"),
                  card_body()
                ),
                grid_card(
                  area = "area1",
                  full_screen = TRUE,
                  card_header("Header")
                ),
                grid_card(
                  area = "area2",
                  full_screen = TRUE,
                  card_header("Header")
                ),
                grid_card(
                  area = "area3",
                  full_screen = TRUE,
                  card_header("Header"),
                  card_body()
                )
              )
            )
          )
        )
      ),
      nav_panel(
        title = "National",
        grid_container(
          layout = c(
            ". .",
            ". .",
            ". ."
          ),
          row_sizes = c(
            "1fr",
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "0.95fr",
            "1.05fr"
          ),
          gap_size = "10px"
        )
      ),
      nav_panel(
        title = "Local",
        grid_container(
          layout = c(
            ". .",
            ". .",
            ". ."
          ),
          gap_size = "10px",
          col_sizes = c(
            "1fr",
            "1fr"
          ),
          row_sizes = c(
            "1fr",
            "1fr",
            "1fr"
          )
        )
      ),
      nav_panel(title = "Accuracy"),
      nav_panel(title = "Download")
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
              title = "Factor-beta",
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
                        card_footer(
                          DTOutput(outputId = "myTable", width = "100%")
                        )
                      ),
                      grid_card(
                        area = "area1",
                        full_screen = TRUE,
                        card_header(
                          "c cyclical
                                                                                                                                                                                                                "
                        )
                      ),
                      grid_card(
                        area = "area2",
                        full_screen = TRUE,
                        card_header(
                          "s systematic
                                                                                                                                                                                                                "
                        )
                      ),
                      grid_card(
                        area = "area3",
                        full_screen = TRUE,
                        card_header("r residual")
                      ),
                      grid_card(
                        area = "area4",
                        full_screen = TRUE,
                        card_header(
                          "m market
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
            nav_panel(title = "Time-series"),
            nav_panel(title = "Download")
          )
        )
      )
    )
  ),
  nav_panel(title = "Data")
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
  

