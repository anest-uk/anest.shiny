ui_navpanel_accuracy <- function(id = NULL) {
  nav_panel(
    title = "Accuracy", #----------------------------------------Accuracy
    card(
      full_screen = TRUE,
      card_header("RMS error sensitivity to key parameters"),
      card_body(
        grid_container(
          layout = c(
            "timesamplinglocal      timesamplingcustom",
            "outlierrejectionlocal  outlierrejectioncustom",
            "crossvalidationlocal   crossvalidationcustom"
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
            area = "timesamplinglocal",
            full_screen = TRUE,
            card_header(
              "Time Sampling"
            ),
            card_body( #--------------accuracy 411
              gt::gt_output("x411")
            )
          ),
          grid_card(
            area = "timesamplingcustom",
            full_screen = TRUE,
            card_header(
              "."
            ),
            card_body( #--------------accuracy 412
              gt::gt_output("x412")
            )
          ),
          grid_card(
            area = "outlierrejectionlocal",
            full_screen = TRUE,
            card_header(
              "Outlier Rejection"
            ),
            card_body( #--------------accuracy 421
              gt::gt_output("x421")
            )
          ),
          grid_card(
            area = "outlierrejectioncustom",
            full_screen = TRUE,
            card_header(
              "."
            ),
            card_body( #--------------accuracy 412
              gt::gt_output("x422")
            )
          ),
          grid_card(
            area = "crossvalidationlocal",
            full_screen = TRUE,
            card_header(
              "Cross Validation"
            ),
            card_body( #--------------accuracy 431
              gt::gt_output("x431")
            )
          ),
          grid_card(
            area = "crossvalidationcustom",
            full_screen = TRUE,
            card_header(
              "Cross Validation"
            ),
            card_body( #--------------accuracy 432
              gt::gt_output("x432")
            )
          )
        )
      )
    )
  )
}
