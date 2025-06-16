D122x()
D122()

rssccG #is incorrect

geoccG

coltab
x <- 
  data.table(coltab[,-'dark'])[,row:=1:.N][]
# C122()
# D122x()

x <- cbind(x,data.table(mtcars[1:6,]))[c(1,3,5),]

x %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = code),
    fn = function(codes) {
      map2_chr(codes, x$light, ~ paste0(
        "<span style='color:", .y, "; font-weight:bold;'>", .x, "</span>"
      )) %>%
        map(htmltools::HTML)
    }
  ) %>%
  cols_hide(columns = light)






#dump('x','')

# x <-
# structure(list(light = c("#FF9FB5", "#F7B84F", "#84E26A", "#84E26A", 
# "#66B2F7", "#BBA3F9"), code = c("1.3", "1.2", "1.1", "2.3", "2.2", 
# "3.3"), row = 1:6), row.names = c(NA, -6L), class = "data.frame")


#option 1
x %>%
  gt() %>%
  gt::tab_style(
    style = list(
      cell_fill(color = x$light),
      cell_text(color = x$light)  # hide text by making it same as background
    ),
    locations = cells_body(columns = light)
  )

#option 1 produces an error:
# Error in `cell_fill()`:
# ! `color` must have a length one, not 6.

#option 2
x %>%
  mutate(color_disk = paste0(
    "<div style='height:15px; width:15px; border-radius:50%; background:", light, "; margin:auto;'></div>"
  )) %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = color_disk),
    fn = function(x) x
  ) %>%
  cols_label(color_disk = "Color")

#option 2 gives no error, but in the rstudio viewer the html shows up literally e.g. <div style='height:15px; width:15px; border-radius:50%; background:#FF9FB5; margin:auto;'></div>



#------------------------------------
library(gt)
library(purrr)

x %>%
  gt() %>%
  {
    tbl <- .
    # Add a row-specific style per color
    walk2(x$light, seq_len(nrow(x)), ~ {
      tbl <<- tab_style(
        tbl,
        style = list(
          cell_fill(color = .x),
          cell_text(color = .x)  # hides the text
        ),
        locations = cells_body(columns = light, rows = .y)
      )
    })
    tbl
  }

library(gt)
library(dplyr)

x %>%
  mutate(color_disk = paste0(
    "<div style='height:15px; width:15px; border-radius:50%; background:", light, "; margin:auto;'></div>"
  )) %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = color_disk),
    fn = function(x) x
  ) %>%
  cols_label(color_disk = "Color") %>%
  opt_html(escape = FALSE)


x %>%
  mutate(color_disk = paste0(
    "<div style='height:15px; width:15px; border-radius:50%; background:", light, "; margin:auto;'></div>"
  )) %>%
  gt(escape = FALSE) %>%
  text_transform(
    locations = cells_body(columns = color_disk),
    fn = identity
  ) %>%
  cols_label(color_disk = "Color")

x %>%
  mutate(color_disk = paste0(
    "<div style='height:15px; width:15px; border-radius:50%; background:", light, "; margin:auto;'></div>"
  )) %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = color_disk),
    fn = function(x) map(x, gt::html)
  ) %>%
  cols_label(color_disk = "Color")


x %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = light),
    fn = function(color) {
      map(color, ~ htmltools::HTML(
        paste0("<span style='color:", .x, ";'>", .x, "</span>")
      ))
    }
  )
