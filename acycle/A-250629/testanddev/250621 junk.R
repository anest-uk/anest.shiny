# C112b <-
#   function(
#     res = C111e(),
#     rsi = res$rsi[nx==1], #all nx -> slow
#     da0 = res$da0) {
#   rsi %>%
#     .[, rbind(.SD[1, .(date = da0, xdotd = 0)], .SD), nx] %>%
#     .[, .(date, xdot = c(0, xdotd[-1] * diff(date))), nx] %>%
#     .[, .(date, xdot, x = cumsum(xdot)), nx]
# }
C112b()

# C112d <- function(
#     x0 = C111e(), # res
#     x1 = C112a( # nx to plot
#       res = x0,
#       rc6tx = rc6tG
#     )[, .(nx)],
#     x2 = C112c(res = x0, nx = x1) # hexcodes from P
#     ) {
#   x3 <- # like dt_main
#     x0$rsi %>%
#     .[x1, on = c(nx = "nx")] %>%
#     C112b(rsi = .) %>% # denorm for plot
#     .[, .(nx, date, x)] %>%
#     .[, col := as.factor(nx)]
#   x4 <-
#     x2[, .(col = as.factor(nx), nx, hexcode)] %>%
#     .[col == 0, hexcode := "steelblue"]
#   x5 <-
#     x4[x3, on = c(col = "col")] %>%
#     x0$lab[., on = c(nx = "nx")] %>%
#     .[, lab := ifelse(x == max(x), lab, ""), nx]
#   ggplot(x5, aes(date, x, color = col, label = lab)) +
#     geom_line() +
#     geom_point() +
#     ggrepel::geom_label_repel() +
#     scale_color_manual(
#       values = unique(x4[,.(hexcode,nx)])[,setNames(hexcode,nx)]  
#     ) +
#     theme_minimal() +
#     theme(legend.position = "none")
# }
C112d()
