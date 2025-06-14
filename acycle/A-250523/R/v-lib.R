#validators
vresult <-
  function(
      x = f250509ed
      ) {
    vestdt(x$estdt)
    vgeo(x$geo)
    vkss(x$kss)
  }


vestdt <-
  function(
      x = f250509ed$estdt,
      xclass = c(xdotd = "numeric", days='numeric', nx = "numeric") # should have rc9 also but make-work
      ) {
    x %>%
      is.data.table(.) &
      vgen(x, xclass)
  }

vgeo <-
  function(
      x = f250509ed$geo,
      xclass = c(lab = "non-numeric", nx = "numeric") # should have rc9 also but make-work
      ) {
    x %>%
      is.data.table(.) &
      vgen(x, xclass)
  }
# vgeo()

vkss <-
  function(x=f250509ed$kfoldsse,
           xclass = c(n = "numeric", nx = "numeric", rc6 = "non-numeric", ssei = "numeric", ssek = "numeric", sser = "numeric", sstr = "numeric", toti = "numeric")) {
    x %>%
      is.data.table(.) &
      vgen(x, xclass)
  }
# vkss(f250509ed$kfoldsse)

vpva <-
  function(x=z110,
           xclass = c(nid = "numeric", m2 = "numeric", rcx = "non-numeric", pv = "numeric", ppm2 = "numeric")) {
    x %>%
      is.data.table(.) &
      vgen(x, xclass)
  }

vgen <-
  function(x = f250509ed$geo,
           xclass = c(lab = "non-numeric", nx = "numeric")) { # should have rc9 also but make-work
    x2 <- # required names found in x
      xclass[names(xclass) %in% names(x)]
    x3 <- # x which should be numeric
      x[, names(x2)[x2 == "numeric"], with = F]
    x4 <- # x which should be non-numeric
      x[, names(x2)[x2 != "numeric"], with = F]
    x5 <- # tests
      c(
        x %>% # all xclass names present
          names(.) %>%
          setdiff(names(xclass), .) %>%
          length(.) == 0,
        x %>% # no NA
          lapply(., is.na) %>%
          lapply(., any) %>%
          unlist(.) %>%
          any(.) %>%
          `!`(.),
        x %>% # non-empty
          nrow(.) %>%
          `>`(x = ., y = 0),
        x3 %>% # numeric
          lapply(., is.numeric) %>%
          unlist(.) %>%
          all(.),
        x4 %>% # non-numeric
          lapply(., is.numeric) %>%
          lapply(., `!`) %>%
          unlist(.) %>%
          all(.)
      ) %>%
      all(.)
    x5
  }

#   vkss <-
#   function(
#     x,
#     xclass=c(n = "numeric", nx = "numeric", rc6 = "non-numeric", ssei = "numeric", ssek = "numeric", sser = "numeric", sstr = "numeric", toti = "numeric")
#     #xclass=c(lab = "non-numeric", n = "numeric", nx = "numeric", rc3 = "non=numeric", rc6 = "non-numeric", ssei = "numeric", ssek = "numeric", sser = "numeric", sstr = "numeric", toti = "numeric")
#     ) {
#     x %>%
#       is.data.table(.)&
#       vgen(x,xclass)
#     # x2 <- xclass[names(xclass) %in% names(x)]
#     # x3 <- x[, names(x2)[x2 == "numeric"], with = F]
#     # x4 <- x[, names(x2)[x2 != "numeric"], with = F]
#     # c(
#     #   x %>% #all xclass names present
#     #     names(.) %>%
#     #     setdiff(., names(xclass)) %>%
#     #     length(.) == 0,
#     #   x %>% #no NA
#     #     lapply(., is.na) %>%
#     #     lapply(., any) %>%
#     #     unlist(.) %>%
#     #     any(.) %>%
#     #     `!`(.),
#     #   x %>% #non-empty
#     #     nrow(.) %>%
#     #     `>`(x = ., y = 0),
#     #   x3 %>% #numeric
#     #     lapply(., is.numeric) %>%
#     #     unlist(.) %>%
#     #     all(.),
#     #   x4 %>% #non-numeric
#     #     lapply(., is.numeric) %>%
#     #     lapply(., `!`) %>%
#     #     unlist(.) %>%
#     #     all(.)
#     # ) %>%
#     #   all(.)
#   }
# vkss(f250509ed$kfoldsse)
#
# vgeo <-
#   function(
#     x=geoaG
#     ) {
#     xclass <- c(lab = "non-numeric", nx = "numeric")  #should have rc9 also but make-work
#     x %>%
#       is.data.table(.)&
#       vgen(x,xclass)
#     # x2 <- xclass[names(xclass) %in% names(x)]
#     # x3 <- x[, names(x2)[x2 == "numeric"], with = F]
#     # x4 <- x[, names(x2)[x2 != "numeric"], with = F]
# }
# vgeo()
#
# vgen <-
#   function(
#     x=geoaG,
#     xclass=c(lab = "non-numeric", nx = "numeric")
#     ) { #should have rc9 also but make-work
#     x2 <- #required names found in x
#       xclass[names(xclass) %in% names(x)]
#     x3 <- #x which should be numeric
#       x[, names(x2)[x2 == "numeric"], with = F]
#     x4 <- #x which should be non-numeric
#       x[, names(x2)[x2 != "numeric"], with = F]
#     x5 <- #tests
#       c(
#       x %>% #all xclass names present
#         names(.) %>%
#         setdiff(names(xclass),.) %>%
#         length(.) == 0,
#       x %>% #no NA
#         lapply(., is.na) %>%
#         lapply(., any) %>%
#         unlist(.) %>%
#         any(.) %>%
#         `!`(.),
#       x %>% #non-empty
#         nrow(.) %>%
#         `>`(x = ., y = 0),
#       x3 %>% #numeric
#         lapply(., is.numeric) %>%
#         unlist(.) %>%
#         all(.),
#       x4 %>% #non-numeric
#         lapply(., is.numeric) %>%
#         lapply(., `!`) %>%
#         unlist(.) %>%
#         all(.)
#     ) %>%
#       all(.)
#   x5
# }
# vgen()
#

# }
# vgen()
