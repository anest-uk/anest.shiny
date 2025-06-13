Vkss <-
  function(x) {
    x1 <- c(lab = "non-numeric", n = "numeric", nx = "numeric", rc3 = "numeric", rc6 = "non-numeric", ssei = "numeric", ssek = "numeric", sser = "numeric", sstr = "numeric", toti = "numeric")
    x %>%
      is.data.table(.)
    x2 <- x1[names(x1) %in% names(x)]
    x3 <- x[, names(x2)[x2 == "numeric"], with = F]
    x4 <- x[, names(x2)[x2 != "numeric"], with = F]
    c(
      x %>% #all x1 names present
        names(.) %>%
        setdiff(., names(x1)) %>%
        length(.) == 0,
      x %>% #no NA
        lapply(., is.na) %>%
        lapply(., any) %>%
        unlist(.) %>%
        any(.) %>%
        `!`(.),
      x %>% #non-empty
        nrow(.) %>%
        `>`(x = ., y = 0),
      x3 %>% #numeric
        lapply(., is.numeric) %>%
        unlist(.) %>%
        all(.),
      x4 %>% #non-numeric
        lapply(., is.numeric) %>%
        lapply(., `!`) %>%
        unlist(.) %>%
        all(.)
    ) %>%
      all(.)
  }

Vgeo <-
  function(x) {
    x1 <- c(lab = "non-numeric", nx = "numeric", rc9 = "non-numeric")
    x %>%
      is.data.table(.)
    x2 <- x1[names(x1) %in% names(x)]
    x3 <- x[, names(x2)[x2 == "numeric"], with = F]
    x4 <- x[, names(x2)[x2 != "numeric"], with = F]
    c(
      x %>% #all x1 names present
        names(.) %>%
        setdiff(., names(x1)) %>%
        length(.) == 0,
      x %>% #no NA
        lapply(., is.na) %>%
        lapply(., any) %>%
        unlist(.) %>%
        any(.) %>%
        `!`(.),
      x %>% #non-empty
        nrow(.) %>%
        `>`(x = ., y = 0),
      x3 %>% #numeric
        lapply(., is.numeric) %>%
        unlist(.) %>%
        all(.),
      x4 %>% #non-numeric
        lapply(., is.numeric) %>%
        lapply(., `!`) %>%
        unlist(.) %>%
        all(.)
    ) %>%
      all(.)
  }



