app_dirG <- normalizePath("c:/users/giles/anest.repo/anest.shiny/acycle/A-250629") 
setwd(app_dirG)   # does nothing in app, is here for interactive/debug
# Source all .R files in R/ into .GlobalEnv
r_dir <- file.path(app_dirG, "R")
r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
message("Sourcing: ", paste(basename(r_files), collapse = ", "))
invisible(x <- lapply(r_files, source, local = .GlobalEnv))
