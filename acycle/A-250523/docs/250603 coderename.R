library(stringr)

# --- CONFIGURATION ---
base_dir <- "C:/temp/R/"
dir(base_dir)
dry_run <- TRUE     # Set to FALSE to apply changes
make_backup <- TRUE # Set to TRUE to save original files as .bak
code <- '111'

x1 <- c(paste0("x", code, "D"),paste0("f", code, "D"),paste0("x", code, "G"))
x2 <- c(paste0("R", code),paste0("D", code),paste0("G", code))
setNames(x2,x1)
f1 <- function(code) {
x1 <- c(paste0("x", code, "D"),paste0("f", code, "D"),paste0("x", code, "G"))
x2 <- c(paste0("R", code),paste0("D", code),paste0("G", code))
setNames(x2,x1)
}
f1('111')

codes <- c("111", "112", "121", "122", "131", "132", "211","211cc","411","412","421","422","431","432")
subs <- unlist(lapply(codes, f1))

# --- FUNCTION TO APPLY SUBSTITUTIONS ---
replace_patterns <- function(
    path, 
    subs, 
    dry_run = TRUE, 
    make_backup = TRUE
    ) {
  original <- readLines(path, warn = FALSE)
  modified <- original

  for (pattern in names(subs)) {
    modified <- gsub(pattern, subs[[pattern]], modified, fixed = TRUE)
  }

  if (!identical(original, modified)) {
    cat("Modified:", path, "\n")
    if (dry_run) {
      diff_lines <- which(original != modified)
      for (i in diff_lines) {
        cat(sprintf("Line %d:\n  - %s\n  + %s\n", i, original[i], modified[i]))
      }
    } else {
      if (make_backup) {
        file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
      }
      writeLines(modified, path)
    }
  }
}


# --- MAIN ---
r_files <- "C:/temp/R/server_constituents.R"  #
r_files <- list.files(base_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
invisible(lapply(r_files, replace_patterns, subs = subs, dry_run = F, make_backup = make_backup))
