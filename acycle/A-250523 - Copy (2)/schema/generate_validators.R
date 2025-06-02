#source('./acycle/A-250523/R/generate_validators.R') #source this file
#generate_validators(schema_manifest,'./acycle/A-250523/schema')

#** n.b. this will overwrite any handwritten validators! **

generate_validators <- function(manifest, output_dir = "R/schema") {
  dir.create(output_dir, showWarnings = FALSE)
  
  for (table in names(manifest)) {
    spec <- manifest[[table]]
    fname <- file.path(output_dir, paste0("validate_", table, ".R"))
    
    lines <- c(
      sprintf("# -------------------------------------------------------------------"),
      sprintf("# @datatype %s", table),
      "# @columns"
    )
    
    for (col in names(spec$columns)) {
      lines <- c(lines, sprintf("#   %-6s : %s", col, spec$columns[[col]]))
    }

    lines <- c(lines, sprintf("# @key {%s}", paste(spec$key, collapse = ", ")))

    if (!is.null(spec$foreign_keys)) {
      lines <- c(lines, "# @foreign_keys:")
      for (fk in spec$foreign_keys) {
        via <- if (!is.null(fk$via)) paste("via", paste(fk$via, collapse = ", ")) else ""
        lines <- c(lines, sprintf("#   - {%s} → %s [%s]",
                                  paste(fk$columns, collapse = ", "),
                                  fk$ref,
                                  via))
      }
    }

    if (!is.null(spec$constraints)) {
      lines <- c(lines, "# @constraints:")
      for (con in spec$constraints) {
        lines <- c(lines, sprintf("#   - %s", con))
      }
    }

    lines <- c(lines,
      "# -------------------------------------------------------------------",
      sprintf("validate_%s <- function(dt) {", table),
      "  stopifnot(data.table::is.data.table(dt))",
      "  # TODO: add checks here",
      "}"
    )

    writeLines(lines, fname)
  }
}
