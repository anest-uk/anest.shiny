#source: chatgpt
#250526

# Step 1: Define schema input — the source of truth
# Here’s a minimal R-native definition of your schemas, which we can later export to YAML/JSON or use to generate code.
# You can update this by hand or generate parts programmatically if needed.

schema_manifest <- list(
  geo = list(
    columns = list(
      nx = "integer",
      rc9 = "character",
      lab = "character"
    ),
    key = c("nx", "rc9"),
    foreign_keys = list(
      list(columns = "nx", ref = "forall", ref_columns = "nx"),
      list(columns = c("nx", "date"), ref = "estdt", via = c("dfn")),
      list(columns = c("nx", "rc9"), ref = "sse", via = c("dfn"))
    ),
    constraints = list(
      "unique(rc9) ∈ GLOBAL_RC9",
      "unique(rc9) has shared prefix",
      "no duplicated (nx, rc9)"
    )
  ),
  estdt = list(
    columns = list(
      ii = "integer",
      nx = "integer",
      date = "Date",
      xdotd = "numeric",
      xdot = "numeric",
      x = "numeric",
      days = "integer"
    ),
    key = c("nx", "date"),
    constraints = list(
      "days >= 0"
    )
  ),
  dfn = list(
    columns = list(
      date = "Date",
      ii = "integer"
    ),
    key = c("date")
  ),
  sse = list(
    columns = list(
      rc9 = "character",
      ssei = "numeric",
      toti = "numeric",
      ssek = "numeric",
      sser = "numeric",
      ssetr = "numeric",
      n = "integer",
      nx = "integer"
    ),
    key = c("nx", "rc9")
  ),
  forall = list(
    columns = list(
      insam = "numeric",
      outsam = "numeric",
      raw = "numeric",
      tot = "numeric",
      rsq = "numeric",
      nx = "integer"
    ),
    key = "nx"
  )
)
