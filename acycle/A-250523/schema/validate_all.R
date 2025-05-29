#source: chatgpt
#250526

# ✅ Usage
# Load your data objects (geo, estdt, etc.), then call:
# 
# r
# Copy
# Edit
# source("R/validate_all.R")
# validate_all()
# It will show something like:
# 
# less
# Copy
# Edit
# ⏳ Running schema validation...
# • geo        ✅ OK
# • estdt      ✅ OK
# • dfn        ❌ missing object
# • sse        ❌ FAILED: not a data.table
# • forall     ✅ OK


validate_all <- function(schema_dir = "R/schema", env = .GlobalEnv) {
  message("⏳ Running schema validation...")

  # 1. Source all validate_*.R files
  schema_files <- list.files(schema_dir, pattern = "^validate_.*\\.R$", full.names = TRUE)
  lapply(schema_files, source)

  # 2. Find all validate_*() functions in the environment
  validators <- grep("^validate_", ls(env), value = TRUE)
  if (length(validators) == 0) {
    stop("No validate_*() functions found.")
  }

  # 3. Loop through each validator
  results <- lapply(validators, function(vfun_name) {
    object_name <- sub("^validate_", "", vfun_name)
    vfun <- get(vfun_name, envir = env, inherits = TRUE)
    data_obj_exists <- exists(object_name, envir = env, inherits = TRUE)

    if (!data_obj_exists) {
      return(list(name = object_name, status = "❌ missing object"))
    }

    obj <- get(object_name, envir = env, inherits = TRUE)

    result <- tryCatch({
      vfun(obj)
      list(name = object_name, status = "✅ OK")
    }, error = function(e) {
      list(name = object_name, status = paste("❌ FAILED:", conditionMessage(e)))
    })

    return(result)
  })

  # 4. Print results
  for (res in results) {
    cat(sprintf("• %-10s %s\n", res$name, res$status))
  }

  invisible(results)
}
