default_cookbook_dir <- function() {
  installed <- system.file("cookbook", package = "multisiteDGP")
  if (nzchar(installed)) {
    return(installed)
  }
  file.path(getwd(), "inst", "cookbook")
}

read_cookbook_manifest <- function(recipe_dir = default_cookbook_dir()) {
  manifest_path <- file.path(recipe_dir, "cookbook-manifest.csv")
  if (!file.exists(manifest_path)) {
    stop("Missing cookbook manifest: ", manifest_path, call. = FALSE)
  }
  utils::read.csv(manifest_path, stringsAsFactors = FALSE)
}

run_one_cookbook_recipe <- function(path, expected_marker, execute = TRUE) {
  parse(path)
  output <- character()
  marker_found <- !isTRUE(execute)
  error <- NA_character_

  if (isTRUE(execute)) {
    env <- new.env(parent = globalenv())
    output <- tryCatch(
      capture.output(sys.source(path, envir = env)),
      error = function(e) {
        error <<- conditionMessage(e)
        character()
      }
    )
    marker_found <- any(grepl(expected_marker, output, fixed = TRUE))
  }

  data.frame(
    file = basename(path),
    parsed = TRUE,
    executed = isTRUE(execute),
    marker_found = marker_found,
    error = error,
    output_lines = length(output),
    stringsAsFactors = FALSE
  )
}

run_cookbook_audit <- function(
  recipe_dir = default_cookbook_dir(),
  execute = TRUE,
  output_csv = NULL,
  verbose = TRUE
) {
  manifest <- read_cookbook_manifest(recipe_dir)
  rows <- lapply(seq_len(nrow(manifest)), function(i) {
    path <- file.path(recipe_dir, manifest$file[[i]])
    if (!file.exists(path)) {
      stop("Missing cookbook recipe: ", path, call. = FALSE)
    }
    result <- run_one_cookbook_recipe(
      path = path,
      expected_marker = manifest$expected_marker[[i]],
      execute = execute
    )
    result$id <- manifest$id[[i]]
    result$title <- manifest$title[[i]]
    result
  })
  out <- do.call(rbind, rows)
  out <- out[, c("id", "title", "file", "parsed", "executed", "marker_found", "error", "output_lines")]

  if (!is.null(output_csv)) {
    utils::write.csv(out, output_csv, row.names = FALSE)
  }

  if (isTRUE(verbose)) {
    print(out)
  }

  if (!all(out$parsed) || (isTRUE(execute) && !all(out$marker_found)) || any(!is.na(out$error))) {
    stop("Cookbook audit failed.", call. = FALSE)
  }
  out
}

if (identical(environment(), globalenv()) && !interactive()) {
  run_cookbook_audit()
}
