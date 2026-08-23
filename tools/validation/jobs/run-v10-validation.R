#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v10-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V10"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 911001L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = FALSE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
plot_dir <- file.path(paths$generated_dir, "plots", run_id)
started_at <- Sys.time()
parameters <- list(plot_contract = "eight-presets-by-three-plots")
run_state <- validation_prepare_run(
  paths, run_id, experiment_id, mode, seed_root, parameters, script_path,
  result_path, summary_path, resume = resume, overwrite = overwrite
)

if (identical(run_state$action, "reuse")) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  status <- if (nrow(summary) == 1L && isTRUE(summary$acceptance_pass[[1L]])) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_reuse(paths, run_state, started_at, ended_at, "Compatible V10 output reused.")
  message("Resumed existing V10 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V10 validation failed in resumed output.")
  quit(status = 0)
}

dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

preset_names <- c(
  "preset_education_small",
  "preset_education_modest",
  "preset_education_substantial",
  "preset_jebs_paper",
  "preset_walters_2024",
  "preset_twin_towers",
  "preset_meta_modest",
  "preset_small_area_estimation"
)
strict_anchor_skip <- data.frame(
  result_schema_version = "phase9-validation-v1",
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  preset = "preset_jebs_strict",
  reason = "explicit_skip_bit_identical_anchor_to_preserve_8x3_ch19_v10_contract",
  stringsAsFactors = FALSE
)
strict_skip_path <- validation_write_csv(
  strict_anchor_skip,
  file.path(paths$generated_dir, paste0(run_id, "-strict-anchor-skip.csv"))
)

plot_specs <- list(
  effects = function(dat) multisiteDGP::plot_effects(dat),
  funnel = function(dat) multisiteDGP::plot_funnel(dat),
  dependence = function(dat) multisiteDGP::plot_dependence(dat)
)
seeds <- validation_seed_stream(length(preset_names), seed_root)

simulate_preset <- function(preset_name, seed) {
  design <- get(preset_name, envir = asNamespace("multisiteDGP"))()
  if (identical(design$paradigm, "direct")) {
    return(multisiteDGP::sim_meta(design, seed = seed))
  }
  multisiteDGP::sim_multisite(design, seed = seed)
}

capture_warnings <- function(expr) {
  warning_messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warning_messages <<- c(warning_messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warning_messages = unique(warning_messages))
}

numeric_range <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 1L) {
    return(NA_real_)
  }
  diff(range(x))
}

build_axis_summary <- function(built) {
  layer_data <- built$data
  nonempty <- vapply(layer_data, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))
  x_ranges <- unlist(lapply(layer_data, function(x) {
    cols <- intersect(c("x", "xmin", "xmax", "xend"), names(x))
    unlist(lapply(cols, function(col) numeric_range(x[[col]])), use.names = FALSE)
  }), use.names = FALSE)
  y_ranges <- unlist(lapply(layer_data, function(x) {
    cols <- intersect(c("y", "ymin", "ymax", "yend"), names(x))
    unlist(lapply(cols, function(col) numeric_range(x[[col]])), use.names = FALSE)
  }), use.names = FALSE)
  x_range <- suppressWarnings(max(x_ranges, na.rm = TRUE))
  y_range <- suppressWarnings(max(y_ranges, na.rm = TRUE))
  if (!is.finite(x_range)) x_range <- NA_real_
  if (!is.finite(y_range)) y_range <- NA_real_
  list(
    layer_count = length(layer_data),
    nonempty_layer_count = sum(nonempty),
    x_range = x_range,
    y_range = y_range,
    nondegenerate = isTRUE((is.finite(x_range) && x_range > 0) || (is.finite(y_range) && y_range > 0))
  )
}

plot_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"

run_plot <- function(preset_name, preset_id, seed, plot_name, plot_fun) {
  tryCatch({
    dat <- simulate_preset(preset_name, seed = seed)
    design <- attr(dat, "design", exact = TRUE)
    diagnostics <- attr(dat, "diagnostics", exact = TRUE)
    provenance <- attr(dat, "provenance", exact = TRUE)
    plot_capture <- capture_warnings(plot_fun(dat))
    p <- plot_capture$value
    build_capture <- capture_warnings(ggplot2::ggplot_build(p))
    axis_summary <- build_axis_summary(build_capture$value)
    file_name <- sprintf("%02d-%s-%s.png", preset_id, preset_name, plot_name)
    file_path <- file.path(plot_dir, file_name)
    save_capture <- capture_warnings(ggplot2::ggsave(
      filename = file_path,
      plot = p,
      width = 8,
      height = 5,
      units = "in",
      dpi = 150,
      device = plot_device
    ))
    labels <- p$labels
    subtitle <- if (is.null(labels$subtitle)) "" else labels$subtitle
    caption <- if (is.null(labels$caption)) "" else labels$caption
    subtitle_has_diagnostics <- grepl("rho_S", subtitle, fixed = TRUE) &&
      (grepl("I_hat", subtitle, fixed = TRUE) || identical(plot_name, "dependence")) &&
      (grepl("R_hat", subtitle, fixed = TRUE) || identical(plot_name, "dependence"))
    caption_has_provenance <- grepl("canonical_hash=", caption, fixed = TRUE)
    render_pass <- file.exists(file_path) && file.info(file_path)$size > 5000
    row_pass <- inherits(p, "ggplot") &&
      axis_summary$nonempty_layer_count > 0L &&
      axis_summary$nondegenerate &&
      isTRUE(render_pass) &&
      isTRUE(subtitle_has_diagnostics) &&
      isTRUE(caption_has_provenance)

    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      preset_id = preset_id,
      preset = preset_name,
      plot_name = plot_name,
      seed = seed,
      status = "completed",
      paradigm = design$paradigm,
      J = nrow(dat),
      true_dist = design$true_dist,
      engine = design$engine,
      returns_ggplot = inherits(p, "ggplot"),
      build_pass = TRUE,
      render_pass = render_pass,
      file_path = .relative_to_root(file_path, paths$package_root),
      file_size = if (file.exists(file_path)) file.info(file_path)$size else NA_real_,
      file_sha256 = validation_file_hash(file_path),
      layer_count = axis_summary$layer_count,
      nonempty_layer_count = axis_summary$nonempty_layer_count,
      x_range = axis_summary$x_range,
      y_range = axis_summary$y_range,
      nondegenerate_axes = axis_summary$nondegenerate,
      subtitle = subtitle,
      caption = caption,
      subtitle_has_diagnostics = subtitle_has_diagnostics,
      caption_has_provenance = caption_has_provenance,
      diagnostics_I = diagnostics$I_hat,
      diagnostics_R = diagnostics$R_hat,
      diagnostics_rho_S = diagnostics$rho_S_residual,
      canonical_hash = provenance$canonical_hash,
      warning_count = length(c(plot_capture$warning_messages, build_capture$warning_messages, save_capture$warning_messages)),
      warning_message = paste(unique(c(plot_capture$warning_messages, build_capture$warning_messages, save_capture$warning_messages)), collapse = " | "),
      acceptance_row_pass = row_pass,
      error_class = NA_character_,
      error_message = NA_character_,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      preset_id = preset_id,
      preset = preset_name,
      plot_name = plot_name,
      seed = seed,
      status = "failed",
      paradigm = NA_character_,
      J = NA_integer_,
      true_dist = NA_character_,
      engine = NA_character_,
      returns_ggplot = FALSE,
      build_pass = FALSE,
      render_pass = FALSE,
      file_path = NA_character_,
      file_size = NA_real_,
      file_sha256 = NA_character_,
      layer_count = NA_integer_,
      nonempty_layer_count = NA_integer_,
      x_range = NA_real_,
      y_range = NA_real_,
      nondegenerate_axes = FALSE,
      subtitle = NA_character_,
      caption = NA_character_,
      subtitle_has_diagnostics = FALSE,
      caption_has_provenance = FALSE,
      diagnostics_I = NA_real_,
      diagnostics_R = NA_real_,
      diagnostics_rho_S = NA_real_,
      canonical_hash = NA_character_,
      warning_count = NA_integer_,
      warning_message = NA_character_,
      acceptance_row_pass = FALSE,
      error_class = paste(class(e), collapse = "/"),
      error_message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

rows <- list()
row_id <- 1L
for (preset_id in seq_along(preset_names)) {
  preset_name <- preset_names[[preset_id]]
  message("V10 preset ", preset_id, " / ", length(preset_names), ": ", preset_name)
  for (plot_name in names(plot_specs)) {
    rows[[row_id]] <- run_plot(
      preset_name = preset_name,
      preset_id = preset_id,
      seed = seeds[[preset_id]],
      plot_name = plot_name,
      plot_fun = plot_specs[[plot_name]]
    )
    row_id <- row_id + 1L
  }
}
results <- do.call(rbind, rows)
result_path <- validation_write_csv(results, result_path)

completed <- results[results$status == "completed", , drop = FALSE]
rendered_hashes <- completed$file_sha256[completed$render_pass]
duplicate_render_hash_count <- length(rendered_hashes) - length(unique(rendered_hashes))
summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  presets = length(preset_names),
  plot_functions = length(plot_specs),
  rows = nrow(results),
  expected_rows = length(preset_names) * length(plot_specs),
  completed = nrow(completed),
  failed = sum(results$status == "failed"),
  row_pass_rate = mean(results$acceptance_row_pass),
  render_pass_rate = mean(results$render_pass),
  build_pass_rate = mean(results$build_pass),
  nondegenerate_axis_rate = mean(results$nondegenerate_axes),
  subtitle_diagnostics_rate = mean(results$subtitle_has_diagnostics),
  caption_provenance_rate = mean(results$caption_has_provenance),
  duplicate_render_hash_count = duplicate_render_hash_count,
  strict_anchor_skip_path = .relative_to_root(strict_skip_path, paths$package_root),
  manual_review_status = "not_reviewed",
  acceptance_pass = nrow(results) == length(preset_names) * length(plot_specs) &&
    all(results$acceptance_row_pass) &&
    identical(duplicate_render_hash_count, 0L),
  acceptance_note = "Automated V10 gates cover render/build/range/caption integrity; visual aesthetics remain manual-review evidence.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, nrow(results), script_path, result_path, summary_path, "V10 automated visual diagnostic render evidence.", parameters = parameters)
print(summary)
message("V10 status: ", status)
validation_maybe_stop_for_blocker(status, "V10 validation failed acceptance criteria.")
