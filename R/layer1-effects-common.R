# nolint start: object_name_linter, object_usage_linter
.new_effects_frame <- function(
  z_j,
  J,
  tau = 0,
  sigma_tau = 0.20,
  formula = NULL,
  beta = NULL,
  data = NULL,
  preflight = NULL
) {
  if (is.null(preflight)) {
    preflight <- .preflight_effects_frame_inputs(
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      formula = formula,
      beta = beta,
      data = data
    )
  }
  J <- preflight$J
  tau <- preflight$tau
  sigma_tau <- preflight$sigma_tau
  z_j <- .validate_z_j(z_j, J)
  covariates <- preflight$covariates

  base <- tibble::tibble(
    site_index = seq_len(J),
    z_j = z_j,
    tau_j = tau + covariates$effect + sigma_tau * z_j
  )

  if (is.null(covariates$data)) {
    return(base)
  }
  tibble::as_tibble(c(as.list(base), as.list(covariates$data)))
}

.preflight_effects_frame_inputs <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  J <- .validate_j(J)
  tau <- .validate_scalar_number(tau, "tau")
  sigma_tau <- .validate_sigma_tau(sigma_tau)
  covariates <- .build_covariate_effect(
    J = J,
    formula = formula,
    beta = beta,
    data = data,
    sigma_tau = sigma_tau
  )
  list(J = J, tau = tau, sigma_tau = sigma_tau, covariates = covariates)
}

.validate_z_j <- function(z_j, J) {
  if (!is.numeric(z_j) || length(z_j) != J || anyNA(z_j) || any(!is.finite(z_j))) {
    .abort_arg(
      "`z_j` must be a finite numeric vector of length `J`.",
      "`z_j` is the standardized residual effect column carried through the pipeline.",
      "Use one finite standardized residual draw per site."
    )
  }
  as.numeric(z_j)
}

.build_covariate_effect <- function(J, formula = NULL, beta = NULL, data = NULL, sigma_tau = NULL) {
  if (is.null(formula)) {
    if (!is.null(beta) || !is.null(data)) {
      .abort_arg(
        "`formula` is required when `beta` or `data` is supplied.",
        "Covariate effects are only defined through the formula/data/beta contract.",
      "Use `formula = ~ x, beta = ... , data = ...` or remove `beta` and `data`."
      )
    }
    return(list(
      effect = rep(0, J),
      data = NULL,
      model_matrix = NULL,
      beta = NULL,
      decomposition = .l1_decomposition(rep(0, J), sigma_tau)
    ))
  }

  .validate_formula(formula)
  covariate_data <- .validate_covariate_data(data, J)
  model_matrix <- .build_model_matrix(formula, covariate_data)
  beta_full <- .resolve_covariate_beta(beta, model_matrix)
  effect <- as.numeric(model_matrix %*% beta_full)
  if (anyNA(effect) || any(!is.finite(effect))) {
    .abort_arg(
      "Covariate linear predictor contains non-finite values.",
      "`X %*% beta` must be finite for every site.",
      "Use finite covariate values and finite `beta` coefficients."
    )
  }

  list(
    effect = effect,
    data = covariate_data,
    model_matrix = model_matrix,
    beta = beta_full,
    decomposition = .l1_decomposition(effect, sigma_tau = sigma_tau)
  )
}

.validate_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    .abort_arg(
      "`formula` must be an R formula.",
      "The L1 covariate path constructs a model matrix from `formula` and `data`.",
      "Use a one-sided formula such as `formula = ~ x`."
    )
  }
  invisible(formula)
}

.validate_covariate_data <- function(data, J) {
  if (is.null(data) || !inherits(data, "data.frame")) {
    .abort_arg(
      "`data` must be a data frame when `formula` is supplied.",
      "Covariate rows must align one-to-one with sites.",
      "Pass `data` with exactly `J` rows."
    )
  }
  if (nrow(data) != J) {
    .abort_arg(
      "`data` must have exactly `J` rows.",
      sprintf("Got %s rows in `data` for J = %s.", nrow(data), J),
      "Use one covariate row per site."
    )
  }
  data <- tibble::as_tibble(data)
  reserved <- intersect(names(data), .l1_reserved_columns())
  if (length(reserved) > 0L) {
    .abort_arg(
      "Covariate data uses reserved L1 column names.",
      sprintf("Reserved names present: %s.", paste(reserved, collapse = ", ")),
      "Use different covariate column names before simulation."
    )
  }
  data
}

.build_model_matrix <- function(formula, data) {
  tryCatch(
    {
      frame <- stats::model.frame(formula, data = data, na.action = stats::na.fail)
      stats::model.matrix(formula, data = frame)
    },
    error = function(e) {
      .abort_arg(
        "Could not construct a covariate model matrix.",
        conditionMessage(e),
        "Use a formula whose variables are present in `data` and contain no missing values."
      )
    }
  )
}

.resolve_covariate_beta <- function(beta, model_matrix) {
  if (is.null(beta)) {
    .abort_arg(
      "`beta` is required when `formula` is supplied.",
      "multisiteDGP does not silently assume zero covariate effects.",
      "Use `beta = 0` for a covariate-off equivalence check."
    )
  }
  if (!is.numeric(beta) || anyNA(beta) || any(!is.finite(beta))) {
    .abort_arg(
      "`beta` must be a finite numeric vector.",
      "`beta` is multiplied by the covariate model matrix.",
      "Use one finite coefficient per model-matrix column, or per non-intercept column."
    )
  }

  column_names <- colnames(model_matrix)
  if (!is.null(names(beta)) && all(nzchar(names(beta)))) {
    return(.resolve_named_beta(beta, column_names))
  }
  .resolve_unnamed_beta(beta, column_names)
}

.resolve_named_beta <- function(beta, column_names) {
  beta_names <- .normalize_beta_names(names(beta))
  column_names_normalized <- .normalize_beta_names(column_names)
  if (anyDuplicated(beta_names) || !all(beta_names %in% column_names_normalized)) {
    .abort_arg(
      "`beta` names must match model-matrix columns.",
      sprintf("Model columns are: %s.", paste(column_names, collapse = ", ")),
      "Use coefficient names from `colnames(model.matrix(formula, data))`."
    )
  }
  out <- numeric(length(column_names))
  names(out) <- column_names
  matched <- match(beta_names, column_names_normalized)
  out[matched] <- as.numeric(beta)

  non_intercept <- column_names_normalized != "(intercept)"
  missing_non_intercept <- non_intercept & !(seq_along(column_names) %in% matched)
  if (any(missing_non_intercept)) {
    .abort_arg(
      "`beta` is missing non-intercept coefficients.",
      sprintf("Missing coefficients: %s.", paste(column_names[missing_non_intercept], collapse = ", ")),
      "Pass all non-intercept coefficients or use an unnamed vector in model-matrix order."
    )
  }
  out
}

.resolve_unnamed_beta <- function(beta, column_names) {
  beta <- as.numeric(beta)
  has_intercept <- "(Intercept)" %in% column_names
  if (length(beta) == length(column_names)) {
    out <- beta
  } else if (has_intercept && length(beta) == length(column_names) - 1L) {
    out <- c(0, beta)
  } else {
    .abort_arg(
      "`beta` length does not match the covariate model matrix.",
      sprintf(
        "`beta` has length %s; model matrix has %s columns.",
        length(beta),
        length(column_names)
      ),
      "Use all model-matrix coefficients, or omit the intercept coefficient to default it to zero."
    )
  }
  names(out) <- column_names
  out
}

.normalize_beta_names <- function(x) {
  normalized <- trimws(x)
  normalized[tolower(normalized) == "intercept"] <- "(Intercept)"
  normalized
}

.l1_reserved_columns <- function() {
  c("site_index", "z_j", "tau_j", "latent_component", "tau_j_hat", "se_j", "se2_j", "n_j")
}

.l1_decomposition <- function(covariate_effect, sigma_tau = NULL) {
  covariate_sd <- if (length(covariate_effect) <= 1L) {
    0
  } else {
    stats::sd(covariate_effect)
  }
  list(
    covariate_sd = covariate_sd,
    sigma_tau_resid = sigma_tau,
    sigma_tau_marg = if (is.null(sigma_tau)) NULL else sqrt(sigma_tau^2 + covariate_sd^2),
    covariates_active = !isTRUE(all.equal(covariate_effect, rep(0, length(covariate_effect))))
  )
}
# nolint end
