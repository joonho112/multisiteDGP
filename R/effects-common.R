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
    .abort(
      sprintf("The standardized site effects `z_j` must be %s finite numbers.", J),
      sprintf("They are %s.", .describe_value(z_j))
    )
  }
  as.numeric(z_j)
}

.build_covariate_effect <- function(J, formula = NULL, beta = NULL, data = NULL, sigma_tau = NULL) {
  if (is.null(formula)) {
    if (!is.null(beta) || !is.null(data)) {
      .abort(
        "`formula` is required when `beta` or `data` is supplied.",
        "Covariates need `formula`, `beta`, and `data` together."
      )
    }
    return(list(
      effect = rep(0, J),
      data = NULL,
      model_matrix = NULL,
      beta = NULL,
      decomposition = .covariate_decomposition(rep(0, J), sigma_tau)
    ))
  }

  .validate_formula(formula)
  covariate_data <- .validate_covariate_data(data, J)
  model_matrix <- .build_model_matrix(formula, covariate_data)
  beta_full <- .resolve_covariate_beta(beta, model_matrix)
  effect <- as.numeric(model_matrix %*% beta_full)
  if (anyNA(effect) || any(!is.finite(effect))) {
    .abort(
      "The covariate term of the site effects must be finite for every site.",
      fix = "Check `data` for infinite values."
    )
  }

  list(
    effect = effect,
    data = covariate_data,
    model_matrix = model_matrix,
    beta = beta_full,
    decomposition = .covariate_decomposition(effect, sigma_tau = sigma_tau)
  )
}

.validate_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    .abort(
      "`formula` must be a one-sided formula such as `~ x`.",
      .you_passed(formula)
    )
  }
  invisible(formula)
}

.validate_covariate_data <- function(data, J) {
  if (is.null(data) || !inherits(data, "data.frame")) {
    .abort(
      "`data` must be a data frame with one row per site when `formula` is supplied.",
      .you_passed(data)
    )
  }
  if (nrow(data) != J) {
    .abort(
      "`data` must have one row per site.",
      sprintf("`nrow(data)` is %s and `J` is %s.", nrow(data), J)
    )
  }
  data <- tibble::as_tibble(data)
  reserved <- intersect(names(data), .reserved_column_names())
  if (length(reserved) > 0L) {
    .abort(
      "`data` has column names that the simulated data use for their own columns.",
      sprintf("Rename: %s.", paste(reserved, collapse = ", "))
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
      .abort(
        "`formula` could not be evaluated in `data`.",
        conditionMessage(e)
      )
    }
  )
}

.resolve_covariate_beta <- function(beta, model_matrix) {
  if (is.null(beta)) {
    .abort(
      "`beta` is required when `formula` is supplied.",
      "It holds one coefficient for each column of the model matrix."
    )
  }
  if (!is.numeric(beta) || anyNA(beta) || any(!is.finite(beta))) {
    .abort(
      "`beta` must be a numeric vector of finite values.",
      if (is.numeric(beta)) "It has missing or infinite values." else .you_passed(beta)
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
    .abort(
      "The names of `beta` must be columns of the model matrix, each used once.",
      sprintf("The columns are: %s.", paste(column_names, collapse = ", "))
    )
  }
  out <- numeric(length(column_names))
  names(out) <- column_names
  matched <- match(beta_names, column_names_normalized)
  out[matched] <- as.numeric(beta)

  # A named `beta` may leave out the intercept, which then stays at zero, as
  # it does for an unnamed `beta`.
  non_intercept <- column_names_normalized != "(Intercept)"
  missing_non_intercept <- non_intercept & !(seq_along(column_names) %in% matched)
  if (any(missing_non_intercept)) {
    .abort(
      "`beta` must have a coefficient for every column of the model matrix except the intercept.",
      sprintf("Missing: %s.", paste(column_names[missing_non_intercept], collapse = ", "))
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
    .abort(
      "`beta` must have one coefficient for each column of the model matrix, with or without the intercept.",
      sprintf(
        "`beta` has length %s. The columns are: %s.",
        length(beta),
        paste(column_names, collapse = ", ")
      )
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

.reserved_column_names <- function() {
  c("site_index", "z_j", "tau_j", "latent_component", "tau_j_hat", "se_j", "se2_j", "n_j")
}

.covariate_decomposition <- function(covariate_effect, sigma_tau = NULL) {
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
