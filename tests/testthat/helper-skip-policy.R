skip_if_not_slow <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("MULTISITEDGP_RUN_SLOW"), "true"),
    "Set MULTISITEDGP_RUN_SLOW=true to run slow multisiteDGP tests."
  )
}

skip_if_not_validation <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("MULTISITEDGP_RUN_VALIDATION"), "true"),
    "Set MULTISITEDGP_RUN_VALIDATION=true to run validation experiments."
  )
}

skip_if_not_property <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("MULTISITEDGP_RUN_PROPERTY"), "true"),
    "Set MULTISITEDGP_RUN_PROPERTY=true to run multisiteDGP property tests."
  )
}

skip_if_not_linux_strict_hash <- function() {
  platform <- tolower(R.version$platform)
  testthat::skip_if_not(
    identical(tolower(Sys.info()[["sysname"]]), "linux") &&
      grepl("x86_64|amd64", platform),
    "Strict canonical_hash equality is Linux x86_64/amd64 baseline only."
  )
}

skip_if_api_missing <- function(name) {
  testthat::skip_if_not(
    exists(name, envir = asNamespace("multisiteDGP"), mode = "function", inherits = FALSE),
    sprintf("multisiteDGP::%s() has not been implemented yet.", name)
  )
}
