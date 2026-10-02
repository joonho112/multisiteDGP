# Fixed-seed regression values. A failure here means the simulated numbers
# changed. Preset summaries use a loose tolerance because the truncated-gamma
# site sizes come from an iterative solver. The JEBS designs draw site sizes
# directly, so they are compared more tightly.

sim_summary <- function(dat) {
  n_j <- dat$n_j
  c(
    mean_tau = mean(dat$tau_j),
    sd_tau = stats::sd(dat$tau_j),
    mean_tau_hat = mean(dat$tau_j_hat),
    mean_se = mean(dat$se_j),
    sd_n = if (all(is.na(n_j))) NA_real_ else stats::sd(n_j),
    rank_corr = suppressWarnings(stats::cor(dat$tau_j, dat$se_j, method = "spearman"))
  )
}

expect_summary <- function(dat, expected, tolerance, label) {
  actual <- sim_summary(dat)
  for (i in seq_along(expected)) {
    expect_equal(
      actual[[i]], expected[[i]],
      tolerance = tolerance,
      info = sprintf("%s, %s", label, names(actual)[[i]])
    )
  }
}

moments <- function(dat) {
  c(
    mean(dat$tau_j), stats::sd(dat$tau_j),
    mean(dat$tau_j_hat), stats::sd(dat$tau_j_hat),
    mean(dat$se2_j)
  )
}

test_that("presets reproduce their seed-1 summaries", {
  expected <- list(
    preset_education_small = c(
      0.00502241399801, 0.04156969652123, -0.06243483500371, 0.35568825764598, 18.7649369075107, 0.26058340431022
    ),
    preset_education_modest = c(
      0.0200896559921, 0.1662787860849, -0.0388765262456, 0.3146677397794, 23.3066601674986, 0.2538798288697
    ),
    preset_education_substantial = c(
      0.0326662100744, 0.269459807898, 0.0437202523266, 0.2344474152396, 35.0626798056212, 0.0955167462368
    ),
    preset_jebs_paper = c(
      0.0246464917649, 0.2073473249151, 0.022418585059, 0.3582455887811, 19.9550106233896, -0.193116053858
    ),
    preset_jebs_strict = c(
      0.002634301350229, 0.153212131448378, -0.000790780554044, 0.254819645957787, 37.6207483587276, -0.105493108364535
    ),
    preset_walters_2024 = c(
      0.0133640414483, 0.1678033122386, 0.0108365565756, 0.1048959212802, 67.7623190575557, 0.1450821981195
    ),
    preset_twin_towers = c(
      -0.0843691910568, 2.0203008928048, -0.073231161905, 0.2, 0, NA
    )
  )
  for (name in names(expected)) {
    design <- do.call(name, list())
    dat <- sim_multisite(design, seed = 1L)
    expect_identical(nrow(dat), design$J, info = name)
    expect_summary(dat, expected[[name]], tolerance = 1e-6, label = name)
  }

  expected_direct <- list(
    preset_meta_modest = c(
      0.0200896559921, 0.1662787860849, 0.046013647011, 0.3060498754704, NA, -0.0934933973589
    ),
    preset_small_area_estimation = c(
      0.0164916340388, 0.1848241653826, 0.0870648163224, 0.4053974619911, NA, 0.1114571746385
    )
  )
  for (name in names(expected_direct)) {
    design <- do.call(name, list())
    dat <- sim_meta(design, seed = 1L)
    expect_identical(nrow(dat), design$J, info = name)
    expect_summary(dat, expected_direct[[name]], tolerance = 1e-6, label = name)
  }
})

test_that("preset_jebs_strict reproduces the JEBS appendix draws", {
  expected <- list(
    list(
      seed = 42L, sum_n = 7263L, n_head = c(35L, 143L, 37L, 120L, 18L),
      moments = c(0.01119308716689, 0.16999140787187, 0.00836756180938, 0.31923956892019, 0.07178314353686)
    ),
    list(
      seed = 1L, sum_n = 7845L, n_head = c(109L, 86L, 46L, 110L, 110L),
      moments = c(0.002634301350229, 0.153212131448378, -0.000790780554044, 0.301090833984576, 0.073603709370388)
    ),
    list(
      seed = 2024L, sum_n = 7654L, n_head = c(56L, 114L, 146L, 49L, 47L),
      moments = c(-0.01232733424599, 0.14008489580989, 0.00179934499965, 0.30306437909533, 0.07126056949744)
    ),
    list(
      seed = 12345L, sum_n = 7737L, n_head = c(32L, 84L, 44L, 71L, 96L),
      moments = c(0.0263867719543, 0.1552696566227, 0.0329442846227, 0.2747212661535, 0.0690807511832)
    )
  )
  for (case in expected) {
    dat <- sim_multisite(preset_jebs_strict(), seed = case$seed)
    label <- sprintf("seed = %d", case$seed)
    expect_identical(names(dat), c("site_index", "z_j", "tau_j", "tau_j_hat", "se_j", "se2_j", "n_j"))
    expect_identical(dat$site_index, seq_len(100L), info = label)
    expect_identical(sum(dat$n_j), case$sum_n, info = label)
    expect_identical(dat$n_j[1:5], case$n_head, info = label)
    expect_equal(moments(dat), case$moments, tolerance = 1e-8, info = label)
  }
})

test_that("the censored site-size generator respects the lower limit of the JEBS design", {
  dat <- sim_multisite(preset_jebs_strict(J = 300L, nj_mean = 10, cv = 0.75), seed = 42L)

  expect_identical(min(dat$n_j), 5L)
  expect_identical(sum(dat$n_j == 5L), 90L)
  expect_identical(sum(dat$n_j), 3218L)
  expect_equal(
    moments(dat),
    c(0.00570231710011, 0.15596213889358, 0.02049102122396, 0.77208065556591, 0.50165800488471),
    tolerance = 1e-8
  )
})

test_that("presets print their size and the simulated data print the number of sites", {
  presets <- c(
    "preset_education_small", "preset_education_modest", "preset_education_substantial",
    "preset_jebs_paper", "preset_jebs_strict", "preset_walters_2024", "preset_twin_towers",
    "preset_meta_modest", "preset_small_area_estimation"
  )
  for (name in presets) {
    design <- do.call(name, list())
    expect_output(print(design), sprintf("J = %d sites", design$J), fixed = TRUE)
    expect_output(print(design), paste("true_dist:", design$true_dist), fixed = TRUE)
  }

  dat <- sim_multisite(preset_education_modest(), seed = 1L)
  expect_output(print(dat), "50 sites, standard errors from site sizes, seed 1", fixed = TRUE)
  expect_output(summary(dat), "sigma_tau", fixed = TRUE)

  meta <- sim_meta(preset_meta_modest(), seed = 1L)
  expect_output(print(meta), "50 sites, standard errors set directly, seed 1", fixed = TRUE)

  walters <- sim_multisite(preset_walters_2024(), seed = 1L)
  expect_output(summary(walters), "multisiteDGP simulation (preset_walters_2024): 46 sites", fixed = TRUE)
})
