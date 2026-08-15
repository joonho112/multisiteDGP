
# ── caterpillar 축 라벨 솎아내기 (D-036) ────────────────────────────────

test_that("the caterpillar y axis thins its labels on large designs", {
  # y 축은 사이트당 이산 수준 하나라 라벨이 전부 그려진다. J = 50 에서 이미
  # 겹쳐 읽을 수 없었고, preset 이 권하는 J = 200 설계에서는 더 나빴다.
  breaks_for <- function(J) {
    dat <- sim_multisite(
      multisitedgp_design(J = J, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
    )
    .caterpillar_site_breaks(.strip_multisitedgp_data(dat))
  }

  # 임계값 이하는 그대로 — 사이트를 하나씩 짚을 수 있어야 한다.
  expect_length(breaks_for(10L), 10L)
  expect_length(breaks_for(25L), 25L)

  # 이상이면 상한을 넘지 않는다.
  for (J in c(50L, 200L)) {
    expect_lte(length(breaks_for(J)), .CATERPILLAR_MAX_LABELS)
  }
})

test_that("thinned labels stay a subset of the real site indices, in effect order", {
  dat <- sim_multisite(
    multisitedgp_design(J = 60L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
  )
  plot_data <- .strip_multisitedgp_data(dat)
  breaks <- .caterpillar_site_breaks(plot_data)

  expect_true(all(breaks %in% as.character(plot_data$site_index)))
  expect_false(anyDuplicated(breaks) > 0L)

  # 축 순서는 효과 크기 순이므로, 남긴 라벨도 그 순서의 부분수열이어야 한다.
  ordered <- as.character(plot_data$site_index[order(plot_data$tau_j_hat)])
  expect_identical(breaks, ordered[match(breaks, ordered)])
})

test_that("plot_effects still renders with the thinned axis", {
  dat <- sim_multisite(
    multisitedgp_design(J = 80L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
  )
  expect_s3_class(plot_effects(dat), "ggplot")
  expect_s3_class(plot_effects(dat, truth = FALSE), "ggplot")
})
