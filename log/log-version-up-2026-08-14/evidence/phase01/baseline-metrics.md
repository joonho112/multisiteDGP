# Step 1.5 — v0.2.0 시작 시점 베이스라인

- **측정일:** 2026-08-14
- **환경:** R 4.6.0 (2026-04-24), aarch64-apple-darwin23, macOS Tahoe 26.5.2, Quarto 1.9.37, pandoc 3.8
- **BLAS/LAPACK:** R 참조 구현 (`libRblas.0.dylib` / `libRlapack.dylib`) — Accelerate 아님
- **커밋:** `ad52c7c` (`upgrade/phase-01-foundation`)
- **원본 출력:** `baseline-raw/` 7 개 파일

**Phase 11 Step 11.1 이 동일한 명령으로 재측정해 이 표와 비교한다.** 명령 전문은 §8 에 있다.

---

## 요약표

| # | 측정 | 베이스라인 | v0.2.0 목표 | 성공 기준 |
|---|---|---|---|---|
| 1 | 기본 테스트 | **18.1 s / 0 실패 / 30 skip** | skip ≤ 3 | G3 |
| 2 | 확장 테스트 | **112.7 s / 0 실패 / 1 skip** | skip 0 | G3 · G4 |
| 3 | check (vignette 제외) | 0E / 0W / **2N** | 0E / 0W | G6 |
| 4 | check (vignette 포함) | **0E / 0W / 1N** | 0E / 0W / 0N | G6 |
| 5 | 커버리지 | **90.89 %** | ≥ 90.89 % | G3 |
| 6 | lint | **65 위반** | 0 (또는 문서화된 예외) | G6 |
| 7 | 성능 | 대표 시나리오 17–37 ms | 유의한 회귀 없음 | — |

---

## 1. 기본 테스트 (게이트 off)

```
real 18.13   user 14.64   sys 1.33
실패 0 | skip 30
```

skip 30 건의 사유별 분포는 [Ch.1 C-1](../../001_plan-master-version-upgrade/_book/01-current-state-audit.html) 과 동일하다 — `MULTISITEDGP_RUN_SLOW` 22, `MULTISITEDGP_RUN_PROPERTY` 7, Linux strict hash 1.

## 2. 확장 테스트 (`MULTISITEDGP_RUN_SLOW=true MULTISITEDGP_RUN_PROPERTY=true`)

```
real 112.73   user 104.87   sys 3.04
실패 0 | skip 1  (T1a — Linux x86_64 전용)
```

::: {.callout-important title="D3 에 직접 영향 — 게이트 제거 비용이 예상보다 훨씬 낮다"}
게이트를 **전부** 켜는 데 드는 추가 비용이 **+94.6 초**뿐이다. 계획서 Ch.7 Step 3.3 은 slow 테스트의 표본 크기·격자를 축소해 비용을 낮추는 것을 전제했으나, 실측 결과 **축소 없이 전부 켜도 113 초**로 권고 시간 예산(3 분) 안에 들어온다.

따라서 D3 의 유력한 답은 **"비용 절감 없이 그대로 켠다"** 이며, 이 경우 Phase 5 의 Step 5.2 · 5.3 (비용 절감) 이 대폭 축소되거나 불필요해진다. Phase 3 Step 3.3 이 이 수치를 근거로 판단한다.

단, CI 는 로컬보다 느리고 5-cell 매트릭스가 병렬로 돌므로 CI 시간 영향은 Phase 6 에서 재확인한다.
:::

## 3–4. `R CMD check --as-cran`

| 실행 | E | W | N | NOTE 내용 |
|---|--:|--:|--:|---|
| `vignettes = FALSE` | 0 | 0 | 2 | ① `man/*.Rd` 의 상대 URL `../articles/*.html` 19 개 파일 ② (미상) |
| `vignettes = TRUE` | 0 | 0 | **1** | `Non-standard file/directory found at top level: 'README.Rmd'` |

**Ch.1 미확인 사항 U-6 해소.** vignette 를 빌드하지 않으면 `../articles/*.html` 참조 대상이 없어 NOTE 가 뜬다. **전체 check 에서는 사라진다.** 남은 NOTE 1 건은 `.Rbuildignore` 에 `^README\.Rmd$` 한 줄을 더하면 해결되지만, pkgdown 워크플로가 `README.Rmd` 를 쓰므로 Phase 6 Step 6.2 에서 CI 영향과 함께 판단한다.

## 5. 커버리지 — 90.89 %

확장 게이트를 켠 상태에서 측정했다.

**90 % 미만 파일:**

| 파일 | % | hit / n |
|---|--:|---|
| `R/layer2-engine-a2.R` | 84.8 | 95 / 112 |
| `R/scenario_audit.R` | 88.1 | 111 / 126 |
| `R/layer1-effects-common.R` | 89.4 | 76 / 85 |

**미커버 라인 수 상위:**

| 파일 | 미커버 | % |
|---|--:|--:|
| `R/diagnostics-core.R` | 22 | 91.6 |
| `R/layer2-engine-a2.R` | 17 | 84.8 |
| `R/scenario_audit.R` | 15 | 88.1 |
| `R/class-multisitedgp_design.R` | 14 | 90.8 |
| `R/utils-reproducibility.R` | 12 | 91.0 |
| `R/layer1-effects-common.R` | 9 | 89.4 |

Phase 5 Step 5.6 · 5.7 의 대상 목록이다. `layer2-engine-a2.R` 의 미커버 17 줄은 blueprint BR1/V04 가 다룬 **A2 solver 실패 영역**과 겹칠 가능성이 높다 — Step 2.6 이 확인한다.

전체 목록: `baseline-raw/coverage-by-file.csv`

## 6. lint — 65 위반

| linter | 건수 |
|---|--:|
| `infix_spaces_linter` | 22 |
| `object_name_linter` | 22 |
| `line_length_linter` | 11 |
| `brace_linter` | 6 |
| `semicolon_linter` | 2 |
| `commented_code_linter` | 1 |
| `indentation_linter` | 1 |

**파일별 상위:** `vignettes/m3-margin-se-models.Rmd` 20, `R/multisitedgp-package.R` 11, `vignettes/m1-statistical-dgp.Rmd` 9, `vignettes/a8-cookbook.Rmd` …

::: {.callout-warning title="CI lint 실패의 유력한 원인"}
위반이 65 건 존재하므로 `lint` workflow 는 **실패하는 것이 정상**이다. 게다가 Step 1.2 에서 확인했듯 **`.lintr` 이 원격에 추적되지 않았다** — CI 는 `.lintr` 없이, 즉 `lintr` 기본 규칙(`object_usage_linter` 포함, `line_length` 80)으로 실행됐으므로 실제 CI 위반 수는 65 건보다 **훨씬 많았을 것**이다. Phase 2 Step 2.1 이 재현으로 확인한다.
:::

전체 목록: `baseline-raw/lint.txt`

## 7. 성능

| 항목 | median |
|---|--:|
| `sim_multisite(preset_education_modest())` | 17 ms |
| `sim_multisite(preset_jebs_paper())` | 34 ms |
| `sim_meta(preset_meta_modest())` | 15 ms |
| **J 스케일링** J = 50 / 200 / 1000 / 5000 | 17 / 17 / 18 / **24 ms** |
| **dependence** rank / copula / hybrid (J = 200) | **274** / 19 / 42 ms |
| **shape** Gaussian / StudentT / SkewN / ALD / Mixture / PMSlab | 17 / 17 / 39 / 39 / 38 / 39 ms |
| `scenario_audit(grid, M = 1L)` | 41 ms |
| `canonical_hash()` | < 1 ms |
| `summary()` | 3 ms |
| `design_grid(9 cells)` | 2 ms |

**관찰.**

- **J 스케일링이 사실상 평탄하다.** J 를 100 배(50 → 5000) 늘려도 17 ms → 24 ms. 벡터화가 잘 되어 있고 고정 오버헤드가 지배적이다.
- **`dependence = "rank"` 가 유일한 병목** — 274 ms 로 다른 경로의 6–16 배. hill-climbing 반복 때문이며 `max_iter` 기본값과 관련이 있다. 대규모 `design_grid()` sweep 에서 누적되므로 Step 2.6 · 2.8 의 확인 대상이다.
- 비정규 shape 4 종이 Gaussian 의 2.3 배(39 ms vs 17 ms). 표준화 계산 비용으로 보이며 절대값이 작아 문제는 아니다.

## 부수 관찰 — 결함이 아닌 것

베이스라인 측정 중 다음을 확인했고, **결함이 아니므로 원장에 올리지 않는다.**

- **`theta_G` 검증 오류 메시지가 정확하다.** shape 별 필수 파라미터를 이름으로 지목한다 — `` `true_dist = "SkewN"` requires `theta_G$slant` ``, `` "ALD" requires `theta_G$rho` ``, `` "Mixture" requires `theta_G$delta`, `theta_G$eps`, and … ``. fail-fast 설계가 의도대로 동작한다.
- **`scenario_audit()` 은 `design_grid()` 출력을 받는다** (시뮬레이션 데이터가 아니라). 잘못 호출하면 `` `grid` must be a multisitedgp_design_grid object `` 와 함께 올바른 사용법을 안내한다.
- **`design_grid()` 는 `seed_root` 를 요구한다.** "multisiteDGP never manufactures seeds from the caller's global RNG state" 정책의 구현이다.

## 8. 재측정 명령

Phase 11 Step 11.1 이 그대로 실행한다.

```bash
cd multisiteDGP-R-package
LOG=log/log-version-up-2026-08-14/evidence/phase11/baseline-raw

# 1. 기본 테스트
/usr/bin/time -p Rscript -e 'suppressMessages(devtools::load_all(".", quiet=TRUE));
  testthat::test_local(".", reporter="summary", stop_on_failure=FALSE)'

# 2. 확장 테스트
MULTISITEDGP_RUN_SLOW=true MULTISITEDGP_RUN_PROPERTY=true \
/usr/bin/time -p Rscript -e 'suppressMessages(devtools::load_all(".", quiet=TRUE));
  testthat::test_local(".", reporter="summary", stop_on_failure=FALSE)'

# 3. check (vignette 제외)
Rscript -e 'devtools::check(".", document=FALSE, vignettes=FALSE,
  args=c("--no-manual","--as-cran"), error_on="never")'

# 4. check (vignette 포함)
Rscript -e 'devtools::check(".", document=FALSE, vignettes=TRUE,
  args=c("--no-manual","--as-cran"), error_on="never")'

# 5. 커버리지  (스크립트: evidence/phase01/scripts/cov.R)
MULTISITEDGP_RUN_SLOW=true MULTISITEDGP_RUN_PROPERTY=true Rscript scripts/cov.R "$LOG"

# 6. lint
Rscript -e 'l <- lintr::lint_package(); cat("총 위반:", length(l), "\n"); print(l)'

# 7. 성능  (스크립트: evidence/phase01/scripts/bench.R)
Rscript scripts/bench.R
```

커버리지와 성능 스크립트는 `evidence/phase01/scripts/` 에 보존되어 있다.
