# Step 2.3 — 재현성 계약 감사

- **실행일:** 2026-08-14
- **제약:** Linux 접근 불가(Docker 미설치, push 보류) → 계획서가 예정한 **Layer 별 경험적 이분 탐색은 수행 불가**
- **대체 설계:** 플랫폼을 흔드는 대신 **solver 조건을 흔들어** "해가 tolerance 수준까지만 결정된다"는 가설을 검증했다. 이 실험은 플랫폼 차이가 만드는 것과 동일한 효과를 재현한다
- **실행 스크립트:** `scripts/hash-drift.R` · 원자료 `hash-drift-analysis.txt`

---

## 결론 — 버그가 아니라 달성 불가능한 계약이다

`canonical_hash()` 의 cross-platform 비트 동일성 계약은 **구현 결함이 아니라 수학적으로 달성할 수 없는 계약**이다. 세 가지가 맞물린다.

| # | 사실 | 근거 |
|---|---|---|
| 1 | 해시는 double 을 **반올림하지 않는다** — 원시 IEEE-754 비트가 곧 해시 | `.canonicalize_for_hash()` 는 `is.atomic(x)` 에서 `unname(x)` 만 반환. 2 ULP 섭동으로 해시가 바뀜을 실측 |
| 2 | **기본 경로가 반복 solver 를 탄다** — `engine = "A2_modern"` 이 기본값이고 `sim_multisite()` 1 회당 `nleqslv` 를 **5 회** 호출 | trace 로 실측 |
| 3 | solver 해는 **상대오차 ~1e-12(약 5,000 ULP)까지만 결정된다** | 5 개 시작점이 모두 `ftol = 1e-12` 를 만족하며 수렴하지만 서로 다른 점에 안착 |

즉 **계약이 요구하는 정밀도(1 ULP)가 계산이 제공하는 정밀도(~5,000 ULP)보다 3~4 자릿수 엄격하다.** 어떤 플랫폼에서 재생성하든 다른 플랫폼에서는 반드시 깨진다.

---

## 1. 해시 스키마 — 무엇이 해시되는가

`.canonical_hash_payload()` 가 data 객체에 대해 만드는 payload:

```
list(
  hash_schema_version,
  object_type = "data_frame_like",
  columns_sorted,          # 전체 컬럼명 (안정 정렬)
  data_canonical,          # 전체 컬럼의 원시 double 벡터
  diagnostics_numeric,     # 아래 8개 파생 스칼라
  manifest                 # version, paradigm, design_hash, schema, hook
)
```

**해시 대상 컬럼(7):** `site_index`, `z_j`, `tau_j`, `tau_j_hat`, `se_j`, `se2_j`, `n_j`

**해시 대상 진단 스칼라(8):** `I_hat`, `R_hat`, `rho_P_marginal`, `rho_P_residual`, `rho_S_marginal`, `rho_S_residual`, `sigma_tau_marg`, `sigma_tau_resid`

`.canonical_diagnostics_allowlist()` 가 이 8 개로 제한한다 — allowlist 자체는 잘 설계되어 있다(변동성 큰 진단은 제외됨).

### 반올림 여부 실측

```
1 값에 2 ULP 섭동 → canonical 표현 동일?  FALSE
digest 동일?                              FALSE
```

**반올림 없음.** 데이터 컬럼이든 진단 스칼라든 1 ULP 섭동으로 해시가 뒤집힌다.

| 섭동 대상 | 기준 해시 | 섭동 후 |
|---|---|---|
| (없음) | `ee1e18af8bd9b8e0` | — |
| `site_index` 1 ULP | | `55edd8a26e2b9aa8` |
| `z_j` 1 ULP | | `ec3f254186b441c7` |
| `tau_j` 1 ULP | | `982612454d2eca0c` |
| `diagnostics` 1 ULP | | `ba12c56effd5ec7f` |

---

## 2. 기본 경로가 반복 solver 를 탄다

```
기본 engine: A2_modern
sim_multisite() 1 회 호출 중 nleqslv 호출 횟수: 5
```

`nleqslv` 는 **R/ 전체에서 `layer2-engine-a2.R` 에서만** 사용된다. 다른 어떤 layer 도 `uniroot`/`optimize`/`optim`/`integrate` 를 쓰지 않는다. 그러나 A2 가 **기본 engine** 이므로 `paradigm = "site_size"` 의 기본 경로 전체가 solver 를 탄다.

호출부 (`R/layer2-engine-a2.R:266`):

```r
.nleqslv_solve(
  x = log(start),
  fn = .trunc_gamma_residual,
  control = list(ftol = 1e-12, xtol = 1e-12, maxit = max_iter)
)
alpha <- exp(fit$x[[1L]])
beta  <- exp(fit$x[[2L]])
```

`log` 스케일에서 풀고 `exp` 로 되돌린다. 5 개 시작점을 시도하는 multi-start 방식이다.

---

## 3. solver 해의 결정도 — 핵심 측정

`n_bar = 100, cv = 0.4, n_min = 5` 에서 5 개 시작점 전부 수렴:

```
수렴한 시작점: 5 / 5
alpha  범위: [6.249980880736187, 6.2499808807434469]
beta   범위: [0.062499836366643323, 0.06249983636672559]
시작점에 따른 해 차이: alpha 5.23e+03 ULP, beta 5.93e+03 ULP
잔차 노름 범위: [1.11e-15, 6.38e-13]   (ftol = 1e-12)
```

**모든 시작점이 수렴 조건을 만족하지만 해가 서로 약 5,000 ULP 떨어져 있다.** 5,000 ULP ≈ 5,000 × 2.2e-16 ≈ **1.1e-12 상대오차** — 정확히 `ftol` 수준이다. 수학적으로 당연한 결과이며 solver 의 결함이 아니다.

### tolerance 섭동 = 플랫폼 차이의 대리 실험

| `ftol` | `alpha` | 기준 대비 |
|---|---|---:|
| 1e-12 | 6.2499808807366311 | 0 ULP |
| **1e-13** | **6.2499808807417967** | **3,720 ULP** |
| 1e-11 | 6.2499808807366311 | 0 ULP |
| 1e-10 | 6.2499808807366311 | 0 ULP |

tolerance 를 한 자릿수 조이는 것만으로 해의 최하위 비트가 3,720 ULP 이동한다. **플랫폼별 libm·컴파일러 최적화·BLAS 차이도 정확히 같은 종류의 효과를 낸다.** Linux(glibc)와 macOS(Apple libm)의 `exp`/`log` 구현 차이는 solver 궤적을 바꾸고, 수렴 조건을 만족하는 공 안의 다른 점에 안착시킨다.

---

## 4. 왜 `design_hash` 는 일치하는가

CI 로그에서 관찰된 핵심 비대칭이 이것으로 설명된다.

| 해시 | Linux vs macOS | 이유 |
|---|---|---|
| `design_hash` | **일치** | design 객체는 **사용자 입력만** 담는다 (`J`, `sigma_tau`, `nj_mean`, `cv`, `seed` …). solver 출력이 들어가지 않는다 |
| `canonical_hash` | **불일치** | 생성된 **데이터**를 담는다. solver 출력이 `alpha`/`beta` → site size → `se2_j` → `tau_j_hat` 로 전파된다 |

표시되는 수치(`feasibility_index: 14.431`, `3 PASS, 3 WARN, 2 FAIL`)가 전부 일치한 것도 정합한다 — 차이가 1e-12 상대오차 수준이므로 소수점 3 자리 표시에서는 보이지 않는다.

---

## 5. same-machine 재현성은 온전하다

```
동일 머신 2 회: ee1e18af8bd9b8e0 / ee1e18af8bd9b8e0 → TRUE
```

T20 계약(같은 머신·같은 seed → 같은 해시)은 성립한다. 추가로 Step 2.6 이 확인:

- `design` 객체 경로와 flat 인자 경로의 `canonical_hash` 동일
- `seed` 지정 시 caller 의 `.Random.seed` 보존

**깨진 것은 cross-platform 계약뿐이다.**

---

## 6. 정책과 아티팩트·구현의 3중 불일치

`tools/cross-os-reproducibility-policy.md` 를 실제와 대조했다.

| 정책 조항 | 실제 | 판정 |
|---|---|---|
| "Linux is the strict cross-run hash baseline" | 체크인된 golden fixture 9 개는 `generated_platform = aarch64-apple-darwin20` | **불일치** (정책 문서가 스스로 자백) |
| "macOS and Windows are demoted from strict cross-OS hash equality" | `test-golden.R` 이 게이트 없이 `.rds` 9 개를 `compare_file_binary` 로 비교 → **면제가 무력화됨** | **불일치** |
| "Same-machine reproducibility is required on every OS" | 성립 | 일치 |
| "canonical_hash() is stable by schema, not by hiding numeric drift" | 사실이나, **드리프트가 숨길 수 없는 종류가 아니라 계약이 감당할 수 없는 종류**임을 정책이 인식하지 못함 | **전제 오류** |
| "Authoritative golden and print-example regeneration is Linux-only" | 아티팩트는 macOS 산출물 | **불일치** |

특히 두 번째 항목이 중요하다. T1a 는 `skip_if_not_linux_strict_hash()` 로 보호되어 macOS/Windows 에서 skip 되지만, **`test-golden.R` 은 보호되지 않은 채 같은 성격의 바이너리 비교를 수행한다.** 정책이 만든 면제를 다른 테스트가 우회한다 (원장 D-007).

---

## 7. D1 옵션에 대한 함의

Phase 3 Step 3.1 이 결정하되, 이 감사는 다음을 말한다.

### O1 (현행 유지 + Linux 재생성) — **문제를 이전시킬 뿐이다**

Linux 에서 fixture 를 재생성하면 Linux CI 는 green 이 되지만 **macOS·Windows 에서 깨진다.** 지금과 정확히 대칭인 상황이 된다. PI 의 개발 머신(macOS)에서 T1a 와 `test-golden.R` 이 영구히 실패하거나 영구히 skip 된다. 현재 정책이 의도한 것이 바로 이 구도이며, 그것이 3 개월 red 를 만들었다.

### O2 (허용 오차 기반 해시) — **원리적으로 옳다**

solver 의 결정도가 상대오차 ~1e-12 이므로, 그보다 **성긴** 정밀도로 정규화한 뒤 해시하면 세 플랫폼이 같은 해시를 낸다. 여유를 3 자릿수 두면 **유효숫자 9 자리(상대 1e-9)** 가 후보다.

- 실제 수치 회귀는 1e-9 보다 훨씬 크게 나타나므로 검출력을 잃지 않는다
- 기존 해시가 전부 무효가 되므로 fixture·snapshot·문서의 해시 문자열을 일괄 교체해야 한다
- 정책상 "canonical_hash schema changes without a documented major-version reproducibility decision" 에 해당하므로 NEWS 에 breaking change 명시 필요

### O3 (이중 계약) — **안전하지만 표면이 는다**

`canonical_hash()` 를 same-machine 계약으로 남기고 cross-platform 용 해시를 추가한다. 기존 해시 문자열이 유효하게 유지되는 것이 장점이나, 두 해시의 용도를 문서로 계속 구분해야 한다.

### 권고

**O2 를 기본으로, 정규화 자리수를 근거와 함께 고정한다.** 다만 결정 전에 Step 4.1 이 확인해야 할 것이 하나 있다 — **드리프트를 원천 제거할 수 있는가?**

`engine = "A1_legacy"` 는 solver 를 쓰지 않는다. 만약 A2 의 truncated-Gamma 적합을 closed-form 또는 결정론적 고정 반복으로 대체할 수 있다면 계약을 유지한 채 문제를 없앨 수 있다. 그러나 이는 **수치 동작 변경**이므로 golden fixture 와 JEBS 재현 증거에 영향을 준다. Step 4.1 이 비용을 산정한 뒤 판단한다.

---

## 8. 계획 대비 미수행

| 계획된 작업 | 상태 | 사유 |
|---|---|---|
| Layer 별 Linux/macOS 중간 산출물 이분 탐색 | **미수행** | Linux 접근 불가 (원장 D-019) |
| 드리프트 크기의 **플랫폼 간** 실측 | **미수행** | 동일. 대신 solver 결정도로 **상한**을 산정 |
| golden fixture / snapshot provenance 전수 확인 | 부분 | manifest 의 `generated_platform` 확인. 개별 파일 재생성 이력은 미확인 |
| 정책 대 실제 대조 | **완료** | §6 |

**§3 의 solver 결정도 측정이 이분 탐색을 상당 부분 대체한다.** 드리프트가 어느 Layer 에서 시작되는지는 Linux 없이도 확정할 수 있었다 — Layer 2 의 engine A2 다. Layer 1(`gen_effects`)은 R 자체 RNG 만 쓰므로 플랫폼 불변이고, Layer 3·4 는 Layer 2 의 출력을 받아 쓰므로 오염이 전파될 뿐이다.
