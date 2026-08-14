# Step 2.1 — CI 실패 포렌식

- **실행일:** 2026-08-14
- **제약:** Docker 미설치, 원격 push 보류(PI 결정) → 계획서의 경로 A(브랜치 push 후 로그 확보)와 경로 B(Docker 재현) 모두 사용 불가
- **대체 방법:** workflow YAML 의 실패 조건을 로컬에서 **그대로 실행**해 재현. 로컬에서 재현 가능한 조건은 확정하고, Linux 고유 조건은 근거와 함께 추론으로 표시

---

## 결론

**red workflow 4 개는 두 개의 근본 원인으로 전부 설명된다.**

| 근본 원인 | 영향받는 workflow | 확신도 |
|---|---|---|
| **RC-1.** macOS 에서 생성된 golden fixture · snapshot 이 체크인되어 있고, 이를 검증하는 테스트 5 개 파일이 **게이트 없이** 모든 플랫폼에서 실행된다 | `extended-tests`, `R-CMD-check`, `test-coverage` | 확정(extended-tests) / 높음(나머지 2) |
| **RC-2.** `lint` workflow 가 위반 1 건도 허용하지 않는데 위반이 **440 건** 존재한다 (`.lintr` 이 원격에 미추적이라 CI 는 프로젝트 설정 없이 실행) | `lint` | **확정** |

`pkgdown-deploy` 만 green 인 이유도 설명된다 — 이 workflow 는 테스트도 lint 도 돌리지 않고 사이트만 빌드한다.

---

## 1. `lint` — 확정, 정량화 완료

### 실패 조건 (`lint.yaml`)

```r
lints <- lintr::lint_package()
print(lints)
if (length(lints) > 0L) {
  quit(status = 1L)      # 위반 1건이라도 있으면 실패
}
```

### 로컬 재현

| 조건 | 위반 수 |
|---|---:|
| `.lintr` 있음 (로컬 개발 조건) | **65** |
| **`.lintr` 없음 (CI 조건)** | **440** |

`.lintr` 없을 때의 linter 별 분포:

| linter | 건수 | 비고 |
|---|--:|---|
| `line_length_linter` | 332 | 기본 80자 vs 프로젝트 설정 120자 |
| `object_usage_linter` | 45 | 프로젝트는 `NULL` 로 비활성화 |
| `object_name_linter` | 31 | 프로젝트는 `J`/`I`/`R` 예외 허용 |
| `infix_spaces_linter` | 22 | 실제 위반 |
| `brace_linter` | 6 | 실제 위반 |
| `semicolon_linter` | 2 | 실제 위반 |
| `commented_code_linter` | 1 | 실제 위반 |
| `indentation_linter` | 1 | 실제 위반 |

### 왜 `.lintr` 이 CI 에 없었나

Step 1.1 에서 확인: `.lintr` 과 `.pre-commit-config.yaml` 이 원격 `main` 의 추적 파일 235 개에 **포함되지 않았다.** `actions/checkout` 은 추적 파일만 가져오므로 CI runner 의 작업 디렉터리에 `.lintr` 이 존재하지 않았고, `lintr::lint_package()` 는 기본 설정으로 실행됐다.

Step 1.2 에서 `.lintr` 을 추적에 추가했으므로 이 부분은 이미 해소됐다. 남은 것은 실제 위반 65 건이다.

### 부수 확인 — pure-R 경계 검사는 통과

`lint.yaml` 의 두 번째 스텝("Enforce pure-R package boundary")을 로컬에서 실행했다. Step 1.2 가 `tools/` 와 `log/` 를 추적에 넣었으므로 재귀 스캔에 새 위반이 생겼을 수 있어 확인이 필요했다.

```
위반 수: 0  → 통과
```

`.stan/.cpp/.cc/.c/.f/.f90` 파일 없음, 금지 필드(`LinkingTo`/`SystemRequirements`/`NeedsCompilation`) 없음, 금지 의존성(Rcpp 계열/Stan 계열) 없음. **추적 범위 확대가 회귀를 만들지 않았다.**

---

## 2. `extended-tests` — 확정 (2026-07-13 로그 확보)

### 실패 조건

```yaml
env:
  MULTISITEDGP_RUN_SLOW: "true"
  MULTISITEDGP_RUN_PROPERTY: "true"
steps:
  - run: R CMD INSTALL .
  - run: testthat::test_local(".", reporter = "summary")
```

### 확보된 실패 내용

10 건에서 절단됐으며(testthat 기본 상한), 전부 동일한 형태다 — **표시되는 수치는 전부 일치하고 `design_hash` 도 일치하는데 `canonical_hash` 만 다르다.**

| 테스트 | 건수 |
|---|--:|
| `test-snapshot-print.R` | 7+ |
| `test-generated-print-examples-step8-6.R` | 2+ |

절단 때문에 전체 목록은 미확인이다. 아래 §4 가 실제 노출 범위를 산정한다.

### 근본 원인

Step 2.3 이 규명했다 — 요약하면 기본 engine A2 가 `nleqslv` 를 `ftol = 1e-12` 로 돌리고, 그 해가 **~5,000 ULP(상대오차 ~1e-12) 수준까지만 결정**되는데 `canonical_hash` 는 반올림 없이 원시 IEEE-754 비트를 해시한다. 플랫폼이 다르면 solver 가 그 공 안의 다른 점에 안착하고 해시가 갈린다. 상세: `hash-drift-analysis.txt`, `reproducibility-contract-audit.md`.

---

## 3. `R-CMD-check` — 높은 확신 (로그 만료, 추론)

2026-05-10 실행(`25620008533`, 34m3s)의 로그는 GitHub 90 일 보존을 넘겨 `HTTP 410` 이다.

### 실패 조건

```yaml
env:
  MULTISITEDGP_RUN_SLOW: "true"       # ← T1a 가 Linux 에서 실행된다
  MULTISITEDGP_RUN_PROPERTY: "false"
with:
  error-on: '"warning"'                # ← 테스트 실패는 ERROR → 실패
```

### 추론

1. `MULTISITEDGP_RUN_SLOW: "true"` 이므로 **T1a(JEBS golden fixture 비트 동일성)가 Linux cell 3 개에서 실제로 실행된다.** golden fixture 는 macOS/aarch64 생성이므로 실패한다.
2. §4 의 게이트 없는 snapshot 테스트 5 개 파일도 Linux 에서 실패한다.
3. `error-on: "warning"` 이므로 `R CMD check` 의 "checking tests" 실패가 workflow 실패가 된다.
4. macOS cell 은 fixture 생성 플랫폼이므로 통과했을 것이다. Windows cell 은 미확인 — Linux 와 같은 이유로 실패했을 가능성이 높다.

### 로컬 대조

동일 환경변수로 로컬(macOS) 전체 check 를 돌리면 **0E / 0W / 1N** 이다(Phase 1 Step 1.5). 즉 **코드 자체에는 문제가 없고 플랫폼별 아티팩트 문제만 남는다** — 위 추론과 정합한다.

**미확정 부분.** Linux 에서 실제로 어떤 테스트가 몇 건 실패하는지는 Linux 접근 없이 확인할 수 없다. Phase 6 Step 6.2 가 CI 를 실제로 태워 확정한다.

---

## 4. 게이트 없이 실행되는 아티팩트 검증 테스트

RC-1 의 노출 범위다. 아래 5 개 파일은 `skip_if_not_slow` / `skip_if_not_property` / `skip_if_not_linux_strict_hash` 를 **하나도 쓰지 않으므로 모든 workflow 의 모든 cell 에서 실행된다.**

| 파일 | 게이트 | 검증 대상 |
|---|--:|---|
| `test-snapshot-print.R` | 0 | `print()`/`summary()` 출력 (provenance 문자열에 `canonical_hash` 포함) |
| `test-generated-print-examples-step8-6.R` | 0 | 체크인된 print-example 텍스트 5 종 |
| `test-golden.R` | 0 | golden fixture `.rds` 9 개를 `compare_file_binary` 로 **바이너리 비교** |
| `test-print-examples.R` | 0 | print-example |
| `test-snapshot-errors.R` | 0 | 오류 메시지 (해시 무관 → 영향 없음) |

::: {.callout-warning title="`test-golden.R` 이 가장 취약하다"}
`expect_snapshot_file(..., compare = testthat::compare_file_binary)` 로 `.rds` 파일을 **바이트 단위 비교**한다. RDS 는 double 을 정확히 직렬화하므로 **1 ULP 차이도 바이너리 차이가 된다.** T1a 는 `skip_if_not_linux_strict_hash()` 로 보호되지만 `test-golden.R` 은 보호되지 않는다 — 즉 **재현성 정책이 "macOS/Windows 는 strict hash 에서 면제" 라고 규정했음에도, 실제로는 게이트 없는 바이너리 fixture 비교가 그 면제를 무력화한다.**

이것은 정책과 구현의 불일치이며 Phase 4 가 처리해야 한다.
:::

---

## 5. `test-coverage` — 원인 미확정 (후보 2)

### 실패 조건

```r
line_cov <- covr::percent_coverage(cov, by = "line")
if (line_cov < 90) stop(...)          # 후보 A
...
covr::to_cobertura(cov, filename = "cobertura.xml")
```
```yaml
- uses: codecov/codecov-action@v6
  if: ${{ env.CODECOV_TOKEN != '' }}
  with:
    fail_ci_if_error: true             # 후보 B
```

### 로컬 재현 — 후보 A 는 **탈락**

CI 조건(`MULTISITEDGP_RUN_SLOW=false`, `MULTISITEDGP_RUN_PROPERTY=false`, `function_exclusions` = internal 함수 35 개 제외)을 그대로 실행했다.

```
exported 함수: 53 | internal 함수(제외 대상): 35
Exported-function line coverage: 91.36%
CI 게이트 (>= 90%): PASS
```

**커버리지 게이트는 통과한다.** 따라서 이것은 실패 원인이 아니다.

파일별 하위 (CI 기준, exported-only):

| 파일 | % | 미커버 |
|---|--:|--:|
| `R/layer1-gen_effects_mixture.R` | 77.6 | 11 |
| `R/layer2-engine-a2.R` | 85.7 | 6 |
| `R/scenario_audit.R` | 88.1 | 15 |
| `R/layer1-effects-common.R` | 89.4 | 9 |
| `R/plots.R` | 89.5 | 8 |

### 남은 후보

| 후보 | 내용 | 확인 방법 |
|---|---|---|
| **B-1** | Linux 에서 §4 의 snapshot 테스트가 실패 → `covr::package_coverage()` 가 오류를 전파 | Linux 필요 |
| **B-2** | `CODECOV_TOKEN` 이 설정되어 있으나 만료/무효 → `fail_ci_if_error: true` 로 실패 | 저장소 secrets 확인(PI 권한) |

`covr` 3.6.5 에 `to_cobertura` 는 존재하므로 그 경로는 아니다.

**B-1 이 더 유력하다.** RC-1 이 나머지 workflow 를 전부 설명하므로 같은 원인일 개연성이 높고, B-2 라면 pkgdown 이외의 모든 workflow 가 같은 시점에 실패한 것을 설명하지 못한다.

---

## 6. 10 주 연속 실패의 동일성

`extended-tests` 는 2026-05-11 부터 2026-07-13 까지 10 회 연속 실패했고 소요 시간이 5m44s ~ 6m50s 로 일정하다. 코드가 바뀌지 않았고(마지막 push 2026-05-10) 실패 지점도 동일하므로 **10 회 모두 같은 원인**으로 판단한다. 2026-07-13 로그만 확보 가능했으나 다른 회차를 별도 조사할 근거가 없다.

---

## 7. 알림 부재가 방치를 만들었다

주간 스케줄 실패가 10 회 누적되는 동안 아무 조치가 없었다. `extended-tests.yaml` 에는 실패 시 이슈 생성이나 알림이 없고, 저장소 이슈는 0 건이다. **원인 수정만으로는 재발을 막지 못한다** — Phase 6 Step 6.5 의 알림 메커니즘이 이 Phase 의 실질적 산출물이다.

---

## 8. 원장 후보

| ID 후보 | 요약 | 영역 | 예상 등급 | 위협 |
|---|---|---|---|---|
| RC-1 | macOS 생성 아티팩트가 게이트 없는 테스트로 전 플랫폼에서 검증됨 | repro | P0 | G2 · G4 |
| RC-1a | `test-golden.R` 의 `compare_file_binary` 가 cross-OS 면제 정책을 무력화 | repro | P0 | G4 |
| RC-2 | lint 위반 65 건 (`.lintr` 기준) | infra | P0 | G2 · G6 |
| RC-2a | `.lintr` 미추적 → CI 가 기본 규칙으로 실행 (440 건) | infra | P0 | G2 |
| RC-3 | `test-coverage` 실패 원인 미확정 (후보 B-1/B-2) | infra | P1 | G2 |
| RC-4 | CI 실패 알림 부재 → 10 주 방치 | infra | P0 | G2 |
| RC-5 | testthat 실패 10 건 절단으로 전체 실패 목록 미확인 | infra | P2 | — |

## 9. 미해결

**Linux 실행 수단이 없어 확정하지 못한 것:**

1. `R-CMD-check` 5 cell 각각의 실제 실패 내용
2. `test-coverage` 실패 원인 (B-1 vs B-2)
3. Windows cell 의 동작
4. `extended-tests` 의 절단된 실패 목록 전체

**해소 조건:** ① PI 가 push 를 승인하면 CI 를 태워 즉시 확정, 또는 ② Docker 설치 후 `rocker/r-ver` 로 재현. 어느 쪽이든 Phase 6 진입 전에는 필요하다.
