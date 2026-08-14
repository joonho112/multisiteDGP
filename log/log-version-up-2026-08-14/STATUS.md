# 돌아오셨을 때 먼저 읽을 것

**2026-08-14 자율 실행 구간 종료 시점 상태입니다.**

---

## 1. 결정 5 건 — 2026-08-14 전부 승인됨

기록: [`decisions/001-pi-decisions-gate-B.md`](decisions/001-pi-decisions-gate-B.md). **게이트 G-B 통과.**

| # | 결정 | 채택안 |
|---|---|---|
| D5 | Linux 검증 수단 | **원격 push 승인** → 실행 완료, CI 가동 중 |
| D1 | 재현성 계약 | Step 4.1 에서 O4 조사 → 실패 시 O2 |
| D2 | 광고-구현 갭 | IES 자료 언급 없음 → **7 로 축소** |
| D3 | 테스트 게이트 | 승인 + Phase 5 잔여 Step 진행 |
| D4 | 버전·범위 | `0.2.0`, 원장 전건 포함 |

::: 중요
**push 후 첫 Linux CI 가 D1 의 전제를 흔들었습니다.** 아래 §3-④ 를 보십시오 — O4 만으로는 닫히지 않을 가능성이 큽니다.
:::

---

## 2. 진행 상황

| Phase | 상태 | 로그 |
|---|---|---|
| 1 작업 기반 복구 | **완료** | [`002_phase01-foundation-recovery.html`](002_phase01-foundation-recovery.html) |
| 2 결함 인벤토리 감사 | **완료** | [`003_phase02-defect-audit.html`](003_phase02-defect-audit.html) |
| 3 v0.2.0 범위 결정 | **완료 — 게이트 G-B 대기** | [`004_phase03-scope-decisions.html`](004_phase03-scope-decisions.html) |
| 4 재현성 계약 + solver 견고화 | **Step 4.1a·4.1b 완료** (4.7 정책문서 잔여) | [`006_phase04-solver-hardening-partial.html`](006_phase04-solver-hardening-partial.html) |
| 5 테스트 스위트 | **부분 (4/6 Step)** | [`005_phase05-test-suite-rebuild-partial.html`](005_phase05-test-suite-rebuild-partial.html) |
| 6–11 | 미착수 | — |

---

## 3. 가장 중요한 세 가지 발견

### ① 우려했던 "3 개월 격차" 는 격차가 아니었습니다

원격 추적 파일 235 개 중 **226 개가 바이트 동일**하고, 로컬에서 원격으로 가야 할 변경은 **하나도 없었습니다.** 다른 9 개는 전부 원격이 최신이었고, 그중 `CONTRIBUTING.md`·`NEWS.md`·`references.bib` 는 공개 릴리스 때 내부 경로를 지운 **정제본이 로컬로 돌아오지 않은 것**이었습니다. 모르고 편집하셨다면 정제가 되돌려질 뻔했습니다.

### ② red CI 4 개가 근본 원인 2 개로 환원됩니다

- **RC-1** — macOS 에서 생성된 golden fixture·snapshot 이 체크인되어 있는데, 이를 검증하는 테스트 5 개 파일이 **게이트 없이** 모든 플랫폼에서 실행됩니다. `extended-tests`·`R-CMD-check`·`test-coverage` 를 설명합니다
- **RC-2** — `lint` 는 위반 1 건도 허용하지 않는데, **`.lintr` 이 원격에 추적되지 않아** CI 가 기본 규칙으로 돌았습니다. 위반이 65 가 아니라 **440** 이었습니다 (Phase 1 에서 `.lintr` 추적으로 이미 65 로 낮췄습니다)

### ④ [2026-08-14 CI] solver 실현 가능 경계가 **플랫폼 의존**입니다 — D1 재검토 필요

push 직후 첫 Linux CI 가 Phase 5 에서 추가한 테스트로 이것을 잡았습니다.

| `solve_trunc_gamma(n_bar=100, cv=0.005, n_min=5)` | macOS | Linux |
|---|--:|--:|
| 최대 스케일 잔차 | 7.44e-07 | **1.908e-06** |
| tolerance | 1e-06 | 1e-06 |
| 판정 | 통과 | **abort** |

잔차가 **2.56 배** 차이납니다. 최하위 비트 드리프트가 아니라 **두 플랫폼이 실질적으로 다른 점에 수렴**하며, PASS/FAIL 판정이 뒤집힙니다. **같은 설계가 macOS 에서 성공하고 Linux 에서 abort 합니다** — 해시 불일치보다 심각합니다.

**D1 에 대한 함의.** O4(출력 양자화)는 두 플랫폼이 같은 점에 도달하고 마지막 비트만 다르다고 가정합니다. `cv = 0.4` 같은 조건수 좋은 영역에서는 성립하지만, 경계 근처(`cv` 작음 → `alpha` 약 40000)에서는 성립하지 않습니다. **다른 점에 수렴하므로 양자화로는 화해시킬 수 없습니다.** solver 견고화(시작점 개선 · tolerance 재설계 · 재매개화)가 별도로 필요합니다. 원장 D-024 (P0).

### ③ 재현성 계약은 버그가 아니라 **달성 불가능한 계약**이었습니다

기본 engine A2 가 `nleqslv` 를 `ftol = 1e-12` 로 돌립니다. 5 개 시작점이 **모두 수렴 조건을 만족하면서 서로 약 5,000 ULP 떨어진 점**에 안착합니다. 반면 해시는 double 을 반올림하지 않아 **1 ULP** 에도 뒤집힙니다.

**계약이 요구하는 정밀도가 계산이 제공하는 정밀도보다 3~4 자릿수 엄격합니다.** 어느 플랫폼에서 재생성해도 다른 플랫폼에서는 반드시 깨집니다. 그래서 D1 에서 O1(Linux 재생성)을 **기각 권고**했습니다 — 문제를 macOS 로 옮길 뿐입니다.

---

## 4. 눈에 보이는 개선

| 지표 | 시작 | 지금 |
|---|--:|--:|
| git 관리 | 없음 | 695 파일 추적, 커밋 12 건 |
| 기본 테스트 skip | **30** | **1** |
| 기본 테스트 실패 | 0 | 0 |
| 기본 테스트 시간 | 18.1 s | 106.0 s (예산 180 s) |
| 커버리지 | 90.89 % | **92.78 %** |
| `layer2-engine-a2.R` | 84.8 % | **95.5 %** |
| `utils-reproducibility.R` | 91.0 % | **95.5 %** |
| 작업 디렉터리 | 462 MB | 417 MB |
| 결함 원장 | 없음 | **27 행**, **13 건 해소** (P0 8 · P1 5) |

`R CMD check --as-cran` (vignette 포함)은 시작 시점부터 **0 error / 0 warning / 1 note** 였습니다. NOTE 는 `README.Rmd` 가 최상위에 있다는 것뿐입니다.

---

## 5. 안전 상태

| 항목 | 상태 |
|---|---|
| **원격 push** | **2026-08-14 승인 후 실행** — 브랜치 3 개 + 태그 push 완료 |
| **외부 저장소 생성** | 하지 않았습니다 — 아카이브는 로컬 git repo 로만 |
| 패키지 코드(`R/`) 변경 | **0 줄** |
| `DESCRIPTION` 변경 | 1 줄 (`hedgehog` 제거) |
| 되돌리기 | `git checkout pre-upgrade-baseline` 으로 전체 원복 |

디스크 사고 위험은 push 로 해소되었습니다. `main` 은 아직 건드리지 않았습니다 — Phase 종료 시 병합합니다.

---

## 6. Linux CI 가 확정한 것 (2026-08-14)

Phase 2 가 Linux 부재로 `partial` 로 남겼던 항목이 전부 해소되었습니다.

| workflow | 결과 | 실패 지점 | 원인 |
|---|---|---|---|
| `lint` | failure | Lint package | **위반 65 건** (`.lintr` 은 정상 적용됨 — 440 이 아님). D-006 확정 |
| `extended-tests` | failure | 테스트 | 해시 불일치 + **D-024** |
| `test-coverage` | failure | covr 실행 | **테스트 실패 전파** — `[FAIL 15 SKIP 36 PASS 4314]`. Codecov 토큰 무관. **D-009 확정 (후보 B-1)** |
| `R-CMD-check` linux-release · linux-oldrel | failure | `check-r-package` | 테스트 실패 |
| `R-CMD-check` macos-release · windows-release | failure | **`setup-r-dependencies`** | 의존성 설치 실패 — 코드에 닿기 전. **D-025 신규 (P0)** |

실제 테스트 실패는 **15 건**입니다 (extended-tests 는 10 건에서 잘렸습니다 — D-017 확정).

`extended-tests` 는 GitHub 이 **자동 비활성화**해 두었습니다 — 무활동 60 일. 2026-07-13 이후 실패가 멈춘 것은 고쳐져서가 아니라 신호가 죽었기 때문입니다 (D-023, 재활성화 완료).

---

## 7. 파일 지도

```
log-version-up-2026-08-14/
├─ STATUS.md                          ← 이 문서
├─ 001_plan-master-version-upgrade/   계획서 (Quarto Book)
├─ 002_phase01-*.qmd/html             Phase 1 로그
├─ 003_phase02-*.qmd/html             Phase 2 로그 (가장 내용이 많습니다)
├─ 004_phase03-*.qmd/html             Phase 3 로그
├─ 005_phase05-*.qmd/html             Phase 5 로그 (부분)
├─ defect-ledger.csv                  결함 원장 22 행
├─ decisions/
│  ├─ 000-PI-approval-packet.html     ★ 먼저 읽으실 것
│  ├─ D1-reproducibility-contract.md
│  ├─ D2-advertised-surface.md
│  ├─ D3-test-gate-policy.md
│  ├─ D4-release-scope.md
│  ├─ 000-pi-approval-record.md       2026-08-14 승인 기록
│  └─ branch-strategy.md
├─ evidence/phase01/                  베이스라인 측정 + 재실행 스크립트
├─ evidence/phase02/                  감사 증거 14 파일 + 스크립트 7
├─ evidence/phase05/                  게이트 제거 전후
└─ artifacts/                         되돌리기용 보존
```
