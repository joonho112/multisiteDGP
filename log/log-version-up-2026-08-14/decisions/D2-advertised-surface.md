# D2 — 광고-구현 갭 처리

- **작성:** 2026-08-14, Phase 3 Step 3.2
- **상태:** **PI 가 2026-08-14 에 방침을 사전 결정함** — "광고에서 축소" (`000-pi-approval-record.md` A4)
- **이 문서의 역할:** 방침 확인 + Phase 2 가 확정한 **실행 범위** 고정
- **근거:** `evidence/phase02/surface-audit.txt`, `evidence/phase02/true-dist-implementation.csv`
- **막는 것:** Phase 7 Step 7.3 · 7.4
- **되돌릴 수 있나:** 예

---

## 1. 확정된 갭

Phase 2 Step 2.4 가 실행으로 검증했다.

### 갭 1 — DPM (P0, 원장 D-004)

| `true_dist` | 결과 |
|---|---|
| Gaussian · StudentT · SkewN · ALD · Mixture · PointMassSlab | **동작** (6) |
| User | `g_fn` 필요 — **설계상 정상**, 콜백을 주면 동작 |
| **DPM** | **abort** — `` `true_dist = "DPM"` is not implemented in multisiteDGP v1. `` |

**사용 가능한 shape 는 7 인데 8 로 광고한다.**

### 갭 2 — 예약 인자 (P1, 원장 D-018)

| 인자 | 동작 |
|---|---|
| `target_marginal_rho` | E23 abort — `not supported in v1.0` |
| `upstream` | abort — `reserved and is not implemented yet` |

### 갭 3 — `multisitepower` 설치 경로 (P1, 원장 D-010)

`as_multisitepower()` adapter 를 광고하나 CRAN 미배포이고 **개발 소스 URL 이 `DESCRIPTION`·vignette·정책·NEWS 어디에도 없다.** 사용자가 설치할 방법이 없다.

---

## 2. "eight" 주장 위치 — 전수 18 곳

| 파일 | 줄 | 건수 |
|---|---|--:|
| `DESCRIPTION` | 9 | 1 |
| `NEWS.md` | 96, 99 | 2 |
| `README.md` | 4, 35, 116, 129, 217 | 5 |
| `README.Rmd` | 19, 48, 129, 142, 230 | 5 |
| `index.md` | 1, 14, 120 | 3 |
| `_pkgdown.yml` | 113, 163 | 2 |

`README.md` 는 `README.Rmd` 에서 생성되므로 실제 편집 대상은 **`README.Rmd`** 이고 `README.md` 는 `devtools::build_readme()` 로 재생성한다.

vignette `m2-g-distribution-catalog.Rmd` 도 확인이 필요하다 (스캔 패턴에 걸리지 않았으나 shape 카탈로그를 다루는 vignette 이다).

---

## 3. 실행 방침 (PI 사전 결정 반영)

### 갭 1 — DPM: 광고 축소

| 대상 | 조치 |
|---|---|
| `DESCRIPTION` Description | "eight distribution shapes ( … and a Dirichlet-process-mixture bridge)" → **"seven distribution shapes ( … )" + DPM 을 예약 슬롯으로 별도 문장** |
| `NEWS.md` v0.1.0 항목 | **수정하지 않는다** — 이력이다. v0.2.0 항목에서 정정을 명시 |
| `README.Rmd`, `index.md`, `_pkgdown.yml` | 개수 서술 정정 |
| `gen_effects_dpm()` roxygen | `@description` 첫 문장에 미구현 명시 |
| `vignette("m2")` | DPM 절 갱신 |
| E22 메시지 | v2 계획 안내 추가 (blueprint §24.2.1) |
| `tools/traceability/api-index.csv` | 갱신 |

::: {.callout-caution title="Step 7.3 실행 전 PI 확인 필요"}
IES annual report·발표 자료·투고 원고에서 이미 "eight distribution shapes" 를 언급했다면 축소 서술이 그 자료와 어긋난다. **Step 7.3 을 실행하기 전에 해당 자료 유무를 PI 에게 확인한다.**

어긋남이 확인되면 대안은 갭 1 을 선택지 C(실험 기능으로 명시 표시, export 유지, 개수는 8 유지)로 전환하는 것이다. 이 경우 "여덟 번째는 예약되어 있으며 v2 에서 구현" 임을 명시하면 문서와 자료가 모두 참이 된다.
:::

### 갭 2 — 예약 인자

| 인자 | 조치 | 근거 |
|---|---|---|
| `target_marginal_rho` | **유지 + 문서 명확화** | 제거하면 기존 사용자 코드가 "인식되지 않는 인자" 오류를 받아 안내가 나빠진다. E23 메시지에 blueprint §24.2.7 의 blocker 요약과 v2 계획을 넣는다 |
| `upstream` | **문서에서 숨김 또는 제거** | 용도가 blueprint 에서 확인되지 않으면 제거. Layer 간 hook 으로 예약된 것이면 `@keywords internal` 처리 |

### 갭 3 — `multisitepower`

| 조치 | 내용 |
|---|---|
| 1 순위 | 개발 소스 URL 을 찾아 `DESCRIPTION` `Additional_repositories` 또는 vignette `m6` 에 설치 안내 추가 |
| URL 을 찾지 못하면 | `as_multisitepower()` 를 `lifecycle::badge("experimental")` 로 표시하고 "이 adapter 를 쓰려면 multisitepower 를 직접 확보해야 한다" 를 명시 |

---

## 4. 검증

Step 7.3 종료 시 다음이 전부 참이어야 한다.

```bash
# 잔여 "eight" 주장이 없다 (NEWS.md 의 v0.1.0 이력 항목 제외)
grep -rn "eight" DESCRIPTION README.Rmd index.md _pkgdown.yml vignettes/*.Rmd

# DPM 언급이 전부 "미구현" 맥락이다
grep -rni "dpm\|dirichlet" DESCRIPTION README.Rmd index.md _pkgdown.yml R/ vignettes/
```

`R CMD check` 통과 + `test-traceability.R` 통과.

---

## 5. PI 결정란

방침(광고 축소)은 이미 승인됨. 아래만 확인이 필요하다.

**IES 보고서·발표 자료에 "eight distribution shapes" 언급이 있는가?**

- [x] **없음 — 권고대로 7 로 축소 진행**
- [ ] 있음 — 갭 1 을 선택지 C(실험 기능 명시, 개수 8 유지)로 전환
- [ ] 확인 필요 — Step 7.3 을 보류

**`upstream` 인자**

- [ ] 제거
- [ ] `@keywords internal` 로 숨김 (용도가 확인되는 경우)

**결정:** JoonHo Lee (PI)  **일자:** 2026-08-14  — 기록: `001-pi-decisions-gate-B.md`
