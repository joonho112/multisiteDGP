# JEBS 재현 증거 — v0.2.0 기준

측정일 2026-08-15 · 계획서 Phase 8 Step 8.4

이 문서는 하나에 답한다. **패키지가 Lee et al. (2025, JEBS) 부록의 계산을 재현하는가.**

---

## 재현 사슬

재현 주장은 비교의 양쪽이 독립일 때만 의미가 있다. 한쪽이 다른 쪽을 호출하면 자기 자신과 비교하는 것이다. 이 사슬은 독립이다.

| 단계 | 무엇 | 패키지 의존 |
|---|---|---|
| 원천 | `2024-01-21_JEBS_Software_Appendix_E.qmd` | — |
| 참조 구현 | `tools/jebs-golden-fixtures/generate-jebs-golden-fixtures.R` 의 `jebs_prior_g_mixture()` · `jebs_tau_j_hat()` | **없음** — `stats::runif` · `rnorm` · `rgamma` · `base::sample.int` 만 |
| 정규화 | 같은 스크립트의 `normalize_for_multisiteDGP()` | **없음** — `z_j = tau_j / sigma_tau`, `se_j = sqrt(se2_j)` 순수 산술 |
| 패키지 구현 | `sim_multisite(preset_jebs_strict(), seed = ...)` | 전부 |
| 비교 | `tests/testthat/test-T1a-jebs-bit-identical.R` | — |

스크립트가 `pkgload::load_all()` 을 부르긴 하지만 **`canonical_hash()` 하나 때문**이다(스크립트 27 행의 stop 메시지가 그렇게 명시한다). 데이터를 만드는 경로에는 패키지가 없다.

### 원천 무결성

부록 QMD 의 SHA256 이 fixture 생성 시점에 기록된 값과 **일치**한다.

```
실측    5f600e0d09fac45f2a2a1bae10615dcfaceab736866f127aa54acd0353fd400b
manifest 5f600e0d09fac45f2a2a1bae10615dcfaceab736866f127aa54acd0353fd400b
```

부록이 바뀌지 않았으므로 fixture 가 여전히 그 부록을 대표한다.

---

## 결과 — 데이터는 비트 단위로 일치한다

`preset_jebs_strict()` 대 참조 구현, 4 seed × 7 열 전수 `identical()` 비교:

| seed | 열 | bit-identical |
|---|--:|---|
| 42 | 7 | **TRUE** |
| 1 | 7 | **TRUE** |
| 2024 | 7 | **TRUE** |
| 12345 | 7 | **TRUE** |

열은 `site_index` · `z_j` · `tau_j` · `n_j` · `se_j` · `se2_j` · `tau_j_hat`.

`all.equal()` 이나 허용오차 비교가 아니라 **`identical()`** 이다. 마지막 비트까지 같다.

### 플랫폼

T1a 는 v0.2.0 에서 플랫폼 게이트 없이 실행된다(D-029). CI run 31889543618 에서 5 cell 전부 통과했다.

| cell | 결과 |
|---|---|
| `linux-release` · `linux-devel` · `linux-oldrel` | success |
| `macos-release` | success |
| `windows-release` | success |

fixture 는 `aarch64-apple-darwin23` 에서 생성됐고 Linux x86_64 와 Windows 에서 같은 `canonical_hash()` 를 낸다. **생성 플랫폼은 이제 아무 의미가 없다.**

---

## Phase 4·5·7 이 수치를 바꿨는가 — 바꾸지 않았다

이번 업그레이드는 solver 를 양자화하고, 해시 스키마를 v3 로 바꾸고, 오류 경로를 여러 곳 고쳤다. 그중 어느 것도 생성 데이터를 움직이지 않았다는 것이 위 bit-identical 결과다.

### 겉보기 차이 하나를 해명한다

V12 의 2026-05 결과 CSV 와 현재 출력을 비교하면 1e-16 수준 차이가 보인다.

| 열 | 최대 절대차 | 최대 상대차 |
|---|--:|--:|
| `tau_j` | 5.00e-16 | 3.08e-15 |
| `tau_j_hat` | 1.55e-15 | 2.75e-15 |
| `se_j` | 5.00e-16 | 1.45e-15 |
| `se2_j` | 4.44e-16 | 4.33e-15 |
| `site_index` · `n_j` | 0 | 0 |

이것은 **수치 변화가 아니라 CSV 직렬화 손실**이다. `write.csv()` 는 double 을 15 유효숫자로 적으므로 왕복 상대오차가 최대 5e-15 다 — 관측된 범위와 정확히 맞는다. 정수 열(`site_index`, `n_j`)만 차이가 0 인 것도 같은 설명이다.

**결정적 근거는 golden `.rds` 다.** `.rds` 는 double 을 바이트 그대로 저장하므로 직렬화 손실이 없고, 그 비교가 위 표대로 `identical()` = TRUE 다. CSV 를 근거로 "값이 바뀌었다" 고 말할 수 없다.

---

## V12 의 박힌 해시는 갱신이 필요하다

`run-v12-validation.R` 이 `expected_hash <- "a96eaabd1c022e32"` 를 들고 있다. 현재 값은 `dab943488dc9d8ae` 다.

**이 불일치는 회귀가 아니다.** `canonical_hash()` 의 payload 에는 스키마 버전이 들어가고, Phase 4 가 스키마를 v1 에서 v3 로 바꿨다(파생 진단을 payload 에서 제거 — 결함 D-002). 스키마가 바뀌면 데이터가 그대로여도 모든 해시가 움직인다. 데이터가 그대로라는 것은 위에서 `.rds` 로 증명했다.

재현성 정책은 *"해시 스키마가 문서화된 결정과 `NEWS.md` 항목 없이 바뀌면"* 릴리스 차단이라고 규정한다. 결정은 문서화되어 있고(Phase 4 로그, D-002), `NEWS.md` 항목은 Phase 9 배정분이다.

---

## manifest 갱신

`jebs-golden-fixture-manifest.csv` 의 `os_policy` 필드가 v0.1 정책 문구를 담고 있었다.

```
이전  Linux x86-64 strict; macOS/Windows numerical equivalence per blueprint ch18 sec18.12
현재  portable canonical_hash; no platform hierarchy; verified on linux-release/devel/oldrel,
      macos-release, windows-release (run 31889543618)
```

v0.2.0 재현성 정책은 플랫폼 위계를 명시적으로 폐기했고, D-029 가 그 폐기를 CI 로 검증했다. 이전 문구는 이제 사실이 아니다.

`generate-jebs-golden-fixtures.R` 의 같은 문자열도 함께 고쳤다. **해시 4 쌍(`canonical_hash`, `rds_sha256`)은 건드리지 않았고, 갱신 후 재확인했다.**

---

## 남는 한계 — Figure 4(b)

`NEWS.md` v0.1.0 의 *"Exact reproduction of JEBS Figure 4(b) is deferred until digitized targets become available"* 는 **여전히 유효하다.**

V12 의 주석이 이유를 정확히 적고 있다: Lee Figure 4(b) 의 lme4 모형은 **개인 수준 입력**이나 **디지타이즈된 그림 목표값**을 요구하는데 둘 다 저장소에 없다. V12 는 그래서 정성적 축소(shrinkage) 패턴 증거만 수용 기준으로 삼는다.

이 한계는 패키지 결함이 아니라 **참조 데이터 부재**다. 부록의 시뮬레이션 계산은 위에서 보듯 비트 단위로 재현된다. Figure 4(b) 는 그 시뮬레이션이 아니라 실증 데이터 적합이다.

Phase 9 의 `NEWS.md` v0.2.0 항목에서 다음 세 한계는 **해소됨**으로 갱신할 수 있다.

| v0.1.0 한계 서술 | 현재 |
|---|---|
| "golden fixture manifest 가 macOS/aarch64 provenance 를 기록한다. Linux x86_64 baseline 은 첫 CI 실행 대기 중" | **해소** — 5 cell 전부 통과, 생성 플랫폼 무의미 |
| "legacy site-size engine 의 cross-OS bit-parity 는 로컬 검증만 되어 있고 GitHub Actions 를 통과하지 않았다" | **해소** — run 31889543618 |
| "Exact reproduction of JEBS Figure 4(b) is deferred" | **유지** — 참조 데이터 부재 |

---

## 재실행

```sh
Rscript tools/jebs-golden-fixtures/generate-jebs-golden-fixtures.R   # fixture 재생성 (의도가 있을 때만)
Rscript -e 'testthat::test_file("tests/testthat/test-T1a-jebs-bit-identical.R")'
```

fixture 재생성은 **데이터를 움직일 의도가 있을 때만** 한다. 의도하지 않은 fixture diff 는 갱신이 아니라 회귀다.
