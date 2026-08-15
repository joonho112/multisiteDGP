# v0.2.0 문서 델타 목록

작성 2026-08-15 · 계획서 Phase 9 Step 9.1
근거 결함 원장 36 행(fixed 34) + Phase 4·5·7·8 로그

---

## 판정 기준

이 목록에 넣는 것은 **사용자가 코드나 결과에서 마주치는 변경**뿐이다. 원장의 34 건 중 상당수는 개발 인프라(CI · lint · git · 테스트 게이트)이고, 그것들은 패키지를 쓰는 사람에게 보이지 않는다.

| 범주 | 원장 건수 | 사용자 대면 |
|---|--:|--:|
| 개발 인프라 (`infra`) | 13 | 0 |
| 테스트 (`test`) | 3 | 0 |
| 커널 동작 (`kernel`) | 3 | **3** |
| 표면 (`surface`) | 3 | **3** |
| 오류 (`errors`) | 3 | **3** |
| 진단 (`diagnostics`) | 2 | **2** |
| 재현성 (`repro`) | 3 | **3** |
| Layer 1 (`layer1`) | 1 | **1** |
| 그림 (`plots`) | 1 | **1** |
| 문서 (`docs` · `documentation`) | 2 | **2** |
| 의존성 (`deps`) | 1 | **1** |

사용자 대면 **19 건**. 아래에서 이것만 다룬다.

---

## 1. Breaking change — 사용자가 조치해야 하는 것

### 1.1 `gen_effects()` 의 `upstream` 인자 제거 (D-018)

Layer 1 은 파이프라인의 **시작**이므로 upstream 이 없다. 이 인자는 비-`NULL` 이면 언제나 abort 했으므로 실제로 동작한 적이 없다.

- **영향**: `gen_effects(..., upstream = NULL)` 을 명시적으로 쓰던 코드가 `unused argument` 로 실패한다
- **조치**: 인자를 지운다
- **주의**: `gen_site_sizes()` · `gen_se_direct()` · `align_*_corr()` · `gen_observations()` 의 **첫 위치 인자 `upstream` 은 그대로다.** 이름이 같을 뿐 다른 것이며, 실제 layer 간 데이터 흐름이다

### 1.2 `canonical_hash()` 값이 전부 바뀐다 — 스키마 v3 (D-002 · D-007)

해시 payload 에서 **파생 진단**(`I_hat` · `R_hat` · `rho_S_*` · `rho_P_*` · `sigma_tau_*`)을 제거하고, 남은 double 을 9 유효숫자로 반올림한다.

- **영향**: v0.1.x 에서 기록한 해시가 v0.2.0 에서 재현되지 않는다. **데이터는 바뀌지 않았다** — 해시가 무엇을 보는지가 바뀐 것이다
- **조치**: 원고·로그·이슈에 적힌 해시를 v0.2.0 에서 다시 계산해 갱신한다
- **얻는 것**: 해시가 **이식 가능**해졌다. 같은 설계·같은 seed 면 어느 플랫폼에서도 같은 해시가 나온다. v0.1 의 "Linux 가 기준, macOS/Windows 는 면제" 구조가 사라졌다
- 특정 진단을 의도적으로 고정하고 싶으면 `canonical_hash(dat, diagnostics_to_include = "I_hat")`

### 1.3 `scenario_audit()` 출력에 `target_source` 열 추가 (D-031)

- **영향**: 열 위치로 접근하거나 열 개수를 검사하는 코드
- **조치**: 이름으로 접근한다

---

## 2. 동작 변경 — 조치는 필요 없지만 알아야 하는 것

### 2.1 `scenario_audit()` 이 7 종 전부에서 동작한다 (D-031)

v0.1.x 에서는 `true_dist` 가 `SkewN` · `ALD` · `Mixture` · `PointMassSlab` 이면 **감사 전체가 abort** 했다. 7 종 중 4 종에서 쓸 수 없었다.

이제 Group A · B · D 는 정상 판정하고, Group C(분포 적합)만 건너뛴다. `target_source` 가 `"auto"` 인지 `"not_available"` 인지 알려준다.

**`pass` 를 읽는 방식이 좁아진다.** `pass = TRUE` 는 *돌 수 있었던* 게이트가 전부 통과했다는 뜻이지, 모든 게이트가 돌았다는 뜻이 아니다. shape 를 섞은 그리드를 쓸어 `pass == TRUE` 로 거를 때는 `target_source` 를 함께 봐야 한다.

### 2.2 이름 붙인 `beta` 가 절편 생략을 허용한다 (D-035)

```r
gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
            beta = c(prior = 0.3), data = d)
```

v0.1.x 에서 이 호출은 *"`beta` is missing non-intercept coefficients. Missing coefficients: (Intercept)."* 로 거부됐다. 절편을 비-절편 계수라 부르는 자기모순이었고, 이름 없는 `beta` 는 같은 경우를 허용하고 있었다. 이제 절편을 생략하면 0 으로 기본값을 잡는다 — 이름 있는 쪽과 없는 쪽이 같아졌다.

### 2.3 Engine A2 의 검증 허용오차가 유도된다 (D-024 · D-022)

post-solve tolerance 가 고정 `1e-6` 에서 **잔차를 실제로 평가할 수 있는 정밀도**에서 유도된 값으로 바뀌었다.

- 같은 설계가 macOS 에서 성공하고 Linux 에서 abort 하던 문제가 사라졌다
- 조건수가 나쁜 영역에서 이전에 거부되던 설계 일부가 이제 통과한다 — 120 칸 격자에서 117 칸 성공
- `cv` 가 약 `1e-3` 아래면 실현 SD 를 그 정밀도까지만 확인할 수 있다는 경고가 나온다. 설계는 그대로 실행된다

### 2.4 Student-t 경고 경계 정정 (D-011)

초과첨도는 \(6/(\nu-4)\) 로 \(\nu \le 4\) 에서 발산한다. v0.1.x 는 `nu < 4` 에서 경고하며 **`nu >= 4` 를 쓰라고 안내**했다 — 첨도가 여전히 무한대인 값이다.

- 경계가 `nu <= 4` 로, 안내가 `nu > 4` 로 바뀌었다
- **수치 동작은 그대로다.** 경고 조건과 문구만 바뀌었다

### 2.5 `plot_effects()` 의 Y축 라벨 (D-036)

caterpillar 의 사이트 라벨이 `J >= 30` 에서 겹쳐 읽을 수 없었다. 효과 크기 순서를 유지한 채 최대 25 개로 솎아낸다. **데이터 행은 전부 그대로 그린다** — 축 라벨만 준다.

---

## 3. 광고 축소 — D2 결정

### 3.1 카탈로그는 7 종이다 (D-004)

`DESCRIPTION` · `README` · `_pkgdown.yml` · vignette 7 편 · roxygen 17 곳이 **여덟** 이라고 말했다. 실제로는 일곱이다.

- **생성되는 7 종**: Gaussian · Student-t · skew-normal · asymmetric Laplace · two-component mixture · point-mass slab · user callback
- **`"DPM"`**: `true_dist` 가 문자열은 받지만 abort 하는 **예약 슬롯**. `g_fn` 브리지로만 도달

vignette `m2` 는 처음부터 일곱 개만 그리고 있었다(플롯 7 개, overlay 캡션이 *"seven shapes"*). 산문만 여덟이라고 말했다.

### 3.2 Engine A2 의 실현 가능 영역이 문서화됐다 (D-012)

`gen_site_sizes()` roxygen 에 실측 표를 넣었다. 경계는 **`n_min / nj_mean` 비율에만** 의존한다 — `nj_mean` 자체는 들어오지 않는다(비율 0.2 에서 `nj_mean` 10~500 전부 상한 `cv = 1.10`).

### 3.3 preset 개수·패러다임 정정 (D-030)

vignette `a7` 이 direct-precision preset 을 하나로 셌다. 실제로는 둘(`preset_meta_modest`, `preset_small_area_estimation`)이고, `m8` 은 총수를 여덟로 셌다 — 아홉이다.

---

## 4. 오류 메시지

### 4.1 존재하지 않는 버전을 말하지 않는다 (D-028)

*"not supported in v1.0"* · *"deferred to v2"* 같은 문구가 5 곳에 있었다. 패키지 버전은 `0.1.1` → `0.2.0` 이므로 `0.2.0` 사용자가 자기에게 해당하는 말인지 알 수 없었다. `this release` / `a future release` 로 바꿨다.

### 4.2 32 비트를 넘는 정수 인자 (D-032)

`multisitedgp_design(seed = 2^40)` 이 *"missing value where TRUE/FALSE needed"* 라는 `simpleError` 를 냈다. 이제 다른 모든 잘못된 인자와 같은 `multisitedgp_arg_error` 를 내고 범위를 말해 준다. `J` · `seed` · `max_iter` · `n_min` · `M` 에서 재현됐다.

### 4.3 fix 라인 규약 (D-033)

여섯 메시지가 `Call` · `Return` · `Check` 로 시작했다. 규약은 `Try|Use|Pass|Remove` 이고 나머지 223 곳이 지키고 있었다. 여섯을 맞췄다.

---

## 5. 의존성

### 5.1 `multisitepower` 는 `Suggests` 가 아니다 (D-010 · D-009)

GitHub 에만 있고 CRAN 에 없어서, 선언해 두면 해결하지 못하는 환경에서 **설치 자체가 실패**했다. 실제로 macOS·Windows CI 가 석 달간 여기서 죽었다.

- `as_multisitepower()` 는 그대로 동작한다. **선언을 뺀 것이지 지원을 뺀 것이 아니다**
- 어댑터가 없을 때 내는 오류가 설치 방법을 알려준다 — `remotes::install_github("jche/multisitepower")`

### 5.2 `hedgehog` 제거 (D-021)

선언만 되고 쓰이지 않던 유령 의존성이었다.

---

## 6. 해소된 "Known limitations" 3 건

PI 결정: **v0.1.0 섹션은 그대로 두고**(당시의 주장을 기록하는 것이 히스토리의 역할) v0.2.0 항목에서 해소를 명시한다.

| v0.1.0 서술 | 판정 | 근거 |
|---|---|---|
| golden fixture manifest 가 macOS/aarch64 provenance 를 기록한다. Linux x86_64 baseline 은 첫 CI 실행 대기 중 | **해소** | CI run 31889543618 5 cell 통과. 생성 플랫폼이 무의미해졌다 |
| legacy site-size engine 의 cross-OS bit-parity 는 로컬 검증만 되어 있고 GitHub Actions 를 통과하지 않았다 | **해소** | 같은 run. T1a 가 플랫폼 게이트 없이 실행된다 |
| funnel · forest plot 의 수동 시각 검토 대기 | **해소** | 2026-08-15 검토 완료. funnel·dependence 정상, caterpillar 는 D-036 로 수정 |
| Exact reproduction of JEBS Figure 4(b) is deferred | **유지** | 참조 데이터 부재 — 개인 수준 입력이나 디지타이즈된 목표값이 저장소에 없다 |
| `multisitepower` 가 소프트 `Suggests` 다 | **서술 변경** | 이제 `Suggests` 가 아니다 (§5.1) |

Figure 4(b) 는 패키지 결함이 아니다. 부록의 **시뮬레이션** 계산은 비트 단위로 재현된다(4 seed × 7 열 `identical()`). Figure 4(b) 는 그 시뮬레이션이 아니라 실증 데이터 적합이다.

---

## 7. `DESCRIPTION` 변경

| 필드 | 이전 | 현재 |
|---|---|---|
| `Version` | `0.1.1` | `0.2.0` (D4 결정) |
| `Description` | "eight distribution shapes ... and a Dirichlet-process-mixture bridge" | "seven distribution shapes ... and a user callback" (Step 7.3 에서 완료) |
| `Suggests` | `hedgehog`, `multisitepower` 포함 | 둘 다 제거 (Phase 6 에서 완료) |
| `RoxygenNote` | — | `document()` 후 자동 |

---

## 8. 이 목록에 **넣지 않은** 것

계획서가 경고한 실패 모드는 *"Phase 9 가 세 번째 문서 오버홀이 되는 것"* 이다. 아래는 델타가 아니므로 건드리지 않는다.

- 개발 인프라 13 건 — CI · lint · git · 워크플로. 사용자에게 보이지 않는다. `CONTRIBUTING.md` 대상이지 `NEWS.md` 대상이 아니다
- 테스트 3 건 — 게이트 제거, 커버리지 보강. 개발자 대상
- 문서를 읽다 눈에 띈 개선 아이디어 — v0.3 백로그

---

## 9. 남은 open 결함 2 건

| id | 제목 | 릴리스 영향 |
|---|---|---|
| D-013 | tarball 9.2 MB 중 `inst/doc` 이 압축 전 13.7 MB | Step 9.3 에서 판정 |
| D-020 | vignette knit 중 경고 16 건 | Step 9.3 에서 판정 |
