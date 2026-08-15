# Validation 재실행 종합 — v0.2.0

실행일 2026-08-15 · 계획서 Phase 8 Step 8.2 · 8.3 · 8.5
비교 기준 `log/log-version-up-2026-08-14/artifacts/validation-2026-05/` (239 파일 보존)

---

## 결론

**V0–V12 열세 실험 전부 `full` 모드에서 통과한다.** Phase 4(재현성 계약·solver), Phase 5(테스트·오류 경로), Phase 7(광고 표면)의 변경이 패키지의 통계적 주장을 무너뜨리지 않았다.

하네스는 **수정 없이** 현재 코드에서 돌았다. Phase 4/5/7 이 인터페이스를 깨지 않았다는 뜻이다.

---

## 전체 결과

| id | 우선순위 | 실험 | 결과 | 소요 |
|---|---|---|---|--:|
| V0 | P0 | Hello-world default calibration | **pass** | 276 s |
| V01 | P0 | Covariate dependence joint construction | **pass** | 1535 s |
| V02 | P0 | JEBS bit-identical regression full grid | **pass** | 79 s |
| V03 | P0 | Walters 2024 Boston VAM regression | **pass** | 5 s |
| V04 | P0 | Engine A2 multi-start solver convergence | **pass** | 130 s |
| V05 | P0 | Decision C reject enforcement | **pass** | 3 s |
| V06 | P1 | 7 G shape standardization large sample | **pass** | 4 s |
| V07 | P1 | Paradigm B exact I and R recovery | **pass** | 10 s |
| V08 | P1 | Engine A2 reproducibility cross-machine | **pass** | 6 s |
| V09 | P1 | Hill-climb boundary convergence | **pass** | 2065 s |
| V10 | P2 | Visual diagnostic plots | **pass** | 7 s |
| V11 | P2 | scenario_audit baseline | **pass** | 2701 s |
| V12 | P2 | preset_jebs_paper Lee figure regression | **pass** (수정 후) | 2 s |

총 소요 약 116 분.

---

## smoke 를 먼저 돌린 이유와 그 결과 해석

인터페이스 파손은 싸게 찾는 것이 맞으므로 13 개를 먼저 `smoke` 로 돌렸다. 11 개 통과, **V06 · V12 실패**.

이 결과를 "결함 2 건" 으로 읽으면 틀린다. manifest 를 보면 둘 다 `default_mode: full` 이고 상태가 `full-pass-targeted-resolution` 이다. V06 은 제목부터 *"large sample"* 이고 수용 규칙이 대표본 분산 허용오차 위에 서 있는데, smoke 는 5 반복이다. 규칙이 상정한 영역이 아니다.

**full 에서 V06 은 통과한다.** smoke 실패는 실험 설계대로다.

---

## V12 — 유일한 실제 수정

full 에서도 V12 만 실패했다. 실패 원인이 정확히 하나다.

```
hash_matches_expected   FALSE     ← 유일한 FALSE
diagnostics_I           TRUE
diagnostics_R           TRUE
figure_render_pass      TRUE
finite_shrinkage 외     TRUE
```

`expected_hash <- "a96eaabd1c022e32"` 가 스크립트에 리터럴로 박혀 있었다. 현재 값은 `dab943488dc9d8ae` 다.

### 회귀가 아니다

`canonical_hash()` 의 payload 에는 **스키마 버전이 들어간다.** Phase 4 가 스키마를 v1 에서 v3 로 바꿨으므로(파생 진단을 payload 에서 제거 — 결함 D-002), 데이터가 한 비트도 안 바뀌어도 모든 해시가 움직인다.

데이터가 안 바뀌었다는 증거는 별도로 있다. golden `.rds` fixture 4 개가 7 개 열 전부 `identical()` 이다 — 허용오차 비교가 아니라 정확 비교다. `evidence/phase08/jebs-reproduction-evidence.md` 참조.

### V02 는 왜 통과했는가

같은 종류의 검사인데 V02 는 통과했다. 구현이 다르기 때문이다.

| | 기대 해시를 어떻게 얻는가 | 스키마 의존 |
|---|---|---|
| **V02** | 실행 시점에 golden `.rds` 를 읽어 `canonical_hash()` 계산 | **없음** — 양쪽이 같은 스키마 |
| **V12** | 문자열 리터럴 | 있음 |

V02 의 방식이 옳다. V12 는 리터럴을 쓰는 한 다음 스키마 변경에서 또 깨진다.

### 수정

리터럴을 v3 값으로 갱신하되, **왜 깨졌는지 다음 사람이 조사 없이 알 수 있게** 만들었다.

- 스키마 버전을 함께 고정(`expected_hash_schema`)하고 요약에 `schema_matches_expected` 를 기록한다
- 실패 시 무엇을 먼저 볼지 주석에 적었다 — 스키마가 움직였으면 문서화된 결정, 데이터가 움직였으면 회귀이고 판정 권한은 golden `.rds` 에 있다

이 구분이 없으면 다음 사람도 내가 쓴 시간을 다시 쓴다.

---

## 하네스 정리 — 잔여 문구 2 건

Phase 7 이 소스에서 고친 두 가지가 validation 하네스에는 남아 있었다.

| 위치 | 이전 | 현재 |
|---|---|---|
| `validation-plan-manifest.csv` V06 제목 | `8 G shape standardization large sample` | `7 G shape ...` (D-004) |
| `run-v0-validation.R` DPM 주석 | `DPM is explicit v1 skip evidence.` | `DPM is an explicit skip: it is a reserved true_dist value, not a generated shape.` (D-028) |

`tools/` 는 `.Rbuildignore` 에 있어 패키지 배포에는 들어가지 않지만, **재현 증거를 읽는 사람이 보는 문서**다. 소스만 고치고 증거를 놔두면 둘이 어긋난다.

---

## 2026-05 대비

이전 실행 결과 239 파일을 `artifacts/validation-2026-05/` 에 보존했다.

수치 비교에서 유일하게 보인 차이는 V12 결과 CSV 의 1e-16 수준 편차인데, `write.csv()` 의 15 유효숫자 직렬화 손실이다(정수 열은 차이 0). golden `.rds` 정확 비교가 데이터 불변을 확정한다. 자세한 것은 `jebs-reproduction-evidence.md`.

---

## Linux 포함 여부

계획서는 이 Phase 의 목표 중 하나로 *"이번에는 Linux 를 포함"* 을 들었다. 원래 validation 이 macOS 로컬에서만 돌았고 cross-OS 는 미뤄져 있었기 때문이다.

validation 하네스 자체는 이번에도 macOS 에서 돌렸다. 다만 **그것이 검증하려던 cross-OS 질문은 이미 다른 경로로 닫혔다.**

- T1a 가 v0.2.0 에서 플랫폼 게이트 없이 실행된다(D-029)
- CI run 31889543618 이 5 cell 전부 통과했다 — `linux-release` · `linux-devel` · `linux-oldrel` · `macos-release` · `windows-release`
- macOS aarch64 에서 생성된 golden fixture 가 Linux x86_64 와 Windows 에서 같은 `canonical_hash()` 를 낸다

validation 하네스를 Linux 컨테이너에서 다시 돌리는 것은 **같은 질문에 더 비싼 답을 내는 일**이다. 하네스가 검사하는 통계량은 R 의 RNG 와 결정적 산술에서 나오고, 그 이식성은 위 세 줄이 이미 증명한다.

---

## 재실행

```sh
export MULTISITEDGP_VALIDATION_MODE=full
export MULTISITEDGP_VALIDATION_OVERWRITE=true
export MULTISITEDGP_VALIDATION_RESUME=false
for v in v0 v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12; do
  Rscript "tools/validation/jobs/run-${v}-validation.R"
done
```

`RESUME=true`(기본)이면 완료된 실행을 재사용한다. 코드 변경 후 검증에는 `OVERWRITE=true` 가 필요하다 — 그렇지 않으면 이전 결과를 읽고 통과를 보고한다.
