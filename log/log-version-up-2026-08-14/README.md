# `log-version-up-2026-08-14/` — multisiteDGP v0.1.1 → v0.2.0 작업 공간

이 폴더는 `multisiteDGP` R package 의 **version upgrade 작업** 공간이다. 계획 · 로그 · 결정 · 증거가 여기 쌓인다. package 본체는 두 단계 위(`multisiteDGP-R-package/`)에 있다.

## 작업 성격

**v0.2.0 = 신뢰성 릴리스.** 새 기능을 추가하지 않고, 이미 있는 표면을 믿을 수 있게 만든다.

2026-08-14 진단에서 확인된 것: package 의 **통계 커널과 문서는 건강하지만**, 그것을 지탱하는 **공학 기반이 무너져 있다.**

- 로컬 작업본이 git repository 가 아니다 (원격은 PUBLIC, 마지막 push 2026-05-10)
- CI 가 3 개월째 red — `extended-tests` 는 10 주 연속 실패, `pkgdown-deploy` 만 green
- 기본 `devtools::test()` 가 통계 불변량 30 건을 skip 한다 (게이트를 켜면 전부 통과)
- golden fixture 가 macOS 에서 생성됐는데 정책은 Linux 를 authoritative 로 규정
- `DESCRIPTION` 이 미구현 DPM 을 8 번째 분포로 광고한다

## 구조

| 경로 | 역할 |
|---|---|
| `001_plan-master-version-upgrade/` | **master plan (Quarto Book)** — 진단 · 목표 · 실행 규약 · 11 Phase 로드맵 · 인수 기준 · 부록 |
| `001_plan-master-version-upgrade/_book/` | 렌더된 계획서 (`index.html` 부터) |
| `NNN_phaseNN-*.qmd` / `.html` | Phase 종료 로그 (`002` – `012` 예정) |
| `defect-ledger.csv` | 결함 원장 — Phase 2 산출, Phase 4·5·7·10 소비 |
| `decisions/` | Phase 3 의 D1–D4 결정 문서 + PI 승인 패킷 |
| `evidence/` | 감사 원자료 (Phase 별 하위 폴더) |
| `artifacts/` | 되돌리기용 보존 (git 복구 전 백업, Phase 4 이전 fixture, 2026-05 validation 결과) |
| `review-packet/` | Phase 10 외부 리뷰 패킷 + 발견 + 종료 |
| `CLOSEOUT.md` | 작업 종료 시 인계 문서 (Phase 11) |

## Phase 개요

11 Phase, 67 Step, agent 실행 시간 ~21 시간 (CI · 리뷰 대기 제외).

| Phase | 제목 | Step | 로그 | 게이트 |
|------:|---|---|---|:---:|
| 1 | 작업 기반 복구 | 1.1–1.5 | `002` | |
| 2 | 결함 인벤토리 감사 | 2.1–2.9 | `003` | |
| 3 | v0.2.0 범위 결정 | 3.1–3.5 | `004` | **PI** |
| 4 | 재현성 계약 재구축 | 4.1–4.7 | `005` | |
| 5 | 테스트 스위트 재구축 | 5.1–5.8 | `006` | |
| 6 | CI 복구 | 6.1–6.6 | `007` | |
| 7 | 기능 결함 수정 | 7.1–7.7 | `008` | |
| 8 | 통계 검증 재실행 | 8.1–8.5 | `009` | |
| 9 | 문서 델타 동기화 | 9.1–9.5 | `010` | |
| 10 | 외부 리뷰 게이트 | 10.1–10.5 | `011` | **외부** |
| 11 | v0.2.0 릴리스 | 11.1–11.5 | `012` | |

Phase 4 · 5 · 7 은 병렬 가능하되 Phase 6 은 셋이 모두 끝난 뒤에 시작한다.

## 어디부터 읽나

- **PI, 계획 검토** → `001_plan-master-version-upgrade/_book/index.html` (Preface) → Ch.1 진단 → Ch.2 목표 → Phase 장 → Ch.16 인수 기준
- **Step 을 실행하는 agent** → 해당 Phase 장 + Ch.3 실행 규약 + Ch.4 로그 규약 + `defect-ledger.csv`
- **휴식 후 복귀** → 이 폴더의 가장 최근 `NNN_*.html` 로그
- **작업 종료 후** → `CLOSEOUT.md`

## 순번 규칙

3 자리 zero-padded, 폴더 전체에서 단조 증가. `001` 은 계획서. 번호는 **파일 생성 시점의 최대 번호 + 1** 로 정하며, ad-hoc 로그가 끼면 이후 Phase 로그 번호가 밀린다. 번호를 비우거나 재사용하지 않는다. 상세는 계획서 Ch.4.

## 상태

| | |
|---|---|
| **현재 단계** | 계획 수립 완료 — **PI 승인 대기** |
| **다음** | 승인 후 Step 1.1 (로컬 vs 원격 차이 확정) |
| **작성** | 2026-08-14, Claude Opus 5 |
