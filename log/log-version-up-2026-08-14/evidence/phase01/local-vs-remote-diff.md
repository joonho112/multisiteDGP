# Step 1.1 — 로컬 작업본 vs 원격 `main` 차이 확정

- **실행일:** 2026-08-14
- **원격:** `https://github.com/joonho112/multisiteDGP` `main` @ `5239cf5`
- **로컬:** `multisiteDGP-R-package/` (git 미관리)
- **방법:** 원격을 세션 스크래치에 클론 후 원격 추적 파일 235 개를 로컬 대응 파일과 `cmp` 로 바이트 비교. 읽기 전용 — 어떤 파일도 복사·수정하지 않았다.

---

## 결론 (한 문장)

**로컬은 원격의 subset 이다.** 패키지 소스에 로컬 고유의 최신 변경은 없고, 차이 나는 9 개 파일은 전부 원격이 새것이다. 다만 로컬에는 원격이 추적하지 않는 **개발 자산 약 380 MB** 가 있다.

---

## 1. 원격 저장소 상태

| 항목 | 값 |
|---|---|
| 커밋 수 | **6** |
| HEAD | `5239cf5` (2026-05-09) |
| 추적 파일 | 235 |
| 작업 트리 크기 | 2.8 MB |
| `.git` 크기 | 736 KB |
| 태그 / 릴리스 | 0 |
| 이슈 | 0 |

**커밋 이력 전체:**

```
5239cf5 2026-05-09 fix(index): flatten HTML feature-card indentation so pandoc treats them as raw HTML
5d4e4fc 2026-05-09 ci: pin Suggests explicitly (multisitepower not on CRAN/r-universe)
a9f3c7c 2026-05-09 ci: install Suggests for R-CMD-check and test-coverage too
8f64a14 2026-05-09 ci(pkgdown-deploy): install Suggests so vignette site builds
bf2961e 2026-05-09 multisiteDGP v0.1.1 — initial public release
a8b4199 2026-05-09 Initial commit
```

::: 관찰
개발 이력이 git 에 없다. 2026-05-09 의 단일 squash 커밋(`bf2961e`)이 v0.1.1 전체를 담고 있고, 그 뒤 CI 수정 4 건이 전부다. build-implementation (127 로그) 과 documentation-updates (99 로그) 로 진행된 실제 개발 과정은 버전 관리 밖에서 이뤄졌다.
:::

**원격 추적 범위 (디렉터리별):**

| 디렉터리 | 파일 수 |
|---|---:|
| `tests/` | 91 |
| `man/` | 47 |
| `R/` | 35 |
| `vignettes/` | 18 |
| `inst/` | 15 |
| (루트) | 12 |
| `.github/` | 10 |
| `pkgdown/` | 7 |

`docs/`, `tools/`, `dev/`, `.lintr`, `.pre-commit-config.yaml`, `README.html` 은 **추적하지 않는다.**

---

## 2. 3 분류 결과

원격 추적 파일 235 개 대조:

| 분류 | 건수 |
|---|---:|
| **동일** | **226** |
| **양쪽 다 있으나 내용 다름** | **7** |
| **로컬에 없음** (원격에만) | **2** |
| **로컬에만 있음** (원격 미추적) | 별도 §4 |

### 2.1 내용이 다른 7 개 — 판정

| 파일 | 원격 최종 커밋 | 판정 | 근거 |
|---|---|---|---|
| `.github/workflows/R-CMD-check.yaml` | `5d4e4fc` | **원격 최신** | 원격에 `any::metafor` 외 Suggests 9 개 명시 설치 블록이 있고 로컬에는 없음. 로컬 mtime 2026-05-08 21:30 으로 커밋(05-09)보다 앞섬 |
| `.github/workflows/pkgdown-deploy.yaml` | `5d4e4fc` | **원격 최신** | 동일 |
| `.github/workflows/test-coverage.yaml` | `5d4e4fc` | **원격 최신** | 동일 |
| `CONTRIBUTING.md` | `bf2961e` | **원격 최신** | 원격은 공개용으로 정제됨. 로컬은 `documentation-updates/…/appendix-b-roxygen-template.qmd` 같은 내부 경로를 참조 |
| `vignettes/references.bib` | `bf2961e` | **원격 최신** | 원격 헤더는 공개용. 로컬 헤더는 내부 프로세스 노트(`documentation-updates/log/064_…` 참조, "PI-pending punch list" 규칙) |
| `.Rbuildignore` | `bf2961e` | **양방향 차이** | §2.2 참조 |

### 2.2 `.Rbuildignore` — 유일한 양방향 차이

**로컬에만 있는 12 개 항목** (로컬 전용 디렉터리를 build 에서 제외하기 위한 것):

```
^PACKAGES$   ^PACKAGES\.gz$   ^PACKAGES\.rds$
^Library$    ^Outputs$        ^documentation-updates$
^blueprint-multisiteDGP-book$
^blueprint-multisiteDGP-book-v1\.0-20260507\.zip$
^multisiteDGP-build-implementation-book$
^log$        ^dev$            ^tools$
```

**원격에만 있는 1 개 항목:**

```
^LICENSE\.md$
```

두 쪽 모두 정당하다. 로컬 항목은 로컬에 실제로 존재하는 디렉터리를 처리하고, 원격 항목은 원격에만 있는 `LICENSE.md` 를 처리한다. **병합(union)이 정답이며 충돌이 아니다.**

### 2.3 로컬에 없는 2 개

| 파일 | 내용 | 조치 |
|---|---|---|
| `.gitignore` | R/RStudio 캐시, macOS, `*.Rcheck/`, `*.tar.gz`, `/docs/`, `*_files/`, `README.html`, `vignettes/*.html`, `/_book/`, `/.quarto/` | 로컬로 가져온다 |
| `LICENSE.md` | MIT 전문 (GitHub 표시용). `LICENSE` 는 R 이 쓰는 2 줄 파일로 별도 존재 | 로컬로 가져온다 |

::: 관찰
원격 `.gitignore` 는 이미 `..Rcheck/`, `*.tar.gz`, `.DS_Store`, `/docs/` 를 무시한다. Step 1.4 의 위생 정리가 이 파일만 가져와도 상당 부분 해결된다.
:::

---

## 3. 로컬 → 원격 방향 변경: **없음**

패키지 소스(`R/`, `tests/`, `man/`, `vignettes/`, `inst/`, `DESCRIPTION`, `NAMESPACE`, `NEWS.md`, `README.*`, `_pkgdown.yml`, `index.md`, `pkgdown/`) 226 개 파일이 **바이트 단위로 동일하다.**

즉 2026-05-10 이후 3 개월간 **패키지 소스는 로컬에서 전혀 수정되지 않았다.** [Ch.1 A-1](../../001_plan-master-version-upgrade/_book/01-current-state-audit.html) 이 우려한 "3 개월의 미확인 격차" 는 **격차가 아니라 정지 상태**였다.

**함의.** git 복구가 병합 없이 단순해진다. 원격을 authoritative 로 삼고, 로컬의 `.Rbuildignore` 추가분만 얹으면 된다. Phase 1 Step 1.2 의 복구 경로 중 **"로컬 == 원격"** 에 해당한다 (엄밀히는 로컬이 9 개 파일에서 뒤처짐).

---

## 4. 로컬 전용 자산 (원격 미추적)

| 항목 | 크기 | 파일 수 | 성격 |
|---|---:|---:|---|
| `blueprint-multisiteDGP-book/` | 122 MB | 2,093 | 설계 청사진 25 장 + `_book/` 렌더본 |
| `multisiteDGP-build-implementation-book/` | 124 MB | 2,076 | 구현 계획 + 로그 127 건 |
| `documentation-updates/` | 98 MB | 1,697 | 문서 오버홀 계획 + 로그 99 건 |
| `tools/` | 24 MB | 274 | **traceability ledger 8 종, validation 결과 ~180 건, 정책 문서 3 종** |
| `docs/` | 14 MB | 277 | pkgdown 빌드 산출물 (원격은 `.gitignore` 로 제외, gh-pages 배포) |
| `log/` | 4.5 MB | 109 | 이번 작업 공간 + 2026-05 이전 draft 3 건 |
| `multisiteDGP.Rcheck/` | 3.5 MB | 387 | check 잔재 |
| `README.html` | 1.3 MB | 1 | 렌더 산출물 |
| `dev/` | 4 KB | 1 | `NEWS-internal.md` |
| `.lintr`, `.pre-commit-config.yaml` | 8 KB | 2 | 개발 설정 (원격 미추적) |
| `Library/`, `Outputs/` | 0 B | 0 | 빈 디렉터리 |
| `..Rcheck/` | 4 KB | 1 | 다른 경로에서 실행된 check 잔재 |
| `blueprint-…-v1.0-20260507.zip` | 41 MB | 1 | 위 blueprint book 의 zip 사본 (중복) |
| `multisiteDGP_0.0.0.9000.tar.gz`, `_0.1.0.tar.gz` | 각 421 KB | 2 | 과거 빌드 |

**합계 약 433 MB.** 이 중 `tools/` (24 MB) 는 **버전 관리가 필요하다** — traceability ledger 와 validation 결과는 이번 작업이 직접 수정하는 대상이고, `test-traceability.R` 이 참조한다. 세 book 폴더 (344 MB) 는 이력 자산이며 별도 처리가 필요하다.

::: {.callout-warning}
`.lintr` 과 `.pre-commit-config.yaml` 이 원격에 추적되지 않는다. CI 의 `lint` workflow 가 `.lintr` 설정에 의존한다면, **원격에서 lint 가 로컬과 다른 규칙으로 실행되고 있었다.** Phase 2 Step 2.1 의 lint 실패 원인 후보다.
:::

---

## 5. 산출물

| 파일 | 내용 |
|---|---|
| `local-vs-remote-diff.md` | 이 문서 |
| `remote-main-log.txt` | 원격 커밋 6 건 전문 + `--stat` |
| `diff-different-files.txt` | 내용이 다른 7 개 파일 목록 |
| `diff-missing-locally.txt` | 로컬에 없는 2 개 파일 목록 |
| `diff-remote-vs-local.patch` | 6 개 파일의 unified diff 전문 |

---

## 6. Step 1.2 로 넘기는 것

1. **복구 경로:** "로컬 == 원격 (로컬이 9 개 파일에서 뒤처짐)" → 원격 클론 후 로컬 전용 자산을 얹는 단순 경로
2. **`.Rbuildignore` 병합:** union (로컬 12 개 + 원격 1 개)
3. **추적 범위 결정 필요:** `tools/`, `log/`, `dev/`, `.lintr`, `.pre-commit-config.yaml`, 세 book 폴더, `docs/` 를 각각 추적할지 — **PI 결정 사항**
4. **`.lintr` 미추적 문제:** Phase 2 Step 2.1 의 lint 실패 원인 후보로 원장 등재 예정
