# Step 1.4 — 저장소 위생 정리 기록

- **실행일:** 2026-08-14
- **PI 결정:** zip 삭제, 나머지는 `artifacts/` 로 이동 (2026-08-14 대화형 확인)

| 항목 | 크기 | 조치 | 근거 |
|---|---:|---|---|
| `blueprint-multisiteDGP-book-v1.0-20260507.zip` | 41 MB | **삭제** | 같은 내용의 `blueprint-multisiteDGP-book/` 폴더가 2,093 파일로 존재함을 삭제 전 확인 |
| `multisiteDGP_0.0.0.9000.tar.gz` | 421 KB | 이동 → `artifacts/pre-upgrade-build-artifacts/` | 과거 빌드. 옛 동작 대조용으로 보존 |
| `multisiteDGP_0.1.0.tar.gz` | 421 KB | 이동 → 동일 | 동일 |
| `..Rcheck/` | 4 KB | 이동 → 동일 | 다른 경로(`IES Multisite Trial Project`, `00_` 접두사 없음)에서 실행된 잔재. `DESCRIPTION` Author/Maintainer ERROR 기록이 가짜 신호를 만들고 있었음 |
| `multisiteDGP.Rcheck/` | 3.5 MB | 이동 → 동일 | check 잔재 |
| `Library/` | 0 B | **삭제** | 빈 디렉터리 확인 후 `rmdir` |
| `Outputs/` | 0 B | **삭제** | 동일 |
| `.DS_Store` | 8 KB | **삭제** | `.gitignore` 에 등재됨 (원격 `.gitignore` 가 이미 처리) |
| 세 history book | 344 MB | **제자리 유지 + 소스 스냅샷 분리** | 원본은 그대로 두어 상대 경로 참조를 보존. 소스만(21 MB, 948 파일) `multisiteDGP-design-archive/` git 저장소로 스냅샷 |

## 결과

| 지표 | 정리 전 | 정리 후 |
|---|---:|---:|
| 작업 디렉터리 | 462 MB | 417 MB |
| 최상위 항목 | 37 | 29 |
| git 추적 파일 | — | 627 |
| `.git` | — | 9.3 MB |

## 검증

- `R CMD build .` 성공 → `multisiteDGP_0.1.1.tar.gz` (9.2 MB)
- `git status --porcelain` 비어 있음
- `.Rbuildignore` union 병합 후에도 build 정상

## tarball 관찰 (Phase 2 Step 2.8 로 이월)

빌드된 tarball 9.2 MB 중 **`inst/doc/` 이 13.7 MB (압축 전)** 를 차지한다.

| 파일 | 크기 |
|---|---:|
| `inst/doc/m2-g-distribution-catalog.html` | 2,245 KB |
| `inst/doc/a8-cookbook.html` | 1,936 KB |
| `inst/doc/a6-case-study-multisite.html` | 1,102 KB |
| `inst/doc/a1-getting-started.html` | 1,006 KB |

plot 이 base64 로 임베드된 것으로 보인다. CRAN 권장 tarball 상한(5 MB)을 넘지만 v0.2.0 은 CRAN 제출 범위가 아니므로 릴리스를 막지 않는다. Step 2.8 이 vignette knit 시간·크기와 함께 감사한다.
