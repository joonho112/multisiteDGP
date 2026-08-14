# 브랜치 전략 및 추적 범위 — Step 1.2

- **확정일:** 2026-08-14
- **승인:** PI (2026-08-14 대화형 확인)
- **적용 범위:** v0.2.0 업그레이드 작업 전체 (Phase 1 – Phase 11)

---

## 1. 복구 방식

로컬 작업 디렉터리가 git repository 가 아니었다. **파일을 옮기지 않는 접목(graft) 방식**으로 복구했다.

```bash
git init -b main
git remote add origin https://github.com/joonho112/multisiteDGP.git
git fetch origin main
git reset origin/main          # --mixed: index만 갱신, 작업 트리 무손상
```

`--mixed` reset 이므로 작업 트리의 어떤 파일도 덮어쓰이지 않았고, 이어진 `git status` 가 Step 1.1 의 비교 결과를 독립적으로 재확인했다 — 수정 7 건, 삭제 2 건, 나머지 226 건 동일.

**대안으로 검토했으나 택하지 않은 것.** ① 원격을 새로 클론한 뒤 로컬 자산을 복사해 넣기 — 파일 이동이 많아 사고 여지가 크다. ② `git init` 후 전체를 새 initial commit 으로 만들기 — 원격 이력 6 건과 연결이 끊긴다.

## 2. 조정된 파일

| 파일 | 조치 |
|---|---|
| `.github/workflows/{R-CMD-check,pkgdown-deploy,test-coverage}.yaml` | 원격 채택 (Suggests 명시 설치, `5d4e4fc`) |
| `CONTRIBUTING.md`, `NEWS.md`, `vignettes/references.bib` | 원격 채택 (공개 릴리스 정제본, `bf2961e`) |
| `.gitignore`, `LICENSE.md` | 원격에서 복원 (로컬에 없었음) |
| `.Rbuildignore` | **union 병합** — 로컬 32 행 + 원격 고유 `^LICENSE\.md$` = 33 행 |

정제 대상 3 파일은 원격 공개본이 내부 경로와 내부 도구명을 제거한 버전이었다. 로컬 버전을 유지했다면 이후 편집 시 정제가 되돌려질 위험이 있었다.

## 3. 추적 범위

**추적한다 (626 파일, `.git` 9.1 MB):**

| 경로 | 파일 수 | 근거 |
|---|---:|---|
| `R/` `tests/` `man/` `vignettes/` `inst/` `pkgdown/` `.github/` + 루트 메타데이터 | 235 | 원격 현행 범위 |
| `tools/` | 274 | traceability ledger 8 종, validation 결과 ~180 건, 정책 3 종. 이번 작업이 직접 수정하고 `test-traceability.R` 이 참조한다 |
| `log/` | 113 | v0.2.0 작업 계획 · Phase 로그 · 증거 · 결정. 작업 자체가 버전 관리되어야 감사 가능하다 |
| `dev/` | 1 | `NEWS-internal.md` |
| `.lintr`, `.pre-commit-config.yaml` | 2 | **원격에 미추적이었다** — CI `lint` 가 로컬과 다른 규칙으로 실행됐을 가능성. Phase 2 Step 2.1 원인 후보 |
| `vignettes/_vignette-frontmatter-template.txt` | 1 | vignette 작성 템플릿 |

**추적하지 않는다:**

| 경로 | 크기 | 처리 |
|---|---:|---|
| `docs/` | 14 MB | pkgdown 빌드 산출물. `pkgdown-deploy` 가 gh-pages 로 배포 |
| `blueprint-multisiteDGP-book/` | 122 MB | **별도 아카이브 저장소로 분리 예정** (Step 1.4) |
| `multisiteDGP-build-implementation-book/` | 124 MB | 동일 |
| `documentation-updates/` | 98 MB | 동일 |
| `blueprint-…-v1.0-*.zip` | 41 MB | 폴더와 중복 → Step 1.4 에서 삭제 |
| `log/…/artifacts/` | — | 되돌리기용 보존 아티팩트. git 에 넣으면 저장소가 비대해진다 |
| 빌드 잔재 (`*.Rcheck/`, `*.tar.gz`, `README.html`, `vignettes/*.html`, `Library/`, `Outputs/`, `.DS_Store`) | — | 원격 `.gitignore` + Step 1.2 추가분이 처리 |

::: {.callout-warning title="저장소가 PUBLIC 이다"}
`joonho112/multisiteDGP` 는 공개 저장소이므로 **추적 = 공개**다. `tools/` 와 `log/` 를 추적한다는 것은 validation 결과, 결함 원장, Phase 로그, 이 계획서가 전부 공개된다는 뜻이다. PI 가 이를 인지하고 승인했다. 방법론 작업물이며 개인정보나 미공개 데이터는 포함하지 않는다.

이후 Phase 에서 공개하기 곤란한 내용(예: 미발표 논문 수치, 외부 리뷰어 신원)이 로그에 들어갈 경우, 해당 파일을 `log/…/artifacts/` 아래로 옮기거나 별도 `.gitignore` 항목을 추가한다.
:::

## 4. 브랜치 전략

```
main                          ← 원격 추적. Phase 종료 시에만 병합
 │
 ├─ upgrade/phase-01-foundation
 ├─ upgrade/phase-02-defect-audit
 ├─ …
 └─ upgrade/phase-11-release
```

- **`main` 에 직접 커밋하지 않는다.** Phase 단위 브랜치에서 작업한다.
- **커밋 단위:** Step 하나 = 커밋 하나가 원칙. Step 안에서 논리적으로 분리되면 여러 개도 허용한다 (Step 1.2 가 2 개로 나뉜 것이 그 예).
- **커밋 메시지:** Conventional Commits + Step 번호. `chore(step1.2): …`, `fix(step7.3): …`. 본문에 결함 원장 ID 를 인용한다.
- **Phase 병합:** Phase 종료 로그 작성 후 `main` 으로 병합하고 push 해 CI 를 태운다.
- **push 시점:** Phase 작업 중에도 브랜치를 원격에 push 할 수 있다 — Phase 6 이후에는 CI 피드백을 받기 위해 오히려 필요하다. 단 **최초 push 는 PI 확인 후 진행한다** (공개 저장소이므로).

## 5. 태그

| 태그 | 커밋 | 의미 |
|---|---|---|
| `pre-upgrade-baseline` | `5239cf5` | v0.2.0 업그레이드 직전의 마지막 공개 상태 (2026-05-09) |
| `v0.2.0` | (예정) | Phase 11 Step 11.4 |

`pre-upgrade-baseline` 을 새 커밋이 아니라 `origin/main` 에 붙였다. 로컬의 복구 전 상태는 커밋이 아니었으므로, 정직한 "before" 지점은 마지막으로 공개된 커밋이다.

## 6. 되돌리기

| 상황 | 방법 |
|---|---|
| Step 1.2 전체 취소 | `git checkout pre-upgrade-baseline` 후 브랜치 삭제. 작업 트리 파일은 그대로 남는다 |
| 특정 파일을 로컬 원본으로 | 세션 스크래치의 `pre-git-safety/` 사본 (6 파일) |
| 개별 커밋 취소 | `git revert <sha>` |

## 7. Step 1.4 로 넘기는 것

1. 세 history book 을 별도 아카이브 저장소로 분리 (소스만, `_book/` 렌더본 제외)
2. `blueprint-…-v1.0-20260507.zip` (41 MB) 삭제 — 폴더와 중복
3. `*.tar.gz` 2 개, `..Rcheck/`, `multisiteDGP.Rcheck/` 를 `log/…/artifacts/` 로 이동
4. `Library/`, `Outputs/` 빈 디렉터리와 `.DS_Store` 삭제
5. 정리 후 `R CMD build` 로 tarball 크기·내용 확인
