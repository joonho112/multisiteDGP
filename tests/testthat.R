library(testthat)
library(multisiteDGP)

# testthat stops listing after 10 failures by default, which on CI reports a
# broad regression as if it were a narrow one — the log says "10 failures"
# whether 10 tests broke or 200 did (defect D-017). Raise the cap so a red run
# is diagnosable in one read, but keep it finite so a total breakage cannot
# bury the log.
options(testthat.progress.max_fails = 200L)

test_check("multisiteDGP")
