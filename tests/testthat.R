library(testthat)
library(multisiteDGP)

# testthat stops listing after 10 failures by default, so the log says
# "10 failures" whether 10 tests broke or 200 did. Raise the cap so one run
# shows how far a regression reaches, but keep it finite so a total breakage
# cannot bury the log.
options(testthat.progress.max_fails = 200L)

test_check("multisiteDGP")
