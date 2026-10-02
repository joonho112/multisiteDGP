# Deterministic equality: use exact identity when possible, otherwise this
# floating-point tolerance.
tol_deterministic <- 1e-12

# Monte Carlo and property-test tolerances.
tol_mc_moment_n1e5 <- 0.01
tol_mc_moment_n1e6 <- 0.005
tol_studentt_inf_kurt_var <- 0.05
tol_spearman_continuous <- 0.02
tol_spearman_ties <- 0.05

default_m_replications <- 1000L
default_n_property_large <- 1e6

# Sample size of the large-sample moment checks of the effect distributions,
# and the standard errors their tolerances are multiples of. For n independent
# draws of a variable with variance 1, the mean has variance 1 / n and the
# sample variance has variance (kurtosis - 1) / n.
n_moment_check <- 100000L
se_mean <- function(n) 1 / sqrt(n)
se_variance <- function(n, kurtosis) sqrt((kurtosis - 1) / n)
se_proportion <- function(n, p) sqrt(p * (1 - p) / n)

shapiro_p_leq_001_count_min <- 4L
shapiro_p_leq_001_count_max <- 17L
permutation_envelope_x <- 1.1
