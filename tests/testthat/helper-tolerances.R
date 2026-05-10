# Deterministic equality: use exact identity when possible, otherwise this
# floating-point tolerance from the blueprint.
tol_deterministic <- 1e-12

# Monte Carlo and property-test tolerances from blueprint ch.18.
tol_mc_moment_n1e5 <- 0.01
tol_mc_moment_n1e6 <- 0.005
tol_studentt_inf_kurt_var <- 0.05
tol_spearman_continuous <- 0.02
tol_spearman_ties <- 0.05
tol_irb_rounding <- 0.005

mc_moment_clt_multiplier <- 2
default_m_replications <- 1000L
default_n_property_large <- 1e6

shapiro_p_leq_001_count_min <- 4L
shapiro_p_leq_001_count_max <- 17L
permutation_envelope_x <- 1.1
