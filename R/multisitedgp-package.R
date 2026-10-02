#' multisiteDGP: Simulate summary data for multisite trials
#'
#' Simulates the summary data of a multisite trial: one estimated effect and
#' one standard error per site. Site effects come from one of six distributions
#' or from a user-supplied function. Standard errors are derived from simulated
#' site sizes or set directly, and can be correlated with the site effects.
#'
#' A simulation has four steps: site effects ([gen_effects()]), site sizes
#' and standard errors ([gen_site_sizes()]), an optional dependence between
#' effects and precision ([align_rank_corr()], [align_copula_corr()], or
#' [align_hybrid_corr()]), and observed estimates ([gen_observations()]).
#' [sim_multisite()] runs the four steps in one call. [sim_meta()] does the
#' same, with the standard errors set directly by [gen_se_direct()].
#'
#' To start, read `vignette("multisiteDGP")` and run [sim_multisite()] with
#' one of the [presets].
#'
#' @section Seeds:
#' With a `seed`, [sim_multisite()] and [sim_meta()] set the random number
#' generator to Mersenne-Twister with inversion and rejection sampling, run
#' the simulation, and then put the generator of your session back as it
#' was. The same seed gives the same data in any session. Without a seed
#' they draw from the generator of your session.
#'
#' @section Errors:
#' Every error that the package signals has class `multisitedgp_error`. An
#' error from a numerical procedure that could not reach its target also
#' has class `multisitedgp_solver_error`.
#'
#' @section Funding:
#' This research was supported by the Institute of Education Sciences, U.S.
#' Department of Education, through Grant R305D240078 to the University of
#' Alabama. The opinions expressed are those of the authors and do not
#' represent views of the Institute or the U.S. Department of Education.
#'
#' @references
#' Azzalini, A. (1985). A class of distributions which includes the normal
#' ones. *Scandinavian Journal of Statistics*, 12(2), 171-178.
#'
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. *Journal of Educational and Behavioral Statistics*,
#' 50(5), 731-764. \doi{10.3102/10769986241254286}
#'
#' Walters, C. (2024). Empirical Bayes methods in labor economics. In
#' *Handbook of Labor Economics* (Vol. 5, pp. 183-260). Elsevier.
#' \doi{10.1016/bs.heslab.2024.11.001}
#'
#' Weiss, M. J., Bloom, H. S., Verbitsky-Savitz, N., Gupta, H., Vigil, A. E.,
#' & Cullinan, D. N. (2017). How much do the effects of education and
#' training programs vary across sites? Evidence from past multisite
#' randomized trials. *Journal of Research on Educational Effectiveness*,
#' 10(4), 843-876. \doi{10.1080/19345747.2017.1300719}
#'
#' Yu, K., & Zhang, J. (2005). A three-parameter asymmetric Laplace
#' distribution and its extension. *Communications in Statistics - Theory
#' and Methods*, 34(9-10), 1867-1879. \doi{10.1080/03610920500199018}
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
## usethis namespace: end
NULL
