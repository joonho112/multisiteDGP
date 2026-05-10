# print.multisitedgp_design snapshots all preset designs

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## education_small
    Code
      print(designs[[name]])
    Output
      <multisitedgp_design>
      Paradigm: site_size    Engine: A2_modern    Framing: superpopulation
      J: 50    Seed: NULL (active RNG)    Lifecycle: experimental
      
      [ Layer 1: G-effects ]
        true_dist:  Gaussian
        tau:        0
        sigma_tau:  0.05
        formula:    NULL
        beta:       NULL
        g_fn:       NULL
      
      [ Layer 2: Margin (Paradigm A) ]
        nj_mean:    40
        cv:         0.5
        nj_min:     5
        p:          0.5
        R2:         0
        var_outcome: 1
      
      [ Layer 3: Dependence ]
        method:        none
        rank_corr:     0
        pearson_corr:  0
        hybrid_init:   copula
        hybrid_polish: hill_climb
        dependence_fn: NULL
      
      [ Layer 4: Observation ]
        obs_fn: NULL
      
      Use sim_multisite(design) or sim_meta(design) to simulate.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## education_modest
    Code
      print(designs[[name]])
    Output
      <multisitedgp_design>
      Paradigm: site_size    Engine: A2_modern    Framing: superpopulation
      J: 50    Seed: NULL (active RNG)    Lifecycle: experimental
      
      [ Layer 1: G-effects ]
        true_dist:  Gaussian
        tau:        0
        sigma_tau:  0.2
        formula:    NULL
        beta:       NULL
        g_fn:       NULL
      
      [ Layer 2: Margin (Paradigm A) ]
        nj_mean:    50
        cv:         0.5
        nj_min:     10
        p:          0.5
        R2:         0
        var_outcome: 1
      
      [ Layer 3: Dependence ]
        method:        none
        rank_corr:     0
        pearson_corr:  0
        hybrid_init:   copula
        hybrid_polish: hill_climb
        dependence_fn: NULL
      
      [ Layer 4: Observation ]
        obs_fn: NULL
      
      Use sim_multisite(design) or sim_meta(design) to simulate.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## education_substantial
    Code
      print(designs[[name]])
    Output
      <multisitedgp_design>
      Paradigm: site_size    Engine: A2_modern    Framing: superpopulation
      J: 100    Seed: NULL (active RNG)    Lifecycle: experimental
      
      [ Layer 1: G-effects ]
        true_dist:  Gaussian
        tau:        0
        sigma_tau:  0.3
        formula:    NULL
        beta:       NULL
        g_fn:       NULL
      
      [ Layer 2: Margin (Paradigm A) ]
        nj_mean:    80
        cv:         0.5
        nj_min:     10
        p:          0.5
        R2:         0
        var_outcome: 1
      
      [ Layer 3: Dependence ]
        method:        none
        rank_corr:     0
        pearson_corr:  0
        hybrid_init:   copula
        hybrid_polish: hill_climb
        dependence_fn: NULL
      
      [ Layer 4: Observation ]
        obs_fn: NULL
      
      Use sim_multisite(design) or sim_meta(design) to simulate.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## jebs_paper
    Code
      print(designs[[name]])
    Output
      <multisitedgp_design>
      Paradigm: site_size    Engine: A1_legacy    Framing: superpopulation
      J: 50    Seed: NULL (active RNG)    Lifecycle: experimental
      
      [ Layer 1: G-effects ]
        true_dist:  Mixture
        tau:        0
        sigma_tau:  0.2
        formula:    NULL
        beta:       NULL
        g_fn:       NULL
      
      [ Layer 2: Margin (Paradigm A) ]
        nj_mean:    40
        cv:         0.5
        nj_min:     5
        p:          0.5
        R2:         0
        var_outcome: 1
      
      [ Layer 3: Dependence ]
        method:        none
        rank_corr:     0
        pearson_corr:  0
        hybrid_init:   copula
        hybrid_polish: hill_climb
        dependence_fn: NULL
      
      [ Layer 4: Observation ]
        obs_fn: NULL
      
      Use sim_multisite(design) or sim_meta(design) to simulate.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## jebs_strict
    Code
      print(designs[[name]])
    Output
      <multisitedgp_design>
      Paradigm: site_size    Engine: A1_legacy    Framing: superpopulation
      J: 100    Seed: NULL (active RNG)    Lifecycle: experimental
      
      [ Layer 1: G-effects ]
        true_dist:  Mixture
        tau:        0
        sigma_tau:  0.15
        formula:    NULL
        beta:       NULL
        g_fn:       NULL
      
      [ Layer 2: Margin (Paradigm A) ]
        nj_mean:    80
        cv:         0.5
        nj_min:     4
        p:          0.5
        R2:         0
        var_outcome: 1
      
      [ Layer 3: Dependence ]
        method:        none
        rank_corr:     0
        pearson_corr:  0
        hybrid_init:   copula
        hybrid_polish: hill_climb
        dependence_fn: NULL
      
      [ Layer 4: Observation ]
        obs_fn: NULL
      
      Use sim_multisite(design) or sim_meta(design) to simulate.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## walters_2024
    Code
      print(designs[[name]])
    Output
      <multisitedgp_design>
      Paradigm: site_size    Engine: A2_modern    Framing: superpopulation
      J: 46    Seed: NULL (active RNG)    Lifecycle: experimental
      
      [ Layer 1: G-effects ]
        true_dist:  Gaussian
        tau:        0
        sigma_tau:  0.197
        formula:    NULL
        beta:       NULL
        g_fn:       NULL
      
      [ Layer 2: Margin (Paradigm A) ]
        nj_mean:    240
        cv:         0.3
        nj_min:     50
        p:          0.5
        R2:         0.4
        var_outcome: 1
      
      [ Layer 3: Dependence ]
        method:        none
        rank_corr:     0
        pearson_corr:  0
        hybrid_init:   copula
        hybrid_polish: hill_climb
        dependence_fn: NULL
      
      [ Layer 4: Observation ]
        obs_fn: NULL
      
      Use sim_multisite(design) or sim_meta(design) to simulate.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## twin_towers
    Code
      print(designs[[name]])
    Output
      <multisitedgp_design>
      Paradigm: site_size    Engine: A2_modern    Framing: superpopulation
      J: 1000    Seed: NULL (active RNG)    Lifecycle: experimental
      
      [ Layer 1: G-effects ]
        true_dist:  Mixture
        tau:        0
        sigma_tau:  2
        formula:    NULL
        beta:       NULL
        g_fn:       NULL
      
      [ Layer 2: Margin (Paradigm A) ]
        nj_mean:    100
        cv:         0
        nj_min:     100
        p:          0.5
        R2:         0
        var_outcome: 1
      
      [ Layer 3: Dependence ]
        method:        none
        rank_corr:     0
        pearson_corr:  0
        hybrid_init:   copula
        hybrid_polish: hill_climb
        dependence_fn: NULL
      
      [ Layer 4: Observation ]
        obs_fn: NULL
      
      Use sim_multisite(design) or sim_meta(design) to simulate.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## meta_modest
    Code
      print(designs[[name]])
    Output
      <multisitedgp_design>
      Paradigm: direct    Engine: A2_modern    Framing: superpopulation
      J: 50    Seed: NULL (active RNG)    Lifecycle: experimental
      
      [ Layer 1: G-effects ]
        true_dist:  Gaussian
        tau:        0
        sigma_tau:  0.2
        formula:    NULL
        beta:       NULL
        g_fn:       NULL
      
      [ Layer 2: Margin (Paradigm B) ]
        I:          0.3
        R:          1.5
        shuffle:    TRUE
        se_fn:      NULL
      
      [ Layer 3: Dependence ]
        method:        none
        rank_corr:     0
        pearson_corr:  0
        hybrid_init:   copula
        hybrid_polish: hill_climb
        dependence_fn: NULL
      
      [ Layer 4: Observation ]
        obs_fn: NULL
      
      Use sim_multisite(design) or sim_meta(design) to simulate.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## small_area_estimation
    Code
      print(designs[[name]])
    Output
      <multisitedgp_design>
      Paradigm: direct    Engine: A2_modern    Framing: superpopulation
      J: 30    Seed: NULL (active RNG)    Lifecycle: experimental
      
      [ Layer 1: G-effects ]
        true_dist:  Gaussian
        tau:        0
        sigma_tau:  0.2
        formula:    NULL
        beta:       NULL
        g_fn:       NULL
      
      [ Layer 2: Margin (Paradigm B) ]
        I:          0.2
        R:          3
        shuffle:    TRUE
        se_fn:      NULL
      
      [ Layer 3: Dependence ]
        method:        none
        rank_corr:     0
        pearson_corr:  0
        hybrid_init:   copula
        hybrid_polish: hill_climb
        dependence_fn: NULL
      
      [ Layer 4: Observation ]
        obs_fn: NULL
      
      Use sim_multisite(design) or sim_meta(design) to simulate.

# print.multisitedgp_data snapshots golden preset outputs

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## jebs_paper
    Code
      print(outputs[[name]], n = 3L)
    Output
      # A multisitedgp_data: 50 sites, paradigm = "site_size"
      # Realized vs intended:
      #   I: realized=0.265 (no target)
      #   R: realized=10.545 (no target)
      #   sigma_tau: target=0.200, realized=0.198, PASS
      #   rho_S: target=0.000, realized=-0.142, PASS
      #   rho_S_marg: realized=-0.142 (no target)
      #   Feasibility: WARN (n_eff=13.755)
      # A tibble: 50 x 7
        site_index    z_j   tau_j tau_j_hat  se_j se2_j   n_j
             <int>  <dbl>   <dbl>     <dbl> <dbl> <dbl> <int>
      1          1 -0.540 -0.108     -0.156 0.338 0.114    35
      2          2 -0.250 -0.0500     0.519 0.603 0.364    11
      3          3 -0.521 -0.104     -0.289 0.343 0.118    34
      # i 47 more rows
      # Use summary(df) for the full diagnostic report.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## jebs_strict
    Code
      print(outputs[[name]], n = 3L)
    Output
      # A multisitedgp_data: 100 sites, paradigm = "site_size"
      # Realized vs intended:
      #   I: realized=0.279 (no target)
      #   R: realized=9.062 (no target)
      #   sigma_tau: target=0.150, realized=0.135, WARN
      #   rho_S: target=0.000, realized=-0.020, PASS
      #   rho_S_marg: realized=-0.020 (no target)
      #   Feasibility: WARN (n_eff=28.883)
      # A tibble: 100 x 7
        site_index    z_j   tau_j tau_j_hat  se_j  se2_j   n_j
             <int>  <dbl>   <dbl>     <dbl> <dbl>  <dbl> <int>
      1          1  0.328  0.0491    0.0473 0.167 0.0278   144
      2          2 -0.205 -0.0307   -0.562  0.231 0.0533    75
      3          3 -0.957 -0.144    -0.154  0.280 0.0784    51
      # i 97 more rows
      # Use summary(df) for the full diagnostic report.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## education_modest
    Code
      print(outputs[[name]], n = 3L)
    Output
      # A multisitedgp_data: 50 sites, paradigm = "site_size"
      # Realized vs intended:
      #   I: realized=0.277 (no target)
      #   R: realized=10.583 (no target)
      #   sigma_tau: target=0.200, realized=0.219, WARN
      #   rho_S: target=0.000, realized=-0.156, PASS
      #   rho_S_marg: realized=-0.156 (no target)
      #   Feasibility: WARN (n_eff=14.431)
      # A tibble: 50 x 7
        site_index    z_j   tau_j tau_j_hat  se_j  se2_j   n_j
             <int>  <dbl>   <dbl>     <dbl> <dbl>  <dbl> <int>
      1          1  0.586  0.117      0.175 0.260 0.0678    59
      2          2  0.709  0.142     -0.228 0.320 0.103     39
      3          3 -0.109 -0.0219     0.204 0.535 0.286     14
      # i 47 more rows
      # Use summary(df) for the full diagnostic report.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## walters_2024
    Code
      print(outputs[[name]], n = 3L)
    Output
      # A multisitedgp_data: 46 sites, paradigm = "site_size"
      # Realized vs intended:
      #   I: realized=0.783 (no target)
      #   R: realized=4.270 (no target)
      #   sigma_tau: target=0.197, realized=0.168, WARN
      #   rho_S: target=0.000, realized=0.145, PASS
      #   rho_S_marg: realized=0.145 (no target)
      #   Feasibility: PASS (n_eff=35.828)
      # A tibble: 46 x 7
        site_index    z_j   tau_j tau_j_hat  se_j  se2_j   n_j
             <int>  <dbl>   <dbl>     <dbl> <dbl>  <dbl> <int>
      1          1 -0.626 -0.123    0.00124 0.107 0.0115   208
      2          2  0.184  0.0362   0.122   0.122 0.015    160
      3          3 -0.836 -0.165    0.0176  0.115 0.0132   182
      # i 43 more rows
      # Use summary(df) for the full diagnostic report.

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## small_area_estimation
    Code
      print(outputs[[name]], n = 3L)
    Output
      # A multisitedgp_meta: 30 sites, paradigm = "direct"
      # Realized vs intended:
      #   I: target=0.200, realized=0.200, PASS
      #   R: target=3.000, realized=3.000, PASS
      #   sigma_tau: target=0.200, realized=0.251, FAIL
      #   rho_S: target=0.000, realized=0.413, WARN
      #   rho_S_marg: realized=0.413 (no target)
      #   Feasibility: WARN (n_eff=6.153)
      # A tibble: 30 x 7
        site_index    z_j   tau_j tau_j_hat  se_j se2_j   n_j
             <int>  <dbl>   <dbl>     <dbl> <dbl> <dbl> <int>
      1          1  1.37   0.274     0.552  0.444 0.197    NA
      2          2 -0.565 -0.113    -0.0945 0.488 0.238    NA
      3          3  0.363  0.0726    0.200  0.419 0.176    NA
      # i 27 more rows
      # Use summary(df) for the full diagnostic report.

# summary.multisitedgp_data snapshots golden preset outputs

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## jebs_paper
    Code
      summary(outputs[[name]])
    Output
      multisiteDGP simulation diagnostics
      ------------------------------------------------------------
      A. Realized vs Intended
         I (informativeness):         0.265  (target N/A)  N/A   [no target]
         R (SE heterogeneity):       10.545  (target N/A)  N/A   [no target]
         sigma_tau:                   0.198  (target 0.200)  PASS  [rel=-0.9%]
         GM(se^2):                    0.111  (target N/A)  N/A   [no target]
      
      B. Dependence
         rank_corr residual:         -0.142  (target 0.000)  PASS  [delta=-0.142]
         rank_corr marginal:         -0.142  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
         pearson_corr residual:      -0.142  (target 0.000)  FAIL  [delta=-0.142]
         pearson_corr marginal:      -0.142  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
      
      C. G shape fit
         KS distance D_J:               N/A  (target 0.000)  N/A   [no target]
         Bhattacharyya BC:              N/A  (target 1.000)  N/A   [no target]
         Q-Q residual:                  N/A  (target 0.000)  N/A   [order-statistic band metadata deferred]
      
      D. Operational feasibility
         mean shrinkage S:            0.275  (target N/A)  PASS  [no target]
         avg MOE (95%):               0.674  (target N/A)  WARN  [no target]
         feasibility_index:          13.755  (target N/A)  WARN  [no target]
      ------------------------------------------------------------
      Overall: 3 PASS, 2 WARN, 1 FAIL.
      Provenance: multisiteDGP <VERSION> | paradigm=site_size | seed=4719 | canonical_hash=a96eaabd1c022e32 | design_hash=b1891bd7d287895f | hash_algo=xxhash64 | R=<R> | hooks=none

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## jebs_strict
    Code
      summary(outputs[[name]])
    Output
      multisiteDGP simulation diagnostics
      ------------------------------------------------------------
      A. Realized vs Intended
         I (informativeness):         0.279  (target N/A)  N/A   [no target]
         R (SE heterogeneity):        9.062  (target N/A)  N/A   [no target]
         sigma_tau:                   0.135  (target 0.150)  WARN  [rel=-9.9%]
         GM(se^2):                    0.058  (target N/A)  N/A   [no target]
      
      B. Dependence
         rank_corr residual:         -0.020  (target 0.000)  PASS  [delta=-0.020]
         rank_corr marginal:         -0.020  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
         pearson_corr residual:      -0.135  (target 0.000)  FAIL  [delta=-0.135]
         pearson_corr marginal:      -0.135  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
      
      C. G shape fit
         KS distance D_J:               N/A  (target 0.000)  N/A   [no target]
         Bhattacharyya BC:              N/A  (target 1.000)  N/A   [no target]
         Q-Q residual:                  N/A  (target 0.000)  N/A   [order-statistic band metadata deferred]
      
      D. Operational feasibility
         mean shrinkage S:            0.289  (target N/A)  PASS  [no target]
         avg MOE (95%):               0.488  (target N/A)  WARN  [no target]
         feasibility_index:          28.883  (target N/A)  WARN  [no target]
      ------------------------------------------------------------
      Overall: 2 PASS, 3 WARN, 1 FAIL.
      Provenance: multisiteDGP <VERSION> | paradigm=site_size | seed=4719 | canonical_hash=871d0fa960030a02 | design_hash=5eda5659b1ad514d | hash_algo=xxhash64 | R=<R> | hooks=none

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## education_modest
    Code
      summary(outputs[[name]])
    Output
      multisiteDGP simulation diagnostics
      ------------------------------------------------------------
      A. Realized vs Intended
         I (informativeness):         0.277  (target N/A)  N/A   [no target]
         R (SE heterogeneity):       10.583  (target N/A)  N/A   [no target]
         sigma_tau:                   0.219  (target 0.200)  WARN  [rel=9.7%]
         GM(se^2):                    0.104  (target N/A)  N/A   [no target]
      
      B. Dependence
         rank_corr residual:         -0.156  (target 0.000)  PASS  [delta=-0.156]
         rank_corr marginal:         -0.156  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
         pearson_corr residual:      -0.186  (target 0.000)  FAIL  [delta=-0.186]
         pearson_corr marginal:      -0.186  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
      
      C. G shape fit
         KS distance D_J:             0.180  (target 0.000)  PASS  [p=0.396]
         Bhattacharyya BC:            0.767  (target 1.000)  FAIL  [rel=-23.3%]
         Q-Q residual:                0.466  (target 0.000)  N/A   [delta=0.466]
      
      D. Operational feasibility
         mean shrinkage S:            0.289  (target N/A)  PASS  [no target]
         avg MOE (95%):               0.656  (target N/A)  WARN  [no target]
         feasibility_index:          14.431  (target N/A)  WARN  [no target]
      ------------------------------------------------------------
      Overall: 3 PASS, 3 WARN, 2 FAIL.
      Provenance: multisiteDGP <VERSION> | paradigm=site_size | seed=12345 | canonical_hash=128c43fb9cfc4416 | design_hash=db50cfd4e1659537 | hash_algo=xxhash64 | R=<R> | hooks=none

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## walters_2024
    Code
      summary(outputs[[name]])
    Output
      multisiteDGP simulation diagnostics
      ------------------------------------------------------------
      A. Realized vs Intended
         I (informativeness):         0.783  (target N/A)  N/A   [no target]
         R (SE heterogeneity):        4.270  (target N/A)  N/A   [no target]
         sigma_tau:                   0.168  (target 0.197)  WARN  [rel=-14.8%]
         GM(se^2):                    0.011  (target N/A)  N/A   [no target]
      
      B. Dependence
         rank_corr residual:          0.145  (target 0.000)  PASS  [delta=0.145]
         rank_corr marginal:          0.145  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
         pearson_corr residual:       0.159  (target 0.000)  FAIL  [delta=0.159]
         pearson_corr marginal:       0.159  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
      
      C. G shape fit
         KS distance D_J:             0.109  (target 0.000)  PASS  [p=0.952]
         Bhattacharyya BC:            0.792  (target 1.000)  FAIL  [rel=-20.8%]
         Q-Q residual:                0.700  (target 0.000)  N/A   [delta=0.700]
      
      D. Operational feasibility
         mean shrinkage S:            0.779  (target N/A)  PASS  [no target]
         avg MOE (95%):               0.206  (target N/A)  PASS  [no target]
         feasibility_index:          35.828  (target N/A)  PASS  [no target]
      ------------------------------------------------------------
      Overall: 5 PASS, 1 WARN, 2 FAIL.
      Provenance: multisiteDGP <VERSION> | paradigm=site_size | seed=1 | canonical_hash=461c8b7f4ff83ec7 | design_hash=3f1b3fa764d5ec00 | hash_algo=xxhash64 | R=<R> | hooks=none

---

    Code
      cat(sprintf("## %s\n", name))
    Output
      ## small_area_estimation
    Code
      summary(outputs[[name]])
    Output
      multisiteDGP simulation diagnostics
      ------------------------------------------------------------
      A. Realized vs Intended
         I (informativeness):         0.200  (target 0.200)  PASS  [rel=0.0%]
         R (SE heterogeneity):        3.000  (target 3.000)  PASS  [rel=0.0%]
         sigma_tau:                   0.251  (target 0.200)  FAIL  [rel=25.5%]
         GM(se^2):                    0.160  (target 0.160)  PASS  [rel=0.0%]
      
      B. Dependence
         rank_corr residual:          0.413  (target 0.000)  WARN  [delta=0.413]
         rank_corr marginal:          0.413  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
         pearson_corr residual:       0.474  (target 0.000)  FAIL  [delta=0.474]
         pearson_corr marginal:       0.474  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
      
      C. G shape fit
         KS distance D_J:             0.167  (target 0.000)  PASS  [p=0.808]
         Bhattacharyya BC:            0.527  (target 1.000)  FAIL  [rel=-47.3%]
         Q-Q residual:                0.796  (target 0.000)  N/A   [delta=0.796]
      
      D. Operational feasibility
         mean shrinkage S:            0.205  (target N/A)  PASS  [no target]
         avg MOE (95%):               0.795  (target N/A)  WARN  [no target]
         feasibility_index:           6.153  (target N/A)  WARN  [no target]
      ------------------------------------------------------------
      Overall: 5 PASS, 3 WARN, 3 FAIL.
      Provenance: multisiteDGP <VERSION> | paradigm=direct | seed=42 | canonical_hash=c743272c10cf35c6 | design_hash=23d5c64c78ea3137 | hash_algo=xxhash64 | R=<R> | hooks=none

# print and summary snapshot a direct meta-analysis preset

    Code
      print(out, n = 3L)
    Output
      # A multisitedgp_meta: 50 sites, paradigm = "direct"
      # Realized vs intended:
      #   I: target=0.300, realized=0.300, PASS
      #   R: target=1.500, realized=1.500, PASS
      #   sigma_tau: target=0.200, realized=0.200, PASS
      #   rho_S: target=0.000, realized=0.073, PASS
      #   rho_S_marg: realized=0.073 (no target)
      #   Feasibility: WARN (n_eff=15.030)
      # A tibble: 50 x 7
        site_index    z_j  tau_j tau_j_hat  se_j se2_j   n_j
             <int>  <dbl>  <dbl>     <dbl> <dbl> <dbl> <int>
      1          1 0.0686 0.0137    0.0940 0.324 0.105    NA
      2          2 1.54   0.308     0.179  0.327 0.107    NA
      3          3 1.75   0.349     0.560  0.331 0.110    NA
      # i 47 more rows
      # Use summary(df) for the full diagnostic report.

---

    Code
      summary(out)
    Output
      multisiteDGP simulation diagnostics
      ------------------------------------------------------------
      A. Realized vs Intended
         I (informativeness):         0.300  (target 0.300)  PASS  [rel=0.0%]
         R (SE heterogeneity):        1.500  (target 1.500)  PASS  [rel=-0.0%]
         sigma_tau:                   0.200  (target 0.200)  PASS  [rel=0.2%]
         GM(se^2):                    0.093  (target 0.093)  PASS  [rel=-0.0%]
      
      B. Dependence
         rank_corr residual:          0.073  (target 0.000)  PASS  [delta=0.073]
         rank_corr marginal:          0.073  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
         pearson_corr residual:       0.023  (target 0.000)  PASS  [delta=0.023]
         pearson_corr marginal:       0.023  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
      
      C. G shape fit
         KS distance D_J:             0.060  (target 0.000)  PASS  [p=1.000]
         Bhattacharyya BC:            0.768  (target 1.000)  FAIL  [rel=-23.2%]
         Q-Q residual:                0.779  (target 0.000)  N/A   [delta=0.779]
      
      D. Operational feasibility
         mean shrinkage S:            0.301  (target N/A)  PASS  [no target]
         avg MOE (95%):               0.600  (target N/A)  WARN  [no target]
         feasibility_index:          15.030  (target N/A)  WARN  [no target]
      ------------------------------------------------------------
      Overall: 8 PASS, 2 WARN, 1 FAIL.
      Provenance: multisiteDGP <VERSION> | paradigm=direct | seed=8602 | canonical_hash=909fc32cf0d7725b | design_hash=6e70faccb2d91462 | hash_algo=xxhash64 | R=<R> | hooks=none

# print and summary snapshot covariate two-number reporting

    Code
      print(out, n = 3L)
    Output
      # A multisitedgp_data: 25 sites, paradigm = "site_size"
      # Realized vs intended:
      #   I: realized=0.317 (no target)
      #   R: realized=13.000 (no target)
      #   sigma_tau: target=0.200, realized=0.166, FAIL
      #   rho_S: target=0.300, realized=0.299, PASS
      #   rho_S_marg: realized=0.213 (no target)
      #   Feasibility: WARN (n_eff=8.292)
      # A tibble: 25 x 8
        site_index    z_j    tau_j tau_j_hat  se_j  se2_j   n_j x_site
             <int>  <dbl>    <dbl>     <dbl> <dbl>  <dbl> <int>  <dbl>
      1          1  0.337 -0.0827    -0.0573 0.210 0.0440    91 -1    
      2          2 -0.398 -0.217     -0.723  0.298 0.0889    45 -0.917
      3          3  0.589 -0.00719    0.415  0.324 0.105     38 -0.833
      # i 22 more rows
      # Use summary(df) for the full diagnostic report.

---

    Code
      summary(out)
    Output
      multisiteDGP simulation diagnostics
      ------------------------------------------------------------
      A. Realized vs Intended
         I (informativeness):         0.317  (target N/A)  N/A   [no target]
         R (SE heterogeneity):       13.000  (target N/A)  N/A   [no target]
         sigma_tau:                   0.166  (target 0.200)  FAIL  [rel=-17.2%]
         GM(se^2):                    0.086  (target N/A)  N/A   [no target]
      
      B. Dependence
         rank_corr residual:          0.299  (target 0.300)  PASS  [rel=-0.5%]
         rank_corr marginal:          0.213  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
         pearson_corr residual:       0.128  (target 0.000)  FAIL  [delta=0.128]
         pearson_corr marginal:       0.220  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
      
      C. G shape fit
         KS distance D_J:             0.160  (target 0.000)  PASS  [p=0.915]
         Bhattacharyya BC:            0.542  (target 1.000)  FAIL  [rel=-45.8%]
         Q-Q residual:                0.521  (target 0.000)  N/A   [delta=0.521]
      
      D. Operational feasibility
         mean shrinkage S:            0.332  (target N/A)  PASS  [no target]
         avg MOE (95%):               0.606  (target N/A)  WARN  [no target]
         feasibility_index:           8.292  (target N/A)  WARN  [no target]
      ------------------------------------------------------------
      Overall: 3 PASS, 2 WARN, 3 FAIL.
      Provenance: multisiteDGP <VERSION> | paradigm=site_size | seed=8601 | canonical_hash=ffbc410750d058c6 | design_hash=3df4aac3babcfc4f | hash_algo=xxhash64 | R=<R> | hooks=none

# summary snapshots row-subset diagnostic recomputation

    Code
      summary(sub)
    Output
      multisiteDGP simulation diagnostics
      ------------------------------------------------------------
      A. Realized vs Intended
         I (informativeness):         0.295  (target N/A)  N/A   [no target]
         R (SE heterogeneity):        4.950  (target N/A)  N/A   [no target]
         sigma_tau:                   0.262  (target 0.200)  FAIL  [rel=30.9%]
         GM(se^2):                    0.096  (target N/A)  N/A   [no target]
      
      B. Dependence
         rank_corr residual:          0.224  (target 0.000)  PASS  [delta=0.224]
         rank_corr marginal:          0.224  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
         pearson_corr residual:       0.133  (target 0.000)  FAIL  [delta=0.133]
         pearson_corr marginal:       0.133  (target N/A)  N/A   [residual target rows only; no finite target; status not assigned]
      
      C. G shape fit
         KS distance D_J:             0.167  (target 0.000)  PASS  [p=0.998]
         Bhattacharyya BC:            0.000  (target 1.000)  FAIL  [rel=-100.0%]
         Q-Q residual:                0.831  (target 0.000)  N/A   [delta=0.831]
      
      D. Operational feasibility
         mean shrinkage S:            0.306  (target N/A)  PASS  [no target]
         avg MOE (95%):               0.627  (target N/A)  WARN  [no target]
         feasibility_index:           3.670  (target N/A)  FAIL  [no target]
      ------------------------------------------------------------
      Overall: 3 PASS, 1 WARN, 4 FAIL.
      Provenance: multisiteDGP <VERSION> | paradigm=site_size | seed=8603 | canonical_hash=a4ae8d4e13e09888 | design_hash=c6e3366dde483d93 | hash_algo=xxhash64 | R=<R> | hooks=none

