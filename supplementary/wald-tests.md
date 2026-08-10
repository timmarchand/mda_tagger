# Restricted vs Full Model Comparison: Topic Moderation Wald Tests

## Purpose

This file reports the full results of the clustered Wald tests used to assess whether topic (PTJ vs SMK) meaningfully moderates the proficiency effect for each of the six modelled evaluative bundles, referenced in Section 4.2 of Marchand, T. (2026), "The Gravity of Evaluation: Modelling Sparse Evaluative Bundle Frequencies in Learner Corpora with PPML," *APCLC 2026 Proceedings*.

## Method

For each of the six bundles, a **restricted model** (level effects only: `raw_count ~ level`) was compared against the **full interaction model** (`raw_count ~ level * topic`) reported in the main text, using a clustered Wald test (`fixest::wald()`) that jointly tests the null hypothesis that the topic main effect and all three level × topic interaction terms are simultaneously zero. Both models were fitted on the same zero-inclusive frequency dataset described in Section 3.5 of the main text, with standard errors clustered by writer ID.

``` r
# Restricted models: level only, no topic
restricted_models <- map(
  setNames(bundle_legend$Bundle, bundle_legend$Bundle),
  function(bundle) {
    model_sub <- complete_regression_df |>
      filter(tag_bundle == bundle) |>
      ungroup()
    feglm(
      raw_count ~ level,
      offset  = ~ log(file_wc),
      cluster = "writer_id",
      family  = "quasipoisson",
      data    = model_sub,
      notes   = FALSE
    )
  }
)

# Wald test: jointly test topic + all level:topic interactions = 0
wald_results <- map_dfr(bundle_legend$Model, function(m) {
  full_mod   <- valid_models[[m]]
  bundle_str <- bundle_legend$Bundle[bundle_legend$Model == m]
  restr_mod  <- restricted_models[[bundle_str]]
  w <- wald(full_mod, keep = "topic")
  tibble(
    Model             = m,
    Bundle            = bundle_str,
    wald_stat         = w$stat,
    wald_df1          = w$df1,
    wald_df2          = w$df2,
    wald_p            = w$p,
    sq_cor_restricted = summary(restr_mod)$sq.cor,
    sq_cor_full       = summary(full_mod)$sq.cor,
    sq_cor_gain       = summary(full_mod)$sq.cor - summary(restr_mod)$sq.cor
  )
})
```

## Results

| Model | Feature Bundle | JSD | Wald χ² | df | p | R² gain | Direction |
|----|----|----|----|----|----|----|----|
| M6 | `{{PRP<PIT>}} {{VBZ<BEMA><VPRT>}} {{JJ<PRED>}}` | 0.0025 | 23.81 | 4, 599 | **\< .001** | .072 | Overused |
| M4 | `{{PRP<FPP1>}} {{VBP<PUBV><SUAV><VPRT>}} {{IN<PIN>}}` | 0.0041 | 10.09 | 4, 599 | **\< .001** | .028 | Overused |
| M3 | `{{IN<PIN>}} {{DT}} {{JJ<JJ>}}` | 0.0107 | 4.98 | 4, 599 | **.001** | .010 | Underused |
| M2 | `{{.}} {{RB<RB>}} {{,}}` | 0.0003 | 4.18 | 4, 599 | **.002** | .010 | Overused |
| M5 | `{{JJ<JJ>}} {{NN<NN>}} {{IN<PIN>}}` | 0.0038 | 2.55 | 4, 599 | **.038** | .010 | Underused |
| M1 | `{{DT}} {{JJ<JJ>}} {{NN<NN>}}` | 0.0014 | 1.51 | 4, 599 | .198 | .004 | Underused |

*Bold p-values indicate significance at α = .05. Ordered by p-value ascending. JSD = Jensen–Shannon Divergence, computed descriptively at the tupling stage (Section 3.2 of the main text) as a predictor of topic sensitivity.*

## Interpretation

Five of the six bundles show a statistically significant Wald test, indicating that topic meaningfully moderates the proficiency effect beyond what a level-only model captures:

- **M6** (predicative adjective frame, χ² = 23.81, p \< .001, R² gain = .072) shows by far the strongest and most consequential topic effect in the dataset — a striking result given its comparatively modest JSD score (0.0025), which would not, on JSD alone, have predicted such a pronounced interaction.
- **M4** (first-person suasive bundle, χ² = 10.09, p \< .001, R² gain = .028) shows the second-strongest effect.
- **M3** (prepositional evaluative NP, χ² = 4.98, p = .001), **M2** (sentence-boundary adverbial, χ² = 4.18, p = .002), and **M5** (adjective-noun-preposition, χ² = 2.55, p = .038) all reach conventional significance, with more modest R² gains (.010 each).
- **M1** (the headline bundle, χ² = 1.51, p = .198) is the sole exception, clearly non-significant, confirming that its underuse is topic-general — consistent with the KWIC finding in Section 4.3 of the main text that avoidance of this pattern reflects a register-general discourse strategy rather than a topic-specific one.

### JSD as a predictor of topic sensitivity

The relationship between JSD (computed descriptively at the tupling stage) and Wald test significance is directionally present but imperfect. JSD values for the six bundles, in descending order, are M3 (.0107), M4 (.0041), M5 (.0038), M6 (.0025), M1 (.0014), and M2 (.0003). M3 and M4, the two highest-JSD bundles, do show significant Wald tests, consistent with the JSD prediction. But M6, with the lowest JSD among the topic-sensitive bundles, shows by far the strongest Wald effect, and M2, with the lowest JSD in the entire set, is nonetheless clearly significant.

This apparent tension is theoretically resolvable: JSD captures whether the *aggregate* topic distribution differs between JPN and NS writers, whereas the Wald test captures whether *topic moderates the proficiency effect specifically* — related but genuinely distinct questions. M6 in particular is a case where the aggregate topic distributions look similar between groups (hence low JSD) while the proficiency-conditioned effect of topic is nonetheless very large.

### R² gains

R² gains from adding topic to the restricted model range from a modest .002 (M1) to a substantial .072 (M6), confirming that proficiency level remains the dominant predictor of evaluative bundle use across all six models while topic's contribution is clearly heterogeneous rather than uniformly negligible: for M6 in particular, topic is a first-order predictor in its own right.

------------------------------------------------------------------------

*This file accompanies Marchand, T. (2026). "The Gravity of Evaluation: Modelling Sparse Evaluative Bundle Frequencies in Learner Corpora with PPML." APCLC 2026 Proceedings.*
