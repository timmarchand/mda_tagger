# Supplementary Materials

Supplementary materials for Marchand, T. (2026). "The Gravity of Evaluation: Modelling Sparse Evaluative Bundle Frequencies in Learner Corpora with PPML." *APCLC 2026 Proceedings*.

Referenced in the paper's Supplementary Materials statement (after Acknowledgements). This repository contains data too extensive for the page-limited main text: full coefficient tables across three estimators, the restricted-vs-full Wald test comparisons underlying the topic-effect statistics in Section 4.2, and the complete concordance output underlying the lexical realisation analysis in Section 4.3.

## Contents

| File | Contents | Referenced in |
|----|----|----|
| `coefficient-tables.md` | Full PPML / GLMM Poisson / GLMM Negative Binomial coefficient tables, all six bundles, all seven terms | Section 4.1 |
| `wald-tests.md` | Restricted-vs-full model Wald test comparisons, all six bundles | Section 4.2 |
| `concordance/*.csv` | Full KWIC concordance line data (all hits, JPN + NS), one file per bundle | Section 4.3 |
| `entropy/*.csv` | TTR and normalised entropy summary statistics, one file per bundle | Section 4.3, Table 3 |

## Bundle reference

All files use the paper's M1–M6 labelling, plus the two bundles excluded from the confirmatory regression analysis under the corrected DP criterion (Section 3.5).

| Label | Feature bundle | Status |
|----|----|----|
| M1 | `{{DT}} {{JJ<JJ>}} {{NN<NN>}}` | Modelled (underused) |
| M2 | `{{.}} {{RB<RB>}} {{,}}` | Modelled (overused) |
| M3 | `{{IN<PIN>}} {{DT}} {{JJ<JJ>}}` | Modelled (underused) |
| M4 | `{{PRP<FPP1>}} {{VBP<PUBV><SUAV><VPRT>}} {{IN<PIN>}}` | Modelled (overused) |
| M5 | `{{JJ<JJ>}} {{NN<NN>}} {{IN<PIN>}}` | Modelled (underused) |
| M6 | `{{PRP<PIT>}} {{VBZ<BEMA><VPRT>}} {{JJ<PRED>}}` | Modelled (overused) |
| NOMZ | `{{DT}} {{JJ<JJ>}} {{NN<NOMZ>}}` | Excluded (DP \> 0.75) |
| WH-SUAV | `{{WRB}} {{PRP<FPP1>}} {{VBP<PUBV><SUAV><VPRT>}}` | Excluded (DP \> 0.75) |

## Concordance files (`concordance/`)

One CSV per bundle, generated via the MDA Tagger's concordancing function (Section 3.4 of the main text), containing every hit across both JPN and NS sub-corpora.

**Filenames:** `<label>-concordance.csv` (e.g. `M1-concordance.csv`, `M4-concordance.csv`, `NOMZ-concordance.csv`)

**Columns:**

| Column | Description |
|----|----|
| `File` | Source file ID (e.g. `W_JPN_PTJ_A2_0_066`) |
| `Category` | Sub-corpus (`JPN` or `ENS`) |
| `Left` | Left context (up to 5 tokens) |
| `Node` | The matched bundle realisation |
| `Right` | Right context (up to 5 tokens) |
| `Freq` | Corpus-wide frequency of this specific lexical realisation |
| `TTR` | Running type-token ratio at this row (cumulative, as displayed in the tool) |

## Entropy summary files (`entropy/`)

One CSV per bundle, giving the aggregate statistics underlying Table 3 of the main text.

**Filenames:** `<label>-entropy.csv` (e.g. `M1-entropy.csv`)

**Columns:**

| Column     | Description                                                 |
|------------|-------------------------------------------------------------|
| `Category` | Sub-corpus (`JPN` or `ENS`)                                 |
| `Hits`     | Total tokens (occurrences) of the bundle in this sub-corpus |
| `Types`    | Number of distinct lexical realisations                     |
| `TTR`      | Types / Hits                                                |
| `Entropy`  | Normalised Shannon entropy (H_norm), 0–1                    |

## Notes on cross-checking

Every number reported in the main text and in `coefficient-tables.md` / `wald-tests.md` was checked against these underlying files before publication (TTR = Types/Hits verified row by row). If you find a discrepancy, please open an issue.

------------------------------------------------------------------------

*Corresponding data pipeline and tagging software: [MDA Tagger](https://github.com/timmarchand/mda_tagger).*
