# zseML

Machine learning for forecasting stock returns on the Zagreb Stock Exchange (ZSE), a thin frontier market.

> **📄 Read the paper:** &nbsp; [**Read online (HTML)**](https://lusiki.github.io/zseML/forecasting_thin_markets.html) &nbsp;·&nbsp; [**Download PDF**](https://lusiki.github.io/zseML/forecasting_thin_markets.pdf) &nbsp;·&nbsp; [**Landing page**](https://lusiki.github.io/zseML/)

## Project Overview

This project applies machine learning methods to forecast weekly returns of stocks listed on the Zagreb Stock Exchange (Zagrebačka burza). It documents whether ML can extract predictive signal in a thin, illiquid frontier market and how predictability varies across the liquidity spectrum.

**Authors:** Mislav Šagovac · Luka Šikić · Petra Palić

## 📄 The Paper

**Forecasting Returns in Thin Markets: A Machine Learning Approach to the Zagreb Stock Exchange**
— by Mislav Šagovac, Luka Šikić, and Petra Palić

| Format | Link |
|---|---|
| 🌐 Rendered HTML (read online) | **[lusiki.github.io/zseML/forecasting_thin_markets.html](https://lusiki.github.io/zseML/forecasting_thin_markets.html)** |
| 📕 PDF | **[lusiki.github.io/zseML/forecasting_thin_markets.pdf](https://lusiki.github.io/zseML/forecasting_thin_markets.pdf)** |
| 📜 Quarto source | [paper/forecasting_thin_markets.qmd](paper/forecasting_thin_markets.qmd) |

> The HTML and PDF are published via **GitHub Pages** from the [`docs/`](docs/) folder.
> See [Publishing the paper](#publishing-the-paper-github-pages) to enable Pages.

### Abstract

This paper investigates the out-of-sample predictability of weekly stock returns on the
Zagreb Stock Exchange (ZSE), a thin frontier market characterized by low liquidity and
concentrated ownership. We construct **over 1,100 predictors** from daily OHLCV data
(**2000–2024**) — technical indicators, time-series features, and wavelet decompositions —
and evaluate four models (**Elastic Net, Random Forest, XGBoost, and a shallow neural
network**) using a rigorous **nested rolling-window cross-validation** framework, assessed
through both statistical metrics and a realistic portfolio backtest. The results show modest
directional accuracy (46–53%), with nonlinear ensembles and neural networks outperforming the
linear benchmark, and a **strong monotonic liquidity gradient** in predictability.

### Key findings

- **Modest but real predictability:** directional accuracy of 46–53%, exceeding random chance and a random-walk benchmark.
- **Nonlinear models win:** Random Forest, XGBoost, and the neural network beat the penalized-linear Elastic Net in portfolio terms.
- **Ensembles add value:** forecast combinations (mean / median) match or exceed the best individual model.
- **Liquidity gradient:** portfolio Sharpe ratios rise monotonically from **0.17** (10 most liquid stocks) to **1.58** for the full universe (up to **1.97** for the best individual model).
- **Implementation caveat:** the highest gross returns come from illiquid stocks where transaction costs bite hardest — backtest figures are an upper bound.

### Earlier version (Croatian)

An earlier, Croatian-language version of this research line is also included:
*"Primjena modela strojnog učenja za predviđanje očekivanih prinosa dionica u RH."*

- HTML: [lusiki.github.io/zseML/zse_ml_paper.html](https://lusiki.github.io/zseML/zse_ml_paper.html)
- PDF: [lusiki.github.io/zseML/zse_ml_paper.pdf](https://lusiki.github.io/zseML/zse_ml_paper.pdf)
- Source: [paper/zse_ml_paper.qmd](paper/zse_ml_paper.qmd)

### A note on reproducibility

The **main paper's** result tables are self-contained and render in full. Two figures reference
chart images that are not bundled in this repository; add `fig3_cropped.png` and
`fig4_cropped.png` to the [`paper/`](paper/) folder and re-render to display them (clean
placeholders appear otherwise).

The **earlier Croatian paper** depends on proprietary trading data (originally under `F:/zse/`),
so its analysis code is **shown but not executed** (`eval: false`). All prose, equations, and
methodology render in full; re-run with the original data and `eval: true` to reproduce every
table and figure.

## Repository Structure

```
zseML/
├── scripts/          # R analysis scripts
│   ├── zse_prepare.R       # Raw data ingestion and feature engineering
│   ├── zse_prepare_ml.R    # ML framework setup, task/learner definitions
│   ├── run_job.R           # Single batch job executor (PBS array jobs)
│   ├── results.R           # Results aggregation and performance metrics
│   ├── paper_trading.R     # Paper trading simulation from model predictions
│   └── download.R          # Azure Blob Storage data downloader
├── cluster/          # HPC cluster job submission scripts
│   ├── run_jobs.sh         # PBS array job submission (892 tasks)
│   ├── zse_prepare_ml.sh   # PBS job for ML data preparation
│   └── download.sh         # Apptainer wrapper for download.R
├── paper/            # Research papers (source) and reports
│   ├── forecasting_thin_markets.qmd  # MAIN paper (English) — Quarto source → HTML + PDF
│   ├── zse_ml_paper.qmd              # Earlier paper (Croatian) — Quarto source → HTML + PDF + DOCX
│   ├── custom.scss                   # Custom HTML theme shared by both papers
│   ├── zse_ml_paper.docx             # Word export of the Croatian paper
│   ├── zse_ml_paper_v2.qmd           # Archived prior version
│   ├── finml_results.qmd             # FinML results dashboard
│   ├── pre_live.qmd                  # Live trading analysis report
│   ├── reference.bib                 # Bibliography
│   └── plot_cv.png                   # Cross-validation performance plot
├── docs/             # Published papers (served via GitHub Pages)
│   ├── index.html                    # Landing page (features the main paper)
│   ├── forecasting_thin_markets.html # Main paper — rendered, self-contained
│   ├── forecasting_thin_markets.pdf  # Main paper — PDF
│   ├── zse_ml_paper.html             # Earlier Croatian paper — HTML
│   ├── zse_ml_paper.pdf              # Earlier Croatian paper — PDF
│   └── zse_ml_paper.docx             # Earlier Croatian paper — DOCX
├── data/
│   └── results/
│       └── preds_perf.csv            # Cross-validation prediction performance metrics
└── zseML.Rproj       # RStudio project file
```

## Workflow

1. **Download** — `scripts/download.R` fetches predictor data from Azure Blob Storage
2. **Prepare** — `scripts/zse_prepare.R` processes raw price data and engineers features (TTR, finfeatures, tsfel via reticulate)
3. **ML Setup** — `scripts/zse_prepare_ml.R` defines mlr3 tasks, learners, cross-validation folds, and batchtools registries
4. **Train** — `cluster/run_jobs.sh` submits an array of PBS jobs, each executing `scripts/run_job.R`
5. **Evaluate** — `scripts/results.R` aggregates job outputs; `scripts/paper_trading.R` simulates trading performance

## Models

| Model | Type |
|---|---|
| `glmnet` | Regularized regression (elastic net) |
| `nnet` | Neural network |
| `ranger` | Random forest |
| `xgboost` | Gradient boosting |

Baselines: `mean_resp`, `median_resp`, `sum_resp`

## Requirements

- **R** with packages: `data.table`, `mlr3`, `mlr3verse`, `mlr3pipelines`, `mlr3tuning`, `mlr3extralearners`, `mlr3batchmark`, `batchtools`, `finfeatures`, `gausscov`, `AzureStor`, `reticulate`
- **Python** (via reticulate): `tsfel`
- **HPC**: PBS job scheduler, Apptainer

## Data

Raw predictor data is stored in Azure Blob Storage and is not included in this repository. Contact the authors for access.

## Rendering the papers

Both papers are [Quarto](https://quarto.org) documents that render to HTML and PDF (the
Croatian paper also to DOCX). PDF output needs a LaTeX engine (e.g. TinyTeX).

```bash
cd paper

# Main paper (English)
quarto render forecasting_thin_markets.qmd            # HTML + PDF
quarto render forecasting_thin_markets.qmd --to html  # just HTML

# Earlier paper (Croatian)
quarto render zse_ml_paper.qmd --to html
quarto render zse_ml_paper.qmd --to pdf
```

- The **main paper** is self-contained except for two figures: drop `fig3_cropped.png` and
  `fig4_cropped.png` into `paper/` to display them (placeholders appear otherwise).
- The **Croatian paper** keeps data-dependent chunks unexecuted (`eval: false`). To reproduce
  its full results, place the source data under `F:/zse/` (or update the paths), set
  `eval: true`, and install the R packages listed under [Requirements](#requirements).

After re-rendering, copy the new outputs into [`docs/`](docs/) and push.

## Publishing (GitHub Pages)

The rendered papers live in [`docs/`](docs/) and are served via GitHub Pages
(**Settings → Pages → Deploy from a branch → `main` / `/docs`**). They are live at:

- **Landing page:** `https://lusiki.github.io/zseML/`
- **Main paper (HTML):** `https://lusiki.github.io/zseML/forecasting_thin_markets.html`
- **Main paper (PDF):** `https://lusiki.github.io/zseML/forecasting_thin_markets.pdf`
- **Croatian paper (HTML):** `https://lusiki.github.io/zseML/zse_ml_paper.html`
- **Croatian paper (PDF):** `https://lusiki.github.io/zseML/zse_ml_paper.pdf`

## Original Source

Forked from [MislavSag/zseML](https://github.com/MislavSag/zseML).
