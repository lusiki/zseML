# zseML

Machine learning models for predicting stock returns on the Zagreb Stock Exchange (ZSE).

> **📄 Read the paper:** &nbsp; [**Read online (HTML)**](https://lusiki.github.io/zseML/zse_ml_paper.html) &nbsp;·&nbsp; [**Download PDF**](https://lusiki.github.io/zseML/zse_ml_paper.pdf) &nbsp;·&nbsp; [**Landing page**](https://lusiki.github.io/zseML/)

## Project Overview

This project applies machine learning methods to predict expected returns of stocks listed on the Zagreb Stock Exchange (Zagrebačka burza). It accompanies the research paper *"Primjena modela strojnog učenja za predviđanje očekivanih prinosa dionica u RH"* (Application of ML models for predicting expected stock returns in Croatia).

## 📄 The Paper

**Primjena modela strojnog učenja za predviđanje očekivanih prinosa dionica u RH**
*(Application of Machine Learning Models for Predicting Expected Stock Returns in Croatia)*
— by Mislav Sagovac

| Format | Link |
|---|---|
| 🌐 Rendered HTML (read online) | **[lusiki.github.io/zseML/zse_ml_paper.html](https://lusiki.github.io/zseML/zse_ml_paper.html)** |
| 📕 PDF | **[lusiki.github.io/zseML/zse_ml_paper.pdf](https://lusiki.github.io/zseML/zse_ml_paper.pdf)** |
| 📝 Word (DOCX) | [paper/zse_ml_paper.docx](paper/zse_ml_paper.docx) |
| 📜 Quarto source | [paper/zse_ml_paper.qmd](paper/zse_ml_paper.qmd) |

> The HTML and PDF are published via **GitHub Pages** from the [`docs/`](docs/) folder.
> See [Publishing the paper](#publishing-the-paper-github-pages) to enable Pages.

### Abstract

The paper investigates the application of machine learning models for predicting expected
stock returns on the Zagreb Stock Exchange (ZSE) over **2000–2024**. It uses a large set of
predictors derived from daily trading data together with a **nested rolling-window
cross-validation (NRWCV)** scheme. Four models — **Random Forest, XGBoost, GLMNET, and a
neural network (NNET)** — are compared alongside simple ensembles. The results show that
tree-based models (Random Forest) and neural networks achieve the best risk/return profile,
and that ensembling predictions further improves performance on the relatively illiquid and
shallow Croatian capital market.

### Key findings

- **Random Forest (`ranger`)** and **neural networks (`nnet`)** deliver the best risk-adjusted returns (highest Sharpe ratio; `nnet` ≈ 1.53).
- **Ensembles** of predictions (median / mean) outperform individual models.
- Novel **predictors engineered from daily trading data** are critical to model performance.
- Machine learning shows real promise even on a **shallow, illiquid** market like Croatia's.

### A note on reproducibility

The analysis chunks in the paper depend on proprietary trading data and a results registry
that are **not part of this repository** (originally under `F:/zse/`). To keep the document
fully renderable, code chunks are **shown but not executed** (`eval: false` is set globally).
All prose, equations, and methodology render in full; re-running [`paper/zse_ml_paper.qmd`](paper/zse_ml_paper.qmd)
with the original data and `eval: true` reproduces every table and figure.

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
├── paper/            # Research paper (source) and reports
│   ├── zse_ml_paper.qmd    # Main research paper — Quarto source (HTML + PDF + DOCX)
│   ├── custom.scss         # Custom HTML theme for the paper
│   ├── zse_ml_paper.docx   # Word export of paper
│   ├── zse_ml_paper_v2.qmd # Archived prior version
│   ├── finml_results.qmd   # FinML results dashboard
│   ├── pre_live.qmd        # Live trading analysis report
│   ├── reference.bib       # Bibliography
│   └── plot_cv.png         # Cross-validation performance plot
├── docs/             # Published paper (served via GitHub Pages)
│   ├── index.html          # Landing page
│   ├── zse_ml_paper.html   # Rendered, self-contained paper
│   ├── zse_ml_paper.pdf    # Rendered PDF
│   └── zse_ml_paper.docx   # Word version
├── data/
│   └── results/
│       └── preds_perf.csv  # Cross-validation prediction performance metrics
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

## Rendering the paper

The paper is a [Quarto](https://quarto.org) document that renders to HTML, PDF, and DOCX.

```bash
cd paper
quarto render zse_ml_paper.qmd            # all formats
quarto render zse_ml_paper.qmd --to html  # just HTML
quarto render zse_ml_paper.qmd --to pdf   # just PDF (needs a LaTeX engine, e.g. TinyTeX)
```

By default the data-dependent code chunks are not executed (`eval: false`). To reproduce the
full results, place the source data under `F:/zse/` (or update the paths in the document),
set `eval: true`, and install the R packages listed under [Requirements](#requirements).

## Publishing the paper (GitHub Pages)

The rendered paper lives in [`docs/`](docs/) and is ready to be served by GitHub Pages:

1. Go to **Settings → Pages** in this repository.
2. Under **Build and deployment**, set **Source = Deploy from a branch**.
3. Choose branch **`main`** and folder **`/docs`**, then **Save**.

Within a minute the paper is live at:

- **Landing page:** `https://lusiki.github.io/zseML/`
- **Paper (HTML):** `https://lusiki.github.io/zseML/zse_ml_paper.html`
- **Paper (PDF):** `https://lusiki.github.io/zseML/zse_ml_paper.pdf`

After re-rendering, refresh `docs/` with the new outputs and push.

## Original Source

Forked from [MislavSag/zseML](https://github.com/MislavSag/zseML).
