# zseML

Machine learning models for predicting stock returns on the Zagreb Stock Exchange (ZSE).

## Project Overview

This project applies machine learning methods to predict expected returns of stocks listed on the Zagreb Stock Exchange (Zagrebačka burza). It accompanies the research paper *"Primjena modela strojnog učenja za predviđanje očekivanih prinosa dionica u RH"* (Application of ML models for predicting expected stock returns in Croatia).

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
├── paper/            # Research paper and reports
│   ├── zse_ml_paper.qmd    # Main research paper (Quarto/Croatian)
│   ├── zse_ml_paper_v2.qmd # Updated paper version
│   ├── zse_ml_paper.docx   # Word export of paper
│   ├── finml_results.qmd   # FinML results dashboard
│   ├── pre_live.qmd        # Live trading analysis report
│   ├── reference.bib       # Bibliography
│   └── plot_cv.png         # Cross-validation performance plot
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

## Paper

The accompanying research paper is in [`paper/zse_ml_paper_v2.qmd`](paper/zse_ml_paper_v2.qmd).

## Original Source

Forked from [MislavSag/zseML](https://github.com/MislavSag/zseML).
