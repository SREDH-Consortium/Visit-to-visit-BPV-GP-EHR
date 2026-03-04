"""
CVD Risk Prediction Model Incorporating Visit-to-visit BPV

This module assembles the full CVD risk prediction workflow:

  1. Load / prepare patient data.
  2. Calculate VVV BPV metrics (SD, CV, ARV) from repeated BP readings.
  3. Merge BPV metrics with patient-level covariates.
  4. Fit and evaluate competing Cox proportional-hazards models:
       - *Single-visit model*: baseline CVD risk factors + mean SBP.
       - *SD model*: single-visit model with SBP mean replaced by SBP SD.
       - *CV model*: single-visit model with SBP mean replaced by SBP CV.
  5. Compare models using bootstrap C-index with 95 % CI.

Reported improvements over single-visit model (from thesis):
  - SD model: ΔC-index = 0.117 (95 % CI: 0.029–0.204)
  - CV model: ΔC-index = 0.122 (95 % CI: 0.013–0.227)
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from bpv_metrics import compute_bpv_metrics
from survival_analysis import compare_models, fit_cox_model, c_index


# ---------------------------------------------------------------------------
# Standard covariate sets
# ---------------------------------------------------------------------------

# Covariates shared by all models (excluding BP representation)
BASE_COVARIATES = [
    "age",
    "sex",           # 0 = female, 1 = male
    "smoking",       # 0 = non-smoker, 1 = ex-smoker, 2 = current
    "diabetes",      # 0/1
    "total_cholesterol",
    "hdl_cholesterol",
    "on_antihypertensives",  # 0/1
]

# BP representation in each model
SINGLE_VISIT_BP = ["sbp_mean"]    # mean of repeated readings (proxy)
SD_BP = ["sbp_sd"]
CV_BP = ["sbp_cv"]


def build_model_dataset(
    bp_df: pd.DataFrame,
    covariate_df: pd.DataFrame,
    duration_col: str = "follow_up_years",
    event_col: str = "cvd_event",
    patient_col: str = "patient_id",
    sbp_col: str = "sbp",
    dbp_col: str = "dbp",
    date_col: str = "visit_date",
) -> pd.DataFrame:
    """Merge BPV metrics with patient covariates.

    Parameters
    ----------
    bp_df:
        Long-format DataFrame with repeated BP readings.
    covariate_df:
        Wide-format DataFrame with one row per patient containing
        :attr:`BASE_COVARIATES`, ``duration_col``, and ``event_col``.
    duration_col:
        Follow-up time column in ``covariate_df``.
    event_col:
        CVD event indicator column in ``covariate_df``.
    patient_col:
        Patient identifier column present in both DataFrames.
    sbp_col, dbp_col, date_col:
        Column names in ``bp_df``.

    Returns
    -------
    pd.DataFrame
        One row per patient suitable for survival modelling.
    """
    bpv = compute_bpv_metrics(
        bp_df,
        patient_col=patient_col,
        sbp_col=sbp_col,
        dbp_col=dbp_col,
        date_col=date_col,
    )
    merged = covariate_df.merge(bpv, on=patient_col, how="inner")
    return merged


def run_cvd_model_comparison(
    model_df: pd.DataFrame,
    duration_col: str = "follow_up_years",
    event_col: str = "cvd_event",
    base_covariates: list[str] | None = None,
    n_bootstrap: int = 1000,
    random_state: int = 42,
) -> dict:
    """Fit and compare single-visit, SD, and CV CVD risk models.

    Parameters
    ----------
    model_df:
        Wide-format patient DataFrame returned by :func:`build_model_dataset`.
    duration_col, event_col:
        Survival columns.
    base_covariates:
        Non-BP predictors; defaults to :attr:`BASE_COVARIATES`.
    n_bootstrap:
        Bootstrap replicates for confidence intervals.
    random_state:
        Random seed.

    Returns
    -------
    dict
        Keys: ``"single_visit"``, ``"sd_model"``, ``"cv_model"``.
        Each value is a sub-dict with ``"c_index"``, ``"delta_c_index"``,
        ``"ci_lower"``, ``"ci_upper"``, and ``"cox_model"``.
    """
    if base_covariates is None:
        base_covariates = BASE_COVARIATES

    # Available covariates (subset to columns present in model_df)
    available = [c for c in base_covariates if c in model_df.columns]

    single_covs = available + SINGLE_VISIT_BP
    sd_covs = available + SD_BP
    cv_covs = available + CV_BP

    results: dict = {}

    # Single-visit model (baseline)
    sv_model = fit_cox_model(model_df, duration_col, event_col, single_covs)
    sv_ci = c_index(model_df, duration_col, event_col, single_covs, sv_model)
    results["single_visit"] = {
        "c_index": sv_ci,
        "delta_c_index": 0.0,
        "ci_lower": 0.0,
        "ci_upper": 0.0,
        "cox_model": sv_model,
    }

    # SD model
    sd_comparison = compare_models(
        model_df, duration_col, event_col,
        base_covariates=single_covs,
        extended_covariates=sd_covs,
        n_bootstrap=n_bootstrap,
        random_state=random_state,
    )
    sd_model = fit_cox_model(model_df, duration_col, event_col, sd_covs)
    results["sd_model"] = {
        "c_index": sd_comparison["extended_c_index"],
        "delta_c_index": sd_comparison["delta_c_index"],
        "ci_lower": sd_comparison["ci_lower"],
        "ci_upper": sd_comparison["ci_upper"],
        "cox_model": sd_model,
    }

    # CV model
    cv_comparison = compare_models(
        model_df, duration_col, event_col,
        base_covariates=single_covs,
        extended_covariates=cv_covs,
        n_bootstrap=n_bootstrap,
        random_state=random_state,
    )
    cv_model = fit_cox_model(model_df, duration_col, event_col, cv_covs)
    results["cv_model"] = {
        "c_index": cv_comparison["extended_c_index"],
        "delta_c_index": cv_comparison["delta_c_index"],
        "ci_lower": cv_comparison["ci_lower"],
        "ci_upper": cv_comparison["ci_upper"],
        "cox_model": cv_model,
    }

    return results


def print_model_comparison(results: dict) -> None:
    """Print a formatted model comparison table.

    Parameters
    ----------
    results:
        Dict returned by :func:`run_cvd_model_comparison`.
    """
    header = f"{'Model':<20} {'C-index':>8} {'ΔC-index':>10} {'95 % CI':>20}"
    print(header)
    print("-" * len(header))
    for name, res in results.items():
        ci_str = f"({res['ci_lower']:.3f}, {res['ci_upper']:.3f})"
        print(
            f"{name:<20} {res['c_index']:>8.3f} "
            f"{res['delta_c_index']:>10.3f} {ci_str:>20}"
        )
