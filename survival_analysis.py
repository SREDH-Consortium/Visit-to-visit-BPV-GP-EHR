"""
Survival Analysis for Visit-to-visit BPV and CVD Risk

This module implements time-to-event (survival) analyses used to:

  1. Establish decision-making cut-off values for VVV BPV.
  2. Determine the minimum number of BP measurements required.
  3. Develop and validate Cox proportional-hazards models for CVD risk
     incorporating VVV BPV metrics (SD, CV, ARV).

Key results reproduced from the thesis:
  - Adding SD to the single-visit model increased C-index by 0.117
    (95 % CI: 0.029–0.204).
  - Adding CV to the single-visit model increased C-index by 0.122
    (95 % CI: 0.013–0.227).
"""

from __future__ import annotations

from typing import Sequence

import numpy as np
import pandas as pd
from lifelines import CoxPHFitter, KaplanMeierFitter
from lifelines.statistics import logrank_test
from lifelines.utils import concordance_index


# ---------------------------------------------------------------------------
# Kaplan-Meier analysis
# ---------------------------------------------------------------------------

def km_analysis(
    df: pd.DataFrame,
    duration_col: str,
    event_col: str,
    group_col: str,
    group_labels: dict | None = None,
    ax=None,
) -> dict:
    """Kaplan-Meier survival analysis with log-rank test.

    Parameters
    ----------
    df:
        DataFrame containing one row per patient.
    duration_col:
        Column with follow-up time (e.g., years to CVD event or censoring).
    event_col:
        Binary column; 1 = event occurred, 0 = censored.
    group_col:
        Column defining two comparison groups (e.g., high vs. low BPV).
    group_labels:
        Optional dict mapping group values to display strings.
    ax:
        Optional matplotlib Axes for plotting.

    Returns
    -------
    dict
        Keys: ``"kmf_groups"`` (dict of fitted KMF objects),
        ``"logrank_p"`` (log-rank test p-value),
        ``"logrank_result"`` (full result object).
    """
    groups = df[group_col].unique()
    kmf_objects: dict = {}

    for grp in groups:
        mask = df[group_col] == grp
        label = group_labels.get(grp, str(grp)) if group_labels else str(grp)
        kmf = KaplanMeierFitter()
        kmf.fit(
            df.loc[mask, duration_col],
            event_observed=df.loc[mask, event_col],
            label=label,
        )
        if ax is not None:
            kmf.plot_survival_function(ax=ax)
        kmf_objects[grp] = kmf

    # Log-rank test (requires exactly 2 groups)
    group_values = list(groups)
    if len(group_values) == 2:
        g0 = df[group_col] == group_values[0]
        g1 = df[group_col] == group_values[1]
        result = logrank_test(
            df.loc[g0, duration_col],
            df.loc[g1, duration_col],
            event_observed_A=df.loc[g0, event_col],
            event_observed_B=df.loc[g1, event_col],
        )
        p_value = result.p_value
    else:
        result = None
        p_value = float("nan")

    return {
        "kmf_groups": kmf_objects,
        "logrank_p": p_value,
        "logrank_result": result,
    }


# ---------------------------------------------------------------------------
# Cox proportional-hazards models
# ---------------------------------------------------------------------------

def fit_cox_model(
    df: pd.DataFrame,
    duration_col: str,
    event_col: str,
    covariates: list[str],
    penalizer: float = 0.0,
    l1_ratio: float = 0.0,
) -> CoxPHFitter:
    """Fit a Cox proportional-hazards model.

    Parameters
    ----------
    df:
        DataFrame with one row per patient.
    duration_col:
        Column with follow-up time.
    event_col:
        Binary event indicator.
    covariates:
        List of predictor column names.
    penalizer:
        Ridge / Elastic-net penalty (default 0 = no regularisation).
    l1_ratio:
        L1 ratio for elastic-net (0 = ridge, 1 = lasso).

    Returns
    -------
    CoxPHFitter
        Fitted model object.
    """
    model_df = df[[duration_col, event_col] + covariates].dropna()
    cph = CoxPHFitter(penalizer=penalizer, l1_ratio=l1_ratio)
    cph.fit(model_df, duration_col=duration_col, event_col=event_col)
    return cph


def c_index(
    df: pd.DataFrame,
    duration_col: str,
    event_col: str,
    covariates: list[str],
    cox_model: CoxPHFitter | None = None,
) -> float:
    """Compute Harrell's C-index for a Cox model.

    Parameters
    ----------
    df:
        Patient data.
    duration_col, event_col, covariates:
        As in :func:`fit_cox_model`.
    cox_model:
        Pre-fitted Cox model. If None a new model is fitted on *df*.

    Returns
    -------
    float
        Harrell's C-index (0.5 = random, 1.0 = perfect discrimination).
    """
    model_df = df[[duration_col, event_col] + covariates].dropna()
    if cox_model is None:
        cox_model = fit_cox_model(
            model_df, duration_col, event_col, covariates
        )
    predictions = cox_model.predict_partial_hazard(model_df)
    return concordance_index(
        model_df[duration_col],
        -predictions,
        model_df[event_col],
    )


def compare_models(
    df: pd.DataFrame,
    duration_col: str,
    event_col: str,
    base_covariates: list[str],
    extended_covariates: list[str],
    n_bootstrap: int = 1000,
    random_state: int = 42,
) -> dict:
    """Compare a base and an extended Cox model via bootstrap C-index.

    Fits both models on *df* and estimates the C-index difference with a
    bootstrap 95 % confidence interval.

    Parameters
    ----------
    df:
        Patient data.
    duration_col, event_col:
        As in :func:`fit_cox_model`.
    base_covariates:
        Predictors for the baseline model (e.g., single-visit BP + covariates).
    extended_covariates:
        Predictors for the extended model (adds BPV metric to the base set).
    n_bootstrap:
        Number of bootstrap replicates.
    random_state:
        Random seed for reproducibility.

    Returns
    -------
    dict
        Keys:
        ``"base_c_index"``, ``"extended_c_index"``,
        ``"delta_c_index"``, ``"ci_lower"``, ``"ci_upper"``.
    """
    rng = np.random.default_rng(random_state)

    base_c = c_index(df, duration_col, event_col, base_covariates)
    ext_c = c_index(df, duration_col, event_col, extended_covariates)

    deltas: list[float] = []
    n = len(df)
    for _ in range(n_bootstrap):
        idx = rng.integers(0, n, size=n)
        boot = df.iloc[idx]
        try:
            bc = c_index(boot, duration_col, event_col, base_covariates)
            ec = c_index(boot, duration_col, event_col, extended_covariates)
            deltas.append(ec - bc)
        except Exception:
            continue

    deltas_arr = np.array(deltas)
    ci_lower = float(np.percentile(deltas_arr, 2.5))
    ci_upper = float(np.percentile(deltas_arr, 97.5))

    return {
        "base_c_index": base_c,
        "extended_c_index": ext_c,
        "delta_c_index": ext_c - base_c,
        "ci_lower": ci_lower,
        "ci_upper": ci_upper,
    }


# ---------------------------------------------------------------------------
# Optimal cut-off determination
# ---------------------------------------------------------------------------

def youden_cutoff(
    df: pd.DataFrame,
    bpv_col: str,
    event_col: str,
    duration_col: str,
    candidate_percentiles: Sequence[float] | None = None,
) -> dict:
    """Find the optimal BPV threshold using Youden's index on survival data.

    For each candidate threshold the data are dichotomised and sensitivity /
    specificity are estimated against the observed binary event indicator
    (ignoring censoring time).  The threshold that maximises Youden's index
    (sensitivity + specificity - 1) is returned.

    Parameters
    ----------
    df:
        Patient-level DataFrame.
    bpv_col:
        Column containing the BPV metric.
    event_col:
        Binary CVD event indicator.
    duration_col:
        Follow-up time column.
    candidate_percentiles:
        Percentiles of ``bpv_col`` to test as candidate thresholds.
        Defaults to the 10th–90th percentile in steps of 5.

    Returns
    -------
    dict
        Keys: ``"threshold"``, ``"sensitivity"``, ``"specificity"``,
        ``"youden_index"``.
    """
    if candidate_percentiles is None:
        candidate_percentiles = np.arange(10, 91, 5)

    sub = df[[bpv_col, event_col, duration_col]].dropna()
    thresholds = np.percentile(sub[bpv_col], candidate_percentiles)
    thresholds = np.unique(thresholds)

    best = {"threshold": float("nan"), "youden_index": -np.inf,
            "sensitivity": float("nan"), "specificity": float("nan")}

    for thr in thresholds:
        high = sub[bpv_col] >= thr
        tp = int(((high) & (sub[event_col] == 1)).sum())
        fn = int(((~high) & (sub[event_col] == 1)).sum())
        tn = int(((~high) & (sub[event_col] == 0)).sum())
        fp = int(((high) & (sub[event_col] == 0)).sum())

        sens = tp / (tp + fn) if (tp + fn) > 0 else float("nan")
        spec = tn / (tn + fp) if (tn + fp) > 0 else float("nan")

        if np.isnan(sens) or np.isnan(spec):
            continue

        youden = sens + spec - 1.0
        if youden > best["youden_index"]:
            best = {
                "threshold": float(thr),
                "youden_index": float(youden),
                "sensitivity": float(sens),
                "specificity": float(spec),
            }

    return best


def minimum_measurements_analysis(
    df: pd.DataFrame,
    patient_col: str,
    duration_col: str,
    event_col: str,
    sbp_col: str,
    date_col: str,
    max_n: int = 10,
    n_bootstrap: int = 200,
    random_state: int = 42,
) -> pd.DataFrame:
    """Assess C-index stability as a function of measurement count.

    For each minimum measurement count *k* (from ``MIN_MEASUREMENTS`` to
    ``max_n``) only patients with >= *k* readings are included and the SD is
    computed on the first *k* visits.  A Cox model with SBP SD is fitted and
    the C-index is recorded.

    Parameters
    ----------
    df:
        Long-format BP DataFrame.
    patient_col, duration_col, event_col, sbp_col, date_col:
        Column names.
    max_n:
        Maximum number of measurements to evaluate.
    n_bootstrap:
        Bootstrap replicates for 95 % CI at each *k*.
    random_state:
        Random seed.

    Returns
    -------
    pd.DataFrame
        Columns: ``n_measurements``, ``c_index``, ``ci_lower``, ``ci_upper``.
    """
    from bpv_metrics import sd as _sd, MIN_MEASUREMENTS  # local import

    rng = np.random.default_rng(random_state)
    df_sorted = df.sort_values([patient_col, date_col])

    rows = []
    for k in range(MIN_MEASUREMENTS, max_n + 1):
        # Build patient-level dataset using first k readings
        records: list[dict] = []
        for pid, grp in df_sorted.groupby(patient_col, sort=False):
            if len(grp) < k:
                continue
            sbp_vals = grp[sbp_col].iloc[:k].to_numpy(dtype=float)
            records.append(
                {
                    patient_col: pid,
                    "sbp_sd_k": _sd(sbp_vals),
                    duration_col: grp[duration_col].iloc[0],
                    event_col: grp[event_col].iloc[0],
                }
            )

        pat_df = pd.DataFrame(records).dropna()
        if len(pat_df) < 20:
            continue

        try:
            ci_val = c_index(pat_df, duration_col, event_col, ["sbp_sd_k"])
        except Exception:
            continue

        # Bootstrap CI
        boot_cis: list[float] = []
        n = len(pat_df)
        for _ in range(n_bootstrap):
            idx = rng.integers(0, n, size=n)
            boot = pat_df.iloc[idx]
            try:
                boot_cis.append(
                    c_index(boot, duration_col, event_col, ["sbp_sd_k"])
                )
            except Exception:
                continue

        if boot_cis:
            ci_lower = float(np.percentile(boot_cis, 2.5))
            ci_upper = float(np.percentile(boot_cis, 97.5))
        else:
            ci_lower = ci_upper = float("nan")

        rows.append(
            {
                "n_measurements": k,
                "c_index": ci_val,
                "ci_lower": ci_lower,
                "ci_upper": ci_upper,
            }
        )

    return pd.DataFrame(rows)
