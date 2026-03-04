"""
Visit-to-visit Blood Pressure Variability (VVV BPV) Metrics

This module provides functions for calculating blood pressure variability
metrics used in cardiovascular disease (CVD) risk prediction:

  - Standard Deviation (SD)
  - Coefficient of Variation (CV)
  - Average Real Variability (ARV)

Reference thresholds (from dose-response meta-analysis):
  - SBP SD  >= 6.72 mmHg  -> 10% increased CVD risk
  - SBP CV  >= 9.05%       -> 10% increased CVD risk

Established cut-off values for CVD prediction:
  - SBP: SD 19 mmHg, CV 14%, ARV 15 mmHg
  - DBP: SD 11 mmHg, CV 12%, ARV 11 mmHg
"""

from __future__ import annotations

import numpy as np
import pandas as pd

# ---------------------------------------------------------------------------
# Published thresholds (from dose-response meta-analysis)
# ---------------------------------------------------------------------------

# 10 % increased CVD risk at these BPV values
META_THRESHOLD_SBP_SD: float = 6.72   # mmHg
META_THRESHOLD_SBP_CV: float = 9.05   # %

# Established cut-off values for CVD prediction
CUTOFF_SBP_SD: float = 19.0    # mmHg
CUTOFF_SBP_CV: float = 14.0    # %
CUTOFF_SBP_ARV: float = 15.0   # mmHg

CUTOFF_DBP_SD: float = 11.0    # mmHg
CUTOFF_DBP_CV: float = 12.0    # %
CUTOFF_DBP_ARV: float = 11.0   # mmHg

# Minimum number of BP measurements recommended for reliable BPV estimation
MIN_MEASUREMENTS: int = 3


# ---------------------------------------------------------------------------
# Core metric functions (scalar / array level)
# ---------------------------------------------------------------------------

def sd(values: np.ndarray | list[float], ddof: int = 1) -> float:
    """Standard deviation of a series of blood pressure readings.

    Parameters
    ----------
    values:
        Sequence of blood pressure measurements (mmHg).
    ddof:
        Delta degrees of freedom (default 1 for sample SD).

    Returns
    -------
    float
        Standard deviation in mmHg, or NaN if fewer than
        ``MIN_MEASUREMENTS`` values are present.
    """
    arr = np.asarray(values, dtype=float)
    arr = arr[~np.isnan(arr)]
    if len(arr) < MIN_MEASUREMENTS:
        return float("nan")
    return float(np.std(arr, ddof=ddof))


def cv(values: np.ndarray | list[float], ddof: int = 1) -> float:
    """Coefficient of variation (SD / mean * 100) of BP readings.

    Parameters
    ----------
    values:
        Sequence of blood pressure measurements (mmHg).
    ddof:
        Delta degrees of freedom passed to the SD calculation.

    Returns
    -------
    float
        CV as a percentage, or NaN if mean is zero or fewer than
        ``MIN_MEASUREMENTS`` values are present.
    """
    arr = np.asarray(values, dtype=float)
    arr = arr[~np.isnan(arr)]
    if len(arr) < MIN_MEASUREMENTS:
        return float("nan")
    mean_val = np.mean(arr)
    if mean_val == 0:
        return float("nan")
    return float(np.std(arr, ddof=ddof) / mean_val * 100.0)


def arv(values: np.ndarray | list[float]) -> float:
    """Average Real Variability (mean of successive absolute differences).

    ARV = mean(|BP_{i+1} - BP_i|)

    Parameters
    ----------
    values:
        Sequence of blood pressure measurements ordered by visit date
        (mmHg).

    Returns
    -------
    float
        ARV in mmHg, or NaN if fewer than ``MIN_MEASUREMENTS`` values
        are present (at least 2 differences needed, so >= 3 readings).
    """
    arr = np.asarray(values, dtype=float)
    arr = arr[~np.isnan(arr)]
    if len(arr) < MIN_MEASUREMENTS:
        return float("nan")
    return float(np.mean(np.abs(np.diff(arr))))


# ---------------------------------------------------------------------------
# Patient-level DataFrame helpers
# ---------------------------------------------------------------------------

def compute_bpv_metrics(
    df: pd.DataFrame,
    patient_col: str = "patient_id",
    sbp_col: str = "sbp",
    dbp_col: str = "dbp",
    date_col: str | None = "visit_date",
) -> pd.DataFrame:
    """Calculate VVV BPV metrics for each patient in a long-format DataFrame.

    Parameters
    ----------
    df:
        Long-format DataFrame with one BP reading per row.
    patient_col:
        Column identifying each patient.
    sbp_col:
        Column containing systolic BP values (mmHg).
    dbp_col:
        Column containing diastolic BP values (mmHg).
    date_col:
        Optional column containing visit dates. When provided readings are
        sorted chronologically before computing ARV.

    Returns
    -------
    pd.DataFrame
        One row per patient with columns:
        ``sbp_sd``, ``sbp_cv``, ``sbp_arv``,
        ``dbp_sd``, ``dbp_cv``, ``dbp_arv``,
        ``sbp_mean``, ``dbp_mean``, ``n_measurements``.
    """
    if date_col and date_col in df.columns:
        df = df.sort_values([patient_col, date_col])
    else:
        df = df.copy()

    records = []
    for pid, grp in df.groupby(patient_col, sort=False):
        sbp_vals = grp[sbp_col].dropna().to_numpy(dtype=float)
        dbp_vals = grp[dbp_col].dropna().to_numpy(dtype=float)

        records.append(
            {
                patient_col: pid,
                "n_measurements": len(sbp_vals),
                "sbp_mean": float(np.mean(sbp_vals)) if len(sbp_vals) > 0 else float("nan"),
                "dbp_mean": float(np.mean(dbp_vals)) if len(dbp_vals) > 0 else float("nan"),
                "sbp_sd": sd(sbp_vals),
                "sbp_cv": cv(sbp_vals),
                "sbp_arv": arv(sbp_vals),
                "dbp_sd": sd(dbp_vals),
                "dbp_cv": cv(dbp_vals),
                "dbp_arv": arv(dbp_vals),
            }
        )

    return pd.DataFrame(records)


def classify_high_bpv(
    metrics_df: pd.DataFrame,
    metric: str = "sd",
    bp: str = "sbp",
) -> pd.Series:
    """Classify patients as high/low VVV BPV using established cut-off values.

    Parameters
    ----------
    metrics_df:
        DataFrame returned by :func:`compute_bpv_metrics`.
    metric:
        BPV metric to use. One of ``"sd"``, ``"cv"``, or ``"arv"``
        (case-insensitive).
    bp:
        Blood pressure component. One of ``"sbp"`` or ``"dbp"``
        (case-insensitive).

    Returns
    -------
    pd.Series
        Boolean series; ``True`` indicates high BPV (>= cut-off).
    """
    metric = metric.lower()
    bp = bp.lower()

    cutoffs = {
        ("sbp", "sd"): CUTOFF_SBP_SD,
        ("sbp", "cv"): CUTOFF_SBP_CV,
        ("sbp", "arv"): CUTOFF_SBP_ARV,
        ("dbp", "sd"): CUTOFF_DBP_SD,
        ("dbp", "cv"): CUTOFF_DBP_CV,
        ("dbp", "arv"): CUTOFF_DBP_ARV,
    }

    key = (bp, metric)
    if key not in cutoffs:
        raise ValueError(
            f"Unknown combination bp='{bp}', metric='{metric}'. "
            "Choose bp in {'sbp','dbp'} and metric in {'sd','cv','arv'}."
        )

    col = f"{bp}_{metric}"
    if col not in metrics_df.columns:
        raise KeyError(f"Column '{col}' not found in metrics_df.")

    cutoff = cutoffs[key]
    return metrics_df[col] >= cutoff
