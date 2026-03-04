"""
Meta-analysis Support for VVV BPV and CVD Risk

This module provides tools for:

  - Fixed-effects and random-effects meta-analysis of BPV–CVD associations.
  - Dose-response (spline) meta-analysis to derive BPV thresholds.
  - Forest-plot and funnel-plot generation.

Key findings reproduced from the thesis:
  - Dose-response meta-analysis: SBP SD 6.72 mmHg or CV 9.05% is associated
    with a 10 % increased risk of CVD.
"""

from __future__ import annotations

import numpy as np
import pandas as pd


# ---------------------------------------------------------------------------
# Basic meta-analysis (DerSimonian-Laird random effects)
# ---------------------------------------------------------------------------

def meta_analysis(
    log_rr: np.ndarray | list[float],
    se: np.ndarray | list[float],
    study_labels: list[str] | None = None,
) -> dict:
    """DerSimonian-Laird random-effects meta-analysis.

    Parameters
    ----------
    log_rr:
        Array of study-level log relative risks (or log hazard ratios).
    se:
        Array of corresponding standard errors.
    study_labels:
        Optional list of study identifiers.

    Returns
    -------
    dict
        Keys: ``"pooled_log_rr"``, ``"pooled_se"``, ``"pooled_rr"``,
        ``"ci_lower_rr"``, ``"ci_upper_rr"``, ``"i_squared"``,
        ``"tau_squared"``, ``"q_stat"``, ``"q_p_value"``,
        ``"study_weights"``.
    """
    log_rr = np.asarray(log_rr, dtype=float)
    se = np.asarray(se, dtype=float)
    wi = 1.0 / se**2  # fixed-effect weights

    # Cochran's Q
    pooled_fe = np.sum(wi * log_rr) / np.sum(wi)
    q = float(np.sum(wi * (log_rr - pooled_fe) ** 2))
    k = len(log_rr)
    df_q = k - 1

    # tau^2 (DerSimonian-Laird)
    c = np.sum(wi) - np.sum(wi**2) / np.sum(wi)
    tau2 = max((q - df_q) / c, 0.0)

    # Random-effects weights
    wi_re = 1.0 / (se**2 + tau2)
    pooled_re = np.sum(wi_re * log_rr) / np.sum(wi_re)
    pooled_se_re = np.sqrt(1.0 / np.sum(wi_re))

    z = 1.96
    ci_lower_log = pooled_re - z * pooled_se_re
    ci_upper_log = pooled_re + z * pooled_se_re

    # I^2
    i2 = max((q - df_q) / q * 100.0, 0.0) if q > 0 else 0.0

    # Q p-value
    from scipy.stats import chi2  # lazy import
    q_p = 1.0 - chi2.cdf(q, df=df_q)

    # Normalised weights (%)
    study_weights = wi_re / np.sum(wi_re) * 100.0

    result = {
        "pooled_log_rr": float(pooled_re),
        "pooled_se": float(pooled_se_re),
        "pooled_rr": float(np.exp(pooled_re)),
        "ci_lower_rr": float(np.exp(ci_lower_log)),
        "ci_upper_rr": float(np.exp(ci_upper_log)),
        "i_squared": float(i2),
        "tau_squared": float(tau2),
        "q_stat": float(q),
        "q_p_value": float(q_p),
        "study_weights": study_weights.tolist(),
    }

    if study_labels is not None:
        result["study_labels"] = study_labels

    return result


# ---------------------------------------------------------------------------
# Fixed-effects meta-analysis (inverse-variance weighting)
# ---------------------------------------------------------------------------

def meta_analysis_fixed(
    log_rr: np.ndarray | list[float],
    se: np.ndarray | list[float],
    study_labels: list[str] | None = None,
) -> dict:
    """Inverse-variance fixed-effects meta-analysis.

    Parameters
    ----------
    log_rr:
        Study-level log relative risks.
    se:
        Corresponding standard errors.
    study_labels:
        Optional study identifiers.

    Returns
    -------
    dict
        Keys: ``"pooled_log_rr"``, ``"pooled_se"``, ``"pooled_rr"``,
        ``"ci_lower_rr"``, ``"ci_upper_rr"``, ``"study_weights"``.
    """
    log_rr = np.asarray(log_rr, dtype=float)
    se = np.asarray(se, dtype=float)
    wi = 1.0 / se**2
    pooled = float(np.sum(wi * log_rr) / np.sum(wi))
    pooled_se = float(np.sqrt(1.0 / np.sum(wi)))

    z = 1.96
    result = {
        "pooled_log_rr": pooled,
        "pooled_se": pooled_se,
        "pooled_rr": float(np.exp(pooled)),
        "ci_lower_rr": float(np.exp(pooled - z * pooled_se)),
        "ci_upper_rr": float(np.exp(pooled + z * pooled_se)),
        "study_weights": (wi / np.sum(wi) * 100.0).tolist(),
    }
    if study_labels is not None:
        result["study_labels"] = study_labels
    return result


# ---------------------------------------------------------------------------
# Dose-response meta-analysis
# ---------------------------------------------------------------------------

def dose_response_threshold(
    bpv_values: np.ndarray | list[float],
    log_rr_values: np.ndarray | list[float],
    target_rr: float = 1.10,
) -> float:
    """Estimate the BPV level associated with a target relative risk.

    Uses linear interpolation on the pooled dose-response curve.

    Parameters
    ----------
    bpv_values:
        BPV dose levels (e.g., SD in mmHg or CV in %).
    log_rr_values:
        Pooled log-RR values corresponding to each dose level.
    target_rr:
        Relative risk threshold of interest (default 1.10 = 10 % increase).

    Returns
    -------
    float
        Estimated BPV value associated with ``target_rr``,
        or NaN if out of range.
    """
    bpv = np.asarray(bpv_values, dtype=float)
    log_rr = np.asarray(log_rr_values, dtype=float)
    log_target = np.log(target_rr)

    # Find where the curve crosses log_target
    for i in range(len(log_rr) - 1):
        if (log_rr[i] <= log_target <= log_rr[i + 1]) or (
            log_rr[i + 1] <= log_target <= log_rr[i]
        ):
            # Linear interpolation
            frac = (log_target - log_rr[i]) / (log_rr[i + 1] - log_rr[i])
            return float(bpv[i] + frac * (bpv[i + 1] - bpv[i]))

    return float("nan")


# ---------------------------------------------------------------------------
# Forest plot
# ---------------------------------------------------------------------------

def forest_plot(
    log_rr: np.ndarray | list[float],
    se: np.ndarray | list[float],
    study_labels: list[str],
    pooled_result: dict,
    ax=None,
    title: str = "Forest Plot",
):
    """Generate a forest plot for meta-analysis results.

    Parameters
    ----------
    log_rr:
        Study-level log relative risks.
    se:
        Corresponding standard errors.
    study_labels:
        Study identifiers.
    pooled_result:
        Dict returned by :func:`meta_analysis` or :func:`meta_analysis_fixed`.
    ax:
        Optional matplotlib Axes.
    title:
        Plot title.

    Returns
    -------
    matplotlib.figure.Figure or None
        The figure object if a new one was created, otherwise None.
    """
    import matplotlib.pyplot as plt  # lazy import

    log_rr = np.asarray(log_rr, dtype=float)
    se = np.asarray(se, dtype=float)

    fig = None
    if ax is None:
        fig, ax = plt.subplots(figsize=(8, max(4, len(study_labels) * 0.5 + 2)))

    rr = np.exp(log_rr)
    ci_lower = np.exp(log_rr - 1.96 * se)
    ci_upper = np.exp(log_rr + 1.96 * se)

    y_positions = list(range(len(study_labels), 0, -1))

    for y, label, r, cil, ciu in zip(
        y_positions, study_labels, rr, ci_lower, ci_upper
    ):
        ax.plot([cil, ciu], [y, y], color="black", linewidth=1)
        ax.plot(r, y, "s", color="steelblue", markersize=6)
        ax.text(-0.02, y, label, ha="right", va="center", fontsize=8,
                transform=ax.get_yaxis_transform())

    # Pooled estimate
    pooled_rr = pooled_result["pooled_rr"]
    pooled_ci_lo = pooled_result["ci_lower_rr"]
    pooled_ci_hi = pooled_result["ci_upper_rr"]
    ax.plot([pooled_ci_lo, pooled_ci_hi], [0, 0], color="black", linewidth=2)
    ax.plot(pooled_rr, 0, "D", color="red", markersize=8, label="Pooled RR")

    ax.axvline(1.0, color="black", linestyle="--", linewidth=0.8)
    ax.set_xlabel("Relative Risk (95 % CI)")
    ax.set_title(title)
    ax.set_yticks([])

    summary = (
        f"Pooled RR = {pooled_rr:.2f} "
        f"({pooled_ci_lo:.2f}–{pooled_ci_hi:.2f})\n"
        f"I² = {pooled_result.get('i_squared', float('nan')):.1f} %"
    )
    ax.text(
        0.99, 0.02, summary, transform=ax.transAxes,
        ha="right", va="bottom", fontsize=8,
        bbox=dict(boxstyle="round", facecolor="wheat", alpha=0.5),
    )

    return fig


# ---------------------------------------------------------------------------
# Funnel plot
# ---------------------------------------------------------------------------

def funnel_plot(
    log_rr: np.ndarray | list[float],
    se: np.ndarray | list[float],
    pooled_log_rr: float,
    ax=None,
    title: str = "Funnel Plot",
):
    """Generate a funnel plot to assess publication bias.

    Parameters
    ----------
    log_rr:
        Study-level log relative risks.
    se:
        Standard errors (used as the precision axis, inverted).
    pooled_log_rr:
        Pooled log-RR for the centre line.
    ax:
        Optional matplotlib Axes.
    title:
        Plot title.

    Returns
    -------
    matplotlib.figure.Figure or None
    """
    import matplotlib.pyplot as plt

    log_rr = np.asarray(log_rr, dtype=float)
    se = np.asarray(se, dtype=float)

    fig = None
    if ax is None:
        fig, ax = plt.subplots(figsize=(6, 5))

    ax.scatter(log_rr, se, color="steelblue", alpha=0.7)
    ax.axvline(pooled_log_rr, color="red", linestyle="--", label="Pooled")

    # Pseudo-confidence funnel
    se_range = np.linspace(0, max(se) * 1.1, 100)
    ax.plot(pooled_log_rr - 1.96 * se_range, se_range, "k--", linewidth=0.8)
    ax.plot(pooled_log_rr + 1.96 * se_range, se_range, "k--", linewidth=0.8)

    ax.set_xlabel("Log Relative Risk")
    ax.set_ylabel("Standard Error")
    ax.invert_yaxis()
    ax.set_title(title)

    return fig
