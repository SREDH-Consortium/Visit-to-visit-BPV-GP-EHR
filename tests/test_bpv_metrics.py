"""
Unit tests for bpv_metrics.py
"""

import math
import numpy as np
import pandas as pd
import pytest

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from bpv_metrics import (
    sd,
    cv,
    arv,
    compute_bpv_metrics,
    classify_high_bpv,
    MIN_MEASUREMENTS,
    CUTOFF_SBP_SD,
    CUTOFF_SBP_CV,
    CUTOFF_SBP_ARV,
    CUTOFF_DBP_SD,
    CUTOFF_DBP_CV,
    CUTOFF_DBP_ARV,
    META_THRESHOLD_SBP_SD,
    META_THRESHOLD_SBP_CV,
)


# ---------------------------------------------------------------------------
# sd()
# ---------------------------------------------------------------------------

class TestSD:
    def test_basic(self):
        vals = [120, 125, 130, 135, 140]
        result = sd(vals)
        assert math.isclose(result, np.std(vals, ddof=1), rel_tol=1e-9)

    def test_below_min_measurements_returns_nan(self):
        assert math.isnan(sd([120, 130]))   # 2 < MIN_MEASUREMENTS (3)

    def test_exactly_min_measurements(self):
        vals = [120, 130, 140]
        assert not math.isnan(sd(vals))

    def test_ignores_nan_values(self):
        vals = [120, float("nan"), 130, 140]
        assert math.isclose(sd(vals), sd([120, 130, 140]), rel_tol=1e-9)

    def test_identical_values_zero_sd(self):
        assert sd([130, 130, 130]) == pytest.approx(0.0)

    def test_single_value_nan(self):
        assert math.isnan(sd([130]))

    def test_empty_returns_nan(self):
        assert math.isnan(sd([]))

    def test_numpy_array_input(self):
        arr = np.array([110, 120, 130, 140])
        assert not math.isnan(sd(arr))


# ---------------------------------------------------------------------------
# cv()
# ---------------------------------------------------------------------------

class TestCV:
    def test_basic(self):
        vals = [120, 130, 140]
        expected = np.std(vals, ddof=1) / np.mean(vals) * 100.0
        assert math.isclose(cv(vals), expected, rel_tol=1e-9)

    def test_below_min_measurements_returns_nan(self):
        assert math.isnan(cv([120, 130]))

    def test_zero_mean_returns_nan(self):
        # Extremely unlikely in BP data but logically tested
        assert math.isnan(cv([0.0, 0.0, 0.0]))

    def test_ignores_nan_values(self):
        vals = [120, float("nan"), 130, 140]
        assert math.isclose(cv(vals), cv([120, 130, 140]), rel_tol=1e-9)

    def test_unit_consistency(self):
        """CV should be a percentage, not a proportion."""
        vals = [120, 130, 140]
        assert cv(vals) > 1.0  # must be expressed as %, not 0-1


# ---------------------------------------------------------------------------
# arv()
# ---------------------------------------------------------------------------

class TestARV:
    def test_basic(self):
        vals = [120, 130, 125, 135]
        diffs = [abs(130 - 120), abs(125 - 130), abs(135 - 125)]
        expected = sum(diffs) / len(diffs)
        assert math.isclose(arv(vals), expected, rel_tol=1e-9)

    def test_monotone_increasing(self):
        vals = [100, 110, 120, 130]
        assert math.isclose(arv(vals), 10.0, rel_tol=1e-9)

    def test_below_min_measurements_returns_nan(self):
        assert math.isnan(arv([120, 130]))

    def test_ignores_nan_values(self):
        vals = [120, float("nan"), 130, 140]
        assert math.isclose(arv(vals), arv([120, 130, 140]), rel_tol=1e-9)

    def test_constant_series_zero_arv(self):
        assert arv([130, 130, 130]) == pytest.approx(0.0)


# ---------------------------------------------------------------------------
# compute_bpv_metrics()
# ---------------------------------------------------------------------------

class TestComputeBPVMetrics:
    @pytest.fixture
    def sample_df(self):
        data = {
            "patient_id": [1, 1, 1, 2, 2, 2, 2],
            "visit_date": [
                "2020-01-01", "2020-04-01", "2020-07-01",
                "2019-06-01", "2019-09-01", "2020-01-01", "2020-06-01",
            ],
            "sbp": [120, 130, 125, 140, 145, 150, 135],
            "dbp": [80, 85, 82, 90, 92, 88, 86],
        }
        df = pd.DataFrame(data)
        df["visit_date"] = pd.to_datetime(df["visit_date"])
        return df

    def test_returns_one_row_per_patient(self, sample_df):
        result = compute_bpv_metrics(sample_df)
        assert len(result) == 2

    def test_columns_present(self, sample_df):
        result = compute_bpv_metrics(sample_df)
        for col in ["sbp_sd", "sbp_cv", "sbp_arv", "dbp_sd", "dbp_cv", "dbp_arv",
                    "sbp_mean", "dbp_mean", "n_measurements"]:
            assert col in result.columns, f"Missing column: {col}"

    def test_measurement_counts(self, sample_df):
        result = compute_bpv_metrics(sample_df).set_index("patient_id")
        assert result.loc[1, "n_measurements"] == 3
        assert result.loc[2, "n_measurements"] == 4

    def test_sd_values_positive(self, sample_df):
        result = compute_bpv_metrics(sample_df)
        assert (result["sbp_sd"].dropna() >= 0).all()

    def test_date_sorting(self):
        """ARV is order-dependent; verify results change when dates differ."""
        data = {
            "patient_id": [1, 1, 1],
            "visit_date": ["2020-03-01", "2020-01-01", "2020-02-01"],
            "sbp": [130, 120, 125],
            "dbp": [85, 80, 82],
        }
        df = pd.DataFrame(data)
        df["visit_date"] = pd.to_datetime(df["visit_date"])
        result = compute_bpv_metrics(df, date_col="visit_date")
        # After sorting by date: 120, 125, 130  -> ARV = 5
        assert math.isclose(result["sbp_arv"].iloc[0], 5.0, rel_tol=1e-9)


# ---------------------------------------------------------------------------
# classify_high_bpv()
# ---------------------------------------------------------------------------

class TestClassifyHighBPV:
    @pytest.fixture
    def metrics_df(self):
        return pd.DataFrame({
            "patient_id": [1, 2, 3],
            "sbp_sd": [18.0, 19.0, 20.0],
            "sbp_cv": [13.0, 14.0, 15.0],
            "sbp_arv": [14.0, 15.0, 16.0],
            "dbp_sd": [10.0, 11.0, 12.0],
            "dbp_cv": [11.0, 12.0, 13.0],
            "dbp_arv": [10.0, 11.0, 12.0],
        })

    def test_sbp_sd_cutoff(self, metrics_df):
        result = classify_high_bpv(metrics_df, metric="sd", bp="sbp")
        assert result.tolist() == [False, True, True]

    def test_sbp_cv_cutoff(self, metrics_df):
        result = classify_high_bpv(metrics_df, metric="cv", bp="sbp")
        assert result.tolist() == [False, True, True]

    def test_sbp_arv_cutoff(self, metrics_df):
        result = classify_high_bpv(metrics_df, metric="arv", bp="sbp")
        assert result.tolist() == [False, True, True]

    def test_dbp_sd_cutoff(self, metrics_df):
        result = classify_high_bpv(metrics_df, metric="sd", bp="dbp")
        assert result.tolist() == [False, True, True]

    def test_invalid_metric_raises(self, metrics_df):
        with pytest.raises(ValueError):
            classify_high_bpv(metrics_df, metric="xyz", bp="sbp")

    def test_invalid_bp_raises(self, metrics_df):
        with pytest.raises(ValueError):
            classify_high_bpv(metrics_df, metric="sd", bp="map")


# ---------------------------------------------------------------------------
# Published thresholds and cut-off values
# ---------------------------------------------------------------------------

class TestPublishedThresholds:
    """Ensure the constants match the published thesis findings."""

    def test_meta_threshold_sbp_sd(self):
        assert META_THRESHOLD_SBP_SD == pytest.approx(6.72)

    def test_meta_threshold_sbp_cv(self):
        assert META_THRESHOLD_SBP_CV == pytest.approx(9.05)

    def test_cutoff_sbp_sd(self):
        assert CUTOFF_SBP_SD == pytest.approx(19.0)

    def test_cutoff_sbp_cv(self):
        assert CUTOFF_SBP_CV == pytest.approx(14.0)

    def test_cutoff_sbp_arv(self):
        assert CUTOFF_SBP_ARV == pytest.approx(15.0)

    def test_cutoff_dbp_sd(self):
        assert CUTOFF_DBP_SD == pytest.approx(11.0)

    def test_cutoff_dbp_cv(self):
        assert CUTOFF_DBP_CV == pytest.approx(12.0)

    def test_cutoff_dbp_arv(self):
        assert CUTOFF_DBP_ARV == pytest.approx(11.0)

    def test_min_measurements(self):
        assert MIN_MEASUREMENTS == 3
