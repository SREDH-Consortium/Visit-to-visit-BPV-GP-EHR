"""
Unit tests for meta_analysis.py
"""

import math
import numpy as np
import pytest

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from meta_analysis import (
    meta_analysis,
    meta_analysis_fixed,
    dose_response_threshold,
    forest_plot,
    funnel_plot,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def study_data():
    """Small synthetic meta-analysis dataset (5 studies)."""
    log_rr = np.log([1.15, 1.20, 1.08, 1.25, 1.18])
    # SE derived from approximate 95 % CI widths
    se = np.array([0.08, 0.12, 0.10, 0.15, 0.09])
    labels = ["Study A", "Study B", "Study C", "Study D", "Study E"]
    return log_rr, se, labels


# ---------------------------------------------------------------------------
# meta_analysis (random effects)
# ---------------------------------------------------------------------------

class TestMetaAnalysis:
    def test_output_keys(self, study_data):
        log_rr, se, labels = study_data
        result = meta_analysis(log_rr, se, labels)
        for key in ("pooled_log_rr", "pooled_se", "pooled_rr",
                    "ci_lower_rr", "ci_upper_rr", "i_squared",
                    "tau_squared", "q_stat", "q_p_value",
                    "study_weights"):
            assert key in result, f"Missing key: {key}"

    def test_pooled_rr_positive(self, study_data):
        log_rr, se, _ = study_data
        result = meta_analysis(log_rr, se)
        assert result["pooled_rr"] > 0

    def test_ci_contains_pooled(self, study_data):
        log_rr, se, _ = study_data
        result = meta_analysis(log_rr, se)
        assert result["ci_lower_rr"] <= result["pooled_rr"] <= result["ci_upper_rr"]

    def test_i_squared_in_range(self, study_data):
        log_rr, se, _ = study_data
        result = meta_analysis(log_rr, se)
        assert 0.0 <= result["i_squared"] <= 100.0

    def test_weights_sum_to_100(self, study_data):
        log_rr, se, _ = study_data
        result = meta_analysis(log_rr, se)
        assert math.isclose(sum(result["study_weights"]), 100.0, rel_tol=1e-6)

    def test_q_p_value_in_range(self, study_data):
        log_rr, se, _ = study_data
        result = meta_analysis(log_rr, se)
        assert 0.0 <= result["q_p_value"] <= 1.0

    def test_homogeneous_studies_low_i2(self):
        """All studies with same log-RR → tau^2 = 0 → I^2 = 0."""
        log_rr = np.full(5, np.log(1.20))
        se = np.full(5, 0.10)
        result = meta_analysis(log_rr, se)
        assert result["tau_squared"] == pytest.approx(0.0, abs=1e-9)


# ---------------------------------------------------------------------------
# meta_analysis_fixed
# ---------------------------------------------------------------------------

class TestMetaAnalysisFixed:
    def test_output_keys(self, study_data):
        log_rr, se, labels = study_data
        result = meta_analysis_fixed(log_rr, se, labels)
        for key in ("pooled_log_rr", "pooled_se", "pooled_rr",
                    "ci_lower_rr", "ci_upper_rr", "study_weights"):
            assert key in result

    def test_ci_contains_pooled(self, study_data):
        log_rr, se, _ = study_data
        result = meta_analysis_fixed(log_rr, se)
        assert result["ci_lower_rr"] <= result["pooled_rr"] <= result["ci_upper_rr"]

    def test_weights_sum_to_100(self, study_data):
        log_rr, se, _ = study_data
        result = meta_analysis_fixed(log_rr, se)
        assert math.isclose(sum(result["study_weights"]), 100.0, rel_tol=1e-6)


# ---------------------------------------------------------------------------
# dose_response_threshold
# ---------------------------------------------------------------------------

class TestDoseResponseThreshold:
    def test_known_interpolation(self):
        """Linear curve: log_rr = 0.05 * bpv  -> RR=1.10 at log(1.10)/0.05."""
        bpv = np.array([0.0, 5.0, 10.0, 15.0, 20.0])
        log_rr = 0.05 * bpv
        thr = dose_response_threshold(bpv, log_rr, target_rr=1.10)
        expected = math.log(1.10) / 0.05
        assert math.isclose(thr, expected, rel_tol=1e-6)

    def test_out_of_range_returns_nan(self):
        bpv = np.array([0.0, 5.0, 10.0])
        log_rr = 0.005 * bpv   # very small effect; never reaches RR=2.0
        thr = dose_response_threshold(bpv, log_rr, target_rr=2.0)
        assert math.isnan(thr)

    def test_sbp_sd_threshold_approximately_known(self):
        """Reproduce meta-analysis threshold: SD 6.72 mmHg ~ 10 % increased risk."""
        bpv = np.array([0.0, 3.0, 6.0, 7.0, 10.0, 15.0])
        # Construct log_rr so that 10% risk occurs at ~6.72 mmHg
        log_rr = np.log(1.0) + (np.log(1.10) / 6.72) * bpv
        thr = dose_response_threshold(bpv, log_rr, target_rr=1.10)
        assert math.isclose(thr, 6.72, rel_tol=0.01)

    def test_sbp_cv_threshold_approximately_known(self):
        """Reproduce meta-analysis threshold: CV 9.05 % ~ 10 % increased risk."""
        bpv = np.array([0.0, 4.0, 8.0, 9.5, 13.0, 18.0])
        log_rr = np.log(1.0) + (np.log(1.10) / 9.05) * bpv
        thr = dose_response_threshold(bpv, log_rr, target_rr=1.10)
        assert math.isclose(thr, 9.05, rel_tol=0.01)


# ---------------------------------------------------------------------------
# Plotting functions (smoke tests – no assertion on visual output)
# ---------------------------------------------------------------------------

class TestPlots:
    def test_forest_plot_runs(self, study_data):
        """forest_plot should not raise."""
        import matplotlib
        matplotlib.use("Agg")
        log_rr, se, labels = study_data
        result = meta_analysis(log_rr, se, labels)
        fig = forest_plot(log_rr, se, labels, result, title="Test Forest")
        assert fig is not None

    def test_funnel_plot_runs(self, study_data):
        """funnel_plot should not raise."""
        import matplotlib
        matplotlib.use("Agg")
        log_rr, se, _ = study_data
        result = meta_analysis(log_rr, se)
        fig = funnel_plot(log_rr, se, result["pooled_log_rr"], title="Test Funnel")
        assert fig is not None
