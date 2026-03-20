"""
Unit tests for survival_analysis.py
"""

import math
import numpy as np
import pandas as pd
import pytest

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from survival_analysis import (
    km_analysis,
    fit_cox_model,
    c_index,
    compare_models,
    youden_cutoff,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def synthetic_cohort():
    """Synthetic patient cohort for survival tests."""
    rng = np.random.default_rng(42)
    n = 300

    age = rng.uniform(40, 80, n)
    sbp_mean = rng.normal(135, 15, n)
    sbp_sd = rng.exponential(8, n)
    sbp_cv = sbp_sd / sbp_mean * 100.0

    # Time-to-event: higher age + higher SD → shorter time
    log_hazard = 0.04 * age + 0.05 * sbp_sd - 5
    scale = np.exp(-log_hazard)
    duration = rng.exponential(scale) + 0.5
    event = (duration < 10).astype(int)
    duration = np.minimum(duration, 10.0)

    return pd.DataFrame({
        "patient_id": range(n),
        "age": age,
        "sbp_mean": sbp_mean,
        "sbp_sd": sbp_sd,
        "sbp_cv": sbp_cv,
        "follow_up_years": duration,
        "cvd_event": event,
    })


# ---------------------------------------------------------------------------
# fit_cox_model
# ---------------------------------------------------------------------------

class TestFitCoxModel:
    def test_returns_fitted_model(self, synthetic_cohort):
        model = fit_cox_model(
            synthetic_cohort,
            duration_col="follow_up_years",
            event_col="cvd_event",
            covariates=["age", "sbp_mean"],
        )
        assert model is not None

    def test_model_summary_has_expected_covariates(self, synthetic_cohort):
        model = fit_cox_model(
            synthetic_cohort,
            duration_col="follow_up_years",
            event_col="cvd_event",
            covariates=["age", "sbp_sd"],
        )
        coef_names = model.params_.index.tolist()
        assert "age" in coef_names
        assert "sbp_sd" in coef_names

    def test_penalizer_accepted(self, synthetic_cohort):
        model = fit_cox_model(
            synthetic_cohort,
            duration_col="follow_up_years",
            event_col="cvd_event",
            covariates=["age"],
            penalizer=0.1,
        )
        assert model is not None


# ---------------------------------------------------------------------------
# c_index
# ---------------------------------------------------------------------------

class TestCIndex:
    def test_range_valid(self, synthetic_cohort):
        ci = c_index(
            synthetic_cohort,
            duration_col="follow_up_years",
            event_col="cvd_event",
            covariates=["age", "sbp_sd"],
        )
        assert 0.5 <= ci <= 1.0, f"C-index {ci} outside expected range"

    def test_sd_model_better_than_random(self, synthetic_cohort):
        """SD was constructed as a signal: SD model should beat 0.5."""
        ci = c_index(
            synthetic_cohort,
            duration_col="follow_up_years",
            event_col="cvd_event",
            covariates=["age", "sbp_sd"],
        )
        assert ci > 0.5


# ---------------------------------------------------------------------------
# compare_models
# ---------------------------------------------------------------------------

class TestCompareModels:
    def test_output_keys(self, synthetic_cohort):
        result = compare_models(
            synthetic_cohort,
            duration_col="follow_up_years",
            event_col="cvd_event",
            base_covariates=["age", "sbp_mean"],
            extended_covariates=["age", "sbp_sd"],
            n_bootstrap=50,
            random_state=0,
        )
        for key in ("base_c_index", "extended_c_index",
                    "delta_c_index", "ci_lower", "ci_upper"):
            assert key in result, f"Missing key: {key}"

    def test_ci_ordering(self, synthetic_cohort):
        result = compare_models(
            synthetic_cohort,
            duration_col="follow_up_years",
            event_col="cvd_event",
            base_covariates=["age", "sbp_mean"],
            extended_covariates=["age", "sbp_sd"],
            n_bootstrap=50,
            random_state=0,
        )
        assert result["ci_lower"] <= result["ci_upper"]

    def test_delta_consistent(self, synthetic_cohort):
        result = compare_models(
            synthetic_cohort,
            duration_col="follow_up_years",
            event_col="cvd_event",
            base_covariates=["age", "sbp_mean"],
            extended_covariates=["age", "sbp_sd"],
            n_bootstrap=50,
            random_state=0,
        )
        expected_delta = result["extended_c_index"] - result["base_c_index"]
        assert math.isclose(result["delta_c_index"], expected_delta, rel_tol=1e-9)


# ---------------------------------------------------------------------------
# km_analysis
# ---------------------------------------------------------------------------

class TestKMAnalysis:
    def test_returns_kmf_objects(self, synthetic_cohort):
        df = synthetic_cohort.copy()
        df["high_bpv"] = (df["sbp_sd"] >= 8).astype(int)
        result = km_analysis(
            df,
            duration_col="follow_up_years",
            event_col="cvd_event",
            group_col="high_bpv",
        )
        assert "kmf_groups" in result
        assert len(result["kmf_groups"]) == 2

    def test_logrank_p_value_in_range(self, synthetic_cohort):
        df = synthetic_cohort.copy()
        df["high_bpv"] = (df["sbp_sd"] >= 8).astype(int)
        result = km_analysis(
            df,
            duration_col="follow_up_years",
            event_col="cvd_event",
            group_col="high_bpv",
        )
        p = result["logrank_p"]
        assert 0.0 <= p <= 1.0


# ---------------------------------------------------------------------------
# youden_cutoff
# ---------------------------------------------------------------------------

class TestYoudenCutoff:
    def test_returns_valid_threshold(self, synthetic_cohort):
        result = youden_cutoff(
            synthetic_cohort,
            bpv_col="sbp_sd",
            event_col="cvd_event",
            duration_col="follow_up_years",
        )
        assert not math.isnan(result["threshold"])
        assert result["threshold"] > 0

    def test_sensitivity_specificity_in_range(self, synthetic_cohort):
        result = youden_cutoff(
            synthetic_cohort,
            bpv_col="sbp_sd",
            event_col="cvd_event",
            duration_col="follow_up_years",
        )
        assert 0.0 <= result["sensitivity"] <= 1.0
        assert 0.0 <= result["specificity"] <= 1.0

    def test_youden_index_non_negative(self, synthetic_cohort):
        result = youden_cutoff(
            synthetic_cohort,
            bpv_col="sbp_sd",
            event_col="cvd_event",
            duration_col="follow_up_years",
        )
        assert result["youden_index"] >= 0.0
