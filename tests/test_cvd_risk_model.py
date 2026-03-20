"""
Unit tests for cvd_risk_model.py
"""

import math
import numpy as np
import pandas as pd
import pytest

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from cvd_risk_model import (
    build_model_dataset,
    run_cvd_model_comparison,
    print_model_comparison,
    BASE_COVARIATES,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def synthetic_data():
    """Synthetic EHR-style data for end-to-end model tests."""
    rng = np.random.default_rng(0)
    n_patients = 200
    n_visits_per_patient = 4

    patient_ids = np.repeat(range(n_patients), n_visits_per_patient)
    visit_dates = pd.date_range("2010-01-01", periods=n_visits_per_patient, freq="90D")
    visit_dates_all = np.tile(visit_dates, n_patients)

    sbp_base = rng.normal(135, 15, n_patients)
    sbp_noise = rng.normal(0, 8, n_patients * n_visits_per_patient)
    sbp = np.repeat(sbp_base, n_visits_per_patient) + sbp_noise

    dbp_base = rng.normal(80, 10, n_patients)
    dbp_noise = rng.normal(0, 5, n_patients * n_visits_per_patient)
    dbp = np.repeat(dbp_base, n_visits_per_patient) + dbp_noise

    bp_df = pd.DataFrame({
        "patient_id": patient_ids,
        "visit_date": visit_dates_all,
        "sbp": sbp,
        "dbp": dbp,
    })

    age = rng.uniform(45, 75, n_patients)
    sbp_sd_true = np.array(
        [np.std(sbp[i * n_visits_per_patient:(i + 1) * n_visits_per_patient], ddof=1)
         for i in range(n_patients)]
    )
    log_h = 0.03 * age + 0.04 * sbp_sd_true - 4.5
    scale = np.exp(-log_h)
    duration = rng.exponential(scale) + 1.0
    event = (duration < 8).astype(int)
    duration = np.minimum(duration, 8.0)

    covariate_df = pd.DataFrame({
        "patient_id": range(n_patients),
        "age": age,
        "sex": rng.integers(0, 2, n_patients),
        "smoking": rng.integers(0, 3, n_patients),
        "diabetes": rng.integers(0, 2, n_patients),
        "total_cholesterol": rng.normal(5.0, 1.0, n_patients),
        "hdl_cholesterol": rng.normal(1.4, 0.4, n_patients),
        "on_antihypertensives": rng.integers(0, 2, n_patients),
        "follow_up_years": duration,
        "cvd_event": event,
    })

    return bp_df, covariate_df


# ---------------------------------------------------------------------------
# build_model_dataset
# ---------------------------------------------------------------------------

class TestBuildModelDataset:
    def test_returns_one_row_per_patient(self, synthetic_data):
        bp_df, cov_df = synthetic_data
        result = build_model_dataset(bp_df, cov_df)
        assert len(result) == len(cov_df)

    def test_bpv_columns_present(self, synthetic_data):
        bp_df, cov_df = synthetic_data
        result = build_model_dataset(bp_df, cov_df)
        for col in ["sbp_sd", "sbp_cv", "sbp_arv", "sbp_mean"]:
            assert col in result.columns, f"Missing column: {col}"

    def test_covariate_columns_preserved(self, synthetic_data):
        bp_df, cov_df = synthetic_data
        result = build_model_dataset(bp_df, cov_df)
        for col in BASE_COVARIATES:
            assert col in result.columns, f"Missing covariate: {col}"


# ---------------------------------------------------------------------------
# run_cvd_model_comparison
# ---------------------------------------------------------------------------

class TestRunCVDModelComparison:
    @pytest.fixture(scope="class")
    def model_df(self, synthetic_data):
        bp_df, cov_df = synthetic_data
        return build_model_dataset(bp_df, cov_df)

    def test_output_keys(self, model_df):
        results = run_cvd_model_comparison(
            model_df, n_bootstrap=50, random_state=42
        )
        assert set(results.keys()) == {"single_visit", "sd_model", "cv_model"}

    def test_c_index_range(self, model_df):
        results = run_cvd_model_comparison(
            model_df, n_bootstrap=50, random_state=42
        )
        for name, res in results.items():
            ci = res["c_index"]
            assert 0.0 < ci < 1.0, f"{name} C-index {ci} out of range"

    def test_single_visit_delta_zero(self, model_df):
        results = run_cvd_model_comparison(
            model_df, n_bootstrap=50, random_state=42
        )
        assert results["single_visit"]["delta_c_index"] == pytest.approx(0.0)

    def test_bpv_models_have_valid_ci(self, model_df):
        results = run_cvd_model_comparison(
            model_df, n_bootstrap=50, random_state=42
        )
        for name in ("sd_model", "cv_model"):
            res = results[name]
            assert res["ci_lower"] <= res["ci_upper"], (
                f"{name}: ci_lower > ci_upper"
            )


# ---------------------------------------------------------------------------
# print_model_comparison (smoke test)
# ---------------------------------------------------------------------------

class TestPrintModelComparison:
    def test_runs_without_error(self, capsys, synthetic_data):
        bp_df, cov_df = synthetic_data
        model_df = build_model_dataset(bp_df, cov_df)
        results = run_cvd_model_comparison(
            model_df, n_bootstrap=20, random_state=0
        )
        print_model_comparison(results)
        captured = capsys.readouterr()
        assert "single_visit" in captured.out
        assert "sd_model" in captured.out
        assert "cv_model" in captured.out
