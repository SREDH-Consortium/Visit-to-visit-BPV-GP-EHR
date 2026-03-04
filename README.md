# Visit-to-visit Blood Pressure Variability – GP EHR

Research code for the PhD thesis:
**"Enhancing CVD Risk Estimation in Primary Care by Incorporating Visit-to-visit Blood Pressure Variability"**

---

## Background

Visit-to-visit blood pressure variability (VVV BPV) is an important yet underutilised
risk factor for cardiovascular disease (CVD) risk prediction.  This repository implements
the statistical and modelling methods used in the thesis to:

1. **Calculate VVV BPV metrics** – standard deviation (SD), coefficient of variation (CV),
   and average real variability (ARV) – from repeated readings in electronic health record
   (EHR) data.
2. **Establish decision-making cut-off values** for VVV BPV using time-to-event analysis.
3. **Develop and validate CVD risk prediction models** incorporating VVV BPV (Cox
   proportional-hazards) and compare them against a single-visit model.
4. **Conduct meta-analysis** (DerSimonian-Laird random effects and fixed effects) to
   synthesise effect sizes from published studies and produce a dose-response curve.

---

## Key Findings

| Finding | Value |
|---------|-------|
| ΔC-index (SD model vs single-visit) | **+0.117** (95 % CI: 0.029–0.204) |
| ΔC-index (CV model vs single-visit) | **+0.122** (95 % CI: 0.013–0.227) |
| Meta-analysis threshold (SBP SD) | **6.72 mmHg** → 10 % increased CVD risk |
| Meta-analysis threshold (SBP CV) | **9.05 %** → 10 % increased CVD risk |
| Cut-off SBP SD / CV / ARV | **19 mmHg / 14 % / 15 mmHg** |
| Cut-off DBP SD / CV / ARV | **11 mmHg / 12 % / 11 mmHg** |
| Minimum BP measurements recommended | **3** |

---

## Repository Structure

```
.
├── bpv_metrics.py        # VVV BPV calculation: SD, CV, ARV; patient-level helpers
├── survival_analysis.py  # Kaplan-Meier, Cox PH, C-index, Youden cut-off
├── cvd_risk_model.py     # End-to-end CVD risk model pipeline
├── meta_analysis.py      # Random/fixed-effects meta-analysis, dose-response, plots
└── tests/
    ├── test_bpv_metrics.py
    ├── test_survival_analysis.py
    ├── test_cvd_risk_model.py
    └── test_meta_analysis.py
```

---

## Requirements

- Python ≥ 3.9
- `numpy`, `pandas`, `scipy`
- `lifelines` (survival analysis)
- `scikit-learn`, `statsmodels`
- `matplotlib`, `seaborn`

Install all dependencies:

```bash
pip install numpy pandas scipy lifelines scikit-learn statsmodels matplotlib seaborn
```

---

## Quick Start

### 1 – Calculate BPV metrics from repeated BP readings

```python
import pandas as pd
from bpv_metrics import compute_bpv_metrics, classify_high_bpv

# Long-format DataFrame: one BP reading per row
bp_df = pd.read_csv("bp_readings.csv")   # patient_id, visit_date, sbp, dbp

metrics = compute_bpv_metrics(bp_df)
# Returns: patient_id, sbp_sd, sbp_cv, sbp_arv, dbp_sd, dbp_cv, dbp_arv, ...

# Classify patients as high BPV (>= established cut-off)
metrics["high_sbp_sd"] = classify_high_bpv(metrics, metric="sd", bp="sbp")
```

### 2 – Build and compare CVD risk models

```python
import pandas as pd
from cvd_risk_model import build_model_dataset, run_cvd_model_comparison, print_model_comparison

bp_df      = pd.read_csv("bp_readings.csv")
covariate_df = pd.read_csv("patient_covariates.csv")
# Required columns: patient_id, age, sex, smoking, diabetes,
#                   total_cholesterol, hdl_cholesterol, on_antihypertensives,
#                   follow_up_years, cvd_event

model_df = build_model_dataset(bp_df, covariate_df)
results  = run_cvd_model_comparison(model_df)
print_model_comparison(results)
```

### 3 – Survival analysis and optimal cut-off

```python
from survival_analysis import youden_cutoff, km_analysis

# Optimal BPV threshold
cutoff = youden_cutoff(model_df, bpv_col="sbp_sd",
                       event_col="cvd_event", duration_col="follow_up_years")
print(f"Optimal SBP SD cut-off: {cutoff['threshold']:.1f} mmHg")

# Kaplan-Meier by high/low BPV group
model_df["high_bpv"] = model_df["sbp_sd"] >= cutoff["threshold"]
km_result = km_analysis(model_df, "follow_up_years", "cvd_event", "high_bpv")
print(f"Log-rank p = {km_result['logrank_p']:.4f}")
```

### 4 – Meta-analysis

```python
import numpy as np
from meta_analysis import meta_analysis, dose_response_threshold, forest_plot

log_rr = np.log([1.15, 1.20, 1.08, 1.25, 1.18])
se     = np.array([0.08, 0.12, 0.10, 0.15, 0.09])
labels = ["Study A", "Study B", "Study C", "Study D", "Study E"]

result = meta_analysis(log_rr, se, labels)
print(f"Pooled RR = {result['pooled_rr']:.2f} "
      f"({result['ci_lower_rr']:.2f}–{result['ci_upper_rr']:.2f}), "
      f"I² = {result['i_squared']:.1f} %")

# Dose-response threshold for 10 % increased CVD risk
bpv_doses = np.array([0, 3, 6, 7, 10, 15])
log_rr_dr = (np.log(1.10) / 6.72) * bpv_doses
thr = dose_response_threshold(bpv_doses, log_rr_dr, target_rr=1.10)
print(f"SBP SD threshold for 10 % increased CVD risk: {thr:.2f} mmHg")
```

---

## Running Tests

```bash
pip install pytest
pytest tests/ -v
```

All 75 tests should pass.

---

## Citation

If you use this code, please cite the associated PhD thesis (to be published).