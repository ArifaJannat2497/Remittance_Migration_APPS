# Remittance–Migration Replication

This repository contains reproducible scripts for:

- DCCE-PMG baseline estimation
- DCCE-PMG-IV estimation with `L2.lnNEMIG` and `L3.lnNEMIG`
- first-stage diagnostics
- Hansen J diagnostic
- Anderson–Rubin weak-IV diagnostic
- plausibly-exogenous sensitivity analysis

## Data

The scripts are designed for:

`Data_new_remittance(1).xlsx`

Sheet:

`Final_add (2)`

The raw data file is not included in the repository unless redistribution is permitted.

## Exact manuscript specification

The final DCCE-PMG specification uses:

- dependent variable: `lnREMT`
- endogenous variable: `lnNEMIG`
- controls: `lnEXC`, `lnGDPG`, `lnFDI`, `lnTRDO`, `lnINF`
- pooled long-run coefficients
- cross-sectional averages
- `cr_lags(0)`
- sample restriction: `Year >= 1993`

The DCCE-PMG-IV model instruments `lnNEMIG` with:

- `L2.lnNEMIG`
- `L3.lnNEMIG`

## Existing manuscript benchmark

The revised manuscript currently reports:

- baseline DCCE-PMG `lnNEMIG` = 0.4217, p < 0.001
- DCCE-PMG-IV `lnNEMIG` = -0.0716, p = 0.910
- first-stage partial F = 147.652, p < 0.001
- Hansen J = 0.126, p = 0.722

These values should be retained only if the exact Stata replication reproduces them.

## Anderson–Rubin

The R AR script calculates a conventional IV AR confidence-set benchmark.

It must NOT be described as the exact DCCE-PMG-IV long-run AR confidence set unless the corresponding long-run PMG restriction has been inverted.

The manuscript should report the numerical AR interval only after exact verification.

## Reproducibility

Run:

1. `R/01_PREPARE_DATA_AND_DCCE_PMG.R`
2. `R/02_AR_AND_CONLEY_BENCHMARKS.R`

Then run:

`remittance_8July.do`

and:

`02_exact_DCCE_PMG_IV_AR.do`

in Stata with `xtdcce2` and `ivreg2` installed.

## Recommended manuscript interpretation

The central conclusion should be:

> The positive long-run elasticity obtained when net emigration is treated as exogenous is not robust to treating net emigration as endogenous. The IV specification therefore does not support interpreting the baseline elasticity as a structural causal parameter. This does not establish that the true causal effect is zero.
