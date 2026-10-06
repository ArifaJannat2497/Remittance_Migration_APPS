# ================================================================
# 01_PREPARE_DATA_AND_DCCE_PMG.R
# Remittance–Migration manuscript replication
#
# Source:
#   Data_new_remittance(1).xlsx
#   Sheet: Final_add (2)
#
# Purpose:
#   1. Import and audit the exact uploaded dataset
#   2. Reproduce the Stata log transformations
#   3. Create the exact L2/L3 internal instruments
#   4. Create DCCE-PMG and DCCE-PMG-IV analysis samples
#   5. Run the R DCCE-PMG benchmark where supported
#   6. Export all data needed for exact xtdcce2 replication in Stata
#
# IMPORTANT:
# The manuscript's exact DCCE-PMG-IV estimator was implemented in
# Stata xtdcce2/ivreg2. R does not provide a one-for-one implementation
# of that estimator. Therefore this script DOES NOT relabel an ordinary
# IV model as the exact DCCE-PMG-IV estimator.
# ================================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

pkgs <- c(
  "readxl","dplyr","tidyr","readr","plm","ggplot2",
  "broom","sandwich","lmtest","writexl"
)
new <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------------------------
# 1. USER SETTINGS
# ---------------------------
DATA_FILE <- file.choose()
SHEET <- "Final_add (2)"
OUT_DIR <- file.path(dirname(DATA_FILE), "remittance_final_results")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Manuscript variables
required <- c(
  "Country","Year","REMT","NEMIG","EXC","GDPG",
  "INF","CPI","UNE","TRDO","FDI"
)

# ---------------------------
# 2. IMPORT
# ---------------------------
raw <- readxl::read_excel(DATA_FILE, sheet = SHEET)
names(raw) <- trimws(names(raw))

missing_vars <- setdiff(required, names(raw))
if(length(missing_vars) > 0){
  stop("Missing variables: ", paste(missing_vars, collapse = ", "))
}

raw <- raw |>
  mutate(
    Country = trimws(as.character(Country)),
    Year = as.integer(Year)
  ) |>
  arrange(Country, Year) |>
  mutate(ID = as.integer(factor(Country)))

# ---------------------------
# 3. DATA AUDIT
# ---------------------------
audit <- data.frame(
  variable = required,
  N = sapply(raw[required], function(x) sum(!is.na(x))),
  missing = sapply(raw[required], function(x) sum(is.na(x))),
  nonpositive = sapply(raw[required], function(x)
    if(is.numeric(x)) sum(x <= 0, na.rm = TRUE) else NA_integer_)
)
write.csv(audit, file.path(OUT_DIR,"01_data_audit.csv"), row.names=FALSE)

write.csv(
  raw |> count(Country, name = "observations"),
  file.path(OUT_DIR,"02_country_counts.csv"),
  row.names=FALSE
)

# ---------------------------
# 4. EXACT STATA LOG TRANSFORMS
# ---------------------------
dat <- raw |>
  mutate(
    lnREMT  = log(REMT),
    lnNEMIG = log(NEMIG),
    lnEXC   = log(EXC),
    lnINF   = log(INF),
    lnCPI   = log(CPI),
    lnUNE   = log(UNE),
    lnGDPG  = log(GDPG),
    lnTRDO  = log(TRDO),
    lnFDI   = log(FDI)
  ) |>
  arrange(Country, Year) |>
  group_by(Country) |>
  mutate(
    z_L2 = lag(lnNEMIG, 2),
    z_L3 = lag(lnNEMIG, 3)
  ) |>
  ungroup()

# ---------------------------
# 5. MANUSCRIPT BASELINE SAMPLE
# ---------------------------
baseline_vars <- c(
  "lnREMT","lnNEMIG","lnEXC","lnGDPG",
  "lnFDI","lnTRDO","lnINF"
)

baseline <- dat |>
  filter(Year >= 1993) |>
  filter(if_all(all_of(baseline_vars), ~ !is.na(.)))

iv_vars <- c(baseline_vars, "z_L2","z_L3")

iv_sample <- dat |>
  filter(Year >= 1993) |>
  filter(if_all(all_of(iv_vars), ~ !is.na(.)))

write.csv(
  baseline |> select(Country, Year, all_of(baseline_vars)),
  file.path(OUT_DIR,"03_DCCE_PMG_analysis_sample.csv"),
  row.names=FALSE
)

write.csv(
  iv_sample |> select(Country, Year, all_of(iv_vars)),
  file.path(OUT_DIR,"04_DCCE_PMG_IV_analysis_sample.csv"),
  row.names=FALSE
)

sample_summary <- data.frame(
  sample = c("DCCE-PMG","DCCE-PMG-IV"),
  countries = c(n_distinct(baseline$Country), n_distinct(iv_sample$Country)),
  observations = c(nrow(baseline), nrow(iv_sample)),
  first_year = c(min(baseline$Year), min(iv_sample$Year)),
  last_year = c(max(baseline$Year), max(iv_sample$Year))
)
write.csv(sample_summary, file.path(OUT_DIR,"05_sample_summary.csv"), row.names=FALSE)

# ---------------------------
# 6. CROSS-SECTIONAL DEPENDENCE
# ---------------------------
# Pesaran CD on the level variables. This is a descriptive replication
# diagnostic; exact xtcd/xtcd2 output is obtained in Stata.
cd_vars <- c("lnREMT","lnNEMIG","lnEXC","lnGDPG","lnINF","lnTRDO","lnFDI")
cd_results <- lapply(cd_vars, function(v){
  x <- dat |> select(Country, Year, value = all_of(v)) |> tidyr::pivot_wider(
    names_from = Country, values_from = value
  ) |> select(-Year)
  # pairwise correlations + average correlation
  C <- cor(x, use = "pairwise.complete.obs")
  n <- ncol(C)
  avg_abs <- mean(abs(C[upper.tri(C)]), na.rm=TRUE)
  data.frame(variable=v, mean_abs_pairwise_correlation=avg_abs)
}) |> bind_rows()

write.csv(cd_results, file.path(OUT_DIR,"06_cross_section_dependence.csv"),
          row.names=FALSE)

# ---------------------------
# 7. CONVENTIONAL IV BENCHMARK
# ---------------------------
# This is intentionally labelled a benchmark.
# It is NOT the exact DCCE-PMG-IV estimator.

if(!requireNamespace("AER", quietly=TRUE))
  install.packages("AER", dependencies=TRUE)
library(AER)

iv_formula <- lnREMT ~ lnNEMIG + lnEXC + lnGDPG + lnFDI + lnTRDO + lnINF |
  z_L2 + z_L3 + lnEXC + lnGDPG + lnFDI + lnTRDO + lnINF

iv_fit <- AER::ivreg(iv_formula, data=iv_sample)

capture.output(
  summary(iv_fit, diagnostics=TRUE),
  file=file.path(OUT_DIR,"07_conventional_IV_benchmark.txt")
)

iv_coef <- broom::tidy(iv_fit)
write.csv(iv_coef, file.path(OUT_DIR,"08_conventional_IV_coefficients.csv"),
          row.names=FALSE)

# First-stage conventional benchmark
fs <- lm(
  lnNEMIG ~ z_L2 + z_L3 + lnEXC + lnGDPG + lnFDI + lnTRDO + lnINF,
  data=iv_sample
)
write.csv(
  broom::tidy(fs),
  file.path(OUT_DIR,"09_first_stage_coefficients.csv"),
  row.names=FALSE
)

# Joint F for excluded instruments
fs_restricted <- lm(
  lnNEMIG ~ lnEXC + lnGDPG + lnFDI + lnTRDO + lnINF,
  data=iv_sample
)
anova_fs <- anova(fs_restricted, fs)
write.csv(anova_fs, file.path(OUT_DIR,"10_first_stage_nested_F.csv"),
          row.names=FALSE)

# ---------------------------
# 8. GENERATE STATA DATA FILE
# ---------------------------
if(requireNamespace("haven", quietly=TRUE)){
  haven::write_dta(
    iv_sample,
    file.path(OUT_DIR,"DCCE_PMG_IV_analysis_sample.dta")
  )
}

# ---------------------------
# 9. WRITE EXACT STATA BRIDGE
# ---------------------------
stata_file <- file.path(OUT_DIR,"02_exact_DCCE_PMG_IV_replication.do")

stata <- c(
'clear all',
'set more off',
'',
'* Load the exact IV analysis sample generated by R',
paste0('use "', gsub("\\\\","/",file.path(OUT_DIR,"DCCE_PMG_IV_analysis_sample.dta")), '", clear'),
'',
'egen ID2 = group(Country)',
'xtset ID2 Year, yearly',
'',
'* Exact manuscript DCCE-PMG specification',
'xtdcce2 d.lnREMT d.lnEXC d.lnGDPG d.lnNEMIG d.lnFDI d.lnTRDO d.lnINF if Year >= 1993, ///',
'    lr(L.lnREMT L.lnEXC L.lnGDPG L.lnNEMIG L.lnFDI L.lnTRDO L.lnINF) ///',
'    pooled(L.lnREMT L.lnEXC L.lnGDPG L.lnNEMIG L.lnFDI L.lnTRDO L.lnINF) ///',
'    crosssectional(d.lnREMT d.lnEXC d.lnGDPG d.lnFDI d.lnTRDO d.lnINF) ///',
'    cr_lags(0) lr_options(xtpmgnames)',
'',
'estimates store BASELINE_DCCE_PMG',
'',
'* Exact manuscript DCCE-PMG-IV specification',
'xtdcce2 d.lnREMT d.lnEXC d.lnGDPG d.lnFDI d.lnTRDO d.lnINF ///',
'    (lnNEMIG = L2.lnNEMIG L3.lnNEMIG) if Year >= 1993, ///',
'    lr(L.lnREMT L.lnEXC L.lnGDPG L.lnNEMIG L.lnFDI L.lnTRDO L.lnINF) ///',
'    pooled(L.lnREMT L.lnEXC L.lnGDPG L.lnNEMIG L.lnFDI L.lnTRDO L.lnINF) ///',
'    crosssectional(d.lnREMT d.lnEXC d.lnGDPG d.lnFDI d.lnTRDO d.lnINF) ///',
'    cr_lags(0) lr_options(xtpmgnames) e_ivreg2 ivslow',
'',
'estimates store DCCE_PMG_IV',
'',
'* Save the exact Stata estimation output',
'capture estimates save "DCCE_PMG_IV_final.ster", replace',
'',
'* Save the exact estimation sample',
'gen byte e_sample = e(sample)',
'export delimited Country Year e_sample using "DCCE_PMG_IV_estimation_sample.csv" if e_sample, replace',
'',
'* Diagnostics returned by xtdcce2/ivreg2',
'ereturn list',
'matrix list e(b)',
'matrix list e(V)',
'',
'* IMPORTANT: do not call a conventional ivreg AR interval',
'* the DCCE-PMG-IV AR interval without an appropriate',
'* long-run restriction/inversion.'
)

writeLines(stata, stata_file)

# ---------------------------
# 10. MANUSCRIPT CHECK
# ---------------------------
manuscript_check <- data.frame(
  item = c(
    "Baseline DCCE-PMG lnNEMIG",
    "DCCE-PMG-IV lnNEMIG",
    "First-stage F",
    "Hansen J",
    "Anderson-Rubin 95% set"
  ),
  status = c(
    "Existing manuscript result: 0.4217***",
    "Existing manuscript result: -0.0716, p=0.910",
    "Existing manuscript result: 147.652",
    "Existing manuscript result: 0.126, p=0.722",
    "MUST BE CALCULATED; do not invent"
  )
)
write.csv(manuscript_check,
          file.path(OUT_DIR,"11_manuscript_result_status.csv"),
          row.names=FALSE)

writeLines(
c(
"FINAL REPLICATION STATUS",
"========================",
"Baseline DCCE-PMG and DCCE-PMG-IV are estimated in Stata xtdcce2.",
"The R conventional IV model is a benchmark only.",
"The Anderson-Rubin interval must not be labelled DCCE-PMG-IV AR",
"unless the long-run PMG restriction has been correctly inverted.",
"Existing manuscript values:",
"  Baseline lnNEMIG = 0.4217***",
"  IV lnNEMIG = -0.0716, p=0.910",
"  First-stage F = 147.652",
"  Hansen J = 0.126, p=0.722"
),
file.path(OUT_DIR,"12_README_RESULTS.txt")
)

message("Preparation complete. Output folder: ", OUT_DIR)
