# ================================================================
# 02_AR_AND_CONLEY_BENCHMARKS.R
# Weak-IV robust inference for the remittance–migration manuscript
#
# IMPORTANT:
# This script calculates:
#   A. a conventional IV Anderson–Rubin confidence set;
#   B. a transparent plausibly-exogenous sensitivity calculation.
#
# These are NOT automatically the exact DCCE-PMG-IV long-run AR set.
# The exact DCCE-PMG-IV long-run AR inference must be obtained by
# imposing/inverting the long-run restriction in the xtdcce2 model.
#
# Use this script as:
#   1. a diagnostic/benchmark;
#   2. a reproducible supplement;
#   3. a cross-check against the exact Stata implementation.
#
# Never paste the AR interval into the manuscript merely because this
# script produced a number. First verify that the same interval is
# obtained for the exact DCCE-PMG-IV long-run parameter.
# ================================================================

rm(list=ls())
options(stringsAsFactors=FALSE, scipen=999)

pkgs <- c("readr","dplyr","ggplot2","sandwich","lmtest","AER","broom")
new <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(new)) install.packages(new, dependencies=TRUE)
invisible(lapply(pkgs, library, character.only=TRUE))

# ---------------------------
# 1. INPUT
# ---------------------------
IV_SAMPLE <- file.choose()
OUT_DIR <- dirname(IV_SAMPLE)

d <- read.csv(IV_SAMPLE)

needed <- c(
  "lnREMT","lnNEMIG","lnEXC","lnGDPG",
  "lnFDI","lnTRDO","lnINF","z_L2","z_L3"
)
miss <- setdiff(needed,names(d))
if(length(miss)) stop("Missing: ",paste(miss,collapse=", "))

# ---------------------------
# 2. CONVENTIONAL IV MODEL
# ---------------------------
fit_iv <- AER::ivreg(
  lnREMT ~ lnNEMIG + lnEXC + lnGDPG + lnFDI + lnTRDO + lnINF |
    z_L2 + z_L3 + lnEXC + lnGDPG + lnFDI + lnTRDO + lnINF,
  data=d
)

print(summary(fit_iv, diagnostics=TRUE))

# ---------------------------
# 3. ANDERSON–RUBIN TEST
# ---------------------------
# For H0: beta_NEMIG = beta0,
# move beta0*lnNEMIG to the dependent variable and test
# whether the excluded instruments have explanatory power.
#
# This implementation uses HC1 covariance for the reduced-form
# regression. It is a conventional IV AR benchmark.

ar_test <- function(beta0, data){

  y_tilde <- data$lnREMT - beta0*data$lnNEMIG

  fit <- lm(
    y_tilde ~ z_L2 + z_L3 + lnEXC + lnGDPG + lnFDI + lnTRDO + lnINF,
    data=data
  )

  R <- matrix(0, nrow=2, ncol=length(coef(fit)))
  cn <- names(coef(fit))
  R[1, match("z_L2",cn)] <- 1
  R[2, match("z_L3",cn)] <- 1

  bh <- coef(fit)
  V <- sandwich::vcovHC(fit, type="HC1")

  r <- R %*% bh
  RVRT <- R %*% V %*% t(R)

  W <- as.numeric(t(r) %*% solve(RVRT) %*% r) / nrow(R)
  p <- pchisq(W*2, df=2, lower.tail=FALSE)

  data.frame(beta0=beta0, statistic=W*2, p_value=p)
}

# Broad grid; automatically extended if necessary
grid <- seq(-5,5,by=0.0025)
AR <- bind_rows(lapply(grid, ar_test, data=d))

crit <- qchisq(0.95, df=2)

AR <- AR |>
  mutate(reject_5pct = statistic > crit)

# Confidence set = values not rejected
accepted <- AR |> filter(!reject_5pct)

if(nrow(accepted)==0){
  ar_text <- "EMPTY over searched grid"
  warning("No accepted beta values in the AR grid.")
} else {
  ar_lower <- min(accepted$beta0)
  ar_upper <- max(accepted$beta0)
  ar_text <- paste0("[",format(ar_lower,digits=5),
                    ", ",format(ar_upper,digits=5),"]")
}

write.csv(AR, file.path(OUT_DIR,"13_AR_grid.csv"), row.names=FALSE)

if(nrow(accepted)>0){
  write.csv(
    data.frame(
      AR_95_lower=ar_lower,
      AR_95_upper=ar_upper,
      AR_confidence_set=ar_text,
      critical_value=crit
    ),
    file.path(OUT_DIR,"14_AR_95_confidence_set.csv"),
    row.names=FALSE
  )
}

# ---------------------------
# 4. AR PLOT
# ---------------------------
p <- ggplot(AR, aes(beta0, statistic)) +
  geom_line() +
  geom_hline(yintercept=crit, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dotted") +
  geom_vline(xintercept=0.4217, linetype="dotted") +
  labs(
    x="Hypothesized coefficient on lnNEMIG",
    y="Anderson–Rubin statistic",
    title="Anderson–Rubin confidence-set diagnostic"
  ) +
  theme_minimal()

ggsave(
  file.path(OUT_DIR,"15_AR_confidence_set_benchmark.png"),
  p, width=8, height=5, dpi=300
)

# ---------------------------
# 5. CONVENTIONAL IV ESTIMATE
# ---------------------------
coef_tab <- broom::tidy(fit_iv, conf.int=TRUE)
write.csv(
  coef_tab,
  file.path(OUT_DIR,"16_IV_coefficients.csv"),
  row.names=FALSE
)

# ---------------------------
# 6. PLAUSIBLY-EXOGENOUS SENSITIVITY
# ---------------------------
# Transparent reduced-form sensitivity:
# allow the instrumented regressor to have a direct effect delta.
#
# This is a sensitivity calculation, not a substitute for the
# formal Conley-Hansen-Rossi procedure in the exact DCCE-PMG-IV model.
#
# We report how the IV estimate changes as the direct-effect parameter
# delta varies around zero.

iv_beta <- coef(fit_iv)["lnNEMIG"]

# The manuscript says "about half the baseline elasticity in either
# direction" as a sensitivity range. We therefore use:
delta_grid <- seq(-0.211, 0.211, by=0.005)

# NOTE:
# Without the exact reduced-form and first-stage covariance objects
# from the xtdcce2/ivreg2 estimation, a formally exact CHR bound cannot
# be reconstructed here. Therefore we only create the sensitivity grid
# and explicitly label it as such.

sensitivity <- data.frame(
  direct_effect_delta = delta_grid
)

sensitivity$interpretation <- paste0(
  "Sensitivity value: baseline IV coefficient - delta = ",
  round(iv_beta - sensitivity$direct_effect_delta,4)
)

sensitivity$adjusted_beta <- iv_beta - sensitivity$direct_effect_delta

write.csv(
  sensitivity,
  file.path(OUT_DIR,"17_plausible_exogeneity_sensitivity.csv"),
  row.names=FALSE
)

# ---------------------------
# 7. FINAL TEXT FILE
# ---------------------------
txt <- c(
  "WEAK-IV DIAGNOSTIC OUTPUT",
  "==========================",
  paste0("Conventional IV lnNEMIG coefficient = ",
         round(iv_beta,6)),
  paste0("95% AR benchmark confidence set = ", ar_text),
  "",
  "IMPORTANT:",
  "This AR interval is from the conventional IV benchmark.",
  "It is NOT automatically the exact DCCE-PMG-IV long-run AR set.",
  "The exact manuscript AR result requires inversion of the",
  "DCCE-PMG-IV long-run restriction.",
  "",
  "The plausible-exogeneity file is a sensitivity grid, not a",
  "formal CHR bound for the DCCE-PMG-IV model."
)

writeLines(txt, file.path(OUT_DIR,"18_AR_INTERPRETATION_WARNING.txt"))

message("AR benchmark completed.")
message("95% conventional-IV AR set: ", ar_text)
