## =========================================================================
##  Remittance_R1_replication.R
##  Manuscript 3839467, Asia & the Pacific Policy Studies — Revision R1
##  "Migration and Remittances Are Jointly Determined: Evidence from
##   Low- and Middle-Income Countries"
##
##  R equivalent of Remittance_R1_full_replication.do (Parts B and C).
##  Stata's xtdcce2 has no R package, so the DCCE-PMG estimator is coded
##  directly: country-specific terms (constant, short-run differences,
##  cross-sectional averages) are partialled out country by country, and
##  the long-run (level) coefficients are pooled — the same Frisch-Waugh
##  step xtdcce2 uses for pooled coefficients. Point estimates match
##  xtdcce2; standard errors are reported as homoskedastic and
##  country-clustered.
##
##  Sections
##    0  setup                     C2  IV, first stage, Hansen J  (Table 9)
##    1  data + sign correction    C3  Anderson-Rubin set [a, b]  (R1 3.2b)
##    2  Tables 3-7 (descriptives,  C4  Conley bounds              (Supp. Table S2)
##       CD, slope heterogeneity, CIPS unit roots, cointegration)
##    3  baseline DCCE-PMG (T8)    C5  magnitudes, pp of GDP      (Table 11)
##    C1 samples + country list    C6  regional sub-samples       (App. 1-2)
##                                 C8  Dumitrescu-Hurlin          (Table 10)
##                                 C9  logs of |x| check, Figure 2 (Sec. 4.9)
##
##  Packages: readxl (required); WDI (optional, downloads GDP growth).
##  Run in RStudio: open the file, set `root` below, press Ctrl+Shift+Enter.
## =========================================================================

## ---------------------------------------------------------------- 0. Setup
root <- "G:/Other computers/My Laptop/Arifa_research/Remittance"   # edit
setwd(root)
xls  <- "Data_new_remittance.xlsx"

## TRANSFORM: how net emigration, GDP growth, inflation and FDI enter.
##   "absolute" = natural log of the absolute values in sheet "Final_add (2)"
##                (as in the submitted manuscript: negative values - net
##                immigration, recessions, deflation, negative FDI - are
##                entered with a positive sign)
##   "signed"   = asinh() for net emigration and FDI, ln(1 + rate/100) for
##                growth and inflation, keeping the sign of every observation
TRANSFORM <- "absolute"
outdir <- file.path(root, "results1")    # all output files go here (full path)
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
cat("Working folder:", getwd(), "\nOutput folder :", outdir, "\n")
writable <- file.create(file.path(outdir, "_write_test.txt"))
if (!writable) stop("Cannot write to ", outdir, " - choose a local folder (e.g. C:/Remittance) for root")
file.remove(file.path(outdir, "_write_test.txt"))
sink(file.path(outdir, "R1_replication_R.log"), split = TRUE)

if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)
set.seed(1)

## ---------------------------------------------------------------- 1. Data
fixname <- function(x) { x[x == "Hondurus"] <- "Honduras"; x }

base <- as.data.frame(read_excel(xls, sheet = "Final_add (2)"))
names(base) <- trimws(names(base))
base$Country <- fixname(base$Country)
base <- base[, c("Country","Year","REMT","NEMIG","EXC","GDP","INF","CPI","UNE","TRDO","FDI","GDPG")]
names(base)[names(base) %in% c("NEMIG","INF","FDI","GDPG")] <-
  c("NEMIG_abs","INF_abs","FDI_abs","GDPG_abs")

## signed net migration and inflation (sheet "Main": col 3 and col 7)
main <- as.data.frame(read_excel(xls, sheet = "Main"))
main <- data.frame(Country = fixname(main[[1]]), Year = as.numeric(main[[2]]),
                   NM  = suppressWarnings(as.numeric(main[[3]])),
                   INF = suppressWarnings(as.numeric(main[[7]])))

## signed FDI (sheet "Final_add", column "FDI")
fa <- as.data.frame(read_excel(xls, sheet = "Final_add"))
names(fa) <- trimws(names(fa))
fa <- data.frame(Country = fixname(fa$Country), Year = as.numeric(fa$Year),
                 FDI = suppressWarnings(as.numeric(fa$FDI)))

## country codes
iso <- c("Bangladesh"="BGD","China"="CHN","Colombia"="COL","Dominican Republic"="DOM",
         "Egypt"="EGY","El Salvador"="SLV","Guatemala"="GTM","Honduras"="HND","India"="IND",
         "Indonesia"="IDN","Mexico"="MEX","Morocco"="MAR","Nepal"="NPL","Pakistan"="PAK",
         "Philippines"="PHL","Thailand"="THA","Ukraine"="UKR","Vietnam"="VNM")

## signed real GDP growth (WDI NY.GDP.MKTP.KD.ZG)
if (requireNamespace("WDI", quietly = TRUE) && !file.exists("wdi_gdp_growth.csv")) {
  w <- WDI::WDI(country = unname(iso), indicator = "NY.GDP.MKTP.KD.ZG",
                start = 1994, end = 2023)
  w <- data.frame(iso3 = w$iso3c, Year = w$year, GDPG = w$NY.GDP.MKTP.KD.ZG)
  write.csv(w, "wdi_gdp_growth.csv", row.names = FALSE)
}
## if WDI cannot be installed: download the series from
## https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG and save it as
## wdi_gdp_growth.csv with columns iso3, Year, GDPG
if (file.exists("wdi_gdp_growth.csv")) {
  wdi <- read.csv("wdi_gdp_growth.csv")
} else {
  ## no WDI file: growth signs unknown; acceptable only for TRANSFORM = "absolute"
  if (TRANSFORM == "signed") stop("wdi_gdp_growth.csv is needed for TRANSFORM = 'signed'")
  message("wdi_gdp_growth.csv not found - using the sheet's growth values (absolute mode)")
  wdi <- data.frame(iso3 = unname(iso), Year = NA_real_, GDPG = NA_real_)[0, ]
}

d <- merge(base, main, by = c("Country","Year"), all.x = TRUE)
d <- merge(d, fa, by = c("Country","Year"), all.x = TRUE)
d$iso3 <- iso[d$Country]
stopifnot(!any(is.na(d$iso3)))
d <- merge(d, wdi, by = c("iso3","Year"), all.x = TRUE)
d$FDI[is.na(d$FDI)] <- d$FDI_abs[is.na(d$FDI)]       # early ".." years (positive)
d$INF[is.na(d$INF)] <- d$INF_abs[is.na(d$INF)]       # Vietnam 1994-95 (positive)
d <- d[order(d$Country, d$Year), ]
rownames(d) <- NULL

## consistency checks: submitted series = |signed series|
stopifnot(nrow(d) == 540, length(unique(d$Country)) == 18)
stopifnot(all(abs(abs(d$NM)  - d$NEMIG_abs) <= 1e-6 * d$NEMIG_abs))
stopifnot(all(abs(abs(d$INF) - d$INF_abs)   <= 1e-4 * pmax(1, d$INF_abs)))
## GDP growth: the submitted sheet holds |growth|. We keep its magnitudes
## and take only the SIGN from WDI, so no value changes except its sign.
## (Recent WDI vintages differ for a few series, e.g. Honduras 1994-2000.)
d$GDPG_wdi <- d$GDPG
gchk <- abs(abs(d$GDPG_wdi) - d$GDPG_abs)
cat("GDP growth: |WDI| vs submitted, differences > 1 point (vintage revisions):\n")
print(d[which(gchk > 1), c("Country","Year","GDPG_wdi","GDPG_abs")])
d$GDPG <- ifelse(!is.na(d$GDPG_wdi) & d$GDPG_wdi < 0, -1, 1) * d$GDPG_abs
cat("\nCountry-years whose sign was lost in the submitted data:\n",
    " net immigration:", sum(d$NM > 0), "\n",
    " deflation      :", sum(d$INF < 0), "\n",
    " negative FDI   :", sum(d$FDI < 0), "\n",
    " recessions     :", sum(d$GDPG < 0, na.rm = TRUE), "\n")

## transformations defined for zero/negative values
d$asia     <- d$iso3 %in% c("BGD","CHN","IND","IDN","NPL","PAK","PHL","THA","VNM")
d$NEMIG    <- -d$NM                  # + = net emigration (persons)
d$lnREMT   <- log(d$REMT)
d$lnEXC    <- log(d$EXC)
d$lnTRDO   <- log(d$TRDO)
if (TRANSFORM == "absolute") {
  d$lnNEMIG <- log(d$NEMIG_abs)
  d$lnFDI   <- log(d$FDI_abs)
  d$lnGDPG  <- log(d$GDPG_abs)
  d$lnINF   <- log(d$INF_abs)
} else {
  d$lnNEMIG <- asinh(d$NEMIG)
  d$lnFDI   <- asinh(d$FDI)
  d$lnGDPG  <- log(1 + d$GDPG / 100)
  d$lnINF   <- log(1 + d$INF / 100)
}
cat("\nTransformation used:", TRANSFORM, "\n")

X  <- c("lnEXC","lnGDPG","lnNEMIG","lnFDI","lnTRDO","lnINF")
lagby <- function(x, g, k = 1) ave(x, g, FUN = function(v) c(rep(NA, k), head(v, -k)))
for (v in c("lnREMT", X)) {
  d[[paste0("L_", v)]] <- lagby(d[[v]], d$Country)
  d[[paste0("D_", v)]] <- d[[v]] - d[[paste0("L_", v)]]
}
d$L2N <- lagby(d$lnNEMIG, d$Country, 2)
d$L3N <- lagby(d$lnNEMIG, d$Country, 3)
## contemporaneous cross-sectional averages of all differenced variables
for (v in c("lnREMT", X))
  d[[paste0("A_", v)]] <- ave(d[[paste0("D_", v)]], d$Year, FUN = function(z) mean(z, na.rm = TRUE))

## ---------------------------------------------------------------- helpers
## country-by-country partialling (Frisch-Waugh), as in xtdcce2 pooled()
partial <- function(s, cols, nuis) {
  out <- s[, cols]
  for (c in unique(s$Country)) {
    i <- which(s$Country == c)
    Z <- cbind(1, as.matrix(s[i, nuis]))
    A <- as.matrix(s[i, cols])
    out[i, ] <- A - Z %*% qr.coef(qr(Z), A)
  }
  out
}
clvcov <- function(Xh, u, g, bread) {          # CR1 cluster-robust
  G <- unique(g); k <- ncol(Xh); n <- length(u)
  meat <- Reduce(`+`, lapply(G, function(c) { s <- crossprod(Xh[g == c, , drop = FALSE], u[g == c]); s %*% t(s) }))
  length(G) / (length(G) - 1) * (n - 1) / (n - k) * bread %*% meat %*% bread
}
iv <- function(y, X, Z, g) {                     # pooled 2SLS
  Xh <- Z %*% qr.coef(qr(Z), X)
  b  <- solve(crossprod(Xh, X), crossprod(Xh, y)); u <- y - X %*% b
  Bi <- solve(crossprod(Xh))
  list(b = drop(b), u = drop(u), Vc = clvcov(Xh, u, g, Bi),
       Vh = sum(u^2) / (length(u) - ncol(X)) * Bi, Xh = Xh)
}
wald <- function(b, V, j) { w <- drop(t(b[j]) %*% solve(V[j, j], b[j])); c(W = w, p = 1 - pchisq(w, length(j))) }
lr_table <- function(fit, lv, vars, nhet = 0) {
  b <- unname(fit$b); phi <- b[1]
  t(sapply(seq_along(vars), function(j) {
    beta <- -b[j + 1] / phi
    gr <- rep(0, length(b)); gr[1] <- b[j + 1] / phi^2; gr[j + 1] <- -1 / phi
    se_h <- sqrt(drop(t(gr) %*% fit$Vh %*% gr)); se_c <- sqrt(drop(t(gr) %*% fit$Vc %*% gr))
    c(LR = beta, se_h = se_h, p_h = 2 * pnorm(-abs(beta / se_h)),
      se_cl = se_c, p_cl = 2 * pnorm(-abs(beta / se_c)))
  })) -> tab
  rownames(tab) <- vars; round(tab, 3)
}


## Mean-Group short-run coefficients (xtdcce2 reports the heterogeneous
## d.x coefficients as MG averages): given pooled long-run coefficients b,
## regress Dy - Xlevels*b on [1, d.x, CSA] country by country.
sr_mg <- function(s, b, lv, dx) {
  csa <- paste0("A_", csa_vars)
  B <- t(sapply(unique(s$Country), function(c) {
    z <- s[s$Country == c, ]
    yy <- z$D_lnREMT - as.matrix(z[, lv]) %*% b
    Z <- cbind(1, as.matrix(z[, dx]), as.matrix(z[, csa]))
    qr.coef(qr(Z), yy)[2:(length(dx) + 1)]
  }))
  m <- colMeans(B, na.rm = TRUE); se <- apply(B, 2, sd, na.rm = TRUE) / sqrt(nrow(B))
  out <- cbind(MG = m, se = se, z = m / se, p = 2 * pnorm(-abs(m / se)))
  rownames(out) <- dx; round(out, 3)
}

## ---------------------------------------------------------------- 2. Descriptives, CD
cat("\n== Descriptive statistics of the signed series (Table 3 reports the series as used) ==\n")
desc <- sapply(d[, c("REMT","EXC","GDPG","NEMIG","FDI","TRDO","INF")],
               function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
                             min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE)))
print(round(t(desc), 2))

cd_test <- function(v) {
  P <- reshape(d[, c("Country","Year", v)], idvar = "Year", timevar = "Country", direction = "wide")
  P <- as.matrix(P[complete.cases(P), -1]); N <- ncol(P); T <- nrow(P)
  R <- cor(P); r <- R[upper.tri(R)]
  c(CD = sqrt(2 * T / (N * (N - 1))) * sum(r), p = 2 * pnorm(-abs(sqrt(2 * T / (N * (N - 1))) * sum(r))),
    corr = mean(r), abs_corr = mean(abs(r)))
}
cat("\n== Table 4: Pesaran CD tests ==\n")
print(round(t(sapply(c("lnREMT", X), cd_test)), 3))

## ---------------------------------------------------------------- 2b. Slope heterogeneity (Table 5)
## Pesaran & Yamagata (2008) delta and adjusted delta, lnREMT on the regressors
py_delta <- function(yv, xv) {
  z <- d[complete.cases(d[, c(yv, xv)]), ]
  ids <- unique(z$Country); N <- length(ids); k <- length(xv)
  dm <- function(M) sweep(M, 2, colMeans(M))
  XX <- list(); Xy <- list(); bi <- list(); s2 <- c(); Ti <- c()
  for (c in ids) {
    zi <- z[z$Country == c, ]; Xm <- dm(as.matrix(zi[, xv])); ym <- zi[[yv]] - mean(zi[[yv]])
    XX[[c]] <- crossprod(Xm); Xy[[c]] <- crossprod(Xm, ym); bi[[c]] <- solve(XX[[c]], Xy[[c]])
    Ti[c] <- nrow(zi)
  }
  bfe <- solve(Reduce(`+`, XX), Reduce(`+`, Xy))
  for (c in ids) {                      # sigma_i^2 from FE residuals
    zi <- z[z$Country == c, ]; Xm <- dm(as.matrix(zi[, xv])); ym <- zi[[yv]] - mean(zi[[yv]])
    e <- ym - Xm %*% bfe; s2[c] <- sum(e^2) / (Ti[c] - 1)
  }
  bw <- solve(Reduce(`+`, lapply(ids, function(c) XX[[c]] / s2[c])),
              Reduce(`+`, lapply(ids, function(c) Xy[[c]] / s2[c])))
  S <- sum(sapply(ids, function(c) drop(t(bi[[c]] - bw) %*% XX[[c]] %*% (bi[[c]] - bw)) / s2[c]))
  T <- mean(Ti)
  dl  <- sqrt(N) * (S / N - k) / sqrt(2 * k)
  dla <- sqrt(N) * (S / N - k) / sqrt(2 * k * (T - k - 1) / (T + 1))
  rbind(delta = c(stat = dl, p = 2 * pnorm(-abs(dl))), delta_adj = c(stat = dla, p = 2 * pnorm(-abs(dla))))
}
cat("\n== Table 5: Pesaran-Yamagata slope homogeneity ==\n")
print(round(py_delta("lnREMT", X), 3))

## ---------------------------------------------------------------- 2c. CIPS unit-root tests (Table 6)
## Pesaran (2007) CADF/CIPS with intercept and one lag; p-values by Monte
## Carlo under the null (independent random walks, same N and T).
cadf_t <- function(M, p = 1) {               # M: T x N matrix (columns = countries)
  Tn <- nrow(M); yb <- rowMeans(M)
  sapply(seq_len(ncol(M)), function(i) {
    y <- M[, i]; dy <- c(NA, diff(y)); dyb <- c(NA, diff(yb))
    lag1 <- function(v, k) c(rep(NA, k), head(v, -k))
    D <- data.frame(dy = dy, yl = lag1(y, 1), ybl = lag1(yb, 1), dyb = dyb)
    for (k in seq_len(p)) { D[[paste0("dyl", k)]] <- lag1(dy, k); D[[paste0("dybl", k)]] <- lag1(dyb, k) }
    D <- D[complete.cases(D), ]
    f <- summary(lm(dy ~ ., data = D)); f$coefficients["yl", "t value"]
  })
}
cips <- function(M, p = 1) mean(pmax(pmin(cadf_t(M, p), 6.19), -6.19))   # Pesaran truncation
panel_matrix <- function(v) {
  P <- reshape(d[, c("Country", "Year", v)], idvar = "Year", timevar = "Country", direction = "wide")
  as.matrix(P[order(P$Year), -1])
}
NT <- dim(panel_matrix("lnREMT"))
null_cips <- replicate(1000, cips(apply(matrix(rnorm(NT[1] * NT[2]), NT[1]), 2, cumsum)))
cat("\n== Table 6: CIPS (intercept, 1 lag); Monte Carlo 5% critical value =",
    round(quantile(null_cips, 0.05), 3), "==\n")
T6 <- t(sapply(c("lnREMT", X), function(v) {
  M <- panel_matrix(v); Md <- apply(M, 2, diff)
  c(CIPS_level = cips(M), p_level = mean(null_cips <= cips(M)),
    CIPS_diff = cips(Md), p_diff = mean(null_cips <= cips(Md)))
}))
print(round(T6, 3))

## ---------------------------------------------------------------- 2d. Cointegration (Table 7)
## Residual-based, cross-section-robust test (Holly, Pesaran & Yamagata 2010):
## CIPS on residuals of country-by-country CCE long-run regressions of lnREMT
## on the regressors. p-value by Monte Carlo under no cointegration.
cce_resid_cips <- function(Y, XL) {          # Y: T x N; XL: list of T x N matrices
  yb <- rowMeans(Y); xb <- sapply(XL, rowMeans)
  E <- sapply(seq_len(ncol(Y)), function(i) {
    Xi <- cbind(1, sapply(XL, function(M) M[, i]), yb, xb)
    ok <- complete.cases(cbind(Y[, i], Xi))
    e <- rep(NA, nrow(Y)); e[ok] <- lm.fit(Xi[ok, ], Y[ok, i])$residuals; e
  })
  E <- E[complete.cases(E), ]
  cips(E)
}
XL <- lapply(X, panel_matrix); Y <- panel_matrix("lnREMT")
stat7 <- cce_resid_cips(Y, XL)
null7 <- replicate(500, {
  rw <- function() apply(matrix(rnorm(NT[1] * NT[2]), NT[1]), 2, cumsum)
  cce_resid_cips(rw(), lapply(seq_along(X), function(j) rw()))
})
cat("\n== Table 7: residual-based CIPS cointegration test ==\n")
cat("CIPS(residuals) =", round(stat7, 3), "  Monte Carlo p =", round(mean(null7 <= stat7), 3),
    "  (H0: no cointegration)\n")
cat("Also see the error-correction term in Table 8: a significant negative phi indicates cointegration.\n")

## ---------------------------------------------------------------- 3. Baseline DCCE-PMG (Table 8)
lv   <- c("L_lnREMT", "L_lnEXC", "L_lnGDPG", "L_lnNEMIG", "L_lnFDI", "L_lnTRDO", "L_lnINF")
csa_vars <- if (TRANSFORM == "absolute") c("lnREMT", setdiff(X, "lnNEMIG")) else c("lnREMT", X)
nuis <- c(paste0("D_", X), paste0("A_", csa_vars))
s <- d[complete.cases(d[, c("D_lnREMT", lv, nuis, "L2N", "L3N")]), ]   # IV sample
cat("\nEstimation sample (IV sample used for both models):", nrow(s), "obs,",
    length(unique(s$Country)), "countries,", min(s$Year), "-", max(s$Year), "\n")
R <- partial(s, c("D_lnREMT", lv, "L2N", "L3N"), nuis)
g <- s$Country
y <- R$D_lnREMT; Xl <- as.matrix(R[, lv])
if (TRANSFORM == "absolute") {                 # submitted Table 8: full sample
  s0 <- d[complete.cases(d[, c("D_lnREMT", lv, nuis)]), ]
  R0 <- partial(s0, c("D_lnREMT", lv), nuis)
  base_fit <- iv(R0$D_lnREMT, as.matrix(R0[, lv]), as.matrix(R0[, lv]), s0$Country)
  s_base <- s0
  cat("Baseline sample (as submitted):", nrow(s0), "obs\n")
} else {
  base_fit <- iv(y, Xl, Xl, g)                 # OLS = IV with X as own instrument
  s_base <- s
}
cat("\n== Table 8b: baseline DCCE-PMG long-run coefficients ==\n")
cat("Pooled ECT phi =", round(base_fit$b[1], 3), " (cluster se", round(sqrt(base_fit$Vc[1, 1]), 3), ")\n")
print(lr_table(base_fit, lv, X))
cat("\n== Table 8a: baseline short-run Mean-Group coefficients ==\n")
print(sr_mg(s_base, base_fit$b, lv, paste0("D_", X)))

## ---------------------------------------------------------------- C1. Samples and country list
cat("\n== Supplementary Table S1: countries ==\n")
A1 <- do.call(rbind, lapply(split(d, d$Country), function(z)
  data.frame(Country = z$Country[1], iso3 = z$iso3[1], Asia = z$asia[1],
             first = min(z$Year), last = max(z$Year), T = nrow(z), REMT_mean = round(mean(z$REMT), 2))))
print(A1, row.names = FALSE)
write.csv(A1, file.path(outdir, "SuppTableS1_countries.csv"), row.names = FALSE)
write.csv(d[, c("Country","iso3","Year","REMT","NEMIG","EXC","GDPG","FDI","TRDO","INF","lnREMT", X)],
          file.path(outdir, "analysis_sample_full.csv"), row.names = FALSE)
write.csv(s[, c("Country","iso3","Year","lnREMT", X, "L2N","L3N")],
          file.path(outdir, "analysis_sample_DCCE_PMG_IV.csv"), row.names = FALSE)

## ---------------------------------------------------------------- C2. DCCE-PMG-IV (Table 9)
ex   <- setdiff(lv, "L_lnNEMIG")
Xiv  <- as.matrix(R[, c(ex, "L_lnNEMIG")])
Ziv  <- as.matrix(R[, c(ex, "L2N", "L3N")])
ivf  <- iv(y, Xiv, Ziv, g)
ord  <- match(lv, colnames(Xiv))
ivf$b <- ivf$b[ord]; ivf$Vc <- ivf$Vc[ord, ord]; ivf$Vh <- ivf$Vh[ord, ord]
cat("\n== Table 9b: DCCE-PMG-IV long-run coefficients (L.lnNEMIG instrumented by L2, L3) ==\n")
cat("Pooled ECT phi =", round(ivf$b[1], 3), " (cluster se", round(sqrt(ivf$Vc[1, 1]), 3), ")\n")
print(lr_table(ivf, lv, X))
cat("\n== Table 9c: IV short-run Mean-Group coefficients ==\n")
print(sr_mg(s, ivf$b, lv, paste0("D_", X)))

## first stage: partial F (homoskedastic and cluster-robust)
xN <- R$L_lnNEMIG
fs <- iv(xN, Ziv, Ziv, g); j <- c(length(ex) + 1, length(ex) + 2)
F_cl <- wald(fs$b, fs$Vc, j)["W"] / 2
r_u <- sum(fs$u^2); r_r <- sum(lm.fit(as.matrix(R[, ex]), xN)$residuals^2)
F_h <- ((r_r - r_u) / 2) / (r_u / (length(xN) - ncol(Ziv)))
## Hansen J (cluster-robust, 2SLS residuals)
gZ <- crossprod(Ziv, ivf$u)
S  <- Reduce(`+`, lapply(unique(g), function(c) { z <- crossprod(Ziv[g == c, ], ivf$u[g == c]); z %*% t(z) }))
J  <- drop(t(gZ) %*% solve(S, gZ))
cat("\n== Table 9a: identification diagnostics ==\n")
cat("First-stage partial F: homoskedastic", round(F_h, 1), "; cluster-robust", round(F_cl, 1), "\n")
cat("Hansen J (1 df) =", round(J, 3), " p =", round(1 - pchisq(J, 1), 3), "\n")

## ---------------------------------------------------------------- C3. Anderson-Rubin set
## H0: theta_N = t0. Regress y - t0*x on exogenous regressors and the two
## instruments; the AR test is the cluster-robust Wald test that both
## instrument coefficients are zero. Collect all t0 not rejected at 5%.
ar_set <- function(grid) {
  keep <- sapply(grid, function(t0) {
    f <- iv(y - t0 * xN, Ziv, Ziv, g)
    wald(f$b, f$Vc, j)["p"] > 0.05
  })
  grid[keep]
}
acc <- ar_set(seq(-1, 1, by = 0.0005))
phi <- unname(ivf$b[1])
cat("\n== Anderson-Rubin 95% set ==\n")
if (length(acc) == 0) {
  cat("AR set is empty on the grid (model rejected).\n")
} else {
  lo <- min(acc); hi <- max(acc)
  edge <- lo <= -1 + 1e-9 || hi >= 1 - 1e-9
  cat("theta_N (coefficient on L.lnNEMIG): [", round(lo, 4), ",", round(hi, 4), "]",
      if (edge) "  -> touches grid edge: widen grid / report as unbounded" else "", "\n")
  cat("Long-run NEMIG [a, b] = theta bounds / (-phi) = [",
      round(lo / -phi, 3), ",", round(hi / -phi, 3), "]   (phi =", round(phi, 3), ")\n")
  cat("Contains 0:", lo / -phi <= 0 & hi / -phi >= 0,
      "  Contains 0.422 (submitted estimate):", lo / -phi <= 0.422 & hi / -phi >= 0.422, "\n")
}

## ---------------------------------------------------------------- C4. Conley et al. (2012) bounds
## gamma = direct effect of each lagged instrument on remittances (theta scale)
conley <- function(lo, hi, n = 9) {
  L <- Inf; U <- -Inf
  for (g1 in seq(lo, hi, length.out = n)) for (g2 in seq(lo, hi, length.out = n)) {
    f <- iv(y - g1 * R$L2N - g2 * R$L3N, Xiv, Ziv, g)
    k <- ncol(Xiv); se <- sqrt(f$Vc[k, k])
    L <- min(L, f$b[k] - 1.96 * se); U <- max(U, f$b[k] + 1.96 * se)
  }
  c(theta_lo = L, theta_hi = U, LR_lo = L / -phi, LR_hi = U / -phi)
}
cat("\n== Supplementary Table S2: Conley union-of-CI bounds for the long-run NEMIG effect ==\n")
A2 <- rbind(`gamma in [-0.01, 0.01]` = conley(-0.01, 0.01),
            `gamma in [-0.02, 0.02]` = conley(-0.02, 0.02),
            `gamma in [0, 0.02]`     = conley(0, 0.02),
            `gamma in [-0.02, 0]`    = conley(-0.02, 0))
print(round(A2, 3))
write.csv(round(A2, 4), file.path(outdir, "SuppTableS2_conley.csv"))

## ---------------------------------------------------------------- C5. Magnitudes (Table 11)
mREMT <- mean(d$REMT)
LRiv  <- lr_table(ivf, lv, X)
mag <- function(beta, dx) (exp(beta * dx) - 1) * 100
cat("\n== Table 11: long-run magnitudes (IV model), mean REMT =", round(mREMT, 2), "% of GDP ==\n")
T11 <- data.frame(
  Scenario = if (TRANSFORM == "absolute")
    c("Trade-to-GDP +10%", "10% depreciation vs US$", "Growth rate +10%", "Inflation rate +10%", "Net emigration +10%") else
    c("Trade-to-GDP +10%", "10% depreciation vs US$", "Growth +1 percentage point",
      "Inflation +1 percentage point", "Net emigration +10%"),
  LR = LRiv[c("lnTRDO","lnEXC","lnGDPG","lnINF","lnNEMIG"), "LR"],
  p_cl = LRiv[c("lnTRDO","lnEXC","lnGDPG","lnINF","lnNEMIG"), "p_cl"],
  dx = if (TRANSFORM == "absolute") rep(log(1.1), 5) else c(log(1.1), log(1.1), 0.01, 0.01, log(1.1)))
T11$pct_change_REMT <- round(mag(T11$LR, T11$dx), 2)
T11$pp_of_GDP       <- round(T11$pct_change_REMT / 100 * mREMT, 3)
print(T11[, -4], row.names = FALSE)
write.csv(T11, file.path(outdir, "Table11_magnitudes.csv"), row.names = FALSE)

## ---------------------------------------------------------------- C6. Regional sub-samples
cat("\n== Appendices 1-2: regional sub-samples (baseline DCCE-PMG) ==\n")
for (grp in c(TRUE, FALSE)) {
  ss <- s_base[s_base$asia == grp, ]           # same sample as the baseline model
  ## recompute cross-sectional averages within the sub-sample
  for (v in c("lnREMT", X)) ss[[paste0("A_", v)]] <- ave(ss[[paste0("D_", v)]], ss$Year, FUN = mean)
  Rs <- partial(ss, c("D_lnREMT", lv), nuis)
  fit <- iv(Rs$D_lnREMT, as.matrix(Rs[, lv]), as.matrix(Rs[, lv]), ss$Country)
  cat(if (grp) "\nAsia (9 countries)\n" else "\nLatin America, Africa, Europe (9 countries)\n")
  cat("Short-run Mean-Group coefficients (Appendices 1-2):\n")
  print(sr_mg(ss, fit$b, lv, paste0("D_", X)))
  cat("Long-run coefficients:\n")
  print(lr_table(fit, lv, X))
}


if (TRANSFORM == "signed") {
## ---------------------------------------------------------------- C9. Logs of absolute values (Section 4.9, Figure 2)
## Same estimator on the submitted transformation: ln|x| for net emigration,
## growth, inflation and FDI (signs discarded), CSA without d.ln|NEMIG|,
## full sample 1995-2023. Reproduces the submitted long-run estimates.
a <- d
a$lnNEMIG_a <- log(a$NEMIG_abs); a$lnGDPG_a <- log(a$GDPG_abs)
a$lnINF_a <- log(a$INF_abs);     a$lnFDI_a <- log(a$FDI_abs)
Xa <- c("lnEXC","lnGDPG_a","lnNEMIG_a","lnFDI_a","lnTRDO","lnINF_a")
for (v in c("lnREMT", Xa)) {
  a[[paste0("L_", v)]] <- lagby(a[[v]], a$Country)
  a[[paste0("D_", v)]] <- a[[v]] - a[[paste0("L_", v)]]
}
csa_a <- c("lnREMT","lnEXC","lnGDPG_a","lnFDI_a","lnTRDO","lnINF_a")
for (v in csa_a) a[[paste0("B_", v)]] <- ave(a[[paste0("D_", v)]], a$Year, FUN = function(z) mean(z, na.rm = TRUE))
lva <- paste0("L_", c("lnREMT", Xa)); nua <- c(paste0("D_", Xa), paste0("B_", csa_a))
sa <- a[complete.cases(a[, c("D_lnREMT", lva, nua)]), ]
Ra <- partial(sa, c("D_lnREMT", lva), nua)
fa_fit <- iv(Ra$D_lnREMT, as.matrix(Ra[, lva]), as.matrix(Ra[, lva]), sa$Country)
cat("\n== Section 4.9: same estimator on logs of absolute values (n =", nrow(sa), ") ==\n")
abs_tab <- lr_table(fa_fit, lva, Xa); print(abs_tab)

## Figure 2
fig <- data.frame(
  lab = c("DCCE-PMG, logs of absolute values\n(signs discarded)", "DCCE-PMG, signed series",
          "DCCE-PMG-IV, signed series", "Anderson-Rubin 95% set\n(IV, weak-instrument robust)"),
  b  = c(abs_tab["lnNEMIG_a", "LR"], lr_table(base_fit, lv, X)["lnNEMIG", "LR"], lr_table(ivf, lv, X)["lnNEMIG", "LR"], NA),
  se = c(abs_tab["lnNEMIG_a", "se_cl"], lr_table(base_fit, lv, X)["lnNEMIG", "se_cl"], lr_table(ivf, lv, X)["lnNEMIG", "se_cl"], NA))
fig$lo <- fig$b - 1.96 * fig$se; fig$hi <- fig$b + 1.96 * fig$se
if (length(acc) > 0) { fig$lo[4] <- min(acc) / -phi; fig$hi[4] <- max(acc) / -phi }
print(fig[, c("b","lo","hi")])
png(file.path(outdir, "Figure2.png"), width = 6.2, height = 2.9, units = "in", res = 300)
par(mar = c(4, 16, 1, 6), family = "serif", cex = 0.85)
yy <- 4:1
plot(NA, xlim = c(-0.35, 1.35), ylim = c(0.5, 4.5), yaxt = "n", ylab = "", bty = "l",
     xlab = "Long-run effect of net emigration on remittances (elasticity)")
abline(v = 0, lty = 2, col = "grey40")
cols <- c("grey60", "black", "black", "black")
segments(fig$lo, yy, fig$hi, yy, lwd = c(1.4, 1.4, 1.4, 2.4), col = cols)
segments(fig$lo, yy - 0.1, fig$lo, yy + 0.1, col = cols); segments(fig$hi, yy - 0.1, fig$hi, yy + 0.1, col = cols)
points(fig$b[1:3], yy[1:3], pch = c(1, 16, 15), col = cols[1:3])
axis(2, at = yy, labels = fig$lab, las = 1, tick = TRUE)
dev.off()
}

## ---------------------------------------------------------------- C8. Dumitrescu-Hurlin (Table 10)
dh <- function(yv, xv, K = 1) {
  W <- c(); Tn <- NA
  for (c in unique(d$Country)) {
    z <- d[d$Country == c, ]; yy <- z[[yv]]; xx <- z[[xv]]; n <- length(yy)
    Y <- yy[(K + 1):n]
    M <- cbind(rep(1, n - K), sapply(1:K, function(k) yy[(K + 1 - k):(n - k)]),
               sapply(1:K, function(k) xx[(K + 1 - k):(n - k)]))
    f <- lm.fit(M, Y); s2 <- sum(f$residuals^2) / (length(Y) - ncol(M))
    V <- s2 * solve(crossprod(M)); jj <- (K + 2):(2 * K + 1)
    W <- c(W, drop(t(f$coefficients[jj]) %*% solve(V[jj, jj], f$coefficients[jj]))); Tn <- length(Y)
  }
  N <- length(W); wbar <- mean(W)
  zt <- sqrt(N / (2 * K) * (Tn - 3 * K - 5) / (Tn - 2 * K - 3)) * ((Tn - 3 * K - 3) / (Tn - 3 * K - 1) * wbar - K)
  c(Wbar = wbar, Zbar_tilde = zt, p = 2 * pnorm(-abs(zt)))
}
cat("\n== Table 10: Dumitrescu-Hurlin (row Granger-causes column), lag 1 ==\n")
vars <- c("lnREMT", X)
T10 <- do.call(rbind, lapply(vars, function(a) do.call(rbind, lapply(setdiff(vars, a), function(b)
  data.frame(cause = a, effect = b, t(round(dh(b, a), 3)))))))
print(T10, row.names = FALSE)
write.csv(T10, file.path(outdir, "Table10_DH.csv"), row.names = FALSE)

sink()
cat("\nDone. Results are in", outdir, "\n"); print(list.files(outdir))
