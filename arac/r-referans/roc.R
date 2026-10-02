# paketler: pROC, epiR, psych
# ROC analizi (pROC) ve tanı testi ölçütleri (epiR + elle hesap) referansları.
suppressMessages(library(pROC))
suppressMessages(library(epiR))
data(aSAH)

durum_aSAH <- as.integer(aSAH$outcome == "Poor")

# Hanley-McNeil (1982) SH
hm_se <- function(A, m, n) { Q1 <- A / (2 - A); Q2 <- 2 * A^2 / (1 + A); sqrt((A * (1 - A) + (m - 1) * (Q1 - A^2) + (n - 1) * (Q2 - A^2)) / (m * n)) }

roc_cikar <- function(deger, durum, yon = "auto") {
  r <- roc(durum, deger, levels = c(0, 1), direction = yon, quiet = TRUE)
  ci <- ci.auc(r, method = "delong")
  y <- coords(r, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"), transpose = FALSE)
  k <- coords(r, "best", best.method = "closest.topleft", ret = c("threshold", "sensitivity", "specificity"), transpose = FALSE)
  A <- as.numeric(r$auc)
  m <- length(r$cases); n <- length(r$controls)
  # H0 (AUC = 0.5) altında: bağ düzeltmeli Mann-Whitney (asimptotik, süreklilik düzeltmesiz)
  w <- wilcox.test(r$cases, r$controls, exact = FALSE, correct = FALSE)
  # Youden kesiminde 2x2 tablo ve epi.tests
  t1 <- y$threshold[1]
  poz <- if (r$direction == "<") deger >= t1 else deger <= t1
  tp <- sum(poz & durum == 1); fp <- sum(poz & durum == 0); fn <- sum(!poz & durum == 1); tn <- sum(!poz & durum == 0)
  e <- epi.tests(c(tp, fp, fn, tn), method = "exact")$detail
  list(
    deger = deger, durum = durum, yon = r$direction,
    esik = r$thresholds, duyarlilik = r$sensitivities, ozgulluk = r$specificities,
    auc = A, gaAlt = ci[1], gaUst = ci[3], varyans = if (m > 1 && n > 1) var(r, method = "delong") else NA,
    shHM = hm_se(A, m, n), pH0 = w$p.value,
    youden = list(esik = y$threshold, duyarlilik = y$sensitivity, ozgulluk = y$specificity),
    kose = list(esik = k$threshold, duyarlilik = k$sensitivity, ozgulluk = k$specificity),
    tablo = c(tp, fp, fn, tn), se = unlist(e[e$statistic == "se", 2:4]), sp = unlist(e[e$statistic == "sp", 2:4]),
    lrArti = unlist(e[e$statistic == "lr.pos", 2:4])
  )
}

set.seed(11)
sentetik <- list(
  baglar = list(deger = c(1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 1, 1, 2, 2, 2, 3, 3, 4, 1, 2),
                durum = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
  kucuk = list(deger = c(0.8, 1.9, 0.5, 1.2, 0.3), durum = c(1, 1, 0, 0, 0)),
  tamAyrisma = list(deger = c(5, 6, 7, 8, 1, 2, 3), durum = c(1, 1, 1, 1, 0, 0, 0)),
  ters = list(deger = round(c(rnorm(30, 10, 2), rnorm(25, 12, 2)), 1), durum = c(rep(1, 30), rep(0, 25))),
  negatif = list(deger = round(c(rnorm(40, -1, 1), rnorm(60, -2, 1.3)), 2), durum = c(rep(1, 40), rep(0, 60))),
  esitMedyan = list(deger = c(1, 5, 3, 3, 2, 4, 3, 6), durum = c(1, 1, 1, 0, 0, 0, 0, 1))
)

roclar <- list(
  s100b = roc_cikar(aSAH$s100b, durum_aSAH),
  ndka = roc_cikar(aSAH$ndka, durum_aSAH),
  wfns = roc_cikar(as.numeric(aSAH$wfns), durum_aSAH),
  s100bTers = roc_cikar(aSAH$s100b, durum_aSAH, ">"),
  age = roc_cikar(aSAH$age, durum_aSAH)
)
for (ad in names(sentetik)) roclar[[ad]] <- roc_cikar(sentetik[[ad]]$deger, sentetik[[ad]]$durum)
roclar$baglarTers <- roc_cikar(sentetik$baglar$deger, sentetik$baglar$durum, ">")

# Eşleştirilmiş DeLong
eslesik <- function(x1, x2, durum) {
  r1 <- roc(durum, x1, levels = c(0, 1), quiet = TRUE)
  r2 <- roc(durum, x2, levels = c(0, 1), quiet = TRUE)
  t <- suppressWarnings(roc.test(r1, r2, method = "delong", paired = TRUE))
  list(z = unname(t$statistic), p = t$p.value, gaAlt = t$conf.int[1], gaUst = t$conf.int[2], auc = unname(t$estimate))
}
wfns <- as.numeric(aSAH$wfns)
eslesmis <- list(
  s100b_ndka = eslesik(aSAH$s100b, aSAH$ndka, durum_aSAH),
  s100b_wfns = eslesik(aSAH$s100b, wfns, durum_aSAH),
  wfns_age = eslesik(wfns, aSAH$age, durum_aSAH),
  sentetik = eslesik(sentetik$baglar$deger, sentetik$baglar$deger + c(0, 1, 0, -1, 0, 2, 0, 0, 1, 0, 0, 1, 0, 0, 2, 0, 0, 1, 0, 0), sentetik$baglar$durum)
)

# Bağımsız DeLong (cinsiyete göre)
bagimsiz <- local({
  e <- aSAH$gender == "Male"
  r1 <- roc(durum_aSAH[e], aSAH$s100b[e], levels = c(0, 1), quiet = TRUE)
  r2 <- roc(durum_aSAH[!e], aSAH$s100b[!e], levels = c(0, 1), quiet = TRUE)
  t <- roc.test(r1, r2, method = "delong", paired = FALSE)
  list(deger1 = aSAH$s100b[e], durum1 = durum_aSAH[e], deger2 = aSAH$s100b[!e], durum2 = durum_aSAH[!e],
       D = unname(t$statistic), sd = unname(t$parameter), p = t$p.value, auc = unname(t$estimate))
})

# ---------- Tanı testi ölçütleri ----------
kappa_fce <- function(tab) {
  N <- sum(tab); p <- tab / N; r <- rowSums(p); c <- colSums(p)
  po <- sum(diag(p)); pe <- sum(r * c); k <- (po - pe) / (1 - pe)
  A <- sum(diag(p) * (1 - (r + c) * (1 - k))^2)
  B <- 0
  for (i in 1:nrow(p)) for (j in 1:ncol(p)) if (i != j) B <- B + p[i, j] * (c[i] + r[j])^2
  B <- B * (1 - k)^2
  C <- (k - pe * (1 - k))^2
  se <- sqrt((A + B - C) / (N * (1 - pe)^2))
  se0 <- sqrt((pe + pe^2 - sum(r * c * (r + c))) / (N * (1 - pe)^2))
  list(kappa = k, se = se, se0 = se0)
}
tani_cikar <- function(tp, fp, fn, tn, prev = 0.1) {
  ex <- epi.tests(c(tp, fp, fn, tn), method = "exact")$detail
  wi <- epi.tests(c(tp, fp, fn, tn), method = "wilson")$detail
  al <- function(d, s) unname(unlist(d[d$statistic == s, 2:4]))
  pt_ <- function(x, n) if (n > 0) c(x / n, prop.test(x, n, correct = FALSE)$conf.int) else c(NA, NA, NA)
  bt <- function(x, n) if (n > 0) c(x / n, binom.test(x, n)$conf.int) else c(NA, NA, NA)
  N <- tp + fp + fn + tn; z <- qnorm(0.975)
  se <- tp / (tp + fn); sp <- tn / (fp + tn); n1 <- tp + fn; n0 <- fp + tn
  J <- se + sp - 1; seJ <- sqrt(se * (1 - se) / n1 + sp * (1 - sp) / n0)
  ptp <- tp / N; pfp <- fp / N; pfn <- fn / N; D <- 2 * ptp + pfp + pfn
  F1 <- 2 * ptp / D; seF <- sqrt(4 * ptp * (pfp + pfn) * (ptp + pfp + pfn) / (N * D^4))
  ppv <- se * prev / (se * prev + (1 - sp) * (1 - prev)); npv <- sp * (1 - prev) / ((1 - se) * prev + sp * (1 - prev))
  lppv <- log(se * prev / ((1 - sp) * (1 - prev))); vppv <- (1 - se) / (se * n1) + sp / ((1 - sp) * n0)
  lnpv <- log(sp * (1 - prev) / ((1 - se) * prev)); vnpv <- se / ((1 - se) * n1) + (1 - sp) / (sp * n0)
  kp <- kappa_fce(matrix(c(tp, fn, fp, tn), 2))
  ps <- if (requireNamespace("psych", quietly = TRUE)) {
    ck <- psych::cohen.kappa(matrix(c(tp, fn, fp, tn), 2))
    c(ck$kappa, sqrt(ck$var.kappa))
  } else NULL
  list(
    tablo = c(tp, fp, fn, tn),
    exact = list(se = al(ex, "se"), sp = al(ex, "sp"), ppv = al(ex, "pv.pos"), npv = al(ex, "pv.neg"),
                 dogruluk = al(ex, "diag.ac"), prev = al(ex, "tp"), lrArti = al(ex, "lr.pos"), lrEksi = al(ex, "lr.neg"),
                 dor = al(ex, "diag.or"), youden = al(ex, "youden")),
    wilsonEpi = list(se = al(wi, "se"), sp = al(wi, "sp"), ppv = al(wi, "pv.pos"), npv = al(wi, "pv.neg"), youden = al(wi, "youden")),
    binom = list(se = bt(tp, n1), sp = bt(tn, n0), ppv = bt(tp, tp + fp), npv = bt(tn, fn + tn), dogruluk = bt(tp + tn, N)),
    wilson = list(se = pt_(tp, n1), sp = pt_(tn, n0), ppv = pt_(tp, tp + fp), npv = pt_(tn, fn + tn), dogruluk = pt_(tp + tn, N)),
    youdenWald = c(J, max(-1, J - z * seJ), min(1, J + z * seJ)),
    f1 = c(F1, max(0, F1 - z * seF), min(1, F1 + z * seF)),
    kappa = c(kp$kappa, kp$se, kp$se0, kp$kappa - z * kp$se, kp$kappa + z * kp$se, 2 * pnorm(-abs(kp$kappa / kp$se0))),
    psych = ps,
    duzeltilmis = list(prev = prev, ppv = c(ppv, plogis(lppv - z * sqrt(vppv)), plogis(lppv + z * sqrt(vppv))),
                       npv = c(npv, plogis(lnpv - z * sqrt(vnpv)), plogis(lnpv + z * sqrt(vnpv))))
  )
}
tani <- list(
  tipik = tani_cikar(80, 15, 20, 185),
  kucuk = tani_cikar(9, 2, 1, 8),
  klasik = tani_cikar(231, 32, 27, 54, prev = 0.3),
  dengesiz = tani_cikar(3, 40, 2, 955, prev = 0.01)
)
# Sıfır hücreli tablo (epi.tests sonsuz/NaN verir; yalnızca oranlar karşılaştırılır)
sifirHucre <- local({
  ex <- epi.tests(c(10, 0, 5, 20), method = "exact")$detail
  al <- function(s) unname(unlist(ex[ex$statistic == s, 2:4]))
  list(se = al("se"), sp = al("sp"), ppv = al("pv.pos"), npv = al("pv.neg"), lrEksi = al("lr.neg"))
})
# k x k kappa (psych ile)
kappa3 <- local({
  tab <- matrix(c(20, 5, 2, 3, 15, 4, 1, 6, 18), 3)
  k <- kappa_fce(tab)
  ps <- if (requireNamespace("psych", quietly = TRUE)) { ck <- psych::cohen.kappa(tab); c(ck$kappa, sqrt(ck$var.kappa)) } else NULL
  list(tablo = unname(tab), kappa = k$kappa, se = k$se, se0 = k$se0, psych = ps)
})

sonuc <- list(roc = roclar, eslesmis = eslesmis, bagimsiz = bagimsiz, tani = tani, sifirHucre = sifirHucre, kappa3 = kappa3)
