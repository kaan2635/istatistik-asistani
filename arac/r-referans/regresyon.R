# paketler: car, lmtest
# Doğrusal regresyon referansları: summary(lm), anova, drop1, car::Anova(type = 3), car::vif,
# lmtest::bptest / dwtest, etki istatistikleri, hiyerarşik ve adımsal (SPSS kuralının R benzetimi).

sutunlar <- function(df) lapply(as.list(df), function(v) if (is.factor(v)) as.character(v) else v)

lmOzet <- function(fit, duzey = 0.95) {
  s <- summary(fit)
  X <- model.matrix(fit)
  y <- model.response(model.frame(fit))
  cf <- coef(fit)                        # NA'lar dahil
  tab <- matrix(NA_real_, length(cf), 4)
  rownames(tab) <- names(cf)
  tab[rownames(s$coefficients), ] <- s$coefficients
  ci <- matrix(NA_real_, length(cf), 2)
  ok <- !is.na(cf)
  ci[ok, ] <- confint(fit, level = duzey)[names(cf)[ok], ]
  sabitVar <- attr(terms(fit), "intercept") == 1
  # SPSS Beta: B * sd(x) / sd(y) (gösterge sütunları dahil)
  beta <- rep(NA_real_, length(cf))
  vif <- rep(NA_real_, length(cf))
  sifir <- kismi <- yari <- rep(NA_real_, length(cf))
  if (sabitVar) {
    idx <- which(ok & names(cf) != "(Intercept)")
    beta[idx] <- cf[idx] * apply(X[, idx, drop = FALSE], 2, sd) / sd(y)
    if (length(idx) > 1) {
      vif[idx] <- diag(solve(cor(X[, idx, drop = FALSE])))
    } else if (length(idx) == 1) vif[idx] <- 1
    # R² of each column on others, kontrol için alternatif VIF
    for (j in idx) {
      sifir[j] <- cor(X[, j], y)
      diger <- setdiff(which(ok), j)
      ex <- resid(lm.fit(X[, diger, drop = FALSE], X[, j]))
      ey <- resid(lm.fit(X[, diger, drop = FALSE], y))
      kismi[j] <- cor(ex, ey)
      yari[j] <- cor(y, ex)
    }
  }
  h <- hatvalues(fit)
  fs <- s$fstatistic
  list(
    n = nrow(X),
    ad = names(cf), B = unname(cf), SH = unname(tab[, 2]), t = unname(tab[, 3]), p = unname(tab[, 4]),
    gaAlt = ci[, 1], gaUst = ci[, 2], beta = beta, VIF = vif,
    sifirDerece = sifir, kismi = kismi, yariKismi = yari,
    R2 = s$r.squared, duzR2 = s$adj.r.squared, sigma = s$sigma,
    F = if (is.null(fs)) NA else unname(fs[1]), sd1 = if (is.null(fs)) NA else unname(fs[2]), sd2 = fit$df.residual,
    pF = if (is.null(fs)) NA else unname(pf(fs[1], fs[2], fs[3], lower.tail = FALSE)),
    AIC = AIC(fit), BIC = BIC(fit), logLik = as.numeric(logLik(fit)),
    anovaKT = c(if (sabitVar) sum((fitted(fit) - mean(fitted(fit)))^2) else sum(fitted(fit)^2), sum(resid(fit)^2)),
    artik = unname(resid(fit)), uyum = unname(fitted(fit)),
    rstandard = unname(rstandard(fit)), rstudent = unname(rstudent(fit)),
    hat = unname(h), cook = unname(cooks.distance(fit)), dffits = unname(dffits(fit)),
    dw = unname(lmtest::dwtest(fit)$statistic),
    bpK = unname(lmtest::bptest(fit)$statistic), bpKp = lmtest::bptest(fit)$p.value, bpSd = unname(lmtest::bptest(fit)$parameter),
    bpB = unname(lmtest::bptest(fit, studentize = FALSE)$statistic), bpBp = lmtest::bptest(fit, studentize = FALSE)$p.value
  )
}

drop1Ozet <- function(fit) {
  d <- drop1(fit, test = "F")
  list(terim = rownames(d)[-1], sd = d$Df[-1], KT = d[["Sum of Sq"]][-1], F = d[["F value"]][-1], p = d[["Pr(>F)"]][-1])
}

# ---------- 1. mtcars: sürekli + 3 düzeyli faktör ----------
mt <- mtcars
mt$cyl <- factor(mt$cyl)
f1 <- lm(mpg ~ wt + hp + qsec + cyl + am, data = mt)
gv <- car::vif(f1)
mtcars1 <- c(lmOzet(f1), list(drop1 = drop1Ozet(f1), gvif = unname(gv[, 1]), gvifdf = unname(gv[, 3]), gvifTerim = rownames(gv)))

# referans = 8, düzey sırası verilmiş
mt2 <- mt
mt2$cyl <- relevel(mt$cyl, ref = "8")
f1b <- lm(mpg ~ wt + cyl, data = mt2)
mtcarsRef8 <- lmOzet(f1b)

# ---------- 2. swiss, stackloss, trees, cars ----------
f2 <- lm(Fertility ~ ., data = swiss)
swissSonuc <- c(lmOzet(f2), list(drop1 = drop1Ozet(f2), gvif = unname(car::vif(f2))))
f3 <- lm(stack.loss ~ ., data = stackloss)
stacklossSonuc <- lmOzet(f3)
tr <- data.frame(lv = log(trees$Volume), lg = log(trees$Girth), lh = log(trees$Height))
f4 <- lm(lv ~ lg + lh, data = tr)
treesSonuc <- lmOzet(f4)
f5 <- lm(dist ~ speed, data = cars)
carsSonuc <- lmOzet(f5)

# ---------- 3. ToothGrowth: etkileşimler ----------
tg <- ToothGrowth
tg$dosef <- factor(tg$dose)
f6 <- lm(len ~ supp * dosef, data = tg)
a3 <- car::Anova(f6, type = 3)
tgKat <- c(lmOzet(f6), list(anova3 = list(terim = rownames(a3), KT = a3[["Sum Sq"]], sd = a3$Df, F = a3[["F value"]], p = a3[["Pr(>F)"]])))
f7 <- lm(len ~ dose * supp, data = tg)
a3b <- car::Anova(f7, type = 3)
tgSur <- c(lmOzet(f7), list(anova3 = list(terim = rownames(a3b), KT = a3b[["Sum Sq"]], sd = a3b$Df, F = a3b[["F value"]], p = a3b[["Pr(>F)"]])))

# ---------- 4. Sentetik: 4 düzeyli faktör (referans C), bağımlı sütun, eksik değerler ----------
set.seed(2024)
n <- 60
s <- data.frame(
  x1 = round(rnorm(n, 10, 3), 2),
  x2 = round(runif(n, 0, 5), 2),
  g = sample(c("A", "B", "C", "D"), n, replace = TRUE),
  stringsAsFactors = FALSE
)
s$x3 <- s$x1 + 2 * s$x2                     # tam doğrusal bağımlı
s$y <- round(5 + 0.8 * s$x1 - 1.5 * s$x2 + c(A = 0, B = 2, C = -1, D = 3)[s$g] + rnorm(n, 0, 2), 2)
s$y[c(3, 17)] <- NA
s$x1[c(8, 40)] <- NA
s$g[c(25)] <- NA
s$x2[51] <- NA
sf <- s
sf$g <- relevel(factor(sf$g), ref = "C")
f8 <- lm(y ~ x1 + x2 + x3 + g, data = sf)
sentetik <- c(lmOzet(f8), list(veri = sutunlar(s), kullanilan = as.integer(rownames(model.frame(f8))) - 1L))
# x3 olmadan + drop1
f8b <- lm(y ~ x1 + x2 + g, data = sf)
sentetik2 <- c(lmOzet(f8b), list(drop1 = drop1Ozet(f8b)))

# sabitsiz model
f9 <- lm(y ~ 0 + x1 + x2, data = sf)
sabitsiz <- lmOzet(f9)

# boş düzey: D düzeyindeki y'ler eksik yapılırsa düzey kalkmalı
s2 <- s
s2$y[s2$g %in% "D"] <- NA
f10 <- lm(y ~ x1 + factor(g), data = s2)
bosDuzey <- c(lmOzet(f10), list(veri = sutunlar(s2)))

# ---------- 5. Hiyerarşik ----------
h1 <- lm(mpg ~ wt, data = mtcars)
h2 <- lm(mpg ~ wt + hp + qsec, data = mtcars)
h3 <- lm(mpg ~ wt + hp + qsec + wt:hp, data = mtcars)
# SPSS F değişimi: her adımda o modelin artık KO paydası (anova(m1, m2, m3) en büyük modelinkini kullanır)
av <- rbind(anova(h1, h2)[2, ], anova(h2, h3)[2, ])
hiyerarsik <- list(
  R2 = sapply(list(h1, h2, h3), function(f) summary(f)$r.squared),
  duzR2 = sapply(list(h1, h2, h3), function(f) summary(f)$adj.r.squared),
  FDeg = c(unname(summary(h1)$fstatistic[1]), av$F), pDeg = c(unname(pf(summary(h1)$fstatistic[1], 1, 30, lower.tail = FALSE)), av[["Pr(>F)"]]),
  sd1 = c(1, av$Df), sd2 = c(h1$df.residual, av$Res.Df),
  m3 = lmOzet(h3)
)

# ---------- 6. Adımsal: SPSS kuralının R benzetimi (add1/drop1 kısmi F) ----------
adimsal <- function(data, yad, adaylar, yontem, pin = 0.05, pout = 0.10) {
  cur <- if (yontem == "geri") adaylar else character(0)
  fitf <- function(v) lm(reformulate(if (length(v)) v else "1", yad), data = data)
  log <- list()
  for (adim in 1:(2 * length(adaylar) + 2)) {
    degisti <- FALSE
    if (yontem %in% c("geri", "adimsal") && length(cur)) {
      d <- drop1(fitf(cur), test = "F")
      pv <- setNames(d[["Pr(>F)"]][-1], rownames(d)[-1])
      if (max(pv) > pout) {
        cik <- names(which.max(pv))
        cur <- setdiff(cur, cik)
        log[[length(log) + 1]] <- list(islem = "cikis", terim = cik, F = d[cik, "F value"], p = unname(pv[cik]), R2 = summary(fitf(cur))$r.squared)
        degisti <- TRUE
        if (yontem == "adimsal") next
      }
    }
    if (yontem == "geri") { if (!degisti) break; next }
    kalan <- setdiff(adaylar, cur)
    if (length(kalan)) {
      a <- add1(fitf(cur), scope = reformulate(adaylar, yad), test = "F")
      pv <- setNames(a[["Pr(>F)"]][-1], rownames(a)[-1])
      if (min(pv) < pin) {
        gir <- names(which.min(pv))
        cur <- c(cur, gir)
        log[[length(log) + 1]] <- list(islem = "giris", terim = gir, F = a[gir, "F value"], p = unname(pv[gir]), R2 = summary(fitf(cur))$r.squared)
        degisti <- TRUE
      }
    }
    if (!degisti) break
  }
  son <- fitf(cur)
  list(adimlar = log, secilen = cur, son = if (length(cur)) lmOzet(son) else NULL)
}
hald <- data.frame(
  x1 = c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10),
  x2 = c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68),
  x3 = c(6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8),
  x4 = c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12),
  y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1, 115.9, 83.8, 113.3, 109.4)
)
sw <- swiss
mtc <- mtcars
mtc$cyl <- factor(mtc$cyl)
mtc$gear <- factor(mtc$gear)
adimsalSonuc <- list(
  swissIleri = adimsal(sw, "Fertility", setdiff(names(sw), "Fertility"), "ileri"),
  swissGeri = adimsal(sw, "Fertility", setdiff(names(sw), "Fertility"), "geri"),
  swissAdimsal = adimsal(sw, "Fertility", setdiff(names(sw), "Fertility"), "adimsal"),
  mtIleri = adimsal(mtc, "mpg", setdiff(names(mtc), "mpg"), "ileri"),
  mtGeri = adimsal(mtc, "mpg", setdiff(names(mtc), "mpg"), "geri"),
  mtAdimsal = adimsal(mtc, "mpg", setdiff(names(mtc), "mpg"), "adimsal"),
  mtAdimsalGevsek = adimsal(mtc, "mpg", setdiff(names(mtc), "mpg"), "adimsal", 0.2, 0.25),
  # Hald çimento verisi: x4, x1, x2 girer, sonra x4 çıkar (PIN 0.10, POUT 0.15)
  haldAdimsal = adimsal(hald, "y", c("x1", "x2", "x3", "x4"), "adimsal", 0.10, 0.15),
  haldGeri = adimsal(hald, "y", c("x1", "x2", "x3", "x4"), "geri", 0.10, 0.15)
)

sonuc <- list(
  veri = list(mtcars = sutunlar(mtcars), swiss = sutunlar(swiss), stackloss = sutunlar(stackloss), tr = sutunlar(tr), cars = sutunlar(cars), tg = sutunlar(ToothGrowth), hald = sutunlar(hald)),
  mtcars1 = mtcars1, mtcarsRef8 = mtcarsRef8, swiss = swissSonuc, stackloss = stacklossSonuc, trees = treesSonuc, cars = carsSonuc,
  tgKat = tgKat, tgSur = tgSur, sentetik = sentetik, sentetik2 = sentetik2, sabitsiz = sabitsiz, bosDuzey = bosDuzey,
  hiyerarsik = hiyerarsik, adimsal = adimsalSonuc
)
