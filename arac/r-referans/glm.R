# paketler: car, emmeans
# GLM (faktöriyel ANOVA / ANCOVA) referansları.
# Tip III: car::Anova(type = 3) toplamı sıfır kodlamayla; Tip II: car::Anova(type = 2) ve SPSS kapsama kuralına göre
# elle model karşılaştırması; Tip I: anova(); Levene: car::leveneTest(center = mean); EMM: emmeans.
options(contrasts = c("contr.sum", "contr.poly"))
suppressPackageStartupMessages({
  library(car)
  library(emmeans)
})

tablo <- function(a) {
  data.frame(
    kaynak = rownames(a), KT = a[["Sum Sq"]], sd = a[["Df"]],
    F = if (is.null(a[["F value"]])) NA else a[["F value"]],
    p = if (is.null(a[["Pr(>F)"]])) NA else a[["Pr(>F)"]],
    stringsAsFactors = FALSE
  )
}

# SPSS Tip II: her etki, onu içermeyen tüm etkilere göre düzeltilir.
# Kapsama: aynı kovaryat kümesi, daha çok faktör, tüm faktörler ortak; kesişim yalnız saf faktör etkilerinin içinde.
spssTip2 <- function(fit, kov) {
  X <- model.matrix(fit)
  asg <- attr(X, "assign")
  tl <- attr(terms(fit), "term.labels")
  y <- model.response(model.frame(fit))
  fac <- attr(terms(fit), "factors")
  bilgi <- lapply(seq_along(tl), function(t) {
    v <- rownames(fac)[fac[, t] > 0]
    list(f = sort(setdiff(v, kov)), c = sort(intersect(v, kov)))
  })
  hepsi <- c(list("kesisim"), bilgi)
  idx <- c(0, seq_along(tl))
  icerir <- function(b, k) {
    if (identical(k, "kesisim")) return(!identical(b, "kesisim") && length(b$c) == 0)
    if (identical(b, "kesisim")) return(FALSE)
    if (!identical(b$c, k$c)) return(FALSE)
    length(b$f) > length(k$f) && all(k$f %in% b$f)
  }
  rss <- function(cols) if (length(cols) == 0) sum(y^2) else sum(lm.fit(X[, cols, drop = FALSE], y)$residuals^2)
  ss <- sapply(seq_along(hepsi), function(h) {
    diger <- which(sapply(seq_along(hepsi), function(o) o != h && !icerir(hepsi[[o]], hepsi[[h]])))
    c0 <- which(asg %in% idx[diger])
    c1 <- c(c0, which(asg == idx[h]))
    rss(c0) - rss(c1)
  })
  data.frame(kaynak = c("(Intercept)", tl), KT = ss, stringsAsFactors = FALSE)
}

emmTablo <- function(e) {
  s <- as.data.frame(summary(e))
  for (k in names(s)) if (is.factor(s[[k]])) s[[k]] <- as.character(s[[k]])
  s
}

ikiliTablo <- function(e, adj) {
  s <- as.data.frame(summary(pairs(e, adjust = adj), infer = c(TRUE, TRUE)))
  for (k in names(s)) if (is.factor(s[[k]])) s[[k]] <- as.character(s[[k]])
  s
}

# Basit etki F'si: emmeans'ın kendi katsayı vektörleri (linfct) ve vcov ile elle
basitF <- function(fit, A, B) {
  e <- emmeans(fit, as.formula(paste("~", A, "|", B)))
  gr <- e@grid
  L <- e@linfct
  b <- coef(fit)
  V <- vcov(fit)
  lapply(levels(gr[[B]]), function(lv) {
    rows <- which(gr[[B]] == lv)
    k <- length(rows)
    C <- L[rows[-k], , drop = FALSE] - matrix(L[rows[k], ], k - 1, ncol(L), byrow = TRUE)
    est <- C %*% b
    F <- as.numeric(t(est) %*% solve(C %*% V %*% t(C), est)) / (k - 1)
    list(etken = A, kosulFaktor = B, kosul = lv, F = F, sd1 = k - 1, sd2 = df.residual(fit),
         p = pf(F, k - 1, df.residual(fit), lower.tail = FALSE),
         ikili = ikiliTablo(e[rows], "bonferroni"))
  })
}

vaka <- function(d, yad, faktorler, kovaryatlar = character(), formul, etkilesimler = "tam", egimEk = NULL) {
  tum <- c(yad, faktorler, kovaryatlar)
  dd <- d[complete.cases(d[tum]), ]
  for (f in faktorler) dd[[f]] <- droplevels(dd[[f]])
  fit <- lm(formul, data = dd)
  y <- dd[[yad]]
  n <- length(y)
  ssm <- sum((fitted(fit) - mean(y))^2)
  dfm <- fit$rank - 1
  mse <- sum(resid(fit)^2) / df.residual(fit)
  hucre <- interaction(dd[faktorler], drop = TRUE)
  if (length(kovaryatlar) == 0) {
    lv <- leveneTest(y ~ hucre, center = mean)
    levene <- list(F = lv[1, "F value"], sd1 = lv[1, "Df"], sd2 = lv[2, "Df"], p = lv[1, "Pr(>F)"])
  } else {
    X <- cbind(model.matrix(~ hucre - 1), as.matrix(dd[kovaryatlar]))
    z <- abs(lm.fit(X, y)$residuals)
    a <- anova(lm(z ~ hucre))
    levene <- list(F = a[1, "F value"], sd1 = a[1, "Df"], sd2 = a[2, "Df"], p = a[1, "Pr(>F)"])
  }
  emm <- list()
  ikili <- list()
  for (f in faktorler) {
    e <- emmeans(fit, f)
    emm[[f]] <- emmTablo(e)
    ikili[[f]] <- list(bonferroni = ikiliTablo(e, "bonferroni"), sidak = ikiliTablo(e, "sidak"), yok = ikiliTablo(e, "none"))
  }
  tl <- attr(terms(fit), "term.labels")
  fac <- attr(terms(fit), "factors")
  basit <- list()
  for (t in seq_along(tl)) {
    v <- rownames(fac)[fac[, t] > 0]
    if (length(v) >= 2 && all(v %in% faktorler)) {
      e <- emmeans(fit, v)
      emm[[paste(v, collapse = ":")]] <- emmTablo(e)
      if (length(v) == 2) {
        basit <- c(basit, basitF(fit, v[1], v[2]), basitF(fit, v[2], v[1]))
      }
    }
  }
  joint <- list()
  if (length(faktorler) >= 2) {
    for (f in faktorler) {
      jt <- as.data.frame(joint_tests(fit, by = f))
      for (k in names(jt)) if (is.factor(jt[[k]])) jt[[k]] <- as.character(jt[[k]])
      joint[[f]] <- jt
    }
  }
  betim <- aggregate(dd[[yad]], dd[faktorler], function(x) c(n = length(x), ortalama = mean(x), ss = if (length(x) > 1) sd(x) else NA))
  betim <- data.frame(lapply(betim[faktorler], as.character), n = betim$x[, "n"], ortalama = betim$x[, "ortalama"], ss = betim$x[, "ss"], stringsAsFactors = FALSE)
  katsayi <- list()
  for (k in kovaryatlar) {
    if (sum(fac[k, ]) == 1) {
      cs <- coef(summary(fit))[k, ]
      ci <- confint(fit)[k, ]
      katsayi[[k]] <- list(B = cs[["Estimate"]], SH = cs[["Std. Error"]], t = cs[["t value"]], p = cs[["Pr(>|t|)"]], gaAlt = ci[[1]], gaUst = ci[[2]])
    }
  }
  egim <- NULL
  if (!is.null(egimEk)) {
    f1 <- update(formul, as.formula(paste(". ~ . +", paste(egimEk, collapse = " + "))))
    fit1 <- lm(f1, data = dd)
    ac <- anova(fit, fit1)
    egim <- list(tablo = tablo(Anova(fit1, type = 3)),
                 toplu = list(KT = ac[["Sum of Sq"]][2], sd = ac$Df[2], F = ac$F[2], hataSd = ac$Res.Df[2], p = ac[["Pr(>F)"]][2]))
  }
  kovListe <- if (length(kovaryatlar)) lapply(d[kovaryatlar], as.numeric) else setNames(list(), character(0))
  list(
    veri = list(y = d[[yad]], faktorler = lapply(d[faktorler], as.character), kovaryatlar = kovListe),
    duzeyler = lapply(dd[faktorler], levels),
    etkilesimler = etkilesimler,
    n = n,
    tip3 = tablo(Anova(fit, type = 3)),
    tip2 = tablo(Anova(fit, type = 2)),
    tip1 = tablo(anova(fit)),
    spss2 = spssTip2(fit, kovaryatlar),
    kesisimTip1 = n * mean(y)^2,
    model = list(KT = ssm, sd = dfm, F = (ssm / dfm) / mse, p = pf((ssm / dfm) / mse, dfm, df.residual(fit), lower.tail = FALSE)),
    hata = list(KT = sum(resid(fit)^2), sd = df.residual(fit)),
    toplam = list(KT = sum(y^2), sd = n),
    duzeltilmisToplam = list(KT = sum((y - mean(y))^2), sd = n - 1),
    R2 = summary(fit)$r.squared, R2d = summary(fit)$adj.r.squared,
    levene = levene,
    emm = emm, ikili = ikili, basit = basit, joint = joint,
    betimsel = betim, katsayi = katsayi, egim = egim,
    kovOrt = if (length(kovaryatlar)) lapply(dd[kovaryatlar], mean) else setNames(list(), character(0))
  )
}

# 1) ToothGrowth: dengeli 2 x 3
tg <- ToothGrowth
tg$dose <- factor(tg$dose)
# 2) warpbreaks: dengeli 2 x 3, düzey sırası L, M, H
wb <- warpbreaks
# 3) mtcars: dengesiz 3 x 2
mt <- mtcars
mt$cyl <- factor(mt$cyl)
mt$am <- factor(mt$am)
# 4) npk'den 4 satır silinerek dengesiz 2 x 2 x 2
np <- npk[-c(1, 6, 11, 17), ]
# 5) CO2: 2 x 2 + kovaryat
co <- as.data.frame(CO2)
co$Type <- factor(as.character(co$Type), levels = c("Quebec", "Mississippi"))
co$Treatment <- factor(as.character(co$Treatment), levels = c("nonchilled", "chilled"))
# 8) PlantGrowth: tek yönlü
pg <- PlantGrowth
# 9) Sentetik: dengesiz 3 x 3, değişen varyans, eksik değerler, kovaryat
set.seed(2026)
nh <- c(4, 7, 5, 8, 3, 6, 5, 9, 4)
A <- rep(rep(c("a1", "a2", "a3"), each = 3), nh)
B <- rep(rep(c("b1", "b2", "b3"), 3), nh)
etkiA <- c(a1 = 0, a2 = 2, a3 = 5)
etkiB <- c(b1 = 0, b2 = -1, b3 = 1.5)
ssA <- c(a1 = 1, a2 = 2.5, a3 = 4)
x <- round(rnorm(length(A), 50, 8), 1)
yy <- round(20 + etkiA[A] + etkiB[B] + ifelse(A == "a3" & B == "b3", 3, 0) + 0.15 * (x - 50) + rnorm(length(A), 0, ssA[A]), 2)
sen <- data.frame(y = unname(yy), A = factor(A), B = factor(B), x = x)
sen$y[c(3, 20, 41)] <- NA
sen$A[12] <- NA
sen$B[30] <- NA
sen$x[15] <- NA

sonuc <- list(
  tooth = vaka(tg, "len", c("supp", "dose"), formul = len ~ supp * dose),
  warp = vaka(wb, "breaks", c("wool", "tension"), formul = breaks ~ wool * tension),
  mtcars = vaka(mt, "mpg", c("cyl", "am"), formul = mpg ~ cyl * am),
  mtcarsAna = vaka(mt, "mpg", c("cyl", "am"), formul = mpg ~ cyl + am, etkilesimler = "yok"),
  npk = vaka(np, "yield", c("N", "P", "K"), formul = yield ~ N * P * K),
  npkOzel = vaka(np, "yield", c("N", "P", "K"), formul = yield ~ N + P + K + N:P + N:K, etkilesimler = list(c("N", "P"), c("N", "K"))),
  co2 = vaka(co, "uptake", c("Type", "Treatment"), "conc", formul = uptake ~ conc + Type * Treatment,
             egimEk = c("Type:conc", "Treatment:conc", "Type:Treatment:conc")),
  ankova = vaka(mt, "mpg", "cyl", c("wt", "hp"), formul = mpg ~ wt + hp + cyl, etkilesimler = "yok",
                egimEk = c("cyl:wt", "cyl:hp")),
  faktorKov = vaka(mt, "mpg", "cyl", "wt", formul = mpg ~ wt + cyl + cyl:wt, etkilesimler = list(c("cyl", "wt"))),
  plant = vaka(pg, "weight", "group", formul = weight ~ group),
  sentetik = vaka(sen, "y", c("A", "B"), formul = y ~ A * B),
  sentetikKov = vaka(sen, "y", c("A", "B"), "x", formul = y ~ x + A * B, egimEk = c("A:x", "B:x", "A:B:x"))
)
