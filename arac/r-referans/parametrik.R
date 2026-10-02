# paketler: car, onewaytests, multcomp, mvtnorm, MBESS, rstatix
# Parametrik testler, varyans homojenliği, post hoc testler ve etki büyüklükleri için referans değerler
# (varyans.js, parametrik.js, posthoc.js, etki.js)
yuv <- function(x) as.numeric(sprintf("%.15g", x))

# ---------- Yardımcılar ----------
ciftler <- function(k) { r <- NULL; for (i in 1:(k - 1)) for (j in (i + 1):k) r <- rbind(r, c(i, j)); r }

# Merkezi olmayan t: P(T <= t; d) = hedef (pt ncp'ye göre azalan)
ncpT <- function(t, df, conf = 0.95) {
  a <- (1 - conf) / 2
  f <- function(d, hedef) pt(t, df, d) - hedef
  ara <- c(t - 25, t + 25)
  c(uniroot(f, ara, hedef = 1 - a, tol = 1e-14, maxiter = 5000)$root,
    uniroot(f, ara, hedef = a, tol = 1e-14, maxiter = 5000)$root)
}
ncpF <- function(F, d1, d2, conf = 0.95) {
  a <- (1 - conf) / 2
  f <- function(l, hedef) pf(F, d1, d2, l) - hedef
  p0 <- pf(F, d1, d2)
  ust <- 10 * F * d1 + 200
  lo <- if (p0 <= 1 - a) 0 else uniroot(f, c(0, ust), hedef = 1 - a, tol = 1e-13, maxiter = 5000)$root
  hi <- if (p0 <= a) 0 else uniroot(f, c(0, ust), hedef = a, tol = 1e-13, maxiter = 5000)$root
  c(lo, hi)
}
J <- function(df) exp(lgamma(df / 2) - log(sqrt(df / 2)) - lgamma((df - 1) / 2))

htest <- function(s) list(t = unname(s$statistic), df = unname(s$parameter), p = s$p.value,
                          ga = unname(as.numeric(s$conf.int)), tahmin = unname(s$estimate))

# ---------- t testleri ----------
bagimsiz <- function(x, y, conf = 0.95) {
  r <- list(x = x, y = y)
  for (alt in c("two.sided", "greater", "less")) {
    r[[paste0("student_", alt)]] <- htest(t.test(x, y, var.equal = TRUE, alternative = alt, conf.level = conf))
    r[[paste0("welch_", alt)]] <- htest(t.test(x, y, var.equal = FALSE, alternative = alt, conf.level = conf))
  }
  n1 <- length(x); n2 <- length(y)
  sp <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  d <- (mean(x) - mean(y)) / sp
  tS <- d / sqrt(1 / n1 + 1 / n2)
  ci <- ncpT(tS, n1 + n2 - 2, conf) * sqrt(1 / n1 + 1 / n2)
  mb <- MBESS::ci.smd(smd = d, n.1 = n1, n.2 = n2, conf.level = conf)
  r$etki <- list(d = d, dGA = ci, J = J(n1 + n2 - 2), g = d * J(n1 + n2 - 2), gGA = ci * J(n1 + n2 - 2),
                 glass = (mean(x) - mean(y)) / sd(y), sp = sp,
                 mbess = c(mb$Lower.Conf.Limit.smd, mb$Upper.Conf.Limit.smd))
  r
}
eslesik <- function(x, y, conf = 0.95) {
  r <- list(x = x, y = y)
  for (alt in c("two.sided", "greater", "less"))
    r[[alt]] <- htest(t.test(x, y, paired = TRUE, alternative = alt, conf.level = conf))
  d <- x - y; n <- length(d)
  dz <- mean(d) / sd(d)
  ci <- ncpT(dz * sqrt(n), n - 1, conf) / sqrt(n)
  ct <- cor.test(x, y)
  r$etki <- list(dz = dz, dGA = ci, g = dz * J(n - 1), gGA = ci * J(n - 1), sdFark = sd(d), seFark = sd(d) / sqrt(n))
  r$kor <- list(r = unname(ct$estimate), p = ct$p.value)
  r
}
tek <- function(x, mu, conf = 0.95) {
  r <- list(x = x, mu = mu)
  for (alt in c("two.sided", "greater", "less"))
    r[[alt]] <- htest(t.test(x, mu = mu, alternative = alt, conf.level = conf))
  n <- length(x)
  d <- (mean(x) - mu) / sd(x)
  ci <- ncpT(d * sqrt(n), n - 1, conf) / sqrt(n)
  r$etki <- list(d = d, dGA = ci, g = d * J(n - 1), gGA = ci * J(n - 1))
  r
}

set.seed(7)
ttestler <- list(
  sleep = bagimsiz(sleep$extra[1:10], sleep$extra[11:20]),
  tooth = bagimsiz(ToothGrowth$len[ToothGrowth$supp == "OJ"], ToothGrowth$len[ToothGrowth$supp == "VC"]),
  mtcars_am = bagimsiz(mtcars$mpg[mtcars$am == 0], mtcars$mpg[mtcars$am == 1], conf = 0.90),
  kucuk = bagimsiz(c(1.2, 3.4, 2.2), c(5.1, 4.4)),
  esitsiz = bagimsiz(yuv(round(rnorm(8, 10, 1), 3)), yuv(round(rnorm(25, 11, 4), 3)))
)
eslesikler <- list(
  sleep = eslesik(sleep$extra[1:10], sleep$extra[11:20]),
  iris = eslesik(iris$Sepal.Length[1:50], iris$Petal.Length[1:50], conf = 0.99),
  kucuk = eslesik(c(10, 12, 9), c(11, 15, 9.5))
)
tekler <- list(
  sleep = tek(sleep$extra[1:10], 0),
  mtcars = tek(mtcars$mpg, 20),
  kucuk = tek(c(2.5, 3.1), 1)
)

# ---------- Gruplu veri setleri ----------
gruplu <- list(
  plant = split(PlantGrowth$weight, PlantGrowth$group),
  insect = split(InsectSprays$count, InsectSprays$spray),
  chick = split(chickwts$weight, chickwts$feed),
  warp = split(warpbreaks$breaks, warpbreaks$tension),
  tooth = split(ToothGrowth$len, ToothGrowth$dose),
  iris = split(iris$Sepal.Width, iris$Species),
  mtcars = split(mtcars$mpg, mtcars$cyl),
  kucuk = list(c(3, 5, 4), c(8, 9), c(1, 2, 2, 6)),
  ikigrup = list(sleep$extra[1:10], sleep$extra[11:20]),
  hetero = list(yuv(round(rnorm(5, 10, 1), 2)), yuv(round(rnorm(12, 12, 5), 2)), yuv(round(rnorm(30, 9, 2), 2)), yuv(round(rnorm(7, 15, 8), 2)))
)

grupHesapla <- function(gr, conf = 0.95) {
  k <- length(gr); n <- lengths(gr); N <- sum(n)
  y <- unlist(gr); g <- factor(rep(seq_len(k), n))
  m <- sapply(gr, mean); v <- sapply(gr, var)
  r <- list(gruplar = unname(lapply(gr, function(x) x)))
  # varyans homojenliği
  lm1 <- car::leveneTest(y ~ g, center = mean)
  lm2 <- car::leveneTest(y ~ g, center = median)
  r$levene <- list(F = lm1[1, "F value"], df = c(lm1[1, "Df"], lm1[2, "Df"]), p = lm1[1, "Pr(>F)"])
  r$leveneMedyan <- list(F = lm2[1, "F value"], p = lm2[1, "Pr(>F)"])
  if (all(n >= 2)) { b <- bartlett.test(gr); r$bartlett <- list(K2 = unname(b$statistic), df = unname(b$parameter), p = b$p.value) }
  fk <- fligner.test(gr); r$fligner <- list(X2 = unname(fk$statistic), p = fk$p.value)
  # ANOVA
  fit <- aov(y ~ g)
  tab <- summary(fit)[[1]]
  SSb <- tab[1, "Sum Sq"]; SSw <- tab[2, "Sum Sq"]; dfb <- tab[1, "Df"]; dfw <- tab[2, "Df"]
  MSw <- SSw / dfw; F <- tab[1, "F value"]
  lam <- ncpF(F, dfb, dfw, conf)
  r$anova <- list(SSb = SSb, SSw = SSw, dfb = dfb, dfw = dfw, MSb = tab[1, "Mean Sq"], MSw = MSw, F = F, p = tab[1, "Pr(>F)"],
                  eta2 = SSb / (SSb + SSw), eta2GA = lam / (lam + N), lambdaGA = lam,
                  omega2 = (SSb - dfb * MSw) / (SSb + SSw + MSw), epsilon2 = (SSb - dfb * MSw) / (SSb + SSw),
                  ort = unname(m), ss = unname(sqrt(v)),
                  gaAlt = unname(sapply(gr, function(x) if (length(x) > 1) t.test(x, conf.level = conf)$conf.int[1] else NA)),
                  gaUst = unname(sapply(gr, function(x) if (length(x) > 1) t.test(x, conf.level = conf)$conf.int[2] else NA)))
  if (all(n >= 2)) {
    w <- oneway.test(y ~ g, var.equal = FALSE)
    r$welch <- list(F = unname(w$statistic), df = unname(w$parameter), p = w$p.value)
    bf <- onewaytests::bf.test(y ~ g, data = data.frame(y = y, g = g), verbose = FALSE)
    # elle Brown-Forsythe (1974)
    ci <- (1 - n / N) * v; cc <- ci / sum(ci)
    Fm <- sum(n * (m - mean(y))^2) / sum(ci)
    df2 <- 1 / sum(cc^2 / (n - 1))
    r$bf <- list(F = Fm, df = c(k - 1, df2), p = pf(Fm, k - 1, df2, lower.tail = FALSE),
                 paketF = unname(bf$statistic), paketDf = unname(c(bf$parameter)), paketP = bf$p.value)
  }
  # post hoc
  cf <- ciftler(k)
  tk <- TukeyHSD(fit, conf.level = conf)$g
  # TukeyHSD: satır "j-i" = m[j] - m[i]; bizim fark m[i] - m[j]
  r$tukey <- list(fark = -unname(tk[, "diff"]), alt = -unname(tk[, "upr"]), ust = -unname(tk[, "lwr"]), p = unname(tk[, "p adj"]))
  se <- apply(cf, 1, function(ij) sqrt(MSw * (1 / n[ij[1]] + 1 / n[ij[2]])))
  fark <- apply(cf, 1, function(ij) m[ij[1]] - m[ij[2]])
  mm <- nrow(cf)
  pt1 <- pairwise.t.test(y, g, pool.sd = TRUE, p.adjust.method = "none")$p.value
  pt2 <- pairwise.t.test(y, g, pool.sd = TRUE, p.adjust.method = "bonferroni")$p.value
  al <- function(P) apply(cf, 1, function(ij) P[ij[2] - 1, ij[1]])
  a <- 1 - conf
  r$lsd <- list(fark = unname(fark), se = unname(se), t = unname(fark / se), p = al(pt1),
                alt = unname(fark - qt(1 - a / 2, dfw) * se), ust = unname(fark + qt(1 - a / 2, dfw) * se))
  r$bonferroni <- list(p = al(pt2), alt = unname(fark - qt(1 - a / (2 * mm), dfw) * se), ust = unname(fark + qt(1 - a / (2 * mm), dfw) * se))
  Fs <- (fark / se)^2 / (k - 1)
  ks <- sqrt((k - 1) * qf(conf, k - 1, dfw))
  r$scheffe <- list(F = unname(Fs), p = unname(pf(Fs, k - 1, dfw, lower.tail = FALSE)), alt = unname(fark - ks * se), ust = unname(fark + ks * se))
  if (all(n >= 2)) {
    gh <- t(apply(cf, 1, function(ij) {
      i <- ij[1]; j <- ij[2]
      f <- m[i] - m[j]; s <- sqrt(v[i] / n[i] + v[j] / n[j])
      df <- (v[i] / n[i] + v[j] / n[j])^2 / ((v[i] / n[i])^2 / (n[i] - 1) + (v[j] / n[j])^2 / (n[j] - 1))
      q <- abs(f) / (s / sqrt(2))
      w <- qtukey(conf, k, df) / sqrt(2) * s
      tt <- f / s; praw <- 2 * pt(-abs(tt), df)
      ws <- qt(1 - (-expm1(log(conf) / mm)) / 2, df) * s
      c(f, s, df, q, ptukey(q, k, df, lower.tail = FALSE), f - w, f + w, tt, -expm1(mm * log1p(-praw)), f - ws, f + ws)
    }))
    r$gh <- list(fark = unname(gh[, 1]), se = unname(gh[, 2]), df = unname(gh[, 3]), q = unname(gh[, 4]), p = unname(gh[, 5]),
                 alt = unname(gh[, 6]), ust = unname(gh[, 7]))
    rs <- rstatix::games_howell_test(data.frame(y = y, g = g), y ~ g, conf.level = conf, detailed = TRUE)
    r$ghRstatix <- list(statistic = rs$statistic, df = rs$df, est = rs$estimate, low = rs$conf.low, high = rs$conf.high)
    r$t2 <- list(t = unname(gh[, 8]), p = unname(gh[, 9]), alt = unname(gh[, 10]), ust = unname(gh[, 11]))
  }
  # Dunnett (kontrol = ilk grup ve son grup). p değerleri sıkı toleransla; eşzamanlı GA'nın kritik değeri
  # multcomp'ta qmvt(ptol = 0.001) kök bulmasıyla sınırlı olduğundan varsayılan ayarla (hız için) alınır.
  # (abseps = 1e-9, maxpts = 2e7 ve sıkı calpha ile betik webR'de ~2.5 saat sürüyordu.)
  r$dunnett <- lapply(c(1, k), function(kont) {
    gg <- relevel(g, ref = as.character(kont))
    fit2 <- aov(y ~ gg)
    K <- multcomp::contrMat(table(gg), type = "Dunnett")
    out <- list(kontrol = kont - 1)
    for (alt in c("two.sided", "greater", "less")) {
      gl <- multcomp::glht(fit2, linfct = multcomp::mcp(gg = K), alternative = alt)
      set.seed(1)
      s <- summary(gl, test = multcomp::adjusted("single-step", abseps = 1e-8, maxpts = 2e6))
      set.seed(1)
      ci <- confint(gl, level = conf, calpha = multcomp::adjusted_calpha())
      # glht satır sırası: relevel sonrası düzey sırası (kontrol hariç)
      duz <- levels(gg)[-1]
      out[[alt]] <- list(deney = as.integer(duz) - 1, t = unname(s$test$tstat), p = unname(as.numeric(s$test$pvalues)),
                         hata = attr(s$test$pvalues, "error"), kritik = attr(ci$confint, "calpha"),
                         alt = unname(ci$confint[, "lwr"]), ust = unname(ci$confint[, "upr"]))
    }
    out
  })
  r
}
gruplar <- lapply(gruplu, grupHesapla)

# ---------- Varyans oranı F testi ----------
vt <- function(x, y, ratio = 1, conf = 0.95) {
  r <- list(x = x, y = y, ratio = ratio, conf = conf)
  for (alt in c("two.sided", "greater", "less")) {
    s <- var.test(x, y, ratio = ratio, alternative = alt, conf.level = conf)
    r[[alt]] <- list(F = unname(s$statistic), df = unname(s$parameter), p = s$p.value, ga = as.numeric(s$conf.int), oran = unname(s$estimate))
  }
  r
}
varyansF <- list(
  sleep = vt(sleep$extra[1:10], sleep$extra[11:20]),
  tooth = vt(ToothGrowth$len[1:30], ToothGrowth$len[31:60], ratio = 2, conf = 0.9),
  asiri = vt(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c(5, 5.01, 5.02, 4.99, 5.005))
)

# ---------- Dunnett dağılımı (doğrudan) ----------
dunnettDag <- function(cc, lam, nu, iki = TRUE) {
  R <- outer(lam, lam); diag(R) <- 1
  set.seed(3)
  if (is.infinite(nu)) {
    v <- if (iki) mvtnorm::pmvnorm(lower = rep(-cc, length(lam)), upper = rep(cc, length(lam)), corr = R, algorithm = mvtnorm::GenzBretz(maxpts = 1e7, abseps = 1e-9, releps = 0))
         else mvtnorm::pmvnorm(lower = rep(-Inf, length(lam)), upper = rep(cc, length(lam)), corr = R, algorithm = mvtnorm::GenzBretz(maxpts = 1e7, abseps = 1e-9, releps = 0))
  } else {
    v <- if (iki) mvtnorm::pmvt(lower = rep(-cc, length(lam)), upper = rep(cc, length(lam)), df = nu, corr = R, algorithm = mvtnorm::GenzBretz(maxpts = 1e7, abseps = 1e-9, releps = 0))
         else mvtnorm::pmvt(lower = rep(-Inf, length(lam)), upper = rep(cc, length(lam)), df = nu, corr = R, algorithm = mvtnorm::GenzBretz(maxpts = 1e7, abseps = 1e-9, releps = 0))
  }
  list(c = cc, lam = lam, nu = nu, iki = iki, p = as.numeric(v), hata = attr(v, "error"))
}
dunnettDagilim <- list(
  dunnettDag(2.5, rep(sqrt(0.5), 3), 20),
  dunnettDag(2.2, sqrt(c(5, 10, 20, 40) / (c(5, 10, 20, 40) + 10)), 75),
  dunnettDag(1.9, sqrt(c(3, 30) / (c(3, 30) + 4)), 7),
  dunnettDag(2.7, sqrt(c(12, 12, 12, 12, 12) / 24), 66, iki = FALSE),
  dunnettDag(2.0, sqrt(c(100, 2, 50) / (c(100, 2, 50) + 3)), 150),
  dunnettDag(2.4, rep(sqrt(0.5), 4), Inf),
  dunnettDag(-0.5, sqrt(c(4, 9) / c(10, 15)), 12, iki = FALSE)
)

# ---------- Etki büyüklüğü yardımcıları (doğrudan) ----------
etkiNcp <- list(
  t = lapply(list(c(2.1, 18), c(-3.7, 9), c(0.4, 58), c(8.2, 30), c(1.5, 3)), function(a) list(t = a[1], df = a[2], ga = ncpT(a[1], a[2]), ga90 = ncpT(a[1], a[2], 0.90))),
  f = lapply(list(c(4.85, 2, 27), c(0.3, 3, 40), c(15.2, 5, 66), c(2.1, 1, 10), c(1.2, 4, 200)), function(a) list(F = a[1], df1 = a[2], df2 = a[3], ga = ncpF(a[1], a[2], a[3]), ga90 = ncpF(a[1], a[2], a[3], 0.90))),
  J = data.frame(df = c(2, 3, 5, 10, 38, 100, 1000, 1e5), J = J(c(2, 3, 5, 10, 38, 100, 1000, 1e5)))
)

sonuc <- list(ttestler = ttestler, eslesikler = eslesikler, tekler = tekler, gruplar = gruplar,
              varyansF = varyansF, dunnettDagilim = dunnettDagilim, etkiNcp = etkiNcp)
