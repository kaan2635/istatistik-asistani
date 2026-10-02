# paketler: coin, PropCIs, rstatix, epitools
# Kategorik veri testleri için referans değerler (kategorik.js, fisher.js)
suppressMessages(library(coin))

mat <- function(x) matrix(as.numeric(x), nrow = nrow(x))

# ---------- Ki-kare ----------
kk <- function(t) {
  t <- mat(t)
  a <- suppressWarnings(chisq.test(t, correct = FALSE))
  r <- nrow(t); c <- ncol(t); N <- sum(t)
  E <- a$expected
  G2 <- 2 * sum(ifelse(t > 0, t * log(t / E), 0))
  u <- rep(1:r, times = c); v <- rep(1:c, each = r); w <- as.vector(t)
  um <- sum(w * u) / N; vm <- sum(w * v) / N
  rr <- sum(w * (u - um) * (v - vm)) / sqrt(sum(w * (u - um)^2) * sum(w * (v - vm)^2))
  # coin ile bağımsız doğrusal-doğrusal denetim
  d <- data.frame(a = factor(rep(rep(1:r, times = c), w), levels = 1:r, ordered = TRUE),
                  b = factor(rep(rep(1:c, each = r), w), levels = 1:c, ordered = TRUE))
  lbl <- unname(statistic(coin::lbl_test(a ~ b, data = d)))^2
  X2 <- unname(a$statistic)
  sonuc <- list(tablo = t, pearson = X2, sd = unname(a$parameter), p = a$p.value,
       beklenen = E, standart = unname(a$residuals), duzeltilmis = unname(a$stdres),
       G2 = G2, pG2 = pchisq(G2, (r - 1) * (c - 1), lower.tail = FALSE),
       M2 = (N - 1) * rr^2, pM2 = pchisq((N - 1) * rr^2, 1, lower.tail = FALSE), M2coin = lbl,
       cramerV = sqrt(X2 / (N * (min(r, c) - 1))), C = sqrt(X2 / (X2 + N)),
       kucuk5 = sum(E < 5), minE = min(E))
  if (r == 2 && c == 2) {
    y <- suppressWarnings(chisq.test(t, correct = TRUE))
    f <- fisher.test(t)
    sonuc$yates <- unname(y$statistic); sonuc$pYates <- y$p.value
    sonuc$phi <- (t[1, 1] * t[2, 2] - t[1, 2] * t[2, 1]) / sqrt(prod(rowSums(t)) * prod(colSums(t)))
    sonuc$fisherP <- f$p.value
    sonuc$fisherKucuk <- fisher.test(t, alternative = "less")$p.value
    sonuc$fisherBuyuk <- fisher.test(t, alternative = "greater")$p.value
  } else sonuc$phi <- sqrt(X2 / N)
  sonuc
}
HE <- margin.table(HairEyeColor, c(1, 2))
TC <- margin.table(Titanic, c(1, 4))
UA <- margin.table(UCBAdmissions, c(1, 2))
cg <- table(mtcars$cyl, mtcars$gear)
kkVakalar <- list(kk(HE), kk(TC), kk(UA), kk(cg), kk(matrix(c(12, 5, 7, 9), 2)), kk(matrix(c(3, 0, 5, 8, 2, 6), 2)),
                  kk(matrix(c(10, 2, 3, 15), 2)), kk(matrix(c(20, 15, 30, 25, 10, 12, 8, 14, 9, 11, 16, 5), 3)),
                  kk(margin.table(Titanic, c(2, 4))), kk(table(esoph$agegp, esoph$alcgp)))

# ---------- Fisher kesin testi ----------
# R fisher.test uniroot'u varsayılan toleransla (~1.2e-4) çağırır; aynı tanımları yüksek hassasiyetle yeniden hesapla
fisherHassas <- function(t, conf.level = 0.95) {
  m <- sum(t[, 1]); n <- sum(t[, 2]); k <- sum(t[1, ]); x <- t[1, 1]
  lo <- max(0, k - n); hi <- min(k, m); s <- lo:hi
  logdc <- dhyper(s, m, n, k, log = TRUE)
  dn <- function(u) { d <- logdc + u * s; d <- exp(d - max(d)); d / sum(d) }
  mn <- function(u) sum(s * dn(u))
  pn <- function(q, u, ust = FALSE) sum(dn(u)[if (ust) s >= q else s <= q])
  kok <- function(f) uniroot(f, c(-5, 5), extendInt = "yes", tol = 1e-14, maxiter = 5000)$root
  mle <- if (x == lo) 0 else if (x == hi) Inf else exp(kok(function(u) mn(u) - x))
  a <- (1 - conf.level) / 2
  L <- if (x == lo) 0 else exp(kok(function(u) pn(x, u, TRUE) - a))
  U <- if (x == hi) Inf else exp(kok(function(u) pn(x, u) - a))
  a1 <- 1 - conf.level
  Lg <- if (x == lo) 0 else exp(kok(function(u) pn(x, u, TRUE) - a1))
  Uk <- if (x == hi) Inf else exp(kok(function(u) pn(x, u) - a1))
  list(mle = mle, ga = c(L, U), gaBuyuk = c(Lg, Inf), gaKucuk = c(0, Uk))
}
fs22 <- function(t, conf.level = 0.95) {
  t <- mat(t)
  f <- fisher.test(t, conf.level = conf.level)
  h <- fisherHassas(t, conf.level)
  list(tablo = t, duzey = conf.level, p = f$p.value, pKucuk = fisher.test(t, alternative = "less")$p.value,
       pBuyuk = fisher.test(t, alternative = "greater")$p.value,
       tahminR = unname(f$estimate), gaR = as.numeric(f$conf.int),
       tahmin = h$mle, ga = h$ga, gaBuyuk = h$gaBuyuk, gaKucuk = h$gaKucuk)
}
TeaTasting <- matrix(c(3, 1, 1, 3), 2)
Convictions <- matrix(c(2, 10, 15, 3), 2)
fisher22 <- list(fs22(TeaTasting), fs22(Convictions), fs22(Convictions, 0.9), fs22(matrix(c(0, 5, 7, 3), 2)),
                 fs22(matrix(c(9, 0, 4, 6), 2)), fs22(UA), fs22(matrix(c(1, 30, 22, 15), 2)), fs22(matrix(c(50, 38, 41, 67), 2)),
                 fs22(matrix(c(0, 10, 0, 10), 2) + c(1, 0, 0, 0)), fs22(matrix(c(5, 5, 5, 5), 2)))

Job <- matrix(c(1, 2, 1, 0, 3, 3, 6, 1, 10, 10, 14, 9, 6, 7, 12, 11), 4, 4)
rc <- function(t) { t <- mat(t); list(tablo = t, p = tryCatch(fisher.test(t, workspace = 2e8)$p.value, error = function(e) NULL)) }
fisherRC <- list(
  rc(Job), rc(cg), rc(matrix(c(3, 1, 0, 2, 4, 1, 0, 3, 5), 3)), rc(matrix(c(2, 0, 0, 0, 3, 0, 0, 0, 4), 3)),
  rc(matrix(c(12, 15, 9, 20, 14, 18, 10, 21, 8, 13), 2, byrow = TRUE)),
  rc(matrix(c(3, 20, 5, 1, 12, 2, 8, 30, 4, 6), 2, byrow = TRUE)),
  rc(matrix(c(0, 3, 0, 1, 2, 4, 0, 0, 2, 1, 1, 1, 5, 0, 0, 0, 2, 1, 3, 4), 4, byrow = TRUE)),
  rc(matrix(c(20, 15, 10, 12, 25, 18, 8, 14, 28), 3, byrow = TRUE)),
  rc(matrix(c(5, 2, 1, 3, 1, 6, 2, 0, 0, 2, 7, 1, 1, 0, 2, 5), 4, byrow = TRUE)),
  rc(matrix(c(1, 0, 2, 0, 1, 1, 3, 0, 2, 1, 0, 4), 2, byrow = TRUE)),
  rc(matrix(c(8, 5, 6, 4, 7, 5, 9, 4, 6, 6, 6, 4, 8, 5, 7, 4, 7, 5, 9, 5, 7, 5, 6, 6, 8), 5, byrow = TRUE)),
  rc(matrix(c(2, 1, 0, 1, 3, 0, 1, 2, 2, 1, 0, 1, 1, 3, 2), 3, byrow = TRUE)),
  rc(matrix(c(4, 0, 1, 0, 3, 1, 1, 2, 2), 3, byrow = TRUE)),
  # daha büyük tez tabloları (hız ve doğruluk)
  rc(matrix(c(20, 15, 10, 12, 18, 12, 25, 18, 9, 14, 8, 14, 28, 15, 12), 3, byrow = TRUE)),
  rc(matrix(c(22, 25, 19, 30, 24, 26, 21, 23, 27, 20, 28, 20, 31, 18, 23, 24, 29, 22, 25, 30), 2, byrow = TRUE)),
  rc(matrix(c(40, 30, 20, 24, 50, 36, 16, 28, 56), 3, byrow = TRUE)),
  rc(matrix(c(5, 1, 0, 2, 0, 1, 4, 2, 0, 1, 0, 2, 6, 1, 2, 2, 0, 1, 5, 3, 0, 1, 2, 3, 6), 5, byrow = TRUE)),
  rc(matrix(c(3, 1, 0, 0, 1, 0, 1, 2, 1, 0, 0, 1, 0, 1, 3, 1, 0, 0, 0, 0, 1, 4, 1, 0, 1, 0, 0, 1, 3, 1, 0, 1, 0, 0, 1, 5), 6, byrow = TRUE)),
  rc(matrix(c(24, 16, 13, 19, 15, 23, 18, 21, 19, 13, 26, 16, 16, 21, 15, 25), 4, byrow = TRUE))
)
# Monte Carlo denetimi için büyük tablo (kesin değer yüksek çalışma alanıyla)
mcTablo <- matrix(c(11, 7, 5, 9, 6, 4, 10, 8, 12, 3, 9, 6, 5, 8, 7, 10), 4, byrow = TRUE)
fisherMC <- list(tablo = mcTablo, p = fisher.test(mcTablo, workspace = 2e8)$p.value)

# ---------- McNemar ve Bowker ----------
Performance <- matrix(c(794, 86, 150, 570), 2)
mcn <- function(t) {
  t <- mat(t)
  list(tablo = t, chiDuz = unname(mcnemar.test(t)$statistic), pDuz = mcnemar.test(t)$p.value,
       chi = unname(mcnemar.test(t, correct = FALSE)$statistic), p = mcnemar.test(t, correct = FALSE)$p.value,
       pKesin = binom.test(t[1, 2], t[1, 2] + t[2, 1])$p.value)
}
mcnVakalar <- list(mcn(Performance), mcn(matrix(c(10, 3, 9, 12), 2)), mcn(matrix(c(25, 7, 7, 30), 2)), mcn(matrix(c(5, 1, 6, 8), 2)))
bowk <- function(t) { t <- mat(t); m <- mcnemar.test(t); list(tablo = t, chi = unname(m$statistic), sd = unname(m$parameter), p = m$p.value) }
bowkerVakalar <- list(bowk(matrix(c(20, 5, 3, 10, 30, 4, 2, 8, 25), 3)), bowk(matrix(c(12, 3, 1, 2, 4, 15, 6, 2, 3, 5, 18, 7, 1, 2, 4, 20), 4)))
# boş çiftli tablo: R NaN verir; boş çift atılınca beklenen
bosT <- matrix(c(20, 5, 0, 10, 30, 4, 0, 8, 25), 3)
bowkerBos <- list(tablo = bosT, chi = (5 - 10)^2 / 15 + (4 - 8)^2 / 12, sd = 2, p = pchisq((5 - 10)^2 / 15 + (4 - 8)^2 / 12, 2, lower.tail = FALSE))

# ---------- Cochran Q ----------
set.seed(7)
cq <- function(m) {
  n <- nrow(m); k <- ncol(m)
  d <- data.frame(id = factor(rep(1:n, k)), olcum = factor(rep(1:k, each = n)), y = as.vector(m))
  r <- rstatix::cochran_qtest(d, y ~ olcum | id)
  C <- colSums(m); R <- rowSums(m); T <- sum(m)
  Q <- (k - 1) * (k * sum(C^2) - T^2) / (k * T - sum(R^2))
  list(matris = m, Q = Q, Qr = unname(r$statistic), sd = k - 1, p = pchisq(Q, k - 1, lower.tail = FALSE), pr = r$p)
}
cqVakalar <- list(cq(matrix(rbinom(60, 1, 0.5), 20)), cq(matrix(c(1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0,
                                                                 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1), ncol = 4, byrow = TRUE)))

# ---------- Cochran-Armitage ----------
smokers <- c(83, 90, 129, 70); patients <- c(86, 93, 136, 82)
ca <- function(x, n, s = seq_along(x)) {
  r <- prop.trend.test(x, n, s)
  list(x = x, n = n, skor = s, chi = unname(r$statistic), p = r$p.value)
}
caVakalar <- list(ca(smokers, patients), ca(smokers, patients, c(0, 1, 2, 5)), ca(c(2, 5, 9, 14), c(30, 30, 30, 30)), ca(c(15, 10, 6), c(40, 42, 39)))

# ---------- Mantel-Haenszel ----------
Rabbits <- array(c(0, 0, 6, 5, 3, 0, 3, 6, 6, 2, 0, 4, 5, 6, 1, 0, 2, 5, 0, 0), dim = c(2, 2, 5))
bd <- function(x) {
  K <- dim(x)[3]
  or <- sum(x[1, 1, ] * x[2, 2, ] / apply(x, 3, sum)) / sum(x[1, 2, ] * x[2, 1, ] / apply(x, 3, sum))
  BD <- 0; sE <- 0; sV <- 0; sA <- 0; gecerli <- 0
  for (k in 1:K) {
    t <- x[, , k]; n1 <- sum(t[1, ]); m1 <- sum(t[, 1]); T <- sum(t)
    lo <- max(0, n1 + m1 - T); hi <- min(n1, m1)
    if (lo == hi) next
    gecerli <- gecerli + 1
    g <- function(e) e * (T - n1 - m1 + e) / ((n1 - e) * (m1 - e)) - or
    E <- uniroot(g, c(lo + 1e-12, hi - 1e-12), tol = 1e-14)$root
    V <- 1 / (1 / E + 1 / (n1 - E) + 1 / (m1 - E) + 1 / (T - n1 - m1 + E))
    BD <- BD + (t[1, 1] - E)^2 / V; sE <- sE + E; sV <- sV + V; sA <- sA + t[1, 1]
  }
  list(BD = BD, tarone = BD - (sA - sE)^2 / sV, sd = gecerli - 1)
}
mh <- function(x) {
  K <- dim(x)[3]
  a <- mantelhaen.test(x); b <- mantelhaen.test(x, correct = FALSE)
  E <- 0; Vc <- 0
  for (k in 1:K) { t <- x[, , k]; T <- sum(t); E <- E + sum(t[1, ]) * sum(t[, 1]) / T; Vc <- Vc + prod(rowSums(t)) * prod(colSums(t)) / T^3 }
  coch <- (sum(x[1, 1, ]) - E)^2 / Vc
  b2 <- bd(x)
  list(tablolar = lapply(1:K, function(k) mat(x[, , k])), chi = unname(a$statistic), p = a$p.value,
       chiDuzsuz = unname(b$statistic), pDuzsuz = b$p.value, or = unname(a$estimate), ga = as.numeric(a$conf.int),
       cochran = coch, BD = b2$BD, tarone = b2$tarone, bdSd = b2$sd)
}
UCB <- aperm(UCBAdmissions, c(2, 1, 3))
mhVakalar <- list(mh(Rabbits), mh(UCB), mh(array(c(10, 5, 8, 12, 7, 9, 4, 11, 6, 6, 5, 9), dim = c(2, 2, 3))))

# ---------- Odds oranı, göreli risk, risk farkı ----------
risk <- function(t, conf.level = 0.95) {
  t <- mat(t); z <- qnorm(1 - (1 - conf.level) / 2)
  t2 <- if (any(t == 0)) t + 0.5 else t
  a <- t2[1, 1]; b <- t2[1, 2]; c <- t2[2, 1]; d <- t2[2, 2]
  or <- a * d / (b * c); se <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  t3 <- if (t[1, 1] == 0 || t[2, 1] == 0) t + 0.5 else t
  rr <- (t3[1, 1] / sum(t3[1, ])) / (t3[2, 1] / sum(t3[2, ]))
  serr <- sqrt(1 / t3[1, 1] - 1 / sum(t3[1, ]) + 1 / t3[2, 1] - 1 / sum(t3[2, ]))
  n1 <- sum(t[1, ]); n2 <- sum(t[2, ]); p1 <- t[1, 1] / n1; p2 <- t[2, 1] / n2
  w1 <- prop.test(t[1, 1], n1, correct = FALSE, conf.level = conf.level)$conf.int
  w2 <- prop.test(t[2, 1], n2, correct = FALSE, conf.level = conf.level)$conf.int
  sew <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  # epitools ile bağımsız denetim (epitools: 1. satır referans, 2. sütun olay)
  ep <- if (all(t > 0)) epitools::oddsratio.wald(t[2:1, 2:1], conf.level = conf.level)$measure[2, ] else NULL
  epr <- if (all(t > 0)) epitools::riskratio.wald(t[2:1, 2:1], conf.level = conf.level)$measure[2, ] else NULL
  list(tablo = t, duzey = conf.level, or = or, orGA = or * exp(c(-1, 1) * z * se), rr = rr, rrGA = rr * exp(c(-1, 1) * z * serr),
       rd = p1 - p2, waldGA = p1 - p2 + c(-1, 1) * z * sew,
       newcombeGA = c(p1 - p2 - sqrt((p1 - w1[1])^2 + (w2[2] - p2)^2), p1 - p2 + sqrt((w1[2] - p1)^2 + (p2 - w2[1])^2)),
       epOR = if (is.null(ep)) NULL else unname(ep), epRR = if (is.null(epr)) NULL else unname(epr))
}
riskVakalar <- list(risk(matrix(c(30, 20, 70, 80), 2)), risk(UA), risk(matrix(c(0, 5, 20, 15), 2)), risk(matrix(c(12, 4, 3, 9), 2), 0.9),
                    risk(matrix(c(8, 0, 12, 20), 2)))

# ---------- Binom testi ve oran güven aralıkları ----------
bt <- function(x, n, p0, alt = "two.sided", conf.level = 0.95) {
  r <- binom.test(x, n, p0, alternative = alt, conf.level = conf.level)
  list(x = x, n = n, p0 = p0, alt = alt, duzey = conf.level, p = r$p.value, ga = as.numeric(r$conf.int))
}
btVakalar <- list(bt(682, 925, 0.75), bt(7, 20, 0.5), bt(7, 20, 0.3), bt(0, 15, 0.1), bt(15, 15, 0.8), bt(3, 50, 0.02),
                  bt(13, 40, 0.25, "greater"), bt(13, 40, 0.25, "less", 0.9), bt(2, 9, 1 / 3), bt(50, 100, 0.5), bt(1, 1, 0.5))
ga1 <- function(x, n, conf.level = 0.95) {
  z <- qnorm(1 - (1 - conf.level) / 2); p <- x / n
  ac <- PropCIs::addz2ci(x, n, conf.level)$conf.int
  list(x = x, n = n, duzey = conf.level, wilson = as.numeric(prop.test(x, n, correct = FALSE, conf.level = conf.level)$conf.int),
       cp = as.numeric(binom.test(x, n, conf.level = conf.level)$conf.int),
       wald = pmin(1, pmax(0, p + c(-1, 1) * z * sqrt(p * (1 - p) / n))), ac = as.numeric(ac))
}
gaVakalar <- list(ga1(15, 40), ga1(1, 30), ga1(0, 25), ga1(25, 25), ga1(81, 263, 0.9), ga1(450, 1000, 0.99))
po <- function(x1, n1, x2, n2, correct, alt = "two.sided", conf.level = 0.95) {
  r <- suppressWarnings(prop.test(c(x1, x2), c(n1, n2), correct = correct, alternative = alt, conf.level = conf.level))
  list(x1 = x1, n1 = n1, x2 = x2, n2 = n2, sureklilik = correct, alt = alt, duzey = conf.level,
       chi = unname(r$statistic), p = r$p.value, ga = as.numeric(r$conf.int))
}
poVakalar <- list(po(83, 86, 90, 93, FALSE), po(83, 86, 90, 93, TRUE), po(15, 50, 25, 48, FALSE), po(15, 50, 25, 48, TRUE),
                  po(15, 50, 25, 48, TRUE, "less"), po(15, 50, 25, 48, FALSE, "greater", 0.9), po(5, 20, 5, 21, TRUE))
gof <- function(x, p = rep(1 / length(x), length(x))) {
  r <- chisq.test(x, p = p, rescale.p = TRUE)
  list(x = x, oran = p, chi = unname(r$statistic), sd = unname(r$parameter), p = r$p.value, beklenen = unname(r$expected), artik = unname(r$residuals))
}
gofVakalar <- list(gof(c(89, 37, 30, 28, 2), c(40, 20, 20, 15, 5)), gof(c(20, 15, 25)), gof(c(315, 108, 101, 32), c(9, 3, 3, 1) / 16))

# ---------- Sütun oranları z testi ----------
sutunZ <- function(t) {
  t <- mat(t); C <- colSums(t); sonuc <- list()
  for (i in 1:nrow(t)) {
    pr <- t[i, ] / C; ad <- which(pr > 0 & pr < 1); z <- c(); p <- c()
    if (length(ad) >= 2) for (a in 1:(length(ad) - 1)) for (b in (a + 1):length(ad)) {
      j <- ad[a]; k <- ad[b]; pb <- (t[i, j] + t[i, k]) / (C[j] + C[k])
      zz <- (pr[j] - pr[k]) / sqrt(pb * (1 - pb) * (1 / C[j] + 1 / C[k])); z <- c(z, zz); p <- c(p, 2 * pnorm(-abs(zz)))
    }
    sonuc[[i]] <- list(z = unname(z), p = unname(p), pAdj = p.adjust(unname(p), "bonferroni"))
  }
  list(tablo = t, satirlar = sonuc)
}
sutunVakalar <- list(sutunZ(HE), sutunZ(TC), sutunZ(matrix(c(30, 10, 25, 15, 5, 35), 2)))

sonuc <- list(kikare = kkVakalar, fisher22 = fisher22, fisherRC = fisherRC, fisherMC = fisherMC,
              mcnemar = mcnVakalar, bowker = bowkerVakalar, bowkerBos = bowkerBos, cochranQ = cqVakalar,
              cochranArmitage = caVakalar, mantelHaenszel = mhVakalar, risk = riskVakalar, binom = btVakalar,
              oranGA = gaVakalar, ikiOran = poVakalar, uyum = gofVakalar, sutunOran = sutunVakalar)
