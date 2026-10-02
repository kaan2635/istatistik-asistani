# paketler: coin, clinfun, rstatix, dunn.test
# Parametrik olmayan testler için referans değerler (nonparametrik.js)
suppressMessages({
  library(coin)
  library(clinfun)
})

# ---------- Mann-Whitney U ----------
# Not: R 4.6'da wilcox.test(exact = NULL) bağlı veride de kesin hesaplar. Bizim 'otomatik' kuralımız (SPSS gibi)
# bağ yoksa ve n1, n2 < 50 ise kesindir; bu yüzden 'otomatik' vakalarda R'ye aynı kural açıkça verilir.
mw <- function(x, y, exact = NULL, correct = FALSE, alt = "two.sided", conf.level = 0.95, mu = 0) {
  Rexact <- if (is.null(exact)) (!anyDuplicated(c(x - mu, y)) && length(x) < 50 && length(y) < 50) else exact
  w <- suppressWarnings(wilcox.test(x, y, exact = Rexact, correct = correct, alternative = alt,
                                    conf.int = TRUE, conf.level = conf.level, mu = mu))
  n1 <- length(x); n2 <- length(y); N <- n1 + n2
  r <- rank(c(x - mu, y))
  NT <- table(r)
  sig <- sqrt(n1 * n2 / 12 * ((N + 1) - sum(NT^3 - NT) / (N * (N - 1))))
  z0 <- (sum(r[1:n1]) - n1 * (n1 + 1) / 2 - n1 * n2 / 2) / sig
  list(x = x, y = y, yontem = if (is.null(exact)) "otomatik" else if (exact) "kesin" else "asimptotik",
       sureklilik = correct, alt = alt, duzey = conf.level, mu = mu,
       W = unname(w$statistic), p = w$p.value, tahmin = unname(w$estimate), ga = as.numeric(w$conf.int),
       gaDuzey = attr(w$conf.int, "conf.level"), z0 = z0, R1 = sum(r[1:n1]), R2 = sum(r[(n1 + 1):N]),
       kesinMi = grepl("exact", w$method), ikiliFarklar = sort(outer(x, y, "-")))
}

set.seed(2024)
aq <- airquality[!is.na(airquality$Ozone), ]
oz5 <- aq$Ozone[aq$Month == 5]; oz8 <- aq$Ozone[aq$Month == 8]
tg <- ToothGrowth
vc <- tg$len[tg$supp == "VC"]; oj <- tg$len[tg$supp == "OJ"]
kucukx <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)       # ?wilcox.test örneği
kucuky <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
depx <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
depy <- c(1.15, 0.88, 0.90, 0.74, 1.21)
bagx <- c(1, 2, 2, 3, 3, 3, 4, 5, 5, 6)
bagy <- c(2, 3, 3, 4, 4, 5, 5, 5, 6, 7, 7, 8)
likx <- c(3, 4, 4, 5, 2, 4, 3, 5, 5, 4, 3, 4)
liky <- c(2, 3, 3, 2, 1, 3, 4, 2, 3, 3, 2, 1, 2)
bx <- round(rnorm(60, 10, 3), 1); by <- round(rnorm(55, 11, 3), 1)

mwVakalar <- list(
  mw(depx, depy),
  mw(depx, depy, alt = "greater"),
  mw(depx, depy, alt = "less", conf.level = 0.9),
  mw(depx, depy, exact = FALSE),
  mw(depx, depy, exact = FALSE, correct = TRUE),
  mw(kucukx, kucuky),
  mw(kucukx, kucuky, correct = TRUE),
  mw(oz5, oz8),
  mw(oz5, oz8, correct = TRUE),
  mw(oz5, oz8, alt = "less"),
  mw(oz5, oz8, alt = "greater", correct = TRUE),
  mw(vc, oj),
  mw(bagx, bagy),
  mw(bagx, bagy, correct = TRUE, conf.level = 0.99),
  mw(likx, liky),
  mw(bx, by),
  mw(bx, by, exact = TRUE),
  mw(c(1.1, 2.2, 3.3), c(4.4, 5.5, 6.6, 7.7)),
  mw(c(5, 7), c(1, 2, 3, 4, 6, 8, 9)),
  mw(c(1.5, 2.5, 9), c(3, 4)),
  mw(depx, depy, mu = 0.3),
  mw(round(rnorm(20), 2), round(rnorm(25, 0.5), 2)),
  mw(c(10, 20, 30, 40), c(15, 25, 35, 45, 55, 65)),
  # bağlı veride R 4.6 kesin testi (koşullu dağılım, orta nokta taramalı güven aralığı)
  mw(bagx, bagy, exact = TRUE),
  mw(bagx, bagy, exact = TRUE, alt = "less", conf.level = 0.9),
  mw(likx, liky, exact = TRUE, alt = "greater"),
  mw(c(1, 1, 2, 2, 2, 3), c(2, 3, 3, 4, 4), exact = TRUE),
  mw(oz5, oz8, exact = TRUE),
  mw(vc, oj, exact = TRUE, conf.level = 0.99)
)

coinmw <- function(x, y) {
  d <- data.frame(v = c(x, y), g = factor(rep(c("a", "b"), c(length(x), length(y)))))
  t <- coin::wilcox_test(v ~ g, data = d, distribution = "exact")
  tg <- coin::wilcox_test(v ~ g, data = d, distribution = "exact", alternative = "greater")
  tl <- coin::wilcox_test(v ~ g, data = d, distribution = "exact", alternative = "less")
  ta <- coin::wilcox_test(v ~ g, data = d)
  list(x = x, y = y, p = pvalue(t), pBuyuk = pvalue(tg), pKucuk = pvalue(tl), z = unname(statistic(ta)))
}
mwKesinBagli <- list(coinmw(bagx, bagy), coinmw(likx, liky), coinmw(c(1, 1, 2, 2, 2, 3), c(2, 3, 3, 4, 4)),
                     coinmw(depx, depy))

# ---------- Wilcoxon işaretli sıralar ----------
sr <- function(x, y = NULL, exact = NULL, correct = FALSE, alt = "two.sided", conf.level = 0.95, mu = 0) {
  dd <- if (is.null(y)) x - mu else x - y - mu
  Rexact <- if (is.null(exact)) (!any(dd == 0) && !anyDuplicated(abs(dd)) && length(dd) < 50) else exact
  w <- suppressWarnings(wilcox.test(x, y, paired = !is.null(y), exact = Rexact, correct = correct,
                                    alternative = alt, conf.int = TRUE, conf.level = conf.level, mu = mu))
  d <- if (is.null(y)) x - mu else x - y - mu
  sifir <- sum(d == 0)
  rTum <- rank(abs(d)); WnegPratt <- sum(rTum[d < 0])
  d <- d[d != 0]; n <- length(d); r <- rank(abs(d)); NT <- table(r)
  sig <- sqrt(n * (n + 1) * (2 * n + 1) / 24 - sum(NT^3 - NT) / 48)
  z0 <- (sum(r[d > 0]) - n * (n + 1) / 4) / sig
  list(x = x, y = y, yontem = if (is.null(exact)) "otomatik" else if (exact) "kesin" else "asimptotik",
       sureklilik = correct, alt = alt, duzey = conf.level, mu = mu,
       V = unname(w$statistic), p = w$p.value, tahmin = unname(w$estimate), ga = as.numeric(w$conf.int),
       gaDuzey = attr(w$conf.int, "conf.level"), z0 = z0, sifir = sifir, Wneg = if (isTRUE(exact)) WnegPratt else sum(r[d < 0]),
       kesinMi = grepl("exact", w$method))
}
once <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
sonra <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
s1 <- sleep$extra[sleep$group == 1]; s2 <- sleep$extra[sleep$group == 2]
lk1 <- c(4, 3, 5, 2, 4, 3, 3, 5, 4, 2, 3, 4, 5, 3, 2)
lk2 <- c(3, 3, 4, 2, 2, 2, 3, 4, 2, 2, 2, 3, 3, 1, 2)
p1 <- round(rnorm(70, 50, 10), 1); p2 <- round(p1 + rnorm(70, 1.5, 4), 1)
srVakalar <- list(
  sr(once, sonra),
  sr(once, sonra, alt = "greater"),
  sr(once, sonra, alt = "less", conf.level = 0.9),
  sr(once, sonra, exact = FALSE),
  sr(once, sonra, exact = FALSE, correct = TRUE),
  sr(s1, s2),
  sr(s1, s2, correct = TRUE),
  sr(s1, s2, alt = "less"),
  sr(lk1, lk2),
  sr(lk1, lk2, correct = TRUE, conf.level = 0.9),
  sr(p1, p2),
  sr(p1, p2, exact = TRUE),
  sr(p1, p2, alt = "greater", correct = TRUE),
  sr(once, NULL, mu = 1),
  sr(once, NULL, mu = 1.5, alt = "greater"),
  sr(c(1.2, -0.4, 2.3, 0.8, 1.1, 3.1, -0.2, 0.5), NULL),
  sr(c(2, 2, 3, -1, 4, 4, 0, 5, -2, 3), NULL),
  sr(c(0.4, 1.1, 2.6, 3.2), c(0.1, 0.2, 0.3, 0.5)),
  sr(c(10, 12, 15, 9, 11), c(8, 10, 15, 7, 13)),
  # R 4.6 kesin testi: bağ ve sıfır farklarla (sıfırlar Pratt yöntemiyle), taramalı güven aralığı
  sr(lk1, lk2, exact = TRUE),
  sr(s1, s2, exact = TRUE, alt = "less"),
  sr(c(2, 2, 3, -1, 4, 4, 0, 5, -2, 3), NULL, exact = TRUE),
  sr(c(10, 12, 15, 9, 11), c(8, 10, 15, 7, 13), exact = TRUE, alt = "greater", conf.level = 0.9),
  sr(c(1.5, 2.5, 2.5, 4, -1, 3, 3, 6), NULL, exact = TRUE, mu = 1)
)
coinsr <- function(x, y) {
  d <- data.frame(x = x, y = y)
  t <- coin::wilcoxsign_test(x ~ y, data = d, zero.method = "Wilcoxon", distribution = "exact")
  tg <- coin::wilcoxsign_test(x ~ y, data = d, zero.method = "Wilcoxon", distribution = "exact", alternative = "greater")
  tl <- coin::wilcoxsign_test(x ~ y, data = d, zero.method = "Wilcoxon", distribution = "exact", alternative = "less")
  tp <- coin::wilcoxsign_test(x ~ y, data = d, zero.method = "Pratt", distribution = "exact")
  list(x = x, y = y, p = pvalue(t), pBuyuk = pvalue(tg), pKucuk = pvalue(tl), pPratt = pvalue(tp))
}
srKesinBagli <- list(coinsr(lk1, lk2), coinsr(s1, s2), coinsr(c(2, 2, 3, -1, 4, 4, 0, 5, -2, 3), rep(0, 10)))

# ---------- İşaret testi ----------
isaret <- function(x, y) {
  d <- x - y; np <- sum(d > 0); nn <- sum(d < 0); n <- np + nn
  list(x = x, y = y, pozitif = np, negatif = nn, pKesin = binom.test(np, n, 0.5)$p.value,
       pBuyuk = binom.test(np, n, 0.5, alternative = "greater")$p.value,
       pKucuk = binom.test(np, n, 0.5, alternative = "less")$p.value,
       z = -max(0, abs(np - nn) - 1) / sqrt(n), pAsim = 2 * pnorm(-max(0, abs(np - nn) - 1) / sqrt(n)))
}
isaretVakalar <- list(isaret(lk1, lk2), isaret(s1, s2), isaret(p1, p2), isaret(once, sonra))

# ---------- Kruskal-Wallis ve Dunn ----------
kw <- function(gruplar) {
  k <- kruskal.test(gruplar)
  N <- length(unlist(gruplar)); kk <- length(gruplar)
  r <- rank(unlist(gruplar)); g <- rep(seq_along(gruplar), lengths(gruplar))
  list(gruplar = gruplar, H = unname(k$statistic), sd = unname(k$parameter), p = k$p.value,
       ortSira = as.numeric(tapply(r, g, mean)), eps2 = unname(k$statistic) / (N - 1),
       eta2 = (unname(k$statistic) - kk + 1) / (N - kk))
}
dunnR <- function(gruplar, yontem) {
  d <- data.frame(v = unlist(gruplar), g = factor(rep(paste0("g", seq_along(gruplar)), lengths(gruplar))))
  dt <- rstatix::dunn_test(d, v ~ g, p.adjust.method = yontem)
  list(grup1 = as.integer(sub("g", "", dt$group1)), grup2 = as.integer(sub("g", "", dt$group2)),
       z = dt$statistic, p = dt$p, pAdj = dt$p.adj)
}
ins <- split(InsectSprays$count, InsectSprays$spray)
names(ins) <- NULL
pg <- unname(split(PlantGrowth$weight, PlantGrowth$group))
aqm <- unname(split(aq$Ozone, aq$Month))
lik3 <- list(c(1, 2, 2, 3, 3, 3, 4), c(2, 3, 3, 4, 4, 5), c(3, 4, 4, 5, 5, 5, 5, 2))
kwVakalar <- list(kw(ins), kw(pg), kw(aqm), kw(lik3), kw(list(c(2.9, 3.0, 2.5, 2.6, 3.2), c(3.8, 2.7, 4.0, 2.4), c(2.8, 3.4, 3.7, 2.2, 2.0))))
dunnVakalar <- list(
  list(gruplar = ins, yontem = "bonferroni", r = dunnR(ins, "bonferroni")),
  list(gruplar = pg, yontem = "holm", r = dunnR(pg, "holm")),
  list(gruplar = aqm, yontem = "BH", r = dunnR(aqm, "BH")),
  list(gruplar = lik3, yontem = "bonferroni", r = dunnR(lik3, "bonferroni"))
)
# dunn.test paketiyle bağımsız denetim (z = (R̄_i - R̄_j)/SE)
dt2 <- dunn.test::dunn.test(unlist(ins), rep(1:6, lengths(ins)), method = "bonferroni", kw = FALSE, table = FALSE)
dunnTest2 <- list(karsilastirma = dt2$comparisons, z = dt2$Z, p = dt2$P, pAdj = dt2$P.adjusted)

# ---------- Friedman ----------
RoundingTimes <- matrix(c(5.40, 5.50, 5.55, 5.85, 5.70, 5.75, 5.20, 5.60, 5.50, 5.55, 5.50, 5.40,
  5.90, 5.85, 5.70, 5.45, 5.55, 5.60, 5.40, 5.40, 5.35, 5.45, 5.50, 5.35, 5.25, 5.15, 5.00,
  5.85, 5.80, 5.70, 5.25, 5.20, 5.10, 5.65, 5.55, 5.45, 5.60, 5.35, 5.45, 5.05, 5.00, 4.95,
  5.50, 5.50, 5.40, 5.45, 5.55, 5.50, 5.55, 5.55, 5.35, 5.45, 5.50, 5.55, 5.50, 5.45, 5.25,
  5.65, 5.60, 5.40, 5.70, 5.65, 5.55, 6.30, 6.30, 6.25), nrow = 22, byrow = TRUE)
lik4 <- matrix(c(3, 4, 4, 5, 2, 3, 3, 4, 4, 4, 5, 5, 1, 2, 2, 3, 3, 3, 4, 4, 2, 4, 3, 5, 3, 3, 3, 3,
                 4, 5, 4, 5, 2, 2, 3, 4, 3, 4, 5, 4), ncol = 4, byrow = TRUE)
fr <- function(m) {
  f <- friedman.test(m)
  n <- nrow(m); k <- ncol(m)
  R <- t(apply(m, 1, rank))
  Rj <- colSums(R)
  # Conover (1999) — ikili t istatistikleri
  A <- sum(R^2)
  sdc <- (n - 1) * (k - 1)
  sh <- sqrt(2 * (n * A - sum(Rj^2)) / sdc)
  cf <- combn(k, 2)
  tcon <- apply(cf, 2, function(ij) (Rj[ij[1]] - Rj[ij[2]]) / sh)
  pcon <- 2 * pt(-abs(tcon), sdc)
  # Nemenyi
  qn <- apply(cf, 2, function(ij) sqrt(2) * abs(Rj[ij[1]] - Rj[ij[2]]) / n / sqrt(k * (k + 1) / (6 * n)))
  pn <- ptukey(qn, k, Inf, lower.tail = FALSE)
  # SPSS tarzı
  zs <- apply(cf, 2, function(ij) (Rj[ij[1]] - Rj[ij[2]]) / n / sqrt(k * (k + 1) / (6 * n)))
  ps <- 2 * pnorm(-abs(zs))
  # Wilcoxon ikili (Bonferroni)
  uzun <- data.frame(v = as.vector(m), g = factor(rep(seq_len(k), each = n)))
  pw <- suppressWarnings(pairwise.wilcox.test(uzun$v, uzun$g, paired = TRUE, p.adjust.method = "bonferroni", correct = FALSE, exact = FALSE))
  pwHam <- suppressWarnings(pairwise.wilcox.test(uzun$v, uzun$g, paired = TRUE, p.adjust.method = "none", correct = FALSE, exact = FALSE))
  pwR46 <- suppressWarnings(pairwise.wilcox.test(uzun$v, uzun$g, paired = TRUE, p.adjust.method = "none", correct = FALSE))
  list(matris = m, chi = unname(f$statistic), sd = unname(f$parameter), p = f$p.value,
       ortSira = as.numeric(Rj / n), W = unname(f$statistic) / (n * (k - 1)),
       conoverT = tcon, conoverP = pcon, conoverPHolm = p.adjust(pcon, "holm"),
       nemenyiQ = qn, nemenyiP = pn, spssZ = zs, spssP = ps, spssPBonf = p.adjust(ps, "bonferroni"),
       wilcoxP = pwHam$p.value[lower.tri(pwHam$p.value, diag = TRUE)], wilcoxPBonf = pw$p.value[lower.tri(pw$p.value, diag = TRUE)],
       wilcoxPR46 = pwR46$p.value[lower.tri(pwR46$p.value, diag = TRUE)])
}
frVakalar <- list(fr(RoundingTimes), fr(lik4), fr(matrix(round(rnorm(45), 2), ncol = 3)),
                  fr(as.matrix(warpbreaks[1:9, "breaks", drop = FALSE] |> cbind(warpbreaks$breaks[10:18], warpbreaks$breaks[19:27]))))

# ---------- Jonckheere-Terpstra ----------
jtElle <- function(gruplar) {
  k <- length(gruplar); n <- lengths(gruplar); N <- sum(n)
  J <- 0
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    a <- gruplar[[i]]; b <- gruplar[[j]]
    J <- J + sum(outer(a, b, "<")) + 0.5 * sum(outer(a, b, "=="))
  }
  t <- table(unlist(gruplar))
  v <- (N * (N - 1) * (2 * N + 5) - sum(n * (n - 1) * (2 * n + 5)) - sum(t * (t - 1) * (2 * t + 5))) / 72 +
    sum(n * (n - 1) * (n - 2)) * sum(t * (t - 1) * (t - 2)) / (36 * N * (N - 1) * (N - 2)) +
    sum(n * (n - 1)) * sum(t * (t - 1)) / (8 * N * (N - 1))
  z <- (J - (N^2 - sum(n^2)) / 4) / sqrt(v)
  list(J = J, ort = (N^2 - sum(n^2)) / 4, var = v, z = z, p = 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE)),
       pArtan = pnorm(z, lower.tail = FALSE), pAzalan = pnorm(z))
}
# Not: clinfun::jonckheere.test'in kesin p değerleri webR/R 4.6'da hatalı (negatif p) çıkıyor; bu yüzden clinfun
# yalnızca normal yaklaşım (N > 100) için kullanılır. Kesin dağılım aşağıda bağımsız olarak hesaplanır.
jtClin <- function(gruplar, alt) {
  x <- unlist(gruplar); g <- rep(seq_along(gruplar), lengths(gruplar))
  r <- suppressWarnings(clinfun::jonckheere.test(x, g, alternative = alt))
  list(J = unname(r$statistic), p = r$p.value)
}
# Kaba kuvvet: tüm grup atamaları (küçük N)
jtKaba <- function(gruplar) {
  n <- lengths(gruplar); N <- sum(n); deg <- seq_len(N)
  Jf <- function(atama) {
    J <- 0
    for (i in 1:(length(n) - 1)) for (j in (i + 1):length(n)) J <- J + sum(outer(deg[atama == i], deg[atama == j], "<"))
    J
  }
  # yalnızca 3 grup için
  stopifnot(length(n) == 3)
  Js <- numeric(choose(N, n[1]) * choose(N - n[1], n[2])); say <- 0
  for (a in combn(N, n[1], simplify = FALSE)) {
    kalan <- setdiff(deg, a)
    for (b in combn(kalan, n[2], simplify = FALSE)) {
      atama <- rep(3L, N); atama[a] <- 1L; atama[b] <- 2L
      say <- say + 1; Js[say] <- Jf(atama)
    }
  }
  Js
}
# Evrişim: J = Σ U(önceki gruplar, grup i); dwilcox her (m, n) için ayrı çağrılır
jtKesinDag <- function(n) {
  d <- 1; kum <- n[1]
  for (i in 2:length(n)) {
    f <- dwilcox(0:(kum * n[i]), kum, n[i])
    yeni <- rep(0, length(d) + length(f) - 1)
    for (a in seq_along(d)) yeni[a:(a + length(f) - 1)] <- yeni[a:(a + length(f) - 1)] + d[a] * f
    d <- yeni; kum <- kum + n[i]
  }
  d
}
jtKesinP <- function(J, d) {
  alt <- sum(d[1:(J + 1)]); ust <- sum(d[(J + 1):length(d)])
  list(iki = min(1, 2 * min(alt, ust)), artan = ust, azalan = alt)
}
jt1 <- list(c(1.2, 3.4, 2.2, 0.8), c(2.5, 4.1, 3.9, 1.7, 3.0), c(4.4, 5.2, 3.3, 6.1))
jt2 <- list(round(rnorm(15, 0), 6), round(rnorm(12, 0.4), 6), round(rnorm(18, 0.8), 6))
jt3 <- lik3
jt4 <- list(round(rnorm(40), 6), round(rnorm(35, 0.2), 6), round(rnorm(38, 0.3), 6))
jt1J <- jtElle(jt1)$J
kaba1 <- jtKaba(jt1)
jt1Kaba <- list(iki = min(1, 2 * min(mean(kaba1 <= jt1J), mean(kaba1 >= jt1J))), artan = mean(kaba1 >= jt1J), azalan = mean(kaba1 <= jt1J))
jtVakalar <- list(
  list(gruplar = jt1, elle = jtElle(jt1), kesin = jt1Kaba, kesinEvrisim = jtKesinP(jt1J, jtKesinDag(lengths(jt1)))),
  list(gruplar = jt2, elle = jtElle(jt2), kesin = jtKesinP(jtElle(jt2)$J, jtKesinDag(lengths(jt2)))),
  list(gruplar = jt3, elle = jtElle(jt3)),
  list(gruplar = jt4, elle = jtElle(jt4), iki = jtClin(jt4, "two.sided"), artan = jtClin(jt4, "increasing"))
)

# ---------- Medyan testi ----------
mt <- function(gruplar) {
  tum <- unlist(gruplar); m <- median(tum)
  tab <- rbind(sapply(gruplar, function(g) sum(g > m)), sapply(gruplar, function(g) sum(g <= m)))
  ch <- suppressWarnings(chisq.test(tab, correct = FALSE))
  list(gruplar = gruplar, medyan = m, tablo = tab, chi = unname(ch$statistic), sd = unname(ch$parameter), p = ch$p.value,
       fisherP = fisher.test(tab)$p.value, minBeklenen = min(ch$expected),
       yates = if (length(gruplar) == 2) unname(suppressWarnings(chisq.test(tab, correct = TRUE))$statistic) else NULL)
}
mtVakalar <- list(mt(pg), mt(lik3), mt(list(depx, depy)), mt(aqm), mt(list(oz5, oz8)))

sonuc <- list(mw = mwVakalar, mwKesinBagli = mwKesinBagli, sr = srVakalar, srKesinBagli = srKesinBagli,
              isaret = isaretVakalar, kw = kwVakalar, dunn = dunnVakalar, dunnTest2 = dunnTest2,
              friedman = frVakalar, jt = jtVakalar, medyan = mtVakalar)
