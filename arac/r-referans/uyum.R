# paketler: irr, psych, vcd, epiR, blandr, lavaan, MBESS
# Uyum (kappa, ICC, Bland-Altman, Lin CCC, yüzde uyum) ve güvenirlik (alfa, KR-20/21, yarıya bölme, omega)
# için referans değerler. Veriler de JSON'a yazılır (testler aynı veriyi kullanır).
library(irr)
library(psych)
# webR'de parallel::detectCores() NA döndürüyor; lavaan seçenek denetimi bu yüzden hata veriyor. 1 çekirdek bildir.
local({ ns <- asNamespace("parallel"); unlockBinding("detectCores", ns); assign("detectCores", function(...) 1L, envir = ns); lockBinding("detectCores", ns) })
sayisalMatris <- function(m) unname(lapply(seq_len(nrow(m)), function(i) unname(as.list(m[i, ]))))

# ================= Cohen kappa =================
# Fleiss-Cohen-Everitt (1969) H0 dışı asimptotik SH (elle) — vcd::Kappa ile de karşılaştırılır
kappaElle <- function(a, b, lev, W) {
  t <- table(factor(a, levels = lev), factor(b, levels = lev))
  n <- sum(t); p <- t / n; pr <- rowSums(p); pc <- colSums(p)
  po <- sum(W * p); pe <- sum(W * outer(pr, pc)); k <- (po - pe) / (1 - pe)
  wi <- as.vector(W %*% pc); wj <- as.vector(t(W) %*% pr)
  v1 <- (sum(p * (W - outer(wi, wj, "+") * (1 - k))^2) - (k - pe * (1 - k))^2) / (n * (1 - pe)^2)
  v0 <- (sum(outer(pr, pc) * (W - outer(wi, wj, "+"))^2) - pe^2) / (n * (1 - pe)^2)
  list(kappa = k, sh = sqrt(v1), sh0 = sqrt(v0), po = po, pe = pe, tablo = unname(lapply(seq_len(nrow(t)), function(i) as.vector(t[i, ]))))
}
agirlikMatrisi <- function(c, tip) {
  d <- abs(outer(1:c, 1:c, "-"))
  switch(tip, yok = diag(c), dogrusal = 1 - d / (c - 1), kuadratik = 1 - (d / (c - 1))^2)
}
irrAgirlik <- c(yok = "unweighted", dogrusal = "equal", kuadratik = "squared")
kappaVaka <- function(a, b, agirlik, lev = NULL, duzey = 0.95) {
  ok <- !is.na(a) & !is.na(b)
  if (is.null(lev)) lev <- if (is.numeric(a)) sort(unique(c(a[ok], b[ok]))) else sort(unique(c(a[ok], b[ok])))
  W <- agirlikMatrisi(length(lev), agirlik)
  e <- kappaElle(a[ok], b[ok], lev, W)
  ir <- kappa2(cbind(a, b), weight = irrAgirlik[[agirlik]])
  vc <- vcd::Kappa(table(factor(a[ok], levels = lev), factor(b[ok], levels = lev)), weights = if (agirlik == "kuadratik") "Fleiss-Cohen" else "Equal-Spacing")
  vcASE <- if (agirlik == "yok") vc$Unweighted[["ASE"]] else vc$Weighted[["ASE"]]
  z <- qnorm((1 + duzey) / 2)
  list(r1 = a, r2 = b, agirlik = agirlik, duzeyler = lev, kappa = ir$value, z = ir$statistic, p = ir$p.value,
       pKesin = 2 * pnorm(-abs(ir$statistic)), sh0 = e$sh0, sh = e$sh, shVcd = vcASE, po = e$po, pe = e$pe,
       gaAlt = e$kappa - z * e$sh, gaUst = e$kappa + z * e$sh, tablo = e$tablo, n = sum(ok),
       yuzde = 100 * mean(a[ok] == b[ok]))
}
data(diagnoses); data(anxiety)
diag1 <- as.character(diagnoses[, 1]); diag2 <- as.character(diagnoses[, 2])
set.seed(11)
gercek <- sample(1:5, 120, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.25, 0.15))
o1 <- pmin(5, pmax(1, gercek + sample(c(-1, 0, 0, 0, 1), 120, replace = TRUE)))
o2 <- pmin(5, pmax(1, gercek + sample(c(-2, -1, 0, 0, 1), 120, replace = TRUE)))
o1[c(5, 17)] <- NA; o2[c(17, 40, 99)] <- NA
kucukA <- c(1, 2, 2, 3, 1, 1, 3, 2, 2, 1); kucukB <- c(1, 2, 3, 3, 1, 2, 3, 2, 1, 1)
# Bir değerlendirici bir kategoriyi hiç kullanmıyor (birleşik kategori kümesi)
eksikA <- c("a", "b", "c", "a", "b", "c", "a", "a", "b", "c", "c", "a")
eksikB <- c("a", "b", "b", "a", "b", "b", "a", "b", "b", "a", "b", "a")
kappaSonuc <- list()
for (ag in c("yok", "dogrusal", "kuadratik")) {
  kappaSonuc[[length(kappaSonuc) + 1]] <- c(list(veri = "anksiyete"), kappaVaka(anxiety$rater1, anxiety$rater2, ag))
  kappaSonuc[[length(kappaSonuc) + 1]] <- c(list(veri = "sentetik"), kappaVaka(o1, o2, ag))
  kappaSonuc[[length(kappaSonuc) + 1]] <- c(list(veri = "kucuk"), kappaVaka(kucukA, kucukB, ag, duzey = 0.9))
}
kappaSonuc[[length(kappaSonuc) + 1]] <- c(list(veri = "tani"), kappaVaka(diag1, diag2, "yok"))
kappaSonuc[[length(kappaSonuc) + 1]] <- c(list(veri = "eksikKategori"), kappaVaka(eksikA, eksikB, "yok"))

# ================= Fleiss kappa =================
fleissElle <- function(m) {
  m <- as.matrix(na.omit(m)); lev <- levels(as.factor(m)); ns <- nrow(m); nr <- ncol(m)
  tt <- t(apply(m, 1, function(r) as.numeric(table(factor(r, levels = lev)))))
  pj <- colSums(tt) / (ns * nr)
  pjk <- (colSums(tt^2) - ns * nr * pj) / (ns * nr * (nr - 1) * pj)
  kK <- (pjk - pj) / (1 - pj); se <- sqrt(2 / (ns * nr * (nr - 1)))
  ir <- kappam.fleiss(m, detail = TRUE)
  list(duzeyler = lev, kappa = ir$value, z = ir$statistic, p = ir$p.value, pKesin = 2 * pnorm(-abs(ir$statistic)),
       sh = ir$value / ir$statistic, kategoriKappa = unname(kK), kategoriZ = unname(kK / se), kategoriSh = se,
       kategoriP = unname(2 * pnorm(-abs(kK / se))), oranlar = unname(pj), n = ns,
       po = mean((rowSums(tt^2) - nr) / (nr * (nr - 1))), pe = sum(pj^2))
}
diagM <- as.matrix(diagnoses); diagM <- apply(diagM, 2, as.character)
set.seed(5)
fm <- matrix(sample(1:4, 25 * 4, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)), 25, 4)
for (i in 1:25) if (runif(1) < 0.5) fm[i, ] <- fm[i, 1]
fm[3, 2] <- NA
fleiss <- list(
  tani = c(list(matris = unname(lapply(seq_len(nrow(diagM)), function(i) as.list(unname(diagM[i, ]))))), fleissElle(diagM)),
  sentetik = c(list(matris = sayisalMatris(fm)), fleissElle(fm))
)

# ================= ICC =================
iccVaka <- function(m) {
  mm <- as.matrix(na.omit(m))
  out <- list(matris = sayisalMatris(m), irr = list(), psych = NULL)
  for (mod in c("oneway", "twoway")) for (tip in c("consistency", "agreement")) for (b in c("single", "average")) for (r0 in c(0, 0.3)) {
    if (mod == "oneway" && tip == "agreement") next
    x <- icc(mm, model = mod, type = tip, unit = b, r0 = r0, conf.level = 0.95)
    out$irr[[length(out$irr) + 1]] <- list(model = mod, tip = tip, birim = b, r0 = r0, icc = x$value, F = x$Fvalue,
      df1 = x$df1, df2 = x$df2, p = x$p.value, alt = x$lbound, ust = x$ubound)
  }
  x90 <- icc(mm, model = "twoway", type = "agreement", unit = "single", conf.level = 0.9)
  out$irr90 <- list(icc = x90$value, alt = x90$lbound, ust = x90$ubound)
  p <- ICC(mm, lmer = FALSE)$results
  out$psych <- lapply(seq_len(nrow(p)), function(i) list(tur = p$type[i], icc = p$ICC[i], F = p$F[i], df1 = p$df1[i],
    df2 = p$df2[i], p = p$p[i], alt = p[["lower bound"]][i], ust = p[["upper bound"]][i]))
  # McGraw & Wong (1996) mutlak uyum testi, r0 ≠ 0 (a, b, v H0 değeriyle) — elle
  n <- nrow(mm); k <- ncol(mm)
  MSr <- var(rowMeans(mm)) * k; MSc <- var(colMeans(mm)) * n
  MSe <- (var(as.numeric(mm)) * (n * k - 1) - MSr * (n - 1) - MSc * (k - 1)) / ((n - 1) * (k - 1))
  mw <- function(a, b) { F <- MSr / (a * MSc + b * MSe); v <- (a * MSc + b * MSe)^2 / ((a * MSc)^2 / (k - 1) + (b * MSe)^2 / ((n - 1) * (k - 1))); c(F, v, pf(F, n - 1, v, lower.tail = FALSE)) }
  r0 <- 0.3
  out$mwTek <- mw(k * r0 / (n * (1 - r0)), 1 + k * r0 * (n - 1) / (n * (1 - r0)))
  out$mwOrt <- mw(r0 / (n * (1 - r0)), 1 + r0 * (n - 1) / (n * (1 - r0)))
  out$MS <- c(MSr = MSr, MSc = MSc, MSe = MSe)
  out
}
sf <- matrix(c(9, 2, 5, 8, 6, 1, 3, 2, 8, 4, 6, 8, 7, 1, 2, 6, 10, 5, 6, 9, 6, 2, 4, 7), ncol = 4, byrow = TRUE)
set.seed(21)
hedef <- rnorm(30, 50, 10)
im <- cbind(hedef + rnorm(30, 0, 4), hedef + 3 + rnorm(30, 0, 4), hedef - 2 + rnorm(30, 0, 5), hedef + rnorm(30, 0, 3))
im <- round(im, 1); im[7, 3] <- NA
iccSonuc <- list(anksiyete = iccVaka(as.matrix(anxiety)), shroutFleiss = iccVaka(sf), sentetik = iccVaka(im))

# ================= Bland-Altman ve Lin CCC =================
set.seed(3)
gercekD <- runif(45, 80, 180)
y1 <- round(gercekD + rnorm(45, 0, 6), 1)
y2 <- round(gercekD * 1.04 - 2 + rnorm(45, 0, 7), 1)
y1[c(4, 30)] <- NA
baVaka <- function(x, y, duzey = 0.95, carpan = 1.96, yuzde = FALSE) {
  ok <- !is.na(x) & !is.na(y); a <- x[ok]; b <- y[ok]; n <- length(a)
  m <- (a + b) / 2; d <- if (yuzde) 100 * (a - b) / m else a - b
  bias <- mean(d); s <- sd(d); al <- 1 - duzey; tk <- qt(1 - al / 2, n - 1)
  se <- s * sqrt(1 / n + carpan^2 / (2 * (n - 1)))
  qlo <- qt(al / 2, n - 1, ncp = carpan * sqrt(n)); qhi <- qt(1 - al / 2, n - 1, ncp = carpan * sqrt(n))
  fit <- summary(lm(d ~ m)); cf <- fit$coefficients
  list(x = x, y = y, duzey = duzey, carpan = carpan, yuzde = yuzde, n = n, bias = bias, ss = s,
       biasAlt = bias - tk * s / sqrt(n), biasUst = bias + tk * s / sqrt(n), tP = t.test(d)$p.value,
       altLoA = bias - carpan * s, ustLoA = bias + carpan * s, shLoA = se,
       altYaklasik = c(bias - carpan * s - tk * se, bias - carpan * s + tk * se),
       ustYaklasik = c(bias + carpan * s - tk * se, bias + carpan * s + tk * se),
       altKesin = c(bias - s * qhi / sqrt(n), bias - s * qlo / sqrt(n)),
       ustKesin = c(bias + s * qlo / sqrt(n), bias + s * qhi / sqrt(n)),
       kesisim = cf[1, 1], egim = cf[2, 1], shKesisim = cf[1, 2], shEgim = cf[2, 2], egimT = cf[2, 3], egimP = cf[2, 4],
       egimGa = unname(confint(lm(d ~ m), level = duzey)[2, ]), r2 = fit$r.squared,
       disarida = 100 * mean(d < bias - carpan * s | d > bias + carpan * s))
}
blandr0 <- blandr::blandr.statistics(y1[!is.na(y1)], y2[!is.na(y1)])
bland <- list(
  ana = baVaka(y1, y2), duzey90 = baVaka(y1, y2, duzey = 0.9, carpan = 2), yuzde = baVaka(y1, y2, yuzde = TRUE),
  kucuk = baVaka(c(10.2, 11.5, 9.8, 12.1, 10.9), c(10.0, 11.9, 9.5, 12.6, 10.7)),
  blandr = list(bias = blandr0$bias, upperLOA = blandr0$upperLOA, lowerLOA = blandr0$lowerLOA, biasUpperCI = blandr0$biasUpperCI,
    biasLowerCI = blandr0$biasLowerCI, upperLOA_upperCI = blandr0$upperLOA_upperCI, upperLOA_lowerCI = blandr0$upperLOA_lowerCI,
    lowerLOA_upperCI = blandr0$lowerLOA_upperCI, lowerLOA_lowerCI = blandr0$lowerLOA_lowerCI)
)
cccVaka <- function(x, y, duzey = 0.95) {
  e <- epiR::epi.ccc(x, y, ci = "z-transform", conf.level = duzey)
  a <- epiR::epi.ccc(x, y, ci = "asymptotic", conf.level = duzey)
  ok <- !is.na(x) & !is.na(y)
  list(x = x, y = y, duzey = duzey, ccc = e$rho.c$est, alt = e$rho.c$lower, ust = e$rho.c$upper,
       altA = a$rho.c$lower, ustA = a$rho.c$upper, olcek = e$s.shift, konum = e$l.shift, Cb = e$C.b, r = cor(x[ok], y[ok]))
}
ccc <- list(ana = cccVaka(y1, y2), duzey90 = cccVaka(y1, y2, 0.9), ters = cccVaka(y2, y1),
            yakin = cccVaka(c(1.1, 2.0, 2.9, 4.2, 5.0, 6.1, 6.8, 8.1), c(1.0, 2.1, 3.0, 4.0, 5.1, 6.0, 7.0, 8.0)))

# ================= Yüzde uyum =================
yuzde <- list(tani = agree(diagM)$value, anksiyete0 = agree(anxiety)$value, anksiyete1 = agree(anxiety, tolerance = 1)$value,
              iki = agree(anxiety[, 1:2])$value)

# ================= Güvenirlik =================
alfaVaka <- function(x, duzey = 0.95) {
  x <- as.matrix(x); xx <- na.omit(x); k <- ncol(xx); n <- nrow(xx)
  a <- suppressWarnings(psych::alpha(as.data.frame(xx), warnings = FALSE, check.keys = FALSE))
  C <- cov(xx); R <- cor(xx)
  olcekOrt <- sapply(1:k, function(i) sum(colMeans(xx)[-i])); olcekVar <- sapply(1:k, function(i) sum(C[-i, -i]))
  rDrop <- sapply(1:k, function(i) cor(xx[, i], rowSums(xx[, -i, drop = FALSE])))
  alfaSil <- if (k > 2) sapply(1:k, function(i) { Ci <- C[-i, -i]; (k - 1) / (k - 2) * (1 - sum(diag(Ci)) / sum(Ci)) }) else c(NA, NA)
  ust <- function(v) list(ortalama = mean(v), minimum = min(v), maksimum = max(v), aralik = max(v) - min(v), oran = max(v) / min(v), varyans = var(v))
  al <- 1 - duzey
  list(matris = sayisalMatris(x), n = n, k = k, alfa = a$total$raw_alpha, std = a$total$std.alpha, rbar = a$total$average_r,
       feldtAlt = 1 - (1 - a$total$raw_alpha) * qf(1 - al / 2, n - 1, (n - 1) * (k - 1)),
       feldtUst = 1 - (1 - a$total$raw_alpha) * qf(al / 2, n - 1, (n - 1) * (k - 1)),
       psychFeldt = c(a$feldt$lower.ci, a$feldt$upper.ci),
       rDropPsych = unname(a$item.stats$r.drop), rDrop = rDrop, smc = unname(smc(R)),
       alfaSilPsych = if (k > 2) unname(a$alpha.drop$raw_alpha) else NULL, alfaSil = alfaSil,
       ortalama = unname(colMeans(xx)), ss = unname(apply(xx, 2, sd)), olcekOrt = olcekOrt, olcekVar = olcekVar,
       olcekVaryans = var(rowSums(xx)),
       ozet = list(ortalamalar = ust(colMeans(xx)), varyanslar = ust(diag(C)), kovaryanslar = ust(C[lower.tri(C)]), korelasyonlar = ust(R[lower.tri(R)])))
}
data(bfi, package = "psych")
bfiA <- as.matrix(bfi[1:250, paste0("A", 1:5)])
set.seed(8)
lt <- rnorm(60)
lik <- sapply(1:6, function(j) pmin(5, pmax(1, round(3 + 0.6 * j / 3 * lt + rnorm(60, 0, 0.9)))))
lik[c(2, 9), c(1, 4)] <- NA
alfa <- list(bfi = alfaVaka(bfiA), likert = alfaVaka(lik, 0.9), ikiMadde = alfaVaka(lik[, 1:2]), ucMadde = alfaVaka(lik[, c(1, 3, 6)]))

set.seed(4)
yetenek <- rnorm(150); zorluk <- seq(-1.5, 1.5, length.out = 10)
ikili <- sapply(zorluk, function(b) as.integer(runif(150) < plogis(1.3 * (yetenek - b))))
krElle <- function(x) {
  k <- ncol(x); n <- nrow(x); p <- colMeans(x); T <- rowSums(x); v <- var(T) * (n - 1) / n; M <- mean(T)
  list(kr20 = k / (k - 1) * (1 - sum(p * (1 - p)) / v), kr21 = k / (k - 1) * (1 - M * (k - M) / (k * v)),
       alfa = psych::alpha(as.data.frame(x), warnings = FALSE)$total$raw_alpha, p = unname(p), v = v, M = M)
}
kr <- c(list(matris = sayisalMatris(ikili)), krElle(ikili))

yariElle <- function(x, k2 = floor(ncol(x) / 2)) {
  ham <- sayisalMatris(x); x <- na.omit(x); k <- ncol(x); k1 <- k - k2
  A <- x[, 1:k1, drop = FALSE]; B <- x[, (k1 + 1):k, drop = FALSE]
  t1 <- rowSums(A); t2 <- rowSums(B); r <- cor(t1, t2); cc <- k1 * k2 / k^2
  af <- function(P) if (ncol(P) < 2) NA else { C <- cov(P); ncol(P) / (ncol(P) - 1) * (1 - sum(diag(C)) / sum(C)) }
  list(matris = ham, k1 = k1, k2 = k2, r = r, sbEsit = 2 * r / (1 + r), sbEsitOlmayan = (-r^2 + sqrt(r^4 + 4 * r^2 * (1 - r^2) * cc)) / (2 * (1 - r^2) * cc),
       guttman = 2 * (1 - (var(t1) + var(t2)) / var(t1 + t2)), alfa1 = af(A), alfa2 = af(B), m1 = mean(t1), m2 = mean(t2), v1 = var(t1), v2 = var(t2))
}
yari <- list(likert = yariElle(lik), bfi = yariElle(bfiA[, 2:5]), tek = yariElle(cbind(lik, lt * 2 + 3)), ozel = yariElle(lik, 2))

# Omega: tek faktörlü ML. factanal sıkı toleransla + ψ_i = 1 − λ_i² sabit noktasına yineleme (kesin çözüm);
# lavaan::cfa ve MBESS::ci.reliability ile de karşılaştırılır (onların optimizasyon toleransı ~1e-6).
omegaVaka <- function(x) {
  xx <- na.omit(x); R <- cor(xx); s <- apply(xx, 2, sd); p <- ncol(xx)
  fa1 <- factanal(covmat = R, factors = 1, n.obs = nrow(xx), control = list(opt = list(factr = 1, maxit = 10000)))
  psi <- unname(fa1$uniquenesses)
  lam <- function(psi) { sc <- 1 / sqrt(psi); e <- eigen(R * outer(sc, sc), symmetric = TRUE); sqrt(psi) * e$vectors[, 1] * sqrt(max(e$values[1] - 1, 0)) }
  for (i in 1:20000) { l <- lam(psi); yeni <- 1 - l^2; if (max(abs(yeni - psi)) < 1e-15) break; psi <- yeni }
  l <- lam(psi); if (sum(l) < 0) l <- -l
  L <- l * s; T <- psi * s^2
  df <- as.data.frame(xx); names(df) <- paste0("m", 1:p)
  fit <- lavaan::cfa(paste("f =~", paste(names(df), collapse = "+")), data = df, std.lv = TRUE)
  pe <- lavaan::parameterEstimates(fit)
  lv <- pe$est[pe$op == "=~"]; th <- pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs != "f"]
  mb <- tryCatch(MBESS::ci.reliability(data = df, type = "omega", interval.type = "none")$est, error = function(e) NA)
  list(matris = sayisalMatris(x), omega = sum(L)^2 / (sum(L)^2 + sum(T)), omegaStd = sum(l)^2 / (sum(l)^2 + sum(psi)),
       yuk = l, psi = psi, lavaan = sum(lv)^2 / (sum(lv)^2 + sum(th)), mbess = mb, factanalPsi = unname(fa1$uniquenesses))
}
omega <- list(bfi = omegaVaka(bfiA[, 2:5]), likert = omegaVaka(lik))

sonuc <- list(kappa = kappaSonuc, fleiss = fleiss, icc = iccSonuc, bland = bland, ccc = ccc, yuzde = yuzde,
              anksiyete = sayisalMatris(as.matrix(anxiety)), alfa = alfa, kr = kr, yari = yari, omega = omega)
