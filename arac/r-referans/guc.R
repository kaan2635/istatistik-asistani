# paketler: pwr, Hmisc, powerMediation, powerSurvEpi, gsDesign
# Güç ve örneklem büyüklüğü analizi için referans değerler.
# pwr bilinmeyeni uniroot'un varsayılan toleransıyla (~1.2e-4) çözer. Kesin karşılaştırma için aynı pwr güç
# fonksiyonu burada uniroot(tol = 1e-11) ile yeniden çözülür ("kesin" sütunlar); pwr'ın kendi çıktısı da ("pwr"
# sütunları) saklanır.
library(pwr)
izgara <- function(...) expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
kes <- function(f, aralik) uniroot(f, aralik, tol = 1e-11, maxiter = 10000)$root
satirla <- function(g, f) do.call(rbind, lapply(seq_len(nrow(g)), function(i) cbind(g[i, , drop = FALSE], as.data.frame(f(g[i, ])))))
dAraligi <- function(alt) switch(alt, two.sided = c(1e-7, 10), less = c(-10, 5), greater = c(-5, 10))
yonlu <- function(d, alt) if (alt == "less") -d else d

# ---------- t testleri ----------
tg <- izgara(tip = c("one.sample", "two.sample", "paired"), d = c(0.2, 0.5, 0.8, 1.5), alt = c("two.sided", "greater", "less"), alfa = c(0.05, 0.01), n = c(8, 30, 100))
tGuc <- satirla(tg, function(r) list(guc = pwr.t.test(n = r$n, d = yonlu(r$d, r$alt), sig.level = r$alfa, type = r$tip, alternative = r$alt)$power))

tn <- izgara(tip = c("one.sample", "two.sample", "paired"), d = c(0.2, 0.5, 0.8, 1.5), alt = c("two.sided", "greater", "less"), alfa = c(0.05, 0.01), guc = c(0.8, 0.95))
tN <- satirla(tn, function(r) {
  d <- yonlu(r$d, r$alt)
  f <- function(x) pwr.t.test(n = x, d = d, sig.level = r$alfa, type = r$tip, alternative = r$alt)$power - r$guc
  nk <- kes(f, c(2 + 1e-10, 1e7))
  nt <- ceiling(nk)
  list(nKesin = nk, nPwr = pwr.t.test(d = d, power = r$guc, sig.level = r$alfa, type = r$tip, alternative = r$alt)$n,
       nTam = nt, gucTam = f(nt) + r$guc)
})

td <- izgara(tip = c("one.sample", "two.sample", "paired"), n = c(6, 25, 200), alt = c("two.sided", "greater", "less"), alfa = c(0.05, 0.01), guc = c(0.8, 0.95))
tD <- satirla(td, function(r) {
  f <- function(x) pwr.t.test(n = r$n, d = x, sig.level = r$alfa, type = r$tip, alternative = r$alt)$power - r$guc
  list(dKesin = kes(f, dAraligi(r$alt)))
})

# Farklı grup büyüklükleri (pwr.t2n.test) ve sabit oran n2 = k·n1
t2 <- izgara(n1 = c(10, 25, 60), n2 = c(5, 40), d = c(0.3, 0.9), alt = c("two.sided", "greater", "less"), alfa = 0.05, guc = c(0.8, 0.9))
t2n <- satirla(t2, function(r) {
  d <- yonlu(r$d, r$alt)
  g <- function(a, b, dd) pwr.t2n.test(n1 = a, n2 = b, d = dd, sig.level = r$alfa, alternative = r$alt)$power
  n1k <- tryCatch(kes(function(x) g(x, r$n2, d) - r$guc, c(2 + 1e-10, 1e7)), error = function(e) NA)
  n2k <- tryCatch(kes(function(x) g(r$n1, x, d) - r$guc, c(2 + 1e-10, 1e7)), error = function(e) NA)
  list(guc0 = g(r$n1, r$n2, d), n1Kesin = n1k, n2Kesin = n2k,
       dKesin = kes(function(x) g(r$n1, r$n2, x) - r$guc, dAraligi(r$alt)))
})
tk <- izgara(k = c(0.5, 2, 3), d = c(0.3, 0.8), alt = c("two.sided", "greater"), alfa = 0.05, guc = c(0.8, 0.9))
tOran <- satirla(tk, function(r) {
  g <- function(a, b) pwr.t2n.test(n1 = a, n2 = b, d = r$d, sig.level = r$alfa, alternative = r$alt)$power
  nk <- kes(function(x) g(x, r$k * x) - r$guc, c(max(2, 2 / r$k) + 1e-10, 1e7))
  n1 <- ceiling(nk); n2 <- ceiling(r$k * nk)
  list(n1Kesin = nk, n1 = n1, n2 = n2, gucTam = g(n1, n2))
})

# ---------- A.R.E. (Mann-Whitney, Wilcoxon): güç(n) = t testi gücü(ARE·n) ----------
are <- c(normal = 3 / pi, lojistik = pi^2 / 9, laplace = 1.5, enKotu = 0.864)
ag <- izgara(ebeveyn = names(are), test = c("mw", "wilcoxon"), d = c(0.4, 0.8), alt = c("two.sided", "greater"), alfa = 0.05, guc = 0.8, n = 30)
areT <- satirla(ag, function(r) {
  A <- are[[r$ebeveyn]]
  tip <- if (r$test == "mw") "two.sample" else "one.sample"
  g <- function(x) pwr.t.test(n = x, d = r$d, sig.level = r$alfa, type = tip, alternative = r$alt)$power
  nt <- kes(function(x) g(x) - r$guc, c(2 + 1e-10, 1e7))
  list(guc0 = g(A * r$n), nKesin = nt / A, nTam = ceiling(nt / A), gucTam = g(A * ceiling(nt / A)),
       dKesin = kes(function(x) pwr.t.test(n = A * r$n, d = x, sig.level = r$alfa, type = tip, alternative = r$alt)$power - r$guc, dAraligi(r$alt)))
})
# G*Power kılavuzundaki örnek: tek yönlü, d = 0.5, α = 0.05, güç 0.80, normal ebeveyn → n = 53 / grup, δ = 2.5152354, sd = 99.2225438
gpowerMW <- list(n = 53, ncp = 0.5 * sqrt(53 * 53 * (3 / pi) / 106), sd = 106 * 3 / pi - 2)

# ---------- ANOVA ----------
an <- izgara(k = c(2, 3, 5), f = c(0.1, 0.25, 0.4), alfa = c(0.05, 0.01), guc = c(0.8, 0.9), n = c(5, 20, 60))
an <- an[an$guc == 0.8 | an$n == 20, ]
# R'nin merkezi olmayan F'si (pnbeta, AS 226) errmax = 1e-9 ile durur; güç ~1e-9 mutlak hatalı olabilir.
# Bu yüzden ayrıca Poisson karışımı serisiyle (her terim pbeta, ~1e-15) kesin güç ve kökler hesaplanır.
pfSeriUst <- function(x, d1, d2, ncp) {
  j <- 0:max(300, ceiling(ncp / 2 + 30 * sqrt(ncp / 2 + 1) + 100))
  y <- d1 * x / (d1 * x + d2)
  sum(dpois(j, ncp / 2) * pbeta(y, d1 / 2 + j, d2 / 2, lower.tail = FALSE))
}
anova <- satirla(an, function(r) {
  g <- function(n, f) pwr.anova.test(k = r$k, n = n, f = f, sig.level = r$alfa)$power
  gs <- function(n, f) { d1 <- r$k - 1; d2 <- (n - 1) * r$k; pfSeriUst(qf(r$alfa, d1, d2, lower.tail = FALSE), d1, d2, r$k * n * f^2) }
  nk <- kes(function(x) gs(x, r$f) - r$guc, c(2 + 1e-10, 1e7))
  list(guc0 = g(r$n, r$f), guc0Seri = gs(r$n, r$f), nKesin = nk, nPwr = pwr.anova.test(k = r$k, f = r$f, power = r$guc, sig.level = r$alfa)$n,
       gucTam = gs(ceiling(nk), r$f), fKesin = kes(function(x) gs(r$n, x) - r$guc, c(1e-7, 100)))
})

# ---------- Korelasyon ----------
ko <- izgara(r = c(0.1, 0.3, 0.5), alt = c("two.sided", "greater", "less"), alfa = c(0.05, 0.01), guc = c(0.8, 0.9), n = c(10, 50, 200))
ko <- ko[ko$guc == 0.8 | ko$n == 50, ]
korelasyon <- satirla(ko, function(r) {
  rr <- yonlu(r$r, r$alt)
  g <- function(n, x) pwr.r.test(n = n, r = x, sig.level = r$alfa, alternative = r$alt)$power
  nk <- kes(function(x) g(x, rr) - r$guc, c(4 + 1e-10, 1e7))
  ra <- if (r$alt == "two.sided") c(1e-10, 1 - 1e-10) else c(-1 + 1e-10, 1 - 1e-10)
  list(guc0 = g(r$n, rr), nKesin = nk, nPwr = pwr.r.test(r = rr, power = r$guc, sig.level = r$alfa, alternative = r$alt)$n,
       gucTam = g(ceiling(nk), rr), rKesin = kes(function(x) g(r$n, x) - r$guc, ra))
})

# ---------- Ki-kare ----------
kk <- izgara(w = c(0.1, 0.3, 0.5), df = c(1, 3, 6), alfa = c(0.05, 0.01), guc = c(0.8, 0.9), N = c(20, 100, 500))
kk <- kk[kk$guc == 0.8 | kk$N == 100, ]
kikare <- satirla(kk, function(r) {
  g <- function(N, w) pwr.chisq.test(w = w, N = N, df = r$df, sig.level = r$alfa)$power
  nk <- kes(function(x) g(x, r$w) - r$guc, c(1 + 1e-10, 1e7))
  list(guc0 = g(r$N, r$w), nKesin = nk, nPwr = pwr.chisq.test(w = r$w, df = r$df, power = r$guc, sig.level = r$alfa)$N,
       gucTam = g(ceiling(nk), r$w), wKesin = kes(function(x) g(r$N, x) - r$guc, c(1e-10, 100)))
})

# ---------- Oranlar: arksinüs ----------
ho <- izgara(p1 = c(0.1, 0.5), p2 = c(0.3, 0.65, 0.9), alt = c("two.sided", "greater", "less"), alfa = 0.05, guc = c(0.8, 0.9), n = c(20, 150))
ho$h <- ES.h(ho$p1, ho$p2)
ho <- ho[!(ho$alt == "greater" & ho$h < 0) & !(ho$alt == "less" & ho$h > 0), ]
ikiOranH <- satirla(ho, function(r) {
  g <- function(n, h) pwr.2p.test(h = h, n = n, sig.level = r$alfa, alternative = r$alt)$power
  nk <- kes(function(x) g(x, r$h) - r$guc, c(2 + 1e-10, 1e7))
  list(guc0 = g(r$n, r$h), nKesin = nk, gucTam = g(ceiling(nk), r$h),
       hKesin = kes(function(x) g(r$n, x) - r$guc, dAraligi(r$alt)))
})
h2 <- izgara(h = c(0.25, 0.6), n1 = c(30, 80), n2 = c(15, 120), alt = c("two.sided", "greater"), alfa = 0.05, guc = 0.8)
ikiOranH2n <- satirla(h2, function(r) {
  g <- function(a, b, h) pwr.2p2n.test(h = h, n1 = a, n2 = b, sig.level = r$alfa, alternative = r$alt)$power
  list(guc0 = g(r$n1, r$n2, r$h),
       n1Kesin = tryCatch(kes(function(x) g(x, r$n2, r$h) - r$guc, c(2 + 1e-10, 1e7)), error = function(e) NA))
})
hk <- izgara(p1 = 0.3, p2 = c(0.45, 0.5), k = c(0.5, 2), alt = "two.sided", alfa = 0.05, guc = 0.8)
ikiOranHOran <- satirla(hk, function(r) {
  h <- ES.h(r$p1, r$p2)
  g <- function(a, b) pwr.2p2n.test(h = h, n1 = a, n2 = b, sig.level = r$alfa, alternative = r$alt)$power
  nk <- kes(function(x) g(x, r$k * x) - r$guc, c(max(2, 2 / r$k) + 1e-10, 1e7))
  list(n1Kesin = nk, n1 = ceiling(nk), n2 = ceiling(r$k * nk), gucTam = g(ceiling(nk), ceiling(r$k * nk)))
})

# ---------- Oranlar: normal yaklaşım (power.prop.test: strict = FALSE) ----------
pp <- izgara(p1 = c(0.1, 0.5), p2 = c(0.25, 0.75), alt = c("two.sided", "one.sided"), alfa = c(0.05, 0.01), guc = c(0.8, 0.9), n = c(40, 200))
pp <- pp[pp$guc == 0.8 | pp$n == 40, ]
ikiOranNormal <- satirla(pp, function(r) {
  list(guc0 = power.prop.test(n = r$n, p1 = r$p1, p2 = r$p2, sig.level = r$alfa, alternative = r$alt)$power,
       nKesin = power.prop.test(p1 = r$p1, p2 = r$p2, power = r$guc, sig.level = r$alfa, alternative = r$alt, tol = 1e-12)$n,
       p2Kesin = power.prop.test(n = r$n, p1 = r$p1, power = r$guc, sig.level = r$alfa, alternative = r$alt, tol = 1e-12)$p2)
})
# Farklı grup oranı (Hmisc::bsamsize: Fleiss, Tytun & Ury, süreklilik düzeltmesiz; n2 = k·n1)
bs <- izgara(p1 = c(0.2, 0.6), p2 = c(0.35, 0.4), k = c(0.5, 1, 3), alfa = c(0.05, 0.01), guc = c(0.8, 0.9))
ikiOranBsam <- satirla(bs, function(r) {
  s <- Hmisc::bsamsize(r$p1, r$p2, fraction = 1 / (1 + r$k), alpha = r$alfa, power = r$guc)
  list(n1Kesin = unname(s["n1"]), n2Kesin = unname(s["n2"]))
})
# Süreklilik düzeltmesi (Fleiss, Levin & Paik 2003, eş. 4.14 — elle)
fleissN <- function(p1, p2, k, alfa, guc, yon) {
  za <- qnorm(1 - alfa / yon); zb <- qnorm(guc)
  pb <- (p1 + k * p2) / (1 + k)
  n <- (za * sqrt(pb * (1 - pb) * (1 + 1 / k)) + zb * sqrt(p1 * (1 - p1) + p2 * (1 - p2) / k))^2 / (p1 - p2)^2
  c(n, n / 4 * (1 + sqrt(1 + 2 * (k + 1) / (k * n * abs(p1 - p2))))^2)
}
fleissGuc <- function(p1, p2, n1, k, alfa, yon, cc) {
  za <- qnorm(1 - alfa / yon)
  pb <- (p1 + k * p2) / (1 + k)
  dl <- abs(p1 - p2) - if (cc) (1 + 1 / k) / (2 * n1) else 0
  pnorm((dl * sqrt(n1) - za * sqrt(pb * (1 - pb) * (1 + 1 / k))) / sqrt(p1 * (1 - p1) + p2 * (1 - p2) / k))
}
fc <- izgara(p1 = c(0.15, 0.5), p2 = c(0.3, 0.8), k = c(1, 2), yon = c(1, 2), alfa = 0.05, guc = c(0.8, 0.9), n = 60)
ikiOranSureklilik <- satirla(fc, function(r) {
  nn <- fleissN(r$p1, r$p2, r$k, r$alfa, r$guc, r$yon)
  list(nKesin = nn[1], nKesinCC = nn[2], guc0 = fleissGuc(r$p1, r$p2, r$n, r$k, r$alfa, r$yon, FALSE),
       guc0CC = fleissGuc(r$p1, r$p2, r$n, r$k, r$alfa, r$yon, TRUE),
       gucCCnKesin = fleissGuc(r$p1, r$p2, nn[2], r$k, r$alfa, r$yon, TRUE))
})

# ---------- Tek oran ----------
po <- izgara(p = c(0.3, 0.65), p0 = 0.5, alt = c("two.sided", "greater", "less"), alfa = c(0.05, 0.01), guc = 0.8, n = c(30, 120))
po$h <- ES.h(po$p, po$p0)
po <- po[!(po$alt == "greater" & po$h < 0) & !(po$alt == "less" & po$h > 0), ]
tekOran <- satirla(po, function(r) {
  g <- function(n, h) pwr.p.test(h = h, n = n, sig.level = r$alfa, alternative = r$alt)$power
  nk <- kes(function(x) g(x, r$h) - r$guc, c(2 + 1e-10, 1e7))
  list(guc0 = g(r$n, r$h), nKesin = nk, gucTam = g(ceiling(nk), r$h), hKesin = kes(function(x) g(r$n, x) - r$guc, dAraligi(r$alt)))
})

# ---------- Çoklu regresyon (f²) ----------
fr <- izgara(u = c(1, 3, 8), f2 = c(0.02, 0.15, 0.35), alfa = c(0.05, 0.01), guc = c(0.8, 0.9), v = c(20, 80, 300))
fr <- fr[fr$guc == 0.8 | fr$v == 80, ]
regresyon <- satirla(fr, function(r) {
  g <- function(v, f2) pwr.f2.test(u = r$u, v = v, f2 = f2, sig.level = r$alfa)$power
  gs <- function(v, f2) pfSeriUst(qf(r$alfa, r$u, v, lower.tail = FALSE), r$u, v, f2 * (r$u + v + 1))
  vk <- kes(function(x) gs(x, r$f2) - r$guc, c(1 + 1e-10, 1e7))
  list(guc0 = g(r$v, r$f2), guc0Seri = gs(r$v, r$f2), vKesin = vk, vPwr = pwr.f2.test(u = r$u, f2 = r$f2, power = r$guc, sig.level = r$alfa)$v,
       gucTam = gs(ceiling(vk), r$f2), f2Kesin = kes(function(x) gs(r$v, x) - r$guc, c(1e-7, 100)))
})

# ---------- Lojistik regresyon (Hsieh 1998; powerMediation) ----------
lb <- izgara(p1 = c(0.1, 0.4), p2 = c(0.2, 0.6), B = c(0.3, 0.5), alfa = 0.05, guc = c(0.8, 0.9), n = 150)
lojistikIkili <- satirla(lb, function(r) {
  za <- qnorm(1 - r$alfa / 2); zb <- qnorm(r$guc)
  p <- (1 - r$B) * r$p1 + r$B * r$p2
  nk <- (za * sqrt(p * (1 - p) / r$B) + zb * sqrt(r$p1 * (1 - r$p1) + r$p2 * (1 - r$p2) * (1 - r$B) / r$B))^2 / ((r$p1 - r$p2)^2 * (1 - r$B))
  list(nKesin = nk, nTam = powerMediation::SSizeLogisticBin(r$p1, r$p2, r$B, r$alfa, r$guc),
       guc0 = powerMediation::powerLogisticBin(r$n, r$p1, r$p2, r$B, r$alfa))
})
lc <- izgara(p1 = c(0.1, 0.5), OR = c(1.3, 2, 0.6), alfa = c(0.05, 0.01), guc = c(0.8, 0.95), n = 200)
lojistikSurekli <- satirla(lc, function(r) {
  nk <- (qnorm(1 - r$alfa / 2) + qnorm(r$guc))^2 / (r$p1 * (1 - r$p1) * log(r$OR)^2)
  list(nKesin = nk, nTam = powerMediation::SSizeLogisticCon(r$p1, r$OR, r$alfa, r$guc),
       guc0 = powerMediation::powerLogisticCon(r$n, r$p1, r$OR, r$alfa))
})

# ---------- Sağkalım ----------
sv <- izgara(hr = c(0.5, 0.7, 1.5), oran = c(1, 2), alfa = c(0.05, 0.01), guc = c(0.8, 0.9), d = 120)
sagkalimSchoenfeld <- satirla(sv, function(r) {
  # gsDesign: ratio = deney/kontrol = n1/n2 = 1/oran; sided = 2 → qnorm(1 − α/2).
  # nEvents gücü yalnızca HR < 1 yönünde hesaplar; iki yönlü (|ln HR|) güç için HR > 1 ise 1/HR verilir.
  list(olayKesin = gsDesign::nEvents(hr = r$hr, alpha = r$alfa, beta = 1 - r$guc, ratio = 1 / r$oran, sided = 2),
       guc0 = gsDesign::nEvents(hr = min(r$hr, 1 / r$hr), alpha = r$alfa, beta = 1 - r$guc, ratio = 1 / r$oran, sided = 2, n = r$d))
})
fv <- izgara(hr = c(0.5, 0.7, 1.5), oran = c(1, 2), alfa = 0.05, guc = c(0.8, 0.9), pE = 0.3, pC = 0.45, nE = 150, nC = 120)
sagkalimFreedman <- satirla(fv, function(r) {
  k <- 1 / r$oran # powerSurvEpi: k = nE/nC
  za <- qnorm(1 - r$alfa / 2); zb <- qnorm(r$guc)
  m <- ((k * r$hr + 1) / (r$hr - 1))^2 * (za + zb)^2 / k
  s <- powerSurvEpi::ssizeCT.default(power = r$guc, k = k, pE = r$pE, pC = r$pC, RR = r$hr, alpha = r$alfa)
  list(olayKesin = m, sE = unname(s["nE"]), sC = unname(s["nC"]),
       guc0 = powerSurvEpi::powerCT.default(nE = r$nE, nC = r$nC, pE = r$pE, pC = r$pC, RR = r$hr, alpha = r$alfa))
})

# ---------- Etki büyüklüğü yardımcıları ----------
etki <- list(
  h = ES.h(0.65, 0.45), w1 = ES.w1(c(0.25, 0.25, 0.25, 0.25), c(0.4, 0.2, 0.2, 0.2)),
  w2 = ES.w2(matrix(c(0.1, 0.2, 0.15, 0.25, 0.1, 0.2), 2) / sum(c(0.1, 0.2, 0.15, 0.25, 0.1, 0.2)))
)

sonuc <- list(
  tGuc = tGuc, tN = tN, tD = tD, t2n = t2n, tOran = tOran, areT = areT, gpowerMW = gpowerMW,
  anova = anova, korelasyon = korelasyon, kikare = kikare,
  ikiOranH = ikiOranH, ikiOranH2n = ikiOranH2n, ikiOranHOran = ikiOranHOran,
  ikiOranNormal = ikiOranNormal, ikiOranBsam = ikiOranBsam, ikiOranSureklilik = ikiOranSureklilik,
  tekOran = tekOran, regresyon = regresyon, lojistikIkili = lojistikIkili, lojistikSurekli = lojistikSurekli,
  sagkalimSchoenfeld = sagkalimSchoenfeld, sagkalimFreedman = sagkalimFreedman, etki = etki
)
