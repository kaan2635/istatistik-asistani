# paketler: psych, GPArotation, lavaan
# Açımlayıcı faktör analizi referansları (src/cekirdek/faktor.js).
# SPSS'e özgü kurallar (PAF yakınsama ölçütü, promax'ta Kaiser normalizasyonu, sıralama/işaret) burada açıkça
# kodlanır; psych / GPArotation / stats sonuçlarıyla çapraz denetlenir.
library(psych)
library(GPArotation)
options(mc.cores = 1) # webR fork desteklemez; psych::fa.parallel mclapply kullanır

# ---- veri ----
bfi25 <- psych::bfi[complete.cases(psych::bfi[, 1:25]), 1:25]
write.csv(bfi25, "/cikti/fa-bfi.csv", row.names = FALSE)
bfi25 <- as.matrix(read.csv("/cikti/fa-bfi.csv"))
hs <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
write.csv(hs, "/cikti/fa-holzinger.csv", row.names = FALSE)
hs <- as.matrix(read.csv("/cikti/fa-holzinger.csv"))
mt <- as.matrix(mtcars[, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec")])
harman <- unname(Harman74.cor$cov)
thurstone <- unname(as.matrix(psych::Thurstone))

# ---- yardımcılar ----
isaret <- function(A) { s <- sign(colSums(A)); s[s == 0] <- 1; A %*% diag(s, ncol(A)) }
ss <- function(A) colSums(A^2)

# SPSS sıralama/işaret kuralı: eğikte yapı matrisi kareler toplamı, işaret örüntü sütun toplamından
kanonik <- function(P, Phi = NULL, M = NULL) {
  k <- ncol(P)
  if (is.null(Phi)) Phi <- diag(k)
  o <- order(ss(P %*% Phi), decreasing = TRUE)
  sg <- sign(colSums(P))[o]; sg[sg == 0] <- 1
  D <- diag(sg, k)
  r <- list(P = unname(P[, o, drop = FALSE] %*% D), Phi = unname(D %*% Phi[o, o] %*% D))
  r$S <- r$P %*% r$Phi
  r$ss <- ss(r$S)
  if (!is.null(M)) r$M <- unname(M[, o, drop = FALSE] %*% D)
  r
}

pca <- function(R, m) {
  e <- eigen(R, symmetric = TRUE)
  isaret(e$vectors[, 1:m, drop = FALSE] %*% diag(sqrt(e$values[1:m]), m))
}

# SPSS PAF: SMC başlangıç, en büyük mutlak ortak varyans değişimi < eps ya da maxit
spss_paf <- function(R, m, maxit = 25, eps = 0.001) {
  h <- 1 - 1 / diag(solve(R))
  for (it in 1:maxit) {
    Rs <- R; diag(Rs) <- h
    e <- eigen(Rs, symmetric = TRUE)
    A <- e$vectors[, 1:m, drop = FALSE] %*% diag(sqrt(e$values[1:m]), m)
    hn <- rowSums(A^2)
    fark <- max(abs(hn - h)); h <- hn
    if (fark < eps) break
  }
  list(A = isaret(A), h = h, it = it, yakinsadi = fark < eps, fark = fark)
}

# ML: factanal sıkı yakınsamayla; yükler kanonik sırada (Ψ^-1/2 R Ψ^-1/2 özdeğerlerine göre)
ml <- function(R, m, n) {
  f <- factanal(covmat = R, factors = m, n.obs = n, rotation = "none",
                control = list(nstart = 1, opt = list(factr = 1, pgtol = 0, maxit = 100000)))
  psi <- f$uniquenesses
  e <- eigen(diag(1 / sqrt(psi)) %*% R %*% diag(1 / sqrt(psi)), symmetric = TRUE)
  A <- diag(sqrt(psi)) %*% e$vectors[, 1:m, drop = FALSE] %*% diag(sqrt(pmax(e$values[1:m] - 1, 0)), m)
  list(A = isaret(A), psi = unname(psi), chi = unname(f$STATISTIC), sd = unname(f$dof), p = unname(f$PVAL),
       amac = unname(f$criteria["objective"]))
}

# Döndürmeler. GPArotation'ın ölçüt/gradyan fonksiyonları (vgQ.*) kullanılır. GPForth/GPFoblq'nun Armijo
# koşulu ölçüt farkı yuvarlama düzeyine inince (gradyan normu ~1e-8) ilerleyemez; bu yüzden GPArotation sonucu
# (eps = 1e-6) aynı ölçütle, yuvarlama bölgesinde gradyan normunu küçülten adımları kabul eden GPA ile
# makine duyarlılığına kadar parlatılır.
vgq <- function(method) get(paste0("vgQ.", method), asNamespace("GPArotation"))
parlat_dik <- function(A, Tm, method, args = NULL, eps = 1e-13, maxit = 5000) {
  kr <- function(Tm) do.call(vgq(method), c(list(A %*% Tm), args))
  iz <- function(Tm, G) { M <- crossprod(Tm, G); G - Tm %*% ((M + t(M)) / 2) }
  v <- kr(Tm); G <- crossprod(A, v$Gq); Gp <- iz(Tm, G); s <- sqrt(sum(Gp^2)); al <- 1
  for (it in 1:maxit) {
    if (s < eps) break
    al <- 2 * al; ok <- FALSE
    for (i in 0:40) {
      sv <- svd(Tm - al * Gp); Tt <- sv$u %*% t(sv$v); vt <- kr(Tt)
      if (abs(vt$f - v$f) <= 1e-12 * max(1, abs(v$f))) {
        Gt <- crossprod(A, vt$Gq); Gpt <- iz(Tt, Gt); st <- sqrt(sum(Gpt^2)); if (st < s) { ok <- TRUE; break }
      } else if (vt$f < v$f - 0.5 * s^2 * al) {
        Gt <- crossprod(A, vt$Gq); Gpt <- iz(Tt, Gt); st <- sqrt(sum(Gpt^2)); ok <- TRUE; break
      }
      al <- al / 2
    }
    if (!ok) break
    Tm <- Tt; v <- vt; G <- Gt; Gp <- Gpt; s <- st
  }
  list(L = A %*% Tm, Th = Tm, s = s)
}
parlat_egik <- function(A, Tm, method, args = NULL, eps = 1e-13, maxit = 5000) {
  orn <- function(Tm) A %*% t(solve(Tm))
  kr <- function(L) do.call(vgq(method), c(list(L), args))
  gr <- function(L, Gq, Tm) -t(t(L) %*% Gq %*% solve(Tm))
  iz <- function(Tm, G) G - Tm %*% diag(colSums(Tm * G), ncol(Tm))
  L <- orn(Tm); v <- kr(L); G <- gr(L, v$Gq, Tm); Gp <- iz(Tm, G); s <- sqrt(sum(Gp^2)); al <- 1
  for (it in 1:maxit) {
    if (s < eps) break
    al <- 2 * al; ok <- FALSE
    for (i in 0:40) {
      X <- Tm - al * Gp; Tt <- X %*% diag(1 / sqrt(colSums(X^2)), ncol(X)); Lt <- orn(Tt); vt <- kr(Lt)
      if (abs(vt$f - v$f) <= 1e-12 * max(1, abs(v$f))) {
        Gt <- gr(Lt, vt$Gq, Tt); Gpt <- iz(Tt, Gt); st <- sqrt(sum(Gpt^2)); if (st < s) { ok <- TRUE; break }
      } else if (v$f - vt$f > 0.5 * s^2 * al) {
        Gt <- gr(Lt, vt$Gq, Tt); Gpt <- iz(Tt, Gt); st <- sqrt(sum(Gpt^2)); ok <- TRUE; break
      }
      al <- al / 2
    }
    if (!ok) break
    Tm <- Tt; L <- Lt; v <- vt; G <- Gt; Gp <- Gpt; s <- st
  }
  list(L = L, Th = Tm, Phi = t(Tm) %*% Tm, s = s)
}
dik <- function(A, method, args = NULL, kaiser = TRUE) {
  h <- if (kaiser) sqrt(rowSums(A^2)) else rep(1, nrow(A))
  g <- GPForth(A / h, method = method, methodArgs = args, normalize = FALSE, eps = 1e-6, maxit = 10000)
  r <- parlat_dik(A / h, g$Th, method, args)
  list(L = r$L * h, Th = r$Th, gpa = g$loadings * h, s = r$s)
}
egik <- function(A, method, args = NULL, kaiser = TRUE) {
  h <- if (kaiser) sqrt(rowSums(A^2)) else rep(1, nrow(A))
  g <- GPFoblq(A / h, method = method, methodArgs = args, normalize = FALSE, eps = 1e-6, maxit = 10000)
  r <- parlat_egik(A / h, g$Th, method, args)
  list(L = r$L * h, Phi = r$Phi, gpa = g$loadings * h, gpaPhi = g$Phi, s = r$s)
}

dondur <- function(A, yontem, kaiser = TRUE, kappa = 4) {
  m <- ncol(A); p <- nrow(A)
  if (yontem %in% c("varimax", "quartimax", "equamax")) {
    r <- if (yontem == "equamax") dik(A, "cf", list(kappa = m / (2 * p)), kaiser) else dik(A, yontem, NULL, kaiser)
    k <- kanonik(r$L, NULL, r$Th)
    k$gpa <- kanonik(r$gpa)$P; k$s <- r$s
    return(k)
  }
  if (yontem == "oblimin") {
    r <- egik(A, "oblimin", list(gam = 0), kaiser)
    k <- kanonik(r$L, r$Phi)
    k$gpa <- kanonik(r$gpa, r$gpaPhi)$P; k$s <- r$s
    return(k)
  }
  if (yontem %in% c("promax", "promax-r")) {
    # SPSS: tüm promax normalize yükler üzerinde, sonunda normalizasyon geri alınır.
    # R (stats::promax): varimax normalize, hedef ve regresyon normalize edilmemiş varimax yüklerinden.
    h <- if (kaiser) sqrt(rowSums(A^2)) else rep(1, p)
    v <- dik(A / h, "varimax", NULL, FALSE)
    V <- if (yontem == "promax") v$L else v$L * h
    Q <- V * abs(V)^(kappa - 1)
    U <- lm.fit(V, Q)$coefficients
    d <- diag(solve(t(U) %*% U)); U <- U %*% diag(sqrt(d))
    P <- V %*% U
    if (yontem == "promax") P <- P * h
    return(kanonik(P, solve(t(U) %*% U)))
  }
  stop("yontem")
}

artik <- function(R, A) {
  E <- R - A %*% t(A)
  e <- E[upper.tri(E)]
  list(sayi = sum(abs(e) > 0.05), yuzde = 100 * mean(abs(e) > 0.05), rmsr = sqrt(mean(e^2)))
}

# [[ ]]: $ kısmi eşleşme yapar (r$s, r$ss'i döndürürdü)
paket <- function(r) list(P = r[["P"]], Phi = r[["Phi"]], S = r[["S"]], ss = r[["ss"]], M = r[["M"]], gpa = r[["gpa"]], s = r[["s"]])

kmo_ref <- function(R, n) {
  k <- psych::KMO(R)
  Ri <- solve(R)
  b <- psych::cortest.bartlett(R, n = n)
  list(kmo = k$MSA, msa = unname(k$MSAi), aiv = unname(Ri / outer(diag(Ri), diag(Ri))),
       aic_dis = unname(k$Image), det = det(R), chi = b$chisq, sd = b$df, p = b$p.value)
}

map_ref <- function(R) {
  p <- ncol(R)
  e <- eigen(R, symmetric = TRUE)
  kare <- dort <- numeric(0)
  for (m in 0:(p - 1)) {
    C <- R
    if (m > 0) C <- R - e$vectors[, 1:m, drop = FALSE] %*% diag(e$values[1:m], m) %*% t(e$vectors[, 1:m, drop = FALSE])
    d <- diag(1 / sqrt(diag(C)))
    Rs <- d %*% C %*% d; diag(Rs) <- 0
    kare <- c(kare, sum(Rs^2) / (p * (p - 1)))
    dort <- c(dort, sum(Rs^4) / (p * (p - 1)))
  }
  vss <- psych::VSS(R, n = min(8, p - 1), rotate = "none", fm = "minres", n.obs = 200, plot = FALSE)
  list(kare = kare, dort = dort, psych_map = vss$map)
}

# ---- bfi (n = 2436, p = 25) ----
R <- cor(bfi25); n <- nrow(bfi25)
ozd <- eigen(R, symmetric = TRUE)$values
m_kaiser <- sum(ozd > 1)
pc <- pca(R, m_kaiser)
pc_psych <- psych::principal(R, nfactors = m_kaiser, rotate = "varimax")
paf <- spss_paf(R, 5)
paf_tam <- spss_paf(R, 5, maxit = 100000, eps = 1e-13)
paf_psych <- psych::fa(R, nfactors = 5, fm = "pa", rotate = "none", SMC = TRUE, max.iter = 100000, min.err = 1e-14, n.obs = n, warnings = FALSE)
mlb <- ml(R, 5, n)
A_paf <- paf$A
bfi_ref <- list(
  n = n, ozdeger = ozd, kmo = kmo_ref(R, n),
  pca = list(m = m_kaiser, A = pc, h2 = rowSums(pc^2), varimax = paket(dondur(pc, "varimax")),
             psych_varimax = paket(kanonik(unclass(pc_psych$loadings))),
             stats_varimax_varsayilan = paket(kanonik(unclass(stats::varimax(pc)$loadings))), artik = artik(R, pc)),
  paf = list(A = A_paf, h2 = paf$h, smc = 1 - 1 / diag(solve(R)), it = paf$it, fark = paf$fark,
             artik = artik(R, A_paf),
             varimax = paket(dondur(A_paf, "varimax")),
             varimax_stats = paket(kanonik(unclass(stats::varimax(A_paf, eps = 1e-10)$loadings))),
             quartimax = paket(dondur(A_paf, "quartimax")),
             equamax = paket(dondur(A_paf, "equamax")),
             oblimin = paket(dondur(A_paf, "oblimin")),
             promax = paket(dondur(A_paf, "promax")),
             promax_r = paket(dondur(A_paf, "promax-r")),
             promax_stats = paket(kanonik(unclass(stats::promax(A_paf, m = 4)$loadings),
                                          {U <- stats::promax(A_paf, m = 4)$rotmat; solve(t(U) %*% U)})),
             varimax_normsuz = paket(dondur(A_paf, "varimax", kaiser = FALSE)),
             oblimin_normsuz = paket(dondur(A_paf, "oblimin", kaiser = FALSE))),
  paf_tam = list(A = paf_tam$A, h2 = paf_tam$h, it = paf_tam$it,
                 psych = unclass(kanonik(unclass(paf_psych$loadings))$P), psych_h2 = unname(paf_psych$communality)),
  ml = c(mlb, list(varimax = paket(dondur(mlb$A, "varimax")), oblimin = paket(dondur(mlb$A, "oblimin")))),
  map = map_ref(R)
)
# Faktör puanları (PAF + varimax, PAF + oblimin)
pv <- dondur(A_paf, "varimax"); po <- dondur(A_paf, "oblimin")
puan <- function(P, Phi, yontem) {
  fs <- psych::factor.scores(bfi25, P, Phi = Phi, method = yontem)
  list(W = unname(fs$weights), ilk = unname(fs$scores[1:8, ]), son = unname(fs$scores[n, ]))
}
bfi_ref$puan <- list(
  varimax_thurstone = puan(pv$P, NULL, "Thurstone"),
  varimax_bartlett = puan(pv$P, NULL, "Bartlett"),
  varimax_anderson = puan(pv$P, NULL, "Anderson"),
  oblimin_thurstone = puan(po$P, po$Phi, "Thurstone"),
  oblimin_bartlett = puan(po$P, po$Phi, "Bartlett")
)

# ---- Harman74 (korelasyon, n = 145, p = 24) ----
R <- harman; n <- 145
pafh <- spss_paf(R, 4)
mlh <- ml(R, 4, n)
harman_ref <- list(
  R = R, n = n, ozdeger = eigen(R, symmetric = TRUE)$values, kmo = kmo_ref(R, n),
  paf = list(A = pafh$A, h2 = pafh$h, it = pafh$it, yakinsadi = pafh$yakinsadi, fark = pafh$fark,
             varimax = paket(dondur(pafh$A, "varimax")), promax = paket(dondur(pafh$A, "promax")),
             oblimin = paket(dondur(pafh$A, "oblimin")), artik = artik(R, pafh$A)),
  paf50 = {r <- spss_paf(R, 4, maxit = 50); list(A = r$A, h2 = r$h, it = r$it)},
  ml = c(mlh, list(varimax = paket(dondur(mlh$A, "varimax")), promax = paket(dondur(mlh$A, "promax")),
                   oblimin = paket(dondur(mlh$A, "oblimin")), artik = artik(R, mlh$A))),
  map = map_ref(R)
)

# ---- Thurstone (9 değişken, n = 213) ----
R <- thurstone; n <- 213
paft <- spss_paf(R, 3)
paft_tam <- spss_paf(R, 3, maxit = 100000, eps = 1e-13)
paft_psych <- psych::fa(R, nfactors = 3, fm = "pa", rotate = "none", SMC = TRUE, max.iter = 100000, min.err = 1e-14, n.obs = n, warnings = FALSE)
mlt <- ml(R, 3, n)
thurstone_ref <- list(
  R = R, n = n, kmo = kmo_ref(R, n),
  paf = list(A = paft$A, h2 = paft$h, it = paft$it, oblimin = paket(dondur(paft$A, "oblimin")),
             promax = paket(dondur(paft$A, "promax")), quartimax = paket(dondur(paft$A, "quartimax"))),
  paf_tam = list(A = paft_tam$A, psych = unclass(kanonik(unclass(paft_psych$loadings))$P)),
  ml = c(mlt, list(promax = paket(dondur(mlt$A, "promax")), oblimin = paket(dondur(mlt$A, "oblimin")),
                   equamax = paket(dondur(mlt$A, "equamax"))))
)

# ---- mtcars (n = 32, p = 7) ----
R <- cor(mt); n <- 32
pcm <- pca(R, 2)
pvm <- dondur(pcm, "varimax")
fsm <- psych::factor.scores(mt, pvm$P, method = "Thurstone")
fsm_ham <- psych::factor.scores(mt, pcm, method = "Thurstone")
pafm <- spss_paf(R, 2)
mtcars_ref <- list(
  veri = unname(mt), n = n, ozdeger = eigen(R, symmetric = TRUE)$values, kmo = kmo_ref(R, n),
  pca = list(A = pcm, varimax = paket(pvm), W = unname(fsm$weights), puan = unname(fsm$scores),
             W_ham = unname(fsm_ham$weights), artik = artik(R, pcm),
             psych = unclass(kanonik(unclass(psych::principal(R, nfactors = 2, rotate = "none")$loadings))$P)),
  paf = list(A = pafm$A, h2 = pafm$h, it = pafm$it, yakinsadi = pafm$yakinsadi, fark = pafm$fark),
  map = map_ref(R)
)

# ---- Holzinger-Swineford (n = 301, p = 9) ----
R <- cor(hs); n <- nrow(hs)
mlhs <- ml(R, 3, n)
set.seed(1)
B <- 4000
sim <- replicate(B, eigen(cor(matrix(rnorm(n * 9), n, 9)), symmetric = TRUE, only.values = TRUE)$values)
simp <- replicate(1000, {X <- matrix(rnorm(n * 9), n, 9); Rr <- cor(X); diag(Rr) <- 1 - 1 / diag(solve(Rr));
                         eigen(Rr, symmetric = TRUE, only.values = TRUE)$values})
fp <- psych::fa.parallel(R, n.obs = n, fa = "pc", n.iter = 200, plot = FALSE)
holz_ref <- list(
  n = n, ozdeger = eigen(R, symmetric = TRUE)$values,
  ml = c(mlhs, list(oblimin = paket(dondur(mlhs$A, "oblimin")), promax = paket(dondur(mlhs$A, "promax")),
                    varimax = paket(dondur(mlhs$A, "varimax")))),
  paralel = list(B = B, ort = rowMeans(sim), sd = apply(sim, 1, sd), q95 = apply(sim, 1, quantile, 0.95, type = 6),
                 paf_ort = rowMeans(simp), paf_sd = apply(simp, 1, sd), psych_pc_sim = fp$pc.sim, psych_ncomp = fp$ncomp),
  map = map_ref(R)
)

sonuc <- list(bfi = bfi_ref, harman = harman_ref, thurstone = thurstone_ref, mtcars = mtcars_ref, holzinger = holz_ref)
