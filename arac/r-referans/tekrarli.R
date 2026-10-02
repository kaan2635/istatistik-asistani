# paketler: car, emmeans
# Tekrarlı ölçümler / karma desen ANOVA referansları.
# car::Anova(mlm, idata, idesign, type = 3): tek değişkenli testler, GG/HF (Lecoutre) epsilonları, Mauchly W (R p'si);
# çok değişkenli testler car'ın SSP/SSPE matrislerinden car:::Pillai/Wilks/HL/Roy ile;
# SPSS'e özgü değerler (Mauchly χ² ve p, Huynh-Feldt özgün formül) elle; EMM ve ikili karşılaştırmalar emmeans (mlm).
options(contrasts = c("contr.sum", "contr.poly"))
suppressPackageStartupMessages({
  library(car)
  library(emmeans)
})

cokDeg <- function(H, E, q, dfe) {
  eig <- Re(eigen(qr.coef(qr(E), H), symmetric = FALSE)$values)
  m <- rbind(car:::Pillai(eig, q, dfe), car:::Wilks(eig, q, dfe), car:::HL(eig, q, dfe), car:::Roy(eig, q, dfe))
  data.frame(test = c("pillai", "wilks", "hotelling", "roy"), deger = m[, 1], F = m[, 2], sd1 = m[, 3], sd2 = m[, 4],
             p = pf(m[, 2], m[, 3], m[, 4], lower.tail = FALSE))
}

dfChr <- function(s) {
  s <- as.data.frame(s)
  for (k in names(s)) if (is.factor(s[[k]])) s[[k]] <- as.character(s[[k]])
  s
}
ikili <- function(e) dfChr(summary(pairs(e, adjust = "bonferroni"), infer = c(TRUE, TRUE)))

# Y: n x K matris; g: grup etiketleri (ya da NULL); icler: list(faktor = düzeyler), ilk faktör en yavaş değişir
rmVaka <- function(Y, g = NULL, icler, grupAdi = "Grup") {
  ok <- complete.cases(Y) & (if (is.null(g)) TRUE else !is.na(g))
  Yc <- Y[ok, , drop = FALSE]
  N <- nrow(Yc)
  K <- ncol(Yc)
  d <- data.frame(row.names = seq_len(N))
  if (!is.null(g)) {
    d[[grupAdi]] <- droplevels(factor(g[ok], levels = if (is.factor(g)) levels(g) else sort(unique(g))))
    G <- nlevels(d[[grupAdi]])
    fit <- lm(as.formula(paste("Yc ~", grupAdi)), data = d)
  } else {
    G <- 1
    fit <- lm(Yc ~ 1)
  }
  dfe <- N - G
  fadlar <- names(icler)
  idata <- expand.grid(rev(icler))[, rev(fadlar), drop = FALSE]
  for (f in fadlar) idata[[f]] <- factor(idata[[f]], levels = icler[[f]])
  av <- Anova(fit, idata = idata, idesign = as.formula(paste("~", paste(fadlar, collapse = "*"))), type = 3,
            icontrasts = c("contr.poly", "contr.poly"))
  sm <- suppressWarnings(summary(av, multivariate = TRUE))
  uni <- sm$univariate.tests
  uniTablo <- data.frame(kaynak = rownames(uni), KT = uni[, "Sum Sq"], sd = uni[, "num Df"], hataKT = uni[, "Error SS"],
                         hataSd = uni[, "den Df"], F = uni[, "F value"], p = uni[, "Pr(>F)"], stringsAsFactors = FALSE)
  adj <- sm$pval.adjustments
  duz <- if (is.null(adj)) NULL else data.frame(kaynak = rownames(adj), gg = adj[, "GG eps"], pgg = adj[, "Pr(>F[GG])"],
                                                  hf = adj[, "HF eps"], phf = adj[, "Pr(>F[HF])"], stringsAsFactors = FALSE)
  sph <- sm$sphericity.tests
  kure <- if (is.null(sph) || nrow(sph) == 0) NULL else data.frame(kaynak = rownames(sph), W = sph[, 1], pR = sph[, 2], stringsAsFactors = FALSE)
  # SPSS: Mauchly χ², p; Huynh-Feldt özgün formül; alt sınır p'leri
  spss <- list()
  for (term in av$terms) {
    P <- av$P[[term]]
    p <- ncol(P)
    if (all(abs(P - P[1, 1]) < 1e-12)) next # gruplar arası terim
    E <- av$SSPE[[term]]
    H <- av$SSP[[term]]
    S <- E / dfe
    gg <- sum(diag(S))^2 / (p * sum(S^2))
    hfS <- (N * p * gg - 2) / (p * (N - G - p * gg))
    hfL <- ((dfe + 1) * p * gg - 2) / (p * (dfe - p * gg))
    Fv <- (sum(diag(H)) / (av$df[[term]] * p)) / (sum(diag(E)) / (dfe * p))
    W <- if (p > 1) det(E) / (sum(diag(E)) / p)^p else 1
    chi <- -(dfe - (2 * p^2 + p + 2) / (6 * p)) * log(W)
    f <- p * (p + 1) / 2 - 1
    spss[[term]] <- list(W = W, kiKare = chi, sd = f, p = if (p > 1) pchisq(chi, f, lower.tail = FALSE) else NA,
                         gg = gg, hfSpss = hfS, hfLecoutre = hfL,
                         pHfSpss = pf(Fv, av$df[[term]] * p * min(1, hfS), dfe * p * min(1, hfS), lower.tail = FALSE),
                         pAlt = pf(Fv, av$df[[term]], dfe, lower.tail = FALSE))
  }
  mv <- lapply(av$terms, function(term) cokDeg(av$SSP[[term]], av$SSPE[[term]], av$df[[term]], av$error.df))
  names(mv) <- av$terms
  # İç kontrastlar (SPSS Tests of Within-Subjects Contrasts): her kontrast sütunu için tek değişkenli model
  Ms <- list()
  for (f in fadlar) {
    parca <- lapply(fadlar, function(h) if (h == f) contr.poly(length(icler[[h]])) else matrix(1 / sqrt(length(icler[[h]])), length(icler[[h]]), 1))
    Ms[[f]] <- Reduce(kronecker, parca)
  }
  if (length(fadlar) == 2) Ms[[paste(fadlar, collapse = ":")]] <- kronecker(contr.poly(length(icler[[1]])), contr.poly(length(icler[[2]])))
  kontrast <- list()
  for (et in names(Ms)) {
    M <- Ms[[et]]
    kontrast[[et]] <- lapply(seq_len(ncol(M)), function(ci) {
      z <- as.numeric(Yc %*% M[, ci])
      if (G > 1) {
        a <- Anova(lm(as.formula(paste("z ~", grupAdi)), data = cbind(d, z = z)), type = 3)
        list(KT = a[1, "Sum Sq"], F = a[1, "F value"], p = a[1, "Pr(>F)"],
             grupKT = a[2, "Sum Sq"], grupF = a[2, "F value"], grupP = a[2, "Pr(>F)"], hataKT = a[3, "Sum Sq"])
      } else {
        kt <- N * mean(z)^2
        e <- sum((z - mean(z))^2)
        list(KT = kt, F = kt / (e / (N - 1)), p = pf(kt / (e / (N - 1)), 1, N - 1, lower.tail = FALSE), hataKT = e)
      }
    })
  }
  # EMM ve ikili karşılaştırmalar (emmeans, mlm)
  rg <- ref_grid(fit, mult.levs = rev(icler))
  emm <- list()
  ciftler <- list()
  for (f in fadlar) {
    e <- emmeans(rg, f)
    emm[[f]] <- dfChr(summary(e))
    ciftler[[f]] <- ikili(e)
  }
  if (length(fadlar) > 1) emm[[paste(fadlar, collapse = ":")]] <- dfChr(summary(emmeans(rg, fadlar)))
  basit <- NULL
  levene <- NULL
  if (G > 1) {
    e <- emmeans(rg, grupAdi)
    emm[[grupAdi]] <- dfChr(summary(e))
    ciftler[[grupAdi]] <- ikili(e)
    basit <- list(grup = list(), ic = list())
    for (f in fadlar) {
      emm[[paste(grupAdi, f, sep = ":")]] <- dfChr(summary(emmeans(rg, c(grupAdi, f))))
      eg <- emmeans(rg, as.formula(paste("~", grupAdi, "|", f)))
      ei <- emmeans(rg, as.formula(paste("~", f, "|", grupAdi)))
      jt <- dfChr(joint_tests(rg, by = f))
      # grup etkisi her düzeyde: ölçümün (diğer iç faktörler üzerinden ortalaması) tek yönlü ANOVA'sı
      for (lv in icler[[f]]) {
        w <- Reduce(kronecker, lapply(fadlar, function(h) if (h == f) as.numeric(icler[[h]] == lv) else rep(1 / length(icler[[h]]), length(icler[[h]]))))
        yw <- as.numeric(Yc %*% w)
        a <- anova(lm(as.formula(paste("yw ~", grupAdi)), data = cbind(d, yw = yw)))
        basit$grup[[length(basit$grup) + 1]] <- list(faktor = f, duzey = lv, KT = a[1, "Sum Sq"], F = a[1, "F value"], p = a[1, "Pr(>F)"],
                                                     hataKT = a[2, "Sum Sq"], jointP = jt$p.value[jt[["model term"]] == grupAdi & jt[[f]] == lv],
                                                     ikili = ikili(eg[eg@grid[[f]] == lv]))
      }
      # iç faktörün her gruptaki çok değişkenli testi: L (grup hücre ortalaması) ve P = iç kontrastlar
      Mf <- Ms[[f]]
      for (gl in levels(d[[grupAdi]])) {
        gi <- which(levels(d[[grupAdi]]) == gl)
        L <- matrix(c(1, contr.sum(G)[gi, ]), nrow = 1)
        lh <- linearHypothesis(fit, L, P = Mf)
        basit$ic[[length(basit$ic) + 1]] <- list(faktor = f, grup = gl, testler = cokDeg(lh$SSPH, lh$SSPE, 1, dfe),
                                                 ikili = ikili(ei[ei@grid[[grupAdi]] == gl]))
      }
    }
    levene <- lapply(seq_len(K), function(j) {
      lv <- leveneTest(Yc[, j] ~ d[[grupAdi]], center = mean)
      list(F = lv[1, "F value"], sd1 = lv[1, "Df"], sd2 = lv[2, "Df"], p = lv[1, "Pr(>F)"])
    })
  }
  mauchlyStats <- NULL
  if (length(fadlar) == 1 && K > 2) {
    mt <- mauchly.test(fit, X = ~1, idata = data.frame(i = factor(seq_len(K))))
    mauchlyStats <- list(W = unname(mt$statistic), p = mt$p.value)
  }
  betim <- list()
  for (j in seq_len(K)) {
    if (G > 1) {
      for (gl in levels(d[[grupAdi]])) {
        v <- Yc[d[[grupAdi]] == gl, j]
        betim[[length(betim) + 1]] <- list(j = j, grup = gl, n = length(v), ortalama = mean(v), ss = sd(v))
      }
    }
    betim[[length(betim) + 1]] <- list(j = j, grup = NA, n = N, ortalama = mean(Yc[, j]), ss = sd(Yc[, j]))
  }
  list(
    veri = Y, gruplar = if (is.null(g)) NULL else as.character(g), grupSirasi = if (G > 1) levels(d[[grupAdi]]) else NULL,
    icler = icler, grupAdi = grupAdi, N = N, G = G,
    uni = uniTablo, duz = duz, kure = kure, spss = spss, mv = mv, kontrast = kontrast,
    emm = emm, ikili = ciftler, basit = basit, levene = levene, mauchly = mauchlyStats, betimsel = betim
  )
}

# 1) Tek denek içi faktör, küresellik açıkça bozuk (AR(1), artan varyans), iki eksik değer
set.seed(11)
n1 <- 18
ss1 <- c(1, 2, 3, 4)
kor1 <- 0.85^abs(outer(1:4, 1:4, "-"))
Sig1 <- diag(ss1) %*% kor1 %*% diag(ss1)
Y1 <- round(matrix(rnorm(n1 * 4), n1, 4) %*% chol(Sig1) + matrix(c(10, 11, 13, 13.5), n1, 4, byrow = TRUE), 2)
Y1[3, 2] <- NA
Y1[10, 4] <- NA

# 2) OBrienKaiser: tedavi (3 grup, dengesiz) x faz (saat ortalamaları)
ob <- OBrienKaiser
faz <- cbind(pre = rowMeans(ob[, 3:7]), post = rowMeans(ob[, 8:12]), fup = rowMeans(ob[, 13:17]))
Yob <- as.matrix(ob[, 3:17])

# 5) Sentetik karma desen: 3 grup (6, 8, 10), 4 ölçüm, küresellik bozuk, eksik ölçüm ve eksik grup etiketi
set.seed(5)
nk <- c(6, 8, 10)
gk <- rep(c("plasebo", "dozA", "dozB"), nk)
Sig2 <- diag(c(1, 1.5, 3, 5)) %*% (0.7^abs(outer(1:4, 1:4, "-"))) %*% diag(c(1, 1.5, 3, 5))
mu2 <- rbind(plasebo = c(20, 20.5, 21, 21), dozA = c(20, 22, 23, 23.5), dozB = c(20, 23, 26, 27))
Yk <- round(mu2[gk, ] + matrix(rnorm(sum(nk) * 4), sum(nk), 4) %*% chol(Sig2), 1)
Yk[5, 3] <- NA
gk[17] <- NA

sonuc <- list(
  tek = rmVaka(Y1, NULL, list(Zaman = c("T1", "T2", "T3", "T4"))),
  ob1 = rmVaka(faz, ob$treatment, list(Faz = c("pre", "post", "fup")), grupAdi = "Tedavi"),
  ob2 = rmVaka(Yob, ob$treatment, list(Faz = c("pre", "post", "fup"), Saat = as.character(1:5)), grupAdi = "Tedavi"),
  ob2tek = rmVaka(Yob, NULL, list(Faz = c("pre", "post", "fup"), Saat = as.character(1:5))),
  iki = rmVaka(faz[, 1:2], ob$gender, list(Faz = c("pre", "post")), grupAdi = "Cinsiyet"),
  karma = rmVaka(Yk, gk, list(Doz = c("hafta0", "hafta2", "hafta4", "hafta8")), grupAdi = "Grup"),
  poli = list(k3 = contr.poly(3), k4 = contr.poly(4), k5 = contr.poly(5), k7 = contr.poly(7), k5s = contr.poly(5, scores = c(1, 2, 4, 8, 16)))
)
