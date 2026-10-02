# paketler: MASS, ResourceSelection, pROC, ordinal, brant, VGAM, nnet
# Lojistik regresyon referansları: glm(binomial) tam yakınsamayla (epsilon = 1e-14), yineleme sayısı varsayılan glm'den,
# kesin profil olabilirlik GA'ları (uniroot), confint (spline; gevşek kontrol), drop1 LRT, anova(test = "Rao"),
# ResourceSelection::hoslem.test, pROC::auc, artıklar ve etki ölçüleri.

sutunlar <- function(df) lapply(as.list(df), function(v) if (is.factor(v)) as.character(v) else v)
KESIN <- glm.control(epsilon = 1e-14, maxit = 100)

profilKesin <- function(X, y, b, se, D0, duzey = 0.95) {
  hedef <- qchisq(duzey, 1)
  t(sapply(seq_along(b), function(j) {
    Xd <- X[, -j, drop = FALSE]
    sicak <- b[-j]
    g <- function(v) {
      off <- v * X[, j]
      dev <- if (ncol(Xd)) {
        # süreklilik: bir önceki noktadaki çözümden başla (IRLS uzak başlangıçtan ıraksayabilir)
        ff <- glm.fit(Xd, y, family = binomial(), offset = off, start = sicak, control = glm.control(epsilon = 1e-12, maxit = 500))
        if (!ff$converged) stop(paste("profil uyumu yakınsamadı", j, v))
        sicak <<- ff$coefficients
        ff$deviance
      } else sum(binomial()$dev.resids(y, plogis(off), rep(1, length(y))))
      dev - D0 - hedef
    }
    bul <- function(yon) {
      # R'nin glm.fit'i adım yarılamadığı için uzak başlangıçtan ıraksayabilir (sapma çöpe gider);
      # bu yüzden b'den dışa doğru küçük adımlarla (0.1·SH) ilerleyip her uyumu bir öncekinden başlatıyoruz.
      sicak <<- b[-j]
      k <- 0
      repeat {
        k <- k + 0.1
        u <- b[j] + yon * k * se[j]
        if (g(u) > 0) break
        uOnce <- u
        sOnce <- sicak
      }
      if (k < 0.15) { uOnce <- b[j]; sOnce <- b[-j] }
      sicak <<- sOnce
      uniroot(g, sort(c(uOnce, u)), tol = 1e-13 * max(1, abs(u)))$root
    }
    c(bul(-1), bul(1))
  }))
}

lojOzet <- function(formul, veri, g = 10, kesim = 0.5, profil = TRUE, terimler = TRUE) {
  f0 <- suppressWarnings(glm(formul, family = binomial, data = veri))
  X0 <- model.matrix(f0)
  y <- f0$y
  asg <- attr(X0, "assign")
  ok <- !is.na(coef(f0))
  X <- X0[, ok, drop = FALSE]
  fit <- glm(y ~ 0 + X, family = binomial, control = KESIN)
  # glm, vcov/hatvalues için son yinelemenin (bir önceki β) ağırlıklarını kullanır; MLE'deki ağırlıklar için bir adım daha
  fit <- glm(y ~ 0 + X, family = binomial, start = coef(fit), control = KESIN)
  b <- unname(coef(fit))
  V <- unname(vcov(fit))
  se <- sqrt(diag(V))
  p <- length(ok)
  tam <- function(v) { r <- rep(NA_real_, p); r[ok] <- v; r }
  D <- deviance(fit)
  n <- length(y)
  sabitVar <- attr(terms(f0), "intercept") == 1
  D0 <- if (sabitVar) deviance(glm(y ~ 1, family = binomial, control = KESIN)) else 2 * n * log(2)
  mu <- unname(fitted(fit))
  # terim testleri (Wald ve LR)
  tt <- NULL
  if (terimler) {
    asgOk <- asg[ok]
    tt <- lapply(setdiff(unique(asg), 0), function(a) {
      idx <- which(asgOk == a)
      w <- if (length(idx)) as.numeric(t(b[idx]) %*% solve(V[idx, idx, drop = FALSE]) %*% b[idx]) else NA
      kalan <- which(asgOk != a)
      Dk <- glm.fit(X[, kalan, drop = FALSE], y, family = binomial(), control = KESIN)$deviance
      list(terim = attr(terms(f0), "term.labels")[a], sd = length(idx), wald = w, waldP = pchisq(w, length(idx), lower.tail = FALSE),
           lr = if (length(idx)) Dk - D else NA, lrP = if (length(idx)) pchisq(Dk - D, length(idx), lower.tail = FALSE) else NA)
    })
  }
  # skor testleri (Blok 0): her sütun, çok sütunlu terimler ve tümü
  skor <- NULL
  if (sabitVar) {
    f00 <- glm(y ~ 1, family = binomial, control = KESIN)
    f00 <- glm(y ~ 1, family = binomial, start = coef(f00), control = KESIN)  # ağırlıklar MLE noktasında
    rao <- function(Z) {
      f1 <- glm(y ~ Z, family = binomial, control = KESIN)
      a <- anova(f00, f1, test = "Rao")
      c(a$Rao[2], a$Df[2])
    }
    asgOk <- asg[ok]
    sk <- list()
    for (a in setdiff(unique(asgOk), 0)) {
      idx <- which(asgOk == a)
      if (length(idx) > 1) sk[[length(sk) + 1]] <- rao(X[, idx, drop = FALSE])
      for (j in idx) sk[[length(sk) + 1]] <- rao(X[, j, drop = FALSE])
    }
    genel <- rao(X[, -1, drop = FALSE])
    skor <- list(skor = sapply(sk, `[`, 1), sd = sapply(sk, `[`, 2), genel = genel[1], genelSd = genel[2])
  }
  hl <- ResourceSelection::hoslem.test(y, mu, g = g)
  tahmin <- as.integer(mu >= kesim)
  roc <- pROC::roc(y, mu, direction = "<", levels = c(0, 1), quiet = TRUE)
  pr <- if (profil) profilKesin(X, y, b, se, D) else NULL
  ci <- if (profil) suppressMessages(confint(fit)) else NULL
  if (!is.null(ci) && is.null(dim(ci))) ci <- matrix(ci, 1)
  z <- qnorm(0.975)
  list(
    n = n, olay = sum(y), iter = f0$iter,
    B = tam(b), SH = tam(se), z = tam(b / se), p = tam(2 * pnorm(-abs(b / se))),
    OR = tam(exp(b)), orGaAlt = tam(exp(b - z * se)), orGaUst = tam(exp(b + z * se)),
    profilAlt = if (profil) tam(pr[, 1]) else NULL, profilUst = if (profil) tam(pr[, 2]) else NULL,
    confintAlt = if (profil) tam(unname(ci[, 1])) else NULL, confintUst = if (profil) tam(unname(ci[, 2])) else NULL,
    vcov = V,
    D = D, D0 = D0, sd = sum(ok) - sabitVar, AIC = AIC(fit), BIC = BIC(fit), logLik = as.numeric(logLik(fit)),
    coxSnell = 1 - exp((D - D0) / n), nagelkerke = (1 - exp((D - D0) / n)) / (1 - exp(-D0 / n)), mcfadden = 1 - D / D0,
    terimler = tt, skor = skor,
    hl = list(ist = unname(hl$statistic), sd = unname(hl$parameter), p = hl$p.value,
              g1 = unname(hl$observed[, 2]), g0 = unname(hl$observed[, 1]), b1 = unname(hl$expected[, 2]), b0 = unname(hl$expected[, 1])),
    sinif = list(dn = sum(y == 0 & tahmin == 0), yp = sum(y == 0 & tahmin == 1), yn = sum(y == 1 & tahmin == 0), dp = sum(y == 1 & tahmin == 1)),
    auc = as.numeric(pROC::auc(roc)),
    mu = mu, eta = unname(fit$linear.predictors),
    pearson = unname(residuals(fit, type = "pearson")), sapma = unname(residuals(fit, type = "deviance")),
    hat = unname(hatvalues(fit)), stdPearson = unname(rstandard(fit, type = "pearson")), stdSapma = unname(rstandard(fit, type = "deviance")),
    cook = unname(cooks.distance(fit)),
    satirlar = as.integer(rownames(model.frame(f0))) - 1L
  )
}

# ---------- veri setleri ----------
mt <- mtcars
infert2 <- infert
bw <- MASS::birthwt
bw$race <- factor(bw$race)
ti <- as.data.frame(Titanic)
ti <- ti[rep(seq_len(nrow(ti)), ti$Freq), 1:4]
rownames(ti) <- NULL
es <- esoph
duz <- function(f) factor(as.character(f), levels = levels(f))
es$agegp <- duz(es$agegp); es$alcgp <- duz(es$alcgp); es$tobgp <- duz(es$tobgp)
es1 <- es[rep(seq_len(nrow(es)), es$ncases), 1:3]; es1$y <- 1
es0 <- es[rep(seq_len(nrow(es)), es$ncontrols), 1:3]; es0$y <- 0
ese <- rbind(es1, es0)
rownames(ese) <- NULL

set.seed(7)
n <- 150
s <- data.frame(x1 = round(rnorm(n), 3), x2 = round(rnorm(n, 50, 10), 1), g = sample(c("A", "B", "C", "D"), n, TRUE), stringsAsFactors = FALSE)
eta <- -0.5 + 0.8 * s$x1 + 0.03 * (s$x2 - 50) + c(A = 0, B = 0.7, C = -0.4, D = 1.1)[s$g]
s$y <- ifelse(runif(n) < plogis(eta), "Var", "Yok")
s$x3 <- 2 * s$x1 - 1
s$y[c(4, 77)] <- NA
s$x2[c(10, 11)] <- NA
s$g[c(120)] <- NA
sf <- s
sf$g <- relevel(factor(sf$g), ref = "C")
sf$olay <- as.integer(sf$y == "Var")

sonuc <- list(
  veri = list(
    mtcars = sutunlar(mt), infert = sutunlar(infert2), birthwt = sutunlar(MASS::birthwt), titanic = sutunlar(ti), esoph = sutunlar(ese),
    sentetik = sutunlar(s),
    duzeyler = list(education = levels(infert$education), agegp = levels(es$agegp), alcgp = levels(es$alcgp), tobgp = levels(es$tobgp))
  ),
  mtcars = lojOzet(am ~ wt + hp, mt),
  infert = lojOzet(case ~ age + parity + education + spontaneous + induced, infert2),
  birthwt = lojOzet(low ~ age + lwt + race + smoke + ptl + ht + ui, bw),
  birthwtEtk = lojOzet(low ~ lwt * smoke + age, bw),
  titanic = lojOzet(Survived ~ Class + Sex + Age, ti),
  esoph = lojOzet(y ~ agegp + alcgp + tobgp, ese),
  sentetik = lojOzet(olay ~ x1 + x2 + x3 + g, sf),
  sabitsiz = lojOzet(am ~ 0 + wt + hp, mt, profil = TRUE),
  hl5 = lojOzet(low ~ age + lwt + race + smoke + ptl + ht + ui, bw, g = 5, kesim = 0.3, profil = FALSE, terimler = FALSE)
)

# ---------- tek değişkenli (crude) modeller: birthwt ----------
tekdeg <- lapply(c("age", "lwt", "race", "smoke", "ht"), function(v) {
  f <- glm(reformulate(v, "low"), family = binomial, data = bw, control = KESIN)
  f0 <- glm(low ~ 1, family = binomial, data = bw, control = KESIN)
  s <- summary(f)$coefficients
  list(B = unname(s[-1, 1]), SH = unname(s[-1, 2]), p = unname(s[-1, 4]), lrP = anova(f0, f, test = "LRT")[2, "Pr(>Chi)"])
})
sonuc$tekDegiskenli <- tekdeg

# ---------- ayrışma ----------
tamAyr <- suppressWarnings(glm(y ~ x, family = binomial, data = data.frame(x = 1:10, y = rep(0:1, each = 5))))
yariAyr <- suppressWarnings(glm(y ~ x, family = binomial, data = data.frame(x = c(1:5, 5:9), y = rep(0:1, each = 5))))
mtAyr <- suppressWarnings(glm(vs ~ mpg + factor(cyl), family = binomial, data = mtcars))
sonuc$ayrisma <- list(tamD = deviance(tamAyr), yariD = deviance(yariAyr), mtD = deviance(mtAyr),
                      mtB = unname(coef(mtAyr)), mtSE = unname(sqrt(diag(vcov(mtAyr)))))

# =====================================================================
# Ordinal lojistik (orantılı odds): ordinal::clm (analitik Newton, sıkı ölçüt), MASS::polr (gevşek), brant::brant,
# paralel çizgiler: clm(nominal = ~ x) ile LR.
# =====================================================================
CLM <- ordinal::clm.control(gradTol = 1e-12, relTol = 1e-14, maxIter = 200)
# brant::brant'in formülü, ancak ikili modeller tam yakınsamış ve vcov MLE noktasında (brant varsayılan glm ölçütünü
# ve son yinelemenin ağırlıklarını kullandığı için ~1e-4 göreli sapma taşır).
brantKesin <- function(formul, veri) {
  X <- model.matrix(formul, veri)
  y <- as.integer(model.response(model.frame(formul, veri)))
  J <- max(y)
  K <- ncol(X) - 1
  fits <- lapply(1:(J - 1), function(m) {
    z <- as.integer(y > m)
    f <- glm(z ~ 0 + X, family = binomial, control = KESIN)
    glm(z ~ 0 + X, family = binomial, control = KESIN, start = coef(f))
  })
  pi <- sapply(fits, fitted)
  bet <- lapply(fits, function(f) unname(coef(f))[-1])
  Vb <- matrix(0, (J - 1) * K, (J - 1) * K)
  for (m in 1:(J - 1)) for (l in m:(J - 1)) {
    Wm <- pi[, m] * (1 - pi[, m]); Wl <- pi[, l] * (1 - pi[, l]); Wml <- pi[, l] - pi[, m] * pi[, l]
    B <- (solve(crossprod(X, X * Wm)) %*% crossprod(X, X * Wml) %*% solve(crossprod(X, X * Wl)))[-1, -1, drop = FALSE]
    Vb[((m - 1) * K + 1):(m * K), ((l - 1) * K + 1):(l * K)] <- B
    Vb[((l - 1) * K + 1):(l * K), ((m - 1) * K + 1):(m * K)] <- t(B)
  }
  bs <- unlist(bet)
  D <- do.call(rbind, lapply(2:(J - 1), function(l) { M <- matrix(0, K, (J - 1) * K); M[, 1:K] <- diag(K); M[, ((l - 1) * K + 1):(l * K)] <- -diag(K); M }))
  wald <- function(D) { d <- D %*% bs; as.numeric(t(d) %*% solve(D %*% Vb %*% t(D)) %*% d) }
  c(wald(D), sapply(1:K, function(k) wald(D[seq(k, nrow(D), by = K), , drop = FALSE])))
}

ordOzet <- function(formul, veri, nominal) {
  veri <- veri[complete.cases(veri[, all.vars(formul)]), , drop = FALSE]  # tüm modeller aynı satırlarla
  # do.call: çağrı veriyi içersin (drop1/brant çağrıyı yeniden değerlendirir)
  f <- do.call(ordinal::clm, list(formul, data = veri, control = CLM))
  V <- unname(vcov(f))
  se <- sqrt(diag(V))
  a <- length(f$alpha)
  b <- unname(c(f$alpha, f$beta))
  f0 <- do.call(ordinal::clm, list(update(formul, . ~ 1), data = veri, control = CLM))
  fn <- do.call(ordinal::clm, list(update(formul, . ~ 1), nominal = nominal, data = veri, control = CLM))
  fp <- do.call(MASS::polr, list(formul, data = veri, Hess = TRUE))
  br <- suppressWarnings(brant::brant(fp))
  asg <- attr(model.matrix(formul, veri), "assign")[-1]
  tt <- lapply(unique(asg), function(k) {
    idx <- a + which(asg == k)
    w <- as.numeric(t(b[idx]) %*% solve(V[idx, idx, drop = FALSE]) %*% b[idx])
    list(sd = length(idx), wald = w, waldP = pchisq(w, length(idx), lower.tail = FALSE))
  })
  d1 <- drop1(f, test = "Chi")
  list(
    n = nobs(f), B = b, SH = se, vcov = V, ll = as.numeric(logLik(f)), ll0 = as.numeric(logLik(f0)), llNom = as.numeric(logLik(fn)),
    AIC = AIC(f), BIC = BIC(f), maxGrad = max(abs(f$gradient)),
    terimWald = tt, lr = d1$LRT[-1], lrP = d1[["Pr(>Chi)"]][-1],
    brant = unname(br[, 1]), brantSd = unname(br[, 2]), brantP = unname(br[, 3]), brantKesin = tryCatch(brantKesin(formul, veri), error = function(e) NA),
    polr = unname(c(fp$zeta, coef(fp))),
    olasilik = unname(fitted(f))
  )
}
wine <- ordinal::wine
hs <- MASS::housing
hs <- hs[rep(seq_len(nrow(hs)), hs$Freq), 1:4]
rownames(hs) <- NULL
set.seed(99)
n <- 200
so <- data.frame(x = round(rnorm(n, 0, 1.5), 3), g = sample(c("a", "b", "c"), n, TRUE), stringsAsFactors = FALSE)
lat <- 0.9 * so$x + c(a = 0, b = 0.8, c = -0.5)[so$g] + rlogis(n)
so$y <- as.character(cut(lat, c(-Inf, -1, 0.5, 2, Inf), labels = c("hic", "az", "orta", "cok")))
so$y[c(5, 50)] <- NA
so$x[c(7)] <- NA
sof <- so
sof$y <- factor(sof$y, levels = c("hic", "az", "orta", "cok"), ordered = TRUE)
sof$g <- factor(sof$g)
sonuc$veri$wine <- sutunlar(wine[, c("rating", "temp", "contact")])
sonuc$veri$housing <- sutunlar(hs)
sonuc$veri$ordSentetik <- sutunlar(so)
sonuc$ordinal <- list(
  wine = ordOzet(rating ~ temp + contact, wine, ~ temp + contact),
  housing = ordOzet(Sat ~ Infl + Type + Cont, hs, ~ Infl + Type + Cont),
  sentetik = ordOzet(y ~ x + g, sof, ~ x + g)
)

# =====================================================================
# Multinominal lojistik: VGAM::vglm (IRLS, sıkı ölçüt; vcov için MLE'den bir adım daha), nnet::multinom (gevşek)
# =====================================================================
mnOzet <- function(formul, veri, ref) {
  veri <- veri[complete.cases(veri[, all.vars(formul)]), , drop = FALSE]  # tüm modeller aynı satırlarla
  kontrol <- VGAM::vglm.control(epsilon = 1e-13, maxit = 200)
  v <- suppressWarnings(VGAM::vglm(formul, VGAM::multinomial(refLevel = ref), data = veri, control = kontrol))
  v <- suppressWarnings(VGAM::vglm(formul, VGAM::multinomial(refLevel = ref), data = veri, control = kontrol, coefstart = VGAM::coef(v)))
  Bm <- VGAM::coef(v, matrix = TRUE)
  se <- sqrt(diag(VGAM::vcov(v)))
  M <- ncol(Bm)
  SEm <- sapply(seq_len(M), function(a) se[paste0(rownames(Bm), ":", a)])
  ll <- as.numeric(VGAM::logLik(v))
  v0 <- suppressWarnings(VGAM::vglm(update(formul, . ~ 1), VGAM::multinomial(refLevel = ref), data = veri, control = kontrol))
  terimler <- attr(terms(formul), "term.labels")
  lrt <- sapply(terimler, function(t) {
    fr <- suppressWarnings(VGAM::vglm(update(formul, as.formula(paste(". ~ . -", t))), VGAM::multinomial(refLevel = ref), data = veri, control = kontrol))
    -2 * as.numeric(VGAM::logLik(fr))
  })
  m <- nnet::multinom(formul, data = veri, trace = FALSE, maxit = 1000, reltol = 1e-14)
  cm <- coef(m)
  if (is.null(dim(cm))) cm <- matrix(cm, 1)
  list(n = nrow(VGAM::model.matrix(v)), B = unname(t(Bm)), SH = unname(t(SEm)), ll = ll, ll0 = as.numeric(VGAM::logLik(v0)),
       ikiLLRed = unname(lrt), AIC = -2 * ll + 2 * length(VGAM::coef(v)), nnet = unname(cm), olasilik = unname(VGAM::fitted(v)))
}
hsN <- hs
hsN$Sat <- factor(as.character(hsN$Sat), levels = c("Low", "Medium", "High"))
set.seed(5)
n <- 240
sm <- data.frame(x1 = round(rnorm(n), 3), x2 = round(runif(n, 20, 60)), g = sample(c("K", "L", "M"), n, TRUE), stringsAsFactors = FALSE)
e2 <- 0.3 + 0.8 * sm$x1 - 0.02 * (sm$x2 - 40) + c(K = 0, L = 0.6, M = -0.4)[sm$g]
e3 <- -0.2 - 0.5 * sm$x1 + 0.03 * (sm$x2 - 40) + c(K = 0, L = -0.3, M = 0.9)[sm$g]
pr <- cbind(1, exp(e2), exp(e3))
pr <- pr / rowSums(pr)
sm$y <- c("A", "B", "C")[apply(pr, 1, function(p) sample(3, 1, prob = p))]
sm$y[3] <- NA
sm$g[9] <- NA
smf <- sm
smf$y <- factor(smf$y)
smf$g <- factor(smf$g)
sonuc$veri$mnSentetik <- sutunlar(sm)
sonuc$multinom <- list(
  housing = mnOzet(Sat ~ Infl + Type + Cont, hsN, 1),
  housingSon = mnOzet(Sat ~ Infl + Type + Cont, hsN, 3),
  sentetik = mnOzet(y ~ x1 + x2 + g, smf, 1)
)

# =====================================================================
# Poisson regresyonu: glm(poisson) tam yakınsama, kesin profil GA, drop1 LRT
# =====================================================================
profilGLM <- function(X, y, b, se, D0, aile, off, duzey = 0.95) {
  hedef <- qchisq(duzey, 1)
  t(sapply(seq_along(b), function(j) {
    Xd <- X[, -j, drop = FALSE]
    sicak <- b[-j]
    g <- function(v) {
      o <- off + v * X[, j]
      dev <- if (ncol(Xd)) {
        ff <- glm.fit(Xd, y, family = aile, offset = o, start = sicak, control = glm.control(epsilon = 1e-12, maxit = 500))
        if (!ff$converged) stop(paste("profil uyumu yakınsamadı", j, v))
        sicak <<- ff$coefficients
        ff$deviance
      } else sum(aile$dev.resids(y, aile$linkinv(o), rep(1, length(y))))
      dev - D0 - hedef
    }
    bul <- function(yon) {
      sicak <<- b[-j]
      k <- 0
      repeat {
        k <- k + 0.1
        u <- b[j] + yon * k * se[j]
        if (g(u) > 0) break
        uOnce <- u
        sOnce <- sicak
      }
      if (k < 0.15) {
        uOnce <- b[j]
        sOnce <- b[-j]
      }
      sicak <<- sOnce
      uniroot(g, sort(c(uOnce, u)), tol = 1e-13 * max(1, abs(u)))$root
    }
    c(bul(-1), bul(1))
  }))
}
poiOzet <- function(formul, veri) {
  veri <- veri[complete.cases(veri[, all.vars(formul)]), , drop = FALSE]  # tüm modeller aynı satırlarla
  f0 <- glm(formul, family = poisson, data = veri)
  X <- model.matrix(f0)
  y <- f0$y
  off <- if (is.null(f0$offset)) rep(0, length(y)) else f0$offset
  fit <- glm(y ~ 0 + X, family = poisson, offset = off, control = KESIN)
  fit <- glm(y ~ 0 + X, family = poisson, offset = off, start = coef(fit), control = KESIN)
  b <- unname(coef(fit))
  V <- unname(vcov(fit))
  se <- sqrt(diag(V))
  D <- deviance(fit)
  pr <- profilGLM(X, y, b, se, D, poisson(), off)
  d1 <- drop1(glm(formul, family = poisson, data = veri, control = KESIN), test = "LRT")
  z <- qnorm(0.975)
  list(n = length(y), iter = f0$iter, B = b, SH = se, p = 2 * pnorm(-abs(b / se)), IRR = exp(b), irrGaAlt = exp(b - z * se), irrGaUst = exp(b + z * se),
       profilAlt = pr[, 1], profilUst = pr[, 2], vcov = V, D = D, D0 = f0$null.deviance, dfRes = fit$df.residual,
       pearson = sum(residuals(fit, type = "pearson")^2), AIC = AIC(fit), BIC = BIC(fit), logLik = as.numeric(logLik(fit)),
       lr = d1$LRT[-1], lrP = d1[["Pr(>Chi)"]][-1], mu = unname(fitted(fit)), hat = unname(hatvalues(fit)), cook = unname(cooks.distance(fit)),
       stdSapma = unname(rstandard(fit, type = "deviance")))
}
ins <- MASS::Insurance
ins$Group <- factor(as.character(ins$Group), levels = levels(MASS::Insurance$Group))
ins$Age <- factor(as.character(ins$Age), levels = levels(MASS::Insurance$Age))
set.seed(3)
n <- 120
sp <- data.frame(x = round(rnorm(n), 2), g = sample(c("u", "v", "w"), n, TRUE), t = round(runif(n, 0.5, 3), 2), stringsAsFactors = FALSE)
sp$y <- rpois(n, sp$t * exp(0.2 + 0.4 * sp$x + c(u = 0, v = 0.5, w = -0.3)[sp$g]))
sp$y[c(2, 30)] <- NA
sp$x[12] <- NA
spf <- sp
spf$g <- factor(spf$g)
sonuc$veri$warpbreaks <- sutunlar(warpbreaks)
sonuc$veri$InsectSprays <- sutunlar(InsectSprays)
sonuc$veri$Insurance <- sutunlar(ins)
sonuc$veri$poiSentetik <- sutunlar(sp)
sonuc$veri$duzeyler$Group <- levels(ins$Group)
sonuc$veri$duzeyler$AgeIns <- levels(ins$Age)
sonuc$poisson <- list(
  warpbreaks = poiOzet(breaks ~ wool + tension, warpbreaks),
  insect = poiOzet(count ~ spray, InsectSprays),
  insurance = poiOzet(Claims ~ District + Group + Age + offset(log(Holders)), ins),
  sentetik = poiOzet(y ~ x + g + offset(log(t)), spf)
)
