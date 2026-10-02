# paketler: ppcor
# Korelasyon analizleri için referans değerler (korelasyon.js)
yuv <- function(x) as.numeric(sprintf("%.15g", x))

set.seed(11)
zb <- rnorm(2000)
ciftler <- list(
  mpg_wt = list(mtcars$mpg, mtcars$wt),
  mpg_hp = list(mtcars$mpg, mtcars$hp),
  cyl_gear = list(mtcars$cyl, mtcars$gear),
  carb_gear = list(mtcars$carb, mtcars$gear),
  iris_sl_pl = list(iris$Sepal.Length, iris$Petal.Length),
  iris_sw_sl = list(iris$Sepal.Width, iris$Sepal.Length),
  warp = list(warpbreaks$breaks, as.numeric(warpbreaks$tension)),
  kucuk4 = list(c(1, 2, 3, 4), c(2.5, 1, 4, 3.2)),
  kucuk5 = list(c(3, 1, 4, 1, 5), c(9, 2, 6, 5, 3)),
  kucuk3 = list(c(1, 2, 3), c(2, 1, 4)),
  mukemmel = list(1:6, 2 * (1:6) + 1),
  negatif = list(c(1, 2, 3, 4, 5, 6, 7), c(10, 8, 8, 5, 3, 3, 1)),
  buyuk = list(yuv(round(zb, 3)), yuv(round(0.4 * zb + rnorm(2000), 2)))
)

tek <- function(x, y) {
  r <- list(x = x, y = y, n = length(x))
  for (alt in c("two.sided", "greater", "less")) {
    p <- cor.test(x, y, method = "pearson", alternative = alt)
    s <- cor.test(x, y, method = "spearman", exact = FALSE, alternative = alt)
    k <- cor.test(x, y, method = "kendall", exact = FALSE, alternative = alt)
    r[[alt]] <- list(
      pearson = list(r = unname(p$estimate), t = unname(p$statistic), df = unname(p$parameter), p = p$p.value,
                     ga = if (is.null(p$conf.int)) NULL else as.numeric(p$conf.int)),
      spearman = list(rho = unname(s$estimate), S = unname(s$statistic), p = s$p.value),
      kendall = list(tau = unname(k$estimate), z = unname(k$statistic), p = k$p.value)
    )
  }
  n <- length(x)
  if (n > 3) {
    rho <- unname(cor(rank(x), rank(y)))
    se <- sqrt((1 + rho^2 / 2) / (n - 3))
    r$spearmanGA <- tanh(atanh(rho) + c(-1, 1) * qnorm(0.975) * se)
  }
  r
}
korelasyonlar <- lapply(ciftler, function(c) tek(c[[1]], c[[2]]))

# ---------- Kısmi korelasyon ----------
kismi <- function(x, y, z) {
  z <- as.data.frame(z)
  pc <- ppcor::pcor.test(x, y, z)
  n <- length(x); q <- ncol(z)
  r <- pc$estimate
  list(x = x, y = y, z = unname(as.list(z)), r = r, t = pc$statistic, p = pc$p.value, n = pc$n, gp = pc$gp,
       ga = tanh(atanh(r) + c(-1, 1) * qnorm(0.975) / sqrt(n - 3 - q)),
       # bağımsız denetim: artıkların korelasyonu
       rArtik = cor(resid(lm(x ~ ., data = z)), resid(lm(y ~ ., data = z))))
}
kismiler <- list(
  mtcars1 = kismi(mtcars$mpg, mtcars$wt, list(hp = mtcars$hp)),
  mtcars3 = kismi(mtcars$mpg, mtcars$wt, list(hp = mtcars$hp, disp = mtcars$disp, qsec = mtcars$qsec)),
  iris2 = kismi(iris$Sepal.Length, iris$Sepal.Width, list(pl = iris$Petal.Length, pw = iris$Petal.Width)),
  kucuk = kismi(c(1, 3, 2, 5, 4, 6), c(2, 1, 4, 3, 6, 5), list(z = c(1, 1, 2, 2, 3, 3)))
)

# ---------- Korelasyon matrisi (eksik verili) ----------
aq <- airquality[, 1:4]
matris <- function(yontem) {
  ikili <- cor(aq, use = "pairwise.complete.obs", method = yontem)
  nIkili <- crossprod(!is.na(aq))
  pIkili <- outer(1:4, 1:4, Vectorize(function(i, j) if (i == j) NA else
    suppressWarnings(cor.test(aq[[i]], aq[[j]], method = yontem, exact = FALSE)$p.value)))
  tam <- na.omit(aq)
  liste <- cor(tam, method = yontem)
  pListe <- outer(1:4, 1:4, Vectorize(function(i, j) if (i == j) NA else
    suppressWarnings(cor.test(tam[[i]], tam[[j]], method = yontem, exact = FALSE)$p.value)))
  list(ikili = unname(ikili), nIkili = unname(nIkili), pIkili = unname(pIkili),
       liste = unname(liste), nListe = nrow(tam), pListe = unname(pListe))
}
matrisler <- list(veri = unname(lapply(aq, function(v) v)), pearson = matris("pearson"), spearman = matris("spearman"), kendall = matris("kendall"))

# ---------- İki bağımsız korelasyon ----------
iki <- function(r1, n1, r2, n2) {
  z <- (atanh(r1) - atanh(r2)) / sqrt(1 / (n1 - 3) + 1 / (n2 - 3))
  list(r1 = r1, n1 = n1, r2 = r2, n2 = n2, z = z, p = 2 * pnorm(-abs(z)), pBuyuk = pnorm(z, lower.tail = FALSE), q = atanh(r1) - atanh(r2))
}
ikiKor <- list(iki(0.5, 50, 0.3, 60), iki(-0.2, 25, 0.45, 120), iki(0.91, 10, 0.88, 400))

sonuc <- list(korelasyonlar = korelasyonlar, kismiler = kismiler, matrisler = matrisler, ikiKor = ikiKor)
