# paketler: nortest, moments, fBasics
# Normallik testleri için referans değerler (normallik.js)
yuv <- function(x) as.numeric(sprintf("%.15g", x)) # JSON'da birebir aynı double'a dönüşsün

set.seed(20261002)
veriler <- list(
  sleep = sleep$extra,
  tooth = ToothGrowth$len,
  plant = PlantGrowth$weight,
  insect = InsectSprays$count,
  iris = iris$Sepal.Length,
  mtcars = mtcars$mpg,
  chick = chickwts$weight,
  warp = warpbreaks$breaks,
  n3a = c(1, 2, 4),
  n3b = c(1, 1, 2),
  n3c = c(2.3, 5.1, 5.2),
  n4 = c(2.1, 3.5, 3.6, 7),
  n5 = c(1, 3, 3, 4, 10),
  n6 = c(4.2, 4.4, 5.1, 5.3, 6.0, 9.9),
  n7 = yuv(round(rnorm(7, 10, 2), 3)),
  n8 = yuv(round(rnorm(8, 10, 2), 3)),
  n10 = yuv(round(runif(10), 4)),
  n11 = yuv(round(rexp(11), 4)),
  n12 = yuv(round(rnorm(12), 4)),
  n20 = yuv(round(rexp(20, 0.3), 3)),
  n50u = yuv(round(runif(50, -1, 1), 5)),
  n200 = yuv(round(rnorm(200, 100, 15), 3)),
  n101t = yuv(round(rt(101, 3), 4)),
  ikinokta50 = rep(c(0, 1), 25),
  yakin1 = c(5, 5, 5, 5.000000001, 5.000000002, 5, 5.000000001, 5),
  yakin2 = yuv(1 + c(0, 1, 2, 3, 5, 8) * 1e-11),
  exp1000 = yuv(round(rexp(1000), 5)),
  n5000 = yuv(round(rnorm(5000, 50, 10), 4))
)

g1 <- function(x) { n <- length(x); m <- mean(x); s <- sd(x); n / ((n - 1) * (n - 2)) * sum(((x - m) / s)^3) }
g2 <- function(x) { n <- length(x); m <- mean(x); s <- sd(x); n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(((x - m) / s)^4) - 3 * (n - 1)^2 / ((n - 2) * (n - 3)) }
seg1 <- function(n) sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
seg2 <- function(n) 2 * seg1(n) * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))

hesapla <- function(x) {
  n <- length(x)
  r <- list(x = x, n = n)
  if (n >= 3 && n <= 5000) { s <- shapiro.test(x); r$sw <- list(W = unname(s$statistic), p = s$p.value) }
  if (n >= 5) { s <- nortest::lillie.test(x); r$ks <- list(D = unname(s$statistic), p = s$p.value) }
  if (n >= 8) {
    s <- nortest::ad.test(x); r$ad <- list(A = unname(s$statistic), p = s$p.value)
    a <- moments::agostino.test(x); r$skew <- list(b1 = unname(a$statistic[1]), z = unname(a$statistic[2]), p = a$p.value)
    k <- tryCatch(suppressWarnings(moments::anscombe.test(x)), error = function(e) NULL)
    if (!is.null(k)) r$kurt <- list(b2 = unname(k$statistic[1]), z = unname(k$statistic[2]), p = k$p.value)
  }
  if (n >= 20) { o <- fBasics::dagoTest(x); r$omni <- list(K2 = unname(o@test$statistic[1]), p = unname(o@test$p.value[1])) }
  if (n >= 4) {
    r$spss <- list(g1 = g1(x), seg1 = seg1(n), g2 = g2(x), seg2 = seg2(n))
  }
  r
}
testler <- lapply(veriler, hesapla)

zler <- c(-60, -45, -38.5, -31, -29.9, -12, -3, 0, 2, 5.5, 9, 40)
logp <- data.frame(z = zler, lp = pnorm(zler, log.p = TRUE))

sonuc <- list(testler = testler, logp = logp)
