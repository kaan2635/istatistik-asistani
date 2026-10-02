# Dağılım fonksiyonları için referans değerler
izgara <- function(...) expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

z <- c(-38, -20, -10, -5, -3, -1.96, -1, -0.5, -1e-5, 0, 0.3, 1, 2.5, 5, 8, 20)
normal <- data.frame(z = z, alt = pnorm(z), ust = pnorm(z, lower.tail = FALSE), d = dnorm(z))

pq <- c(1e-300, 1e-100, 1e-20, 1e-10, 1e-5, 0.001, 0.025, 0.1, 0.3, 0.5, 0.7, 0.975, 0.999999)
qnormal <- data.frame(p = pq, q = qnorm(pq), qust = qnorm(pq, lower.tail = FALSE))

g <- izgara(t = c(-50, -5, -2.1, -0.5, 0, 0.7, 1.96, 3, 10, 100), df = c(1, 2, 3.5, 10, 30, 100, 1000, 1e5, 1e8))
g$alt <- pt(g$t, g$df); g$ust <- pt(g$t, g$df, lower.tail = FALSE); g$d <- dt(g$t, g$df)
tdag <- g

g <- izgara(p = c(1e-10, 0.001, 0.025, 0.05, 0.3, 0.7, 0.95, 0.975, 0.9995), df = c(1, 2, 5, 17.776, 30, 1e4))
g$q <- qt(g$p, g$df)
qtdag <- g

g <- izgara(x = c(0.001, 0.5, 1, 3.84, 10, 50, 200, 1000), df = c(1, 2, 3, 7.5, 20, 100, 1000, 1e5))
g$alt <- pchisq(g$x, g$df); g$ust <- pchisq(g$x, g$df, lower.tail = FALSE); g$d <- dchisq(g$x, g$df)
kikare <- g

g <- izgara(p = c(1e-8, 0.01, 0.05, 0.5, 0.95, 0.99, 0.999999), df = c(1, 2, 4.5, 30, 500))
g$q <- qchisq(g$p, g$df)
qkikare <- g

ciftler <- list(c(1, 1), c(1, 10), c(2, 27), c(3, 100), c(5, 2.5), c(10, 1000), c(100, 1e5), c(1e4, 1e4))
fdag <- do.call(rbind, lapply(ciftler, function(d) {
  f <- c(0.01, 0.5, 1, 2.5, 4, 10, 100)
  data.frame(f = f, d1 = d[1], d2 = d[2], alt = pf(f, d[1], d[2]), ust = pf(f, d[1], d[2], lower.tail = FALSE), d = df(f, d[1], d[2]))
}))
qfdag <- do.call(rbind, lapply(ciftler[1:6], function(d) {
  p <- c(0.001, 0.05, 0.5, 0.95, 0.999)
  data.frame(p = p, d1 = d[1], d2 = d[2], q = qf(p, d[1], d[2]))
}))

g <- izgara(x = c(1e-5, 0.01, 0.2, 0.5, 0.8, 0.99), a = c(0.5, 1, 2.5, 20, 300), b = c(0.5, 3, 50))
g$alt <- pbeta(g$x, g$a, g$b); g$ust <- pbeta(g$x, g$a, g$b, lower.tail = FALSE)
betadag <- g
g <- izgara(p = c(0.001, 0.025, 0.5, 0.975, 0.999), a = c(0.5, 1, 7, 120), b = c(1, 4, 90))
g$q <- qbeta(g$p, g$a, g$b)
qbetadag <- g

binom <- rbind(
  data.frame(x = 0:5, n = 5, p = 0.3),
  data.frame(x = c(0, 3, 10, 17, 20), n = 20, p = 0.5),
  data.frame(x = c(1, 30, 50, 99), n = 100, p = 0.37),
  data.frame(x = c(400, 500, 512, 600), n = 1000, p = 0.5),
  data.frame(x = c(49000, 50000, 50300), n = 1e5, p = 0.5)
)
binom$d <- dbinom(binom$x, binom$n, binom$p)
binom$alt <- pbinom(binom$x, binom$n, binom$p)
binom$ust <- pbinom(binom$x, binom$n, binom$p, lower.tail = FALSE)

hiper <- data.frame(x = c(0, 1, 2, 3, 5, 10, 25), m = c(5, 5, 10, 10, 20, 30, 50), n = c(5, 15, 10, 30, 20, 40, 70), k = c(4, 6, 8, 12, 15, 25, 50))
hiper$d <- dhyper(hiper$x, hiper$m, hiper$n, hiper$k)
hiper$alt <- phyper(hiper$x, hiper$m, hiper$n, hiper$k)
hiper$ust <- phyper(hiper$x, hiper$m, hiper$n, hiper$k, lower.tail = FALSE)

pois <- data.frame(x = c(0, 1, 3, 10, 50, 120), lambda = c(0.5, 2, 3, 7.5, 60, 100))
pois$d <- dpois(pois$x, pois$lambda)
pois$alt <- ppois(pois$x, pois$lambda)
pois$ust <- ppois(pois$x, pois$lambda, lower.tail = FALSE)

g <- izgara(q = c(0.5, 1, 2, 3, 3.5, 4, 5, 6, 8), k = c(2, 3, 4, 5, 10, 20), df = c(2, 5, 10, 20, 60, 1000, Inf))
g$p <- ptukey(g$q, g$k, g$df)
tukey <- g
g <- izgara(p = c(0.9, 0.95, 0.99), k = c(2, 3, 5, 8), df = c(3, 10, 27, 120))
g$q <- qtukey(g$p, g$k, g$df)
qtukeydag <- g

g <- izgara(t = c(-2, 0, 1, 2.0484, 3, 5), df = c(3, 10, 38, 200), ncp = c(-1, 0.5, 2, 3.5))
g$alt <- pt(g$t, g$df, g$ncp); g$ust <- pt(g$t, g$df, g$ncp, lower.tail = FALSE)
ntdag <- g

g <- izgara(f = c(0.5, 2.5, 3.2, 6), d1 = c(1, 3, 5), d2 = c(10, 57), ncp = c(0.5, 4, 12))
g$alt <- pf(g$f, g$d1, g$d2, g$ncp); g$ust <- pf(g$f, g$d1, g$d2, g$ncp, lower.tail = FALSE)
nfdag <- g

g <- izgara(x = c(0.5, 3.84, 7.81, 15), df = c(1, 3, 6), ncp = c(0.3, 5, 15))
g$alt <- pchisq(g$x, g$df, g$ncp); g$ust <- pchisq(g$x, g$df, g$ncp, lower.tail = FALSE)
nkikare <- g

# Not: R 4.6'da pwilcox/psignrank, m ve n'nin satırdan satıra değiştiği vektörel çağrılarda
# önbelleği yenilemediği için yanlış sonuç veriyor; bu yüzden her satır ayrı çağrılır.
isaretli <- izgara(v = c(0, 3, 10, 27, 50, 100), n = c(5, 10, 20, 30))
isaretli$alt <- mapply(function(v, n) psignrank(v, n), isaretli$v, isaretli$n)
isaretli$ust <- mapply(function(v, n) psignrank(v, n, lower.tail = FALSE), isaretli$v, isaretli$n)

wil <- izgara(u = c(0, 5, 12, 30, 61, 200), m = c(3, 8, 15), n = c(4, 10, 27))
wil$alt <- mapply(function(u, m, n) pwilcox(u, m, n), wil$u, wil$m, wil$n)
wil$ust <- mapply(function(u, m, n) pwilcox(u, m, n, lower.tail = FALSE), wil$u, wil$m, wil$n)

xs <- c(1e-8, 0.1, 0.5, 1, 1.5, 2, 2.5, 3.7, 10, 14.99, 15, 33.3, 171.5, 1e3, 1e6, 1e10)
gam <- data.frame(x = xs, lg = lgamma(xs), dg = digamma(xs))
b <- izgara(a = c(0.5, 1, 7.5, 20, 1e3, 1e6), b = c(0.5, 2, 17, 40, 5e5))
b$lb <- lbeta(b$a, b$b)

sonuc <- list(
  normal = normal, qnormal = qnormal, t = tdag, qt = qtdag, kikare = kikare, qkikare = qkikare,
  f = fdag, qf = qfdag, beta = betadag, qbeta = qbetadag, binom = binom, hiper = hiper, pois = pois,
  tukey = tukey, qtukey = qtukeydag, nt = ntdag, nf = nfdag, nkikare = nkikare,
  isaretli = isaretli, wilcox = wil, gamma = gam, lbeta = b
)
