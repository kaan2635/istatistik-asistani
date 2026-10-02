# Temel modüller: p düzeltmeleri, betimsel istatistikler, matris işlemleri
p1 <- c(0.01, 0.02, 0.03, 0.04, 0.05)
p2 <- c(0.0001, 0.2, 0.004, 0.03, 0.5, 0.012, 0.9, 0.04, 0.04)
yontemler <- c("bonferroni", "holm", "hochberg", "hommel", "BH", "BY")
duzeltme <- lapply(list(p1 = p1, p2 = p2), function(p) setNames(lapply(yontemler, function(y) p.adjust(p, y)), yontemler))

set.seed(42)
x1 <- round(rnorm(37, 50, 10), 2)
x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4)
x3 <- round(rexp(25, 0.2), 3)
g2 <- function(x) { n <- length(x); m <- mean(x); s <- sd(x); n/((n-1)*(n-2)) * sum(((x-m)/s)^3) }
k2 <- function(x) { n <- length(x); m <- mean(x); s <- sd(x); n*(n+1)/((n-1)*(n-2)*(n-3)) * sum(((x-m)/s)^4) - 3*(n-1)^2/((n-2)*(n-3)) }
ozet <- function(x) list(
  x = x, ortalama = mean(x), ss = sd(x), medyan = median(x),
  q6 = unname(quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95), type = 6)),
  q7 = unname(quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95), type = 7)),
  q2 = unname(quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95), type = 2)),
  carpiklik = g2(x), basiklik = k2(x), kirpilmis = mean(x, trim = 0.05),
  ga = unname(t.test(x)$conf.int), sira = rank(x)
)
betimsel <- list(x1 = ozet(x1), x2 = ozet(x2), x3 = ozet(x3))

X <- cbind(1, mtcars$wt, mtcars$hp, mtcars$wt * 2 + 1, mtcars$qsec)
fit <- lm(mtcars$mpg ~ X - 1)
A <- crossprod(cbind(1, mtcars$wt, mtcars$hp, mtcars$qsec))
ozd <- eigen(cor(mtcars[, c("mpg", "disp", "hp", "drat", "wt")]))
matris <- list(
  y = mtcars$mpg, X = X, katsayi = unname(coef(fit)),
  A = A, Ainv = solve(A), det = det(A),
  R = cor(mtcars[, c("mpg", "disp", "hp", "drat", "wt")]), ozdeger = ozd$values
)
sonuc <- list(duzeltme = duzeltme, betimsel = betimsel, matris = matris)
