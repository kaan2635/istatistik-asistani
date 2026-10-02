# paketler: lavaan, psych
# Doğrulayıcı faktör analizi referansları (src/cekirdek/dfa.js): lavaan::cfa() varsayılanları
# (ML, işaretçi değişken, beklenen bilgi, likelihood = "normal" yani N paydası).
library(lavaan)
# webR'de parallel::detectCores() NA döndürür; lavaan'ın seçenek aralık denetimi NA ile hata verir.
# Denetim yalnızca seçenek doğrulamasıdır (hesaba etkisi yok); NA içeren aralıklar atlanır.
local({
  f0 <- lavaan:::lav_options_checkinterval
  assignInNamespace("lav_options_checkinterval", function(x, nm, num2int) {
    if (any(is.na(x)) || any(is.na(nm$bounds))) return(TRUE)
    f0(x, nm, num2int)
  }, "lavaan")
})

hs <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
write.csv(hs, "/cikti/fa-holzinger.csv", row.names = FALSE)
hs <- read.csv("/cikti/fa-holzinger.csv")
bfi25 <- psych::bfi[complete.cases(psych::bfi[, 1:25]), 1:25]
write.csv(bfi25, "/cikti/fa-bfi.csv", row.names = FALSE)
bfi25 <- read.csv("/cikti/fa-bfi.csv")

# Sıkı yakınsama: varsayılan nlminb gradyanı ~3e-7'de bırakır; BFGS + reltol 1e-16 ~1e-10'a iner.
# (nlminb'ye çok sıkı tolerans verilince "false convergence" ile model yakınsamamış sayılıyor.)
cfa2 <- function(...) cfa(..., optim.method = "BFGS", control = list(reltol = 1e-16, maxit = 100000))

ozet <- function(fit, n) {
  pe <- parameterEstimates(fit, remove.nonfree = FALSE, ci = FALSE)
  ss <- standardizedSolution(fit, ci = FALSE)
  fm <- fitMeasures(fit)
  mi <- modindices(fit, sort. = TRUE)
  X2 <- unname(fm["chisq"]); df <- unname(fm["df"])
  # RMSEA güven aralığı yüksek duyarlıkla (lavaan uniroot'u varsayılan tol ~1e-4 ile çağırır)
  lo <- if (pchisq(X2, df, ncp = 0) < 0.95) 0 else
    sqrt(uniroot(function(l) pchisq(X2, df, ncp = l) - 0.95, c(0, X2), tol = 1e-14)$root / (n * df))
  up <- sqrt(uniroot(function(l) pchisq(X2, df, ncp = l) - 0.05, c(0, max(n, 4 * X2)), tol = 1e-14)$root / (n * df))
  r2 <- lavInspect(fit, "r2")
  list(
    pe = pe[, c("lhs", "op", "rhs", "est", "se", "z", "pvalue")],
    std = ss[, c("lhs", "op", "rhs", "est.std", "se", "z", "pvalue")],
    fm = as.list(fm),
    rmsea_ci = c(lo, up),
    r2 = as.list(r2),
    mi = head(mi[, c("lhs", "op", "rhs", "mi", "epc", "sepc.all")], 15),
    mi_sayi = nrow(mi),
    gradyan = max(abs(lavInspect(fit, "optim.gradient"))),
    fx = fit@optim$fx,
    implied = unname(lavInspect(fit, "implied")$cov),
    S = unname(lavInspect(fit, "sampstat")$cov)
  )
}

m3 <- " visual =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed =~ x7 + x8 + x9 "
fit3 <- cfa2(m3, data = hs)
fit3lv <- cfa2(m3, data = hs, std.lv = TRUE)
fit3e <- cfa2(paste(m3, "\n x7 ~~ x8 \n x1 ~~ x9"), data = hs)
fit1 <- cfa2("g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9", data = hs)
fit3cov <- cfa2(m3, sample.cov = cov(hs), sample.nobs = nrow(hs))
mb <- " A =~ A1 + A2 + A3 + A4 + A5
        C =~ C1 + C2 + C3 + C4 + C5
        E =~ E1 + E2 + E3 + E4 + E5
        N =~ N1 + N2 + N3 + N4 + N5
        O =~ O1 + O2 + O3 + O4 + O5 "
fitb <- cfa2(mb, data = bfi25)

# Birleşik güvenirlik ve AOV (standartlaştırılmış çözümden)
bg <- function(fit) {
  s <- standardizedSolution(fit)
  l <- s[s$op == "=~", ]
  lapply(split(l, l$lhs), function(d) {
    th <- sapply(d$rhs, function(v) s$est.std[s$op == "~~" & s$lhs == v & s$rhs == v])
    list(cr = sum(d$est.std)^2 / (sum(d$est.std)^2 + sum(th)), ave = mean(d$est.std^2))
  })
}

sonuc <- list(
  hs3 = c(ozet(fit3, 301), list(bg = bg(fit3))),
  hs3lv = ozet(fit3lv, 301),
  hs3e = ozet(fit3e, 301),
  hs1 = ozet(fit1, 301),
  hs3cov = ozet(fit3cov, 301),
  bfi5 = c(ozet(fitb, nrow(bfi25)), list(bg = bg(fitb))),
  surum = as.character(packageVersion("lavaan"))
)
