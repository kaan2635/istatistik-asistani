# paketler: survival
# Sağkalım analizi referansları: Kaplan-Meier (survfit), log-rank ailesi (survdiff + elle ağırlıklı log-rank),
# Cox regresyonu (coxph, Efron ve Breslow), concordance ve cox.zph.
library(survival)

# ---------- Veri setleri ----------
veriler <- list(
  lung = list(time = lung$time, status = lung$status - 1, grup = lung$sex),
  aml = list(time = aml$time, status = aml$status, grup = as.character(aml$x)),
  veteran = list(time = veteran$time, status = veteran$status, grup = as.character(veteran$celltype)),
  ovarian = list(time = ovarian$futime, status = ovarian$fustat, grup = ovarian$rx),
  colon = local({ d <- colon[colon$etype == 2, ][1:300, ]; list(time = d$time, status = d$status, grup = as.character(d$rx)) }),
  baglar = list(time = c(1, 2, 2, 3, 4, 5, 5, 6, 6, 6, 8, 9, 9, 12, 12, 12, 15, 20),
                status = c(1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0),
                grup = c(1, 2, 1, 2, 1, 1, 2, 2, 1, 2, 1, 2, 1, 1, 2, 2, 1, 2)),
  agirSansur = local({ set.seed(7); n <- 60; t <- round(rexp(n, 0.02), 1); s <- rbinom(n, 1, 0.15)
                      list(time = t, status = s, grup = rep(c("a", "b", "c"), length.out = n)) }),
  sonOlay = list(time = c(1, 2, 2, 3, 4, 5, 5, 6), status = c(1, 1, 0, 1, 0, 1, 1, 1), grup = c(1, 1, 1, 1, 2, 2, 2, 2)),
  yarimDuz = list(time = c(1, 2, 3, 4), status = c(1, 1, 0, 0), grup = c(1, 1, 2, 2)),
  yarimDusen = list(time = 1:6, status = c(1, 1, 1, 0, 1, 0), grup = c(1, 1, 1, 2, 2, 2)),
  hicOlay = list(time = c(3, 5, 7, 9), status = c(0, 0, 0, 0), grup = c(1, 1, 2, 2)),
  tekGozlem = list(time = 4, status = 1, grup = 1),
  sifirZaman = list(time = c(0, 0, 1, 2, 2, 3, 5), status = c(1, 0, 1, 1, 0, 1, 0), grup = c(1, 2, 1, 2, 1, 2, 1)),
  # 0.1 + 0.2 ile 0.3 R'de (aeqSurv) aynı süre sayılır; test bu veriyi JS'de aynı ifadeyle kurar
  yuvarlama = list(time = c(0.1 + 0.2, 0.3, 0.5, 0.7, 0.7 + 1e-12, 1), status = c(1, 1, 0, 1, 1, 0), grup = c(1, 2, 1, 2, 1, 2))
)

km_cikar <- function(f) {
  s <- summary(f, rmean = "common")$table
  q <- quantile(f, c(0.25, 0.5, 0.75))
  list(
    zaman = f$time, riskAltinda = f$n.risk, olay = f$n.event, sansur = f$n.censor,
    sagkalim = f$surv, sh = f$std.err * f$surv, gaAlt = f$lower, gaUst = f$upper, kumHaz = f$cumhaz,
    medyan = unname(s["median"]), medyanAlt = unname(s[8]), medyanUst = unname(s[9]),
    rmean = unname(s["rmean"]), rmeanSH = unname(s["se(rmean)"]),
    q = unname(q$quantile), qAlt = unname(q$lower), qUst = unname(q$upper),
    n = unname(s["records"]), olaySayisi = unname(s["events"])
  )
}

km <- list()
for (ad in names(veriler)) {
  v <- veriler[[ad]]
  r <- list()
  for (tip in c("log", "log-log", "plain")) {
    f <- survfit(Surv(v$time, v$status) ~ 1, conf.type = tip)
    r[[tip]] <- km_cikar(f)
  }
  f90 <- survfit(Surv(v$time, v$status) ~ 1, conf.int = 0.90)
  r$duzey90 <- km_cikar(f90)
  r$veri <- v
  km[[ad]] <- r
}

# Belirli zamanlarda sağkalım ve sayısal üst sınırlı kısıtlı ortalama
zaman_ozet <- function(v, zamanlar, tip = "log") {
  f <- survfit(Surv(v$time, v$status) ~ 1, conf.type = tip)
  s <- summary(f, times = zamanlar)
  list(istenen = zamanlar, zaman = s$time, riskAltinda = s$n.risk, olay = s$n.event, sansur = s$n.censor,
       sagkalim = s$surv, sh = s$std.err, gaAlt = s$lower, gaUst = s$upper)
}
zamanlarda <- list(
  lung = zaman_ozet(veriler$lung, c(90, 180, 365, 730, 1000, 2000)),
  baglar = zaman_ozet(veriler$baglar, c(0, 1.5, 2, 6, 7, 20, 30), "log-log"),
  sonOlay = zaman_ozet(veriler$sonOlay, c(0, 1.5, 2, 5.5, 6)),
  sifirZaman = zaman_ozet(veriler$sifirZaman, c(0, 1, 2.5, 5))
)
rmeanSayisal <- list(
  lung365 = unname(summary(survfit(Surv(time, status) ~ 1, data = lung), rmean = 365)$table[c("rmean", "se(rmean)")]),
  aml100 = unname(summary(survfit(Surv(time, status) ~ 1, data = aml), rmean = 100)$table[c("rmean", "se(rmean)")]),
  aml200 = unname(summary(survfit(Surv(time, status) ~ 1, data = aml), rmean = 200)$table[c("rmean", "se(rmean)")])
)

# ---------- Gruplu KM ----------
gruplu <- list()
for (ad in c("lung", "veteran", "ovarian", "baglar", "agirSansur", "sonOlay", "hicOlay")) {
  v <- veriler[[ad]]
  f <- survfit(Surv(v$time, v$status) ~ v$grup)
  s1 <- summary(f, rmean = "common")$table
  s2 <- summary(f, rmean = "individual")$table
  q <- quantile(f, c(0.25, 0.5, 0.75))
  gr <- list()
  for (i in seq_along(f$strata)) {
    fi <- f[i]
    gr[[i]] <- list(
      zaman = fi$time, riskAltinda = fi$n.risk, olay = fi$n.event, sansur = fi$n.censor,
      sagkalim = fi$surv, gaAlt = fi$lower, gaUst = fi$upper,
      medyan = unname(s1[i, "median"]), medyanAlt = unname(s1[i, 8]), medyanUst = unname(s1[i, 9]),
      rmeanOrtak = unname(s1[i, "rmean"]), rmeanOrtakSH = unname(s1[i, "se(rmean)"]),
      rmeanBireysel = unname(s2[i, "rmean"]), rmeanBireyselSH = unname(s2[i, "se(rmean)"]),
      q = unname(q$quantile[i, ]), qAlt = unname(q$lower[i, ]), qUst = unname(q$upper[i, ])
    )
  }
  gruplu[[ad]] <- gr
}

# ---------- Log-rank ailesi ----------
# Bağımsız (elle) ağırlıklı log-rank: survdiff'ten farklı biçimde yazıldı
wlr <- function(time, status, group, tip, rho = 0) {
  time <- aeqSurv(Surv(time, status))[, 1]  # survdiff gibi yakın süreleri birleştir
  g <- factor(group); k <- nlevels(g); gi <- as.integer(g)
  ut <- sort(unique(time[status == 1]))
  O <- E <- numeric(k); V <- matrix(0, k, k)
  for (t in ut) {
    risk <- time >= t
    n <- sum(risk); nk <- tabulate(gi[risk], k)
    dk <- tabulate(gi[time == t & status == 1], k); d <- sum(dk)
    S <- if (tip == "fh") { u <- sort(unique(time[time < t])); prod(vapply(u, function(s) 1 - sum(time == s & status == 1) / sum(time >= s), numeric(1))) } else 1
    w <- switch(tip, logrank = 1, breslow = n, tw = sqrt(n), fh = S^rho)
    O <- O + w * dk; E <- E + w * d * nk / n
    if (n > 1) V <- V + w^2 * d * (n - d) / (n - 1) * (diag(nk / n, k) - outer(nk, nk) / n^2)
  }
  idx <- which(E > 0)[-1]
  chisq <- if (length(idx)) drop(t((O - E)[idx]) %*% solve(V[idx, idx, drop = FALSE], (O - E)[idx])) else 0
  df <- sum(E > 0) - 1
  list(gozlenen = O, beklenen = E, V = V, kikare = chisq, sd = df, p = pchisq(chisq, df, lower.tail = FALSE))
}
trend <- function(r, skor) {
  U <- sum(skor * (r$gozlenen - r$beklenen)); v <- drop(t(skor) %*% r$V %*% skor)
  list(U = U, v = v, kikare = U^2 / v, p = pchisq(U^2 / v, 1, lower.tail = FALSE))
}

logrank <- list()
for (ad in c("lung", "aml", "veteran", "ovarian", "colon", "baglar", "agirSansur", "sonOlay", "sifirZaman", "yuvarlama")) {
  v <- veriler[[ad]]
  sd0 <- survdiff(Surv(v$time, v$status) ~ v$grup, rho = 0)
  sd1 <- survdiff(Surv(v$time, v$status) ~ v$grup, rho = 1)
  sd5 <- survdiff(Surv(v$time, v$status) ~ v$grup, rho = 0.5)
  r <- list(
    survdiff0 = list(gozlenen = unname(sd0$obs), beklenen = unname(sd0$exp), V = unname(sd0$var), kikare = sd0$chisq, p = sd0$pvalue),
    survdiff1 = list(gozlenen = unname(sd1$obs), beklenen = unname(sd1$exp), V = unname(sd1$var), kikare = sd1$chisq, p = sd1$pvalue),
    survdiff05 = list(gozlenen = unname(sd5$obs), beklenen = unname(sd5$exp), kikare = sd5$chisq, p = sd5$pvalue),
    elle = list(logrank = wlr(v$time, v$status, v$grup, "logrank"), breslow = wlr(v$time, v$status, v$grup, "breslow"),
                tw = wlr(v$time, v$status, v$grup, "tw"), peto = wlr(v$time, v$status, v$grup, "fh", 1)),
    n = as.vector(table(v$grup))
  )
  k <- length(unique(v$grup))
  if (!(ad %in% c("yuvarlama"))) r$trend <- lapply(r$elle, function(x) trend(x, seq_len(k)))
  # ikili karşılaştırmalar
  lev <- sort(unique(v$grup))
  if (length(lev) > 2) {
    ik <- list()
    for (a in 1:(length(lev) - 1)) for (b in (a + 1):length(lev)) {
      sec <- v$grup %in% lev[c(a, b)]
      ik[[length(ik) + 1]] <- list(
        logrank = survdiff(Surv(v$time[sec], v$status[sec]) ~ v$grup[sec])$chisq,
        breslow = wlr(v$time[sec], v$status[sec], v$grup[sec], "breslow")$kikare)
    }
    r$ikili <- ik
  }
  logrank[[ad]] <- r
}
# Olay içermeyen grup
sifirOlayGrup <- list(time = c(2, 3, 5, 6, 8, 9, 11, 12, 4, 7, 10), status = c(1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0), grup = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3))
logrank$sifirOlayGrup <- list(
  survdiff0 = with(sifirOlayGrup, { s <- survdiff(Surv(time, status) ~ grup); list(gozlenen = s$obs, beklenen = s$exp, kikare = s$chisq, p = s$pvalue) }),
  elle = with(sifirOlayGrup, list(breslow = wlr(time, status, grup, "breslow"), tw = wlr(time, status, grup, "tw"))),
  veri = sifirOlayGrup)

# ---------- Cox regresyonu ----------
uyarilariYakala <- function(ifade) {
  u <- character(0)
  sonuc <- withCallingHandlers(ifade, warning = function(w) { u <<- c(u, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(sonuc = sonuc, uyarilar = u)
}
cox_cikar <- function(formul, veri, ties, zph_terms = TRUE) {
  y <- uyarilariYakala(coxph(formul, data = veri, ties = ties, x = TRUE, y = TRUE, model = TRUE))
  fit <- y$sonuc
  X <- fit$x
  yy <- fit$y
  s <- summary(fit)
  out <- list(
    sure = unname(yy[, 1]), olay = unname(yy[, 2]), X = unname(X), adlar = colnames(X),
    B = unname(coef(fit)), SH = unname(sqrt(diag(fit$var))), V = unname(fit$var),
    z = unname(s$coefficients[, "z"]), p = unname(s$coefficients[, "Pr(>|z|)"]),
    gaAlt = unname(s$conf.int[, "lower .95"]), gaUst = unname(s$conf.int[, "upper .95"]),
    loglik = unname(fit$loglik), skor = unname(fit$score), wald = unname(fit$wald.test),
    lr = unname(s$logtest["test"]), sd = unname(s$logtest["df"]), lrP = unname(s$logtest["pvalue"]),
    waldP = unname(s$waldtest["pvalue"]), skorP = unname(s$sctest["pvalue"]),
    iter = fit$iter, concordance = unname(fit$concordance), uyarilar = y$uyarilar
  )
  if (!any(is.na(coef(fit)))) {
    z1 <- cox.zph(fit, terms = zph_terms)
    out$zph <- list(tablo = unname(z1$table), y = unname(z1$y), x = unname(z1$x), zaman = unname(z1$time), var = unname(z1$var))
    if (zph_terms) {
      z2 <- cox.zph(fit, terms = FALSE)
      out$zphSutun <- list(tablo = unname(z2$table), y = unname(z2$y))
    }
  }
  out
}

lung2 <- lung[complete.cases(lung[, c("time", "status", "age", "sex", "ph.ecog")]), ]
vet <- veteran
colon2 <- colon[colon$etype == 2, ][1:300, ]
colon2 <- colon2[complete.cases(colon2[, c("nodes")]), ]
pbc2 <- pbc[1:312, ]
pbc2$olum <- as.integer(pbc2$status == 2)
sentetik <- data.frame(
  t = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 5, 5, 6, 6, 6, 7, 8, 8, 9, 10, 10, 10, 11, 12),
  d = c(1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0),
  x1 = c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0),
  x2 = c(2.1, 3.5, 1.2, 0.5, 4.4, 2.2, 3.1, 1.9, 2.8, 0.7, 3.3, 2.0, 1.5, 2.6, 3.9, 1.1, 2.4, 3.0, 0.9, 1.8, 2.9, 3.6, 1.4, 2.7)
)
ayrisma <- data.frame(t = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), d = c(1, 1, 1, 1, 0, 1, 0, 0, 0, 0),
                      x = c(1, 1, 1, 1, 0, 1, 0, 0, 0, 0), z = c(0.5, 1.2, -0.3, 0.8, 1.1, -0.9, 0.2, 0.4, -1.5, 0.7))
dogrusal <- sentetik; dogrusal$x3 <- 2 * dogrusal$x2 + 1

cox <- list()
for (ties in c("efron", "breslow")) {
  cox[[ties]] <- list(
    lung = cox_cikar(Surv(time, status) ~ age + sex + ph.ecog, lung2, ties),
    veteran = cox_cikar(Surv(time, status) ~ trt + karno + diagtime + age + prior + celltype, vet, ties),
    aml = cox_cikar(Surv(time, status) ~ x, aml, ties),
    ovarian = cox_cikar(Surv(futime, fustat) ~ age + ecog.ps + rx, ovarian, ties),
    colon = cox_cikar(Surv(time, status) ~ rx + age + sex + nodes, colon2, ties),
    pbc = cox_cikar(Surv(time, olum) ~ age + log(bili) + albumin + edema + log(protime), pbc2, ties),
    sentetik = cox_cikar(Surv(t, d) ~ x1 + x2, sentetik, ties),
    ayrisma = cox_cikar(Surv(t, d) ~ x + z, ayrisma, ties),
    dogrusal = cox_cikar(Surv(t, d) ~ x1 + x2 + x3, dogrusal, ties)
  )
}
# Tabakalı model
tabakali <- local({
  fit <- coxph(Surv(time, status) ~ age + ph.ecog + strata(sex), data = lung2, x = TRUE, y = TRUE, model = TRUE)
  z <- cox.zph(fit)
  list(sure = lung2$time, olay = lung2$status - 1, X = unname(cbind(lung2$age, lung2$ph.ecog)), tabaka = lung2$sex,
       B = unname(coef(fit)), V = unname(fit$var), loglik = unname(fit$loglik), skor = unname(fit$score),
       wald = unname(fit$wald.test), iter = fit$iter, concordance = unname(fit$concordance),
       zph = list(tablo = unname(z$table), y = unname(z$y), x = unname(z$x), zaman = unname(z$time)))
})

sonuc <- list(km = km, zamanlarda = zamanlarda, rmeanSayisal = rmeanSayisal, gruplu = gruplu,
              logrank = logrank, cox = cox, tabakali = tabakali)
