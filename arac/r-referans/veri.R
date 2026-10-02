# paketler: haven, writexl
# Veri içe aktarma katmanı (src/veri) için test dosyaları ve beklenen değerler.
# haven::write_sav ile üç sıkıştırma türünde (.sav: none, byte; .zsav) dosya yazılır,
# haven::read_sav(user_na = TRUE) ile geri okunup her değer, etiket, değer etiketi ve
# eksik değer tanımı JSON'a dökülür. Sayılar "%.17g" ile metin olarak yazılır ki
# JavaScript tarafında bit düzeyinde aynı double elde edilsin.
library(haven)

set.seed(20261002)
n <- 40

tam <- function(x) ifelse(is.na(x), NA_character_, sprintf("%.17g", as.numeric(x)))

# ---- Ana örnek veri seti ----
df <- data.frame(id = 1:n)
yas <- round(rnorm(n, 45, 12), 1); yas[c(3, 17)] <- NA
df$yas <- yas
attr(df$yas, "label") <- "Yaş (yıl)"
attr(df$yas, "format.spss") <- "F5.1"

df$cinsiyet <- labelled(sample(c(1, 2), n, TRUE), c(Erkek = 1, "Kadın" = 2), label = "Cinsiyet")

memn <- sample(c(1:5, 8, 9), n, TRUE); memn[1:3] <- c(8, 9, 5)
df$memnuniyet <- labelled_spss(memn,
  labels = c("Hiç memnun değil" = 1, "Çok memnun" = 5, "Bilmiyorum" = 8, "Cevap yok" = 9),
  na_values = c(8, 9), label = "Memnuniyet düzeyi (1-5)")

gelir <- round(runif(n, 1000, 90000), 2); gelir[c(2, 9)] <- c(-1, 0); gelir[c(5, 30)] <- c(-7.5, 999)
df$gelir <- labelled_spss(gelir, labels = c("Yanıt yok" = 999), na_range = c(-Inf, 0), na_values = 999,
  label = "Aylık gelir (TL)")
attr(df$gelir, "format.spss") <- "F10.2"

sist <- round(rnorm(n, 125, 25)); sist[c(4, 20)] <- c(-99, 251); sist[7] <- NA
df$kan_basinci_sistolik_olcumu <- sist
attr(df$kan_basinci_sistolik_olcumu, "label") <- "Sistolik kan basıncı (mmHg)"
attr(df$kan_basinci_sistolik_olcumu, "format.spss") <- "F6.0"
attr(df$kan_basinci_sistolik_olcumu, "display_width") <- 14

df$egitim_duzeyi_aciklamasi <- factor(sample(c("İlkokul", "Ortaokul", "Lise", "Üniversite", "Lisansüstü"), n, TRUE),
  levels = c("İlkokul", "Ortaokul", "Lise", "Üniversite", "Lisansüstü"), ordered = TRUE)
df$bolum <- factor(sample(c("Dahiliye", "Göğüs Hastalıkları", "Çocuk Sağlığı"), n, TRUE))

kisa <- sample(c("E", "H", "X", "Z", ""), n, TRUE); kisa[1:5] <- c("E", "H", "X", "Z", "")
df$kisa_metin <- labelled_spss(kisa, labels = c(Evet = "E", "Hayır" = "H", Bilinmiyor = "X"),
  na_values = c("X", "Z"), label = "Kısa metin (≤ 8 bayt)")

orta <- sample(c("çok iyi durumda", "kötü", "orta düzeyde iyileşme", "belirsiz-kayıt", "ŞIĞÜÖÇ ışğüöç İi"), n, TRUE)
orta[1:5] <- c("çok iyi durumda", "kötü", "orta düzeyde iyileşme", "belirsiz-kayıt", "ŞIĞÜÖÇ ışğüöç İi")
df$orta_metin <- labelled_spss(orta, labels = c("Kötü durum" = "kötü", "Çok iyi" = "çok iyi durumda"),
  na_values = c("belirsiz-kayıt"), label = "Uzun metin (9-255 bayt) ve etiketleri")

orta2 <- sample(c("eksik-veri-1", "bilinmiyor", "yok-yanit", "geçerli yanıt", "başka bir yanıt"), n, TRUE)
orta2[1:5] <- c("eksik-veri-1", "bilinmiyor", "yok-yanit", "geçerli yanıt", "başka bir yanıt")
df$orta_metin_uc_eksik <- labelled_spss(orta2, na_values = c("eksik-v", "bilinmiy", "yok-yanit"),
  label = "Üç eksik değerli uzun metin")

ar <- round(rnorm(n, 50, 30)); ar[1:4] <- c(-5, 0, 100, 101)
df$aralik_eksik <- labelled_spss(ar, na_range = c(100, Inf), labels = c("Tavan" = 100), label = "Yalnız aralık (100 ve üstü)")
uc <- sample(c(1:4, -9, -8, 99), n, TRUE); uc[1:3] <- c(-9, -8, 99)
df$uc_tekil_eksik <- labelled_spss(uc, na_values = c(-9, -8, 99), labels = c("Bilmiyor" = -9, "Yanıtsız" = -8, "Uygulanamaz" = 99, "Orta" = 2.5))
attr(df$uc_tekil_eksik, "format.spss") <- "F4.1"

uzun <- vapply(1:n, function(i) {
  k <- (i * 37) %% 700 + 1
  parca <- paste0(sprintf("[%03d]", 1:200), c("ağaç", "şiş", "ılık", "İğne", "öğün", "üç"))
  s <- paste(parca, collapse = "")
  substr(s, 1, k)
}, "")
uzun[2] <- ""
uzun[3] <- strrep("ş", 300)   # 600 bayt (UTF-8)
df$uzun_metin <- uzun
attr(df$uzun_metin, "label") <- "Çok uzun metin (> 255 bayt)"

dt <- as.Date("2000-01-01") + round(runif(n, -30000, 9000)); dt[6] <- NA
dt[1:4] <- as.Date(c("1970-01-01", "1899-12-31", "1582-10-15", "2026-10-02"))
df$dogum_tarihi <- dt

zaman <- as.POSIXct("2020-03-11 08:30:00", tz = "UTC") + round(runif(n, 0, 3e7)); zaman[8] <- NA
zaman[1] <- as.POSIXct("1969-12-31 23:59:59", tz = "UTC")
df$olcum_zamani <- zaman

df$sure <- hms::hms(seconds = c(0, 59, 3600, 86399, 90061, round(runif(n - 5, 0, 20000))))

buyuk <- c(1e300, -1e-300, 2^53, 0.1, 1/3, -0, pi, -123456.789, 1e-5, 123456789012, rnorm(n - 10))
df$buyuk <- buyuk
attr(df$buyuk, "format.spss") <- "F12.4"

df$tamamen_eksik <- rep(NA_real_, n)

df$Değişken_Türkçe_Ad_ıİşŞğĞ <- round(runif(n, 0, 10))

ciktilar <- list()
for (s in c("none", "byte", "zsav")) {
  dosya <- sprintf("/cikti/iceaktar-ornek-%s.%s", s, if (s == "zsav") "zsav" else "sav")
  write_sav(df, dosya, compress = s)
  ciktilar[[s]] <- dosya
}

dok <- function(dosya) {
  d <- read_sav(dosya, user_na = TRUE)
  degiskenler <- lapply(names(d), function(ad) {
    x <- d[[ad]]
    sinif <- class(x)
    fmt <- attr(x, "format.spss")
    lab <- attr(x, "labels")
    nav <- attr(x, "na_values")
    nar <- attr(x, "na_range")
    if (inherits(x, "Date")) {
      degerler <- tam(as.numeric(x) * 86400000); tur <- "tarih"
    } else if (inherits(x, "POSIXct")) {
      degerler <- tam(as.numeric(x) * 1000); tur <- "tarih"
    } else if (inherits(x, "hms")) {
      degerler <- tam(as.numeric(x)); tur <- "sure"
    } else if (is.character(x)) {
      degerler <- as.character(unclass(x)); tur <- "metin"
    } else {
      degerler <- tam(unclass(x)); tur <- "sayi"
    }
    list(
      ad = ad,
      sinif = sinif,
      tur = tur,
      etiket = if (is.null(attr(x, "label", exact = TRUE))) "" else attr(x, "label", exact = TRUE),
      format = if (is.null(fmt)) NULL else fmt,
      genislik = if (is.null(attr(x, "display_width", exact = TRUE))) NULL else attr(x, "display_width", exact = TRUE),
      degerEtiketleri = if (is.null(lab)) NULL else lapply(seq_along(lab), function(i)
        list(deger = if (is.character(lab)) unname(lab[i]) else tam(unname(lab[i])), etiket = names(lab)[i])),
      eksikTekil = if (is.null(nav)) NULL else if (is.character(nav)) as.list(nav) else as.list(tam(nav)),
      eksikAralik = if (is.null(nar)) NULL else as.list(ifelse(is.infinite(nar), ifelse(nar > 0, "Inf", "-Inf"), tam(nar))),
      degerler = as.list(degerler),
      eksik = as.list(is.na(x))
    )
  })
  list(satirSayisi = nrow(d), degiskenler = degiskenler)
}

ornek <- lapply(ciktilar, dok)
# haven'ın yazdığı ölçüm düzeyi (ReadStat): ordered → ordinal, factor/character → nominal, diğerleri → scale
olcumler <- lapply(df, function(x) if (is.ordered(x)) "sirali" else if (is.factor(x) || is.character(x)) "kategorik" else "surekli")

# ---- Başarım testi: 50 000 satır ----
N <- 50000
b <- data.frame(id = 1:N)
b$grup <- labelled(sample(1:3, N, TRUE) * 1.0, c(Kontrol = 1, "Deney A" = 2, "Deney B" = 3), label = "Grup")
b$yas <- round(rnorm(N, 40, 15)); b$yas[sample(N, 500)] <- NA
b$skor <- rnorm(N, 100, 15)
b$cinsiyet <- labelled(sample(c(1, 2, 9), N, TRUE, prob = c(.48, .48, .04)), c(Erkek = 1, "Kadın" = 2))
b$tarih <- as.Date("2015-01-01") + sample(0:3650, N, TRUE)
b$sehir <- sample(c("Ankara", "İzmir", "Diyarbakır", "Eskişehir", "Muş"), N, TRUE)
b$puan <- sample(c(1:100), N, TRUE) / 4
write_sav(b, "/cikti/iceaktar-buyuk.sav", compress = "byte")
bb <- read_sav("/cikti/iceaktar-buyuk.sav")
satirlar <- c(1, 2, 12345, 25000, 49999, 50000)
buyuk_ozet <- list(
  satirSayisi = nrow(bb),
  adlar = names(bb),
  toplamYas = tam(sum(bb$yas, na.rm = TRUE)), eksikYas = sum(is.na(bb$yas)),
  toplamSkor = tam(sum(bb$skor)), toplamPuan = tam(sum(bb$puan)),
  toplamGrup = tam(sum(unclass(bb$grup))),
  toplamTarih = tam(sum(as.numeric(bb$tarih)) * 86400000),
  sehirFrekans = as.list(table(bb$sehir)),
  ornekSatirlar = lapply(satirlar, function(i) list(
    satir = i - 1, id = tam(bb$id[i]), grup = tam(unclass(bb$grup)[i]), yas = tam(bb$yas[i]),
    skor = tam(bb$skor[i]), cinsiyet = tam(unclass(bb$cinsiyet)[i]),
    tarih = tam(as.numeric(bb$tarih[i]) * 86400000), sehir = bb$sehir[i], puan = tam(bb$puan[i])))
)

# ---- R'nin yazdığı Türkçe CSV (write.csv2: ; ayırıcı, ondalık virgül) ----
cs <- data.frame(
  Hasta = c("Ayşe Yılmaz", "Ömer Çelik", "İsmail Şahin", "Gülşen Öztürk", "Işıl Ağaoğlu"),
  Yas = c(34L, 51L, NA, 28L, 67L),
  Kilo = c(61.5, 88.25, 70, NA, 79.125),
  Grup = c("Kontrol", "Deney", "Deney", "Kontrol", "Deney; özel"),
  stringsAsFactors = FALSE)
write.csv2(cs, "/cikti/iceaktar-r-csv2-utf8.csv", row.names = FALSE, na = "")
csv_okunan <- read.csv2("/cikti/iceaktar-r-csv2-utf8.csv", stringsAsFactors = FALSE, na.strings = "")
csv2 <- list(
  adlar = names(csv_okunan),
  Hasta = csv_okunan$Hasta, Yas = as.list(tam(csv_okunan$Yas)),
  Kilo = as.list(tam(csv_okunan$Kilo)), Grup = csv_okunan$Grup)

# ---- writexl ile bağımsız bir yazıcının ürettiği .xlsx ----
xl <- data.frame(
  ad = c("Çağrı", "Şule", "İlker", "Ilgın"),
  deger = c(1.5, -2.25, NA, 1e10),
  tarih = as.Date(c("2021-03-04", "1999-12-31", NA, "1900-03-01")),
  zaman = as.POSIXct(c("2021-03-04 13:45:30", NA, "2000-01-01 00:00:00", "2010-06-15 23:59:59"), tz = "UTC"),
  mantik = c(TRUE, FALSE, NA, TRUE),
  stringsAsFactors = FALSE)
writexl::write_xlsx(list(Veriler = xl, `İkinci Sayfa` = data.frame(x = 1:3)), "/cikti/iceaktar-writexl.xlsx")
xlsx <- list(
  ad = xl$ad, deger = as.list(tam(xl$deger)),
  tarih = as.list(tam(as.numeric(xl$tarih) * 86400000)),
  zaman = as.list(tam(as.numeric(xl$zaman) * 1000)))

sonuc <- list(ornek = ornek, olcumler = olcumler, buyuk = buyuk_ozet, csv2 = csv2, xlsx = xlsx)
