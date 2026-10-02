# İstatistik Asistanı

Akademik yayınlar ve tezler için **otomatik istatistik programı**. Verinizi yüklersiniz, araştırma sorunuzu seçersiniz. Doğru testi program seçer, varsayımları sınar, seçimin nedenini açıklar ve yayına hazır tablo, grafik ve raporlama satırı üretir.

- **Kurulum gerekmez:** Tarayıcıda çalışır. Tek bir HTML dosyasıdır; internetsiz de kullanılabilir.
- **Veriler bilgisayarınızda kalır:** Tüm hesaplar tarayıcınızda yapılır, hiçbir veri sunucuya gönderilmez. Sayfanın dış bağlantı kurması güvenlik ilkesiyle (CSP `connect-src 'none'`) engellenmiştir. Hasta verisiyle (KVKK) çalışmaya uygundur.
- **Doğruluğu test edildi:** Her istatistiksel fonksiyon R ile karşılaştırılarak test edilmiştir. Toplam 376 otomatik test vardır; sonuçlar çoğunlukla 10⁻¹⁰ göreli hassasiyetle R ile aynıdır.
- **SPSS ile uyumlu:** Varsayılan tanımlar SPSS'inkilerle aynıdır: Levene ortalamaya göre, çeyreklikler "ağırlıklı ortalama" yöntemiyle, Mann-Whitney süreklilik düzeltmesiz Z, Huynh-Feldt özgün formülle, Tip III kareler toplamı.

## Kullanım

**Çevrim içi:** GitHub Pages adresini açın.
**Çevrim dışı:** `index.html` dosyasını indirip çift tıklayın.

1. **Veri:** Excel (.xlsx, .xls, .ods), CSV/metin ya da SPSS (.sav, .zsav) dosyasını sürükleyin ya da Excel'den yapıştırın. Türkçe ondalık virgül ve Türkçe karakterler (UTF-8 / Windows-1254) otomatik tanınır. Excel'deki birden çok sayfadan biri seçilebilir.
2. **Değişkenler:** Program her değişkenin türünü (sürekli, sıralı, kategorik, kimlik) tahmin eder. Emin olmadığı değişkenleri "Kontrol edin" olarak işaretler.
   - Grup değişkenini, tekrarlı ölçümleri (ör. `VAS_0`, `VAS_1ay`), ölçek maddelerini ve sağkalım değişkenlerini kendisi bulur.
   - Kodlu kategorilere etiket ekleyebilir, eksik değer kodlarını ve kategori sırasını belirleyebilirsiniz.
   - Formülle yeni değişken hesaplayabilir; değişkenleri gruplara ayırabilir, yeniden kodlayabilir, ölçek puanı (ters maddelerle) ve tarih farkı hesaplayabilirsiniz.
   - Vaka seçimi (filtre) uygulayabilirsiniz.
3. **Analiz:**
   - **Otomatik analiz:** Betimsel istatistikleri, Tablo 1'i, tekrarlı ölçümleri, korelasyonları ve sağkalımı tek tıkla yapar.
   - **Soruya göre analiz:** Araştırma sorunuza uyan kartı seçersiniz.
4. **Rapor:** Her sonuçla birlikte şunlar verilir:
   - "Neden bu test?" açıklaması
   - akademik (APA) düzende tablolar
   - grafikler (SVG / yüksek çözünürlüklü PNG)
   - kopyalanabilir raporlama satırı (ör. `t(198) = 4,01; p < 0,001; d = 0,57`)

   Tüm rapor Word (.docx) ya da HTML olarak indirilebilir, yazdırılabilir. Projenin tamamı (veri, ayarlar, analizler) bir dosyaya kaydedilip yeniden açılabilir.

## Analizler

| Soru | Program neye bakar | Seçilen yöntemler |
|---|---|---|
| Gruplar arasında fark var mı? (Tablo 1) | Değişken türü, her grupta normallik, Levene | t testi (Student/Welch), Mann-Whitney U, ANOVA + Tukey, Welch ANOVA + Games-Howell, Kruskal-Wallis + Dunn (Bonferroni), ki-kare, Fisher, Fisher-Freeman-Halton |
| Önce–sonra / tekrarlı ölçüm | Farkların ya da ölçümlerin normalliği, Mauchly | Eşleştirilmiş t, Wilcoxon, tekrarlı ölçümler ANOVA (Greenhouse-Geisser / Huynh-Feldt), Friedman + ikili Wilcoxon, karma desen ANOVA, McNemar, McNemar-Bowker, Cochran Q |
| İki değişken ilişkili mi? | Normallik, ölçek | Pearson, Spearman, Kendall tau-b, kısmi korelasyon; çapraz tablo, OR, RR, Mantel-Haenszel |
| Bilinen değerle karşılaştırma | Normallik | Tek örneklem t, Wilcoxon, binom, ki-kare uyum iyiliği |
| Etkileyen faktörler | Sonuç değişkeninin türü | Doğrusal regresyon (VIF, Durbin-Watson, Breusch-Pagan, Cook), ikili / ordinal / multinomial lojistik regresyon (Hosmer-Lemeshow, Nagelkerke, ayrışma denetimi), tarama (p < 0,20) ve adımsal seçim |
| Tanısal doğruluk | — | ROC, AUC (DeLong), Youden kesim değeri, duyarlılık, özgüllük, PPD, NPD, olabilirlik oranları, ROC karşılaştırması |
| Sağkalım | — | Kaplan-Meier, log-rank, Breslow, Tarone-Ware, Cox regresyonu, orantılı tehlikeler (Schoenfeld) |
| Ölçüm uyumu | Ölçek | Cohen kappa, ağırlıklı kappa, Fleiss kappa, ICC (tüm modeller), Bland-Altman, Lin uyum katsayısı |
| Ölçek güvenirliği | — | Cronbach alfa (Feldt GA), madde-toplam istatistikleri, McDonald omega, yarıya bölme |
| Yapı geçerliği | — | KMO, Bartlett, paralel analiz, temel bileşenler / temel eksen / en çok olabilirlik, varimax / promax / oblimin; doğrulayıcı faktör analizi (CFI, TLI, RMSEA, SRMR, CR, AVE) |
| Örneklem büyüklüğü | — | t testleri, ANOVA, oranlar, korelasyon, ki-kare, regresyon, lojistik, sağkalım (G*Power / pwr ile aynı) |

### Normallik nasıl değerlendiriliyor?

Varsayılan "otomatik" yöntem örneklem büyüklüğünü dikkate alır (Kim HY, 2013):

| Örneklem | Ölçüt |
|---|---|
| n ≤ 50 | Shapiro-Wilk |
| 50 < n ≤ 300 | Shapiro-Wilk; anlamlıysa çarpıklık/basıklık z değerleri ±3,29 içinde mi? |
| n > 300 | \|çarpıklık\| ≤ 2 ve \|basıklık\| ≤ 7 |

Ayarlardan şu seçeneklere geçilebilir:
- SPSS alışkanlığı: n < 50 Shapiro-Wilk, n ≥ 50 Kolmogorov-Smirnov
- yalnızca Shapiro-Wilk
- yalnızca Kolmogorov-Smirnov (Lilliefors)
- çarpıklık/basıklık sınırı

### Ayarlar

Ondalık ayırıcı (virgül/nokta), basamak sayısı, APA tarzı p değeri, anlamlılık düzeyi, güven düzeyi, normallik yöntemi, Welch tercihi, Yates düzeltmesi, sürekli değişken gösterimi (ort ± SS / medyan (Ç1–Ç3) / medyan (min–maks)), çeyreklik yöntemi (SPSS / R), çoklu karşılaştırma düzeltmesi ve grafik teması (yayın / renkli).

## Doğruluk

R'nin WebAssembly sürümü (webR, R 4.6) Node içinde çalıştırılır. Her modülün R betiği (`arac/r-referans/*.R`) referans değerleri üretir; testler (`test/*.test.js`) programın sonuçlarını bunlarla karşılaştırır. Karşılaştırma için kullanılan R kaynakları:

- R tabanı: `t.test`, `wilcox.test`, `fisher.test`, `TukeyHSD` …
- Paketler: car, emmeans, survival, pROC, irr, psych, lavaan, GPArotation, pwr, nortest, haven, ResourceSelection, MASS, ordinal, VGAM

Doğrulama sırasında R'de bulunup belgelenen sınırlar:
- `ptukey`, küçük serbestlik derecesinde 10⁻⁴'e varan hata verir; bu programın hesabı kesin değerle 10⁻¹³'e kadar tutar.
- R 4.6'da vektörel `pwilcox` çağrısı yanlış sonuç verir.
- `multcomp` paketinin Dunnett kritik değeri yaklaşık hesaplanır.
- `psych::principal` varimax döndürmesi 10⁻³ düzeyinde erken durur.

## Geliştirme

Node.js 20 veya üstü gerekir.

```bash
npm install
npm test               # tüm testler (ağ ve R gerektirmez)
npm run build          # index.html (tek dosya) üretir
npm run dev            # http://localhost:5173 (her istekte yeniden derler)
npm run r-referans     # R referanslarını yeniden üretir (webR indirir; uzun sürebilir)
```

Kod yapısı ve kurallar [GELISTIRME.md](GELISTIRME.md) dosyasındadır:

| Klasör | İçerik |
|---|---|
| `src/cekirdek` | saf hesap modülleri |
| `src/veri` | dosya okuma, değişken tanıma |
| `src/karar` | test seçim kuralları ve analiz düzenleyicileri |
| `src/rapor` | tablo, Word, HTML |
| `src/grafik` | SVG grafikler |
| `src/arayuz` | kullanıcı arayüzü |

## Sınırlar

- Program doğru testi seçmeye ve hesabı hatasız yapmaya yardım eder; araştırma tasarımı, çoklu karşılaştırma stratejisi ve sonuçların bilimsel yorumu araştırmacının ve danışmanının sorumluluğundadır. Raporda yorum paragrafı üretilmez; yalnızca tablolar, karar gerekçeleri ve standart raporlama satırları verilir.
- Ağırlıklı veri (frekans/olgu ağırlığı), çok düzeyli (karma etkili) modeller, yapısal eşitlik modellerinde yol analizi ve zamana bağlı Cox modelleri yoktur.
- Çok büyük veri setlerinde (yüz binlerce satır) tarayıcı yavaşlayabilir.

## Lisans

MIT
