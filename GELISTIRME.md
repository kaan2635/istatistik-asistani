# Geliştirici Kılavuzu

Bu belge kod yazan herkes (insan ya da yapay zekâ ajanı) içindir. Kullanıcı belgesi `README.md`'dir.

## Amaç

Tarayıcıda çalışan, veriyi bilgisayardan dışarı göndermeyen akademik istatistik programı. Kullanıcı verisini yükler, değişken türlerini onaylar, araştırma sorusunu seçer; program varsayımları sınar, uygun testi seçer, nedenini açıklar ve yayına hazır tablolar üretir. Hesaplar **hatasız** olmalıdır: her istatistiksel fonksiyon R ile karşılaştırılarak test edilir.

## Klasörler

```
src/cekirdek/   Saf hesaplama (DOM yok, dış bağımlılık yok). Node'da test edilir.
src/veri/       Dosya okuma (CSV, Excel, SPSS .sav), veri seti modeli, değişken türü tanıma
src/karar/      Test seçim motoru ve analiz düzenleyicileri
src/rapor/      Tablo, raporlama satırı, Word/HTML dışa aktarım, biçimlendirme
src/grafik/     SVG grafikler (saf fonksiyonlar, SVG metni döndürür)
src/arayuz/     Kullanıcı arayüzü
test/           node:test dosyaları; test/referans: R'nin ürettiği JSON referanslar; test/veri: test verileri
arac/           Derleme, geliştirme sunucusu, R referans üretici
```

## Kod kuralları

- ES modülleri, `.js` uzantısı. Tanımlayıcılar Türkçe ve yalnızca ASCII harflerle (`ortalama`, `standartSapma`, `gruplarArasi`). Kullanıcıya görünen metinler Türkçe ve Türkçe karakterlidir.
- `src/cekirdek` içindeki dosyalar yalnızca birbirini içe aktarır. DOM, `window`, `document` kullanılmaz.
- Fonksiyonlar saf olmalı: girdiyi değiştirmez, global durum tutmaz (önbellek hariç).
- Her dışa aktarılan fonksiyonun JSDoc açıklaması olur: ne yaptığı, hangi yöntemi uyguladığı, hangi R/SPSS çıktısıyla aynı olduğu.
- Kısa ve gerekçeli yorum yazın; açık kodu yorumla tekrar etmeyin.

## Çekirdek API (kullanıma hazır)

`src/cekirdek/sayisal.js`: `lgamma`, `lbeta`, `logFaktoriyel`, `logKombinasyon`, `digamma`, `gammaP`, `gammaQ`, `ibeta(x,a,b,altKuyruk,y)`, `brent(f,a,b,tol)`, `tersCoz(g,hedef,baslangic,alt,ust)`, `integral(f,a,b,tol)`, `gaussLegendre(n)`, `logTopla`.

`src/cekirdek/dagilim.js` (R ile 1e-10 göreli doğrulukta test edildi; `altKuyruk=false` üst kuyruğu doğrudan ve hassas hesaplar):
`dnorm pnorm qnorm pnormUst qnormUst`, `dt pt ptIkiYonlu qt`, `dchisq pchisq qchisq`, `df_ pf qf`, `dbeta pbeta qbeta`, `pgamma`, `dbinom pbinom`, `dpois ppois`, `dhyper phyper`, `ptukey qtukey`, `pnt` (merkezi olmayan t), `pnf` (merkezi olmayan F), `pnchisq`, `psignrank`, `isaretliSiraFrekans`, `pwilcox`, `mannWhitneyFrekans`.
Not: R'nin `ptukey`'i ~1e-7 (sd < 5 iken ~1e-4) doğrulukta; bizimki daha hassastır. R'nin `pwilcox`/`psignrank`'ı, m ve n satırdan satıra değişen vektörel çağrılarda hatalıdır; referans üretirken satır satır çağırın (`mapply`).

`src/cekirdek/matris.js`: `sifir birim kopya transpoz carp carpVektor icCarpim XtX Xty iz cholesky lu coz ters simetrikTers determinant logDeterminant enKucukKareler simetrikOzdeger kovaryansMatrisi korelasyonMatrisi`. `enKucukKareler(X, y)` R'nin `lm()`'i gibi doğrusal bağımlı sütunları saptar (`bagimli`, katsayısı `null`).

`src/cekirdek/betimsel.js`: `temizSayilar toplam ortalama varyans standartSapma standartHata sirali minimum maksimum yuzdelik(x,p,tip) medyan carpiklik basiklik kirpilmisOrtalama mod geometrikOrtalama ortalamaGA medyanGA betimsel siralar bagToplami frekanslar`. Yüzdelik varsayılanı tip 6 (SPSS); `siralar(x)` → `{sira, baglar}` (bağlarda ortalama sıra).

`src/cekirdek/duzeltme.js`: `pDuzelt(p, yontem, m)` — `bonferroni holm hochberg hommel BH BY sidak yok` (R `p.adjust` ile aynı).

`src/cekirdek/hata.js`: `IstatistikHatasi`, `gerekli(kosul, mesaj)`.

## Girdi kuralları

- Sayısal girdiler eksik değerlerden arındırılmış `number[]` dizileridir (çağıran temizler). Gruplar `number[][]`, eşleştirilmiş/tekrarlı ölçümler `number[][]` (satır = denek, sütun = ölçüm).
- Kategorik girdiler: çapraz tablo `number[][]` (satır × sütun frekansları) ya da etiket dizileri. Fonksiyon hangisini beklediğini JSDoc'ta söyler.
- Hesaplanamayan durumda (yetersiz gözlem, sabit değişken, tekil matris) `IstatistikHatasi` fırlatın; mesaj kullanıcıya gösterilir: "Shapiro-Wilk testi için en az 3 gözlem gerekir." gibi.
- Hesap yapılabiliyor ama dikkat gerekiyorsa sonuca `uyarilar: string[]` ekleyin (ör. "Beklenen değeri 5'ten küçük hücre oranı %25.").

## Sonuç nesnesi

Hipotez testleri düz bir nesne döndürür. Ortak alanlar:

```js
{
  yontem: 'welch-t',          // değişmez kimlik (rapor katmanı adlandırma ve dil çevirisi için kullanır)
  ad: 'Welch t testi',        // Türkçe görünen ad
  istatistik: -1.8608,        // ana test istatistiği
  istatistikAdi: 't',         // simge: t, F, U, W, H, χ², z, Q ...
  sd: 17.776,                 // serbestlik derecesi: sayı, [sd1, sd2] ya da null
  p: 0.0794,                  // iki yönlü p (aksi belirtilmedikçe)
  pYontemi: 'asimptotik',     // 'kesin' | 'asimptotik' | 'monte-carlo' (yalnızca birden çok yol varsa)
  etki: { ad: 'Cohen d', sembol: 'd', deger: -0.83, gaAlt: -1.74, gaUst: 0.10 },  // varsa
  ga: { duzey: 0.95, alt: -3.37, ust: 0.21, neyin: 'ortalama farkı' },           // varsa
  n: 20,
  uyarilar: [],
  // ... teste özgü alanlar (betimsel değerler, ara hesaplar)
}
```

Bir fonksiyon birden fazla tablo üretiyorsa (regresyon, ANOVA) alanları açıkça adlandırın: `katsayilar: [{ad, B, SH, beta, t, p, gaAlt, gaUst, VIF}]`, `anovaTablosu: [{kaynak, KT, sd, KO, F, p}]` gibi.

## SPSS ve R farkları

Türkiye'de akademisyenler sonuçları çoğunlukla SPSS ile karşılaştırır. İki program farklı varsayılan kullanıyorsa **varsayılan SPSS'inki** olsun; diğeri seçenek olarak sunulsun. Doğrulama yine R ile yapılır: R'yi aynı yöntemi kullanacak şekilde ayarlayın (ör. Levene için `car::leveneTest(center = mean)`, Mann-Whitney asimptotik için `wilcox.test(correct = FALSE, exact = FALSE)`). JSDoc'ta hangi seçeneğin SPSS'e, hangisinin R'ye karşılık geldiğini yazın.

## Doğrulama: R referansları

R kurulu değildir; R'nin WebAssembly sürümü (webR) Node içinde çalışır.

1. `arac/r-referans/<modul>.R` yazın. Betik `sonuc` adında bir liste tanımlamalıdır. İlk satırlarda `# paketler: survival, pROC` yazarsanız paketler kurulur (çoğu CRAN paketi webR deposunda var; `Rmpfr`/`PMCMRplus`/`DescTools` yok).
2. `node arac/r-referans/uret.cjs <modul>` çalıştırın → `test/referans/<modul>.json` oluşur (15 anlamlı basamak, Inf → null).
3. `test/<modul>.test.js` içinde `referans('<modul>')` ile okuyup `yakin(gercek, beklenen, goreli, mutlak, mesaj)` / `yakinDerin(...)` ile karşılaştırın (`test/yardimci.js`).
4. `node --test test/<modul>.test.js` ile çalıştırın.

Veri dosyaları: `test/veri/` içindekiler R'de `/veri/` altında okunabilir. R betiği `/cikti/` altına dosya yazarsa (`write.csv(..., "/cikti/x.csv")`) `test/veri/` içine kopyalanır. Dosya adlarına modül önekini koyun (`kategorik-ornek1.csv`).

Toleranslar: hedef göreli 1e-10. R'nin kendi algoritması yaklaşıksa (ör. ptukey, AS 226) toleransı gerekçesiyle gevşetin ve mümkünse kesin bir değerle ayrıca test edin. Her test: normal durum, bağlı değerler, küçük örneklem, eşit olmayan grup büyüklükleri, uç durumlar (sıfır varyans, tek gruplu, boş hücre) ve hata mesajları.

## Paralel çalışma kuralları

- Yalnızca size atanan dosyaları oluşturun/değiştirin. `package.json`, `src/cekirdek/` altındaki ortak dosyalar (sayisal, dagilim, matris, betimsel, duzeltme, hata) ve başkasının modülü değiştirilmez. Ortak dosyada hata bulursanız düzeltmeyin; raporunuzda yazın.
- `npm test` yerine kendi test dosyalarınızı çalıştırın (diğer modüller yapım aşamasında olabilir).
- Paket eklemeyin.
