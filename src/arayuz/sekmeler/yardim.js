// Yardım sekmesi: kullanım, test seçim mantığı, sık sorulan sorular.

import { h } from '../dom.js';

function tablo(basliklar, satirlar) {
  return h('table', {}, h('thead', {}, h('tr', {}, basliklar.map((b) => h('th', {}, b)))), h('tbody', {}, satirlar.map((s) => h('tr', {}, s.map((c) => h('td', {}, c))))));
}

export function yardimSekmesi(kap) {
  kap.append(
    h(
      'section',
      { class: 'kart yardim' },
      h('h2', {}, 'Nasıl kullanılır?'),
      h(
        'ol',
        {},
        h('li', {}, h('strong', {}, 'Veri: '), 'Excel, CSV ya da SPSS dosyanızı yükleyin. Her satır bir katılımcı, her sütun bir değişken olmalı.'),
        h('li', {}, h('strong', {}, 'Değişkenler: '), 'Program her değişkenin türünü tahmin eder (sürekli, sıralı, kategorik). "Kontrol edin" işaretli olanlara bakın; kodlu kategorilere etiket ekleyin.'),
        h('li', {}, h('strong', {}, 'Analiz: '), '"Otomatik analiz" ile tüm temel analizleri tek seferde yapın ya da araştırma sorunuza uyan kartı seçin. Hangi testin kullanılacağına program karar verir.'),
        h('li', {}, h('strong', {}, 'Rapor: '), 'Her sonuçta "Neden bu test?" bölümü, yayına hazır tablolar, grafikler ve raporlama satırı bulunur. Tabloları Word\'e kopyalayın ya da tüm raporu .docx olarak indirin.'),
      ),
      h('h3', {}, 'Test seçim mantığı'),
      h('p', {}, 'Program önce değişkenin türüne, sonra grupların ya da ölçümlerin normal dağılıp dağılmadığına, gerekirse varyansların eşitliğine bakar:'),
      tablo(
        ['Amaç', 'Veri', 'Varsayımlar sağlanıyorsa', 'Sağlanmıyorsa'],
        [
          ['İki bağımsız grup', 'Sürekli', 'Bağımsız örneklem t testi (varyanslar eşit değilse Welch t testi)', 'Mann-Whitney U testi'],
          ['İkiden çok bağımsız grup', 'Sürekli', 'Tek yönlü ANOVA + Tukey HSD (varyanslar eşit değilse Welch ANOVA + Games-Howell)', 'Kruskal-Wallis H + Dunn (Bonferroni)'],
          ['Bağımsız gruplar', 'Sıralı', '—', 'Mann-Whitney U / Kruskal-Wallis'],
          ['Bağımsız gruplar', 'Kategorik', 'Pearson ki-kare (beklenen değerler ≥ 5)', '2×2: Fisher kesin testi; r×c: Fisher-Freeman-Halton'],
          ['İki eşleştirilmiş ölçüm', 'Sürekli', 'Eşleştirilmiş t testi (farklar normal)', 'Wilcoxon işaretli sıralar testi'],
          ['İkiden çok tekrarlı ölçüm', 'Sürekli', 'Tekrarlı ölçümler ANOVA (Mauchly; gerekirse Greenhouse-Geisser/Huynh-Feldt)', 'Friedman + Bonferroni düzeltmeli Wilcoxon'],
          ['Tekrarlı ölçüm + grup', 'Sürekli', 'Karma desen ANOVA (grup × zaman)', 'Her zamanda gruplar arası, her grupta zamanlar arası parametrik olmayan testler'],
          ['Eşleştirilmiş', 'İki kategorili', 'McNemar (iki ölçüm), Cochran Q (ikiden çok)', '—'],
          ['İki sürekli değişkenin ilişkisi', 'Sürekli', 'Pearson korelasyonu', 'Spearman sıra korelasyonu'],
          ['Sonucu etkileyen faktörler', 'Sürekli sonuç', 'Çoklu doğrusal regresyon', '—'],
          ['Sonucu etkileyen faktörler', 'İki kategorili sonuç', 'İkili lojistik regresyon', '—'],
          ['Olaya kadar geçen süre', 'Süre + olay', 'Kaplan-Meier, log-rank, Cox regresyonu', '—'],
          ['Tanısal doğruluk', 'Sürekli belirteç', 'ROC analizi (DeLong), Youden kesim değeri', '—'],
          ['Gözlemciler arası uyum', 'Kategorik / sıralı / sürekli', 'Cohen/Fleiss kappa, ağırlıklı kappa, ICC, Bland-Altman', '—'],
        ],
      ),
      h('h3', {}, 'Normallik nasıl değerlendiriliyor?'),
      h(
        'p',
        {},
        'Varsayılan "otomatik" yöntem örneklem büyüklüğünü dikkate alır (Kim, 2013): n ≤ 50 iken Shapiro-Wilk testi; 50 < n ≤ 300 iken Shapiro-Wilk anlamlı olsa bile çarpıklık ve basıklık z değerleri ±3,29 içindeyse dağılım normal kabul edilir; n > 300 iken |çarpıklık| ≤ 2 ve |basıklık| ≤ 7 ölçütü kullanılır, çünkü büyük örneklemlerde normallik testleri çok küçük sapmalarda bile anlamlı çıkar. Ayarlardan SPSS alışkanlığına (n < 50 Shapiro-Wilk, n ≥ 50 Kolmogorov-Smirnov) ya da yalnızca bir teste geçebilirsiniz.',
      ),
      h('h3', {}, 'Sık sorulan sorular'),
      h('p', {}, h('strong', {}, 'Sonuçlar SPSS ile aynı mı? '), 'Varsayılan ayarlar SPSS ile aynı tanımları kullanır (Levene "ortalamaya göre", çeyreklikler "ağırlıklı ortalama", Mann-Whitney için süreklilik düzeltmesiz Z). Tüm hesaplar R yazılımının ürettiği referans değerlerle test edilmiştir. Küçük farklar varsa nedeni genellikle yöntem seçeneğidir (ör. Yates düzeltmesi, kesin p değeri).'),
      h('p', {}, h('strong', {}, '"p = 0,000" neden yok? '), 'p değeri hiçbir zaman sıfır değildir; 0,001\'den küçük değerler "p < 0,001" olarak yazılır (APA ve Vancouver önerisi).'),
      h('p', {}, h('strong', {}, 'Etki büyüklükleri nasıl yorumlanır? '), "Cohen'e (1988) göre: d için 0,2 küçük, 0,5 orta, 0,8 büyük; r için 0,1 / 0,3 / 0,5; η² için 0,01 / 0,06 / 0,14. Bu sınırlar alanınıza göre değişebilir."),
      h('p', {}, h('strong', {}, 'Verilerim güvende mi? '), 'Evet. Program tamamen tarayıcınızda çalışır; dosyanız hiçbir sunucuya gönderilmez. Sayfa bir güvenlik ilkesiyle (Content-Security-Policy) dış bağlantı kurması engellenerek yayımlanır. Programı indirip internet bağlantısı olmadan da kullanabilirsiniz.'),
      h('p', {}, h('strong', {}, 'Program bir istatistikçinin yerini tutar mı? '), 'Hayır. Program doğru testi seçmenize ve hesaplamayı hatasız yapmanıza yardımcı olur; ancak araştırma tasarımı, örneklem büyüklüğü, çoklu karşılaştırma stratejisi ve sonuçların klinik/bilimsel yorumu sizin ve danışmanınızın sorumluluğundadır.'),
    ),
  );
}
