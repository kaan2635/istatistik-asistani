// Kullanıcı ayarları ve varsayılanları. Varsayılanlar Türkiye'deki tez ve makale pratiğine göre seçilmiştir;
// her seçeneğin açıklaması ayarlar penceresinde gösterilir.

export const VARSAYILAN_AYARLAR = {
  // Genel
  alfa: 0.05,
  guvenDuzeyi: 0.95,

  // Biçim
  ondalikAyrac: ',',
  basamak: 2,
  pBasamak: 3,
  pBasSifir: true,
  ciktiDili: 'tr',

  // Normallik
  normallikYontemi: 'otomatik', // 'otomatik' | 'shapiro' | 'lilliefors' | 'spss' | 'carpiklik-basiklik'
  carpiklikSiniri: 1.5, // 'carpiklik-basiklik' yönteminde |çarpıklık| ve |basıklık| sınırı

  // Sürekli değişkenlerin gösterimi
  surekliGosterim: 'otomatik', // 'otomatik' | 'ortss' | 'medyan-ceyrek' | 'medyan-minmaks' | 'ikisi'
  yuzdelikTipi: 6, // 6: SPSS, 7: R/Excel

  // Test seçimi
  tTestiVaryans: 'levene', // 'levene': Levene'e göre Student/Welch; 'welch': her zaman Welch
  ikiliTabloYates: false, // 2×2 tabloda Yates düzeltmeli ki-kare
  siraliTest: 'sira', // sıralı değişkenler: 'sira' (Mann-Whitney/Kruskal-Wallis) | 'kikare'
  postHocHerZaman: false, // genel test anlamlı değilse de ikili karşılaştırma göster
  parametrikOlmayanDuzeltme: 'bonferroni', // Dunn ve ikili Wilcoxon için
  kategorikYuzde: 'sutun', // grup içi yüzde (sütun) | 'satir'
  korelasyonYontemi: 'otomatik', // 'otomatik' | 'pearson' | 'spearman' | 'kendall'
  etkiBuyuklugu: true,
  aykiriDegerUyarisi: true,

  // Grafik
  grafikTema: 'yayin', // 'yayin' | 'renkli'
  grafikYaziTipi: 'Arial, Helvetica, sans-serif',
  grafikler: true,
};

export const AYAR_ACIKLAMALARI = {
  alfa: 'Anlamlılık düzeyi (α). Testlerin ve varsayım denetimlerinin karar sınırı.',
  guvenDuzeyi: 'Güven aralıklarının düzeyi.',
  ondalikAyrac: 'Türkçe metinlerde virgül, İngilizce yayınlarda nokta kullanılır.',
  basamak: 'Ortalama, standart sapma gibi değerlerde ondalık basamak sayısı.',
  pBasamak: 'p değerlerinde ondalık basamak sayısı (0,001\'den küçükler "<0,001" yazılır).',
  pBasSifir: 'Kapalıysa p değerleri APA stilinde baştaki sıfır olmadan yazılır (p = ,034).',
  normallikYontemi:
    'Otomatik: n ≤ 50 Shapiro-Wilk; 50 < n ≤ 300 Shapiro-Wilk ya da çarpıklık/basıklık z değerleri ±3,29 içinde; n > 300 |çarpıklık| ≤ 2 ve |basıklık| ≤ 7 (Kim, 2013). ' +
    'SPSS alışkanlığı: n < 50 Shapiro-Wilk, n ≥ 50 Kolmogorov-Smirnov (Lilliefors).',
  surekliGosterim:
    'Otomatik: normal dağılan değişkenler ortalama ± standart sapma, dağılmayanlar medyan (1. çeyrek–3. çeyrek) olarak gösterilir.',
  yuzdelikTipi: 'Çeyrekliklerin hesap yöntemi. SPSS ile aynı sonuç için 6, R/Excel için 7.',
  tTestiVaryans: 'Levene testi anlamlıysa Welch t testi, değilse Student t testi kullanılır. "Her zaman Welch" seçeneği varyans eşitliği varsayımı gerektirmez.',
  ikiliTabloYates: '2×2 tablolarda süreklilik (Yates) düzeltmeli ki-kare raporlanır.',
  siraliTest: 'Sıralı kategorik değişkenler (evre, derece, Likert) için sıra testleri ya da ki-kare.',
  postHocHerZaman: 'Genel test anlamlı olmasa da ikili karşılaştırmaları göster.',
  parametrikOlmayanDuzeltme: 'Parametrik olmayan ikili karşılaştırmalarda çoklu karşılaştırma düzeltmesi.',
  kategorikYuzde: 'Çapraz tablolarda yüzdenin tabanı: grup içi (sütun) ya da satır.',
};

export function ayarlariBirlestir(ayarlar = {}) {
  return { ...VARSAYILAN_AYARLAR, ...ayarlar };
}
