// Ayarlar penceresi.

import { AYAR_ACIKLAMALARI } from '../karar/ayarlar.js';
import { ayarDegistir, ayarlariSifirla, durum } from './durum.js';
import { depo, h } from './dom.js';
import { pencereAc } from './pencere.js';

const SECENEKLER = [
  { baslik: 'Genel' },
  { anahtar: 'alfa', etiket: 'Anlamlılık düzeyi (α)', secenekler: [[0.05, '0,05'], [0.01, '0,01'], [0.1, '0,10']] },
  { anahtar: 'guvenDuzeyi', etiket: 'Güven aralığı düzeyi', secenekler: [[0.95, '%95'], [0.99, '%99'], [0.9, '%90']] },
  { baslik: 'Sayı biçimi' },
  { anahtar: 'ondalikAyrac', etiket: 'Ondalık ayırıcı', secenekler: [[',', 'Virgül (12,34) — Türkçe'], ['.', 'Nokta (12.34) — İngilizce']] },
  { anahtar: 'basamak', etiket: 'Ondalık basamak', secenekler: [[1, '1'], [2, '2'], [3, '3']] },
  { anahtar: 'pBasamak', etiket: 'p değeri basamağı', secenekler: [[3, '3 (p = 0,034)'], [2, '2 (p = 0,03)'], [4, '4']] },
  { anahtar: 'pBasSifir', etiket: 'p değerinde baştaki sıfır', secenekler: [[true, '0,034'], [false, ',034 (APA)']] },
  { baslik: 'Test seçimi' },
  { anahtar: 'normallikYontemi', etiket: 'Normallik değerlendirmesi', secenekler: [['otomatik', 'Otomatik (örneklem büyüklüğüne göre, önerilen)'], ['spss', 'n < 50 Shapiro-Wilk, n ≥ 50 Kolmogorov-Smirnov'], ['shapiro', 'Her zaman Shapiro-Wilk'], ['lilliefors', 'Her zaman Kolmogorov-Smirnov (Lilliefors)'], ['carpiklik-basiklik', 'Çarpıklık ve basıklık']] },
  { anahtar: 'carpiklikSiniri', etiket: 'Çarpıklık/basıklık sınırı', secenekler: [[1, '±1'], [1.5, '±1,5 (Tabachnick ve Fidell)'], [2, '±2 (George ve Mallery)']], gorunur: (a) => a.normallikYontemi === 'carpiklik-basiklik' },
  { anahtar: 'tTestiVaryans', etiket: 'İki grup t testi', secenekler: [['levene', 'Levene testine göre Student ya da Welch'], ['welch', 'Her zaman Welch']] },
  { anahtar: 'ikiliTabloYates', etiket: '2×2 tablolarda ki-kare', secenekler: [[false, 'Pearson ki-kare'], [true, 'Yates düzeltmeli ki-kare']] },
  { anahtar: 'siraliTest', etiket: 'Sıralı kategorik değişkenler', secenekler: [['sira', 'Sıra testleri (Mann-Whitney, Kruskal-Wallis)'], ['kikare', 'Ki-kare / Fisher']] },
  { anahtar: 'parametrikOlmayanDuzeltme', etiket: 'Parametrik olmayan ikili karşılaştırma düzeltmesi', secenekler: [['bonferroni', 'Bonferroni'], ['holm', 'Holm'], ['BH', 'Benjamini-Hochberg']] },
  { anahtar: 'postHocHerZaman', etiket: 'İkili karşılaştırmalar', secenekler: [[false, 'Yalnızca genel test anlamlıysa'], [true, 'Her zaman göster']] },
  { anahtar: 'korelasyonYontemi', etiket: 'Korelasyon', secenekler: [['otomatik', 'Otomatik (Pearson / Spearman)'], ['pearson', 'Her zaman Pearson'], ['spearman', 'Her zaman Spearman'], ['kendall', 'Her zaman Kendall tau-b']] },
  { baslik: 'Tablolar' },
  { anahtar: 'surekliGosterim', etiket: 'Sürekli değişkenlerin gösterimi', secenekler: [['otomatik', 'Otomatik (normal: ort ± SS, değilse medyan (Ç1–Ç3))'], ['ortss', 'Her zaman ortalama ± SS'], ['medyan-ceyrek', 'Her zaman medyan (Ç1–Ç3)'], ['medyan-minmaks', 'Her zaman medyan (min–maks)'], ['ikisi', 'Hem ortalama ± SS hem medyan (Ç1–Ç3)']] },
  { anahtar: 'yuzdelikTipi', etiket: 'Çeyreklik hesabı', secenekler: [[6, 'SPSS ile aynı'], [7, 'R / Excel ile aynı']] },
  { anahtar: 'kategorikYuzde', etiket: 'Çapraz tablo yüzdeleri', secenekler: [['sutun', 'Grup içi (sütun) yüzde'], ['satir', 'Satır yüzdesi']] },
  { baslik: 'Grafikler' },
  { anahtar: 'grafikTema', etiket: 'Grafik teması', secenekler: [['yayin', 'Yayın (siyah-beyaz)'], ['renkli', 'Renkli']] },
  { anahtar: 'grafikYaziTipi', etiket: 'Grafik yazı tipi', secenekler: [['Arial, Helvetica, sans-serif', 'Arial'], ['"Times New Roman", Times, serif', 'Times New Roman'], ['Calibri, Carlito, sans-serif', 'Calibri']] },
];

export function ayarlarPenceresi() {
  const govde = h('div', { class: 'yigin' });
  const ciz = () => {
    govde.replaceChildren(
      ...SECENEKLER.filter((s) => !s.gorunur || s.gorunur(durum.ayarlar)).map((s) => {
        if (s.baslik) return h('h3', { style: 'margin:.75rem 0 0' }, s.baslik);
        const mevcut = durum.ayarlar[s.anahtar];
        return h(
          'div',
          { class: 'alan' },
          h('label', {}, s.etiket),
          h(
            'select',
            {
              onchange: (e) => {
                const secilen = s.secenekler[e.target.selectedIndex][0];
                ayarDegistir(s.anahtar, secilen);
                ciz();
              },
            },
            s.secenekler.map(([v, ad]) => h('option', { selected: v === mevcut }, ad)),
          ),
          AYAR_ACIKLAMALARI[s.anahtar] ? h('div', { class: 'aciklama' }, AYAR_ACIKLAMALARI[s.anahtar]) : null,
        );
      }),
      h('h3', { style: 'margin:.75rem 0 0' }, 'Görünüm'),
      h(
        'div',
        { class: 'alan' },
        h('label', {}, 'Tema'),
        h(
          'select',
          {
            onchange: (e) => {
              const v = e.target.value;
              if (v) document.documentElement.setAttribute('data-tema', v);
              else document.documentElement.removeAttribute('data-tema');
              depo.yaz('istatistik-asistani-tema', v);
            },
          },
          [['', 'Sistem'], ['acik', 'Açık'], ['koyu', 'Koyu']].map(([v, ad]) => h('option', { value: v, selected: (document.documentElement.getAttribute('data-tema') || '') === v }, ad)),
        ),
      ),
    );
  };
  ciz();
  pencereAc({
    baslik: 'Ayarlar',
    govde,
    dugmeler: [
      {
        metin: 'Varsayılanlara dön',
        tikla: () => {
          ayarlariSifirla();
          ciz();
          return false;
        },
      },
      { metin: 'Tamam', ana: true },
    ],
  });
}
