// Test seçim kuralları. Her karar, kullanıcıya "Neden bu test?" bölümünde gösterilecek adımlarla birlikte döner.
// Kurallar ve kaynakları README'deki "Test seçim mantığı" bölümünde ayrıca açıklanmıştır.

import { basiklik, carpiklik, standartSapma } from '../cekirdek/betimsel.js';
import { lilliefors, shapiroWilk } from '../cekirdek/normallik.js';
import { levene } from '../cekirdek/varyans.js';
import * as F from '../rapor/bicim.js';

/** Bir adım kaydı: baslik (ne sınandı), metin (sonuç), durum ('tamam' | 'uyari' | 'bilgi') */
export function adim(baslik, metin, durum = 'bilgi') {
  return { baslik, metin, durum };
}

function guvenli(fn) {
  try {
    return fn();
  } catch {
    return null;
  }
}

/**
 * Normallik kararı.
 * @param {number[]} x
 * @param {object} ayarlar normallikYontemi, alfa, carpiklikSiniri
 * @param {object} b biçim ayarları
 * @returns {{n:number, normal:boolean|null, yontem:string, sw:object|null, ks:object|null, carpiklik:number, basiklik:number,
 *   zCarpiklik:number, zBasiklik:number, ozet:string}}
 */
export function normallikKarari(x, ayarlar, b = F.VARSAYILAN_BICIM) {
  const n = x.length;
  const alfa = ayarlar.alfa ?? 0.05;
  const sonuc = { n, normal: null, yontem: '', sw: null, ks: null, carpiklik: NaN, basiklik: NaN, zCarpiklik: NaN, zBasiklik: NaN, ozet: '' };
  if (n < 3) {
    sonuc.ozet = `n = ${n}; normallik sınanamaz`;
    return sonuc;
  }
  if (!(standartSapma(x) > 0)) {
    sonuc.ozet = 'tüm değerler aynı; normallik sınanamaz';
    return sonuc;
  }
  const c = carpiklik(x);
  const k = basiklik(x);
  sonuc.carpiklik = c.deger;
  sonuc.basiklik = k.deger;
  sonuc.zCarpiklik = c.deger / c.sh;
  sonuc.zBasiklik = k.deger / k.sh;
  sonuc.sw = n <= 5000 ? guvenli(() => shapiroWilk(x)) : null;
  sonuc.ks = n >= 5 ? guvenli(() => lilliefors(x)) : null;
  const swMetin = sonuc.sw ? `Shapiro-Wilk ${F.pEsitlik(sonuc.sw.p, b)}` : '';
  const ksMetin = sonuc.ks ? `Kolmogorov-Smirnov (Lilliefors) ${F.pEsitlik(sonuc.ks.p, b)}` : '';
  const ckMetin = `çarpıklık ${F.sayi(c.deger, 2, b)}, basıklık ${F.sayi(k.deger, 2, b)}`;
  const yontem = ayarlar.normallikYontemi ?? 'otomatik';

  const swKarar = () => {
    if (!sonuc.sw) return null;
    return sonuc.sw.p > alfa;
  };
  const ksKarar = () => {
    if (!sonuc.ks) return null;
    return sonuc.ks.p > alfa;
  };
  const ckKarar = (s) => Math.abs(c.deger) <= s && (Number.isNaN(k.deger) || Math.abs(k.deger) <= s);

  if (yontem === 'shapiro') {
    sonuc.yontem = 'Shapiro-Wilk';
    sonuc.normal = swKarar() ?? ksKarar();
    sonuc.ozet = swMetin || ksMetin;
  } else if (yontem === 'lilliefors') {
    sonuc.yontem = 'Kolmogorov-Smirnov (Lilliefors)';
    sonuc.normal = ksKarar() ?? swKarar();
    sonuc.ozet = ksMetin || swMetin;
  } else if (yontem === 'spss') {
    if (n < 50) {
      sonuc.yontem = 'Shapiro-Wilk (n < 50)';
      sonuc.normal = swKarar();
      sonuc.ozet = swMetin;
    } else {
      sonuc.yontem = 'Kolmogorov-Smirnov (Lilliefors) (n ≥ 50)';
      sonuc.normal = ksKarar();
      sonuc.ozet = ksMetin;
    }
  } else if (yontem === 'carpiklik-basiklik') {
    const s = ayarlar.carpiklikSiniri ?? 1.5;
    sonuc.yontem = `Çarpıklık ve basıklık (±${F.sayi(s, 1, b)})`;
    sonuc.normal = ckKarar(s);
    sonuc.ozet = ckMetin;
  } else {
    // Otomatik (Kim HY. Restor Dent Endod 2013;38:52-4)
    if (n <= 50) {
      sonuc.yontem = 'Shapiro-Wilk (n ≤ 50)';
      sonuc.normal = swKarar();
      sonuc.ozet = swMetin;
    } else if (n <= 300) {
      const sw = swKarar();
      const zUygun = Math.abs(sonuc.zCarpiklik) < 3.29 && Math.abs(sonuc.zBasiklik) < 3.29;
      if (sw) {
        sonuc.yontem = 'Shapiro-Wilk';
        sonuc.normal = true;
        sonuc.ozet = swMetin;
      } else {
        sonuc.yontem = 'Shapiro-Wilk ve çarpıklık/basıklık z değerleri (50 < n ≤ 300)';
        sonuc.normal = zUygun;
        sonuc.ozet = `${swMetin}; z(çarpıklık) = ${F.sayi(sonuc.zCarpiklik, 2, b)}, z(basıklık) = ${F.sayi(sonuc.zBasiklik, 2, b)}${zUygun ? ' (±3,29 içinde)' : ''}`;
      }
    } else {
      sonuc.yontem = 'Çarpıklık ≤ 2 ve basıklık ≤ 7 (n > 300)';
      sonuc.normal = Math.abs(c.deger) <= 2 && Math.abs(k.deger) <= 7;
      sonuc.ozet = ckMetin;
    }
  }
  return sonuc;
}

/** Normallik sonucunun kısa Türkçe ifadesi */
export function normallikIfadesi(nk) {
  if (nk.normal === null) return 'sınanamadı';
  return nk.normal ? 'normal dağılıma uygun' : 'normal dağılmıyor';
}

/**
 * Bağımsız gruplarda sürekli/sıralı değişken için test seçimi.
 * @returns {{parametrik:boolean, test:string, postHoc:string|null, normallik:object[], levene:object|null, adimlar:object[]}}
 */
export function surekliGrupTestiSec(gruplar, adlar, olcum, ayarlar, b) {
  const k = gruplar.length;
  const alfa = ayarlar.alfa ?? 0.05;
  const adimlar = [];
  const sonuc = { parametrik: false, test: null, postHoc: null, normallik: [], levene: null, adimlar };
  adimlar.push(adim('Tasarım', `${k} bağımsız grup (${adlar.map((a, i) => `${a}: n = ${gruplar[i].length}`).join('; ')}).`));

  const kucukGrup = gruplar.some((g) => g.length < 3);
  if (olcum === 'sirali') {
    adimlar.push(adim('Ölçek', 'Değişken sıralı (ordinal) ölçekte; ortalama ve standart sapma anlamlı olmadığından parametrik olmayan test kullanılır.', 'bilgi'));
  } else {
    sonuc.normallik = gruplar.map((g) => normallikKarari(g, ayarlar, b));
    const satirlar = sonuc.normallik.map((nk, i) => `${adlar[i]}: ${nk.ozet} → ${normallikIfadesi(nk)}`);
    const hepsiNormal = sonuc.normallik.every((nk) => nk.normal === true);
    const yontemAdi = sonuc.normallik.find((nk) => nk.yontem)?.yontem ?? '';
    adimlar.push(adim(`Normallik${yontemAdi ? ` (${yontemAdi})` : ''}`, satirlar.join('. ') + '.', hepsiNormal ? 'tamam' : 'uyari'));
    if (kucukGrup) adimlar.push(adim('Örneklem', 'En az bir grupta 3\'ten az gözlem var; dağılım sınanamadığından parametrik olmayan test seçildi.', 'uyari'));
    sonuc.parametrik = hepsiNormal && !kucukGrup;
  }

  if (sonuc.parametrik) {
    let esit = true;
    sonuc.levene = guvenli(() => levene(gruplar, { merkez: 'ortalama' }));
    if (sonuc.levene) {
      esit = sonuc.levene.p > alfa;
      adimlar.push(
        adim(
          'Varyans homojenliği (Levene)',
          `F(${F.sd(sonuc.levene.sd, b)}) = ${F.sayi(sonuc.levene.istatistik, 2, b)}; ${F.pEsitlik(sonuc.levene.p, b)} → varyanslar ${esit ? 'homojen' : 'homojen değil'}.`,
          esit ? 'tamam' : 'uyari',
        ),
      );
    }
    if (k === 2) {
      if (ayarlar.tTestiVaryans === 'welch') {
        sonuc.test = 'welch-t';
        adimlar.push(adim('Seçilen test', 'Gruplar normal dağıldığı için bağımsız örneklem t testi; ayarlarda "her zaman Welch" seçili olduğundan Welch düzeltmesi uygulandı.', 'tamam'));
      } else {
        sonuc.test = esit ? 'student-t' : 'welch-t';
        adimlar.push(adim('Seçilen test', esit ? 'Normal dağılım ve homojen varyans → bağımsız örneklem t testi (Student).' : 'Normal dağılım, homojen olmayan varyans → Welch t testi.', 'tamam'));
      }
    } else {
      sonuc.test = esit ? 'anova' : 'welch-anova';
      sonuc.postHoc = esit ? 'tukey' : 'games-howell';
      adimlar.push(
        adim(
          'Seçilen test',
          esit ? 'Normal dağılım ve homojen varyans → tek yönlü varyans analizi (ANOVA); ikili karşılaştırmalar Tukey HSD ile.' : 'Normal dağılım, homojen olmayan varyans → Welch ANOVA; ikili karşılaştırmalar Games-Howell ile.',
          'tamam',
        ),
      );
    }
  } else if (k === 2) {
    sonuc.test = 'mann-whitney-u';
    adimlar.push(adim('Seçilen test', `${olcum === 'sirali' ? 'Sıralı değişken' : 'Normal dağılım varsayımı sağlanmadı'} → Mann-Whitney U testi.`, 'tamam'));
  } else {
    sonuc.test = 'kruskal-wallis';
    sonuc.postHoc = 'dunn';
    const duz = ayarlar.parametrikOlmayanDuzeltme ?? 'bonferroni';
    adimlar.push(adim('Seçilen test', `${olcum === 'sirali' ? 'Sıralı değişken' : 'Normal dağılım varsayımı sağlanmadı'} → Kruskal-Wallis H testi; ikili karşılaştırmalar Dunn testi (${duz === 'bonferroni' ? 'Bonferroni' : duz} düzeltmeli) ile.`, 'tamam'));
  }
  return sonuc;
}

/** Çapraz tablonun beklenen değerleri ve küçük beklenen hücre özeti */
export function beklenenOzeti(tablo) {
  const r = tablo.length;
  const c = tablo[0].length;
  const satir = tablo.map((s) => s.reduce((a, v) => a + v, 0));
  const sutun = tablo[0].map((_, j) => tablo.reduce((a, s) => a + s[j], 0));
  const N = satir.reduce((a, v) => a + v, 0);
  let kucuk = 0;
  let enKucuk = Infinity;
  for (let i = 0; i < r; i++) {
    for (let j = 0; j < c; j++) {
      const e = (satir[i] * sutun[j]) / N;
      if (e < 5) kucuk++;
      if (e < enKucuk) enKucuk = e;
    }
  }
  return { r, c, N, kucuk, oran: kucuk / (r * c), enKucuk };
}

/**
 * Bağımsız kategorik değişkenler için test seçimi (çapraz tablo).
 * @returns {{test:'pearson'|'yates'|'fisher'|'fisher-freeman-halton', adimlar:object[], beklenen:object}}
 */
export function kategorikTestSec(tablo, ayarlar, b) {
  const adimlar = [];
  // Tamamen boş satır/sütunlar analizden önce çıkarılmış olmalıdır
  const be = beklenenOzeti(tablo);
  const oranMetni = `Beklenen değeri 5'ten küçük hücre: ${be.kucuk}/${be.r * be.c} (%${F.sayi(be.oran * 100, 1, b)}); en küçük beklenen değer ${F.sayi(be.enKucuk, 2, b)}.`;
  let test;
  if (be.r === 2 && be.c === 2) {
    if (be.kucuk > 0) {
      test = 'fisher';
      adimlar.push(adim('Beklenen değerler', oranMetni + " 2×2 tabloda beklenen değeri 5'ten küçük hücre olduğu için Fisher'in kesin testi kullanıldı.", 'uyari'));
    } else {
      test = ayarlar.ikiliTabloYates ? 'yates' : 'pearson';
      adimlar.push(adim('Beklenen değerler', oranMetni + ` Tüm beklenen değerler ≥ 5 → ${test === 'yates' ? 'Yates düzeltmeli ki-kare' : 'Pearson ki-kare'} testi.`, 'tamam'));
    }
  } else if (be.oran > 0.2 || be.enKucuk < 1) {
    test = 'fisher-freeman-halton';
    adimlar.push(adim('Beklenen değerler', oranMetni + " Hücrelerin %20'sinden fazlasında beklenen değer 5'ten küçük ya da bir hücrede 1'den küçük olduğu için ki-kare yaklaşımı güvenilir değil → Fisher-Freeman-Halton kesin testi.", 'uyari'));
  } else {
    test = 'pearson';
    adimlar.push(adim('Beklenen değerler', oranMetni + " Ki-kare varsayımı sağlanıyor (küçük beklenen hücre oranı ≤ %20, hiçbiri < 1) → Pearson ki-kare testi.", 'tamam'));
  }
  return { test, adimlar, beklenen: be };
}

/**
 * Eşleştirilmiş (tekrarlı) sürekli ölçümler için test seçimi.
 * @param {number[][]} matris n × k (tam vakalar)
 */
export function eslestirilmisTestSec(matris, adlar, olcum, ayarlar, b) {
  const k = adlar.length;
  const n = matris.length;
  const adimlar = [adim('Tasarım', `${n} denekte ${k} eşleştirilmiş ölçüm (${adlar.join(', ')}).`)];
  const sonuc = { parametrik: false, test: null, postHoc: null, normallik: [], adimlar };
  if (olcum === 'sirali') {
    adimlar.push(adim('Ölçek', 'Sıralı ölçek → parametrik olmayan test.'));
  } else if (k === 2) {
    const fark = matris.map((s) => s[1] - s[0]);
    const nk = normallikKarari(fark, ayarlar, b);
    sonuc.normallik = [nk];
    adimlar.push(adim(`Farkların normalliği${nk.yontem ? ` (${nk.yontem})` : ''}`, `${nk.ozet} → ${normallikIfadesi(nk)}.`, nk.normal ? 'tamam' : 'uyari'));
    sonuc.parametrik = nk.normal === true;
  } else {
    sonuc.normallik = adlar.map((_, j) => normallikKarari(matris.map((s) => s[j]), ayarlar, b));
    const hepsi = sonuc.normallik.every((nk) => nk.normal === true);
    adimlar.push(adim('Normallik (her ölçüm)', sonuc.normallik.map((nk, j) => `${adlar[j]}: ${nk.ozet} → ${normallikIfadesi(nk)}`).join('. ') + '.', hepsi ? 'tamam' : 'uyari'));
    sonuc.parametrik = hepsi;
  }
  if (k === 2) {
    sonuc.test = sonuc.parametrik ? 'eslestirilmis-t' : 'wilcoxon';
    adimlar.push(adim('Seçilen test', sonuc.parametrik ? 'Farklar normal dağılıyor → eşleştirilmiş örneklem t testi.' : `${olcum === 'sirali' ? 'Sıralı ölçek' : 'Farklar normal dağılmıyor'} → Wilcoxon işaretli sıralar testi.`, 'tamam'));
  } else {
    sonuc.test = sonuc.parametrik ? 'tekrarli-anova' : 'friedman';
    sonuc.postHoc = sonuc.parametrik ? 'eslestirilmis-t-bonferroni' : 'wilcoxon-bonferroni';
    adimlar.push(
      adim(
        'Seçilen test',
        sonuc.parametrik
          ? 'Tüm ölçümler normal dağılıyor → tekrarlı ölçümler varyans analizi (küresellik Mauchly testiyle sınanır); ikili karşılaştırmalar Bonferroni düzeltmeli.'
          : `${olcum === 'sirali' ? 'Sıralı ölçek' : 'Normal dağılım varsayımı sağlanmadı'} → Friedman testi; ikili karşılaştırmalar Bonferroni düzeltmeli Wilcoxon testiyle.`,
        'tamam',
      ),
    );
  }
  return sonuc;
}

/**
 * İki değişken arasındaki korelasyon yöntemi.
 * @returns {{yontem:'pearson'|'spearman'|'kendall', adimlar:object[], normallik:object[]}}
 */
export function korelasyonYontemiSec(x, y, olcumX, olcumY, adX, adY, ayarlar, b) {
  const adimlar = [];
  const zorla = ayarlar.korelasyonYontemi ?? 'otomatik';
  if (zorla !== 'otomatik') {
    adimlar.push(adim('Yöntem', `Ayarlarda ${zorla === 'pearson' ? 'Pearson' : zorla === 'spearman' ? 'Spearman' : 'Kendall tau-b'} korelasyonu seçili.`));
    return { yontem: zorla, adimlar, normallik: [] };
  }
  if (olcumX === 'sirali' || olcumY === 'sirali') {
    adimlar.push(adim('Yöntem', 'Değişkenlerden en az biri sıralı → Spearman sıra korelasyonu.', 'tamam'));
    return { yontem: 'spearman', adimlar, normallik: [] };
  }
  const nx = normallikKarari(x, ayarlar, b);
  const ny = normallikKarari(y, ayarlar, b);
  const ikisi = nx.normal === true && ny.normal === true;
  adimlar.push(adim('Normallik', `${adX}: ${nx.ozet} → ${normallikIfadesi(nx)}. ${adY}: ${ny.ozet} → ${normallikIfadesi(ny)}.`, ikisi ? 'tamam' : 'uyari'));
  adimlar.push(adim('Yöntem', ikisi ? 'İki değişken de normal dağılıyor → Pearson korelasyonu.' : 'En az bir değişken normal dağılmıyor → Spearman sıra korelasyonu.', 'tamam'));
  return { yontem: ikisi ? 'pearson' : 'spearman', adimlar, normallik: [nx, ny] };
}

/**
 * Sürekli değişkenin tabloda nasıl gösterileceği.
 * @returns {'ortss'|'medyan-ceyrek'|'medyan-minmaks'|'ikisi'}
 */
export function gosterimSec(normalMi, ayarlar) {
  const g = ayarlar.surekliGosterim ?? 'otomatik';
  if (g !== 'otomatik') return g;
  return normalMi ? 'ortss' : 'medyan-ceyrek';
}
