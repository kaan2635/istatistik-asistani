// Analiz düzenleyicilerinin ortak yardımcıları: raporlama satırı, hücre biçimleri, grafik seçenekleri.

import { betimsel } from '../../cekirdek/betimsel.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';

/** Test kimliğinden kısa Türkçe ad (tablo dipnotları için) */
export const TEST_ADLARI = {
  'student-t': 'Bağımsız örneklem t testi',
  'welch-t': 'Welch t testi',
  'mann-whitney-u': 'Mann-Whitney U testi',
  anova: 'Tek yönlü varyans analizi (ANOVA)',
  'tek-yonlu-anova': 'Tek yönlü varyans analizi (ANOVA)',
  'welch-anova': 'Welch ANOVA',
  'kruskal-wallis': 'Kruskal-Wallis H testi',
  pearson: 'Pearson ki-kare testi',
  'pearson-ki-kare': 'Pearson ki-kare testi',
  yates: 'Yates düzeltmeli ki-kare testi',
  fisher: "Fisher'in kesin testi",
  'fisher-kesin': "Fisher'in kesin testi",
  'fisher-freeman-halton': 'Fisher-Freeman-Halton kesin testi',
  'eslestirilmis-t': 'Eşleştirilmiş örneklem t testi',
  wilcoxon: 'Wilcoxon işaretli sıralar testi',
  'wilcoxon-isaretli-sira': 'Wilcoxon işaretli sıralar testi',
  'tekrarli-anova': 'Tekrarlı ölçümler varyans analizi',
  friedman: 'Friedman testi',
  mcnemar: 'McNemar testi',
  'mcnemar-bowker': 'McNemar-Bowker testi',
  'cochran-q': "Cochran'ın Q testi",
  'tek-orneklem-t': 'Tek örneklem t testi',
};

/** Etki büyüklüğü: "d = 0,45 (95% GA: 0,12; 0,78)" */
export function etkiMetni(etki, b, gaGoster = true) {
  if (!etki || etki.deger === null || etki.deger === undefined || Number.isNaN(etki.deger)) return '';
  let m = `${etki.sembol} = ${F.sayi(etki.deger, 2, b)}`;
  if (gaGoster && Number.isFinite(etki.gaAlt) && Number.isFinite(etki.gaUst)) m += ` (%95 GA: ${F.sayi(etki.gaAlt, 2, b)}; ${F.sayi(etki.gaUst, 2, b)})`;
  return m;
}

/** Bir test sonucunun standart raporlama satırı: "t(18) = -5,73; p < 0,001; d = -2,56" */
export function testSatiri(s, b) {
  if (!s) return '';
  const parcalar = [];
  const p = F.pEsitlik(s.p, b);
  switch (s.yontem) {
    case 'mann-whitney-u':
      parcalar.push(`U = ${F.sayi(s.U, 1, b)}`, `Z = ${F.sayi(s.z, 2, b)}`, p);
      break;
    case 'wilcoxon-isaretli-sira':
    case 'wilcoxon-tek-orneklem':
      parcalar.push(`Z = ${F.sayi(s.z, 2, b)}`, p);
      break;
    case 'fisher-kesin':
    case 'fisher-freeman-halton':
      parcalar.push(`Fisher kesin testi ${p}`);
      break;
    case 'mcnemar':
      parcalar.push(s.pYontemi === 'kesin' ? `McNemar (kesin binom) ${p}` : `χ²(1) = ${F.sayi(s.kiKare.istatistik, 2, b)}`, ...(s.pYontemi === 'kesin' ? [] : [p]));
      break;
    default:
      if (s.istatistikAdi && Number.isFinite(s.istatistik)) parcalar.push(F.istatistik(s.istatistikAdi, s.istatistik, s.sd, b));
      parcalar.push(p);
  }
  const e = etkiMetni(s.etki, b, false);
  if (e) parcalar.push(e);
  return parcalar.join('; ');
}

/** Tablodaki istatistik hücresi: "t = -5,73" ya da "U = 3,0" ya da "χ² = 8,04" */
export function istatistikHucresi(s, b) {
  if (!s) return '–';
  switch (s.yontem) {
    case 'mann-whitney-u':
      return `U = ${F.sayi(s.U, 1, b)}; Z = ${F.sayi(s.z, 2, b)}`;
    case 'wilcoxon-isaretli-sira':
    case 'wilcoxon-tek-orneklem':
      return `Z = ${F.sayi(s.z, 2, b)}`;
    case 'fisher-kesin':
    case 'fisher-freeman-halton':
      return '–';
    default:
      if (s.istatistikAdi && Number.isFinite(s.istatistik)) return F.istatistik(s.istatistikAdi, s.istatistik, s.sd, b);
      return '–';
  }
}

/** p hücresi: anlamlıysa kalın */
export function pHucresi(p, alfa, b, ust) {
  return hucre(F.p(p, b), { anlamli: Number.isFinite(p) && p < alfa, ust });
}

/** Sürekli değişken özeti hücre metni */
export function surekliMetin(x, gosterim, b, tip = 6) {
  if (!x.length) return '–';
  const d = betimsel(x, { yuzdelikTipi: tip });
  const ortss = x.length > 1 ? F.ortSS(d.ortalama, d.standartSapma, undefined, b) : F.sayi(d.ortalama, undefined, b);
  switch (gosterim) {
    case 'ortss':
      return ortss;
    case 'medyan-minmaks':
      return F.medyanAralik(d.medyan, d.minimum, d.maksimum, undefined, b);
    case 'ikisi':
      return `${ortss}\n${F.medyanAralik(d.medyan, d.q1, d.q3, undefined, b)}`;
    default:
      return F.medyanAralik(d.medyan, d.q1, d.q3, undefined, b);
  }
}

export const GOSTERIM_ETIKETI = {
  ortss: 'ort ± SS',
  'medyan-ceyrek': 'medyan (Ç1–Ç3)',
  'medyan-minmaks': 'medyan (min–maks)',
  ikisi: 'ort ± SS / medyan (Ç1–Ç3)',
};

/** Grafik fonksiyonlarına geçirilecek ortak seçenekler */
export function grafikSecenek(baglam, ek = {}) {
  return {
    tema: baglam.ayarlar.grafikTema,
    yaziTipi: baglam.ayarlar.grafikYaziTipi,
    ondalikAyrac: baglam.ayarlar.ondalikAyrac,
    ...ek,
  };
}

/** Bir hesaplamayı güvenle çalıştırır; hata olursa null ve uyarı listesine mesaj ekler */
export function dene(fn, uyarilar, onEk = '') {
  try {
    return fn();
  } catch (e) {
    if (uyarilar) uyarilar.push(`${onEk}${e.message}`);
    return null;
  }
}

/** Dipnot harfleri a, b, c … */
export function harfAtayici() {
  const harita = new Map();
  return {
    al(anahtar) {
      if (!harita.has(anahtar)) harita.set(anahtar, String.fromCharCode(97 + harita.size));
      return harita.get(anahtar);
    },
    dipnotlar(adlar) {
      return [...harita.entries()].map(([k, h]) => `${h}: ${adlar[k] ?? k}`);
    },
  };
}

/** Uyarı listesinden not blokları */
export function uyariBloklari(uyarilar) {
  return [...new Set(uyarilar.filter(Boolean))].map((metin) => ({ tur: 'not', metin, seviye: 'uyari' }));
}
