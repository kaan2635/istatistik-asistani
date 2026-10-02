// Etki büyüklükleri: merkezi olmayan dağılımların tersine çevrilmesiyle güven aralıkları, Hedges düzeltmesi,
// dönüşümler ve Cohen (1988) ölçütlerine göre Türkçe yorum etiketleri.
//
// Güven aralıkları "pivot" yöntemiyle bulunur (Steiger & Fouladi 1997; MBESS ci.smd / ci.pvaf ile aynı yaklaşım):
// gözlenen t (ya da F) istatistiğini sırasıyla %α/2 ve %(1 − α/2) persentili yapan merkezi olmama parametreleri
// aranır, sonra etki büyüklüğü ölçeğine çevrilir.

import { pf, pnf, pnt, qnorm } from './dagilim.js';
import { brent, lgammaFark } from './sayisal.js';
import { IstatistikHatasi } from './hata.js';

function duzeyDenetle(duzey) {
  if (!(duzey > 0 && duzey < 1)) throw new IstatistikHatasi('Güven düzeyi 0 ile 1 arasında olmalıdır.');
}

/** Azalan bir h fonksiyonu için h(x) = hedef kökünü bulur; aralık başlangıç noktasından genişletilerek kurulur. */
function azalanKok(h, hedef, baslangic, alt = -Infinity) {
  const f = (x) => h(x) - hedef;
  let a = baslangic;
  let b = baslangic;
  let fa = f(a);
  if (fa === 0) return a;
  let adim = Math.max(1, Math.abs(baslangic) * 0.25);
  if (fa > 0) {
    // kök sağda
    let fb = fa;
    for (let i = 0; i < 200 && fb > 0; i++) {
      a = b;
      fa = fb;
      b += adim;
      fb = f(b);
      adim *= 2;
    }
    if (fb > 0) return NaN;
  } else {
    let fb = fa;
    b = a;
    for (let i = 0; i < 200 && fb < 0; i++) {
      b = a;
      a = Math.max(a - adim, alt);
      fb = f(a);
      adim *= 2;
      if (a === alt) break;
    }
    if (fb < 0) return alt;
  }
  return brent(f, a, b, 1e-13 * Math.max(1, Math.abs(a), Math.abs(b)));
}

/**
 * Merkezi olmayan t dağılımının ν (serbestlik) ve gözlenen t için merkezi olmama parametresinin (δ) güven aralığı.
 * Alt sınır: P(T ≤ t; δ_alt) = 1 − α/2, üst sınır: P(T ≤ t; δ_üst) = α/2 (MBESS conf.limits.nct ile aynı tanım).
 * @returns {{alt:number, ust:number}}
 */
export function ncpTGA(t, df, duzey = 0.95) {
  duzeyDenetle(duzey);
  if (!Number.isFinite(t) || !(df > 0)) return { alt: NaN, ust: NaN };
  const a = (1 - duzey) / 2;
  const yayilim = qnorm(1 - a) * Math.sqrt(1 + (t * t) / (2 * df));
  // pnt(t; δ) δ'ya göre azalır
  const alt = azalanKok((d) => pnt(t, df, d), 1 - a, t - yayilim);
  const ust = azalanKok((d) => pnt(t, df, d), a, t + yayilim);
  return { alt, ust };
}

/**
 * Merkezi olmayan F dağılımında gözlenen F için merkezi olmama parametresinin (λ) güven aralığı.
 * λ ≥ 0 ile sınırlıdır: P(F ≤ f; 0) gerekli olasılığın altındaysa sınır 0 olur.
 * @returns {{alt:number, ust:number}}
 */
export function ncpFGA(F, df1, df2, duzey = 0.95) {
  duzeyDenetle(duzey);
  if (!(F >= 0) || !Number.isFinite(F)) return { alt: NaN, ust: NaN };
  const a = (1 - duzey) / 2;
  const p0 = pf(F, df1, df2);
  const h = (l) => pnf(F, df1, df2, l);
  const bas = Math.max(1, F * df1);
  const alt = p0 <= 1 - a ? 0 : azalanKok(h, 1 - a, Math.max(0, F * df1 - df1), 0);
  const ust = p0 <= a ? 0 : azalanKok(h, a, bas, 0);
  return { alt, ust };
}

/**
 * Hedges düzeltme çarpanı J(ν) = Γ(ν/2) / (√(ν/2) Γ((ν−1)/2)) (kesin gama biçimi; Hedges 1981).
 * Büyük ν'de 1 − 3/(4ν − 1) yaklaşımına yakınsar.
 */
export function hedgesJ(df) {
  if (!(df > 1)) return NaN;
  // log Γ(ν/2) − log Γ((ν−1)/2) iptal hatası olmadan
  return Math.exp(lgammaFark((df - 1) / 2, 0.5) - 0.5 * Math.log(df / 2));
}

/**
 * Bağımsız iki grup için Cohen d'nin güven aralığı (MBESS ci.smd). δ = d √(n1 n2 / (n1 + n2)), ν = n1 + n2 − 2.
 * @returns {{alt:number, ust:number}}
 */
export function cohenDGA(d, n1, n2, duzey = 0.95) {
  const c = Math.sqrt(1 / n1 + 1 / n2);
  const g = ncpTGA(d / c, n1 + n2 - 2, duzey);
  return { alt: g.alt * c, ust: g.ust * c };
}

/**
 * Tek örneklem / eşleştirilmiş (d_z) Cohen d'nin güven aralığı. δ = d √n, ν = n − 1.
 * @returns {{alt:number, ust:number}}
 */
export function tekOrneklemDGA(d, n, duzey = 0.95) {
  const k = Math.sqrt(n);
  const g = ncpTGA(d * k, n - 1, duzey);
  return { alt: g.alt / k, ust: g.ust / k };
}

/**
 * (Kısmi) eta karenin güven aralığı: η² = λ / (λ + sd1 + sd2 + 1) (Steiger 2004; tek yönlü ANOVA'da paydadaki
 * sd1 + sd2 + 1 = N). MBESS ci.pvaf ve effectsize::F_to_eta2 ile aynı dönüşüm.
 * @returns {{alt:number, ust:number}}
 */
export function etaKareGA(F, df1, df2, duzey = 0.95) {
  const l = ncpFGA(F, df1, df2, duzey);
  const N = df1 + df2 + 1;
  return { alt: l.alt / (l.alt + N), ust: l.ust / (l.ust + N) };
}

// ---------- Dönüşümler ----------

/** Bağımsız t → Cohen d: d = t √(1/n1 + 1/n2) */
export function tdenD(t, n1, n2) {
  return t * Math.sqrt(1 / n1 + 1 / n2);
}

/** Tek örneklem / eşleştirilmiş t → d (d_z): d = t / √n */
export function tdenDz(t, n) {
  return t / Math.sqrt(n);
}

/** t → r: r = t / √(t² + ν) */
export function tdenR(t, df) {
  return t / Math.sqrt(t * t + df);
}

/** F → kısmi eta kare: η²p = F·sd1 / (F·sd1 + sd2) */
export function fdenEtaKare(F, df1, df2) {
  return (F * df1) / (F * df1 + df2);
}

/** F → kısmi omega kare (tek yönlü ANOVA'da ω²): (F − 1) sd1 / ((F − 1) sd1 + N), N = sd1 + sd2 + 1 */
export function fdenOmegaKare(F, df1, df2) {
  const N = df1 + df2 + 1;
  return ((F - 1) * df1) / ((F - 1) * df1 + N);
}

/** χ² → Cramér V: V = √(χ² / (n · min(r − 1, c − 1))) */
export function kiKaredenCramerV(kiKare, n, satir, sutun) {
  const m = Math.min(satir - 1, sutun - 1);
  if (!(m >= 1) || !(n > 0)) return NaN;
  return Math.sqrt(kiKare / (n * m));
}

/** z → r: r = z / √n (Rosenthal 1991; parametrik olmayan testlerde etki büyüklüğü) */
export function zdenR(z, n) {
  return z / Math.sqrt(n);
}

/** d → r (eşit grup büyüklüğü varsayımıyla): r = d / √(d² + 4) */
export function ddenR(d) {
  return d / Math.sqrt(d * d + 4);
}

// ---------- Yorumlar (Cohen 1988) ----------
// Eşikler mutlak değere uygulanır; eşik değeri üst sınıfa dahildir (ör. |d| = 0.5 → "orta").
//   d (ve Hedges g, Glass Δ):  < 0.2 önemsiz, < 0.5 küçük, < 0.8 orta, ≥ 0.8 büyük
//   r (ve ρ, τ, φ):            < 0.1 önemsiz, < 0.3 küçük, < 0.5 orta, ≥ 0.5 büyük
//   η², ω², ε² (ve kısmi):     < 0.01 önemsiz, < 0.06 küçük, < 0.14 orta, ≥ 0.14 büyük
//   Cramér V (sd* = min(r−1, c−1)): Cohen w eşikleri 0.1 / 0.3 / 0.5, √sd* ile bölünerek (w = V √sd*)

function siniflandir(deger, esikler) {
  if (deger === null || deger === undefined || Number.isNaN(deger)) return null;
  const v = Math.abs(deger);
  if (v < esikler[0]) return 'önemsiz';
  if (v < esikler[1]) return 'küçük';
  if (v < esikler[2]) return 'orta';
  return 'büyük';
}

/** Cohen d / Hedges g yorumu: 0.2 küçük, 0.5 orta, 0.8 büyük */
export function yorumD(d) {
  return siniflandir(d, [0.2, 0.5, 0.8]);
}

/** Korelasyon katsayısı yorumu: 0.1 küçük, 0.3 orta, 0.5 büyük */
export function yorumR(r) {
  return siniflandir(r, [0.1, 0.3, 0.5]);
}

/** η² / ω² / ε² yorumu: 0.01 küçük, 0.06 orta, 0.14 büyük */
export function yorumEtaKare(e) {
  return siniflandir(e, [0.01, 0.06, 0.14]);
}

/**
 * Cramér V yorumu. sd = min(satır − 1, sütun − 1). Cohen'in w ölçütleri (0.1, 0.3, 0.5) V = w / √sd ile çevrilir;
 * sd = 1 (2×k tablolar) için eşikler 0.1 / 0.3 / 0.5'tir.
 */
export function yorumCramerV(v, df = 1) {
  const k = Math.sqrt(Math.max(1, df));
  return siniflandir(v, [0.1 / k, 0.3 / k, 0.5 / k]);
}

/** Cohen f yorumu (ANOVA): 0.1 küçük, 0.25 orta, 0.4 büyük */
export function yorumF(f) {
  return siniflandir(f, [0.1, 0.25, 0.4]);
}

/** Etki nesnesi kurucu (sonuç nesnelerinde ortak biçim) */
export function etkiNesnesi(ad, sembol, deger, ga = null, yorum = null) {
  const e = { ad, sembol, deger };
  if (ga) {
    e.gaAlt = ga.alt;
    e.gaUst = ga.ust;
  }
  if (yorum !== null) e.yorum = yorum;
  return e;
}

