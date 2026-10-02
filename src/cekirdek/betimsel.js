// Betimsel istatistikler ve sıralama yardımcıları.
// Varsayılanlar SPSS ile uyumludur: varyans n-1 paydalı, çarpıklık/basıklık G1/G2 (standart hatalarıyla),
// yüzdelikler "ağırlıklı ortalama" (R type = 6). type = 7 (R/Excel varsayılanı) seçilebilir.

import { qt, qnorm, pbinom } from './dagilim.js';
import { IstatistikHatasi } from './hata.js';

/** Sonlu sayıları ayıklar (null, undefined, NaN, ±Infinity atılır) */
export function temizSayilar(dizi) {
  const sonuc = [];
  for (const v of dizi) if (typeof v === 'number' && Number.isFinite(v)) sonuc.push(v);
  return sonuc;
}

export function toplam(x) {
  // Kahan-Babuska toplama: büyük veri setlerinde yuvarlama hatasını azaltır
  let s = 0;
  let c = 0;
  for (const v of x) {
    const t = s + v;
    if (Math.abs(s) >= Math.abs(v)) c += s - t + v;
    else c += v - t + s;
    s = t;
  }
  return s + c;
}

export function ortalama(x) {
  if (x.length === 0) return NaN;
  const m = toplam(x) / x.length;
  // ikinci geçiş düzeltmesi
  let d = 0;
  for (const v of x) d += v - m;
  return m + d / x.length;
}

/** Örneklem varyansı (n-1) */
export function varyans(x, ort = ortalama(x)) {
  const n = x.length;
  if (n < 2) return NaN;
  let s = 0;
  let c = 0;
  for (const v of x) {
    const d = v - ort;
    s += d * d;
    c += d;
  }
  return (s - (c * c) / n) / (n - 1);
}

export function standartSapma(x, ort) {
  return Math.sqrt(varyans(x, ort));
}

export function standartHata(x) {
  return standartSapma(x) / Math.sqrt(x.length);
}

export function sirali(x) {
  return Float64Array.from(x).sort();
}

export function minimum(x) {
  let m = Infinity;
  for (const v of x) if (v < m) m = v;
  return x.length ? m : NaN;
}

export function maksimum(x) {
  let m = -Infinity;
  for (const v of x) if (v > m) m = v;
  return x.length ? m : NaN;
}

/**
 * Yüzdelik (0 <= p <= 1). tip: 6 (SPSS "weighted average", varsayılan) ya da 7 (R/Excel), 2 (ters ECDF ortalaması).
 * siraliMi: x zaten sıralıysa true.
 */
export function yuzdelik(x, p, tip = 6, siraliMi = false) {
  const s = siraliMi ? x : sirali(x);
  const n = s.length;
  if (n === 0) return NaN;
  if (n === 1) return s[0];
  if (tip === 7) {
    const h = (n - 1) * p;
    const alt = Math.floor(h);
    const ust = Math.min(alt + 1, n - 1);
    return s[alt] + (h - alt) * (s[ust] - s[alt]);
  }
  if (tip === 2) {
    const np = n * p;
    const j = Math.floor(np + 1e-12);
    if (Math.abs(np - j) < 1e-12) return j <= 0 ? s[0] : j >= n ? s[n - 1] : (s[j - 1] + s[j]) / 2;
    return s[Math.min(n - 1, Math.max(0, Math.ceil(np) - 1))];
  }
  // tip 6: h = (n + 1) p
  const h = (n + 1) * p;
  if (h <= 1) return s[0];
  if (h >= n) return s[n - 1];
  const alt = Math.floor(h);
  return s[alt - 1] + (h - alt) * (s[alt] - s[alt - 1]);
}

export function medyan(x) {
  const s = sirali(x);
  const n = s.length;
  if (n === 0) return NaN;
  const m = n >> 1;
  return n % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
}

/** Çarpıklık (G1, SPSS) ve standart hatası */
export function carpiklik(x) {
  const n = x.length;
  if (n < 3) return { deger: NaN, sh: NaN };
  const m = ortalama(x);
  const s = standartSapma(x, m);
  if (s === 0) return { deger: NaN, sh: NaN };
  let t = 0;
  for (const v of x) t += ((v - m) / s) ** 3;
  const deger = (n / ((n - 1) * (n - 2))) * t;
  const sh = Math.sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)));
  return { deger, sh };
}

/** Basıklık (G2, fazlalık basıklık, SPSS) ve standart hatası */
export function basiklik(x) {
  const n = x.length;
  if (n < 4) return { deger: NaN, sh: NaN };
  const m = ortalama(x);
  const s = standartSapma(x, m);
  if (s === 0) return { deger: NaN, sh: NaN };
  let t = 0;
  for (const v of x) t += ((v - m) / s) ** 4;
  const deger = ((n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))) * t - (3 * (n - 1) ** 2) / ((n - 2) * (n - 3));
  const shCarpiklik = Math.sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)));
  const sh = 2 * shCarpiklik * Math.sqrt((n * n - 1) / ((n - 3) * (n + 5)));
  return { deger, sh };
}

/** Kırpılmış ortalama: her uçtan floor(n·oran) gözlem atılır (R: mean(x, trim = oran)) */
export function kirpilmisOrtalama(x, oran = 0.05) {
  const s = sirali(x);
  const n = s.length;
  if (n === 0) return NaN;
  const k = Math.floor(n * oran);
  if (n - 2 * k <= 0) return medyan(x);
  return ortalama(Array.from(s.subarray(k, n - k)));
}

export function mod(x) {
  const say = new Map();
  for (const v of x) say.set(v, (say.get(v) || 0) + 1);
  let enCok = 0;
  for (const c of say.values()) enCok = Math.max(enCok, c);
  const modlar = [...say.entries()].filter(([, c]) => c === enCok).map(([v]) => v).sort((a, b) => a - b);
  return { deger: modlar[0], coklu: modlar.length > 1, frekans: enCok };
}

export function geometrikOrtalama(x) {
  if (x.some((v) => v <= 0)) return NaN;
  return Math.exp(ortalama(x.map(Math.log)));
}

/** Ortalamanın t tabanlı güven aralığı */
export function ortalamaGA(x, duzey = 0.95) {
  const n = x.length;
  const m = ortalama(x);
  if (n < 2) return { alt: NaN, ust: NaN };
  const se = standartHata(x);
  const t = qt(1 - (1 - duzey) / 2, n - 1);
  return { alt: m - t * se, ust: m + t * se };
}

/**
 * Medyanın dağılımdan bağımsız güven aralığı (binom sıra istatistikleri; Conover).
 * Kesin düzey, binom ayrıklığı nedeniyle istenenden biraz yüksektir; gerçekleşen düzey de döner.
 */
export function medyanGA(x, duzey = 0.95) {
  const s = sirali(x);
  const n = s.length;
  if (n < 2) return { alt: NaN, ust: NaN, gercekDuzey: NaN };
  const a = (1 - duzey) / 2;
  // P(X <= k-1) <= a olan en büyük k (Binom(n, 1/2))
  let k = 0;
  while (k + 1 <= n && pbinom(k, n, 0.5) <= a) k++;
  if (k === 0) return { alt: s[0], ust: s[n - 1], gercekDuzey: 1 - 2 * 0.5 ** n };
  const gercekDuzey = 1 - 2 * pbinom(k - 1, n, 0.5);
  return { alt: s[k - 1], ust: s[n - k], gercekDuzey };
}

/**
 * Tam betimsel özet.
 * @param {number[]} x eksik değerler ayıklanmış sayılar
 * @param {{yuzdelikTipi?:number, duzey?:number}} secenek
 */
export function betimsel(x, secenek = {}) {
  const tip = secenek.yuzdelikTipi ?? 6;
  const duzey = secenek.duzey ?? 0.95;
  const n = x.length;
  if (n === 0) throw new IstatistikHatasi('Geçerli gözlem yok.');
  const s = sirali(x);
  const ort = ortalama(x);
  const ss = n > 1 ? standartSapma(x, ort) : NaN;
  const ga = ortalamaGA(x, duzey);
  const c = carpiklik(x);
  const b = basiklik(x);
  const q1 = yuzdelik(s, 0.25, tip, true);
  const q3 = yuzdelik(s, 0.75, tip, true);
  return {
    n,
    ortalama: ort,
    standartSapma: ss,
    standartHata: ss / Math.sqrt(n),
    varyans: ss * ss,
    gaAlt: ga.alt,
    gaUst: ga.ust,
    medyan: yuzdelik(s, 0.5, tip, true),
    q1,
    q3,
    iqr: q3 - q1,
    minimum: s[0],
    maksimum: s[n - 1],
    ranj: s[n - 1] - s[0],
    carpiklik: c.deger,
    carpiklikSH: c.sh,
    basiklik: b.deger,
    basiklikSH: b.sh,
    kirpilmisOrtalama: kirpilmisOrtalama(x),
    degiskenlikKatsayisi: ort !== 0 ? (ss / Math.abs(ort)) * 100 : NaN,
    mod: mod(x).deger,
    yuzdelik5: yuzdelik(s, 0.05, tip, true),
    yuzdelik95: yuzdelik(s, 0.95, tip, true),
  };
}

// ---------- Sıralar ----------

/**
 * Ortalama sıralar (bağlarda sıra ortalaması, R: rank(ties = "average")).
 * @returns {{sira:Float64Array, baglar:number[]}} baglar: her bağ grubunun büyüklüğü (>1 olanlar)
 */
export function siralar(x) {
  const n = x.length;
  const idx = Array.from({ length: n }, (_, i) => i).sort((a, b) => x[a] - x[b]);
  const sira = new Float64Array(n);
  const baglar = [];
  let i = 0;
  while (i < n) {
    let j = i;
    while (j + 1 < n && x[idx[j + 1]] === x[idx[i]]) j++;
    const ort = (i + j + 2) / 2;
    for (let k = i; k <= j; k++) sira[idx[k]] = ort;
    if (j > i) baglar.push(j - i + 1);
    i = j + 1;
  }
  return { sira, baglar };
}

/** Σ (t³ - t) — sıralı testlerde bağ düzeltmesi için */
export function bagToplami(baglar) {
  let s = 0;
  for (const t of baglar) s += t * t * t - t;
  return s;
}

/** Frekans tablosu: değer → sayı (giriş sırasıyla) */
export function frekanslar(x) {
  const m = new Map();
  for (const v of x) m.set(v, (m.get(v) || 0) + 1);
  return m;
}

export { qnorm };
