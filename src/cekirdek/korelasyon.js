// Korelasyon analizleri: Pearson, Spearman, Kendall tau-b, kısmi korelasyon, korelasyon matrisi ve
// iki bağımsız korelasyonun karşılaştırılması.
//
// Alternatif hipotez: 'iki' (varsayılan), 'buyuk' (pozitif ilişki), 'kucuk' (negatif ilişki).
// Güven aralıkları Fisher z dönüşümüyle ve her zaman iki yönlü verilir (R cor.test tek yönlü alternatifte
// tek yönlü aralık verir; SPSS iki yönlü aralık verir).

import { pnorm, pt, qnorm } from './dagilim.js';
import { ortalama, siralar } from './betimsel.js';
import { enKucukKareler } from './matris.js';
import { etkiNesnesi, yorumR } from './etki.js';
import { IstatistikHatasi } from './hata.js';

const ALTERNATIFLER = ['iki', 'buyuk', 'kucuk'];

function secenekler(secenek) {
  const alternatif = secenek.alternatif ?? 'iki';
  const duzey = secenek.duzey ?? 0.95;
  if (!ALTERNATIFLER.includes(alternatif)) throw new IstatistikHatasi(`Bilinmeyen alternatif hipotez: ${alternatif}`);
  if (!(duzey > 0 && duzey < 1)) throw new IstatistikHatasi('Güven düzeyi 0 ile 1 arasında olmalıdır.');
  return { alternatif, duzey };
}

function ciftDenetle(x, y, testAdi, enAz) {
  if (!x || !y || x.length !== y.length) throw new IstatistikHatasi(`${testAdi} için iki değişkenin gözlem sayısı eşit olmalıdır.`);
  for (let i = 0; i < x.length; i++) {
    if (typeof x[i] !== 'number' || !Number.isFinite(x[i]) || typeof y[i] !== 'number' || !Number.isFinite(y[i])) {
      throw new IstatistikHatasi(`${testAdi}: veride eksik ya da sayısal olmayan değer var.`);
    }
  }
  if (x.length < enAz) throw new IstatistikHatasi(`${testAdi} için en az ${enAz} gözlem çifti gerekir.`);
}

/** İki dizinin Pearson korelasyonu (ortalamadan sapmalarla, iki geçişli). Sabit değişkende NaN. */
function rHesapla(x, y) {
  const mx = ortalama(x);
  const my = ortalama(y);
  let sxy = 0;
  let sxx = 0;
  let syy = 0;
  for (let i = 0; i < x.length; i++) {
    const a = x[i] - mx;
    const b = y[i] - my;
    sxy += a * b;
    sxx += a * a;
    syy += b * b;
  }
  if (!(sxx > 0) || !(syy > 0)) return NaN;
  return Math.max(-1, Math.min(1, sxy / Math.sqrt(sxx) / Math.sqrt(syy)));
}

function tP(t, df, alternatif) {
  if (alternatif === 'iki') return Math.min(1, 2 * pt(-Math.abs(t), df));
  if (alternatif === 'buyuk') return pt(t, df, false);
  return pt(t, df);
}

function zP(z, alternatif) {
  if (alternatif === 'iki') return Math.min(1, 2 * pnorm(-Math.abs(z)));
  if (alternatif === 'buyuk') return pnorm(z, 0, 1, false);
  return pnorm(z);
}

/** r için t = r √(ν / (1 − r²)); |r| = 1 iken ±∞ */
function rdenT(r, df) {
  if (Math.abs(r) === 1) return r * Infinity;
  return (r * Math.sqrt(df)) / Math.sqrt((1 - r) * (1 + r));
}

/** Fisher z güven aralığı: tanh(atanh(r) ± z_{1−α/2} · sh) */
function fisherGA(r, sh, duzey) {
  if (!(sh > 0) || !Number.isFinite(sh)) return { alt: NaN, ust: NaN };
  if (Math.abs(r) === 1) return { alt: r, ust: r };
  const z = Math.atanh(r);
  const q = qnorm(1 - (1 - duzey) / 2);
  return { alt: Math.tanh(z - q * sh), ust: Math.tanh(z + q * sh) };
}

function sabitHatasi(testAdi) {
  return new IstatistikHatasi(`${testAdi}: değişkenlerden biri sabit (varyans sıfır); korelasyon hesaplanamaz.`);
}

/**
 * Pearson korelasyon katsayısı (SPSS CORRELATIONS; R cor.test(method = "pearson")).
 * t = r √(n − 2)/√(1 − r²), ν = n − 2. GA: Fisher z, SH = 1/√(n − 3) (n ≥ 4).
 * @param {number[]} x
 * @param {number[]} y
 * @param {{alternatif?:'iki'|'buyuk'|'kucuk', duzey?:number}} [secenek]
 */
export function pearson(x, y, secenek = {}) {
  const { alternatif, duzey } = secenekler(secenek);
  ciftDenetle(x, y, 'Pearson korelasyonu', 3);
  const n = x.length;
  const r = rHesapla(x, y);
  if (Number.isNaN(r)) throw sabitHatasi('Pearson korelasyonu');
  const df = n - 2;
  const t = rdenT(r, df);
  const ga = n > 3 ? fisherGA(r, 1 / Math.sqrt(n - 3), duzey) : { alt: NaN, ust: NaN };
  return {
    yontem: 'pearson',
    ad: 'Pearson korelasyonu',
    istatistik: t,
    istatistikAdi: 't',
    sd: df,
    p: tP(t, df, alternatif),
    alternatif,
    r,
    rKare: r * r,
    etki: etkiNesnesi('Pearson r', 'r', r, ga, yorumR(r)),
    ga: { duzey, alt: ga.alt, ust: ga.ust, neyin: 'r' },
    n,
    uyarilar: n > 3 ? [] : ['Güven aralığı için en az 4 gözlem gerekir.'],
  };
}

/**
 * Spearman sıra korelasyonu (bağlarda ortalama sıra). p, t yaklaşımıyla: t = ρ √((n − 2)/(1 − ρ²)), ν = n − 2
 * (SPSS NONPAR CORR; R cor.test(method = "spearman", exact = FALSE)).
 * GA: Fisher z, Bonett & Wright (2000) SH = √((1 + ρ²/2)/(n − 3)).
 * @param {number[]} x
 * @param {number[]} y
 * @param {{alternatif?:'iki'|'buyuk'|'kucuk', duzey?:number}} [secenek]
 */
export function spearman(x, y, secenek = {}) {
  const { alternatif, duzey } = secenekler(secenek);
  ciftDenetle(x, y, 'Spearman korelasyonu', 3);
  const n = x.length;
  const rho = rHesapla(Array.from(siralar(x).sira), Array.from(siralar(y).sira));
  if (Number.isNaN(rho)) throw sabitHatasi('Spearman korelasyonu');
  const df = n - 2;
  const t = rdenT(rho, df);
  const ga = n > 3 ? fisherGA(rho, Math.sqrt((1 + (rho * rho) / 2) / (n - 3)), duzey) : { alt: NaN, ust: NaN };
  return {
    yontem: 'spearman',
    ad: 'Spearman sıra korelasyonu',
    istatistik: t,
    istatistikAdi: 't',
    sd: df,
    p: tP(t, df, alternatif),
    pYontemi: 'asimptotik',
    alternatif,
    rho,
    S: ((n * n * n - n) * (1 - rho)) / 6,
    etki: etkiNesnesi('Spearman ρ', 'ρ', rho, ga, yorumR(rho)),
    ga: { duzey, alt: ga.alt, ust: ga.ust, neyin: 'ρ' },
    n,
    uyarilar: n > 3 ? [] : ['Güven aralığı için en az 4 gözlem gerekir.'],
  };
}

/** Bağ grubu büyüklükleri (sıralı dizide ardışık eşitler) */
function bagGruplari(sirali) {
  const t = [];
  let i = 0;
  while (i < sirali.length) {
    let j = i;
    while (j + 1 < sirali.length && sirali[j + 1] === sirali[i]) j++;
    if (j > i) t.push(j - i + 1);
    i = j + 1;
  }
  return t;
}

/** Birleştirme sıralamasıyla ters çevrim (discordant çift) sayısı; dizi yerinde sıralanır */
function tersCevrimSay(a) {
  const n = a.length;
  let tampon = new Float64Array(n);
  let kaynak = a;
  let say = 0;
  for (let genislik = 1; genislik < n; genislik *= 2) {
    for (let bas = 0; bas < n; bas += 2 * genislik) {
      const orta = Math.min(bas + genislik, n);
      const son = Math.min(bas + 2 * genislik, n);
      let i = bas;
      let j = orta;
      let k = bas;
      while (i < orta && j < son) {
        if (kaynak[j] < kaynak[i]) {
          tampon[k++] = kaynak[j++];
          say += orta - i;
        } else tampon[k++] = kaynak[i++];
      }
      while (i < orta) tampon[k++] = kaynak[i++];
      while (j < son) tampon[k++] = kaynak[j++];
    }
    [kaynak, tampon] = [tampon, kaynak];
  }
  if (kaynak !== a) a.set(kaynak);
  return say;
}

/**
 * Kendall S = (uyumlu − uyumsuz çift sayısı) ve bağ bilgileri; Knight (1966) O(n log n) algoritması.
 * @returns {{S:number, n0:number, n1:number, n2:number, xBag:number[], yBag:number[]}}
 */
export function kendallS(x, y) {
  const n = x.length;
  const idx = Array.from({ length: n }, (_, i) => i).sort((a, b) => x[a] - x[b] || y[a] - y[b]);
  const xs = idx.map((i) => x[i]);
  const ys = Float64Array.from(idx, (i) => y[i]);
  const xBag = bagGruplari(xs);
  // x ve y'de birlikte bağlı çiftler
  let n3 = 0;
  let i = 0;
  while (i < n) {
    let j = i;
    while (j + 1 < n && xs[j + 1] === xs[i] && ys[j + 1] === ys[i]) j++;
    const t = j - i + 1;
    n3 += (t * (t - 1)) / 2;
    i = j + 1;
  }
  const uyumsuz = tersCevrimSay(ys); // ys artık sıralı
  const yBag = bagGruplari(ys);
  const n0 = (n * (n - 1)) / 2;
  const n1 = xBag.reduce((s, t) => s + (t * (t - 1)) / 2, 0);
  const n2 = yBag.reduce((s, t) => s + (t * (t - 1)) / 2, 0);
  const S = n0 - n1 - n2 + n3 - 2 * uyumsuz;
  return { S, n0, n1, n2, xBag, yBag };
}

/**
 * Kendall tau-b (bağ düzeltmeli). z = S / √Var(S), bağ düzeltmeli varyansla (Kendall 1970):
 * Var(S) = [n(n−1)(2n+5) − Σt(t−1)(2t+5) − Σu(u−1)(2u+5)]/18 + Σt(t−1)·Σu(u−1) / (2n(n−1))
 *          + Σt(t−1)(t−2)·Σu(u−1)(u−2) / (9n(n−1)(n−2)).
 * R cor.test(method = "kendall", exact = FALSE) ile aynıdır (süreklilik düzeltmesi yok).
 * @param {number[]} x
 * @param {number[]} y
 * @param {{alternatif?:'iki'|'buyuk'|'kucuk', duzey?:number}} [secenek]
 */
export function kendallTauB(x, y, secenek = {}) {
  const { alternatif, duzey } = secenekler(secenek);
  ciftDenetle(x, y, 'Kendall tau-b', 3);
  const n = x.length;
  const { S, n0, n1, n2, xBag, yBag } = kendallS(x, y);
  if (n0 - n1 === 0 || n0 - n2 === 0) throw sabitHatasi('Kendall tau-b');
  const tau = Math.max(-1, Math.min(1, S / Math.sqrt(n0 - n1) / Math.sqrt(n0 - n2)));
  const top = (b, f) => b.reduce((s, t) => s + f(t), 0);
  const v0 = n * (n - 1) * (2 * n + 5);
  const vt = top(xBag, (t) => t * (t - 1) * (2 * t + 5));
  const vu = top(yBag, (t) => t * (t - 1) * (2 * t + 5));
  const v1 = top(xBag, (t) => t * (t - 1)) * top(yBag, (t) => t * (t - 1));
  const v2 = top(xBag, (t) => t * (t - 1) * (t - 2)) * top(yBag, (t) => t * (t - 1) * (t - 2));
  const varS = (v0 - vt - vu) / 18 + v1 / (2 * n * (n - 1)) + v2 / (9 * n * (n - 1) * (n - 2));
  const z = S / Math.sqrt(varS);
  return {
    yontem: 'kendall-tau-b',
    ad: 'Kendall tau-b',
    istatistik: z,
    istatistikAdi: 'z',
    sd: null,
    p: zP(z, alternatif),
    pYontemi: 'asimptotik',
    alternatif,
    tau,
    S,
    varyansS: varS,
    etki: etkiNesnesi('Kendall τ-b', 'τ', tau, null, yorumR(tau)),
    duzey,
    n,
    uyarilar: [],
  };
}

/**
 * Kısmi korelasyon: x ve y'nin kontrol değişkenlerinden arındırılmış (doğrusal regresyon artıkları) Pearson
 * korelasyonu (SPSS PARTIAL CORR; ppcor::pcor.test). ν = n − 2 − q, t = r √(ν/(1 − r²)).
 * GA: Fisher z, SH = 1/√(n − 3 − q).
 * @param {number[]} x
 * @param {number[]} y
 * @param {number[][]} kontroller kontrol değişkenleri listesi (her biri x ile aynı uzunlukta)
 * @param {{alternatif?:'iki'|'buyuk'|'kucuk', duzey?:number}} [secenek]
 */
export function kismiKorelasyon(x, y, kontroller, secenek = {}) {
  const { alternatif, duzey } = secenekler(secenek);
  if (!Array.isArray(kontroller) || kontroller.length === 0) throw new IstatistikHatasi('Kısmi korelasyon için en az bir kontrol değişkeni gerekir.');
  ciftDenetle(x, y, 'Kısmi korelasyon', 3);
  const n = x.length;
  const q = kontroller.length;
  for (const z of kontroller) ciftDenetle(x, z, 'Kısmi korelasyon', 3);
  const df = n - 2 - q;
  if (df < 1) throw new IstatistikHatasi(`Kısmi korelasyon için en az ${q + 3} gözlem gerekir.`);
  const X = Array.from({ length: n }, (_, i) => [1, ...kontroller.map((z) => z[i])]);
  const ex = enKucukKareler(X, x);
  const ey = enKucukKareler(X, y);
  const uyarilar = [];
  if (ex.bagimli.length > 0) uyarilar.push('Kontrol değişkenleri arasında tam doğrusal bağımlılık var; bağımlı değişken(ler) çıkarıldı.');
  const r = rHesapla(ex.kalan, ey.kalan);
  if (Number.isNaN(r)) throw new IstatistikHatasi('Kısmi korelasyon hesaplanamıyor: kontrol değişkenleri x ya da y\'yi tamamen açıklıyor.');
  const t = rdenT(r, df);
  const ga = n - 3 - q > 0 ? fisherGA(r, 1 / Math.sqrt(n - 3 - q), duzey) : { alt: NaN, ust: NaN };
  return {
    yontem: 'kismi-korelasyon',
    ad: 'Kısmi korelasyon',
    istatistik: t,
    istatistikAdi: 't',
    sd: df,
    p: tP(t, df, alternatif),
    alternatif,
    r,
    kontrolSayisi: q,
    etki: etkiNesnesi('Kısmi r', 'r', r, ga, yorumR(r)),
    ga: { duzey, alt: ga.alt, ust: ga.ust, neyin: 'kısmi r' },
    n,
    uyarilar,
  };
}

function gecerli(v) {
  return typeof v === 'number' && Number.isFinite(v);
}

/**
 * Korelasyon matrisi.
 * eksik = 'ikili' (varsayılan, SPSS CORRELATIONS varsayılanı): her çift için iki değişkende de dolu gözlemler;
 * eksik = 'liste': tüm değişkenlerde dolu olan gözlemler.
 * Hesaplanamayan hücrelerde (sabit değişken, yetersiz gözlem) r ve p null'dur. Köşegende r = 1, p = null.
 * @param {(number|null)[][]} degiskenler sütunlar listesi (her biri aynı uzunlukta)
 * @param {{yontem?:'pearson'|'spearman'|'kendall', eksik?:'ikili'|'liste', adlar?:string[], alternatif?:'iki'|'buyuk'|'kucuk'}} [secenek]
 * @returns {{yontem:string, eksik:string, adlar:string[], r:(number|null)[][], p:(number|null)[][], n:number[][]}}
 */
export function korelasyonMatrisi(degiskenler, secenek = {}) {
  const yontem = secenek.yontem ?? 'pearson';
  const eksik = secenek.eksik ?? 'ikili';
  const alternatif = secenek.alternatif ?? 'iki';
  if (!['pearson', 'spearman', 'kendall'].includes(yontem)) throw new IstatistikHatasi(`Bilinmeyen korelasyon yöntemi: ${yontem}`);
  if (!['ikili', 'liste'].includes(eksik)) throw new IstatistikHatasi(`Bilinmeyen eksik veri yöntemi: ${eksik}`);
  if (!Array.isArray(degiskenler) || degiskenler.length < 2) throw new IstatistikHatasi('Korelasyon matrisi için en az 2 değişken gerekir.');
  const satirSayisi = degiskenler[0].length;
  if (degiskenler.some((d) => d.length !== satirSayisi)) throw new IstatistikHatasi('Tüm değişkenlerin gözlem sayısı eşit olmalıdır.');
  const p = degiskenler.length;
  const adlar = secenek.adlar ?? degiskenler.map((_, i) => `Değişken ${i + 1}`);
  let satirlar = Array.from({ length: satirSayisi }, (_, i) => i);
  if (eksik === 'liste') satirlar = satirlar.filter((i) => degiskenler.every((d) => gecerli(d[i])));
  const fonk = yontem === 'pearson' ? pearson : yontem === 'spearman' ? spearman : kendallTauB;
  const R = Array.from({ length: p }, () => new Array(p).fill(null));
  const P = Array.from({ length: p }, () => new Array(p).fill(null));
  const Nm = Array.from({ length: p }, () => new Array(p).fill(0));
  for (let a = 0; a < p; a++) {
    const tekil = satirlar.filter((i) => gecerli(degiskenler[a][i]));
    R[a][a] = 1;
    Nm[a][a] = tekil.length;
    for (let b = a + 1; b < p; b++) {
      const ortak = satirlar.filter((i) => gecerli(degiskenler[a][i]) && gecerli(degiskenler[b][i]));
      const x = ortak.map((i) => degiskenler[a][i]);
      const y = ortak.map((i) => degiskenler[b][i]);
      Nm[a][b] = Nm[b][a] = ortak.length;
      try {
        const s = fonk(x, y, { alternatif });
        const r = yontem === 'pearson' ? s.r : yontem === 'spearman' ? s.rho : s.tau;
        R[a][b] = R[b][a] = r;
        P[a][b] = P[b][a] = s.p;
      } catch (e) {
        if (!(e instanceof IstatistikHatasi)) throw e;
      }
    }
  }
  return { yontem, eksik, adlar, r: R, p: P, n: Nm };
}

/**
 * İki bağımsız korelasyon katsayısının karşılaştırılması (Fisher z):
 * z = (atanh r₁ − atanh r₂) / √(1/(n₁ − 3) + 1/(n₂ − 3)). Etki büyüklüğü Cohen q = atanh r₁ − atanh r₂.
 * @param {number} r1
 * @param {number} n1
 * @param {number} r2
 * @param {number} n2
 * @param {{alternatif?:'iki'|'buyuk'|'kucuk'}} [secenek]
 */
export function ikiKorelasyonKarsilastir(r1, n1, r2, n2, secenek = {}) {
  const { alternatif } = secenekler(secenek);
  for (const r of [r1, r2]) if (!(r > -1 && r < 1)) throw new IstatistikHatasi('Korelasyon katsayıları −1 ile 1 arasında (uçlar hariç) olmalıdır.');
  if (!(n1 > 3 && n2 > 3)) throw new IstatistikHatasi('İki korelasyonu karşılaştırmak için her örneklemde en az 4 gözlem gerekir.');
  const q = Math.atanh(r1) - Math.atanh(r2);
  const sh = Math.sqrt(1 / (n1 - 3) + 1 / (n2 - 3));
  const z = q / sh;
  return {
    yontem: 'iki-korelasyon-fisher-z',
    ad: 'İki bağımsız korelasyonun karşılaştırılması (Fisher z)',
    istatistik: z,
    istatistikAdi: 'z',
    sd: null,
    p: zP(z, alternatif),
    alternatif,
    etki: etkiNesnesi('Cohen q', 'q', q),
    sh,
    n: n1 + n2,
    uyarilar: [],
  };
}
