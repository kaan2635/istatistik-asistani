// Olasılık dağılımları: yoğunluk (d), birikimli dağılım (p) ve ters birikimli (q) fonksiyonları.
// altKuyruk = true → P(X <= x); false → P(X > x). Üst kuyruk ayrı hesaplandığı için küçük p değerleri hassastır.

import {
  EPS,
  LOG_SQRT_2PI,
  SQRT_2PI,
  brent,
  gammaP,
  gaussLegendre,
  gammaQ,
  ibeta,
  integral,
  lbeta,
  lgamma,
  logFaktoriyel,
  log1pmx,
  stirlingDuzeltme,
  tersCoz,
} from './sayisal.js';

// ---------- Normal ----------

export function dnorm(x, ort = 0, ss = 1) {
  const z = (x - ort) / ss;
  return Math.exp(-0.5 * z * z) / (SQRT_2PI * ss);
}

/** Standart normal birikimli dağılım; Φ(-|z|) = Q(1/2, z²/2) / 2 bağıntısıyla hesaplanır. */
export function pnorm(x, ort = 0, ss = 1, altKuyruk = true) {
  if (Number.isNaN(x)) return NaN;
  let z = (x - ort) / ss;
  if (!altKuyruk) z = -z;
  if (z === -Infinity) return 0;
  if (z === Infinity) return 1;
  const h = 0.5 * z * z;
  if (z < 0) return 0.5 * gammaQ(0.5, h);
  return 0.5 + 0.5 * gammaP(0.5, h);
}

/** Üst kuyruk olasılığı P(Z > z) */
export function pnormUst(z) {
  return pnorm(z, 0, 1, false);
}

function qnormAlt(p) {
  // p <= 0.5 için. Başlangıç: Abramowitz & Stegun 26.2.23, ardından Halley iterasyonları.
  const t = Math.sqrt(-2 * Math.log(p));
  let x = -(t - (2.515517 + 0.802853 * t + 0.010328 * t * t) / (1 + 1.432788 * t + 0.189269 * t * t + 0.001308 * t * t * t));
  for (let i = 0; i < 6; i++) {
    const e = pnorm(x) - p;
    const u = e * SQRT_2PI * Math.exp(0.5 * x * x);
    const yeni = x - u / (1 + 0.5 * x * u);
    if (!Number.isFinite(yeni)) break;
    const fark = Math.abs(yeni - x);
    x = yeni;
    if (fark <= 1e-15 * Math.max(1, Math.abs(x))) break;
  }
  return x;
}

/** Standart normal dağılımın ters fonksiyonu */
export function qnorm(p, ort = 0, ss = 1, altKuyruk = true) {
  if (Number.isNaN(p) || p < 0 || p > 1) return NaN;
  if (!altKuyruk) return ort + ss * qnormUst(p);
  if (p === 0) return -Infinity;
  if (p === 1) return Infinity;
  if (p === 0.5) return ort;
  const z = p < 0.5 ? qnormAlt(p) : -qnormAlt(1 - p);
  return ort + ss * z;
}

/** P(Z > z) = p çözümü (küçük p için hassas) */
export function qnormUst(p) {
  if (p <= 0) return Infinity;
  if (p >= 1) return -Infinity;
  if (p === 0.5) return 0;
  if (p < 0.5) return -qnormAlt(p);
  return qnormAlt(1 - p);
}

// ---------- t ----------

export function dt(x, df) {
  if (df === Infinity) return dnorm(x);
  return Math.exp(-lbeta(0.5, df / 2) - 0.5 * Math.log(df) - ((df + 1) / 2) * Math.log1p((x * x) / df));
}

/** Student t birikimli dağılımı */
export function pt(t, df, altKuyruk = true) {
  if (Number.isNaN(t) || !(df > 0)) return NaN;
  if (df === Infinity) return pnorm(t, 0, 1, altKuyruk);
  if (t === Infinity) return altKuyruk ? 1 : 0;
  if (t === -Infinity) return altKuyruk ? 0 : 1;
  if (df > 4e5) {
    // Çok büyük sd'de eksik beta zincir kesri yavaşlar; R ile aynı düzeltilmiş normal yaklaşım (hata O(1/sd²))
    const v = 1 / (4 * df);
    return pnorm((t * (1 - v)) / Math.sqrt(1 + t * t * 2 * v), 0, 1, altKuyruk);
  }
  const t2 = t * t;
  const x = df / (df + t2);
  const y = t2 / (df + t2);
  const kuyruk = 0.5 * ibeta(x, df / 2, 0.5, true, y); // P(T > |t|)
  const negatif = t < 0;
  if (altKuyruk) return negatif ? kuyruk : 1 - kuyruk;
  return negatif ? 1 - kuyruk : kuyruk;
}

/** İki yönlü p değeri: P(|T| >= |t|) */
export function ptIkiYonlu(t, df) {
  return Math.min(1, 2 * pt(-Math.abs(t), df));
}

export function qt(p, df, altKuyruk = true) {
  if (Number.isNaN(p) || p < 0 || p > 1 || !(df > 0)) return NaN;
  if (df === Infinity) return qnorm(p, 0, 1, altKuyruk);
  let alt = altKuyruk ? p : 1 - p;
  const ust = altKuyruk ? 1 - p : p;
  if (alt === 0) return -Infinity;
  if (ust === 0) return Infinity;
  if (alt === 0.5) return 0;
  // Simetri: alt kuyrukta çöz, böylece küçük olasılıklar hassas kalır.
  const kucuk = Math.min(alt, ust);
  const isaret = alt < 0.5 ? -1 : 1;
  const z = qnormUst(kucuk);
  // Cornish-Fisher başlangıcı
  const g1 = (z ** 3 + z) / 4;
  const g2 = (5 * z ** 5 + 16 * z ** 3 + 3 * z) / 96;
  let bas = z + g1 / df + g2 / (df * df);
  if (!Number.isFinite(bas) || bas <= 0) bas = 1;
  // P(T > x) = kucuk, x > 0
  const kok = tersCoz((x) => -pt(x, df, false), -kucuk, bas, 0, Infinity, 1e-14);
  return isaret * kok;
}

// ---------- Ki-kare ----------

export function dchisq(x, df) {
  if (x < 0) return 0;
  if (x === 0) return df === 2 ? 0.5 : df < 2 ? Infinity : 0;
  const k = df / 2;
  return Math.exp((k - 1) * Math.log(x) - x / 2 - k * Math.LN2 - lgamma(k));
}

export function pchisq(x, df, altKuyruk = true) {
  if (Number.isNaN(x) || !(df > 0)) return NaN;
  if (x <= 0) return altKuyruk ? 0 : 1;
  return altKuyruk ? gammaP(df / 2, x / 2) : gammaQ(df / 2, x / 2);
}

export function qchisq(p, df, altKuyruk = true) {
  if (Number.isNaN(p) || p < 0 || p > 1 || !(df > 0)) return NaN;
  const ust = altKuyruk ? 1 - p : p;
  const alt = altKuyruk ? p : 1 - p;
  if (alt === 0) return 0;
  if (ust === 0) return Infinity;
  // Wilson-Hilferty başlangıcı
  const z = qnormUst(ust);
  const h = 2 / (9 * df);
  let bas = df * (1 - h + z * Math.sqrt(h)) ** 3;
  if (!(bas > 0)) bas = Math.max(df * 0.5, 0.01);
  if (alt < ust) return tersCoz((x) => pchisq(x, df), alt, bas, 0, Infinity, 1e-14);
  return tersCoz((x) => -pchisq(x, df, false), -ust, bas, 0, Infinity, 1e-14);
}

// ---------- F ----------

export function df_(x, d1, d2) {
  if (x < 0) return 0;
  if (x === 0) return d1 === 2 ? 1 : d1 < 2 ? Infinity : 0;
  const lg = (d1 / 2) * Math.log(d1) + (d2 / 2) * Math.log(d2) + (d1 / 2 - 1) * Math.log(x) - ((d1 + d2) / 2) * Math.log(d2 + d1 * x) - lbeta(d1 / 2, d2 / 2);
  return Math.exp(lg);
}

export function pf(f, d1, d2, altKuyruk = true) {
  if (Number.isNaN(f) || !(d1 > 0) || !(d2 > 0)) return NaN;
  if (f <= 0) return altKuyruk ? 0 : 1;
  if (f === Infinity) return altKuyruk ? 1 : 0;
  if (d2 === Infinity) return pchisq(f * d1, d1, altKuyruk);
  const pay = d1 * f;
  const x = pay / (pay + d2);
  const y = d2 / (pay + d2);
  return ibeta(x, d1 / 2, d2 / 2, altKuyruk, y);
}

export function qf(p, d1, d2, altKuyruk = true) {
  if (Number.isNaN(p) || p < 0 || p > 1) return NaN;
  const ust = altKuyruk ? 1 - p : p;
  const alt = altKuyruk ? p : 1 - p;
  if (alt === 0) return 0;
  if (ust === 0) return Infinity;
  const bas = Math.max(qchisq(alt, d1) / d1, 1e-3);
  if (alt < ust) return tersCoz((x) => pf(x, d1, d2), alt, bas, 0, Infinity, 1e-14);
  return tersCoz((x) => -pf(x, d1, d2, false), -ust, bas, 0, Infinity, 1e-14);
}

// ---------- Beta ----------

export function dbeta(x, a, b) {
  if (x < 0 || x > 1) return 0;
  return Math.exp((a - 1) * Math.log(x) + (b - 1) * Math.log1p(-x) - lbeta(a, b));
}

export function pbeta(x, a, b, altKuyruk = true) {
  if (x <= 0) return altKuyruk ? 0 : 1;
  if (x >= 1) return altKuyruk ? 1 : 0;
  return ibeta(x, a, b, altKuyruk);
}

export function qbeta(p, a, b, altKuyruk = true) {
  if (Number.isNaN(p) || p < 0 || p > 1) return NaN;
  const alt = altKuyruk ? p : 1 - p;
  const ust = altKuyruk ? 1 - p : p;
  if (alt === 0) return 0;
  if (ust === 0) return 1;
  const f = alt < ust ? (x) => pbeta(x, a, b) - alt : (x) => ust - pbeta(x, a, b, false);
  // Kökü içeren aralığı daralt (sınırlara yakın kökler için geometrik adımlarla), sonra Brent
  let lo = 0;
  let hi = 1;
  let orta = a / (a + b);
  for (let i = 0; i < 200; i++) {
    const v = f(orta);
    if (v === 0) return orta;
    if (v < 0) lo = orta;
    else hi = orta;
    if (hi - lo < 1e-3) break;
    orta = lo === 0 ? hi / 16 : hi === 1 ? 1 - (1 - lo) / 16 : (lo + hi) / 2;
  }
  const tol = Math.min(1e-15, (hi - lo) * 1e-12) || 1e-300;
  return brent(f, lo, hi, tol);
}

// ---------- Gamma ----------

export function pgamma(x, sekil, olcek = 1, altKuyruk = true) {
  if (x <= 0) return altKuyruk ? 0 : 1;
  return altKuyruk ? gammaP(sekil, x / olcek) : gammaQ(sekil, x / olcek);
}

// ---------- Kesikli dağılımlar ----------

/** Loader (2000) sapma fonksiyonu: x ln(x/np) + np - x */
function bd0(x, np) {
  if (Math.abs(x - np) < 0.1 * (x + np)) {
    let v = (x - np) / (x + np);
    let s = (x - np) * v;
    let ej = 2 * x * v;
    v *= v;
    for (let j = 1; j < 1000; j++) {
      ej *= v;
      const s1 = s + ej / (2 * j + 1);
      if (s1 === s) return s1;
      s = s1;
    }
    return s;
  }
  return x * Math.log(x / np) + np - x;
}

/** lgamma(n+1) - (n + 0.5) ln n + n - ln √(2π); bu ifade Stirling düzeltme terimine eşittir. */
function stirlerr(n) {
  return stirlingDuzeltme(n);
}

/** Binom olasılık fonksiyonu (Loader'ın hassas algoritması) */
export function dbinom(x, n, p, log = false) {
  if (x < 0 || x > n || !Number.isInteger(x)) return log ? -Infinity : 0;
  let lg;
  if (p === 0) lg = x === 0 ? 0 : -Infinity;
  else if (p === 1) lg = x === n ? 0 : -Infinity;
  else if (x === 0) lg = n * Math.log1p(-p);
  else if (x === n) lg = n * Math.log(p);
  else {
    const q = 1 - p;
    lg = stirlerr(n) - stirlerr(x) - stirlerr(n - x) - bd0(x, n * p) - bd0(n - x, n * q);
    lg += 0.5 * Math.log(n / (2 * Math.PI * x * (n - x)));
  }
  return log ? lg : Math.exp(lg);
}

/** P(X <= x) binom */
export function pbinom(x, n, p, altKuyruk = true) {
  x = Math.floor(x + 1e-7);
  if (x < 0) return altKuyruk ? 0 : 1;
  if (x >= n) return altKuyruk ? 1 : 0;
  // P(X <= x) = I_{1-p}(n - x, x + 1)
  return ibeta(1 - p, n - x, x + 1, altKuyruk, p);
}

export function dpois(x, lambda, log = false) {
  if (x < 0 || !Number.isInteger(x)) return log ? -Infinity : 0;
  if (lambda === 0) return x === 0 ? (log ? 0 : 1) : log ? -Infinity : 0;
  let lg;
  if (x === 0) lg = -lambda;
  else lg = -stirlerr(x) - bd0(x, lambda) - 0.5 * Math.log(2 * Math.PI * x);
  return log ? lg : Math.exp(lg);
}

export function ppois(x, lambda, altKuyruk = true) {
  x = Math.floor(x + 1e-7);
  if (x < 0) return altKuyruk ? 0 : 1;
  return altKuyruk ? gammaQ(x + 1, lambda) : gammaP(x + 1, lambda);
}

/**
 * Hipergeometrik olasılık: m beyaz, n siyah toptan k çekilişte x beyaz.
 * (R: dhyper(x, m, n, k))
 */
export function dhyper(x, m, n, k, log = false) {
  if (x < Math.max(0, k - n) || x > Math.min(k, m)) return log ? -Infinity : 0;
  const lg = logFaktoriyel(m) - logFaktoriyel(x) - logFaktoriyel(m - x) + logFaktoriyel(n) - logFaktoriyel(k - x) - logFaktoriyel(n - k + x) - (logFaktoriyel(m + n) - logFaktoriyel(k) - logFaktoriyel(m + n - k));
  return log ? lg : Math.exp(lg);
}

export function phyper(x, m, n, k, altKuyruk = true) {
  const lo = Math.max(0, k - n);
  const hi = Math.min(k, m);
  x = Math.floor(x + 1e-7);
  if (x < lo) return altKuyruk ? 0 : 1;
  if (x >= hi) return altKuyruk ? 1 : 0;
  let alt = 0;
  for (let i = lo; i <= x; i++) alt += dhyper(i, m, n, k);
  let ust = 0;
  for (let i = x + 1; i <= hi; i++) ust += dhyper(i, m, n, k);
  const toplam = alt + ust;
  return altKuyruk ? alt / toplam : ust / toplam;
}

// ---------- Studentized range (Tukey) ----------

// Açıklık dağılımı için sabit ızgara: z ∈ [-9, 9], 12 panel × 16 noktalı Gauss-Legendre.
// Φ(z) ve φ(z) ızgarada bir kez hesaplanır; yalnızca Φ(z - w) her çağrıda yeniden hesaplanır.
const ARALIK_IZGARA = (() => {
  const { x, w } = gaussLegendre(16);
  const paneller = 12;
  const alt = -9;
  const genislik = 18 / paneller;
  const z = [];
  const agirlik = [];
  for (let p = 0; p < paneller; p++) {
    const a = alt + p * genislik;
    for (let i = 0; i < x.length; i++) {
      z.push(a + (genislik * (x[i] + 1)) / 2);
      agirlik.push((w[i] * genislik) / 2);
    }
  }
  const phi = z.map((v) => dnorm(v));
  const Phi = z.map((v) => pnorm(v));
  return { z, agirlik, phi, Phi };
})();

/** k bağımsız standart normalin açıklığının dağılımı: P(R <= w) = k ∫ φ(z) [Φ(z) - Φ(z - w)]^(k-1) dz */
function aralikOlasiligi(w, k) {
  if (w <= 0) return 0;
  if (w > 40) return 1;
  const { z, agirlik, phi, Phi } = ARALIK_IZGARA;
  let s = 0;
  for (let i = 0; i < z.length; i++) {
    const fark = Phi[i] - pnorm(z[i] - w);
    if (fark > 0) s += agirlik[i] * phi[i] * Math.exp((k - 1) * Math.log(fark));
  }
  return Math.min(1, k * s);
}

/**
 * s = √(χ²_ν / ν) değişkeninin log yoğunluğu. a = ν/2 için iptalsiz biçim:
 * ln 2 + ½ ln a − ln √(2π) − S(a) + a·[ln(s²) − (s² − 1)] − ln s  (S: Stirling düzeltme terimi)
 */
function logSYogunluk(s, nu) {
  if (s <= 0) return -Infinity;
  const a = nu / 2;
  return Math.LN2 + 0.5 * Math.log(a) - LOG_SQRT_2PI - stirlingDuzeltme(a) + a * log1pmx(s * s - 1) - Math.log(s);
}

function sAraligi(nu) {
  const kok = Math.sqrt(2 * nu);
  const ustChi = nu + 14 * kok + 120;
  const altChi = Math.max(0, nu - 14 * kok);
  return [Math.sqrt(altChi / nu), Math.sqrt(ustChi / nu)];
}

function sKirilmaNoktalari(nu) {
  const [a, b] = sAraligi(nu);
  const ss = 1 / Math.sqrt(2 * nu);
  const noktalar = [a];
  for (const k of [-8, -5, -3, -2, -1, 0, 1, 2, 3, 5, 8, 12]) {
    const v = 1 + k * ss;
    if (v > a && v < b) noktalar.push(v);
  }
  noktalar.push(b);
  return noktalar.sort((x, y) => x - y);
}

/** s üzerinde uyarlamalı integral; aralık, yoğunluğun yoğunlaştığı bölgeye göre parçalanır */
function sIntegrali(g, nu) {
  const noktalar = sKirilmaNoktalari(nu);
  let s = 0;
  for (let i = 0; i < noktalar.length - 1; i++) {
    if (noktalar[i + 1] > noktalar[i]) s += integral((v) => g(v) * Math.exp(logSYogunluk(v, nu)), noktalar[i], noktalar[i + 1], 1e-12);
  }
  return s;
}

/** s üzerinde sabit Gauss-Legendre (pahalı integrandlar için) */
function sIntegraliSabit(g, nu) {
  const noktalar = sKirilmaNoktalari(nu);
  const { x, w } = gaussLegendre(20);
  let s = 0;
  for (let i = 0; i < noktalar.length - 1; i++) {
    const a = noktalar[i];
    const b = noktalar[i + 1];
    if (b <= a) continue;
    const yari = (b - a) / 2;
    const orta = (a + b) / 2;
    for (let j = 0; j < x.length; j++) {
      const v = orta + yari * x[j];
      s += w[j] * yari * g(v) * Math.exp(logSYogunluk(v, nu));
    }
  }
  return s;
}

/** Studentized range birikimli dağılımı P(Q <= q; k, ν) (R: ptukey(q, nmeans = k, df = ν)) */
export function ptukey(q, k, nu, altKuyruk = true) {
  if (q <= 0) return altKuyruk ? 0 : 1;
  let p;
  if (nu === Infinity || nu > 25000) p = aralikOlasiligi(q, k);
  else p = sIntegraliSabit((s) => aralikOlasiligi(q * s, k), nu);
  p = Math.min(1, Math.max(0, p));
  return altKuyruk ? p : 1 - p;
}

export function qtukey(p, k, nu) {
  if (p <= 0) return 0;
  if (p >= 1) return Infinity;
  const bas = Math.max(0.5, Math.SQRT2 * qnorm(1 - (1 - p) / (k * (k - 1))));
  return tersCoz((q) => ptukey(q, k, nu), p, bas, 0, Infinity, 1e-11);
}

// ---------- Merkezi olmayan dağılımlar (güç analizi, etki büyüklüğü güven aralıkları) ----------

/** Merkezi olmayan t: P(T <= t; ν, δ) */
export function pnt(t, df, ncp, altKuyruk = true) {
  if (ncp === 0) return pt(t, df, altKuyruk);
  if (df === Infinity || df > 1e6) return pnorm(t - ncp, 0, 1, altKuyruk);
  // T = (Z + δ) / S, S = √(χ²/ν): P(T <= t) = E[Φ(t S - δ)]
  const g = altKuyruk ? (s) => pnorm(t * s - ncp) : (s) => pnorm(ncp - t * s);
  return Math.min(1, Math.max(0, sIntegrali(g, df)));
}

function poissonAgirlikliToplam(lambdaYari, terim) {
  // Σ_j Pois(j; λ/2) · terim(j), moddan iki yöne
  if (lambdaYari === 0) return terim(0);
  const mod = Math.floor(lambdaYari);
  let toplam = 0;
  let agirlikToplam = 0;
  for (let j = mod; j >= 0; j--) {
    const w = Math.exp(-lambdaYari + j * Math.log(lambdaYari) - logFaktoriyel(j));
    toplam += w * terim(j);
    agirlikToplam += w;
    if (w < 1e-17 && j < mod) break;
  }
  for (let j = mod + 1; j < mod + 100000; j++) {
    const w = Math.exp(-lambdaYari + j * Math.log(lambdaYari) - logFaktoriyel(j));
    toplam += w * terim(j);
    agirlikToplam += w;
    if (w < 1e-17) break;
  }
  return toplam;
}

/** Merkezi olmayan F: P(F <= f; d1, d2, λ) */
export function pnf(f, d1, d2, ncp, altKuyruk = true) {
  if (ncp === 0) return pf(f, d1, d2, altKuyruk);
  if (f <= 0) return altKuyruk ? 0 : 1;
  const pay = d1 * f;
  const x = pay / (pay + d2);
  const y = d2 / (pay + d2);
  return Math.min(1, Math.max(0, poissonAgirlikliToplam(ncp / 2, (j) => ibeta(x, d1 / 2 + j, d2 / 2, altKuyruk, y))));
}

/** Merkezi olmayan ki-kare: P(X <= x; df, λ) */
export function pnchisq(x, df, ncp, altKuyruk = true) {
  if (ncp === 0) return pchisq(x, df, altKuyruk);
  if (x <= 0) return altKuyruk ? 0 : 1;
  return Math.min(1, Math.max(0, poissonAgirlikliToplam(ncp / 2, (j) => (altKuyruk ? gammaP(df / 2 + j, x / 2) : gammaQ(df / 2 + j, x / 2)))));
}

// ---------- Kesin parametrik olmayan dağılımlar ----------

const ISARETLI_SIRA_ONBELLEK = new Map();

/** Wilcoxon işaretli sıralar istatistiğinin kesin frekansları (n gözlem, bağ yok). c[s] = toplamı s olan alt küme sayısı */
export function isaretliSiraFrekans(n) {
  if (ISARETLI_SIRA_ONBELLEK.has(n)) return ISARETLI_SIRA_ONBELLEK.get(n);
  const maks = (n * (n + 1)) / 2;
  const c = new Float64Array(maks + 1);
  c[0] = 1;
  for (let i = 1; i <= n; i++) {
    for (let s = (i * (i + 1)) / 2; s >= i; s--) c[s] += c[s - i];
  }
  ISARETLI_SIRA_ONBELLEK.set(n, c);
  return c;
}

/** P(V <= v), V: işaretli sıralar toplamı (R: psignrank) */
export function psignrank(v, n, altKuyruk = true) {
  const c = isaretliSiraFrekans(n);
  const toplam = 2 ** n;
  v = Math.floor(v + 1e-7);
  let s = 0;
  if (altKuyruk) {
    for (let i = 0; i <= Math.min(v, c.length - 1); i++) s += c[i];
  } else {
    for (let i = Math.max(v + 1, 0); i < c.length; i++) s += c[i];
  }
  return s / toplam;
}

const SIRA_TOPLAM_ONBELLEK = new Map();

/** Mann-Whitney U'nun kesin frekansları (m, n; bağ yok). c[u], u = 0..m*n */
export function mannWhitneyFrekans(m, n) {
  const anahtar = `${m},${n}`;
  if (SIRA_TOPLAM_ONBELLEK.has(anahtar)) return SIRA_TOPLAM_ONBELLEK.get(anahtar);
  // U'nun üretici fonksiyonu Gauss binom katsayısıdır: f(m, n) = f(m-1, n) q^n + f(m, n-1)
  // Satır satır dinamik programlama: d[j] = f(i, j) dizileri
  let onceki = [];
  for (let j = 0; j <= n; j++) onceki.push(Float64Array.of(1)); // i = 0: f(0, j) = 1
  for (let i = 1; i <= m; i++) {
    const simdiki = [Float64Array.of(1)]; // f(i, 0) = 1
    for (let j = 1; j <= n; j++) {
      const a = onceki[j]; // f(i-1, j), j kaydırmalı
      const b = simdiki[j - 1]; // f(i, j-1)
      const uzunluk = i * j + 1;
      const c = new Float64Array(uzunluk);
      for (let u = 0; u < a.length; u++) c[u + j] += a[u];
      for (let u = 0; u < b.length; u++) c[u] += b[u];
      simdiki.push(c);
    }
    onceki = simdiki;
  }
  const sonuc = onceki[n];
  SIRA_TOPLAM_ONBELLEK.set(anahtar, sonuc);
  return sonuc;
}

/** P(U <= u) (R: pwilcox(u, m, n)) */
export function pwilcox(u, m, n, altKuyruk = true) {
  const c = mannWhitneyFrekans(m, n);
  let toplam = 0;
  for (let i = 0; i < c.length; i++) toplam += c[i];
  u = Math.floor(u + 1e-7);
  let s = 0;
  if (altKuyruk) for (let i = 0; i <= Math.min(u, c.length - 1); i++) s += c[i];
  else for (let i = Math.max(0, u + 1); i < c.length; i++) s += c[i];
  return s / toplam;
}

export { EPS, LOG_SQRT_2PI };
