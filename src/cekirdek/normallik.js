// Normallik testleri: Shapiro-Wilk, Kolmogorov-Smirnov (Lilliefors), Anderson-Darling, D'Agostino-Pearson
// ve SPSS'in çarpıklık/basıklık z değerleri. Her fonksiyon eksik değerlerden arındırılmış number[] alır.

import { pchisq, pnorm, qnorm } from './dagilim.js';
import { basiklik, carpiklik, ortalama, sirali, standartSapma, toplam } from './betimsel.js';
import { IstatistikHatasi } from './hata.js';
import { SQRT_2PI } from './sayisal.js';

function sayiDenetle(x, enAz, testAdi) {
  if (!Array.isArray(x) && !ArrayBuffer.isView(x)) throw new IstatistikHatasi(`${testAdi} için sayı dizisi gerekir.`);
  for (const v of x) {
    if (typeof v !== 'number' || !Number.isFinite(v)) throw new IstatistikHatasi(`${testAdi}: veride eksik ya da sayısal olmayan değer var.`);
  }
  if (x.length < enAz) throw new IstatistikHatasi(`${testAdi} için en az ${enAz} gözlem gerekir.`);
}

function sabitDenetle(s, testAdi) {
  if (s[s.length - 1] - s[0] === 0) throw new IstatistikHatasi(`Tüm değerler aynı; ${testAdi} hesaplanamaz.`);
}

/** log Φ(z); z ≪ 0 için Mills oranının zincir kesri kullanılır (R: pnorm(z, log.p = TRUE)). */
export function logPnorm(z) {
  if (z > 5) return Math.log1p(-pnorm(z, 0, 1, false));
  if (z > -30) return Math.log(pnorm(z));
  // Φ(z) = φ(t) R(t), t = −z; R(t) = 1 / (t + 1/(t + 2/(t + 3/(t + …)))) (geriye doğru hesap)
  const t = -z;
  let kesir = t;
  for (let k = 80; k >= 1; k--) kesir = t + k / kesir;
  return -0.5 * t * t - Math.log(SQRT_2PI) - Math.log(kesir);
}

// ---------- Shapiro-Wilk (Royston 1995, AS R94) ----------

function polinom(c, x) {
  // R swilk.c poly(): c[0] + c[1] x + ... (Horner, aynı işlem sırası)
  const n = c.length;
  let sonuc = c[0];
  if (n > 1) {
    let p = x * c[n - 1];
    for (let j = n - 2; j > 0; j--) p = (p + c[j]) * x;
    sonuc += p;
  }
  return sonuc;
}

const SW_G = [-2.273, 0.459];
const SW_C1 = [0.0, 0.221157, -0.147981, -2.07119, 4.434685, -2.706056];
const SW_C2 = [0.0, 0.042981, -0.293762, -1.752461, 5.682633, -3.582633];
const SW_C3 = [0.544, -0.39978, 0.025054, -6.714e-4];
const SW_C4 = [1.3822, -0.77857, 0.062767, -0.0020322];
const SW_C5 = [-1.5861, -0.31082, -0.083751, 0.0038915];
const SW_C6 = [-0.4803, -0.082676, 0.0030302];

/** R'nin swilk.c'sinin birebir uyarlaması. x sıralı olmalıdır. */
function swilk(x) {
  const n = x.length;
  const nn2 = Math.floor(n / 2);
  const a = new Float64Array(nn2 + 1); // 1 tabanlı
  const kucuk = 1e-19;
  if (n === 3) {
    a[1] = Math.SQRT1_2;
  } else {
    const an25 = n + 0.25;
    let summ2 = 0;
    for (let i = 1; i <= nn2; i++) {
      a[i] = qnorm((i - 0.375) / an25);
      summ2 += a[i] * a[i];
    }
    summ2 *= 2;
    const ssumm2 = Math.sqrt(summ2);
    const rsn = 1 / Math.sqrt(n);
    const a1 = polinom(SW_C1, rsn) - a[1] / ssumm2;
    let i1;
    let fac;
    if (n > 5) {
      i1 = 3;
      const a2 = -a[2] / ssumm2 + polinom(SW_C2, rsn);
      fac = Math.sqrt((summ2 - 2 * (a[1] * a[1]) - 2 * (a[2] * a[2])) / (1 - 2 * (a1 * a1) - 2 * (a2 * a2)));
      a[2] = a2;
    } else {
      i1 = 2;
      fac = Math.sqrt((summ2 - 2 * (a[1] * a[1])) / (1 - 2 * (a1 * a1)));
    }
    a[1] = a1;
    for (let i = i1; i <= nn2; i++) a[i] /= -fac;
  }

  const aralik = x[n - 1] - x[0];
  if (aralik < kucuk) throw new IstatistikHatasi('Tüm değerler aynı; Shapiro-Wilk testi hesaplanamaz.');

  let xx = x[0] / aralik;
  let sx = xx;
  let sa = -a[1];
  for (let i = 1, j = n - 1; i < n; j--) {
    const xi = x[i] / aralik;
    sx += xi;
    i++;
    if (i !== j) sa += Math.sign(i - j) * a[Math.min(i, j)];
    xx = xi;
  }

  // W, veri ile katsayılar arasındaki korelasyonun karesi; 1 − W ayrıca hesaplanır (W ≈ 1 iken yuvarlama hatası)
  sa /= n;
  sx /= n;
  let ssa = 0;
  let ssx = 0;
  let sax = 0;
  for (let i = 0, j = n - 1; i < n; i++, j--) {
    const asa = i !== j ? Math.sign(i - j) * a[1 + Math.min(i, j)] - sa : -sa;
    const xsx = x[i] / aralik - sx;
    ssa += asa * asa;
    ssx += xsx * xsx;
    sax += asa * xsx;
  }
  const ssassx = Math.sqrt(ssa * ssx);
  const w1 = ((ssassx - sax) * (ssassx + sax)) / (ssa * ssx);
  const w = 1 - w1;

  if (n === 3) {
    // kesin p değeri
    const pi6 = 1.90985931710274; // 6/π
    const stqr = 1.0471975511966; // π/3
    const pw = Math.max(0, pi6 * (Math.asin(Math.sqrt(w)) - stqr));
    return { w, p: Math.min(1, pw) };
  }
  let y = Math.log(w1);
  const lxx = Math.log(n);
  let m;
  let s;
  if (n <= 11) {
    const gamma = polinom(SW_G, n);
    if (y >= gamma) return { w, p: 1e-99 };
    y = -Math.log(gamma - y);
    m = polinom(SW_C3, n);
    s = Math.exp(polinom(SW_C4, n));
  } else {
    m = polinom(SW_C5, lxx);
    s = Math.exp(polinom(SW_C6, lxx));
  }
  return { w, p: pnorm(y, m, s, false) };
}

/**
 * Shapiro-Wilk normallik testi. Royston (1995) AS R94 algoritması; R'nin shapiro.test() fonksiyonunun
 * C kodunun (swilk.c) birebir uyarlamasıdır, SPSS EXAMINE "Tests of Normality" tablosundaki Shapiro-Wilk ile aynıdır.
 * 3 ≤ n ≤ 5000. n = 3 için p kesindir.
 * @param {number[]} x
 * @returns {{yontem:string, ad:string, istatistik:number, istatistikAdi:string, sd:null, p:number, n:number, uyarilar:string[]}}
 */
export function shapiroWilk(x) {
  sayiDenetle(x, 3, 'Shapiro-Wilk testi');
  const n = x.length;
  if (n > 5000) throw new IstatistikHatasi('Shapiro-Wilk testi en fazla 5000 gözlem için tanımlıdır; Kolmogorov-Smirnov (Lilliefors) ya da Anderson-Darling testini kullanın.');
  let s = sirali(x);
  sabitDenetle(s, 'Shapiro-Wilk testi');
  const aralik = s[n - 1] - s[0];
  // R: aralık çok küçükse veri aralığa bölünerek ölçeklenir (shapiro.test)
  if (aralik < 1e-10) s = s.map((v) => v / aralik);
  const { w, p } = swilk(s);
  const uyarilar = [];
  if (n < 7) uyarilar.push('Örneklem çok küçük (n < 7); normallik testlerinin gücü düşüktür.');
  return { yontem: 'shapiro-wilk', ad: 'Shapiro-Wilk testi', istatistik: w, istatistikAdi: 'W', sd: null, p, n, uyarilar };
}

// ---------- Kolmogorov-Smirnov (Lilliefors) ----------

/**
 * Lilliefors düzeltmeli Kolmogorov-Smirnov normallik testi (ortalama ve standart sapma veriden kestirilir).
 * p değeri nortest::lillie.test ile aynıdır: Dallal-Wilkinson (1986) yaklaşımı; bu yaklaşım 0.1'den büyük
 * çıkarsa Stephens (1974) düzeltilmiş istatistiği üzerinden parçalı polinom kullanılır. n > 100 için
 * D, (n/100)^0.49 ile ölçeklenerek n = 100 formülüne taşınır.
 * SPSS bu testte 0.2'den büyük p değerlerini ".200*" (gerçek anlamlılığın alt sınırı) olarak yazar;
 * bu durumda sonuçta `pSpssSiniri: true` döner.
 * @param {number[]} x en az 5 gözlem
 */
export function lilliefors(x) {
  sayiDenetle(x, 5, 'Kolmogorov-Smirnov (Lilliefors) testi');
  const n = x.length;
  const s = sirali(x);
  sabitDenetle(s, 'Kolmogorov-Smirnov testi');
  const m = ortalama(x);
  const ss = standartSapma(x, m);
  let dArti = -Infinity;
  let dEksi = -Infinity;
  for (let i = 0; i < n; i++) {
    const p = pnorm((s[i] - m) / ss);
    dArti = Math.max(dArti, (i + 1) / n - p);
    dEksi = Math.max(dEksi, p - i / n);
  }
  const K = Math.max(dArti, dEksi);
  let Kd = K;
  let nd = n;
  if (n > 100) {
    Kd = K * (n / 100) ** 0.49;
    nd = 100;
  }
  let p = Math.exp(-7.01256 * Kd * Kd * (nd + 2.78019) + 2.99587 * Kd * Math.sqrt(nd + 2.78019) - 0.122119 + 0.974598 / Math.sqrt(nd) + 1.67997 / nd);
  if (p > 0.1) {
    const KK = (Math.sqrt(n) - 0.01 + 0.85 / Math.sqrt(n)) * K;
    if (KK <= 0.302) p = 1;
    else if (KK <= 0.5) p = 2.76773 - 19.828315 * KK + 80.709644 * KK ** 2 - 138.55152 * KK ** 3 + 81.218052 * KK ** 4;
    else if (KK <= 0.9) p = -4.901232 + 40.662806 * KK - 97.490286 * KK ** 2 + 94.029866 * KK ** 3 - 32.355711 * KK ** 4;
    else if (KK <= 1.31) p = 6.198765 - 19.558097 * KK + 23.186922 * KK ** 2 - 12.234627 * KK ** 3 + 2.423045 * KK ** 4;
    else p = 0;
  }
  return {
    yontem: 'lilliefors',
    ad: 'Kolmogorov-Smirnov testi (Lilliefors düzeltmeli)',
    istatistik: K,
    istatistikAdi: 'D',
    sd: null,
    p,
    pSpssSiniri: p >= 0.2,
    dArti,
    dEksi,
    n,
    uyarilar: [],
  };
}

// ---------- Anderson-Darling ----------

/**
 * Anderson-Darling normallik testi (ortalama ve standart sapma veriden kestirilir). nortest::ad.test ile aynı:
 * A² istatistiği ve D'Agostino & Stephens (1986) düzeltmesi A²* = A²(1 + 0.75/n + 2.25/n²) üzerinden parçalı p.
 * @param {number[]} x en az 8 gözlem
 */
export function andersonDarling(x) {
  sayiDenetle(x, 8, 'Anderson-Darling testi');
  const n = x.length;
  const s = sirali(x);
  sabitDenetle(s, 'Anderson-Darling testi');
  const m = ortalama(x);
  const ss = standartSapma(x, m);
  const logp1 = new Float64Array(n);
  const logp2 = new Float64Array(n);
  for (let i = 0; i < n; i++) {
    const z = (s[i] - m) / ss;
    logp1[i] = logPnorm(z);
    logp2[i] = logPnorm(-z);
  }
  // A = −n − ortalama(h); h büyük terimlerin toplamı olduğundan telafili toplama kullanılır (iptal hatası)
  const h = new Float64Array(n);
  for (let i = 0; i < n; i++) h[i] = (2 * (i + 1) - 1) * (logp1[i] + logp2[n - 1 - i]);
  const A = -n - toplam(h) / n;
  const AA = (1 + 0.75 / n + 2.25 / (n * n)) * A;
  let p;
  if (AA < 0.2) p = 1 - Math.exp(-13.436 + 101.14 * AA - 223.73 * AA * AA);
  else if (AA < 0.34) p = 1 - Math.exp(-8.318 + 42.796 * AA - 59.938 * AA * AA);
  else if (AA < 0.6) p = Math.exp(0.9177 - 4.279 * AA - 1.38 * AA * AA);
  else if (AA < 10) p = Math.exp(1.2937 - 5.709 * AA + 0.0186 * AA * AA);
  else p = 3.7e-24;
  return {
    yontem: 'anderson-darling',
    ad: 'Anderson-Darling testi',
    istatistik: A,
    istatistikAdi: 'A²',
    sd: null,
    p,
    duzeltilmisIstatistik: AA,
    n,
    uyarilar: [],
  };
}

// ---------- D'Agostino-Pearson ----------

/**
 * D'Agostino-Pearson K² omnibus normallik testi (D'Agostino, Belanger & D'Agostino 1990).
 * Çarpıklık: D'Agostino (1970) z dönüşümü (moments::agostino.test ile aynı).
 * Basıklık: Anscombe & Glynn (1983) z dönüşümü (moments::anscombe.test ile aynı).
 * K² = z₁² + z₂², χ²(2) ile (fBasics::dagoTest omnibus ile aynı).
 * Not: Anscombe-Glynn formülündeki küp kök negatif tabanda işaretini koruyarak alınır (fBasics ve SciPy gibi);
 * moments::anscombe.test bu durumda NaN üretir. Bu durum yalnızca çok basık (platykurtik) küçük örneklemlerde görülür.
 * @param {number[]} x en az 8 gözlem (basıklık testi için n ≥ 20 önerilir)
 */
export function dagostinoPearson(x) {
  sayiDenetle(x, 8, "D'Agostino-Pearson testi");
  const n = x.length;
  const s = sirali(x);
  sabitDenetle(s, "D'Agostino-Pearson testi");
  const m = ortalama(x);
  const d2 = x.map((v) => (v - m) ** 2);
  const m2 = toplam(d2);
  const m3 = toplam(x.map((v) => (v - m) ** 3));
  const m4 = toplam(d2.map((v) => v * v));
  // çarpıklık √b1 (moments: (Σd³/n) / (Σd²/n)^(3/2))
  const b1kok = m3 / n / (m2 / n) ** 1.5;
  const y = b1kok * Math.sqrt(((n + 1) * (n + 3)) / (6 * (n - 2)));
  const beta2 = (3 * (n * n + 27 * n - 70) * (n + 1) * (n + 3)) / ((n - 2) * (n + 5) * (n + 7) * (n + 9));
  const w = Math.sqrt(-1 + Math.sqrt(2 * (beta2 - 1)));
  const delta = 1 / Math.sqrt(Math.log(w));
  const alfa = Math.sqrt(2 / (w * w - 1));
  const ya = y / alfa;
  const z1 = delta * Math.log(ya + Math.sqrt(ya * ya + 1));
  // basıklık b2 (moments: n Σd⁴ / (Σd²)²)
  const b2 = (n * m4) / (m2 * m2);
  const eb2 = (3 * (n - 1)) / (n + 1);
  const vb2 = (24 * n * (n - 2) * (n - 3)) / ((n + 1) ** 2 * (n + 3) * (n + 5));
  const ms3 = ((6 * (n * n - 5 * n + 2)) / ((n + 7) * (n + 9))) * Math.sqrt((6 * (n + 3) * (n + 5)) / (n * (n - 2) * (n - 3)));
  const A = 6 + (8 / ms3) * (2 / ms3 + Math.sqrt(1 + 4 / (ms3 * ms3)));
  const xx = (b2 - eb2) / Math.sqrt(vb2);
  const taban = (1 - 2 / A) / (1 + xx * Math.sqrt(2 / (A - 4)));
  const z2 = (1 - 2 / (9 * A) - Math.cbrt(taban)) / Math.sqrt(2 / (9 * A));
  const K2 = z1 * z1 + z2 * z2;
  const uyarilar = [];
  if (n < 20) uyarilar.push('Basıklık testi (Anscombe-Glynn) için n ≥ 20 önerilir; küçük örneklemde p değerleri yaklaşıktır.');
  return {
    yontem: 'dagostino-pearson',
    ad: "D'Agostino-Pearson K² testi",
    istatistik: K2,
    istatistikAdi: 'K²',
    sd: 2,
    p: pchisq(K2, 2, false),
    carpiklik: { b1: b1kok, z: z1, p: 2 * pnorm(-Math.abs(z1)) },
    basiklik: { b2, z: z2, p: 2 * pnorm(-Math.abs(z2)) },
    n,
    uyarilar,
  };
}

// ---------- SPSS çarpıklık / basıklık z ----------

/**
 * SPSS'in çarpıklık (G1) ve fazlalık basıklık (G2) değerleri, SPSS standart hatalarıyla z = değer / SH ve
 * iki yönlü normal p değerleri. (Kılavuzlarda |z| > 1.96 ya da küçük örneklemde 2.58 ölçütü kullanılır.)
 * SH(G1) = √(6n(n−1) / ((n−2)(n+1)(n+3))), SH(G2) = 2·SH(G1)·√((n²−1) / ((n−3)(n+5))).
 * @param {number[]} x en az 4 gözlem
 */
export function carpiklikBasiklikZ(x) {
  sayiDenetle(x, 4, 'Çarpıklık ve basıklık z değerleri');
  const s = sirali(x);
  sabitDenetle(s, 'çarpıklık ve basıklık');
  const c = carpiklik(x);
  const b = basiklik(x);
  const zc = c.deger / c.sh;
  const zb = b.deger / b.sh;
  return {
    yontem: 'carpiklik-basiklik-z',
    ad: 'Çarpıklık ve basıklık z testleri',
    carpiklik: { deger: c.deger, sh: c.sh, z: zc, p: 2 * pnorm(-Math.abs(zc)) },
    basiklik: { deger: b.deger, sh: b.sh, z: zb, p: 2 * pnorm(-Math.abs(zb)) },
    n: x.length,
    uyarilar: [],
  };
}
