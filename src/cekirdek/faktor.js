// Açımlayıcı faktör analizi (AFA): KMO ve Bartlett testi, temel bileşenler / temel eksen / en çok olabilirlik
// çıkarımı, dik (varimax, quartimax, equamax) ve eğik (promax, doğrudan oblimin) döndürmeler, paralel analiz,
// Velicer MAP testi ve faktör puanları.
//
// Varsayılanlar SPSS FACTOR ile aynıdır (Kaiser normalizasyonu, özdeğer > 1, temel eksen için 25 iterasyon ve
// 0,001 yakınsama ölçütü, promax kappa = 4, oblimin delta = 0). Doğrulama R ile yapılır (psych, GPArotation,
// stats::factanal; ayrıntı arac/r-referans/faktor.R).

import { IstatistikHatasi, gerekli } from './hata.js';
import { sifir, birim, transpoz, carp, korelasyonMatrisi, simetrikTers, ters } from './matris.js';
import { pchisq } from './dagilim.js';
import { yuzdelik as yuzdelikHesapla } from './betimsel.js';

const TEKIL_ESIK = 1e-11; // en küçük özdeğer bunun altındaysa korelasyon matrisi tekil sayılır
const ML_ALT_SINIR = 0.005; // factanal/psych ile aynı: tekliklerin alt sınırı

// ---------------------------------------------------------------------------------------------
// Özdeğer ayrıştırması (Householder üçgenleştirme + örtük QL; JAMA/EISPACK tred2-tql2).
// matris.js'teki Jacobi yöntemiyle aynı sonucu verir, ancak büyük matrislerde ve paralel analizdeki
// binlerce tekrar için çok daha hızlıdır.
// ---------------------------------------------------------------------------------------------

/**
 * Simetrik matrisin özdeğer/özvektörleri (Householder + QL). Özdeğerler büyükten küçüğe sıralıdır.
 * @param {number[][]} A simetrik matris
 * @param {boolean} [vektorIste=true] false ise yalnızca özdeğerler hesaplanır (daha hızlı)
 * @returns {{degerler:number[], vektorler:number[][]|null}} vektorler[i][k]: k. özvektörün i. bileşeni
 */
export function ozdegerAyristir(A, vektorIste = true) {
  const n = A.length;
  const V = A.map((s) => Float64Array.from(s));
  const d = new Float64Array(n);
  const e = new Float64Array(n);
  if (n === 1) return { degerler: [A[0][0]], vektorler: vektorIste ? [[1]] : null };
  // tred2: Householder ile üç köşegenli forma indirgeme
  for (let j = 0; j < n; j++) d[j] = V[n - 1][j];
  for (let i = n - 1; i > 0; i--) {
    let olcek = 0;
    let h = 0;
    for (let k = 0; k < i; k++) olcek += Math.abs(d[k]);
    if (olcek === 0) {
      e[i] = d[i - 1];
      for (let j = 0; j < i; j++) {
        d[j] = V[i - 1][j];
        V[i][j] = 0;
        V[j][i] = 0;
      }
    } else {
      for (let k = 0; k < i; k++) {
        d[k] /= olcek;
        h += d[k] * d[k];
      }
      let f = d[i - 1];
      let g = Math.sqrt(h);
      if (f > 0) g = -g;
      e[i] = olcek * g;
      h -= f * g;
      d[i - 1] = f - g;
      for (let j = 0; j < i; j++) e[j] = 0;
      for (let j = 0; j < i; j++) {
        f = d[j];
        V[j][i] = f;
        g = e[j] + V[j][j] * f;
        for (let k = j + 1; k <= i - 1; k++) {
          g += V[k][j] * d[k];
          e[k] += V[k][j] * f;
        }
        e[j] = g;
      }
      f = 0;
      for (let j = 0; j < i; j++) {
        e[j] /= h;
        f += e[j] * d[j];
      }
      const hh = f / (h + h);
      for (let j = 0; j < i; j++) e[j] -= hh * d[j];
      for (let j = 0; j < i; j++) {
        f = d[j];
        g = e[j];
        for (let k = j; k <= i - 1; k++) V[k][j] -= f * e[k] + g * d[k];
        d[j] = V[i - 1][j];
        V[i][j] = 0;
      }
    }
    d[i] = h;
  }
  for (let i = 0; i < n - 1; i++) {
    V[n - 1][i] = V[i][i];
    V[i][i] = 1;
    const h = d[i + 1];
    if (h !== 0) {
      for (let k = 0; k <= i; k++) d[k] = V[k][i + 1] / h;
      for (let j = 0; j <= i; j++) {
        let g = 0;
        for (let k = 0; k <= i; k++) g += V[k][i + 1] * V[k][j];
        for (let k = 0; k <= i; k++) V[k][j] -= g * d[k];
      }
    }
    for (let k = 0; k <= i; k++) V[k][i + 1] = 0;
  }
  for (let j = 0; j < n; j++) {
    d[j] = V[n - 1][j];
    V[n - 1][j] = 0;
  }
  V[n - 1][n - 1] = 1;
  e[0] = 0;
  // tql2: örtük kaydırmalı QL
  for (let i = 1; i < n; i++) e[i - 1] = e[i];
  e[n - 1] = 0;
  let f = 0;
  let tst1 = 0;
  const eps = 2 ** -52;
  for (let l = 0; l < n; l++) {
    tst1 = Math.max(tst1, Math.abs(d[l]) + Math.abs(e[l]));
    let m = l;
    while (m < n) {
      if (Math.abs(e[m]) <= eps * tst1) break;
      m++;
    }
    if (m > l) {
      let iter = 0;
      do {
        if (++iter > 100) throw new IstatistikHatasi('Özdeğer hesabı yakınsamadı.');
        let g = d[l];
        let p = (d[l + 1] - g) / (2 * e[l]);
        let r = Math.hypot(p, 1);
        if (p < 0) r = -r;
        d[l] = e[l] / (p + r);
        d[l + 1] = e[l] * (p + r);
        const dl1 = d[l + 1];
        let h = g - d[l];
        for (let i = l + 2; i < n; i++) d[i] -= h;
        f += h;
        p = d[m];
        let c = 1;
        let c2 = c;
        let c3 = c;
        const el1 = e[l + 1];
        let s = 0;
        let s2 = 0;
        for (let i = m - 1; i >= l; i--) {
          c3 = c2;
          c2 = c;
          s2 = s;
          g = c * e[i];
          h = c * p;
          r = Math.hypot(p, e[i]);
          e[i + 1] = s * r;
          s = e[i] / r;
          c = p / r;
          p = c * d[i] - s * g;
          d[i + 1] = h + s * (c * g + s * d[i]);
          if (vektorIste) {
            for (let k = 0; k < n; k++) {
              const Vk = V[k];
              h = Vk[i + 1];
              Vk[i + 1] = s * Vk[i] + c * h;
              Vk[i] = c * Vk[i] - s * h;
            }
          }
        }
        p = (-s * s2 * c3 * el1 * e[l]) / dl1;
        e[l] = s * p;
        d[l] = c * p;
      } while (Math.abs(e[l]) > eps * tst1);
    }
    d[l] += f;
    e[l] = 0;
  }
  const sira = Array.from({ length: n }, (_, i) => i).sort((a, b) => d[b] - d[a]);
  const degerler = sira.map((i) => d[i]);
  if (!vektorIste) return { degerler, vektorler: null };
  const vektorler = Array.from({ length: n }, (_, i) => sira.map((k) => V[i][k]));
  return { degerler, vektorler };
}

// ---------------------------------------------------------------------------------------------
// Yardımcılar
// ---------------------------------------------------------------------------------------------

function satirKareToplami(L) {
  return L.map((s) => s.reduce((t, v) => t + v * v, 0));
}

function sutunKareToplami(L) {
  const m = L[0].length;
  const t = new Array(m).fill(0);
  for (const s of L) for (let j = 0; j < m; j++) t[j] += s[j] * s[j];
  return t;
}

function sutunToplami(L) {
  const m = L[0].length;
  const t = new Array(m).fill(0);
  for (const s of L) for (let j = 0; j < m; j++) t[j] += s[j];
  return t;
}

function frobenius(A) {
  let t = 0;
  for (const s of A) for (const v of s) t += v * v;
  return Math.sqrt(t);
}

/** Simetrik pozitif tanımlı matrisin M^(-1/2) kökü (özdeğer ayrıştırmasıyla) */
function tersKarekok(M) {
  const { degerler, vektorler } = ozdegerAyristir(M);
  const k = M.length;
  if (!(degerler[k - 1] > 0)) throw new IstatistikHatasi('Matris pozitif tanımlı değil; ters karekökü alınamıyor.');
  const S = sifir(k, k);
  for (let i = 0; i < k; i++) {
    for (let j = 0; j < k; j++) {
      let t = 0;
      for (let l = 0; l < k; l++) t += (vektorler[i][l] * vektorler[j][l]) / Math.sqrt(degerler[l]);
      S[i][j] = t;
    }
  }
  return S;
}

/** X'in kutupsal (polar) çarpanı: X = U D V' için U V' = X (X'X)^(-1/2) */
function kutupsal(X) {
  return carp(X, tersKarekok(carp(transpoz(X), X)));
}

/** Sütun permütasyonu ve işaret çevirmesi uygula */
function sutunlariDuzenle(M, sira, isaret) {
  return M.map((s) => sira.map((j, k) => s[j] * isaret[k]));
}

function kareDuzenle(M, sira, isaret) {
  return sira.map((a, i) => sira.map((b, j) => M[a][b] * isaret[i] * isaret[j]));
}

function kaiserYorumu(kmo) {
  if (kmo >= 0.9) return 'mükemmel';
  if (kmo >= 0.8) return 'çok iyi';
  if (kmo >= 0.7) return 'iyi';
  if (kmo >= 0.6) return 'orta';
  if (kmo >= 0.5) return 'zayıf';
  return 'kabul edilemez';
}

/**
 * Girdiyi çözümler: ham veri (n × p dizi), {veri} ya da {korelasyon, n}.
 * @returns {{R:number[][], n:number|null, p:number, veri:number[][]|null}}
 */
function girdiyiCoz(girdi) {
  let veri = null;
  if (Array.isArray(girdi)) veri = girdi;
  else if (girdi && Array.isArray(girdi.veri)) veri = girdi.veri;
  else if (girdi && Array.isArray(girdi.korelasyon)) {
    const K = girdi.korelasyon;
    const p = K.length;
    gerekli(p >= 2, 'Faktör analizi için en az iki değişken gerekir.');
    for (const s of K) gerekli(Array.isArray(s) && s.length === p, 'Korelasyon matrisi kare olmalıdır.');
    const R = sifir(p, p);
    for (let i = 0; i < p; i++) {
      for (let j = 0; j < p; j++) {
        const v = K[i][j];
        gerekli(Number.isFinite(v), 'Korelasyon matrisinde eksik ya da sayı olmayan değer var.');
        gerekli(Math.abs(v - K[j][i]) <= 1e-8, 'Korelasyon matrisi simetrik değil.');
        gerekli(Math.abs(v) <= 1 + 1e-10, 'Korelasyon matrisinde mutlak değeri 1\'den büyük değer var.');
        R[i][j] = i === j ? 1 : (K[i][j] + K[j][i]) / 2;
      }
      gerekli(Math.abs(K[i][i] - 1) <= 1e-8, 'Korelasyon matrisinin köşegeni 1 olmalıdır.');
    }
    const n = girdi.n ?? null;
    if (n !== null) gerekli(Number.isFinite(n) && n > 1, 'Örneklem büyüklüğü (n) 1\'den büyük bir sayı olmalıdır.');
    return { R, n, p, veri: null };
  } else {
    throw new IstatistikHatasi('Faktör analizi için veri matrisi (gözlem × değişken) ya da {korelasyon, n} gerekir.');
  }
  const n = veri.length;
  gerekli(n >= 3, 'Faktör analizi için en az 3 gözlem gerekir.');
  const p = Array.isArray(veri[0]) ? veri[0].length : 0;
  gerekli(p >= 2, 'Faktör analizi için en az iki değişken gerekir.');
  for (const s of veri) {
    gerekli(Array.isArray(s) && s.length === p, 'Veri matrisinin tüm satırları aynı sayıda değişken içermelidir.');
    for (const v of s) gerekli(Number.isFinite(v), 'Veride eksik ya da sayı olmayan değer var; önce eksik değerleri ayıklayın.');
  }
  for (let j = 0; j < p; j++) {
    const ilk = veri[0][j];
    if (veri.every((s) => s[j] === ilk)) {
      throw new IstatistikHatasi(`${j + 1}. değişkenin varyansı sıfır (tüm değerler aynı); faktör analizinden çıkarın.`);
    }
  }
  return { R: korelasyonMatrisi(veri), n, p, veri };
}

/** Kare çoklu korelasyonlar (SMC = 1 - 1/r^ii). Tekil matriste SPSS gibi en büyük mutlak korelasyon kullanılır. */
function kareCokluKorelasyon(R, tekil) {
  const p = R.length;
  if (!tekil) {
    const Ri = simetrikTers(R);
    return R.map((_, i) => 1 - 1 / Ri[i][i]);
  }
  return R.map((s, i) => {
    let m = 0;
    for (let j = 0; j < p; j++) if (j !== i) m = Math.max(m, Math.abs(s[j]));
    return m;
  });
}

// ---------------------------------------------------------------------------------------------
// KMO ve Bartlett
// ---------------------------------------------------------------------------------------------

/**
 * Kaiser-Meyer-Olkin örneklem yeterliliği ve Bartlett küresellik testi (SPSS FACTOR /PRINT KMO AIC DET).
 * KMO ve değişken bazında MSA, R'de psych::KMO ile; Bartlett testi psych::cortest.bartlett ile aynıdır:
 * χ² = −(n − 1 − (2p + 5)/6)·ln|R|, sd = p(p − 1)/2.
 * Anti-imaj korelasyon matrisinin köşegeni MSA değerleri, köşegen dışı kısmi korelasyonların negatifidir (SPSS).
 * @param {number[][]|{veri:number[][]}|{korelasyon:number[][], n:number}} girdi
 * @returns {{yontem:'kmo-bartlett', ad:string, kmo:number|null, kmoYorum:string|null, msa:number[]|null,
 *   antiImajKorelasyon:number[][]|null, antiImajKovaryans:number[][]|null, determinant:number, logDeterminant:number,
 *   istatistik:number|null, istatistikAdi:'χ²', sd:number, p:number|null, n:number|null, degiskenSayisi:number, uyarilar:string[]}}
 */
export function kmoBartlett(girdi) {
  const { R, n, p } = girdiyiCoz(girdi);
  const uyarilar = [];
  const { degerler } = ozdegerAyristir(R, false);
  const tekil = degerler[p - 1] <= TEKIL_ESIK;
  const sd = (p * (p - 1)) / 2;
  const sonuc = {
    yontem: 'kmo-bartlett',
    ad: 'KMO ve Bartlett küresellik testi',
    kmo: null,
    kmoYorum: null,
    msa: null,
    antiImajKorelasyon: null,
    antiImajKovaryans: null,
    determinant: 0,
    logDeterminant: -Infinity,
    istatistik: null,
    istatistikAdi: 'χ²',
    sd,
    p: null,
    n,
    degiskenSayisi: p,
    uyarilar,
  };
  if (tekil) {
    uyarilar.push('Korelasyon matrisi pozitif tanımlı değil (tekil): KMO ve Bartlett testi hesaplanamaz. Birbirinin doğrusal bileşimi olan değişkenleri ya da değişken sayısından az gözlemi kontrol edin.');
    return sonuc;
  }
  let logDet = 0;
  for (const v of degerler) logDet += Math.log(v);
  sonuc.logDeterminant = logDet;
  sonuc.determinant = Math.exp(logDet);
  if (sonuc.determinant < 1e-5) uyarilar.push('Korelasyon matrisinin determinantı 0,00001\'den küçük: değişkenler arasında çoklu bağlantı olabilir.');
  const Ri = simetrikTers(R);
  const aic = sifir(p, p);
  const aiv = sifir(p, p);
  let r2 = 0;
  let q2 = 0;
  const r2i = new Array(p).fill(0);
  const q2i = new Array(p).fill(0);
  for (let i = 0; i < p; i++) {
    for (let j = 0; j < p; j++) {
      aiv[i][j] = Ri[i][j] / (Ri[i][i] * Ri[j][j]);
      if (i === j) continue;
      const q = Ri[i][j] / Math.sqrt(Ri[i][i] * Ri[j][j]);
      aic[i][j] = q;
      r2i[i] += R[i][j] ** 2;
      q2i[i] += q * q;
    }
    r2 += r2i[i];
    q2 += q2i[i];
  }
  const msa = r2i.map((v, i) => v / (v + q2i[i]));
  for (let i = 0; i < p; i++) aic[i][i] = msa[i];
  sonuc.kmo = r2 / (r2 + q2);
  sonuc.kmoYorum = kaiserYorumu(sonuc.kmo);
  sonuc.msa = msa;
  sonuc.antiImajKorelasyon = aic;
  sonuc.antiImajKovaryans = aiv;
  const dusuk = msa.map((v, i) => (v < 0.5 ? i + 1 : 0)).filter(Boolean);
  if (sonuc.kmo < 0.5) uyarilar.push('KMO değeri 0,50\'nin altında: veri faktör analizi için uygun değil.');
  if (dusuk.length) uyarilar.push(`Örneklem yeterliliği (MSA) 0,50'nin altında olan değişkenler: ${dusuk.join(', ')}. Bu değişkenleri çıkarmayı düşünün.`);
  if (n === null) {
    uyarilar.push('Örneklem büyüklüğü verilmediği için Bartlett testi hesaplanmadı.');
  } else {
    const chi = -(n - 1 - (2 * p + 5) / 6) * logDet;
    sonuc.istatistik = chi;
    sonuc.p = pchisq(chi, sd, false);
  }
  return sonuc;
}

// ---------------------------------------------------------------------------------------------
// Çıkarım yöntemleri
// ---------------------------------------------------------------------------------------------

function temelBilesenler(ozd, m) {
  const p = ozd.degerler.length;
  for (let j = 0; j < m; j++) {
    if (!(ozd.degerler[j] > TEKIL_ESIK)) throw new IstatistikHatasi(`${j + 1}. bileşenin özdeğeri sıfır; ${m} bileşen çıkarılamaz.`);
  }
  const A = sifir(p, m);
  for (let j = 0; j < m; j++) {
    const k = Math.sqrt(ozd.degerler[j]);
    for (let i = 0; i < p; i++) A[i][j] = ozd.vektorler[i][j] * k;
  }
  return A;
}

/**
 * SPSS temel eksen faktörlemesi (PAF): köşegen SMC ile başlar; her iterasyonda indirgenmiş matrisin ilk m
 * özvektöründen yükler, yüklerden yeni ortak varyanslar hesaplanır; en büyük mutlak değişim ölçütün altına
 * inince ya da iterasyon sınırına ulaşılınca durulur (SPSS ECONVERGE = 0,001, ITERATE = 25).
 */
function anaEksen(R, m, h0, maksIterasyon, olcut) {
  const p = R.length;
  const Rs = R.map((s) => s.slice());
  let h = h0.slice();
  let A = null;
  let fark = Infinity;
  let it = 0;
  let yakinsadi = false;
  let heywood = false;
  let indirgenmis = null;
  while (it < maksIterasyon) {
    it++;
    for (let i = 0; i < p; i++) Rs[i][i] = h[i];
    const ozd = ozdegerAyristir(Rs);
    for (let j = 0; j < m; j++) {
      if (!(ozd.degerler[j] > 0)) {
        throw new IstatistikHatasi(`Temel eksen iterasyonunda ${j + 1}. özdeğer sıfır ya da negatif oldu; ${m} faktör çıkarılamıyor. Faktör sayısını azaltın.`);
      }
    }
    A = sifir(p, m);
    for (let j = 0; j < m; j++) {
      const k = Math.sqrt(ozd.degerler[j]);
      for (let i = 0; i < p; i++) A[i][j] = ozd.vektorler[i][j] * k;
    }
    indirgenmis = ozd.degerler;
    const yeni = satirKareToplami(A);
    fark = 0;
    for (let i = 0; i < p; i++) fark = Math.max(fark, Math.abs(yeni[i] - h[i]));
    h = yeni;
    if (h.some((v) => v > 1)) heywood = true;
    if (fark < olcut) {
      yakinsadi = true;
      break;
    }
  }
  return { A, h, iterasyon: it, yakinsadi, sonFark: fark, heywood, indirgenmisOzdegerler: indirgenmis };
}

/** ML amaç fonksiyonu (yoğunlaştırılmış, factanal FAfn/FAgr ile aynı) ve tekliklere göre gradyanı */
function mlDegerlendir(R, psi, m) {
  const p = R.length;
  const sc = psi.map((v) => 1 / Math.sqrt(v));
  const S = R.map((s, i) => s.map((v, j) => v * sc[i] * sc[j]));
  const { degerler: e, vektorler: V } = ozdegerAyristir(S);
  let f = -(p - m);
  for (let j = m; j < p; j++) {
    if (!(e[j] > 0)) return { f: Infinity };
    f += e[j] - Math.log(e[j]);
  }
  const L = sifir(p, m);
  for (let j = 0; j < m; j++) {
    const k = Math.sqrt(Math.max(e[j] - 1, 0));
    for (let i = 0; i < p; i++) L[i][j] = Math.sqrt(psi[i]) * V[i][j] * k;
  }
  const g = psi.map((ps, i) => {
    let t = 0;
    for (let j = 0; j < m; j++) t += L[i][j] ** 2;
    return (t + ps - R[i][i]) / (ps * ps);
  });
  return { f, g, L, e };
}

/**
 * En çok olabilirlik faktör çıkarımı. Teklikler (ψ) log ölçeğinde, [0,005; 1] sınırları içinde, izdüşümlü
 * BFGS ile tam yakınsamaya kadar en iyilenir (stats::factanal ile aynı amaç fonksiyonu ve sınırlar).
 */
function enCokOlabilirlik(R, m, maksIterasyon) {
  const p = R.length;
  const Ri = simetrikTers(R);
  const alt = Math.log(ML_ALT_SINIR);
  const ust = 0;
  const sinirla = (x) => Math.min(ust, Math.max(alt, x));
  let x = R.map((_, i) => sinirla(Math.log((1 - (0.5 * m) / p) / Ri[i][i])));
  const degerlendir = (th) => {
    const psi = th.map(Math.exp);
    const d = mlDegerlendir(R, psi, m);
    if (Number.isFinite(d.f)) d.gt = d.g.map((v, i) => v * psi[i]);
    return d;
  };
  let d = degerlendir(x);
  let H = birim(p);
  let ilkGuncelleme = true;
  let aktifOnceki = null;
  let yakinsadi = false;
  let it = 0;
  const TOL = 1e-11;
  for (; it < maksIterasyon; it++) {
    const aktif = x.map((v, i) => (v <= alt && d.gt[i] > 0) || (v >= ust && d.gt[i] < 0));
    let pgMaks = 0;
    for (let i = 0; i < p; i++) if (!aktif[i]) pgMaks = Math.max(pgMaks, Math.abs(d.gt[i]));
    if (pgMaks < TOL) {
      yakinsadi = true;
      break;
    }
    if (aktifOnceki && aktif.some((a, i) => a !== aktifOnceki[i])) {
      H = birim(p);
      ilkGuncelleme = true;
    }
    aktifOnceki = aktif;
    const gs = d.gt.map((v, i) => (aktif[i] ? 0 : v));
    let yon = H.map((s, i) => (aktif[i] ? 0 : -s.reduce((t, h, j) => t + h * gs[j], 0)));
    let egim = yon.reduce((t, v, i) => t + v * gs[i], 0);
    if (!(egim < 0)) {
      H = birim(p);
      ilkGuncelleme = true;
      yon = gs.map((v) => -v);
      egim = -gs.reduce((t, v) => t + v * v, 0);
    }
    let t = 1;
    let yeniX = null;
    let yeniD = null;
    for (let k = 0; k < 60; k++) {
      const aday = x.map((v, i) => sinirla(v + t * yon[i]));
      const dd = degerlendir(aday);
      let beklenen = 0;
      for (let i = 0; i < p; i++) beklenen += d.gt[i] * (aday[i] - x[i]);
      if (Number.isFinite(dd.f) && dd.f <= d.f + 1e-4 * beklenen) {
        yeniX = aday;
        yeniD = dd;
        break;
      }
      t /= 2;
    }
    if (!yeniX) {
      // Satır araması ilerleyemiyor: amaç fonksiyonu makine duyarlılığında en küçük.
      yakinsadi = pgMaks < 1e-7;
      break;
    }
    const s = yeniX.map((v, i) => (aktif[i] ? 0 : v - x[i]));
    const y = yeniD.gt.map((v, i) => (aktif[i] ? 0 : v - d.gt[i]));
    const sy = s.reduce((a, v, i) => a + v * y[i], 0);
    if (sy > 1e-300) {
      if (ilkGuncelleme) {
        const yy = y.reduce((a, v) => a + v * v, 0);
        const olcek = sy / yy;
        H = H.map((sat, i) => sat.map((_, j) => (i === j ? olcek : 0)));
        ilkGuncelleme = false;
      }
      const rho = 1 / sy;
      const Hy = H.map((sat) => sat.reduce((a, h, j) => a + h * y[j], 0));
      const yHy = y.reduce((a, v, i) => a + v * Hy[i], 0);
      for (let i = 0; i < p; i++) {
        for (let j = 0; j < p; j++) {
          H[i][j] += (1 + rho * yHy) * rho * s[i] * s[j] - rho * (Hy[i] * s[j] + s[i] * Hy[j]);
        }
      }
    }
    const fOnceki = d.f;
    x = yeniX;
    d = yeniD;
    if (Math.abs(fOnceki - d.f) <= 1e-16 * Math.max(1, Math.abs(d.f)) && s.every((v) => Math.abs(v) < 1e-13)) {
      yakinsadi = true;
      break;
    }
  }
  const psi = x.map(Math.exp);
  return { A: d.L, psi, f: d.f, iterasyon: it, yakinsadi, heywood: psi.some((v) => v <= ML_ALT_SINIR * (1 + 1e-6)) };
}

// ---------------------------------------------------------------------------------------------
// Döndürmeler (gradyan izdüşümü algoritması; Jennrich 2001, 2002 — GPArotation::GPForth / GPFoblq)
// ---------------------------------------------------------------------------------------------

/** Orthomax ailesi ölçütü: γ = 0 quartimax, 1 varimax, m/2 equamax. f en küçüklenir. */
function orthomaxKriteri(gamma) {
  return (L) => {
    const p = L.length;
    const c = sutunKareToplami(L);
    let f = 0;
    const Gq = L.map((s) =>
      s.map((v, j) => {
        f -= v ** 4;
        return -(v ** 3) + (gamma / p) * v * c[j];
      }),
    );
    for (const cj of c) f += (gamma / p) * cj * cj;
    return { f: f / 4, Gq };
  };
}

/** Doğrudan oblimin ölçütü (Jennrich-Sampson); delta = 0 quartimin. GPArotation::oblimin(gam = delta) ile aynı. */
function obliminKriteri(delta) {
  return (L) => {
    const p = L.length;
    const k = L[0].length;
    const L2 = L.map((s) => s.map((v) => v * v));
    const satirTop = L2.map((s) => s.reduce((a, v) => a + v, 0));
    let X = L2.map((s, i) => s.map((v) => satirTop[i] - v)); // L² (11' − I)
    if (delta !== 0) {
      const ort = new Array(k).fill(0);
      for (const s of X) for (let j = 0; j < k; j++) ort[j] += s[j] / p;
      X = X.map((s) => s.map((v, j) => v - delta * ort[j]));
    }
    let f = 0;
    const Gq = L.map((s, i) =>
      s.map((v, j) => {
        f += L2[i][j] * X[i][j];
        return v * X[i][j];
      }),
    );
    return { f: f / 4, Gq };
  };
}

// Gradyan izdüşümü (GPA) adımları. GPArotation'daki Armijo koşulu ölçüt değerlerini karşılaştırır; gradyan normu
// ~1e-8'e inince ölçütteki iyileşme yuvarlama hatasının altında kalır ve algoritma ilerleyemez. Bu bölgede
// (ölçüt farkı yuvarlama düzeyindeyken) adım, izdüşümlü gradyan normunu küçültüyorsa kabul edilir; böylece
// çözüm makine duyarlılığına kadar yakınsar. En iyi nokta ve kullanılan ölçüt GPArotation ile aynıdır.
function yuvarlamaBolgesi(fYeni, fEski) {
  return Math.abs(fYeni - fEski) <= 1e-12 * Math.max(1, Math.abs(fEski));
}

function gpaDik(A, kriter, eps, maksIt) {
  const k = A[0].length;
  const At = transpoz(A);
  const izdusum = (T, G) => {
    const M = carp(transpoz(T), G);
    const S = M.map((r, i) => r.map((x, j) => (x + M[j][i]) / 2));
    const TS = carp(T, S);
    return G.map((r, i) => r.map((x, j) => x - TS[i][j]));
  };
  let T = birim(k);
  let v = kriter(A);
  let G = carp(At, v.Gq);
  let Gp = izdusum(T, G);
  let s = frobenius(Gp);
  let al = 1;
  let it = 0;
  for (; it <= maksIt; it++) {
    if (s < eps) break;
    al *= 2;
    let kabul = null;
    for (let i = 0; i <= 40; i++) {
      const Tt = kutupsal(T.map((r, a) => r.map((x, b) => x - al * Gp[a][b])));
      const vt = kriter(carp(A, Tt));
      if (yuvarlamaBolgesi(vt.f, v.f)) {
        const Gt = carp(At, vt.Gq);
        const Gpt = izdusum(Tt, Gt);
        const st = frobenius(Gpt);
        if (st < s) kabul = { Tt, vt, Gt, Gpt, st };
      } else if (vt.f < v.f - 0.5 * s * s * al) {
        kabul = { Tt, vt };
      }
      if (kabul) break;
      al /= 2;
    }
    if (!kabul) break;
    T = kabul.Tt;
    v = kabul.vt;
    G = kabul.Gt ?? carp(At, v.Gq);
    Gp = kabul.Gpt ?? izdusum(T, G);
    s = kabul.st ?? frobenius(Gp);
  }
  return { L: carp(A, T), T, iterasyon: it, yakinsadi: s < eps, gradyanNormu: s };
}

function gpaEgik(A, kriter, eps, maksIt) {
  const k = A[0].length;
  const oruntu = (Tm) => carp(A, transpoz(ters(Tm)));
  const gradyan = (Lm, Gq, Tm) => transpoz(carp(carp(transpoz(Lm), Gq), ters(Tm))).map((r) => r.map((x) => -x));
  const izdusum = (T, G) => {
    const kd = new Array(k).fill(0);
    for (let i = 0; i < k; i++) for (let j = 0; j < k; j++) kd[j] += T[i][j] * G[i][j];
    return G.map((r, i) => r.map((x, j) => x - T[i][j] * kd[j]));
  };
  let T = birim(k);
  let L = oruntu(T);
  let v = kriter(L);
  let G = gradyan(L, v.Gq, T);
  let Gp = izdusum(T, G);
  let s = frobenius(Gp);
  let al = 1;
  let it = 0;
  for (; it <= maksIt; it++) {
    if (s < eps) break;
    al *= 2;
    let kabul = null;
    for (let i = 0; i <= 40; i++) {
      const X = T.map((r, a) => r.map((x, b) => x - al * Gp[a][b]));
      const norm = new Array(k).fill(0);
      for (const r of X) for (let j = 0; j < k; j++) norm[j] += r[j] * r[j];
      const Tt = X.map((r) => r.map((x, j) => x / Math.sqrt(norm[j])));
      const Lt = oruntu(Tt);
      const vt = kriter(Lt);
      if (yuvarlamaBolgesi(vt.f, v.f)) {
        const Gt = gradyan(Lt, vt.Gq, Tt);
        const Gpt = izdusum(Tt, Gt);
        const st = frobenius(Gpt);
        if (st < s) kabul = { Tt, Lt, vt, Gt, Gpt, st };
      } else if (v.f - vt.f > 0.5 * s * s * al) {
        kabul = { Tt, Lt, vt };
      }
      if (kabul) break;
      al /= 2;
    }
    if (!kabul) break;
    T = kabul.Tt;
    L = kabul.Lt;
    v = kabul.vt;
    G = kabul.Gt ?? gradyan(L, v.Gq, T);
    Gp = kabul.Gpt ?? izdusum(T, G);
    s = kabul.st ?? frobenius(Gp);
  }
  return { L, T, Phi: carp(transpoz(T), T), iterasyon: it, yakinsadi: s < eps, gradyanNormu: s };
}

/**
 * Promax (Hendrickson ve White 1964). 'spss': Kaiser normalizasyonu tüm işleme uygulanır (hedef matris ve en küçük
 * kareler normalize yüklerden). 'r': stats::promax / psych::Promax gibi; varimax normalize, hedef ve en küçük
 * kareler normalize edilmemiş yüklerden.
 */
function promax(A, kappa, kaiser, tur, eps, maksIt) {
  const p = A.length;
  const h = kaiser ? satirKareToplami(A).map(Math.sqrt) : new Array(p).fill(1);
  const An = A.map((s, i) => s.map((v) => v / h[i]));
  const vari = gpaDik(An, orthomaxKriteri(1), eps, maksIt);
  // tur 'spss': hedef ve regresyon normalize yüklerle; 'r': normalize edilmemiş varimax yükleriyle
  const V = tur === 'spss' ? vari.L : vari.L.map((s, i) => s.map((v) => v * h[i]));
  const Q = V.map((s) => s.map((v) => v * Math.abs(v) ** (kappa - 1)));
  const Vt = transpoz(V);
  let U = carp(simetrikTers(carp(Vt, V)), carp(Vt, Q));
  const UtUi = ters(carp(transpoz(U), U));
  const olcek = UtUi.map((s, i) => Math.sqrt(s[i]));
  U = U.map((s) => s.map((v, j) => v * olcek[j]));
  let P = carp(V, U);
  if (tur === 'spss') P = P.map((s, i) => s.map((v) => v * h[i]));
  const M = carp(vari.T, U); // P = A M
  const Phi = ters(carp(transpoz(U), U));
  return { P, M, Phi, iterasyon: vari.iterasyon, yakinsadi: vari.yakinsadi };
}

const DONDURME_ADLARI = {
  yok: 'Döndürme yok',
  varimax: 'Varimax',
  quartimax: 'Quartimax',
  equamax: 'Equamax',
  promax: 'Promax',
  oblimin: 'Doğrudan oblimin',
};

/**
 * Yük matrisini döndürür. Dönüş: yukler (örüntü), faktorKorelasyonlari (Φ), donusumMatrisi (M: yukler = A·M).
 */
function dondur(A, yontem, s) {
  const p = A.length;
  const m = A[0].length;
  const eps = s.donmeYakinsama;
  const maksIt = s.donmeMaksIterasyon;
  const kaiser = s.kaiserNormalizasyonu;
  if (yontem === 'promax') {
    const r = promax(A, s.promaxKappa, kaiser, s.promaxTuru, eps, maksIt);
    return { P: r.P, M: r.M, Phi: r.Phi, egik: true, iterasyon: r.iterasyon, yakinsadi: r.yakinsadi };
  }
  const h = kaiser ? satirKareToplami(A).map(Math.sqrt) : new Array(p).fill(1);
  if (h.some((v) => !(v > 0))) throw new IstatistikHatasi('Ortak varyansı sıfır olan değişken var; Kaiser normalizasyonu uygulanamaz.');
  const An = A.map((r, i) => r.map((v) => v / h[i]));
  if (yontem === 'oblimin') {
    const r = gpaEgik(An, obliminKriteri(s.obliminDelta), eps, maksIt);
    const P = r.L.map((row, i) => row.map((v) => v * h[i]));
    return { P, M: transpoz(ters(r.T)), Phi: r.Phi, egik: true, iterasyon: r.iterasyon, yakinsadi: r.yakinsadi };
  }
  const gamma = { varimax: 1, quartimax: 0, equamax: m / 2 }[yontem];
  const r = gpaDik(An, orthomaxKriteri(gamma), eps, maksIt);
  const P = r.L.map((row, i) => row.map((v) => v * h[i]));
  return { P, M: r.T, Phi: birim(m), egik: false, iterasyon: r.iterasyon, yakinsadi: r.yakinsadi };
}

// ---------------------------------------------------------------------------------------------
// Rastgele sayılar (paralel analiz için tohumlu, tekrarlanabilir)
// ---------------------------------------------------------------------------------------------

/** xoshiro128** üreteci; tohum splitmix32 ile genişletilir. [0,1) aralığında 53 bitlik ondalık döndürür. */
function rastgeleUretec(tohum) {
  let t = (Number.isFinite(tohum) ? Math.floor(tohum) : 0) >>> 0;
  const sm = () => {
    t = (t + 0x9e3779b9) >>> 0;
    let z = t;
    z = Math.imul(z ^ (z >>> 16), 0x85ebca6b) >>> 0;
    z = Math.imul(z ^ (z >>> 13), 0xc2b2ae35) >>> 0;
    return (z ^ (z >>> 16)) >>> 0;
  };
  let a = sm();
  let b = sm();
  let c = sm();
  let d = sm();
  if ((a | b | c | d) === 0) a = 1;
  const sonraki = () => {
    const sonuc = Math.imul(((Math.imul(b, 5) << 7) | (Math.imul(b, 5) >>> 25)) >>> 0, 9) >>> 0;
    const tt = (b << 9) >>> 0;
    c ^= a;
    d ^= b;
    b ^= c;
    a ^= d;
    c ^= tt;
    d = ((d << 11) | (d >>> 21)) >>> 0;
    a >>>= 0;
    b >>>= 0;
    c >>>= 0;
    return sonuc;
  };
  const tekduze = () => ((sonraki() >>> 5) * 67108864 + (sonraki() >>> 6) + 0.5) / 9007199254740992;
  let yedek = null;
  const normal = () => {
    if (yedek !== null) {
      const v = yedek;
      yedek = null;
      return v;
    }
    const r = Math.sqrt(-2 * Math.log(tekduze()));
    const th = 2 * Math.PI * tekduze();
    yedek = r * Math.sin(th);
    return r * Math.cos(th);
  };
  // Gamma(a, 1): Marsaglia-Tsang (a < 1 için a+1 üzerinden)
  const gamma = (sekil) => {
    if (sekil < 1) return gamma(sekil + 1) * tekduze() ** (1 / sekil);
    const dd = sekil - 1 / 3;
    const cc = 1 / Math.sqrt(9 * dd);
    for (;;) {
      let x;
      let v;
      do {
        x = normal();
        v = 1 + cc * x;
      } while (v <= 0);
      v = v * v * v;
      const u = tekduze();
      if (Math.log(u) < 0.5 * x * x + dd - dd * v + dd * Math.log(v)) return dd * v;
    }
  };
  return { tekduze, normal, gamma };
}

/** Bağımsız standart normal n × p verinin korelasyon matrisi. Wishart (Bartlett ayrıştırması) ya da doğrudan veri üretimiyle. */
function rastgeleKorelasyon(rng, n, p, wishart) {
  let W;
  if (wishart) {
    // (n−1)S ~ Wishart(n−1, I) = T T', T alt üçgen: T_ii² ~ χ²(n−i), T_ij ~ N(0,1) (Bartlett 1933)
    const T = sifir(p, p);
    for (let i = 0; i < p; i++) {
      T[i][i] = Math.sqrt(2 * rng.gamma((n - 1 - i) / 2));
      for (let j = 0; j < i; j++) T[i][j] = rng.normal();
    }
    W = sifir(p, p);
    for (let i = 0; i < p; i++) {
      for (let j = 0; j <= i; j++) {
        let t = 0;
        for (let k = 0; k <= j; k++) t += T[i][k] * T[j][k];
        W[i][j] = t;
        W[j][i] = t;
      }
    }
  } else {
    const X = Array.from({ length: n }, () => Array.from({ length: p }, () => rng.normal()));
    W = korelasyonMatrisi(X);
  }
  const d = W.map((s, i) => 1 / Math.sqrt(s[i]));
  return W.map((s, i) => s.map((v, j) => (i === j ? 1 : v * d[i] * d[j])));
}

// ---------------------------------------------------------------------------------------------
// Paralel analiz ve MAP testi
// ---------------------------------------------------------------------------------------------

/**
 * Horn paralel analizi. Gerçek korelasyon matrisinin özdeğerleri, aynı n ve p ile üretilen bağımsız normal
 * verilerin özdeğerlerinin ortalaması ve yüzdeliğiyle karşılaştırılır; gerçek özdeğeri rastgele yüzdeliği aşan
 * ardışık ilk faktörlerin sayısı önerilir. Tohumlu üreteç sayesinde aynı tohumla sonuç aynıdır.
 * Rastgele korelasyon matrisleri varsayılan olarak Wishart dağılımından (Bartlett ayrıştırması) üretilir; bu,
 * n × p normal veri üretip korelasyon hesaplamakla dağılımsal olarak özdeştir ve n'den bağımsız hızdadır.
 * @param {number[][]|{veri:number[][]}|{korelasyon:number[][], n:number}} girdi
 * @param {{tekrar?:number, yuzdelik?:number, tohum?:number, tur?:'tbt'|'ana-eksen', uretim?:'wishart'|'veri'}} [secenek]
 *   tur 'ana-eksen': köşegeni SMC olan indirgenmiş matrisin özdeğerleri (O'Connor 2000 PAF seçeneği).
 *   Yüzdelik SPSS yöntemiyle (tip 6) hesaplanır.
 * @returns {{yontem:'paralel-analiz', ad:string, tur:string, gercek:number[], ortalama:number[], yuzdelikDegerleri:number[],
 *   standartSapma:number[], onerilenFaktorSayisi:number, onerilenFaktorSayisiOrtalama:number, tekrar:number,
 *   yuzdelik:number, tohum:number, n:number, degiskenSayisi:number, satirlar:object[], uyarilar:string[]}}
 */
export function paralelAnaliz(girdi, secenek = {}) {
  const { R, n, p } = girdiyiCoz(girdi);
  gerekli(n !== null, 'Paralel analiz için örneklem büyüklüğü (n) gerekir.');
  const tekrar = secenek.tekrar ?? 1000;
  const yuzdelik = secenek.yuzdelik ?? 95;
  const tohum = secenek.tohum ?? 20240601;
  const tur = secenek.tur ?? 'tbt';
  gerekli(Number.isInteger(tekrar) && tekrar >= 10, 'Paralel analiz için en az 10 tekrar gerekir.');
  gerekli(yuzdelik > 0 && yuzdelik < 100, 'Yüzdelik 0 ile 100 arasında olmalıdır.');
  gerekli(tur === 'tbt' || tur === 'ana-eksen', "Paralel analiz türü 'tbt' ya da 'ana-eksen' olmalıdır.");
  const uyarilar = [];
  const wishart = (secenek.uretim ?? 'wishart') === 'wishart' && n - 1 >= p;
  if (tur === 'ana-eksen') gerekli(n > p, 'Temel eksen paralel analizi için gözlem sayısı değişken sayısından büyük olmalıdır.');
  const ozdegerler = (K) => {
    if (tur === 'ana-eksen') {
      const Ri = simetrikTers(K);
      K = K.map((s, i) => s.map((v, j) => (i === j ? 1 - 1 / Ri[i][i] : v)));
    }
    return ozdegerAyristir(K, false).degerler;
  };
  const gercek = ozdegerler(R);
  const rng = rastgeleUretec(tohum);
  const sim = Array.from({ length: p }, () => new Float64Array(tekrar));
  for (let r = 0; r < tekrar; r++) {
    const e = ozdegerler(rastgeleKorelasyon(rng, n, p, wishart));
    for (let j = 0; j < p; j++) sim[j][r] = e[j];
  }
  const ortalama = sim.map((x) => x.reduce((a, v) => a + v, 0) / tekrar);
  const standartSapma = sim.map((x, j) => Math.sqrt(x.reduce((a, v) => a + (v - ortalama[j]) ** 2, 0) / (tekrar - 1)));
  const yuzdelikDegerleri = sim.map((x) => yuzdelikHesapla(Array.from(x), yuzdelik / 100, 6));
  const say = (esik) => {
    let k = 0;
    while (k < p && gercek[k] > esik[k]) k++;
    return k;
  };
  const satirlar = gercek.map((g, j) => ({ faktor: j + 1, gercek: g, ortalama: ortalama[j], yuzdelik: yuzdelikDegerleri[j], tut: false }));
  const oneri = say(yuzdelikDegerleri);
  for (let j = 0; j < oneri; j++) satirlar[j].tut = true;
  return {
    yontem: 'paralel-analiz',
    ad: 'Paralel analiz (Horn)',
    tur,
    gercek,
    ortalama,
    standartSapma,
    yuzdelikDegerleri,
    onerilenFaktorSayisi: oneri,
    onerilenFaktorSayisiOrtalama: say(ortalama),
    tekrar,
    yuzdelik,
    tohum,
    uretim: wishart ? 'wishart' : 'veri',
    n,
    degiskenSayisi: p,
    satirlar,
    uyarilar,
  };
}

/**
 * Velicer'in MAP (minimum average partial) testi. m = 0, 1, …, p − 1 temel bileşen kısmi olarak çıkarıldıktan
 * sonra kalan kısmi korelasyonların karelerinin (Velicer 1976) ve dördüncü kuvvetlerinin (Velicer ve ark. 2000)
 * ortalaması; en küçük ortalamayı veren m önerilir. m ≥ 1 için kare ortalamalar psych::VSS()$map ile aynıdır.
 * @param {number[][]|{veri:number[][]}|{korelasyon:number[][], n?:number}} girdi
 * @param {{maksFaktor?:number}} [secenek]
 * @returns {{yontem:'map', ad:string, kareOrtalamalari:number[], dorduncuKuvvetOrtalamalari:number[],
 *   onerilenFaktorSayisi:number, onerilenFaktorSayisi4:number, satirlar:object[], uyarilar:string[]}}
 *   Dizilerin k. öğesi k bileşen çıkarıldıktan sonraki değerdir (k = 0: ham korelasyonlar).
 */
export function mapTesti(girdi, secenek = {}) {
  const { R, p } = girdiyiCoz(girdi);
  const maks = Math.min(secenek.maksFaktor ?? p - 1, p - 1);
  const { degerler, vektorler } = ozdegerAyristir(R);
  const kare = [];
  const dort = [];
  const C = R.map((s) => s.slice());
  for (let m = 0; m <= maks; m++) {
    if (m > 0) {
      const l = degerler[m - 1];
      for (let i = 0; i < p; i++) for (let j = 0; j < p; j++) C[i][j] -= l * vektorler[i][m - 1] * vektorler[j][m - 1];
    }
    let t2 = 0;
    let t4 = 0;
    let gecersiz = false;
    for (let i = 0; i < p; i++) {
      for (let j = 0; j < p; j++) {
        if (i === j) continue;
        const pay = C[i][i] * C[j][j];
        if (!(pay > 0)) gecersiz = true;
        const r2 = (C[i][j] * C[i][j]) / pay;
        t2 += r2;
        t4 += r2 * r2;
      }
    }
    if (gecersiz) break;
    kare.push(t2 / (p * (p - 1)));
    dort.push(t4 / (p * (p - 1)));
  }
  const enKucuk = (x) => x.reduce((b, v, i) => (v < x[b] ? i : b), 0);
  return {
    yontem: 'map',
    ad: 'Velicer MAP testi',
    kareOrtalamalari: kare,
    dorduncuKuvvetOrtalamalari: dort,
    onerilenFaktorSayisi: enKucuk(kare),
    onerilenFaktorSayisi4: enKucuk(dort),
    satirlar: kare.map((v, m) => ({ faktor: m, kare: v, dorduncuKuvvet: dort[m] })),
    uyarilar: [],
  };
}

// ---------------------------------------------------------------------------------------------
// Faktör analizi
// ---------------------------------------------------------------------------------------------

const CIKARIM_ADLARI = {
  tbt: 'Temel bileşenler analizi',
  'ana-eksen': 'Temel eksen faktörlemesi',
  ml: 'En çok olabilirlik',
};

/**
 * Açımlayıcı faktör analizi (SPSS FACTOR ile uyumlu).
 *
 * Çıkarım: 'tbt' temel bileşenler (SPSS PC; psych::principal), 'ana-eksen' yinelemeli temel eksen (SPSS PAF:
 * SMC başlangıcı, en büyük ortak varyans değişimi < yakinsamaOlcutu ya da maksIterasyon), 'ml' en çok olabilirlik
 * (tam yakınsama; stats::factanal ile aynı çözüm, Bartlett düzeltmeli χ² uyum testi).
 * Döndürme: dik 'varimax' | 'quartimax' | 'equamax' (orthomax γ = 1, 0, m/2), eğik 'promax' (kappa) |
 * 'oblimin' (doğrudan oblimin, delta). Kaiser normalizasyonu varsayılan olarak açıktır (SPSS). Döndürmeler
 * gradyan izdüşümü algoritmasıyla tam yakınsamaya kadar yürütülür (GPArotation ile aynı ölçüt); SPSS'in 25
 * döngülük sınırı yerine sonuç ölçütün gerçek en iyisidir.
 * Faktör sıralaması ve işaret: döndürülmüş faktörler SPSS gibi açıkladıkları kareler toplamına göre (eğikte yapı
 * matrisinden) büyükten küçüğe sıralanır; her faktörün yük toplamı pozitif olacak biçimde işaret çevrilir.
 *
 * @param {number[][]|{veri:number[][]}|{korelasyon:number[][], n:number}} girdi
 * @param {{cikarim?:'tbt'|'ana-eksen'|'ml', faktorSayisi?:number|'kaiser'|'paralel'|'map', ozdegerEsigi?:number,
 *   dondurme?:'yok'|'varimax'|'quartimax'|'equamax'|'promax'|'oblimin', kaiserNormalizasyonu?:boolean,
 *   maksIterasyon?:number, yakinsamaOlcutu?:number, promaxKappa?:number, promaxTuru?:'spss'|'r',
 *   obliminDelta?:number, donmeYakinsama?:number, donmeMaksIterasyon?:number, siralama?:boolean,
 *   paralel?:object, degiskenAdlari?:string[]}} [secenek]
 * @returns {object} SPSS tablolarına karşılık gelen alanlar:
 *   ozdegerler (R'nin özdeğerleri), toplamVaryans [{bilesen, ozdeger, varyansYuzdesi, birikimliYuzde}] ("Initial
 *   Eigenvalues"), cikarimKareToplamlari ve donmeKareToplamlari [{faktor, toplam, varyansYuzdesi, birikimliYuzde}]
 *   (eğik döndürmede yapı matrisinden, yüzdeler null), ortakliklar {baslangic, cikarim} ("Communalities"),
 *   yuklerDondurulmemis ("Factor/Component Matrix"), yukler (döndürülmüş / örüntü matrisi), yapiMatrisi,
 *   faktorKorelasyonlari (Φ), donusumMatrisi (yukler = yuklerDondurulmemis × donusumMatrisi; dikte "Factor
 *   Transformation Matrix"), teklikler (1 − h²), uretilenKorelasyon (ΛΛ'), artiklar {sayi, yuzde, esik: 0.05,
 *   toplamCift, rmsr} ("Reproduced Correlations" dipnotu), yakinsama {cikarim, dondurme}, uyum (yalnız ML:
 *   {istatistik, sd, p, amacFonksiyonu}), heywood, paralelAnaliz / map (faktör sayısı bunlarla seçildiyse), uyarilar.
 */
export function faktorAnalizi(girdi, secenek = {}) {
  const { R, n, p, veri } = girdiyiCoz(girdi);
  const s = {
    cikarim: 'tbt',
    faktorSayisi: 'kaiser',
    ozdegerEsigi: 1,
    dondurme: 'varimax',
    kaiserNormalizasyonu: true,
    maksIterasyon: 25,
    yakinsamaOlcutu: 0.001,
    promaxKappa: 4,
    promaxTuru: 'spss',
    obliminDelta: 0,
    donmeYakinsama: 1e-10,
    donmeMaksIterasyon: 10000,
    mlMaksIterasyon: 2000,
    siralama: true,
    ...secenek,
  };
  gerekli(CIKARIM_ADLARI[s.cikarim] !== undefined, "Çıkarım yöntemi 'tbt', 'ana-eksen' ya da 'ml' olmalıdır.");
  gerekli(DONDURME_ADLARI[s.dondurme] !== undefined, "Döndürme yöntemi 'yok', 'varimax', 'quartimax', 'equamax', 'promax' ya da 'oblimin' olmalıdır.");
  gerekli(Number.isInteger(s.maksIterasyon) && s.maksIterasyon >= 1, 'İterasyon sayısı pozitif tam sayı olmalıdır.');
  gerekli(s.promaxKappa > 1, 'Promax kappa değeri 1\'den büyük olmalıdır.');
  gerekli(s.obliminDelta <= 0.8, 'Oblimin delta değeri 0,8\'den büyük olamaz.');
  const uyarilar = [];
  const ozd = ozdegerAyristir(R);
  const tekil = ozd.degerler[p - 1] <= TEKIL_ESIK;
  if (tekil) uyarilar.push('Korelasyon matrisi pozitif tanımlı değil (tekil). Sonuçları dikkatle yorumlayın; doğrusal bağımlı değişkenleri ya da gözlem sayısını kontrol edin.');
  if (n !== null && n <= p) uyarilar.push(`Gözlem sayısı (${n}) değişken sayısından (${p}) fazla değil; faktör analizi için yetersiz.`);

  // Faktör sayısı
  let m;
  let paralel = null;
  let map = null;
  let faktorSayisiYontemi = 'sabit';
  if (typeof s.faktorSayisi === 'number') {
    m = s.faktorSayisi;
    gerekli(Number.isInteger(m) && m >= 1, 'Faktör sayısı pozitif tam sayı olmalıdır.');
  } else if (s.faktorSayisi === 'kaiser') {
    faktorSayisiYontemi = 'kaiser';
    m = Math.max(1, ozd.degerler.filter((v) => v > s.ozdegerEsigi).length);
  } else if (s.faktorSayisi === 'paralel') {
    faktorSayisiYontemi = 'paralel';
    paralel = paralelAnaliz(girdi, s.paralel ?? {});
    m = Math.max(1, paralel.onerilenFaktorSayisi);
  } else if (s.faktorSayisi === 'map') {
    faktorSayisiYontemi = 'map';
    map = mapTesti(girdi);
    m = Math.max(1, map.onerilenFaktorSayisi);
  } else {
    throw new IstatistikHatasi("Faktör sayısı bir tam sayı, 'kaiser', 'paralel' ya da 'map' olmalıdır.");
  }
  if (s.cikarim === 'tbt') gerekli(m <= p, `En fazla ${p} bileşen çıkarılabilir.`);
  else gerekli(m < p, `Bu yöntemle en fazla ${p - 1} faktör çıkarılabilir.`);

  // Çıkarım
  let A;
  let baslangic;
  let yakinsamaCikarim = null;
  let uyum = null;
  let heywood = false;
  if (s.cikarim === 'tbt') {
    A = temelBilesenler(ozd, m);
    baslangic = new Array(p).fill(1);
  } else if (s.cikarim === 'ana-eksen') {
    if (tekil) uyarilar.push('Korelasyon matrisi tekil olduğundan başlangıç ortak varyansları için SMC yerine en büyük mutlak korelasyon kullanıldı.');
    baslangic = kareCokluKorelasyon(R, tekil);
    const r = anaEksen(R, m, baslangic, s.maksIterasyon, s.yakinsamaOlcutu);
    A = r.A;
    heywood = r.heywood;
    yakinsamaCikarim = { iterasyon: r.iterasyon, yakinsadi: r.yakinsadi, sonDegisim: r.sonFark, olcut: s.yakinsamaOlcutu, maksIterasyon: s.maksIterasyon };
    if (!r.yakinsadi) {
      uyarilar.push(`${m} faktör çıkarılmaya çalışıldı; ${s.maksIterasyon} iterasyon yetmedi (son değişim ${r.sonFark.toPrecision(3)}). SPSS bu durumda çıkarımı sonlandırır; iterasyon sayısını artırın.`);
    }
    if (heywood) uyarilar.push('İterasyonlar sırasında 1\'den büyük ortak varyans tahmini oluştu (Heywood durumu). Çözümü dikkatle yorumlayın.');
  } else {
    if (tekil) throw new IstatistikHatasi('En çok olabilirlik çıkarımı için korelasyon matrisi pozitif tanımlı olmalıdır.');
    const sdUyum = ((p - m) ** 2 - (p + m)) / 2;
    gerekli(sdUyum >= 0, `${p} değişken için ${m} faktör fazla: en çok olabilirlik modelinin serbestlik derecesi negatif olur.`);
    baslangic = kareCokluKorelasyon(R, false);
    const r = enCokOlabilirlik(R, m, s.mlMaksIterasyon);
    A = r.A;
    heywood = r.heywood;
    yakinsamaCikarim = { iterasyon: r.iterasyon, yakinsadi: r.yakinsadi };
    if (!r.yakinsadi) uyarilar.push('En çok olabilirlik kestirimi yakınsamadı; sonuçları dikkatle yorumlayın.');
    if (heywood) uyarilar.push('Bazı değişkenlerin tekliği alt sınıra (0,005) indi (Heywood durumu): ortak varyans 1\'e ulaşıyor. Çözümü dikkatle yorumlayın.');
    let chi = null;
    let pDeger = null;
    if (n !== null) {
      chi = (n - 1 - (2 * p + 5) / 6 - (2 * m) / 3) * r.f;
      pDeger = sdUyum > 0 ? pchisq(chi, sdUyum, false) : null;
    } else {
      uyarilar.push('Örneklem büyüklüğü verilmediği için uyum iyiliği χ² testi hesaplanmadı.');
    }
    uyum = { istatistik: chi, istatistikAdi: 'χ²', sd: sdUyum, p: pDeger, amacFonksiyonu: r.f };
  }
  // Dönmemiş çözümün işaret düzeltmesi (sütun toplamı pozitif)
  const isaretA = sutunToplami(A).map((t) => (t < 0 ? -1 : 1));
  A = A.map((r) => r.map((v, j) => v * isaretA[j]));
  const h2 = satirKareToplami(A);
  if (s.cikarim !== 'ana-eksen' && h2.some((v) => v > 1 + 1e-8)) {
    heywood = true;
    uyarilar.push('1\'den büyük ortak varyans var (Heywood durumu).');
  }

  // Döndürme
  let P = A;
  let Phi = birim(m);
  let M = birim(m);
  let egik = false;
  let dondurme = s.dondurme;
  let yakinsamaDondurme = null;
  if (dondurme !== 'yok' && m < 2) {
    uyarilar.push('Yalnızca bir faktör çıkarıldığı için çözüm döndürülemez.');
    dondurme = 'yok';
  }
  if (dondurme !== 'yok') {
    const r = dondur(A, dondurme, s);
    P = r.P;
    Phi = r.Phi;
    M = r.M;
    egik = r.egik;
    yakinsamaDondurme = { iterasyon: r.iterasyon, yakinsadi: r.yakinsadi };
    if (!r.yakinsadi) uyarilar.push(`Döndürme ${s.donmeMaksIterasyon} iterasyonda yakınsamadı.`);
    // Sıralama ve işaret
    const S0 = egik ? carp(P, Phi) : P;
    const ss = sutunKareToplami(S0);
    const sira = Array.from({ length: m }, (_, j) => j);
    if (s.siralama) sira.sort((a, b) => ss[b] - ss[a]);
    const top = sutunToplami(P);
    const isaret = sira.map((j) => (top[j] < 0 ? -1 : 1));
    P = sutunlariDuzenle(P, sira, isaret);
    M = sutunlariDuzenle(M, sira, isaret);
    Phi = kareDuzenle(Phi, sira, isaret);
  }
  const S = egik ? carp(P, Phi) : P;

  // Tablolar
  const tablo = (deger) => {
    let birikimli = 0;
    return deger.map((v, j) => {
      const yuzde = (v / p) * 100;
      birikimli += yuzde;
      return { bilesen: j + 1, ozdeger: v, varyansYuzdesi: yuzde, birikimliYuzde: birikimli };
    });
  };
  const toplamVaryans = tablo(ozd.degerler);
  const cikarimKareToplamlari = tablo(sutunKareToplami(A)).map(({ bilesen, ozdeger, varyansYuzdesi, birikimliYuzde }) => ({ faktor: bilesen, toplam: ozdeger, varyansYuzdesi, birikimliYuzde }));
  let donmeKareToplamlari = null;
  if (dondurme !== 'yok') {
    donmeKareToplamlari = egik
      ? sutunKareToplami(S).map((v, j) => ({ faktor: j + 1, toplam: v, varyansYuzdesi: null, birikimliYuzde: null }))
      : tablo(sutunKareToplami(P)).map(({ bilesen, ozdeger, varyansYuzdesi, birikimliYuzde }) => ({ faktor: bilesen, toplam: ozdeger, varyansYuzdesi, birikimliYuzde }));
  }

  // Üretilen korelasyonlar ve artıklar
  const uretilen = carp(A, transpoz(A));
  let sayi = 0;
  let kareTop = 0;
  const cift = (p * (p - 1)) / 2;
  for (let i = 0; i < p; i++) {
    for (let j = i + 1; j < p; j++) {
      const r = R[i][j] - uretilen[i][j];
      kareTop += r * r;
      if (Math.abs(r) > 0.05) sayi++;
    }
  }
  const artiklar = { esik: 0.05, sayi, toplamCift: cift, yuzde: (sayi / cift) * 100, rmsr: Math.sqrt(kareTop / cift) };

  return {
    yontem: 'faktor-analizi',
    ad: 'Açımlayıcı faktör analizi',
    cikarim: s.cikarim,
    cikarimAdi: CIKARIM_ADLARI[s.cikarim],
    dondurme,
    dondurmeAdi: DONDURME_ADLARI[dondurme],
    egik,
    kaiserNormalizasyonu: s.kaiserNormalizasyonu,
    n,
    degiskenSayisi: p,
    degiskenAdlari: s.degiskenAdlari ?? null,
    faktorSayisi: m,
    faktorSayisiYontemi,
    korelasyon: R,
    ozdegerler: ozd.degerler,
    toplamVaryans,
    cikarimKareToplamlari,
    donmeKareToplamlari,
    ortakliklar: { baslangic, cikarim: h2 },
    teklikler: h2.map((v) => 1 - v),
    yuklerDondurulmemis: A,
    yukler: P,
    yapiMatrisi: S,
    faktorKorelasyonlari: Phi,
    donusumMatrisi: M,
    uretilenKorelasyon: uretilen,
    artiklar,
    yakinsama: { cikarim: yakinsamaCikarim, dondurme: yakinsamaDondurme },
    uyum,
    heywood,
    paralelAnaliz: paralel,
    map,
    uyarilar,
  };
}

// ---------------------------------------------------------------------------------------------
// Faktör puanları
// ---------------------------------------------------------------------------------------------

/**
 * Faktör puanları (SPSS FACTOR /SAVE). Veri, örneklem ortalaması ve (n − 1) paydalı standart sapmayla
 * standartlaştırılır; puan = Z·W.
 * 'regresyon' (Thurstone, SPSS varsayılanı): W = R⁻¹·S (S yapı matrisi) — psych::factor.scores(method = 'Thurstone').
 * 'bartlett': W = U⁻²Λ(Λ'U⁻²Λ)⁻¹ — psych 'Bartlett'. 'anderson-rubin': W = U⁻²Λ(Λ'U⁻²RU⁻²Λ)^(-1/2) — psych 'Anderson'.
 * (Λ örüntü matrisi, U² = 1 − ortak varyans.)
 * @param {number[][]} veri faktör analizinde kullanılan veri (n × p)
 * @param {object} sonuc faktorAnalizi() sonucu
 * @param {{yontem?:'regresyon'|'bartlett'|'anderson-rubin'}} [secenek]
 * @returns {{yontem:string, puanlar:number[][], katsayilar:number[][], uyarilar:string[]}}
 *   katsayilar: SPSS "Factor Score Coefficient Matrix" (p × m).
 */
export function faktorPuanlari(veri, sonuc, secenek = {}) {
  const yontem = secenek.yontem ?? 'regresyon';
  gerekli(['regresyon', 'bartlett', 'anderson-rubin'].includes(yontem), "Faktör puanı yöntemi 'regresyon', 'bartlett' ya da 'anderson-rubin' olmalıdır.");
  gerekli(sonuc && Array.isArray(sonuc.yukler), 'Faktör puanları için faktör analizi sonucu gerekir.');
  gerekli(Array.isArray(veri) && veri.length >= 2, 'Faktör puanları için ham veri gerekir (korelasyon matrisinden puan hesaplanamaz).');
  const p = sonuc.yukler.length;
  const m = sonuc.yukler[0].length;
  for (const r of veri) {
    gerekli(Array.isArray(r) && r.length === p, 'Verideki değişken sayısı faktör çözümüyle aynı olmalıdır.');
    for (const v of r) gerekli(Number.isFinite(v), 'Veride eksik ya da sayı olmayan değer var.');
  }
  const uyarilar = [];
  const R = sonuc.korelasyon;
  const P = sonuc.yukler;
  const Phi = sonuc.faktorKorelasyonlari;
  const S = sonuc.yapiMatrisi;
  let W;
  if (yontem === 'regresyon') {
    let Ri;
    try {
      const { degerler } = ozdegerAyristir(R, false);
      if (degerler[p - 1] <= TEKIL_ESIK) throw new IstatistikHatasi('tekil');
      Ri = simetrikTers(R);
      W = carp(Ri, S);
    } catch (e) {
      if (sonuc.cikarim !== 'tbt') throw new IstatistikHatasi('Korelasyon matrisi tekil olduğundan regresyon yöntemiyle faktör puanı hesaplanamaz.');
      // Temel bileşenlerde R⁻¹S = P(P'P)⁻¹ özdeşliği tekil R'de de geçerlidir
      W = carp(P, ters(carp(transpoz(P), P)));
    }
  } else {
    const h2 = carp(carp(P, Phi), transpoz(P)).map((r, i) => r[i]);
    const u2i = h2.map((v) => 1 - v);
    gerekli(u2i.every((v) => v > 1e-12), 'Tekliği sıfır olan değişken var; bu yöntemle faktör puanı hesaplanamaz.');
    const UP = P.map((r, i) => r.map((v) => v / u2i[i]));
    if (yontem === 'bartlett') {
      W = carp(UP, ters(carp(transpoz(P), UP)));
    } else {
      if (sonuc.egik) uyarilar.push('Anderson-Rubin yöntemi dik (ilişkisiz) faktörler için tasarlanmıştır; eğik çözümde puanlar yine ilişkisiz olur.');
      W = carp(UP, tersKarekok(carp(carp(transpoz(UP), R), UP)));
    }
  }
  const n = veri.length;
  const ort = new Array(p).fill(0);
  for (const r of veri) for (let j = 0; j < p; j++) ort[j] += r[j] / n;
  const ss = new Array(p).fill(0);
  for (const r of veri) for (let j = 0; j < p; j++) ss[j] += (r[j] - ort[j]) ** 2;
  const sd = ss.map((v) => Math.sqrt(v / (n - 1)));
  const puanlar = veri.map((r) => {
    const z = r.map((v, j) => (v - ort[j]) / sd[j]);
    return Array.from({ length: m }, (_, k) => z.reduce((t, zj, j) => t + zj * W[j][k], 0));
  });
  return { yontem, puanlar, katsayilar: W, uyarilar };
}
