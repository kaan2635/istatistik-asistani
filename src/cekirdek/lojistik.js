// Lojistik regresyon: ikili (binom, logit), tek değişkenli tarama; GLM için ortak IRLS çekirdeği (Poisson da kullanır).
// Sonuçlar SPSS LOGISTIC REGRESSION tablolarıyla aynı yapıdadır ve R glm(family = binomial) ile doğrulanmıştır.

import { enKucukKareler, sifir, coz, simetrikTers, carp, transpoz } from './matris.js';
import { pchisq, qchisq, qnorm, pnorm } from './dagilim.js';
import { brent, lgamma } from './sayisal.js';
import { siralar } from './betimsel.js';
import { IstatistikHatasi } from './hata.js';
import { modelMatrisi, duzeySirala, eksikMi, tamSatirlar, tahminciSec } from './model.js';

const EPS = Number.EPSILON;

// ---------- Aileler (bağlantı fonksiyonlarıyla birlikte) ----------

/** log(1 + e^x), taşmasız */
function softplus(x) {
  return x > 0 ? x + Math.log1p(Math.exp(-x)) : Math.log1p(Math.exp(x));
}

/** Lojistik dağılım: p = 1/(1+e^-η) ve q = 1 - p, kayıpsız hesap */
function lojistikPQ(eta) {
  if (eta >= 0) {
    const e = Math.exp(-eta);
    return [1 / (1 + e), e / (1 + e)];
  }
  const e = Math.exp(eta);
  return [e / (1 + e), 1 / (1 + e)];
}

export const AILELER = {
  binom: {
    ad: 'binom',
    baglanti: 'logit',
    /** R binomial()$initialize: mustart = (y + 0.5)/2 */
    baslangicEta: (y) => y.map((v) => {
      const m = (v + 0.5) / 2;
      return Math.log(m / (1 - m));
    }),
    ortalama: (eta) => lojistikPQ(eta)[0],
    /** çalışma ağırlığı w = μ(1-μ) ve (y-μ)/(dμ/dη) */
    guncelle(eta, y) {
      const [p, q] = lojistikPQ(eta);
      const w = p * q;
      const fark = y * q - (1 - y) * p; // y - p (0/1 y için kayıpsız)
      return { w, z: w > 0 ? fark / w : 0, mu: p };
    },
    /** sapma katkısı d_i (R binomial dev.resids, 0/1 y) */
    sapmaKatkisi: (y, eta) => 2 * (y * softplus(-eta) + (1 - y) * softplus(eta)),
    varyans: (mu) => mu * (1 - mu),
    logOlabilirlik: (y, eta) => -(y * softplus(-eta) + (1 - y) * softplus(eta)),
  },
  poisson: {
    ad: 'poisson',
    baglanti: 'log',
    /** R poisson()$initialize: mustart = y + 0.1 */
    baslangicEta: (y) => y.map((v) => Math.log(v + 0.1)),
    ortalama: (eta) => Math.max(Math.exp(eta), EPS),
    guncelle(eta, y) {
      const mu = Math.max(Math.exp(eta), EPS);
      return { w: mu, z: (y - mu) / mu, mu };
    },
    sapmaKatkisi(y, eta) {
      const mu = Math.max(Math.exp(eta), EPS);
      return 2 * ((y > 0 ? y * Math.log(y / mu) : 0) - (y - mu));
    },
    varyans: (mu) => mu,
    logOlabilirlik(y, eta) {
      const mu = Math.max(Math.exp(eta), EPS);
      return (y > 0 ? y * Math.log(mu) : 0) - mu - lgamma(y + 1);
    },
  },
};

function sapmaHesapla(aile, y, eta) {
  let s = 0;
  for (let i = 0; i < y.length; i++) s += aile.sapmaKatkisi(y[i], eta[i]);
  return s;
}

function dogrusalTahmin(X, beta, offset) {
  const n = X.length;
  const eta = new Array(n);
  for (let i = 0; i < n; i++) {
    let s = offset ? offset[i] : 0;
    const xi = X[i];
    for (let j = 0; j < beta.length; j++) s += xi[j] * beta[j];
    eta[i] = s;
  }
  return eta;
}

/** Ağırlıklı EKK adımı: (√w X) β = √w z */
function aeKK(X, eta, offset, aile, y) {
  const n = X.length;
  const r = X[0].length;
  const XW = new Array(n);
  const zW = new Array(n);
  for (let i = 0; i < n; i++) {
    const g = aile.guncelle(eta[i], y[i]);
    const sw = Math.sqrt(g.w);
    const xi = X[i];
    const satir = new Array(r);
    for (let j = 0; j < r; j++) satir[j] = xi[j] * sw;
    XW[i] = satir;
    zW[i] = (eta[i] - (offset ? offset[i] : 0) + g.z) * sw;
  }
  return enKucukKareler(XW, zW, 1e-11);
}

/**
 * GLM uyumu: R glm.fit ile aynı IRLS (başlangıç değerleri, sapma tabanlı yakınsama ölçütü
 * |D - D_eski|/(|D| + 0.1) < epsilon ve yineleme sayımı), ardından tam yakınsamaya dek Newton "cilalama" adımları.
 * Sapma artarsa ya da sonlu değilse adım yarılanır. Ayrışma (separation) saptanır.
 *
 * @param {number[][]} X tasarım matrisi (tam ranklı olmalı; bağımlı sütunlar önceden çıkarılmalı)
 * @param {number[]} y
 * @param {object} aile AILELER.binom | AILELER.poisson
 * @param {{offset?:number[], maksIterasyon?:number, epsilon?:number, baslangicBeta?:number[], cilaIterasyon?:number,
 *   kovaryansIste?:boolean}} [secenekler] kovaryansIste = false: son (X'WX)⁻¹ hesaplanmaz (profil uyumları için)
 * @returns {{beta:number[], eta:number[], sapma:number, iterasyon:number, yakinsadi:boolean, cilaYakinsadi:boolean,
 *   ayrisanSutunlar:number[], tekil:boolean, XtXTers:number[][]|null}}
 */
export function glmIRLS(X, y, aile, secenekler = {}) {
  const { offset = null, maksIterasyon = 25, epsilon = 1e-8, baslangicBeta = null, cilaIterasyon = 60, kovaryansIste = true } = secenekler;
  const n = y.length;
  const r = n ? X[0].length : 0;
  if (r === 0) {
    const eta = offset ? offset.slice() : new Array(n).fill(0);
    return { beta: [], eta, sapma: sapmaHesapla(aile, y, eta), iterasyon: 0, yakinsadi: true, cilaYakinsadi: true, ayrisanSutunlar: [], tekil: false, XtXTers: [] };
  }
  let beta = baslangicBeta ? baslangicBeta.slice() : null;
  let eta = beta ? dogrusalTahmin(X, beta, offset) : aile.baslangicEta(y);
  let sapma = sapmaHesapla(aile, y, eta);
  let iterasyon = 0;
  let yakinsadi = false;
  let tekil = false;
  let sonEKK = null;

  // Bir Newton/IRLS adımı (yarılamalı). Dönüş: {beta, eta, sapma} ya da null (tekil)
  const adim = (betaEski, etaEski, sapmaEski) => {
    const e = aeKK(X, etaEski, offset, aile, y);
    if (e.rank < r || e.katsayi.some((v) => v === null || !Number.isFinite(v))) return null;
    let b = e.katsayi;
    let et = dogrusalTahmin(X, b, offset);
    let d = sapmaHesapla(aile, y, et);
    let yarilama = 0;
    while (betaEski && (!Number.isFinite(d) || d > sapmaEski * (1 + 1e-12) + 1e-12) && yarilama < 30) {
      b = b.map((v, j) => (v + betaEski[j]) / 2);
      et = dogrusalTahmin(X, b, offset);
      d = sapmaHesapla(aile, y, et);
      yarilama++;
    }
    if (!Number.isFinite(d)) return null;
    return { beta: b, eta: et, sapma: d };
  };

  for (let it = 1; it <= maksIterasyon; it++) {
    const s = adim(beta, eta, sapma);
    if (!s) {
      tekil = true;
      break;
    }
    iterasyon = it;
    const fark = Math.abs(s.sapma - sapma) / (Math.abs(s.sapma) + 0.1);
    beta = s.beta;
    eta = s.eta;
    sapma = s.sapma;
    if (fark < epsilon) {
      yakinsadi = true;
      break;
    }
  }
  // Cilalama: katsayılar makine duyarlığında durana dek
  let cilaYakinsadi = false;
  let sonFark = null;
  if (beta && !tekil) {
    const olcek = new Array(r).fill(0);
    for (const xi of X) for (let j = 0; j < r; j++) olcek[j] = Math.max(olcek[j], Math.abs(xi[j]));
    for (let k = 0; k < cilaIterasyon; k++) {
      const s = adim(beta, eta, sapma);
      if (!s) break;
      const d = s.beta.map((v, j) => Math.abs(v - beta[j]));
      sonFark = d.map((v, j) => v * olcek[j]);
      const durdu = d.every((v, j) => v * olcek[j] <= 1e-10 * (1 + Math.abs(s.beta[j]) * olcek[j]));
      beta = s.beta;
      eta = s.eta;
      sapma = s.sapma;
      if (durdu) {
        cilaYakinsadi = true;
        break;
      }
    }
  }
  // Ayrışma: katsayısı yineleme başına doğrusal ıraksayan (|Δβ_j|·max|x_j| büyük kalan) sütunlar
  const ayrisanSutunlar = [];
  if (!cilaYakinsadi && sonFark) sonFark.forEach((v, j) => v > 1e-3 && ayrisanSutunlar.push(j));
  if (beta && kovaryansIste) {
    const e = aeKK(X, eta, offset, aile, y);
    sonEKK = e.rank === r ? e : null;
  }
  return {
    beta: beta || new Array(r).fill(NaN),
    eta,
    sapma,
    iterasyon,
    yakinsadi,
    cilaYakinsadi,
    ayrisanSutunlar,
    tekil,
    XtXTers: sonEKK ? sonEKK.XtXTers : null,
  };
}

/**
 * Model matrisi + IRLS + standart hatalar: lojistik ve Poisson regresyonun ortak çekirdeği.
 * Bağımlı (aliased) sütunlar R lm()'deki gibi baştan saptanıp çıkarılır.
 */
export function glmUydur(mm, y, aile, secenekler = {}) {
  const X = mm.X;
  const p = mm.sutunAdlari.length;
  const n = X.length;
  // Doğrusal bağımlılık (ağırlıktan bağımsız; R lm toleransı 1e-7)
  const yapi = p ? enKucukKareler(X, new Array(n).fill(0)) : { bagimli: [], katsayi: [] };
  const bagimli = yapi.bagimli;
  const bagSet = new Set(bagimli);
  const kullanilan = [];
  for (let j = 0; j < p; j++) if (!bagSet.has(j)) kullanilan.push(j);
  const Xr = X.map((s) => kullanilan.map((j) => s[j]));
  const fit = glmIRLS(Xr, y, aile, secenekler);
  const katsayi = new Array(p).fill(null);
  kullanilan.forEach((j, k) => (katsayi[j] = fit.beta[k]));
  const kovaryans = sifir(p, p).map((s) => s.fill(null));
  if (fit.XtXTers) kullanilan.forEach((a, ka) => kullanilan.forEach((b, kb) => (kovaryans[a][b] = fit.XtXTers[ka][kb])));
  return { ...fit, katsayi, kovaryans, bagimli, kullanilan, Xr, rank: kullanilan.length, ayrisanSutunlar: fit.ayrisanSutunlar.map((k) => kullanilan[k]) };
}

/** Profil olabilirlik güven aralığı (R confint.glm'in tanımı; spline yerine kesin kök bulma ile). */
function profilGA(uyum, y, aile, j, duzey, offset) {
  const { Xr, kullanilan, sapma: D0 } = uyum;
  const k = kullanilan.indexOf(j);
  const bHat = uyum.katsayi[j];
  const se = Math.sqrt(uyum.kovaryans[j][j]);
  const hedef = qchisq(duzey, 1);
  const Xd = Xr.map((s) => s.filter((_, m) => m !== k));
  const xj = Xr.map((s) => s[k]);
  const digerBeta = uyum.beta.filter((_, m) => m !== k);
  let sicak = digerBeta.slice();
  // ζ(b) = işaret(b − b̂)·√(D(b) − D̂): profilde neredeyse doğrusaldır (R profile.glm'in "z" ölçeği); kök ζ(b) = ±z_{α/2}
  // ⇔ D(b) − D̂ = χ²_{1,α}. Doğrusala yakın olduğundan sekant/Brent birkaç değerlendirmede yakınsar.
  const zeta = (b) => {
    const off = xj.map((v, i) => v * b + (offset ? offset[i] : 0));
    const f = glmIRLS(Xd, y, aile, { offset: off, baslangicBeta: sicak, maksIterasyon: 100, kovaryansIste: false });
    if (f.tekil || !Number.isFinite(f.sapma)) return NaN;
    if (f.cilaYakinsadi) sicak = f.beta;
    return Math.sign(b - bHat) * Math.sqrt(Math.max(f.sapma - D0, 0));
  };
  const zKrit = Math.sqrt(hedef);
  const sinir = (yon) => {
    sicak = digerBeta.slice();
    const h = (b) => zeta(b) - yon * zKrit;
    let a = bHat;
    let ha = -yon * zKrit;
    let b = bHat + yon * zKrit * se;
    let hb = h(b);
    let sayac = 0;
    // köke kadar dışa doğru ilerle (sekant tahmini; en az %10, en çok 2 kat)
    while (Number.isFinite(hb) && Math.sign(hb) === Math.sign(ha) && sayac < 60) {
      const uzak = Math.abs(b - bHat);
      let ad = Math.abs((hb * (b - a)) / (hb - ha));
      if (!Number.isFinite(ad)) ad = uzak;
      ad = Math.min(Math.max(1.2 * ad, 0.1 * uzak), 2 * uzak);
      a = b;
      ha = hb;
      b += yon * ad;
      hb = h(b);
      sayac++;
    }
    if (!Number.isFinite(hb) || Math.sign(hb) === Math.sign(ha)) return null;
    return brent(h, Math.min(a, b), Math.max(a, b), 1e-12 * Math.max(1, Math.abs(bHat), Math.abs(b)), 200);
  };
  return [sinir(-1), sinir(1)];
}

/** Hosmer-Lemeshow (ResourceSelection::hoslem.test ile aynı gruplama: tip 7 yüzdelik kırılımları, (a,b] aralıkları) */
export function hosmerLemeshow(y, mu, g = 10) {
  const n = mu.length;
  const sirali = Float64Array.from(mu).sort();
  const kir = [];
  const fuzz = 4 * EPS;
  for (let k = 0; k <= g; k++) {
    const prob = Math.min(k * (1 / g), 1);
    const index = 1 + Math.max(n - 1, 0) * prob;
    const lo = Math.floor(index * (1 + fuzz));
    const hi = Math.ceil(index);
    let q = sirali[lo - 1];
    if (index > lo && sirali[hi - 1] !== q) {
      const h = index - lo;
      q = (1 - h) * q + h * sirali[hi - 1];
    }
    kir.push(q);
  }
  const kirilim = [];
  for (const v of kir) if (!kirilim.includes(v)) kirilim.push(v);
  kirilim.sort((a, b) => a - b);
  const m = kirilim.length - 1;
  const gruplar = Array.from({ length: Math.max(m, 1) }, (_, i) => ({
    grup: i + 1,
    alt: kirilim[i],
    ust: kirilim[Math.min(i + 1, m)],
    n: 0,
    gozlenenOlay: 0,
    beklenenOlay: 0,
    gozlenenOlmayan: 0,
    beklenenOlmayan: 0,
  }));
  for (let i = 0; i < n; i++) {
    const x = mu[i];
    let k;
    if (m <= 0) k = 0;
    else if (x === kirilim[0]) k = 0;
    else {
      let lo = 0;
      let hi = m;
      while (hi - lo >= 2) {
        const mid = (hi + lo) >> 1;
        if (x > kirilim[mid]) lo = mid;
        else hi = mid;
      }
      k = lo;
    }
    const G = gruplar[k];
    G.n++;
    G.gozlenenOlay += y[i];
    G.gozlenenOlmayan += 1 - y[i];
    G.beklenenOlay += x;
    G.beklenenOlmayan += 1 - x;
  }
  const dolu = gruplar.filter((G) => G.n > 0);
  let ki = 0;
  for (const G of dolu) {
    ki += (G.gozlenenOlmayan - G.beklenenOlmayan) ** 2 / G.beklenenOlmayan + (G.gozlenenOlay - G.beklenenOlay) ** 2 / G.beklenenOlay;
  }
  const sd = dolu.length - 2;
  return {
    istatistik: sd > 0 ? ki : null,
    sd,
    p: sd > 0 ? pchisq(ki, sd, false) : null,
    grupSayisi: dolu.length,
    gruplar: dolu.map((G, i) => ({ ...G, grup: i + 1 })),
  };
}

/** ROC eğrisi altındaki alan: Mann-Whitney U / (n1·n0), bağlarda ortalama sıra (pROC::auc ile aynı, yön: olay > olmayan) */
export function aucHesapla(y, skor) {
  const { sira } = siralar(skor);
  let n1 = 0;
  let toplamSira = 0;
  for (let i = 0; i < y.length; i++) {
    if (y[i] === 1) {
      n1++;
      toplamSira += sira[i];
    }
  }
  const n0 = y.length - n1;
  if (n1 === 0 || n0 === 0) return null;
  return (toplamSira - (n1 * (n1 + 1)) / 2) / (n1 * n0);
}

function siniflamaTablosu(y, mu, kesim) {
  let dp = 0;
  let yp = 0;
  let yn = 0;
  let dn = 0;
  for (let i = 0; i < y.length; i++) {
    const tahmin = mu[i] >= kesim ? 1 : 0; // SPSS: olasılık kesim değerine eşit ya da büyükse olay
    if (y[i] === 1) {
      if (tahmin === 1) dp++;
      else yn++;
    } else if (tahmin === 1) yp++;
    else dn++;
  }
  const oran = (a, b) => (a + b > 0 ? a / (a + b) : null);
  return {
    kesim,
    tablo: { dogruNegatif: dn, yanlisPozitif: yp, yanlisNegatif: yn, dogruPozitif: dp },
    duyarlilik: oran(dp, yn),
    ozgulluk: oran(dn, yp),
    pozitifKestirimDegeri: oran(dp, yp),
    negatifKestirimDegeri: oran(dn, yn),
    dogruSiniflamaYuzdesi: (100 * (dp + dn)) / y.length,
    olmayanDogruYuzde: dn + yp > 0 ? (100 * dn) / (dn + yp) : null,
    olayDogruYuzde: dp + yn > 0 ? (100 * dp) / (dp + yn) : null,
  };
}

/** Skor (Rao) testi: sabit-yalnız modele Z sütunlarının eklenmesi (SPSS Blok 0 "Variables not in the Equation") */
function skorTesti(Z, y, ybar) {
  const n = y.length;
  const k = Z[0].length;
  const w = ybar * (1 - ybar);
  const zOrt = new Array(k).fill(0);
  for (const zi of Z) for (let j = 0; j < k; j++) zOrt[j] += zi[j] / n;
  const U = new Array(k).fill(0);
  for (let i = 0; i < n; i++) for (let j = 0; j < k; j++) U[j] += (Z[i][j] - zOrt[j]) * (y[i] - ybar);
  // I = w Zc'Zc; U'I^-1U via EKK: Zc β = (y - ȳ)/w ... doğrudan çöz
  const Zc = Z.map((zi) => zi.map((v, j) => v - zOrt[j]));
  const e = enKucukKareler(Zc, new Array(n).fill(0));
  let S = 0;
  let sd = 0;
  for (let a = 0; a < k; a++) {
    if (e.katsayi[a] === null) continue;
    sd++;
    for (let b = 0; b < k; b++) if (e.katsayi[b] !== null) S += (U[a] * e.XtXTers[a][b] * U[b]) / w;
  }
  return { skor: S, sd, p: sd > 0 ? pchisq(S, sd, false) : null };
}

/** Bağımlı değişkeni 0/1'e çevirir; olay düzeyi verilmezse sıralı düzeylerin sonuncusu (SPSS: büyük kod = 1). */
function ikiliHazirla(degerler, olay) {
  const dolu = degerler.filter((v) => !eksikMi(v));
  const duzeyler = duzeySirala(dolu);
  if (duzeyler.length !== 2) {
    throw new IstatistikHatasi(
      duzeyler.length < 2
        ? 'Bağımlı değişkende tek bir sonuç değeri var; lojistik regresyon için iki farklı sonuç gerekir.'
        : `İkili lojistik regresyon için bağımlı değişken iki düzeyli olmalıdır (${duzeyler.length} düzey bulundu). Çok kategorili sonuçlar için multinominal ya da ordinal lojistik regresyon kullanın.`,
    );
  }
  let olayDuzeyi = duzeyler[1];
  if (olay !== undefined && olay !== null) {
    const bul = duzeyler.find((d) => String(d) === String(olay));
    if (bul === undefined) throw new IstatistikHatasi(`Olay düzeyi olarak seçilen '${olay}' bağımlı değişkende yok.`);
    olayDuzeyi = bul;
  }
  const digerDuzey = duzeyler.find((d) => String(d) !== String(olayDuzeyi));
  const kod = degerler.map((v) => (eksikMi(v) ? null : String(v) === String(olayDuzeyi) ? 1 : 0));
  return { kod, olayDuzeyi, digerDuzey };
}

/** GLM tanılama değerleri (R: hatvalues, residuals(type), rstandard, cooks.distance) */
function glmTanilama(uyum, y, aile) {
  const { Xr, eta } = uyum;
  const n = y.length;
  const XtXTers = uyum.XtXTers;
  const r = uyum.rank;
  const mu = eta.map((e) => aile.ortalama(e));
  const kaldirac = new Array(n);
  const pearson = new Array(n);
  const sapmaArtik = new Array(n);
  const stdPearson = new Array(n);
  const stdSapma = new Array(n);
  const cook = new Array(n);
  for (let i = 0; i < n; i++) {
    const g = aile.guncelle(eta[i], y[i]);
    let h = null;
    if (XtXTers) {
      h = 0;
      const xi = Xr[i];
      for (let a = 0; a < r; a++) {
        let s = 0;
        for (let b = 0; b < r; b++) s += XtXTers[a][b] * xi[b];
        h += xi[a] * s;
      }
      h *= g.w;
      if (h > 1 - 10 * EPS) h = 1;
    }
    kaldirac[i] = h;
    const v = aile.varyans(mu[i]);
    pearson[i] = (y[i] - mu[i]) / Math.sqrt(v);
    const d = aile.sapmaKatkisi(y[i], eta[i]);
    sapmaArtik[i] = (y[i] > mu[i] ? 1 : y[i] < mu[i] ? -1 : 0) * Math.sqrt(Math.max(d, 0));
    if (h === null || h >= 1) {
      stdPearson[i] = stdSapma[i] = cook[i] = null;
    } else {
      stdPearson[i] = pearson[i] / Math.sqrt(1 - h);
      stdSapma[i] = sapmaArtik[i] / Math.sqrt(1 - h);
      cook[i] = ((pearson[i] / (1 - h)) ** 2 * h) / r;
    }
  }
  return { olasilik: mu, dogrusalTahmin: eta, kaldirac, pearsonArtik: pearson, sapmaArtik, standartPearsonArtik: stdPearson, standartSapmaArtik: stdSapma, cookD: cook };
}

/** Wald testi: b_T' V_TT^-1 b_T */
function waldTesti(katsayi, kovaryans, sutunlar) {
  const idx = sutunlar.filter((j) => katsayi[j] !== null && kovaryans[j][j] !== null);
  if (!idx.length) return { wald: null, sd: 0, p: null };
  const V = idx.map((a) => idx.map((b) => kovaryans[a][b]));
  const b = idx.map((j) => katsayi[j]);
  const e = enKucukKareler(V, b, 1e-12);
  if (e.bagimli.length) return { wald: null, sd: idx.length, p: null };
  let W = 0;
  for (let k = 0; k < idx.length; k++) W += b[k] * e.katsayi[k];
  return { wald: W, sd: idx.length, p: pchisq(W, idx.length, false) };
}

/**
 * İkili lojistik regresyon (logit bağlantı, en çok olabilirlik; IRLS/Newton-Raphson, adım yarılamalı).
 *
 * R karşılığı: `glm(y ~ ..., family = binomial)`, `summary`, `confint` (profil olabilirlik; burada spline enterpolasyonu yerine
 * kesin kök bulma ile), `drop1(fit, test = "LRT")`, `anova(fit0, fit1, test = "Rao")`, `ResourceSelection::hoslem.test(g = 10)`,
 * `pROC::auc`. SPSS karşılığı: LOGISTIC REGRESSION — Block 0 (sınıflama, Variables not in the Equation: skor testleri),
 * Omnibus Tests, Model Summary (−2LL, Cox & Snell R², Nagelkerke R²), Hosmer-Lemeshow, Classification Table (CUT, olasılık
 * ≥ kesim → olay), Variables in the Equation (B, S.E., Wald = (B/SE)², df, Sig., Exp(B), Exp(B) için Wald GA; kategorik
 * değişkenin genel Wald satırı).
 *
 * Kurallar ve farklar:
 * - Olay düzeyi `olay` ile seçilir; verilmezse sıralı düzeylerin sonuncusu (0/1 için 1; SPSS de büyük kodu 1 yapar).
 * - Kategorik tahmincide referans varsayılanı İLK düzeydir (R). SPSS'in varsayılanı SON kategoridir; aynı sonuç için
 *   `referans` verin.
 * - Hosmer-Lemeshow gruplaması R ResourceSelection ile aynıdır (tahmin olasılıklarının tip 7 ondalıkları). SPSS aynı
 *   kovaryat örüntüsündeki gözlemleri bölmeden ~10 eşit gruba ayırır; bağlı olasılık çoksa SPSS'in χ² ve sd değeri biraz farklı
 *   olabilir. Boş grup oluşursa (R NaN verir) boş gruplar atlanır.
 * - `iterasyon`: R'nin `fit$iter` değeri (Fisher scoring yineleme sayısı). SPSS farklı başlangıç ve ölçüt kullanır.
 * - Kestirimler R'nin varsayılan ölçütünden sonra Newton cilalamasıyla tam yakınsatılır (R'de glm.control(epsilon = 1e-14)).
 * - Ayrışma (complete / quasi-complete separation) varsa ıraksayan katsayıların B/SH/OR değerleri null döner ve uyarı eklenir.
 * - Standart hatalar MLE noktasındaki ağırlıklarla hesaplanır. R summary.glm son yinelemenin (bir önceki β) ağırlıklarını
 *   kullandığından R'nin varsayılan çıktısındaki SH'ler 4.–6. anlamlı basamakta farklı olabilir.
 * - EPV = min(olay, olay olmayan) / tahminci parametresi sayısı; 10'un altında uyarı.
 * - Tanılama (tahminler): olasilik, dogrusalTahmin, tahminiGrup, kaldirac (R hatvalues), pearsonArtik (SPSS ZRESID),
 *   sapmaArtik (SPSS DEV), standartPearsonArtik, standartSapmaArtik (R rstandard type = pearson/deviance), cookD (R
 *   cooks.distance).
 *
 * @param {object} secenekler
 * @param {Array} secenekler.y bağımlı değişken (0/1 ya da iki düzeyli etiketler; eksikler null)
 * @param {any} [secenekler.olay] olay (1 kodlanacak) düzeyi
 * @param {Array<object>} secenekler.tahminciler bkz. model.js
 * @param {string[][]} [secenekler.etkilesimler]
 * @param {number} [secenekler.duzey=0.95]
 * @param {number} [secenekler.kesim=0.5] sınıflama kesim değeri
 * @param {number} [secenekler.hlGrupSayisi=10]
 * @param {boolean} [secenekler.profilGA=true] profil olabilirlik GA'ları (büyük veride yavaş olabilir)
 * @param {boolean} [secenekler.terimLR=true] terim çıkarma olabilirlik oranı testleri
 * @param {boolean} [secenekler.sabit=true]
 * @param {number} [secenekler.maksIterasyon=25] R glm.control(maxit) aşaması için en çok yineleme
 * @returns {object} `{yontem, ad, istatistik, istatistikAdi, sd, p, n,
 *   olayDuzeyi, digerDuzey, olaySayisi, olmayanSayisi, katsayilar:[{ad, terim, duzey, B, SH, z, wald, sd, p, OR, orGaAlt,
 *   orGaUst, gaAlt, gaUst, profilGaAlt, profilGaUst, orProfilGaAlt, orProfilGaUst, bagimli, ayrisma}], terimTestleri:[{ad,
 *   tur, sd, wald, waldP, lrKiKare, lrSd, lrP}], modelOzeti:{ikiLLSifir, ikiLL, kiKare, sd, p, coxSnellR2, nagelkerkeR2,
 *   mcFaddenR2, AIC, BIC, logOlabilirlik, iterasyon, n}, hosmerLemeshow, siniflama, auc, epv, blok0, tahminler, kovaryans,
 *   yakinsadi, ayrisma, bagimliSutunlar, sutunAdlari, terimler, satirlar, uyarilar}`
 */
export function lojistikRegresyon(secenekler) {
  const {
    y,
    olay,
    tahminciler = [],
    etkilesimler = [],
    duzey = 0.95,
    kesim = 0.5,
    hlGrupSayisi = 10,
    profilGA: profilIste = true,
    terimLR = true,
    sabit = true,
    maksIterasyon = 25,
  } = secenekler || {};
  if (!Array.isArray(y)) throw new IstatistikHatasi('Bağımlı değişken (y) bir dizi olmalıdır.');
  if (!(kesim > 0 && kesim < 1)) throw new IstatistikHatasi('Sınıflama kesim değeri 0 ile 1 arasında olmalıdır.');
  const mm = modelMatrisi(tahminciler, { sabit, etkilesimler, ekDiziler: [y] });
  const { kod, olayDuzeyi, digerDuzey } = ikiliHazirla(mm.ekler[0], olay);
  const yv = kod;
  const n = yv.length;
  const uyarilar = [...mm.uyarilar];
  let n1 = 0;
  for (const v of yv) n1 += v;
  const n0 = n - n1;
  if (n1 === 0 || n0 === 0) {
    throw new IstatistikHatasi('Eksik değerler çıkarıldıktan sonra bağımlı değişkenin yalnızca bir sonucu kaldı; lojistik regresyon yapılamaz.');
  }
  if (mm.sutunAdlari.length === 0) throw new IstatistikHatasi('Modelde hiç terim yok.');
  const aile = AILELER.binom;
  const uyum = glmUydur(mm, yv, aile, { maksIterasyon });
  if (uyum.tekil && !uyum.XtXTers) {
    throw new IstatistikHatasi('Model kestirilemedi: bilgi matrisi tekil (tahminciler arasında tam bağımlılık ya da ayrışma).');
  }
  const p = mm.sutunAdlari.length;
  const rank = uyum.rank;
  const bagimliAdlar = uyum.bagimli.map((j) => mm.sutunAdlari[j]);
  if (bagimliAdlar.length) {
    uyarilar.push(`Tam çoklu doğrusal bağlantı: ${bagimliAdlar.map((a) => `'${a}'`).join(', ')} diğer değişkenlerin doğrusal bileşimi olduğu için modelden çıkarıldı (R'de katsayısı NA görünür).`);
  }

  // Ayrışma
  const D = uyum.sapma;
  let ayrisma = null;
  const ayrisanSet = new Set(uyum.ayrisanSutunlar);
  if (!uyum.cilaYakinsadi) {
    const adlar = uyum.ayrisanSutunlar.map((j) => mm.sutunAdlari[j]);
    const tur = D < 1e-6 ? 'tam' : 'yari-tam';
    ayrisma = { tur, sutunlar: adlar };
    if (adlar.length) {
      const degiskenler = adlar.filter((a) => a !== mm.sutunAdlari[0] || !sabit);
      uyarilar.push(
        `${tur === 'tam' ? 'Tam' : 'Yarı-tam'} ayrışma (${tur === 'tam' ? 'complete' : 'quasi-complete'} separation) saptandı: ` +
          `${degiskenler.length ? degiskenler.map((a) => `'${a}'`).join(', ') : 'tahmincilerin bir bileşimi'} sonucu ${tur === 'tam' ? 'kusursuz' : 'bir alt grupta kusursuz'} ayırıyor. ` +
          'En çok olabilirlik kestirimi sonlu değil; bu katsayıların B, standart hata, Wald ve OR değerleri raporlanmadı (sonsuza ıraksıyor). ' +
          'Kategorileri birleştirmeyi, değişkeni çıkarmayı ya da Firth düzeltmeli (cezalı) lojistik regresyonu düşünün.',
      );
    } else {
      uyarilar.push('Model yakınsamadı (en çok yineleme sayısına ulaşıldı); sonuçlar güvenilir olmayabilir.');
    }
  }
  if (!uyum.yakinsadi && uyum.cilaYakinsadi) {
    uyarilar.push(`Model R'nin varsayılan ${maksIterasyon} yinelemesinde yakınsamadı; ek Newton adımlarıyla yakınsatıldı.`);
  }

  // Sıfır (yalnız sabit) model
  const ybar = n1 / n;
  const ikiLLSifir = sabit ? -2 * (n1 * Math.log(ybar) + n0 * Math.log(1 - ybar)) : 2 * n * Math.LN2;
  const kiKare = ikiLLSifir - D;
  const sdModel = rank - (sabit ? 1 : 0);
  const pModel = sdModel > 0 ? pchisq(Math.max(kiKare, 0), sdModel, false) : null;
  const coxSnellR2 = 1 - Math.exp(-kiKare / n);
  const nagelkerkeR2 = coxSnellR2 / (1 - Math.exp(-ikiLLSifir / n));
  const mcFaddenR2 = 1 - D / ikiLLSifir;
  const logL = -D / 2;
  const AIC = D + 2 * rank;
  const BIC = D + Math.log(n) * rank;

  // Katsayılar
  const z = qnorm(1 - (1 - duzey) / 2);
  const katsayilar = [];
  for (let j = 0; j < p; j++) {
    const bilgi = mm.sutunlar[j];
    const satir = {
      ad: mm.sutunAdlari[j],
      terim: bilgi.terim,
      duzey: bilgi.duzey,
      B: null,
      SH: null,
      z: null,
      wald: null,
      sd: 1,
      p: null,
      OR: null,
      orGaAlt: null,
      orGaUst: null,
      gaAlt: null,
      gaUst: null,
      profilGaAlt: null,
      profilGaUst: null,
      orProfilGaAlt: null,
      orProfilGaUst: null,
      bagimli: uyum.katsayi[j] === null,
      ayrisma: ayrisanSet.has(j),
    };
    if (!satir.bagimli && !satir.ayrisma && uyum.kovaryans[j][j] !== null) {
      const B = uyum.katsayi[j];
      const SH = Math.sqrt(uyum.kovaryans[j][j]);
      satir.B = B;
      satir.SH = SH;
      satir.z = B / SH;
      satir.wald = satir.z * satir.z;
      satir.p = 2 * pnorm(-Math.abs(satir.z));
      satir.OR = Math.exp(B);
      satir.gaAlt = B - z * SH;
      satir.gaUst = B + z * SH;
      satir.orGaAlt = Math.exp(satir.gaAlt);
      satir.orGaUst = Math.exp(satir.gaUst);
    }
    katsayilar.push(satir);
  }
  if (profilIste && !ayrisma) {
    for (const j of uyum.kullanilan) {
      const [a, b] = profilGA(uyum, yv, aile, j, duzey, null);
      const s = katsayilar[j];
      s.profilGaAlt = a;
      s.profilGaUst = b;
      s.orProfilGaAlt = a === null ? null : Math.exp(a);
      s.orProfilGaUst = b === null ? null : Math.exp(b);
    }
  }

  // Terim testleri (Wald: SPSS'teki kategorik değişken genel satırı; LR: drop1(test = "LRT"))
  const terimTestleri = [];
  for (const t of mm.terimler) {
    const w = waldTesti(
      uyum.katsayi.map((v, j) => (ayrisanSet.has(j) ? null : v)),
      uyum.kovaryans,
      t.sutunlar,
    );
    const satir = { ad: t.ad, tur: t.tur, sd: w.sd, wald: w.wald, waldP: w.p, lrKiKare: null, lrSd: null, lrP: null };
    if (terimLR) {
      const sil = new Set(t.sutunlar);
      const kalan = uyum.kullanilan.filter((j) => !sil.has(j));
      const Xk = mm.X.map((s) => kalan.map((j) => s[j]));
      const f = glmIRLS(Xk, yv, aile, { maksIterasyon, kovaryansIste: false });
      const sd = rank - kalan.length;
      if (sd > 0 && Number.isFinite(f.sapma)) {
        satir.lrKiKare = f.sapma - D;
        satir.lrSd = sd;
        satir.lrP = pchisq(Math.max(satir.lrKiKare, 0), sd, false);
      }
    }
    terimTestleri.push(satir);
  }

  const tahminler = glmTanilama(uyum, yv, aile);
  const mu = tahminler.olasilik;
  tahminler.tahminiGrup = mu.map((m) => (m >= kesim ? 1 : 0));

  // Hosmer-Lemeshow
  const hl = hosmerLemeshow(yv, mu, hlGrupSayisi);
  if (hl.istatistik === null) uyarilar.push('Hosmer-Lemeshow testi hesaplanamadı: farklı tahmin olasılığı sayısı en az 3 grup oluşturmaya yetmiyor.');
  else if (hl.grupSayisi < hlGrupSayisi) uyarilar.push(`Hosmer-Lemeshow testi için veriler ${hlGrupSayisi} yerine ${hl.grupSayisi} grup oluşturabildi.`);

  const siniflama = siniflamaTablosu(yv, mu, kesim);
  const auc = aucHesapla(yv, mu);

  // EPV (olay/değişken): sınırlayıcı sonuç sayısı = min(olay, olmayan) / tahminci parametresi sayısı
  const parametre = sdModel;
  const sinirlayici = Math.min(n1, n0);
  const epv = { deger: parametre > 0 ? sinirlayici / parametre : null, sinirlayiciSonucSayisi: sinirlayici, parametreSayisi: parametre };
  if (epv.deger !== null && epv.deger < 10) {
    uyarilar.push(`Değişken başına olay sayısı (EPV = ${epv.deger.toFixed(1).replace('.', ',')}) 10'un altında; katsayılar yanlı ve model aşırı uyumlu olabilir. Tahminci sayısını azaltmayı düşünün.`);
  }

  // Blok 0 (SPSS): yalnız sabitli model sınıflaması ve skor testleri
  let blok0 = null;
  if (sabit) {
    const sabitMu = new Array(n).fill(ybar);
    const skorTestleri = [];
    for (const t of mm.terimler) {
      const sut = t.sutunlar.filter((j) => uyum.katsayi[j] !== null);
      if (!sut.length) continue;
      if (sut.length > 1) {
        const s = skorTesti(mm.X.map((r) => sut.map((j) => r[j])), yv, ybar);
        skorTestleri.push({ ad: t.ad, terim: t.ad, ...s });
      }
      for (const j of sut) {
        const s = skorTesti(mm.X.map((r) => [r[j]]), yv, ybar);
        skorTestleri.push({ ad: mm.sutunAdlari[j], terim: t.ad, ...s });
      }
    }
    const tum = uyum.kullanilan.filter((j) => j !== 0);
    const genelSkor = tum.length ? skorTesti(mm.X.map((r) => tum.map((j) => r[j])), yv, ybar) : null;
    blok0 = { sabitB: Math.log(ybar / (1 - ybar)), siniflama: siniflamaTablosu(yv, sabitMu, kesim), skorTestleri, genelSkor };
  }

  return {
    yontem: 'lojistik-regresyon',
    ad: 'İkili lojistik regresyon',
    istatistik: kiKare,
    istatistikAdi: 'χ²',
    sd: sdModel,
    p: pModel,
    n,
    olayDuzeyi,
    digerDuzey,
    olaySayisi: n1,
    olmayanSayisi: n0,
    etki: { ad: 'Nagelkerke R²', sembol: 'R²N', deger: nagelkerkeR2 },
    katsayilar,
    terimTestleri,
    modelOzeti: {
      ikiLLSifir,
      ikiLL: D,
      kiKare,
      sd: sdModel,
      p: pModel,
      coxSnellR2,
      nagelkerkeR2,
      mcFaddenR2,
      AIC,
      BIC,
      logOlabilirlik: logL,
      iterasyon: uyum.iterasyon,
      n,
    },
    hosmerLemeshow: hl,
    siniflama,
    auc,
    epv,
    blok0,
    tahminler,
    kovaryans: uyum.kovaryans,
    yakinsadi: uyum.cilaYakinsadi,
    ayrisma,
    bagimliSutunlar: bagimliAdlar,
    sutunAdlari: mm.sutunAdlari,
    terimler: mm.terimler,
    satirlar: mm.satirlar,
    duzey,
    uyarilar,
  };
}

/**
 * Tek değişkenli (univariable, "crude") lojistik regresyonlar: her tahminci için ayrı model.
 * Makalelerdeki "tek değişkenli → çok değişkenli" tablosunun ilk sütunu. Varsayılan olarak her model kendi tam
 * gözlemleriyle kurulur; `ortakOrneklem: true` ise tüm tahmincilerde tam olan satırlar kullanılır (çok değişkenli modelle
 * aynı örneklem).
 *
 * @param {{y:Array, olay?:any, tahminciler:object[], duzey?:number, ortakOrneklem?:boolean, profilGA?:boolean}} secenekler
 * @returns {{yontem, ad, sonuclar:Array<{ad, n, olaySayisi, katsayilar, lrKiKare, sd, p, waldP, AIC, uyarilar}>, uyarilar}}
 *   katsayilar: sabit hariç satırlar (lojistikRegresyon katsayı satırları); p: modelin olabilirlik oranı p'si (= terim LR p);
 *   waldP: terimin genel Wald p'si.
 */
export function tekDegiskenliLojistik(secenekler) {
  const { y, olay, tahminciler, duzey = 0.95, ortakOrneklem = false, profilGA: profil = true, kesim = 0.5 } = secenekler || {};
  if (!Array.isArray(tahminciler) || !tahminciler.length) throw new IstatistikHatasi('En az bir tahminci gerekir.');
  let yK = y;
  let tah = tahminciler;
  if (ortakOrneklem) {
    const satirlar = tamSatirlar([y, ...tahminciler.map((t) => t.degerler)]);
    yK = satirlar.map((i) => y[i]);
    tah = tahminciler.map((t) => tahminciSec(t, satirlar));
  }
  const sonuclar = [];
  const uyarilar = [];
  for (const t of tah) {
    try {
      const s = lojistikRegresyon({ y: yK, olay, tahminciler: [t], duzey, profilGA: profil, kesim, terimLR: false });
      const w = s.terimTestleri[0];
      sonuclar.push({
        ad: t.ad,
        n: s.n,
        olaySayisi: s.olaySayisi,
        katsayilar: s.katsayilar.filter((k) => k.terim !== null),
        lrKiKare: s.modelOzeti.kiKare,
        sd: s.modelOzeti.sd,
        p: s.modelOzeti.p,
        waldP: w.waldP,
        AIC: s.modelOzeti.AIC,
        uyarilar: s.uyarilar.filter((u) => !u.startsWith('Değişken başına olay')),
      });
    } catch (e) {
      if (!(e instanceof IstatistikHatasi)) throw e;
      sonuclar.push({ ad: t.ad, hata: e.message, katsayilar: [], uyarilar: [] });
      uyarilar.push(`'${t.ad}': ${e.message}`);
    }
  }
  return { yontem: 'tek-degiskenli-lojistik', ad: 'Tek değişkenli lojistik regresyonlar', sonuclar, uyarilar };
}

export { profilGA as profilGuvenAraligi, glmTanilama, waldTesti };

// ---------- Çok kategorili modeller: ortak yardımcılar ----------

/** F(x) = 1/(1+e^-x); taşmasız */
function plogis(x) {
  return lojistikPQ(x)[0];
}

/**
 * Newton-Raphson ile en büyükleme (adım yarılamalı). fn(θ) → {ll, g, H} (H: Hessian, negatif tanımlı) ya da geçersizse null.
 * Yakınsama: Newton azalımı gᵀ(-H)⁻¹g < 1e-20 ve ölçekli adım küçük. Ayrışmada adımlar sabit büyüklükte kalır → yakınsamaz.
 */
function newtonEnBuyuk(fn, theta0, olcek, maksIt = 300) {
  let theta = theta0.slice();
  let cur = fn(theta);
  if (!cur || !Number.isFinite(cur.ll)) throw new IstatistikHatasi('Başlangıç değerlerinde olabilirlik hesaplanamadı.');
  let yakinsadi = false;
  let tekil = false;
  let sonAdim = null;
  let llFark = Infinity;
  let it = 0;
  const m = theta.length;
  for (it = 1; it <= maksIt; it++) {
    let d;
    try {
      d = coz(cur.H.map((r) => r.map((v) => -v)), cur.g);
    } catch (e) {
      tekil = true;
      break;
    }
    if (d.some((v) => !Number.isFinite(v))) {
      tekil = true;
      break;
    }
    let azalim = 0;
    for (let j = 0; j < m; j++) azalim += cur.g[j] * d[j];
    const kucukAdim = d.every((v, j) => Math.abs(v) * olcek[j] <= 1e-6 * (1 + Math.abs(theta[j]) * olcek[j]));
    let t = 1;
    let yeni = null;
    let thetaYeni = null;
    for (let h = 0; h < 50; h++) {
      thetaYeni = theta.map((v, j) => v + t * d[j]);
      yeni = fn(thetaYeni);
      if (yeni && Number.isFinite(yeni.ll) && yeni.ll >= cur.ll - 1e-11 * (1 + Math.abs(cur.ll))) break;
      yeni = null;
      t /= 2;
    }
    if (!yeni) {
      // yuvarlama düzeyinde iyileşme kalmadı
      if (kucukAdim) yakinsadi = true;
      break;
    }
    sonAdim = thetaYeni.map((v, j) => Math.abs(v - theta[j]) * olcek[j]);
    llFark = Math.abs(yeni.ll - cur.ll);
    theta = thetaYeni;
    cur = yeni;
    if (azalim < 1e-20 && kucukAdim) {
      yakinsadi = true;
      break;
    }
  }
  return { theta, ...cur, iterasyon: it, yakinsadi, tekil, sonAdim, llFark };
}

/** Çok kategorili sonuç: düzeyleri belirler, her gözlemin kategori indeksini döndürür. Boş düzeyler atılır. */
function kategorikSonuc(degerler, duzeyler, uyarilar) {
  let liste;
  if (Array.isArray(duzeyler) && duzeyler.length) {
    const tanimli = new Set(duzeyler.map(String));
    for (const v of degerler) {
      if (!tanimli.has(String(v))) throw new IstatistikHatasi(`Bağımlı değişkende tanımlı düzeyler dışında bir değer var: '${v}'.`);
    }
    const mevcut = new Set(degerler.map(String));
    const bos = duzeyler.filter((d) => !mevcut.has(String(d)));
    if (bos.length) uyarilar.push(`Bağımlı değişkenin şu düzeylerinde gözlem yok, analizden çıkarıldı: ${bos.join(', ')}.`);
    liste = duzeyler.filter((d) => mevcut.has(String(d)));
  } else {
    liste = duzeySirala(degerler);
  }
  const konum = new Map(liste.map((d, k) => [String(d), k]));
  return { duzeyler: liste, kod: degerler.map((v) => konum.get(String(v))) };
}

/** Doğrusal bağımlı sütunları (sabit dahil, lm toleransı) ayıklar */
function bagimsizSutunlar(mm) {
  const X = mm.X;
  const p = mm.sutunAdlari.length;
  const yapi = p ? enKucukKareler(X, new Array(X.length).fill(0)) : { bagimli: [] };
  const bag = new Set(yapi.bagimli);
  const kullanilan = [];
  for (let j = 0; j < p; j++) if (!bag.has(j)) kullanilan.push(j);
  return { kullanilan, bagimli: yapi.bagimli };
}

function sutunOlcek(X) {
  const r = X.length ? X[0].length : 0;
  const o = new Array(r).fill(0);
  for (const xi of X) for (let j = 0; j < r; j++) o[j] = Math.max(o[j], Math.abs(xi[j]));
  return o.map((v) => (v > 0 ? v : 1));
}

function katsayiSatiri(ad, B, SH, z, ek = {}) {
  if (B === null || SH === null || !Number.isFinite(SH)) {
    return { ad, B: null, SH: null, z: null, wald: null, sd: 1, p: null, OR: null, orGaAlt: null, orGaUst: null, gaAlt: null, gaUst: null, ...ek };
  }
  const zz = B / SH;
  return {
    ad,
    B,
    SH,
    z: zz,
    wald: zz * zz,
    sd: 1,
    p: 2 * pnorm(-Math.abs(zz)),
    OR: Math.exp(B),
    orGaAlt: Math.exp(B - z * SH),
    orGaUst: Math.exp(B + z * SH),
    gaAlt: B - z * SH,
    gaUst: B + z * SH,
    ...ek,
  };
}

function pseudoR2(ikiLLSifir, ikiLL, n) {
  const kiKare = ikiLLSifir - ikiLL;
  const coxSnellR2 = 1 - Math.exp(-kiKare / n);
  return { coxSnellR2, nagelkerkeR2: coxSnellR2 / (1 - Math.exp(-ikiLLSifir / n)), mcFaddenR2: 1 - ikiLL / ikiLLSifir };
}

// ---------- Ordinal lojistik (orantılı odds) ----------

/** P = F(a) − F(b) (a > b); kuyruklarda kayıpsız */
function aralikOlasiligi(a, b) {
  if (b >= 0) return plogis(-b) - plogis(-a);
  return plogis(a) - plogis(b);
}

/**
 * Kümülatif logit olabilirliği, gradyanı ve Hessian'ı: logit P(Y ≤ k) = θ_k − xᵀβ_(k).
 * paralel = true → tüm eşiklerde ortak β (orantılı odds); false → her eşik için ayrı β (genel model).
 * Parametre düzeni: θ_1..θ_{K-1}, ardından β blok(lar)ı.
 */
function kumulatifOlabilirlik(theta, X, kod, K, paralel) {
  const n = X.length;
  const p = n ? X[0].length : 0;
  const m = theta.length;
  for (let k = 1; k < K - 1; k++) if (paralel && !(theta[k] > theta[k - 1])) return null;
  const g = new Array(m).fill(0);
  const H = sifir(m, m);
  let ll = 0;
  const blok = (k) => K - 1 + (paralel ? 0 : k * p);
  for (let i = 0; i < n; i++) {
    const c = kod[i];
    const xi = X[i];
    const sinir = [];
    if (c < K - 1) sinir.push({ k: c, s: 1 });
    if (c > 0) sinir.push({ k: c - 1, s: -1 });
    const arg = sinir.map(({ k }) => {
      let e = theta[k];
      const b0 = blok(k);
      for (let j = 0; j < p; j++) e -= xi[j] * theta[b0 + j];
      return e;
    });
    const a = c < K - 1 ? arg[0] : Infinity;
    const b = c > 0 ? arg[sinir.length - 1] : -Infinity;
    const P = aralikOlasiligi(a, b);
    if (!(P > 0)) return null;
    ll += Math.log(P);
    // u = ∂P/∂θ = Σ s f(arg) v,  ∂²P = Σ s f'(arg) v vᵀ,  v = (e_θk, −x [blok])
    const idx = [];
    const uv = [];
    const ekle = (j, v) => {
      const yer = idx.indexOf(j);
      if (yer < 0) {
        idx.push(j);
        uv.push(v);
      } else uv[yer] += v;
    };
    const turev = [];
    sinir.forEach(({ k, s }, r) => {
      const F = plogis(arg[r]);
      const Fm = plogis(-arg[r]);
      const f = F * Fm;
      turev.push({ k, s, fp: f * (Fm - F) });
      ekle(k, s * f);
      const b0 = blok(k);
      for (let j = 0; j < p; j++) ekle(b0 + j, -s * f * xi[j]);
    });
    for (let r = 0; r < idx.length; r++) {
      g[idx[r]] += uv[r] / P;
      const Hr = H[idx[r]];
      for (let q = 0; q < idx.length; q++) Hr[idx[q]] -= (uv[r] * uv[q]) / (P * P);
    }
    for (const { k, s, fp } of turev) {
      const b0 = blok(k);
      const vi = [k];
      const vv = [1];
      for (let j = 0; j < p; j++) {
        vi.push(b0 + j);
        vv.push(-xi[j]);
      }
      const c2 = (s * fp) / P;
      for (let r = 0; r < vi.length; r++) {
        if (vv[r] === 0) continue;
        const Hr = H[vi[r]];
        for (let q = 0; q < vi.length; q++) Hr[vi[q]] += c2 * vv[r] * vv[q];
      }
    }
  }
  return { ll, g, H };
}

/**
 * Ordinal lojistik regresyon (orantılı odds / kümülatif logit modeli): logit P(Y ≤ k) = θ_k − xᵀβ.
 * R karşılığı: `MASS::polr(method = "logistic")`, `ordinal::clm` (aynı parametreleme ve işaret); SPSS karşılığı: PLUM
 * (Threshold = θ_k, Location = β). Pozitif β daha yüksek kategorilere kaymayı gösterir; OR = exp(β): daha yüksek kategoride
 * olma (Y > k) odds oranı, tüm k için ortak. Standart hatalar gözlenen bilgi matrisinden (analitik Hessian; clm gibi).
 *
 * Orantılı odds varsayımı: (1) SPSS PLUM "Test of Parallel Lines" — eşik başına ayrı β'lı genel kümülatif modelle olabilirlik
 * oranı testi (= ordinal::clm(nominal = ~ x) ile LR); (2) Brant (1990) Wald testi, genel ve sütun bazında (= brant::brant).
 * Not: SPSS PLUM −2LL'yi kovaryat örüntüleri üzerinden "çekirdek" olabilirlikle verir; tekrarlı örüntüler varsa −2LL bir sabit
 * kadar farklı olabilir; χ², sd ve p aynıdır. Burada tam (R) olabilirlik kullanılır.
 *
 * @param {{y:Array, duzeyler?:Array, tahminciler:object[], etkilesimler?:string[][], duzey?:number, paralelTest?:boolean,
 *   terimLR?:boolean}} secenekler duzeyler: bağımlı değişkenin sıralı düzeyleri (verilmezse sayılar artan / alfabetik)
 * @returns {{yontem, ad, istatistik, istatistikAdi, sd, p, n, duzeyler, frekanslar,
 *   esikler:Array<{ad, duzey, B, SH, z, wald, sd, p, gaAlt, gaUst}>,
 *   katsayilar:Array<{ad, terim, duzey, B, SH, z, wald, sd, p, OR, orGaAlt, orGaUst, gaAlt, gaUst, bagimli}>,
 *   terimTestleri:Array<{ad, tur, sd, wald, waldP, lrKiKare, lrP}>, modelOzeti:{ikiLLSifir, ikiLL, kiKare, sd, p, coxSnellR2,
 *   nagelkerkeR2, mcFaddenR2, AIC, BIC, logOlabilirlik, iterasyon, n}, paralelCizgiler:{ikiLLGenel, kiKare, sd, p}|null,
 *   brant:{genel:{kiKare, sd, p}, degiskenler:Array<{ad, kiKare, sd, p}>}|null, kovaryans, parametreAdlari,
 *   tahminler:{olasiliklar}, yakinsadi, uyarilar}}
 */
export function ordinalLojistik(secenekler) {
  const { y, duzeyler: yDuzeyler, tahminciler = [], etkilesimler = [], duzey = 0.95, paralelTest = true, terimLR = true } = secenekler || {};
  if (!Array.isArray(y)) throw new IstatistikHatasi('Bağımlı değişken (y) bir dizi olmalıdır.');
  if (!tahminciler.length) throw new IstatistikHatasi('Ordinal lojistik regresyon için en az bir tahminci gerekir.');
  const mm = modelMatrisi(tahminciler, { sabit: true, etkilesimler, ekDiziler: [y] });
  const uyarilar = [...mm.uyarilar];
  const { duzeyler, kod } = kategorikSonuc(mm.ekler[0], yDuzeyler, uyarilar);
  const K = duzeyler.length;
  if (K < 3) {
    throw new IstatistikHatasi(`Ordinal lojistik regresyon için bağımlı değişken en az üç sıralı düzey içermelidir (${K} bulundu); iki düzey için ikili lojistik regresyon kullanın.`);
  }
  const n = kod.length;
  const frekanslar = duzeyler.map((_, k) => kod.filter((c) => c === k).length);
  const { kullanilan, bagimli } = bagimsizSutunlar(mm);
  const tahminSutun = kullanilan.filter((j) => j !== 0);
  const X = mm.X.map((s) => tahminSutun.map((j) => s[j]));
  const p = tahminSutun.length;
  const bagimliAdlar = bagimli.map((j) => mm.sutunAdlari[j]);
  if (bagimliAdlar.length) uyarilar.push(`Tam çoklu doğrusal bağlantı: ${bagimliAdlar.map((a) => `'${a}'`).join(', ')} diğer değişkenlerin doğrusal bileşimi olduğu için modelden çıkarıldı.`);

  // eşik başlangıçları: marjinal kümülatif oranların logiti
  const kum = [];
  let t = 0;
  for (let k = 0; k < K - 1; k++) {
    t += frekanslar[k];
    kum.push(Math.log(t / (n - t)));
  }
  const uydur = (Xm, bas) => newtonEnBuyuk((th) => kumulatifOlabilirlik(th, Xm, kod, K, true), bas, [...new Array(K - 1).fill(1), ...sutunOlcek(Xm)]);
  const fit = uydur(X, [...kum, ...new Array(p).fill(0)]);
  const ikiLL = -2 * fit.ll;
  let ikiLLSifir = 0;
  for (const f of frekanslar) ikiLLSifir -= 2 * f * Math.log(f / n);
  const iraksayan = new Set();
  if (!fit.yakinsadi) {
    (fit.sonAdim || []).forEach((v, j) => v > 1e-3 && iraksayan.add(j));
    uyarilar.push(
      iraksayan.size
        ? 'Tam/yarı-tam ayrışma saptandı (bazı katsayılar sonsuza ıraksıyor; boş hücreler olabilir); bu katsayılar raporlanmadı.'
        : 'Model yakınsamadı; sonuçlar güvenilir olmayabilir.',
    );
  }
  const m = K - 1 + p;
  let V = null;
  try {
    V = simetrikTers(fit.H.map((r) => r.map((v) => -v)));
  } catch (e) {
    uyarilar.push('Bilgi matrisi tekil; standart hatalar hesaplanamadı.');
  }
  const z = qnorm(1 - (1 - duzey) / 2);
  const se = (j) => (V && !iraksayan.has(j) && V[j][j] > 0 ? Math.sqrt(V[j][j]) : null);
  const esikler = [];
  for (let k = 0; k < K - 1; k++) {
    const s = katsayiSatiri(`${duzeyler[k]}|${duzeyler[k + 1]}`, iraksayan.has(k) ? null : fit.theta[k], se(k), z, { duzey: duzeyler[k] });
    delete s.OR;
    delete s.orGaAlt;
    delete s.orGaUst;
    esikler.push(s);
  }
  const katsayilar = [];
  for (let j = 1; j < mm.sutunAdlari.length; j++) {
    const bilgi = mm.sutunlar[j];
    const k = tahminSutun.indexOf(j);
    const r = K - 1 + k;
    const ek = { terim: bilgi.terim, duzey: bilgi.duzey, bagimli: k < 0, ayrisma: k >= 0 && iraksayan.has(r) };
    katsayilar.push(k < 0 || ek.ayrisma ? katsayiSatiri(mm.sutunAdlari[j], null, null, z, ek) : katsayiSatiri(mm.sutunAdlari[j], fit.theta[r], se(r), z, ek));
  }
  const kiKare = ikiLLSifir - ikiLL;
  const pModel = p > 0 ? pchisq(Math.max(kiKare, 0), p, false) : null;

  // Terim testleri: Wald (çok sütunlu terimler) ve LR (terimi çıkarma)
  const terimTestleri = [];
  for (const tr of mm.terimler) {
    const idx = tr.sutunlar.map((j) => tahminSutun.indexOf(j)).filter((k) => k >= 0);
    const satir = { ad: tr.ad, tur: tr.tur, sd: idx.length, wald: null, waldP: null, lrKiKare: null, lrP: null };
    if (idx.length && V) {
      const katsayi = fit.theta.map((v, r) => (iraksayan.has(r) ? null : v));
      const w = waldTesti(katsayi, V, idx.map((k) => K - 1 + k));
      satir.wald = w.wald;
      satir.waldP = w.p;
    }
    if (idx.length && terimLR) {
      const kalan = [...Array(p).keys()].filter((k) => !idx.includes(k));
      const Xk = X.map((s) => kalan.map((k) => s[k]));
      const f = uydur(Xk, [...fit.theta.slice(0, K - 1), ...kalan.map((k) => fit.theta[K - 1 + k])]);
      satir.lrKiKare = -2 * f.ll - ikiLL;
      satir.lrP = pchisq(Math.max(satir.lrKiKare, 0), idx.length, false);
    }
    terimTestleri.push(satir);
  }

  // Orantılı odds varsayımı
  let paralelCizgiler = null;
  let brant = null;
  if (paralelTest && p > 0 && fit.yakinsadi) {
    const bas = fit.theta.slice(0, K - 1);
    const olcekG = new Array(K - 1).fill(1);
    const so = sutunOlcek(X);
    for (let k = 0; k < K - 1; k++) {
      bas.push(...fit.theta.slice(K - 1));
      olcekG.push(...so);
    }
    let genel = null;
    try {
      genel = newtonEnBuyuk((th) => kumulatifOlabilirlik(th, X, kod, K, false), bas, olcekG);
    } catch (e) {
      genel = null;
    }
    // Genel modelde boş hücre/ayrışma varsa bazı eşik katsayıları sonsuza gider ama log-olabilirlik sonlu bir üst sınıra
    // yakınsar; LR testi bu üst sınırla hesaplanır (ordinal::clm(nominal) de aynı değeri verir).
    const sinirda = genel && !genel.yakinsadi && genel.llFark < 1e-9 * (1 + Math.abs(genel.ll));
    if (genel && (genel.yakinsadi || sinirda)) {
      if (sinirda) uyarilar.push('Paralel çizgiler testindeki genel modelde bazı katsayılar sonsuza ıraksıyor (boş hücreler); test istatistiği olabilirliğin üst sınırıyla hesaplandı, yorumlarken dikkatli olun.');
      const ikiLLGenel = -2 * genel.ll;
      const kk = ikiLL - ikiLLGenel;
      const sd = p * (K - 2);
      paralelCizgiler = { ikiLLGenel, kiKare: kk, sd, p: pchisq(Math.max(kk, 0), sd, false) };
      if (paralelCizgiler.p < 0.05) {
        uyarilar.push('Orantılı odds (paralel çizgiler) varsayımı reddedildi (p < 0,05); kısmi orantılı odds ya da multinominal model düşünülebilir.');
      }
    } else {
      uyarilar.push('Paralel çizgiler testi için genel (eşik başına ayrı katsayılı) model yakınsamadı; test raporlanmadı.');
    }
    brant = brantTesti(X, kod, K, tahminSutun.map((j) => mm.sutunAdlari[j]));
    if (!brant) uyarilar.push('Brant testi hesaplanamadı: eşik başına ikili modellerden birinde ayrışma (boş hücre) var.');
  }

  // Tahmin olasılıkları
  const olasiliklar = X.map((xi) => {
    let eta = 0;
    for (let j = 0; j < p; j++) eta += xi[j] * fit.theta[K - 1 + j];
    const pr = [];
    for (let k = 0; k < K; k++) pr.push(aralikOlasiligi(k < K - 1 ? fit.theta[k] - eta : Infinity, k > 0 ? fit.theta[k - 1] - eta : -Infinity));
    return pr;
  });
  return {
    yontem: 'ordinal-lojistik',
    ad: 'Ordinal lojistik regresyon (orantılı odds)',
    istatistik: kiKare,
    istatistikAdi: 'χ²',
    sd: p,
    p: pModel,
    n,
    duzeyler,
    frekanslar,
    esikler,
    katsayilar,
    terimTestleri,
    modelOzeti: {
      ikiLLSifir,
      ikiLL,
      kiKare,
      sd: p,
      p: pModel,
      ...pseudoR2(ikiLLSifir, ikiLL, n),
      AIC: ikiLL + 2 * m,
      BIC: ikiLL + Math.log(n) * m,
      logOlabilirlik: fit.ll,
      iterasyon: fit.iterasyon,
      n,
    },
    paralelCizgiler,
    brant,
    kovaryans: V,
    parametreAdlari: [...esikler.map((e) => e.ad), ...tahminSutun.map((j) => mm.sutunAdlari[j])],
    tahminler: { olasiliklar },
    yakinsadi: fit.yakinsadi,
    bagimliSutunlar: bagimliAdlar,
    sutunAdlari: mm.sutunAdlari,
    terimler: mm.terimler,
    satirlar: mm.satirlar,
    duzey,
    uyarilar,
  };
}

/**
 * Brant (1990) orantılı odds testi: K−1 ikili lojistik modelin (Y > k) eğim katsayılarının eşitliği için Wald testi
 * (brant::brant ile aynı: ikili modellerin ortak kovaryansı (XᵀW_kk X)⁻¹ XᵀW_kl X (XᵀW_ll X)⁻¹, W_kl = diag(π_l(1 − π_k)), l ≥ k).
 */
function brantTesti(X, kod, K, adlar) {
  const n = X.length;
  const p = X[0].length;
  const X1 = X.map((s) => [1, ...s]);
  const q = p + 1;
  const M = K - 1;
  const modeller = [];
  for (let k = 0; k < M; k++) {
    const yk = kod.map((c) => (c > k ? 1 : 0));
    if (!yk.some((v) => v === 1) || !yk.some((v) => v === 0)) return null;
    const f = glmIRLS(X1, yk, AILELER.binom, {});
    if (!f.cilaYakinsadi || !f.XtXTers) return null;
    modeller.push({ beta: f.beta, pi: f.eta.map(plogis), Vinv: f.XtXTers });
  }
  const Vs = sifir(q * M, q * M);
  for (let k = 0; k < M; k++) {
    for (let l = k; l < M; l++) {
      const W = sifir(q, q);
      for (let i = 0; i < n; i++) {
        const w = modeller[l].pi[i] * (1 - modeller[k].pi[i]);
        const xi = X1[i];
        for (let a = 0; a < q; a++) for (let b = 0; b < q; b++) W[a][b] += w * xi[a] * xi[b];
      }
      const blok = carp(carp(modeller[k].Vinv, W), modeller[l].Vinv);
      for (let a = 0; a < q; a++) {
        for (let b = 0; b < q; b++) {
          Vs[k * q + a][l * q + b] = blok[a][b];
          Vs[l * q + b][k * q + a] = blok[a][b];
        }
      }
    }
  }
  const secim = [];
  for (let k = 0; k < M; k++) for (let a = 1; a < q; a++) secim.push(k * q + a);
  const bStar = [];
  for (let k = 0; k < M; k++) bStar.push(...modeller[k].beta.slice(1));
  const VStar = secim.map((r) => secim.map((c) => Vs[r][c]));
  const testEt = (degiskenler) => {
    const D = [];
    for (const j of degiskenler) {
      for (let k = 1; k < M; k++) {
        const satir = new Array(p * M).fill(0);
        satir[j] = 1;
        satir[k * p + j] = -1;
        D.push(satir);
      }
    }
    const Db = D.map((s) => s.reduce((a, v, i) => a + v * bStar[i], 0));
    const DVD = carp(carp(D, VStar), transpoz(D));
    const cozum = coz(DVD, Db);
    const kk = Db.reduce((a, v, i) => a + v * cozum[i], 0);
    return { kiKare: kk, sd: D.length, p: pchisq(Math.max(kk, 0), D.length, false) };
  };
  try {
    return { genel: testEt([...Array(p).keys()]), degiskenler: adlar.map((ad, j) => ({ ad, ...testEt([j]) })) };
  } catch (e) {
    return null;
  }
}

// ---------- Multinominal lojistik ----------

function multinomOlabilirlik(theta, X, kod, K, ref) {
  const n = X.length;
  const p = X[0].length;
  const M = K - 1;
  const m = M * p;
  const g = new Array(m).fill(0);
  const H = sifir(m, m);
  let ll = 0;
  const sira = new Array(K).fill(-1); // kategori → parametre bloğu
  let a0 = 0;
  for (let k = 0; k < K; k++) if (k !== ref) sira[k] = a0++;
  const P = new Array(M);
  const eta = new Array(M);
  for (let i = 0; i < n; i++) {
    const xi = X[i];
    let enb = 0;
    for (let a = 0; a < M; a++) {
      let s = 0;
      for (let j = 0; j < p; j++) s += xi[j] * theta[a * p + j];
      eta[a] = s;
      if (s > enb) enb = s;
    }
    let payda = Math.exp(-enb);
    for (let a = 0; a < M; a++) payda += Math.exp(eta[a] - enb);
    const logPayda = enb + Math.log(payda);
    for (let a = 0; a < M; a++) P[a] = Math.exp(eta[a] - logPayda);
    const ca = sira[kod[i]];
    ll += (ca >= 0 ? eta[ca] : 0) - logPayda;
    for (let a = 0; a < M; a++) {
      const r = (ca === a ? 1 : 0) - P[a];
      for (let j = 0; j < p; j++) g[a * p + j] += r * xi[j];
      for (let b = a; b < M; b++) {
        const w = P[a] * ((a === b ? 1 : 0) - P[b]);
        if (w === 0) continue;
        for (let j = 0; j < p; j++) {
          const wx = w * xi[j];
          if (wx === 0) continue;
          const Hr = H[a * p + j];
          for (let l = 0; l < p; l++) Hr[b * p + l] -= wx * xi[l];
        }
      }
    }
  }
  for (let r = 0; r < m; r++) for (let c = 0; c < r; c++) H[r][c] = H[c][r];
  return { ll, g, H };
}

/**
 * Multinominal (nominal) lojistik regresyon: log(P_k / P_ref) = xᵀβ_k (temel kategori logiti), Newton-Raphson.
 * R karşılığı: `nnet::multinom`, `VGAM::vglm(family = multinomial(refLevel = ...))`; SPSS karşılığı: NOMREG
 * (Parameter Estimates, Likelihood Ratio Tests, Model Fitting Information, Pseudo R-Square, Classification).
 * Referans kategori varsayılanı İLK düzeydir (nnet); SPSS NOMREG varsayılanı SON kategoridir — `referansKategori` ile seçin.
 * Not: SPSS NOMREG −2LL'yi kovaryat örüntüleri üzerinden çekirdek olabilirlikle verir; tekrarlı örüntülerde −2LL bir sabit
 * kadar farklı olabilir; χ², sd ve p aynıdır.
 *
 * @param {{y:Array, duzeyler?:Array, referansKategori?:any, tahminciler:object[], etkilesimler?:string[][], duzey?:number,
 *   sabit?:boolean}} secenekler
 * @returns {{yontem, ad, istatistik, istatistikAdi, sd, p, n, duzeyler, referansKategori, frekanslar,
 *   kategoriler:Array<{kategori, katsayilar:Array<{ad, terim, duzey, B, SH, z, wald, sd, p, OR, orGaAlt, orGaUst, gaAlt, gaUst,
 *   bagimli, ayrisma}>}>, olabilirlikOraniTestleri:Array<{ad, ikiLLIndirgenmis, kiKare, sd, p}>,
 *   terimWaldTestleri:Array<{ad, wald, sd, p}>, modelOzeti:{ikiLLSifir, ikiLL, kiKare, sd, p, coxSnellR2, nagelkerkeR2,
 *   mcFaddenR2, AIC, BIC, logOlabilirlik, iterasyon, n}, siniflama:{tablo, dogruYuzde, genelYuzde},
 *   tahminler:{olasiliklar, tahminiKategori}, kovaryans, yakinsadi, uyarilar}}
 */
export function multinomialLojistik(secenekler) {
  const { y, duzeyler: yDuzeyler, referansKategori, tahminciler = [], etkilesimler = [], duzey = 0.95, sabit = true } = secenekler || {};
  if (!Array.isArray(y)) throw new IstatistikHatasi('Bağımlı değişken (y) bir dizi olmalıdır.');
  const mm = modelMatrisi(tahminciler, { sabit, etkilesimler, ekDiziler: [y] });
  const uyarilar = [...mm.uyarilar];
  const { duzeyler, kod } = kategorikSonuc(mm.ekler[0], yDuzeyler, uyarilar);
  const K = duzeyler.length;
  if (K < 2) throw new IstatistikHatasi('Bağımlı değişkende en az iki kategori olmalıdır.');
  if (K === 2) uyarilar.push('Bağımlı değişken iki kategorili; multinominal model ikili lojistik regresyonla aynıdır.');
  let ref = 0;
  if (referansKategori !== undefined && referansKategori !== null) {
    ref = duzeyler.findIndex((d) => String(d) === String(referansKategori));
    if (ref < 0) throw new IstatistikHatasi(`Referans kategori ('${referansKategori}') bağımlı değişkende yok.`);
  }
  const n = kod.length;
  const frekanslar = duzeyler.map((_, k) => kod.filter((c) => c === k).length);
  const { kullanilan, bagimli } = bagimsizSutunlar(mm);
  const X = mm.X.map((s) => kullanilan.map((j) => s[j]));
  const p = kullanilan.length;
  if (p === 0) throw new IstatistikHatasi('Modelde hiç terim yok.');
  const bagimliAdlar = bagimli.map((j) => mm.sutunAdlari[j]);
  if (bagimliAdlar.length) uyarilar.push(`Tam çoklu doğrusal bağlantı: ${bagimliAdlar.map((a) => `'${a}'`).join(', ')} diğer değişkenlerin doğrusal bileşimi olduğu için modelden çıkarıldı.`);
  const kats = [];
  for (let k = 0; k < K; k++) if (k !== ref) kats.push(k);
  const M = K - 1;
  const so = sutunOlcek(X);
  const olcek = [];
  for (let a = 0; a < M; a++) olcek.push(...so);
  const sabitVar = sabit && kullanilan[0] === 0;
  const bas = new Array(M * p).fill(0);
  if (sabitVar) kats.forEach((k, a) => (bas[a * p] = Math.log(frekanslar[k] / frekanslar[ref])));
  const uydur = (Xm, b0, ol) => newtonEnBuyuk((th) => multinomOlabilirlik(th, Xm, kod, K, ref), b0, ol);
  const fit = uydur(X, bas, olcek);
  const ikiLL = -2 * fit.ll;
  let ikiLLSifir = 0;
  if (sabit) for (const f of frekanslar) ikiLLSifir -= 2 * f * Math.log(f / n);
  else ikiLLSifir = 2 * n * Math.log(K);
  const iraksayan = new Set();
  if (!fit.yakinsadi) {
    (fit.sonAdim || []).forEach((v, j) => v > 1e-3 && iraksayan.add(j));
    uyarilar.push(
      iraksayan.size
        ? 'Tam/yarı-tam ayrışma saptandı: bazı katsayılar sonsuza ıraksıyor (bir kategoride hiç gözlem olmayan hücreler olabilir); bu katsayılar raporlanmadı.'
        : 'Model yakınsamadı; sonuçlar güvenilir olmayabilir.',
    );
  }
  let V = null;
  try {
    V = simetrikTers(fit.H.map((r) => r.map((v) => -v)));
  } catch (e) {
    uyarilar.push('Bilgi matrisi tekil; standart hatalar hesaplanamadı.');
  }
  const z = qnorm(1 - (1 - duzey) / 2);
  const kategoriler = kats.map((k, a) => ({
    kategori: duzeyler[k],
    katsayilar: mm.sutunAdlari.map((ad, j) => {
      const idx = kullanilan.indexOf(j);
      const ek = { terim: mm.sutunlar[j].terim, duzey: mm.sutunlar[j].duzey, bagimli: idx < 0, ayrisma: idx >= 0 && iraksayan.has(a * p + idx) };
      if (idx < 0 || ek.ayrisma) return katsayiSatiri(ad, null, null, z, ek);
      const r = a * p + idx;
      return katsayiSatiri(ad, fit.theta[r], V && V[r][r] > 0 ? Math.sqrt(V[r][r]) : null, z, ek);
    }),
  }));
  const sdModel = M * (p - (sabitVar ? 1 : 0));
  const kiKare = ikiLLSifir - ikiLL;
  const pModel = sdModel > 0 ? pchisq(Math.max(kiKare, 0), sdModel, false) : null;

  // Olabilirlik oranı testleri (SPSS NOMREG "Likelihood Ratio Tests"): her terim çıkarılarak
  const olabilirlikOraniTestleri = [];
  const terimWaldTestleri = [];
  for (const tr of mm.terimler) {
    const idx = tr.sutunlar.map((j) => kullanilan.indexOf(j)).filter((k) => k >= 0);
    if (!idx.length) {
      olabilirlikOraniTestleri.push({ ad: tr.ad, ikiLLIndirgenmis: null, kiKare: null, sd: 0, p: null });
      terimWaldTestleri.push({ ad: tr.ad, wald: null, sd: 0, p: null });
      continue;
    }
    const kalan = [...Array(p).keys()].filter((k) => !idx.includes(k));
    let ikiLLRed;
    if (kalan.length) {
      const Xk = X.map((s) => kalan.map((k) => s[k]));
      const b0 = [];
      const ol = [];
      for (let a = 0; a < M; a++) {
        for (const k of kalan) {
          b0.push(fit.theta[a * p + k]);
          ol.push(so[k]);
        }
      }
      ikiLLRed = -2 * uydur(Xk, b0, ol).ll;
    } else ikiLLRed = 2 * n * Math.log(K);
    const sd = M * idx.length;
    const kk = ikiLLRed - ikiLL;
    olabilirlikOraniTestleri.push({ ad: tr.ad, ikiLLIndirgenmis: ikiLLRed, kiKare: kk, sd, p: pchisq(Math.max(kk, 0), sd, false) });
    if (V) {
      const sut = [];
      for (let a = 0; a < M; a++) for (const k of idx) sut.push(a * p + k);
      const w = waldTesti(fit.theta.map((v, r) => (iraksayan.has(r) ? null : v)), V, sut);
      terimWaldTestleri.push({ ad: tr.ad, wald: w.wald, sd: w.sd, p: w.p });
    }
  }

  // Tahminler ve sınıflama (en olası kategori)
  const olasiliklar = X.map((xi) => {
    const e = new Array(K).fill(0);
    const eta = kats.map((_, a) => {
      let s = 0;
      for (let j = 0; j < p; j++) s += xi[j] * fit.theta[a * p + j];
      return s;
    });
    const enb = Math.max(0, ...eta);
    e[ref] = Math.exp(-enb);
    kats.forEach((k, a) => (e[k] = Math.exp(eta[a] - enb)));
    const t = e.reduce((s, v) => s + v, 0);
    return e.map((v) => v / t);
  });
  const tahminiIndeks = olasiliklar.map((pr) => pr.indexOf(Math.max(...pr)));
  const tablo = duzeyler.map(() => new Array(K).fill(0));
  kod.forEach((c, i) => tablo[c][tahminiIndeks[i]]++);
  const dogruYuzde = tablo.map((r, k) => (frekanslar[k] ? (100 * r[k]) / frekanslar[k] : null));
  const genelYuzde = (100 * tablo.reduce((s, r, k) => s + r[k], 0)) / n;
  const npar = M * p;
  return {
    yontem: 'multinomial-lojistik',
    ad: 'Multinominal lojistik regresyon',
    istatistik: kiKare,
    istatistikAdi: 'χ²',
    sd: sdModel,
    p: pModel,
    n,
    duzeyler,
    referansKategori: duzeyler[ref],
    frekanslar,
    kategoriler,
    olabilirlikOraniTestleri,
    terimWaldTestleri,
    modelOzeti: {
      ikiLLSifir,
      ikiLL,
      kiKare,
      sd: sdModel,
      p: pModel,
      ...pseudoR2(ikiLLSifir, ikiLL, n),
      AIC: ikiLL + 2 * npar,
      BIC: ikiLL + Math.log(n) * npar,
      logOlabilirlik: fit.ll,
      iterasyon: fit.iterasyon,
      n,
    },
    siniflama: { tablo, dogruYuzde, genelYuzde },
    tahminler: { olasiliklar, tahminiKategori: tahminiIndeks.map((k) => duzeyler[k]) },
    kovaryans: V,
    yakinsadi: fit.yakinsadi,
    bagimliSutunlar: bagimliAdlar,
    sutunAdlari: mm.sutunAdlari,
    terimler: mm.terimler,
    satirlar: mm.satirlar,
    duzey,
    uyarilar,
  };
}
