// Uyum (agreement) istatistikleri: Cohen kappa (ağırlıklı), Fleiss kappa, sınıf içi korelasyon (ICC),
// Bland-Altman analizi, Lin'in uyum korelasyon katsayısı, yüzde uyum.

import { pnorm, qnorm, qt, pt, pnt, qf, pf } from './dagilim.js';
import { tersCoz } from './sayisal.js';
import { ortalama, varyans } from './betimsel.js';
import { IstatistikHatasi, gerekli } from './hata.js';

// ---------- Yardımcılar ----------

const eksik = (v) => v === null || v === undefined || v === '' || (typeof v === 'number' && !Number.isFinite(v));

/** Kategori etiketlerini sıralar: tümü sayıysa sayısal, değilse Türkçe alfabetik. */
function duzeyleriSirala(degerler) {
  const tekil = [...new Set(degerler)];
  if (tekil.every((v) => typeof v === 'number')) return tekil.sort((a, b) => a - b);
  return tekil.sort((a, b) => String(a).localeCompare(String(b), 'tr', { numeric: true }));
}

function guvenDuzeyi(duzey) {
  gerekli(typeof duzey === 'number' && duzey > 0 && duzey < 1, 'Güven düzeyi 0 ile 1 arasında olmalıdır.');
  return duzey;
}

/** Landis & Koch (1977) kappa yorumu */
function kappaYorumu(k) {
  if (k < 0) return 'uyum yok (şans düzeyinin altında)';
  if (k <= 0.2) return 'önemsiz';
  if (k <= 0.4) return 'zayıf';
  if (k <= 0.6) return 'orta';
  if (k <= 0.8) return 'iyi';
  return 'çok iyi';
}

/** Koo & Li (2016) ICC yorumu */
function iccYorumu(r) {
  if (r < 0.5) return 'zayıf';
  if (r < 0.75) return 'orta';
  if (r < 0.9) return 'iyi';
  return 'mükemmel';
}

// ---------- Cohen kappa ----------

/**
 * İki değerlendirici için Cohen kappa; isteğe bağlı doğrusal ya da kuadratik ağırlıklı kappa.
 *
 * Girdi: aynı deneklere iki değerlendiricinin verdiği kategoriler (etiket ya da kod). Eksik çiftler atılır.
 * Kategori sırası (ağırlıklı kappa için önemlidir): `duzeyler` verilirse o sıra; yoksa tümü sayıysa sayısal sıra,
 * değilse alfabetik sıra. Ağırlıklar kategorilerin sıra numarasına göredir (c kategori, i, j = 1..c):
 *   'yok': w = 1 (i = j), 0 (i ≠ j); 'dogrusal': w = 1 − |i−j|/(c−1); 'kuadratik': w = 1 − (i−j)²/(c−1)².
 * (R irr::kappa2 weight = 'unweighted' | 'equal' | 'squared'; SPSS Weighted Kappa "Linear" | "Quadratic".)
 *
 * κ = (p_o − p_e)/(1 − p_e), p_o = Σ w_ij p_ij, p_e = Σ w_ij p_i. p_.j.
 * z ve p: H0 (κ = 0) altındaki standart hatayla (Fleiss, Cohen & Everitt 1969) — irr::kappa2 ile aynı; SPSS'in
 *   "Approx. T" değeri de bu SH ile hesaplanır.
 * Güven aralığı: H0 varsayılmadan hesaplanan asimptotik SH ile κ ± z·SH (Fleiss, Cohen & Everitt 1969) —
 *   SPSS'in "Asymptotic Std. Error" değeri ve vcd::Kappa'nın ASE'si ile aynı.
 * Not: irr::kappa2 sayısal kodları karakter olarak sıralar (10 < 2); burada sayısal sıra kullanılır.
 *
 * @param {Array<string|number>} r1
 * @param {Array<string|number>} r2
 * @param {{agirlik?:'yok'|'dogrusal'|'kuadratik', duzey?:number, duzeyler?:Array<string|number>}} [secenek]
 */
export function cohenKappa(r1, r2, secenek = {}) {
  const { agirlik = 'yok', duzey = 0.95 } = secenek;
  guvenDuzeyi(duzey);
  gerekli(['yok', 'dogrusal', 'kuadratik'].includes(agirlik), "Ağırlık 'yok', 'dogrusal' ya da 'kuadratik' olmalıdır.");
  gerekli(Array.isArray(r1) && Array.isArray(r2) && r1.length === r2.length, 'İki değerlendiricinin puan dizileri aynı uzunlukta olmalıdır.');
  const uyarilar = [];
  const a = [];
  const b = [];
  for (let i = 0; i < r1.length; i++) {
    if (eksik(r1[i]) || eksik(r2[i])) continue;
    a.push(r1[i]);
    b.push(r2[i]);
  }
  const n = a.length;
  if (n < r1.length) uyarilar.push(`Eksik değer içeren ${r1.length - n} çift analizden çıkarıldı.`);
  gerekli(n >= 2, 'Kappa için en az 2 tam çift gerekir.');
  let duzeyler;
  if (secenek.duzeyler) {
    duzeyler = [...secenek.duzeyler];
    const kume = new Set(duzeyler);
    gerekli(kume.size === duzeyler.length, 'Kategori listesinde tekrar var.');
    gerekli(a.every((v) => kume.has(v)) && b.every((v) => kume.has(v)), 'Verideki bazı kategoriler, verilen kategori listesinde yok.');
  } else {
    duzeyler = duzeyleriSirala([...a, ...b]);
    if (agirlik !== 'yok' && !duzeyler.every((v) => typeof v === 'number')) {
      uyarilar.push('Ağırlıklı kappa için kategoriler alfabetik sıralandı; sıralı ölçek düzenini kategori listesiyle (duzeyler) belirtin.');
    }
  }
  const c = duzeyler.length;
  gerekli(c >= 2, 'Kappa için en az iki farklı kategori gerekir.');
  const indeks = new Map(duzeyler.map((v, i) => [v, i]));
  const tablo = Array.from({ length: c }, () => new Array(c).fill(0));
  for (let i = 0; i < n; i++) tablo[indeks.get(a[i])][indeks.get(b[i])]++;

  const W = Array.from({ length: c }, (_, i) =>
    Array.from({ length: c }, (_, j) => {
      if (agirlik === 'dogrusal') return 1 - Math.abs(i - j) / (c - 1);
      if (agirlik === 'kuadratik') return 1 - ((i - j) / (c - 1)) ** 2;
      return i === j ? 1 : 0;
    }),
  );
  const p = tablo.map((s) => s.map((v) => v / n));
  const satir = p.map((s) => s.reduce((x, y) => x + y, 0));
  const sutun = duzeyler.map((_, j) => p.reduce((x, s) => x + s[j], 0));
  let po = 0;
  let pe = 0;
  let ham = 0;
  for (let i = 0; i < c; i++) {
    ham += p[i][i];
    for (let j = 0; j < c; j++) {
      po += W[i][j] * p[i][j];
      pe += W[i][j] * satir[i] * sutun[j];
    }
  }
  if (!(1 - pe > 1e-12)) throw new IstatistikHatasi('Beklenen uyum %100 olduğu için kappa hesaplanamaz (iki değerlendirici de tek kategori kullanmış).');
  const kappa = (po - pe) / (1 - pe);
  // Ağırlıklı satır/sütun ortalamaları: wi[i] = Σ_j p_.j w_ij, wj[j] = Σ_i p_i. w_ij
  const wi = duzeyler.map((_, i) => W[i].reduce((s, w, j) => s + w * sutun[j], 0));
  const wj = duzeyler.map((_, j) => W.reduce((s, sat, i) => s + sat[j] * satir[i], 0));
  let s0 = 0;
  let s1 = 0;
  for (let i = 0; i < c; i++) {
    for (let j = 0; j < c; j++) {
      s0 += satir[i] * sutun[j] * (W[i][j] - (wi[i] + wj[j])) ** 2;
      s1 += p[i][j] * (W[i][j] - (wi[i] + wj[j]) * (1 - kappa)) ** 2;
    }
  }
  const payda = n * (1 - pe) ** 2;
  const sh0 = Math.sqrt(Math.max(0, s0 - pe * pe) / payda);
  const sh = Math.sqrt(Math.max(0, s1 - (kappa - pe * (1 - kappa)) ** 2) / payda);
  const z = kappa / sh0;
  const zk = qnorm((1 + duzey) / 2);
  const ad = agirlik === 'yok' ? 'Cohen kappa' : agirlik === 'dogrusal' ? 'Ağırlıklı kappa (doğrusal)' : 'Ağırlıklı kappa (kuadratik)';
  if (n < 30) uyarilar.push('Örneklem küçük (n < 30); asimptotik standart hata ve güven aralığı yaklaşıktır.');
  return {
    yontem: agirlik === 'yok' ? 'cohen-kappa' : `agirlikli-kappa-${agirlik}`,
    ad,
    istatistik: z,
    istatistikAdi: 'z',
    sd: null,
    p: Math.min(1, 2 * pnorm(-Math.abs(z))),
    kappa,
    sh,
    sh0,
    etki: { ad, sembol: 'κ', deger: kappa, gaAlt: kappa - zk * sh, gaUst: kappa + zk * sh },
    ga: { duzey, alt: kappa - zk * sh, ust: kappa + zk * sh, neyin: 'kappa' },
    gozlenenUyum: po,
    beklenenUyum: pe,
    yuzdeUyum: 100 * ham,
    agirlik,
    agirliklar: W,
    tablo,
    duzeyler,
    n,
    yorum: kappaYorumu(kappa),
    uyarilar,
  };
}

// ---------- Fleiss kappa ----------

/**
 * Çok değerlendiricili (m ≥ 2) Fleiss kappa (Fleiss 1971). R: irr::kappam.fleiss(ratings, detail = TRUE).
 * Girdi: satır = denek, sütun = değerlendirici; hücreler kategori etiketi/kodu. Eksik değerli satırlar atılır.
 * SH (H0: κ = 0 altında, Fleiss, Nee & Landis 1979) z ve p için kullanılır; güven aralığı κ ± z·SH olarak verilir
 * (SPSS 28+ "Fleiss Multiple Rater Kappa" çıktısının mantığı; bu SH H0 altında türetildiğinden aralık yaklaşıktır).
 * Kategori bazında κ_j, SH_j = √(2/(n·m·(m−1))), z ve p de döner.
 * @param {Array<Array<string|number>>} matris
 * @param {{duzey?:number}} [secenek]
 */
export function fleissKappa(matris, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  guvenDuzeyi(duzey);
  gerekli(Array.isArray(matris) && matris.length > 0, 'Veri matrisi boş.');
  const m = matris[0].length;
  gerekli(m >= 2 && matris.every((s) => s.length === m), 'Her satırda aynı sayıda değerlendirici olmalıdır (en az 2).');
  const uyarilar = [];
  const satirlar = matris.filter((s) => s.every((v) => !eksik(v)));
  if (satirlar.length < matris.length) uyarilar.push(`Eksik değer içeren ${matris.length - satirlar.length} denek analizden çıkarıldı.`);
  const n = satirlar.length;
  gerekli(n >= 2, 'Fleiss kappa için en az 2 tam denek gerekir.');
  const duzeyler = duzeyleriSirala(satirlar.flat());
  const c = duzeyler.length;
  gerekli(c >= 2, 'Fleiss kappa için en az iki farklı kategori gerekir.');
  const indeks = new Map(duzeyler.map((v, i) => [v, i]));
  const sayim = satirlar.map((s) => {
    const t = new Array(c).fill(0);
    for (const v of s) t[indeks.get(v)]++;
    return t;
  });
  let Pbar = 0;
  for (const t of sayim) Pbar += (t.reduce((x, v) => x + v * v, 0) - m) / (m * (m - 1));
  Pbar /= n;
  const pj = duzeyler.map((_, j) => sayim.reduce((s, t) => s + t[j], 0) / (n * m));
  const Pe = pj.reduce((s, v) => s + v * v, 0);
  if (!(1 - Pe > 1e-12)) throw new IstatistikHatasi('Tüm değerlendirmeler aynı kategoride; kappa hesaplanamaz.');
  const kappa = (Pbar - Pe) / (1 - Pe);
  const pq = pj.reduce((s, p) => s + p * (1 - p), 0);
  const pqqp = pj.reduce((s, p) => s + p * (1 - p) * (1 - 2 * p), 0);
  const sh = Math.sqrt((2 / (pq * pq * n * m * (m - 1))) * (pq * pq - pqqp));
  const z = kappa / sh;
  const zk = qnorm((1 + duzey) / 2);
  const shK = Math.sqrt(2 / (n * m * (m - 1)));
  const kategoriler = duzeyler.map((d, j) => {
    const toplamKare = sayim.reduce((s, t) => s + t[j] * t[j], 0);
    const pjk = (toplamKare - n * m * pj[j]) / (n * m * (m - 1) * pj[j]);
    const k = (pjk - pj[j]) / (1 - pj[j]);
    const zz = k / shK;
    return { kategori: d, oran: pj[j], kappa: k, sh: shK, z: zz, p: Math.min(1, 2 * pnorm(-Math.abs(zz))) };
  });
  return {
    yontem: 'fleiss-kappa',
    ad: 'Fleiss kappa',
    istatistik: z,
    istatistikAdi: 'z',
    sd: null,
    p: Math.min(1, 2 * pnorm(-Math.abs(z))),
    kappa,
    sh,
    etki: { ad: 'Fleiss kappa', sembol: 'κ', deger: kappa, gaAlt: kappa - zk * sh, gaUst: kappa + zk * sh },
    ga: { duzey, alt: kappa - zk * sh, ust: kappa + zk * sh, neyin: 'kappa' },
    gozlenenUyum: Pbar,
    beklenenUyum: Pe,
    kategoriler,
    duzeyler,
    n,
    degerlendiriciSayisi: m,
    yorum: kappaYorumu(kappa),
    uyarilar,
  };
}

// ---------- Sınıf içi korelasyon (ICC) ----------

/** n × k tam veri matrisi için kareler ortalamaları (iki yönlü ANOVA, tekrarsız) */
function iccKareler(matris) {
  gerekli(Array.isArray(matris) && matris.length > 0, 'Veri matrisi boş.');
  const k = matris[0].length;
  gerekli(k >= 2 && matris.every((s) => s.length === k), 'Her satırda aynı sayıda ölçüm (değerlendirici) olmalıdır (en az 2).');
  const uyarilar = [];
  const X = matris.filter((s) => s.every((v) => typeof v === 'number' && Number.isFinite(v)));
  if (X.length < matris.length) uyarilar.push(`Eksik değer içeren ${matris.length - X.length} denek analizden çıkarıldı (liste bazında silme).`);
  const n = X.length;
  gerekli(n >= 2, 'ICC için en az 2 tam denek gerekir.');
  const satirOrt = X.map((s) => ortalama(s));
  const sutunOrt = Array.from({ length: k }, (_, j) => ortalama(X.map((s) => s[j])));
  const genel = ortalama(satirOrt);
  let SSr = 0;
  for (const r of satirOrt) SSr += (r - genel) ** 2;
  SSr *= k;
  let SSc = 0;
  for (const c of sutunOrt) SSc += (c - genel) ** 2;
  SSc *= n;
  let SSw = 0;
  let SSe = 0;
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < k; j++) {
      SSw += (X[i][j] - satirOrt[i]) ** 2;
      SSe += (X[i][j] - satirOrt[i] - sutunOrt[j] + genel) ** 2;
    }
  }
  const MSr = SSr / (n - 1);
  const MSc = SSc / (k - 1);
  const MSw = SSw / (n * (k - 1));
  const MSe = SSe / ((n - 1) * (k - 1));
  if (!(MSw > 0) && !(MSe > 0)) throw new IstatistikHatasi('Tüm değerlendiriciler her denekte aynı puanı vermiş; hata varyansı sıfır olduğu için ICC hesaplanamaz.');
  return { n, k, MSr, MSc, MSw, MSe, SSr, SSc, SSw, SSe, uyarilar };
}

/** Satterthwaite sd'si (uyum tipi ICC): (a·MSc + b·MSe)² / [(a·MSc)²/(k−1) + (b·MSe)²/((n−1)(k−1))] */
function satterthwaite(a, b, K) {
  const { n, k, MSc, MSe } = K;
  return (a * MSc + b * MSe) ** 2 / ((a * MSc) ** 2 / (k - 1) + (b * MSe) ** 2 / ((n - 1) * (k - 1)));
}

/** Tek bir ICC biçimi (McGraw & Wong 1996). model: 'tek-yonlu' | 'iki-yonlu'; tip: 'uyum' | 'tutarlilik'. */
function iccHesapla(K, model, tip, birim, duzey, r0) {
  const { n, k, MSr, MSc, MSw, MSe } = K;
  const alfa = 1 - duzey;
  const ust = (d1, d2) => qf(1 - alfa / 2, d1, d2);
  if (!((model === 'tek-yonlu' ? MSw : MSe) > 0)) {
    throw new IstatistikHatasi('Hata varyansı sıfır (değerlendiriciler tam tutarlı); ICC = 1 olur, F testi ve güven aralığı hesaplanamaz.');
  }
  let icc;
  let F;
  let sd1 = n - 1;
  let sd2;
  let alt;
  let ustSinir;
  let irr = null;
  if (model === 'tek-yonlu' || tip === 'tutarlilik') {
    const MSh = model === 'tek-yonlu' ? MSw : MSe;
    sd2 = model === 'tek-yonlu' ? n * (k - 1) : (n - 1) * (k - 1);
    const F0 = MSr / MSh;
    const FL = F0 / ust(sd1, sd2);
    const FU = F0 * ust(sd2, sd1);
    if (birim === 'tek') {
      icc = (MSr - MSh) / (MSr + (k - 1) * MSh);
      F = F0 * ((1 - r0) / (1 + (k - 1) * r0));
      alt = (FL - 1) / (FL + k - 1);
      ustSinir = (FU - 1) / (FU + k - 1);
    } else {
      icc = (MSr - MSh) / MSr;
      F = F0 * (1 - r0);
      alt = 1 - 1 / FL;
      ustSinir = 1 - 1 / FU;
    }
  } else {
    // Mutlak uyum. Güven aralığı için v, tek ölçüm ICC(A,1) tahmininden (McGraw & Wong 1996; psych::ICC; SPSS).
    const tekIcc = (MSr - MSe) / (MSr + (k - 1) * MSe + (k * (MSc - MSe)) / n);
    const vCI = satterthwaite((k * tekIcc) / (n * (1 - tekIcc)), 1 + (k * tekIcc * (n - 1)) / (n * (1 - tekIcc)), K);
    const FL = ust(n - 1, vCI);
    const FU = ust(vCI, n - 1);
    let a0;
    let b0;
    if (birim === 'tek') {
      icc = tekIcc;
      a0 = (k * r0) / (n * (1 - r0));
      b0 = 1 + (k * r0 * (n - 1)) / (n * (1 - r0));
      alt = (n * (MSr - FL * MSe)) / (FL * (k * MSc + (k * n - k - n) * MSe) + n * MSr);
      ustSinir = (n * (FU * MSr - MSe)) / (k * MSc + (k * n - k - n) * MSe + n * FU * MSr);
    } else {
      icc = (MSr - MSe) / (MSr + (MSc - MSe) / n);
      a0 = r0 / (n * (1 - r0));
      b0 = 1 + (r0 * (n - 1)) / (n * (1 - r0));
      alt = (n * (MSr - FL * MSe)) / (FL * (MSc - MSe) + n * MSr);
      ustSinir = (n * (FU * MSr - MSe)) / (MSc - MSe + n * FU * MSr);
    }
    F = MSr / (a0 * MSc + b0 * MSe);
    // Test sd'si: H0 değeri r0 ile (McGraw & Wong 1996, Tablo 8); r0 = 0 iken (n−1)(k−1) — SPSS ve psych::ICC ile aynı.
    sd2 = r0 === 0 ? (n - 1) * (k - 1) : satterthwaite(a0, b0, K);
    // irr::icc: test ve aralık için v'yi ilgili ICC tahmininden (ortalama birimde ICC(A,k)'den) hesaplar.
    const vIrr = satterthwaite((k * icc) / (n * (1 - icc)), 1 + (k * icc * (n - 1)) / (n * (1 - icc)), K);
    const FLi = ust(n - 1, vIrr);
    const FUi = ust(vIrr, n - 1);
    irr = {
      sd2: vIrr,
      p: pf(F, n - 1, vIrr, false),
      gaAlt: birim === 'tek' ? (n * (MSr - FLi * MSe)) / (FLi * (k * MSc + (k * n - k - n) * MSe) + n * MSr) : (n * (MSr - FLi * MSe)) / (FLi * (MSc - MSe) + n * MSr),
      gaUst: birim === 'tek' ? (n * (FUi * MSr - MSe)) / (k * MSc + (k * n - k - n) * MSe + n * FUi * MSr) : (n * (FUi * MSr - MSe)) / (MSc - MSe + n * FUi * MSr),
    };
  }
  return { icc, F, sd1, sd2, p: pf(F, sd1, sd2, false), gaAlt: alt, gaUst: ustSinir, irr };
}

const MODEL_ADLARI = {
  'tek-yonlu': 'Tek yönlü rastgele',
  'iki-yonlu-rastgele': 'İki yönlü rastgele',
  'iki-yonlu-karma': 'İki yönlü karma',
};

/** McGraw & Wong ve Shrout & Fleiss gösterimleri */
function iccGosterim(model, tip, birim) {
  const kk = birim === 'tek' ? '1' : 'k';
  if (model === 'tek-yonlu') return { mcGrawWong: birim === 'tek' ? 'ICC(1)' : 'ICC(k)', shroutFleiss: `ICC(1,${kk})` };
  let sf = null;
  if (tip === 'uyum' && model === 'iki-yonlu-rastgele') sf = `ICC(2,${kk})`;
  if (tip === 'tutarlilik' && model === 'iki-yonlu-karma') sf = `ICC(3,${kk})`;
  return { mcGrawWong: `ICC(${tip === 'uyum' ? 'A' : 'C'},${kk})`, shroutFleiss: sf };
}

/**
 * Sınıf içi korelasyon katsayısı (ICC) — McGraw & Wong (1996) tanımları; SPSS RELIABILITY /ICC ile aynı.
 * Girdi: satır = denek, sütun = değerlendirici/ölçüm (sayısal). Eksik değerli satırlar atılır.
 *
 * Seçenekler (varsayılanlar SPSS'inkiler):
 *   model: 'iki-yonlu-karma' (SPSS "Two-Way Mixed") | 'iki-yonlu-rastgele' ("Two-Way Random") | 'tek-yonlu' ("One-Way Random")
 *   tip:   'tutarlilik' ("Consistency") | 'uyum' ("Absolute Agreement"); tek yönlü modelde tip yok sayılır
 *   birim: 'tek' ("Single Measures") | 'ortalama' ("Average Measures")
 *   duzey: güven düzeyi; r0: H0 altındaki ICC değeri (SPSS "Test value", varsayılan 0)
 * İki yönlü rastgele ve karma modeller aynı sayısal sonucu verir (yalnızca yorum farklıdır).
 *
 * Shrout & Fleiss (1979) karşılıkları: ICC(1,1)/ICC(1,k) = tek yönlü tek/ortalama; ICC(2,1)/ICC(2,k) = iki yönlü
 * rastgele, mutlak uyum; ICC(3,1)/ICC(3,k) = iki yönlü karma, tutarlılık. (ICC(3,k) = Cronbach alfa.)
 * R: irr::icc(model = 'oneway'|'twoway', type, unit) ve psych::ICC(lmer = FALSE).
 * Mutlak uyumda irr::icc, F testinin payda sd'sini ve ortalama birimin güven aralığını farklı bir Satterthwaite
 * sd'siyle hesaplar; burada SPSS/psych/McGraw-Wong kullanılır, irr'nin değerleri `irr` alanında verilir.
 *
 * @param {number[][]} matris
 * @param {{model?:string, tip?:'uyum'|'tutarlilik', birim?:'tek'|'ortalama', duzey?:number, r0?:number}} [secenek]
 */
export function icc(matris, secenek = {}) {
  const { model = 'iki-yonlu-karma', birim = 'tek', duzey = 0.95, r0 = 0 } = secenek;
  let tip = secenek.tip ?? 'tutarlilik';
  gerekli(Object.hasOwn(MODEL_ADLARI, model), "Model 'tek-yonlu', 'iki-yonlu-rastgele' ya da 'iki-yonlu-karma' olmalıdır.");
  gerekli(tip === 'uyum' || tip === 'tutarlilik', "Tip 'uyum' ya da 'tutarlilik' olmalıdır.");
  gerekli(birim === 'tek' || birim === 'ortalama', "Birim 'tek' ya da 'ortalama' olmalıdır.");
  guvenDuzeyi(duzey);
  gerekli(r0 >= 0 && r0 < 1, 'Test değeri (r0) 0 ile 1 arasında olmalıdır.');
  const K = iccKareler(matris);
  const uyarilar = [...K.uyarilar];
  if (model === 'tek-yonlu') tip = null;
  const ic = model === 'tek-yonlu' ? 'tek-yonlu' : 'iki-yonlu';
  const satir = (b) => ({ birim: b, ...iccHesapla(K, ic, tip, b, duzey, r0), ...iccGosterim(model, tip, b) });
  const satirlar = [satir('tek'), satir('ortalama')];
  const s = satirlar.find((x) => x.birim === birim);
  if (s.icc < 0) uyarilar.push('ICC negatif: denekler arası değişkenlik, ölçüm hatasından küçük; güvenirlik yok kabul edilir.');
  if (model === 'iki-yonlu-karma' && tip === 'tutarlilik' && birim === 'ortalama') {
    uyarilar.push('Ortalama ölçüm ICC(3,k), etkileşim olmadığı varsayımıyla hesaplanır (Cronbach alfaya eşittir).');
  }
  const ad = `ICC — ${MODEL_ADLARI[model]}${tip ? `, ${tip === 'uyum' ? 'mutlak uyum' : 'tutarlılık'}` : ''}, ${birim === 'tek' ? 'tek ölçüm' : 'ortalama ölçüm'}`;
  return {
    yontem: 'icc',
    ad,
    model,
    tip,
    birim,
    istatistik: s.F,
    istatistikAdi: 'F',
    sd: [s.sd1, s.sd2],
    p: s.p,
    icc: s.icc,
    r0,
    etki: { ad: 'ICC', sembol: 'ICC', deger: s.icc, gaAlt: s.gaAlt, gaUst: s.gaUst },
    ga: { duzey, alt: s.gaAlt, ust: s.gaUst, neyin: 'ICC' },
    mcGrawWong: s.mcGrawWong,
    shroutFleiss: s.shroutFleiss,
    irr: s.irr,
    satirlar,
    anova: { n: K.n, k: K.k, MSr: K.MSr, MSc: K.MSc, MSw: K.MSw, MSe: K.MSe },
    n: K.n,
    degerlendiriciSayisi: K.k,
    yorum: iccYorumu(s.icc),
    uyarilar,
  };
}

/**
 * Altı Shrout-Fleiss ICC biçimini birlikte hesaplar (R: psych::ICC(x, lmer = FALSE)).
 * Satırlar: ICC1, ICC2, ICC3, ICC1k, ICC2k, ICC3k; her biri {tur, ad, icc, F, sd1, sd2, p, gaAlt, gaUst, spss}.
 * @param {number[][]} matris
 * @param {{duzey?:number}} [secenek]
 */
export function iccTumu(matris, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  guvenDuzeyi(duzey);
  const K = iccKareler(matris);
  const tanimlar = [
    ['ICC1', 'tek-yonlu', null, 'tek', 'Tek değerlendirici, mutlak (tek yönlü)', 'One-Way Random, Single Measures'],
    ['ICC2', 'iki-yonlu', 'uyum', 'tek', 'Tek rastgele değerlendirici', 'Two-Way Random, Absolute Agreement, Single Measures'],
    ['ICC3', 'iki-yonlu', 'tutarlilik', 'tek', 'Tek sabit değerlendirici', 'Two-Way Mixed, Consistency, Single Measures'],
    ['ICC1k', 'tek-yonlu', null, 'ortalama', 'Ortalama, mutlak (tek yönlü)', 'One-Way Random, Average Measures'],
    ['ICC2k', 'iki-yonlu', 'uyum', 'ortalama', 'Ortalama, rastgele değerlendiriciler', 'Two-Way Random, Absolute Agreement, Average Measures'],
    ['ICC3k', 'iki-yonlu', 'tutarlilik', 'ortalama', 'Ortalama, sabit değerlendiriciler', 'Two-Way Mixed, Consistency, Average Measures'],
  ];
  const satirlar = tanimlar.map(([tur, model, tip, birim, ad, spss]) => {
    const r = iccHesapla(K, model, tip, birim, duzey, 0);
    return { tur, ad, spss, icc: r.icc, F: r.F, sd1: r.sd1, sd2: r.sd2, p: r.p, gaAlt: r.gaAlt, gaUst: r.gaUst };
  });
  return {
    yontem: 'icc-tumu',
    ad: 'Sınıf içi korelasyon katsayıları (Shrout & Fleiss)',
    satirlar,
    anova: { n: K.n, k: K.k, MSr: K.MSr, MSc: K.MSc, MSw: K.MSw, MSe: K.MSe },
    ga: { duzey },
    n: K.n,
    degerlendiriciSayisi: K.k,
    uyarilar: K.uyarilar,
  };
}

// ---------- Bland-Altman ----------

/** Merkezi olmayan t'nin ters fonksiyonu (R: qt(p, df, ncp)) */
function qnt(p, df, ncp) {
  const bas = ncp + qt(p, df);
  return tersCoz((x) => pnt(x, df, ncp), p, bas, -Infinity, Infinity, 1e-13);
}

function ciftleriTemizle(x, y, ad) {
  gerekli(Array.isArray(x) && Array.isArray(y) && x.length === y.length, `${ad}: iki ölçüm dizisi aynı uzunlukta olmalıdır.`);
  const a = [];
  const b = [];
  for (let i = 0; i < x.length; i++) {
    if (typeof x[i] === 'number' && Number.isFinite(x[i]) && typeof y[i] === 'number' && Number.isFinite(y[i])) {
      a.push(x[i]);
      b.push(y[i]);
    }
  }
  const uyarilar = a.length < x.length ? [`Eksik değer içeren ${x.length - a.length} çift analizden çıkarıldı.`] : [];
  return { a, b, uyarilar };
}

/**
 * Bland-Altman uyum analizi (Bland & Altman 1986, 1999).
 * fark = x − y (farkTipi 'yuzde' ise 100·(x − y)/ortalama), ortalama = (x + y)/2.
 * Yanlılık (bias) = farkların ortalaması; GA: t dağılımıyla.
 * Uyum sınırları (LoA) = bias ± carpan·SS (carpan varsayılan 1.96; blandr, MedCalc ve Bland-Altman gibi).
 * LoA güven aralıkları:
 *   yaklaşık (Bland & Altman 1999): SH = SS·√(1/n + carpan²/(2(n−1))), LoA ± t_{n−1}·SH (blandr ile aynı formül;
 *     blandr SH'de carpan yerine z_{0.975} = 1.95996 kullanır)
 *   kesin (Carkeet 2015): μ ± carpan·σ için merkezi olmayan t: üst sınır için
 *     [d̄ + SS·t'_{α/2}/√n, d̄ + SS·t'_{1−α/2}/√n], t' ~ t(n−1, δ = carpan·√n); alt sınır simetrik.
 * Oransal yanlılık: fark = b0 + b1·ortalama doğrusal regresyonu (b1 = 0 testi, sd = n − 2).
 * @param {number[]} x 1. yöntem
 * @param {number[]} y 2. yöntem
 * @param {{duzey?:number, carpan?:number, farkTipi?:'fark'|'yuzde'}} [secenek]
 */
export function blandAltman(x, y, secenek = {}) {
  const { duzey = 0.95, carpan = 1.96, farkTipi = 'fark' } = secenek;
  guvenDuzeyi(duzey);
  gerekli(carpan > 0, 'Uyum sınırı çarpanı pozitif olmalıdır.');
  gerekli(farkTipi === 'fark' || farkTipi === 'yuzde', "Fark tipi 'fark' ya da 'yuzde' olmalıdır.");
  const { a, b, uyarilar } = ciftleriTemizle(x, y, 'Bland-Altman');
  const n = a.length;
  gerekli(n >= 3, 'Bland-Altman analizi için en az 3 tam çift gerekir.');
  const ort = a.map((v, i) => (v + b[i]) / 2);
  if (farkTipi === 'yuzde') gerekli(ort.every((v) => v !== 0), 'Yüzde fark için ölçüm ortalamaları sıfır olmamalıdır.');
  const fark = a.map((v, i) => (farkTipi === 'yuzde' ? (100 * (v - b[i])) / ort[i] : v - b[i]));
  const bias = ortalama(fark);
  const ss = Math.sqrt(varyans(fark, bias));
  gerekli(ss > 0, 'Tüm farklar aynı; uyum sınırları hesaplanamaz.');
  const alfa = 1 - duzey;
  const tk = qt(1 - alfa / 2, n - 1);
  const shBias = ss / Math.sqrt(n);
  const altLoA = bias - carpan * ss;
  const ustLoA = bias + carpan * ss;
  const shLoA = ss * Math.sqrt(1 / n + (carpan * carpan) / (2 * (n - 1)));
  const kn = Math.sqrt(n);
  const tAlt = qnt(alfa / 2, n - 1, carpan * kn);
  const tUst = qnt(1 - alfa / 2, n - 1, carpan * kn);
  // Oransal yanlılık: fark ~ ortalama
  const mOrt = ortalama(ort);
  let Sxx = 0;
  let Sxy = 0;
  for (let i = 0; i < n; i++) {
    Sxx += (ort[i] - mOrt) ** 2;
    Sxy += (ort[i] - mOrt) * (fark[i] - bias);
  }
  let regresyon = null;
  if (Sxx > 0 && n > 2) {
    const b1 = Sxy / Sxx;
    const b0 = bias - b1 * mOrt;
    let SSE = 0;
    for (let i = 0; i < n; i++) SSE += (fark[i] - b0 - b1 * ort[i]) ** 2;
    const s2 = SSE / (n - 2);
    const shB1 = Math.sqrt(s2 / Sxx);
    const shB0 = Math.sqrt(s2 * (1 / n + (mOrt * mOrt) / Sxx));
    const t = b1 / shB1;
    const t2 = qt(1 - alfa / 2, n - 2);
    const SST = (n - 1) * ss * ss;
    regresyon = {
      kesisim: b0,
      egim: b1,
      shKesisim: shB0,
      shEgim: shB1,
      t,
      sd: n - 2,
      p: Number.isFinite(t) ? 2 * pt(-Math.abs(t), n - 2) : 0,
      egimGaAlt: b1 - t2 * shB1,
      egimGaUst: b1 + t2 * shB1,
      r2: SST > 0 ? 1 - SSE / SST : null,
    };
    if (regresyon.p < 0.05) uyarilar.push('Oransal yanlılık var: farklar ölçüm büyüklüğüyle birlikte değişiyor (eğim anlamlı). Uyum sınırları tüm ölçüm aralığı için geçerli olmayabilir.');
  }
  const disarida = fark.filter((d) => d < altLoA || d > ustLoA).length;
  return {
    yontem: 'bland-altman',
    ad: 'Bland-Altman uyum analizi',
    istatistik: bias / shBias,
    istatistikAdi: 't',
    sd: n - 1,
    p: 2 * pt(-Math.abs(bias / shBias), n - 1),
    n,
    farkTipi,
    bias,
    ssFark: ss,
    shBias,
    biasGa: { duzey, alt: bias - tk * shBias, ust: bias + tk * shBias },
    ga: { duzey, alt: bias - tk * shBias, ust: bias + tk * shBias, neyin: 'yanlılık (ortalama fark)' },
    carpan,
    altSinir: altLoA,
    ustSinir: ustLoA,
    shSinir: shLoA,
    altSinirGa: { yaklasik: [altLoA - tk * shLoA, altLoA + tk * shLoA], kesin: [bias - (ss * tUst) / kn, bias - (ss * tAlt) / kn] },
    ustSinirGa: { yaklasik: [ustLoA - tk * shLoA, ustLoA + tk * shLoA], kesin: [bias + (ss * tAlt) / kn, bias + (ss * tUst) / kn] },
    oransalYanlilik: regresyon,
    disaridaSayi: disarida,
    disaridaYuzde: (100 * disarida) / n,
    noktalar: ort.map((m, i) => ({ ortalama: m, fark: fark[i] })),
    uyarilar,
  };
}

// ---------- Lin uyum korelasyon katsayısı ----------

/** McBride (2005) CCC yorumu */
function cccYorumu(r) {
  if (r < 0.9) return 'zayıf';
  if (r < 0.95) return 'orta';
  if (r <= 0.99) return 'iyi';
  return 'çok iyi';
}

/**
 * Lin'in uyum korelasyon katsayısı (CCC; Lin 1989, düzeltme 2000). R: epiR::epi.ccc(x, y, ci = 'z-transform').
 * ρc = 2·s_xy / (s_x² + s_y² + (x̄ − ȳ)²) (varyans ve kovaryanslar n paydalı).
 * Kesinlik (precision) = Pearson r, doğruluk (accuracy) Cb = ρc / r.
 * SH (Lin 2000, n − 2 paydalı); GA: Fisher z dönüşümüyle (varsayılan) ya da asimptotik (ρc ± z·SH).
 * olcekKaymasi v = s_y / s_x, konumKaymasi u = (ȳ − x̄)/√(s_x·s_y) (epiR'deki s.shift, l.shift).
 * @param {number[]} x
 * @param {number[]} y
 * @param {{duzey?:number}} [secenek]
 */
export function linCCC(x, y, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  guvenDuzeyi(duzey);
  const { a, b, uyarilar } = ciftleriTemizle(x, y, 'Lin CCC');
  const n = a.length;
  gerekli(n >= 3, 'Uyum korelasyon katsayısı için en az 3 tam çift gerekir.');
  const mx = ortalama(a);
  const my = ortalama(b);
  let sxx = 0;
  let syy = 0;
  let sxy = 0;
  for (let i = 0; i < n; i++) {
    sxx += (a[i] - mx) ** 2;
    syy += (b[i] - my) ** 2;
    sxy += (a[i] - mx) * (b[i] - my);
  }
  gerekli(sxx > 0 && syy > 0, 'Ölçümlerden biri sabit; uyum korelasyon katsayısı hesaplanamaz.');
  const r = sxy / Math.sqrt(sxx * syy);
  const vx = sxx / n;
  const vy = syy / n;
  const p = (2 * (sxy / n)) / (vx + vy + (my - mx) ** 2);
  const u = (my - mx) / Math.sqrt(Math.sqrt(vx * vy));
  const sh = Math.sqrt(((1 - r * r) * p * p * (1 - p * p)) / (r * r) + (2 * p ** 3 * (1 - p) * u * u) / r - (0.5 * p ** 4 * u ** 4) / (r * r)) / Math.sqrt(n - 2);
  const zk = qnorm((1 + duzey) / 2);
  const zt = Math.atanh(p);
  const shz = sh / (1 - p * p);
  const ga = { duzey, alt: Math.tanh(zt - zk * shz), ust: Math.tanh(zt + zk * shz), neyin: 'CCC', yontem: 'Fisher z' };
  return {
    yontem: 'lin-ccc',
    ad: 'Lin uyum korelasyon katsayısı',
    ccc: p,
    sh,
    etki: { ad: 'Lin CCC', sembol: 'ρc', deger: p, gaAlt: ga.alt, gaUst: ga.ust },
    ga,
    gaAsimptotik: { duzey, alt: p - zk * sh, ust: p + zk * sh },
    kesinlik: r,
    dogruluk: p / r,
    olcekKaymasi: Math.sqrt(syy / sxx),
    konumKaymasi: u,
    n,
    yorum: cccYorumu(p),
    uyarilar,
  };
}

// ---------- Yüzde uyum ----------

/**
 * Yüzde uyum: tüm değerlendiricilerin aynı puanı verdiği deneklerin yüzdesi (R: irr::agree).
 * Girdi: ya iki dizi (r1, r2) ya da tek bir matris (satır = denek, sütun = değerlendirici).
 * Sayısal veride `tolerans` > 0 ise en büyük − en küçük puan farkı toleransı aşmayan denekler uyumlu sayılır.
 * @param {Array|Array<Array>} r1 matris ya da 1. değerlendirici
 * @param {Array|{tolerans?:number}} [r2] 2. değerlendirici ya da seçenekler
 * @param {{tolerans?:number}} [secenek]
 */
export function yuzdeUyum(r1, r2, secenek = {}) {
  let matris;
  let ayar = secenek;
  if (Array.isArray(r2)) {
    gerekli(Array.isArray(r1) && r1.length === r2.length, 'İki değerlendiricinin puan dizileri aynı uzunlukta olmalıdır.');
    matris = r1.map((v, i) => [v, r2[i]]);
  } else {
    matris = r1;
    ayar = r2 ?? {};
  }
  const tolerans = ayar.tolerans ?? 0;
  gerekli(tolerans >= 0, 'Tolerans negatif olamaz.');
  gerekli(Array.isArray(matris) && matris.length > 0, 'Veri boş.');
  const satirlar = matris.filter((s) => s.every((v) => !eksik(v)));
  const uyarilar = satirlar.length < matris.length ? [`Eksik değer içeren ${matris.length - satirlar.length} denek analizden çıkarıldı.`] : [];
  const n = satirlar.length;
  gerekli(n >= 1, 'Yüzde uyum için en az bir tam denek gerekir.');
  const sayisal = satirlar.every((s) => s.every((v) => typeof v === 'number'));
  if (!sayisal && tolerans > 0) uyarilar.push('Kategorik veride tolerans kullanılmaz.');
  let uyumlu = 0;
  for (const s of satirlar) {
    if (sayisal) {
      if (Math.max(...s) - Math.min(...s) <= tolerans + 1e-12) uyumlu++;
    } else if (new Set(s.map(String)).size === 1) uyumlu++;
  }
  return {
    yontem: 'yuzde-uyum',
    ad: 'Yüzde uyum',
    yuzde: (100 * uyumlu) / n,
    uyumluSayi: uyumlu,
    n,
    degerlendiriciSayisi: satirlar[0].length,
    tolerans: sayisal ? tolerans : 0,
    uyarilar,
  };
}
