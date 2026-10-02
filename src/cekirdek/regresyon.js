// Doğrusal regresyon (en küçük kareler): çoklu, basit, hiyerarşik (bloklu) ve adımsal.
// Çıktılar SPSS REGRESSION ve R summary(lm) ile aynı tabloları verir; R ile 1e-10 göreli doğrulukta test edilmiştir.

import { enKucukKareler, logDeterminant, sifir } from './matris.js';
import { ptIkiYonlu, qt, pf, pchisq } from './dagilim.js';
import { IstatistikHatasi } from './hata.js';
import { modelMatrisi, sayiyaCevir } from './model.js';

const LOG_2PI = Math.log(2 * Math.PI);

function ortalamaDizi(x) {
  let s = 0;
  for (const v of x) s += v;
  const m = s / x.length;
  let d = 0;
  for (const v of x) d += v - m;
  return m + d / x.length;
}

function kareToplamSapma(x, m = ortalamaDizi(x)) {
  let s = 0;
  for (const v of x) s += (v - m) * (v - m);
  return s;
}

function sutunAl(X, j) {
  return X.map((s) => s[j]);
}

function sutunlariSec(X, idx) {
  return X.map((s) => idx.map((j) => s[j]));
}

function kareToplam(x) {
  let s = 0;
  for (const v of x) s += v * v;
  return s;
}

/** Bağımlı değişkeni sayıya çevirir (eksikler null kalır). */
function bagimliHazirla(y) {
  if (!Array.isArray(y) && !ArrayBuffer.isView(y)) throw new IstatistikHatasi('Bağımlı değişken (y) bir dizi olmalıdır.');
  return Array.from(y);
}

/** Alt model için sütun seçimi: verilen terimlerin sütunları (+ sabit). */
function altSutunlar(mm, terimAdlari) {
  const idx = mm.sabit ? [0] : [];
  for (const ad of terimAdlari) {
    const t = mm.terimler.find((u) => u.ad === ad);
    idx.push(...t.sutunlar);
  }
  return idx.sort((a, b) => a - b);
}

/** Model matrisinden yalnızca verilen terimleri içeren yeni bir model matrisi nesnesi üretir. */
function altModelMatrisi(mm, terimAdlari) {
  const secili = new Set(terimAdlari);
  const terimler = mm.terimler.filter((t) => secili.has(t.ad));
  const idx = altSutunlar(mm, terimler.map((t) => t.ad));
  const yeniIndeks = new Map(idx.map((j, k) => [j, k]));
  return {
    ...mm,
    X: sutunlariSec(mm.X, idx),
    sutunAdlari: idx.map((j) => mm.sutunAdlari[j]),
    sutunlar: idx.map((j) => mm.sutunlar[j]),
    terimler: terimler.map((t) => ({ ...t, sutunlar: t.sutunlar.map((j) => yeniIndeks.get(j)) })),
  };
}

/** Hızlı EKK: yalnızca artık KT ve rank (adımsal/hiyerarşik karşılaştırmalar için). */
function hizliEKK(X, y) {
  const e = enKucukKareler(X, y);
  return { rss: kareToplam(e.kalan), rank: e.rank, ekk: e };
}

/**
 * Çekirdek: hazır model matrisi ve y üzerinde doğrusal regresyon. Dışa açık fonksiyonlar bunu kullanır.
 * @param {object} mm modelMatrisi çıktısı
 * @param {number[]} y mm.satirlar'a göre süzülmüş sayısal y
 */
function dogrusalUydur(mm, y, secenekler = {}) {
  const { duzey = 0.95, ucDegerEsigi = 3, satirHaritasi = null } = secenekler;
  const uyarilar = [...(mm.uyarilar || [])];
  const X = mm.X;
  const n = X.length;
  const p = mm.sutunAdlari.length;
  const sabit = mm.sabit;
  if (n === 0) throw new IstatistikHatasi('Analiz için gözlem yok.');
  if (p === 0) throw new IstatistikHatasi('Modelde hiç terim yok (sabit de çıkarılmış).');

  const yTSS = sabit ? kareToplamSapma(y) : kareToplam(y);
  if (sabit && yTSS === 0) throw new IstatistikHatasi('Bağımlı değişken sabit (tüm değerler aynı); regresyon yapılamaz.');

  const ekk = enKucukKareler(X, y);
  const rank = ekk.rank;
  const sd2 = n - rank;
  if (sd2 <= 0) {
    throw new IstatistikHatasi(`Gözlem sayısı (${n}) tahmin edilen parametre sayısından (${rank}) fazla olmalıdır; artık serbestlik derecesi sıfır.`);
  }
  const bagimliAdlar = ekk.bagimli.map((j) => mm.sutunAdlari[j]);
  if (bagimliAdlar.length) {
    uyarilar.push(
      `Tam çoklu doğrusal bağlantı: ${bagimliAdlar.map((a) => `'${a}'`).join(', ')} diğer değişkenlerin doğrusal bileşimi olduğu için modelden çıkarıldı (R'de katsayısı NA görünür).`,
    );
  }
  const kullanilan = [];
  for (let j = 0; j < p; j++) if (ekk.katsayi[j] !== null) kullanilan.push(j);

  const e = ekk.kalan;
  const yhat = ekk.uyum;
  const rss = kareToplam(e);
  const sigma2 = rss / sd2;
  const sigma = Math.sqrt(sigma2);
  const yOrt = ortalamaDizi(y);
  // R summary.lm: mss = Σ(f - ort(f))² (sabitli) ya da Σf² (sabitsiz)
  const mss = sabit ? kareToplamSapma(yhat) : kareToplam(yhat);
  const R2 = mss / (mss + rss);
  const sdSabit = sabit ? 1 : 0;
  const sd1 = rank - sdSabit;
  const duzR2 = 1 - (1 - R2) * ((n - sdSabit) / sd2);
  const F = sd1 > 0 ? mss / sd1 / sigma2 : null;
  const pF = sd1 > 0 ? pf(F, sd1, sd2, false) : null;
  if (rss <= 1e-28 * Math.max(yTSS, 1e-300) || R2 > 1 - 1e-14) uyarilar.push('Model veriye kusursuz uyuyor (artıklar ~0); çıkarım istatistikleri anlamsızdır.');

  // log-olabilirlik (R logLik.lm), AIC/BIC: parametre sayısı = rank + 1 (σ dahil)
  const logL = 0.5 * (-n * (LOG_2PI + 1 - Math.log(n) + Math.log(rss)));
  const k = rank + 1;
  const AIC = -2 * logL + 2 * k;
  const BIC = -2 * logL + Math.log(n) * k;

  // Katsayılar
  const tKritik = qt(1 - (1 - duzey) / 2, sd2);
  const sdY = Math.sqrt(yTSS / (n - 1));
  const katsayilar = [];
  const xOrt = new Array(p).fill(null);
  const xSST = new Array(p).fill(null);
  for (let j = 0; j < p; j++) {
    const xj = sutunAl(X, j);
    xOrt[j] = ortalamaDizi(xj);
    xSST[j] = kareToplamSapma(xj, xOrt[j]);
  }
  for (let j = 0; j < p; j++) {
    const bilgi = mm.sutunlar[j];
    const satir = {
      ad: mm.sutunAdlari[j],
      terim: bilgi.terim,
      duzey: bilgi.duzey,
      B: null,
      SH: null,
      beta: null,
      t: null,
      p: null,
      gaAlt: null,
      gaUst: null,
      tolerans: null,
      VIF: null,
      sifirDereceKorelasyon: null,
      kismiKorelasyon: null,
      yariKismiKorelasyon: null,
      bagimli: ekk.katsayi[j] === null,
    };
    if (!satir.bagimli) {
      const B = ekk.katsayi[j];
      const SH = Math.sqrt(sigma2 * ekk.XtXTers[j][j]);
      const t = B / SH;
      satir.B = B;
      satir.SH = SH;
      satir.t = t;
      satir.p = ptIkiYonlu(t, sd2);
      satir.gaAlt = B - tKritik * SH;
      satir.gaUst = B + tKritik * SH;
      const sabitSutunu = sabit && j === 0;
      if (sabit && !sabitSutunu && xSST[j] > 0) {
        // SPSS: Beta = B · s(x)/s(y) (gösterge sütunları da sayısal değişken gibi standartlaştırılır)
        satir.beta = (B * Math.sqrt(xSST[j] / (n - 1))) / sdY;
        // Tolerans = 1 - R²_j; VIF_j = (X'X)^-1_jj · Σ(x_j - x̄_j)²
        const VIF = ekk.XtXTers[j][j] * xSST[j];
        satir.VIF = VIF;
        satir.tolerans = 1 / VIF;
        // Korelasyonlar (SPSS "Correlations": zero-order, partial, part)
        let sxy = 0;
        for (let i = 0; i < n; i++) sxy += (X[i][j] - xOrt[j]) * (y[i] - yOrt);
        satir.sifirDereceKorelasyon = sxy / Math.sqrt(xSST[j] * yTSS);
        satir.kismiKorelasyon = t / Math.sqrt(t * t + sd2);
        satir.yariKismiKorelasyon = t * Math.sqrt((1 - R2) / sd2);
      }
    }
    katsayilar.push(satir);
  }

  // Katsayı kovaryans matrisi (σ² (X'X)^-1); bağımlı sütunlar null
  const kovaryans = sifir(p, p).map((s) => s.fill(null));
  for (const a of kullanilan) for (const b of kullanilan) kovaryans[a][b] = sigma2 * ekk.XtXTers[a][b];

  // ANOVA tablosu
  const anovaTablosu = [
    { kaynak: 'Regresyon', KT: mss, sd: sd1, KO: sd1 > 0 ? mss / sd1 : null, F, p: pF },
    { kaynak: 'Artık', KT: rss, sd: sd2, KO: sigma2, F: null, p: null },
    { kaynak: 'Toplam', KT: mss + rss, sd: sd1 + sd2, KO: null, F: null, p: null },
  ];

  // Terim testleri: terimi modelden çıkarma (drop-one) kısmi F testi = R drop1(fit, test = "F").
  // Etkileşimi olmayan modelde Tip II = Tip III'tür; etkileşime giren ana etkiler için test, diğer değişkenin
  // referans düzeyindeki (ya da 0 değerindeki) koşullu etkiyi sınar (car::Anova(type = 3), gösterge kodlaması).
  const etkilesimBilesenleri = new Set();
  for (const t of mm.terimler) if (t.tur === 'etkilesim') for (const b of t.bilesenler) etkilesimBilesenleri.add(b);
  const terimTestleri = [];
  for (const t of mm.terimler) {
    const sil = new Set(t.sutunlar);
    const kalanIdx = [];
    for (let j = 0; j < p; j++) if (!sil.has(j)) kalanIdx.push(j);
    let rssRed;
    let rankRed;
    if (kalanIdx.length === 0) {
      rssRed = kareToplam(y);
      rankRed = 0;
    } else {
      const r = hizliEKK(sutunlariSec(X, kalanIdx), y);
      rssRed = r.rss;
      rankRed = r.rank;
    }
    const sd = rank - rankRed;
    const KT = rssRed - rss;
    const satir = { ad: t.ad, tur: t.tur, sd, KT: sd > 0 ? KT : null, F: null, p: null, kismiEtaKare: null, etkilesimdeVar: etkilesimBilesenleri.has(t.ad) };
    if (sd > 0) {
      satir.F = KT / sd / sigma2;
      satir.p = pf(satir.F, sd, sd2, false);
      satir.kismiEtaKare = KT / rssRed;
    }
    terimTestleri.push(satir);
  }
  if ([...etkilesimBilesenleri].length) {
    uyarilar.push(
      'Etkileşime giren değişkenlerin ana etki katsayıları ve terim testleri, diğer değişkenin referans düzeyinde (sürekli ise 0 değerinde) koşullu etkiyi gösterir.',
    );
  }

  // Genelleştirilmiş VIF (Fox & Monette; car::vif): çok sütunlu terimler için
  const gvifUygun = sabit && mm.terimler.length >= 2 && ekk.bagimli.length === 0;
  if (gvifUygun) {
    const tahminIdx = kullanilan.filter((j) => j !== 0);
    const Rk = korelasyon(X, tahminIdx, xOrt, xSST);
    const logDetR = logDeterminant(Rk);
    const konum = new Map(tahminIdx.map((j, k) => [j, k]));
    for (const satir of terimTestleri) {
      const t = mm.terimler.find((u) => u.ad === satir.ad);
      const ic = t.sutunlar.map((j) => konum.get(j));
      const icSet = new Set(ic);
      const dis = tahminIdx.map((j) => konum.get(j)).filter((k) => !icSet.has(k));
      const R11 = ic.map((a) => ic.map((b) => Rk[a][b]));
      const R22 = dis.map((a) => dis.map((b) => Rk[a][b]));
      const gvif = Math.exp(logDeterminant(R11) + (dis.length ? logDeterminant(R22) : 0) - logDetR);
      satir.GVIF = gvif;
      satir.GVIFdf = gvif ** (1 / (2 * t.sutunlar.length));
    }
  }

  // Tanılama
  const tanilama = tanilamaHesapla({ X, y, e, yhat, kullanilan, XtXTers: ekk.XtXTers, sigma, sigma2, rss, rank, sd2, sabit, ucDegerEsigi, mm, satirHaritasi });

  const R = Math.sqrt(R2);
  const modelOzeti = {
    R,
    R2,
    duzeltilmisR2: duzR2,
    tahminSH: sigma,
    F,
    sd1,
    sd2,
    p: pF,
    AIC,
    BIC,
    logOlabilirlik: logL,
    durbinWatson: tanilama.durbinWatson,
    n,
  };

  return {
    yontem: 'dogrusal-regresyon',
    ad: mm.terimler.length === 1 && mm.terimler[0].tur === 'surekli' ? 'Basit doğrusal regresyon' : 'Çoklu doğrusal regresyon',
    istatistik: F,
    istatistikAdi: 'F',
    sd: [sd1, sd2],
    p: pF,
    n,
    etki: { ad: 'R²', sembol: 'R²', deger: R2 },
    katsayilar,
    modelOzeti,
    anovaTablosu,
    terimTestleri,
    tanilama,
    kovaryans,
    bagimliSutunlar: bagimliAdlar,
    sutunAdlari: mm.sutunAdlari,
    terimler: mm.terimler,
    satirlar: satirHaritasi ? mm.satirlar.map((i) => satirHaritasi[i]) : mm.satirlar,
    duzey,
    uyarilar,
  };
}

function korelasyon(X, idx, xOrt, xSST) {
  const m = idx.length;
  const R = sifir(m, m);
  for (let a = 0; a < m; a++) {
    const ja = idx[a];
    R[a][a] = 1;
    for (let b = a + 1; b < m; b++) {
      const jb = idx[b];
      let s = 0;
      for (let i = 0; i < X.length; i++) s += (X[i][ja] - xOrt[ja]) * (X[i][jb] - xOrt[jb]);
      R[a][b] = R[b][a] = s / Math.sqrt(xSST[ja] * xSST[jb]);
    }
  }
  return R;
}

function tanilamaHesapla({ X, y, e, yhat, kullanilan, XtXTers, sigma, sigma2, rss, rank, sd2, sabit, ucDegerEsigi, mm, satirHaritasi }) {
  const n = X.length;
  // Kaldıraç h_ii = x_i' (X'X)^-1 x_i
  const kaldirac = new Array(n);
  for (let i = 0; i < n; i++) {
    const xi = X[i];
    let h = 0;
    for (const a of kullanilan) {
      let s = 0;
      for (const b of kullanilan) s += XtXTers[a][b] * xi[b];
      h += xi[a] * s;
    }
    // R lm.influence: 1'e çok yakın kaldıraç 1 kabul edilir
    if (h > 1 - 10 * Number.EPSILON) h = 1;
    kaldirac[i] = h;
  }
  const standartlastirilmis = e.map((v) => v / sigma);
  const student = new Array(n);
  const silinmis = new Array(n);
  const cook = new Array(n);
  const dffits = new Array(n);
  for (let i = 0; i < n; i++) {
    const h = kaldirac[i];
    if (h >= 1) {
      student[i] = null;
      silinmis[i] = null;
      cook[i] = null;
      dffits[i] = null;
      continue;
    }
    const r = e[i] / (sigma * Math.sqrt(1 - h));
    student[i] = r;
    cook[i] = (r * r * h) / (rank * (1 - h));
    if (sd2 > 1) {
      const s2i = (rss - (e[i] * e[i]) / (1 - h)) / (sd2 - 1);
      const t = e[i] / (Math.sqrt(s2i) * Math.sqrt(1 - h));
      silinmis[i] = t;
      dffits[i] = t * Math.sqrt(h / (1 - h));
    } else {
      silinmis[i] = null;
      dffits[i] = null;
    }
  }
  // Durbin-Watson (gözlem sırasıyla)
  let pay = 0;
  for (let i = 1; i < n; i++) pay += (e[i] - e[i - 1]) ** 2;
  const durbinWatson = pay / rss;

  // Breusch-Pagan (lmtest::bptest ile aynı formül; Z = modeldeki tahminciler)
  const Z = sutunlariSec(X, kullanilan);
  let breuschPagan = null;
  if (kullanilan.length >= 2 || (!sabit && kullanilan.length >= 1)) {
    const s2ml = rss / n;
    const w = e.map((v) => v * v - s2ml);
    const auxK = enKucukKareler(Z, w);
    const bpK = (n * kareToplam(auxK.uyum)) / kareToplam(w);
    const f = e.map((v) => (v * v) / s2ml - 1);
    const auxB = enKucukKareler(Z, f);
    const bpB = 0.5 * kareToplam(auxB.uyum);
    const sd = auxK.rank - 1;
    breuschPagan = {
      koenker: { ad: 'Breusch-Pagan (Koenker, studentize)', istatistik: bpK, sd, p: sd > 0 ? pchisq(bpK, sd, false) : null },
      klasik: { ad: 'Breusch-Pagan (klasik)', istatistik: bpB, sd, p: sd > 0 ? pchisq(bpB, sd, false) : null },
    };
  }

  const orjinal = (i) => (satirHaritasi ? satirHaritasi[mm.satirlar[i]] : mm.satirlar[i]);
  const ucDegerler = [];
  for (let i = 0; i < n; i++) {
    if (Math.abs(standartlastirilmis[i]) > ucDegerEsigi) {
      ucDegerler.push({ satir: orjinal(i), y: y[i], tahmin: yhat[i], artik: e[i], standartArtik: standartlastirilmis[i] });
    }
  }
  return {
    uyumlanan: yhat,
    artiklar: e,
    standartlastirilmisArtiklar: standartlastirilmis,
    studentArtiklar: student,
    silinmisStudentArtiklar: silinmis,
    kaldirac,
    merkezliKaldirac: sabit ? kaldirac.map((h) => h - 1 / n) : null,
    mahalanobis: sabit ? kaldirac.map((h) => (n - 1) * (h - 1 / n)) : null,
    cookD: cook,
    dffits,
    durbinWatson,
    breuschPagan,
    ucDegerEsigi,
    ucDegerSayisi: ucDegerler.length,
    ucDegerler,
  };
}

/**
 * Çoklu doğrusal regresyon (en küçük kareler; sütun pivotlu Householder QR).
 *
 * R karşılığı: `summary(lm(y ~ ...))`, `anova`, `drop1(fit, test = "F")`, `rstandard`, `rstudent`, `hatvalues`,
 * `cooks.distance`, `lmtest::bptest` (studentize = TRUE/FALSE), `lmtest::dwtest` istatistiği, `car::vif`.
 * SPSS karşılığı: REGRESSION (Model Summary, ANOVA, Coefficients: B, Std. Error, Beta, t, Sig., 95% CI,
 * Correlations: zero-order/partial/part, Collinearity: Tolerance/VIF; Durbin-Watson; ZRESID, SRESID, SDRESID,
 * COOK, LEVER (merkezli kaldıraç), MAHAL; Casewise diagnostics |ZRESID| > 3).
 *
 * Kategorik tahminciler gösterge kodlanır (referans varsayılanı ilk düzey). Tam doğrusal bağımlı sütunlar R'deki gibi
 * modelden atılır ve uyarı eklenir. Eksik değerli satırlar liste bazında atılır.
 *
 * @param {object} secenekler
 * @param {Array<number|null>} secenekler.y bağımlı değişken
 * @param {Array<object>} secenekler.tahminciler `{ad, tur:'surekli'|'kategorik', degerler, referans?, duzeyler?}` (bkz. model.js)
 * @param {string[][]} [secenekler.etkilesimler] ör. [['yas','grup']]
 * @param {boolean} [secenekler.sabit=true] sabit terim (SPSS: "Include constant in equation")
 * @param {number} [secenekler.duzey=0.95] güven düzeyi
 * @param {number} [secenekler.ucDegerEsigi=3] uç değer tanımı |standartlaştırılmış artık| > eşik (SPSS Casewise diagnostics)
 * @returns {object} `{yontem, ad, istatistik(F), istatistikAdi, sd:[sd1,sd2], p, n, etki, katsayilar:[{ad, terim, duzey, B, SH,
 *   beta, t, p, gaAlt, gaUst, tolerans, VIF, sifirDereceKorelasyon, kismiKorelasyon, yariKismiKorelasyon, bagimli}],
 *   modelOzeti:{R, R2, duzeltilmisR2, tahminSH, F, sd1, sd2, p, AIC, BIC, logOlabilirlik, durbinWatson, n},
 *   anovaTablosu:[{kaynak, KT, sd, KO, F, p}], terimTestleri:[{ad, tur, sd, KT, F, p, kismiEtaKare, etkilesimdeVar, GVIF?, GVIFdf?}],
 *   tanilama:{uyumlanan, artiklar, standartlastirilmisArtiklar, studentArtiklar, silinmisStudentArtiklar, kaldirac,
 *   merkezliKaldirac, mahalanobis, cookD, dffits, durbinWatson, breuschPagan:{koenker, klasik}, ucDegerSayisi, ucDegerler},
 *   kovaryans, bagimliSutunlar, sutunAdlari, terimler, satirlar, uyarilar}`
 */
export function dogrusalRegresyon(secenekler) {
  const { y, tahminciler = [], etkilesimler = [], sabit = true, duzey = 0.95, ucDegerEsigi = 3 } = secenekler || {};
  const yHam = bagimliHazirla(y);
  const mm = modelMatrisi(tahminciler, { sabit, etkilesimler, ekDiziler: [yHam] });
  const yv = mm.ekler[0].map((v) => sayiyaCevir(v, 'Bağımlı değişken'));
  return dogrusalUydur(mm, yv, { duzey, ucDegerEsigi });
}

/**
 * Basit doğrusal regresyon kısayolu: y = b0 + b1·x.
 * @param {Array<number|null>} x
 * @param {Array<number|null>} y
 * @param {{xAdi?:string, duzey?:number}} [secenekler]
 */
export function basitDogrusalRegresyon(x, y, secenekler = {}) {
  const { xAdi = 'x', duzey = 0.95, ucDegerEsigi = 3 } = secenekler;
  return dogrusalRegresyon({ y, tahminciler: [{ ad: xAdi, tur: 'surekli', degerler: x }], duzey, ucDegerEsigi });
}

function blokNormalize(b) {
  if (Array.isArray(b)) return { tahminciler: b, etkilesimler: [] };
  return { tahminciler: b.tahminciler || [], etkilesimler: b.etkilesimler || [] };
}

/**
 * Hiyerarşik (bloklu) doğrusal regresyon: her blok bir önceki modele eklenir; R² değişimi ve F değişimi testleri
 * (SPSS REGRESSION /METHOD=ENTER blok1 /METHOD=ENTER blok2, "R squared change"). Tüm bloklardaki değişkenlerde eksik
 * değeri olan satırlar baştan atılır (SPSS liste bazında silme), böylece tüm modeller aynı örneklemle kurulur.
 * Moderasyon analizi için son bloğa etkileşim eklenebilir: `{tahminciler: [], etkilesimler: [['x','m']]}`.
 *
 * @param {{y:Array, bloklar:Array<Array<object>|{tahminciler:object[], etkilesimler?:string[][]}>, duzey?:number}} secenekler
 * @returns {{yontem, ad, n, modeller:Array<object>, degisimler:Array<{model, R2, R2Degisim, FDegisim, sd1, sd2, p}>, uyarilar}}
 *   modeller[k]: k. bloğa kadar olan modelin dogrusalRegresyon sonucu.
 */
export function hiyerarsikRegresyon(secenekler) {
  const { y, bloklar, duzey = 0.95, ucDegerEsigi = 3 } = secenekler || {};
  if (!Array.isArray(bloklar) || bloklar.length === 0) throw new IstatistikHatasi('En az bir blok gerekir.');
  const bl = bloklar.map(blokNormalize);
  const tumTahminciler = bl.flatMap((b) => b.tahminciler);
  const tumEtkilesimler = bl.flatMap((b) => b.etkilesimler);
  const yHam = bagimliHazirla(y);
  const mm = modelMatrisi(tumTahminciler, { sabit: true, etkilesimler: tumEtkilesimler, ekDiziler: [yHam] });
  const yv = mm.ekler[0].map((v) => sayiyaCevir(v, 'Bağımlı değişken'));
  const modeller = [];
  const degisimler = [];
  const terimAdlari = [];
  let oncekiR2 = 0;
  let oncekiRank = 1;
  let oncekiRSS = kareToplamSapma(yv);
  for (let k = 0; k < bl.length; k++) {
    for (const t of bl[k].tahminciler) terimAdlari.push(t.ad);
    for (const e of bl[k].etkilesimler) {
      for (const b of e) {
        if (!terimAdlari.includes(b)) throw new IstatistikHatasi(`Etkileşimdeki '${b}' değişkeni aynı ya da daha önceki bir blokta ana etki olarak yer almalıdır.`);
      }
      terimAdlari.push(e.join(' × '));
    }
    if (terimAdlari.length === 0) throw new IstatistikHatasi(`${k + 1}. blok boş.`);
    const alt = altModelMatrisi(mm, terimAdlari);
    alt.uyarilar = k === 0 ? mm.uyarilar : [];
    const sonuc = dogrusalUydur(alt, yv, { duzey, ucDegerEsigi });
    sonuc.ad = `Hiyerarşik regresyon — Model ${k + 1}`;
    modeller.push(sonuc);
    const rss = sonuc.anovaTablosu[1].KT;
    const rank = sonuc.modelOzeti.sd1 + 1;
    const sd1 = rank - oncekiRank;
    const sd2 = sonuc.modelOzeti.sd2;
    const R2 = sonuc.modelOzeti.R2;
    const satir = { model: k + 1, R: sonuc.modelOzeti.R, R2, duzeltilmisR2: sonuc.modelOzeti.duzeltilmisR2, tahminSH: sonuc.modelOzeti.tahminSH, R2Degisim: R2 - oncekiR2, FDegisim: null, sd1, sd2, p: null };
    if (sd1 > 0) {
      satir.FDegisim = (oncekiRSS - rss) / sd1 / (rss / sd2);
      satir.p = pf(satir.FDegisim, sd1, sd2, false);
    }
    degisimler.push(satir);
    oncekiR2 = R2;
    oncekiRank = rank;
    oncekiRSS = rss;
  }
  const uyarilar = [];
  for (const m of modeller) for (const u of m.uyarilar) if (!uyarilar.includes(u)) uyarilar.push(u);
  return { yontem: 'hiyerarsik-regresyon', ad: 'Hiyerarşik (bloklu) doğrusal regresyon', n: mm.n, modeller, degisimler, uyarilar };
}

/**
 * Adımsal doğrusal regresyon (SPSS REGRESSION /METHOD = FORWARD | BACKWARD | STEPWISE, "Use probability of F").
 *
 * Kurallar (SPSS ile aynı): giriş için kısmi F testinin p'si `girisP`'den küçük olan adaylar arasında en küçük p'li terim
 * girer; çıkış için p'si `cikisP`'den büyük olanlar arasında en büyük p'li terim çıkar. 'adimsal' yöntemde her adımda
 * önce çıkış, sonra giriş denetlenir. Tolerans ölçütü (SPSS TOLERANCE(0.0001)): giren terim ya da modeldeki herhangi bir
 * sütunun toleransı 0.0001'in altına düşecekse terim giremez. Kategorik değişkenler tüm gösterge sütunlarıyla birlikte
 * tek terim olarak girer/çıkar (çok serbestlik dereceli kısmi F; SPSS REGRESSION'da göstergeler ayrı değişkenlerdir).
 * Kısmi F, R'nin `add1(fit, scope, test = "F")` / `drop1(fit, test = "F")` değerleriyle aynıdır.
 * Tüm aday değişkenlerde eksik değeri olan satırlar baştan atılır (SPSS liste bazında silme).
 *
 * @param {{y:Array, tahminciler:object[], yontem?:'ileri'|'geri'|'adimsal', girisP?:number, cikisP?:number,
 *   tolerans?:number, duzey?:number}} secenekler
 * @returns {{yontem, ad, n, adimlar:Array<{adim, islem:'giris'|'cikis', terim, F, sd1, sd2, p, R, R2, duzeltilmisR2,
 *   R2Degisim, modeldekiler:string[]}>, secilenler:string[], sonModel:object|null, dislananlar:Array<{ad, F, sd1, sd2, p,
 *   betaIcinde, kismiKorelasyon, minTolerans}>, uyarilar}}
 */
export function adimsalRegresyon(secenekler) {
  const { y, tahminciler, yontem = 'adimsal', girisP = 0.05, cikisP = 0.1, tolerans = 1e-4, duzey = 0.95, ucDegerEsigi = 3 } = secenekler || {};
  if (!['ileri', 'geri', 'adimsal'].includes(yontem)) throw new IstatistikHatasi("Yöntem 'ileri', 'geri' ya da 'adimsal' olmalıdır.");
  if (!Array.isArray(tahminciler) || tahminciler.length === 0) throw new IstatistikHatasi('Adımsal regresyon için en az bir aday değişken gerekir.');
  if (yontem === 'adimsal' && !(girisP < cikisP)) throw new IstatistikHatasi('Giriş olasılığı çıkış olasılığından küçük olmalıdır (SPSS: PIN < POUT).');
  const yHam = bagimliHazirla(y);
  const mm = modelMatrisi(tahminciler, { sabit: true, ekDiziler: [yHam] });
  const yv = mm.ekler[0].map((v) => sayiyaCevir(v, 'Bağımlı değişken'));
  const n = mm.n;
  const uyarilar = [...mm.uyarilar];
  const tumTerimler = mm.terimler.map((t) => t.ad);
  const yTSS = kareToplamSapma(yv);
  if (yTSS === 0) throw new IstatistikHatasi('Bağımlı değişken sabit (tüm değerler aynı); regresyon yapılamaz.');

  const onbellek = new Map();
  const uydur = (terimler) => {
    const anahtar = [...terimler].sort().join('\u0000');
    if (onbellek.has(anahtar)) return onbellek.get(anahtar);
    const idx = altSutunlar(mm, terimler);
    const Xs = sutunlariSec(mm.X, idx);
    const r = hizliEKK(Xs, yv);
    // Minimum tolerans: her sütun için 1/VIF
    let minTol = Infinity;
    if (r.ekk.bagimli.length) minTol = 0;
    else {
      for (let k = 1; k < idx.length; k++) {
        const xk = Xs.map((s) => s[k]);
        const sst = kareToplamSapma(xk);
        const tol = sst > 0 ? 1 / (r.ekk.XtXTers[k][k] * sst) : 0;
        if (tol < minTol) minTol = tol;
      }
    }
    const sonuc = { rss: r.rss, rank: r.rank, minTol, idx, ekk: r.ekk, Xs };
    onbellek.set(anahtar, sonuc);
    return sonuc;
  };
  const ozet = (fit) => {
    const R2 = 1 - fit.rss / yTSS;
    const sd2 = n - fit.rank;
    return { R: Math.sqrt(Math.max(R2, 0)), R2, duzeltilmisR2: 1 - (1 - R2) * ((n - 1) / sd2) };
  };
  const girisTesti = (mevcut, aday) => {
    const once = uydur(mevcut);
    const sonra = uydur([...mevcut, aday]);
    const sd1 = sonra.rank - once.rank;
    const sd2 = n - sonra.rank;
    if (sd1 <= 0 || sd2 <= 0 || sonra.minTol < tolerans) return { aday, F: null, sd1, sd2, p: null, minTol: sonra.minTol, uygun: false };
    const F = (once.rss - sonra.rss) / sd1 / (sonra.rss / sd2);
    return { aday, F, sd1, sd2, p: pf(F, sd1, sd2, false), minTol: sonra.minTol, uygun: true, sonra };
  };
  const cikisTesti = (mevcut, terim) => {
    const once = uydur(mevcut);
    const sonra = uydur(mevcut.filter((t) => t !== terim));
    const sd1 = once.rank - sonra.rank;
    const sd2 = n - once.rank;
    if (sd1 <= 0) return { terim, F: 0, sd1, sd2, p: 1 };
    const F = (sonra.rss - once.rss) / sd1 / (once.rss / sd2);
    return { terim, F, sd1, sd2, p: pf(F, sd1, sd2, false) };
  };

  let mevcut = yontem === 'geri' ? tumTerimler.slice() : [];
  if (yontem === 'geri') {
    const tam = uydur(mevcut);
    if (n - tam.rank <= 0) throw new IstatistikHatasi('Geriye doğru eleme için gözlem sayısı tüm değişkenlerin parametre sayısından fazla olmalıdır.');
  }
  const adimlar = [];
  const adimKaydet = (islem, test) => {
    const once = adimlar.length ? adimlar[adimlar.length - 1].R2 : yontem === 'geri' ? null : 0;
    const fit = uydur(mevcut);
    const o = ozet(fit);
    adimlar.push({
      adim: adimlar.length + 1,
      islem,
      terim: islem === 'giris' ? test.aday : test.terim,
      F: test.F,
      sd1: test.sd1,
      sd2: test.sd2,
      p: test.p,
      ...o,
      R2Degisim: once === null ? null : o.R2 - once,
      modeldekiler: mevcut.slice(),
    });
  };
  if (yontem === 'geri') {
    // başlangıç modeli kaydı (SPSS Model 1 = tüm değişkenler)
    const fit = uydur(mevcut);
    adimlar.push({ adim: 0, islem: 'baslangic', terim: null, F: null, sd1: null, sd2: null, p: null, ...ozet(fit), R2Degisim: null, modeldekiler: mevcut.slice() });
  }
  const maksAdim = 2 * tumTerimler.length + 2;
  let adimSayisi = 0;
  while (adimSayisi++ < maksAdim) {
    let degisti = false;
    if ((yontem === 'geri' || yontem === 'adimsal') && mevcut.length) {
      const testler = mevcut.map((t) => cikisTesti(mevcut, t));
      let enKotu = null;
      for (const t of testler) if (t.p > cikisP && (!enKotu || t.p > enKotu.p || (t.p === enKotu.p && t.F < enKotu.F))) enKotu = t;
      if (enKotu) {
        mevcut = mevcut.filter((t) => t !== enKotu.terim);
        adimKaydet('cikis', enKotu);
        degisti = true;
        if (yontem === 'adimsal') continue;
      }
    }
    if (yontem === 'geri') {
      if (!degisti) break;
      continue;
    }
    const adaylar = tumTerimler.filter((t) => !mevcut.includes(t));
    let enIyi = null;
    for (const a of adaylar) {
      const t = girisTesti(mevcut, a);
      if (t.uygun && t.p < girisP && (!enIyi || t.p < enIyi.p || (t.p === enIyi.p && t.F > enIyi.F))) enIyi = t;
    }
    if (enIyi) {
      mevcut = [...mevcut, enIyi.aday];
      adimKaydet('giris', enIyi);
      degisti = true;
    }
    if (!degisti) break;
  }
  if (adimSayisi > maksAdim) uyarilar.push('Adımsal seçim en fazla adım sayısına ulaştı; seçim döngüye girmiş olabilir.');

  let sonModel = null;
  if (mevcut.length) {
    const sira = tumTerimler.filter((t) => mevcut.includes(t));
    const alt = altModelMatrisi(mm, sira);
    alt.uyarilar = [];
    sonModel = dogrusalUydur(alt, yv, { duzey, ucDegerEsigi });
  } else {
    uyarilar.push('Hiçbir değişken giriş ölçütünü karşılamadı; model yalnızca sabit terimden oluşuyor.');
  }

  // Dışlanan değişkenler (SPSS "Excluded Variables": Beta In, t/F, Sig., Partial Correlation, Minimum Tolerance)
  const sdY = Math.sqrt(yTSS / (n - 1));
  const dislananlar = [];
  for (const a of tumTerimler.filter((t) => !mevcut.includes(t))) {
    const t = girisTesti(mevcut, a);
    const satir = { ad: a, F: t.F, sd1: t.sd1, sd2: t.sd2, p: t.p, t: null, betaIcinde: null, kismiKorelasyon: null, minTolerans: t.minTol };
    if (t.uygun && t.sd1 === 1) {
      const terim = mm.terimler.find((u) => u.ad === a);
      const j = terim.sutunlar[0];
      const k = t.sonra.idx.indexOf(j);
      const B = t.sonra.ekk.katsayi[k];
      const xk = mm.X.map((s) => s[j]);
      satir.betaIcinde = (B * Math.sqrt(kareToplamSapma(xk) / (n - 1))) / sdY;
      satir.t = Math.sign(B) * Math.sqrt(t.F);
      satir.kismiKorelasyon = satir.t / Math.sqrt(satir.t * satir.t + t.sd2);
    }
    dislananlar.push(satir);
  }

  const secilenler = tumTerimler.filter((t) => mevcut.includes(t));
  const adlar = { ileri: 'İleriye doğru seçim', geri: 'Geriye doğru eleme', adimsal: 'Adımsal (stepwise) seçim' };
  return {
    yontem: 'adimsal-regresyon',
    ad: `Adımsal doğrusal regresyon — ${adlar[yontem]}`,
    secimYontemi: yontem,
    girisP,
    cikisP,
    n,
    adimlar,
    secilenler,
    sonModel,
    dislananlar,
    satirlar: mm.satirlar,
    uyarilar,
  };
}

