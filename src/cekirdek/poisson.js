// Poisson regresyonu (log bağlantı): sayım verileri, olay hızları (offset = log(maruziyet)), IRR ve aşırı yayılım.
// R glm(family = poisson) ile doğrulanmıştır; SPSS GENLIN (Poisson loglinear) tablolarıyla aynı yapıdadır.

import { pchisq, qnorm, pnorm } from './dagilim.js';
import { IstatistikHatasi } from './hata.js';
import { modelMatrisi, sayiyaCevir } from './model.js';
import { AILELER, glmUydur, glmIRLS, profilGuvenAraligi, glmTanilama, waldTesti } from './lojistik.js';

/**
 * Poisson regresyonu (log bağlantı, en çok olabilirlik; R glm.fit ile aynı IRLS + tam yakınsama).
 *
 * R karşılığı: `glm(y ~ ... + offset(log(t)), family = poisson)`, `confint` (profil; burada kesin kök bulma ile),
 * `drop1(test = "LRT")`. SPSS karşılığı: GENLIN /MODEL DISTRIBUTION=POISSON LINK=LOG (Goodness of Fit: Deviance,
 * Pearson Chi-Square, Value/df, Log Likelihood (tam), AIC, BIC; Omnibus Test; Parameter Estimates: B, Std. Error, Wald
 * χ², Sig., Exp(B) ve Wald GA).
 * Aşırı yayılım (overdispersion): Pearson χ²/sd > 1,5 ise uyarı (negatif binom ya da quasi-Poisson önerilir).
 *
 * @param {{y:Array<number|null>, tahminciler:object[], etkilesimler?:string[][], offset?:Array<number|null>,
 *   maruziyet?:Array<number|null>, duzey?:number, profilGA?:boolean, sabit?:boolean}} secenekler
 *   offset: doğrusal tahmine eklenecek bilinen terim (ör. log(kişi-yıl)); maruziyet verilirse offset = log(maruziyet).
 * @returns {{yontem, ad, istatistik, istatistikAdi, sd, p, n, katsayilar:Array<{ad, terim, duzey, B, SH, z, wald, sd, p,
 *   IRR, irrGaAlt, irrGaUst, gaAlt, gaUst, profilGaAlt, profilGaUst, irrProfilGaAlt, irrProfilGaUst, bagimli, ayrisma}>,
 *   terimTestleri:Array<{ad, tur, sd, wald, waldP, lrKiKare, lrP}>, modelOzeti:{sapma, sapmaSifir, sapmaSd, sapmaOran,
 *   pearsonKiKare, pearsonOran, kiKare, sd, p, AIC, BIC, logOlabilirlik, iterasyon, n}, tahminler, kovaryans, uyarilar}}
 */
export function poissonRegresyon(secenekler) {
  const { y, tahminciler = [], etkilesimler = [], offset = null, maruziyet = null, duzey = 0.95, profilGA: profilIste = true, sabit = true, maksIterasyon = 25 } = secenekler || {};
  if (!Array.isArray(y)) throw new IstatistikHatasi('Bağımlı değişken (y) bir dizi olmalıdır.');
  if (offset && maruziyet) throw new IstatistikHatasi('offset ve maruziyet birlikte verilemez.');
  const ek = [y];
  if (offset) ek.push(offset);
  if (maruziyet) ek.push(maruziyet);
  const mm = modelMatrisi(tahminciler, { sabit, etkilesimler, ekDiziler: ek });
  const uyarilar = [...mm.uyarilar];
  const yv = mm.ekler[0].map((v) => sayiyaCevir(v, 'Bağımlı değişken'));
  if (yv.some((v) => v < 0)) throw new IstatistikHatasi('Poisson regresyonu için bağımlı değişken negatif olamaz (sayım verisi gerekir).');
  if (yv.some((v) => !Number.isInteger(v))) uyarilar.push('Bağımlı değişkende tam sayı olmayan değerler var; Poisson olabilirliği sayım verisi varsayar.');
  let off = null;
  if (offset) off = mm.ekler[1].map((v) => sayiyaCevir(v, 'offset'));
  if (maruziyet) {
    off = mm.ekler[1].map((v) => {
      const t = sayiyaCevir(v, 'maruziyet');
      if (!(t > 0)) throw new IstatistikHatasi('Maruziyet (süre/nüfus) değerleri pozitif olmalıdır.');
      return Math.log(t);
    });
  }
  const n = yv.length;
  if (mm.sutunAdlari.length === 0) throw new IstatistikHatasi('Modelde hiç terim yok.');
  if (yv.every((v) => v === 0)) throw new IstatistikHatasi('Bağımlı değişkenin tüm değerleri sıfır; Poisson regresyonu kestirilemez.');
  const aile = AILELER.poisson;
  const uyum = glmUydur(mm, yv, aile, { offset: off, maksIterasyon });
  if (!uyum.XtXTers) throw new IstatistikHatasi('Model kestirilemedi: bilgi matrisi tekil.');
  const p = mm.sutunAdlari.length;
  const rank = uyum.rank;
  const bagimliAdlar = uyum.bagimli.map((j) => mm.sutunAdlari[j]);
  if (bagimliAdlar.length) uyarilar.push(`Tam çoklu doğrusal bağlantı: ${bagimliAdlar.map((a) => `'${a}'`).join(', ')} diğer değişkenlerin doğrusal bileşimi olduğu için modelden çıkarıldı (R'de katsayısı NA görünür).`);
  const ayrisanSet = new Set(uyum.ayrisanSutunlar);
  if (!uyum.cilaYakinsadi) {
    uyarilar.push(
      ayrisanSet.size
        ? `Model yakınsamadı: ${[...ayrisanSet].map((j) => `'${mm.sutunAdlari[j]}'`).join(', ')} katsayısı sonsuza ıraksıyor (ör. bir kategorideki tüm sayımlar sıfır); bu katsayılar raporlanmadı.`
        : 'Model yakınsamadı; sonuçlar güvenilir olmayabilir.',
    );
  }
  const D = uyum.sapma;
  // Sıfır modeli: sabitli → μ = exp(β0 + offset), β0 = log(Σy / Σexp(offset)); sabitsiz → μ = exp(offset)
  const offs = off || new Array(n).fill(0);
  let sifirEta;
  if (sabit) {
    const sy = yv.reduce((a, b) => a + b, 0);
    const se = offs.reduce((a, b) => a + Math.exp(b), 0);
    const b0 = Math.log(sy / se);
    sifirEta = offs.map((o) => b0 + o);
  } else sifirEta = offs.slice();
  let sapmaSifir = 0;
  for (let i = 0; i < n; i++) sapmaSifir += aile.sapmaKatkisi(yv[i], sifirEta[i]);
  const kiKare = sapmaSifir - D;
  const sdModel = rank - (sabit ? 1 : 0);
  const pModel = sdModel > 0 ? pchisq(Math.max(kiKare, 0), sdModel, false) : null;
  let logL = 0;
  for (let i = 0; i < n; i++) logL += aile.logOlabilirlik(yv[i], uyum.eta[i]);
  const tahminler = glmTanilama(uyum, yv, aile);
  const mu = tahminler.olasilik;
  tahminler.beklenen = mu;
  delete tahminler.olasilik;
  let pearsonKiKare = 0;
  for (let i = 0; i < n; i++) pearsonKiKare += (yv[i] - mu[i]) ** 2 / mu[i];
  const sapmaSd = n - rank;
  const pearsonOran = sapmaSd > 0 ? pearsonKiKare / sapmaSd : null;
  if (pearsonOran !== null && pearsonOran > 1.5) {
    uyarilar.push(
      `Aşırı yayılım (overdispersion) var: Pearson χ²/sd = ${pearsonOran.toFixed(2).replace('.', ',')}. Standart hatalar olduğundan küçük, p değerleri olduğundan küçük olabilir; negatif binom ya da quasi-Poisson model düşünün.`,
    );
  }

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
      IRR: null,
      irrGaAlt: null,
      irrGaUst: null,
      gaAlt: null,
      gaUst: null,
      profilGaAlt: null,
      profilGaUst: null,
      irrProfilGaAlt: null,
      irrProfilGaUst: null,
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
      satir.IRR = Math.exp(B);
      satir.gaAlt = B - z * SH;
      satir.gaUst = B + z * SH;
      satir.irrGaAlt = Math.exp(satir.gaAlt);
      satir.irrGaUst = Math.exp(satir.gaUst);
    }
    katsayilar.push(satir);
  }
  if (profilIste && uyum.cilaYakinsadi) {
    for (const j of uyum.kullanilan) {
      const [a, b] = profilGuvenAraligi(uyum, yv, aile, j, duzey, off);
      const s = katsayilar[j];
      s.profilGaAlt = a;
      s.profilGaUst = b;
      s.irrProfilGaAlt = a === null ? null : Math.exp(a);
      s.irrProfilGaUst = b === null ? null : Math.exp(b);
    }
  }

  const terimTestleri = [];
  for (const t of mm.terimler) {
    const w = waldTesti(uyum.katsayi.map((v, j) => (ayrisanSet.has(j) ? null : v)), uyum.kovaryans, t.sutunlar);
    const satir = { ad: t.ad, tur: t.tur, sd: w.sd, wald: w.wald, waldP: w.p, lrKiKare: null, lrSd: null, lrP: null };
    const sil = new Set(t.sutunlar);
    const kalan = uyum.kullanilan.filter((j) => !sil.has(j));
    const sd = rank - kalan.length;
    if (sd > 0) {
      const f = glmIRLS(mm.X.map((s) => kalan.map((j) => s[j])), yv, aile, { offset: off, maksIterasyon, kovaryansIste: false });
      satir.lrKiKare = f.sapma - D;
      satir.lrSd = sd;
      satir.lrP = pchisq(Math.max(satir.lrKiKare, 0), sd, false);
    }
    terimTestleri.push(satir);
  }

  return {
    yontem: 'poisson-regresyon',
    ad: 'Poisson regresyonu',
    istatistik: kiKare,
    istatistikAdi: 'χ²',
    sd: sdModel,
    p: pModel,
    n,
    katsayilar,
    terimTestleri,
    modelOzeti: {
      sapma: D,
      sapmaSifir,
      sapmaSd,
      sapmaOran: sapmaSd > 0 ? D / sapmaSd : null,
      pearsonKiKare,
      pearsonOran,
      kiKare,
      sd: sdModel,
      p: pModel,
      AIC: -2 * logL + 2 * rank,
      BIC: -2 * logL + Math.log(n) * rank,
      logOlabilirlik: logL,
      iterasyon: uyum.iterasyon,
      n,
    },
    tahminler,
    kovaryans: uyum.kovaryans,
    yakinsadi: uyum.cilaYakinsadi,
    bagimliSutunlar: bagimliAdlar,
    sutunAdlari: mm.sutunAdlari,
    terimler: mm.terimler,
    satirlar: mm.satirlar,
    duzey,
    uyarilar,
  };
}

