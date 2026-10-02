// Parametrik ortalama karşılaştırmaları: tek örneklem, bağımsız (Student / Welch) ve eşleştirilmiş t testleri,
// tek yönlü ANOVA, Welch ANOVA ve Brown-Forsythe ANOVA. Etki büyüklükleri ve güven aralıkları dahildir.
//
// Alternatif hipotez: 'iki' (iki yönlü, varsayılan), 'buyuk' (μ > μ0 ya da μ1 > μ2), 'kucuk'.
// Tek yönlü alternatiflerde ortalama farkının güven aralığı R t.test() gibi tek yönlüdür (bir ucu ±∞);
// etki büyüklüklerinin güven aralıkları her zaman iki yönlüdür (SPSS ve MBESS gibi).

import { pf, pt, qt } from './dagilim.js';
import { maksimum, minimum, ortalama, toplam, varyans } from './betimsel.js';
import { cohenDGA, etaKareGA, etkiNesnesi, hedgesJ, tekOrneklemDGA, yorumD, yorumEtaKare } from './etki.js';
import { IstatistikHatasi } from './hata.js';
import { EPS } from './sayisal.js';

const ALTERNATIFLER = ['iki', 'buyuk', 'kucuk'];

function secenekleriDenetle(alternatif, duzey) {
  if (!ALTERNATIFLER.includes(alternatif)) throw new IstatistikHatasi(`Bilinmeyen alternatif hipotez: ${alternatif}`);
  if (!(duzey > 0 && duzey < 1)) throw new IstatistikHatasi('Güven düzeyi 0 ile 1 arasında olmalıdır.');
}

function sayiDenetle(x, testAdi) {
  if (!Array.isArray(x) && !ArrayBuffer.isView(x)) throw new IstatistikHatasi(`${testAdi} için sayı dizisi gerekir.`);
  for (const v of x) {
    if (typeof v !== 'number' || !Number.isFinite(v)) throw new IstatistikHatasi(`${testAdi}: veride eksik ya da sayısal olmayan değer var.`);
  }
}

/** t istatistiğinin p değeri */
function tP(t, df, alternatif) {
  if (alternatif === 'iki') return Math.min(1, 2 * pt(-Math.abs(t), df));
  if (alternatif === 'buyuk') return pt(t, df, false);
  return pt(t, df);
}

/** tahmin ± t·SH güven aralığı (tek yönlü alternatiflerde bir ucu sonsuz) */
function tGA(tahmin, sh, df, alternatif, duzey) {
  if (alternatif === 'iki') {
    const q = qt(1 - (1 - duzey) / 2, df);
    return { alt: tahmin - q * sh, ust: tahmin + q * sh };
  }
  const q = qt(duzey, df);
  if (alternatif === 'buyuk') return { alt: tahmin - q * sh, ust: Infinity };
  return { alt: -Infinity, ust: tahmin + q * sh };
}

function grupOzeti(x, ad) {
  const n = x.length;
  const m = ortalama(x);
  const v = n > 1 ? varyans(x, m) : NaN;
  const ss = Math.sqrt(v);
  const o = { n, ortalama: m, standartSapma: n > 1 ? ss : null, standartHata: n > 1 ? ss / Math.sqrt(n) : null };
  if (ad !== undefined) o.ad = ad;
  return o;
}

function sabitDenetle(sh, olcek) {
  // R t.test: "data are essentially constant"
  if (!(sh > 0) || sh < 10 * EPS * olcek) throw new IstatistikHatasi('Veriler sabit (varyans sıfır); t testi hesaplanamaz.');
}

function pearsonR(x, y) {
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
  return Math.max(-1, Math.min(1, sxy / Math.sqrt(sxx * syy)));
}

// ---------- t testleri ----------

/**
 * Tek örneklem t testi (SPSS T-TEST TESTVAL; R t.test(x, mu)).
 * Etki büyüklüğü: Cohen d = (x̄ − μ0) / s, merkezi olmayan t ile güven aralığı; Hedges g = d·J(n − 1).
 * @param {number[]} x
 * @param {{mu?:number, alternatif?:'iki'|'buyuk'|'kucuk', duzey?:number}} [secenek]
 * @returns sonuç nesnesi; ga: ortalama farkının (x̄ − μ0) GA'sı (SPSS), ortalamaGA: ortalamanın GA'sı (R)
 */
export function tekOrneklemT(x, secenek = {}) {
  const mu = secenek.mu ?? 0;
  const alternatif = secenek.alternatif ?? 'iki';
  const duzey = secenek.duzey ?? 0.95;
  secenekleriDenetle(alternatif, duzey);
  sayiDenetle(x, 'Tek örneklem t testi');
  if (x.length < 2) throw new IstatistikHatasi('Tek örneklem t testi için en az 2 gözlem gerekir.');
  if (!Number.isFinite(mu)) throw new IstatistikHatasi('Test değeri (μ0) sayı olmalıdır.');
  const o = grupOzeti(x);
  const n = o.n;
  const df = n - 1;
  const sh = o.standartHata;
  sabitDenetle(sh, Math.abs(o.ortalama));
  const fark = o.ortalama - mu;
  const t = fark / sh;
  const ga = tGA(fark, sh, df, alternatif, duzey);
  const d = fark / o.standartSapma;
  const dGA = tekOrneklemDGA(d, n, duzey);
  const J = hedgesJ(df);
  return {
    yontem: 'tek-orneklem-t',
    ad: 'Tek örneklem t testi',
    istatistik: t,
    istatistikAdi: 't',
    sd: df,
    p: tP(t, df, alternatif),
    alternatif,
    mu,
    etki: etkiNesnesi('Cohen d', 'd', d, dGA, yorumD(d)),
    hedgesG: etkiNesnesi('Hedges g', 'g', d * J, { alt: dGA.alt * J, ust: dGA.ust * J }, yorumD(d * J)),
    ga: { duzey, alt: ga.alt, ust: ga.ust, neyin: 'ortalama farkı' },
    ortalamaGA: { duzey, alt: ga.alt + mu, ust: ga.ust + mu, neyin: 'ortalama' },
    ortalamaFarki: fark,
    betimsel: o,
    n,
    uyarilar: [],
  };
}

/**
 * Bağımsız iki örneklem t testi.
 * esitVaryans = true (varsayılan): Student t, SPSS "Equal variances assumed" satırı (R t.test(var.equal = TRUE)).
 * esitVaryans = false: Welch t, SPSS "Equal variances not assumed" satırı (R t.test() varsayılanı).
 * Fark = x̄₁ − x̄₂. Etki büyüklükleri her iki durumda da birleşik (pooled) standart sapmayla hesaplanır:
 * Cohen d (merkezi olmayan t ile GA, MBESS ci.smd), Hedges g = d·J(n₁ + n₂ − 2) (GA sınırları da J ile çarpılır),
 * Glass Δ = fark / s₂ (ikinci grup kontrol kabul edilir; yalnızca nokta tahmini).
 * @param {number[]} x birinci grup
 * @param {number[]} y ikinci grup
 * @param {{esitVaryans?:boolean, alternatif?:'iki'|'buyuk'|'kucuk', duzey?:number, mu?:number, adlar?:string[]}} [secenek]
 */
export function bagimsizT(x, y, secenek = {}) {
  const esitVaryans = secenek.esitVaryans ?? true;
  const alternatif = secenek.alternatif ?? 'iki';
  const duzey = secenek.duzey ?? 0.95;
  const mu = secenek.mu ?? 0;
  const adlar = secenek.adlar ?? [];
  secenekleriDenetle(alternatif, duzey);
  sayiDenetle(x, 'Bağımsız örneklem t testi');
  sayiDenetle(y, 'Bağımsız örneklem t testi');
  if (x.length < 2 || y.length < 2) throw new IstatistikHatasi('Bağımsız örneklem t testi için her grupta en az 2 gözlem gerekir.');
  const a = grupOzeti(x, adlar[0]);
  const b = grupOzeti(y, adlar[1]);
  const n1 = a.n;
  const n2 = b.n;
  const v1 = a.standartSapma ** 2;
  const v2 = b.standartSapma ** 2;
  const fark = a.ortalama - b.ortalama;
  const sdHavuz = n1 + n2 - 2;
  const vHavuz = ((n1 - 1) * v1 + (n2 - 1) * v2) / sdHavuz;
  let sh;
  let df;
  if (esitVaryans) {
    sh = Math.sqrt(vHavuz * (1 / n1 + 1 / n2));
    df = sdHavuz;
  } else {
    const s1 = v1 / n1;
    const s2 = v2 / n2;
    sh = Math.sqrt(s1 + s2);
    df = (s1 + s2) ** 2 / ((s1 * s1) / (n1 - 1) + (s2 * s2) / (n2 - 1));
  }
  sabitDenetle(sh, Math.max(Math.abs(a.ortalama), Math.abs(b.ortalama)));
  const t = (fark - mu) / sh;
  const ga = tGA(fark, sh, df, alternatif, duzey);
  const ssHavuz = Math.sqrt(vHavuz);
  const d = fark / ssHavuz;
  const dGA = cohenDGA(d, n1, n2, duzey);
  const J = hedgesJ(sdHavuz);
  const uyarilar = [];
  return {
    yontem: esitVaryans ? 'student-t' : 'welch-t',
    ad: esitVaryans ? 'Bağımsız örneklem t testi (Student)' : 'Welch t testi',
    istatistik: t,
    istatistikAdi: 't',
    sd: df,
    p: tP(t, df, alternatif),
    alternatif,
    esitVaryans,
    etki: etkiNesnesi('Cohen d', 'd', d, dGA, yorumD(d)),
    hedgesG: etkiNesnesi('Hedges g', 'g', d * J, { alt: dGA.alt * J, ust: dGA.ust * J }, yorumD(d * J)),
    glassDelta: b.standartSapma > 0 ? etkiNesnesi('Glass Δ', 'Δ', fark / b.standartSapma, null, yorumD(fark / b.standartSapma)) : null,
    ga: { duzey, alt: ga.alt, ust: ga.ust, neyin: 'ortalama farkı' },
    ortalamaFarki: fark,
    farkSH: sh,
    birlesikSS: ssHavuz,
    gruplar: [a, b],
    n: n1 + n2,
    uyarilar,
  };
}

/**
 * Eşleştirilmiş (bağımlı) örneklem t testi; farklar d = x − y üzerinde tek örneklem t testi
 * (SPSS T-TEST PAIRS; R t.test(x, y, paired = TRUE)).
 * Etki büyüklüğü: d_z = d̄ / s_d (SPSS "Cohen's d", standartlaştırıcı farkların SS'si), merkezi olmayan t ile GA;
 * Hedges g_z = d_z·J(n − 1). Ayrıca ölçümler arası Pearson korelasyonu (SPSS "Paired Samples Correlations").
 * @param {number[]} x birinci ölçüm
 * @param {number[]} y ikinci ölçüm (aynı denekler, aynı sıra)
 * @param {{alternatif?:'iki'|'buyuk'|'kucuk', duzey?:number, mu?:number, adlar?:string[]}} [secenek]
 */
export function eslestirilmisT(x, y, secenek = {}) {
  const alternatif = secenek.alternatif ?? 'iki';
  const duzey = secenek.duzey ?? 0.95;
  const mu = secenek.mu ?? 0;
  const adlar = secenek.adlar ?? [];
  secenekleriDenetle(alternatif, duzey);
  sayiDenetle(x, 'Eşleştirilmiş t testi');
  sayiDenetle(y, 'Eşleştirilmiş t testi');
  if (x.length !== y.length) throw new IstatistikHatasi('Eşleştirilmiş t testi için iki ölçümün gözlem sayısı eşit olmalıdır.');
  if (x.length < 2) throw new IstatistikHatasi('Eşleştirilmiş t testi için en az 2 gözlem çifti gerekir.');
  const farklar = x.map((v, i) => v - y[i]);
  const o = grupOzeti(farklar);
  const n = o.n;
  const df = n - 1;
  const sh = o.standartHata;
  sabitDenetle(sh, Math.abs(o.ortalama));
  const t = (o.ortalama - mu) / sh;
  const ga = tGA(o.ortalama, sh, df, alternatif, duzey);
  const dz = o.ortalama / o.standartSapma;
  const dGA = tekOrneklemDGA(dz, n, duzey);
  const J = hedgesJ(df);
  const r = pearsonR(x, y);
  let korelasyon = null;
  if (Number.isFinite(r) && n > 2) {
    const tr = Math.abs(r) === 1 ? Math.sign(r) * Infinity : (r * Math.sqrt(n - 2)) / Math.sqrt(1 - r * r);
    korelasyon = { r, p: Math.abs(r) === 1 ? 0 : Math.min(1, 2 * pt(-Math.abs(tr), n - 2)) };
  }
  return {
    yontem: 'eslestirilmis-t',
    ad: 'Eşleştirilmiş örneklem t testi',
    istatistik: t,
    istatistikAdi: 't',
    sd: df,
    p: tP(t, df, alternatif),
    alternatif,
    etki: etkiNesnesi('Cohen d (d_z)', 'd_z', dz, dGA, yorumD(dz)),
    hedgesG: etkiNesnesi('Hedges g (g_z)', 'g_z', dz * J, { alt: dGA.alt * J, ust: dGA.ust * J }, yorumD(dz * J)),
    ga: { duzey, alt: ga.alt, ust: ga.ust, neyin: 'ortalama farkı' },
    ortalamaFarki: o.ortalama,
    farklar: { ortalama: o.ortalama, standartSapma: o.standartSapma, standartHata: sh },
    olcumler: [grupOzeti(x, adlar[0]), grupOzeti(y, adlar[1])],
    korelasyon,
    n,
    uyarilar: [],
  };
}

// ---------- Tek yönlü ANOVA ve dayanıklı alternatifleri ----------

function anovaGruplariDenetle(gruplar, testAdi, enAz = 1) {
  if (!Array.isArray(gruplar) || gruplar.length < 2) throw new IstatistikHatasi(`${testAdi} için en az 2 grup gerekir.`);
  for (const g of gruplar) {
    sayiDenetle(g, testAdi);
    if (g.length < enAz) throw new IstatistikHatasi(enAz > 1 ? `${testAdi} için her grupta en az ${enAz} gözlem gerekir.` : `${testAdi}: boş grup var.`);
  }
}

/**
 * Tek yönlü varyans analizi (SPSS ONEWAY; R aov / oneway.test(var.equal = TRUE)).
 * Etki büyüklükleri: η² = KT_ara / KT_toplam (merkezi olmayan F ile iki yönlü GA; MBESS ci.pvaf),
 * ω² = (KT_ara − (k−1) KO_iç) / (KT_toplam + KO_iç), ε² = (KT_ara − (k−1) KO_iç) / KT_toplam
 * (ω² ve ε² negatif çıkabilir; SPSS gibi kırpılmadan verilir), Cohen f = √(η² / (1 − η²)).
 * @param {number[][]} gruplar
 * @param {{adlar?:string[], duzey?:number}} [secenek]
 */
export function tekYonluAnova(gruplar, secenek = {}) {
  const duzey = secenek.duzey ?? 0.95;
  const adlar = secenek.adlar ?? [];
  secenekleriDenetle('iki', duzey);
  anovaGruplariDenetle(gruplar, 'Tek yönlü ANOVA');
  const k = gruplar.length;
  const N = gruplar.reduce((s, g) => s + g.length, 0);
  if (N - k < 1) throw new IstatistikHatasi('Tek yönlü ANOVA için toplam gözlem sayısı grup sayısından büyük olmalıdır.');
  const ortalamalar = gruplar.map((g) => ortalama(g));
  const genel = ortalama(gruplar.flat());
  const KTa = toplam(gruplar.map((g, i) => g.length * (ortalamalar[i] - genel) ** 2));
  const KTi = toplam(gruplar.flatMap((g, i) => g.map((v) => (v - ortalamalar[i]) ** 2)));
  if (!(KTi > 0)) throw new IstatistikHatasi('Grup içi varyans sıfır (her grupta tüm değerler aynı); F hesaplanamaz.');
  const KTt = KTa + KTi;
  const sd1 = k - 1;
  const sd2 = N - k;
  const KOa = KTa / sd1;
  const KOi = KTi / sd2;
  const F = KOa / KOi;
  const p = pf(F, sd1, sd2, false);
  const eta2 = KTa / KTt;
  const etaGA = etaKareGA(F, sd1, sd2, duzey);
  const omega2 = (KTa - sd1 * KOi) / (KTt + KOi);
  const epsilon2 = (KTa - sd1 * KOi) / KTt;
  const tGrup = gruplar.map((g, i) => {
    const o = grupOzeti(g, adlar[i] ?? `Grup ${i + 1}`);
    if (o.n > 1) {
      const q = qt(1 - (1 - duzey) / 2, o.n - 1);
      o.gaAlt = o.ortalama - q * o.standartHata;
      o.gaUst = o.ortalama + q * o.standartHata;
    } else {
      o.gaAlt = null;
      o.gaUst = null;
    }
    o.minimum = minimum(g);
    o.maksimum = maksimum(g);
    return o;
  });
  const uyarilar = [];
  if (gruplar.some((g) => g.length < 2)) uyarilar.push('Bazı gruplarda yalnızca 1 gözlem var.');
  return {
    yontem: 'tek-yonlu-anova',
    ad: 'Tek yönlü varyans analizi (ANOVA)',
    istatistik: F,
    istatistikAdi: 'F',
    sd: [sd1, sd2],
    p,
    etki: etkiNesnesi('Eta kare', 'η²', eta2, etaGA, yorumEtaKare(eta2)),
    etaKare: eta2,
    omegaKare: omega2,
    epsilonKare: epsilon2,
    cohenF: Math.sqrt(eta2 / (1 - eta2)),
    anovaTablosu: [
      { kaynak: 'Gruplar arası', KT: KTa, sd: sd1, KO: KOa, F, p },
      { kaynak: 'Gruplar içi', KT: KTi, sd: sd2, KO: KOi, F: null, p: null },
      { kaynak: 'Toplam', KT: KTt, sd: N - 1, KO: null, F: null, p: null },
    ],
    gruplar: tGrup,
    genelOrtalama: genel,
    duzey,
    n: N,
    uyarilar,
  };
}

/**
 * Welch ANOVA: varyansların eşit olmadığı durumda ortalamaların karşılaştırılması
 * (Welch 1951; SPSS ONEWAY "Robust Tests of Equality of Means: Welch"; R oneway.test(var.equal = FALSE)).
 * Her grupta en az 2 gözlem ve sıfırdan büyük varyans gerekir.
 * @param {number[][]} gruplar
 */
export function welchAnova(gruplar) {
  anovaGruplariDenetle(gruplar, 'Welch ANOVA', 2);
  const k = gruplar.length;
  const n = gruplar.map((g) => g.length);
  const m = gruplar.map((g) => ortalama(g));
  const v = gruplar.map((g, i) => varyans(g, m[i]));
  if (v.some((s) => !(s > 0))) throw new IstatistikHatasi('Welch ANOVA: varyansı sıfır olan grup var.');
  const w = n.map((ni, i) => ni / v[i]);
  const W = toplam(w);
  const mw = toplam(w.map((wi, i) => wi * m[i])) / W;
  const tmp = toplam(w.map((wi, i) => (1 - wi / W) ** 2 / (n[i] - 1))) / (k * k - 1);
  const F = toplam(w.map((wi, i) => wi * (m[i] - mw) ** 2)) / ((k - 1) * (1 + 2 * (k - 2) * tmp));
  const sd1 = k - 1;
  const sd2 = 1 / (3 * tmp);
  return {
    yontem: 'welch-anova',
    ad: 'Welch ANOVA',
    istatistik: F,
    istatistikAdi: 'F',
    sd: [sd1, sd2],
    p: pf(F, sd1, sd2, false),
    n: n.reduce((s, x) => s + x, 0),
    uyarilar: [],
  };
}

/**
 * Brown-Forsythe (1974) ortalamaların eşitliği testi (SPSS ONEWAY "Robust Tests: Brown-Forsythe").
 * F* = Σ nᵢ(x̄ᵢ − x̄)² / Σ (1 − nᵢ/N) sᵢ²; sd1 = k − 1,
 * sd2 = 1 / Σ (cᵢ² / (nᵢ − 1)), cᵢ = (1 − nᵢ/N) sᵢ² / Σⱼ (1 − nⱼ/N) sⱼ² (Satterthwaite yaklaşımı).
 * (Varyans homojenliği için kullanılan "Brown-Forsythe Levene" testiyle karıştırılmamalıdır.)
 * @param {number[][]} gruplar
 */
export function brownForsytheAnova(gruplar) {
  anovaGruplariDenetle(gruplar, 'Brown-Forsythe testi', 2);
  const k = gruplar.length;
  const n = gruplar.map((g) => g.length);
  const N = n.reduce((s, x) => s + x, 0);
  const m = gruplar.map((g) => ortalama(g));
  const v = gruplar.map((g, i) => varyans(g, m[i]));
  const genel = ortalama(gruplar.flat());
  const pay = toplam(n.map((ni, i) => ni * (m[i] - genel) ** 2));
  const terimler = n.map((ni, i) => (1 - ni / N) * v[i]);
  const payda = toplam(terimler);
  if (!(payda > 0)) throw new IstatistikHatasi('Brown-Forsythe testi: tüm grupların varyansı sıfır.');
  const F = pay / payda;
  const c = terimler.map((t) => t / payda);
  const sd1 = k - 1;
  const sd2 = 1 / toplam(c.map((ci, i) => (ci * ci) / (n[i] - 1)));
  return {
    yontem: 'brown-forsythe-anova',
    ad: 'Brown-Forsythe testi',
    istatistik: F,
    istatistikAdi: 'F*',
    sd: [sd1, sd2],
    p: pf(F, sd1, sd2, false),
    n: N,
    uyarilar: [],
  };
}
