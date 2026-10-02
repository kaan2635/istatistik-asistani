// Parametrik olmayan testler: Mann-Whitney U, Wilcoxon işaretli sıralar, işaret testi, Kruskal-Wallis, Dunn,
// Friedman (ve ikili karşılaştırmaları), Kendall W, Jonckheere-Terpstra, Mood medyan testi.
//
// Varsayılanlar SPSS ile uyumludur: z istatistikleri bağ düzeltmeli ve süreklilik düzeltmesizdir.
// R karşılıkları her fonksiyonun açıklamasında yazılıdır (doğrulama test/nonparametrik.test.js içinde).

import {
  mannWhitneyFrekans,
  isaretliSiraFrekans,
  pbinom,
  pchisq,
  pnorm,
  pt,
  ptukey,
  psignrank,
  pwilcox,
  qnorm,
} from './dagilim.js';
import { bagToplami, medyan, siralar, yuzdelik } from './betimsel.js';
import { pDuzelt } from './duzeltme.js';
import { IstatistikHatasi, gerekli } from './hata.js';
import { fisherKesin, kiKare } from './kategorik.js';

const DBL_EPS = 2.220446049250313e-16;

// ---------- Ortak yardımcılar ----------

/** Alternatif hipotez adını normalleştirir. R adları (two.sided, greater, less) da kabul edilir. */
export function alternatifAl(a = 'iki-yonlu') {
  const harita = {
    'iki-yonlu': 'iki-yonlu',
    iki: 'iki-yonlu',
    'two.sided': 'iki-yonlu',
    buyuk: 'buyuk',
    greater: 'buyuk',
    kucuk: 'kucuk',
    less: 'kucuk',
  };
  const s = harita[a];
  if (!s) throw new IstatistikHatasi(`Bilinmeyen alternatif hipotez: ${a}. 'iki-yonlu', 'buyuk' ya da 'kucuk' olmalıdır.`);
  return s;
}

function sayiDizisiMi(x) {
  return (Array.isArray(x) || ArrayBuffer.isView(x)) && Array.prototype.every.call(x, (v) => typeof v === 'number' && Number.isFinite(v));
}

function sayiKontrol(x, ad) {
  if (!sayiDizisiMi(x)) throw new IstatistikHatasi(`${ad} yalnızca sonlu sayılardan oluşmalıdır (eksik değerler önceden ayıklanmalıdır).`);
}

/** Normal yaklaşımla p (R wilcox.test ile aynı: iki yönlü = 2·min(alt, üst)) */
function normalP(z, alt) {
  if (alt === 'kucuk') return pnorm(z);
  if (alt === 'buyuk') return pnorm(z, 0, 1, false);
  return Math.min(1, 2 * Math.min(pnorm(z), pnorm(z, 0, 1, false)));
}

function sureklilikPayi(dz, alt, sureklilik) {
  if (!sureklilik) return 0;
  if (alt === 'iki-yonlu') return Math.sign(dz) * 0.5;
  return alt === 'buyuk' ? 0.5 : -0.5;
}

/** Gruptaki aynı değerlerin bağ toplamı Σ(t³ - t) */
function grupIciBagToplami(x) {
  const say = new Map();
  for (const v of x) say.set(v, (say.get(v) || 0) + 1);
  let s = 0;
  for (const t of say.values()) if (t > 1) s += t * t * t - t;
  return s;
}

/**
 * R'nin qwilcox / qsignrank algoritması (10·DBL_EPSILON tolerans payıyla): P(X <= q) >= p olan en küçük q.
 * frek: kesin frekanslar, toplam: frekans toplamı, maks: istatistiğin en büyük değeri.
 */
function qKesikli(p, frek, toplam, maks) {
  let q = 0;
  let c = 0;
  if (p <= 0.5) {
    const x = p - 10 * DBL_EPS;
    for (;;) {
      c += frek[q] / toplam;
      if (c >= x) break;
      q++;
    }
  } else {
    const x = 1 - p + 10 * DBL_EPS;
    for (;;) {
      c += frek[q] / toplam;
      if (c > x) {
        q = maks - q;
        break;
      }
      q++;
    }
  }
  return q;
}

/** Sıralı dizinin ayrık değerleri ve her değerden büyük eleman sayısı */
function ayrikDegerler(D) {
  const deger = [];
  const buyukSay = [];
  const N = D.length;
  let i = 0;
  while (i < N) {
    let j = i;
    while (j + 1 < N && D[j + 1] === D[i]) j++;
    deger.push(D[i]);
    buyukSay.push(N - j - 1);
    i = j + 1;
  }
  return { deger, buyukSay };
}

/**
 * Adım fonksiyonunun kökü (R'nin uniroot ile yaklaşık bulduğu noktanın kesin değeri).
 * fUc(d): uç noktada doğrudan hesap; fAcik(U): ardışık iki ayrık değer arasındaki açık aralıkta değer (U = d'den büyük eleman sayısı).
 * fAcik, d arttıkça artmayan (U ile azalmayan) bir fonksiyondur.
 */
function adimKoku(ayrik, fUcAlt, fUcUst, fAcik) {
  const { deger, buyukSay } = ayrik;
  const K = deger.length;
  if (fUcAlt <= 0) return deger[0];
  if (fUcUst >= 0) return deger[K - 1];
  let lo = 0;
  let hi = K - 1;
  while (lo < hi) {
    const orta = (lo + hi) >> 1;
    if (fAcik(buyukSay[orta]) <= 0) hi = orta;
    else lo = orta + 1;
  }
  return deger[lo];
}

function grupOzeti(degerler, siraToplami) {
  const n = degerler.length;
  return {
    n,
    siraToplami,
    ortalamaSira: siraToplami / n,
    medyan: medyan(degerler),
    q1: yuzdelik(degerler, 0.25, 6),
    q3: yuzdelik(degerler, 0.75, 6),
  };
}

// ---------- Kesin dağılımlar (bağlı veriler için) ----------

/**
 * Puanların (2·sıra, tamsayı) m elemanlı alt küme toplamlarının frekansları (orta sıralarla koşullu permütasyon
 * dağılımı; R 4.6 .dwilcox(z = sıralar) ile aynı dağılım). Bütçe aşılırsa null.
 */
function siraToplamKesinDagilim(puan2, m, butce = 3e8, sayac = null) {
  const N = puan2.length;
  const sirali = Array.from(puan2).sort((a, b) => b - a);
  let smaks = 0;
  for (let i = 0; i < m; i++) smaks += sirali[i];
  const maliyet = m * (N - m + 1) * (smaks + 1);
  if (maliyet > butce) return null;
  if (sayac) {
    if (sayac.kalan < maliyet) return null;
    sayac.kalan -= maliyet;
  }
  const say = [];
  for (let j = 0; j <= m; j++) say.push(new Float64Array(smaks + 1));
  say[0][0] = 1;
  // j elemanlı alt kümelerin ulaşılabilir toplam aralığı (boş bölgeler taranmaz)
  const lo = new Int32Array(m + 1).fill(-1);
  const hi = new Int32Array(m + 1).fill(-1);
  lo[0] = 0;
  hi[0] = 0;
  for (let i = 0; i < N; i++) {
    const a = puan2[i];
    // i. elemandan sonra kalan N - i - 1 elemanla m'ye ulaşılamayacak j'ler gereksiz
    const jAlt = Math.max(1, m - (N - i - 1));
    for (let j = Math.min(i + 1, m); j >= jAlt; j--) {
      if (lo[j - 1] < 0) continue;
      const hedef = say[j];
      const kaynak = say[j - 1];
      const ust = Math.min(hi[j - 1], smaks - a);
      for (let s = ust; s >= lo[j - 1]; s--) {
        const v = kaynak[s];
        if (v !== 0) hedef[s + a] += v;
      }
      const yLo = lo[j - 1] + a;
      const yHi = ust + a;
      if (lo[j] < 0) {
        lo[j] = yLo;
        hi[j] = yHi;
      } else {
        if (yLo < lo[j]) lo[j] = yLo;
        if (yHi > hi[j]) hi[j] = yHi;
      }
    }
  }
  return say[m];
}

/** İşaretli sıralar istatistiğinin (orta sıralarla) kesin dağılımı: Σ z_i B_i, B_i ~ Bernoulli(1/2). Puanlar 2·sıra. */
function isaretliKesinDagilim(puan2, butce = 3e8, sayac = null) {
  let smaks = 0;
  for (const a of puan2) smaks += a;
  const maliyet = puan2.length * (smaks + 1);
  if (maliyet > butce) return null;
  if (sayac) {
    if (sayac.kalan < maliyet) return null;
    sayac.kalan -= maliyet;
  }
  const f = new Float64Array(smaks + 1);
  f[0] = 1;
  let ust = 0;
  for (const a of puan2) {
    for (let s = ust; s >= 0; s--) if (f[s] !== 0) f[s + a] += f[s];
    ust += a;
  }
  return f;
}

/** Frekanslardan birikimli olasılıklar: alt[s] = P(S <= s), ust[s] = P(S >= s) (s: 2·toplam ölçeği) */
function dagilimOzeti(f) {
  const L = f.length;
  let toplam = 0;
  for (let s = 0; s < L; s++) toplam += f[s];
  const alt = new Float64Array(L);
  const ust = new Float64Array(L);
  let c = 0;
  for (let s = 0; s < L; s++) {
    c += f[s];
    alt[s] = c / toplam;
  }
  c = 0;
  for (let s = L - 1; s >= 0; s--) {
    c += f[s];
    ust[s] = c / toplam;
  }
  return { f, toplam, alt, ust, altP: (s) => (s < 0 ? 0 : s >= L ? 1 : alt[s]), ustP: (s) => (s <= 0 ? 1 : s >= L ? 0 : ust[s]) };
}

/** İki örneklem: x grubunun (n1 eleman) 2·sıra toplamının kesin dağılımı */
function ikiOrneklemKesinDagilim(puan2, n1, butce, sayac = null) {
  const N = puan2.length;
  const kucukX = n1 <= N - n1;
  const frek = siraToplamKesinDagilim(puan2, kucukX ? n1 : N - n1, butce, sayac);
  if (!frek) return null;
  let toplam2 = 0;
  for (const a of puan2) toplam2 += a;
  const f = new Float64Array(toplam2 + 1);
  if (kucukX) f.set(frek);
  else for (let s = 0; s < frek.length; s++) if (frek[s] !== 0) f[toplam2 - s] += frek[s];
  return dagilimOzeti(f);
}

/** Sıralı puan çokluk kümesi anahtarı (önbellek için) */
function puanAnahtari(puan2) {
  return Int32Array.from(puan2).sort().join(',');
}

/**
 * R 4.6 wilcox.test kesin (bağlı) güven aralığı taraması: sıralı adaylar D (ikili farklar ya da Walsh ortalamaları)
 * üzerinde ardışık orta noktalarda kuyruk olasılığı hesaplanır; α'yı aşan ilk nokta sınırdır.
 * kuyruk(mu, altKuyruk) R'deki ptail ile aynıdır.
 */
function kesinTaramaGA(D, kuyruk, alpha, alt) {
  const toler = 10 * DBL_EPS;
  const nd = D.length;
  const altSinir = (a) => {
    a += toler;
    let pi = 0;
    let mu = -Infinity;
    if (kuyruk(D[0] - 1, false) > a) return [mu, pi];
    for (let k = 0; k < nd - 1; k++) {
      const p = kuyruk((D[k] + D[k + 1]) / 2, false);
      if (p > a) {
        mu = D[k];
        break;
      }
      pi = p;
    }
    if (!Number.isFinite(mu)) mu = D[nd - 1];
    return [mu, pi];
  };
  const ustSinir = (a) => {
    a += toler;
    let pi = 0;
    let mu = Infinity;
    if (kuyruk(D[nd - 1] + 1, true) > a) return [mu, pi];
    for (let k = nd - 2; k >= 0; k--) {
      const p = kuyruk((D[k] + D[k + 1]) / 2, true);
      if (p > a) {
        mu = D[k + 1];
        break;
      }
      pi = p;
    }
    if (!Number.isFinite(mu)) mu = D[0];
    return [mu, pi];
  };
  if (alt === 'iki-yonlu') {
    const l = altSinir(alpha / 2);
    const u = ustSinir(alpha / 2);
    return { alt: l[0], ust: u[0], ulasilan: l[1] + u[1] };
  }
  if (alt === 'buyuk') {
    const l = altSinir(alpha);
    return { alt: l[0], ust: Infinity, ulasilan: l[1] };
  }
  const u = ustSinir(alpha);
  return { alt: -Infinity, ust: u[0], ulasilan: u[1] };
}

/**
 * Bağsız kesin güven aralığı (R 4.6 .wilcox_test_*_cint_exact, z = NULL dalı).
 * D: sıralı adaylar, qf(p): kesin dağılımın ters fonksiyonu, pf(q): P(S <= q), M: aday sayısı.
 */
function kesinBagsizGA(D, qf, pf, M, alpha, alt) {
  const toler = 10 * DBL_EPS;
  const a = alt === 'iki-yonlu' ? alpha / 2 : alpha;
  let qu = qf(a);
  if (pf(qu) <= a + toler) qu += 1;
  if (qu === 0) return { alt: -Infinity, ust: Infinity, ulasilan: 0 };
  const ql = M - qu;
  const ulasilan = (alt === 'iki-yonlu' ? 2 : 1) * pf(Math.trunc(qu) - 1);
  if (alt === 'iki-yonlu') return { alt: D[qu - 1], ust: D[ql], ulasilan };
  if (alt === 'buyuk') return { alt: D[qu - 1], ust: Infinity, ulasilan };
  return { alt: -Infinity, ust: D[ql], ulasilan };
}

/**
 * Kesin iki yönlü p. 'iki-kat' (varsayılan, R 4.6 wilcox.test): min(2·P(S <= s), 2·P(S >= s), 1);
 * 'sapma' (R coin / StatXact): P(|S - E| >= |s - E|).
 */
function kesinP(dag, s2, beklenen2, alt, kural) {
  if (alt === 'buyuk') return dag.ustP(s2);
  if (alt === 'kucuk') return dag.altP(s2);
  if (kural === 'sapma') {
    const sapma = Math.abs(s2 - beklenen2);
    let pay = 0;
    for (let s = 0; s < dag.f.length; s++) if (dag.f[s] !== 0 && Math.abs(s - beklenen2) >= sapma) pay += dag.f[s];
    return Math.min(1, pay / dag.toplam);
  }
  return Math.min(2 * dag.altP(s2), 2 * dag.ustP(s2), 1);
}

// ---------- Mann-Whitney U ----------

/**
 * Mann-Whitney U testi (Wilcoxon sıra toplamı testi), iki bağımsız grup.
 *
 * Yöntem: x ve y birlikte sıralanır (bağlarda ortalama sıra). U1 = R1 - n1(n1+1)/2 (x için; R'nin W istatistiği),
 * U2 = n1·n2 - U1, U = min(U1, U2) (SPSS ve tezlerde raporlanan). Z bağ düzeltmeli varyansla hesaplanır; SPSS gibi
 * varsayılan olarak süreklilik düzeltmesi yoktur (sureklilik: true → R wilcox.test(correct = TRUE)).
 *
 * yontem: 'otomatik' → bağ yoksa ve n1, n2 < 50 ise kesin, aksi halde asimptotik (SPSS gibi bağlı veride normal
 *                      yaklaşım; R 4.6'nın exact = NULL varsayılanı bağlı veride de kesin hesaplar, ona 'kesin' karşılık gelir);
 *         'kesin' → bağ yoksa pwilcox; bağ varsa orta sıraların koşullu permütasyon dağılımı (R 4.6
 *                    wilcox.test(exact = TRUE)). İki yönlü p: kesinIkiYonlu = 'iki-kat' (R: 2·min kuyruk) ya da
 *                    'sapma' (R coin::wilcox_test(distribution = "exact"): P(|W - E| >= |w - E|));
 *         'asimptotik' → R wilcox.test(exact = FALSE, correct = sureklilik).
 *
 * Hodges-Lehmann konum farkı (x - y) ve güven aralığı R 4.6 wilcox.test(conf.int = TRUE) ile aynıdır: kesin bağsız
 * durumda ikili farkların sıra istatistikleri, kesin bağlı durumda R'nin orta nokta taraması; gerçekleşen güven
 * düzeyi ga.duzey'dedir. Asimptotik durumda R'nin uniroot(tol = 1e-4) ile yaklaşık bulduğu sıçrama noktasının kesin
 * değeri verilir (R'den en çok ~1e-4 farklı olabilir); tahmin ikili farkların medyanıdır.
 *
 * @param {number[]} x 1. grup
 * @param {number[]} y 2. grup
 * @param {{yontem?:'otomatik'|'kesin'|'asimptotik', sureklilik?:boolean, duzey?:number, alternatif?:string, mu?:number,
 *   hodgesLehmann?:boolean, kesinIkiYonlu?:'iki-kat'|'sapma', kesinGAButce?:number}} secenek
 *   kesinGAButce: bağlı veride kesin güven aralığı taramasının hesap bütçesi (dinamik programlama hücre işlemi);
 *   aşılırsa asimptotik güven aralığı verilir ve uyarı eklenir (p yine kesindir).
 */
export function mannWhitneyU(x, y, secenek = {}) {
  const { yontem = 'otomatik', sureklilik = false, duzey = 0.95, mu = 0, hodgesLehmann = true, kesinIkiYonlu = 'iki-kat', kesinGAButce = 3e9 } = secenek;
  const alt = alternatifAl(secenek.alternatif);
  sayiKontrol(x, '1. grup');
  sayiKontrol(y, '2. grup');
  const n1 = x.length;
  const n2 = y.length;
  gerekli(n1 >= 1 && n2 >= 1, 'Mann-Whitney U testi için her grupta en az 1 gözlem gerekir.');
  gerekli(['otomatik', 'kesin', 'asimptotik'].includes(yontem), `Bilinmeyen yöntem: ${yontem}`);
  gerekli(['iki-kat', 'sapma'].includes(kesinIkiYonlu), `Bilinmeyen kesin iki yönlü p kuralı: ${kesinIkiYonlu}`);
  gerekli(duzey > 0 && duzey < 1, 'Güven düzeyi 0 ile 1 arasında olmalıdır.');
  const N = n1 + n2;
  const uyarilar = [];

  const birlesik = new Array(N);
  for (let i = 0; i < n1; i++) birlesik[i] = x[i] - mu;
  for (let j = 0; j < n2; j++) birlesik[n1 + j] = y[j];
  const { sira, baglar } = siralar(birlesik);
  let R1 = 0;
  for (let i = 0; i < n1; i++) R1 += sira[i];
  let R2 = 0;
  for (let j = 0; j < n2; j++) R2 += sira[n1 + j];
  const U1 = R1 - (n1 * (n1 + 1)) / 2;
  const U2 = n1 * n2 - U1;
  const U = Math.min(U1, U2);
  const bagVar = baglar.length > 0;
  const T = bagToplami(baglar);
  const sigma = Math.sqrt(((n1 * n2) / 12) * (N + 1 - T / (N * (N - 1))));
  const dz = U1 - (n1 * n2) / 2;
  const tumuBagli = !(sigma > 0);

  // SPSS: Z = (U - n1n2/2)/σ (U = min → negatif), süreklilik düzeltmesi yok
  const z0 = tumuBagli ? NaN : dz / sigma;
  const zSPSS = tumuBagli ? NaN : -Math.abs(z0);
  const zTest = tumuBagli ? NaN : (dz - sureklilikPayi(dz, alt, sureklilik)) / sigma;
  const pAsimptotik = tumuBagli ? NaN : normalP(zTest, alt);

  let kesin = false;
  if (yontem === 'kesin') kesin = true;
  else if (yontem === 'otomatik') kesin = !bagVar && n1 < 50 && n2 < 50;

  let p = pAsimptotik;
  let pYontemi = 'asimptotik';
  let pKesin = null;
  let kesinBagli = false;
  if (kesin && !bagVar) {
    if (alt === 'iki-yonlu') {
      const q = U1 > (n1 * n2) / 2 ? pwilcox(U1 - 1, n1, n2, false) : pwilcox(U1, n1, n2);
      pKesin = Math.min(2 * q, 1);
    } else if (alt === 'buyuk') pKesin = pwilcox(U1 - 1, n1, n2, false);
    else pKesin = pwilcox(U1, n1, n2);
  } else if (kesin && bagVar) {
    const dag = ikiOrneklemKesinDagilim(Array.from(sira, (r) => Math.round(2 * r)), n1);
    if (dag) {
      pKesin = kesinP(dag, Math.round(2 * R1), n1 * (N + 1), alt, kesinIkiYonlu);
      kesinBagli = true;
      uyarilar.push('Bağlı değerler var; kesin p orta sıraların koşullu permütasyon dağılımından hesaplandı.');
    } else {
      uyarilar.push('Bağlı veride kesin dağılım bu örneklem büyüklüğü için hesaplanamayacak kadar büyük; asimptotik p kullanıldı.');
    }
  }
  if (pKesin !== null) {
    p = pKesin;
    pYontemi = 'kesin';
  } else if (tumuBagli) {
    throw new IstatistikHatasi('Tüm gözlemler aynı değere sahip; Mann-Whitney U testi hesaplanamaz.');
  }

  const sonuc = {
    yontem: 'mann-whitney-u',
    ad: 'Mann-Whitney U testi',
    istatistik: U,
    istatistikAdi: 'U',
    sd: null,
    p,
    pYontemi,
    alternatif: alt,
    U,
    U1,
    U2,
    W: U1, // R wilcox.test istatistiği (x grubu için U)
    // SPSS "Wilcoxon W": ortalama sırası küçük olan grubun (U'yu veren grubun) sıra toplamı; eşitse 2. grubun
    wilcoxonW: U1 < U2 ? R1 : R2,
    z: zSPSS,
    zYonlu: zTest,
    pAsimptotik,
    pKesin,
    sureklilik,
    gruplar: [grupOzeti(x, R1), grupOzeti(y, R2)],
    n: N,
    baglar: { grupSayisi: baglar.length, toplam: T },
    etki: { ad: 'r', sembol: 'r', deger: tumuBagli ? NaN : Math.abs(z0) / Math.sqrt(N) },
    etkiler: {
      r: tumuBagli ? NaN : Math.abs(z0) / Math.sqrt(N),
      rankBiserial: (U1 - U2) / (n1 * n2),
      ortakDilEtkisi: U1 / (n1 * n2), // P(X > Y) + 0.5·P(X = Y)
    },
    uyarilar,
  };

  if (hodgesLehmann) {
    if (n1 * n2 > 4e6) uyarilar.push('Hodges-Lehmann tahmini çok büyük örneklem nedeniyle hesaplanmadı.');
    else {
      const tur = pKesin === null ? 'asimptotik' : kesinBagli ? 'kesin-bagli' : 'kesin';
      Object.assign(sonuc, mwHodgesLehmann(x, y, { tur, duzey, alt, sureklilik, uyarilar, kesinGAButce }));
    }
  }
  return sonuc;
}

function mwHodgesLehmann(x, y, { tur, duzey, alt, sureklilik, uyarilar, kesinGAButce }) {
  const n1 = x.length;
  const n2 = y.length;
  const m = n1 * n2;
  const D = new Float64Array(m);
  let k = 0;
  for (let i = 0; i < n1; i++) for (let j = 0; j < n2; j++) D[k++] = x[i] - y[j];
  D.sort();
  const tahmin = m % 2 ? D[(m - 1) / 2] : (D[m / 2 - 1] + D[m / 2]) / 2;
  const alpha = 1 - duzey;
  let gaAlt = -Infinity;
  let gaUst = Infinity;
  let gercekDuzey = duzey;
  const N = n1 + n2;
  if (tur === 'kesin-bagli') {
    // R 4.6 taraması: her orta noktada sıralar yeniden hesaplanır, koşullu dağılım çokluk kümesine göre önbelleklenir
    const onbellek = new Map();
    const sayac = { kalan: kesinGAButce };
    const b = new Array(N);
    const kuyruk = (mu, altKuyruk) => {
      for (let i = 0; i < n1; i++) b[i] = x[i] - mu;
      for (let j = 0; j < n2; j++) b[n1 + j] = y[j];
      const { sira } = siralar(b);
      const puan2 = Array.from(sira, (r) => Math.round(2 * r));
      let s2 = 0;
      for (let i = 0; i < n1; i++) s2 += puan2[i];
      const ky = puanAnahtari(puan2);
      let dag = onbellek.get(ky);
      if (!dag) {
        dag = ikiOrneklemKesinDagilim(puan2, n1, 3e8, sayac);
        if (!dag) throw new IstatistikHatasi('Bağlı veride kesin güven aralığı taraması hesap bütçesini aştı.');
        onbellek.set(ky, dag);
      }
      // R: alt = P(W <= w), üst = 1 - P(W <= w - 1/2) (yarım ızgarada P(W >= w))
      return altKuyruk ? dag.altP(s2) : 1 - dag.altP(s2 - 1);
    };
    try {
      const g = kesinTaramaGA(D, kuyruk, alpha, alt);
      gaAlt = g.alt;
      gaUst = g.ust;
      gercekDuzey = 1 - g.ulasilan;
    } catch (e) {
      if (!(e instanceof IstatistikHatasi)) throw e;
      uyarilar.push(`${e.message} Asimptotik güven aralığı verildi.`);
      tur = 'asimptotik';
    }
  } else if (tur === 'kesin') {
    const f = mannWhitneyFrekans(n1, n2);
    let t = 0;
    for (let i = 0; i < f.length; i++) t += f[i];
    const g = kesinBagsizGA(D, (p) => qKesikli(p, f, t, m), (q) => pwilcox(q, n1, n2), m, alpha, alt);
    gaAlt = g.alt;
    gaUst = g.ust;
    gercekDuzey = 1 - g.ulasilan;
  }
  if (tur === 'asimptotik') {
    const mumin = D[0];
    const mumax = D[m - 1];
    const wdiff = (d, zq) => {
      const b = new Array(N);
      for (let i = 0; i < n1; i++) b[i] = x[i] - d;
      for (let j = 0; j < n2; j++) b[n1 + j] = y[j];
      const { sira, baglar } = siralar(b);
      let s = 0;
      for (let i = 0; i < n1; i++) s += sira[i];
      const dz = s - (n1 * (n1 + 1)) / 2 - m / 2;
      const sg = Math.sqrt((m / 12) * (N + 1 - bagToplami(baglar) / (N * (N - 1))));
      if (!(sg > 0)) throw new IstatistikHatasi('Tüm gözlemler bağlı olduğundan güven aralığı hesaplanamaz.');
      return (dz - sureklilikPayi(dz, alt, sureklilik)) / sg - zq;
    };
    const sigma0 = Math.sqrt((m / 12) * (N + 1 - (grupIciBagToplami(x) + grupIciBagToplami(y)) / (N * (N - 1))));
    const ayrik = ayrikDegerler(D);
    const kok = (zq) => {
      const fAlt = wdiff(mumin, zq);
      const fUst = fAlt <= 0 ? 0 : wdiff(mumax, zq);
      return adimKoku(ayrik, fAlt, fUst, (U) => {
        const dz = U - m / 2;
        return (dz - sureklilikPayi(dz, alt, sureklilik)) / sigma0 - zq;
      });
    };
    try {
      if (alt === 'iki-yonlu') {
        gaAlt = kok(qnorm(alpha / 2, 0, 1, false));
        gaUst = kok(qnorm(alpha / 2));
      } else if (alt === 'buyuk') gaAlt = kok(qnorm(alpha, 0, 1, false));
      else gaUst = kok(qnorm(alpha));
    } catch (e) {
      if (!(e instanceof IstatistikHatasi)) throw e;
      uyarilar.push(e.message);
      gaAlt = NaN;
      gaUst = NaN;
    }
  }
  const ga = { duzey: gercekDuzey, alt: gaAlt, ust: gaUst, neyin: 'konum farkı (Hodges-Lehmann, 1. grup - 2. grup)' };
  return { hodgesLehmann: { tahmin, ga }, ga };
}

// ---------- Wilcoxon işaretli sıralar ----------

/**
 * Wilcoxon işaretli sıralar testi (eşleştirilmiş iki örneklem ya da tek örneklem için konum = mu).
 *
 * Farklar d = (x - y) - mu (y null ise d = x - mu). Sıfır farklar atılır (Wilcoxon yöntemi; SPSS ve R asimptotik
 * testi ile aynı), sayısı `sifirFark` alanında raporlanır. V = W+ (pozitif farkların sıra toplamı, R'nin istatistiği),
 * W- negatiflerin. Z bağ düzeltmeli varyansla, varsayılan olarak süreklilik düzeltmesiz (SPSS); SPSS Z'si
 * min(W+, W-) üzerinden (negatif).
 *
 * yontem: 'otomatik' → bağ ve sıfır yoksa ve n < 50 ise kesin, aksi halde asimptotik (SPSS gibi);
 *         'kesin' → bağ/sıfır yoksa psignrank; varsa orta sıraların koşullu permütasyon dağılımı. sifirYontemi:
 *                   'wilcoxon' (varsayılan; sıfırlar sıralamadan önce atılır, R coin zero.method = "Wilcoxon") ya da
 *                   'pratt' (sıfırlar sıralamaya katılıp sonra atılır; R 4.6 wilcox.test(exact = TRUE) ve coin "Pratt";
 *                   bu durumda V/W+/W- Pratt sıralarıyla raporlanır). İki yönlü p: gözlenen taraftaki kuyruğun iki katı.
 *         'asimptotik' → R wilcox.test(paired = TRUE, exact = FALSE, correct = sureklilik).
 * Hodges-Lehmann sözde medyanı ve güven aralığı R 4.6 wilcox.test(conf.int = TRUE) ile aynı yolla tüm farklar
 * (sıfırlar dahil) üzerinden hesaplanır; asimptotik durumda R'nin uniroot ile yaklaşık bulduğu noktanın kesin değeri verilir.
 *
 * @param {number[]} x 1. ölçüm (ya da tek örneklem)
 * @param {number[]|null} y 2. ölçüm (tek örneklem için null)
 * @param {{mu?:number, yontem?:string, sureklilik?:boolean, duzey?:number, alternatif?:string, hodgesLehmann?:boolean,
 *   sifirYontemi?:'wilcoxon'|'pratt', kesinGAButce?:number}} secenek
 *   kesinGAButce: bağlı/sıfırlı veride kesin güven aralığı taramasının hesap bütçesi; aşılırsa asimptotik aralık verilir.
 */
export function wilcoxonIsaretliSira(x, y = null, secenek = {}) {
  const { mu = 0, yontem = 'otomatik', sureklilik = false, duzey = 0.95, hodgesLehmann = true, sifirYontemi = 'wilcoxon', kesinGAButce = 3e9 } = secenek;
  const alt = alternatifAl(secenek.alternatif);
  sayiKontrol(x, '1. ölçüm');
  gerekli(['otomatik', 'kesin', 'asimptotik'].includes(yontem), `Bilinmeyen yöntem: ${yontem}`);
  gerekli(['wilcoxon', 'pratt'].includes(sifirYontemi), `Bilinmeyen sıfır fark yöntemi: ${sifirYontemi}`);
  gerekli(duzey > 0 && duzey < 1, 'Güven düzeyi 0 ile 1 arasında olmalıdır.');
  const tekOrneklem = y === null || y === undefined;
  let ham;
  if (tekOrneklem) ham = Array.from(x);
  else {
    sayiKontrol(y, '2. ölçüm');
    gerekli(x.length === y.length, 'Eşleştirilmiş ölçümlerin uzunlukları aynı olmalıdır.');
    ham = Array.from(x, (v, i) => v - y[i]);
  }
  const d = ham.map((v) => v - mu);
  const nToplam = d.length;
  gerekli(nToplam >= 1, 'Wilcoxon işaretli sıralar testi için en az 1 gözlem gerekir.');
  const uyarilar = [];
  const sifirlar = d.filter((v) => v === 0).length;
  const dn = d.filter((v) => v !== 0);
  const n = dn.length;
  if (n === 0) throw new IstatistikHatasi('Tüm farklar sıfır; Wilcoxon işaretli sıralar testi hesaplanamaz.');
  const { sira, baglar } = siralar(dn.map(Math.abs));
  let Vp = 0;
  let Vn = 0;
  let nPoz = 0;
  for (let i = 0; i < n; i++) {
    if (dn[i] > 0) {
      Vp += sira[i];
      nPoz++;
    } else Vn += sira[i];
  }
  const nNeg = n - nPoz;
  const bagVar = baglar.length > 0;
  const T = bagToplami(baglar);
  const beklenen = (n * (n + 1)) / 4;
  const sigma = Math.sqrt((n * (n + 1) * (2 * n + 1)) / 24 - T / 48);
  const dz = Vp - beklenen;
  const z0 = dz / sigma;
  const zTest = (dz - sureklilikPayi(dz, alt, sureklilik)) / sigma;
  const pAsimptotik = normalP(zTest, alt);

  let kesin = false;
  if (yontem === 'kesin') kesin = true;
  else if (yontem === 'otomatik') kesin = !bagVar && sifirlar === 0 && n < 50;
  let pKesin = null;
  let kesinBagli = false;
  let V = Vp;
  let Wn = Vn;
  if (kesin && !bagVar && sifirlar === 0) {
    if (alt === 'iki-yonlu') {
      const q = Vp > beklenen ? psignrank(Vp - 1, n, false) : psignrank(Vp, n);
      pKesin = Math.min(2 * q, 1);
    } else if (alt === 'buyuk') pKesin = psignrank(Vp - 1, n, false);
    else pKesin = psignrank(Vp, n);
  } else if (kesin) {
    // Kesin koşullu dağılım: Wilcoxon (sıfırlar atılıp sıralanır) ya da Pratt (sıfırlarla sıralanıp atılır)
    let z = Array.from(sira);
    if (sifirYontemi === 'pratt' && sifirlar > 0) {
      const tum = siralar(d.map(Math.abs)).sira;
      z = [];
      V = 0;
      Wn = 0;
      for (let i = 0; i < nToplam; i++) {
        if (d[i] === 0) continue;
        z.push(tum[i]);
        if (d[i] > 0) V += tum[i];
        else Wn += tum[i];
      }
    }
    const puan2 = z.map((r) => Math.round(2 * r));
    const frek = isaretliKesinDagilim(puan2);
    if (frek) {
      const dag = dagilimOzeti(frek);
      let toplam2 = 0;
      for (const a of puan2) toplam2 += a;
      const q2 = Math.round(2 * V);
      if (alt === 'buyuk') pKesin = dag.ustP(q2);
      else if (alt === 'kucuk') pKesin = dag.altP(q2);
      else pKesin = Math.min(1, 2 * (q2 > toplam2 / 2 ? dag.ustP(q2) : dag.altP(q2)));
      kesinBagli = true;
      uyarilar.push(
        sifirYontemi === 'pratt' && sifirlar > 0
          ? 'Kesin p, Pratt sıralarının (sıfır farklar sıralamaya katılıp atılarak) koşullu dağılımından hesaplandı.'
          : 'Bağlı ya da sıfır farklar var; kesin p orta sıraların koşullu dağılımından hesaplandı (sıfır farklar atıldı).',
      );
    } else uyarilar.push('Kesin dağılım bu örneklem için hesaplanamayacak kadar büyük; asimptotik p kullanıldı.');
  }
  if (!(sigma > 0) && pKesin === null) throw new IstatistikHatasi('Farkların tümü bağlı; test hesaplanamaz.');
  const p = pKesin !== null ? pKesin : pAsimptotik;
  if (sifirlar > 0) uyarilar.push(`${sifirlar} gözlemde fark sıfır olduğundan analiz dışında bırakıldı.`);

  const sonuc = {
    yontem: tekOrneklem ? 'wilcoxon-tek-orneklem' : 'wilcoxon-isaretli-sira',
    ad: tekOrneklem ? 'Tek örneklem Wilcoxon işaretli sıralar testi' : 'Wilcoxon işaretli sıralar testi',
    istatistik: V,
    istatistikAdi: 'W+',
    sd: null,
    p,
    pYontemi: pKesin !== null ? 'kesin' : 'asimptotik',
    alternatif: alt,
    V,
    Warti: V, // W+
    Weksi: Wn, // W-
    z: -Math.abs(z0), // SPSS: min(W+, W-) üzerinden
    zYonlu: zTest,
    pAsimptotik,
    pKesin,
    sureklilik,
    pozitifSira: { n: nPoz, siraToplami: Vp, ortalamaSira: nPoz ? Vp / nPoz : NaN },
    negatifSira: { n: nNeg, siraToplami: Vn, ortalamaSira: nNeg ? Vn / nNeg : NaN },
    sifirFark: sifirlar,
    n: nToplam,
    nSifirsiz: n,
    baglar: { grupSayisi: baglar.length, toplam: T },
    etki: { ad: 'r', sembol: 'r', deger: Math.abs(z0) / Math.sqrt(nToplam) },
    etkiler: {
      r: Math.abs(z0) / Math.sqrt(nToplam), // N = çift sayısı (sıfır farklar dahil)
      rankBiserial: (Vp - Vn) / (Vp + Vn), // eşleştirilmiş sıra-iki serili korelasyon (Kerby)
    },
    uyarilar,
  };
  if (hodgesLehmann) {
    if (nToplam > 4000) uyarilar.push('Hodges-Lehmann tahmini çok büyük örneklem nedeniyle hesaplanmadı.');
    else {
      const tur = pKesin === null ? 'asimptotik' : kesinBagli ? 'kesin-bagli' : 'kesin';
      Object.assign(sonuc, isaretliHodgesLehmann(ham, { tur, duzey, alt, sureklilik, uyarilar, kesinGAButce }));
    }
  }
  return sonuc;
}

function isaretliHodgesLehmann(x, { tur, duzey, alt, sureklilik, uyarilar, kesinGAButce }) {
  const n = x.length;
  const M = (n * (n + 1)) / 2;
  const W = new Float64Array(M);
  let k = 0;
  for (let i = 0; i < n; i++) for (let j = i; j < n; j++) W[k++] = x[i] + x[j];
  W.sort();
  for (let i = 0; i < M; i++) W[i] /= 2;
  const tahmin = M % 2 ? W[(M - 1) / 2] : (W[M / 2 - 1] + W[M / 2]) / 2;
  let alpha = 1 - duzey;
  let gaAlt = -Infinity;
  let gaUst = Infinity;
  let gercekDuzey = duzey;
  if (tur === 'kesin-bagli') {
    const onbellek = new Map();
    const sayac = { kalan: kesinGAButce };
    const kuyruk = (mu, altKuyruk) => {
      const xd = x.map((v) => v - mu);
      const { sira } = siralar(xd.map(Math.abs));
      const puan2 = Array.from(sira, (r) => Math.round(2 * r));
      let v2 = 0;
      for (let i = 0; i < n; i++) if (xd[i] > 0) v2 += puan2[i];
      const ky = puanAnahtari(puan2);
      let dag = onbellek.get(ky);
      if (!dag) {
        const f = isaretliKesinDagilim(puan2, 3e8, sayac);
        if (!f) throw new IstatistikHatasi('Bağlı veride kesin güven aralığı taraması hesap bütçesini aştı.');
        dag = dagilimOzeti(f);
        onbellek.set(ky, dag);
      }
      // R: alt = P(V <= v), üst = 1 - P(V <= v)
      return altKuyruk ? dag.altP(v2) : 1 - dag.altP(v2);
    };
    try {
      const g = kesinTaramaGA(W, kuyruk, alpha, alt);
      gaAlt = g.alt;
      gaUst = g.ust;
      gercekDuzey = 1 - g.ulasilan;
    } catch (e) {
      if (!(e instanceof IstatistikHatasi)) throw e;
      uyarilar.push(`${e.message} Asimptotik güven aralığı verildi.`);
      tur = 'asimptotik';
    }
  } else if (tur === 'kesin') {
    const f = isaretliSiraFrekans(n);
    const g = kesinBagsizGA(W, (p) => qKesikli(p, f, 2 ** n, M), (q) => psignrank(q, n), M, alpha, alt);
    gaAlt = g.alt;
    gaUst = g.ust;
    gercekDuzey = 1 - g.ulasilan;
  }
  if (tur === 'asimptotik') {
    const mumin = W[0];
    const mumax = W[M - 1];
    const Wfn = (d) => {
      const xd = [];
      for (const v of x) {
        const f = v - d;
        if (f !== 0) xd.push(f);
      }
      const nx = xd.length;
      if (nx === 0) return 0;
      const { sira, baglar } = siralar(xd.map(Math.abs));
      let zd = -(nx * (nx + 1)) / 4;
      for (let i = 0; i < nx; i++) if (xd[i] > 0) zd += sira[i];
      const sg = Math.sqrt((nx * (nx + 1) * (2 * nx + 1)) / 24 - bagToplami(baglar) / 48);
      if (!(sg > 0)) throw new IstatistikHatasi('Farkların tümü sıfır ya da bağlı olduğundan güven aralığı hesaplanamaz.');
      return (zd - sureklilikPayi(zd, alt, sureklilik)) / sg;
    };
    const sigma0 = Math.sqrt((n * (n + 1) * (2 * n + 1)) / 24 - grupIciBagToplami(x) / 48);
    const ayrik = ayrikDegerler(W);
    try {
      const Wmin = Wfn(mumin);
      const Wmax = Wfn(mumax);
      const kok = (zq) =>
        adimKoku(ayrik, Wmin - zq, Wmax - zq, (U) => {
          const zd = U - (n * (n + 1)) / 4;
          return (zd - sureklilikPayi(zd, alt, sureklilik)) / sigma0 - zq;
        });
      // R: istenen düzeye ulaşılamıyorsa alfa iki katına çıkarılır
      if (alt === 'iki-yonlu') {
        for (;;) {
          const minFark = Wmin - qnorm(alpha / 2, 0, 1, false);
          const maksFark = Wmax - qnorm(alpha / 2);
          if (minFark < 0 || maksFark > 0) alpha *= 2;
          else break;
        }
      } else if (alt === 'buyuk') {
        while (Wmin - qnorm(alpha, 0, 1, false) < 0) alpha *= 2;
      } else {
        while (Wmax - qnorm(alpha / 2) > 0) alpha *= 2;
      }
      if (alpha >= 1 || 1 - duzey < alpha * 0.75) {
        gercekDuzey = 1 - Math.min(1, alpha);
        uyarilar.push('İstenen güven düzeyine ulaşılamıyor; gerçekleşen düzey raporlandı.');
      }
      if (alpha < 1) {
        if (alt === 'iki-yonlu') {
          gaAlt = kok(qnorm(alpha / 2, 0, 1, false));
          gaUst = kok(qnorm(alpha / 2));
        } else if (alt === 'buyuk') gaAlt = kok(qnorm(alpha, 0, 1, false));
        else gaUst = kok(qnorm(alpha));
      } else {
        const md = medyan(x);
        if (alt !== 'kucuk') gaAlt = md;
        if (alt !== 'buyuk') gaUst = md;
      }
    } catch (e) {
      if (!(e instanceof IstatistikHatasi)) throw e;
      uyarilar.push(e.message);
      gaAlt = NaN;
      gaUst = NaN;
    }
  }
  const ga = { duzey: gercekDuzey, alt: gaAlt, ust: gaUst, neyin: 'sözde medyan (Hodges-Lehmann)' };
  return { hodgesLehmann: { tahmin, ga }, ga };
}

// ---------- İşaret testi ----------

/**
 * İşaret testi (eşleştirilmiş iki ölçüm; y null ise x'in mu'ya göre işaretleri).
 * Eşit çiftler atılır. Kesin p: Binom(n, 1/2) (R binom.test(n+, n, 0.5) ile aynı).
 * SPSS: n <= 25 ise kesin binom p raporlanır; n > 25 ise Z = (|n+ - n-| - 1)/√n (süreklilik düzeltmeli, negatif işaretli)
 * ve asimptotik p. 'otomatik' bu kuralı uygular; pKesin her durumda döner.
 *
 * @param {number[]} x
 * @param {number[]|null} y
 * @param {{mu?:number, yontem?:'otomatik'|'kesin'|'asimptotik', alternatif?:string}} secenek
 */
export function isaretTesti(x, y = null, secenek = {}) {
  const { mu = 0, yontem = 'otomatik' } = secenek;
  const alt = alternatifAl(secenek.alternatif);
  sayiKontrol(x, '1. ölçüm');
  let d;
  if (y === null || y === undefined) d = Array.from(x, (v) => v - mu);
  else {
    sayiKontrol(y, '2. ölçüm');
    gerekli(x.length === y.length, 'Eşleştirilmiş ölçümlerin uzunlukları aynı olmalıdır.');
    d = Array.from(x, (v, i) => v - y[i] - mu);
  }
  let nPoz = 0;
  let nNeg = 0;
  for (const v of d) {
    if (v > 0) nPoz++;
    else if (v < 0) nNeg++;
  }
  const n = nPoz + nNeg;
  const esit = d.length - n;
  if (n === 0) throw new IstatistikHatasi('Tüm farklar sıfır; işaret testi hesaplanamaz.');
  let pKesin;
  if (alt === 'iki-yonlu') pKesin = Math.min(1, 2 * pbinom(Math.min(nPoz, nNeg), n, 0.5));
  else if (alt === 'buyuk') pKesin = pbinom(nPoz - 1, n, 0.5, false);
  else pKesin = pbinom(nPoz, n, 0.5);
  const fark = Math.abs(nPoz - nNeg);
  const z = -Math.max(0, fark - 1) / Math.sqrt(n);
  let pAsimptotik;
  const zYonlu = (Math.sign(nPoz - nNeg) * Math.max(0, fark - 1)) / Math.sqrt(n);
  if (alt === 'iki-yonlu') pAsimptotik = Math.min(1, 2 * pnorm(-Math.abs(z)));
  else if (alt === 'buyuk') pAsimptotik = pnorm((nPoz - nNeg - 1) / Math.sqrt(n), 0, 1, false);
  else pAsimptotik = pnorm((nPoz - nNeg + 1) / Math.sqrt(n));
  const kesin = yontem === 'kesin' || (yontem === 'otomatik' && n <= 25);
  return {
    yontem: 'isaret-testi',
    ad: 'İşaret testi',
    istatistik: kesin ? Math.min(nPoz, nNeg) : z,
    istatistikAdi: kesin ? 'S' : 'z',
    sd: null,
    p: kesin ? pKesin : pAsimptotik,
    pYontemi: kesin ? 'kesin' : 'asimptotik',
    alternatif: alt,
    pozitif: nPoz,
    negatif: nNeg,
    esit,
    n: d.length,
    nSifirsiz: n,
    z,
    zYonlu,
    pKesin,
    pAsimptotik,
    uyarilar: esit > 0 ? [`${esit} çiftte fark sıfır olduğundan analiz dışında bırakıldı.`] : [],
  };
}

// ---------- Kruskal-Wallis ----------

function gruplariHazirla(gruplar, enAz = 2, testAdi = 'Test') {
  gerekli(Array.isArray(gruplar), `${testAdi} için gruplar bir dizi olmalıdır.`);
  const uyarilar = [];
  const dolu = [];
  const indisler = [];
  gruplar.forEach((g, i) => {
    sayiKontrol(g, `${i + 1}. grup`);
    if (g.length > 0) {
      dolu.push(g);
      indisler.push(i);
    } else uyarilar.push(`${i + 1}. grupta gözlem olmadığından analiz dışında bırakıldı.`);
  });
  gerekli(dolu.length >= enAz, `${testAdi} için gözlemi olan en az ${enAz} grup gerekir.`);
  return { dolu, indisler, uyarilar };
}

function birlesikSira(gruplar) {
  const tum = [];
  for (const g of gruplar) for (const v of g) tum.push(v);
  const { sira, baglar } = siralar(tum);
  const siraToplamlari = [];
  let bas = 0;
  for (const g of gruplar) {
    let s = 0;
    for (let i = 0; i < g.length; i++) s += sira[bas + i];
    siraToplamlari.push(s);
    bas += g.length;
  }
  return { sira, baglar, siraToplamlari, N: tum.length };
}

/**
 * Kruskal-Wallis H testi (bağ düzeltmeli; R kruskal.test ve SPSS ile aynı).
 * Etki büyüklükleri: ε² = H/(N - 1) (Tomczak & Tomczak 2014), η²_H = (H - k + 1)/(N - k).
 * @param {number[][]} gruplar
 */
export function kruskalWallis(gruplar) {
  const { dolu, indisler, uyarilar } = gruplariHazirla(gruplar, 2, 'Kruskal-Wallis testi');
  const k = dolu.length;
  const { baglar, siraToplamlari, N } = birlesikSira(dolu);
  let s = 0;
  dolu.forEach((g, i) => {
    s += (siraToplamlari[i] * siraToplamlari[i]) / g.length;
  });
  const Hbag = (12 / (N * (N + 1))) * s - 3 * (N + 1);
  const T = bagToplami(baglar);
  const duzeltme = 1 - T / (N * N * N - N);
  if (!(duzeltme > 0)) throw new IstatistikHatasi('Tüm gözlemler aynı değere sahip; Kruskal-Wallis testi hesaplanamaz.');
  const H = Hbag / duzeltme;
  const sd = k - 1;
  const p = pchisq(H, sd, false);
  const eps2 = H / (N - 1);
  const eta2 = N > k ? (H - k + 1) / (N - k) : NaN;
  return {
    yontem: 'kruskal-wallis',
    ad: 'Kruskal-Wallis H testi',
    istatistik: H,
    istatistikAdi: 'H',
    sd,
    p,
    Hduzeltmesiz: Hbag,
    bagDuzeltmesi: duzeltme,
    gruplar: dolu.map((g, i) => ({ grup: indisler[i], ...grupOzeti(g, siraToplamlari[i]) })),
    n: N,
    k,
    etki: { ad: 'epsilon kare', sembol: 'ε²', deger: eps2 },
    etkiler: { epsilonKare: eps2, etaKareH: eta2 },
    uyarilar,
  };
}

/**
 * Dunn (1964) ikili karşılaştırmaları (Kruskal-Wallis sonrası).
 * z_ij = (R̄_i - R̄_j) / √[(N(N+1)/12 - Σ(t³-t)/(12(N-1))) (1/n_i + 1/n_j)], iki yönlü p = 2(1 - Φ(|z|)).
 * SPSS ikili karşılaştırmaları Bonferroni kullanır (varsayılan). R: rstatix::dunn_test / dunn.test ile aynı.
 * @param {number[][]} gruplar
 * @param {{duzeltme?:string}} secenek pDuzelt yöntemlerinden biri
 */
export function dunn(gruplar, secenek = {}) {
  const { duzeltme = 'bonferroni' } = secenek;
  const { dolu, indisler, uyarilar } = gruplariHazirla(gruplar, 2, 'Dunn testi');
  const { baglar, siraToplamlari, N } = birlesikSira(dolu);
  const T = bagToplami(baglar);
  const varyansTemel = (N * (N + 1)) / 12 - T / (12 * (N - 1));
  if (!(varyansTemel > 0)) throw new IstatistikHatasi('Tüm gözlemler aynı değere sahip; Dunn testi hesaplanamaz.');
  const ort = dolu.map((g, i) => siraToplamlari[i] / g.length);
  const karsilastirmalar = [];
  for (let i = 0; i < dolu.length; i++) {
    for (let j = i + 1; j < dolu.length; j++) {
      const fark = ort[i] - ort[j];
      const sh = Math.sqrt(varyansTemel * (1 / dolu[i].length + 1 / dolu[j].length));
      const z = fark / sh;
      const p = Math.min(1, 2 * pnorm(-Math.abs(z)));
      karsilastirmalar.push({ grup1: indisler[i], grup2: indisler[j], siraFarki: fark, sh, z, p });
    }
  }
  const duz = pDuzelt(karsilastirmalar.map((c) => c.p), duzeltme);
  karsilastirmalar.forEach((c, i) => {
    c.pDuzeltilmis = duz[i];
  });
  return {
    yontem: 'dunn',
    ad: 'Dunn ikili karşılaştırmaları',
    duzeltme,
    ortalamaSiralar: ort,
    karsilastirmalar,
    n: N,
    uyarilar,
  };
}

// ---------- Friedman ----------

function matrisKontrol(matris, enAzSutun, testAdi) {
  gerekli(Array.isArray(matris) && matris.length > 0, `${testAdi} için veri matrisi boş olamaz.`);
  const k = matris[0].length;
  gerekli(k >= enAzSutun, `${testAdi} için en az ${enAzSutun} ölçüm (sütun) gerekir.`);
  matris.forEach((s, i) => {
    gerekli(s.length === k, `${i + 1}. satırdaki ölçüm sayısı diğerlerinden farklı.`);
    sayiKontrol(s, `${i + 1}. satır`);
  });
  return k;
}

function satirSiralari(matris) {
  const n = matris.length;
  const k = matris[0].length;
  const siraToplamlari = new Array(k).fill(0);
  let kareToplami = 0;
  let T = 0;
  const siraMatrisi = [];
  for (const satir of matris) {
    const { sira, baglar } = siralar(satir);
    for (let j = 0; j < k; j++) {
      siraToplamlari[j] += sira[j];
      kareToplami += sira[j] * sira[j];
    }
    T += bagToplami(baglar);
    siraMatrisi.push(Array.from(sira));
  }
  return { n, k, siraToplamlari, kareToplami, T, siraMatrisi };
}

/**
 * Friedman testi (bağ düzeltmeli; R friedman.test ve SPSS ile aynı).
 * χ² = 12 Σ (R_j - n(k+1)/2)² / [n k (k+1) - Σ(t³ - t)/(k - 1)], sd = k - 1.
 * Kendall W = χ²/(n(k - 1)).
 * @param {number[][]} matris n denek × k koşul
 */
export function friedman(matris) {
  const k = matrisKontrol(matris, 2, 'Friedman testi');
  const { n, siraToplamlari, T } = satirSiralari(matris);
  let pay = 0;
  for (const R of siraToplamlari) pay += (R - (n * (k + 1)) / 2) ** 2;
  const payda = n * k * (k + 1) - T / (k - 1);
  if (!(payda > 0)) throw new IstatistikHatasi('Her denekte tüm ölçümler eşit; Friedman testi hesaplanamaz.');
  const chi = (12 * pay) / payda;
  const sd = k - 1;
  const W = chi / (n * (k - 1));
  return {
    yontem: 'friedman',
    ad: 'Friedman testi',
    istatistik: chi,
    istatistikAdi: 'χ²',
    sd,
    p: pchisq(chi, sd, false),
    siraToplamlari,
    ortalamaSiralar: siraToplamlari.map((R) => R / n),
    medyanlar: Array.from({ length: k }, (_, j) => medyan(matris.map((s) => s[j]))),
    n,
    k,
    etki: { ad: 'Kendall W', sembol: 'W', deger: W },
    kendallW: W,
    uyarilar: [],
  };
}

/**
 * Kendall uyum katsayısı W (SPSS "Kendall's W" testi; satırlar = denek/değerlendirici, sütunlar = ölçüm/nesne).
 * Bağ düzeltmeli W = χ²_F/(n(k-1)); anlamlılık Friedman χ² ile sınanır.
 * @param {number[][]} matris
 */
export function kendallW(matris) {
  const f = friedman(matris);
  return {
    yontem: 'kendall-w',
    ad: 'Kendall uyum katsayısı (W)',
    istatistik: f.kendallW,
    istatistikAdi: 'W',
    sd: f.sd,
    p: f.p,
    W: f.kendallW,
    kiKare: f.istatistik,
    ortalamaSiralar: f.ortalamaSiralar,
    n: f.n,
    k: f.k,
    uyarilar: [],
  };
}

/**
 * Friedman sonrası ikili karşılaştırmalar.
 * yontem:
 *  'wilcoxon-bonferroni' (varsayılan): her çift için Wilcoxon işaretli sıralar testi (secenek.wilcoxonYontemi,
 *      varsayılan 'otomatik'; secenek.sifirYontemi; süreklilik düzeltmesiz), p'ler `duzeltme` ile düzeltilir
 *      (R: pairwise.wilcox.test(paired = TRUE, correct = FALSE, exact = FALSE, p.adjust.method); R 4.6 varsayılanı
 *      exact = NULL ise wilcoxonYontemi: 'kesin', sifirYontemi: 'pratt' ile aynıdır).
 *  'spss': SPSS'in ikili karşılaştırması: z = (R̄_i - R̄_j)/√(k(k+1)/(6n)), p Bonferroni ile düzeltilir.
 *  'conover': Conover (1999) t testi: |R_i - R_j| / √[2(n·A - ΣR_j²)/((n-1)(k-1))], sd = (n-1)(k-1);
 *      A = Σ R_ij² (bağlar dahil). p'ler `duzeltme` ile düzeltilir (PMCMRplus::frdAllPairsConoverTest).
 *  'nemenyi': q = √2·|R̄_i - R̄_j| / √(k(k+1)/(6n)), p = P(Q_{k,∞} > q) (studentized range; düzeltme gerekmez)
 *      (PMCMRplus::frdAllPairsNemenyiTest).
 * @param {number[][]} matris n × k
 * @param {{yontem?:string, duzeltme?:string, wilcoxonYontemi?:string}} secenek
 */
export function friedmanPostHoc(matris, secenek = {}) {
  const { yontem = 'wilcoxon-bonferroni', wilcoxonYontemi = 'otomatik', sifirYontemi = 'wilcoxon' } = secenek;
  const k = matrisKontrol(matris, 2, 'Friedman ikili karşılaştırmaları');
  const { n, siraToplamlari, kareToplami } = satirSiralari(matris);
  const duzeltme = secenek.duzeltme ?? (yontem === 'nemenyi' ? 'yok' : 'bonferroni');
  const karsilastirmalar = [];
  const uyarilar = [];
  const ciftler = [];
  for (let i = 0; i < k; i++) for (let j = i + 1; j < k; j++) ciftler.push([i, j]);
  if (yontem === 'wilcoxon-bonferroni') {
    for (const [i, j] of ciftler) {
      const xi = matris.map((s) => s[i]);
      const xj = matris.map((s) => s[j]);
      let w;
      try {
        w = wilcoxonIsaretliSira(xi, xj, { yontem: wilcoxonYontemi, sureklilik: false, hodgesLehmann: false, sifirYontemi });
      } catch (e) {
        if (!(e instanceof IstatistikHatasi)) throw e;
        karsilastirmalar.push({ olcum1: i, olcum2: j, istatistik: NaN, z: NaN, p: NaN });
        uyarilar.push(`${i + 1}. ve ${j + 1}. ölçüm karşılaştırılamadı: ${e.message}`);
        continue;
      }
      karsilastirmalar.push({ olcum1: i, olcum2: j, istatistik: w.V, z: w.z, p: w.p, pYontemi: w.pYontemi });
    }
  } else if (yontem === 'spss') {
    const sh = Math.sqrt((k * (k + 1)) / (6 * n));
    for (const [i, j] of ciftler) {
      const fark = (siraToplamlari[i] - siraToplamlari[j]) / n;
      const z = fark / sh;
      karsilastirmalar.push({ olcum1: i, olcum2: j, istatistik: fark, sh, z, p: Math.min(1, 2 * pnorm(-Math.abs(z))) });
    }
  } else if (yontem === 'conover') {
    gerekli(n >= 2, 'Conover testi için en az 2 denek gerekir.');
    let RR = 0;
    for (const R of siraToplamlari) RR += R * R;
    const sd = (n - 1) * (k - 1);
    const sh = Math.sqrt((2 * (n * kareToplami - RR)) / sd);
    if (!(sh > 0)) throw new IstatistikHatasi('Sıralarda değişkenlik olmadığından Conover testi hesaplanamaz.');
    for (const [i, j] of ciftler) {
      const fark = siraToplamlari[i] - siraToplamlari[j];
      const t = fark / sh;
      karsilastirmalar.push({ olcum1: i, olcum2: j, istatistik: fark, sh, t, sd, p: Math.min(1, 2 * pt(-Math.abs(t), sd)) });
    }
  } else if (yontem === 'nemenyi') {
    const sh = Math.sqrt((k * (k + 1)) / (6 * n));
    for (const [i, j] of ciftler) {
      const fark = (siraToplamlari[i] - siraToplamlari[j]) / n;
      const q = (Math.SQRT2 * Math.abs(fark)) / sh;
      karsilastirmalar.push({ olcum1: i, olcum2: j, istatistik: fark, q, p: ptukey(q, k, Infinity, false) });
    }
  } else throw new IstatistikHatasi(`Bilinmeyen ikili karşılaştırma yöntemi: ${yontem}`);
  const duz = pDuzelt(karsilastirmalar.map((c) => c.p), duzeltme);
  karsilastirmalar.forEach((c, i) => {
    c.pDuzeltilmis = duz[i];
  });
  return {
    yontem: `friedman-posthoc-${yontem}`,
    ad: 'Friedman sonrası ikili karşılaştırmalar',
    karsilastirmaYontemi: yontem,
    duzeltme,
    ortalamaSiralar: siraToplamlari.map((R) => R / n),
    karsilastirmalar,
    n,
    k,
    uyarilar,
  };
}

// ---------- Jonckheere-Terpstra ----------

/**
 * Jonckheere-Terpstra sıralı alternatifler testi. Gruplar beklenen sırayla verilmelidir.
 * J = Σ_{i<j} #{(a ∈ G_i, b ∈ G_j): a < b} + 0.5·#{a = b}. Büyük J artan eğilimi gösterir.
 * Varyans bağ düzeltmelidir (Lehmann 1975; SPSS ile aynı); bağ yoksa clinfun::jonckheere.test ile aynıdır.
 * yontem: 'otomatik' → bağ yoksa ve N <= 100 ise kesin dağılım (Mann-Whitney dağılımlarının evrişimi), aksi halde normal;
 * alternatif: 'iki-yonlu' | 'artan' | 'azalan'. İki yönlü kesin p = 2·min(alt, üst) (clinfun ile aynı).
 * @param {number[][]} siraliGruplar
 * @param {{yontem?:string, alternatif?:string}} secenek
 */
export function jonckheereTerpstra(siraliGruplar, secenek = {}) {
  const { yontem = 'otomatik' } = secenek;
  const altHarita = { 'iki-yonlu': 'iki-yonlu', 'two.sided': 'iki-yonlu', artan: 'artan', increasing: 'artan', buyuk: 'artan', azalan: 'azalan', decreasing: 'azalan', kucuk: 'azalan' };
  const alt = altHarita[secenek.alternatif ?? 'iki-yonlu'];
  gerekli(alt, `Bilinmeyen alternatif hipotez: ${secenek.alternatif}`);
  const { dolu, indisler, uyarilar } = gruplariHazirla(siraliGruplar, 2, 'Jonckheere-Terpstra testi');
  const k = dolu.length;
  const nler = dolu.map((g) => g.length);
  const N = nler.reduce((a, b) => a + b, 0);
  let J = 0;
  for (let i = 0; i < k; i++) {
    for (let j = i + 1; j < k; j++) {
      const birlesik = dolu[i].concat(dolu[j]);
      const { sira } = siralar(birlesik);
      let Rj = 0;
      for (let t = dolu[i].length; t < birlesik.length; t++) Rj += sira[t];
      J += Rj - (nler[j] * (nler[j] + 1)) / 2;
    }
  }
  let sumN2 = 0;
  for (const n of nler) sumN2 += n * n;
  const ort = (N * N - sumN2) / 4;
  const { baglar } = siralar(dolu.flat());
  let a1 = N * (N - 1) * (2 * N + 5);
  let b1 = 0;
  let b2 = 0;
  for (const n of nler) {
    a1 -= n * (n - 1) * (2 * n + 5);
    b1 += n * (n - 1) * (n - 2);
    b2 += n * (n - 1);
  }
  let c1 = 0;
  let c2 = 0;
  for (const t of baglar) {
    a1 -= t * (t - 1) * (2 * t + 5);
    c1 += t * (t - 1) * (t - 2);
    c2 += t * (t - 1);
  }
  let varyans = a1 / 72;
  if (N > 2) varyans += (b1 * c1) / (36 * N * (N - 1) * (N - 2));
  varyans += (b2 * c2) / (8 * N * (N - 1));
  if (!(varyans > 0)) throw new IstatistikHatasi('Tüm gözlemler aynı değere sahip; Jonckheere-Terpstra testi hesaplanamaz.');
  const ss = Math.sqrt(varyans);
  const z = (J - ort) / ss;
  const bagVar = baglar.length > 0;
  const kesin = yontem === 'kesin' ? !bagVar : yontem === 'otomatik' ? !bagVar && N <= 100 : false;
  if (yontem === 'kesin' && bagVar) uyarilar.push('Bağlı değerler olduğundan kesin dağılım yerine normal yaklaşım kullanıldı.');
  let p;
  let pAsimptotik;
  if (alt === 'artan') pAsimptotik = pnorm(z, 0, 1, false);
  else if (alt === 'azalan') pAsimptotik = pnorm(z);
  else pAsimptotik = Math.min(1, 2 * Math.min(pnorm(z), pnorm(z, 0, 1, false)));
  let pKesin = null;
  if (kesin) {
    const dag = jtDagilim(nler);
    const j = Math.round(J);
    let alt_ = 0;
    for (let i = 0; i <= j && i < dag.length; i++) alt_ += dag[i];
    let ust = 0;
    for (let i = Math.max(0, j); i < dag.length; i++) ust += dag[i];
    if (alt === 'artan') pKesin = Math.min(1, ust);
    else if (alt === 'azalan') pKesin = Math.min(1, alt_);
    else pKesin = Math.min(1, 2 * Math.min(alt_, ust));
  }
  p = pKesin !== null ? pKesin : pAsimptotik;
  return {
    yontem: 'jonckheere-terpstra',
    ad: 'Jonckheere-Terpstra testi',
    istatistik: J,
    istatistikAdi: 'J',
    sd: null,
    p,
    pYontemi: pKesin !== null ? 'kesin' : 'asimptotik',
    alternatif: alt,
    ortalama: ort,
    standartSapma: ss,
    z,
    pAsimptotik,
    pKesin,
    gruplar: dolu.map((g, i) => ({ grup: indisler[i], n: g.length, medyan: medyan(g) })),
    n: N,
    etki: { ad: 'r', sembol: 'r', deger: Math.abs(z) / Math.sqrt(N) },
    uyarilar,
  };
}

/** Bağsız durumda Jonckheere J'nin kesin olasılık dağılımı (indis = J) */
function jtDagilim(nler) {
  let dag = Float64Array.of(1);
  let kum = nler[0];
  for (let i = 1; i < nler.length; i++) {
    const f = mannWhitneyFrekans(kum, nler[i]);
    let t = 0;
    for (let u = 0; u < f.length; u++) t += f[u];
    const yeni = new Float64Array(dag.length + f.length - 1);
    for (let a = 0; a < dag.length; a++) {
      const da = dag[a];
      if (da === 0) continue;
      for (let b = 0; b < f.length; b++) yeni[a + b] += (da * f[b]) / t;
    }
    dag = yeni;
    kum += nler[i];
  }
  return dag;
}

// ---------- Mood medyan testi ----------

/**
 * Medyan testi (SPSS NPAR TESTS /MEDIAN gibi): tüm gözlemlerin medyanı hesaplanır; her grupta medyandan büyük
 * ve medyana eşit ya da küçük gözlemler sayılır; 2 × k tablo Pearson ki-kare ile sınanır.
 * Beklenen değeri 5'ten küçük hücre varsa Fisher(-Freeman-Halton) kesin p ana p olarak raporlanır.
 * @param {number[][]} gruplar
 * @param {{medyan?:number, fisher?:boolean|'otomatik'}} secenek medyan: karşılaştırma değeri (varsayılan: birleşik medyan)
 */
export function medyanTesti(gruplar, secenek = {}) {
  const { dolu, indisler, uyarilar } = gruplariHazirla(gruplar, 2, 'Medyan testi');
  const tum = dolu.flat();
  const m = secenek.medyan ?? medyan(tum);
  const ust = dolu.map((g) => g.filter((v) => v > m).length);
  const altSay = dolu.map((g, i) => g.length - ust[i]);
  const tablo = [ust, altSay];
  if (ust.every((v) => v === 0) || altSay.every((v) => v === 0)) {
    throw new IstatistikHatasi('Tüm gözlemler medyanın aynı tarafında; medyan testi hesaplanamaz.');
  }
  const kk = kiKare(tablo);
  const fisherIste = secenek.fisher ?? 'otomatik';
  const kucukBeklenen = kk.beklenenKucuk5.sayi > 0;
  let fisher = null;
  if (fisherIste === true || (fisherIste === 'otomatik' && kucukBeklenen)) {
    fisher = fisherKesin(tablo);
  }
  const fisherAna = fisher !== null && (fisherIste === true || kucukBeklenen);
  if (kucukBeklenen) uyarilar.push('Beklenen değeri 5\'ten küçük hücre var; Fisher kesin testi sonucu raporlandı.');
  return {
    yontem: 'medyan-testi',
    ad: 'Medyan testi',
    istatistik: kk.pearson.istatistik,
    istatistikAdi: 'χ²',
    sd: kk.pearson.sd,
    p: fisherAna ? fisher.p : kk.pearson.p,
    pYontemi: fisherAna ? fisher.pYontemi : 'asimptotik',
    medyan: m,
    tablo,
    gruplar: dolu.map((g, i) => ({ grup: indisler[i], n: g.length, buyuk: ust[i], kucukEsit: altSay[i] })),
    kiKare: kk.pearson,
    yates: kk.yates ?? null,
    fisher: fisher ? { p: fisher.p, pYontemi: fisher.pYontemi } : null,
    beklenen: kk.beklenen,
    n: tum.length,
    uyarilar,
  };
}
