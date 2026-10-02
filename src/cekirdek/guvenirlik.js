// İç tutarlılık güvenirliği: Cronbach alfa (madde istatistikleriyle), KR-20, KR-21, yarıya bölme (split-half),
// McDonald omega (tek faktörlü model).

import { qf } from './dagilim.js';
import { kovaryansMatrisi, simetrikTers, simetrikOzdeger, coz } from './matris.js';
import { ortalama, varyans } from './betimsel.js';
import { IstatistikHatasi, gerekli } from './hata.js';

// ---------- Yardımcılar ----------

/** Liste bazında silme: yalnızca tüm maddeleri sayısal olan satırlar kalır */
function tamSatirlar(matris, enAzMadde = 2) {
  gerekli(Array.isArray(matris) && matris.length > 0, 'Veri matrisi boş.');
  const k = matris[0].length;
  gerekli(k >= enAzMadde && matris.every((s) => Array.isArray(s) && s.length === k), `Her satırda aynı sayıda madde olmalıdır (en az ${enAzMadde}).`);
  const X = matris.filter((s) => s.every((v) => typeof v === 'number' && Number.isFinite(v)));
  const uyarilar = X.length < matris.length ? [`Eksik değer içeren ${matris.length - X.length} satır analizden çıkarıldı (liste bazında silme).`] : [];
  return { X, k, uyarilar };
}

function topla(A) {
  let s = 0;
  for (const r of A) for (const v of r) s += v;
  return s;
}

function altMatris(A, atilan) {
  return A.filter((_, i) => i !== atilan).map((r) => r.filter((_, j) => j !== atilan));
}

/** Kovaryans matrisinden ham alfa: k/(k−1)·(1 − iz(C)/ΣC) */
function alfaC(C) {
  const k = C.length;
  let iz = 0;
  for (let i = 0; i < k; i++) iz += C[i][i];
  return (k / (k - 1)) * (1 - iz / topla(C));
}

/** Feldt (1965/1987) F tabanlı güven aralığı (psych::alpha.ci; SPSS'te ICC(3,k) aralığına eşit) */
function feldtGA(alfa, n, k, duzey) {
  const a = 1 - duzey;
  return {
    duzey,
    alt: 1 - (1 - alfa) * qf(1 - a / 2, n - 1, (n - 1) * (k - 1)),
    ust: 1 - (1 - alfa) * qf(a / 2, n - 1, (n - 1) * (k - 1)),
    yontem: 'Feldt',
  };
}

function ozetIstatistik(degerler) {
  const ort = ortalama(degerler);
  const min = Math.min(...degerler);
  const maks = Math.max(...degerler);
  return {
    ortalama: ort,
    minimum: min,
    maksimum: maks,
    aralik: maks - min,
    maksMinOrani: min !== 0 ? maks / min : null,
    varyans: degerler.length > 1 ? varyans(degerler, ort) : null,
    sayi: degerler.length,
  };
}

// ---------- Cronbach alfa ----------

/**
 * Cronbach alfa ve madde istatistikleri (SPSS RELIABILITY /MODEL=ALPHA /SUMMARY=TOTAL ile aynı sayılar).
 * Girdi: satır = katılımcı, sütun = madde (sayısal). Eksik değerli satırlar atılır (liste bazında; SPSS gibi).
 * Varyansı sıfır olan maddeler ölçekten çıkarılır (SPSS ve psych gibi) ve uyarı verilir.
 *
 * Döndürür: alfa (ham), standartAlfa (korelasyonlardan: k·r̄/(1 + (k−1)r̄)), ga (Feldt F tabanlı; psych::alpha.ci),
 * ortalamaMaddeArasiKorelasyon (r̄), maddeler: [{ad, ortalama, ss, n, olcekOrtalamasiSilinirse,
 * olcekVaryansiSilinirse, duzeltilmisMaddeToplamKorelasyonu, karesiAlinmisCokluKorelasyon, alfaSilinirse}],
 * ozet (SPSS "Summary Item Statistics": maddeOrtalamalari, maddeVaryanslari, maddeArasiKovaryanslar,
 * maddeArasiKorelasyonlar), olcek (toplam puanın ortalama, varyans, ss).
 * R: psych::alpha (raw_alpha, std.alpha, r.drop, smc, alpha.drop) ile doğrulanmıştır.
 * Negatif düzeltilmiş madde-toplam korelasyonu ters kodlanmış madde işareti olabilir; uyarı verilir.
 *
 * @param {number[][]} matris
 * @param {{duzey?:number, maddeAdlari?:string[]}} [secenek]
 */
export function cronbachAlfa(matris, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  gerekli(duzey > 0 && duzey < 1, 'Güven düzeyi 0 ile 1 arasında olmalıdır.');
  const tam = tamSatirlar(matris, 2);
  const uyarilar = tam.uyarilar;
  let X = tam.X;
  let adlar = secenek.maddeAdlari ?? Array.from({ length: tam.k }, (_, i) => `Madde ${i + 1}`);
  gerekli(adlar.length === tam.k, 'Madde adlarının sayısı sütun sayısına eşit olmalıdır.');
  const n = X.length;
  gerekli(n >= 2, 'Cronbach alfa için en az 2 tam gözlem gerekir.');
  // Sıfır varyanslı maddeler
  const varyanslar = adlar.map((_, j) => varyans(X.map((s) => s[j])));
  const sabit = varyanslar.map((v, j) => (v > 0 ? -1 : j)).filter((j) => j >= 0);
  if (sabit.length) {
    uyarilar.push(`Varyansı sıfır olan maddeler ölçekten çıkarıldı: ${sabit.map((j) => adlar[j]).join(', ')}.`);
    const kalan = adlar.map((_, j) => j).filter((j) => !sabit.includes(j));
    X = X.map((s) => kalan.map((j) => s[j]));
    adlar = kalan.map((j) => adlar[j]);
  }
  const k = adlar.length;
  gerekli(k >= 2, 'Cronbach alfa için varyansı sıfırdan büyük en az 2 madde gerekir.');
  const C = kovaryansMatrisi(X);
  const sd = C.map((r, i) => Math.sqrt(r[i]));
  const R = C.map((r, i) => r.map((v, j) => v / (sd[i] * sd[j])));
  const toplamC = topla(C);
  const alfa = alfaC(C);
  const rbar = (topla(R) - k) / (k * (k - 1));
  const standartAlfa = (k * rbar) / (1 + (k - 1) * rbar);
  // SMC: 1 − 1/(R⁻¹)_ii
  let smc = null;
  if (n > k) {
    try {
      const Ri = simetrikTers(R);
      smc = Ri.map((r, i) => 1 - 1 / r[i]);
      if (!smc.every((v) => Number.isFinite(v) && v >= -1e-10 && v <= 1 + 1e-10)) smc = null;
    } catch {
      smc = null;
    }
  }
  if (smc === null) uyarilar.push('Korelasyon matrisi tekil (ya da madde sayısı ≥ gözlem sayısı); kareli çoklu korelasyonlar hesaplanamadı.');
  const ortalamalar = adlar.map((_, j) => ortalama(X.map((s) => s[j])));
  const ortToplam = ortalamalar.reduce((a, b) => a + b, 0);
  const maddeler = adlar.map((ad, i) => {
    const Ci = altMatris(C, i);
    const vSil = topla(Ci);
    const satirToplam = C[i].reduce((a, b) => a + b, 0);
    return {
      ad,
      ortalama: ortalamalar[i],
      ss: sd[i],
      n,
      olcekOrtalamasiSilinirse: ortToplam - ortalamalar[i],
      olcekVaryansiSilinirse: vSil,
      duzeltilmisMaddeToplamKorelasyonu: (satirToplam - C[i][i]) / Math.sqrt(C[i][i] * vSil),
      karesiAlinmisCokluKorelasyon: smc ? smc[i] : null,
      alfaSilinirse: k > 2 ? alfaC(Ci) : null,
    };
  });
  const negatif = maddeler.filter((m) => m.duzeltilmisMaddeToplamKorelasyonu < 0);
  if (negatif.length) {
    uyarilar.push(`Düzeltilmiş madde-toplam korelasyonu negatif olan maddeler var (${negatif.map((m) => m.ad).join(', ')}); bu maddeler ters kodlanmış olabilir.`);
  }
  if (alfa < 0) uyarilar.push('Alfa negatif: maddeler arası ortalama kovaryans negatif. Bu, güvenirlik modelinin varsayımlarını ihlal eder (ters kodlanmış maddeleri denetleyin).');
  const kovaryanslar = [];
  const korelasyonlar = [];
  for (let i = 0; i < k; i++) {
    for (let j = i + 1; j < k; j++) {
      kovaryanslar.push(C[i][j]);
      korelasyonlar.push(R[i][j]);
    }
  }
  const ga = feldtGA(alfa, n, k, duzey);
  return {
    yontem: 'cronbach-alfa',
    ad: 'Cronbach alfa',
    alfa,
    standartAlfa,
    ga: { ...ga, neyin: 'Cronbach alfa' },
    ortalamaMaddeArasiKorelasyon: rbar,
    maddeSayisi: k,
    n,
    maddeler,
    ozet: {
      maddeOrtalamalari: ozetIstatistik(ortalamalar),
      maddeVaryanslari: ozetIstatistik(sd.map((s) => s * s)),
      maddeArasiKovaryanslar: ozetIstatistik(kovaryanslar),
      maddeArasiKorelasyonlar: ozetIstatistik(korelasyonlar),
    },
    olcek: { ortalama: ortToplam, varyans: toplamC, ss: Math.sqrt(toplamC), maddeSayisi: k },
    uyarilar,
  };
}

// ---------- KR-20 / KR-21 ----------

function ikiliDenetle(X) {
  gerekli(X.every((s) => s.every((v) => v === 0 || v === 1)), 'KR-20/KR-21 yalnızca 0/1 (yanlış/doğru) puanlanmış maddeler içindir.');
}

/** Toplam puanların n paydalı (popülasyon) varyansı */
function toplamPuanlar(X) {
  return X.map((s) => s.reduce((a, b) => a + b, 0));
}

/**
 * Kuder-Richardson 20: KR-20 = k/(k−1)·(1 − Σ p_i q_i / σ²_X). p_i q_i ve σ²_X aynı (n) paydayla hesaplandığından
 * 0/1 maddelerde Cronbach alfaya tam eşittir (SPSS bu durumda alfa olarak aynı değeri verir). Feldt GA'sı da döner.
 * @param {number[][]} matris 0/1 matris (satır = katılımcı, sütun = madde)
 * @param {{duzey?:number}} [secenek]
 */
export function kr20(matris, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  const { X, k, uyarilar } = tamSatirlar(matris, 2);
  ikiliDenetle(X);
  const n = X.length;
  gerekli(n >= 2, 'KR-20 için en az 2 tam gözlem gerekir.');
  const p = Array.from({ length: k }, (_, j) => X.reduce((s, r) => s + r[j], 0) / n);
  const T = toplamPuanlar(X);
  const varPop = (varyans(T) * (n - 1)) / n;
  gerekli(varPop > 0, 'Toplam puanların varyansı sıfır; KR-20 hesaplanamaz.');
  const pq = p.reduce((s, v) => s + v * (1 - v), 0);
  const deger = (k / (k - 1)) * (1 - pq / varPop);
  return {
    yontem: 'kr20',
    ad: 'Kuder-Richardson 20',
    kr20: deger,
    ga: { ...feldtGA(deger, n, k, duzey), neyin: 'KR-20' },
    maddeSayisi: k,
    n,
    maddeGuclukleri: p,
    toplamVaryans: varPop,
    ortalamaPuan: ortalama(T),
    uyarilar,
  };
}

/**
 * Kuder-Richardson 21: KR-21 = k/(k−1)·(1 − M(k − M)/(k·σ²_X)); M ortalama toplam puan, σ²_X n paydalı.
 * Tüm maddelerin güçlüğü eşitse KR-20'ye eşittir, aksi halde ondan küçüktür.
 * @param {number[][]} matris 0/1 matris
 */
export function kr21(matris) {
  const { X, k, uyarilar } = tamSatirlar(matris, 2);
  ikiliDenetle(X);
  const n = X.length;
  gerekli(n >= 2, 'KR-21 için en az 2 tam gözlem gerekir.');
  const T = toplamPuanlar(X);
  const M = ortalama(T);
  const varPop = (varyans(T, M) * (n - 1)) / n;
  gerekli(varPop > 0, 'Toplam puanların varyansı sıfır; KR-21 hesaplanamaz.');
  return {
    yontem: 'kr21',
    ad: 'Kuder-Richardson 21',
    kr21: (k / (k - 1)) * (1 - (M * (k - M)) / (k * varPop)),
    maddeSayisi: k,
    n,
    ortalamaPuan: M,
    toplamVaryans: varPop,
    uyarilar,
  };
}

// ---------- Yarıya bölme ----------

/**
 * Yarıya bölme güvenirliği (SPSS RELIABILITY /MODEL=SPLIT ile aynı tanımlar).
 * Maddeler sırasıyla iki parçaya ayrılır: ilk ⌈k/2⌉ madde 1. parça, kalanlar 2. parça (SPSS varsayılanı: madde
 * sayısı tekse fazla madde ilk parçaya gider). `ikinciParcaMaddeSayisi` ile değiştirilebilir (SPSS SPLIT(n)).
 * r: iki parça toplamları arasındaki Pearson korelasyonu ("Correlation Between Forms").
 * Spearman-Brown eşit uzunluk: 2r/(1 + r).
 * Spearman-Brown eşit olmayan uzunluk (Horst): [−r² + √(r⁴ + 4r²(1 − r²)k1k2/k²)] / [2(1 − r²)k1k2/k²].
 * Guttman yarıya bölme: 2·(1 − (s1² + s2²)/s²_toplam).
 * Her parçanın Cronbach alfası (en az 2 madde varsa).
 * @param {number[][]} matris
 * @param {{ikinciParcaMaddeSayisi?:number}} [secenek]
 */
export function splitHalf(matris, secenek = {}) {
  const { X, k, uyarilar } = tamSatirlar(matris, 2);
  const n = X.length;
  gerekli(n >= 3, 'Yarıya bölme güvenirliği için en az 3 tam gözlem gerekir.');
  const k2 = secenek.ikinciParcaMaddeSayisi ?? Math.floor(k / 2);
  gerekli(Number.isInteger(k2) && k2 >= 1 && k2 < k, 'İkinci parçadaki madde sayısı 1 ile (madde sayısı − 1) arasında olmalıdır.');
  const k1 = k - k2;
  const A = X.map((s) => s.slice(0, k1));
  const B = X.map((s) => s.slice(k1));
  const t1 = A.map((s) => s.reduce((a, b) => a + b, 0));
  const t2 = B.map((s) => s.reduce((a, b) => a + b, 0));
  const tt = t1.map((v, i) => v + t2[i]);
  const m1 = ortalama(t1);
  const m2 = ortalama(t2);
  const v1 = varyans(t1, m1);
  const v2 = varyans(t2, m2);
  const vt = varyans(tt);
  gerekli(v1 > 0 && v2 > 0, 'Parçalardan birinin toplam puan varyansı sıfır; yarıya bölme güvenirliği hesaplanamaz.');
  let c12 = 0;
  for (let i = 0; i < n; i++) c12 += (t1[i] - m1) * (t2[i] - m2);
  c12 /= n - 1;
  const r = c12 / Math.sqrt(v1 * v2);
  const r2 = r * r;
  const c = (k1 * k2) / (k * k);
  const esitOlmayan = r2 === 1 ? 1 : (-r2 + Math.sqrt(r2 * r2 + 4 * r2 * (1 - r2) * c)) / (2 * (1 - r2) * c);
  const parcaAlfa = (P) => (P[0].length >= 2 ? alfaC(kovaryansMatrisi(P)) : null);
  if (r < 0) uyarilar.push('Parçalar arası korelasyon negatif; Spearman-Brown katsayısı anlamlı değildir.');
  return {
    yontem: 'yariya-bolme',
    ad: 'Yarıya bölme güvenirliği',
    parcalarArasiKorelasyon: r,
    spearmanBrownEsit: (2 * r) / (1 + r),
    spearmanBrownEsitOlmayan: esitOlmayan,
    guttman: 2 * (1 - (v1 + v2) / vt),
    parca1: { maddeSayisi: k1, alfa: parcaAlfa(A), ortalama: m1, varyans: v1 },
    parca2: { maddeSayisi: k2, alfa: parcaAlfa(B), ortalama: m2, varyans: v2 },
    olcek: { ortalama: ortalama(tt), varyans: vt },
    maddeSayisi: k,
    n,
    uyarilar,
  };
}

// ---------- McDonald omega ----------

/**
 * Tek faktörlü modelin en çok olabilirlik (ML) çözümü (korelasyon matrisi üzerinde, ölçekten bağımsız).
 * Verilen ψ için en iyi yükler: Ψ^{-1/2} R Ψ^{-1/2}'nin en büyük özdeğeri θ ve özvektörü v ile λ = Ψ^{1/2}·v·√(θ − 1).
 * ML durağanlık koşulu ψ_i = 1 − λ_i(ψ)² (Heywood durumu dışında) Newton yöntemiyle (sayısal Jacobian) çözülür.
 */
function tekFaktorML(R) {
  const p = R.length;
  const yukler = (psi) => {
    const s = psi.map(Math.sqrt);
    const A = R.map((r, i) => r.map((v, j) => v / (s[i] * s[j])));
    const { degerler, vektorler } = simetrikOzdeger(A);
    const c = Math.sqrt(Math.max(degerler[0] - 1, 0));
    return s.map((si, i) => si * vektorler[i][0] * c);
  };
  const h = (psi) => {
    const l = yukler(psi);
    return psi.map((v, i) => v + l[i] * l[i] - 1);
  };
  const ALT = 0.005; // factanal'ın alt sınırı
  // Başlangıç: ψ = 1 − SMC (factanal gibi)
  let psi;
  try {
    const Ri = simetrikTers(R);
    psi = Ri.map((r, i) => Math.min(1, Math.max(ALT, 1 / r[i])));
  } catch {
    psi = new Array(p).fill(0.5);
  }
  // Önce sabit nokta yinelemesiyle yaklaş, sonra Newton
  for (let it = 0; it < 50; it++) {
    const l = yukler(psi);
    psi = l.map((v) => Math.min(1, Math.max(ALT, 1 - v * v)));
  }
  let heywood = false;
  for (let it = 0; it < 100; it++) {
    const f = h(psi);
    const norm = Math.max(...f.map(Math.abs));
    if (norm < 1e-14) break;
    const J = Array.from({ length: p }, () => new Array(p).fill(0));
    for (let j = 0; j < p; j++) {
      const e = 1e-7 * Math.max(psi[j], 1e-3);
      const ps = psi.slice();
      ps[j] += e;
      const fp = h(ps);
      const pm = psi.slice();
      pm[j] -= e;
      const fm = h(pm);
      for (let i = 0; i < p; i++) J[i][j] = (fp[i] - fm[i]) / (2 * e);
    }
    let adim;
    try {
      adim = coz(J, f);
    } catch {
      adim = null;
    }
    if (!adim || !adim.every(Number.isFinite)) break;
    // Adım yarılama
    let t = 1;
    let yeni;
    for (let y = 0; y < 30; y++) {
      yeni = psi.map((v, i) => Math.min(1, Math.max(ALT, v - t * adim[i])));
      if (Math.max(...h(yeni).map(Math.abs)) < norm) break;
      t /= 2;
    }
    if (yeni.every((v, i) => v === psi[i])) break;
    psi = yeni;
  }
  if (psi.some((v) => v <= ALT + 1e-12)) heywood = true;
  const l = yukler(psi);
  // Yük yönü: toplamı pozitif olsun
  const isaret = l.reduce((a, b) => a + b, 0) < 0 ? -1 : 1;
  return { yukler: l.map((v) => v * isaret), psi, heywood };
}

/**
 * McDonald omega (ω_toplam) — tek faktörlü (konjenerik) modelden, ML kestirimiyle.
 * ω = (Σλ_i)² / [(Σλ_i)² + Σθ_i]; λ_i ve θ_i ham (kovaryans) ölçekteki yükler ve hata varyansları (ML ölçekten
 * bağımsız olduğu için korelasyon matrisindeki standart çözüm s_i ile çarpılır). Bu, lavaan::cfa (std.lv, ML) ile
 * kurulan tek faktörlü modelin ω'sıdır (MBESS::ci.reliability(type = 'omega') nokta kestirimi, JASP/jamovi "ω").
 * omegaStandart: maddeler standartlaştırılmışsa (z puanları toplamı) ω.
 * Not: psych::omega(nfactors = 1) farklı bir tanım (gözlenen toplam varyans, minres) kullanır; sonuçlar farklıdır.
 * Güven aralığı verilmez.
 * @param {number[][]} matris
 * @param {{maddeAdlari?:string[]}} [secenek]
 */
export function mcdonaldOmega(matris, secenek = {}) {
  const tam = tamSatirlar(matris, 3);
  const { X, k } = tam;
  const uyarilar = tam.uyarilar;
  const n = X.length;
  gerekli(n > k, 'Omega için gözlem sayısı madde sayısından fazla olmalıdır.');
  const C = kovaryansMatrisi(X);
  const s = C.map((r, i) => Math.sqrt(r[i]));
  gerekli(s.every((v) => v > 0), 'Varyansı sıfır olan madde var; omega hesaplanamaz.');
  const R = C.map((r, i) => r.map((v, j) => v / (s[i] * s[j])));
  let cozum;
  try {
    cozum = tekFaktorML(R);
  } catch (e) {
    throw new IstatistikHatasi('Tek faktörlü model kestirilemedi (korelasyon matrisi tekil olabilir).');
  }
  if (cozum.heywood) uyarilar.push('Heywood durumu: bir maddenin hata varyansı sıfıra dayandı; omega güvenilir olmayabilir.');
  const lam = cozum.yukler.map((v, i) => v * s[i]);
  const teta = cozum.psi.map((v, i) => v * s[i] * s[i]);
  const L = lam.reduce((a, b) => a + b, 0);
  const T = teta.reduce((a, b) => a + b, 0);
  const Ls = cozum.yukler.reduce((a, b) => a + b, 0);
  const Ts = cozum.psi.reduce((a, b) => a + b, 0);
  const adlar = secenek.maddeAdlari ?? Array.from({ length: k }, (_, i) => `Madde ${i + 1}`);
  const negatif = cozum.yukler.map((v, i) => (v < 0 ? adlar[i] : null)).filter(Boolean);
  if (negatif.length) uyarilar.push(`Faktör yükü negatif olan maddeler var (${negatif.join(', ')}); ters kodlanmış olabilir.`);
  return {
    yontem: 'mcdonald-omega',
    ad: 'McDonald omega (tek faktör, ML)',
    omega: (L * L) / (L * L + T),
    omegaStandart: (Ls * Ls) / (Ls * Ls + Ts),
    maddeler: adlar.map((ad, i) => ({ ad, standartYuk: cozum.yukler[i], yuk: lam[i], hataVaryansi: teta[i], ortakVaryans: cozum.yukler[i] ** 2 })),
    maddeSayisi: k,
    n,
    uyarilar,
  };
}
