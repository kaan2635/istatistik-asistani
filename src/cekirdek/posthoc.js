// Tek yönlü ANOVA sonrası çoklu karşılaştırmalar (post hoc testler).
// Her fonksiyon gruplar (number[][]) alır ve ikili karşılaştırma satırları döndürür:
//   {i, j, grupI, grupJ, fark, sh, istatistik, sd, p, gaAlt, gaUst}
// fark = x̄ᵢ − x̄ⱼ (SPSS "Mean Difference (I−J)"), sh: farkın standart hatası (SPSS "Std. Error"),
// p: düzeltilmiş anlamlılık, [gaAlt, gaUst]: eşzamanlı güven aralığı. Satırlar i < j sırasıyla gelir.
//
// Havuzlanmış hata varyansı kullananlar (varyans homojenliği varsayımı): Tukey HSD, Bonferroni, LSD, Scheffé, Dunnett.
// Varyans homojenliği gerektirmeyenler: Games-Howell, Tamhane T2.

import { pf, pt, ptukey, qf, qt, qtukey } from './dagilim.js';
import { ortalama, toplam, varyans } from './betimsel.js';
import { LOG_SQRT_2PI, brent, gaussLegendre, log1pmx, stirlingDuzeltme } from './sayisal.js';
import { IstatistikHatasi } from './hata.js';

// ---------- Ortak hazırlık ----------

function hazirla(gruplar, secenek, testAdi, enAz = 1) {
  if (!Array.isArray(gruplar) || gruplar.length < 2) throw new IstatistikHatasi(`${testAdi} için en az 2 grup gerekir.`);
  const duzey = secenek.duzey ?? 0.95;
  if (!(duzey > 0 && duzey < 1)) throw new IstatistikHatasi('Güven düzeyi 0 ile 1 arasında olmalıdır.');
  const adlar = secenek.adlar ?? [];
  const ozet = gruplar.map((g, i) => {
    if (!g || g.length < enAz) {
      throw new IstatistikHatasi(enAz > 1 ? `${testAdi} için her grupta en az ${enAz} gözlem gerekir.` : `${testAdi}: boş grup var.`);
    }
    for (const v of g) {
      if (typeof v !== 'number' || !Number.isFinite(v)) throw new IstatistikHatasi(`${testAdi}: veride eksik ya da sayısal olmayan değer var.`);
    }
    const m = ortalama(g);
    const v = g.length > 1 ? varyans(g, m) : NaN;
    return { ad: adlar[i] ?? `Grup ${i + 1}`, n: g.length, ortalama: m, varyans: v, standartSapma: g.length > 1 ? Math.sqrt(v) : null };
  });
  const k = gruplar.length;
  const N = ozet.reduce((s, o) => s + o.n, 0);
  return { ozet, k, N, duzey };
}

function havuzluHata(gruplar, ozet, N, k, testAdi) {
  const sdHata = N - k;
  if (sdHata < 1) throw new IstatistikHatasi(`${testAdi} için toplam gözlem sayısı grup sayısından büyük olmalıdır.`);
  const KT = toplam(gruplar.flatMap((g, i) => g.map((v) => (v - ozet[i].ortalama) ** 2)));
  const KO = KT / sdHata;
  if (!(KO > 0)) throw new IstatistikHatasi(`${testAdi}: grup içi varyans sıfır; karşılaştırma yapılamaz.`);
  return { sdHata, KO };
}

function ciftler(k) {
  const c = [];
  for (let i = 0; i < k; i++) for (let j = i + 1; j < k; j++) c.push([i, j]);
  return c;
}

function satir(ozet, i, j, fark, sh, istatistik, sd, p, yari) {
  return { i, j, grupI: ozet[i].ad, grupJ: ozet[j].ad, fark, sh, istatistik, sd, p: Math.min(1, Math.max(0, p)), gaAlt: fark - yari, gaUst: fark + yari };
}

function sonuc(yontem, ad, istatistikAdi, h, karsilastirmalar, ek = {}) {
  return {
    yontem,
    ad,
    istatistikAdi,
    duzey: h.duzey,
    k: h.k,
    n: h.N,
    gruplar: h.ozet.map(({ ad: a, n, ortalama: m, standartSapma }) => ({ ad: a, n, ortalama: m, standartSapma })),
    ...ek,
    karsilastirmalar,
    uyarilar: ek.uyarilar ?? [],
  };
}

/** Welch–Satterthwaite: farkın SH'si ve serbestlik derecesi */
function welchSH(a, b) {
  const s1 = a.varyans / a.n;
  const s2 = b.varyans / b.n;
  const sh = Math.sqrt(s1 + s2);
  const sd = (s1 + s2) ** 2 / ((s1 * s1) / (a.n - 1) + (s2 * s2) / (b.n - 1));
  return { sh, sd };
}

// ---------- Havuzlanmış hata varyanslı testler ----------

/**
 * Tukey HSD (eşit olmayan n'de Tukey-Kramer). R TukeyHSD(aov(...)) ile aynı; SPSS ONEWAY POSTHOC = TUKEY
 * "Multiple Comparisons" tablosu (SPSS homojen alt kümeler tablosunda harmonik ortalama n kullanır, burada değil).
 * q = |fark| / √(KO_hata/2 · (1/nᵢ + 1/nⱼ)), p = P(Q_{k, ν} > q), GA = fark ± q_{1−α; k, ν}/√2 · sh.
 * @param {number[][]} gruplar
 * @param {{adlar?:string[], duzey?:number}} [secenek]
 */
export function tukeyHSD(gruplar, secenek = {}) {
  const h = hazirla(gruplar, secenek, 'Tukey HSD testi');
  const { sdHata, KO } = havuzluHata(gruplar, h.ozet, h.N, h.k, 'Tukey HSD testi');
  const kritik = qtukey(h.duzey, h.k, sdHata) / Math.SQRT2;
  const satirlar = ciftler(h.k).map(([i, j]) => {
    const a = h.ozet[i];
    const b = h.ozet[j];
    const fark = a.ortalama - b.ortalama;
    const sh = Math.sqrt(KO * (1 / a.n + 1 / b.n));
    const q = Math.abs(fark) / (sh / Math.SQRT2);
    return satir(h.ozet, i, j, fark, sh, q, sdHata, ptukey(q, h.k, sdHata, false), kritik * sh);
  });
  return sonuc('tukey', 'Tukey HSD', 'q', h, satirlar, { sdHata, KOhata: KO, kritikDeger: kritik * Math.SQRT2 });
}

function tTabanli(gruplar, secenek, yontem, ad, duzeltme) {
  const h = hazirla(gruplar, secenek, `${ad} testi`);
  const { sdHata, KO } = havuzluHata(gruplar, h.ozet, h.N, h.k, `${ad} testi`);
  const m = (h.k * (h.k - 1)) / 2;
  const alfa = 1 - h.duzey;
  let kritik;
  if (duzeltme === 'bonferroni') kritik = qt(1 - alfa / (2 * m), sdHata);
  else if (duzeltme === 'scheffe') kritik = Math.sqrt((h.k - 1) * qf(h.duzey, h.k - 1, sdHata));
  else kritik = qt(1 - alfa / 2, sdHata);
  const satirlar = ciftler(h.k).map(([i, j]) => {
    const a = h.ozet[i];
    const b = h.ozet[j];
    const fark = a.ortalama - b.ortalama;
    const sh = Math.sqrt(KO * (1 / a.n + 1 / b.n));
    const t = fark / sh;
    let p;
    let istatistik = t;
    if (duzeltme === 'scheffe') {
      istatistik = (t * t) / (h.k - 1);
      p = pf(istatistik, h.k - 1, sdHata, false);
    } else {
      p = 2 * pt(-Math.abs(t), sdHata);
      if (duzeltme === 'bonferroni') p = Math.min(1, p * m);
    }
    return satir(h.ozet, i, j, fark, sh, istatistik, duzeltme === 'scheffe' ? [h.k - 1, sdHata] : sdHata, p, kritik * sh);
  });
  return sonuc(yontem, ad, duzeltme === 'scheffe' ? 'F' : 't', h, satirlar, { sdHata, KOhata: KO, kritikDeger: kritik, karsilastirmaSayisi: m });
}

/**
 * Bonferroni düzeltmeli ikili t testleri, havuzlanmış KO_hata ile (SPSS POSTHOC = BONFERRONI;
 * R pairwise.t.test(pool.sd = TRUE, p.adjust.method = "bonferroni")). p = min(1, m·p_ham), m = k(k−1)/2,
 * GA = fark ± t_{1−α/(2m); N−k} · sh.
 * @param {number[][]} gruplar
 * @param {{adlar?:string[], duzey?:number}} [secenek]
 */
export function bonferroniPostHoc(gruplar, secenek = {}) {
  return tTabanli(gruplar, secenek, 'bonferroni', 'Bonferroni', 'bonferroni');
}

/**
 * Fisher LSD: düzeltmesiz ikili t testleri, havuzlanmış KO_hata ile (SPSS POSTHOC = LSD;
 * R pairwise.t.test(pool.sd = TRUE, p.adjust.method = "none")).
 * @param {number[][]} gruplar
 * @param {{adlar?:string[], duzey?:number}} [secenek]
 */
export function lsd(gruplar, secenek = {}) {
  return tTabanli(gruplar, secenek, 'lsd', 'LSD', 'yok');
}

/**
 * Scheffé testi (SPSS POSTHOC = SCHEFFE). F = (fark/sh)² / (k − 1), p = P(F_{k−1, N−k} > F),
 * GA = fark ± √((k − 1) F_{1−α; k−1, N−k}) · sh. istatistik alanı bu F değeridir.
 * @param {number[][]} gruplar
 * @param {{adlar?:string[], duzey?:number}} [secenek]
 */
export function scheffe(gruplar, secenek = {}) {
  return tTabanli(gruplar, secenek, 'scheffe', 'Scheffé', 'scheffe');
}

// ---------- Varyans homojenliği gerektirmeyen testler ----------

/**
 * Games-Howell testi (SPSS POSTHOC = GH; rstatix::games_howell_test ile aynı).
 * sh = √(sᵢ²/nᵢ + sⱼ²/nⱼ), ν Welch–Satterthwaite, q = |fark| / (sh/√2), p = P(Q_{k, ν} > q),
 * GA = fark ± q_{1−α; k, ν}/√2 · sh. Her grupta en az 2 gözlem gerekir.
 * @param {number[][]} gruplar
 * @param {{adlar?:string[], duzey?:number}} [secenek]
 */
export function gamesHowell(gruplar, secenek = {}) {
  const h = hazirla(gruplar, secenek, 'Games-Howell testi', 2);
  const satirlar = ciftler(h.k).map(([i, j]) => {
    const a = h.ozet[i];
    const b = h.ozet[j];
    const { sh, sd } = welchSH(a, b);
    if (!(sh > 0)) throw new IstatistikHatasi(`Games-Howell testi: "${a.ad}" ve "${b.ad}" gruplarının ikisinin de varyansı sıfır.`);
    const fark = a.ortalama - b.ortalama;
    const q = Math.abs(fark) / (sh / Math.SQRT2);
    const kritik = qtukey(h.duzey, h.k, sd) / Math.SQRT2;
    return satir(h.ozet, i, j, fark, sh, q, sd, ptukey(q, h.k, sd, false), kritik * sh);
  });
  return sonuc('games-howell', 'Games-Howell', 'q', h, satirlar);
}

/**
 * Tamhane T2 testi (SPSS POSTHOC = T2): Welch t testleri, Šidák düzeltmesiyle.
 * p = 1 − (1 − p_ham)^m, m = k(k−1)/2; GA = fark ± t_{1−α'/2; ν} · sh, α' = 1 − (1 − α)^(1/m).
 * @param {number[][]} gruplar
 * @param {{adlar?:string[], duzey?:number}} [secenek]
 */
export function tamhaneT2(gruplar, secenek = {}) {
  const h = hazirla(gruplar, secenek, 'Tamhane T2 testi', 2);
  const m = (h.k * (h.k - 1)) / 2;
  const alfaKiyas = -Math.expm1(Math.log(h.duzey) / m); // 1 − duzey^(1/m)
  const satirlar = ciftler(h.k).map(([i, j]) => {
    const a = h.ozet[i];
    const b = h.ozet[j];
    const { sh, sd } = welchSH(a, b);
    if (!(sh > 0)) throw new IstatistikHatasi(`Tamhane T2 testi: "${a.ad}" ve "${b.ad}" gruplarının ikisinin de varyansı sıfır.`);
    const fark = a.ortalama - b.ortalama;
    const t = fark / sh;
    const pHam = 2 * pt(-Math.abs(t), sd);
    const p = -Math.expm1(m * Math.log1p(-Math.min(pHam, 1))); // 1 − (1 − p)^m, iptal hatasız
    return satir(h.ozet, i, j, fark, sh, t, sd, p, qt(1 - alfaKiyas / 2, sd) * sh);
  });
  return sonuc('tamhane-t2', 'Tamhane T2', 't', h, satirlar, { karsilastirmaSayisi: m });
}

// ---------- Dunnett (kontrol grubuyla karşılaştırma) ----------

// Hızlı standart normal dağılım fonksiyonu: W. J. Cody (1969) rasyonel yaklaşımları, R'nin pnorm_both'u ile aynı
// katsayılar (göreli hata ~1e-16). Dunnett integralinde milyonlarca kez çağrıldığı için ayrı tutulur.
const CA = [2.2352520354606839287, 161.02823106855587881, 1067.6894854603709582, 18154.981253343561249, 0.065682337918207449113];
const CB = [47.20258190468824187, 976.09855173777669322, 10260.932208618978205, 45507.789335026729956];
const CC = [
  0.39894151208813466764, 8.8831497943883759412, 93.506656132177855979, 597.27027639480026226, 2494.5375852903726711, 6848.1904505362823326,
  11602.651437647350124, 9842.7148383839780218, 1.0765576773720192317e-8,
];
const CD = [22.266688044328115691, 235.38790178262499861, 1519.377599407554805, 6485.558298266760755, 18615.571640885098091, 34900.952721145977266, 38912.003286093271411, 19685.429676859990727];
const CP = [0.21589853405795699, 0.1274011611602473639, 0.022235277870649807, 0.001421619193227893466, 2.9112874951168792e-5, 0.02307344176494017303];
const CQ = [1.28426009614491121, 0.468238212480865118, 0.0659881378689285515, 0.00378239633202758244, 7.29751555083966205e-5];
const M_1_SQRT_2PI = 0.398942280401432677939946059934;

/** Φ(x) (alt kuyruk) */
export function phiHizli(x) {
  const y = Math.abs(x);
  if (y <= 0.67448975) {
    let xnum = 0;
    let xden = 0;
    if (y > 1.1102230246251565e-16) {
      const xsq = x * x;
      xnum = CA[4] * xsq;
      xden = xsq;
      for (let i = 0; i < 3; i++) {
        xnum = (xnum + CA[i]) * xsq;
        xden = (xden + CB[i]) * xsq;
      }
    }
    return 0.5 + (x * (xnum + CA[3])) / (xden + CB[3]);
  }
  let temp;
  if (y <= 5.656854249492380195206754896838) {
    let xnum = CC[8] * y;
    let xden = y;
    for (let i = 0; i < 7; i++) {
      xnum = (xnum + CC[i]) * y;
      xden = (xden + CD[i]) * y;
    }
    temp = (xnum + CC[7]) / (xden + CD[7]);
  } else if (y < 37.5193) {
    const xsq = 1 / (x * x);
    let xnum = CP[5] * xsq;
    let xden = xsq;
    for (let i = 0; i < 4; i++) {
      xnum = (xnum + CP[i]) * xsq;
      xden = (xden + CQ[i]) * xsq;
    }
    temp = (xsq * (xnum + CP[4])) / (xden + CQ[4]);
    temp = (M_1_SQRT_2PI - temp) / y;
  } else {
    return x > 0 ? 1 : 0;
  }
  const xsq = Math.trunc(y * 16) / 16;
  const del = (y - xsq) * (y + xsq);
  const kuyruk = Math.exp(-xsq * xsq * 0.5) * Math.exp(-del * 0.5) * temp; // P(Z > |x|)
  return x > 0 ? 1 - kuyruk : kuyruk;
}

/**
 * s = √(χ²_ν/ν) için log yoğunluk. Büyük ν'de terimler birbirini götürdüğünden Stirling biçiminde yazılır:
 * log f(s) = ½ log(2ν) − log √(2π) − δ(ν/2) + (ν/2)[log(1+e) − e] − ½ log(1+e),  e = s² − 1,
 * δ: Stirling düzeltme terimi. (Doğrudan lgamma ile yazılan biçim ν = 1e5'te ~1e-10 göreli hata verir.)
 */
function logSYogunluk(s, nu) {
  if (s <= 0) return -Infinity;
  const e = (s - 1) * (s + 1);
  return 0.5 * Math.log(2 * nu) - LOG_SQRT_2PI - stirlingDuzeltme(nu / 2) + (nu / 2) * log1pmx(e) - 0.5 * Math.log1p(e);
}

/** s üzerindeki integral için parça sınırları: yoğunluğun yoğunlaştığı 1 ± k/√(2ν) çevresi */
function sParcalari(nu) {
  const kok = Math.sqrt(2 * nu);
  const a = Math.sqrt(Math.max(0, nu - 14 * kok) / nu);
  const b = Math.sqrt((nu + 14 * kok + 120) / nu);
  const ss = 1 / kok;
  const noktalar = [a];
  for (const k of [-8, -5, -3, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 3, 5, 8, 12]) {
    const v = 1 + k * ss;
    if (v > a && v < b) noktalar.push(v);
  }
  noktalar.push(b);
  return noktalar.sort((x, y) => x - y);
}

/**
 * Dunnett dağılımı: P(max |Tᵢ| ≤ c) (ikiYonlu) ya da P(max Tᵢ ≤ c), Tᵢ korelasyonları ρᵢⱼ = λᵢλⱼ olan çok değişkenli t.
 * Çarpım yapısı sayesinde koşullu bağımsızlıkla kesin hesaplanır (Dunnett 1955):
 *   P = E_s ∫ φ(z) Πᵢ [Φ((c·s − λᵢz)/√(1−λᵢ²)) − Φ((−c·s − λᵢz)/√(1−λᵢ²))] dz,  s = √(χ²_ν/ν).
 * İç integral z ∈ [−8.5, 8.5] üzerinde parçalı 16 noktalı Gauss-Legendre (parça genişliği min √(1−λ²)/λ ölçeğine
 * göre seçilir), dış integral s üzerinde parçalı 20 noktalı Gauss-Legendre ile alınır. ν = ∞ normal durumdur.
 * @param {number} c kritik değer
 * @param {number[]} lambdalar λᵢ ∈ [0, 1)
 * @param {number} nu serbestlik derecesi (Infinity olabilir)
 * @param {boolean} [ikiYonlu=true]
 */
export function dunnettOlasilik(c, lambdalar, nu, ikiYonlu = true) {
  if (Number.isNaN(c)) return NaN;
  if (ikiYonlu && c <= 0) return 0;
  if (c === Infinity) return 1;
  if (c === -Infinity) return 0;
  // aynı λ değerlerini birleştir (dengeli tasarımlarda hesap tek terime iner)
  const sayac = new Map();
  for (const l of lambdalar) sayac.set(l, (sayac.get(l) || 0) + 1);
  const L = [...sayac.entries()].map(([l, m]) => ({ l, s: Math.sqrt((1 - l) * (1 + l)), m }));
  let enDar = Infinity;
  for (const { l, s } of L) if (l > 0) enDar = Math.min(enDar, s / l);
  const genislik = Math.min(1, 1.5 * enDar);
  const parca = Math.ceil(17 / genislik);
  const h = 17 / parca;
  const { x: gx, w: gw } = gaussLegendre(16);
  const Z = new Float64Array(parca * 16);
  const W = new Float64Array(parca * 16);
  for (let p = 0; p < parca; p++) {
    const a = -8.5 + p * h;
    for (let i = 0; i < 16; i++) {
      const z = a + (h * (gx[i] + 1)) / 2;
      Z[p * 16 + i] = z;
      W[p * 16 + i] = ((gw[i] * h) / 2) * M_1_SQRT_2PI * Math.exp(-0.5 * z * z);
    }
  }
  const G = (u) => {
    let t = 0;
    for (let i = 0; i < Z.length; i++) {
      const z = Z[i];
      let urun = 1;
      for (const { l, s, m } of L) {
        const lz = l * z;
        const q = ikiYonlu ? phiHizli((u - lz) / s) - phiHizli((-u - lz) / s) : phiHizli((u - lz) / s);
        urun *= m === 1 ? q : q ** m;
        if (urun === 0) break;
      }
      t += W[i] * urun;
    }
    return t;
  };
  if (nu === Infinity || nu > 1e7) return Math.min(1, Math.max(0, G(c)));
  const noktalar = sParcalari(nu);
  const { x: sx, w: sw } = gaussLegendre(20);
  let toplamP = 0;
  for (let i = 0; i < noktalar.length - 1; i++) {
    const a = noktalar[i];
    const b = noktalar[i + 1];
    if (!(b > a)) continue;
    const yari = (b - a) / 2;
    const orta = (a + b) / 2;
    for (let j = 0; j < 20; j++) {
      const s = orta + yari * sx[j];
      toplamP += sw[j] * yari * G(c * s) * Math.exp(logSYogunluk(s, nu));
    }
  }
  return Math.min(1, Math.max(0, toplamP));
}

/** Dunnett kritik değeri: P(c) = duzey çözümü (Bonferroni ve düzeltmesiz t sınırları arasında Brent) */
export function dunnettKritik(duzey, lambdalar, nu, ikiYonlu = true) {
  const m = lambdalar.length;
  const alfa = 1 - duzey;
  const kuyruk = ikiYonlu ? 2 : 1;
  let alt = qt(1 - alfa / kuyruk, nu);
  let ust = qt(1 - alfa / (kuyruk * m), nu);
  if (m === 1) return alt;
  const f = (c) => dunnettOlasilik(c, lambdalar, nu, ikiYonlu) - duzey;
  // sınırlar kuramsal olarak kökü içerir; sayısal güvenlik için genişlet
  alt *= 0.999;
  ust *= 1.001;
  return brent(f, alt, ust, 1e-12);
}

/**
 * Dunnett testi: her deney grubunun kontrol grubuyla karşılaştırılması (SPSS POSTHOC = DUNNETT;
 * R multcomp::glht(aov, mcp(g = "Dunnett")) tek adımlı düzeltme). Havuzlanmış KO_hata, ν = N − k.
 * tᵢ = (x̄ᵢ − x̄₀) / √(KO(1/nᵢ + 1/n₀)); p ve eşzamanlı GA çok değişkenli t dağılımından kesin hesaplanır
 * (bkz. dunnettOlasilik; ρᵢⱼ = λᵢλⱼ, λᵢ = √(nᵢ/(nᵢ + n₀))).
 * alternatif: 'iki' (SPSS "2-sided"), 'buyuk' (SPSS "> Control"), 'kucuk' (SPSS "< Control").
 * Satırlarda i deney grubu, j kontrol grubudur; fark = x̄ᵢ − x̄₀.
 * @param {number[][]} gruplar
 * @param {number} [kontrolIndeksi=0] kontrol grubunun sırası (0 tabanlı)
 * @param {{adlar?:string[], duzey?:number, alternatif?:'iki'|'buyuk'|'kucuk'}} [secenek]
 */
export function dunnett(gruplar, kontrolIndeksi = 0, secenek = {}) {
  const alternatif = secenek.alternatif ?? 'iki';
  if (!['iki', 'buyuk', 'kucuk'].includes(alternatif)) throw new IstatistikHatasi(`Bilinmeyen alternatif hipotez: ${alternatif}`);
  const h = hazirla(gruplar, secenek, 'Dunnett testi');
  if (!Number.isInteger(kontrolIndeksi) || kontrolIndeksi < 0 || kontrolIndeksi >= h.k) throw new IstatistikHatasi('Kontrol grubu geçerli bir grup olmalıdır.');
  const { sdHata, KO } = havuzluHata(gruplar, h.ozet, h.N, h.k, 'Dunnett testi');
  const kontrol = h.ozet[kontrolIndeksi];
  const deney = h.ozet.map((_, i) => i).filter((i) => i !== kontrolIndeksi);
  const lambdalar = deney.map((i) => Math.sqrt(h.ozet[i].n / (h.ozet[i].n + kontrol.n)));
  const ikiYonlu = alternatif === 'iki';
  const kritik = dunnettKritik(h.duzey, lambdalar, sdHata, ikiYonlu);
  const satirlar = deney.map((i) => {
    const a = h.ozet[i];
    const fark = a.ortalama - kontrol.ortalama;
    const sh = Math.sqrt(KO * (1 / a.n + 1 / kontrol.n));
    const t = fark / sh;
    let p;
    if (ikiYonlu) p = 1 - dunnettOlasilik(Math.abs(t), lambdalar, sdHata, true);
    else p = 1 - dunnettOlasilik(alternatif === 'buyuk' ? t : -t, lambdalar, sdHata, false);
    const r = satir(h.ozet, i, kontrolIndeksi, fark, sh, t, sdHata, p, kritik * sh);
    if (alternatif === 'buyuk') r.gaUst = Infinity;
    if (alternatif === 'kucuk') r.gaAlt = -Infinity;
    return r;
  });
  return sonuc('dunnett', 'Dunnett', 't', h, satirlar, { sdHata, KOhata: KO, kritikDeger: kritik, kontrolIndeksi, kontrol: kontrol.ad, alternatif });
}
