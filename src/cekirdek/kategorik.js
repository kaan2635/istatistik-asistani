// Kategorik veri analizi: çapraz tablo, ki-kare (Pearson, Yates, olabilirlik oranı, doğrusal-doğrusal ilişki),
// Fisher kesin testi, McNemar, McNemar-Bowker, Cochran Q, Cochran-Armitage eğilim testi, Mantel-Haenszel
// (Breslow-Day ile), odds oranı / göreli risk / risk farkı, binom testi, oran güven aralıkları, iki oran testi,
// uyum iyiliği ki-kare testi ve SPSS tarzı sütun oranları karşılaştırması.
//
// Tablo yönü (aksi belirtilmedikçe): satırlar = grup / maruziyet, sütunlar = sonuç. 2 × 2 tablolarda
// [[a, b], [c, d]]: a = 1. grupta olay var, b = 1. grupta olay yok, c = 2. grupta olay var, d = 2. grupta olay yok.

import { dhyper, pbinom, dbinom, pchisq, pnorm, qbeta, qnorm, phyper } from './dagilim.js';
import { brent } from './sayisal.js';
import { pDuzelt } from './duzeltme.js';
import { IstatistikHatasi, gerekli } from './hata.js';
import { fisherRxC } from './fisher.js';

// ---------- Yardımcılar ----------

function alternatifAl(a = 'iki-yonlu') {
  const harita = { 'iki-yonlu': 'iki-yonlu', iki: 'iki-yonlu', 'two.sided': 'iki-yonlu', buyuk: 'buyuk', greater: 'buyuk', kucuk: 'kucuk', less: 'kucuk' };
  const s = harita[a];
  if (!s) throw new IstatistikHatasi(`Bilinmeyen alternatif hipotez: ${a}. 'iki-yonlu', 'buyuk' ya da 'kucuk' olmalıdır.`);
  return s;
}

function duzeyKontrol(duzey) {
  gerekli(typeof duzey === 'number' && duzey > 0 && duzey < 1, 'Güven düzeyi 0 ile 1 arasında olmalıdır.');
}

/** Tablo doğrulama: dikdörtgen, negatif olmayan sonlu sayılar (tamsayi: true ise tamsayı) */
function tabloKontrol(tablo, { tamsayi = false, ad = 'Tablo' } = {}) {
  gerekli(Array.isArray(tablo) && tablo.length > 0 && Array.isArray(tablo[0]), `${ad} sayı dizilerinden oluşan bir matris olmalıdır.`);
  const c = tablo[0].length;
  gerekli(c > 0, `${ad} boş olamaz.`);
  for (const s of tablo) {
    gerekli(Array.isArray(s) && s.length === c, `${ad}nun tüm satırları aynı uzunlukta olmalıdır.`);
    for (const v of s) {
      gerekli(typeof v === 'number' && Number.isFinite(v) && v >= 0, `${ad} yalnızca negatif olmayan sayılar içermelidir.`);
      if (tamsayi) gerekli(Number.isInteger(v), `${ad} frekansları tamsayı olmalıdır.`);
    }
  }
}

function toplamlar(t) {
  const satir = t.map((s) => s.reduce((a, b) => a + b, 0));
  const sutun = t[0].map((_, j) => t.reduce((a, s) => a + s[j], 0));
  const N = satir.reduce((a, b) => a + b, 0);
  return { satir, sutun, N };
}

/** Toplamı sıfır olan satır ve sütunları atar */
function bosSatirSutunAt(tablo) {
  const { satir, sutun } = toplamlar(tablo);
  const satirIdx = satir.map((v, i) => (v > 0 ? i : -1)).filter((i) => i >= 0);
  const sutunIdx = sutun.map((v, j) => (v > 0 ? j : -1)).filter((j) => j >= 0);
  const t = satirIdx.map((i) => sutunIdx.map((j) => tablo[i][j]));
  const uyarilar = [];
  if (satirIdx.length < satir.length) uyarilar.push(`Gözlemi olmayan ${satir.length - satirIdx.length} satır analizden çıkarıldı.`);
  if (sutunIdx.length < sutun.length) uyarilar.push(`Gözlemi olmayan ${sutun.length - sutunIdx.length} sütun analizden çıkarıldı.`);
  return { t, satirIdx, sutunIdx, uyarilar };
}

function chiP(x, sd) {
  return pchisq(x, sd, false);
}

function ikiYonluNormalP(z) {
  return Math.min(1, 2 * pnorm(-Math.abs(z)));
}

// ---------- Çapraz tablo ----------

function eksikMi(v) {
  return v === null || v === undefined || v === '' || (typeof v === 'number' && Number.isNaN(v));
}

function duzeyleriSirala(degerler) {
  const sayisal = degerler.every((v) => typeof v === 'number');
  return sayisal ? degerler.sort((a, b) => a - b) : degerler.sort((a, b) => String(a).localeCompare(String(b), 'tr'));
}

/**
 * Etiket dizilerinden çapraz tablo. Eksik değer (null, undefined, '', NaN) içeren çiftler atılır.
 * Düzeyler sayısalsa küçükten büyüğe, değilse Türkçe alfabetik sıralanır; secenek.satirSirasi / sutunSirasi ile
 * sıra verilebilir (listede olmayan değer hata verir; listede olup gözlenmeyen düzey sıfır satır/sütun olarak kalır).
 * @param {Array} satirEtiketleri
 * @param {Array} sutunEtiketleri
 * @param {{satirSirasi?:Array, sutunSirasi?:Array}} secenek
 */
export function caprazTablo(satirEtiketleri, sutunEtiketleri, secenek = {}) {
  gerekli(Array.isArray(satirEtiketleri) && Array.isArray(sutunEtiketleri), 'Satır ve sütun etiketleri dizi olmalıdır.');
  gerekli(satirEtiketleri.length === sutunEtiketleri.length, 'Satır ve sütun değişkenlerinin uzunlukları aynı olmalıdır.');
  const ciftler = [];
  let eksik = 0;
  for (let i = 0; i < satirEtiketleri.length; i++) {
    const a = satirEtiketleri[i];
    const b = sutunEtiketleri[i];
    if (eksikMi(a) || eksikMi(b)) eksik++;
    else ciftler.push([a, b]);
  }
  gerekli(ciftler.length > 0, 'Çapraz tablo için geçerli gözlem yok.');
  const satirlar = secenek.satirSirasi ? secenek.satirSirasi.slice() : duzeyleriSirala([...new Set(ciftler.map((c) => c[0]))]);
  const sutunlar = secenek.sutunSirasi ? secenek.sutunSirasi.slice() : duzeyleriSirala([...new Set(ciftler.map((c) => c[1]))]);
  const si = new Map(satirlar.map((v, i) => [v, i]));
  const sj = new Map(sutunlar.map((v, j) => [v, j]));
  const tablo = satirlar.map(() => new Array(sutunlar.length).fill(0));
  for (const [a, b] of ciftler) {
    const i = si.get(a);
    const j = sj.get(b);
    if (i === undefined) throw new IstatistikHatasi(`Satır düzeyi sıralamada yok: ${a}`);
    if (j === undefined) throw new IstatistikHatasi(`Sütun düzeyi sıralamada yok: ${b}`);
    tablo[i][j]++;
  }
  const { satir, sutun, N } = toplamlar(tablo);
  return { tablo, satirlar, sutunlar, satirToplam: satir, sutunToplam: sutun, N, eksik };
}

// ---------- Ki-kare bağımsızlık testi ----------

/**
 * Pearson ki-kare bağımsızlık testi ve SPSS "Chi-Square Tests" / "Symmetric Measures" tablolarındaki değerler.
 *
 * - Pearson χ² (R chisq.test(correct = FALSE)), sd = (r-1)(c-1)
 * - Süreklilik düzeltmeli (Yates) χ², yalnız 2 × 2 (R chisq.test(correct = TRUE): |O - E| - min(0.5, |O - E|))
 * - Olabilirlik oranı G² = 2 Σ O ln(O/E)
 * - Doğrusal-doğrusal ilişki M² = (N - 1) r² (satır ve sütun puanları 1, 2, ...; SPSS "Linear-by-Linear Association")
 * - Beklenen değerler, beklenen < 5 hücre sayısı/yüzdesi, en küçük beklenen
 * - Standart (O - E)/√E ve düzeltilmiş standart artıklar (O - E)/√[E(1 - r_i/N)(1 - c_j/N)] (R chisq.test()$stdres)
 *   ve iki yönlü p değerleri
 * - Phi (2 × 2'de işaretli (ad - bc)/√(r1 r2 c1 c2), diğerlerinde √(χ²/N)), Cramér V, kontenjans katsayısı
 * - 2 × 2 tablolarda Fisher kesin testi p değerleri (SPSS gibi)
 * Toplamı sıfır olan satır/sütunlar çıkarılır (uyarı verilir).
 *
 * @param {number[][]} tablo frekans tablosu
 * @param {{yates?:boolean, satirSkorlari?:number[], sutunSkorlari?:number[]}} secenek
 *   yates: true ise 2 × 2'de ana istatistik süreklilik düzeltmeli χ² olur (varsayılan SPSS gibi Pearson).
 */
export function kiKare(tablo, secenek = {}) {
  const { yates = false } = secenek;
  tabloKontrol(tablo);
  const { t, satirIdx, sutunIdx, uyarilar } = bosSatirSutunAt(tablo);
  const r = t.length;
  const c = r ? t[0].length : 0;
  if (r < 2 || c < 2) throw new IstatistikHatasi('Ki-kare testi için gözlemi olan en az 2 satır ve 2 sütun gerekir.');
  const { satir, sutun, N } = toplamlar(t);
  const E = t.map((_, i) => sutun.map((cj) => (satir[i] * cj) / N));
  let X2 = 0;
  let G2 = 0;
  let kucuk5 = 0;
  let minE = Infinity;
  const standart = [];
  const duzeltilmis = [];
  const artikP = [];
  for (let i = 0; i < r; i++) {
    standart.push([]);
    duzeltilmis.push([]);
    artikP.push([]);
    for (let j = 0; j < c; j++) {
      const o = t[i][j];
      const e = E[i][j];
      X2 += ((o - e) * (o - e)) / e;
      if (o > 0) G2 += 2 * o * Math.log(o / e);
      if (e < 5) kucuk5++;
      if (e < minE) minE = e;
      standart[i].push((o - e) / Math.sqrt(e));
      const z = (o - e) / Math.sqrt(e * (1 - satir[i] / N) * (1 - sutun[j] / N));
      duzeltilmis[i].push(z);
      artikP[i].push(ikiYonluNormalP(z));
    }
  }
  const sd = (r - 1) * (c - 1);
  const pearson = { istatistik: X2, sd, p: chiP(X2, sd) };
  const olabilirlikOrani = { istatistik: G2, sd, p: chiP(G2, sd) };

  // Doğrusal-doğrusal ilişki (puanlar: varsayılan 1..r, 1..c; verilen puanlar özgün satır/sütun sırasına göredir)
  const u = secenek.satirSkorlari ? satirIdx.map((i) => secenek.satirSkorlari[i]) : satirIdx.map((_, i) => i + 1);
  const v = secenek.sutunSkorlari ? sutunIdx.map((j) => secenek.sutunSkorlari[j]) : sutunIdx.map((_, j) => j + 1);
  let um = 0;
  let vm = 0;
  for (let i = 0; i < r; i++) um += satir[i] * u[i];
  for (let j = 0; j < c; j++) vm += sutun[j] * v[j];
  um /= N;
  vm /= N;
  let suu = 0;
  let svv = 0;
  let suv = 0;
  for (let i = 0; i < r; i++) suu += satir[i] * (u[i] - um) ** 2;
  for (let j = 0; j < c; j++) svv += sutun[j] * (v[j] - vm) ** 2;
  for (let i = 0; i < r; i++) for (let j = 0; j < c; j++) suv += t[i][j] * (u[i] - um) * (v[j] - vm);
  const rKor = suv / Math.sqrt(suu * svv);
  const M2 = (N - 1) * rKor * rKor;
  const dogrusalIliski = { istatistik: M2, sd: 1, p: chiP(M2, 1), r: rKor };

  let yatesSonuc = null;
  let phi;
  let fisher = null;
  if (r === 2 && c === 2) {
    let minFark = 0.5;
    for (let i = 0; i < 2; i++) for (let j = 0; j < 2; j++) minFark = Math.min(minFark, Math.abs(t[i][j] - E[i][j]));
    let Y = 0;
    for (let i = 0; i < 2; i++) for (let j = 0; j < 2; j++) Y += (Math.abs(t[i][j] - E[i][j]) - minFark) ** 2 / E[i][j];
    yatesSonuc = { istatistik: Y, sd: 1, p: chiP(Y, 1) };
    const [[a, b], [cc, d]] = t;
    phi = (a * d - b * cc) / Math.sqrt(satir[0] * satir[1] * sutun[0] * sutun[1]);
    if (t.every((s) => s.every((x) => Number.isInteger(x)))) {
      const f = fisher2x2(t, 0.95, 'iki-yonlu', false);
      fisher = { p: f.p, pTekYonlu: f.pTekYonlu, pKucuk: f.pKucuk, pBuyuk: f.pBuyuk };
    }
  } else phi = Math.sqrt(X2 / N);
  const cramerV = Math.sqrt(X2 / (N * (Math.min(r, c) - 1)));
  const kontenjans = Math.sqrt(X2 / (X2 + N));
  const yuzde = (100 * kucuk5) / (r * c);
  if (kucuk5 > 0 && yuzde > 20) {
    uyarilar.push(`Beklenen değeri 5'ten küçük hücre oranı %${yuzde.toFixed(1)} (en küçük beklenen ${minE.toFixed(2)}); ki-kare yaklaşımı güvenilir olmayabilir, Fisher kesin testi önerilir.`);
  } else if (minE < 1) uyarilar.push(`En küçük beklenen değer ${minE.toFixed(2)} (< 1); ki-kare yaklaşımı güvenilir olmayabilir.`);
  const anaYates = yates && yatesSonuc;
  const ana = anaYates ? yatesSonuc : pearson;
  return {
    yontem: anaYates ? 'yates-ki-kare' : 'pearson-ki-kare',
    ad: anaYates ? 'Süreklilik düzeltmeli (Yates) ki-kare testi' : 'Pearson ki-kare bağımsızlık testi',
    istatistik: ana.istatistik,
    istatistikAdi: 'χ²',
    sd: ana.sd,
    p: ana.p,
    pearson,
    yates: yatesSonuc,
    olabilirlikOrani,
    dogrusalIliski,
    fisher,
    gozlenen: t,
    beklenen: E,
    satirToplam: satir,
    sutunToplam: sutun,
    satirIndeksleri: satirIdx,
    sutunIndeksleri: sutunIdx,
    kalintilar: { standart, duzeltilmis, p: artikP },
    beklenenKucuk5: { sayi: kucuk5, yuzde, minimum: minE },
    etki: r === 2 && c === 2 ? { ad: 'Phi', sembol: 'φ', deger: phi } : { ad: 'Cramér V', sembol: 'V', deger: cramerV },
    etkiler: { phi, cramerV, kontenjansKatsayisi: kontenjans },
    n: N,
    uyarilar,
  };
}

// ---------- Fisher kesin testi ----------

/** Koşullu (merkezi olmayan hipergeometrik) dağılım yardımcıları — R fisher.test ile aynı tanımlar */
function kosulluDagilim(x, m, n, k) {
  const lo = Math.max(0, k - n);
  const hi = Math.min(k, m);
  const destek = [];
  const logdc = [];
  for (let s = lo; s <= hi; s++) {
    destek.push(s);
    logdc.push(dhyper(s, m, n, k, true));
  }
  // u = log(ψ) için normalleştirilmiş olasılıklar
  const dn = (u) => {
    const d = logdc.map((l, i) => l + u * destek[i]);
    let mx = -Infinity;
    for (const v of d) if (v > mx) mx = v;
    let s = 0;
    const e = d.map((v) => {
      const z = Math.exp(v - mx);
      s += z;
      return z;
    });
    return e.map((z) => z / s);
  };
  const ortalama = (u) => {
    const d = dn(u);
    let s = 0;
    for (let i = 0; i < d.length; i++) s += destek[i] * d[i];
    return s;
  };
  // P(X <= q) ya da P(X >= q)
  const olasilik = (q, u, ust) => {
    if (u === 0) return ust ? phyper(q - 1, m, n, k, false) : phyper(q, m, n, k);
    const d = dn(u);
    let s = 0;
    for (let i = 0; i < d.length; i++) if (ust ? destek[i] >= q : destek[i] <= q) s += d[i];
    return s;
  };
  return { lo, hi, destek, logdc, dn, ortalama, olasilik };
}

/** Artan g(u) için g(u) = hedef çözümü (u ∈ ℝ, aralık genişletilerek), yüksek hassasiyetle */
function logKok(g, hedef) {
  let a = -1;
  let b = 1;
  let fa = g(a) - hedef;
  let fb = g(b) - hedef;
  let i = 0;
  while (fa > 0 && i++ < 200) {
    b = a;
    fb = fa;
    a *= 2;
    fa = g(a) - hedef;
  }
  i = 0;
  while (fb < 0 && i++ < 200) {
    a = b;
    fa = fb;
    b *= 2;
    fb = g(b) - hedef;
  }
  if (fa > 0 || fb < 0) return fa > 0 ? -Infinity : Infinity;
  return brent((u) => g(u) - hedef, a, b, 1e-15);
}

function fisher2x2(t, duzey, alt, gaHesapla = true) {
  const x = t[0][0];
  const m = t[0][0] + t[1][0];
  const n = t[0][1] + t[1][1];
  const k = t[0][0] + t[0][1];
  const K = kosulluDagilim(x, m, n, k);
  const { lo, hi } = K;
  const pKucuk = K.olasilik(x, 0, false);
  const pBuyuk = K.olasilik(x, 0, true);
  // İki yönlü: olasılığı gözlenenin (1 + 1e-7) katından küçük ya da eşit tabloların toplamı
  const d = K.dn(0);
  const dx = d[x - lo] * (1 + 1e-7);
  let pIki = 0;
  for (const v of d) if (v <= dx) pIki += v;
  pIki = Math.min(1, pIki);
  const p = alt === 'kucuk' ? pKucuk : alt === 'buyuk' ? pBuyuk : pIki;
  const sonuc = { p, pIkiYonlu: pIki, pKucuk, pBuyuk, pTekYonlu: Math.min(pKucuk, pBuyuk) };
  if (!gaHesapla) return sonuc;
  // Koşullu en çok olabilirlik odds oranı
  let tahmin;
  if (x === lo) tahmin = 0;
  else if (x === hi) tahmin = Infinity;
  else tahmin = Math.exp(logKok(K.ortalama, x));
  const ncpAlt = (a) => (x === lo ? 0 : Math.exp(logKok((u) => K.olasilik(x, u, true), a)));
  const ncpUst = (a) => (x === hi ? Infinity : Math.exp(logKok((u) => -K.olasilik(x, u, false), -a)));
  let ga;
  if (alt === 'kucuk') ga = [0, ncpUst(1 - duzey)];
  else if (alt === 'buyuk') ga = [ncpAlt(1 - duzey), Infinity];
  else {
    const a = (1 - duzey) / 2;
    ga = [ncpAlt(a), ncpUst(a)];
  }
  sonuc.oddsOrani = { tahmin, ga: { duzey, alt: ga[0], ust: ga[1], neyin: 'koşullu en çok olabilirlik odds oranı' } };
  return sonuc;
}

/**
 * Fisher kesin testi.
 * 2 × 2: R fisher.test ile aynı: iki yönlü p = olasılığı gözlenen tablonunkinin (1 + 1e-7) katını aşmayan tabloların
 * toplamı; tek yönlü p'ler (pKucuk: P(X <= a), pBuyuk: P(X >= a); SPSS "Exact Sig. (1-sided)" = pTekYonlu = küçüğü),
 * koşullu en çok olabilirlik odds oranı ve kesin güven aralığı (merkezi olmayan hipergeometrik dağılımın kökleri;
 * R uniroot'u varsayılan toleransla (~1e-4) çağırdığından R'nin değerleri bizimkinden ~1e-4 göreli farklı olabilir).
 * r × c: Fisher-Freeman-Halton kesin p (fisher.js ağ algoritması; bütçe aşılırsa Monte Carlo, pYontemi bildirilir).
 *
 * @param {number[][]} tablo tamsayı frekanslar
 * @param {{duzey?:number, alternatif?:string, yontem?:string, B?:number, tohum?:number, zamanSiniri?:number}} secenek
 */
export function fisherKesin(tablo, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  const alt = alternatifAl(secenek.alternatif);
  duzeyKontrol(duzey);
  tabloKontrol(tablo, { tamsayi: true });
  const r = tablo.length;
  const c = tablo[0].length;
  gerekli(r >= 2 && c >= 2, 'Fisher kesin testi için en az 2 satır ve 2 sütun gerekir.');
  const { N } = toplamlar(tablo);
  gerekli(N > 0, 'Tabloda gözlem yok.');
  if (r === 2 && c === 2) {
    const uyarilar = [];
    const { satir, sutun } = toplamlar(tablo);
    if (satir.some((v) => v === 0) || sutun.some((v) => v === 0)) {
      uyarilar.push('Bir satır ya da sütunun toplamı sıfır; tablo bilgi taşımadığından p = 1.');
      return { yontem: 'fisher-kesin', ad: 'Fisher kesin testi', istatistik: null, istatistikAdi: null, sd: null, p: 1, pYontemi: 'kesin', pIkiYonlu: 1, pKucuk: 1, pBuyuk: 1, pTekYonlu: 1, oddsOrani: { tahmin: NaN, ga: { duzey, alt: NaN, ust: NaN } }, n: N, uyarilar };
    }
    const f = fisher2x2(tablo, duzey, alt);
    return {
      yontem: 'fisher-kesin',
      ad: 'Fisher kesin testi',
      istatistik: null,
      istatistikAdi: null,
      sd: null,
      ...f,
      pYontemi: 'kesin',
      alternatif: alt,
      ga: f.oddsOrani.ga,
      n: N,
      uyarilar,
    };
  }
  gerekli(alt === 'iki-yonlu', 'r × c tablolarda Fisher testi yalnızca iki yönlüdür.');
  const f = fisherRxC(tablo, secenek);
  return {
    yontem: 'fisher-freeman-halton',
    ad: 'Fisher-Freeman-Halton kesin testi',
    istatistik: null,
    istatistikAdi: null,
    sd: null,
    p: f.p,
    pYontemi: f.pYontemi,
    sh: f.sh ?? null,
    B: f.B ?? null,
    tohum: f.tohum ?? null,
    sure: f.sure,
    n: N,
    uyarilar: f.uyarilar,
  };
}

// ---------- McNemar ----------

/**
 * McNemar testi (eşleştirilmiş 2 × 2 tablo: satır = 1. ölçüm, sütun = 2. ölçüm). Uyumsuz çiftler b = t[0][1], c = t[1][0].
 * yontem: 'kesin' → iki yönlü binom p = min(1, 2·P(X <= min(b, c))), X ~ Binom(b + c, 1/2) (SPSS);
 *         'asimptotik' → χ² = (|b - c| - 1)²/(b + c) (sureklilik: true, R mcnemar.test varsayılanı) ya da (b - c)²/(b + c);
 *         'otomatik' → b + c < 25 ise kesin, değilse asimptotik.
 * Her durumda pKesin ve kiKare alanları döner.
 * @param {number[][]} tablo
 * @param {{yontem?:string, sureklilik?:boolean}} secenek
 */
export function mcnemar(tablo, secenek = {}) {
  const { yontem = 'otomatik', sureklilik = true } = secenek;
  tabloKontrol(tablo, { tamsayi: true });
  gerekli(tablo.length === 2 && tablo[0].length === 2, 'McNemar testi 2 × 2 tablo gerektirir (k × k için McNemar-Bowker kullanın).');
  gerekli(['otomatik', 'kesin', 'asimptotik'].includes(yontem), `Bilinmeyen yöntem: ${yontem}`);
  const b = tablo[0][1];
  const c = tablo[1][0];
  const n = b + c;
  const uyarilar = [];
  const pKesin = n === 0 ? 1 : Math.min(1, 2 * pbinom(Math.min(b, c), n, 0.5));
  let kiKareSonuc = null;
  if (n > 0) {
    const duz = sureklilik && b !== c ? 1 : 0;
    const X2 = (Math.abs(b - c) - duz) ** 2 / n;
    kiKareSonuc = { istatistik: X2, sd: 1, p: chiP(X2, 1), sureklilik: duz === 1 };
  } else uyarilar.push('Uyumsuz çift yok (b + c = 0); ki-kare istatistiği hesaplanamaz, p = 1.');
  const kesin = yontem === 'kesin' || (yontem === 'otomatik' && n < 25) || n === 0;
  const N = tablo[0][0] + tablo[0][1] + tablo[1][0] + tablo[1][1];
  return {
    yontem: 'mcnemar',
    ad: 'McNemar testi',
    istatistik: kesin ? Math.min(b, c) : kiKareSonuc.istatistik,
    istatistikAdi: kesin ? 'b' : 'χ²',
    sd: kesin ? null : 1,
    p: kesin ? pKesin : kiKareSonuc.p,
    pYontemi: kesin ? 'kesin' : 'asimptotik',
    b,
    c,
    pKesin,
    kiKare: kiKareSonuc,
    oranlar: { olcum1: (tablo[0][0] + tablo[0][1]) / N, olcum2: (tablo[0][0] + tablo[1][0]) / N },
    n: N,
    uyarilar,
  };
}

/**
 * McNemar-Bowker simetri testi (k × k). χ² = Σ_{i<j} (n_ij - n_ji)²/(n_ij + n_ji).
 * n_ij + n_ji = 0 olan çiftler bilgi taşımadığından toplamdan ve serbestlik derecesinden çıkarılır (uyarı verilir);
 * tüm çiftler doluysa sd = k(k-1)/2 ve sonuç R mcnemar.test ile aynıdır (R boş çiftte NaN verir).
 * @param {number[][]} tablo kare tablo
 */
export function mcnemarBowker(tablo) {
  tabloKontrol(tablo, { tamsayi: true });
  const k = tablo.length;
  gerekli(k >= 2 && tablo.every((s) => s.length === k), 'McNemar-Bowker testi kare (k × k) tablo gerektirir.');
  let X2 = 0;
  let sd = 0;
  let bos = 0;
  for (let i = 0; i < k; i++) {
    for (let j = i + 1; j < k; j++) {
      const s = tablo[i][j] + tablo[j][i];
      if (s === 0) {
        bos++;
        continue;
      }
      X2 += (tablo[i][j] - tablo[j][i]) ** 2 / s;
      sd++;
    }
  }
  if (sd === 0) throw new IstatistikHatasi('Köşegen dışı hücrelerin tümü boş; simetri testi hesaplanamaz.');
  const uyarilar = bos > 0 ? [`${bos} hücre çiftinde (n_ij + n_ji = 0) gözlem olmadığından serbestlik derecesi ${sd} alındı.`] : [];
  return {
    yontem: 'mcnemar-bowker',
    ad: 'McNemar-Bowker simetri testi',
    istatistik: X2,
    istatistikAdi: 'χ²',
    sd,
    p: chiP(X2, sd),
    n: tablo.flat().reduce((a, b) => a + b, 0),
    uyarilar,
  };
}

// ---------- Cochran Q ----------

/**
 * Cochran Q testi (n denek × k ikili ölçüm, değerler 0/1).
 * Q = (k - 1)[k Σ C_j² - T²] / (k T - Σ R_i²), sd = k - 1 (SPSS ve R rstatix::cochran_qtest ile aynı).
 * Etki: η²_Q = Q / (n(k - 1)) (Serlin vd. 1982).
 * @param {number[][]} matris
 */
export function cochranQ(matris) {
  gerekli(Array.isArray(matris) && matris.length > 0, 'Cochran Q testi için veri matrisi boş olamaz.');
  const k = matris[0].length;
  gerekli(k >= 2, 'Cochran Q testi için en az 2 ölçüm gerekir.');
  for (const s of matris) {
    gerekli(Array.isArray(s) && s.length === k, 'Tüm satırlarda ölçüm sayısı aynı olmalıdır.');
    for (const v of s) gerekli(v === 0 || v === 1, 'Cochran Q testi için değerler 0 ya da 1 olmalıdır.');
  }
  const n = matris.length;
  const C = new Array(k).fill(0);
  let T = 0;
  let RR = 0;
  for (const s of matris) {
    let R = 0;
    for (let j = 0; j < k; j++) {
      C[j] += s[j];
      R += s[j];
    }
    T += R;
    RR += R * R;
  }
  const payda = k * T - RR;
  if (!(payda > 0)) throw new IstatistikHatasi('Tüm deneklerde ölçümler aynı; Cochran Q testi hesaplanamaz.');
  let CC = 0;
  for (const cj of C) CC += cj * cj;
  const Q = ((k - 1) * (k * CC - T * T)) / payda;
  return {
    yontem: 'cochran-q',
    ad: 'Cochran Q testi',
    istatistik: Q,
    istatistikAdi: 'Q',
    sd: k - 1,
    p: chiP(Q, k - 1),
    basariSayilari: C,
    oranlar: C.map((cj) => cj / n),
    n,
    etki: { ad: 'eta kare (Q)', sembol: 'η²', deger: Q / (n * (k - 1)) },
    uyarilar: [],
  };
}

// ---------- Cochran-Armitage eğilim testi ----------

/**
 * Cochran-Armitage oranlarda eğilim testi (R prop.trend.test ile aynı χ²).
 * tablo 2 × k: 1. satır olay sayıları (x_i), 2. satır olay olmayanlar; sütunlar sıralı gruplar.
 * χ² = [Σ x_i (s_i - s̄)]² / [p̄(1 - p̄) Σ n_i (s_i - s̄)²], z = işaretli kök (pozitif: puanla artan oran).
 * @param {number[][]} tablo
 * @param {number[]} [skorlar] varsayılan 1..k
 */
export function cochranArmitage(tablo, skorlar) {
  tabloKontrol(tablo);
  gerekli(tablo.length === 2 && tablo[0].length >= 2, 'Cochran-Armitage testi 2 × k (k >= 2) tablo gerektirir: 1. satır olay var, 2. satır olay yok.');
  const k = tablo[0].length;
  const s = skorlar ?? Array.from({ length: k }, (_, i) => i + 1);
  gerekli(s.length === k && s.every((v) => Number.isFinite(v)), 'Skor sayısı sütun sayısına eşit olmalıdır.');
  const x = tablo[0];
  const n = x.map((v, i) => v + tablo[1][i]);
  gerekli(n.every((v) => v > 0), 'Her grupta en az bir gözlem olmalıdır.');
  const N = n.reduce((a, b) => a + b, 0);
  const X = x.reduce((a, b) => a + b, 0);
  const pbar = X / N;
  if (!(pbar > 0 && pbar < 1)) throw new IstatistikHatasi('Tüm gözlemler aynı kategoride; eğilim testi hesaplanamaz.');
  let sbar = 0;
  for (let i = 0; i < k; i++) sbar += n[i] * s[i];
  sbar /= N;
  let T = 0;
  let V = 0;
  for (let i = 0; i < k; i++) {
    T += x[i] * (s[i] - sbar);
    V += n[i] * (s[i] - sbar) ** 2;
  }
  V *= pbar * (1 - pbar);
  if (!(V > 0)) throw new IstatistikHatasi('Skorlarda değişkenlik yok; eğilim testi hesaplanamaz.');
  const z = T / Math.sqrt(V);
  const X2 = (T * T) / V;
  return {
    yontem: 'cochran-armitage',
    ad: 'Cochran-Armitage eğilim testi',
    istatistik: X2,
    istatistikAdi: 'χ²',
    sd: 1,
    p: chiP(X2, 1),
    z,
    pTekYonluArtan: pnorm(z, 0, 1, false),
    pTekYonluAzalan: pnorm(z),
    oranlar: x.map((v, i) => v / n[i]),
    skorlar: s,
    n: N,
    uyarilar: [],
  };
}

// ---------- Mantel-Haenszel ----------

/** Ortak odds oranı ψ altında 2 × 2 tablonun a hücresinin beklenen değeri (Breslow-Day) */
function bdBeklenen(n1, m1, T, psi) {
  const lo = Math.max(0, n1 + m1 - T);
  const hi = Math.min(n1, m1);
  if (psi === 1) return (n1 * m1) / T;
  // (1 - ψ) E² + (T - n1 - m1 + ψ(n1 + m1)) E - ψ n1 m1 = 0, kök [lo, hi] içinde
  const A = 1 - psi;
  const B = T - n1 - m1 + psi * (n1 + m1);
  const Cc = -psi * n1 * m1;
  const disk = Math.sqrt(Math.max(0, B * B - 4 * A * Cc));
  // Sayısal kararlı kökler
  const q = -0.5 * (B + Math.sign(B || 1) * disk);
  const k1 = q / A;
  const k2 = Cc / q;
  const icinde = (e) => e >= lo - 1e-9 && e <= hi + 1e-9;
  if (icinde(k1) && !icinde(k2)) return k1;
  if (icinde(k2) && !icinde(k1)) return k2;
  // Belirsizse doğrudan kök bul
  const g = (e) => e * (T - n1 - m1 + e) - psi * (n1 - e) * (m1 - e);
  return brent(g, lo, hi, 1e-15);
}

/**
 * Mantel-Haenszel testi (2 × 2 × K katmanlı tablolar).
 * - MH χ² = (|Σa - ΣE(a)| - 0.5)² / Σ Var(a), Var = n1 n2 m1 m2 / (T²(T - 1)); sureklilik: false ise 0.5 çıkarılmaz
 *   (R mantelhaen.test(correct = sureklilik); SPSS "Mantel-Haenszel" satırı süreklilik düzeltmelidir).
 * - Cochran χ² (SPSS "Cochran's"): düzeltmesiz, Var = n1 n2 m1 m2 / T³.
 * - Ortak odds oranı (MH) ve Robins-Breslow-Greenland güven aralığı (R ile aynı).
 * - Breslow-Day homojenlik testi ve Tarone düzeltmesi (sd = bilgi taşıyan katman sayısı - 1).
 * @param {number[][][]} tablolar her biri [[a, b], [c, d]]
 * @param {{sureklilik?:boolean, duzey?:number}} secenek
 */
export function mantelHaenszel(tablolar, secenek = {}) {
  const { sureklilik = true, duzey = 0.95 } = secenek;
  duzeyKontrol(duzey);
  gerekli(Array.isArray(tablolar) && tablolar.length >= 2, 'Mantel-Haenszel testi için en az 2 katman (2 × 2 tablo) gerekir.');
  const katmanlar = [];
  for (const [i, t] of tablolar.entries()) {
    tabloKontrol(t, { ad: `${i + 1}. katman` });
    gerekli(t.length === 2 && t[0].length === 2, `${i + 1}. katman 2 × 2 olmalıdır.`);
    const [[a, b], [c, d]] = t;
    const T = a + b + c + d;
    gerekli(T > 1, 'Her katmanda örneklem büyüklüğü 1\'den büyük olmalıdır.');
    katmanlar.push({ a, b, c, d, T, n1: a + b, n2: c + d, m1: a + c, m2: b + d });
  }
  const uyarilar = [];
  let sa = 0;
  let sE = 0;
  let sV = 0;
  let sVc = 0;
  let sDiag = 0;
  let sOffd = 0;
  for (const k of katmanlar) {
    sa += k.a;
    sE += (k.n1 * k.m1) / k.T;
    sV += (k.n1 * k.n2 * k.m1 * k.m2) / (k.T * k.T * (k.T - 1));
    sVc += (k.n1 * k.n2 * k.m1 * k.m2) / (k.T * k.T * k.T);
    sDiag += (k.a * k.d) / k.T;
    sOffd += (k.b * k.c) / k.T;
  }
  const delta = sa - sE;
  if (!(sV > 0)) throw new IstatistikHatasi('Katmanlarda varyans sıfır; Mantel-Haenszel testi hesaplanamaz.');
  const yates = sureklilik && Math.abs(delta) >= 0.5 ? 0.5 : 0;
  const MH = (Math.abs(delta) - yates) ** 2 / sV;
  const cochran = (delta * delta) / sVc;
  const or = sDiag / sOffd;
  // Robins-Breslow-Greenland
  let s1 = 0;
  let s2 = 0;
  let s3 = 0;
  for (const k of katmanlar) {
    const n2 = k.T * k.T;
    s1 += ((k.a + k.d) * k.a * k.d) / n2;
    s2 += ((k.a + k.d) * k.b * k.c + (k.b + k.c) * k.a * k.d) / n2;
    s3 += ((k.b + k.c) * k.b * k.c) / n2;
  }
  const logSH = Math.sqrt(s1 / (2 * sDiag * sDiag) + s2 / (2 * sDiag * sOffd) + s3 / (2 * sOffd * sOffd));
  const z = qnorm(1 - (1 - duzey) / 2);
  const ga = { duzey, alt: or * Math.exp(-z * logSH), ust: or * Math.exp(z * logSH), neyin: 'Mantel-Haenszel ortak odds oranı' };

  // Breslow-Day
  let BD = 0;
  let sEbd = 0;
  let sVbd = 0;
  let sAbd = 0;
  let gecerli = 0;
  if (Number.isFinite(or) && or > 0) {
    for (const k of katmanlar) {
      // a hücresinin alabileceği tek değer varsa (bir satır/sütun toplamı sıfır) katman bilgi taşımaz
      if (Math.max(0, k.n1 + k.m1 - k.T) === Math.min(k.n1, k.m1)) continue;
      const E = bdBeklenen(k.n1, k.m1, k.T, or);
      const hucreler = [E, k.n1 - E, k.m1 - E, k.T - k.n1 - k.m1 + E];
      if (hucreler.some((h) => !(h > 1e-12))) continue;
      const V = 1 / hucreler.reduce((s, h) => s + 1 / h, 0);
      BD += (k.a - E) ** 2 / V;
      sEbd += E;
      sVbd += V;
      sAbd += k.a;
      gecerli++;
    }
  }
  let breslowDay = null;
  let tarone = null;
  if (gecerli >= 2) {
    if (gecerli < katmanlar.length) {
      uyarilar.push(`${katmanlar.length - gecerli} katman bilgi taşımadığından (satır ya da sütun toplamı sıfır) Breslow-Day testinden çıkarıldı.`);
    }
    const sdBD = gecerli - 1;
    breslowDay = { istatistik: BD, sd: sdBD, p: chiP(BD, sdBD) };
    const tar = BD - (sAbd - sEbd) ** 2 / sVbd;
    tarone = { istatistik: tar, sd: sdBD, p: chiP(tar, sdBD) };
  } else uyarilar.push('Breslow-Day homojenlik testi hesaplanamadı (ortak odds oranı tanımsız ya da yetersiz katman).');
  return {
    yontem: 'mantel-haenszel',
    ad: 'Mantel-Haenszel testi',
    istatistik: MH,
    istatistikAdi: 'χ²',
    sd: 1,
    p: chiP(MH, 1),
    sureklilik: yates > 0,
    cochran: { istatistik: cochran, sd: 1, p: chiP(cochran, 1) },
    ortakOddsOrani: { tahmin: or, logSH, ga },
    ga,
    breslowDay,
    tarone,
    katmanlar: katmanlar.map((k) => ({ oddsOrani: (k.a * k.d) / (k.b * k.c), n: k.T })),
    n: katmanlar.reduce((s, k) => s + k.T, 0),
    uyarilar,
  };
}

// ---------- Odds oranı, göreli risk, risk farkı ----------

function wilsonAraligi(x, n, z) {
  const p = x / n;
  const z22n = (z * z) / (2 * n);
  const kok = z * Math.sqrt((p * (1 - p)) / n + z22n / (2 * n));
  return [(p + z22n - kok) / (1 + 2 * z22n), (p + z22n + kok) / (1 + 2 * z22n)];
}

/**
 * 2 × 2 tabloda risk ölçüleri. Tablo [[a, b], [c, d]]: 1. satır = maruz kalan / 1. grup, 2. satır = maruz kalmayan /
 * karşılaştırma grubu; 1. sütun = olay var, 2. sütun = olay yok.
 * - Odds oranı ad/bc, Woolf (logit) güven aralığı. Sıfır hücre varsa tüm hücrelere 0.5 eklenir (Haldane-Anscombe; uyarı).
 * - Göreli risk [a/(a+b)]/[c/(c+d)], log yöntemiyle güven aralığı (a ya da c sıfırsa 0.5 düzeltmesi; uyarı).
 *   SPSS "For cohort" satırları: goreliRisk (1. sütun için) ve goreliRiskSutun2 (2. sütun için).
 * - Risk farkı p1 - p2: Wald ve Newcombe (hibrit skor, Wilson aralıklarıyla; Newcombe 1998 yöntem 10) güven aralıkları.
 * @param {number[][]} tablo
 * @param {{duzey?:number}} secenek
 */
export function oddsOrani(tablo, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  duzeyKontrol(duzey);
  tabloKontrol(tablo);
  gerekli(tablo.length === 2 && tablo[0].length === 2, 'Odds oranı 2 × 2 tablo gerektirir.');
  const [[a, b], [c, d]] = tablo;
  const n1 = a + b;
  const n2 = c + d;
  gerekli(n1 > 0 && n2 > 0, 'Her iki satırda da gözlem olmalıdır.');
  const z = qnorm(1 - (1 - duzey) / 2);
  const uyarilar = [];
  // Odds oranı
  const sifirVar = a === 0 || b === 0 || c === 0 || d === 0;
  const [A, B, C, D] = sifirVar ? [a + 0.5, b + 0.5, c + 0.5, d + 0.5] : [a, b, c, d];
  if (sifirVar) uyarilar.push('Tabloda sıfır hücre olduğundan odds oranı ve güven aralığı tüm hücrelere 0.5 eklenerek hesaplandı (Haldane-Anscombe düzeltmesi).');
  const or = (A * D) / (B * C);
  const orSH = Math.sqrt(1 / A + 1 / B + 1 / C + 1 / D);
  const oddsOraniSonuc = {
    tahmin: or,
    logSH: orSH,
    ga: { duzey, alt: or * Math.exp(-z * orSH), ust: or * Math.exp(z * orSH), neyin: 'odds oranı (Woolf)' },
    duzeltme: sifirVar,
  };
  // Göreli risk
  const rr = (A2, B2, C2, D2, ad) => {
    const duz = A2 === 0 || C2 === 0;
    if (duz) {
      A2 += 0.5;
      B2 += 0.5;
      C2 += 0.5;
      D2 += 0.5;
      uyarilar.push(`${ad} için olay sayısı sıfır olan satır bulunduğundan tüm hücrelere 0.5 eklendi.`);
    }
    const r1 = A2 / (A2 + B2);
    const r2 = C2 / (C2 + D2);
    const tahmin = r1 / r2;
    const sh = Math.sqrt(1 / A2 - 1 / (A2 + B2) + 1 / C2 - 1 / (C2 + D2));
    return { tahmin, logSH: sh, ga: { duzey, alt: tahmin * Math.exp(-z * sh), ust: tahmin * Math.exp(z * sh), neyin: 'göreli risk' }, duzeltme: duz };
  };
  const goreliRisk = rr(a, b, c, d, 'Göreli risk (1. sütun)');
  const goreliRiskSutun2 = rr(b, a, d, c, 'Göreli risk (2. sütun)');
  // Risk farkı
  const p1 = a / n1;
  const p2 = c / n2;
  const fark = p1 - p2;
  const waldSH = Math.sqrt((p1 * (1 - p1)) / n1 + (p2 * (1 - p2)) / n2);
  const [l1, u1] = wilsonAraligi(a, n1, z);
  const [l2, u2] = wilsonAraligi(c, n2, z);
  const riskFarki = {
    tahmin: fark,
    p1,
    p2,
    wald: { duzey, alt: fark - z * waldSH, ust: fark + z * waldSH, sh: waldSH },
    newcombe: { duzey, alt: fark - Math.sqrt((p1 - l1) ** 2 + (u2 - p2) ** 2), ust: fark + Math.sqrt((u1 - p1) ** 2 + (p2 - l2) ** 2) },
  };
  return {
    yontem: 'risk-olculeri',
    ad: 'Odds oranı ve göreli risk',
    oddsOrani: oddsOraniSonuc,
    goreliRisk,
    goreliRiskSutun2,
    riskFarki,
    etki: { ad: 'Odds oranı', sembol: 'OR', deger: or, gaAlt: oddsOraniSonuc.ga.alt, gaUst: oddsOraniSonuc.ga.ust },
    n: n1 + n2,
    uyarilar,
  };
}

// ---------- Binom testi ve oran güven aralıkları ----------

/**
 * Kesin binom testi (R binom.test ile aynı): iki yönlü p, olasılığı gözlenenin (1 + 1e-7) katını aşmayan sonuçların
 * toplamıdır; Clopper-Pearson güven aralığı.
 * @param {number} x başarı sayısı
 * @param {number} n deneme sayısı
 * @param {number} p0 sıfır hipotezi oranı
 * @param {{alternatif?:string, duzey?:number}} secenek
 */
export function binomTesti(x, n, p0 = 0.5, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  const alt = alternatifAl(secenek.alternatif);
  duzeyKontrol(duzey);
  gerekli(Number.isInteger(n) && n >= 1, 'Deneme sayısı pozitif tamsayı olmalıdır.');
  gerekli(Number.isInteger(x) && x >= 0 && x <= n, 'Başarı sayısı 0 ile n arasında tamsayı olmalıdır.');
  gerekli(p0 >= 0 && p0 <= 1, 'Sıfır hipotezi oranı 0 ile 1 arasında olmalıdır.');
  let p;
  if (alt === 'kucuk') p = pbinom(x, n, p0);
  else if (alt === 'buyuk') p = pbinom(x - 1, n, p0, false);
  else if (p0 === 0) p = x === 0 ? 1 : 0;
  else if (p0 === 1) p = x === n ? 1 : 0;
  else {
    const relErr = 1 + 1e-7;
    const d = dbinom(x, n, p0);
    const m = n * p0;
    if (x === m) p = 1;
    else if (x < m) {
      let y = 0;
      for (let i = Math.ceil(m); i <= n; i++) if (dbinom(i, n, p0) <= d * relErr) y++;
      p = pbinom(x, n, p0) + pbinom(n - y, n, p0, false);
    } else {
      let y = 0;
      for (let i = 0; i <= Math.floor(m); i++) if (dbinom(i, n, p0) <= d * relErr) y++;
      p = pbinom(y - 1, n, p0) + pbinom(x - 1, n, p0, false);
    }
  }
  p = Math.min(1, p);
  const pL = (a) => (x === 0 ? 0 : qbeta(a, x, n - x + 1));
  const pU = (a) => (x === n ? 1 : qbeta(1 - a, x + 1, n - x));
  let ga;
  if (alt === 'kucuk') ga = [0, pU(1 - duzey)];
  else if (alt === 'buyuk') ga = [pL(1 - duzey), 1];
  else ga = [pL((1 - duzey) / 2), pU((1 - duzey) / 2)];
  return {
    yontem: 'binom-testi',
    ad: 'Binom testi',
    istatistik: x,
    istatistikAdi: 'x',
    sd: null,
    p,
    pYontemi: 'kesin',
    alternatif: alt,
    oran: x / n,
    p0,
    ga: { duzey, alt: ga[0], ust: ga[1], neyin: 'oran (Clopper-Pearson)' },
    n,
    uyarilar: [],
  };
}

/**
 * Tek oranın güven aralığı.
 * yontem: 'wilson' (skor; R prop.test(correct = FALSE)), 'clopper-pearson' (kesin; R binom.test),
 *         'wald' (p ± z√(p(1-p)/n), [0, 1]'e kırpılır), 'agresti-coull' (ñ = n + z², p̃ = (x + z²/2)/ñ; PropCIs::addz2ci).
 * @param {number} x
 * @param {number} n
 * @param {{yontem?:string, duzey?:number}} secenek
 */
export function oranGA(x, n, secenek = {}) {
  const { yontem = 'wilson', duzey = 0.95 } = secenek;
  duzeyKontrol(duzey);
  gerekli(n > 0 && x >= 0 && x <= n, 'Oran için 0 <= x <= n ve n > 0 olmalıdır.');
  const z = qnorm(1 - (1 - duzey) / 2);
  const p = x / n;
  const uyarilar = [];
  let alt;
  let ust;
  if (yontem === 'wilson') [alt, ust] = wilsonAraligi(x, n, z);
  else if (yontem === 'clopper-pearson') {
    const a = (1 - duzey) / 2;
    alt = x === 0 ? 0 : qbeta(a, x, n - x + 1);
    ust = x === n ? 1 : qbeta(1 - a, x + 1, n - x);
  } else if (yontem === 'wald') {
    const sh = Math.sqrt((p * (1 - p)) / n);
    alt = p - z * sh;
    ust = p + z * sh;
    if (alt < 0 || ust > 1) uyarilar.push('Wald aralığı [0, 1] dışına taştığı için kırpıldı.');
    alt = Math.max(0, alt);
    ust = Math.min(1, ust);
  } else if (yontem === 'agresti-coull') {
    const nt = n + z * z;
    const pt = (x + (z * z) / 2) / nt;
    const sh = Math.sqrt((pt * (1 - pt)) / nt);
    alt = Math.max(0, pt - z * sh);
    ust = Math.min(1, pt + z * sh);
  } else throw new IstatistikHatasi(`Bilinmeyen güven aralığı yöntemi: ${yontem}`);
  return { oran: p, duzey, alt, ust, yontem, n, uyarilar };
}

/**
 * İki bağımsız oranın karşılaştırılması (R prop.test(c(x1, x2), c(n1, n2), correct = sureklilik) ile aynı).
 * Varsayılan süreklilik düzeltmesizdir (SPSS çapraz tablo Pearson χ² ile aynı); fark için Wald güven aralığı
 * (sureklilik: true ise Yates payı eklenir).
 * @param {number} x1 @param {number} n1 @param {number} x2 @param {number} n2
 * @param {{sureklilik?:boolean, duzey?:number, alternatif?:string}} secenek
 */
export function ikiOranTesti(x1, n1, x2, n2, secenek = {}) {
  const { sureklilik = false, duzey = 0.95 } = secenek;
  const alt = alternatifAl(secenek.alternatif);
  duzeyKontrol(duzey);
  for (const [x, n] of [[x1, n1], [x2, n2]]) gerekli(n > 0 && x >= 0 && x <= n, 'Her grupta 0 <= x <= n ve n > 0 olmalıdır.');
  const p1 = x1 / n1;
  const p2 = x2 / n2;
  const pbar = (x1 + x2) / (n1 + n2);
  if (!(pbar > 0 && pbar < 1)) throw new IstatistikHatasi('Tüm gözlemler aynı kategoride; iki oran testi hesaplanamaz.');
  const delta = p1 - p2;
  let yates = sureklilik ? 0.5 : 0;
  yates = Math.min(yates, Math.abs(delta) / (1 / n1 + 1 / n2));
  const zq = qnorm(alt === 'iki-yonlu' ? (1 + duzey) / 2 : duzey);
  const genislik = zq * Math.sqrt((p1 * (1 - p1)) / n1 + (p2 * (1 - p2)) / n2) + yates * (1 / n1 + 1 / n2);
  let ga;
  if (alt === 'iki-yonlu') ga = [Math.max(delta - genislik, -1), Math.min(delta + genislik, 1)];
  else if (alt === 'buyuk') ga = [Math.max(delta - genislik, -1), 1];
  else ga = [-1, Math.min(delta + genislik, 1)];
  const x = [[x1, n1 - x1], [x2, n2 - x2]];
  const E = [[n1 * pbar, n1 * (1 - pbar)], [n2 * pbar, n2 * (1 - pbar)]];
  let X2 = 0;
  for (let i = 0; i < 2; i++) for (let j = 0; j < 2; j++) X2 += (Math.abs(x[i][j] - E[i][j]) - yates) ** 2 / E[i][j];
  const z = Math.sign(delta) * Math.sqrt(X2);
  let p;
  if (alt === 'iki-yonlu') p = chiP(X2, 1);
  else p = pnorm(z, 0, 1, alt === 'kucuk');
  const uyarilar = [];
  if (E.flat().some((e) => e < 5)) uyarilar.push('Beklenen değeri 5\'ten küçük hücre var; ki-kare yaklaşımı güvenilir olmayabilir.');
  return {
    yontem: 'iki-oran-testi',
    ad: 'İki oran testi',
    istatistik: X2,
    istatistikAdi: 'χ²',
    sd: 1,
    p,
    alternatif: alt,
    z,
    oranlar: [p1, p2],
    fark: delta,
    sureklilik: yates > 0,
    ga: { duzey, alt: ga[0], ust: ga[1], neyin: 'oran farkı (p1 - p2)' },
    n: n1 + n2,
    uyarilar,
  };
}

/**
 * Uyum iyiliği ki-kare testi (R chisq.test(x, p = oranlar)). Oranlar verilmezse eşit dağılım varsayılır;
 * oranların toplamı 1 değilse ölçeklenir (uyarı). Her kategori için gözlenen, beklenen, artık ve standart artık döner.
 * @param {number[]} gozlenen
 * @param {number[]} [beklenenOranlar]
 */
export function uyumIyiligiKiKare(gozlenen, beklenenOranlar) {
  gerekli(Array.isArray(gozlenen) && gozlenen.length >= 2, 'Uyum iyiliği testi için en az 2 kategori gerekir.');
  gerekli(gozlenen.every((v) => Number.isFinite(v) && v >= 0), 'Gözlenen frekanslar negatif olmayan sayılar olmalıdır.');
  const k = gozlenen.length;
  const N = gozlenen.reduce((a, b) => a + b, 0);
  gerekli(N > 0, 'Gözlem yok.');
  const uyarilar = [];
  let p = beklenenOranlar ?? new Array(k).fill(1 / k);
  gerekli(p.length === k, 'Beklenen oran sayısı kategori sayısına eşit olmalıdır.');
  gerekli(p.every((v) => Number.isFinite(v) && v > 0), 'Beklenen oranlar pozitif olmalıdır.');
  const ps = p.reduce((a, b) => a + b, 0);
  if (Math.abs(ps - 1) > 1e-8) {
    uyarilar.push('Beklenen oranların toplamı 1 olmadığından oranlar ölçeklendi.');
    p = p.map((v) => v / ps);
  }
  const E = p.map((v) => N * v);
  let X2 = 0;
  for (let i = 0; i < k; i++) X2 += (gozlenen[i] - E[i]) ** 2 / E[i];
  const kucuk = E.filter((e) => e < 5).length;
  if (kucuk > 0) uyarilar.push(`${kucuk} kategoride beklenen değer 5'ten küçük; ki-kare yaklaşımı güvenilir olmayabilir.`);
  return {
    yontem: 'uyum-iyiligi-ki-kare',
    ad: 'Ki-kare uyum iyiliği testi',
    istatistik: X2,
    istatistikAdi: 'χ²',
    sd: k - 1,
    p: chiP(X2, k - 1),
    kategoriler: gozlenen.map((o, i) => ({ gozlenen: o, beklenen: E[i], artik: o - E[i], standartArtik: (o - E[i]) / Math.sqrt(E[i]) })),
    n: N,
    uyarilar,
  };
}

// ---------- SPSS sütun oranları karşılaştırması ----------

/** Piepho (2004) ekle-emme algoritmasıyla harf gösterimi: aynı harfi paylaşan sütunlar farklı değildir. */
function harfGosterimi(adaylar, anlamliCiftler) {
  let sutunlar = [new Set(adaylar)];
  for (const [i, j] of anlamliCiftler) {
    const yeni = [];
    for (const s of sutunlar) {
      if (s.has(i) && s.has(j)) {
        const s1 = new Set(s);
        s1.delete(i);
        const s2 = new Set(s);
        s2.delete(j);
        yeni.push(s1, s2);
      } else yeni.push(s);
    }
    // emme: başka bir kümenin alt kümesi olanları at
    sutunlar = yeni.filter((s, a) => s.size > 0 && !yeni.some((t, b) => b !== a && [...s].every((x) => t.has(x)) && (t.size > s.size || b < a)));
  }
  // harfleri ilk üyenin sırasına göre ata
  sutunlar.sort((s, t) => Math.min(...s) - Math.min(...t) || Math.max(...s) - Math.max(...t));
  const harfler = {};
  for (const a of adaylar) harfler[a] = '';
  sutunlar.forEach((s, h) => {
    const harf = String.fromCharCode(97 + (h % 26)) + (h >= 26 ? String(Math.floor(h / 26)) : '');
    for (const a of s) harfler[a] += harf;
  });
  return harfler;
}

/**
 * SPSS Crosstabs "Compare column proportions" (z testi) karşılığı: her satırda sütun oranları p_j = n_ij / C_j
 * ikişer ikişer havuzlanmış z testiyle karşılaştırılır: z = (p_j - p_k)/√[p̄(1 - p̄)(1/C_j + 1/C_k)].
 * p değerleri her satır içinde Bonferroni ile düzeltilir (SPSS varsayılanı). Oranı 0 ya da 1 olan sütunlar o satırda
 * karşılaştırmaya alınmaz (SPSS gibi). Harfler: aynı harfi paylaşan sütunların oranları anlamlı farklı değildir.
 * @param {number[][]} tablo
 * @param {{alfa?:number, duzeltme?:string}} secenek
 */
export function sutunOranKarsilastirma(tablo, secenek = {}) {
  const { alfa = 0.05, duzeltme = 'bonferroni' } = secenek;
  tabloKontrol(tablo);
  const r = tablo.length;
  const c = tablo[0].length;
  gerekli(c >= 2, 'Sütun oranlarını karşılaştırmak için en az 2 sütun gerekir.');
  const { sutun } = toplamlar(tablo);
  const satirlar = [];
  for (let i = 0; i < r; i++) {
    const oranlar = tablo[i].map((v, j) => (sutun[j] > 0 ? v / sutun[j] : NaN));
    const adaylar = [];
    for (let j = 0; j < c; j++) if (sutun[j] > 0 && oranlar[j] > 0 && oranlar[j] < 1) adaylar.push(j);
    const karsilastirmalar = [];
    for (let a = 0; a < adaylar.length; a++) {
      for (let b = a + 1; b < adaylar.length; b++) {
        const j = adaylar[a];
        const k = adaylar[b];
        const pbar = (tablo[i][j] + tablo[i][k]) / (sutun[j] + sutun[k]);
        const z = (oranlar[j] - oranlar[k]) / Math.sqrt(pbar * (1 - pbar) * (1 / sutun[j] + 1 / sutun[k]));
        karsilastirmalar.push({ sutun1: j, sutun2: k, oran1: oranlar[j], oran2: oranlar[k], z, p: ikiYonluNormalP(z) });
      }
    }
    const duz = pDuzelt(karsilastirmalar.map((k) => k.p), duzeltme);
    karsilastirmalar.forEach((k, idx) => {
      k.pDuzeltilmis = duz[idx];
      k.anlamli = duz[idx] < alfa;
    });
    const harfMap = harfGosterimi(adaylar, karsilastirmalar.filter((k) => k.anlamli).map((k) => [k.sutun1, k.sutun2]));
    const harfler = Array.from({ length: c }, (_, j) => (j in harfMap ? harfMap[j] : null));
    satirlar.push({ satir: i, oranlar, karsilastirmalar, harfler, karsilastirmaDisi: Array.from({ length: c }, (_, j) => j).filter((j) => !adaylar.includes(j)) });
  }
  return {
    yontem: 'sutun-oranlari-z',
    ad: 'Sütun oranlarının karşılaştırılması (z testi)',
    duzeltme,
    alfa,
    satirlar,
    uyarilar: [],
  };
}
