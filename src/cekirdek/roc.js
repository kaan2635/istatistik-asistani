// ROC analizi: eğri noktaları, AUC ve standart hatası (DeLong; Hanley-McNeil), en iyi kesim noktası,
// kesim noktasındaki tanısal ölçütler ve iki ROC eğrisinin DeLong testiyle karşılaştırılması.
//
// R pROC (1.19) ile birebir aynıdır: roc(durum, deger, direction = ...), ci.auc(method = 'delong'),
// coords(x = 'best', best.method = 'youden' | 'closest.topleft'), roc.test(method = 'delong', paired = TRUE/FALSE).

import { medyan } from './betimsel.js';
import { pnorm, pt, qnorm } from './dagilim.js';
import { IstatistikHatasi, gerekli } from './hata.js';
import { taniTesti } from './tani.js';

// ---------- Yardımcılar ----------

function durumDiziye(durum, n) {
  gerekli(Array.isArray(durum) || ArrayBuffer.isView(durum), 'Durum (hasta/sağlam) değişkeni bir dizi olmalıdır.');
  gerekli(durum.length === n, 'Değer ve durum değişkenlerinin uzunlukları eşit olmalıdır.');
  return Array.from(durum, (v) => {
    if (v === 1 || v === true) return 1;
    if (v === 0 || v === false) return 0;
    throw new IstatistikHatasi('Durum değişkeni yalnızca 1 (hasta / pozitif) ve 0 (sağlam / negatif) değerlerini almalıdır.');
  });
}

function yonCoz(yon) {
  if (yon === 'buyuk-pozitif' || yon === '<') return '<';
  if (yon === 'kucuk-pozitif' || yon === '>') return '>';
  if (yon === 'otomatik' || yon === undefined || yon === null) return 'otomatik';
  throw new IstatistikHatasi("Yön 'otomatik', 'buyuk-pozitif' ya da 'kucuk-pozitif' olmalıdır.");
}

const yonAdi = (y) => (y === '<' ? 'buyuk-pozitif' : 'kucuk-pozitif');

function ayir(deger, durum) {
  gerekli(Array.isArray(deger) || ArrayBuffer.isView(deger), 'Test değeri bir dizi olmalıdır.');
  const d = durumDiziye(durum, deger.length);
  const vakalar = [];
  const kontroller = [];
  for (let i = 0; i < deger.length; i++) {
    const v = deger[i];
    gerekli(typeof v === 'number' && Number.isFinite(v), 'Test değerinde eksik ya da sayısal olmayan değer var.');
    (d[i] === 1 ? vakalar : kontroller).push(v);
  }
  if (vakalar.length === 0) throw new IstatistikHatasi('ROC analizi için en az bir hasta (durum = 1) gerekir.');
  if (kontroller.length === 0) throw new IstatistikHatasi('ROC analizi için en az bir sağlam (durum = 0) gerekir.');
  return { vakalar, kontroller };
}

// sıralı dizide: x'ten küçük ve x'e eşit eleman sayısı
function altEsit(sirali, x) {
  let lo = 0;
  let hi = sirali.length;
  while (lo < hi) {
    const m = (lo + hi) >> 1;
    if (sirali[m] < x) lo = m + 1;
    else hi = m;
  }
  const kucuk = lo;
  hi = sirali.length;
  while (lo < hi) {
    const m = (lo + hi) >> 1;
    if (sirali[m] <= x) lo = m + 1;
    else hi = m;
  }
  return { kucuk, esit: lo - kucuk };
}

// pROC roc_utils_thresholds
function esikler(tumu, yon) {
  const u = Array.from(new Set(tumu)).sort((a, b) => a - b);
  const e = [];
  for (let i = 0; i <= u.length; i++) {
    const sol = i === 0 ? -Infinity : u[i - 1];
    const sag = i === u.length ? Infinity : u[i];
    const t1 = (sol + sag) / 2;
    e.push(Math.abs(t1) > 1e100 ? sol / 2 + sag / 2 : t1);
  }
  const kume = new Set(tumu);
  for (let i = 0; i < e.length; i++) {
    if (!kume.has(e[i])) continue;
    // Sayısal hassasiyet sınırında orta nokta bir veri değerine eşit çıktı (pROC düzeltmesi)
    if (yon === '>') {
      if (e[i] === u[i - 1]) continue;
      if (e[i] === u[i]) e[i] = u[i - 1];
    } else if (e[i] === u[i - 1]) e[i] = u[i];
  }
  return e;
}

// DeLong yerleşimleri (placements): V10 (her hasta için), V01 (her sağlam için), θ = AUC
function yerlesimler(vakalar, kontroller, yon) {
  const m = vakalar.length;
  const n = kontroller.length;
  const sv = Float64Array.from(vakalar).sort();
  const sk = Float64Array.from(kontroller).sort();
  const V10 = vakalar.map((x) => {
    const { kucuk, esit } = altEsit(sk, x);
    const lehte = yon === '<' ? kucuk : n - kucuk - esit;
    return (lehte + 0.5 * esit) / n;
  });
  const V01 = kontroller.map((y) => {
    const { kucuk, esit } = altEsit(sv, y);
    const lehte = yon === '<' ? m - kucuk - esit : kucuk;
    return (lehte + 0.5 * esit) / m;
  });
  let toplam = 0;
  for (const v of V10) toplam += v * n;
  const theta = toplam / (m * n);
  return { V10, V01, theta };
}

function delongVaryans(Y) {
  const m = Y.V10.length;
  const n = Y.V01.length;
  if (m <= 1 || n <= 1) return NaN;
  let sx = 0;
  for (const v of Y.V10) sx += (v - Y.theta) ** 2;
  let sy = 0;
  for (const v of Y.V01) sy += (v - Y.theta) ** 2;
  return sx / (m - 1) / m + sy / (n - 1) / n;
}

function rocHesap(deger, durum, yonSecim) {
  const { vakalar, kontroller } = ayir(deger, durum);
  let yon = yonCoz(yonSecim);
  const otomatik = yon === 'otomatik';
  if (otomatik) yon = medyan(kontroller) <= medyan(vakalar) ? '<' : '>';
  return { vakalar, kontroller, yon, otomatik, Y: yerlesimler(vakalar, kontroller, yon) };
}

/**
 * ROC analizi (R pROC::roc ile aynı).
 *
 * - yon: 'otomatik' (pROC direction = 'auto': sağlamların medyanı hastalarınkinden büyük değilse yüksek değer
 *   pozitif sayılır), 'buyuk-pozitif' (değer ≥ eşik → pozitif; pROC '<') ya da 'kucuk-pozitif' (değer ≤ eşik → pozitif;
 *   pROC '>'). Kullanılan yön `yon` alanında bildirilir.
 * - noktalar: pROC'taki tüm eşikler (−∞, ardışık farklı değerlerin orta noktaları, +∞) ve her eşikte duyarlılık /
 *   özgüllük, pROC'un sırasıyla (özgüllük artan).
 * - AUC = Mann-Whitney U / (n₁n₀) (bağlar ½). SH ve GA iki yolla:
 *   DeLong (1988) — pROC ci.auc(method = 'delong'), MedCalc varsayılanı (gaAlt/gaUst; [0, 1]'e kırpılır);
 *   Hanley-McNeil (1982) — negatif üstel varsayımı (MedCalc seçeneği; shHanleyMcNeil, gaHanleyMcNeil).
 * - p (H0: AUC = 0.5): pDeLong = z = (AUC − 0.5)/SH_DeLong ile (MedCalc yaklaşımı); pH0 = H0 altındaki SH ile
 *   (bağ düzeltmeli Mann-Whitney U testi, wilcox.test(exact = FALSE, correct = FALSE) ile aynı). SPSS ROC
 *   "Asymptotic Sig." H0 altındaki SH'yi kullanır; pH0'ın SPSS ile aynı olduğu SPSS çıktısıyla doğrulanmamıştır.
 *   SPSS'in "Std. Error" (parametrik olmayan) değeri DeLong'a çok yakındır ama birebir aynı olmayabilir.
 * - En iyi kesim: Youden (duyarlılık + özgüllük en büyük) ve (0, 1) köşesine en yakın nokta; eşitlik durumunda
 *   pROC coords(x = 'best') gibi tüm eşit noktalar `esitler`de döner, ilki seçilir. Seçilen kesimde tanısal
 *   ölçütler (taniTesti) hesaplanır.
 *
 * @param {number[]} deger test sonucu (sürekli ya da sıralı)
 * @param {(0|1|boolean)[]} durum 1 = hasta (vaka), 0 = sağlam (kontrol)
 * @param {{yon?:'otomatik'|'buyuk-pozitif'|'kucuk-pozitif', duzey?:number, oranYontemi?:string}} [secenek]
 */
export function rocAnalizi(deger, durum, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  gerekli(duzey > 0 && duzey < 1, 'Güven düzeyi 0 ile 1 arasında olmalıdır.');
  const r = rocHesap(deger, durum, secenek.yon);
  const { vakalar, kontroller, yon, Y } = r;
  const m = vakalar.length;
  const n = kontroller.length;
  const z = qnorm(1 - (1 - duzey) / 2);
  const uyarilar = [];

  // Eğri noktaları
  const sv = Float64Array.from(vakalar).sort();
  const sk = Float64Array.from(kontroller).sort();
  let es = esikler([...kontroller, ...vakalar], yon);
  if (yon === '>') es = es.reverse(); // özgüllük artan sıra (pROC sort_roc)
  const noktalar = es.map((t) => {
    let tp;
    let tn;
    if (yon === '<') {
      tp = m - altEsit(sv, t).kucuk; // vakalar ≥ t
      tn = altEsit(sk, t).kucuk; // kontroller < t
    } else {
      const a = altEsit(sv, t);
      tp = a.kucuk + a.esit; // vakalar ≤ t
      const b = altEsit(sk, t);
      tn = n - b.kucuk - b.esit; // kontroller > t
    }
    return { esik: t, duyarlilik: tp / m, ozgulluk: tn / n, tp, tn };
  });

  // AUC ve standart hatalar
  const auc = Y.theta;
  const varDL = delongVaryans(Y);
  const shDL = Math.sqrt(varDL);
  if (Number.isNaN(varDL)) uyarilar.push('DeLong standart hatası için her grupta en az iki gözlem gerekir.');
  const kirp = (v) => Math.min(1, Math.max(0, v));
  const Q1 = auc / (2 - auc);
  const Q2 = (2 * auc * auc) / (1 + auc);
  const shHM = Math.sqrt((auc * (1 - auc) + (m - 1) * (Q1 - auc * auc) + (n - 1) * (Q2 - auc * auc)) / (m * n));
  // H0 altında (Mann-Whitney, bağ düzeltmeli)
  const tum = [...vakalar, ...kontroller].sort((a, b) => a - b);
  let bag = 0;
  for (let i = 0; i < tum.length; ) {
    let j = i;
    while (j < tum.length && tum[j] === tum[i]) j++;
    const tt = j - i;
    bag += tt * tt * tt - tt;
    i = j;
  }
  const N = m + n;
  const varU = ((m * n) / 12) * (N + 1 - bag / (N * (N - 1)));
  const sh0 = Math.sqrt(varU) / (m * n);
  const z0 = (auc - 0.5) / sh0;
  const zDL = (auc - 0.5) / shDL;
  if (auc === 1 || auc === 0) uyarilar.push('Hastalar ve sağlamlar tam ayrışıyor (AUC = 0 ya da 1); güven aralığı yanıltıcı olabilir.');
  if (auc < 0.5) uyarilar.push("AUC 0.5'ten küçük: seçilen yönde test ters çalışıyor olabilir.");

  // En iyi kesim noktaları
  const youden = noktalar.map((p) => p.duyarlilik + 1 * p.ozgulluk);
  const yakinlik = noktalar.map((p) => -((1 - p.duyarlilik) ** 2 + 1 * (1 - p.ozgulluk) ** 2));
  const enIyi = (olcut, ad) => {
    const enb = Math.max(...olcut);
    const esitler = noktalar.filter((_, i) => olcut[i] === enb).map((p) => ({ esik: p.esik, duyarlilik: p.duyarlilik, ozgulluk: p.ozgulluk }));
    const sec = esitler[0];
    const metrik = kesimNoktasiMetrikleri(deger, durum, sec.esik, yon, secenek);
    return { olcut: ad, ...sec, youdenJ: sec.duyarlilik + sec.ozgulluk - 1, esitler, metrikler: metrik };
  };

  return {
    yontem: 'roc',
    ad: 'ROC analizi',
    yon: yonAdi(yon),
    yonOtomatik: r.otomatik,
    duzey,
    nHasta: m,
    nSaglam: n,
    auc,
    sh: shDL,
    gaAlt: kirp(auc - z * shDL),
    gaUst: kirp(auc + z * shDL),
    varyansDeLong: varDL,
    shHanleyMcNeil: shHM,
    gaHanleyMcNeil: { alt: kirp(auc - z * shHM), ust: kirp(auc + z * shHM) },
    shH0: sh0,
    zDeLong: zDL,
    pDeLong: 2 * pnorm(-Math.abs(zDL)),
    zH0: z0,
    pH0: 2 * pnorm(-Math.abs(z0)),
    p: 2 * pnorm(-Math.abs(zDL)),
    noktalar: noktalar.map(({ esik, duyarlilik, ozgulluk }) => ({ esik, duyarlilik, ozgulluk })),
    enIyiYouden: enIyi(youden, 'youden'),
    enIyiKose: enIyi(yakinlik, 'kose'),
    uyarilar,
  };
}

/**
 * Belirli bir kesim noktasında 2×2 tablo ve tanısal ölçütler (taniTesti).
 * yon 'buyuk-pozitif' (ya da '<'): değer ≥ kesim pozitif; 'kucuk-pozitif' (ya da '>'): değer ≤ kesim pozitif
 * (pROC'un eşik tanımı). 'otomatik' verilirse yön rocAnalizi'ndeki kuralla belirlenir.
 * @returns taniTesti() sonucu + {kesim, yon, kriter}
 */
export function kesimNoktasiMetrikleri(deger, durum, kesim, yon = 'buyuk-pozitif', secenek = {}) {
  gerekli(typeof kesim === 'number' && !Number.isNaN(kesim), 'Kesim noktası bir sayı olmalıdır.');
  let y = yonCoz(yon);
  if (y === 'otomatik') y = rocHesap(deger, durum, 'otomatik').yon;
  const d = durumDiziye(durum, deger.length);
  let tp = 0;
  let fp = 0;
  let fn = 0;
  let tn = 0;
  for (let i = 0; i < deger.length; i++) {
    const v = deger[i];
    gerekli(typeof v === 'number' && Number.isFinite(v), 'Test değerinde eksik ya da sayısal olmayan değer var.');
    const pozitif = y === '<' ? v >= kesim : v <= kesim;
    if (d[i] === 1) pozitif ? tp++ : fn++;
    else pozitif ? fp++ : tn++;
  }
  const { prevalans, duzey, oranYontemi, youdenYontemi, sifirDuzeltme } = secenek;
  const t = taniTesti(tp, fp, fn, tn, { prevalans, duzey, oranYontemi, youdenYontemi, sifirDuzeltme });
  return { ...t, kesim, yon: yonAdi(y), kriter: `${y === '<' ? '≥' : '≤'} ${kesim}` };
}

/**
 * Aynı hastalarda ölçülmüş iki testin AUC'lerinin karşılaştırılması: eşleştirilmiş DeLong testi
 * (R pROC::roc.test(roc1, roc2, method = 'delong', paired = TRUE)). Her eğrinin yönü ayrı belirlenir
 * (varsayılan 'otomatik', pROC gibi; secenek.yon1 / yon2 ile verilebilir).
 * @returns {{istatistik: Z, p, fark, gaAlt, gaUst, auc1, auc2, sh1, sh2, korelasyon, ...}}
 */
export function rocKarsilastir(deger1, deger2, durum, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  gerekli(deger1.length === deger2.length, 'Eşleştirilmiş karşılaştırmada iki testin gözlem sayısı aynı olmalıdır.');
  const r1 = rocHesap(deger1, durum, secenek.yon1 ?? secenek.yon);
  const r2 = rocHesap(deger2, durum, secenek.yon2 ?? secenek.yon);
  const m = r1.vakalar.length;
  const n = r1.kontroller.length;
  if (m <= 1 || n <= 1) throw new IstatistikHatasi('DeLong testi için her grupta en az iki gözlem gerekir.');
  const A = r1.Y;
  const B = r2.Y;
  const kov = (x, y, tx, ty, k) => {
    let s = 0;
    for (let i = 0; i < x.length; i++) s += (x[i] - tx) * (y[i] - ty);
    return s / (k - 1);
  };
  const S11 = kov(A.V10, A.V10, A.theta, A.theta, m) / m + kov(A.V01, A.V01, A.theta, A.theta, n) / n;
  const S22 = kov(B.V10, B.V10, B.theta, B.theta, m) / m + kov(B.V01, B.V01, B.theta, B.theta, n) / n;
  const S12 = kov(A.V10, B.V10, A.theta, B.theta, m) / m + kov(A.V01, B.V01, A.theta, B.theta, n) / n;
  const sig = Math.sqrt(S11 + S22 - 2 * S12);
  const fark = A.theta - B.theta;
  let Z = fark / sig;
  if (Number.isNaN(Z) && fark === 0 && sig === 0) Z = 0;
  const zk = qnorm(1 - (1 - duzey) / 2);
  const uyarilar = [];
  if (r1.yon !== r2.yon) uyarilar.push('İki eğrinin yönü farklı; DeLong testi aynı yönlü eğriler için uygundur.');
  return {
    yontem: 'delong-eslestirilmis',
    ad: 'İki bağımlı ROC eğrisinin karşılaştırılması (DeLong)',
    istatistik: Z,
    istatistikAdi: 'z',
    sd: null,
    p: 2 * pnorm(-Math.abs(Z)),
    auc1: A.theta,
    auc2: B.theta,
    sh1: Math.sqrt(S11),
    sh2: Math.sqrt(S22),
    korelasyon: S12 / Math.sqrt(S11 * S22),
    fark,
    shFark: sig,
    gaAlt: fark - zk * sig,
    gaUst: fark + zk * sig,
    ga: { duzey, alt: fark - zk * sig, ust: fark + zk * sig, neyin: 'AUC farkı' },
    yon1: yonAdi(r1.yon),
    yon2: yonAdi(r2.yon),
    nHasta: m,
    nSaglam: n,
    uyarilar,
  };
}

/**
 * Farklı hasta gruplarındaki iki ROC eğrisinin karşılaştırılması (bağımsız DeLong; R pROC::roc.test(paired = FALSE)).
 * pROC gibi D = (AUC₁ − AUC₂)/√(V₁ + V₂) istatistiği Welch benzeri serbestlik derecesiyle t dağılımından
 * değerlendirilir (p). Hanley-McNeil (1982) / MedCalc'ın normal yaklaşımı pNormal'dadır.
 * @param r1 rocAnalizi() sonucu ya da {deger, durum, yon?}
 * @param r2 rocAnalizi() sonucu ya da {deger, durum, yon?}
 */
export function rocBagimsizKarsilastir(r1, r2, secenek = {}) {
  const { duzey = 0.95 } = secenek;
  const hazirla = (r) => (r && r.yontem === 'roc' ? r : rocAnalizi(r.deger, r.durum, { yon: r.yon }));
  const a = hazirla(r1);
  const b = hazirla(r2);
  const V1 = a.varyansDeLong;
  const V2 = b.varyansDeLong;
  if (!Number.isFinite(V1) || !Number.isFinite(V2)) throw new IstatistikHatasi('DeLong testi için her grupta en az iki gözlem gerekir.');
  const fark = a.auc - b.auc;
  const se = Math.sqrt(V1 + V2);
  const D = fark / se;
  const sd = (V1 + V2) ** 2 / (V1 ** 2 / (a.nHasta + a.nSaglam - 1) + V2 ** 2 / (b.nHasta + b.nSaglam - 1));
  const zk = qnorm(1 - (1 - duzey) / 2);
  return {
    yontem: 'delong-bagimsiz',
    ad: 'İki bağımsız ROC eğrisinin karşılaştırılması (DeLong)',
    istatistik: D,
    istatistikAdi: 'D',
    sd,
    p: 2 * pt(-Math.abs(D), sd),
    pNormal: 2 * pnorm(-Math.abs(D)),
    auc1: a.auc,
    auc2: b.auc,
    fark,
    shFark: se,
    gaAlt: fark - zk * se,
    gaUst: fark + zk * se,
    ga: { duzey, alt: fark - zk * se, ust: fark + zk * se, neyin: 'AUC farkı (normal yaklaşım)' },
    uyarilar: a.yon !== b.yon ? ['İki eğrinin yönü farklı.'] : [],
  };
}
