// Sağkalım analizi: Kaplan-Meier kestirimi ve log-rank ailesi testleri.
//
// Hesaplar R'nin survival paketiyle (3.8) birebir aynıdır:
//   kaplanMeier        = survfit(Surv(sure, olay) ~ 1, conf.type = ...)
//                        + print/summary tablosu (medyan ve GA'sı, kısıtlı ortalama), quantile(), summary(times = )
//   logRank            = survdiff(rho = 0) (log-rank), survdiff(rho = 1) (Peto-Peto);
//                        Breslow (genelleştirilmiş Wilcoxon, ağırlık = riskteki kişi sayısı) ve
//                        Tarone-Ware (ağırlık = √riskteki) SPSS'in tanımıyla.
// R gibi, birbirine 1.5e-8'den (göreli) daha yakın süreler aynı süre sayılır (survival::aeqSurv).

import { pchisq, qnorm } from './dagilim.js';
import { pDuzelt } from './duzeltme.js';
import { IstatistikHatasi, gerekli } from './hata.js';

const TOL = Math.sqrt(2.220446049250313e-16); // sqrt(.Machine$double.eps)

// ---------- Girdi denetimi ve ortak yardımcılar ----------

function olayDiziye(olay, n) {
  gerekli(Array.isArray(olay) || ArrayBuffer.isView(olay), 'Olay (durum) değişkeni bir dizi olmalıdır.');
  gerekli(olay.length === n, 'Süre ve olay değişkenlerinin uzunlukları eşit olmalıdır.');
  const d = new Int8Array(n);
  for (let i = 0; i < n; i++) {
    const v = olay[i];
    if (v === 1 || v === true) d[i] = 1;
    else if (v === 0 || v === false) d[i] = 0;
    else throw new IstatistikHatasi('Olay değişkeni yalnızca 1 (olay gerçekleşti) ve 0 (sansürlü) değerlerini almalıdır.');
  }
  return d;
}

function sureDenetle(sure) {
  gerekli(Array.isArray(sure) || ArrayBuffer.isView(sure), 'Süre değişkeni bir dizi olmalıdır.');
  gerekli(sure.length > 0, 'Sağkalım analizi için en az bir gözlem gerekir.');
  for (const t of sure) {
    gerekli(typeof t === 'number' && Number.isFinite(t), 'Süre değişkeninde eksik ya da sayısal olmayan değer var.');
    gerekli(t >= 0, 'Sağkalım süreleri negatif olamaz.');
  }
}

/**
 * Birbirine çok yakın (yuvarlama kaynaklı) süreleri birleştirir: R survival::aeqSurv ile aynı.
 * Ardışık iki farklı süre arasındaki fark √ε'dan ya da (fark / ortalama |süre|) √ε'dan küçükse
 * ikisi de küçük olana eşitlenir. Temiz verilerde hiçbir şeyi değiştirmez.
 * @param {number[]} sure
 * @returns {number[]}
 */
export function zamanDuzelt(sure) {
  const y = Array.from(new Set(sure)).filter(Number.isFinite).sort((a, b) => a - b);
  if (y.length < 2) return Array.from(sure);
  let ortAbs = 0;
  for (const v of y) ortAbs += Math.abs(v);
  ortAbs /= y.length;
  const kesim = [y[0]];
  let bag = false;
  for (let i = 1; i < y.length; i++) {
    const dy = y[i] - y[i - 1];
    if (dy <= TOL || dy / ortAbs <= TOL) bag = true;
    else kesim.push(y[i]);
  }
  if (!bag) return Array.from(sure);
  return Array.from(sure, (t) => {
    // findInterval: t'den küçük ya da eşit en büyük kesim
    let lo = 0;
    let hi = kesim.length - 1;
    while (lo < hi) {
      const orta = (lo + hi + 1) >> 1;
      if (kesim[orta] <= t) lo = orta;
      else hi = orta - 1;
    }
    return kesim[lo];
  });
}

/** Grup düzeylerini sıralar (sayılar sayısal, metinler alfabetik) ve her gözleme düzey indeksini verir. */
export function gruplariAyir(grup, duzeyler = null) {
  gerekli(Array.isArray(grup) || ArrayBuffer.isView(grup), 'Grup değişkeni bir dizi olmalıdır.');
  for (const g of grup) gerekli(g !== null && g !== undefined && !(typeof g === 'number' && Number.isNaN(g)), 'Grup değişkeninde eksik değer var.');
  let d;
  if (duzeyler) {
    d = Array.from(duzeyler);
  } else {
    d = Array.from(new Set(grup));
    if (d.every((v) => typeof v === 'number')) d.sort((a, b) => a - b);
    else d.sort((a, b) => String(a).localeCompare(String(b), 'tr'));
  }
  const harita = new Map(d.map((v, i) => [v, i]));
  const indeks = new Int32Array(grup.length);
  for (let i = 0; i < grup.length; i++) {
    const k = harita.get(grup[i]);
    gerekli(k !== undefined, `Grup değeri "${grup[i]}" belirtilen düzeyler arasında yok.`);
    indeks[i] = k;
  }
  return { duzeyler: d, indeks };
}

// R'nin approx(x, y, v, method = "constant", f = 1) davranışı (x artan, tekrarsız)
function approxSabit(xs, ys, v) {
  const n = xs.length;
  if (v < xs[0] || v > xs[n - 1]) return NaN;
  let i = 0;
  let j = n - 1;
  while (i < j - 1) {
    const ij = (i + j) >> 1;
    if (v < xs[ij]) j = ij;
    else i = ij;
  }
  if (v === xs[j]) return ys[j];
  if (v === xs[i]) return ys[i];
  return ys[j];
}

// approx'un ön işlemi: NA çiftleri atılır, x'e göre sıralanır
function approxHazirla(x, idx) {
  const ciftler = [];
  for (let i = 0; i < x.length; i++) if (!Number.isNaN(x[i])) ciftler.push([x[i], idx[i]]);
  ciftler.sort((a, b) => a[0] - b[0]);
  return { xs: ciftler.map((c) => c[0]), ys: ciftler.map((c) => c[1]) };
}

// survival:::findq — eğrinin p düzeylerini kestiği süreler (düz kesimlerde orta nokta)
function findq(x, y, ps) {
  let maks = -Infinity;
  for (const v of y) if (!Number.isNaN(v) && v > maks) maks = v;
  if (maks < Math.min(...ps)) return ps.map(() => NaN);
  const xmax = x[x.length - 1];
  const gorulen = new Set();
  const xx = [];
  const yy = [];
  for (let i = 0; i < y.length; i++) {
    const anahtar = Number.isNaN(y[i]) ? 'NA' : y[i];
    if (gorulen.has(anahtar)) continue;
    gorulen.add(anahtar);
    xx.push(x[i]);
    yy.push(y[i]);
  }
  const n = yy.length;
  const idx = yy.map((_, i) => i);
  const ust = approxHazirla(yy.map((v) => v + TOL), idx);
  const alt = approxHazirla(yy.map((v) => v - TOL), idx);
  if (ust.xs.length < 2) return ps.map(() => NaN);
  return ps.map((p) => {
    const i1 = approxSabit(ust.xs, ust.ys, p);
    const i2 = approxSabit(alt.xs, alt.ys, p);
    let q = Number.isNaN(i1) || Number.isNaN(i2) ? NaN : (xx[i1] + xx[i2]) / 2;
    if (p === 0) q = xx[0];
    if (!Number.isNaN(yy[n - 1]) && Math.abs(p - yy[n - 1]) < TOL) q = Number.isNaN(i1) ? NaN : (xx[i1] + xmax) / 2;
    return q;
  });
}

// survival:::survmean içindeki minmin — print(survfit) medyanı ve GA'sı
function minmin(y, x) {
  const xk = [];
  const yk = [];
  for (let i = 0; i < y.length; i++) {
    if (y[i] !== null && !Number.isNaN(y[i]) && y[i] < 0.5 + TOL) {
      xk.push(x[i]);
      yk.push(y[i]);
    }
  }
  if (xk.length === 0) return null;
  if (Math.abs(yk[0] - 0.5) < TOL) {
    const j = yk.findIndex((v) => v < yk[0]);
    if (j >= 0) return (xk[0] + xk[j]) / 2;
  }
  return xk[0];
}

const nullIse = (v) => (v === null || Number.isNaN(v) ? null : v);

// survfit_confint (logse = TRUE, conf.lower = "usual")
function guvenSiniri(S, se, z, tip) {
  if (tip === 'plain') {
    const se2 = se * S * z;
    return [Math.max(S - se2, 0), Math.min(S + se2, 1)];
  }
  if (tip === 'log') {
    if (S === 0) return [NaN, NaN];
    if (se === 0) return [S, S];
    return [Math.exp(Math.log(S) - z * se), Math.min(Math.exp(Math.log(S) + z * se), 1)];
  }
  if (tip === 'log-log') {
    if (se === 0) return [S, S];
    if (S === 0 || S === 1) return [NaN, NaN];
    const se2 = (z * se) / Math.log(S);
    const ll = Math.log(-Math.log(S));
    return [Math.exp(-Math.exp(ll - se2)), Math.exp(-Math.exp(ll + se2))];
  }
  throw new IstatistikHatasi(`Bilinmeyen güven aralığı türü: ${tip}`);
}

// Tek bir eğri için survfit hesabı (süreler düzeltilmiş olmalı)
function kmEgrisi(sure, olay, z, gaTipi) {
  const n = sure.length;
  const sira = Array.from({ length: n }, (_, i) => i).sort((a, b) => sure[a] - sure[b] || a - b);
  const zaman = [];
  const riskAltinda = [];
  const olaySay = [];
  const sansur = [];
  let i = 0;
  while (i < n) {
    const t = sure[sira[i]];
    let d = 0;
    let c = 0;
    let j = i;
    while (j < n && sure[sira[j]] === t) {
      if (olay[sira[j]] === 1) d++;
      else c++;
      j++;
    }
    zaman.push(t);
    riskAltinda.push(n - i);
    olaySay.push(d);
    sansur.push(c);
    i = j;
  }
  const k = zaman.length;
  const sagkalim = new Array(k);
  const logSH = new Array(k);
  const kumHaz = new Array(k);
  const kumHazSH = new Array(k);
  let S = 1;
  let v = 0;
  let H = 0;
  let vH = 0;
  for (let m = 0; m < k; m++) {
    const r = riskAltinda[m];
    const d = olaySay[m];
    if (d > 0) {
      S *= (r - d) / r;
      v += r === d ? Infinity : d / (r * (r - d));
      H += d / r;
      vH += d / (r * r);
    }
    sagkalim[m] = S;
    logSH[m] = Math.sqrt(v);
    kumHaz[m] = H;
    kumHazSH[m] = Math.sqrt(vH);
  }
  const alt = new Array(k);
  const ust = new Array(k);
  for (let m = 0; m < k; m++) [alt[m], ust[m]] = guvenSiniri(sagkalim[m], logSH[m], z, gaTipi);
  return { zaman, riskAltinda, olay: olaySay, sansur, sagkalim, logSH, alt, ust, kumHaz, kumHazSH };
}

// survmean: kısıtlı ortalama ve standart hatası (başlangıç zamanı 0)
function kisitliOrtalama(e, ustSinir) {
  const { zaman, sagkalim, riskAltinda, olay } = e;
  const tut = [];
  for (let i = 0; i < zaman.length; i++) if (zaman[i] <= ustSinir) tut.push(i);
  if (tut.length === 0) return { deger: ustSinir, sh: 0 };
  const tz = tut.map((i) => zaman[i]).concat([ustSinir]);
  const ts = tut.map((i) => sagkalim[i]);
  const hh = tut.map((i) => (riskAltinda[i] - olay[i] === 0 ? 0 : olay[i] / (riskAltinda[i] * (riskAltinda[i] - olay[i]))));
  const n = tz.length;
  const dik = new Array(n);
  let onceki = 0;
  for (let i = 0; i < n; i++) {
    dik[i] = (tz[i] - onceki) * (i === 0 ? 1 : ts[i - 1]);
    onceki = tz[i];
  }
  let ortalama = 0;
  for (const r of dik) ortalama += r;
  // var = Σ_j A_j² h_j; A_j = j. zamandan üst sınıra kadar eğri altındaki alan
  let varyans = 0;
  let A = 0;
  for (let j = n - 2; j >= 0; j--) {
    A += dik[j + 1];
    varyans += A * A * hh[j];
  }
  return { deger: ortalama, sh: Math.sqrt(varyans) };
}

function sagkalimZamanlarda(e, zamanlar, uyarilar) {
  const { zaman, riskAltinda, olay, sansur, sagkalim, logSH, alt, ust } = e;
  // survfit0: başta (0, S = 1) satırı (ilk süre 0 değilse)
  const ekle = zaman[0] !== 0;
  const t0 = ekle ? [0, ...zaman] : zaman;
  const r0 = ekle ? [riskAltinda[0], ...riskAltinda] : riskAltinda;
  const d0 = ekle ? [0, ...olay] : olay;
  const c0 = ekle ? [0, ...sansur] : sansur;
  const s0 = ekle ? [1, ...sagkalim] : sagkalim;
  const se0 = ekle ? [0, ...logSH] : logSH;
  const a0 = ekle ? [1, ...alt] : alt;
  const u0 = ekle ? [1, ...ust] : ust;
  const sonZaman = zaman[zaman.length - 1];
  const istenen = Array.from(new Set(zamanlar)).sort((a, b) => a - b);
  const atilan = istenen.filter((t) => t > sonZaman);
  if (atilan.length) uyarilar.push(`Son izlem süresinden (${sonZaman}) sonraki zamanlar için sağkalım kestirilemez: ${atilan.join(', ')}.`);
  const sonuc = [];
  let oncekiI1 = 0;
  for (const t of istenen) {
    if (t > sonZaman) continue;
    let i1 = 0; // t'ye eşit ya da küçük süre sayısı
    let i2 = 0; // t'den küçük süre sayısı
    for (let m = 0; m < t0.length; m++) {
      if (t0[m] <= t) i1++;
      if (t0[m] < t) i2++;
    }
    const satir = Math.max(1, i1) - 1;
    let olaySay = 0;
    let sansurSay = 0;
    for (let m = oncekiI1; m < i1; m++) {
      olaySay += d0[m];
      sansurSay += c0[m];
    }
    oncekiI1 = i1;
    const S = s0[satir];
    sonuc.push({
      zaman: t,
      riskAltinda: i2 < r0.length ? r0[i2] : 0,
      olay: olaySay,
      sansur: sansurSay,
      sagkalim: S,
      sh: nullIse(se0[satir] * S),
      gaAlt: nullIse(a0[satir]),
      gaUst: nullIse(u0[satir]),
    });
  }
  return sonuc;
}

function kmOzet(e, n, z, duzey, gaTipi, ustSinir, zamanlar, uyarilar) {
  const tablo = e.zaman.map((t, i) => ({
    zaman: t,
    riskAltinda: e.riskAltinda[i],
    olay: e.olay[i],
    sansur: e.sansur[i],
    sagkalim: e.sagkalim[i],
    sh: nullIse(e.logSH[i] * e.sagkalim[i]),
    gaAlt: nullIse(e.alt[i]),
    gaUst: nullIse(e.ust[i]),
    kumulatifHazard: e.kumHaz[i],
  }));
  const olaySayisi = e.olay.reduce((a, b) => a + b, 0);
  const altS = e.alt.map(nullIse);
  const ustS = e.ust.map(nullIse);
  const medyan = { deger: minmin(e.sagkalim, e.zaman), gaAlt: minmin(altS, e.zaman), gaUst: minmin(ustS, e.zaman) };
  // quantile.survfit: x = (0, süreler), y = (0, 1 - S)
  const xq = [0, ...e.zaman];
  const yS = [0, ...e.sagkalim.map((s) => 1 - s)];
  const yAlt = [0, ...e.alt.map((s) => 1 - s)];
  const yUst = [0, ...e.ust.map((s) => 1 - s)];
  const ps = [0.25, 0.5, 0.75];
  const qS = findq(xq, yS, ps);
  const qAlt = findq(xq, yAlt, ps);
  const qUst = findq(xq, yUst, ps);
  const yuzdelikler = ps.map((p, i) => ({ p, deger: nullIse(qS[i]), gaAlt: nullIse(qAlt[i]), gaUst: nullIse(qUst[i]) }));
  const ko = kisitliOrtalama(e, ustSinir);
  const sonuc = {
    n,
    olaySayisi,
    sansurluSayisi: n - olaySayisi,
    tablo,
    medyan,
    yuzdelikler,
    kisitliOrtalama: { deger: ko.deger, sh: ko.sh, gaAlt: ko.deger - z * ko.sh, gaUst: ko.deger + z * ko.sh, ustSinir },
  };
  if (zamanlar) sonuc.zamanlarda = sagkalimZamanlarda(e, zamanlar, uyarilar);
  if (medyan.deger === null) uyarilar.push('Sağkalım oranı %50\'nin altına inmediği için medyan sağkalım süresi hesaplanamadı.');
  return sonuc;
}

function kmSecenek(secenek) {
  const { duzey = 0.95, gaTipi = 'log' } = secenek;
  gerekli(duzey > 0 && duzey < 1, 'Güven düzeyi 0 ile 1 arasında olmalıdır.');
  gerekli(['log', 'log-log', 'plain'].includes(gaTipi), "Güven aralığı türü 'log', 'log-log' ya da 'plain' olmalıdır.");
  return { duzey, gaTipi, z: qnorm(1 - (1 - duzey) / 2) };
}

/**
 * Kaplan-Meier sağkalım kestirimi (R: survfit(Surv(sure, olay) ~ 1)).
 *
 * - Basamak tablosu her farklı süre için (sansürlü süreler dahil, survfit gibi): riskteki sayı, olay, sansür,
 *   sağkalım, Greenwood standart hatası (sh = S·√Σ d/(n(n−d)), SPSS'in "Std. Error"ı ile aynı) ve güven aralığı,
 *   Nelson-Aalen kümülatif hazardı.
 * - Güven aralığı türü varsayılan olarak R'deki gibi 'log' (exp(log S ± z·σ)); 'log-log' ve 'plain'
 *   (S ± z·SH, SPSS'in kullandığı doğrusal aralık) seçilebilir.
 * - Medyan ve GA'sı: print(survfit) / summary(fit)$table ile aynı (güven bantlarının 0.5'i kestiği süreler,
 *   Brookmeyer-Crowley yaklaşımı). SPSS medyan GA'sını farklı (medyan ± 1.96·SH) hesaplar.
 * - Yüzdelikler (%25, %50, %75 → S = 0.75, 0.50, 0.25): quantile(fit) ile aynı.
 *   Not: S tam olarak 0.5'te kalıp izlem sonuna kadar düz giderse quantile() ile print() medyanı farklıdır
 *   (R'deki gibi).
 * - Kısıtlı ortalama sağkalım süresi (RMST) ve SH: summary(fit, rmean = ...)$table ile aynı. Üst sınır varsayılan
 *   olarak en uzun gözlenen süredir (R 'common'; SPSS ortalamayı da en uzun süreyle sınırlar).
 * - zamanlar verilirse belirli zamanlardaki sağkalım: summary(fit, times = zamanlar) ile aynı (olay ve sansür
 *   sayıları bir önceki istenen zamandan bu zamana kadar olanlardır).
 *
 * @param {number[]} sure izlem süreleri (≥ 0)
 * @param {(0|1|boolean)[]} olay 1 = olay gerçekleşti, 0 = sansürlü
 * @param {{duzey?:number, gaTipi?:'log'|'log-log'|'plain', zamanlar?:number[], rmstUstSinir?:number}} [secenek]
 */
export function kaplanMeier(sure, olay, secenek = {}) {
  sureDenetle(sure);
  const d = olayDiziye(olay, sure.length);
  const { duzey, gaTipi, z } = kmSecenek(secenek);
  const t = zamanDuzelt(sure);
  const uyarilar = [];
  const e = kmEgrisi(t, d, z, gaTipi);
  const sonZaman = e.zaman[e.zaman.length - 1];
  const ustSinir = secenek.rmstUstSinir ?? sonZaman;
  gerekli(ustSinir >= 0, 'Kısıtlı ortalama için üst sınır negatif olamaz.');
  const ozet = kmOzet(e, sure.length, z, duzey, gaTipi, ustSinir, secenek.zamanlar, uyarilar);
  if (ozet.olaySayisi === 0) uyarilar.push('Hiç olay gözlenmedi; sağkalım eğrisi 1\'de sabit.');
  return {
    yontem: 'kaplan-meier',
    ad: 'Kaplan-Meier sağkalım analizi',
    duzey,
    gaTipi,
    ...ozet,
    uyarilar,
  };
}

/**
 * Gruplara göre Kaplan-Meier (R: survfit(Surv(sure, olay) ~ grup)).
 * Her grup için kaplanMeier() ile aynı alanlar döner. Kısıtlı ortalamanın üst sınırı varsayılan olarak
 * tüm gruplardaki en uzun süredir (R rmean = 'common'); rmstUstSinir: 'bireysel' her grubun kendi en uzun süresini
 * (R 'individual'), sayı ise o sınırı kullanır.
 *
 * @param {number[]} sure
 * @param {(0|1|boolean)[]} olay
 * @param {(number|string)[]} grup
 * @param {{duzey?:number, gaTipi?:string, zamanlar?:number[], rmstUstSinir?:number|'ortak'|'bireysel', duzeyler?:Array}} [secenek]
 */
export function kaplanMeierGruplu(sure, olay, grup, secenek = {}) {
  sureDenetle(sure);
  const d = olayDiziye(olay, sure.length);
  gerekli(grup && grup.length === sure.length, 'Süre ve grup değişkenlerinin uzunlukları eşit olmalıdır.');
  const { duzey, gaTipi, z } = kmSecenek(secenek);
  const t = zamanDuzelt(sure);
  const { duzeyler, indeks } = gruplariAyir(grup, secenek.duzeyler);
  const egriler = duzeyler.map((_, g) => {
    const ti = [];
    const di = [];
    for (let i = 0; i < t.length; i++) if (indeks[i] === g) {
      ti.push(t[i]);
      di.push(d[i]);
    }
    return { ti, di, e: ti.length ? kmEgrisi(ti, di, z, gaTipi) : null };
  });
  const sonlar = egriler.filter((x) => x.e).map((x) => x.e.zaman[x.e.zaman.length - 1]);
  const ust = secenek.rmstUstSinir ?? 'ortak';
  const uyarilar = [];
  const gruplar = egriler.map((x, g) => {
    if (!x.e) return { grup: duzeyler[g], n: 0, olaySayisi: 0, sansurluSayisi: 0, uyarilar: ['Bu grupta gözlem yok.'] };
    const gu = [];
    const ustSinir = ust === 'ortak' ? Math.max(...sonlar) : ust === 'bireysel' ? x.e.zaman[x.e.zaman.length - 1] : ust;
    gerekli(typeof ustSinir === 'number' && ustSinir >= 0, "rmstUstSinir 'ortak', 'bireysel' ya da negatif olmayan bir sayı olmalıdır.");
    const ozet = kmOzet(x.e, x.ti.length, z, duzey, gaTipi, ustSinir, secenek.zamanlar, gu);
    return { grup: duzeyler[g], ...ozet, uyarilar: gu };
  });
  const toplamOlay = gruplar.reduce((a, g) => a + g.olaySayisi, 0);
  return {
    yontem: 'kaplan-meier-gruplu',
    ad: 'Gruplara göre Kaplan-Meier sağkalım analizi',
    duzey,
    gaTipi,
    n: sure.length,
    olaySayisi: toplamOlay,
    sansurluSayisi: sure.length - toplamOlay,
    gruplar,
    uyarilar,
  };
}

// ---------- Log-rank ailesi ----------

const AGIRLIK_ADLARI = {
  'log-rank': 'Log-rank (Mantel-Cox) testi',
  breslow: 'Breslow (genelleştirilmiş Wilcoxon) testi',
  'tarone-ware': 'Tarone-Ware testi',
  'peto-peto': 'Peto-Peto testi',
  'fleming-harrington': 'Fleming-Harrington G(ρ) testi',
};

// survdiff.fit karşılığı: ağırlıklı gözlenen, beklenen ve varyans matrisi
function logRankHesap(t, d, indeks, k, agirlik, rho) {
  const n = t.length;
  const sira = Array.from({ length: n }, (_, i) => i).sort((a, b) => t[a] - t[b] || a - b);
  // Zaman blokları (artan)
  const bloklar = [];
  let i = 0;
  while (i < n) {
    let j = i;
    while (j < n && t[sira[j]] === t[sira[i]]) j++;
    bloklar.push([i, j]);
    i = j;
  }
  const gozlenen = new Array(k).fill(0);
  const beklenen = new Array(k).fill(0);
  const olaySay = new Array(k).fill(0);
  const V = Array.from({ length: k }, () => new Array(k).fill(0));
  const risk = new Array(k).fill(0);
  for (let m = 0; m < n; m++) risk[indeks[m]]++;
  let km = 1; // havuzlanmış KM, sol-sürekli S(t-)
  for (const [bas, son] of bloklar) {
    const nRisk = n - bas;
    const dk = new Array(k).fill(0);
    let olum = 0;
    for (let m = bas; m < son; m++) {
      const p = sira[m];
      if (d[p] === 1) {
        dk[indeks[p]]++;
        olum++;
      }
    }
    if (olum > 0) {
      let w;
      if (agirlik === 'log-rank') w = 1;
      else if (agirlik === 'breslow') w = nRisk;
      else if (agirlik === 'tarone-ware') w = Math.sqrt(nRisk);
      else w = rho === 0 ? 1 : km ** rho;
      for (let g = 0; g < k; g++) {
        gozlenen[g] += w * dk[g];
        olaySay[g] += dk[g];
        beklenen[g] += (w * olum * risk[g]) / nRisk;
      }
      if (nRisk > 1) {
        const kt = (w * w * olum * (nRisk - olum)) / (nRisk * (nRisk - 1));
        for (let a = 0; a < k; a++) {
          const tmp = risk[a] * kt;
          V[a][a] += tmp;
          for (let b = 0; b < k; b++) V[a][b] -= (tmp * risk[b]) / nRisk;
        }
      }
      km *= (nRisk - olum) / nRisk;
    }
    for (let m = bas; m < son; m++) risk[indeks[sira[m]]]--;
  }
  return { gozlenen, beklenen, olaySay, V };
}

// (O-E)' V⁻ (O-E): beklenen > 0 olan gruplardan ilki atılarak (R survdiff)
function kikareHesap(fark, V, beklenen) {
  const tut = [];
  for (let g = 0; g < fark.length; g++) if (beklenen[g] > 0) tut.push(g);
  const sd = tut.length - 1;
  if (tut.length < 2) return { kikare: 0, sd };
  const idx = tut.slice(1);
  const A = idx.map((a) => idx.map((b) => V[a][b]));
  const b = idx.map((a) => fark[a]);
  const x = simetrikCoz(A, b);
  let kikare = 0;
  for (let m = 0; m < idx.length; m++) kikare += x[m] * b[m];
  return { kikare, sd };
}

// Simetrik sistem çözümü (Gauss eliminasyonu, kısmi pivot). Tekil ise hata.
function simetrikCoz(A, b) {
  const n = b.length;
  const M = A.map((s, i) => [...s, b[i]]);
  for (let c = 0; c < n; c++) {
    let p = c;
    for (let r = c + 1; r < n; r++) if (Math.abs(M[r][c]) > Math.abs(M[p][c])) p = r;
    if (Math.abs(M[p][c]) < 1e-300 || !Number.isFinite(M[p][c])) throw new IstatistikHatasi('Log-rank varyans matrisi tekil; gruplar karşılaştırılamıyor.');
    [M[c], M[p]] = [M[p], M[c]];
    for (let r = c + 1; r < n; r++) {
      const f = M[r][c] / M[c][c];
      for (let s = c; s <= n; s++) M[r][s] -= f * M[c][s];
    }
  }
  const x = new Array(n).fill(0);
  for (let r = n - 1; r >= 0; r--) {
    let s = M[r][n];
    for (let c = r + 1; c < n; c++) s -= M[r][c] * x[c];
    x[r] = s / M[r][r];
  }
  return x;
}

function agirlikSecenek(secenek) {
  const agirlik = secenek.agirlik ?? 'log-rank';
  gerekli(agirlik in AGIRLIK_ADLARI, "Ağırlık 'log-rank', 'breslow', 'tarone-ware', 'peto-peto' ya da 'fleming-harrington' olmalıdır.");
  let rho = 0;
  if (agirlik === 'peto-peto') rho = 1;
  if (agirlik === 'fleming-harrington') {
    rho = secenek.rho ?? 1;
    gerekli(typeof rho === 'number' && rho >= 0, 'Fleming-Harrington ρ parametresi negatif olmayan bir sayı olmalıdır.');
  }
  return { agirlik, rho };
}

function logRankHazirla(sure, olay, grup, secenek) {
  sureDenetle(sure);
  const d = olayDiziye(olay, sure.length);
  gerekli(grup && grup.length === sure.length, 'Süre ve grup değişkenlerinin uzunlukları eşit olmalıdır.');
  const t = zamanDuzelt(sure);
  const { duzeyler, indeks } = gruplariAyir(grup, secenek.duzeyler);
  return { t, d, duzeyler, indeks };
}

/**
 * Sağkalım eğrilerinin karşılaştırılması: ağırlıklı log-rank ailesi.
 *
 * Her olay zamanında Σ w·(d_g − d·n_g/n) farkları ve hipergeometrik varyans Σ w²·d(n−d)/(n−1)·(n_g/n)(δ − n_h/n)
 * toplanır; χ² = (O−E)' V⁻¹ (O−E), sd = grup sayısı − 1 (beklenen değeri 0 olan gruplar atılır, R gibi).
 * - 'log-rank' (w = 1): R survdiff(rho = 0) ve SPSS "Log Rank (Mantel-Cox)" ile aynı.
 * - 'breslow' (w = riskteki kişi sayısı, Gehan-Breslow genelleştirilmiş Wilcoxon): SPSS "Breslow".
 * - 'tarone-ware' (w = √riskteki): SPSS "Tarone-Ware".
 * - 'peto-peto' (w = havuzlanmış KM S(t−)): R survdiff(rho = 1).
 * - 'fleming-harrington' (w = S(t−)^ρ, secenek.rho): R survdiff(rho = ρ).
 * gruplar[].gozlenen ve .beklenen ağırlıklıdır (survdiff gibi); olaySayisi ağırlıksız olay sayısıdır.
 *
 * @param {number[]} sure
 * @param {(0|1|boolean)[]} olay
 * @param {(number|string)[]} grup
 * @param {{agirlik?:string, rho?:number, duzeyler?:Array}} [secenek]
 */
export function logRank(sure, olay, grup, secenek = {}) {
  const { agirlik, rho } = agirlikSecenek(secenek);
  const { t, d, duzeyler, indeks } = logRankHazirla(sure, olay, grup, secenek);
  const k = duzeyler.length;
  if (k < 2) throw new IstatistikHatasi('Sağkalım eğrilerini karşılaştırmak için en az iki grup gerekir.');
  const h = logRankHesap(t, d, indeks, k, agirlik, rho);
  const fark = h.gozlenen.map((o, g) => o - h.beklenen[g]);
  const { kikare, sd } = kikareHesap(fark, h.V, h.beklenen);
  const uyarilar = [];
  if (h.beklenen.some((e) => e === 0)) uyarilar.push('Bazı grupların beklenen olay sayısı 0; bu gruplar teste katılmadı.');
  const nG = new Array(k).fill(0);
  for (const g of indeks) nG[g]++;
  if (h.olaySay.some((o) => o === 0)) uyarilar.push('Bazı gruplarda hiç olay gözlenmedi.');
  return {
    yontem: agirlik,
    ad: AGIRLIK_ADLARI[agirlik],
    istatistik: kikare,
    istatistikAdi: 'χ²',
    sd,
    p: sd > 0 ? pchisq(kikare, sd, false) : null,
    rho: agirlik === 'peto-peto' || agirlik === 'fleming-harrington' ? rho : undefined,
    gruplar: duzeyler.map((g, i) => ({
      grup: g,
      n: nG[i],
      olaySayisi: h.olaySay[i],
      gozlenen: h.gozlenen[i],
      beklenen: h.beklenen[i],
    })),
    varyans: h.V,
    n: sure.length,
    uyarilar,
  };
}

/**
 * Sıralı gruplar için eğilim (trend) testi: U = Σ c_g (O_g − E_g), χ² = U² / (c' V c), sd = 1.
 * Ağırlıklar logRank() ile aynıdır. Skorlar varsayılan olarak düzey sırası 1, 2, ..., k'dır
 * (secenek.skorlar ile değiştirilebilir; düzeylerin sırası secenek.duzeyler ile verilebilir).
 * SPSS'in "Linear trend for factor levels" seçeneğine karşılık gelir (SPSS çıktısıyla ayrıca doğrulanmamıştır).
 */
export function logRankTrend(sure, olay, grup, secenek = {}) {
  const { agirlik, rho } = agirlikSecenek(secenek);
  const { t, d, duzeyler, indeks } = logRankHazirla(sure, olay, grup, secenek);
  const k = duzeyler.length;
  if (k < 2) throw new IstatistikHatasi('Eğilim testi için en az iki grup gerekir.');
  const c = secenek.skorlar ? Array.from(secenek.skorlar) : duzeyler.map((_, i) => i + 1);
  gerekli(c.length === k && c.every(Number.isFinite), 'Skor sayısı grup sayısına eşit olmalıdır.');
  const h = logRankHesap(t, d, indeks, k, agirlik, rho);
  let U = 0;
  for (let g = 0; g < k; g++) U += c[g] * (h.gozlenen[g] - h.beklenen[g]);
  let v = 0;
  for (let a = 0; a < k; a++) for (let b = 0; b < k; b++) v += c[a] * h.V[a][b] * c[b];
  gerekli(v > 0, 'Eğilim testi varyansı sıfır; test yapılamıyor.');
  const kikare = (U * U) / v;
  return {
    yontem: `${agirlik}-trend`,
    ad: `${AGIRLIK_ADLARI[agirlik]} — doğrusal eğilim`,
    istatistik: kikare,
    istatistikAdi: 'χ²',
    sd: 1,
    p: pchisq(kikare, 1, false),
    U,
    varyansU: v,
    skorlar: c,
    duzeyler,
    n: sure.length,
    uyarilar: [],
  };
}

/**
 * Grupların ikişer ikişer karşılaştırılması (SPSS "Pairwise over strata"): her çift için yalnızca o iki grubun
 * verisiyle logRank() yapılır. secenek.pDuzeltme ('yok' varsayılan, SPSS gibi; 'bonferroni', 'holm', ...)
 * ile çoklu karşılaştırma düzeltmesi uygulanır (R p.adjust).
 * @returns {{karsilastirmalar: {grup1, grup2, istatistik, sd, p, pDuzeltilmis}[]}}
 */
export function ciftliLogRank(sure, olay, grup, secenek = {}) {
  const { agirlik } = agirlikSecenek(secenek);
  sureDenetle(sure);
  olayDiziye(olay, sure.length);
  const { duzeyler, indeks } = gruplariAyir(grup, secenek.duzeyler);
  gerekli(duzeyler.length >= 2, 'İkili karşılaştırma için en az iki grup gerekir.');
  const yontem = secenek.pDuzeltme ?? 'yok';
  const karsilastirmalar = [];
  for (let a = 0; a < duzeyler.length; a++) {
    for (let b = a + 1; b < duzeyler.length; b++) {
      const s = [];
      const o = [];
      const g = [];
      for (let i = 0; i < sure.length; i++) {
        if (indeks[i] === a || indeks[i] === b) {
          s.push(sure[i]);
          o.push(olay[i]);
          g.push(indeks[i]);
        }
      }
      const r = logRank(s, o, g, { ...secenek, duzeyler: [a, b] });
      karsilastirmalar.push({ grup1: duzeyler[a], grup2: duzeyler[b], istatistik: r.istatistik, sd: r.sd, p: r.p });
    }
  }
  const pd = pDuzelt(karsilastirmalar.map((k) => k.p), yontem);
  karsilastirmalar.forEach((k, i) => (k.pDuzeltilmis = pd[i]));
  return {
    yontem: `${agirlik}-ikili`,
    ad: `${AGIRLIK_ADLARI[agirlik]} — ikili karşılaştırmalar`,
    pDuzeltme: yontem,
    karsilastirmalar,
    uyarilar: [],
  };
}
