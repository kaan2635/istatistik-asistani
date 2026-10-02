// Tanı testi performansı (2×2 tablo): duyarlılık, özgüllük, kestirim değerleri, olabilirlik oranları,
// doğruluk, Youden J, tanısal odds oranı, F1 ve Cohen kappa — her biri güven aralığıyla.
//
// Yöntemler (R'de doğrulandı):
//   oranlar      : Clopper-Pearson "kesin" (varsayılan; epiR::epi.tests(method = 'exact'), binom.test, MedCalc)
//                  ya da Wilson skor aralığı (prop.test(correct = FALSE))
//   LR+ / LR−    : log yöntemi (Simel ve ark. 1991; epiR, MedCalc)
//   DOR          : log yöntemi, SE = √(1/a + 1/b + 1/c + 1/d) (epiR)
//   Youden J     : Wald (varsayılan) ya da epiR'nin uç değer yöntemi
//   F1           : delta yöntemi (Wald)
//   kappa        : Fleiss, Cohen ve Everitt (1969) asimptotik SH (SPSS Crosstabs "Asymp. Std. Error")
//   prevalansa göre KD: Mercaldo, Lau ve Zhou (2007) logit aralığı (MedCalc)

import { pnorm, qbeta, qnorm } from './dagilim.js';
import { IstatistikHatasi, gerekli } from './hata.js';

const bos = { deger: null, gaAlt: null, gaUst: null };

/**
 * Oran ve güven aralığı.
 * @param {number} x başarı sayısı
 * @param {number} n deneme sayısı
 * @param {number} duzey
 * @param {'clopper-pearson'|'wilson'} yontem
 */
export function oranGA(x, n, duzey = 0.95, yontem = 'clopper-pearson') {
  if (!(n > 0)) return { ...bos };
  const p = x / n;
  const alfa = 1 - duzey;
  if (yontem === 'clopper-pearson') {
    const alt = x === 0 ? 0 : qbeta(alfa / 2, x, n - x + 1);
    const ust = x === n ? 1 : qbeta(1 - alfa / 2, x + 1, n - x);
    return { deger: p, gaAlt: alt, gaUst: ust };
  }
  if (yontem === 'wilson') {
    // R prop.test(correct = FALSE) ile aynı ifade
    const z = qnorm(1 - alfa / 2);
    const z22n = (z * z) / (2 * n);
    const kok = z * Math.sqrt((p * (1 - p)) / n + z22n / (2 * n));
    const ust = p >= 1 ? 1 : (p + z22n + kok) / (1 + 2 * z22n);
    const alt = p <= 0 ? 0 : (p + z22n - kok) / (1 + 2 * z22n);
    return { deger: p, gaAlt: alt, gaUst: ust };
  }
  throw new IstatistikHatasi("Oran güven aralığı yöntemi 'clopper-pearson' ya da 'wilson' olmalıdır.");
}

function logGA(tahmin, logSH, z) {
  if (!Number.isFinite(tahmin) || !(tahmin > 0) || !Number.isFinite(logSH)) return { deger: tahmin, gaAlt: null, gaUst: null };
  return { deger: tahmin, gaAlt: Math.exp(Math.log(tahmin) - z * logSH), gaUst: Math.exp(Math.log(tahmin) + z * logSH) };
}

const sinirla = (v, a, b) => Math.min(Math.max(v, a), b);

/**
 * Cohen kappa (2×2 ya da k×k tablo; satır = test, sütun = referans) ve asimptotik standart hataları.
 * SH (H0 varsayılmadan) Fleiss-Cohen-Everitt (1969) formülüdür ve GA'da kullanılır; z testi H0 (κ = 0) altındaki SH ile
 * yapılır (SPSS Crosstabs "Approx. T" ve "Approx. Sig.").
 */
export function cohenKappa(tablo, duzey = 0.95) {
  const k = tablo.length;
  let N = 0;
  for (const s of tablo) for (const v of s) N += v;
  gerekli(N > 0, 'Kappa için tabloda gözlem olmalıdır.');
  const p = tablo.map((s) => s.map((v) => v / N));
  const sat = p.map((s) => s.reduce((a, b) => a + b, 0));
  const sut = p[0].map((_, j) => p.reduce((a, s) => a + s[j], 0));
  let po = 0;
  let pe = 0;
  for (let i = 0; i < k; i++) {
    po += p[i][i];
    pe += sat[i] * sut[i];
  }
  if (pe === 1) return { deger: null, sh: null, sh0: null, z: null, p: null, gaAlt: null, gaUst: null };
  const kappa = (po - pe) / (1 - pe);
  let A = 0;
  for (let i = 0; i < k; i++) A += p[i][i] * (1 - (sat[i] + sut[i]) * (1 - kappa)) ** 2;
  let B = 0;
  for (let i = 0; i < k; i++) for (let j = 0; j < k; j++) if (i !== j) B += p[i][j] * (sut[i] + sat[j]) ** 2;
  B *= (1 - kappa) ** 2;
  const C = (kappa - pe * (1 - kappa)) ** 2;
  const sh = Math.sqrt(Math.max(A + B - C, 0) / (N * (1 - pe) ** 2));
  let s0 = 0;
  for (let i = 0; i < k; i++) s0 += sat[i] * sut[i] * (sat[i] + sut[i]);
  const sh0 = Math.sqrt((pe + pe * pe - s0) / (N * (1 - pe) ** 2));
  const z = qnorm(1 - (1 - duzey) / 2);
  const zIst = kappa / sh0;
  return {
    deger: kappa,
    sh,
    sh0,
    z: zIst,
    p: 2 * pnorm(-Math.abs(zIst)),
    gaAlt: kappa - z * sh,
    gaUst: kappa + z * sh,
  };
}

/**
 * Tanı testi performansı (2×2 tablo).
 *
 *                 Hasta (referans +)   Sağlam (referans −)
 *   Test +              dp (tp)              yp (fp)
 *   Test −              yn (fn)              dn (tn)
 *
 * Döndürülen her ölçüt {deger, gaAlt, gaUst}: duyarlilik, ozgulluk, pozitifKestirimDegeri (PPV),
 * negatifKestirimDegeri (NPV), dogruluk, prevalans (tablodaki), pozitifOlabilirlikOrani (LR+),
 * negatifOlabilirlikOrani (LR−), youdenJ, tanisalOddsOrani (DOR), f1, kappa (+ sh, z, p).
 * secenek.prevalans verilirse (0–1) duzeltilmis.{pozitifKestirimDegeri, negatifKestirimDegeri} Bayes kuralıyla
 * o prevalansa göre ve Mercaldo logit GA'sıyla hesaplanır (vaka-kontrol çalışmaları için).
 *
 * @param {number} tp doğru pozitif  @param {number} fp yanlış pozitif
 * @param {number} fn yanlış negatif @param {number} tn doğru negatif
 * @param {{prevalans?:number, duzey?:number, oranYontemi?:'clopper-pearson'|'wilson',
 *          youdenYontemi?:'wald'|'epiR', sifirDuzeltme?:boolean}} [secenek]
 *   sifirDuzeltme: herhangi bir hücre 0 ise DOR ve LR için tüm hücrelere 0.5 eklenir (Haldane-Anscombe; varsayılan kapalı, epiR gibi).
 */
export function taniTesti(tp, fp, fn, tn, secenek = {}) {
  for (const v of [tp, fp, fn, tn]) gerekli(Number.isInteger(v) && v >= 0, 'Tablo hücreleri negatif olmayan tam sayılar olmalıdır.');
  const { duzey = 0.95, oranYontemi = 'clopper-pearson', youdenYontemi = 'wald', sifirDuzeltme = false, prevalans = null } = secenek;
  gerekli(duzey > 0 && duzey < 1, 'Güven düzeyi 0 ile 1 arasında olmalıdır.');
  gerekli(['clopper-pearson', 'wilson'].includes(oranYontemi), "Oran güven aralığı yöntemi 'clopper-pearson' ya da 'wilson' olmalıdır.");
  gerekli(['wald', 'epiR'].includes(youdenYontemi), "Youden GA yöntemi 'wald' ya da 'epiR' olmalıdır.");
  const N = tp + fp + fn + tn;
  gerekli(N > 0, 'Tabloda hiç gözlem yok.');
  const z = qnorm(1 - (1 - duzey) / 2);
  const uyarilar = [];
  const hasta = tp + fn;
  const saglam = fp + tn;
  if (hasta === 0) uyarilar.push('Tabloda hasta (referans pozitif) yok; duyarlılık hesaplanamaz.');
  if (saglam === 0) uyarilar.push('Tabloda sağlam (referans negatif) yok; özgüllük hesaplanamaz.');
  if (tp + fp === 0) uyarilar.push('Test pozitif sonuç vermedi; pozitif kestirim değeri hesaplanamaz.');
  if (fn + tn === 0) uyarilar.push('Test negatif sonuç vermedi; negatif kestirim değeri hesaplanamaz.');

  const duyarlilik = oranGA(tp, hasta, duzey, oranYontemi);
  const ozgulluk = oranGA(tn, saglam, duzey, oranYontemi);
  const se = duyarlilik.deger;
  const sp = ozgulluk.deger;

  // Olabilirlik oranları ve DOR (sıfır hücre düzeltmesi isteğe bağlı)
  const sifirVar = tp === 0 || fp === 0 || fn === 0 || tn === 0;
  const ek = sifirDuzeltme && sifirVar ? 0.5 : 0;
  const a = tp + ek;
  const b = fp + ek;
  const c = fn + ek;
  const d = tn + ek;
  let lrArti = { ...bos };
  let lrEksi = { ...bos };
  if (hasta > 0 && saglam > 0) {
    const seL = a / (a + c);
    const spL = d / (b + d);
    lrArti = logGA(seL / (1 - spL), Math.sqrt(1 / a - 1 / (a + c) + 1 / b - 1 / (b + d)), z);
    lrEksi = logGA((1 - seL) / spL, Math.sqrt(1 / c - 1 / (a + c) + 1 / d - 1 / (b + d)), z);
  }
  const tanisalOddsOrani = logGA((a * d) / (b * c), Math.sqrt(1 / a + 1 / b + 1 / c + 1 / d), z);
  if (sifirVar && !sifirDuzeltme) uyarilar.push('Tabloda sıfır hücre var; olabilirlik oranları ve tanısal odds oranı sonsuz/tanımsız olabilir (sifirDuzeltme seçeneği 0.5 ekler).');
  if (sifirVar && sifirDuzeltme) uyarilar.push('Sıfır hücre nedeniyle olabilirlik oranları ve tanısal odds oranı için tüm hücrelere 0.5 eklendi.');

  // Youden J
  let youdenJ = { ...bos };
  if (hasta > 0 && saglam > 0) {
    const J = se + sp - 1;
    if (youdenYontemi === 'wald') {
      const shJ = Math.sqrt((se * (1 - se)) / hasta + (sp * (1 - sp)) / saglam);
      youdenJ = { deger: J, gaAlt: sinirla(J - z * shJ, -1, 1), gaUst: sinirla(J + z * shJ, -1, 1), sh: shJ };
    } else {
      const c1 = duyarlilik.gaAlt + ozgulluk.gaAlt - 1;
      const c2 = duyarlilik.gaUst + ozgulluk.gaUst - 1;
      youdenJ = { deger: J, gaAlt: Math.min(c1, c2), gaUst: Math.max(c1, c2) };
    }
  }

  // F1 (delta yöntemi): Var = 4 p_tp (p_fp + p_fn)(p_tp + p_fp + p_fn) / (N D⁴), D = 2 p_tp + p_fp + p_fn
  let f1 = { ...bos };
  if (2 * tp + fp + fn > 0) {
    const ptp = tp / N;
    const pfp = fp / N;
    const pfn = fn / N;
    const D = 2 * ptp + pfp + pfn;
    const F = (2 * ptp) / D;
    const shF = Math.sqrt((4 * ptp * (pfp + pfn) * (ptp + pfp + pfn)) / (N * D ** 4));
    f1 = { deger: F, gaAlt: sinirla(F - z * shF, 0, 1), gaUst: sinirla(F + z * shF, 0, 1), sh: shF };
  }

  const sonuc = {
    yontem: 'tani-testi',
    ad: 'Tanı testi performansı',
    tablo: { tp, fp, fn, tn },
    n: N,
    hastaSayisi: hasta,
    saglamSayisi: saglam,
    duzey,
    oranYontemi,
    prevalans: oranGA(hasta, N, duzey, oranYontemi),
    duyarlilik,
    ozgulluk,
    pozitifKestirimDegeri: oranGA(tp, tp + fp, duzey, oranYontemi),
    negatifKestirimDegeri: oranGA(tn, fn + tn, duzey, oranYontemi),
    dogruluk: oranGA(tp + tn, N, duzey, oranYontemi),
    pozitifOlabilirlikOrani: lrArti,
    negatifOlabilirlikOrani: lrEksi,
    youdenJ,
    tanisalOddsOrani,
    f1,
    kappa: cohenKappa([[tp, fp], [fn, tn]], duzey),
    uyarilar,
  };

  if (prevalans !== null && prevalans !== undefined) {
    gerekli(prevalans > 0 && prevalans < 1, 'Prevalans 0 ile 1 arasında (uçlar hariç) olmalıdır.');
    const P = prevalans;
    const ppv = (se * P) / (se * P + (1 - sp) * (1 - P));
    const npv = (sp * (1 - P)) / ((1 - se) * P + sp * (1 - P));
    const logit = (lp, v) => {
      if (!Number.isFinite(lp) || !Number.isFinite(v)) return { gaAlt: null, gaUst: null };
      const s = Math.sqrt(v);
      return { gaAlt: 1 / (1 + Math.exp(-(lp - z * s))), gaUst: 1 / (1 + Math.exp(-(lp + z * s))) };
    };
    const ppvGA = logit(Math.log((se * P) / ((1 - sp) * (1 - P))), (1 - se) / (se * hasta) + sp / ((1 - sp) * saglam));
    const npvGA = logit(Math.log((sp * (1 - P)) / ((1 - se) * P)), se / ((1 - se) * hasta) + (1 - sp) / (sp * saglam));
    if (ppvGA.gaAlt === null || npvGA.gaAlt === null) uyarilar.push('Duyarlılık ya da özgüllük 0 veya 1 olduğundan prevalansa göre kestirim değerlerinin logit güven aralığı hesaplanamadı.');
    sonuc.duzeltilmis = {
      prevalans: P,
      pozitifKestirimDegeri: { deger: ppv, ...ppvGA },
      negatifKestirimDegeri: { deger: npv, ...npvGA },
      dogruluk: { deger: se * P + sp * (1 - P), gaAlt: null, gaUst: null },
    };
  }
  return sonuc;
}
