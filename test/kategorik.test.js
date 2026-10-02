import { test } from 'node:test';
import assert from 'node:assert/strict';
import * as K from '../src/cekirdek/kategorik.js';
import { fisherRxC, rastgeleTablo, rastgeleUretec } from '../src/cekirdek/fisher.js';
import { logFaktoriyel } from '../src/cekirdek/sayisal.js';
import { pnorm } from '../src/cekirdek/dagilim.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { inf, referans, yakin, yakinDerin } from './yardimci.js';

const R = referans('kategorik');
const altAd = (a) => ({ 'two.sided': 'iki-yonlu', greater: 'buyuk', less: 'kucuk' })[a];

test('Ki-kare: R chisq.test, coin::lbl_test ve elle hesap', () => {
  R.kikare.forEach((r, i) => {
    const ad = `kikare[${i}]`;
    const s = K.kiKare(r.tablo);
    yakin(s.pearson.istatistik, r.pearson, 1e-12, 1e-14, `${ad} χ²`);
    assert.equal(s.pearson.sd, r.sd);
    yakin(s.pearson.p, r.p, 1e-10, 1e-300, `${ad} p`);
    yakinDerin(s.beklenen, r.beklenen, 1e-13, 1e-14, `${ad} beklenen`);
    yakinDerin(s.kalintilar.standart, r.standart, 1e-12, 1e-13, `${ad} standart artık`);
    yakinDerin(s.kalintilar.duzeltilmis, r.duzeltilmis, 1e-12, 1e-13, `${ad} düzeltilmiş artık`);
    yakin(s.olabilirlikOrani.istatistik, r.G2, 1e-12, 1e-14, `${ad} G²`);
    yakin(s.olabilirlikOrani.p, r.pG2, 1e-10, 1e-300, `${ad} G² p`);
    yakin(s.dogrusalIliski.istatistik, r.M2, 1e-12, 1e-14, `${ad} M²`);
    yakin(s.dogrusalIliski.istatistik, r.M2coin, 1e-10, 1e-13, `${ad} M² (coin)`);
    yakin(s.dogrusalIliski.p, r.pM2, 1e-10, 1e-300, `${ad} M² p`);
    yakin(s.etkiler.cramerV, r.cramerV, 1e-12, 1e-15, `${ad} Cramér V`);
    yakin(s.etkiler.kontenjansKatsayisi, r.C, 1e-12, 1e-15, `${ad} C`);
    yakin(s.etkiler.phi, r.phi, 1e-12, 1e-15, `${ad} phi`);
    assert.equal(s.beklenenKucuk5.sayi, r.kucuk5, `${ad} beklenen < 5`);
    yakin(s.beklenenKucuk5.minimum, r.minE, 1e-13, 1e-14, `${ad} en küçük beklenen`);
    if (r.yates !== undefined && r.yates !== null) {
      yakin(s.yates.istatistik, r.yates, 1e-12, 1e-14, `${ad} Yates`);
      yakin(s.yates.p, r.pYates, 1e-10, 1e-300, `${ad} Yates p`);
      yakin(s.fisher.p, r.fisherP, 1e-11, 1e-300, `${ad} Fisher`);
      yakin(s.fisher.pKucuk, r.fisherKucuk, 1e-11, 1e-300, `${ad} Fisher küçük`);
      yakin(s.fisher.pBuyuk, r.fisherBuyuk, 1e-11, 1e-300, `${ad} Fisher büyük`);
      const y = K.kiKare(r.tablo, { yates: true });
      assert.equal(y.yontem, 'yates-ki-kare');
      yakin(y.p, r.pYates, 1e-10, 1e-300, `${ad} Yates ana p`);
    } else assert.equal(s.yates, null);
    // düzeltilmiş artık p değerleri
    s.kalintilar.duzeltilmis.forEach((satir, a) => satir.forEach((z, b) => yakin(s.kalintilar.p[a][b], Math.min(1, 2 * pnorm(-Math.abs(z))), 1e-14, 0, 'artık p')));
  });
});

test('Ki-kare: boş satır/sütun atılır, uyarılar', () => {
  const s = K.kiKare([[10, 0, 5], [0, 0, 0], [3, 0, 8]]);
  assert.equal(s.gozlenen.length, 2);
  assert.equal(s.gozlenen[0].length, 2);
  assert.ok(s.uyarilar.some((u) => u.includes('satır')));
  assert.ok(s.uyarilar.some((u) => u.includes('sütun')));
  const k = K.kiKare([[1, 2], [3, 1]]);
  assert.ok(k.uyarilar.some((u) => u.includes('Fisher')));
});

function unirootYakin(a, b, mesaj) {
  if (!Number.isFinite(b) || b === 0) {
    assert.equal(a, b, mesaj);
    return;
  }
  const fark = Math.min(Math.abs(a - b), Math.abs(1 / a - 1 / b));
  assert.ok(fark <= 2.5e-4, `${mesaj}: ${a} vs ${b}`);
}

test('Fisher 2 × 2: R fisher.test ile karşılaştırma (p, koşullu MLE ve kesin GA)', () => {
  R.fisher22.forEach((r, i) => {
    const ad = `fisher22[${i}]`;
    const s = K.fisherKesin(r.tablo, { duzey: r.duzey });
    yakin(s.p, r.p, 1e-11, 1e-300, `${ad} p`);
    yakin(s.pKucuk, r.pKucuk, 1e-11, 1e-300, `${ad} p küçük`);
    yakin(s.pBuyuk, r.pBuyuk, 1e-11, 1e-300, `${ad} p büyük`);
    assert.equal(s.pTekYonlu, Math.min(s.pKucuk, s.pBuyuk));
    // yüksek hassasiyetli R hesabı (uniroot tol = 1e-14)
    yakin(s.oddsOrani.tahmin, inf(r.tahmin), 1e-8, 1e-12, `${ad} MLE`);
    yakin(s.oddsOrani.ga.alt, inf(r.ga[0]), 1e-8, 1e-12, `${ad} GA alt`);
    yakin(s.oddsOrani.ga.ust, inf(r.ga[1]), 1e-8, 1e-12, `${ad} GA üst`);
    // R fisher.test kökleri uniroot(tol ≈ 1.22e-4) ile ψ ∈ (0, 1) ya da 1/ψ ∈ (0, 1) ölçeğinde bulur
    unirootYakin(s.oddsOrani.tahmin, inf(r.tahminR), `${ad} MLE (fisher.test)`);
    unirootYakin(s.oddsOrani.ga.alt, inf(r.gaR[0]), `${ad} GA alt (fisher.test)`);
    unirootYakin(s.oddsOrani.ga.ust, inf(r.gaR[1]), `${ad} GA üst (fisher.test)`);
    const b = K.fisherKesin(r.tablo, { duzey: r.duzey, alternatif: 'buyuk' });
    yakin(b.p, r.pBuyuk, 1e-11, 1e-300, `${ad} büyük p`);
    yakin(b.oddsOrani.ga.alt, inf(r.gaBuyuk[0]), 1e-8, 1e-12, `${ad} tek yönlü GA alt`);
    assert.equal(b.oddsOrani.ga.ust, Infinity);
    const k = K.fisherKesin(r.tablo, { duzey: r.duzey, alternatif: 'kucuk' });
    assert.equal(k.oddsOrani.ga.alt, 0);
    yakin(k.oddsOrani.ga.ust, inf(r.gaKucuk[1]), 1e-8, 1e-12, `${ad} tek yönlü GA üst`);
  });
});

test('Fisher-Freeman-Halton r × c: R fisher.test ile karşılaştırma', () => {
  // R'nin FEXACT'i kendi içinde ~1e-9 (büyük tablolarda ~5e-8) göreli doğrulukta çalışır; bizim sonuçlarımız sütun
  // sırası ve birleştirme toleransı değiştirildiğinde 1e-11 içinde aynı kalır ve küçük tablolarda kaba kuvvetle
  // 1e-12 içinde aynıdır (aşağıdaki test). 4 × 4, N = 300 tablosunda FEXACT farkı 4.2e-8'dir.
  R.fisherRC.forEach((r, i) => {
    if (r.p === null) return;
    const N = r.tablo.flat().reduce((a, b) => a + b, 0);
    const s = K.fisherKesin(r.tablo, { zamanSiniri: 120000 });
    assert.equal(s.pYontemi, 'kesin', `fisherRC[${i}] kesin hesaplanmalı`);
    yakin(s.p, r.p, N >= 250 && r.tablo.length >= 4 ? 6e-8 : 2e-9, 1e-14, `fisherRC[${i}] p`);
  });
});

/** Tüm tabloları sayarak kesin p (küçük tablolar için bağımsız denetim) */
function kabaKuvvetFisher(t) {
  const r = t.length;
  const c = t[0].length;
  const Rs = t.map((s) => s.reduce((a, b) => a + b, 0));
  const Cs = t[0].map((_, j) => t.reduce((a, s) => a + s[j], 0));
  const N = Rs.reduce((a, b) => a + b, 0);
  let sabit = -logFaktoriyel(N);
  for (const v of Rs) sabit += logFaktoriyel(v);
  for (const v of Cs) sabit += logFaktoriyel(v);
  const logP = (tab) => {
    let v = sabit;
    for (const s of tab) for (const x of s) v -= logFaktoriyel(x);
    return v;
  };
  const gozlenen = logP(t);
  let p = 0;
  let toplam = 0;
  const cur = Array.from({ length: r }, () => new Array(c).fill(0));
  const kol = Cs.slice();
  const ozyineli = (i, j, kalan) => {
    if (i === r - 1) {
      for (let jj = 0; jj < c; jj++) cur[i][jj] = kol[jj];
      const lp = logP(cur);
      toplam += Math.exp(lp);
      if (lp <= gozlenen + 1e-7) p += Math.exp(lp);
      return;
    }
    if (j === c - 1) {
      if (kalan > kol[j]) return;
      cur[i][j] = kalan;
      kol[j] -= kalan;
      ozyineli(i + 1, 0, Rs[i + 1]);
      kol[j] += kalan;
      return;
    }
    for (let v = 0; v <= Math.min(kalan, kol[j]); v++) {
      cur[i][j] = v;
      kol[j] -= v;
      ozyineli(i, j + 1, kalan - v);
      kol[j] += v;
    }
  };
  ozyineli(0, 0, Rs[0]);
  return { p, toplam };
}

test('Fisher r × c: kaba kuvvet sayımıyla karşılaştırma (rastgele küçük tablolar, kesin ve gevşek sınırlar)', () => {
  const rnd = rastgeleUretec(99);
  let sayi = 0;
  while (sayi < 120) {
    const r = 2 + Math.floor(rnd() * 3);
    const c = 2 + Math.floor(rnd() * 3);
    const t = Array.from({ length: r }, () => Array.from({ length: c }, () => Math.floor(rnd() * (rnd() < 0.3 ? 2 : 6))));
    const N = t.flat().reduce((a, b) => a + b, 0);
    if (r * c > 9 && N > 30) continue;
    if (t.filter((s) => s.some((v) => v > 0)).length < 2) continue;
    sayi++;
    const k = kabaKuvvetFisher(t);
    yakin(k.toplam, 1, 1e-10, 0, 'olasılık toplamı');
    yakin(fisherRxC(t, { yontem: 'kesin' }).p, Math.min(1, k.p), 1e-10, 1e-14, `kaba ${JSON.stringify(t)}`);
    yakin(fisherRxC(t, { yontem: 'kesin', kesinSinir: false }).p, Math.min(1, k.p), 1e-10, 1e-14, `kaba (gevşek) ${JSON.stringify(t)}`);
  }
});

test('Fisher r × c: Monte Carlo (Patefield) kesin değerle uyumlu', () => {
  const r = R.fisherMC;
  const s = K.fisherKesin(r.tablo, { yontem: 'monte-carlo', B: 100000, tohum: 7 });
  assert.equal(s.pYontemi, 'monte-carlo');
  assert.ok(Math.abs(s.p - r.p) < 4 * s.sh + 1e-5, `MC p ${s.p} ± ${s.sh}, kesin ${r.p}`);
  // aynı tohum → aynı sonuç
  assert.equal(K.fisherKesin(r.tablo, { yontem: 'monte-carlo', B: 2000, tohum: 3 }).p, K.fisherKesin(r.tablo, { yontem: 'monte-carlo', B: 2000, tohum: 3 }).p);
  // kesin hesap da R ile aynı
  const k = K.fisherKesin(r.tablo, { zamanSiniri: 60000 });
  if (k.pYontemi === 'kesin') yakin(k.p, r.p, 1e-9, 1e-14, 'MC tablosu kesin p');
  // bütçe çok küçükse otomatik olarak Monte Carlo'ya geçer
  const b = K.fisherKesin(r.tablo, { islemSiniri: 10, B: 5000 });
  assert.equal(b.pYontemi, 'monte-carlo');
  assert.ok(b.uyarilar.some((u) => u.includes('Monte Carlo')));
  assert.throws(() => K.fisherKesin(r.tablo, { yontem: 'kesin', islemSiniri: 10 }), IstatistikHatasi);
});

test('Patefield üreteci doğru koşullu dağılımı verir (2 × 2 hipergeometrik)', () => {
  const rnd = rastgeleUretec(11);
  const B = 40000;
  const say = new Map();
  for (let b = 0; b < B; b++) {
    const t = rastgeleTablo([5, 7], [6, 6], rnd);
    assert.deepEqual(t[0].reduce((a, v) => a + v, 0), 5);
    assert.deepEqual(t[0][0] + t[1][0], 6);
    say.set(t[0][0], (say.get(t[0][0]) || 0) + 1);
  }
  for (let x = 0; x <= 5; x++) {
    const p = Math.exp(logFaktoriyel(6) * 2 + logFaktoriyel(5) + logFaktoriyel(7) - logFaktoriyel(12) - logFaktoriyel(x) - logFaktoriyel(5 - x) - logFaktoriyel(6 - x) - logFaktoriyel(1 + x));
    const g = (say.get(x) || 0) / B;
    assert.ok(Math.abs(g - p) < 5 * Math.sqrt((p * (1 - p)) / B) + 1e-4, `x=${x}: ${g} vs ${p}`);
  }
});

test('McNemar ve McNemar-Bowker: R mcnemar.test ve binom.test', () => {
  R.mcnemar.forEach((r, i) => {
    const a = K.mcnemar(r.tablo, { yontem: 'asimptotik' });
    yakin(a.kiKare.istatistik, r.chiDuz, 1e-12, 1e-14, `mcnemar[${i}] χ² düzeltmeli`);
    yakin(a.p, r.pDuz, 1e-10, 1e-300, `mcnemar[${i}] p düzeltmeli`);
    const b = K.mcnemar(r.tablo, { yontem: 'asimptotik', sureklilik: false });
    yakin(b.kiKare.istatistik, r.chi, 1e-12, 1e-14, `mcnemar[${i}] χ²`);
    yakin(b.p, r.p, 1e-10, 1e-300, `mcnemar[${i}] p`);
    yakin(a.pKesin, r.pKesin, 1e-11, 1e-300, `mcnemar[${i}] kesin`);
    const o = K.mcnemar(r.tablo);
    const bc = r.tablo[0][1] + r.tablo[1][0];
    assert.equal(o.pYontemi, bc < 25 ? 'kesin' : 'asimptotik');
  });
  R.bowker.forEach((r, i) => {
    const s = K.mcnemarBowker(r.tablo);
    yakin(s.istatistik, r.chi, 1e-12, 1e-14, `bowker[${i}]`);
    assert.equal(s.sd, r.sd);
    yakin(s.p, r.p, 1e-10, 1e-300, `bowker[${i}] p`);
  });
  const bos = K.mcnemarBowker(R.bowkerBos.tablo);
  yakin(bos.istatistik, R.bowkerBos.chi, 1e-12, 1e-14, 'bowker boş çift');
  assert.equal(bos.sd, R.bowkerBos.sd);
  yakin(bos.p, R.bowkerBos.p, 1e-10, 1e-300, 'bowker boş çift p');
  assert.ok(bos.uyarilar.length > 0);
  // 2 × 2'de Bowker = düzeltmesiz McNemar
  yakin(K.mcnemarBowker(R.mcnemar[0].tablo).istatistik, R.mcnemar[0].chi, 1e-12, 0, 'bowker 2×2');
});

test('Cochran Q: R rstatix::cochran_qtest ve elle', () => {
  R.cochranQ.forEach((r, i) => {
    const s = K.cochranQ(r.matris);
    yakin(s.istatistik, r.Q, 1e-12, 1e-14, `cochranQ[${i}]`);
    yakin(s.istatistik, r.Qr, 1e-12, 1e-14, `cochranQ[${i}] rstatix`);
    assert.equal(s.sd, r.sd);
    yakin(s.p, r.p, 1e-10, 1e-300, `cochranQ[${i}] p`);
    yakin(s.p, r.pr, 1e-10, 1e-300, `cochranQ[${i}] p rstatix`);
  });
});

test('Cochran-Armitage: R prop.trend.test', () => {
  R.cochranArmitage.forEach((r, i) => {
    const t = [r.x, r.x.map((v, j) => r.n[j] - v)];
    const s = K.cochranArmitage(t, r.skor);
    yakin(s.istatistik, r.chi, 1e-11, 1e-14, `CA[${i}] χ²`);
    yakin(s.p, r.p, 1e-10, 1e-300, `CA[${i}] p`);
    yakin(s.z * s.z, r.chi, 1e-11, 1e-14, `CA[${i}] z²`);
  });
});

test('Mantel-Haenszel, ortak OR (RGB) ve Breslow-Day: R mantelhaen.test ve elle', () => {
  R.mantelHaenszel.forEach((r, i) => {
    const ad = `MH[${i}]`;
    const s = K.mantelHaenszel(r.tablolar);
    yakin(s.istatistik, r.chi, 1e-12, 1e-14, `${ad} χ²`);
    yakin(s.p, r.p, 1e-10, 1e-300, `${ad} p`);
    const d = K.mantelHaenszel(r.tablolar, { sureklilik: false });
    yakin(d.istatistik, r.chiDuzsuz, 1e-12, 1e-14, `${ad} χ² düzeltmesiz`);
    yakin(d.p, r.pDuzsuz, 1e-10, 1e-300, `${ad} p düzeltmesiz`);
    yakin(s.cochran.istatistik, r.cochran, 1e-12, 1e-14, `${ad} Cochran`);
    yakin(s.ortakOddsOrani.tahmin, r.or, 1e-12, 1e-14, `${ad} OR`);
    yakin(s.ga.alt, r.ga[0], 1e-11, 1e-14, `${ad} GA alt`);
    yakin(s.ga.ust, r.ga[1], 1e-11, 1e-14, `${ad} GA üst`);
    yakin(s.breslowDay.istatistik, r.BD, 1e-9, 1e-12, `${ad} Breslow-Day`);
    assert.equal(s.breslowDay.sd, r.bdSd, `${ad} BD sd`);
    yakin(s.tarone.istatistik, r.tarone, 1e-9, 1e-12, `${ad} Tarone`);
  });
});

test('Odds oranı, göreli risk, risk farkı: elle hesap ve R epitools', () => {
  R.risk.forEach((r, i) => {
    const ad = `risk[${i}]`;
    const s = K.oddsOrani(r.tablo, { duzey: r.duzey });
    yakin(s.oddsOrani.tahmin, r.or, 1e-13, 1e-14, `${ad} OR`);
    yakin(s.oddsOrani.ga.alt, r.orGA[0], 1e-12, 1e-14, `${ad} OR GA alt`);
    yakin(s.oddsOrani.ga.ust, r.orGA[1], 1e-12, 1e-14, `${ad} OR GA üst`);
    yakin(s.goreliRisk.tahmin, r.rr, 1e-13, 1e-14, `${ad} RR`);
    yakin(s.goreliRisk.ga.alt, r.rrGA[0], 1e-12, 1e-14, `${ad} RR GA alt`);
    yakin(s.goreliRisk.ga.ust, r.rrGA[1], 1e-12, 1e-14, `${ad} RR GA üst`);
    yakin(s.riskFarki.tahmin, r.rd, 1e-13, 1e-15, `${ad} RD`);
    yakin(s.riskFarki.wald.alt, r.waldGA[0], 1e-12, 1e-14, `${ad} Wald alt`);
    yakin(s.riskFarki.wald.ust, r.waldGA[1], 1e-12, 1e-14, `${ad} Wald üst`);
    yakin(s.riskFarki.newcombe.alt, r.newcombeGA[0], 1e-12, 1e-14, `${ad} Newcombe alt`);
    yakin(s.riskFarki.newcombe.ust, r.newcombeGA[1], 1e-12, 1e-14, `${ad} Newcombe üst`);
    if (r.epOR) {
      yakin(s.oddsOrani.tahmin, r.epOR[0], 1e-12, 1e-14, `${ad} epitools OR`);
      yakin(s.oddsOrani.ga.alt, r.epOR[1], 1e-12, 1e-14, `${ad} epitools OR alt`);
      yakin(s.oddsOrani.ga.ust, r.epOR[2], 1e-12, 1e-14, `${ad} epitools OR üst`);
      yakin(s.goreliRisk.tahmin, r.epRR[0], 1e-12, 1e-14, `${ad} epitools RR`);
      yakin(s.goreliRisk.ga.alt, r.epRR[1], 1e-12, 1e-14, `${ad} epitools RR alt`);
      yakin(s.goreliRisk.ga.ust, r.epRR[2], 1e-12, 1e-14, `${ad} epitools RR üst`);
    } else assert.ok(s.uyarilar.length > 0, `${ad} sıfır hücre uyarısı`);
  });
});

test('Binom testi: R binom.test (p ve Clopper-Pearson)', () => {
  R.binom.forEach((r, i) => {
    const s = K.binomTesti(r.x, r.n, r.p0, { alternatif: altAd(r.alt), duzey: r.duzey });
    yakin(s.p, r.p, 1e-11, 1e-300, `binom[${i}] p`);
    yakin(s.ga.alt, r.ga[0], 1e-10, 1e-14, `binom[${i}] GA alt`);
    yakin(s.ga.ust, r.ga[1], 1e-10, 1e-14, `binom[${i}] GA üst`);
  });
});

test('Oran güven aralıkları: Wilson, Clopper-Pearson, Wald, Agresti-Coull', () => {
  R.oranGA.forEach((r, i) => {
    const sec = { duzey: r.duzey };
    const w = K.oranGA(r.x, r.n, { ...sec, yontem: 'wilson' });
    yakin(w.alt, r.wilson[0], 1e-12, 1e-14, `GA[${i}] Wilson alt`);
    yakin(w.ust, r.wilson[1], 1e-12, 1e-14, `GA[${i}] Wilson üst`);
    const cp = K.oranGA(r.x, r.n, { ...sec, yontem: 'clopper-pearson' });
    yakin(cp.alt, r.cp[0], 1e-10, 1e-14, `GA[${i}] CP alt`);
    yakin(cp.ust, r.cp[1], 1e-10, 1e-14, `GA[${i}] CP üst`);
    const wa = K.oranGA(r.x, r.n, { ...sec, yontem: 'wald' });
    yakin(wa.alt, r.wald[0], 1e-12, 1e-14, `GA[${i}] Wald alt`);
    yakin(wa.ust, r.wald[1], 1e-12, 1e-14, `GA[${i}] Wald üst`);
    const ac = K.oranGA(r.x, r.n, { ...sec, yontem: 'agresti-coull' });
    yakin(ac.alt, Math.max(0, r.ac[0]), 1e-12, 1e-14, `GA[${i}] AC alt`);
    yakin(ac.ust, Math.min(1, r.ac[1]), 1e-12, 1e-14, `GA[${i}] AC üst`);
  });
});

test('İki oran testi: R prop.test', () => {
  R.ikiOran.forEach((r, i) => {
    const s = K.ikiOranTesti(r.x1, r.n1, r.x2, r.n2, { sureklilik: r.sureklilik, alternatif: altAd(r.alt), duzey: r.duzey });
    yakin(s.istatistik, r.chi, 1e-12, 1e-14, `ikiOran[${i}] χ²`);
    yakin(s.p, r.p, 1e-10, 1e-300, `ikiOran[${i}] p`);
    yakin(s.ga.alt, r.ga[0], 1e-12, 1e-14, `ikiOran[${i}] GA alt`);
    yakin(s.ga.ust, r.ga[1], 1e-12, 1e-14, `ikiOran[${i}] GA üst`);
  });
});

test('Uyum iyiliği ki-kare: R chisq.test(x, p)', () => {
  R.uyum.forEach((r, i) => {
    const s = K.uyumIyiligiKiKare(r.x, r.oran);
    yakin(s.istatistik, r.chi, 1e-12, 1e-14, `uyum[${i}] χ²`);
    assert.equal(s.sd, r.sd);
    yakin(s.p, r.p, 1e-10, 1e-300, `uyum[${i}] p`);
    s.kategoriler.forEach((k, j) => {
      yakin(k.beklenen, r.beklenen[j], 1e-13, 1e-13, `uyum[${i}] beklenen`);
      yakin(k.standartArtik, r.artik[j], 1e-12, 1e-14, `uyum[${i}] artık`);
    });
  });
  assert.ok(K.uyumIyiligiKiKare([10, 20, 30], [1, 1, 2]).uyarilar.some((u) => u.includes('ölçeklendi')));
});

test('Sütun oranları z testi (SPSS) ve harf gösterimi', () => {
  R.sutunOran.forEach((r, i) => {
    const s = K.sutunOranKarsilastirma(r.tablo);
    s.satirlar.forEach((satir, a) => {
      const beklenen = r.satirlar[a];
      // jsonlite: tek elemanlı vektör → sayı, boş vektör → null
      const dizi = (v) => (v === null || v === undefined ? [] : Array.isArray(v) ? v : [v]);
      const z = dizi(beklenen.z);
      const p = dizi(beklenen.p);
      const pa = dizi(beklenen.pAdj);
      assert.equal(satir.karsilastirmalar.length, z.length, `sutun[${i}] satır ${a} karşılaştırma sayısı`);
      satir.karsilastirmalar.forEach((k, j) => {
        yakin(k.z, z[j], 1e-12, 1e-14, `sutun[${i}] z`);
        yakin(k.p, p[j], 1e-10, 1e-300, `sutun[${i}] p`);
        yakin(k.pDuzeltilmis, pa[j], 1e-10, 1e-300, `sutun[${i}] p Bonferroni`);
      });
    });
  });
  // Elle denetlenen harf örneği: 3 sütun; 1-3 farklı, 1-2 ve 2-3 farklı değil → a, ab, b
  const t = [[40, 30, 20], [60, 70, 80]];
  const s = K.sutunOranKarsilastirma(t, { alfa: 0.05 });
  const ilk = s.satirlar[0];
  const anlamli = ilk.karsilastirmalar.filter((k) => k.anlamli).map((k) => `${k.sutun1}-${k.sutun2}`);
  assert.deepEqual(anlamli, ['0-2']);
  assert.deepEqual(ilk.harfler, ['a', 'ab', 'b']);
  // hiç fark yoksa hepsi aynı harf; oranı 0 olan sütun karşılaştırma dışı
  const u = K.sutunOranKarsilastirma([[10, 11, 0], [20, 19, 30]]);
  assert.deepEqual(u.satirlar[0].harfler, ['a', 'a', null]);
  assert.deepEqual(u.satirlar[0].karsilastirmaDisi, [2]);
});

test('Çapraz tablo', () => {
  const a = ['E', 'K', 'K', 'E', 'K', null, 'E', 'Ç'];
  const b = [1, 2, 2, 3, 1, 2, NaN, 3];
  const t = K.caprazTablo(a, b);
  assert.deepEqual(t.satirlar, ['Ç', 'E', 'K']); // Türkçe alfabetik
  assert.deepEqual(t.sutunlar, [1, 2, 3]);
  assert.deepEqual(t.tablo, [[0, 0, 1], [1, 0, 1], [1, 2, 0]]);
  assert.equal(t.N, 6);
  assert.equal(t.eksik, 2);
  assert.deepEqual(t.satirToplam, [1, 2, 3]);
  assert.deepEqual(t.sutunToplam, [2, 2, 2]);
  const s = K.caprazTablo(a, b, { satirSirasi: ['K', 'E', 'Ç', 'X'] });
  assert.deepEqual(s.tablo[3], [0, 0, 0]);
  assert.throws(() => K.caprazTablo(a, b, { satirSirasi: ['K'] }), IstatistikHatasi);
  assert.throws(() => K.caprazTablo([1, 2], [1]), IstatistikHatasi);
});

test('Hata durumları', () => {
  const hata = (f, re) => assert.throws(f, (e) => e instanceof IstatistikHatasi && (!re || re.test(e.message)));
  hata(() => K.kiKare([[1, 2, 3]]), /en az 2 satır/);
  hata(() => K.kiKare([[1, -2], [3, 4]]), /negatif/);
  hata(() => K.kiKare([[1, 2], [3]]), /aynı uzunlukta/);
  hata(() => K.fisherKesin([[1.5, 2], [3, 4]]), /tamsayı/);
  hata(() => K.fisherKesin([[1, 2, 3]]), /en az 2 satır/);
  hata(() => K.fisherKesin([[1, 2, 3], [4, 5, 6]], { alternatif: 'buyuk' }), /iki yönlü/);
  hata(() => K.mcnemar([[1, 2, 3], [1, 2, 3], [1, 2, 3]]), /2 × 2/);
  hata(() => K.mcnemarBowker([[1, 2, 3], [1, 2, 3]]), /kare/);
  hata(() => K.mcnemarBowker([[5, 0], [0, 7]]), /boş/);
  hata(() => K.cochranQ([[0, 2], [1, 0]]), /0 ya da 1/);
  hata(() => K.cochranQ([[1, 1], [0, 0]]), /aynı/);
  hata(() => K.cochranArmitage([[1, 2, 3]]), /2 × k/);
  hata(() => K.mantelHaenszel([[[1, 2], [3, 4]]]), /en az 2 katman/);
  hata(() => K.mantelHaenszel([[[1, 0], [0, 0]], [[1, 2], [3, 4]]]), /1'den büyük/);
  hata(() => K.oddsOrani([[1, 2, 3], [4, 5, 6]]), /2 × 2/);
  hata(() => K.binomTesti(5, 3), /0 ile n/);
  hata(() => K.binomTesti(1, 3, 1.5), /0 ile 1/);
  hata(() => K.oranGA(3, 10, { yontem: 'xyz' }), /Bilinmeyen/);
  hata(() => K.ikiOranTesti(0, 10, 0, 12), /aynı kategoride/);
  hata(() => K.uyumIyiligiKiKare([1, 2], [0.5]), /eşit olmalıdır/);
  hata(() => K.fisherKesin([[1, 2], [3, 4]], { duzey: 1.2 }), /Güven düzeyi/);
});

test('Uç durumlar', () => {
  // Fisher 2 × 2, sıfır toplamlı satır → p = 1
  const f = K.fisherKesin([[0, 0], [3, 4]]);
  assert.equal(f.p, 1);
  // Fisher r × c, tek dolu satır → p = 1
  assert.equal(fisherRxC([[0, 0, 0], [1, 2, 3]]).p, 1);
  // McNemar b + c = 0
  const m = K.mcnemar([[5, 0], [0, 7]]);
  assert.equal(m.p, 1);
  // Mantel-Haenszel: bilgi taşımayan katman Breslow-Day'den çıkarılır
  const mh = K.mantelHaenszel([[[0, 6], [0, 5]], [[3, 3], [0, 6]], [[6, 0], [2, 4]], [[5, 1], [6, 0]]]);
  assert.equal(mh.breslowDay.sd, 2);
  assert.ok(mh.uyarilar.some((u) => u.includes('Breslow-Day')));
  // risk ölçüleri: sıfır hücre düzeltmesi bildirilir
  const r = K.oddsOrani([[0, 10], [5, 5]]);
  assert.ok(r.oddsOrani.duzeltme);
  assert.ok(r.goreliRisk.duzeltme);
  assert.ok(!r.goreliRiskSutun2.duzeltme);
});
