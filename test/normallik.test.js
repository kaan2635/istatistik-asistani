import { test } from 'node:test';
import assert from 'node:assert/strict';
import { andersonDarling, carpiklikBasiklikZ, dagostinoPearson, lilliefors, logPnorm, shapiroWilk } from '../src/cekirdek/normallik.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin } from './yardimci.js';

const R = referans('normallik');
const veriler = Object.entries(R.testler);

test('Shapiro-Wilk (R shapiro.test, swilk.c)', () => {
  let say = 0;
  for (const [ad, r] of veriler) {
    if (!r.sw) continue;
    const s = shapiroWilk(r.x);
    yakin(s.istatistik, r.sw.W, 1e-12, 1e-15, `${ad} W`);
    // p, Royston'un normal yaklaşımından gelir; aynı işlemler yapıldığından göreli 1e-11 içinde kalır
    yakin(s.p, r.sw.p, 1e-11, 1e-300, `${ad} p`);
    assert.equal(s.n, r.n);
    assert.equal(s.istatistikAdi, 'W');
    say++;
  }
  assert.ok(say >= 25);
});

test('Shapiro-Wilk: n = 3 kesin p, sınırlar ve hatalar', () => {
  // n = 3 için W ∈ [0.75, 1]; eşit aralıklı veride W = 1 → p = 1
  yakin(shapiroWilk([1, 2, 3]).p, 1, 1e-12, 1e-12, 'eşit aralık');
  yakin(shapiroWilk([1, 1, 2]).p, 0, 0, 1e-12, 'W = 0.75');
  assert.throws(() => shapiroWilk([1, 2]), IstatistikHatasi);
  assert.throws(() => shapiroWilk([4, 4, 4, 4]), /Tüm değerler aynı/);
  assert.throws(() => shapiroWilk(new Array(5001).fill(0).map((_, i) => i)), /5000/);
  assert.throws(() => shapiroWilk([1, 2, NaN, 4]), IstatistikHatasi);
  // ölçekten bağımsızlık
  const x = R.testler.sleep.x;
  yakin(shapiroWilk(x.map((v) => v * 1000 + 7)).istatistik, shapiroWilk(x).istatistik, 1e-12, 0, 'doğrusal dönüşüm');
});

test('Kolmogorov-Smirnov / Lilliefors (nortest::lillie.test)', () => {
  let say = 0;
  for (const [ad, r] of veriler) {
    if (!r.ks) continue;
    const s = lilliefors(r.x);
    yakin(s.istatistik, r.ks.D, 1e-12, 1e-15, `${ad} D`);
    yakin(s.p, r.ks.p, 1e-11, 1e-300, `${ad} p`);
    assert.equal(s.pSpssSiniri, s.p >= 0.2, `${ad} SPSS sınırı`);
    say++;
  }
  assert.ok(say >= 20);
  assert.throws(() => lilliefors([1, 2, 3, 4]), /en az 5/);
  assert.throws(() => lilliefors([2, 2, 2, 2, 2]), /aynı/);
});

test('Anderson-Darling (nortest::ad.test)', () => {
  let say = 0;
  for (const [ad, r] of veriler) {
    if (!r.ad) continue;
    const s = andersonDarling(r.x);
    yakin(s.istatistik, r.ad.A, 1e-12, 1e-14, `${ad} A`);
    yakin(s.p, r.ad.p, 1e-11, 1e-300, `${ad} p`);
    say++;
  }
  assert.ok(say >= 20);
  // A²* ≥ 10 dalı (3.7e-24) üstel örneklemde sınanır
  assert.equal(andersonDarling(R.testler.exp1000.x).p, 3.7e-24);
  assert.throws(() => andersonDarling([1, 2, 3, 4, 5, 6, 7]), /en az 8/);
});

test("D'Agostino-Pearson (moments::agostino.test, anscombe.test, fBasics::dagoTest)", () => {
  let say = 0;
  for (const [ad, r] of veriler) {
    if (!r.skew) continue;
    const s = dagostinoPearson(r.x);
    yakin(s.carpiklik.b1, r.skew.b1, 1e-11, 1e-14, `${ad} √b1`);
    yakin(s.carpiklik.z, r.skew.z, 1e-11, 1e-13, `${ad} z çarpıklık`);
    yakin(s.carpiklik.p, r.skew.p, 1e-11, 1e-15, `${ad} p çarpıklık`);
    if (r.kurt) {
      yakin(s.basiklik.b2, r.kurt.b2, 1e-12, 1e-14, `${ad} b2`);
      yakin(s.basiklik.z, r.kurt.z, 1e-11, 1e-13, `${ad} z basıklık`);
      yakin(s.basiklik.p, r.kurt.p, 1e-11, 1e-15, `${ad} p basıklık`);
    }
    if (r.omni) {
      yakin(s.istatistik, r.omni.K2, 1e-11, 1e-13, `${ad} K²`);
      // fBasics p'yi 1 − pchisq ile hesaplar (küçük p'lerde 1e-16 mutlak doğruluk); bizimki üst kuyruğu doğrudan hesaplar
      yakin(s.p, r.omni.p, 1e-9, 1e-15, `${ad} p omnibus`);
    }
    if (!r.omni) yakin(s.istatistik, s.carpiklik.z ** 2 + s.basiklik.z ** 2, 1e-14, 0, `${ad} K² tanım`);
    assert.equal(s.sd, 2);
    say++;
  }
  assert.ok(say >= 20);
  // moments::anscombe.test negatif tabanda NaN verir; fBasics gibi işaret korunur
  const iki = dagostinoPearson(R.testler.ikinokta50.x);
  assert.ok(Number.isFinite(iki.basiklik.z));
  assert.ok(dagostinoPearson(R.testler.n8.x).uyarilar.length > 0, 'n < 20 uyarısı');
  assert.throws(() => dagostinoPearson([1, 2, 3, 4, 5, 6, 7]), /en az 8/);
});

test('SPSS çarpıklık / basıklık z değerleri', () => {
  for (const [ad, r] of veriler) {
    if (!r.spss || r.x.length < 4) continue;
    const s = carpiklikBasiklikZ(r.x);
    yakin(s.carpiklik.deger, r.spss.g1, 1e-10, 1e-13, `${ad} G1`);
    yakin(s.carpiklik.sh, r.spss.seg1, 1e-14, 0, `${ad} SH G1`);
    yakin(s.basiklik.deger, r.spss.g2, 1e-10, 1e-13, `${ad} G2`);
    yakin(s.basiklik.sh, r.spss.seg2, 1e-14, 0, `${ad} SH G2`);
    yakin(s.carpiklik.z, r.spss.g1 / r.spss.seg1, 1e-10, 1e-13, `${ad} z G1`);
    yakin(s.basiklik.z, r.spss.g2 / r.spss.seg2, 1e-10, 1e-13, `${ad} z G2`);
  }
  assert.throws(() => carpiklikBasiklikZ([1, 2, 3]), /en az 4/);
  assert.throws(() => carpiklikBasiklikZ([3, 3, 3, 3, 3]), /aynı/);
});

test('log Φ(z) (R pnorm(log.p = TRUE))', () => {
  for (const r of R.logp) yakin(logPnorm(r.z), r.lp, 1e-13, 1e-300, `logPnorm(${r.z})`);
});
