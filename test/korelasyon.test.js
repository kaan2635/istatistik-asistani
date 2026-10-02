import { test } from 'node:test';
import assert from 'node:assert/strict';
import { ikiKorelasyonKarsilastir, kendallS, kendallTauB, kismiKorelasyon, korelasyonMatrisi, pearson, spearman } from '../src/cekirdek/korelasyon.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin } from './yardimci.js';

const R = referans('korelasyon');
const ALT = { 'two.sided': 'iki', greater: 'buyuk', less: 'kucuk' };
const ciftler = Object.entries(R.korelasyonlar);

test('Pearson korelasyonu (R cor.test)', () => {
  for (const [ad, r] of ciftler) {
    for (const [rAlt, alt] of Object.entries(ALT)) {
      const b = r[rAlt].pearson;
      const s = pearson(r.x, r.y, { alternatif: alt });
      const m = `${ad} ${rAlt}`;
      yakin(s.r, b.r, 1e-13, 1e-15, `${m} r`);
      yakin(s.rKare, b.r * b.r, 1e-12, 1e-15, `${m} r²`);
      assert.equal(s.sd, b.df);
      if (ad === 'mukemmel') {
        assert.equal(s.istatistik, Infinity);
        assert.equal(s.p, rAlt === 'less' ? 1 : 0);
        continue;
      }
      yakin(s.istatistik, b.t, 1e-11, 1e-14, `${m} t`);
      yakin(s.p, b.p, 1e-10, 1e-300, `${m} p`);
      if (rAlt === 'two.sided') {
        if (b.ga) {
          yakin(s.ga.alt, b.ga[0], 1e-12, 1e-14, `${m} GA alt`);
          yakin(s.ga.ust, b.ga[1], 1e-12, 1e-14, `${m} GA üst`);
        } else {
          // n = 3: R güven aralığı vermez
          assert.ok(Number.isNaN(s.ga.alt));
          assert.ok(s.uyarilar.length > 0);
        }
      }
    }
  }
  const m = pearson(R.korelasyonlar.mukemmel.x, R.korelasyonlar.mukemmel.y);
  assert.deepEqual([m.ga.alt, m.ga.ust], [1, 1]);
  assert.throws(() => pearson([1, 2, 3], [4, 4, 4]), /sabit/);
  assert.throws(() => pearson([1, 2], [3, 4]), /en az 3/);
  assert.throws(() => pearson([1, 2, 3], [3, 4]), /eşit/);
  assert.throws(() => pearson([1, 2, NaN], [3, 4, 5]), IstatistikHatasi);
  assert.throws(() => pearson([1, 2, 3], [3, 4, 5], { alternatif: 'x' }), /alternatif/);
});

test('Spearman korelasyonu (R cor.test(exact = FALSE), Bonett-Wright GA)', () => {
  for (const [ad, r] of ciftler) {
    for (const [rAlt, alt] of Object.entries(ALT)) {
      const b = r[rAlt].spearman;
      const s = spearman(r.x, r.y, { alternatif: alt });
      const m = `${ad} ${rAlt}`;
      yakin(s.rho, b.rho, 1e-13, 1e-15, `${m} ρ`);
      // R'nin S'si (n³ − n)(1 − ρ)/6; mükemmel uyumda 0
      yakin(s.S, b.S, 1e-11, 1e-9, `${m} S`);
      if (ad === 'mukemmel') {
        assert.equal(s.p, rAlt === 'less' ? 1 : 0);
        continue;
      }
      yakin(s.p, b.p, 1e-10, 1e-300, `${m} p`);
    }
    if (r.spearmanGA && ad !== 'mukemmel') {
      const s = spearman(r.x, r.y);
      yakin(s.ga.alt, r.spearmanGA[0], 1e-12, 1e-14, `${ad} GA alt`);
      yakin(s.ga.ust, r.spearmanGA[1], 1e-12, 1e-14, `${ad} GA üst`);
    }
  }
  assert.throws(() => spearman([1, 2, 3, 4], [5, 5, 5, 5]), /sabit/);
});

test('Kendall tau-b (R cor.test(exact = FALSE), bağ düzeltmeli varyans)', () => {
  for (const [ad, r] of ciftler) {
    for (const [rAlt, alt] of Object.entries(ALT)) {
      const b = r[rAlt].kendall;
      const s = kendallTauB(r.x, r.y, { alternatif: alt });
      const m = `${ad} ${rAlt}`;
      yakin(s.tau, b.tau, 1e-13, 1e-15, `${m} τ`);
      yakin(s.istatistik, b.z, 1e-12, 1e-14, `${m} z`);
      yakin(s.p, b.p, 1e-11, 1e-300, `${m} p`);
    }
  }
  assert.throws(() => kendallTauB([1, 2, 3], [7, 7, 7]), /sabit/);
});

test('Kendall S: O(n log n) algoritması kaba kuvvetle aynı', () => {
  const kaba = (x, y) => {
    let S = 0;
    for (let i = 0; i < x.length; i++) for (let j = i + 1; j < x.length; j++) S += Math.sign(x[i] - x[j]) * Math.sign(y[i] - y[j]);
    return S;
  };
  let tohum = 12345;
  const rast = () => {
    tohum = (tohum * 1103515245 + 12345) % 2147483648;
    return tohum / 2147483648;
  };
  for (const [n, duzey] of [[2, 3], [7, 2], [30, 4], [101, 10], [500, 1e6]]) {
    const x = Array.from({ length: n }, () => Math.floor(rast() * duzey));
    const y = Array.from({ length: n }, () => Math.floor(rast() * duzey));
    assert.equal(kendallS(x, y).S, kaba(x, y), `n = ${n}`);
  }
  for (const [ad, r] of ciftler) assert.equal(kendallS(r.x, r.y).S, kaba(r.x, r.y), ad);
});

test('kısmi korelasyon (ppcor::pcor.test)', () => {
  for (const [ad, r] of Object.entries(R.kismiler)) {
    const s = kismiKorelasyon(r.x, r.y, r.z);
    yakin(s.r, r.r, 1e-11, 1e-14, `${ad} r`);
    yakin(s.r, r.rArtik, 1e-11, 1e-14, `${ad} r (artıklar)`);
    yakin(s.istatistik, r.t, 1e-10, 1e-13, `${ad} t`);
    yakin(s.p, r.p, 1e-9, 1e-300, `${ad} p`);
    assert.equal(s.sd, r.n - 2 - r.gp);
    yakin(s.ga.alt, r.ga[0], 1e-11, 1e-14, `${ad} GA alt`);
    yakin(s.ga.ust, r.ga[1], 1e-11, 1e-14, `${ad} GA üst`);
  }
  assert.throws(() => kismiKorelasyon([1, 2, 3], [3, 1, 2], []), /kontrol/);
  assert.throws(() => kismiKorelasyon([1, 2, 3], [3, 1, 2], [[1, 2, 4]]), /en az 4/);
  // kontrol değişkeni x'i tamamen açıklıyor
  assert.throws(() => kismiKorelasyon([1, 2, 3, 4, 5], [2, 1, 4, 3, 5], [[2, 4, 6, 8, 10]]), /tamamen/);
  // bağımlı kontrol değişkenleri: uyarı ve tek kontrolle aynı sonuç
  const x = R.kismiler.mtcars1.x;
  const y = R.kismiler.mtcars1.y;
  const z = R.kismiler.mtcars1.z[0];
  const k = kismiKorelasyon(x, y, [z, z.map((v) => 2 * v + 1)]);
  yakin(k.r, R.kismiler.mtcars1.r, 1e-10, 0, 'bağımlı kontroller');
  assert.ok(k.uyarilar.length > 0);
});

test('korelasyon matrisi: ikili ve liste bazında silme (airquality)', () => {
  const veri = R.matrisler.veri;
  for (const yontem of ['pearson', 'spearman', 'kendall']) {
    const b = R.matrisler[yontem];
    const ikili = korelasyonMatrisi(veri, { yontem });
    const liste = korelasyonMatrisi(veri, { yontem, eksik: 'liste' });
    for (let i = 0; i < 4; i++) {
      for (let j = 0; j < 4; j++) {
        const m = `${yontem} [${i},${j}]`;
        yakin(ikili.r[i][j], b.ikili[i][j], 1e-12, 1e-15, `${m} r ikili`);
        assert.equal(ikili.n[i][j], b.nIkili[i][j], `${m} n ikili`);
        yakin(liste.r[i][j], b.liste[i][j], 1e-12, 1e-15, `${m} r liste`);
        assert.equal(liste.n[i][j], b.nListe, `${m} n liste`);
        if (i === j) {
          assert.equal(ikili.p[i][j], null);
          continue;
        }
        yakin(ikili.p[i][j], b.pIkili[i][j], 1e-9, 1e-300, `${m} p ikili`);
        yakin(liste.p[i][j], b.pListe[i][j], 1e-9, 1e-300, `${m} p liste`);
      }
    }
    assert.equal(ikili.yontem, yontem);
  }
  // sabit değişken: hücre null, hata fırlatılmaz
  const s = korelasyonMatrisi([[1, 2, 3, 4], [5, 5, 5, 5], [2, 1, 4, 3]], { adlar: ['a', 'b', 'c'] });
  assert.equal(s.r[0][1], null);
  assert.equal(s.p[1][2], null);
  assert.ok(Number.isFinite(s.r[0][2]));
  assert.deepEqual(s.adlar, ['a', 'b', 'c']);
  assert.throws(() => korelasyonMatrisi([[1, 2, 3]]), /en az 2/);
  assert.throws(() => korelasyonMatrisi([[1, 2, 3], [1, 2]]), /eşit/);
  assert.throws(() => korelasyonMatrisi([[1, 2, 3], [1, 2, 4]], { yontem: 'x' }), /yöntemi/);
});

test('iki bağımsız korelasyonun karşılaştırılması (Fisher z)', () => {
  for (const r of R.ikiKor) {
    const s = ikiKorelasyonKarsilastir(r.r1, r.n1, r.r2, r.n2);
    yakin(s.istatistik, r.z, 1e-13, 1e-15, 'z');
    yakin(s.p, r.p, 1e-12, 1e-300, 'p');
    yakin(s.etki.deger, r.q, 1e-13, 1e-15, 'q');
    yakin(ikiKorelasyonKarsilastir(r.r1, r.n1, r.r2, r.n2, { alternatif: 'buyuk' }).p, r.pBuyuk, 1e-12, 1e-300, 'p büyük');
  }
  assert.throws(() => ikiKorelasyonKarsilastir(1, 10, 0.2, 10), /−1 ile 1/);
  assert.throws(() => ikiKorelasyonKarsilastir(0.3, 3, 0.2, 10), /en az 4/);
});
