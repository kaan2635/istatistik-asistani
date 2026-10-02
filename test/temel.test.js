import { test } from 'node:test';
import assert from 'node:assert/strict';
import { pDuzelt } from '../src/cekirdek/duzeltme.js';
import * as B from '../src/cekirdek/betimsel.js';
import * as M from '../src/cekirdek/matris.js';
import { referans, yakin, yakinDerin } from './yardimci.js';

const R = referans('temel');

test('p düzeltmeleri (R p.adjust)', () => {
  for (const [ad, grup] of Object.entries(R.duzeltme)) {
    const p = ad === 'p1' ? [0.01, 0.02, 0.03, 0.04, 0.05] : [0.0001, 0.2, 0.004, 0.03, 0.5, 0.012, 0.9, 0.04, 0.04];
    for (const [yontem, beklenen] of Object.entries(grup)) {
      yakinDerin(pDuzelt(p, yontem), beklenen, 1e-12, 1e-15, `${ad} ${yontem}`);
    }
  }
  // eksik değerler yerinde kalır
  assert.deepEqual(pDuzelt([0.01, null, 0.02], 'bonferroni'), [0.02, null, 0.04]);
});

test('betimsel istatistikler', () => {
  for (const [ad, r] of Object.entries(R.betimsel)) {
    const x = r.x;
    const d = B.betimsel(x);
    yakin(d.ortalama, r.ortalama, 1e-13, 1e-13, `${ad} ortalama`);
    yakin(d.standartSapma, r.ss, 1e-12, 1e-13, `${ad} ss`);
    yakin(B.medyan(x), r.medyan, 1e-13, 1e-13, `${ad} medyan`);
    yakin(d.carpiklik, r.carpiklik, 1e-11, 1e-13, `${ad} çarpıklık`);
    yakin(d.basiklik, r.basiklik, 1e-11, 1e-13, `${ad} basıklık`);
    yakin(d.kirpilmisOrtalama, r.kirpilmis, 1e-13, 1e-13, `${ad} kırpılmış`);
    yakin(d.gaAlt, r.ga[0], 1e-12, 1e-13, `${ad} GA alt`);
    yakin(d.gaUst, r.ga[1], 1e-12, 1e-13, `${ad} GA üst`);
    const p = [0.05, 0.25, 0.5, 0.75, 0.95];
    for (const [tip, anahtar] of [[6, 'q6'], [7, 'q7'], [2, 'q2']]) {
      p.forEach((pp, i) => yakin(B.yuzdelik(x, pp, tip), r[anahtar][i], 1e-13, 1e-13, `${ad} yüzdelik tip ${tip} p=${pp}`));
    }
    yakinDerin(Array.from(B.siralar(x).sira), r.sira, 0, 0, `${ad} sıra`);
  }
});

test('matris işlemleri', () => {
  const r = R.matris;
  const ekk = M.enKucukKareler(r.X, r.y);
  assert.equal(ekk.rank, 4);
  assert.deepEqual(ekk.bagimli, [3]); // 4. sütun = 2*wt + 1 doğrusal bağımlı (R'de NA)
  yakinDerin(ekk.katsayi, r.katsayi, 1e-10, 1e-12, 'EKK katsayıları');
  yakinDerin(M.ters(r.A), r.Ainv, 1e-9, 1e-14, 'ters');
  yakinDerin(M.simetrikTers(r.A), r.Ainv, 1e-9, 1e-14, 'simetrik ters');
  yakin(M.determinant(r.A), r.det, 1e-10, 0, 'determinant');
  yakinDerin(M.simetrikOzdeger(r.R).degerler, r.ozdeger, 1e-12, 1e-14, 'özdeğerler');
});
