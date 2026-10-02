import { test } from 'node:test';
import assert from 'node:assert/strict';
import { dogrusalRegresyon, basitDogrusalRegresyon, hiyerarsikRegresyon, adimsalRegresyon } from '../src/cekirdek/regresyon.js';
import { modelMatrisi } from '../src/cekirdek/model.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin, yakinDerin } from './yardimci.js';

const R = referans('regresyon');
const V = R.veri;
const TOL = 1e-10;

const sur = (ad, degerler) => ({ ad, tur: 'surekli', degerler });
const kat = (ad, degerler, ek = {}) => ({ ad, tur: 'kategorik', degerler, ...ek });

/** dogrusalRegresyon sonucunu R lmOzet() çıktısıyla karşılaştırır */
function lmKarsilastir(s, r, etiket, tol = TOL) {
  const p = r.B.length;
  assert.equal(s.katsayilar.length, p, `${etiket}: katsayı sayısı`);
  assert.equal(s.n, r.n, `${etiket}: n`);
  const ym = Math.max(...r.uyum.map(Math.abs));
  s.katsayilar.forEach((k, j) => {
    const m = `${etiket} ${k.ad}`;
    yakin(k.B, r.B[j], tol, 1e-12 * ym, `${m} B`);
    yakin(k.SH, r.SH[j], tol, 0, `${m} SH`);
    yakin(k.t, r.t[j], tol, 1e-11, `${m} t`);
    yakin(k.p, r.p[j], 1e-9, 1e-300, `${m} p`);
    yakin(k.gaAlt, r.gaAlt[j], tol, 1e-12 * ym, `${m} GA alt`);
    yakin(k.gaUst, r.gaUst[j], tol, 1e-12 * ym, `${m} GA üst`);
    yakin(k.beta, r.beta[j], tol, 1e-12, `${m} beta`);
    yakin(k.VIF, r.VIF[j], 1e-9, 0, `${m} VIF`);
    if (r.VIF[j] !== null) yakin(k.tolerans, 1 / r.VIF[j], 1e-9, 0, `${m} tolerans`);
    yakin(k.sifirDereceKorelasyon, r.sifirDerece[j], tol, 1e-13, `${m} r0`);
    yakin(k.kismiKorelasyon, r.kismi[j], 1e-9, 1e-12, `${m} kısmi r`);
    yakin(k.yariKismiKorelasyon, r.yariKismi[j], 1e-9, 1e-12, `${m} yarı kısmi r`);
  });
  const o = s.modelOzeti;
  yakin(o.R2, r.R2, tol, 0, `${etiket} R2`);
  yakin(o.duzeltilmisR2, r.duzR2, tol, 0, `${etiket} düz. R2`);
  yakin(o.tahminSH, r.sigma, tol, 0, `${etiket} sigma`);
  yakin(o.F, r.F, tol, 0, `${etiket} F`);
  if (r.sd1 !== null) assert.equal(o.sd1, r.sd1, `${etiket} sd1`);
  assert.equal(o.sd2, r.sd2, `${etiket} sd2`);
  yakin(o.p, r.pF, 1e-9, 1e-300, `${etiket} p(F)`);
  yakin(o.AIC, r.AIC, tol, 0, `${etiket} AIC`);
  yakin(o.BIC, r.BIC, tol, 0, `${etiket} BIC`);
  yakin(o.logOlabilirlik, r.logLik, tol, 0, `${etiket} logLik`);
  yakin(s.anovaTablosu[0].KT, r.anovaKT[0], tol, 0, `${etiket} KT reg`);
  yakin(s.anovaTablosu[1].KT, r.anovaKT[1], tol, 0, `${etiket} KT artık`);
  const t = s.tanilama;
  yakinDerin(t.artiklar, r.artik, 1e-9, 1e-11 * ym, `${etiket} artık`);
  yakinDerin(t.uyumlanan, r.uyum, tol, 1e-12 * ym, `${etiket} uyum`);
  yakinDerin(t.studentArtiklar, r.rstandard, 1e-9, 1e-11, `${etiket} rstandard`);
  yakinDerin(t.silinmisStudentArtiklar, r.rstudent, 1e-9, 1e-11, `${etiket} rstudent`);
  yakinDerin(t.kaldirac, r.hat, tol, 1e-14, `${etiket} kaldıraç`);
  yakinDerin(t.cookD, r.cook, 1e-9, 1e-14, `${etiket} Cook`);
  yakinDerin(t.dffits, r.dffits, 1e-9, 1e-12, `${etiket} DFFITS`);
  yakin(t.durbinWatson, r.dw, tol, 0, `${etiket} DW`);
  yakin(t.breuschPagan.koenker.istatistik, r.bpK, 1e-9, 0, `${etiket} BP Koenker`);
  yakin(t.breuschPagan.koenker.p, r.bpKp, 1e-9, 1e-300, `${etiket} BP Koenker p`);
  assert.equal(t.breuschPagan.koenker.sd, r.bpSd, `${etiket} BP sd`);
  yakin(t.breuschPagan.klasik.istatistik, r.bpB, 1e-9, 0, `${etiket} BP klasik`);
  yakin(t.breuschPagan.klasik.p, r.bpBp, 1e-9, 1e-300, `${etiket} BP klasik p`);
}

function drop1Karsilastir(s, d, etiket) {
  assert.equal(s.terimTestleri.length, d.terim.length, `${etiket}: terim sayısı`);
  s.terimTestleri.forEach((t, k) => {
    assert.equal(t.sd, d.sd[k], `${etiket} ${t.ad} sd`);
    yakin(t.KT, d.KT[k], 1e-9, 0, `${etiket} ${t.ad} KT`);
    yakin(t.F, d.F[k], 1e-9, 0, `${etiket} ${t.ad} F`);
    yakin(t.p, d.p[k], 1e-9, 1e-300, `${etiket} ${t.ad} p`);
  });
}

const mt = V.mtcars;

test('model matrisi: gösterge kodlama, referans, etkileşim, eksik değer', () => {
  const mm = modelMatrisi(
    [sur('x', [1, 2, null, 4, 5, 6]), kat('g', ['a', 'b', 'c', 'a', '', 'c'], { referans: 'b' })],
    { etkilesimler: [['x', 'g']], ekDiziler: [[1, 2, 3, NaN, 5, 6]] },
  );
  assert.deepEqual(mm.satirlar, [0, 1, 5]);
  assert.deepEqual(mm.sutunAdlari, ['(Sabit)', 'x', 'g=a', 'g=c', 'x × g=a', 'x × g=c']);
  assert.deepEqual(mm.X, [
    [1, 1, 1, 0, 1, 0],
    [1, 2, 0, 0, 0, 0],
    [1, 6, 0, 1, 0, 6],
  ]);
  assert.deepEqual(mm.terimler.map((t) => t.sutunlar), [[1], [2, 3], [4, 5]]);
  assert.equal(mm.terimler[1].referans, 'b');
  assert.deepEqual(mm.terimler[1].frekanslar, [1, 1, 1]);
  assert.deepEqual(mm.ekler[0], [1, 2, 6]);
  assert.equal(mm.atilanSatir, 3);
  // Kategorik × kategorik: ilk bileşen en hızlı değişir (R model.matrix sırası)
  const mm2 = modelMatrisi([kat('a', ['1', '2', '3', '1']), kat('b', ['u', 'v', 'w', 'w'])], { etkilesimler: [['a', 'b']] });
  assert.deepEqual(mm2.sutunAdlari.slice(5), ['a=2 × b=v', 'a=3 × b=v', 'a=2 × b=w', 'a=3 × b=w']);
  // Hatalar
  assert.throws(() => modelMatrisi([kat('g', ['a', 'a', null])]), IstatistikHatasi);
  assert.throws(() => modelMatrisi([kat('g', ['a', 'b'], { referans: 'z' })]), IstatistikHatasi);
  assert.throws(() => modelMatrisi([sur('x', [1, 'abc'])]), IstatistikHatasi);
  assert.throws(() => modelMatrisi([sur('x', [1, 2])], { etkilesimler: [['x', 'z']] }), IstatistikHatasi);
  assert.throws(() => modelMatrisi([sur('x', [1, 2]), sur('x', [1, 2])]), IstatistikHatasi);
  // Düzey sırası verilirse referans varsayılanı o sıranın ilki
  const mm3 = modelMatrisi([kat('g', ['düşük', 'yüksek', 'orta'], { duzeyler: ['yüksek', 'orta', 'düşük'] })]);
  assert.deepEqual(mm3.sutunAdlari, ['(Sabit)', 'g=orta', 'g=düşük']);
});

test('mtcars: sürekli + 3 düzeyli faktör (summary.lm, drop1, car::vif, bptest, dwtest)', () => {
  const s = dogrusalRegresyon({
    y: mt.mpg,
    tahminciler: [sur('wt', mt.wt), sur('hp', mt.hp), sur('qsec', mt.qsec), kat('cyl', mt.cyl), sur('am', mt.am)],
  });
  lmKarsilastir(s, R.mtcars1, 'mtcars');
  drop1Karsilastir(s, R.mtcars1.drop1, 'mtcars drop1');
  s.terimTestleri.forEach((t, k) => {
    yakin(t.GVIF, R.mtcars1.gvif[k], 1e-9, 0, `GVIF ${t.ad}`);
    yakin(t.GVIFdf, R.mtcars1.gvifdf[k], 1e-9, 0, `GVIF^(1/2df) ${t.ad}`);
  });
  assert.equal(s.yontem, 'dogrusal-regresyon');
  assert.deepEqual(s.sd, [6, 25]);
  assert.equal(s.uyarilar.length, 0);
  // ANOVA tablosu tutarlılığı
  const [reg, art, top] = s.anovaTablosu;
  yakin(reg.F, s.modelOzeti.F, 1e-14, 0, 'anova F');
  assert.equal(top.sd, 31);
  yakin(top.KT, reg.KT + art.KT, 1e-14, 0, 'toplam KT');
  // referans düzeyi değiştirme (relevel(cyl, "8"))
  const s8 = dogrusalRegresyon({ y: mt.mpg, tahminciler: [sur('wt', mt.wt), kat('cyl', mt.cyl, { referans: 8 })] });
  lmKarsilastir(s8, R.mtcarsRef8, 'mtcars ref=8');
  assert.deepEqual(s8.sutunAdlari, ['(Sabit)', 'wt', 'cyl=4', 'cyl=6']);
});

test('swiss, stackloss, trees, cars', () => {
  const sw = V.swiss;
  const ad = ['Agriculture', 'Examination', 'Education', 'Catholic', 'Infant.Mortality'];
  const s = dogrusalRegresyon({ y: sw.Fertility, tahminciler: ad.map((a) => sur(a, sw[a])) });
  lmKarsilastir(s, R.swiss, 'swiss');
  drop1Karsilastir(s, R.swiss.drop1, 'swiss drop1');
  s.terimTestleri.forEach((t, k) => yakin(t.GVIF, R.swiss.gvif[k], 1e-9, 0, `swiss GVIF ${t.ad}`));
  const st = V.stackloss;
  lmKarsilastir(
    dogrusalRegresyon({ y: st['stack.loss'], tahminciler: ['Air.Flow', 'Water.Temp', 'Acid.Conc.'].map((a) => sur(a, st[a])) }),
    R.stackloss,
    'stackloss',
  );
  lmKarsilastir(dogrusalRegresyon({ y: V.tr.lv, tahminciler: [sur('lg', V.tr.lg), sur('lh', V.tr.lh)] }), R.trees, 'trees');
  const c = basitDogrusalRegresyon(V.cars.speed, V.cars.dist, { xAdi: 'speed' });
  lmKarsilastir(c, R.cars, 'cars');
  assert.equal(c.ad, 'Basit doğrusal regresyon');
  // Basit regresyonda beta = r, kısmi = yarı kısmi = r
  yakin(c.katsayilar[1].beta, Math.sqrt(c.modelOzeti.R2), 1e-12, 0, 'beta = r');
});

test('ToothGrowth: kategorik × kategorik ve sürekli × kategorik etkileşim (car::Anova tip 3)', () => {
  const tg = V.tg;
  const s = dogrusalRegresyon({
    y: tg.len,
    tahminciler: [kat('supp', tg.supp), kat('dosef', tg.dose)],
    etkilesimler: [['supp', 'dosef']],
  });
  lmKarsilastir(s, R.tgKat, 'TG kat×kat');
  const a = R.tgKat.anova3;
  s.terimTestleri.forEach((t, k) => {
    assert.equal(t.sd, a.sd[k + 1]);
    yakin(t.KT, a.KT[k + 1], 1e-9, 0, `TG ${t.ad} KT`);
    yakin(t.F, a.F[k + 1], 1e-9, 0, `TG ${t.ad} F`);
    yakin(t.p, a.p[k + 1], 1e-9, 1e-300, `TG ${t.ad} p`);
  });
  assert.equal(s.terimTestleri[0].etkilesimdeVar, true);
  assert.equal(s.terimTestleri[2].etkilesimdeVar, false);
  assert.ok(s.uyarilar.some((u) => u.includes('koşullu')));
  const s2 = dogrusalRegresyon({ y: tg.len, tahminciler: [sur('dose', tg.dose), kat('supp', tg.supp)], etkilesimler: [['dose', 'supp']] });
  lmKarsilastir(s2, R.tgSur, 'TG sür×kat');
  const b = R.tgSur.anova3;
  s2.terimTestleri.forEach((t, k) => {
    yakin(t.F, b.F[k + 1], 1e-9, 0, `TG2 ${t.ad} F`);
    yakin(t.p, b.p[k + 1], 1e-9, 1e-300, `TG2 ${t.ad} p`);
  });
});

test('sentetik: 4 düzeyli faktör (referans C), doğrusal bağımlı sütun, eksik değerler', () => {
  const v = R.sentetik.veri;
  const tah = [sur('x1', v.x1), sur('x2', v.x2), sur('x3', v.x3), kat('g', v.g, { referans: 'C' })];
  const s = dogrusalRegresyon({ y: v.y, tahminciler: tah });
  lmKarsilastir(s, R.sentetik, 'sentetik');
  assert.deepEqual(s.satirlar, R.sentetik.kullanilan);
  assert.deepEqual(s.bagimliSutunlar, ['x3']);
  assert.equal(s.katsayilar[3].bagimli, true);
  assert.ok(s.uyarilar.some((u) => u.includes('doğrusal bileşimi')));
  assert.ok(s.uyarilar.some((u) => u.includes('liste bazında')));
  assert.deepEqual(s.sd, [5, R.sentetik.sd2]);
  // x3 olmadan + drop1
  const s2 = dogrusalRegresyon({ y: v.y, tahminciler: [tah[0], tah[1], tah[3]] });
  lmKarsilastir(s2, R.sentetik2, 'sentetik2');
  drop1Karsilastir(s2, R.sentetik2.drop1, 'sentetik2 drop1');
  // sabitsiz model (R² sıfırdan, F sd1 = 2)
  const s3 = dogrusalRegresyon({ y: v.y, tahminciler: [tah[0], tah[1]], sabit: false });
  lmKarsilastir(s3, R.sabitsiz, 'sabitsiz');
  // y eksikliği nedeniyle boşalan düzey modelden çıkar (drop.unused.levels)
  const b = R.bosDuzey.veri;
  const s4 = dogrusalRegresyon({ y: b.y, tahminciler: [sur('x1', b.x1), kat('g', b.g)] });
  lmKarsilastir(s4, R.bosDuzey, 'boş düzey');
  assert.deepEqual(s4.sutunAdlari, ['(Sabit)', 'x1', 'g=B', 'g=C']);
});

test('uç değerler, merkezli kaldıraç, Mahalanobis', () => {
  const y = mt.mpg.slice();
  y[5] = 60; // yapay uç değer
  const s = dogrusalRegresyon({ y, tahminciler: [sur('wt', mt.wt)] });
  const t = s.tanilama;
  assert.ok(t.ucDegerSayisi >= 1);
  assert.equal(t.ucDegerler[0].satir, 5);
  const n = s.n;
  t.kaldirac.forEach((h, i) => {
    yakin(t.merkezliKaldirac[i], h - 1 / n, 1e-14, 1e-15, 'merkezli');
    yakin(t.mahalanobis[i], (n - 1) * (h - 1 / n), 1e-13, 1e-14, 'mahalanobis');
  });
  // basit regresyonda Mahalanobis = z²
  const x = mt.wt;
  const ort = x.reduce((a, b) => a + b) / n;
  const ss = Math.sqrt(x.reduce((a, b) => a + (b - ort) ** 2, 0) / (n - 1));
  yakin(t.mahalanobis[0], ((x[0] - ort) / ss) ** 2, 1e-12, 0, 'mahalanobis = z²');
});

test('hiyerarşik regresyon: R² değişimi ve F değişimi (anova(m1, m2, m3))', () => {
  const h = hiyerarsikRegresyon({
    y: mt.mpg,
    bloklar: [[sur('wt', mt.wt)], [sur('hp', mt.hp), sur('qsec', mt.qsec)], { tahminciler: [], etkilesimler: [['wt', 'hp']] }],
  });
  const r = R.hiyerarsik;
  assert.equal(h.modeller.length, 3);
  h.degisimler.forEach((d, k) => {
    yakin(d.R2, r.R2[k], TOL, 0, `R2 ${k}`);
    yakin(d.duzeltilmisR2, r.duzR2[k], TOL, 0, `düz R2 ${k}`);
    yakin(d.FDegisim, r.FDeg[k], 1e-9, 0, `F değişim ${k}`);
    yakin(d.p, r.pDeg[k], 1e-9, 1e-300, `p değişim ${k}`);
    assert.equal(d.sd1, r.sd1[k]);
    assert.equal(d.sd2, r.sd2[k]);
  });
  yakin(h.degisimler[1].R2Degisim, r.R2[1] - r.R2[0], 1e-9, 0, 'R2 değişim');
  lmKarsilastir(h.modeller[2], r.m3, 'hiyerarşik model 3');
  assert.throws(
    () => hiyerarsikRegresyon({ y: mt.mpg, bloklar: [{ tahminciler: [sur('wt', mt.wt)], etkilesimler: [['wt', 'hp']] }, [sur('hp', mt.hp)]] }),
    IstatistikHatasi,
  );
});

test('adımsal regresyon: SPSS kuralı (R add1/drop1 benzetimi ile)', () => {
  const sw = V.swiss;
  const swAd = ['Agriculture', 'Examination', 'Education', 'Catholic', 'Infant.Mortality'];
  const swT = swAd.map((a) => sur(a, sw[a]));
  const mtAd = ['cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb'];
  const mtT = mtAd.map((a) => (a === 'cyl' || a === 'gear' ? kat(a, mt[a]) : sur(a, mt[a])));
  const durumlar = [
    ['swissIleri', sw.Fertility, swT, 'ileri'],
    ['swissGeri', sw.Fertility, swT, 'geri'],
    ['swissAdimsal', sw.Fertility, swT, 'adimsal'],
    ['mtIleri', mt.mpg, mtT, 'ileri'],
    ['mtGeri', mt.mpg, mtT, 'geri'],
    ['mtAdimsal', mt.mpg, mtT, 'adimsal'],
    ['mtAdimsalGevsek', mt.mpg, mtT, 'adimsal', 0.2, 0.25],
    ['haldAdimsal', V.hald.y, ['x1', 'x2', 'x3', 'x4'].map((a) => sur(a, V.hald[a])), 'adimsal', 0.1, 0.15],
    ['haldGeri', V.hald.y, ['x1', 'x2', 'x3', 'x4'].map((a) => sur(a, V.hald[a])), 'geri', 0.1, 0.15],
  ];
  for (const [ad, y, tah, yontem, girisP = 0.05, cikisP = 0.1] of durumlar) {
    const s = adimsalRegresyon({ y, tahminciler: tah, yontem, girisP, cikisP });
    const r = R.adimsal[ad];
    const adimlar = s.adimlar.filter((a) => a.islem !== 'baslangic');
    assert.equal(adimlar.length, r.adimlar.length, `${ad}: adım sayısı`);
    adimlar.forEach((a, k) => {
      const ra = r.adimlar[k];
      assert.equal(a.islem, ra.islem, `${ad} adım ${k + 1} işlem`);
      assert.equal(a.terim, ra.terim, `${ad} adım ${k + 1} terim`);
      yakin(a.F, ra.F, 1e-9, 0, `${ad} adım ${k + 1} F`);
      yakin(a.p, ra.p, 1e-9, 1e-300, `${ad} adım ${k + 1} p`);
      yakin(a.R2, ra.R2, 1e-10, 0, `${ad} adım ${k + 1} R2`);
    });
    assert.deepEqual([...s.secilenler].sort(), [...r.secilen].sort(), `${ad}: seçilenler`);
    if (r.son) {
      // R'de seçilenler giriş sırasıyla, bizde aday sırasıyla: katsayıları ada göre eşle
      const sira = r.secilen;
      const bizim = s.sonModel;
      yakin(bizim.modelOzeti.R2, r.son.R2, TOL, 0, `${ad} son R2`);
      yakin(bizim.modelOzeti.F, r.son.F, TOL, 0, `${ad} son F`);
      yakin(bizim.modelOzeti.AIC, r.son.AIC, TOL, 0, `${ad} son AIC`);
      for (const terim of sira) {
        const kb = bizim.katsayilar.filter((k) => k.terim === terim);
        const kr = r.son.ad.map((a, j) => ({ a, j })).filter(({ a }) => a === terim || a.startsWith(terim));
        assert.equal(kb.length, kr.length, `${ad} ${terim} sütun sayısı`);
        kb.forEach((k, i) => yakin(k.B, r.son.B[kr[i].j], TOL, 1e-12, `${ad} ${terim} B`));
      }
    }
  }
  // Hald: x4, x1, x2 girer, ardından x4 çıkar (adımsal yöntemin çıkış adımı)
  const hald = adimsalRegresyon({ y: V.hald.y, tahminciler: ['x1', 'x2', 'x3', 'x4'].map((a) => sur(a, V.hald[a])), girisP: 0.1, cikisP: 0.15 });
  assert.deepEqual(hald.adimlar.map((a) => `${a.islem}:${a.terim}`), ['giris:x4', 'giris:x1', 'giris:x2', 'cikis:x4']);
  assert.deepEqual(hald.secilenler, ['x1', 'x2']);
  // geri yöntemde başlangıç modeli kaydı
  const g = adimsalRegresyon({ y: sw.Fertility, tahminciler: swT, yontem: 'geri' });
  assert.equal(g.adimlar[0].islem, 'baslangic');
  assert.equal(g.adimlar[0].modeldekiler.length, 5);
  // Dışlanan değişkenler: kısmi korelasyon ve Beta In tek sütunlu terimler için, F = t²
  const f = adimsalRegresyon({ y: sw.Fertility, tahminciler: swT, yontem: 'ileri' });
  for (const d of f.dislananlar) {
    yakin(d.t * d.t, d.F, 1e-12, 0, 'F = t²');
    assert.ok(Math.abs(d.kismiKorelasyon) < 1);
    assert.ok(d.p >= 0.05);
  }
  assert.throws(() => adimsalRegresyon({ y: sw.Fertility, tahminciler: swT, girisP: 0.1, cikisP: 0.05 }), IstatistikHatasi);
});

test('hata durumları', () => {
  assert.throws(() => dogrusalRegresyon({ y: [1, 1, 1, 1], tahminciler: [sur('x', [1, 2, 3, 4])] }), /sabit/);
  assert.throws(() => dogrusalRegresyon({ y: [1, 2], tahminciler: [sur('x', [1, 2])] }), /serbestlik/);
  assert.throws(() => dogrusalRegresyon({ y: [null, null], tahminciler: [sur('x', [1, 2])] }), IstatistikHatasi);
  // kusursuz uyum uyarısı
  const s = dogrusalRegresyon({ y: [1, 3, 5, 7, 9.000000000000002], tahminciler: [sur('x', [0, 1, 2, 3, 4])] });
  assert.ok(s.uyarilar.some((u) => u.includes('kusursuz')));
});
