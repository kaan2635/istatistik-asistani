import { test } from 'node:test';
import assert from 'node:assert/strict';
import { faktoriyelAnova, duzeyleriBelirle, ardisikQR } from '../src/cekirdek/glm.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin } from './yardimci.js';

const R = referans('glm');
const TOL = 1e-10;

// R terim adı ("A:B", "(Intercept)") ve bizim ad ("A * B", "Kesişim") için ortak anahtar
const anahtar = (s) => (s === '(Intercept)' || s === 'Kesişim' ? 'Kesişim' : s.split(/:| \* /).sort().join('|'));

function nesne(x) {
  return Array.isArray(x) ? {} : x;
}

function calistir(r, ek = {}) {
  return faktoriyelAnova({
    y: r.veri.y,
    faktorler: r.veri.faktorler,
    kovaryatlar: nesne(r.veri.kovaryatlar),
    etkilesimler: r.etkilesimler,
    duzeySirasi: r.duzeyler,
    ...ek,
  });
}

function satirBul(sonuc, kaynak) {
  const a = anahtar(kaynak);
  const s = sonuc.anovaTablosu.find((x) => (x.tur === 'terim' || x.tur === 'kesisim') && anahtar(x.kaynak) === a);
  assert.ok(s, `satır yok: ${kaynak}`);
  return s;
}

function tabloKarsilastir(sonuc, rTablo, mesaj, tol = TOL, kesisimHaric = false) {
  for (const r of rTablo) {
    if (r.kaynak === 'Residuals') {
      const h = sonuc.anovaTablosu.find((x) => x.tur === 'hata');
      yakin(h.KT, r.KT, tol, 1e-12, `${mesaj} hata KT`);
      assert.equal(h.sd, r.sd);
      continue;
    }
    if (kesisimHaric && r.kaynak === '(Intercept)') continue;
    const s = satirBul(sonuc, r.kaynak);
    yakin(s.KT, r.KT, tol, 1e-12, `${mesaj} ${r.kaynak} KT`);
    assert.equal(s.sd, r.sd, `${mesaj} ${r.kaynak} sd`);
    yakin(s.F, r.F, tol, 1e-12, `${mesaj} ${r.kaynak} F`);
    yakin(s.p, r.p, tol, 1e-300, `${mesaj} ${r.kaynak} p`);
  }
}

function emmKarsilastir(sonuc, rEmm, faktorler, mesaj) {
  for (const [terim, satirlar] of Object.entries(rEmm)) {
    const adlar = terim.split(':');
    const benim = sonuc.marjinalOrtalamalar[Object.keys(sonuc.marjinalOrtalamalar).find((k) => anahtar(k) === anahtar(terim))];
    assert.ok(benim, `${mesaj}: EMM yok ${terim}`);
    assert.equal(benim.length, satirlar.length);
    for (const r of satirlar) {
      const b = benim.find((x) => adlar.every((a) => x.duzeyler[a] === r[a]));
      assert.ok(b, `${mesaj}: hücre yok ${terim}`);
      const m = `${mesaj} EMM ${terim} ${adlar.map((a) => r[a]).join(',')}`;
      yakin(b.ortalama, r.emmean, TOL, 1e-12, `${m} ortalama`);
      yakin(b.SH, r.SE, TOL, 1e-14, `${m} SH`);
      assert.equal(b.sd, r.df);
      yakin(b.gaAlt, r['lower.CL'], TOL, 1e-12, `${m} gaAlt`);
      yakin(b.gaUst, r['upper.CL'], TOL, 1e-12, `${m} gaUst`);
    }
  }
}

function ikiliKarsilastir(benim, rTablo, mesaj) {
  assert.equal(benim.length, rTablo.length, `${mesaj} karşılaştırma sayısı`);
  rTablo.forEach((r, i) => {
    const b = benim[i];
    const m = `${mesaj} [${r.contrast}]`;
    yakin(b.fark, r.estimate, TOL, 1e-12, `${m} fark`);
    yakin(b.SH, r.SE, TOL, 1e-14, `${m} SH`);
    yakin(b.t, r['t.ratio'], TOL, 1e-12, `${m} t`);
    assert.equal(b.sd, r.df);
    yakin(b.p, r['p.value'], TOL, 1e-13, `${m} p`);
    yakin(b.gaAlt, r['lower.CL'], TOL, 1e-12, `${m} gaAlt`);
    yakin(b.gaUst, r['upper.CL'], TOL, 1e-12, `${m} gaUst`);
  });
}

// faktör × kovaryat etkileşimi içeren modeller: car'ın Tip II'si SPSS'inkinden farklıdır
const KOV_ETKILESIMLI = new Set(['faktorKov']);

for (const [ad, r] of Object.entries(R)) {
  test(`GLM ${ad}: Tip III tablo, model, R², Levene (car::Anova type 3)`, () => {
    const s = calistir(r);
    assert.equal(s.n, r.n);
    tabloKarsilastir(s, r.tip3, `${ad} tip3`);
    const model = s.anovaTablosu.find((x) => x.tur === 'model');
    yakin(model.KT, r.model.KT, TOL, 1e-12, 'model KT');
    assert.equal(model.sd, r.model.sd);
    yakin(model.F, r.model.F, TOL, 0, 'model F');
    yakin(model.p, r.model.p, TOL, 1e-300, 'model p');
    const hata = s.anovaTablosu.find((x) => x.tur === 'hata');
    yakin(hata.KT, r.hata.KT, TOL, 0, 'hata KT');
    assert.equal(hata.sd, r.hata.sd);
    const top = s.anovaTablosu.find((x) => x.tur === 'toplam');
    yakin(top.KT, r.toplam.KT, 1e-13, 0, 'toplam KT');
    assert.equal(top.sd, r.toplam.sd);
    const dt = s.anovaTablosu.find((x) => x.tur === 'duzeltilmisToplam');
    yakin(dt.KT, r.duzeltilmisToplam.KT, 1e-12, 0, 'düzeltilmiş toplam KT');
    assert.equal(dt.sd, r.duzeltilmisToplam.sd);
    yakin(s.R2, r.R2, TOL, 0, 'R2');
    yakin(s.duzeltilmisR2, r.R2d, TOL, 0, 'düzeltilmiş R2');
    // kısmi eta kare = KT / (KT + hata KT)
    for (const x of s.anovaTablosu.filter((v) => v.F !== undefined)) yakin(x.kismiEtaKare, x.KT / (x.KT + r.hata.KT), 1e-12, 0, 'kısmi eta');
    yakin(s.levene.istatistik, r.levene.F, TOL, 0, 'Levene F');
    assert.deepEqual(s.levene.sd, [r.levene.sd1, r.levene.sd2]);
    yakin(s.levene.p, r.levene.p, TOL, 1e-300, 'Levene p');
  });

  test(`GLM ${ad}: Tip I (anova) ve Tip II (SPSS kuralı; car type 2)`, () => {
    const s1 = calistir(r, { tip: 1 });
    tabloKarsilastir(s1, r.tip1, `${ad} tip1`);
    yakin(satirBul(s1, '(Intercept)').KT, r.kesisimTip1, TOL, 0, 'Tip I kesişim = n·ȳ²');
    const s2 = calistir(r, { tip: 2 });
    if (!KOV_ETKILESIMLI.has(ad)) tabloKarsilastir(s2, r.tip2, `${ad} tip2`);
    // SPSS kapsama kuralına göre elle model karşılaştırması (R'de RSS farkı → 1e-9)
    for (const x of r.spss2) yakin(satirBul(s2, x.kaynak).KT, x.KT, 1e-9, 1e-9, `${ad} SPSS tip II ${x.kaynak}`);
  });

  test(`GLM ${ad}: kestirilen marjinal ortalamalar ve ikili karşılaştırmalar (emmeans)`, () => {
    const s = calistir(r);
    emmKarsilastir(s, r.emm, Object.keys(r.veri.faktorler), ad);
    for (const [yontem, rAd] of [['bonferroni', 'bonferroni'], ['sidak', 'sidak'], ['yok', 'yok']]) {
      const sy = yontem === 'bonferroni' ? s : calistir(r, { duzeltme: yontem });
      for (const [f, tablolar] of Object.entries(r.ikili)) ikiliKarsilastir(sy.ikiliKarsilastirmalar[f], tablolar[rAd], `${ad} ${f} ${yontem}`);
    }
  });

  test(`GLM ${ad}: basit etkiler, betimsel, kovaryat katsayıları, eğim homojenliği`, () => {
    const s = calistir(r);
    for (const rb of r.basit) {
      const b = s.basitEtkiler.find((x) => x.etken === rb.etken && x.kosul[rb.kosulFaktor] === rb.kosul);
      assert.ok(b, `basit etki yok: ${rb.etken} | ${rb.kosulFaktor}=${rb.kosul}`);
      const m = `${ad} basit ${rb.etken}|${rb.kosul}`;
      yakin(b.F, rb.F, TOL, 0, `${m} F`);
      assert.equal(b.sd, rb.sd1);
      assert.equal(b.hataSd, rb.sd2);
      yakin(b.p, rb.p, TOL, 1e-300, `${m} p`);
      yakin(b.KT, b.F * b.sd * s.hataKO, 1e-12, 0, `${m} KT`);
      ikiliKarsilastir(b.ikiliKarsilastirmalar, rb.ikili, `${m} ikili`);
    }
    // joint_tests (F yuvarlanmış yazıldığından yalnız p karşılaştırılır)
    for (const [kosulF, satirlar] of Object.entries(r.joint ?? {})) {
      if (Array.isArray(satirlar) && satirlar.length === 0) continue;
      for (const j of satirlar) {
        const terim = j['model term'];
        if (terim.includes(':') || !Object.keys(r.veri.faktorler).includes(terim)) continue;
        const b = s.basitEtkiler.find((x) => x.etken === terim && x.kosul[kosulF] === j[kosulF]);
        if (!b) continue; // etkileşimi modelde olmayan çiftler
        yakin(b.p, j['p.value'], TOL, 1e-300, `${ad} joint_tests ${terim}|${j[kosulF]}`);
        yakin(b.F, j['F.ratio'], 0, 5.0001e-4, `${ad} joint_tests F (3 basamağa yuvarlanmış)`);
      }
    }
    for (const rc of r.betimsel) {
      const fk = Object.keys(r.veri.faktorler);
      const b = s.betimsel.hucreler.find((h) => fk.every((f) => h.duzeyler[f] === rc[f]));
      assert.ok(b);
      assert.equal(b.n, rc.n);
      yakin(b.ortalama, rc.ortalama, 1e-13, 1e-13, 'hücre ortalaması');
      yakin(b.standartSapma, rc.ss, 1e-12, 1e-13, 'hücre ss');
    }
    assert.equal(s.betimsel.hucreler.length, r.betimsel.length);
    for (const [k, rc] of Object.entries(nesne(r.katsayi))) {
      const b = s.kovaryatKatsayilari.find((x) => x.ad === k);
      for (const alan of ['B', 'SH', 't', 'p', 'gaAlt', 'gaUst']) yakin(b[alan], rc[alan], TOL, 1e-300, `${ad} katsayı ${k} ${alan}`);
    }
    assert.equal(s.kovaryatKatsayilari.length, Object.keys(nesne(r.katsayi)).length);
    for (const [k, v] of Object.entries(nesne(r.kovOrt))) yakin(s.kovaryatlar.find((x) => x.ad === k).ortalama, v, 1e-14, 0, 'kovaryat ortalaması');
    if (r.egim) {
      const e = s.egimHomojenligi;
      assert.ok(e);
      const ekAnahtar = e.tablo.map((x) => anahtar(x.kaynak));
      for (const rt of r.egim.tablo) {
        const k = anahtar(rt.kaynak);
        if (!ekAnahtar.includes(k)) continue;
        const b = e.tablo.find((x) => anahtar(x.kaynak) === k);
        yakin(b.KT, rt.KT, TOL, 0, `${ad} eğim ${rt.kaynak} KT`);
        assert.equal(b.sd, rt.sd);
        yakin(b.F, rt.F, TOL, 0, `${ad} eğim ${rt.kaynak} F`);
        yakin(b.p, rt.p, TOL, 1e-300, `${ad} eğim ${rt.kaynak} p`);
      }
      const ozgun = new Set(r.tip3.map((x) => anahtar(x.kaynak)));
      const rEk = r.egim.tablo.filter((x) => x.kaynak !== 'Residuals' && !ozgun.has(anahtar(x.kaynak)));
      assert.deepEqual(ekAnahtar.slice().sort(), rEk.map((x) => anahtar(x.kaynak)).sort(), `${ad} eğim terimleri`);
      yakin(e.toplu.KT, r.egim.toplu.KT, 1e-9, 0, 'eğim toplu KT');
      assert.equal(e.toplu.sd, r.egim.toplu.sd);
      assert.equal(e.toplu.hataSd, r.egim.toplu.hataSd);
      yakin(e.toplu.F, r.egim.toplu.F, 1e-9, 0, 'eğim toplu F');
      yakin(e.toplu.p, r.egim.toplu.p, 1e-9, 0, 'eğim toplu p');
    }
  });
}

test('GLM: dengeli tasarımda Tip I = Tip II = Tip III', () => {
  const r = R.tooth;
  const [a, b, c] = [1, 2, 3].map((tip) => calistir(r, { tip }));
  assert.equal(c.dengeli, true);
  for (const kaynak of ['supp', 'dose', 'supp * dose']) {
    yakin(satirBul(a, kaynak).KT, satirBul(c, kaynak).KT, 1e-12, 0, kaynak);
    yakin(satirBul(b, kaynak).KT, satirBul(c, kaynak).KT, 1e-12, 0, kaynak);
  }
  assert.equal(calistir(R.mtcars).dengeli, false);
});

test('GLM: eksik değerler liste bazında çıkarılır', () => {
  const s = calistir(R.sentetik);
  assert.equal(s.cikarilanGozlem, 5);
  assert.equal(s.n, R.sentetik.n);
  const k = calistir(R.sentetikKov);
  assert.equal(k.cikarilanGozlem, 6);
});

test('GLM: düzey sırası varsayılanı (sayısal / alfabetik) ve özel sıra', () => {
  assert.deepEqual(duzeyleriBelirle([2, 0.5, 1, 10, 2]), ['0.5', '1', '2', '10']);
  assert.deepEqual(duzeyleriBelirle(['10', '9', '1']), ['1', '9', '10']);
  assert.deepEqual(duzeyleriBelirle(['ç', 'c', 'd', 'Ğ', 'a']), ['a', 'c', 'ç', 'd', 'Ğ']);
  assert.deepEqual(duzeyleriBelirle(['L', 'M', 'H'], ['L', 'M', 'H']), ['L', 'M', 'H']);
  assert.deepEqual(duzeyleriBelirle(['L', 'M', 'H', 'X'], ['M']), ['M', 'H', 'L', 'X']);
  // varsayılan sıralamayla (H, L, M) Tip III sonuçlar aynı, EMM'ler aynı düzey için aynı kalır
  const r = R.warp;
  const s = faktoriyelAnova({ y: r.veri.y, faktorler: r.veri.faktorler });
  assert.deepEqual(s.faktorler[1].duzeyler, ['H', 'L', 'M']);
  tabloKarsilastir(s, r.tip3, 'warp varsayılan sıra');
  const sayisal = faktoriyelAnova({ y: R.tooth.veri.y, faktorler: { supp: R.tooth.veri.faktorler.supp, dose: R.tooth.veri.faktorler.dose.map(Number) } });
  assert.deepEqual(sayisal.faktorler[1].duzeyler, ['0.5', '1', '2']);
  tabloKarsilastir(sayisal, R.tooth.tip3, 'tooth sayısal etiket');
});

test('GLM: boş hücre hatası ve yalnız ana etki modelinde uyarı', () => {
  const y = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
  const A = ['a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c'];
  const B = ['x', 'x', 'y', 'y', 'x', 'x', 'y', 'y', 'x', 'x', 'x', 'x']; // c-y hücresi boş
  assert.throws(
    () => faktoriyelAnova({ y, faktorler: { A, B } }),
    (e) => e instanceof IstatistikHatasi && /boş hücre/.test(e.message) && e.ayrinti.bosHucreler.length === 1 && e.ayrinti.bosHucreler[0].A === 'c',
  );
  const s = faktoriyelAnova({ y: y.map((v, i) => v + (i % 3) * 0.7), faktorler: { A, B }, etkilesimler: 'yok' });
  assert.ok(s.uyarilar.some((u) => /hiç gözlem yok/.test(u)));
  assert.equal(s.marjinalOrtalamalar.B.length, 2);
});

test('GLM: hata durumları', () => {
  const y = [1, 2, 3, 4, 5, 6];
  const A = ['a', 'a', 'b', 'b', 'c', 'c'];
  const hata = (f, re) => assert.throws(f, (e) => e instanceof IstatistikHatasi && re.test(e.message));
  hata(() => faktoriyelAnova({ y, faktorler: {} }), /En az bir faktör/);
  hata(() => faktoriyelAnova({ y, faktorler: { A: ['a', 'b'] } }), /uzunluğu/);
  hata(() => faktoriyelAnova({ y, faktorler: { A: ['a', 'a', 'a', 'a', 'a', 'a'] } }), /en az iki düzeyi/);
  hata(() => faktoriyelAnova({ y, faktorler: { A }, tip: 4 }), /tipi/);
  hata(() => faktoriyelAnova({ y, faktorler: { A }, duzeltme: 'tukey' }), /düzeltme/);
  hata(() => faktoriyelAnova({ y: [1, 1, 2, 2, 3, 3], faktorler: { A } }), /Hata kareler toplamı sıfır/);
  hata(() => faktoriyelAnova({ y: [1, 2, 3], faktorler: { A: ['a', 'b', 'c'] } }), /hata serbestlik/);
  hata(() => faktoriyelAnova({ y, faktorler: { A }, kovaryatlar: { x: [1, 2, 3, 4, 5, 7], z: [2, 4, 6, 8, 10, 14] } }), /tam ranklı değil/);
  hata(() => faktoriyelAnova({ y, faktorler: { A }, etkilesimler: [['A', 'Q']] }), /'Q'/);
  hata(() => faktoriyelAnova({ y, faktorler: { A }, kovaryatlar: { A: y } }), /hem faktör hem kovaryat/);
});

test('ardisikQR: doğrusal bağımlı sütun atlanır, KT toplamı korunur', () => {
  const x1 = Float64Array.from([1, 1, 1, 1, 1]);
  const x2 = Float64Array.from([1, 2, 3, 4, 6]);
  const x3 = Float64Array.from([3, 5, 7, 9, 13]); // 1 + 2·x2
  const y = [2, 4, 5, 4, 9];
  const r = ardisikQR([x1, x2, x3], y);
  assert.equal(r.rank, 2);
  assert.equal(r.etki[2], null);
  const ss = y.reduce((t, v) => t + v * v, 0);
  yakin(r.etki[0] ** 2 + r.etki[1] ** 2 + r.kalanKT, ss, 1e-13, 0, 'KT ayrışımı');
});
