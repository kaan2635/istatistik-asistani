import { test } from 'node:test';
import assert from 'node:assert/strict';
import { rocAnalizi, rocKarsilastir, rocBagimsizKarsilastir, kesimNoktasiMetrikleri } from '../src/cekirdek/roc.js';
import { taniTesti, cohenKappa, oranGA } from '../src/cekirdek/tani.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin, yakinDerin } from './yardimci.js';

const R = referans('roc');
const dizi = (x) => (Array.isArray(x) ? x : [x]);
// R'nin ±Inf değerleri JSON'da null olur
const esikEsit = (g, b, m) => (b === null ? assert.ok(!Number.isFinite(g), `${m}: ±Inf bekleniyordu, gelen ${g}`) : yakin(g, b, 1e-12, 1e-14, m));

test('ROC: pROC eşikleri, duyarlılık/özgüllük, AUC, DeLong GA ve varyansı, yön', () => {
  for (const [ad, b] of Object.entries(R.roc)) {
    const yon = b.yon === '<' ? 'buyuk-pozitif' : 'kucuk-pozitif';
    const otomatik = !['s100bTers', 'baglarTers'].includes(ad);
    const r = rocAnalizi(b.deger, b.durum, otomatik ? {} : { yon });
    assert.equal(r.yon, yon, `${ad} yön`);
    assert.equal(r.noktalar.length, b.esik.length, `${ad} nokta sayısı`);
    r.noktalar.forEach((p, i) => esikEsit(p.esik, b.esik[i], `${ad} eşik ${i}`));
    yakinDerin(r.noktalar.map((p) => p.duyarlilik), b.duyarlilik, 1e-14, 1e-15, `${ad} duyarlılık`);
    yakinDerin(r.noktalar.map((p) => p.ozgulluk), b.ozgulluk, 1e-14, 1e-15, `${ad} özgüllük`);
    yakin(r.auc, b.auc, 1e-13, 1e-15, `${ad} AUC`);
    yakin(r.gaAlt, b.gaAlt, 1e-10, 1e-13, `${ad} GA alt`);
    yakin(r.gaUst, b.gaUst, 1e-10, 1e-13, `${ad} GA üst`);
    yakin(r.varyansDeLong, b.varyans, 1e-10, 1e-15, `${ad} DeLong varyans`);
    yakin(r.shHanleyMcNeil, b.shHM, 1e-12, 1e-15, `${ad} Hanley-McNeil SH`);
    yakin(r.pH0, b.pH0, 1e-9, 1e-15, `${ad} p (H0, Mann-Whitney)`);
    // En iyi kesimler (pROC coords best; eşitlikte tüm noktalar)
    for (const [alan, ref] of [['enIyiYouden', b.youden], ['enIyiKose', b.kose]]) {
      const es = r[alan].esitler;
      const re = dizi(ref.esik);
      assert.equal(es.length, re.length, `${ad} ${alan} eşit nokta sayısı`);
      es.forEach((e, i) => {
        esikEsit(e.esik, re[i], `${ad} ${alan} eşik`);
        yakin(e.duyarlilik, dizi(ref.duyarlilik)[i], 1e-14, 1e-15, `${ad} ${alan} duyarlılık`);
        yakin(e.ozgulluk, dizi(ref.ozgulluk)[i], 1e-14, 1e-15, `${ad} ${alan} özgüllük`);
      });
    }
    // Youden kesiminde 2x2 tablo ve epiR ölçütleri
    const m = r.enIyiYouden.metrikler;
    assert.deepEqual([m.tablo.tp, m.tablo.fp, m.tablo.fn, m.tablo.tn], b.tablo, `${ad} 2x2`);
    for (const [alan, ref] of [['duyarlilik', b.se], ['ozgulluk', b.sp]]) {
      yakin(m[alan].deger, ref[0], 1e-12, 1e-15, `${ad} ${alan}`);
      yakin(m[alan].gaAlt, ref[1], 1e-9, 1e-13, `${ad} ${alan} alt`);
      yakin(m[alan].gaUst, ref[2], 1e-9, 1e-13, `${ad} ${alan} üst`);
    }
    if (Number.isFinite(b.lrArti[0])) yakin(m.pozitifOlabilirlikOrani.deger, b.lrArti[0], 1e-12, 1e-15, `${ad} LR+`);
  }
});

test('ROC: iki bağımlı eğri (pROC roc.test DeLong, paired)', () => {
  const s = R.roc.s100b;
  const deger = {
    s100b: s.deger,
    ndka: R.roc.ndka.deger,
    wfns: R.roc.wfns.deger,
    age: R.roc.age.deger,
  };
  const ciftler = { s100b_ndka: ['s100b', 'ndka'], s100b_wfns: ['s100b', 'wfns'], wfns_age: ['wfns', 'age'] };
  for (const [ad, [a, b]] of Object.entries(ciftler)) {
    const r = rocKarsilastir(deger[a], deger[b], s.durum);
    const ref = R.eslesmis[ad];
    yakin(r.istatistik, ref.z, 1e-10, 1e-13, `${ad} z`);
    yakin(r.p, ref.p, 1e-9, 1e-15, `${ad} p`);
    yakin(r.gaAlt, ref.gaAlt, 1e-10, 1e-13, `${ad} GA alt`);
    yakin(r.gaUst, ref.gaUst, 1e-10, 1e-13, `${ad} GA üst`);
    yakinDerin([r.auc1, r.auc2], ref.auc, 1e-13, 1e-15, `${ad} AUC`);
  }
  const sb = R.roc.baglar;
  const ek = [0, 1, 0, -1, 0, 2, 0, 0, 1, 0, 0, 1, 0, 0, 2, 0, 0, 1, 0, 0];
  const r = rocKarsilastir(sb.deger, sb.deger.map((v, i) => v + ek[i]), sb.durum);
  yakin(r.istatistik, R.eslesmis.sentetik.z, 1e-10, 1e-13, 'sentetik z');
  yakin(r.p, R.eslesmis.sentetik.p, 1e-9, 1e-15, 'sentetik p');
  // aynı test kendisiyle: fark 0
  const ayni = rocKarsilastir(s.deger, s.deger, s.durum);
  assert.equal(ayni.istatistik, 0);
  assert.throws(() => rocKarsilastir([1, 2, 3], [1, 2], [0, 1, 1]), IstatistikHatasi);
});

test('ROC: iki bağımsız eğri (pROC roc.test DeLong, unpaired)', () => {
  const b = R.bagimsiz;
  const r = rocBagimsizKarsilastir({ deger: b.deger1, durum: b.durum1 }, { deger: b.deger2, durum: b.durum2 });
  yakin(r.istatistik, b.D, 1e-10, 1e-13, 'D');
  yakin(r.sd, b.sd, 1e-10, 1e-13, 'sd');
  yakin(r.p, b.p, 1e-9, 1e-15, 'p');
  yakinDerin([r.auc1, r.auc2], b.auc, 1e-13, 1e-15, 'AUC');
  const r2 = rocBagimsizKarsilastir(rocAnalizi(b.deger1, b.durum1), rocAnalizi(b.deger2, b.durum2));
  assert.equal(r2.istatistik, r.istatistik);
});

test('ROC: kesim noktası metrikleri, yön ve hata mesajları', () => {
  const s = R.roc.s100b;
  const k = kesimNoktasiMetrikleri(s.deger, s.durum, 0.205, 'buyuk-pozitif');
  assert.deepEqual([k.tablo.tp, k.tablo.fp, k.tablo.fn, k.tablo.tn], s.tablo);
  assert.equal(k.kriter, '≥ 0.205');
  const t = kesimNoktasiMetrikleri([1, 2, 3, 4], [1, 1, 0, 0], 2, 'kucuk-pozitif');
  assert.deepEqual(t.tablo, { tp: 2, fp: 0, fn: 0, tn: 2 });
  assert.equal(kesimNoktasiMetrikleri([1, 2, 3, 4], [1, 1, 0, 0], 2, 'otomatik').yon, 'kucuk-pozitif');
  assert.throws(() => rocAnalizi([1, 2, 3], [1, 1, 1]), /sağlam/);
  assert.throws(() => rocAnalizi([1, 2, 3], [0, 0, 0]), /hasta/);
  assert.throws(() => rocAnalizi([1, 2, 3], [0, 1, 2]), IstatistikHatasi);
  assert.throws(() => rocAnalizi([1, NaN, 3], [0, 1, 1]), /eksik/);
  assert.throws(() => rocAnalizi([1, 2], [0, 1], { yon: 'yukari' }), IstatistikHatasi);
  // tek hasta: DeLong SH yok, uyarı
  const tek = rocAnalizi([1, 2, 3], [0, 0, 1]);
  assert.ok(Number.isNaN(tek.sh));
  assert.ok(tek.uyarilar.length > 0);
});

test('Tanı testi: epiR::epi.tests (exact, Wilson), binom.test, prop.test, kappa, F1, Youden, prevalansa göre KD', () => {
  const al = (o) => [o.deger, o.gaAlt, o.gaUst];
  for (const [ad, b] of Object.entries(R.tani)) {
    const [tp, fp, fn, tn] = b.tablo;
    const r = taniTesti(tp, fp, fn, tn, { prevalans: b.duzeltilmis.prev });
    const e = b.exact;
    const esle = {
      duyarlilik: e.se, ozgulluk: e.sp, pozitifKestirimDegeri: e.ppv, negatifKestirimDegeri: e.npv,
      dogruluk: e.dogruluk, prevalans: e.prev, pozitifOlabilirlikOrani: e.lrArti, negatifOlabilirlikOrani: e.lrEksi,
      tanisalOddsOrani: e.dor,
    };
    for (const [alan, ref] of Object.entries(esle)) yakinDerin(al(r[alan]), ref, 1e-9, 1e-13, `${ad} ${alan} (epiR exact)`);
    for (const [alan, ref] of Object.entries({ duyarlilik: b.binom.se, ozgulluk: b.binom.sp, pozitifKestirimDegeri: b.binom.ppv, negatifKestirimDegeri: b.binom.npv, dogruluk: b.binom.dogruluk })) {
      yakinDerin(al(r[alan]), ref, 1e-9, 1e-13, `${ad} ${alan} (binom.test)`);
    }
    yakinDerin(al(r.youdenJ), b.youdenWald, 1e-12, 1e-14, `${ad} Youden Wald`);
    yakinDerin(al(r.f1), b.f1, 1e-12, 1e-14, `${ad} F1`);
    yakinDerin([r.kappa.deger, r.kappa.sh, r.kappa.sh0, r.kappa.gaAlt, r.kappa.gaUst, r.kappa.p], b.kappa, 1e-12, 1e-14, `${ad} kappa`);
    if (b.psych) yakinDerin([r.kappa.deger, r.kappa.sh], b.psych, 1e-10, 1e-14, `${ad} kappa (psych)`);
    yakinDerin(al(r.duzeltilmis.pozitifKestirimDegeri), b.duzeltilmis.ppv, 1e-12, 1e-14, `${ad} PPV (prevalans)`);
    yakinDerin(al(r.duzeltilmis.negatifKestirimDegeri), b.duzeltilmis.npv, 1e-12, 1e-14, `${ad} NPV (prevalans)`);
    // Wilson
    const w = taniTesti(tp, fp, fn, tn, { oranYontemi: 'wilson' });
    for (const [alan, ref] of Object.entries({ duyarlilik: b.wilson.se, ozgulluk: b.wilson.sp, pozitifKestirimDegeri: b.wilson.ppv, negatifKestirimDegeri: b.wilson.npv, dogruluk: b.wilson.dogruluk })) {
      yakinDerin(al(w[alan]), ref, 1e-12, 1e-14, `${ad} ${alan} (prop.test Wilson)`);
    }
    for (const [alan, ref] of Object.entries({ duyarlilik: b.wilsonEpi.se, ozgulluk: b.wilsonEpi.sp })) {
      yakinDerin(al(w[alan]), ref, 1e-9, 1e-13, `${ad} ${alan} (epiR Wilson)`);
    }
    // epiR'nin Youden yöntemi
    const ey = taniTesti(tp, fp, fn, tn, { youdenYontemi: 'epiR' });
    yakinDerin(al(ey.youdenJ), e.youden, 1e-9, 1e-13, `${ad} Youden (epiR exact)`);
    const eyw = taniTesti(tp, fp, fn, tn, { youdenYontemi: 'epiR', oranYontemi: 'wilson' });
    yakinDerin(al(eyw.youdenJ), b.wilsonEpi.youden, 1e-9, 1e-13, `${ad} Youden (epiR Wilson)`);
  }
  // Sıfır hücre
  const z = taniTesti(10, 0, 5, 20);
  const s = R.sifirHucre;
  yakinDerin(al(z.duyarlilik), s.se, 1e-9, 1e-13, 'sıfır hücre se');
  yakinDerin(al(z.ozgulluk), s.sp, 1e-9, 1e-13, 'sıfır hücre sp');
  yakinDerin(al(z.pozitifKestirimDegeri), s.ppv, 1e-9, 1e-13, 'sıfır hücre ppv');
  yakinDerin(al(z.negatifOlabilirlikOrani), s.lrEksi, 1e-9, 1e-13, 'sıfır hücre LR-');
  assert.equal(z.pozitifOlabilirlikOrani.deger, Infinity);
  assert.equal(z.pozitifOlabilirlikOrani.gaAlt, null);
  assert.ok(z.uyarilar.length > 0);
  const hz = taniTesti(10, 0, 5, 20, { sifirDuzeltme: true });
  yakin(hz.tanisalOddsOrani.deger, (10.5 * 20.5) / (0.5 * 5.5), 1e-14, 0, 'Haldane DOR');
  // k x k kappa
  const k3 = cohenKappa(R.kappa3.tablo);
  yakin(k3.deger, R.kappa3.kappa, 1e-12, 1e-14, 'kappa 3x3');
  yakin(k3.sh, R.kappa3.se, 1e-12, 1e-14, 'kappa 3x3 SH');
  yakin(k3.sh0, R.kappa3.se0, 1e-12, 1e-14, 'kappa 3x3 SH0');
  if (R.kappa3.psych) yakin(k3.sh, R.kappa3.psych[1], 1e-10, 1e-14, 'kappa 3x3 psych');
  // Hata mesajları ve uç durumlar
  assert.throws(() => taniTesti(1, 2, 3, -1), IstatistikHatasi);
  assert.throws(() => taniTesti(1.5, 2, 3, 4), IstatistikHatasi);
  assert.throws(() => taniTesti(0, 0, 0, 0), /gözlem/);
  assert.throws(() => taniTesti(1, 2, 3, 4, { prevalans: 1.2 }), /Prevalans/);
  assert.equal(taniTesti(0, 0, 3, 4).pozitifKestirimDegeri.deger, null);
  assert.deepEqual(oranGA(0, 10, 0.95, 'wilson').gaAlt, 0);
  assert.equal(oranGA(10, 10).gaUst, 1);
});
