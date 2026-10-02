import { test } from 'node:test';
import assert from 'node:assert/strict';
import { lojistikRegresyon, tekDegiskenliLojistik, hosmerLemeshow, aucHesapla, ordinalLojistik, multinomialLojistik } from '../src/cekirdek/lojistik.js';
import { poissonRegresyon } from '../src/cekirdek/poisson.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin, yakinDerin } from './yardimci.js';

const R = referans('lojistik');
const V = R.veri;
const TOL = 1e-9; // tam yakınsamış iteratif uyum (R: glm.control(epsilon = 1e-14))

const sur = (ad, degerler) => ({ ad, tur: 'surekli', degerler });
const kat = (ad, degerler, ek = {}) => ({ ad, tur: 'kategorik', degerler, ...ek });

/** lojistikRegresyon sonucunu R lojOzet() çıktısıyla karşılaştırır */
function lojKarsilastir(s, r, etiket, { profil = true, terim = true } = {}) {
  assert.equal(s.n, r.n, `${etiket}: n`);
  assert.equal(s.olaySayisi, r.olay, `${etiket}: olay sayısı`);
  assert.equal(s.katsayilar.length, r.B.length, `${etiket}: katsayı sayısı`);
  s.katsayilar.forEach((k, j) => {
    const m = `${etiket} ${k.ad}`;
    yakin(k.B, r.B[j], TOL, 1e-12, `${m} B`);
    yakin(k.SH, r.SH[j], TOL, 0, `${m} SH`);
    yakin(k.z, r.z[j], TOL, 1e-12, `${m} z`);
    if (r.z[j] !== null) yakin(k.wald, r.z[j] ** 2, TOL, 1e-20, `${m} Wald`);
    yakin(k.p, r.p[j], 1e-8, 1e-300, `${m} p`);
    yakin(k.OR, r.OR[j], TOL, 0, `${m} OR`);
    yakin(k.orGaAlt, r.orGaAlt[j], TOL, 0, `${m} OR GA alt`);
    yakin(k.orGaUst, r.orGaUst[j], TOL, 0, `${m} OR GA üst`);
    if (profil) {
      yakin(k.profilGaAlt, r.profilAlt[j], 1e-9, 1e-11, `${m} profil alt`);
      yakin(k.profilGaUst, r.profilUst[j], 1e-9, 1e-11, `${m} profil üst`);
      // R confint() profili spline ile enterpole eder: ~1e-3 göreli içinde aynı (kesin değerler yukarıda uniroot ile)
      yakin(k.profilGaAlt, r.confintAlt[j], 1e-2, 1e-3, `${m} confint alt`);
      yakin(k.profilGaUst, r.confintUst[j], 1e-2, 1e-3, `${m} confint üst`);
    }
  });
  const o = s.modelOzeti;
  yakin(o.ikiLL, r.D, 1e-11, 0, `${etiket} -2LL`);
  yakin(o.ikiLLSifir, r.D0, 1e-11, 0, `${etiket} -2LL sıfır`);
  yakin(o.kiKare, r.D0 - r.D, 1e-10, 0, `${etiket} omnibus χ²`);
  assert.equal(o.sd, r.sd, `${etiket} sd`);
  yakin(o.AIC, r.AIC, 1e-11, 0, `${etiket} AIC`);
  yakin(o.BIC, r.BIC, 1e-11, 0, `${etiket} BIC`);
  yakin(o.logOlabilirlik, r.logLik, 1e-11, 0, `${etiket} logLik`);
  yakin(o.coxSnellR2, r.coxSnell, 1e-10, 0, `${etiket} Cox-Snell`);
  yakin(o.nagelkerkeR2, r.nagelkerke, 1e-10, 0, `${etiket} Nagelkerke`);
  yakin(o.mcFaddenR2, r.mcfadden, 1e-10, 0, `${etiket} McFadden`);
  assert.equal(o.iterasyon, r.iter, `${etiket} iterasyon (R fit$iter)`);
  // kovaryans
  const idx = s.katsayilar.map((k, j) => j).filter((j) => !s.katsayilar[j].bagimli);
  idx.forEach((a, ka) => idx.forEach((b, kb) => yakin(s.kovaryans[a][b], r.vcov[ka][kb], 1e-8, 1e-14, `${etiket} vcov[${a},${b}]`)));
  // terim testleri
  if (terim && r.terimler) {
    assert.equal(s.terimTestleri.length, r.terimler.length, `${etiket} terim sayısı`);
    s.terimTestleri.forEach((t, k) => {
      const rt = r.terimler[k];
      assert.equal(t.sd, rt.sd, `${etiket} ${t.ad} sd`);
      if (rt.sd === 0) {
        // tümüyle bağımlı (aliased) terim: test tanımsız
        assert.equal(t.wald, null);
        assert.equal(t.lrKiKare, null);
        return;
      }
      yakin(t.wald, rt.wald, 1e-8, 0, `${etiket} ${t.ad} Wald`);
      yakin(t.waldP, rt.waldP, 1e-8, 1e-300, `${etiket} ${t.ad} Wald p`);
      yakin(t.lrKiKare, rt.lr, 1e-9, 1e-12, `${etiket} ${t.ad} LR`);
      yakin(t.lrP, rt.lrP, 1e-8, 1e-300, `${etiket} ${t.ad} LR p`);
    });
  }
  // skor testleri (Blok 0)
  if (r.skor) {
    const sk = s.blok0.skorTestleri;
    assert.equal(sk.length, r.skor.skor.length, `${etiket} skor test sayısı`);
    sk.forEach((t, k) => {
      yakin(t.skor, r.skor.skor[k], 1e-9, 1e-12, `${etiket} skor ${t.ad}`);
      assert.equal(t.sd, r.skor.sd[k], `${etiket} skor sd ${t.ad}`);
    });
    yakin(s.blok0.genelSkor.skor, r.skor.genel, 1e-9, 0, `${etiket} genel skor`);
    assert.equal(s.blok0.genelSkor.sd, r.skor.genelSd);
  }
  // Hosmer-Lemeshow
  const hl = s.hosmerLemeshow;
  yakin(hl.istatistik, r.hl.ist, 1e-9, 0, `${etiket} HL`);
  assert.equal(hl.sd, r.hl.sd, `${etiket} HL sd`);
  yakin(hl.p, r.hl.p, 1e-8, 0, `${etiket} HL p`);
  yakinDerin(hl.gruplar.map((g) => g.gozlenenOlay), r.hl.g1, 0, 0, `${etiket} HL gözlenen olay`);
  yakinDerin(hl.gruplar.map((g) => g.gozlenenOlmayan), r.hl.g0, 0, 0, `${etiket} HL gözlenen olmayan`);
  yakinDerin(hl.gruplar.map((g) => g.beklenenOlay), r.hl.b1, 1e-9, 1e-12, `${etiket} HL beklenen olay`);
  yakinDerin(hl.gruplar.map((g) => g.beklenenOlmayan), r.hl.b0, 1e-9, 1e-12, `${etiket} HL beklenen olmayan`);
  // sınıflama ve AUC
  const t = s.siniflama.tablo;
  assert.deepEqual([t.dogruNegatif, t.yanlisPozitif, t.yanlisNegatif, t.dogruPozitif], [r.sinif.dn, r.sinif.yp, r.sinif.yn, r.sinif.dp], `${etiket} sınıflama`);
  yakin(s.auc, r.auc, 1e-12, 0, `${etiket} AUC`);
  // tahminler ve artıklar
  const th = s.tahminler;
  yakinDerin(th.olasilik, r.mu, 1e-9, 1e-14, `${etiket} olasılık`);
  yakinDerin(th.dogrusalTahmin, r.eta, 1e-9, 1e-11, `${etiket} η`);
  yakinDerin(th.pearsonArtik, r.pearson, 1e-8, 1e-11, `${etiket} Pearson artık`);
  yakinDerin(th.sapmaArtik, r.sapma, 1e-8, 1e-11, `${etiket} sapma artık`);
  yakinDerin(th.kaldirac, r.hat, 1e-8, 1e-13, `${etiket} kaldıraç`);
  yakinDerin(th.standartPearsonArtik, r.stdPearson, 1e-8, 1e-11, `${etiket} std Pearson`);
  yakinDerin(th.standartSapmaArtik, r.stdSapma, 1e-8, 1e-11, `${etiket} std sapma`);
  yakinDerin(th.cookD, r.cook, 1e-8, 1e-14, `${etiket} Cook`);
}

test('mtcars: am ~ wt + hp', () => {
  const mt = V.mtcars;
  const s = lojistikRegresyon({ y: mt.am, tahminciler: [sur('wt', mt.wt), sur('hp', mt.hp)] });
  lojKarsilastir(s, R.mtcars, 'mtcars');
  assert.equal(s.yontem, 'lojistik-regresyon');
  assert.equal(s.olayDuzeyi, 1);
  assert.equal(s.ayrisma, null);
  assert.equal(s.yakinsadi, true);
  // EPV = min(13, 19)/2 = 6.5 < 10 → uyarı
  yakin(s.epv.deger, 6.5, 0, 0, 'EPV');
  assert.ok(s.uyarilar.some((u) => u.includes('EPV')));
  // Blok 0: yalnız sabit modeli; ȳ = 13/32 < 0.5 → tümü 0 tahmin edilir
  yakin(s.blok0.sabitB, Math.log(13 / 19), 1e-14, 0, 'blok0 sabit');
  yakin(s.blok0.siniflama.dogruSiniflamaYuzdesi, (100 * 19) / 32, 1e-14, 0, 'blok0 sınıflama');
  // sınıflama oranları
  const c = s.siniflama;
  const t = c.tablo;
  yakin(c.duyarlilik, t.dogruPozitif / (t.dogruPozitif + t.yanlisNegatif), 1e-15, 0, 'duyarlılık');
  yakin(c.ozgulluk, t.dogruNegatif / (t.dogruNegatif + t.yanlisPozitif), 1e-15, 0, 'özgüllük');
});

test('infert: 3 düzeyli faktör (düzey sırası verilmiş)', () => {
  const d = V.infert;
  const s = lojistikRegresyon({
    y: d.case,
    tahminciler: [sur('age', d.age), sur('parity', d.parity), kat('education', d.education, { duzeyler: V.duzeyler.education }), sur('spontaneous', d.spontaneous), sur('induced', d.induced)],
  });
  lojKarsilastir(s, R.infert, 'infert');
});

test('birthwt: ırk faktörü; etkileşimli model; g = 5 ve kesim 0.3', () => {
  const d = V.birthwt;
  const tah = [sur('age', d.age), sur('lwt', d.lwt), kat('race', d.race), sur('smoke', d.smoke), sur('ptl', d.ptl), sur('ht', d.ht), sur('ui', d.ui)];
  const s = lojistikRegresyon({ y: d.low, tahminciler: tah });
  lojKarsilastir(s, R.birthwt, 'birthwt');
  const s2 = lojistikRegresyon({ y: d.low, tahminciler: [sur('lwt', d.lwt), sur('smoke', d.smoke), sur('age', d.age)], etkilesimler: [['lwt', 'smoke']] });
  lojKarsilastir(s2, R.birthwtEtk, 'birthwt etkileşim');
  const s3 = lojistikRegresyon({ y: d.low, tahminciler: tah, hlGrupSayisi: 5, kesim: 0.3, profilGA: false, terimLR: false });
  lojKarsilastir(s3, R.hl5, 'birthwt g=5', { profil: false, terim: false });
  assert.equal(s3.katsayilar[1].profilGaAlt, null);
});

test('Titanic (genişletilmiş, 2201 satır): bağlı olasılıklarda HL grupları birleşir', () => {
  const d = V.titanic;
  const s = lojistikRegresyon({
    y: d.Survived,
    olay: 'Yes',
    tahminciler: [kat('Class', d.Class), kat('Sex', d.Sex, { duzeyler: ['Male', 'Female'] }), kat('Age', d.Age, { duzeyler: ['Child', 'Adult'] })],
  });
  lojKarsilastir(s, R.titanic, 'Titanic');
  assert.equal(s.hosmerLemeshow.grupSayisi, 5);
  assert.ok(s.uyarilar.some((u) => u.includes('Hosmer-Lemeshow')));
  assert.equal(s.olayDuzeyi, 'Yes');
  assert.equal(s.digerDuzey, 'No');
});

test('esoph (genişletilmiş, 975 satır): 6, 4, 4 düzeyli faktörler', () => {
  const d = V.esoph;
  const s = lojistikRegresyon({
    y: d.y,
    tahminciler: [kat('agegp', d.agegp, { duzeyler: V.duzeyler.agegp }), kat('alcgp', d.alcgp, { duzeyler: V.duzeyler.alcgp }), kat('tobgp', d.tobgp, { duzeyler: V.duzeyler.tobgp })],
  });
  lojKarsilastir(s, R.esoph, 'esoph');
});

test('sentetik: etiketli sonuç, referans C, bağımlı sütun, eksik değerler', () => {
  const d = V.sentetik;
  const s = lojistikRegresyon({
    y: d.y,
    olay: 'Var',
    tahminciler: [sur('x1', d.x1), sur('x2', d.x2), sur('x3', d.x3), kat('g', d.g, { referans: 'C' })],
  });
  lojKarsilastir(s, R.sentetik, 'sentetik');
  assert.deepEqual(s.satirlar, R.sentetik.satirlar);
  assert.deepEqual(s.bagimliSutunlar, ['x3']);
  assert.ok(s.uyarilar.some((u) => u.includes('doğrusal bileşimi')));
  assert.ok(s.uyarilar.some((u) => u.includes('liste bazında')));
  assert.deepEqual(s.sutunAdlari, ['(Sabit)', 'x1', 'x2', 'x3', 'g=A', 'g=B', 'g=D']);
  // olay düzeyini ters çevirmek tüm katsayıların işaretini değiştirir
  const t = lojistikRegresyon({ y: d.y, olay: 'Yok', tahminciler: [sur('x1', d.x1), sur('x2', d.x2), kat('g', d.g, { referans: 'C' })], profilGA: false });
  const u = lojistikRegresyon({ y: d.y, olay: 'Var', tahminciler: [sur('x1', d.x1), sur('x2', d.x2), kat('g', d.g, { referans: 'C' })], profilGA: false });
  t.katsayilar.forEach((k, j) => yakin(k.B, -u.katsayilar[j].B, 1e-10, 1e-14, 'işaret'));
});

test('sabitsiz model: sıfır modeli η = 0 (R null.deviance = 2n log 2)', () => {
  const mt = V.mtcars;
  const s = lojistikRegresyon({ y: mt.am, tahminciler: [sur('wt', mt.wt), sur('hp', mt.hp)], sabit: false });
  lojKarsilastir(s, R.sabitsiz, 'sabitsiz');
  assert.equal(s.blok0, null);
});

test('tek değişkenli lojistik regresyonlar (birthwt)', () => {
  const d = V.birthwt;
  const ad = ['age', 'lwt', 'race', 'smoke', 'ht'];
  const tah = ad.map((a) => (a === 'race' ? kat(a, d[a]) : sur(a, d[a])));
  const s = tekDegiskenliLojistik({ y: d.low, tahminciler: tah });
  assert.equal(s.sonuclar.length, 5);
  s.sonuclar.forEach((r, k) => {
    const ref = R.tekDegiskenli[k];
    const dz = (v) => [].concat(v); // jsonlite tek elemanlı vektörü sayıya indirger
    assert.equal(r.katsayilar.length, dz(ref.B).length);
    r.katsayilar.forEach((c, j) => {
      yakin(c.B, dz(ref.B)[j], TOL, 0, `${r.ad} B`);
      yakin(c.SH, dz(ref.SH)[j], TOL, 0, `${r.ad} SH`);
      yakin(c.p, dz(ref.p)[j], 1e-8, 0, `${r.ad} p`);
    });
    yakin(r.p, ref.lrP, 1e-8, 0, `${r.ad} LR p`);
    assert.equal(r.n, 189);
  });
  // ortak örneklem seçeneği: eksik değerli satır tüm modellerden çıkar
  const age = d.age.slice();
  age[0] = null;
  const o = tekDegiskenliLojistik({ y: d.low, tahminciler: [sur('age', age), sur('lwt', d.lwt)], ortakOrneklem: true, profilGA: false });
  assert.deepEqual(o.sonuclar.map((r) => r.n), [188, 188]);
  const a = tekDegiskenliLojistik({ y: d.low, tahminciler: [sur('age', age), sur('lwt', d.lwt)], profilGA: false });
  assert.deepEqual(a.sonuclar.map((r) => r.n), [188, 189]);
});

test('ayrışma: tam, yarı-tam ve kategorik düzeyde yarı-tam', () => {
  const tam = lojistikRegresyon({ y: [0, 0, 0, 0, 0, 1, 1, 1, 1, 1], tahminciler: [sur('x', [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])] });
  assert.equal(tam.ayrisma.tur, 'tam');
  assert.ok(tam.ayrisma.sutunlar.includes('x'));
  assert.equal(tam.katsayilar[1].B, null);
  assert.equal(tam.katsayilar[1].ayrisma, true);
  assert.ok(tam.uyarilar.some((u) => u.includes('Tam ayrışma')));
  assert.ok(tam.modelOzeti.ikiLL < 1e-6 && R.ayrisma.tamD < 1e-6);

  const yari = lojistikRegresyon({ y: [0, 0, 0, 0, 0, 1, 1, 1, 1, 1], tahminciler: [sur('x', [1, 2, 3, 4, 5, 5, 6, 7, 8, 9])] });
  assert.equal(yari.ayrisma.tur, 'yari-tam');
  assert.ok(yari.uyarilar.some((u) => u.includes('Yarı-tam ayrışma')));
  // sınırdaki sapma: x = 5'teki iki gözlem p = 0.5 → 2·2·log 2
  yakin(yari.modelOzeti.ikiLL, 4 * Math.LN2, 1e-8, 0, 'yarı-tam -2LL');
  yakin(yari.modelOzeti.ikiLL, R.ayrisma.yariD, 1e-6, 0, 'yarı-tam -2LL (R)');

  const mt = V.mtcars;
  const m = lojistikRegresyon({ y: mt.vs, tahminciler: [sur('mpg', mt.mpg), kat('cyl', mt.cyl)] });
  assert.equal(m.ayrisma.tur, 'yari-tam');
  assert.deepEqual(m.ayrisma.sutunlar, ['cyl=8']);
  assert.equal(m.katsayilar[3].B, null);
  // ıraksamayan katsayılar R ile aynı (R'nin kendi yakınsama noktasında; ıraksayan yön diğerlerini etkilemez)
  yakin(m.katsayilar[1].B, R.ayrisma.mtB[1], 1e-6, 0, 'mpg B');
  yakin(m.katsayilar[2].B, R.ayrisma.mtB[2], 1e-6, 0, 'cyl=6 B');
  yakin(m.katsayilar[1].SH, R.ayrisma.mtSE[1], 1e-6, 0, 'mpg SH');
  yakin(m.modelOzeti.ikiLL, R.ayrisma.mtD, 1e-6, 0, 'mt -2LL');
  // ayrışma varken terim Wald testi ıraksayan sütunları kullanmaz
  assert.equal(m.terimTestleri[1].sd, 1);
});

test('Hosmer-Lemeshow ve AUC yardımcıları', () => {
  // AUC: bağlarda 1/2 katkı
  yakin(aucHesapla([0, 0, 1, 1], [0.1, 0.5, 0.5, 0.9]), (1 + 1 + 1 + 0.5) / 4, 1e-15, 0, 'AUC bağ');
  assert.equal(aucHesapla([1, 1], [0.2, 0.3]), null);
  // Tüm olasılıklar aynı → tek grup → HL hesaplanamaz
  const h = hosmerLemeshow([0, 1, 0, 1], [0.5, 0.5, 0.5, 0.5]);
  assert.equal(h.istatistik, null);
});

test('hata durumları', () => {
  assert.throws(() => lojistikRegresyon({ y: [1, 2, 3, 1], tahminciler: [sur('x', [1, 2, 3, 4])] }), /iki düzeyli/);
  assert.throws(() => lojistikRegresyon({ y: [1, 1, 1], tahminciler: [sur('x', [1, 2, 3])] }), IstatistikHatasi);
  assert.throws(() => lojistikRegresyon({ y: [0, 1, null, 1], tahminciler: [sur('x', [1, 2, 3, null])], olay: 5 }), /Olay düzeyi/);
  assert.throws(() => lojistikRegresyon({ y: [0, 1, 0, 1], tahminciler: [sur('x', [1, 2, 3, 4])], kesim: 1.5 }), IstatistikHatasi);
});

// ---------------- Ordinal lojistik ----------------

/** ordinalLojistik sonucunu R ordOzet() (ordinal::clm, brant, polr) ile karşılaştırır */
function ordKarsilastir(s, r, etiket, { brant = true } = {}) {
  assert.equal(s.n, r.n, `${etiket}: n`);
  const B = [...s.esikler.map((e) => e.B), ...s.katsayilar.map((k) => k.B)];
  const SH = [...s.esikler.map((e) => e.SH), ...s.katsayilar.map((k) => k.SH)];
  yakinDerin(B, r.B, 1e-9, 1e-12, `${etiket} B`);
  yakinDerin(SH, r.SH, 1e-8, 0, `${etiket} SH`);
  yakinDerin(s.kovaryans, r.vcov, 1e-8, 1e-14, `${etiket} vcov`);
  // MASS::polr (BFGS, optim reltol ~1e-8): gevşek
  yakinDerin(B, r.polr, 1e-4, 1e-5, `${etiket} polr`);
  const o = s.modelOzeti;
  yakin(o.logOlabilirlik, r.ll, 1e-12, 0, `${etiket} logLik`);
  yakin(o.ikiLLSifir, -2 * r.ll0, 1e-12, 0, `${etiket} -2LL sıfır`);
  yakin(o.AIC, r.AIC, 1e-12, 0, `${etiket} AIC`);
  yakin(o.BIC, r.BIC, 1e-12, 0, `${etiket} BIC`);
  s.katsayilar.forEach((k, j) => yakin(k.OR, Math.exp(r.B[s.esikler.length + j]), 1e-9, 0, `${etiket} OR`));
  s.terimTestleri.forEach((t, k) => {
    assert.equal(t.sd, r.terimWald[k].sd);
    yakin(t.wald, r.terimWald[k].wald, 1e-8, 0, `${etiket} ${t.ad} Wald`);
    yakin(t.waldP, r.terimWald[k].waldP, 1e-8, 1e-300, `${etiket} ${t.ad} Wald p`);
    yakin(t.lrKiKare, r.lr[k], 1e-9, 1e-12, `${etiket} ${t.ad} LR`);
    yakin(t.lrP, r.lrP[k], 1e-8, 1e-300, `${etiket} ${t.ad} LR p`);
  });
  // Paralel çizgiler (genel kümülatif model = clm nominal)
  yakin(s.paralelCizgiler.ikiLLGenel, -2 * r.llNom, 1e-10, 0, `${etiket} genel model -2LL`);
  yakin(s.paralelCizgiler.kiKare, 2 * (r.llNom - r.ll), 1e-7, 1e-9, `${etiket} paralel χ²`);
  // Brant
  if (!brant) {
    // R brant::brant ayrışmış ikili modelle ("combinations do not occur" uyarısıyla) yakınsama noktasına bağlı bir değer verir
    assert.equal(s.brant, null);
    return;
  }
  // kesin: brant formülü tam yakınsamış ikili modellerle (R'de); brant::brant (varsayılan glm ölçütü, eski ağırlıklar) ~1e-2 içinde
  yakin(s.brant.genel.kiKare, r.brantKesin[0], 1e-8, 0, `${etiket} Brant genel`);
  yakin(s.brant.genel.kiKare, r.brant[0], 1e-2, 0, `${etiket} brant::brant genel`);
  assert.equal(s.brant.genel.sd, r.brantSd[0]);
  s.brant.degiskenler.forEach((d, j) => {
    yakin(d.kiKare, r.brantKesin[j + 1], 1e-8, 1e-12, `${etiket} Brant ${d.ad}`);
    yakin(d.kiKare, r.brant[j + 1], 1e-2, 1e-4, `${etiket} brant::brant ${d.ad}`);
    assert.equal(d.sd, r.brantSd[j + 1]);
  });
}

test('ordinal lojistik: wine (clm), housing (1681 satır, 3 faktör), sentetik (eksik değerler)', () => {
  const w = V.wine;
  const s = ordinalLojistik({ y: w.rating, tahminciler: [kat('temp', w.temp), kat('contact', w.contact)] });
  ordKarsilastir(s, R.ordinal.wine, 'wine', { brant: false });
  assert.ok(s.uyarilar.some((u) => u.includes('üst sınırıyla')));
  assert.deepEqual(s.duzeyler, ['1', '2', '3', '4', '5']);
  assert.deepEqual(s.esikler.map((e) => e.ad), ['1|2', '2|3', '3|4', '4|5']);
  // her gözlemin kendi kategorisinin olasılığı = fitted(clm)
  const yak = s.tahminler.olasiliklar.map((pr, i) => pr[s.duzeyler.indexOf(w.rating[i])]);
  yakinDerin(yak, R.ordinal.wine.olasilik, 1e-9, 1e-14, 'wine fitted');
  s.tahminler.olasiliklar.forEach((pr) => yakin(pr.reduce((a, b) => a + b), 1, 1e-14, 1e-14, 'olasılık toplamı'));

  const h = V.housing;
  const sh = ordinalLojistik({
    y: h.Sat,
    duzeyler: ['Low', 'Medium', 'High'],
    tahminciler: [
      kat('Infl', h.Infl, { duzeyler: ['Low', 'Medium', 'High'] }),
      kat('Type', h.Type, { duzeyler: ['Tower', 'Apartment', 'Atrium', 'Terrace'] }),
      kat('Cont', h.Cont, { duzeyler: ['Low', 'High'] }),
    ],
  });
  ordKarsilastir(sh, R.ordinal.housing, 'housing');

  const d = V.ordSentetik;
  const ss = ordinalLojistik({ y: d.y, duzeyler: ['hic', 'az', 'orta', 'cok'], tahminciler: [sur('x', d.x), kat('g', d.g)] });
  ordKarsilastir(ss, R.ordinal.sentetik, 'ordinal sentetik');
  assert.ok(ss.uyarilar.some((u) => u.includes('liste bazında')));
  // Brant/paralel testi reddederse uyarı
  if (ss.paralelCizgiler.p < 0.05) assert.ok(ss.uyarilar.some((u) => u.includes('paralel')));
});

test('ordinal lojistik: ayrışma ve hata durumları', () => {
  // mtcars: gear = 3 → am = 0, gear = 5 → am = 1 (yarı-tam ayrışma)
  const mt = V.mtcars;
  const s = ordinalLojistik({ y: mt.gear, tahminciler: [sur('wt', mt.wt), sur('am', mt.am)] });
  assert.equal(s.yakinsadi, false);
  assert.equal(s.katsayilar[1].B, null);
  assert.ok(s.uyarilar.some((u) => u.includes('ayrışma')));
  assert.throws(() => ordinalLojistik({ y: [1, 2, 1, 2], tahminciler: [sur('x', [1, 2, 3, 4])] }), /üç/);
  assert.throws(() => ordinalLojistik({ y: [1, 2, 3], tahminciler: [] }), IstatistikHatasi);
});

// ---------------- Multinominal lojistik ----------------

function mnKarsilastir(s, r, etiket, { nnet = true } = {}) {
  assert.equal(s.n, r.olasilik.length, `${etiket}: n`);
  s.kategoriler.forEach((c, a) => {
    yakinDerin(c.katsayilar.map((k) => k.B), r.B[a], 1e-8, 1e-11, `${etiket} ${c.kategori} B`);
    yakinDerin(c.katsayilar.map((k) => k.SH), r.SH[a], 1e-7, 0, `${etiket} ${c.kategori} SH`);
    // nnet::multinom (BFGS): gevşek
    if (nnet) yakinDerin(c.katsayilar.map((k) => k.B), r.nnet[a], 1e-5, 1e-6, `${etiket} ${c.kategori} nnet`);
    c.katsayilar.forEach((k) => yakin(k.OR, Math.exp(k.B), 1e-15, 0, 'OR'));
  });
  const o = s.modelOzeti;
  yakin(o.logOlabilirlik, r.ll, 1e-11, 0, `${etiket} logLik`);
  yakin(o.ikiLLSifir, -2 * r.ll0, 1e-11, 0, `${etiket} -2LL sıfır`);
  yakin(o.AIC, r.AIC, 1e-11, 0, `${etiket} AIC`);
  s.olabilirlikOraniTestleri.forEach((t, k) => yakin(t.ikiLLIndirgenmis, r.ikiLLRed[k], 1e-10, 0, `${etiket} ${t.ad} -2LL indirgenmiş`));
  yakinDerin(s.tahminler.olasiliklar, r.olasilik, 1e-8, 1e-12, `${etiket} olasılıklar`);
}

test('multinominal lojistik: housing (ilk ve son referans), sentetik (eksik değerler)', () => {
  const h = V.housing;
  const tah = [
    kat('Infl', h.Infl, { duzeyler: ['Low', 'Medium', 'High'] }),
    kat('Type', h.Type, { duzeyler: ['Tower', 'Apartment', 'Atrium', 'Terrace'] }),
    kat('Cont', h.Cont, { duzeyler: ['Low', 'High'] }),
  ];
  const s = multinomialLojistik({ y: h.Sat, duzeyler: ['Low', 'Medium', 'High'], tahminciler: tah });
  mnKarsilastir(s, R.multinom.housing, 'housing');
  assert.equal(s.referansKategori, 'Low');
  assert.deepEqual(s.kategoriler.map((c) => c.kategori), ['Medium', 'High']);
  assert.equal(s.modelOzeti.sd, 12);
  const t = s.siniflama.tablo;
  assert.equal(t.flat().reduce((a, b) => a + b), 1681);
  // SPSS NOMREG varsayılanı: son kategori referans
  const s2 = multinomialLojistik({ y: h.Sat, duzeyler: ['Low', 'Medium', 'High'], referansKategori: 'High', tahminciler: tah });
  mnKarsilastir(s2, R.multinom.housingSon, 'housing ref=High', { nnet: false }); // nnet hep ilk düzeyi referans alır
  yakin(s2.modelOzeti.ikiLL, s.modelOzeti.ikiLL, 1e-12, 0, 'referanstan bağımsız -2LL');
  const d = V.mnSentetik;
  const s3 = multinomialLojistik({ y: d.y, tahminciler: [sur('x1', d.x1), sur('x2', d.x2), kat('g', d.g)] });
  mnKarsilastir(s3, R.multinom.sentetik, 'multinom sentetik');
  assert.equal(s3.n, 238);
});

// ---------------- Poisson ----------------

function poiKarsilastir(s, r, etiket) {
  assert.equal(s.n, r.n, `${etiket}: n`);
  s.katsayilar.forEach((k, j) => {
    const m = `${etiket} ${k.ad}`;
    yakin(k.B, r.B[j], 1e-9, 1e-12, `${m} B`);
    yakin(k.SH, r.SH[j], 1e-9, 0, `${m} SH`);
    yakin(k.p, r.p[j], 1e-8, 1e-300, `${m} p`);
    yakin(k.IRR, r.IRR[j], 1e-9, 0, `${m} IRR`);
    yakin(k.irrGaAlt, r.irrGaAlt[j], 1e-9, 0, `${m} IRR GA alt`);
    yakin(k.irrGaUst, r.irrGaUst[j], 1e-9, 0, `${m} IRR GA üst`);
    yakin(k.profilGaAlt, r.profilAlt[j], 1e-9, 1e-11, `${m} profil alt`);
    yakin(k.profilGaUst, r.profilUst[j], 1e-9, 1e-11, `${m} profil üst`);
  });
  yakinDerin(s.kovaryans, r.vcov, 1e-8, 1e-14, `${etiket} vcov`);
  const o = s.modelOzeti;
  yakin(o.sapma, r.D, 1e-10, 1e-12, `${etiket} sapma`);
  yakin(o.sapmaSifir, r.D0, 1e-10, 0, `${etiket} sıfır sapma`);
  assert.equal(o.sapmaSd, r.dfRes);
  yakin(o.pearsonKiKare, r.pearson, 1e-9, 0, `${etiket} Pearson χ²`);
  yakin(o.AIC, r.AIC, 1e-11, 0, `${etiket} AIC`);
  yakin(o.BIC, r.BIC, 1e-11, 0, `${etiket} BIC`);
  yakin(o.logOlabilirlik, r.logLik, 1e-11, 0, `${etiket} logLik`);
  assert.equal(o.iterasyon, r.iter, `${etiket} iterasyon`);
  const dz = (v) => [].concat(v); // jsonlite tek elemanlı vektörü sayıya indirger
  s.terimTestleri.forEach((t, k) => {
    yakin(t.lrKiKare, dz(r.lr)[k], 1e-9, 1e-12, `${etiket} ${t.ad} LR`);
    yakin(t.lrP, dz(r.lrP)[k], 1e-8, 1e-300, `${etiket} ${t.ad} LR p`);
  });
  yakinDerin(s.tahminler.beklenen, r.mu, 1e-9, 1e-12, `${etiket} μ`);
  yakinDerin(s.tahminler.kaldirac, r.hat, 1e-8, 1e-13, `${etiket} kaldıraç`);
  yakinDerin(s.tahminler.cookD, r.cook, 1e-8, 1e-14, `${etiket} Cook`);
  yakinDerin(s.tahminler.standartSapmaArtik, r.stdSapma, 1e-8, 1e-11, `${etiket} std sapma artığı`);
}

test('Poisson regresyonu: warpbreaks (aşırı yayılım), InsectSprays, Insurance (offset), sentetik (maruziyet, eksik)', () => {
  const wb = V.warpbreaks;
  const s = poissonRegresyon({ y: wb.breaks, tahminciler: [kat('wool', wb.wool), kat('tension', wb.tension, { duzeyler: ['L', 'M', 'H'] })] });
  poiKarsilastir(s, R.poisson.warpbreaks, 'warpbreaks');
  assert.ok(s.modelOzeti.pearsonOran > 1.5);
  assert.ok(s.uyarilar.some((u) => u.includes('Aşırı yayılım')));
  const is = V.InsectSprays;
  poiKarsilastir(poissonRegresyon({ y: is.count, tahminciler: [kat('spray', is.spray)] }), R.poisson.insect, 'InsectSprays');
  const ins = V.Insurance;
  const si = poissonRegresyon({
    y: ins.Claims,
    maruziyet: ins.Holders,
    tahminciler: [kat('District', ins.District), kat('Group', ins.Group, { duzeyler: V.duzeyler.Group }), kat('Age', ins.Age, { duzeyler: V.duzeyler.AgeIns })],
  });
  poiKarsilastir(si, R.poisson.insurance, 'Insurance');
  // offset doğrudan verilirse aynı sonuç
  const si2 = poissonRegresyon({
    y: ins.Claims,
    offset: ins.Holders.map(Math.log),
    tahminciler: [kat('District', ins.District), kat('Group', ins.Group, { duzeyler: V.duzeyler.Group }), kat('Age', ins.Age, { duzeyler: V.duzeyler.AgeIns })],
    profilGA: false,
  });
  yakin(si2.katsayilar[3].B, si.katsayilar[3].B, 1e-13, 0, 'offset = log(maruziyet)');
  const d = V.poiSentetik;
  poiKarsilastir(poissonRegresyon({ y: d.y, maruziyet: d.t, tahminciler: [sur('x', d.x), kat('g', d.g)] }), R.poisson.sentetik, 'Poisson sentetik');
});

test('Poisson: tüm sayımları sıfır olan kategori ve hata durumları', () => {
  const y = [0, 0, 0, 0, 2, 3, 1, 4, 1, 2, 5, 0];
  const g = ['a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c'];
  const s = poissonRegresyon({ y, tahminciler: [kat('g', g, { referans: 'b' })] });
  assert.equal(s.katsayilar[1].ad, 'g=a');
  assert.equal(s.katsayilar[1].B, null);
  assert.equal(s.katsayilar[1].ayrisma, true);
  assert.ok(s.uyarilar.some((u) => u.includes('ıraksıyor')));
  assert.throws(() => poissonRegresyon({ y: [1, -1, 2], tahminciler: [sur('x', [1, 2, 3])] }), /negatif/);
  assert.throws(() => poissonRegresyon({ y: [1, 2, 3], maruziyet: [1, 0, 2], tahminciler: [sur('x', [1, 2, 3])] }), /pozitif/);
});
