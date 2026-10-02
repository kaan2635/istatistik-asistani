// Uyum ve güvenirlik istatistikleri: R (irr, psych, vcd, epiR, blandr, lavaan/MBESS) ile karşılaştırma.
// Referans: arac/r-referans/uyum.R
import { test } from 'node:test';
import assert from 'node:assert/strict';
import * as U from '../src/cekirdek/uyum.js';
import * as G from '../src/cekirdek/guvenirlik.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin, yakinDerin } from './yardimci.js';

const R = referans('uyum');
const T = 1e-10; // kapalı formüller: göreli tolerans
const P_M = 1e-15; // p değerleri için mutlak tolerans (R 1 − pnorm kullanır)

// ---------- Cohen kappa ----------

test('Cohen kappa: ağırlıksız, doğrusal, kuadratik (irr::kappa2, vcd::Kappa, Fleiss-Cohen-Everitt SH)', () => {
  for (const r of R.kappa) {
    const m = `${r.veri}/${r.agirlik}`;
    const s = U.cohenKappa(r.r1, r.r2, { agirlik: r.agirlik, duzey: r.veri === 'kucuk' ? 0.9 : 0.95 });
    yakin(s.kappa, r.kappa, T, 1e-15, `kappa ${m}`);
    yakin(s.istatistik, r.z, T, 1e-15, `z ${m}`);
    yakin(s.p, r.pKesin, 1e-9, P_M, `p ${m}`);
    yakin(s.p, r.p, 1e-9, 1e-14, `p (irr) ${m}`);
    yakin(s.sh0, r.sh0, T, 0, `SH0 ${m}`);
    yakin(s.sh, r.sh, T, 0, `SH ${m}`);
    yakin(s.sh, r.shVcd, T, 0, `SH (vcd) ${m}`);
    yakin(s.gozlenenUyum, r.po, T, 1e-15, `po ${m}`);
    yakin(s.beklenenUyum, r.pe, T, 1e-15, `pe ${m}`);
    yakin(s.ga.alt, r.gaAlt, T, 1e-15, `GA alt ${m}`);
    yakin(s.ga.ust, r.gaUst, T, 1e-15, `GA üst ${m}`);
    yakin(s.yuzdeUyum, r.yuzde, T, 0, `% uyum ${m}`);
    assert.equal(s.n, r.n);
    assert.deepEqual(s.duzeyler, r.duzeyler, `düzeyler ${m}`);
    assert.deepEqual(s.tablo, r.tablo, `tablo ${m}`);
  }
  // Eksik çiftler uyarısı; kategori listesi verilerek de aynı sonuç
  const sen = R.kappa.find((r) => r.veri === 'sentetik' && r.agirlik === 'kuadratik');
  const s = U.cohenKappa(sen.r1, sen.r2, { agirlik: 'kuadratik', duzeyler: [1, 2, 3, 4, 5] });
  yakin(s.kappa, sen.kappa, T);
  assert.ok(s.uyarilar.some((u) => u.includes('Eksik')));
  // Ağırlıklı kappada kategori listesi sırası önemlidir
  const a = ['düşük', 'orta', 'yüksek', 'orta', 'düşük', 'yüksek', 'orta'];
  const b = ['düşük', 'yüksek', 'yüksek', 'orta', 'orta', 'orta', 'orta'];
  const sirali = U.cohenKappa(a, b, { agirlik: 'dogrusal', duzeyler: ['düşük', 'orta', 'yüksek'] });
  const kod = U.cohenKappa(a.map((v) => ({ düşük: 1, orta: 2, yüksek: 3 })[v]), b.map((v) => ({ düşük: 1, orta: 2, yüksek: 3 })[v]), { agirlik: 'dogrusal' });
  yakin(sirali.kappa, kod.kappa, 1e-15);
  assert.ok(U.cohenKappa(a, b, { agirlik: 'dogrusal' }).uyarilar.some((u) => u.includes('alfabetik')));
  // Hatalar
  assert.throws(() => U.cohenKappa([1, 1, 1], [1, 1, 1]), IstatistikHatasi);
  assert.throws(() => U.cohenKappa([1, 2], [1]), IstatistikHatasi);
  assert.throws(() => U.cohenKappa([1, 2, 3], [1, 2, 4], { duzeyler: [1, 2, 3] }), /kategori listesinde yok/);
  // Tam uyum: κ = 1, H0 dışı SH = 0
  const tam = U.cohenKappa([1, 2, 3, 1, 2, 3], [1, 2, 3, 1, 2, 3]);
  yakin(tam.kappa, 1, 1e-15);
  yakin(tam.sh, 0, 0, 1e-15);
});

// ---------- Fleiss kappa ----------

test('Fleiss kappa (irr::kappam.fleiss, detail = TRUE)', () => {
  for (const [ad, r] of Object.entries(R.fleiss)) {
    const s = U.fleissKappa(r.matris);
    yakin(s.kappa, r.kappa, T, 0, `kappa ${ad}`);
    yakin(s.istatistik, r.z, T, 0, `z ${ad}`);
    yakin(s.p, r.pKesin, 1e-9, P_M, `p ${ad}`);
    yakin(s.sh, r.sh, T, 0, `SH ${ad}`);
    yakin(s.gozlenenUyum, r.po, T, 0, `Po ${ad}`);
    yakin(s.beklenenUyum, r.pe, T, 0, `Pe ${ad}`);
    assert.equal(s.n, r.n);
    assert.deepEqual(s.duzeyler.map(String), r.duzeyler.map(String));
    s.kategoriler.forEach((k, j) => {
      yakin(k.kappa, r.kategoriKappa[j], T, 1e-15, `kategori kappa ${ad}[${j}]`);
      yakin(k.z, r.kategoriZ[j], T, 1e-14, `kategori z ${ad}[${j}]`);
      yakin(k.p, r.kategoriP[j], 1e-9, P_M, `kategori p ${ad}[${j}]`);
      yakin(k.sh, r.kategoriSh, T, 0, 'kategori SH');
      yakin(k.oran, r.oranlar[j], T, 0, 'oran');
    });
    const z = 1.959963984540054;
    yakin(s.ga.alt, r.kappa - z * r.sh, T);
  }
  assert.ok(U.fleissKappa(R.fleiss.sentetik.matris).uyarilar.some((u) => u.includes('Eksik')));
  assert.throws(() => U.fleissKappa([[1, 1], [1, 1]]), IstatistikHatasi);
});

// ---------- ICC ----------

const MODEL = { oneway: 'tek-yonlu', twoway: 'iki-yonlu-rastgele' };
const TIP = { consistency: 'tutarlilik', agreement: 'uyum' };
const BIRIM = { single: 'tek', average: 'ortalama' };

test('ICC: tek yönlü ve tutarlılık biçimleri (irr::icc ile birebir)', () => {
  for (const [ad, vaka] of Object.entries(R.icc)) {
    for (const r of vaka.irr.filter((x) => x.model === 'oneway' || x.tip === 'consistency')) {
      const m = `${ad} ${r.model}/${r.tip}/${r.birim}/r0=${r.r0}`;
      for (const model of r.model === 'oneway' ? ['tek-yonlu'] : ['iki-yonlu-rastgele', 'iki-yonlu-karma']) {
        const s = U.icc(vaka.matris, { model, tip: TIP[r.tip], birim: BIRIM[r.birim], r0: r.r0 });
        yakin(s.icc, r.icc, T, 1e-15, `ICC ${m}`);
        yakin(s.istatistik, r.F, T, 0, `F ${m}`);
        assert.deepEqual(s.sd, [r.df1, r.df2], `sd ${m}`);
        yakin(s.p, r.p, 1e-9, 1e-15, `p ${m}`);
        yakin(s.ga.alt, r.alt, T, 1e-14, `GA alt ${m}`);
        yakin(s.ga.ust, r.ust, T, 1e-14, `GA üst ${m}`);
      }
    }
  }
});

test('ICC: mutlak uyum (SPSS/psych/McGraw-Wong; irr değişkeni ayrıca)', () => {
  for (const [ad, vaka] of Object.entries(R.icc)) {
    const ps = Object.fromEntries(vaka.psych.map((x) => [x.tur, x]));
    for (const r of vaka.irr.filter((x) => x.tip === 'agreement')) {
      const m = `${ad} ${r.birim}/r0=${r.r0}`;
      const s = U.icc(vaka.matris, { model: 'iki-yonlu-rastgele', tip: 'uyum', birim: BIRIM[r.birim], r0: r.r0 });
      yakin(s.icc, r.icc, T, 1e-15, `ICC ${m}`);
      yakin(s.istatistik, r.F, T, 0, `F ${m}`);
      // irr'nin kendi sd/p/GA'sı
      yakin(s.irr.sd2, r.df2, T, 0, `irr sd2 ${m}`);
      yakin(s.irr.p, r.p, 1e-9, 1e-15, `irr p ${m}`);
      yakin(s.irr.gaAlt, r.alt, 1e-9, 1e-14, `irr GA alt ${m}`);
      yakin(s.irr.gaUst, r.ust, 1e-9, 1e-14, `irr GA üst ${m}`);
      // Ana sonuç: tek ölçümde GA irr ile aynı; ortalama ölçümde psych ICC2k (McGraw-Wong)
      const ref = r.birim === 'single' ? ps.ICC2 : ps.ICC2k;
      yakin(s.ga.alt, r.birim === 'single' ? r.alt : ref.alt, 1e-9, 1e-14, `GA alt ${m}`);
      yakin(s.ga.ust, r.birim === 'single' ? r.ust : ref.ust, 1e-9, 1e-14, `GA üst ${m}`);
      if (r.r0 === 0) {
        assert.deepEqual(s.sd, [ref.df1, ref.df2], `sd ${m}`);
        yakin(s.p, ref.p, 1e-9, 1e-15, `p ${m}`);
      } else {
        const mw = r.birim === 'single' ? vaka.mwTek : vaka.mwOrt;
        yakin(s.istatistik, mw[0], T, 0, `MW F ${m}`);
        yakin(s.sd[1], mw[1], T, 0, `MW sd2 ${m}`);
        yakin(s.p, mw[2], 1e-9, 1e-15, `MW p ${m}`);
      }
      // İki yönlü karma model aynı sayıları verir
      const k = U.icc(vaka.matris, { model: 'iki-yonlu-karma', tip: 'uyum', birim: BIRIM[r.birim], r0: r.r0 });
      assert.equal(k.icc, s.icc);
      assert.equal(k.ga.alt, s.ga.alt);
    }
    const s90 = U.icc(vaka.matris, { model: 'iki-yonlu-rastgele', tip: 'uyum', duzey: 0.9 });
    yakin(s90.icc, vaka.irr90.icc, T);
    yakin(s90.ga.alt, vaka.irr90.alt, 1e-9, 1e-14, `GA90 alt ${ad}`);
    yakin(s90.ga.ust, vaka.irr90.ust, 1e-9, 1e-14, `GA90 üst ${ad}`);
  }
});

test('ICC: altı Shrout-Fleiss biçimi (psych::ICC, lmer = FALSE)', () => {
  for (const [ad, vaka] of Object.entries(R.icc)) {
    const s = U.iccTumu(vaka.matris);
    assert.equal(s.satirlar.length, 6);
    vaka.psych.forEach((r, i) => {
      const x = s.satirlar[i];
      const m = `${ad} ${r.tur}`;
      assert.equal(x.tur, r.tur);
      yakin(x.icc, r.icc, T, 1e-15, `ICC ${m}`);
      yakin(x.F, r.F, T, 0, `F ${m}`);
      assert.equal(x.sd1, r.df1);
      assert.equal(x.sd2, r.df2);
      yakin(x.p, r.p, 1e-9, 1e-15, `p ${m}`);
      yakin(x.gaAlt, r.alt, 1e-9, 1e-14, `GA alt ${m}`);
      yakin(x.gaUst, r.ust, 1e-9, 1e-14, `GA üst ${m}`);
    });
    yakin(s.anova.MSr, vaka.MS[0], T);
    yakin(s.anova.MSc, vaka.MS[1], T);
    yakin(s.anova.MSe, vaka.MS[2], T);
  }
  // Gösterimler ve eksik veri
  const s = U.icc(R.icc.sentetik.matris, { model: 'iki-yonlu-karma', tip: 'tutarlilik', birim: 'ortalama' });
  assert.equal(s.shroutFleiss, 'ICC(3,k)');
  assert.equal(s.mcGrawWong, 'ICC(C,k)');
  assert.ok(s.uyarilar.some((u) => u.includes('Eksik')));
  assert.equal(U.icc(R.icc.sentetik.matris, { model: 'tek-yonlu' }).shroutFleiss, 'ICC(1,1)');
  assert.equal(U.icc(R.icc.sentetik.matris, { model: 'iki-yonlu-rastgele', tip: 'uyum' }).shroutFleiss, 'ICC(2,1)');
  // ICC(3,k) = Cronbach alfa
  const sf = R.icc.shroutFleiss.matris;
  yakin(U.icc(sf, { birim: 'ortalama' }).icc, G.cronbachAlfa(sf).alfa, 1e-13);
  yakin(U.icc(sf, { birim: 'ortalama' }).ga.alt, G.cronbachAlfa(sf).ga.alt, 1e-12);
  assert.throws(() => U.icc([[1, 2]]), IstatistikHatasi);
  assert.throws(() => U.icc([[1, 1], [2, 2], [3, 3]], { model: 'tek-yonlu' }), IstatistikHatasi);
});

// ---------- Bland-Altman ----------

test('Bland-Altman (elle R; blandr; kesin GA: merkezi olmayan t)', () => {
  for (const [ad, r] of Object.entries(R.bland)) {
    if (ad === 'blandr') continue;
    const s = U.blandAltman(r.x, r.y, { duzey: r.duzey, carpan: r.carpan, farkTipi: r.yuzde ? 'yuzde' : 'fark' });
    const m = ad;
    assert.equal(s.n, r.n);
    yakin(s.bias, r.bias, T, 1e-14, `bias ${m}`);
    yakin(s.ssFark, r.ss, T, 0, `SS ${m}`);
    yakin(s.biasGa.alt, r.biasAlt, T, 1e-14, `bias GA ${m}`);
    yakin(s.biasGa.ust, r.biasUst, T, 1e-14, `bias GA ${m}`);
    yakin(s.p, r.tP, 1e-9, 1e-15, `t p ${m}`);
    yakin(s.altSinir, r.altLoA, T, 0, `LoA alt ${m}`);
    yakin(s.ustSinir, r.ustLoA, T, 0, `LoA üst ${m}`);
    yakin(s.shSinir, r.shLoA, T, 0, `LoA SH ${m}`);
    yakinDerin(s.altSinirGa.yaklasik, r.altYaklasik, T, 1e-13, `alt LoA GA ${m}`);
    yakinDerin(s.ustSinirGa.yaklasik, r.ustYaklasik, T, 1e-13, `üst LoA GA ${m}`);
    // Kesin GA: R'nin qt(ncp) (AS 243 tabanlı pnt, ~1e-12) ve bizim pnt'miz (~1e-10) arasındaki fark
    yakinDerin(s.altSinirGa.kesin, r.altKesin, 1e-9, 1e-10, `alt LoA kesin GA ${m}`);
    yakinDerin(s.ustSinirGa.kesin, r.ustKesin, 1e-9, 1e-10, `üst LoA kesin GA ${m}`);
    const o = s.oransalYanlilik;
    yakin(o.kesisim, r.kesisim, 1e-9, 1e-12, `kesişim ${m}`);
    yakin(o.egim, r.egim, 1e-9, 1e-14, `eğim ${m}`);
    yakin(o.shKesisim, r.shKesisim, 1e-9, 0, `SH kesişim ${m}`);
    yakin(o.shEgim, r.shEgim, 1e-9, 0, `SH eğim ${m}`);
    yakin(o.t, r.egimT, 1e-9, 1e-12, `eğim t ${m}`);
    yakin(o.p, r.egimP, 1e-9, 1e-15, `eğim p ${m}`);
    yakin(o.egimGaAlt, r.egimGa[0], 1e-9, 1e-14, `eğim GA ${m}`);
    yakin(o.egimGaUst, r.egimGa[1], 1e-9, 1e-14, `eğim GA ${m}`);
    yakin(o.r2, r.r2, 1e-9, 1e-14, `R² ${m}`);
    yakin(s.disaridaYuzde, r.disarida, T, 0, `% dışarıda ${m}`);
    assert.equal(s.noktalar.length, r.n);
  }
  // blandr: yanlılık ve sınırlar birebir; sınır GA'sında blandr SH için 1.96 yerine qnorm(0.975) kullanır (~1e-6 fark)
  const s = U.blandAltman(R.bland.ana.x, R.bland.ana.y);
  const b = R.bland.blandr;
  yakin(s.bias, b.bias, T);
  yakin(s.ustSinir, b.upperLOA, T);
  yakin(s.altSinir, b.lowerLOA, T);
  yakin(s.biasGa.alt, b.biasLowerCI, T);
  yakin(s.biasGa.ust, b.biasUpperCI, T);
  yakinDerin(s.ustSinirGa.yaklasik, [b.upperLOA_lowerCI, b.upperLOA_upperCI], 1e-5);
  yakinDerin(s.altSinirGa.yaklasik, [b.lowerLOA_lowerCI, b.lowerLOA_upperCI], 1e-5);
  assert.ok(s.uyarilar.some((u) => u.includes('Eksik')));
  assert.throws(() => U.blandAltman([1, 2, 3], [0, 1, 2]), /farklar aynı/);
  assert.throws(() => U.blandAltman([1, 2], [1, 3]), IstatistikHatasi);
});

// ---------- Lin CCC ----------

test('Lin uyum korelasyon katsayısı (epiR::epi.ccc)', () => {
  for (const [ad, r] of Object.entries(R.ccc)) {
    const s = U.linCCC(r.x, r.y, { duzey: r.duzey });
    yakin(s.ccc, r.ccc, T, 0, `CCC ${ad}`);
    yakin(s.ga.alt, r.alt, T, 0, `GA alt ${ad}`);
    yakin(s.ga.ust, r.ust, T, 0, `GA üst ${ad}`);
    yakin(s.gaAsimptotik.alt, r.altA, T, 0, `GA asim. alt ${ad}`);
    yakin(s.gaAsimptotik.ust, r.ustA, T, 0, `GA asim. üst ${ad}`);
    yakin(s.olcekKaymasi, r.olcek, T, 0, `ölçek ${ad}`);
    yakin(s.konumKaymasi, r.konum, T, 1e-15, `konum ${ad}`);
    yakin(s.dogruluk, r.Cb, T, 0, `Cb ${ad}`);
    yakin(s.kesinlik, r.r, T, 0, `r ${ad}`);
  }
  assert.throws(() => U.linCCC([1, 1, 1], [1, 2, 3]), IstatistikHatasi);
});

// ---------- Yüzde uyum ----------

test('yüzde uyum (irr::agree)', () => {
  yakin(U.yuzdeUyum(R.fleiss.tani.matris).yuzde, R.yuzde.tani, T);
  yakin(U.yuzdeUyum(R.anksiyete).yuzde, R.yuzde.anksiyete0, 0, 1e-14);
  yakin(U.yuzdeUyum(R.anksiyete, { tolerans: 1 }).yuzde, R.yuzde.anksiyete1, T);
  const r1 = R.anksiyete.map((s) => s[0]);
  const r2 = R.anksiyete.map((s) => s[1]);
  yakin(U.yuzdeUyum(r1, r2).yuzde, R.yuzde.iki, T);
  yakin(U.cohenKappa(r1, r2).yuzdeUyum, R.yuzde.iki, T);
});

// ---------- Cronbach alfa ----------

test('Cronbach alfa ve madde istatistikleri (psych::alpha; SPSS RELIABILITY tanımları)', () => {
  for (const [ad, r] of Object.entries(R.alfa)) {
    const duzey = ad === 'likert' ? 0.9 : 0.95;
    const s = G.cronbachAlfa(r.matris, { duzey });
    const m = ad;
    assert.equal(s.n, r.n);
    assert.equal(s.maddeSayisi, r.k);
    yakin(s.alfa, r.alfa, T, 0, `alfa ${m}`);
    yakin(s.standartAlfa, r.std, T, 0, `std alfa ${m}`);
    yakin(s.ortalamaMaddeArasiKorelasyon, r.rbar, T, 0, `r̄ ${m}`);
    yakin(s.ga.alt, r.feldtAlt, T, 0, `Feldt alt ${m}`);
    yakin(s.ga.ust, r.feldtUst, T, 0, `Feldt üst ${m}`);
    if (duzey === 0.95) {
      yakin(s.ga.alt, r.psychFeldt.raw_alpha, T, 0, `psych Feldt ${m}`);
      yakin(s.ga.ust, r.psychFeldt['raw_alpha.1'], T, 0, `psych Feldt ${m}`);
    }
    s.maddeler.forEach((md, i) => {
      yakin(md.ortalama, r.ortalama[i], T, 0, `ortalama ${m}[${i}]`);
      yakin(md.ss, r.ss[i], T, 0, `ss ${m}[${i}]`);
      yakin(md.duzeltilmisMaddeToplamKorelasyonu, r.rDrop[i], T, 0, `r.drop ${m}[${i}]`);
      yakin(md.duzeltilmisMaddeToplamKorelasyonu, r.rDropPsych[i], T, 0, `r.drop psych ${m}[${i}]`);
      yakin(md.karesiAlinmisCokluKorelasyon, r.smc[i], 1e-9, 0, `SMC ${m}[${i}]`);
      yakin(md.olcekOrtalamasiSilinirse, r.olcekOrt[i], T, 0, `ölçek ort. ${m}[${i}]`);
      yakin(md.olcekVaryansiSilinirse, r.olcekVar[i], T, 0, `ölçek var. ${m}[${i}]`);
      if (r.k > 2) {
        yakin(md.alfaSilinirse, r.alfaSil[i], T, 0, `alfa silinirse ${m}[${i}]`);
        yakin(md.alfaSilinirse, r.alfaSilPsych[i], T, 0, `alfa silinirse psych ${m}[${i}]`);
      } else assert.equal(md.alfaSilinirse, null);
    });
    yakin(s.olcek.varyans, r.olcekVaryans, T, 0, `ölçek varyansı ${m}`);
    const oz = { maddeOrtalamalari: 'ortalamalar', maddeVaryanslari: 'varyanslar', maddeArasiKovaryanslar: 'kovaryanslar', maddeArasiKorelasyonlar: 'korelasyonlar' };
    for (const [a, b] of Object.entries(oz)) {
      const x = s.ozet[a];
      const y = r.ozet[b];
      yakin(x.ortalama, y.ortalama, T, 1e-15, `${a} ort ${m}`);
      yakin(x.minimum, y.minimum, T, 0, `${a} min ${m}`);
      yakin(x.maksimum, y.maksimum, T, 0, `${a} maks ${m}`);
      yakin(x.aralik, y.aralik, T, 0, `${a} aralık ${m}`);
      yakin(x.maksMinOrani, y.oran, T, 0, `${a} oran ${m}`);
      if (y.varyans !== null) yakin(x.varyans, y.varyans, 1e-9, 1e-15, `${a} varyans ${m}`);
    }
  }
  // bfi A1 ters kodlu: uyarı; eksik veri uyarısı
  const bfi = G.cronbachAlfa(R.alfa.bfi.matris);
  assert.ok(bfi.uyarilar.some((u) => u.includes('ters kodlanmış')));
  assert.ok(bfi.uyarilar.some((u) => u.includes('liste bazında')));
  // Sabit madde çıkarılır
  const sabitli = R.alfa.ucMadde.matris.map((s) => [...s, 3]);
  const s = G.cronbachAlfa(sabitli);
  yakin(s.alfa, R.alfa.ucMadde.alfa, T);
  assert.ok(s.uyarilar.some((u) => u.includes('Varyansı sıfır')));
  assert.throws(() => G.cronbachAlfa([[1], [2]]), IstatistikHatasi);
  assert.throws(() => G.cronbachAlfa([[1, 2, 3]]), IstatistikHatasi);
});

test('KR-20 ve KR-21', () => {
  const r = R.kr;
  const s20 = G.kr20(r.matris);
  yakin(s20.kr20, r.kr20, T, 0, 'KR-20');
  yakin(s20.kr20, r.alfa, T, 0, 'KR-20 = alfa (psych)');
  yakin(s20.kr20, G.cronbachAlfa(r.matris).alfa, 1e-13, 0, 'KR-20 = alfa');
  yakinDerin(s20.maddeGuclukleri, r.p, T);
  yakin(s20.toplamVaryans, r.v, T);
  const s21 = G.kr21(r.matris);
  yakin(s21.kr21, r.kr21, T, 0, 'KR-21');
  yakin(s21.ortalamaPuan, r.M, T);
  assert.throws(() => G.kr20([[0, 1], [1, 2]]), /0\/1/);
  // Tüm madde güçlükleri eşitse KR-21 = KR-20
  const esit = [[1, 0, 1, 0], [0, 1, 0, 1], [1, 1, 1, 1], [0, 0, 0, 0], [1, 1, 0, 0], [0, 0, 1, 1]];
  yakin(G.kr21(esit).kr21, G.kr20(esit).kr20, 1e-13);
});

test('yarıya bölme güvenirliği (SPSS MODEL=SPLIT tanımları, elle R)', () => {
  for (const [ad, r] of Object.entries(R.yari)) {
    const s = G.splitHalf(r.matris, ad === 'ozel' ? { ikinciParcaMaddeSayisi: 2 } : {});
    const m = ad;
    assert.equal(s.parca1.maddeSayisi, r.k1);
    assert.equal(s.parca2.maddeSayisi, r.k2);
    yakin(s.parcalarArasiKorelasyon, r.r, T, 0, `r ${m}`);
    yakin(s.spearmanBrownEsit, r.sbEsit, T, 0, `SB eşit ${m}`);
    yakin(s.spearmanBrownEsitOlmayan, r.sbEsitOlmayan, T, 0, `SB eşit olmayan ${m}`);
    yakin(s.guttman, r.guttman, T, 0, `Guttman ${m}`);
    yakin(s.parca1.alfa, r.alfa1, T, 0, `alfa1 ${m}`);
    yakin(s.parca2.alfa, r.alfa2, T, 0, `alfa2 ${m}`);
    yakin(s.parca1.ortalama, r.m1, T, 0, `m1 ${m}`);
    yakin(s.parca2.varyans, r.v2, T, 0, `v2 ${m}`);
  }
  assert.throws(() => G.splitHalf(R.yari.likert.matris, { ikinciParcaMaddeSayisi: 6 }), IstatistikHatasi);
});

test('McDonald omega (tek faktör ML; lavaan / MBESS::ci.reliability)', () => {
  for (const [ad, r] of Object.entries(R.omega)) {
    const s = G.mcdonaldOmega(r.matris);
    // R'de factanal (factr = 1) + ψ = 1 − λ² sabit noktasına 1e-15'e kadar yineleme: kesin ML çözümü
    yakin(s.omega, r.omega, 1e-10, 0, `omega ${ad}`);
    yakin(s.omegaStandart, r.omegaStd, 1e-10, 0, `omega std ${ad}`);
    s.maddeler.forEach((md, i) => {
      yakin(md.standartYuk, r.yuk[i], 1e-9, 1e-12, `yük ${ad}[${i}]`);
      yakin(md.hataVaryansi / (md.yuk / md.standartYuk) ** 2, r.psi[i], 1e-9, 1e-12, `ψ ${ad}[${i}]`);
    });
    // lavaan (nlminb) ve MBESS'in optimizasyon toleransı ~1e-7 göreli
    yakin(s.omega, r.lavaan, 1e-6, 0, `lavaan ${ad}`);
    yakin(s.omega, r.mbess, 1e-6, 0, `MBESS ${ad}`);
  }
  assert.throws(() => G.mcdonaldOmega([[1, 2], [2, 3], [3, 5]]), IstatistikHatasi);
});
