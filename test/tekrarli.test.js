import { test } from 'node:test';
import assert from 'node:assert/strict';
import { tekrarliOlcumAnova, polinomKontrastlari, cokDegiskenliTestler } from '../src/cekirdek/tekrarli.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin, yakinDerin } from './yardimci.js';

const R = referans('tekrarli');
const TOL = 1e-10;
const TOL_MV = 1e-9; // Mauchly, epsilon, çok değişkenli testler

const anahtar = (s) => (s === '(Intercept)' || s === 'Kesişim' ? 'Kesişim' : s.split(/:| \* /).sort().join('|'));

function calistir(r, ek = {}) {
  const icAdlar = Object.keys(r.icler);
  const temel = { veri: r.veri, gruplar: r.gruplar ?? undefined, grupAdi: r.grupAdi, grupSirasi: r.grupSirasi ?? undefined, ...ek };
  if (icAdlar.length === 1) return tekrarliOlcumAnova({ ...temel, faktorAdi: icAdlar[0], duzeyAdlari: r.icler[icAdlar[0]] });
  return tekrarliOlcumAnova({ ...temel, icFaktorler: icAdlar.map((ad) => ({ ad, duzeyler: r.icler[ad] })) });
}

const VAKALAR = ['tek', 'ob1', 'ob2', 'ob2tek', 'iki', 'karma'];

function icSatir(s, kaynak, duzeltme = 'kuresellik') {
  const x = s.icEtkilerTablosu.find((v) => v.tur === 'etki' && v.duzeltme === duzeltme && anahtar(v.kaynak) === anahtar(kaynak));
  assert.ok(x, `iç etki satırı yok: ${kaynak}`);
  return x;
}

function icEtkiAdi(s, kaynak) {
  // R'deki "Tedavi:Faz" → bizim "Faz" etkisi (grup adı çıkarılır)
  const g = s.grup?.ad;
  return kaynak.split(':').filter((v) => v !== g).join(' * ');
}

function emmKarsilastir(benim, satirlar, adlar, mesaj) {
  assert.ok(benim, `${mesaj}: EMM yok`);
  assert.equal(benim.length, satirlar.length, `${mesaj}: hücre sayısı`);
  for (const r of satirlar) {
    const b = benim.find((x) => adlar.every((a) => x.duzeyler[a] === r[a]));
    assert.ok(b, `${mesaj}: hücre yok`);
    const m = `${mesaj} ${adlar.map((a) => r[a]).join(',')}`;
    yakin(b.ortalama, r.emmean, TOL, 1e-12, `${m} ortalama`);
    yakin(b.SH, r.SE, TOL, 1e-14, `${m} SH`);
    assert.equal(b.sd, r.df);
    yakin(b.gaAlt, r['lower.CL'], TOL, 1e-12, `${m} gaAlt`);
    yakin(b.gaUst, r['upper.CL'], TOL, 1e-12, `${m} gaUst`);
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

function mvKarsilastir(benim, rTestler, mesaj) {
  for (const rt of rTestler) {
    const b = benim.find((x) => x.test === rt.test);
    assert.ok(b, `${mesaj} ${rt.test} yok`);
    const m = `${mesaj} ${rt.test}`;
    yakin(b.deger, rt.deger, TOL_MV, 1e-13, `${m} değer`);
    yakin(b.F, rt.F, TOL_MV, 1e-12, `${m} F`);
    yakin(b.sd1, rt.sd1, 1e-12, 0, `${m} sd1`);
    yakin(b.sd2, rt.sd2, 1e-12, 0, `${m} sd2`);
    yakin(b.p, rt.p, TOL_MV, 1e-300, `${m} p`);
  }
}

for (const ad of VAKALAR) {
  const r = R[ad];

  test(`Tekrarlı ${ad}: denek içi ve gruplar arası tek değişkenli testler (car::Anova type 3)`, () => {
    const s = calistir(r);
    assert.equal(s.n, r.N);
    for (const u of r.uni) {
      const ara = s.gruplarArasiTablo.find((x) => anahtar(x.kaynak) === anahtar(u.kaynak));
      if (ara) {
        // gruplar arası terim
        yakin(ara.KT, u.KT, TOL, 1e-12, `${ad} ${u.kaynak} KT`);
        assert.equal(ara.sd, u.sd);
        yakin(ara.F, u.F, TOL, 1e-12, `${ad} ${u.kaynak} F`);
        yakin(ara.p, u.p, TOL, 1e-300, `${ad} ${u.kaynak} p`);
        const h = s.gruplarArasiTablo.find((x) => x.tur === 'hata');
        yakin(h.KT, u.hataKT, TOL, 0, `${ad} gruplar arası hata KT`);
        assert.equal(h.sd, u.hataSd);
        yakin(ara.kismiEtaKare, u.KT / (u.KT + u.hataKT), 1e-12, 1e-15, 'kısmi eta');
        continue;
      }
      const x = icSatir(s, u.kaynak);
      yakin(x.KT, u.KT, TOL, 1e-12, `${ad} ${u.kaynak} KT`);
      assert.equal(x.sd, u.sd);
      yakin(x.F, u.F, TOL, 1e-12, `${ad} ${u.kaynak} F`);
      yakin(x.p, u.p, TOL, 1e-300, `${ad} ${u.kaynak} p`);
      yakin(x.kismiEtaKare, u.KT / (u.KT + u.hataKT), 1e-12, 1e-15, 'kısmi eta');
      const h = s.icEtkilerTablosu.find((v) => v.tur === 'hata' && v.duzeltme === 'kuresellik' && v.etki === x.etki);
      yakin(h.KT, u.hataKT, TOL, 0, `${ad} ${u.kaynak} hata KT`);
      assert.equal(h.sd, u.hataSd);
    }
  });

  test(`Tekrarlı ${ad}: Mauchly, epsilonlar (GG, HF SPSS/Lecoutre, alt sınır) ve düzeltilmiş p`, () => {
    const s = calistir(r);
    const sL = calistir(r, { hfFormulu: 'lecoutre' });
    for (const [terim, sp] of Object.entries(r.spss)) {
      const etki = icEtkiAdi(s, terim);
      const k = s.kuresellik.find((x) => anahtar(x.etki) === anahtar(etki));
      assert.ok(k, `küresellik yok: ${etki}`);
      const m = `${ad} ${terim}`;
      yakin(k.epsilon.greenhouseGeisser, sp.gg, TOL_MV, 0, `${m} GG`);
      yakin(k.epsilon.huynhFeldtSpss, sp.hfSpss, TOL_MV, 0, `${m} HF SPSS`);
      yakin(k.epsilon.huynhFeldtLecoutre, sp.hfLecoutre, TOL_MV, 0, `${m} HF Lecoutre`);
      yakin(k.epsilon.huynhFeldt, Math.min(1, sp.hfSpss), TOL_MV, 0, `${m} HF (SPSS, ≤ 1)`);
      const hs = s.icEtkilerTablosu.find((v) => v.tur === 'hata' && v.duzeltme === 'kuresellik' && v.etki === k.etki);
      yakin(k.epsilon.altSinir, (r.N - r.G) / hs.sd, 1e-14, 0, `${m} alt sınır = 1/p`);
      if (sp.p === null) {
        assert.equal(k.W, 1);
        assert.equal(k.sd, 0);
        assert.equal(k.p, null);
      } else {
        yakin(k.W, sp.W, TOL_MV, 1e-15, `${m} W`);
        yakin(k.kiKare, sp.kiKare, TOL_MV, 1e-13, `${m} χ²`);
        assert.equal(k.sd, sp.sd);
        yakin(k.p, sp.p, TOL_MV, 1e-300, `${m} Mauchly p (SPSS)`);
      }
      // SPSS HF düzeltmeli p ve alt sınır p'si
      yakin(icSatir(s, terim, 'huynh-feldt').p, sp.pHfSpss, TOL_MV, 1e-300, `${m} p HF SPSS`);
      yakin(icSatir(s, terim, 'alt-sinir').p, sp.pAlt, TOL_MV, 1e-300, `${m} p alt sınır`);
    }
    for (const kr of r.kure ?? []) {
      const etki = icEtkiAdi(s, kr.kaynak);
      const k = s.kuresellik.find((x) => anahtar(x.etki) === anahtar(etki));
      yakin(k.W, kr.W, TOL_MV, 1e-15, `${ad} ${kr.kaynak} W (car)`);
      yakin(k.pR, kr.pR, TOL_MV, 1e-300, `${ad} ${kr.kaynak} Mauchly p (car, w2 düzeltmeli)`);
    }
    if (r.mauchly) {
      yakin(s.kuresellik[0].W, r.mauchly.W, TOL_MV, 0, 'mauchly.test W');
      yakin(s.kuresellik[0].pR, r.mauchly.p, TOL_MV, 0, 'mauchly.test p');
    }
    for (const d of r.duz ?? []) {
      if (d.gg === null) continue; // tek kontrastlı etki
      const x = icSatir(s, d.kaynak, 'greenhouse-geisser');
      yakin(x.epsilon, d.gg, TOL_MV, 0, `${ad} ${d.kaynak} GG (car)`);
      yakin(x.p, d.pgg, TOL_MV, 1e-300, `${ad} ${d.kaynak} p GG (car)`);
      const etki = icEtkiAdi(s, d.kaynak);
      const k = sL.kuresellik.find((v) => anahtar(v.etki) === anahtar(etki));
      yakin(k.epsilon.huynhFeldtLecoutre, d.hf, TOL_MV, 0, `${ad} ${d.kaynak} HF (car)`);
      yakin(icSatir(sL, d.kaynak, 'huynh-feldt').p, d.phf, TOL_MV, 1e-300, `${ad} ${d.kaynak} p HF Lecoutre (car)`);
    }
  });

  test(`Tekrarlı ${ad}: çok değişkenli testler ve iç kontrastlar`, () => {
    const s = calistir(r);
    for (const [terim, testler] of Object.entries(r.mv)) {
      if (s.gruplarArasiTablo.some((x) => anahtar(x.kaynak) === anahtar(terim))) continue; // gruplar arası terim
      const benim = s.cokDegiskenliTestler.filter((x) => anahtar(x.kaynak) === anahtar(terim));
      assert.equal(benim.length, 4, `${ad} ${terim} test sayısı`);
      mvKarsilastir(benim, testler, `${ad} ${terim}`);
    }
    for (const [etkiR, liste] of Object.entries(r.kontrast)) {
      const etki = etkiR.split(':').join(' * ');
      const satirlar = s.icKontrastlar.filter((x) => x.etki === etki && x.kaynak === etki);
      const hatalar = s.icKontrastlar.filter((x) => x.etki === etki && x.kaynak === `Hata(${etki})`);
      const grupSat = s.grup ? s.icKontrastlar.filter((x) => x.etki === etki && x.kaynak === `${etki} * ${s.grup.ad}`) : [];
      assert.equal(satirlar.length, liste.length);
      liste.forEach((k, ci) => {
        const m = `${ad} kontrast ${etki} ${satirlar[ci].kontrast}`;
        yakin(satirlar[ci].KT, k.KT, TOL, 1e-12, `${m} KT`);
        yakin(satirlar[ci].F, k.F, TOL, 0, `${m} F`);
        yakin(satirlar[ci].p, k.p, TOL, 1e-300, `${m} p`);
        yakin(hatalar[ci].KT, k.hataKT, TOL, 0, `${m} hata KT`);
        if (s.grup) {
          yakin(grupSat[ci].KT, k.grupKT, TOL, 1e-12, `${m} grup KT`);
          yakin(grupSat[ci].F, k.grupF, TOL, 0, `${m} grup F`);
          yakin(grupSat[ci].p, k.grupP, TOL, 1e-300, `${m} grup p`);
        }
      });
    }
  });

  test(`Tekrarlı ${ad}: EMM, ikili karşılaştırmalar, basit etkiler, Levene, betimsel (emmeans, linearHypothesis)`, () => {
    const s = calistir(r);
    for (const [terim, satirlar] of Object.entries(r.emm)) {
      const adlar = terim.split(':');
      const k = Object.keys(s.marjinalOrtalamalar).find((x) => anahtar(x) === anahtar(terim));
      emmKarsilastir(s.marjinalOrtalamalar[k], satirlar, adlar, `${ad} EMM ${terim}`);
    }
    for (const [f, tablo] of Object.entries(r.ikili)) ikiliKarsilastir(s.ikiliKarsilastirmalar[f], tablo, `${ad} ikili ${f}`);
    if (r.basit) {
      for (const b of r.basit.grup) {
        const x = s.basitEtkiler.grupHerDuzeyde.find((v) => v.kosul[b.faktor] === b.duzey);
        const m = `${ad} grup | ${b.faktor}=${b.duzey}`;
        yakin(x.KT, b.KT, TOL, 1e-12, `${m} KT`);
        yakin(x.F, b.F, TOL, 0, `${m} F`);
        yakin(x.p, b.p, TOL, 1e-300, `${m} p`);
        yakin(x.p, b.jointP, TOL, 1e-300, `${m} p (joint_tests)`);
        yakin(x.kismiEtaKare, b.KT / (b.KT + b.hataKT), 1e-12, 0, `${m} kısmi eta`);
        ikiliKarsilastir(x.ikiliKarsilastirmalar, b.ikili, `${m} ikili`);
      }
      for (const b of r.basit.ic) {
        const x = s.basitEtkiler.icFaktorHerGrupta.find((v) => v.etken === b.faktor && v.kosul[s.grup.ad] === b.grup);
        const m = `${ad} ${b.faktor} | grup=${b.grup}`;
        mvKarsilastir(x.cokDegiskenli, b.testler, m);
        ikiliKarsilastir(x.ikiliKarsilastirmalar, b.ikili, `${m} ikili`);
      }
      r.levene.forEach((lv, j) => {
        yakin(s.levene[j].istatistik, lv.F, TOL, 0, `${ad} Levene ${j}`);
        assert.deepEqual(s.levene[j].sd, [lv.sd1, lv.sd2]);
        yakin(s.levene[j].p, lv.p, TOL, 1e-300, `${ad} Levene p ${j}`);
      });
    } else {
      assert.equal(s.basitEtkiler, null);
      assert.equal(s.levene, null);
    }
    // betimsel: R sırası ölçüm başına (gruplar, sonra toplam); bizimki aynı
    assert.equal(s.betimsel.length, r.betimsel.length);
    r.betimsel.forEach((b, i) => {
      const x = s.betimsel[i];
      assert.equal(x.grup, b.grup);
      assert.equal(x.n, b.n);
      yakin(x.ortalama, b.ortalama, 1e-13, 1e-13, 'betimsel ortalama');
      yakin(x.standartSapma, b.ss, 1e-12, 1e-13, 'betimsel ss');
    });
  });
}

test('Polinom kontrastları R contr.poly ile aynı (işaret dahil)', () => {
  yakinDerin(polinomKontrastlari(3), R.poli.k3, 1e-13, 1e-15, 'k=3');
  yakinDerin(polinomKontrastlari(4), R.poli.k4, 1e-13, 1e-15, 'k=4');
  yakinDerin(polinomKontrastlari(5), R.poli.k5, 1e-13, 1e-15, 'k=5');
  yakinDerin(polinomKontrastlari(7), R.poli.k7, 1e-12, 1e-14, 'k=7');
  yakinDerin(polinomKontrastlari(5, [1, 2, 4, 8, 16]), R.poli.k5s, 1e-12, 1e-14, 'k=5, eşit aralıksız');
  yakinDerin(polinomKontrastlari(2), [[-Math.SQRT1_2], [Math.SQRT1_2]], 1e-15, 1e-15, 'k=2');
});

test('Tekrarlı: liste bazında silme, HF formüllerinin tek grupta eşitliği, iki düzeyli etki', () => {
  const t = calistir(R.tek);
  assert.equal(t.cikarilanGozlem, 2);
  assert.equal(t.n, 16);
  const e = t.kuresellik[0].epsilon;
  yakin(e.huynhFeldtSpss, e.huynhFeldtLecoutre, 1e-14, 0, 'g = 1 iken iki HF formülü aynı');
  assert.ok(t.uyarilar.some((u) => /küresellik varsayımı sağlanmıyor/.test(u)));
  assert.equal(t.kuresellik[0].onerilenDuzeltme, 'greenhouse-geisser');
  const k = calistir(R.karma);
  assert.equal(k.cikarilanGozlem, 2);
  assert.deepEqual(k.grup.n, [8, 9, 5]); // dozA, dozB, plasebo (eksik ölçüm: plasebo; eksik etiket: dozB)
  assert.deepEqual(k.grup.duzeyler, R.karma.grupSirasi);
  const iki = calistir(R.iki);
  const ku = iki.kuresellik[0];
  assert.equal(ku.W, 1);
  assert.equal(ku.kiKare, 0);
  assert.equal(ku.sd, 0);
  assert.equal(ku.p, null);
  assert.equal(ku.epsilon.greenhouseGeisser, 1);
  assert.equal(ku.epsilon.huynhFeldt, 1);
  assert.equal(ku.epsilon.altSinir, 1);
});

test('Tekrarlı: grupsuz desende ikili karşılaştırma = Bonferroni düzeltmeli eşleştirilmiş t testi', () => {
  const s = calistir(R.tek);
  const Y = R.tek.veri.filter((r) => r.every((v) => v !== null));
  const n = Y.length;
  const d = Y.map((r) => r[0] - r[2]);
  const ort = d.reduce((a, b) => a + b, 0) / n;
  const ss = Math.sqrt(d.reduce((a, b) => a + (b - ort) ** 2, 0) / (n - 1));
  const c = s.ikiliKarsilastirmalar.Zaman.find((x) => x.duzey1 === 'T1' && x.duzey2 === 'T3');
  yakin(c.fark, ort, 1e-12, 0, 'fark');
  yakin(c.t, ort / (ss / Math.sqrt(n)), 1e-12, 0, 't');
  yakin(c.p, Math.min(1, 6 * c.pHam), 1e-14, 0, 'Bonferroni p');
  assert.equal(c.sd, n - 1);
});

test('Tekrarlı: hata sd kontrast sayısından küçükse Mauchly ve çok değişkenli testler hesaplanmaz', () => {
  const veri = [
    [1, 2, 4, 3, 5],
    [2, 2, 3, 5, 6],
    [3, 5, 4, 4, 8],
  ];
  const s = tekrarliOlcumAnova({ veri });
  assert.equal(s.kuresellik[0].W, null);
  assert.equal(s.kuresellik[0].p, null);
  assert.equal(s.cokDegiskenliTestler.length, 0);
  assert.ok(s.uyarilar.some((u) => /çok değişkenli testler/.test(u)));
  assert.ok(Number.isFinite(icSatir(s, 'Zaman').F));
  assert.ok(s.kuresellik[0].epsilon.greenhouseGeisser > 0.25 && s.kuresellik[0].epsilon.greenhouseGeisser <= 1);
});

test('Tekrarlı: hata durumları', () => {
  const hata = (f, re) => assert.throws(f, (e) => e instanceof IstatistikHatasi && re.test(e.message));
  hata(() => tekrarliOlcumAnova({ veri: [[1], [2]] }), /en az iki ölçüm/);
  hata(() => tekrarliOlcumAnova({ veri: [[1, 2], [2, 3, 4]] }), /ölçüm sayısı aynı/);
  hata(() => tekrarliOlcumAnova({ veri: [[1, 2], [2, 3]], gruplar: ['a'] }), /Grup etiketlerinin sayısı/);
  hata(() => tekrarliOlcumAnova({ veri: [[1, 2], [2, 3], [3, 5]], gruplar: ['a', 'a', 'a'] }), /en az iki düzeyi/);
  hata(() => tekrarliOlcumAnova({ veri: [[1, 2], [2, 3]], gruplar: ['a', 'b'] }), /Hata serbestlik/);
  hata(() => tekrarliOlcumAnova({ veri: [[1, 2]] }), /en az iki denek/);
  hata(() => tekrarliOlcumAnova({ veri: [[1, 2, 3], [2, 3, 5], [1, 1, 1]], icFaktorler: [{ ad: 'A', duzeyler: ['x', 'y'] }] }), /çarpımı/);
  hata(() => tekrarliOlcumAnova({ veri: [[1, 2], [2, 3]], duzeyAdlari: ['a'] }), /Düzey adlarının sayısı/);
  hata(() => tekrarliOlcumAnova({ veri: [[1, 2], [2, 3]], hfFormulu: 'x' }), /hfFormulu/);
  hata(() => tekrarliOlcumAnova({ veri: [[1, 2], [2, 3]], duzeltme: 'holm' }), /düzeltme/);
});

test('cokDegiskenliTestler: tekil hata matrisinde null', () => {
  assert.equal(cokDegiskenliTestler([[1, 0], [0, 1]], [[1, 1], [1, 1]], 1, 5), null);
});

test('Tekrarlı: denek içi hata varyansı sıfırsa hata', () => {
  // her denek yalnızca sabit bir kaymayla farklı: Y = denek + zaman → hata SSCP'si sıfır
  const veri = [[1, 3, 6], [2, 4, 7], [5, 7, 10], [0, 2, 5]];
  assert.throws(() => tekrarliOlcumAnova({ veri }), (e) => e instanceof IstatistikHatasi && /hata varyansı sıfır/.test(e.message));
});
