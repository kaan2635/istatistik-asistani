import { test } from 'node:test';
import assert from 'node:assert/strict';
import { kaplanMeier, kaplanMeierGruplu, logRank, logRankTrend, ciftliLogRank, zamanDuzelt } from '../src/cekirdek/sagkalim.js';
import { coxRegresyon, coxPHVarsayimi } from '../src/cekirdek/cox.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin, yakinDerin } from './yardimci.js';

const R = referans('sagkalim');
const dizi = (x) => (Array.isArray(x) ? x : x === undefined ? [] : [x]);
// R'nin Inf değerleri JSON'da null olur
const sonsuzMu = (g, b) => b === null && typeof g === 'number' && !Number.isFinite(g) && !Number.isNaN(g);
const GT = { log: 'log', 'log-log': 'log-log', plain: 'plain' };

// R betiğinde ifadeyle kurulan veri (JSON 15 basamağa yuvarladığı için burada aynı ifadeyle kurulur)
function veriAl(ad) {
  if (ad === 'yuvarlama') {
    return { time: [0.1 + 0.2, 0.3, 0.5, 0.7, 0.7 + 1e-12, 1], status: [1, 1, 0, 1, 1, 0], grup: [1, 2, 1, 2, 1, 2] };
  }
  const v = R.km[ad].veri;
  return { time: dizi(v.time), status: dizi(v.status), grup: dizi(v.grup) };
}

function kmKarsilastir(r, b, etiket, tol = 1e-10) {
  const t = r.tablo;
  yakinDerin(t.map((s) => s.zaman), dizi(b.zaman), tol, 1e-14, `${etiket} zaman`);
  yakinDerin(t.map((s) => s.riskAltinda), dizi(b.riskAltinda), 0, 0, `${etiket} risk`);
  yakinDerin(t.map((s) => s.olay), dizi(b.olay), 0, 0, `${etiket} olay`);
  yakinDerin(t.map((s) => s.sansur), dizi(b.sansur), 0, 0, `${etiket} sansür`);
  yakinDerin(t.map((s) => s.sagkalim), dizi(b.sagkalim), tol, 1e-14, `${etiket} S`);
  if (b.sh !== undefined) yakinDerin(t.map((s) => s.sh), dizi(b.sh), tol, 1e-14, `${etiket} SH`);
  yakinDerin(t.map((s) => s.gaAlt), dizi(b.gaAlt), tol, 1e-14, `${etiket} GA alt`);
  yakinDerin(t.map((s) => s.gaUst), dizi(b.gaUst), tol, 1e-14, `${etiket} GA üst`);
  if (b.kumHaz !== undefined) yakinDerin(t.map((s) => s.kumulatifHazard), dizi(b.kumHaz), tol, 1e-14, `${etiket} H`);
  yakin(r.medyan.deger, b.medyan, tol, 1e-14, `${etiket} medyan`);
  yakin(r.medyan.gaAlt, b.medyanAlt, tol, 1e-14, `${etiket} medyan alt`);
  yakin(r.medyan.gaUst, b.medyanUst, tol, 1e-14, `${etiket} medyan üst`);
  const q = dizi(b.q);
  const qa = dizi(b.qAlt);
  const qu = dizi(b.qUst);
  r.yuzdelikler.forEach((y, i) => {
    yakin(y.deger, q[i], tol, 1e-14, `${etiket} q${y.p}`);
    yakin(y.gaAlt, qa[i], tol, 1e-14, `${etiket} q${y.p} alt`);
    yakin(y.gaUst, qu[i], tol, 1e-14, `${etiket} q${y.p} üst`);
  });
}

test('Kaplan-Meier: survfit tablosu, medyan, yüzdelikler, kısıtlı ortalama (log, log-log, plain, %90)', () => {
  for (const ad of Object.keys(R.km)) {
    const v = veriAl(ad);
    for (const tip of Object.keys(GT)) {
      const b = R.km[ad][tip];
      const r = kaplanMeier(v.time, v.status, { gaTipi: tip });
      kmKarsilastir(r, b, `${ad}/${tip}`);
      assert.equal(r.n, b.n, `${ad} n`);
      assert.equal(r.olaySayisi, b.olaySayisi, `${ad} olay sayısı`);
      yakin(r.kisitliOrtalama.deger, b.rmean, 1e-10, 1e-14, `${ad} rmean`);
      yakin(r.kisitliOrtalama.sh, b.rmeanSH, 1e-10, 1e-14, `${ad} rmean SH`);
    }
    const b90 = R.km[ad].duzey90;
    kmKarsilastir(kaplanMeier(v.time, v.status, { duzey: 0.9 }), b90, `${ad}/%90`);
  }
});

test('Kaplan-Meier: belirli zamanlarda sağkalım (summary(fit, times =)) ve sayısal RMST üst sınırı', () => {
  const tipler = { lung: 'log', baglar: 'log-log', sonOlay: 'log', sifirZaman: 'log' };
  for (const [ad, b] of Object.entries(R.zamanlarda)) {
    const v = veriAl(ad);
    const r = kaplanMeier(v.time, v.status, { gaTipi: tipler[ad], zamanlar: dizi(b.istenen) });
    const z = r.zamanlarda;
    yakinDerin(z.map((s) => s.zaman), dizi(b.zaman), 0, 0, `${ad} zaman`);
    yakinDerin(z.map((s) => s.riskAltinda), dizi(b.riskAltinda), 0, 0, `${ad} risk`);
    yakinDerin(z.map((s) => s.olay), dizi(b.olay), 0, 0, `${ad} olay`);
    yakinDerin(z.map((s) => s.sansur), dizi(b.sansur), 0, 0, `${ad} sansür`);
    yakinDerin(z.map((s) => s.sagkalim), dizi(b.sagkalim), 1e-10, 1e-14, `${ad} S`);
    yakinDerin(z.map((s) => s.sh), dizi(b.sh), 1e-10, 1e-14, `${ad} SH`);
    yakinDerin(z.map((s) => s.gaAlt), dizi(b.gaAlt), 1e-10, 1e-14, `${ad} alt`);
    yakinDerin(z.map((s) => s.gaUst), dizi(b.gaUst), 1e-10, 1e-14, `${ad} üst`);
    if (dizi(b.istenen).some((t) => t > Math.max(...v.time))) assert.ok(r.uyarilar.length > 0, `${ad} kapsam dışı uyarısı`);
  }
  const lung = veriAl('lung');
  const aml = veriAl('aml');
  const k = (v, u) => kaplanMeier(v.time, v.status, { rmstUstSinir: u }).kisitliOrtalama;
  for (const [ad, v, u] of [['lung365', lung, 365], ['aml100', aml, 100], ['aml200', aml, 200]]) {
    yakin(k(v, u).deger, R.rmeanSayisal[ad][0], 1e-10, 1e-14, `${ad} rmean`);
    yakin(k(v, u).sh, R.rmeanSayisal[ad][1], 1e-10, 1e-14, `${ad} rmean SH`);
  }
});

test('Kaplan-Meier: gruplara göre (ortak ve bireysel RMST üst sınırı)', () => {
  for (const [ad, gruplar] of Object.entries(R.gruplu)) {
    const v = veriAl(ad);
    const ortak = kaplanMeierGruplu(v.time, v.status, v.grup);
    const bireysel = kaplanMeierGruplu(v.time, v.status, v.grup, { rmstUstSinir: 'bireysel' });
    assert.equal(ortak.gruplar.length, gruplar.length, `${ad} grup sayısı`);
    gruplar.forEach((b, i) => {
      const r = ortak.gruplar[i];
      kmKarsilastir(r, b, `${ad} grup ${r.grup}`);
      yakin(r.kisitliOrtalama.deger, b.rmeanOrtak, 1e-10, 1e-14, `${ad} ${r.grup} rmean ortak`);
      yakin(r.kisitliOrtalama.sh, b.rmeanOrtakSH, 1e-10, 1e-14, `${ad} ${r.grup} rmean ortak SH`);
      yakin(bireysel.gruplar[i].kisitliOrtalama.deger, b.rmeanBireysel, 1e-10, 1e-14, `${ad} ${r.grup} rmean bireysel`);
      yakin(bireysel.gruplar[i].kisitliOrtalama.sh, b.rmeanBireyselSH, 1e-10, 1e-14, `${ad} ${r.grup} rmean bireysel SH`);
    });
  }
});

test('Kaplan-Meier: uç durumlar ve hata mesajları', () => {
  const r = kaplanMeier([3, 5, 7], [0, 0, 0]);
  assert.equal(r.medyan.deger, null);
  assert.ok(r.uyarilar.some((u) => u.includes('olay')));
  assert.throws(() => kaplanMeier([], []), IstatistikHatasi);
  assert.throws(() => kaplanMeier([1, 2], [1]), /uzunluk/);
  assert.throws(() => kaplanMeier([1, -2], [1, 0]), /negatif/);
  assert.throws(() => kaplanMeier([1, 2], [1, 2]), /1 \(olay/);
  assert.throws(() => kaplanMeier([1, 2], [1, 0], { gaTipi: 'logit' }), IstatistikHatasi);
  // aeqSurv
  assert.deepEqual(zamanDuzelt([0.1 + 0.2, 0.3, 1]), [0.3, 0.3, 1]);
  assert.deepEqual(zamanDuzelt([1, 2, 3]), [1, 2, 3]);
});

test('Log-rank ailesi: survdiff (rho = 0, 1, 0.5) ve elle hesaplanan Breslow / Tarone-Ware', () => {
  for (const [ad, b] of Object.entries(R.logrank)) {
    const v = ad === 'sifirOlayGrup' ? { time: b.veri.time, status: b.veri.status, grup: b.veri.grup } : veriAl(ad);
    const kars = [
      ['log-rank', b.survdiff0, {}],
      ['peto-peto', b.survdiff1, {}],
      ['fleming-harrington', b.survdiff05, { rho: 0.5 }],
      ['breslow', b.elle.breslow, {}],
      ['tarone-ware', b.elle.tw, {}],
    ];
    for (const [agirlik, ref, ek] of kars) {
      if (!ref) continue;
      const r = logRank(v.time, v.status, v.grup, { agirlik, ...ek });
      yakin(r.istatistik, ref.kikare, 1e-10, 1e-13, `${ad} ${agirlik} χ²`);
      yakin(r.p, ref.p, 1e-9, 1e-15, `${ad} ${agirlik} p`);
      yakinDerin(r.gruplar.map((g) => g.gozlenen), dizi(ref.gozlenen), 1e-10, 1e-12, `${ad} ${agirlik} O`);
      yakinDerin(r.gruplar.map((g) => g.beklenen), dizi(ref.beklenen), 1e-10, 1e-12, `${ad} ${agirlik} E`);
      if (ref.V) yakinDerin(r.varyans, ref.V, 1e-10, 1e-12, `${ad} ${agirlik} V`);
    }
    if (b.elle && b.elle.peto) {
      // elle yazılmış Peto-Peto (FH ρ = 1) survdiff(rho = 1) ile de aynı olmalı
      yakin(b.elle.peto.kikare, b.survdiff1.kikare, 1e-10, 1e-13, `${ad} peto R içi tutarlılık`);
    }
    if (b.trend) {
      const ag = { logrank: 'log-rank', breslow: 'breslow', tw: 'tarone-ware', peto: 'peto-peto' };
      for (const [k, ref] of Object.entries(b.trend)) {
        const r = logRankTrend(v.time, v.status, v.grup, { agirlik: ag[k] });
        yakin(r.U, ref.U, 1e-10, 1e-12, `${ad} trend ${k} U`);
        yakin(r.istatistik, ref.kikare, 1e-10, 1e-13, `${ad} trend ${k} χ²`);
        yakin(r.p, ref.p, 1e-9, 1e-15, `${ad} trend ${k} p`);
      }
    }
    if (b.ikili) {
      const l = ciftliLogRank(v.time, v.status, v.grup, { pDuzeltme: 'bonferroni' });
      const br = ciftliLogRank(v.time, v.status, v.grup, { agirlik: 'breslow' });
      b.ikili.forEach((ref, i) => {
        yakin(l.karsilastirmalar[i].istatistik, ref.logrank, 1e-10, 1e-13, `${ad} ikili ${i} log-rank`);
        yakin(br.karsilastirmalar[i].istatistik, ref.breslow, 1e-10, 1e-13, `${ad} ikili ${i} breslow`);
        const m = b.ikili.length;
        yakin(l.karsilastirmalar[i].pDuzeltilmis, Math.min(1, l.karsilastirmalar[i].p * m), 1e-14, 0, 'bonferroni');
      });
    }
  }
  assert.throws(() => logRank([1, 2, 3], [1, 1, 0], [1, 1, 1]), /en az iki grup/);
  assert.throws(() => logRank([1, 2, 3], [1, 1, 0], [1, 2, 1], { agirlik: 'gehan' }), IstatistikHatasi);
});

// ---------- Cox ----------

function coxKarsilastir(r, b, etiket, tol = 1e-8) {
  const adlar = dizi(b.adlar);
  const B = dizi(b.B);
  r.katsayilar.forEach((k, j) => {
    assert.equal(k.ad, adlar[j]);
    yakin(k.B, B[j], tol, 1e-12, `${etiket} B ${k.ad}`);
    yakin(k.SH, dizi(b.SH)[j], tol, 1e-12, `${etiket} SH ${k.ad}`);
    yakin(k.z, dizi(b.z)[j], tol, 1e-12, `${etiket} z ${k.ad}`);
    yakin(k.p, dizi(b.p)[j], 1e-7, 1e-15, `${etiket} p ${k.ad}`);
    if (!sonsuzMu(k.gaAlt, dizi(b.gaAlt)[j])) yakin(k.gaAlt, dizi(b.gaAlt)[j], tol, 1e-12, `${etiket} HR alt ${k.ad}`);
    if (!sonsuzMu(k.gaUst, dizi(b.gaUst)[j])) yakin(k.gaUst, dizi(b.gaUst)[j], tol, 1e-12, `${etiket} HR üst ${k.ad}`);
  });
  const V = Array.isArray(b.V) ? (Array.isArray(b.V[0]) ? b.V : [b.V]) : [[b.V]];
  yakinDerin(r.kovaryans, V, tol, 1e-14, `${etiket} V`);
  yakin(r.logOlabilirlik.baslangic, b.loglik[0], 1e-12, 1e-12, `${etiket} LL0`);
  yakin(r.logOlabilirlik.son, b.loglik[1], 1e-10, 1e-12, `${etiket} LL`);
  yakin(r.modelTestleri.skor.istatistik, b.skor, 1e-10, 1e-12, `${etiket} skor`);
  yakin(r.modelTestleri.wald.istatistik, b.wald, tol, 1e-12, `${etiket} Wald`);
  if (b.lr !== undefined) {
    yakin(r.modelTestleri.olabilirlikOrani.istatistik, b.lr, 1e-8, 1e-10, `${etiket} LR`);
    assert.equal(r.modelTestleri.olabilirlikOrani.sd, b.sd, `${etiket} sd`);
    yakin(r.modelTestleri.olabilirlikOrani.p, b.lrP, 1e-7, 1e-15, `${etiket} LR p`);
    yakin(r.modelTestleri.wald.p, b.waldP, 1e-7, 1e-15, `${etiket} Wald p`);
    yakin(r.modelTestleri.skor.p, b.skorP, 1e-9, 1e-15, `${etiket} skor p`);
  }
  assert.equal(r.iterasyon, b.iter, `${etiket} iterasyon`);
  const c = b.concordance;
  assert.equal(r.uyum.uyumlu, c[0], `${etiket} uyumlu`);
  assert.equal(r.uyum.uyumsuz, c[1], `${etiket} uyumsuz`);
  assert.equal(r.uyum.bagliX, c[2], `${etiket} bağlı x`);
  assert.equal(r.uyum.bagliY, c[3], `${etiket} bağlı y`);
  assert.equal(r.uyum.bagliXY, c[4], `${etiket} bağlı xy`);
  yakin(r.uyum.deger, c[5], 1e-12, 1e-14, `${etiket} C`);
  yakin(r.uyum.sh, c[6], 1e-8, 1e-14, `${etiket} C SH`);
}

function zphKarsilastir(z, b, etiket, tol = 1e-7) {
  const tablo = b.tablo;
  const satirlar = [...z.testler, z.genel];
  assert.equal(satirlar.length, tablo.length, `${etiket} satır sayısı`);
  satirlar.forEach((s, i) => {
    yakin(s.istatistik, tablo[i][0], tol, 1e-10, `${etiket} χ² ${s.ad}`);
    assert.equal(s.sd, tablo[i][1], `${etiket} sd ${s.ad}`);
    yakin(s.p, tablo[i][2], tol, 1e-12, `${etiket} p ${s.ad}`);
  });
  if (b.x) {
    yakinDerin(z.artiklar.gZaman, dizi(b.x), 1e-12, 1e-14, `${etiket} g(t)`);
    yakinDerin(z.artiklar.zaman, dizi(b.zaman), 0, 0, `${etiket} zaman`);
  }
  const y = b.y.map((s) => dizi(s));
  yakinDerin(z.artiklar.olcekli, y, tol, 1e-9, `${etiket} ölçekli artıklar`);
}

const TERIMLER = { veteran: [[0], [1], [2], [3], [4], [5, 6, 7]], colon: [[0, 1], [2], [3], [4]] };

test('Cox regresyonu: coxph (Efron ve Breslow), model testleri, concordance', () => {
  for (const baglar of ['efron', 'breslow']) {
    for (const [ad, b] of Object.entries(R.cox[baglar])) {
      const X = b.X.map((s) => dizi(s));
      const r = coxRegresyon({ sure: dizi(b.sure), olay: dizi(b.olay), X, adlar: dizi(b.adlar), baglar });
      const etiket = `${ad}/${baglar}`;
      if (ad === 'dogrusal') {
        assert.equal(r.katsayilar[2].B, null, 'doğrusal bağımlı sütun');
        assert.ok(r.uyarilar.some((u) => u.includes('bağımlılık')));
        const b2 = { ...b, adlar: dizi(b.adlar), B: dizi(b.B).slice(0, 2), SH: dizi(b.SH).slice(0, 2) };
        r.katsayilar.slice(0, 2).forEach((k, j) => {
          yakin(k.B, b2.B[j], 1e-8, 1e-12, `${etiket} B`);
          yakin(k.SH, b2.SH[j], 1e-8, 1e-12, `${etiket} SH`);
        });
        yakin(r.logOlabilirlik.son, b.loglik[1], 1e-10, 1e-12, `${etiket} LL`);
        yakin(r.modelTestleri.skor.istatistik, b.skor, 1e-10, 1e-12, `${etiket} skor`);
        yakin(r.modelTestleri.wald.istatistik, b.wald, 1e-8, 1e-12, `${etiket} Wald`);
        assert.equal(r.modelTestleri.wald.sd, 2);
        continue;
      }
      // ayrisma: β sonsuza gidiyor (yakınsamayan model); exp(β·x) çok büyük olduğundan yuvarlama farkı büyür
      coxKarsilastir(r, b, etiket, ad === "ayrisma" ? 1e-6 : 1e-8);
      if (b.uyarilar && dizi(b.uyarilar).length) assert.ok(r.uyarilar.length > 0, `${etiket} uyarı bekleniyordu: ${b.uyarilar}`);
      else assert.equal(r.uyarilar.length, 0, `${etiket} beklenmeyen uyarı: ${r.uyarilar}`);
      if (b.zph) {
        const z = coxPHVarsayimi(r, TERIMLER[ad] ? { terimler: TERIMLER[ad] } : {});
        zphKarsilastir(z, b.zph, `${etiket} zph`);
        if (b.zphSutun) zphKarsilastir(coxPHVarsayimi(r), b.zphSutun, `${etiket} zph sütun`);
      }
    }
  }
});

test('Cox regresyonu: tabakalı model ve tabakalı cox.zph', () => {
  const b = R.tabakali;
  const r = coxRegresyon({ sure: b.sure, olay: b.olay, X: b.X, adlar: ['age', 'ph.ecog'], tabaka: b.tabaka });
  yakinDerin(r.katsayilar.map((k) => k.B), b.B, 1e-8, 1e-12, 'B');
  yakinDerin(r.kovaryans, b.V, 1e-8, 1e-14, 'V');
  yakinDerin([r.logOlabilirlik.baslangic, r.logOlabilirlik.son], b.loglik, 1e-11, 1e-12, 'LL');
  yakin(r.modelTestleri.skor.istatistik, b.skor, 1e-10, 1e-12, 'skor');
  yakin(r.modelTestleri.wald.istatistik, b.wald, 1e-8, 1e-12, 'wald');
  assert.equal(r.iterasyon, b.iter);
  yakin(r.uyum.deger, b.concordance[5], 1e-12, 1e-14, 'C');
  yakin(r.uyum.sh, b.concordance[6], 1e-8, 1e-14, 'C SH');
  zphKarsilastir(coxPHVarsayimi(r), b.zph, 'tabakalı zph');
});

test('Cox regresyonu: girdi hataları', () => {
  assert.throws(() => coxRegresyon({ sure: [1, 2, 3], olay: [0, 0, 0], X: [[1], [2], [3]] }), /olay gözlenmediği/);
  assert.throws(() => coxRegresyon({ sure: [1, 2], olay: [1, 0], X: [[1], [2], [3]] }), IstatistikHatasi);
  assert.throws(() => coxRegresyon({ sure: [1, 2], olay: [1, 0], X: [[1], [null]] }), /eksik/);
  assert.throws(() => coxRegresyon({ sure: [1, 2], olay: [1, 0], X: [[1], [2]], baglar: 'exact' }), IstatistikHatasi);
  // coxPHVarsayimi girdiyle de çalışır
  const b = R.cox.efron.aml;
  const z = coxPHVarsayimi({ sure: b.sure, olay: b.olay, X: b.X.map((s) => dizi(s)) });
  yakin(z.genel.istatistik, b.zph.tablo[1][0], 1e-7, 1e-10, 'aml zph');
});
