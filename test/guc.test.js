// Güç / örneklem büyüklüğü analizi: R pwr (ve G*Power) ile karşılaştırma.
// Referans: arac/r-referans/guc.R. Çözülen bilinmeyenler R'de aynı pwr güç fonksiyonu uniroot(tol = 1e-11) ile
// yeniden çözülmüştür; pwr'ın kendi çıktısı (uniroot varsayılan tol ≈ 1.2e-4) ayrıca gevşek toleransla denetlenir.
import { test } from 'node:test';
import assert from 'node:assert/strict';
import * as G from '../src/cekirdek/guc.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin } from './yardimci.js';

const R = referans('guc');
const ALT = { 'two.sided': 'iki-yonlu', greater: 'buyuk', less: 'kucuk' };
const TIP = { 'one.sample': 'tek', 'two.sample': 'iki', paired: 'eslestirilmis' };
const yonlu = (d, alt) => (alt === 'less' ? -d : d);

// Toleranslar:
//  - doğrudan hesaplanan güç: merkezi olmayan t/F/ki-kare R ile ~1e-10 göreli uyumlu (dagilim.test.js); 1e-9 göreli
//    + 1e-12 mutlak (çok küçük güçlerde R'nin AS 243/226 algoritmalarının mutlak hatası belirleyicidir).
//  - çözülen n / etki: hedef 1e-8 göreli. Kök her iki tarafta ~1e-12 doğrulukla bulunur; fark, güç fonksiyonundaki
//    ~1e-10'luk farkın eğime bölünmesinden gelir.
//  - Merkezi olmayan F (ANOVA, regresyon): R'nin pf(ncp = λ) fonksiyonu (pnbeta, AS 226) errmax = 1e-9'da durur ve
//    ~1e-9 mutlak hata yapar (seriyle doğrulandı). Bu yüzden R betiği gücü ve kökleri ayrıca Poisson karışımı serisiyle
//    kesin hesaplar (guc0Seri, nKesin, fKesin…: 1e-12 / 1e-10); R'nin pf'si ile fark F_M = 2e-9 mutlak toleransla denetlenir.
const F_M = 2e-9;
const GUC_G = 1e-9;
const GUC_M = 1e-12;
const KOK = 1e-8;

test('t testi gücü (pwr.t.test)', () => {
  for (const r of R.tGuc) {
    const s = G.tTestGuc({ tip: TIP[r.tip], d: yonlu(r.d, r.alt), n: r.n, alfa: r.alfa, alternatif: ALT[r.alt] });
    yakin(s.guc, r.guc, GUC_G, GUC_M, `t gücü ${JSON.stringify(r)}`);
    assert.equal(s.gercekGuc, s.guc);
    assert.equal(s.cozulen, 'guc');
  }
});

test('t testi örneklem büyüklüğü (n çözümü)', () => {
  for (const r of R.tN) {
    const s = G.tTestGuc({ tip: TIP[r.tip], d: yonlu(r.d, r.alt), guc: r.guc, alfa: r.alfa, alternatif: ALT[r.alt] });
    const m = JSON.stringify(r);
    yakin(s.nKesin, r.nKesin, KOK, 0, `nKesin ${m}`);
    assert.ok(Math.abs(s.nKesin - r.nPwr) < 5e-4, `pwr çıktısı ${m}: ${s.nKesin} / ${r.nPwr}`);
    assert.equal(s.n, r.nTam, `n tam ${m}`);
    yakin(s.gercekGuc, r.gucTam, GUC_G, GUC_M, `gerçek güç ${m}`);
    assert.ok(s.gercekGuc >= r.guc);
    if (r.tip === 'two.sample') assert.equal(s.toplamN, 2 * s.n);
  }
});

test('t testi etki büyüklüğü (d çözümü)', () => {
  for (const r of R.tD) {
    const s = G.tTestGuc({ tip: TIP[r.tip], n: r.n, guc: r.guc, alfa: r.alfa, alternatif: ALT[r.alt] });
    yakin(s.etki.deger, r.dKesin, KOK, 0, `d ${JSON.stringify(r)}`);
  }
});

test('farklı grup büyüklükleriyle t testi (pwr.t2n.test)', () => {
  for (const r of R.t2n) {
    const d = yonlu(r.d, r.alt);
    const alt = ALT[r.alt];
    const m = JSON.stringify(r);
    yakin(G.tTestIkiNGuc({ d, n1: r.n1, n2: r.n2, alfa: r.alfa, alternatif: alt }).guc, r.guc0, GUC_G, GUC_M, `güç ${m}`);
    yakin(G.tTestIkiNGuc({ n1: r.n1, n2: r.n2, guc: r.guc, alfa: r.alfa, alternatif: alt }).etki.deger, r.dKesin, KOK, 0, `d ${m}`);
    for (const [ad, ref] of [['n1', r.n1Kesin], ['n2', r.n2Kesin]]) {
      const girdi = { d, n1: r.n1, n2: r.n2, guc: r.guc, alfa: r.alfa, alternatif: alt };
      girdi[ad] = null;
      if (ref === null) {
        assert.throws(() => G.tTestIkiNGuc(girdi), IstatistikHatasi, `${ad} ulaşılamaz ${m}`);
      } else {
        const s = G.tTestIkiNGuc(girdi);
        yakin(s.nKesin, ref, KOK, 0, `${ad} ${m}`);
        assert.equal(s[ad], Math.ceil(ref - 1e-9));
        assert.ok(s.gercekGuc >= r.guc);
      }
    }
  }
});

test('t testi grup oranı n2 = k·n1', () => {
  for (const r of R.tOran) {
    const s = G.tTestGuc({ tip: 'iki', d: r.d, guc: r.guc, alfa: r.alfa, alternatif: ALT[r.alt], oran: r.k });
    const m = JSON.stringify(r);
    yakin(s.n1Kesin, r.n1Kesin, KOK, 0, `n1 ${m}`);
    assert.equal(s.n1, r.n1, m);
    assert.equal(s.n2, r.n2, m);
    assert.equal(s.toplamN, r.n1 + r.n2);
    yakin(s.gercekGuc, r.gucTam, GUC_G, GUC_M, `güç ${m}`);
  }
});

test('Mann-Whitney / Wilcoxon (A.R.E. yöntemi)', () => {
  for (const r of R.areT) {
    const f = r.test === 'mw' ? G.mannWhitneyGuc : G.wilcoxonGuc;
    const ortak = { ebeveyn: r.ebeveyn, alfa: r.alfa, alternatif: ALT[r.alt] };
    const m = JSON.stringify(r);
    yakin(f({ ...ortak, d: r.d, n: r.n }).guc, r.guc0, GUC_G, GUC_M, `güç ${m}`);
    const s = f({ ...ortak, d: r.d, guc: r.guc });
    yakin(s.nKesin, r.nKesin, KOK, 0, `n ${m}`);
    assert.equal(s.n, r.nTam, m);
    yakin(s.gercekGuc, r.gucTam, GUC_G, GUC_M, `gerçek güç ${m}`);
    yakin(f({ ...ortak, n: r.n, guc: r.guc }).etki.deger, r.dKesin, KOK, 0, `d ${m}`);
  }
  // G*Power kılavuzu örneği: tek yönlü, d = 0.5, güç 0.80, normal ebeveyn → 53/grup, δ = 2.5152354, sd = 99.2225438
  const g = G.mannWhitneyGuc({ d: 0.5, guc: 0.8, alternatif: 'buyuk' });
  assert.equal(g.n, R.gpowerMW.n);
  yakin(g.ncp, 2.5152354, 0, 5e-8, 'G*Power δ');
  yakin(g.sd, 99.2225438, 0, 5e-8, 'G*Power sd');
  yakin(g.ncp, R.gpowerMW.ncp, 1e-13);
  assert.equal(G.mannWhitneyGuc({ d: 0.5, guc: 0.8, ebeveyn: 'enKotu' }).nKesin.toFixed(8), (G.tTestGuc({ d: 0.5, guc: 0.8 }).nKesin / 0.864).toFixed(8));
});

test('tek yönlü ANOVA (pwr.anova.test)', () => {
  for (const r of R.anova) {
    const m = JSON.stringify(r);
    const g0 = G.anovaGuc({ k: r.k, f: r.f, n: r.n, alfa: r.alfa }).guc;
    yakin(g0, r.guc0Seri, 1e-12, 1e-15, `güç (seri) ${m}`);
    yakin(g0, r.guc0, 0, F_M, `güç (pf) ${m}`);
    const s = G.anovaGuc({ k: r.k, f: r.f, guc: r.guc, alfa: r.alfa });
    yakin(s.nKesin, r.nKesin, 1e-10, 0, `n ${m}`);
    assert.ok(Math.abs(s.nKesin - r.nPwr) < 5e-4, `pwr ${m}`);
    assert.equal(s.n, Math.ceil(r.nKesin));
    assert.equal(s.toplamN, r.k * s.n);
    yakin(s.gercekGuc, r.gucTam, 1e-12, 1e-15, `gerçek güç ${m}`);
    yakin(G.anovaGuc({ k: r.k, n: r.n, guc: r.guc, alfa: r.alfa }).etki.deger, r.fKesin, 1e-10, 0, `f ${m}`);
  }
});

test('korelasyon (pwr.r.test)', () => {
  for (const r of R.korelasyon) {
    const rr = yonlu(r.r, r.alt);
    const alt = ALT[r.alt];
    const m = JSON.stringify(r);
    yakin(G.korelasyonGuc({ r: rr, n: r.n, alfa: r.alfa, alternatif: alt }).guc, r.guc0, 1e-12, 1e-14, `güç ${m}`);
    const s = G.korelasyonGuc({ r: rr, guc: r.guc, alfa: r.alfa, alternatif: alt });
    yakin(s.nKesin, r.nKesin, KOK, 0, `n ${m}`);
    assert.ok(Math.abs(s.nKesin - r.nPwr) < 5e-4, `pwr ${m}`);
    assert.equal(s.n, Math.ceil(r.nKesin));
    yakin(s.gercekGuc, r.gucTam, 1e-12, 1e-14, `gerçek güç ${m}`);
    yakin(G.korelasyonGuc({ n: r.n, guc: r.guc, alfa: r.alfa, alternatif: alt }).etki.deger, r.rKesin, KOK, 0, `r ${m}`);
  }
});

test('ki-kare (pwr.chisq.test)', () => {
  for (const r of R.kikare) {
    const m = JSON.stringify(r);
    yakin(G.kiKareGuc({ w: r.w, df: r.df, N: r.N, alfa: r.alfa }).guc, r.guc0, GUC_G, GUC_M, `güç ${m}`);
    const s = G.kiKareGuc({ w: r.w, df: r.df, guc: r.guc, alfa: r.alfa });
    yakin(s.nKesin, r.nKesin, KOK, 0, `N ${m}`);
    assert.ok(Math.abs(s.nKesin - r.nPwr) < 5e-4, `pwr ${m}`);
    assert.equal(s.N, Math.ceil(r.nKesin));
    yakin(s.gercekGuc, r.gucTam, GUC_G, GUC_M, `gerçek güç ${m}`);
    yakin(G.kiKareGuc({ df: r.df, N: r.N, guc: r.guc, alfa: r.alfa }).etki.deger, r.wKesin, KOK, 0, `w ${m}`);
  }
});

test('iki oran — arksinüs (pwr.2p.test, pwr.2p2n.test)', () => {
  for (const r of R.ikiOranH) {
    const alt = ALT[r.alt];
    const m = JSON.stringify(r);
    yakin(G.ikiOranGuc({ p1: r.p1, p2: r.p2, n: r.n, alfa: r.alfa, alternatif: alt }).guc, r.guc0, 1e-12, 1e-14, `güç ${m}`);
    const s = G.ikiOranGuc({ p1: r.p1, p2: r.p2, guc: r.guc, alfa: r.alfa, alternatif: alt });
    yakin(s.h, r.h, 1e-14, 0, 'h');
    yakin(s.nKesin, r.nKesin, KOK, 0, `n ${m}`);
    assert.equal(s.n, Math.ceil(r.nKesin));
    yakin(s.gercekGuc, r.gucTam, 1e-12, 1e-14, `gerçek güç ${m}`);
    // h doğrudan verilerek
    yakin(G.ikiOranGuc({ h: r.h, guc: r.guc, alfa: r.alfa, alternatif: alt }).nKesin, r.nKesin, KOK, 0, `n (h) ${m}`);
    yakin(G.ikiOranGuc({ n: r.n, guc: r.guc, alfa: r.alfa, alternatif: alt }).etki.deger, r.hKesin, KOK, 0, `h çözümü ${m}`);
  }
  for (const r of R.ikiOranH2n) {
    const alt = ALT[r.alt];
    const m = JSON.stringify(r);
    yakin(G.ikiOranGuc({ h: r.h, n: r.n1, n2: r.n2, alfa: r.alfa, alternatif: alt }).guc, r.guc0, 1e-12, 1e-14, `güç ${m}`);
  }
  for (const r of R.ikiOranHOran) {
    const s = G.ikiOranGuc({ p1: r.p1, p2: r.p2, guc: r.guc, alfa: r.alfa, oran: r.k });
    const m = JSON.stringify(r);
    yakin(s.nKesin, r.n1Kesin, KOK, 0, `n1 ${m}`);
    assert.equal(s.n1, r.n1, m);
    assert.equal(s.n2, r.n2, m);
    yakin(s.gercekGuc, r.gucTam, 1e-12, 1e-14, `gerçek güç ${m}`);
  }
});

test('iki oran — normal yaklaşım (power.prop.test, Hmisc::bsamsize) ve süreklilik düzeltmesi', () => {
  for (const r of R.ikiOranNormal) {
    // power.prop.test 'one.sided' yönsüzdür; burada doğru yön seçilir
    const alt = r.alt === 'two.sided' ? 'iki-yonlu' : r.p1 > r.p2 ? 'buyuk' : 'kucuk';
    const m = JSON.stringify(r);
    const ortak = { p1: r.p1, alfa: r.alfa, alternatif: alt, yontem: 'normal' };
    yakin(G.ikiOranGuc({ ...ortak, p2: r.p2, n: r.n }).guc, r.guc0, 1e-12, 1e-14, `güç ${m}`);
    yakin(G.ikiOranGuc({ ...ortak, p2: r.p2, guc: r.guc }).nKesin, r.nKesin, KOK, 0, `n ${m}`);
    const altP2 = r.alt === 'two.sided' ? 'iki-yonlu' : 'kucuk'; // power.prop.test p2'yi (p1, 1) aralığında arar
    yakin(G.ikiOranGuc({ ...ortak, alternatif: altP2, n: r.n, guc: r.guc }).p2, r.p2Kesin, KOK, 0, `p2 ${m}`);
  }
  for (const r of R.ikiOranBsam) {
    const s = G.ikiOranGuc({ p1: r.p1, p2: r.p2, guc: r.guc, alfa: r.alfa, oran: r.k, yontem: 'normal' });
    yakin(s.nKesin, r.n1Kesin, 1e-12, 0, `n1 ${JSON.stringify(r)}`);
    assert.equal(s.n2, Math.ceil(r.n2Kesin - 1e-9));
  }
  for (const r of R.ikiOranSureklilik) {
    const alt = r.yon === 2 ? 'iki-yonlu' : r.p1 > r.p2 ? 'buyuk' : 'kucuk';
    const ortak = { p1: r.p1, p2: r.p2, alfa: r.alfa, alternatif: alt, oran: r.k };
    const m = JSON.stringify(r);
    yakin(G.ikiOranGuc({ ...ortak, guc: r.guc, yontem: 'normal' }).nKesin, r.nKesin, 1e-12, 0, `n ${m}`);
    yakin(G.ikiOranGuc({ ...ortak, guc: r.guc, yontem: 'normal-sureklilik' }).nKesin, r.nKesinCC, 1e-12, 0, `n CC ${m}`);
    yakin(G.ikiOranGuc({ ...ortak, n: r.n, yontem: 'normal' }).guc, r.guc0, 1e-12, 1e-14, `güç ${m}`);
    yakin(G.ikiOranGuc({ ...ortak, n: r.n, yontem: 'normal-sureklilik' }).guc, r.guc0CC, 1e-12, 1e-14, `güç CC ${m}`);
    // kapalı form n' ile güç formülü birbirinin tersi
    yakin(r.gucCCnKesin, r.guc, 1e-12);
    yakin(G.ikiOranGuc({ ...ortak, n: r.nKesinCC, yontem: 'normal-sureklilik' }).guc, r.guc, 1e-12, 0, `ters ${m}`);
  }
});

test('tek oran (pwr.p.test)', () => {
  for (const r of R.tekOran) {
    const alt = ALT[r.alt];
    const m = JSON.stringify(r);
    yakin(G.tekOranGuc({ p: r.p, p0: r.p0, n: r.n, alfa: r.alfa, alternatif: alt }).guc, r.guc0, 1e-12, 1e-14, `güç ${m}`);
    const s = G.tekOranGuc({ p: r.p, p0: r.p0, guc: r.guc, alfa: r.alfa, alternatif: alt });
    yakin(s.nKesin, r.nKesin, KOK, 0, `n ${m}`);
    assert.equal(s.n, Math.ceil(r.nKesin));
    yakin(s.gercekGuc, r.gucTam, 1e-12, 1e-14, `gerçek güç ${m}`);
    const e = G.tekOranGuc({ p0: r.p0, n: r.n, guc: r.guc, alfa: r.alfa, alternatif: alt });
    yakin(e.etki.deger, r.hKesin, KOK, 0, `h ${m}`);
    yakin(G.hdenOranlar(e.p, r.p0), r.hKesin, KOK, 0, `p çözümü ${m}`);
  }
});

test('çoklu regresyon (pwr.f2.test)', () => {
  for (const r of R.regresyon) {
    const m = JSON.stringify(r);
    const g0 = G.regresyonGuc({ u: r.u, v: r.v, f2: r.f2, alfa: r.alfa }).guc;
    yakin(g0, r.guc0Seri, 1e-12, 1e-15, `güç (seri) ${m}`);
    yakin(g0, r.guc0, 0, F_M, `güç (pf) ${m}`);
    yakin(G.regresyonGuc({ u: r.u, N: r.v + r.u + 1, f2: r.f2, alfa: r.alfa }).guc, g0, 1e-15, 0, `güç (N) ${m}`);
    const s = G.regresyonGuc({ u: r.u, f2: r.f2, guc: r.guc, alfa: r.alfa });
    yakin(s.vKesin, r.vKesin, 1e-10, 0, `v ${m}`);
    assert.ok(Math.abs(s.vKesin - r.vPwr) < 5e-4, `pwr ${m}`);
    assert.equal(s.v, Math.ceil(r.vKesin));
    assert.equal(s.toplamN, s.v + r.u + 1);
    yakin(s.gercekGuc, r.gucTam, 1e-12, 1e-15, `gerçek güç ${m}`);
    yakin(G.regresyonGuc({ u: r.u, v: r.v, guc: r.guc, alfa: r.alfa }).etki.deger, r.f2Kesin, 1e-10, 0, `f2 ${m}`);
  }
  // R² artışı: 3 kontrol + 2 sınanan yordayıcı, N = 100 → v = 94
  const a = G.regresyonGuc({ u: 2, p: 5, N: 100, f2: 0.1 });
  const b = G.regresyonGuc({ u: 2, v: 94, f2: 0.1 });
  yakin(a.guc, b.guc, 1e-15);
  assert.equal(G.regresyonGuc({ u: 2, p: 5, f2: 0.1, guc: 0.8 }).toplamN, G.regresyonGuc({ u: 2, f2: 0.1, guc: 0.8 }).v + 6);
});

test('lojistik regresyon (Hsieh 1998; powerMediation)', () => {
  for (const r of R.lojistikIkili) {
    const ortak = { tip: 'ikili', p1: r.p1, p2: r.p2, B: r.B, alfa: r.alfa };
    const m = JSON.stringify(r);
    const s = G.lojistikGuc({ ...ortak, guc: r.guc });
    yakin(s.nKesin, r.nKesin, 1e-12, 0, `n ${m}`);
    assert.equal(s.n, r.nTam, m);
    yakin(G.lojistikGuc({ ...ortak, n: r.n }).guc, r.guc0, 1e-12, 1e-14, `güç ${m}`);
    // OR ile verilen girdi aynı sonucu vermeli
    const OR = (r.p2 / (1 - r.p2)) / (r.p1 / (1 - r.p1));
    yakin(G.lojistikGuc({ tip: 'ikili', p1: r.p1, OR, B: r.B, alfa: r.alfa, guc: r.guc }).nKesin, r.nKesin, 1e-12);
    // OR çözümü geri dönmeli
    const e = G.lojistikGuc({ ...ortak, p2: undefined, n: s.nKesin, guc: r.guc });
    if (r.p2 > r.p1) yakin(e.etki.deger, OR, 1e-9, 0, `OR çözümü ${m}`);
  }
  for (const r of R.lojistikSurekli) {
    const ortak = { tip: 'surekli', p1: r.p1, OR: r.OR, alfa: r.alfa };
    const m = JSON.stringify(r);
    const s = G.lojistikGuc({ ...ortak, guc: r.guc });
    yakin(s.nKesin, r.nKesin, 1e-12, 0, `n ${m}`);
    assert.equal(s.n, r.nTam, m);
    yakin(G.lojistikGuc({ ...ortak, n: r.n }).guc, r.guc0, 1e-12, 1e-14, `güç ${m}`);
    const e = G.lojistikGuc({ tip: 'surekli', p1: r.p1, n: s.nKesin, guc: r.guc, alfa: r.alfa, alternatif: r.OR > 1 ? 'buyuk' : 'kucuk' });
    // tek yönlü çözüm, iki yönlü n ile farklı z kullanır; yalnızca yönü denetle
    assert.ok(r.OR > 1 ? e.etki.deger > 1 : e.etki.deger < 1);
    const e2 = G.lojistikGuc({ tip: 'surekli', p1: r.p1, n: s.nKesin, guc: r.guc, alfa: r.alfa });
    yakin(Math.abs(Math.log(e2.etki.deger)), Math.abs(Math.log(r.OR)), 1e-9, 0, `OR çözümü ${m}`);
  }
  // R² düzeltmesi: n / (1 − R²)
  const a = G.lojistikGuc({ p1: 0.3, OR: 1.5, guc: 0.8 });
  const b = G.lojistikGuc({ p1: 0.3, OR: 1.5, guc: 0.8, r2: 0.2 });
  yakin(b.nKesin, a.nKesin / 0.8, 1e-14);
  yakin(G.lojistikGuc({ p1: 0.3, OR: 1.5, n: b.nKesin, r2: 0.2 }).guc, 0.8, 1e-12);
});

test('sağkalım: Schoenfeld (gsDesign::nEvents) ve Freedman (powerSurvEpi)', () => {
  for (const r of R.sagkalimSchoenfeld) {
    const m = JSON.stringify(r);
    const s = G.sagkalimGuc({ hr: r.hr, oran: r.oran, alfa: r.alfa, guc: r.guc });
    yakin(s.olaySayisiKesin, r.olayKesin, 1e-12, 0, `olay ${m}`);
    assert.equal(s.olaySayisi, Math.ceil(r.olayKesin));
    yakin(G.sagkalimGuc({ hr: r.hr, oran: r.oran, alfa: r.alfa, olaySayisi: r.d }).guc, r.guc0, 1e-12, 1e-14, `güç ${m}`);
    // HR çözümü: hr < 1 olan kök etki.deger, > 1 olan hrUst
    const e = G.sagkalimGuc({ oran: r.oran, alfa: r.alfa, guc: r.guc, olaySayisi: r.olayKesin });
    yakin(r.hr < 1 ? e.etki.deger : e.hrUst, r.hr, 1e-9, 0, `HR çözümü ${m}`);
  }
  for (const r of R.sagkalimFreedman) {
    const m = JSON.stringify(r);
    const s = G.sagkalimGuc({ hr: r.hr, oran: r.oran, alfa: r.alfa, guc: r.guc, olayOlasiligi: [r.pE, r.pC], yontem: 'freedman' });
    yakin(s.olaySayisiKesin, r.olayKesin, 1e-12, 0, `olay ${m}`);
    assert.equal(s.n1, r.sE, `nE ${m}`);
    assert.equal(s.n2, r.sC, `nC ${m}`);
    const g = G.sagkalimGuc({ hr: r.hr, oran: r.nC / r.nE, alfa: r.alfa, N: r.nE + r.nC, olayOlasiligi: [r.pE, r.pC], yontem: 'freedman' });
    yakin(g.guc, r.guc0, 1e-12, 1e-14, `güç ${m}`);
  }
  // Toplam örneklem: genel olay olasılığı
  const s = G.sagkalimGuc({ hr: 0.7, guc: 0.8, olayOlasiligi: 0.4 });
  assert.equal(s.toplamN, s.n1 + s.n2);
  yakin(s.nKesin, s.olaySayisiKesin / 0.4, 1e-14);
});

test('etki büyüklüğü yardımcıları', () => {
  yakin(G.hdenOranlar(0.65, 0.45), R.etki.h, 1e-14);
  yakin(G.wdenOranlar([0.25, 0.25, 0.25, 0.25], [0.4, 0.2, 0.2, 0.2]), R.etki.w1, 1e-14);
  yakin(G.wdenTablo([[0.1, 0.15, 0.1], [0.2, 0.25, 0.2]]), R.etki.w2, 1e-12);
  yakin(G.fdenEta2(0.0588235294117647), 0.25, 1e-14);
  yakin(G.f2denR2(0.13043478260869565), 0.15, 1e-14);
  yakin(G.f2denR2(0.3, 0.2), 0.1 / 0.7, 1e-14);
  // d: birleştirilmiş SS
  yakin(G.dHesapla(10, 2, 20, 9, 3, 30), 1 / Math.sqrt((19 * 4 + 29 * 9) / 48), 1e-14);
  yakin(G.dHesapla(10, 2, null, 9, 3, null), 1 / Math.sqrt(6.5), 1e-14);
});

test('hata ve uç durumlar', () => {
  assert.throws(() => G.tTestGuc({ d: 0.5 }), IstatistikHatasi); // iki bilinmeyen
  assert.throws(() => G.tTestGuc({ d: 0.5, n: 20, guc: 0.8 }), IstatistikHatasi); // bilinmeyen yok
  assert.throws(() => G.tTestGuc({ d: 0.5, guc: 0.03 }), /anlamlılık/); // güç < alfa
  assert.throws(() => G.tTestGuc({ d: 0.5, guc: 0.8, alternatif: 'kucuk' }), IstatistikHatasi); // yön çelişkisi
  assert.throws(() => G.tTestGuc({ d: 0.5, guc: 0.8, alternatif: 'xx' }), /alternatif/);
  assert.throws(() => G.anovaGuc({ k: 1, f: 0.25, guc: 0.8 }), IstatistikHatasi);
  assert.throws(() => G.korelasyonGuc({ r: 1.2, guc: 0.8 }), IstatistikHatasi);
  assert.throws(() => G.ikiOranGuc({ p1: 0.3, p2: 0.3, guc: 0.8 }), IstatistikHatasi);
  assert.throws(() => G.sagkalimGuc({ hr: 1, guc: 0.8 }), IstatistikHatasi);
  // R adlarıyla alternatif
  yakin(G.tTestGuc({ d: 0.5, n: 20, alternatif: 'greater' }).guc, G.tTestGuc({ d: 0.5, n: 20, alternatif: 'buyuk' }).guc, 0);
  // Çok büyük etki: en küçük n yeterli
  const s = G.tTestGuc({ tip: 'tek', d: 20, guc: 0.8 });
  assert.equal(s.n, 2);
  assert.ok(s.uyarilar.length === 1);
});

test('G*Power çıktılarıyla karşılaştırma (yayımlanmış örnekler)', () => {
  // t testi, iki yönlü, d = 0.5, α = 0.05, güç 0.80: grup başına 64, gerçek güç 0.8014596, kritik t = 1.9789706, δ = 2.8284271
  const t = G.tTestGuc({ d: 0.5, guc: 0.8 });
  assert.equal(t.n, 64);
  assert.equal(t.toplamN, 128);
  yakin(t.gercekGuc, 0.8014596, 0, 5e-8, 'gerçek güç');
  yakin(t.kritik, 1.9789706, 0, 5e-8, 'kritik t');
  yakin(t.ncp, 2.8284271, 0, 5e-8, 'δ');
  assert.equal(t.sd, 126);
});
