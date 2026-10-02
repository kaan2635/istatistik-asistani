import { test } from 'node:test';
import assert from 'node:assert/strict';
import * as NP from '../src/cekirdek/nonparametrik.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { inf, referans, yakin } from './yardimci.js';

const R = referans('nonparametrik');

// R'nin asimptotik Hodges-Lehmann güven aralığı uniroot(tol = 1e-4) ile bulunur; bizimki adım fonksiyonunun kesin
// sıçrama noktasıdır. Bu yüzden asimptotik aralık sınırları 2e-4 mutlak toleransla karşılaştırılır.
const HL_TOL = 2e-4;

function ortaIkili(D) {
  const s = Float64Array.from(D).sort();
  const m = s.length;
  return m % 2 ? [s[(m - 1) / 2], s[(m - 1) / 2]] : [s[m / 2 - 1], s[m / 2]];
}

const altSinir = (v) => (v === null ? -Infinity : v);

test('Mann-Whitney U: R 4.6 wilcox.test ile karşılaştırma', () => {
  R.mw.forEach((r, i) => {
    const ad = `mw[${i}] (${r.yontem})`;
    const s = NP.mannWhitneyU(r.x, r.y, { yontem: r.yontem, sureklilik: r.sureklilik, alternatif: r.alt, duzey: r.duzey, mu: r.mu });
    yakin(s.W, r.W, 1e-13, 1e-12, `${ad} W`);
    yakin(s.p, r.p, 1e-10, 1e-14, `${ad} p`);
    assert.equal(s.pYontemi, r.kesinMi ? 'kesin' : 'asimptotik', `${ad} p yöntemi`);
    yakin(s.z, -Math.abs(r.z0), 1e-12, 1e-14, `${ad} z (SPSS)`);
    yakin(s.gruplar[0].siraToplami, r.R1, 1e-14, 1e-12, `${ad} R1`);
    yakin(s.gruplar[1].siraToplami, r.R2, 1e-14, 1e-12, `${ad} R2`);
    yakin(s.U, Math.min(r.W, r.x.length * r.y.length - r.W), 1e-14, 1e-12, `${ad} U`);
    yakin(s.etki.deger, Math.abs(r.z0) / Math.sqrt(r.x.length + r.y.length), 1e-12, 1e-14, `${ad} r`);
    const g0 = altSinir(r.ga[0]);
    const g1 = inf(r.ga[1]);
    const butceAsildi = s.uyarilar.some((u) => u.includes('bütçesini aştı'));
    if (r.kesinMi && !butceAsildi) {
      yakin(s.hodgesLehmann.tahmin, r.tahmin, 1e-12, 1e-12, `${ad} HL tahmin`);
      yakin(s.ga.alt, g0, 1e-12, 1e-12, `${ad} GA alt`);
      yakin(s.ga.ust, g1, 1e-12, 1e-12, `${ad} GA üst`);
      // kesin aralıkta gerçekleşen düzey (R 4.6 her zaman raporlar)
      yakin(s.ga.duzey, r.gaDuzey, 1e-12, 1e-14, `${ad} GA düzeyi`);
    } else if (r.kesinMi) {
      // büyük örneklemde (n1 = 60, n2 = 55) bağlı kesin GA taraması bütçeyi aşar: p kesin, GA asimptotik
      yakin(s.hodgesLehmann.tahmin, r.tahmin, 1e-12, 1e-12, `${ad} HL tahmin`);
      assert.equal(s.ga.duzey, r.duzey);
      const as = NP.mannWhitneyU(r.x, r.y, { yontem: 'asimptotik', alternatif: r.alt, duzey: r.duzey, mu: r.mu });
      assert.equal(s.ga.alt, as.ga.alt);
      assert.equal(s.ga.ust, as.ga.ust);
    } else {
      if (Number.isFinite(g0)) yakin(s.ga.alt, g0, 0, HL_TOL, `${ad} GA alt (asimptotik)`);
      else assert.equal(s.ga.alt, -Infinity);
      if (Number.isFinite(g1)) yakin(s.ga.ust, g1, 0, HL_TOL, `${ad} GA üst (asimptotik)`);
      else assert.equal(s.ga.ust, Infinity);
      // R'nin tahmini: ikili farkların medyanı (ya da ortadaki iki farkın arasındaki düzlükte bir nokta)
      const [a, b] = ortaIkili(r.ikiliFarklar);
      assert.ok(r.tahmin >= a - HL_TOL && r.tahmin <= b + HL_TOL, `${ad} R HL tahmini ortada değil`);
      yakin(s.hodgesLehmann.tahmin, (a + b) / 2, 1e-12, 1e-12, `${ad} HL medyan`);
      yakin(s.ga.duzey, r.gaDuzey, 1e-12, 1e-14, `${ad} GA düzeyi`);
    }
  });
});

test('Mann-Whitney U: bağlı kesin GA taraması bütçe yeterliyse R 4.6 ile aynı (n1 = 60, n2 = 55)', () => {
  const r = R.mw[16];
  assert.equal(r.yontem, 'kesin');
  const s = NP.mannWhitneyU(r.x, r.y, { yontem: 'kesin', kesinGAButce: 1e12 });
  yakin(s.ga.alt, r.ga[0], 1e-12, 1e-12, 'GA alt');
  yakin(s.ga.ust, r.ga[1], 1e-12, 1e-12, 'GA üst');
  yakin(s.ga.duzey, r.gaDuzey, 1e-12, 1e-14, 'GA düzeyi');
});

test('Mann-Whitney U: bağlı veride kesin p, "sapma" kuralı (R coin::wilcox_test)', () => {
  R.mwKesinBagli.forEach((r, i) => {
    const sec = { yontem: 'kesin', kesinIkiYonlu: 'sapma' };
    yakin(NP.mannWhitneyU(r.x, r.y, sec).p, r.p, 1e-9, 1e-14, `kesin[${i}] iki yönlü`);
    yakin(NP.mannWhitneyU(r.x, r.y, { ...sec, alternatif: 'buyuk' }).p, r.pBuyuk, 1e-9, 1e-14, `kesin[${i}] büyük`);
    yakin(NP.mannWhitneyU(r.x, r.y, { ...sec, alternatif: 'kucuk' }).p, r.pKucuk, 1e-9, 1e-14, `kesin[${i}] küçük`);
    yakin(NP.mannWhitneyU(r.x, r.y, { yontem: 'asimptotik' }).zYonlu, r.z, 1e-12, 1e-14, `kesin[${i}] coin z`);
  });
});

test('Mann-Whitney U: SPSS alanları ve etki büyüklükleri', () => {
  const x = [1, 2, 2, 3, 3, 3, 4, 5, 5, 6];
  const y = [2, 3, 3, 4, 4, 5, 5, 5, 6, 7, 7, 8];
  const s = NP.mannWhitneyU(x, y);
  assert.equal(s.pYontemi, 'asimptotik'); // bağ var → asimptotik (SPSS gibi)
  assert.equal(s.U, Math.min(s.U1, s.U2));
  assert.equal(s.U1 + s.U2, 120);
  assert.equal(s.wilcoxonW, s.U1 < s.U2 ? s.gruplar[0].siraToplami : s.gruplar[1].siraToplami);
  yakin(s.etkiler.rankBiserial, (s.U1 - s.U2) / 120, 1e-14, 1e-15, 'rank-biserial');
  assert.ok(s.z < 0);
  // grupların yer değiştirmesi U ve p'yi değiştirmez, z yönü değişir
  const t = NP.mannWhitneyU(y, x);
  yakin(t.p, s.p, 1e-13, 0, 'simetri p');
  assert.equal(t.U, s.U);
  yakin(t.zYonlu, -s.zYonlu, 1e-13, 0, 'simetri z');
  assert.equal(t.wilcoxonW, s.wilcoxonW);
});

test('Wilcoxon işaretli sıralar: R 4.6 wilcox.test(paired) ile karşılaştırma', () => {
  R.sr.forEach((r, i) => {
    const ad = `sr[${i}] (${r.yontem})`;
    const s = NP.wilcoxonIsaretliSira(r.x, r.y, {
      yontem: r.yontem,
      sureklilik: r.sureklilik,
      alternatif: r.alt,
      duzey: r.duzey,
      mu: r.mu,
      sifirYontemi: r.yontem === 'kesin' ? 'pratt' : 'wilcoxon', // R 4.6 kesin testi sıfırları Pratt yöntemiyle işler
    });
    yakin(s.V, r.V, 1e-13, 1e-12, `${ad} V`);
    yakin(s.Weksi, r.Wneg, 1e-13, 1e-12, `${ad} W-`);
    yakin(s.p, r.p, 1e-10, 1e-14, `${ad} p`);
    assert.equal(s.pYontemi, r.kesinMi ? 'kesin' : 'asimptotik', `${ad} p yöntemi`);
    assert.equal(s.sifirFark, r.sifir, `${ad} sıfır fark`);
    yakin(s.z, -Math.abs(r.z0), 1e-12, 1e-14, `${ad} z`);
    const g0 = altSinir(r.ga[0]);
    const g1 = inf(r.ga[1]);
    if (r.kesinMi) {
      yakin(s.hodgesLehmann.tahmin, r.tahmin, 1e-12, 1e-12, `${ad} HL tahmin`);
      yakin(s.ga.alt, g0, 1e-12, 1e-12, `${ad} GA alt`);
      yakin(s.ga.ust, g1, 1e-12, 1e-12, `${ad} GA üst`);
    } else {
      if (Number.isFinite(g0)) yakin(s.ga.alt, g0, 0, HL_TOL, `${ad} GA alt (asimptotik)`);
      else assert.equal(s.ga.alt, -Infinity);
      if (Number.isFinite(g1)) yakin(s.ga.ust, g1, 0, HL_TOL, `${ad} GA üst (asimptotik)`);
      else assert.equal(s.ga.ust, Infinity);
      const d = r.x.map((v, j) => v - (r.y ? r.y[j] : 0));
      const W = [];
      for (let a = 0; a < d.length; a++) for (let b = a; b < d.length; b++) W.push((d[a] + d[b]) / 2);
      const [a, b] = ortaIkili(W);
      assert.ok(r.tahmin >= a - HL_TOL && r.tahmin <= b + HL_TOL, `${ad} R HL tahmini ortada değil`);
      yakin(s.hodgesLehmann.tahmin, (a + b) / 2, 1e-12, 1e-12, `${ad} HL medyan`);
    }
    yakin(s.ga.duzey, r.gaDuzey, 1e-12, 1e-14, `${ad} GA düzeyi`);
  });
});

test('Wilcoxon işaretli sıralar: bağlı/sıfır farklı kesin p (R coin::wilcoxsign_test)', () => {
  R.srKesinBagli.forEach((r, i) => {
    const sec = { yontem: 'kesin', sifirYontemi: 'wilcoxon' };
    yakin(NP.wilcoxonIsaretliSira(r.x, r.y, sec).p, r.p, 1e-9, 1e-14, `kesin[${i}]`);
    yakin(NP.wilcoxonIsaretliSira(r.x, r.y, { ...sec, alternatif: 'buyuk' }).p, r.pBuyuk, 1e-9, 1e-14, `kesin[${i}] büyük`);
    yakin(NP.wilcoxonIsaretliSira(r.x, r.y, { ...sec, alternatif: 'kucuk' }).p, r.pKucuk, 1e-9, 1e-14, `kesin[${i}] küçük`);
    yakin(NP.wilcoxonIsaretliSira(r.x, r.y, { yontem: 'kesin', sifirYontemi: 'pratt' }).p, r.pPratt, 1e-9, 1e-14, `kesin[${i}] Pratt`);
  });
});

test('İşaret testi: R binom.test ile karşılaştırma', () => {
  R.isaret.forEach((r, i) => {
    const s = NP.isaretTesti(r.x, r.y);
    assert.equal(s.pozitif, r.pozitif);
    assert.equal(s.negatif, r.negatif);
    yakin(s.pKesin, r.pKesin, 1e-11, 1e-300, `işaret[${i}] kesin`);
    yakin(s.z, r.z, 1e-13, 1e-14, `işaret[${i}] z`);
    yakin(s.pAsimptotik, r.pAsim, 1e-12, 1e-300, `işaret[${i}] asimptotik`);
    yakin(s.p, s.pozitif + s.negatif <= 25 ? r.pKesin : r.pAsim, 1e-11, 1e-300, `işaret[${i}] SPSS kuralı`);
    yakin(NP.isaretTesti(r.x, r.y, { alternatif: 'buyuk', yontem: 'kesin' }).p, r.pBuyuk, 1e-11, 1e-300, `işaret[${i}] büyük`);
    yakin(NP.isaretTesti(r.x, r.y, { alternatif: 'kucuk', yontem: 'kesin' }).p, r.pKucuk, 1e-11, 1e-300, `işaret[${i}] küçük`);
  });
});

test('Kruskal-Wallis: R kruskal.test ile karşılaştırma', () => {
  R.kw.forEach((r, i) => {
    const s = NP.kruskalWallis(r.gruplar);
    yakin(s.istatistik, r.H, 1e-12, 1e-14, `kw[${i}] H`);
    assert.equal(s.sd, r.sd);
    yakin(s.p, r.p, 1e-10, 1e-300, `kw[${i}] p`);
    r.ortSira.forEach((o, j) => yakin(s.gruplar[j].ortalamaSira, o, 1e-13, 1e-13, `kw[${i}] ortalama sıra ${j}`));
    yakin(s.etkiler.epsilonKare, r.eps2, 1e-12, 1e-15, `kw[${i}] ε²`);
    yakin(s.etkiler.etaKareH, r.eta2, 1e-12, 1e-14, `kw[${i}] η²`);
  });
});

test('Dunn: R rstatix::dunn_test ve dunn.test ile karşılaştırma', () => {
  for (const r of R.dunn) {
    const s = NP.dunn(r.gruplar, { duzeltme: r.yontem });
    s.karsilastirmalar.forEach((c, i) => {
      assert.equal(c.grup1 + 1, r.r.grup1[i]);
      assert.equal(c.grup2 + 1, r.r.grup2[i]);
      // rstatix: z = (R̄_2 - R̄_1)/SE; bizimki (SPSS ve dunn.test gibi) R̄_1 - R̄_2
      yakin(c.z, -r.r.z[i], 1e-12, 1e-14, `dunn z ${i}`);
      yakin(c.p, r.r.p[i], 1e-10, 1e-300, `dunn p ${i}`);
      yakin(c.pDuzeltilmis, r.r.pAdj[i], 1e-10, 1e-300, `dunn p düzeltilmiş ${i}`);
    });
  }
  // dunn.test tek yönlü p raporlar (P(Z >= |z|)); iki yönlü = 2 katı
  const d2 = R.dunnTest2;
  const s = NP.dunn(R.dunn[0].gruplar, { duzeltme: 'bonferroni' });
  d2.karsilastirma.forEach((ad, i) => {
    const [a, b] = ad.split(' - ').map(Number);
    const c = s.karsilastirmalar.find((k) => k.grup1 === a - 1 && k.grup2 === b - 1);
    yakin(c.z, d2.z[i], 1e-12, 1e-14, `dunn.test z ${ad}`);
    yakin(c.p, 2 * d2.p[i], 1e-10, 1e-300, `dunn.test p ${ad}`);
  });
});

test('Friedman, Kendall W ve ikili karşılaştırmalar', () => {
  R.friedman.forEach((r, i) => {
    const ad = `friedman[${i}]`;
    const s = NP.friedman(r.matris);
    yakin(s.istatistik, r.chi, 1e-12, 1e-14, `${ad} χ²`);
    assert.equal(s.sd, r.sd);
    yakin(s.p, r.p, 1e-10, 1e-300, `${ad} p`);
    r.ortSira.forEach((o, j) => yakin(s.ortalamaSiralar[j], o, 1e-13, 1e-13, `${ad} ortalama sıra`));
    yakin(s.kendallW, r.W, 1e-12, 1e-14, `${ad} W`);
    const kw = NP.kendallW(r.matris);
    yakin(kw.W, r.W, 1e-12, 1e-14, `${ad} kendallW`);
    yakin(kw.p, r.p, 1e-10, 1e-300, `${ad} kendallW p`);

    const con = NP.friedmanPostHoc(r.matris, { yontem: 'conover', duzeltme: 'yok' });
    con.karsilastirmalar.forEach((c, j) => {
      yakin(c.t, r.conoverT[j], 1e-12, 1e-13, `${ad} Conover t`);
      yakin(c.p, r.conoverP[j], 1e-10, 1e-300, `${ad} Conover p`);
    });
    const conH = NP.friedmanPostHoc(r.matris, { yontem: 'conover', duzeltme: 'holm' });
    conH.karsilastirmalar.forEach((c, j) => yakin(c.pDuzeltilmis, r.conoverPHolm[j], 1e-10, 1e-300, `${ad} Conover Holm`));
    // R ptukey ~1e-7 doğrulukta (bkz. dagilim.test.js)
    const nem = NP.friedmanPostHoc(r.matris, { yontem: 'nemenyi' });
    nem.karsilastirmalar.forEach((c, j) => {
      yakin(c.q, r.nemenyiQ[j], 1e-12, 1e-13, `${ad} Nemenyi q`);
      yakin(c.p, r.nemenyiP[j], 0, 2e-7, `${ad} Nemenyi p`);
    });
    const sp = NP.friedmanPostHoc(r.matris, { yontem: 'spss' });
    sp.karsilastirmalar.forEach((c, j) => {
      yakin(c.z, r.spssZ[j], 1e-12, 1e-13, `${ad} SPSS z`);
      yakin(c.p, r.spssP[j], 1e-10, 1e-300, `${ad} SPSS p`);
      yakin(c.pDuzeltilmis, r.spssPBonf[j], 1e-10, 1e-300, `${ad} SPSS Bonferroni`);
    });
    const wil = NP.friedmanPostHoc(r.matris, { wilcoxonYontemi: 'asimptotik' });
    wil.karsilastirmalar.forEach((c, j) => {
      yakin(c.p, r.wilcoxP[j], 1e-10, 1e-300, `${ad} Wilcoxon p`);
      yakin(c.pDuzeltilmis, r.wilcoxPBonf[j], 1e-10, 1e-300, `${ad} Wilcoxon Bonferroni`);
    });
    // R 4.6 pairwise.wilcox.test varsayılanı (exact = NULL; n < 50 → kesin, sıfırlar Pratt)
    const wil46 = NP.friedmanPostHoc(r.matris, { wilcoxonYontemi: 'kesin', sifirYontemi: 'pratt', duzeltme: 'yok' });
    wil46.karsilastirmalar.forEach((c, j) => yakin(c.p, r.wilcoxPR46[j], 1e-10, 1e-14, `${ad} Wilcoxon R 4.6 varsayılanı`));
  });
});

test('Jonckheere-Terpstra: elle hesap, kesin dağılım ve clinfun', () => {
  R.jt.forEach((r, i) => {
    const ad = `jt[${i}]`;
    const as = NP.jonckheereTerpstra(r.gruplar, { yontem: 'asimptotik' });
    yakin(as.istatistik, r.elle.J, 1e-14, 1e-12, `${ad} J`);
    yakin(as.ortalama, r.elle.ort, 1e-14, 1e-12, `${ad} ortalama`);
    yakin(as.standartSapma ** 2, r.elle.var, 1e-12, 1e-12, `${ad} varyans`);
    yakin(as.z, r.elle.z, 1e-12, 1e-14, `${ad} z`);
    yakin(as.p, r.elle.p, 1e-10, 1e-300, `${ad} p`);
    yakin(NP.jonckheereTerpstra(r.gruplar, { yontem: 'asimptotik', alternatif: 'artan' }).p, r.elle.pArtan, 1e-10, 1e-300, `${ad} artan`);
    yakin(NP.jonckheereTerpstra(r.gruplar, { yontem: 'asimptotik', alternatif: 'azalan' }).p, r.elle.pAzalan, 1e-10, 1e-300, `${ad} azalan`);
    if (r.kesin) {
      const k = NP.jonckheereTerpstra(r.gruplar);
      assert.equal(k.pYontemi, 'kesin');
      yakin(k.p, r.kesin.iki, 1e-10, 1e-300, `${ad} kesin`);
      yakin(NP.jonckheereTerpstra(r.gruplar, { alternatif: 'artan' }).p, r.kesin.artan, 1e-10, 1e-300, `${ad} kesin artan`);
      yakin(NP.jonckheereTerpstra(r.gruplar, { alternatif: 'azalan' }).p, r.kesin.azalan, 1e-10, 1e-300, `${ad} kesin azalan`);
    }
    // kaba kuvvet (tüm atamalar) ile R evrişimi aynı → evrişim yönteminin bağımsız denetimi
    if (r.kesinEvrisim) yakin(r.kesinEvrisim.iki, r.kesin.iki, 1e-12, 1e-300, `${ad} R evrişim = kaba kuvvet`);
    if (r.iki) {
      const k = NP.jonckheereTerpstra(r.gruplar);
      assert.equal(k.pYontemi, 'asimptotik'); // N > 100
      yakin(k.istatistik, r.iki.J, 1e-14, 1e-12, `${ad} clinfun J`);
      yakin(k.p, r.iki.p, 1e-10, 1e-300, `${ad} clinfun p`);
      yakin(NP.jonckheereTerpstra(r.gruplar, { alternatif: 'artan' }).p, r.artan.p, 1e-10, 1e-300, `${ad} clinfun artan`);
    }
  });
});

test('Medyan testi', () => {
  R.medyan.forEach((r, i) => {
    const s = NP.medyanTesti(r.gruplar);
    yakin(s.medyan, r.medyan, 1e-14, 1e-14, `medyan[${i}]`);
    assert.deepEqual(s.tablo, r.tablo);
    yakin(s.kiKare.istatistik, r.chi, 1e-12, 1e-14, `medyan[${i}] χ²`);
    yakin(s.kiKare.p, r.p, 1e-10, 1e-300, `medyan[${i}] p`);
    if (r.yates !== null && r.yates !== undefined) yakin(s.yates.istatistik, r.yates, 1e-12, 1e-14, `medyan[${i}] Yates`);
    if (r.minBeklenen < 5) {
      assert.equal(s.pYontemi, 'kesin');
      yakin(s.p, r.fisherP, 1e-10, 1e-300, `medyan[${i}] Fisher`);
    } else yakin(s.p, r.p, 1e-10, 1e-300, `medyan[${i}] ana p`);
    yakin(NP.medyanTesti(r.gruplar, { fisher: true }).fisher.p, r.fisherP, 1e-10, 1e-300, `medyan[${i}] Fisher (zorla)`);
  });
});

test('Küçük örneklem ve uç durumlar', () => {
  // n1 = n2 = 1: kesin p = 1; R 4.6 kuralıyla güven aralığı sonsuz
  const s = NP.mannWhitneyU([1], [2]);
  assert.equal(s.pYontemi, 'kesin');
  assert.equal(s.p, 1);
  assert.equal(s.ga.alt, -Infinity);
  assert.equal(s.ga.ust, Infinity);
  // tek örneklem, tek gözlem
  const w = NP.wilcoxonIsaretliSira([3], null);
  assert.equal(w.p, 1);
  // sıfır farklar raporlanır
  const w2 = NP.wilcoxonIsaretliSira([1, 2, 3, 4, 5], [1, 1, 1, 1, 1]);
  assert.equal(w2.sifirFark, 1);
  assert.equal(w2.nSifirsiz, 4);
  assert.ok(w2.uyarilar.length > 0);
  // kesin bağlı p ile bağsız p, bağ yokken aynı
  const a = [1.1, 2.3, 0.4, 3.9, 2.8];
  const b = [0.2, 0.9, 1.7, 5.1, 4.4, 3.3];
  yakin(NP.mannWhitneyU(a, b, { yontem: 'kesin', kesinIkiYonlu: 'sapma' }).p, NP.mannWhitneyU(a, b).p, 1e-12, 0, 'bağsız kesin');
  // Kruskal: boş grup atılır
  const k = NP.kruskalWallis([[1, 2, 3], [], [4, 5, 6]]);
  assert.equal(k.k, 2);
  assert.ok(k.uyarilar.length > 0);
  // Friedman k = 2
  const f = NP.friedman([[1, 2], [3, 5], [2, 1], [4, 6]]);
  assert.equal(f.sd, 1);
});

test('Hata durumları', () => {
  const hata = (f, re) => assert.throws(f, (e) => e instanceof IstatistikHatasi && (!re || re.test(e.message)));
  hata(() => NP.mannWhitneyU([], [1, 2]), /en az 1 gözlem/);
  hata(() => NP.mannWhitneyU([1, NaN], [1, 2]), /sonlu sayı/);
  hata(() => NP.mannWhitneyU([2, 2], [2, 2, 2], { yontem: 'asimptotik' }), /aynı değere/);
  hata(() => NP.mannWhitneyU([1, 2], [3, 4], { yontem: 'xyz' }));
  hata(() => NP.mannWhitneyU([1, 2], [3, 4], { alternatif: 'yukari' }), /alternatif/);
  hata(() => NP.mannWhitneyU([1, 2], [3, 4], { kesinIkiYonlu: 'xyz' }));
  hata(() => NP.wilcoxonIsaretliSira([1, 2], [1, 2]), /Tüm farklar sıfır/);
  hata(() => NP.wilcoxonIsaretliSira([1, 2, 3], [1, 2]), /uzunluk/);
  hata(() => NP.wilcoxonIsaretliSira([1, 2, 3], null, { sifirYontemi: 'xyz' }));
  hata(() => NP.isaretTesti([1, 2], [1, 2]), /sıfır/);
  hata(() => NP.kruskalWallis([[1, 2, 3]]), /en az 2 grup/);
  hata(() => NP.kruskalWallis([[5, 5], [5, 5, 5]]), /aynı değere/);
  hata(() => NP.dunn([[1], []]), /en az 2 grup/);
  hata(() => NP.friedman([[1, 1, 1], [2, 2, 2]]), /tüm ölçümler eşit/);
  hata(() => NP.friedman([[1, 2], [1]]), /ölçüm sayısı/);
  hata(() => NP.friedman([[1], [2]]), /en az 2 ölçüm/);
  hata(() => NP.friedmanPostHoc([[1, 2, 3], [2, 1, 3]], { yontem: 'xyz' }), /Bilinmeyen/);
  hata(() => NP.jonckheereTerpstra([[1, 1], [1, 1]]), /aynı değere/);
  hata(() => NP.medyanTesti([[1, 1], [1, 1]]), /aynı tarafında/);
});
