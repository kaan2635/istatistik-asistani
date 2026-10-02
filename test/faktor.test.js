import { test } from 'node:test';
import assert from 'node:assert/strict';
import * as F from '../src/cekirdek/faktor.js';
import { simetrikOzdeger, carp, transpoz } from '../src/cekirdek/matris.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, veriDosyasi, yakin, yakinDerin } from './yardimci.js';

const R = referans('faktor');
const csv = (ad) =>
  veriDosyasi(ad)
    .trim()
    .split(/\r?\n/)
    .slice(1)
    .map((s) => s.split(',').map(Number));
const bfi = csv('fa-bfi.csv');
const hs = csv('fa-holzinger.csv');

// Döndürme toleransları: JS gradyan normu < 1e-10, R (GPArotation + parlatma) < 1e-13; ölçülen fark ~1e-11.
// ML tabanlı döndürmelerde mutlak tolerans 2e-8: factanal tekliklerinin kendi duyarlığı ~1e-8.
const DON = 1e-8;
const DON_M = 5e-10;

function matrisYakin(gercek, beklenen, goreli, mutlak, mesaj) {
  yakinDerin(gercek, beklenen, goreli, mutlak, mesaj);
}

function donmeKarsilastir(sonuc, ref, mesaj, gor = DON, mut = DON_M) {
  matrisYakin(sonuc.yukler, ref.P, gor, mut, `${mesaj} örüntü`);
  matrisYakin(sonuc.faktorKorelasyonlari, ref.Phi, gor, mut, `${mesaj} Φ`);
  matrisYakin(sonuc.yapiMatrisi, ref.S, gor, mut, `${mesaj} yapı`);
  yakinDerin(
    sonuc.donmeKareToplamlari.map((d) => d.toplam),
    ref.ss,
    gor,
    mut,
    `${mesaj} döndürme kareler toplamı`,
  );
  if (ref.M) matrisYakin(sonuc.donusumMatrisi, ref.M, gor, mut, `${mesaj} dönüşüm matrisi`);
  // GPArotation'ın kendi sonucu (eps = 1e-6, parlatılmamış): gevşek
  if (ref.gpa) matrisYakin(sonuc.yukler, ref.gpa, 0, 2e-5, `${mesaj} GPArotation (eps 1e-6)`);
  if (ref.s !== undefined) assert.ok(ref.s < 1e-10, `${mesaj}: R parlatması yakınsamadı (${ref.s})`);
}

function icTutarlilik(s, mesaj) {
  // yukler = A · M ; yapı = örüntü · Φ ; üretilen korelasyon döndürmeden bağımsız
  const AM = carp(s.yuklerDondurulmemis, s.donusumMatrisi);
  matrisYakin(AM, s.yukler, 1e-9, 1e-11, `${mesaj} A·M`);
  matrisYakin(carp(s.yukler, s.faktorKorelasyonlari), s.yapiMatrisi, 1e-12, 1e-13, `${mesaj} PΦ`);
  const PPt = carp(carp(s.yukler, s.faktorKorelasyonlari), transpoz(s.yukler));
  matrisYakin(PPt, s.uretilenKorelasyon, 1e-9, 1e-11, `${mesaj} PΦP' = AA'`);
}

function kmoKarsilastir(k, ref, mesaj) {
  yakin(k.kmo, ref.kmo, 1e-11, 1e-14, `${mesaj} KMO`);
  yakinDerin(k.msa, ref.msa, 1e-11, 1e-14, `${mesaj} MSA`);
  matrisYakin(k.antiImajKovaryans, ref.aiv, 1e-10, 1e-13, `${mesaj} anti-imaj kovaryans`);
  const p = ref.msa.length;
  for (let i = 0; i < p; i++) {
    for (let j = 0; j < p; j++) {
      if (i !== j) yakin(k.antiImajKorelasyon[i][j], ref.aic_dis[i][j], 1e-10, 1e-13, `${mesaj} anti-imaj korelasyon [${i}][${j}]`);
      else yakin(k.antiImajKorelasyon[i][i], ref.msa[i], 1e-11, 1e-14, `${mesaj} anti-imaj köşegen`);
    }
  }
  yakin(k.determinant, ref.det, 1e-9, 1e-300, `${mesaj} determinant`);
  yakin(k.istatistik, ref.chi, 1e-10, 1e-12, `${mesaj} Bartlett χ²`);
  assert.equal(k.sd, ref.sd);
  yakin(k.p, ref.p, 1e-9, 1e-300, `${mesaj} Bartlett p`);
}

test('özdeğer ayrıştırması Jacobi ile ve R ile aynı', () => {
  const ozd = F.ozdegerAyristir(R.harman.R);
  yakinDerin(ozd.degerler, R.harman.ozdeger, 1e-12, 1e-14, 'Harman özdeğerler');
  const j = simetrikOzdeger(R.harman.R);
  yakinDerin(ozd.degerler, j.degerler, 1e-12, 1e-14, 'Jacobi');
  // A v = λ v ve dikgenlik
  const A = R.harman.R;
  const p = A.length;
  for (let k = 0; k < p; k++) {
    for (let i = 0; i < p; i++) {
      let t = 0;
      for (let l = 0; l < p; l++) t += A[i][l] * ozd.vektorler[l][k];
      yakin(t, ozd.degerler[k] * ozd.vektorler[i][k], 1e-10, 1e-12, 'Av = λv');
    }
  }
  // yalnızca değerler
  yakinDerin(F.ozdegerAyristir(A, false).degerler, R.harman.ozdeger, 1e-12, 1e-14, 'yalnız değerler');
  // tekil ve köşegen matrisler
  yakinDerin(F.ozdegerAyristir([[2, 0], [0, 3]]).degerler, [3, 2], 0, 0, 'köşegen');
  yakinDerin(F.ozdegerAyristir([[1, 1], [1, 1]]).degerler, [2, 0], 1e-15, 1e-15, 'tekil');
  yakinDerin(F.ozdegerAyristir([[5]]).degerler, [5], 0, 0, '1×1');
});

test('KMO ve Bartlett (psych::KMO, cortest.bartlett)', () => {
  kmoKarsilastir(F.kmoBartlett(bfi), R.bfi.kmo, 'bfi');
  kmoKarsilastir(F.kmoBartlett({ korelasyon: R.harman.R, n: R.harman.n }), R.harman.kmo, 'Harman');
  kmoKarsilastir(F.kmoBartlett({ korelasyon: R.thurstone.R, n: R.thurstone.n }), R.thurstone.kmo, 'Thurstone');
  const k = F.kmoBartlett(R.mtcars.veri);
  kmoKarsilastir(k, R.mtcars.kmo, 'mtcars');
  const esikler = [[0.9, 'mükemmel'], [0.8, 'çok iyi'], [0.7, 'iyi'], [0.6, 'orta'], [0.5, 'zayıf'], [-1, 'kabul edilemez']];
  assert.equal(k.kmoYorum, esikler.find(([e]) => R.mtcars.kmo.kmo >= e)[1]);
  assert.equal(F.kmoBartlett(bfi).kmoYorum, 'çok iyi'); // KMO = 0.85
  // {veri} biçimi de kabul edilir
  yakin(F.kmoBartlett({ veri: R.mtcars.veri }).kmo, R.mtcars.kmo.kmo, 1e-11, 0, '{veri}');
  // n verilmezse Bartlett hesaplanmaz
  const kn = F.kmoBartlett({ korelasyon: R.harman.R });
  assert.equal(kn.istatistik, null);
  assert.ok(kn.uyarilar.some((u) => u.includes('Bartlett')));
});

test('temel bileşenler: bfi, Kaiser ölçütü, varimax', () => {
  const s = F.faktorAnalizi(bfi, { cikarim: 'tbt', faktorSayisi: 'kaiser', dondurme: 'varimax' });
  const r = R.bfi.pca;
  assert.equal(s.faktorSayisi, r.m);
  assert.equal(s.faktorSayisiYontemi, 'kaiser');
  yakinDerin(s.ozdegerler, R.bfi.ozdeger, 1e-11, 1e-14, 'özdeğerler');
  matrisYakin(s.yuklerDondurulmemis, r.A, 1e-9, 1e-12, 'bileşen matrisi');
  yakinDerin(s.ortakliklar.cikarim, r.h2, 1e-10, 1e-13, 'ortak varyans');
  assert.ok(s.ortakliklar.baslangic.every((v) => v === 1));
  donmeKarsilastir(s, r.varimax, 'varimax');
  // psych::principal varimax = stats::varimax(eps = 1e-5). Bu durma ölçütü (ölçütün göreli artışı < 1e-5) yükleri
  // ancak ~1e-2 doğrulukla verir (bu veride en büyük fark 5,2e-3); bizim sonucumuz sıkı yakınsatılmış GPArotation
  // ile 1e-7 içinde aynıdır. psych'in sonucunun stats::varimax varsayılanıyla aynı olduğu ayrıca doğrulanır.
  matrisYakin(r.psych_varimax.P, r.stats_varimax_varsayilan.P, 0, 1e-10, 'psych::principal = stats::varimax varsayılan');
  matrisYakin(s.yukler, r.psych_varimax.P, 0, 1e-2, 'psych::principal varimax');
  // tablolar
  const tv = s.toplamVaryans;
  assert.equal(tv.length, 25);
  yakin(tv[0].varyansYuzdesi, (R.bfi.ozdeger[0] / 25) * 100, 1e-11, 0, '% varyans');
  yakin(tv[24].birikimliYuzde, 100, 1e-12, 0, 'birikimli %100');
  yakinDerin(s.cikarimKareToplamlari.map((d) => d.toplam), R.bfi.ozdeger.slice(0, r.m), 1e-10, 0, 'çıkarım KT = özdeğer');
  yakin(
    s.donmeKareToplamlari.reduce((a, d) => a + d.toplam, 0),
    s.cikarimKareToplamlari.reduce((a, d) => a + d.toplam, 0),
    1e-11,
    0,
    'dik döndürme toplam varyansı korur',
  );
  assert.equal(s.artiklar.sayi, r.artik.sayi);
  yakin(s.artiklar.yuzde, r.artik.yuzde, 1e-12, 0, 'artık yüzdesi');
  yakin(s.artiklar.rmsr, r.artik.rmsr, 1e-9, 0, 'RMSR');
  icTutarlilik(s, 'TBT varimax');
  // döndürülmüş faktörler kareler toplamına göre sıralı, sütun toplamları pozitif
  const ss = s.donmeKareToplamlari.map((d) => d.toplam);
  for (let j = 1; j < ss.length; j++) assert.ok(ss[j - 1] >= ss[j]);
  for (let j = 0; j < s.faktorSayisi; j++) assert.ok(s.yukler.reduce((a, row) => a + row[j], 0) > 0);
});

test('temel eksen (SPSS PAF, 25 iterasyon, 0,001) ve tüm döndürmeler: bfi', () => {
  const r = R.bfi.paf;
  const temel = { cikarim: 'ana-eksen', faktorSayisi: 5 };
  const s = F.faktorAnalizi(bfi, { ...temel, dondurme: 'yok' });
  matrisYakin(s.yuklerDondurulmemis, r.A, 1e-9, 1e-11, 'faktör matrisi');
  yakinDerin(s.ortakliklar.baslangic, r.smc, 1e-11, 1e-14, 'başlangıç ortak varyansları (SMC)');
  yakinDerin(s.ortakliklar.cikarim, r.h2, 1e-9, 1e-12, 'çıkarım ortak varyansları');
  assert.equal(s.yakinsama.cikarim.iterasyon, r.it);
  assert.equal(s.yakinsama.cikarim.yakinsadi, true);
  yakin(s.yakinsama.cikarim.sonDegisim, r.fark, 1e-6, 1e-12, 'son değişim');
  assert.equal(s.artiklar.sayi, r.artik.sayi);
  yakin(s.artiklar.rmsr, r.artik.rmsr, 1e-8, 0, 'RMSR');
  assert.equal(s.donmeKareToplamlari, null);
  assert.deepEqual(s.yukler, s.yuklerDondurulmemis);
  for (const yontem of ['varimax', 'quartimax', 'equamax', 'oblimin', 'promax']) {
    const d = F.faktorAnalizi(bfi, { ...temel, dondurme: yontem });
    donmeKarsilastir(d, r[yontem], yontem);
    icTutarlilik(d, yontem);
    assert.equal(d.egik, yontem === 'oblimin' || yontem === 'promax');
    assert.equal(d.yakinsama.dondurme.yakinsadi, true);
  }
  // varimax: GPA ve stats::varimax (SVD algoritması, eps = 1e-10, en çok 1000 döngü) aynı en iyi noktaya ulaşır;
  // stats::varimax'ın ölçüt temelli durma kuralı ~1e-6 doğruluk verir
  matrisYakin(F.faktorAnalizi(bfi, { ...temel, dondurme: 'varimax' }).yukler, r.varimax_stats.P, 0, 1e-5, 'stats::varimax');
  // promax R türü (stats::promax / psych::Promax)
  const pr = F.faktorAnalizi(bfi, { ...temel, dondurme: 'promax', promaxTuru: 'r' });
  donmeKarsilastir(pr, r.promax_r, 'promax (R türü)');
  // stats::promax içindeki varimax eps = 1e-5 ile durur (yukarıdaki not): gevşek tolerans
  matrisYakin(pr.yukler, r.promax_stats.P, 0, 5e-3, 'stats::promax (varimax eps 1e-5)');
  // Kaiser normalizasyonu olmadan
  donmeKarsilastir(F.faktorAnalizi(bfi, { ...temel, dondurme: 'varimax', kaiserNormalizasyonu: false }), r.varimax_normsuz, 'varimax normalizasyonsuz');
  donmeKarsilastir(F.faktorAnalizi(bfi, { ...temel, dondurme: 'oblimin', kaiserNormalizasyonu: false }), r.oblimin_normsuz, 'oblimin normalizasyonsuz');
  // eğik döndürmede kareler toplamı yüzdeleri verilmez (SPSS dipnotu)
  const ob = F.faktorAnalizi(bfi, { ...temel, dondurme: 'oblimin' });
  assert.equal(ob.donmeKareToplamlari[0].varyansYuzdesi, null);
});

test('temel eksen tam yakınsama psych::fa(fm = "pa") ile aynı', () => {
  const s = F.faktorAnalizi(bfi, { cikarim: 'ana-eksen', faktorSayisi: 5, dondurme: 'yok', yakinsamaOlcutu: 1e-13, maksIterasyon: 100000 });
  matrisYakin(s.yuklerDondurulmemis, R.bfi.paf_tam.A, 1e-8, 1e-10, 'tam yakınsama (R döngüsü)');
  matrisYakin(s.yuklerDondurulmemis, R.bfi.paf_tam.psych, 1e-6, 1e-8, 'psych::fa pa');
  const t = F.faktorAnalizi({ korelasyon: R.thurstone.R, n: 213 }, { cikarim: 'ana-eksen', faktorSayisi: 3, dondurme: 'yok', yakinsamaOlcutu: 1e-13, maksIterasyon: 100000 });
  matrisYakin(t.yuklerDondurulmemis, R.thurstone.paf_tam.A, 1e-8, 1e-10, 'Thurstone tam');
  matrisYakin(t.yuklerDondurulmemis, R.thurstone.paf_tam.psych, 1e-6, 1e-8, 'Thurstone psych');
});

test('en çok olabilirlik (stats::factanal) ve döndürmeler: bfi', () => {
  const r = R.bfi.ml;
  const s = F.faktorAnalizi(bfi, { cikarim: 'ml', faktorSayisi: 5, dondurme: 'yok' });
  assert.equal(s.yakinsama.cikarim.yakinsadi, true);
  yakinDerin(s.teklikler, r.psi, 1e-6, 1e-8, 'teklikler');
  matrisYakin(s.yuklerDondurulmemis, r.A, 1e-6, 1e-8, 'ML yükleri');
  yakin(s.uyum.amacFonksiyonu, r.amac, 1e-8, 1e-12, 'amaç fonksiyonu');
  yakin(s.uyum.istatistik, r.chi, 1e-8, 1e-10, 'χ²');
  assert.equal(s.uyum.sd, r.sd);
  yakin(s.uyum.p, r.p, 1e-6, 1e-300, 'p');
  yakinDerin(s.ortakliklar.baslangic, R.bfi.paf.smc, 1e-11, 0, 'başlangıç = SMC');
  donmeKarsilastir(F.faktorAnalizi(bfi, { cikarim: 'ml', faktorSayisi: 5, dondurme: 'varimax' }), r.varimax, 'ML varimax', 1e-7, 2e-8);
  donmeKarsilastir(F.faktorAnalizi(bfi, { cikarim: 'ml', faktorSayisi: 5, dondurme: 'oblimin' }), r.oblimin, 'ML oblimin', 1e-7, 2e-8);
});

test('faktör puanları (psych::factor.scores): regresyon, Bartlett, Anderson-Rubin', () => {
  const temel = { cikarim: 'ana-eksen', faktorSayisi: 5 };
  const v = F.faktorAnalizi(bfi, { ...temel, dondurme: 'varimax' });
  const o = F.faktorAnalizi(bfi, { ...temel, dondurme: 'oblimin' });
  const kontrol = (sonuc, yontem, ref, mesaj) => {
    const fp = F.faktorPuanlari(bfi, sonuc, { yontem });
    matrisYakin(fp.katsayilar, ref.W, 1e-7, 1e-9, `${mesaj} katsayılar`);
    matrisYakin(fp.puanlar.slice(0, 8), ref.ilk, 1e-7, 1e-9, `${mesaj} ilk puanlar`);
    yakinDerin(fp.puanlar[fp.puanlar.length - 1], ref.son, 1e-7, 1e-9, `${mesaj} son puan`);
    return fp;
  };
  const reg = kontrol(v, 'regresyon', R.bfi.puan.varimax_thurstone, 'varimax regresyon');
  assert.equal(reg.yontem, 'regresyon');
  kontrol(v, 'bartlett', R.bfi.puan.varimax_bartlett, 'varimax Bartlett');
  kontrol(v, 'anderson-rubin', R.bfi.puan.varimax_anderson, 'varimax Anderson-Rubin');
  kontrol(o, 'regresyon', R.bfi.puan.oblimin_thurstone, 'oblimin regresyon');
  kontrol(o, 'bartlett', R.bfi.puan.oblimin_bartlett, 'oblimin Bartlett');
  // varsayılan yöntem regresyon
  assert.equal(F.faktorPuanlari(bfi, v).yontem, 'regresyon');
  // Anderson-Rubin puanları ilişkisiz ve birim varyanslı
  const ar = F.faktorPuanlari(bfi, v, { yontem: 'anderson-rubin' }).puanlar;
  const n = ar.length;
  for (let a = 0; a < 5; a++) {
    for (let b = 0; b <= a; b++) {
      let t = 0;
      for (const row of ar) t += row[a] * row[b];
      yakin(t / (n - 1), a === b ? 1 : 0, 1e-9, 1e-9, 'AR kovaryansı');
    }
  }
});

test('temel bileşen puanları: mtcars (küçük örneklem)', () => {
  const r = R.mtcars.pca;
  const s = F.faktorAnalizi(R.mtcars.veri, { cikarim: 'tbt', faktorSayisi: 2, dondurme: 'varimax' });
  yakinDerin(s.ozdegerler, R.mtcars.ozdeger, 1e-11, 1e-14, 'özdeğerler');
  matrisYakin(s.yuklerDondurulmemis, r.A, 1e-9, 1e-12, 'bileşen matrisi');
  matrisYakin(s.yuklerDondurulmemis, r.psych, 1e-9, 1e-12, 'psych::principal');
  donmeKarsilastir(s, r.varimax, 'mtcars varimax');
  const fp = F.faktorPuanlari(R.mtcars.veri, s);
  matrisYakin(fp.katsayilar, r.W, 1e-7, 1e-9, 'bileşen puan katsayıları');
  matrisYakin(fp.puanlar, r.puan, 1e-7, 1e-9, 'bileşen puanları');
  const ham = F.faktorAnalizi(R.mtcars.veri, { cikarim: 'tbt', faktorSayisi: 2, dondurme: 'yok' });
  matrisYakin(F.faktorPuanlari(R.mtcars.veri, ham).katsayilar, r.W_ham, 1e-9, 1e-12, 'döndürülmemiş katsayılar');
  assert.equal(s.artiklar.sayi, r.artik.sayi);
  // PAF küçük örneklem
  const paf = F.faktorAnalizi(R.mtcars.veri, { cikarim: 'ana-eksen', faktorSayisi: 2, dondurme: 'yok' });
  assert.equal(paf.yakinsama.cikarim.iterasyon, R.mtcars.paf.it);
  assert.equal(paf.yakinsama.cikarim.yakinsadi, R.mtcars.paf.yakinsadi);
  matrisYakin(paf.yuklerDondurulmemis, R.mtcars.paf.A, 1e-8, 1e-10, 'mtcars PAF');
});

test('Harman74: korelasyon matrisi girdisi, PAF, ML, döndürmeler', () => {
  const girdi = { korelasyon: R.harman.R, n: R.harman.n };
  const r = R.harman;
  const s = F.faktorAnalizi(girdi, { cikarim: 'ana-eksen', faktorSayisi: 4, dondurme: 'yok' });
  assert.equal(s.yakinsama.cikarim.iterasyon, r.paf.it);
  assert.equal(s.yakinsama.cikarim.yakinsadi, r.paf.yakinsadi);
  matrisYakin(s.yuklerDondurulmemis, r.paf.A, 1e-8, 1e-10, 'PAF');
  yakinDerin(s.ortakliklar.cikarim, r.paf.h2, 1e-8, 1e-10, 'PAF h²');
  assert.equal(s.artiklar.sayi, r.paf.artik.sayi);
  if (!r.paf.yakinsadi) assert.ok(s.uyarilar.some((u) => u.includes('iterasyon')));
  const s50 = F.faktorAnalizi(girdi, { cikarim: 'ana-eksen', faktorSayisi: 4, dondurme: 'yok', maksIterasyon: 50 });
  assert.equal(s50.yakinsama.cikarim.iterasyon, r.paf50.it);
  matrisYakin(s50.yuklerDondurulmemis, r.paf50.A, 1e-8, 1e-10, 'PAF 50 iterasyon');
  for (const y of ['varimax', 'promax', 'oblimin']) {
    donmeKarsilastir(F.faktorAnalizi(girdi, { cikarim: 'ana-eksen', faktorSayisi: 4, dondurme: y }), r.paf[y], `Harman PAF ${y}`);
  }
  const ml = F.faktorAnalizi(girdi, { cikarim: 'ml', faktorSayisi: 4, dondurme: 'yok' });
  yakinDerin(ml.teklikler, r.ml.psi, 1e-6, 1e-8, 'ML teklikler');
  matrisYakin(ml.yuklerDondurulmemis, r.ml.A, 1e-6, 1e-8, 'ML yükler');
  yakin(ml.uyum.istatistik, r.ml.chi, 1e-8, 0, 'ML χ²');
  assert.equal(ml.uyum.sd, r.ml.sd);
  yakin(ml.uyum.p, r.ml.p, 1e-7, 0, 'ML p');
  assert.equal(ml.artiklar.sayi, r.ml.artik.sayi);
  for (const y of ['varimax', 'promax', 'oblimin']) {
    donmeKarsilastir(F.faktorAnalizi(girdi, { cikarim: 'ml', faktorSayisi: 4, dondurme: y }), r.ml[y], `Harman ML ${y}`, 1e-7, 2e-8);
  }
  // korelasyon girdisinden puan hesaplanamaz
  assert.throws(() => F.faktorPuanlari(null, ml), IstatistikHatasi);
});

test('Thurstone: PAF oblimin/promax/quartimax, ML promax/oblimin/equamax', () => {
  const girdi = { korelasyon: R.thurstone.R, n: R.thurstone.n };
  const r = R.thurstone;
  const s = F.faktorAnalizi(girdi, { cikarim: 'ana-eksen', faktorSayisi: 3, dondurme: 'oblimin' });
  assert.equal(s.yakinsama.cikarim.iterasyon, r.paf.it);
  matrisYakin(s.yuklerDondurulmemis, r.paf.A, 1e-8, 1e-10, 'PAF');
  donmeKarsilastir(s, r.paf.oblimin, 'PAF oblimin');
  donmeKarsilastir(F.faktorAnalizi(girdi, { cikarim: 'ana-eksen', faktorSayisi: 3, dondurme: 'promax' }), r.paf.promax, 'PAF promax');
  donmeKarsilastir(F.faktorAnalizi(girdi, { cikarim: 'ana-eksen', faktorSayisi: 3, dondurme: 'quartimax' }), r.paf.quartimax, 'PAF quartimax');
  const ml = F.faktorAnalizi(girdi, { cikarim: 'ml', faktorSayisi: 3, dondurme: 'promax' });
  yakinDerin(ml.teklikler, r.ml.psi, 1e-6, 1e-8, 'ML teklikler');
  yakin(ml.uyum.istatistik, r.ml.chi, 1e-7, 1e-10, 'ML χ²');
  donmeKarsilastir(ml, r.ml.promax, 'ML promax', 1e-7, 2e-8);
  donmeKarsilastir(F.faktorAnalizi(girdi, { cikarim: 'ml', faktorSayisi: 3, dondurme: 'oblimin' }), r.ml.oblimin, 'ML oblimin', 1e-7, 2e-8);
  donmeKarsilastir(F.faktorAnalizi(girdi, { cikarim: 'ml', faktorSayisi: 3, dondurme: 'equamax' }), r.ml.equamax, 'ML equamax', 1e-7, 2e-8);
});

test('Holzinger-Swineford: ML 3 faktör', () => {
  const r = R.holzinger.ml;
  const s = F.faktorAnalizi(hs, { cikarim: 'ml', faktorSayisi: 3, dondurme: 'oblimin' });
  yakinDerin(s.ozdegerler, R.holzinger.ozdeger, 1e-11, 1e-14, 'özdeğerler');
  yakinDerin(s.teklikler, r.psi, 1e-6, 1e-8, 'teklikler');
  yakin(s.uyum.istatistik, r.chi, 1e-7, 1e-10, 'χ²');
  assert.equal(s.uyum.sd, r.sd);
  yakin(s.uyum.p, r.p, 1e-6, 1e-12, 'p');
  donmeKarsilastir(s, r.oblimin, 'oblimin', 1e-7, 2e-8);
  donmeKarsilastir(F.faktorAnalizi(hs, { cikarim: 'ml', faktorSayisi: 3, dondurme: 'promax' }), r.promax, 'promax', 1e-7, 2e-8);
  donmeKarsilastir(F.faktorAnalizi(hs, { cikarim: 'ml', faktorSayisi: 3, dondurme: 'varimax' }), r.varimax, 'varimax', 1e-7, 2e-8);
});

test('MAP testi (psych::VSS map ve Velicer 2000 dördüncü kuvvet)', () => {
  for (const [ad, girdi] of [
    ['bfi', bfi],
    ['harman', { korelasyon: R.harman.R, n: 145 }],
    ['mtcars', R.mtcars.veri],
    ['holzinger', hs],
  ]) {
    const r = R[ad].map;
    const m = F.mapTesti(girdi);
    // m = p − 1'de artık matris rank 1'dir; kısmi korelasyonlar ±1 ve değer yuvarlamaya duyarlıdır
    const son = r.kare.length - 1;
    yakinDerin(m.kareOrtalamalari.slice(0, son), r.kare.slice(0, son), 1e-9, 1e-14, `${ad} MAP kare`);
    yakinDerin(m.dorduncuKuvvetOrtalamalari.slice(0, son), r.dort.slice(0, son), 1e-9, 1e-14, `${ad} MAP 4. kuvvet`);
    yakin(m.kareOrtalamalari[son], r.kare[son], 1e-6, 0, `${ad} MAP kare m = p − 1`);
    assert.equal(m.kareOrtalamalari.length, r.kare.length);
    yakinDerin(m.kareOrtalamalari.slice(1, 1 + r.psych_map.length), r.psych_map, 1e-9, 1e-14, `${ad} psych::VSS map`);
    const enk = (x) => x.reduce((b, v, i) => (v < x[b] ? i : b), 0);
    assert.equal(m.onerilenFaktorSayisi, enk(r.kare));
    assert.equal(m.onerilenFaktorSayisi4, enk(r.dort));
  }
  // faktör sayısı MAP ile
  const s = F.faktorAnalizi(bfi, { faktorSayisi: 'map', dondurme: 'yok' });
  assert.equal(s.faktorSayisiYontemi, 'map');
  assert.equal(s.faktorSayisi, Math.max(1, s.map.onerilenFaktorSayisi));
});

test('paralel analiz: dağılım R simülasyonuyla uyumlu, tohumla tekrarlanabilir', () => {
  const r = R.holzinger.paralel;
  const B = 4000;
  const pa = F.paralelAnaliz(hs, { tekrar: B, tohum: 7 });
  assert.equal(pa.uretim, 'wishart');
  const p = 9;
  for (let j = 0; j < p; j++) {
    // ortalamalar arasındaki fark, iki bağımsız Monte Carlo ortalamasının standart hatasının 4 katından küçük
    const se = Math.sqrt(r.sd[j] ** 2 / r.B + pa.standartSapma[j] ** 2 / B);
    assert.ok(Math.abs(pa.ortalama[j] - r.ort[j]) < 4 * se, `ortalama ${j}: ${pa.ortalama[j]} vs ${r.ort[j]}`);
    assert.ok(Math.abs(pa.standartSapma[j] / r.sd[j] - 1) < 0.1, `sd ${j}`);
    assert.ok(Math.abs(pa.yuzdelikDegerleri[j] - r.q95[j]) < 6 * se + 0.01, `yüzdelik ${j}`);
    // psych::fa.parallel (200 tekrar) ile gevşek uyum
    assert.ok(Math.abs(pa.ortalama[j] - r.psych_pc_sim[j]) < 0.03, `psych ${j}`);
  }
  assert.equal(pa.onerilenFaktorSayisi, pa.gercek.filter((g, j) => g > pa.yuzdelikDegerleri[j]).length);
  assert.equal(pa.onerilenFaktorSayisi, r.psych_ncomp);
  // doğrudan veri üretimi aynı dağılımı verir
  const pv = F.paralelAnaliz(hs, { tekrar: 1500, tohum: 3, uretim: 'veri' });
  assert.equal(pv.uretim, 'veri');
  for (let j = 0; j < p; j++) {
    const se = Math.sqrt(r.sd[j] ** 2 / r.B + pv.standartSapma[j] ** 2 / 1500);
    assert.ok(Math.abs(pv.ortalama[j] - r.ort[j]) < 4 * se, `veri üretimi ortalama ${j}`);
  }
  // temel eksen türü (indirgenmiş matris)
  const pp = F.paralelAnaliz(hs, { tekrar: 1000, tohum: 11, tur: 'ana-eksen' });
  for (let j = 0; j < p; j++) {
    const se = Math.sqrt(r.paf_sd[j] ** 2 / 1000 + pp.standartSapma[j] ** 2 / 1000);
    assert.ok(Math.abs(pp.ortalama[j] - r.paf_ort[j]) < 4 * se, `PAF paralel ortalama ${j}`);
  }
  // tekrarlanabilirlik
  const a = F.paralelAnaliz(hs, { tekrar: 50, tohum: 99 });
  const b = F.paralelAnaliz(hs, { tekrar: 50, tohum: 99 });
  const c = F.paralelAnaliz(hs, { tekrar: 50, tohum: 100 });
  assert.deepEqual(a.ortalama, b.ortalama);
  assert.notDeepEqual(a.ortalama, c.ortalama);
  // faktör sayısı paralel analizle
  const s = F.faktorAnalizi(hs, { cikarim: 'ml', faktorSayisi: 'paralel', paralel: { tekrar: 200, tohum: 1 } });
  assert.equal(s.faktorSayisiYontemi, 'paralel');
  assert.equal(s.faktorSayisi, Math.max(1, s.paralelAnaliz.onerilenFaktorSayisi));
  assert.equal(s.paralelAnaliz.tohum, 1);
});

test('uç durumlar ve hata mesajları', () => {
  // p > n: korelasyon matrisi tekil
  const kucuk = R.mtcars.veri.slice(0, 5);
  const k = F.kmoBartlett(kucuk);
  assert.equal(k.kmo, null);
  assert.equal(k.istatistik, null);
  assert.ok(k.uyarilar.length > 0);
  const tbt = F.faktorAnalizi(kucuk, { faktorSayisi: 2, dondurme: 'varimax' });
  assert.ok(tbt.uyarilar.some((u) => u.includes('tekil')));
  assert.ok(tbt.uyarilar.some((u) => u.includes('Gözlem sayısı')));
  assert.throws(() => F.faktorAnalizi(kucuk, { cikarim: 'ml', faktorSayisi: 1 }), IstatistikHatasi);
  // tekil R: bir değişken diğerlerinin doğrusal bileşimi
  const tekilVeri = R.mtcars.veri.map((s) => [...s, s[0] + 2 * s[3]]);
  const kt = F.kmoBartlett(tekilVeri);
  assert.equal(kt.kmo, null);
  assert.ok(kt.uyarilar[0].includes('tekil'));
  const paf = F.faktorAnalizi(tekilVeri, { cikarim: 'ana-eksen', faktorSayisi: 2 });
  assert.ok(paf.uyarilar.some((u) => u.includes('en büyük mutlak korelasyon')));
  // Heywood: tek faktörde λ1² = r12 r13 / r23 = 0.81 / 0.7 > 1
  const hw = { korelasyon: [[1, 0.9, 0.9], [0.9, 1, 0.7], [0.9, 0.7, 1]], n: 100 };
  const ph = F.faktorAnalizi(hw, { cikarim: 'ana-eksen', faktorSayisi: 1 });
  assert.equal(ph.heywood, true);
  assert.ok(ph.uyarilar.some((u) => u.includes('Heywood')));
  assert.ok(ph.uyarilar.some((u) => u.includes('döndürülemez')));
  const mh = F.faktorAnalizi(hw, { cikarim: 'ml', faktorSayisi: 1 });
  assert.equal(mh.heywood, true);
  assert.ok(mh.teklikler[0] < 0.01, "ML: ortak varyans 1'e ulaşır");
  // sabit değişken, eksik değer, geçersiz seçenekler
  assert.throws(() => F.faktorAnalizi(R.mtcars.veri.map((s) => [...s, 1])), /varyansı sıfır/);
  assert.throws(() => F.faktorAnalizi([[1, 2], [NaN, 3], [2, 2], [3, 1]]), /eksik/);
  assert.throws(() => F.faktorAnalizi([[1, 2], [2, 3]]), /en az 3 gözlem/);
  assert.throws(() => F.faktorAnalizi([[1], [2], [3]]), /iki değişken/);
  assert.throws(() => F.faktorAnalizi(bfi, { cikarim: 'xyz' }), IstatistikHatasi);
  assert.throws(() => F.faktorAnalizi(bfi, { dondurme: 'xyz' }), IstatistikHatasi);
  assert.throws(() => F.faktorAnalizi(bfi, { faktorSayisi: 0 }), IstatistikHatasi);
  assert.throws(() => F.faktorAnalizi(bfi, { cikarim: 'ana-eksen', faktorSayisi: 25 }), /en fazla 24/);
  assert.throws(() => F.faktorAnalizi(hs, { cikarim: 'ml', faktorSayisi: 6 }), /serbestlik/);
  assert.throws(() => F.faktorAnalizi({ korelasyon: [[1, 0.5], [0.4, 1]], n: 10 }), /simetrik/);
  assert.throws(() => F.faktorAnalizi({ korelasyon: [[1, 1.5], [1.5, 1]], n: 10 }), /1'den büyük/);
  assert.throws(() => F.faktorAnalizi('veri'), IstatistikHatasi);
  assert.throws(() => F.paralelAnaliz({ korelasyon: R.harman.R }), /örneklem/);
  // PAF iterasyon sınırı aşılınca uyarı
  const az = F.faktorAnalizi(bfi, { cikarim: 'ana-eksen', faktorSayisi: 5, maksIterasyon: 2 });
  assert.equal(az.yakinsama.cikarim.yakinsadi, false);
  assert.ok(az.uyarilar.some((u) => u.includes('SPSS')));
  // girdi değiştirilmez
  const kopya = JSON.stringify(R.mtcars.veri);
  F.faktorAnalizi(R.mtcars.veri, { cikarim: 'ana-eksen', faktorSayisi: 2, dondurme: 'promax' });
  assert.equal(JSON.stringify(R.mtcars.veri), kopya);
});
