import { test } from 'node:test';
import assert from 'node:assert/strict';
import { dogrulayiciFaktorAnalizi } from '../src/cekirdek/dfa.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, veriDosyasi, yakin, yakinDerin } from './yardimci.js';

const R = referans('dfa');
const oku = (ad) => {
  const satirlar = veriDosyasi(ad).trim().split(/\r?\n/);
  const adlar = satirlar[0].split(',').map((s) => s.replace(/"/g, ''));
  return { adlar, veri: satirlar.slice(1).map((s) => s.split(',').map(Number)) };
};
const hs = oku('fa-holzinger.csv');
const bfi = oku('fa-bfi.csv');

const M3 = { visual: ['x1', 'x2', 'x3'], textual: ['x4', 'x5', 'x6'], speed: ['x7', 'x8', 'x9'] };
const MB = Object.fromEntries(['A', 'C', 'E', 'N', 'O'].map((f) => [f, [1, 2, 3, 4, 5].map((i) => `${f}${i}`)]));

// lavaan BFGS ile gradyan normu ~1e-10 (HS3) .. 1e-7 (diğerleri) düzeyine yakınsatıldı; HS3'te ölçülen fark ~3e-10.
const GOR = 1e-7;
const MUT = 1e-9;

/** lavaan parametre satırını sonuçtaki karşılığıyla eşleştirir */
function bul(s, satir) {
  const { lhs, op, rhs } = satir;
  if (op === '=~') return s.yukler.find((y) => y.faktor === lhs && y.degisken === rhs);
  const lv = s.faktorler.includes(lhs);
  if (lhs === rhs) return lv ? s.faktorVaryanslari.find((v) => v.faktor === lhs) : s.hataVaryanslari.find((v) => v.degisken === lhs);
  if (lv) return s.faktorKovaryanslari.find((c) => (c.faktor1 === lhs && c.faktor2 === rhs) || (c.faktor1 === rhs && c.faktor2 === lhs));
  return s.hataKovaryanslari.find((c) => (c.degisken1 === lhs && c.degisken2 === rhs) || (c.degisken1 === rhs && c.degisken2 === lhs));
}

function karsilastir(s, r, mesaj, gor = GOR, mut = MUT) {
  assert.equal(s.yakinsama.yakinsadi, true, `${mesaj}: yakınsama`);
  // ham kestirimler
  for (const satir of r.pe) {
    const b = bul(s, satir);
    const ad = `${mesaj} ${satir.lhs}${satir.op}${satir.rhs}`;
    assert.ok(b, `${ad}: bulunamadı`);
    yakin(b.tahmin, satir.est, gor, mut, `${ad} tahmin`);
    if (satir.se === 0) {
      assert.equal(b.sabit, true, `${ad} sabit`);
      assert.equal(b.SH, null);
      continue;
    }
    yakin(b.SH, satir.se, gor, mut, `${ad} SH`);
    yakin(b.z, satir.z, gor, mut, `${ad} z`);
    // lavaan p = 2(1 − Φ(|z|)): mutlak hata ~2e-16, |z| > 8,3 için 0; bizimki üst kuyruğu doğrudan hesaplar
    yakin(b.p, satir.pvalue, 1e-5, 1e-15, `${ad} p`);
  }
  // standartlaştırılmış çözüm (std.all) ve delta yöntemi standart hataları
  for (const satir of r.std) {
    const b = bul(s, satir);
    const ad = `${mesaj} std ${satir.lhs}${satir.op}${satir.rhs}`;
    yakin(b.std, satir['est.std'], gor, mut, `${ad} tahmin`);
    if (satir.se === 0) {
      assert.ok(b.stdSH === null || Math.abs(b.stdSH) < 1e-8, `${ad} SH sıfır`);
      continue;
    }
    yakin(b.stdSH, satir.se, 1e-5, 1e-8, `${ad} SH`);
    yakin(b.stdZ, satir.z, 1e-5, 1e-8, `${ad} z`);
    yakin(b.stdP, satir.pvalue, 1e-4, 1e-15, `${ad} p`);
  }
  // R² ve kovaryans matrisleri
  for (const [ad, deger] of Object.entries(r.r2)) yakin(s.R2.find((x) => x.degisken === ad).deger, deger, gor, mut, `${mesaj} R² ${ad}`);
  yakinDerin(s.ornekKovaryans, r.S, 1e-12, 1e-14, `${mesaj} S (N paydalı)`);
  yakinDerin(s.ortukKovaryans, r.implied, gor, mut, `${mesaj} Σ`);
  // uyum ölçütleri (fitMeasures)
  const u = s.uyum;
  const fm = r.fm;
  const esle = {
    kiKare: 'chisq', sd: 'df', p: 'pvalue', temelKiKare: 'baseline.chisq', temelSd: 'baseline.df', temelP: 'baseline.pvalue',
    CFI: 'cfi', TLI: 'tli', NNFI: 'nnfi', NFI: 'nfi', RFI: 'rfi', IFI: 'ifi', RNI: 'rni', PNFI: 'pnfi',
    RMSEA: 'rmsea', rmseaP: 'rmsea.pvalue', SRMR: 'srmr', GFI: 'gfi', AGFI: 'agfi', PGFI: 'pgfi',
    logL: 'logl', logLDoymus: 'unrestricted.logl', AIC: 'aic', BIC: 'bic', SABIC: 'bic2', ECVI: 'ecvi', parametreSayisi: 'npar',
  };
  for (const [bizim, lav] of Object.entries(esle)) {
    if (fm[lav] === null || fm[lav] === undefined) continue;
    yakin(u[bizim], fm[lav], bizim === 'p' || bizim === 'rmseaP' || bizim === 'temelP' ? 1e-5 : gor, bizim === 'p' || bizim === 'temelP' || bizim === 'rmseaP' ? 1e-15 : mut, `${mesaj} ${bizim}`);
  }
  yakin(u.amacFonksiyonu / 2, fm.fmin, gor, mut, `${mesaj} fmin = F/2`);
  assert.equal(s.istatistik, u.kiKare);
  // RMSEA %90 GA: yüksek duyarlıklı R çözümüyle sıkı, lavaan'ın (uniroot tol ~1e-4) değeriyle gevşek
  yakin(u.rmseaAlt, r.rmsea_ci[0], 1e-7, 1e-10, `${mesaj} RMSEA alt`);
  yakin(u.rmseaUst, r.rmsea_ci[1], 1e-7, 1e-10, `${mesaj} RMSEA üst`);
  yakin(u.rmseaAlt, fm['rmsea.ci.lower'], 1e-3, 1e-6, `${mesaj} lavaan RMSEA alt`);
  yakin(u.rmseaUst, fm['rmsea.ci.upper'], 1e-3, 1e-6, `${mesaj} lavaan RMSEA üst`);
}

function miKarsilastir(s, r, mesaj) {
  const tum = s.tumMI;
  assert.equal(tum.length, r.mi_sayi, `${mesaj}: aday parametre sayısı`);
  for (const satir of r.mi.slice(0, 10)) {
    const b = tum.find((m) => m.op === satir.op && ((m.sol === satir.lhs && m.sag === satir.rhs) || (m.op === '~~' && m.sol === satir.rhs && m.sag === satir.lhs)));
    const ad = `${mesaj} MI ${satir.lhs}${satir.op}${satir.rhs}`;
    assert.ok(b, `${ad}: bulunamadı`);
    yakin(b.mi, satir.mi, 1e-5, 1e-7, `${ad} mi`);
    yakin(b.bdd, satir.epc, 1e-5, 1e-7, `${ad} epc`);
    yakin(b.stdBdd, satir['sepc.all'], 1e-5, 1e-7, `${ad} sepc.all`);
  }
  // ilk 10, MI'ye göre azalan sırada
  const ilk = s.modifikasyonIndeksleri;
  assert.equal(ilk.length, Math.min(10, r.mi_sayi));
  for (let i = 1; i < ilk.length; i++) assert.ok(ilk[i - 1].mi >= ilk[i].mi);
  yakin(ilk[0].mi, r.mi[0].mi, 1e-5, 0, `${mesaj} en büyük MI`);
}

function calistir(secenek) {
  const s = dogrulayiciFaktorAnalizi(secenek);
  s.tumMI = dogrulayiciFaktorAnalizi({ ...secenek, modifikasyonSayisi: Infinity }).modifikasyonIndeksleri;
  return s;
}

test('Holzinger-Swineford 3 faktör (işaretçi değişken) = lavaan::cfa', () => {
  const s = calistir({ veri: hs.veri, degiskenAdlari: hs.adlar, model: M3 });
  assert.equal(s.olcekleme, 'isaretci');
  assert.equal(s.n, 301);
  assert.equal(s.sd, 24);
  karsilastir(s, R.hs3, 'HS3');
  miKarsilastir(s, R.hs3, 'HS3');
  // birleşik güvenirlik ve AOV
  for (const g of s.guvenirlik) {
    yakin(g.birlesikGuvenirlik, R.hs3.bg[g.faktor].cr, GOR, MUT, `BG ${g.faktor}`);
    yakin(g.ortalamaAciklananVaryans, R.hs3.bg[g.faktor].ave, GOR, MUT, `AOV ${g.faktor}`);
  }
  // faktör korelasyonları = standartlaştırılmış kovaryanslar
  for (const c of s.faktorKovaryanslari) {
    const f = s.faktorler.indexOf(c.faktor1);
    const g = s.faktorler.indexOf(c.faktor2);
    yakin(s.faktorKorelasyonlari[f][g], c.std, 1e-12, 0, 'faktör korelasyonu');
  }
  assert.ok(Math.abs(s.uyum.kiKareSd - s.uyum.kiKare / 24) < 1e-12);
  assert.deepEqual(s.uyarilar, []);
});

test('faktör varyansı ölçeklemesi (std.lv = TRUE)', () => {
  const s = calistir({ veri: hs.veri, degiskenAdlari: hs.adlar, model: M3, olcekleme: 'faktor-varyansi' });
  karsilastir(s, R.hs3lv, 'HS3 std.lv', 1e-5, 1e-7);
  assert.ok(s.faktorVaryanslari.every((v) => v.sabit && v.tahmin === 1));
  assert.ok(s.yukler.every((y) => !y.sabit));
});

test('ilişkili hatalar (x7 ~~ x8, x1 ~~ x9)', () => {
  const s = calistir({ veri: hs.veri, degiskenAdlari: hs.adlar, model: M3, iliskiliHatalar: [['x7', 'x8'], ['x9', 'x1']] });
  assert.equal(s.hataKovaryanslari.length, 2);
  karsilastir(s, R.hs3e, 'HS3 ilişkili hatalar', 1e-5, 1e-7);
  miKarsilastir(s, R.hs3e, 'HS3 ilişkili hatalar');
});

test('tek faktörlü model', () => {
  const s = calistir({ veri: hs.veri, degiskenAdlari: hs.adlar, model: { g: hs.adlar } });
  assert.equal(s.faktorKovaryanslari.length, 0);
  karsilastir(s, R.hs1, 'HS1', 1e-5, 1e-7);
  miKarsilastir(s, R.hs1, 'HS1');
});

test('kovaryans matrisi girdisi (lavaan sample.cov, (n−1)/n ölçeklemesi)', () => {
  const n = hs.veri.length;
  const p = 9;
  const ort = new Array(p).fill(0);
  for (const s of hs.veri) for (let j = 0; j < p; j++) ort[j] += s[j] / n;
  const K = Array.from({ length: p }, (_, a) => Array.from({ length: p }, (_, b) => hs.veri.reduce((t, s) => t + (s[a] - ort[a]) * (s[b] - ort[b]), 0) / (n - 1)));
  const s = calistir({ kovaryans: K, n, degiskenAdlari: hs.adlar, model: M3 });
  karsilastir(s, R.hs3cov, 'HS3 kovaryans');
  const ham = dogrulayiciFaktorAnalizi({ veri: hs.veri, degiskenAdlari: hs.adlar, model: M3 });
  yakin(s.uyum.kiKare, ham.uyum.kiKare, 1e-10, 0, 'ham veri ile aynı χ²');
});

test('bfi 5 faktör, 25 madde (n = 2436)', () => {
  const s = calistir({ veri: bfi.veri, degiskenAdlari: bfi.adlar, model: MB });
  assert.equal(s.sd, 265);
  karsilastir(s, R.bfi5, 'bfi5', 1e-5, 1e-7);
  miKarsilastir(s, R.bfi5, 'bfi5');
  for (const g of s.guvenirlik) {
    yakin(g.birlesikGuvenirlik, R.bfi5.bg[g.faktor].cr, 1e-5, 1e-7, `BG ${g.faktor}`);
    yakin(g.ortalamaAciklananVaryans, R.bfi5.bg[g.faktor].ave, 1e-5, 1e-7, `AOV ${g.faktor}`);
  }
  // A1 (ters madde) işaretçi olduğu için yükü +1'e sabitlenir; faktörün yönü A1'e göre belirlenir ve diğer A maddeleri negatif yüklenir
  assert.ok(s.yukler.find((y) => y.degisken === 'A1').std > 0);
  assert.ok(s.yukler.find((y) => y.degisken === 'A2').std < 0);
});

test('değişken sırası modeldeki görünüş sırasıdır; veri sütun sırası önemsizdir', () => {
  const ters = hs.adlar.slice().reverse();
  const veri = hs.veri.map((s) => s.slice().reverse());
  const s = dogrulayiciFaktorAnalizi({ veri, degiskenAdlari: ters, model: M3 });
  assert.deepEqual(s.degiskenler, ['x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9']);
  yakin(s.uyum.kiKare, R.hs3.fm.chisq, GOR, 0, 'χ²');
});

test('uç durumlar ve hata mesajları', () => {
  const temel = { veri: hs.veri, degiskenAdlari: hs.adlar };
  assert.throws(() => dogrulayiciFaktorAnalizi({ ...temel, model: { a: ['x1', 'x2', 'y'] } }), /bulunamadı/);
  assert.throws(() => dogrulayiciFaktorAnalizi({ ...temel, model: { a: ['x1', 'x2', 'x3'], b: ['x3', 'x4', 'x5'] } }), /çapraz/);
  assert.throws(() => dogrulayiciFaktorAnalizi({ ...temel, model: { a: ['x1'] } }), /en az iki/);
  assert.throws(() => dogrulayiciFaktorAnalizi({ ...temel, model: { a: ['x1', 'x2'] } }), /tanımlanamıyor/);
  assert.throws(() => dogrulayiciFaktorAnalizi({ ...temel, model: { x1: ['x2', 'x3', 'x4'] } }), /aynı olamaz/);
  assert.throws(() => dogrulayiciFaktorAnalizi({ ...temel, model: [] }), IstatistikHatasi);
  assert.throws(() => dogrulayiciFaktorAnalizi({ ...temel, model: M3, kestirim: 'wls' }), /en çok olabilirlik/);
  assert.throws(() => dogrulayiciFaktorAnalizi({ ...temel, model: M3, olcekleme: 'xyz' }), IstatistikHatasi);
  assert.throws(() => dogrulayiciFaktorAnalizi({ ...temel, model: M3, iliskiliHatalar: [['x1', 'x1']] }), /farklı/);
  assert.throws(() => dogrulayiciFaktorAnalizi({ ...temel, model: M3, iliskiliHatalar: [['x1', 'z']] }), /göstergelerden/);
  // eksik değer
  const eksik = hs.veri.map((s, i) => (i === 3 ? [NaN, ...s.slice(1)] : s));
  assert.throws(() => dogrulayiciFaktorAnalizi({ veri: eksik, degiskenAdlari: hs.adlar, model: M3 }), /eksik/);
  // doğrusal bağımlı göstergeler: S pozitif tanımlı değil
  const bagimli = hs.veri.map((s) => [...s, s[0] + s[1]]);
  assert.throws(
    () => dogrulayiciFaktorAnalizi({ veri: bagimli, degiskenAdlari: [...hs.adlar, 'x10'], model: { a: ['x1', 'x2', 'x10'], b: ['x4', 'x5', 'x6'] } }),
    /pozitif tanımlı/,
  );
  // gözlem sayısı yetersiz
  assert.throws(() => dogrulayiciFaktorAnalizi({ veri: hs.veri.slice(0, 5), degiskenAdlari: hs.adlar, model: M3 }), /gözlem sayısı/);
  // Heywood: tek faktör, λ1² = 0.81/0.7 > 1 → negatif hata varyansı; tam tanımlı model (sd = 0)
  const hw = dogrulayiciFaktorAnalizi({ kovaryans: [[1, 0.9, 0.9], [0.9, 1, 0.7], [0.9, 0.7, 1]], n: 200, degiskenAdlari: ['a', 'b', 'c'], model: { F: ['a', 'b', 'c'] } });
  assert.equal(hw.sd, 0);
  assert.ok(hw.hataVaryanslari[0].tahmin < 0);
  assert.ok(hw.uyarilar.some((u) => u.includes('Heywood')));
  assert.ok(hw.uyarilar.some((u) => u.includes('serbestlik derecesi 0')));
  yakin(hw.uyum.kiKare, 0, 0, 1e-8, 'tam tanımlı χ² = 0');
  // girdi değiştirilmez
  const kopya = JSON.stringify(hs.veri.slice(0, 20));
  dogrulayiciFaktorAnalizi({ ...temel, model: M3 });
  assert.equal(JSON.stringify(hs.veri.slice(0, 20)), kopya);
});
