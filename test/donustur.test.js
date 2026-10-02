import { test } from 'node:test';
import assert from 'node:assert/strict';
import { ifadeDerle } from '../src/veri/ifade.js';
import * as T from '../src/veri/donustur.js';
import { olcumTahmin, siraliDuzeyBul, tekrarliOlcumAdaylari, grupAdaylari, duzeyleriBul } from '../src/veri/tanima.js';

function degisken(ad, degerler, ozellik = {}) {
  const sayi = degerler.every((v) => v === null || typeof v === 'number');
  return { ad, etiket: '', veriTuru: sayi ? 'sayi' : 'metin', degerler, olcum: null, degerEtiketleri: null, eksikKodlar: null, ondalik: null, kaynakSutun: 0, ...ozellik };
}
function veriSeti(...ds) {
  return { ad: 'test', kaynak: 'csv', satirSayisi: ds[0].degerler.length, degiskenler: ds, uyarilar: [] };
}

test('ifade: aritmetik, öncelik ve fonksiyonlar', () => {
  const g = (deg) => (ad) => deg[ad];
  const e = (m, d = {}) => ifadeDerle(m, Object.keys(d)).hesapla(g(d));
  assert.equal(e('1 + 2 * 3'), 7);
  assert.equal(e('(1 + 2) * 3'), 9);
  assert.equal(e('2 ^ 3 ^ 2'), 512);
  assert.equal(e('-2 ^ 2'), -4);
  assert.equal(e('kilo / (boy/100)^2', { kilo: 80, boy: 200 }), 20);
  assert.equal(e('ln(exp(2))'), 2);
  assert.equal(e('ortalama(a, b, c)', { a: 1, b: null, c: 5 }), 3);
  assert.equal(e('toplam(a, b)', { a: null, b: null }), null);
  assert.equal(e('a + b', { a: 1, b: null }), null);
  assert.equal(e('eger(yas >= 65, 1, 0)', { yas: 70 }), 1);
  assert.equal(e('eger(yas >= 65 ve cins == "K", 1, 0)', { yas: 70, cins: 'E' }), 0);
  assert.equal(e('[Sistolik KB] - 10', { 'Sistolik KB': 130 }), 120);
  assert.equal(e('1 / 0'), null);
  assert.equal(e('ln(-1)'), null);
  assert.equal(e('yuvarla(2.456, 2)'), 2.46);
  assert.throws(() => ifadeDerle('bilinmeyen + 1', []), /bilinmeyen/);
  assert.throws(() => ifadeDerle('(1 + 2', []), /bekleniyordu/);
  assert.throws(() => ifadeDerle('foo(1)', []), /Bilinmeyen fonksiyon/);
});

test('dönüştürme araçları', () => {
  const vs = veriSeti(
    degisken('kilo', [70, 80, null, 90]),
    degisken('boy', [175, 180, 170, null]),
    degisken('yas', [20, 35, 50, 70]),
    degisken('evre', ['I', 'II', 'III', 'IV']),
    degisken('S1', [1, 5, 3, 2]),
    degisken('S2', [2, 4, null, 2]),
  );
  const bmi = T.hesapla(vs, 'BMI', 'kilo/(boy/100)^2');
  assert.ok(Math.abs(bmi.degerler[0] - 70 / 1.75 ** 2) < 1e-12);
  assert.equal(bmi.degerler[2], null);
  assert.equal(bmi.degerler[3], null);

  const yasGrup = T.kategorize(vs, 'yas', 'yasGrup', [35, 65]);
  assert.deepEqual(yasGrup.degerler, [1, 2, 2, 3]);
  assert.deepEqual(yasGrup.degerEtiketleri, { 1: '<35', 2: '35–<65', 3: '≥65' });

  const evre2 = T.yenidenKodla(vs, 'evre', 'evre2', { I: 'Erken', II: 'Erken', III: 'İleri', IV: 'İleri' });
  assert.deepEqual(evre2.degerler, ['Erken', 'Erken', 'İleri', 'İleri']);

  const puan = T.olcekPuani(vs, ['S1', 'S2'], 'puan', { ters: ['S2'], min: 1, maks: 5 });
  assert.deepEqual(puan.degerler, [1 + 4, 5 + 2, null, 2 + 4]);
  const puanOrt = T.olcekPuani(vs, ['S1', 'S2'], 'puan2', { yontem: 'ortalama', enAzGecerli: 1 });
  assert.deepEqual(puanOrt.degerler, [1.5, 4.5, 3, 2]);
  assert.equal(T.adKullanilabilir(vs, 'Kilo'), '"Kilo" adında bir değişken zaten var.');
});

test('değişken türü tanıma', () => {
  assert.equal(olcumTahmin(degisken('Yaş', [23, 45, 67, 34, 56, 21, 78, 45, 33, 61, 29])).olcum, 'surekli');
  assert.equal(olcumTahmin(degisken('Cinsiyet', ['Erkek', 'Kadın', 'Kadın', 'Erkek'])).olcum, 'kategorik');
  assert.equal(olcumTahmin(degisken('Grup', [1, 2, 1, 2, 1])).olcum, 'kategorik');
  assert.equal(olcumTahmin(degisken('Şiddet', ['Hafif', 'Orta', 'Şiddetli', 'Hafif'])).olcum, 'sirali');
  assert.equal(olcumTahmin(degisken('Hasta No', [1, 2, 3, 4, 5, 6])).olcum, 'kimlik');
  assert.deepEqual(siraliDuzeyBul(['Evre III', 'Evre I', 'Evre II']), ['Evre I', 'Evre II', 'Evre III']);
  assert.deepEqual(siraliDuzeyBul(['Orta', 'Şiddetli', 'Hafif']), ['Hafif', 'Orta', 'Şiddetli']);
  assert.equal(siraliDuzeyBul(['Elma', 'Armut']), null);
  const d = degisken('Eğitim', ['Lise', 'İlkokul', 'Üniversite', 'Lise']);
  assert.deepEqual(duzeyleriBul(d).map((x) => x.deger), ['İlkokul', 'Lise', 'Üniversite']);
});

test('tekrarlı ölçüm ve grup adayları', () => {
  const vs = veriSeti(
    degisken('Grup', ['A', 'B', 'A', 'B']),
    degisken('VAS_0', [5, 6, 7, 8]),
    degisken('VAS_1ay', [4, 5, 6, 7]),
    degisken('VAS_6ay', [3, 4, 5, 6]),
    degisken('kilo_once', [70, 80, 90, 60]),
    degisken('kilo_sonra', [68, 79, 85, 61]),
    degisken('S1', [1, 2, 3, 4]),
    degisken('S2', [1, 2, 3, 4]),
    degisken('S3', [1, 2, 3, 4]),
    degisken('S4', [1, 2, 3, 4]),
    degisken('S5', [1, 2, 3, 4]),
  );
  const olcumler = Object.fromEntries(vs.degiskenler.map((d) => [d.ad, d.ad === 'Grup' ? 'kategorik' : d.ad.startsWith('S') ? 'sirali' : 'surekli']));
  const t = tekrarliOlcumAdaylari(vs, olcumler);
  assert.deepEqual(t.tekrarli.map((x) => x.degiskenler), [['VAS_0', 'VAS_1ay', 'VAS_6ay'], ['kilo_once', 'kilo_sonra']]);
  assert.deepEqual(t.olcekMaddeleri.map((x) => x.degiskenler), [['S1', 'S2', 'S3', 'S4', 'S5']]);
  assert.equal(grupAdaylari(vs, olcumler)[0].ad, 'Grup');
});
