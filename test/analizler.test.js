// Bütünleşik testler: her analiz türü örnek veri setlerinde hatasız çalışmalı, tablolar boş/NaN/undefined içermemeli,
// test seçimi beklenen yöntemi vermeli ve tablodaki p değerleri doğrudan hesaplananlarla aynı olmalı.
import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { yapistirilanMetinOku } from '../src/veri/index.js';
import { olcumTahmin } from '../src/veri/tanima.js';
import { ayarlariBirlestir } from '../src/karar/ayarlar.js';
import { bicimAyarlari } from '../src/rapor/bicim.js';
import { analizCalistir } from '../src/karar/index.js';
import { tabloMetin } from '../src/rapor/tablo.js';
import * as P from '../src/cekirdek/parametrik.js';
import * as NP from '../src/cekirdek/nonparametrik.js';

const KOK = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');

async function yukle(dosya, ayar = {}) {
  const vs = await yapistirilanMetinOku(fs.readFileSync(path.join(KOK, 'ornek-veriler', dosya), 'utf8'));
  const olcumler = {};
  for (const d of vs.degiskenler) {
    const t = olcumTahmin(d);
    olcumler[d.ad] = t.olcum;
    if (t.siraliDuzeyler && !d.duzeySirasi) d.duzeySirasi = t.siraliDuzeyler;
  }
  const ayarlar = ayarlariBirlestir(ayar);
  return { vs, baglam: { olcumler, ayarlar, bicim: bicimAyarlari(ayarlar), satirMaskesi: null } };
}

function denetle(oge, ad) {
  assert.equal(oge.hata, undefined, `${ad}: ${oge.hata}`);
  assert.ok(oge.bloklar.length > 0, `${ad}: blok yok`);
  for (const b of oge.bloklar) {
    if (b.tur === 'tablo') {
      const m = tabloMetin(b.tablo, 1);
      assert.ok(!/NaN|undefined|\[object Object\]|Infinity/.test(m), `${ad}: tabloda geçersiz değer\n${m}`);
    }
    if (b.tur === 'grafik') assert.ok(!/NaN|undefined/.test(b.svg), `${ad}: grafikte geçersiz değer (${b.baslik})`);
    if (b.tur === 'satir' || b.tur === 'not') assert.ok(!/NaN|undefined|\[object Object\]/.test(b.metin), `${ad}: metinde geçersiz değer: ${b.metin}`);
  }
  return oge;
}

const ISTEKLER = [
  ['klinik-calisma.csv', { tur: 'betimsel', secim: { degiskenler: ['Yas', 'CRP', 'Cinsiyet', 'Evre', 'Kan_Grubu'], grafik: true } }],
  ['klinik-calisma.csv', { tur: 'grup', secim: { grup: 'Grup', degiskenler: ['Yas', 'Cinsiyet', 'VKI', 'Sigara', 'Diyabet', 'Kan_Grubu', 'CRP', 'Hemoglobin', 'Evre', 'Yatis_Suresi', 'Komplikasyon'], grafik: true } }],
  ['klinik-calisma.csv', { tur: 'grup', secim: { grup: 'Kan_Grubu', degiskenler: ['Yas', 'CRP', 'Cinsiyet', 'Evre', 'Komplikasyon'], grafik: true } }],
  ['klinik-calisma.csv', { tur: 'korelasyon', secim: { degiskenler: ['Yas', 'VKI', 'CRP', 'Hemoglobin'], grafik: true } }],
  ['klinik-calisma.csv', { tur: 'korelasyon', secim: { degiskenler: ['Yas', 'VKI'], kontrol: ['Hemoglobin'], grafik: false } }],
  ['klinik-calisma.csv', { tur: 'capraz', secim: { satir: 'Sigara', sutun: 'Komplikasyon', katman: 'Cinsiyet', grafik: true } }],
  ['klinik-calisma.csv', { tur: 'capraz', secim: { satir: 'Kan_Grubu', sutun: 'Grup', grafik: true } }],
  ['klinik-calisma.csv', { tur: 'tekorneklem', secim: { degisken: 'Glukoz', deger: '100' } }],
  ['klinik-calisma.csv', { tur: 'tekorneklem', secim: { degisken: 'Kan_Grubu', oranlar: '0,3; 0,4; 0,1; 0,2' } }],
  ['klinik-calisma.csv', { tur: 'tekorneklem', secim: { degisken: 'Cinsiyet', oranlar: '' } }],
  ['klinik-calisma.csv', { tur: 'regresyon', secim: { bagimli: 'Komplikasyon', olayDuzeyi: 'Var', bagimsizlar: ['Yas', 'Grup', 'CRP', 'Cinsiyet'], tarama: '0.2', grafik: true } }],
  ['klinik-calisma.csv', { tur: 'regresyon', secim: { bagimli: 'Hemoglobin', bagimsizlar: ['Yas', 'Grup', 'Cinsiyet', 'VKI', 'Kan_Grubu'], tarama: 'hepsi', yontem: 'gir', grafik: true } }],
  ['klinik-calisma.csv', { tur: 'regresyon', secim: { bagimli: 'Hemoglobin', bagimsizlar: ['Yas', 'Grup', 'Cinsiyet', 'VKI', 'Kan_Grubu'], tarama: 'hepsi', yontem: 'geri', grafik: false } }],
  ['klinik-calisma.csv', { tur: 'regresyon', secim: { bagimli: 'Evre', bagimsizlar: ['Yas', 'CRP', 'Cinsiyet'], grafik: false } }],
  ['klinik-calisma.csv', { tur: 'regresyon', secim: { bagimli: 'Kan_Grubu', bagimsizlar: ['Yas', 'Cinsiyet'], grafik: false } }],
  ['once-sonra.csv', { tur: 'eslestirilmis', secim: { degiskenler: ['Kilo_once', 'Kilo_sonra'], grafik: true } }],
  ['once-sonra.csv', { tur: 'eslestirilmis', secim: { degiskenler: ['VAS_0', 'VAS_1ay', 'VAS_3ay'], grafik: true } }],
  ['once-sonra.csv', { tur: 'eslestirilmis', secim: { degiskenler: ['Kilo_once', 'Kilo_sonra'], grup: 'Tedavi', grafik: true } }],
  ['once-sonra.csv', { tur: 'eslestirilmis', secim: { degiskenler: ['VAS_0', 'VAS_1ay', 'VAS_3ay'], grup: 'Tedavi', grafik: true } }],
  ['once-sonra.csv', { tur: 'eslestirilmis', secim: { degiskenler: ['Agri_once', 'Agri_sonra'], grafik: true } }],
  ['olcek.csv', { tur: 'guvenirlik', secim: { degiskenler: ['S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10', 'S11', 'S12'] } }],
  ['olcek.csv', { tur: 'faktor', secim: { degiskenler: ['S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10', 'S11', 'S12'], tur: 'afa', cikarim: 'ana-eksen', faktorSayisi: 'paralel', dondurme: 'promax' } }],
  ['olcek.csv', { tur: 'faktor', secim: { degiskenler: ['S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10', 'S11', 'S12'], tur: 'dfa', model: 'A: S1, S2, S3, S4, S5, S6; B: S7, S8, S9, S10, S11, S12' } }],
  ['olcek.csv', { tur: 'grup', secim: { grup: 'Egitim', degiskenler: ['Yas', 'Cinsiyet', 'S1'], grafik: false } }],
  ['sagkalim.csv', { tur: 'sagkalim', secim: { sure: 'Sure_ay', olay: 'Olum', olayDuzeyi: 'Var', grup: 'Tedavi', zamanlar: '12; 36', kovaryatlar: ['Tedavi', 'Yas', 'Evre'], grafik: true } }],
  ['sagkalim.csv', { tur: 'sagkalim', secim: { sure: 'Sure_ay', olay: 'Olum', olayDuzeyi: 'Var', grup: 'Evre', grafik: true } }],
  ['sagkalim.csv', { tur: 'sagkalim', secim: { sure: 'Sure_ay', olay: 'Olum', grafik: true } }],
  ['tani-testi.csv', { tur: 'roc', secim: { durum: 'Sepsis', pozitif: 'Var', belirtecler: ['CRP', 'Prokalsitonin', 'Laktat'], grafik: true } }],
  ['tani-testi.csv', { tur: 'tani', secim: { test: 'Hizli_Test', testPozitif: 'Pozitif', referans: 'Sepsis', referansPozitif: 'Var', prevalans: '10' } }],
  ['gozlemci-uyumu.csv', { tur: 'uyum', secim: { degiskenler: ['Olcum_Gozlemci1', 'Olcum_Gozlemci2'], grafik: true } }],
  ['gozlemci-uyumu.csv', { tur: 'uyum', secim: { degiskenler: ['Derece_Gozlemci1', 'Derece_Gozlemci2'] } }],
  ['gozlemci-uyumu.csv', { tur: 'uyum', secim: { degiskenler: ['Tani_Gozlemci1', 'Tani_Gozlemci2'] } }],
  ['gozlemci-uyumu.csv', { tur: 'guc', secim: { test: 'iki-bagimsiz-t', hesap: 'n', etki: '0,5', guc: '0,8', alfa: '0,05' } }],
];

test('her analiz türü örnek verilerde hatasız çalışır', async () => {
  for (const [dosya, istek] of ISTEKLER) {
    const { vs, baglam } = await yukle(dosya);
    denetle(analizCalistir(istek, vs, baglam), `${dosya} ${istek.tur} ${JSON.stringify(istek.secim).slice(0, 60)}`);
  }
});

test('ondalık nokta ve APA biçimi ayarlarıyla da hatasız', async () => {
  for (const [dosya, istek] of ISTEKLER.slice(0, 12)) {
    const { vs, baglam } = await yukle(dosya, { ondalikAyrac: '.', pBasSifir: false, surekliGosterim: 'ikisi', normallikYontemi: 'spss', grafikTema: 'renkli' });
    const o = denetle(analizCalistir(istek, vs, baglam), `${dosya} ${istek.tur} (nokta)`);
    for (const b of o.bloklar.filter((x) => x.tur === 'satir')) assert.ok(!/\d,\d/.test(b.metin.replace(/; /g, '').replace(/, /g, '')), `ondalık virgül kalmış: ${b.metin}`);
  }
});

test('test seçimi: normal/homojen → t testi, çarpık → Mann-Whitney; Tablo 1 p değerleri doğrudan hesapla aynı', async () => {
  const { vs, baglam } = await yukle('klinik-calisma.csv');
  const o = analizCalistir({ tur: 'grup', secim: { grup: 'Grup', degiskenler: ['VKI', 'CRP'] } }, vs, baglam);
  assert.ok(o.yontemler.includes('student-t'));
  assert.ok(o.yontemler.includes('mann-whitney-u'));
  const sutun = (ad) => vs.degiskenler.find((d) => d.ad === ad).degerler;
  const grup = sutun('Grup');
  const ayir = (ad) => ['Hasta', 'Kontrol'].map((g) => sutun(ad).filter((v, i) => grup[i] === g && typeof v === 'number'));
  const [h1, k1] = ayir('VKI');
  const t = P.bagimsizT(h1, k1, { esitVaryans: true });
  const [h2, k2] = ayir('CRP');
  const mw = NP.mannWhitneyU(h2, k2);
  const tablo = o.bloklar.find((b) => b.tur === 'tablo').tablo;
  const pMetni = (satirBasi) => tablo.satirlar.find((s) => String(s.hucreler[0].metin ?? s.hucreler[0]).startsWith(satirBasi)).hucreler.at(-1).metin;
  const bicim = (p) => (p < 0.001 ? '<0,001' : p.toFixed(3).replace('.', ','));
  assert.equal(pMetni('VKI'), bicim(t.p));
  assert.equal(pMetni('CRP'), bicim(mw.p));
});

test('kategorik test seçimi: küçük beklenen değerde Fisher, büyük tabloda Fisher-Freeman-Halton', async () => {
  const { vs, baglam } = await yukle('klinik-calisma.csv');
  const o = analizCalistir({ tur: 'capraz', secim: { satir: 'Sigara', sutun: 'Komplikasyon' } }, vs, baglam);
  assert.ok(o.yontemler.includes('fisher'));
  const o2 = analizCalistir({ tur: 'grup', secim: { grup: 'Kan_Grubu', degiskenler: ['Komplikasyon'] } }, vs, baglam);
  assert.ok(o2.yontemler.includes('fisher-freeman-halton'));
});

test('hatalı istekler anlaşılır Türkçe hata döndürür', async () => {
  const { vs, baglam } = await yukle('klinik-calisma.csv');
  const o = analizCalistir({ tur: 'grup', secim: { grup: 'Grup', degiskenler: [] } }, vs, baglam);
  assert.match(o.hata, /uygun değişken/);
  const o2 = analizCalistir({ tur: 'regresyon', secim: { bagimli: 'Hasta_No', bagimsizlar: [] } }, vs, baglam);
  assert.ok(o2.hata);
  const o3 = analizCalistir({ tur: 'bilinmeyen', secim: {} }, vs, baglam);
  assert.match(o3.hata, /tanınmadı/);
});
