// Veri içe aktarma katmanı testleri: model, kodlama, CSV, yapıştırma, Excel, SPSS .sav/.zsav, dosyaOku.
// SPSS beklenen değerleri: arac/r-referans/veri.R (haven::write_sav → haven::read_sav, test/referans/veri.json).
import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import * as XLSX from 'xlsx';
import { referans } from './yardimci.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import {
  bosVeriSeti, degiskenEkle, kopyala, eksikMi, etkinDeger, etkinDegerler, benzersizAd, adAnahtari, yeniDegisken,
} from '../src/veri/model.js';
import { metinCoz, kodlamaAlgila, cozucuOlustur } from '../src/veri/kodlama.js';
import { csvAyristir, ayiriciBul, csvOku, csvOkuAyrintili, csvMetniOku, yapistirilanMetinOku } from '../src/veri/csv.js';
import { sayiAyristir, tarihAyristir, tablodanVeriSeti, tablodanVeriSetiAyrintili, ondalikCikar, SAYISAL_ESIK } from '../src/veri/tablo.js';
import { excelAc, excelOku, excelOkuAyrintili, excelSayfalari, bicimOndaligi } from '../src/veri/excel.js';
import { savOku, savOkuAyrintili, SPSS_EPOCH_SANIYE } from '../src/veri/sav.js';
import { dosyaOku, dosyaOkuAyrintili, dosyaTuru } from '../src/veri/index.js';

const VERI = path.join(path.dirname(fileURLToPath(import.meta.url)), 'veri');
const dosya = (ad) => new Uint8Array(fs.readFileSync(path.join(VERI, ad)));
const R = referans('veri');
const utf8 = (s) => new TextEncoder().encode(s);
const utc = (y, a, g, sa = 0, dk = 0, sn = 0) => Date.UTC(y, a - 1, g, sa, dk, sn);
const bul = (vs, ad) => {
  const d = vs.degiskenler.find((x) => x.ad === ad);
  assert.ok(d, `"${ad}" değişkeni yok; olanlar: ${vs.degiskenler.map((x) => x.ad).join(', ')}`);
  return d;
};

// Bağımsız windows-1254 kodlayıcı (test verisi üretmek için; çözücüyü sınamak amacıyla koddan ayrı tutulur)
const CP1254 = { 'Ğ': 0xd0, 'İ': 0xdd, 'Ş': 0xde, 'ğ': 0xf0, 'ı': 0xfd, 'ş': 0xfe, 'Ç': 0xc7, 'ç': 0xe7, 'Ö': 0xd6, 'ö': 0xf6, 'Ü': 0xdc, 'ü': 0xfc, 'â': 0xe2, '€': 0x80, '’': 0x92 };
function cp1254(s) {
  const b = [];
  for (const ch of s) {
    const k = ch.charCodeAt(0);
    if (k < 0x80) b.push(k);
    else if (CP1254[ch] !== undefined) b.push(CP1254[ch]);
    else throw new Error(`windows-1254 ile kodlanamaz: ${ch}`);
  }
  return Uint8Array.from(b);
}

async function hataBekle(fn, desen) {
  await assert.rejects(
    async () => fn(),
    (e) => {
      assert.ok(e instanceof IstatistikHatasi, `IstatistikHatasi bekleniyordu, gelen: ${e && e.stack}`);
      if (desen) assert.match(e.message, desen);
      return true;
    },
  );
}

/** Her okuyucunun çıktısı veri modelinin birebir şeklinde olmalı. */
function modelSekli(vs) {
  assert.deepEqual(Object.keys(vs), ['ad', 'kaynak', 'satirSayisi', 'degiskenler', 'uyarilar']);
  assert.equal(typeof vs.ad, 'string');
  assert.ok(['csv', 'excel', 'sav', 'yapistir', 'proje'].includes(vs.kaynak));
  assert.ok(Number.isInteger(vs.satirSayisi) && vs.satirSayisi >= 0);
  assert.ok(Array.isArray(vs.uyarilar) && vs.uyarilar.every((u) => typeof u === 'string'));
  const anahtarlar = new Set();
  for (const d of vs.degiskenler) {
    assert.deepEqual(Object.keys(d), ['ad', 'etiket', 'veriTuru', 'degerler', 'olcum', 'degerEtiketleri', 'eksikKodlar', 'ondalik', 'kaynakSutun']);
    assert.ok(typeof d.ad === 'string' && d.ad.trim() !== '', 'ad boş olmamalı');
    assert.ok(!anahtarlar.has(adAnahtari(d.ad)), `ad benzersiz olmalı: ${d.ad}`);
    anahtarlar.add(adAnahtari(d.ad));
    assert.equal(typeof d.etiket, 'string');
    assert.ok(['sayi', 'metin', 'tarih'].includes(d.veriTuru));
    assert.ok([null, 'surekli', 'sirali', 'kategorik'].includes(d.olcum));
    assert.equal(d.degerler.length, vs.satirSayisi, `${d.ad} uzunluğu`);
    for (const v of d.degerler) {
      if (v === null) continue;
      if (d.veriTuru === 'metin') assert.equal(typeof v, 'string', `${d.ad}: metin bekleniyordu`);
      else assert.ok(typeof v === 'number' && Number.isFinite(v), `${d.ad}: sonlu sayı bekleniyordu (${v})`);
    }
    assert.ok(d.degerEtiketleri === null || (typeof d.degerEtiketleri === 'object' && Object.values(d.degerEtiketleri).every((x) => typeof x === 'string')));
    if (d.eksikKodlar !== null) {
      assert.deepEqual(Object.keys(d.eksikKodlar), ['tekil', 'aralik']);
      assert.ok(Array.isArray(d.eksikKodlar.tekil));
      assert.ok(d.eksikKodlar.aralik === null || d.eksikKodlar.aralik.length === 2);
    }
    assert.ok(d.ondalik === null || (Number.isInteger(d.ondalik) && d.ondalik >= 0));
    assert.ok(Number.isInteger(d.kaynakSutun) && d.kaynakSutun >= 0);
  }
}

// ======================================================================= model

describe('model', () => {
  it('bosVeriSeti şekli', () => {
    const vs = bosVeriSeti('deneme', 'csv');
    assert.deepEqual(vs, { ad: 'deneme', kaynak: 'csv', satirSayisi: 0, degiskenler: [], uyarilar: [] });
    assert.throws(() => bosVeriSeti('x', 'word'), IstatistikHatasi);
  });

  it('degiskenEkle: girdiyi değiştirmez, ad çakışmasını çözer, uzunluk denetler', () => {
    const vs0 = bosVeriSeti('d', 'yapistir');
    const vs1 = degiskenEkle(vs0, { ad: 'Yaş', degerler: [1, 2, 3] });
    assert.equal(vs0.degiskenler.length, 0);
    assert.equal(vs1.satirSayisi, 3);
    const vs2 = degiskenEkle(vs1, { ad: 'yaş', degerler: [4, 5, 6] });
    assert.deepEqual(vs2.degiskenler.map((d) => d.ad), ['Yaş', 'yaş_2']);
    assert.equal(vs2.degiskenler[1].kaynakSutun, 1);
    assert.equal(vs1.degiskenler.length, 1);
    const vs3 = degiskenEkle(vs2, { ad: '', degerler: ['a', 'b', null], veriTuru: 'metin' });
    assert.equal(vs3.degiskenler[2].ad, 'Değişken3');
    modelSekli(vs3);
    assert.throws(() => degiskenEkle(vs3, { ad: 'Kısa', degerler: [1] }), /3 satır/);
    assert.throws(() => yeniDegisken({ ad: 'x', degerler: [], veriTuru: 'yazi' }), IstatistikHatasi);
  });

  it('benzersizAd Türkçe büyük/küçük harf kurallarıyla', () => {
    assert.equal(adAnahtari('IŞIK'), 'ışık');
    assert.equal(adAnahtari('İL'), 'il');
    const k = new Set(['ışık', 'il', 'yaş', 'yaş_2']);
    assert.equal(benzersizAd('ISIK', new Set()), 'ISIK');
    assert.equal(benzersizAd('IŞIK', k), 'IŞIK_2');
    assert.equal(benzersizAd('İl', k), 'İl_2');
    assert.equal(benzersizAd('Yaş', k), 'Yaş_3');
    assert.equal(benzersizAd('  ', k, 'Değişken7'), 'Değişken7');
  });

  it('kopyala derin kopya üretir', () => {
    const vs = degiskenEkle(bosVeriSeti('d'), {
      ad: 'x', degerler: [1, 2, 99], degerEtiketleri: { 1: 'bir' }, eksikKodlar: { tekil: [99], aralik: [100, Infinity] },
    });
    const k = kopyala(vs);
    k.degiskenler[0].degerler[0] = 5;
    k.degiskenler[0].degerEtiketleri[1] = 'değişti';
    k.degiskenler[0].eksikKodlar.tekil.push(98);
    k.degiskenler[0].eksikKodlar.aralik[0] = 0;
    k.uyarilar.push('u');
    assert.deepEqual(vs.degiskenler[0].degerler, [1, 2, 99]);
    assert.equal(vs.degiskenler[0].degerEtiketleri[1], 'bir');
    assert.deepEqual(vs.degiskenler[0].eksikKodlar, { tekil: [99], aralik: [100, Infinity] });
    assert.deepEqual(vs.uyarilar, []);
  });

  it('eksikMi / etkinDeger: null, NaN, tekil kodlar, açık uçlu aralık, metin kodları', () => {
    const d = yeniDegisken({ ad: 'g', degerler: [null, NaN, -7.5, 0, 0.5, 999, 998, 1e300], eksikKodlar: { tekil: [999], aralik: [-Infinity, 0] } });
    assert.deepEqual(d.degerler.map((v) => eksikMi(d, v)), [true, true, true, true, false, true, false, false]);
    assert.equal(etkinDeger(d, 2), null);
    assert.equal(etkinDeger(d, 4), 0.5);
    assert.deepEqual(etkinDegerler(d), [null, null, null, null, 0.5, null, 998, 1e300]);
    const m = yeniDegisken({ ad: 'm', veriTuru: 'metin', degerler: ['X', 'X ', 'x', 'belirsiz-kayıt', null], eksikKodlar: { tekil: ['X', 'belirsiz'], aralik: null } });
    assert.deepEqual(m.degerler.map((v) => eksikMi(m, v)), [true, true, false, false, true]);
    const yok = yeniDegisken({ ad: 'y', degerler: [1, null] });
    assert.deepEqual(etkinDegerler(yok), [1, null]);
  });
});

// ======================================================================= kodlama

describe('kodlama', () => {
  it('windows-1254 tablosu WHATWG/ICU TextDecoder ile aynı', (t) => {
    let td;
    try {
      td = new TextDecoder('windows-1254');
    } catch {
      t.skip('Bu Node windows-1254 desteklemiyor');
      return;
    }
    const b = Uint8Array.from({ length: 256 }, (_, i) => i);
    assert.equal(cozucuOlustur('windows-1254')(b), td.decode(b));
    assert.equal(cozucuOlustur('ISO-8859-9')(cp1254('ğüşıöç İĞÜŞÖÇ')), 'ğüşıöç İĞÜŞÖÇ');
  });

  it('algılama: UTF-8 (BOM\'lu/BOM\'suz), UTF-16 LE/BE (BOM\'lu/BOM\'suz), windows-1254', () => {
    const s = 'Ad;Şehir\nIşık;İğdır\n';
    assert.deepEqual(kodlamaAlgila(utf8(s)), { kodlama: 'utf-8', bom: false, ascii: false });
    assert.deepEqual(kodlamaAlgila(utf8('abc')), { kodlama: 'utf-8', bom: false, ascii: true });
    assert.equal(kodlamaAlgila(Uint8Array.from([0xef, 0xbb, 0xbf, ...utf8(s)])).bom, true);
    const le = Buffer.from('\ufeff' + s, 'utf16le');
    assert.deepEqual(metinCoz(le), { metin: s, kodlama: 'utf-16le', bom: true });
    const be = Buffer.from(Buffer.from('\ufeff' + s, 'utf16le')).swap16();
    assert.deepEqual(metinCoz(be), { metin: s, kodlama: 'utf-16be', bom: true });
    assert.equal(metinCoz(Buffer.from(s + s + s, 'utf16le')).metin, s + s + s);
    assert.deepEqual(metinCoz(cp1254(s)), { metin: s, kodlama: 'windows-1254', bom: false });
    // Kesik UTF-8 (son karakter yarım) yine UTF-8 sayılır
    const kesik = utf8('Yaş;Kilo\n1;ğ').slice(0, -1);
    assert.equal(kodlamaAlgila(kesik).kodlama, 'utf-8');
    // Zorlanan kodlama
    assert.equal(metinCoz(cp1254('ş'), { kodlama: 'windows-1254' }).metin, 'ş');
  });
});

// ======================================================================= sayı ve tarih ayrıştırma

describe('sayı ve tarih ayrıştırma', () => {
  it('sayiAyristir: ondalık virgül/nokta, binlik ayırıcılar, işaretler, yüzde, bilimsel', () => {
    const durumlar = [
      // metin, virgül sonucu, nokta sonucu
      ['12,5', 12.5, NaN],
      ['12.5', NaN, 12.5],
      ['1.234,56', 1234.56, NaN],
      ['1,234.56', NaN, 1234.56],
      ['1 234,5', 1234.5, NaN],
      ['1\u00a0234.5', NaN, 1234.5],
      ['1.234.567', 1234567, NaN],
      ['12,345,678', NaN, 12345678],
      ['1,234', 1.234, 1234],
      ['1.234', 1234, 1.234],
      ['0,125', 0.125, NaN],
      [',5', 0.5, NaN],
      ['−3,5', -3.5, NaN],
      ['-3.5', NaN, -3.5],
      ['+7', 7, 7],
      ['007', 7, 7],
      ['%12,5', 0.125, NaN],
      ['12,5 %', 0.125, NaN],
      ['45%', 0.45, 0.45],
      ['1,5E-03', 0.0015, NaN],
      ['1.5e3', NaN, 1500],
      ['2E5', 200000, 200000],
      ['abc', NaN, NaN],
      ['1.2.3', NaN, NaN],
      ['12a', NaN, NaN],
      ['--5', NaN, NaN],
      ['', NaN, NaN],
    ];
    for (const [s, v, n] of durumlar) {
      assert.ok(Object.is(sayiAyristir(s, ','), v) || sayiAyristir(s, ',') === v, `"${s}" (virgül): ${sayiAyristir(s, ',')} ≠ ${v}`);
      assert.ok(Object.is(sayiAyristir(s, '.'), n) || sayiAyristir(s, '.') === n, `"${s}" (nokta): ${sayiAyristir(s, '.')} ≠ ${n}`);
    }
    // Ondalık metnin en yakın double karşılığı (ara işlemsiz)
    assert.equal(sayiAyristir('0,1', ','), 0.1);
    assert.equal(sayiAyristir('33,3%', ','), Number('0.333'));
    assert.equal(sayiAyristir('123456789,123456789', ','), Number('123456789.123456789'));
    assert.ok(Object.is(sayiAyristir('-0', ','), -0));
  });

  it('ondalikCikar', () => {
    assert.equal(ondalikCikar(0.1 + 0.2), 1);
    assert.equal(ondalikCikar(12.5), 1);
    assert.equal(ondalikCikar(100), 0);
    assert.equal(ondalikCikar(1e-7), 7);
    assert.equal(ondalikCikar(1.23e-10), 12);
    assert.equal(ondalikCikar(-0.333), 3);
  });

  it('tarihAyristir: ISO, Türkçe sıra, saat, saat dilimi, ABD sırası, geçersiz tarih', () => {
    assert.equal(tarihAyristir('2024-03-15'), utc(2024, 3, 15));
    assert.equal(tarihAyristir('2024-03-15T14:30:05'), utc(2024, 3, 15, 14, 30, 5));
    assert.equal(tarihAyristir('2024-03-15 14:30'), utc(2024, 3, 15, 14, 30));
    assert.equal(tarihAyristir('2024-03-15T10:00:00+03:00'), utc(2024, 3, 15, 7));
    assert.equal(tarihAyristir('2024-03-15T10:00:00.250Z'), utc(2024, 3, 15, 10) + 250);
    assert.equal(tarihAyristir('15.03.2024'), utc(2024, 3, 15));
    assert.equal(tarihAyristir('5.3.2024 08:05:09'), utc(2024, 3, 5, 8, 5, 9));
    assert.equal(tarihAyristir('15/03/2024'), utc(2024, 3, 15));
    assert.equal(tarihAyristir('03/15/2024', 'ay-gun'), utc(2024, 3, 15));
    assert.equal(tarihAyristir('15.03.2024', 'ay-gun'), utc(2024, 3, 15), 'noktalı yazım her zaman gün.ay');
    assert.equal(tarihAyristir('29.02.2024'), utc(2024, 2, 29));
    assert.equal(tarihAyristir('0050-01-01'), Date.UTC(2000, 0, 1) - 1950 * 365.2425 * 86400000 + (tarihAyristir('0050-01-01') - (Date.UTC(2000, 0, 1) - 1950 * 365.2425 * 86400000)));
    assert.equal(new Date(tarihAyristir('0050-01-01')).getUTCFullYear(), 50);
    for (const s of ['29.02.2023', '32.01.2024', '15.13.2024', '2024-02-30', '12:30', '2024', '15.03.24', '2024-03-15 25:00']) {
      assert.ok(Number.isNaN(tarihAyristir(s)), `${s} geçersiz olmalı`);
    }
  });
});

// ======================================================================= CSV

describe('CSV ayrıştırma', () => {
  it('RFC 4180: tırnak, kaçış, alan içi satır sonu, CRLF/LF/CR, sondaki ayırıcı', () => {
    const m = 'a,"b ""c""","d\r\ne"\r\n1,,3\n"x",y,\r4,5,6';
    assert.deepEqual(csvAyristir(m, ','), [['a', 'b "c"', 'd\r\ne'], ['1', '', '3'], ['x', 'y', ''], ['4', '5', '6']]);
    assert.deepEqual(csvAyristir('a;b\n', ';'), [['a', 'b']]);
    assert.deepEqual(csvAyristir('', ','), []);
    assert.deepEqual(csvAyristir('tek', ','), [['tek']]);
    assert.deepEqual(csvAyristir('a, "b,c" ,d', ','), [['a', 'b,c', 'd']], 'tırnaktan önce/sonra boşluk');
    const uyarilar = [];
    assert.deepEqual(csvAyristir('a,"bc\nd', ',', { uyarilar }), [['a', 'bc\nd']]);
    assert.equal(uyarilar.length, 1);
    assert.match(uyarilar[0], /kapanmamış/);
  });

  it('ayırıcı tahmini', () => {
    assert.equal(ayiriciBul('Ad;Yaş;Kilo\nAli;34;61,5\nVeli;51;88,25\n').ayirici, ';');
    assert.equal(ayiriciBul('Ad,Yaş,Kilo\nAli,34,61.5\n"Veli, Can",51,"88,25"\n').ayirici, ',');
    assert.equal(ayiriciBul('Ad\tYaş\nAli\t34\nVeli\t51\n').ayirici, '\t');
    assert.equal(ayiriciBul('Ad|Yaş\nAli|34,5\nVeli|51,1\n').ayirici, '|');
    // Her satırda tam bir ondalık virgül olsa da ';' tercih edilir
    assert.equal(ayiriciBul('x;y\n1,5;2,5\n3,5;4,5\n').ayirici, ';');
    assert.deepEqual(ayiriciBul('tek\nsütun\n'), { ayirici: '\t', alanSayisi: 1, tutarlilik: 1 });
    // Tek sütunlu ondalık virgüllü veri: virgül ayırıcı sayılmaz
    assert.equal(ayiriciBul('Yaş\n12,5\n13,25\n7\n').alanSayisi, 1);
    const tek = csvMetniOku('Yaş\n12,5\n13,25\n7\n');
    assert.deepEqual(tek.degiskenler.map((d) => d.ad), ['Yaş']);
    assert.deepEqual(tek.degiskenler[0].degerler, [12.5, 13.25, 7]);
  });
});

describe('CSV okuma', () => {
  it('R write.csv2 çıktısı (; ve ondalık virgül) read.csv2 ile aynı okunur', () => {
    const { veriSeti: vs, bilgi } = csvOkuAyrintili(dosya('iceaktar-r-csv2-utf8.csv'), { ad: 'r' });
    modelSekli(vs);
    assert.equal(bilgi.ayirici, ';');
    assert.equal(bilgi.kodlama, 'utf-8');
    assert.equal(bilgi.baslik, true);
    assert.deepEqual(vs.degiskenler.map((d) => d.ad), R.csv2.adlar);
    assert.deepEqual(bul(vs, 'Hasta').degerler, R.csv2.Hasta);
    assert.deepEqual(bul(vs, 'Grup').degerler, R.csv2.Grup);
    assert.deepEqual(bul(vs, 'Yas').degerler, R.csv2.Yas.map((x) => (x === null ? null : Number(x))));
    assert.deepEqual(bul(vs, 'Kilo').degerler, R.csv2.Kilo.map((x) => (x === null ? null : Number(x))));
    assert.equal(bul(vs, 'Kilo').ondalik, 3);
    assert.equal(bul(vs, 'Yas').ondalik, 0);
    assert.deepEqual(vs.uyarilar, []);
  });

  it('virgüllü CSV, ondalık nokta, tırnaklı alanlar, CRLF', () => {
    const m = 'id,boy,ad,not\r\n1,1.75,"Yılmaz, Ayşe","satır1\r\nsatır2"\r\n2,1.8,"Ali ""Can""",\r\n3,-0.5e1,Veli,x\r\n';
    const { veriSeti: vs, bilgi } = csvOkuAyrintili(utf8(m));
    modelSekli(vs);
    assert.equal(bilgi.ayirici, ',');
    assert.equal(vs.satirSayisi, 3);
    assert.deepEqual(bul(vs, 'id').degerler, [1, 2, 3]);
    assert.deepEqual(bul(vs, 'boy').degerler, [1.75, 1.8, -5]);
    assert.equal(bul(vs, 'boy').ondalik, 2);
    assert.deepEqual(bul(vs, 'ad').degerler, ['Yılmaz, Ayşe', 'Ali "Can"', 'Veli']);
    assert.deepEqual(bul(vs, 'not').degerler, ['satır1\r\nsatır2', null, 'x']);
    assert.equal(vs.kaynak, 'csv');
  });

  it('Türkçe Excel CSV: ;, ondalık virgül, binlik nokta, yüzde, eksi işareti, BOM', () => {
    const m = '\ufeffHasta;Gelir;Oran;Değişim;Tarih\r\nAyşe;1.234,56;%12,5;−3,5;15.03.2024\r\nIşık;12.000;7 %;+2;01.02.2023 14:30\r\nÖmer;950,5;100%;0;31.12.2023\r\n';
    const { veriSeti: vs, bilgi } = csvOkuAyrintili(utf8(m));
    modelSekli(vs);
    assert.equal(bilgi.bom, true);
    assert.equal(vs.degiskenler[0].ad, 'Hasta', 'BOM ada karışmamalı');
    assert.deepEqual(bul(vs, 'Gelir').degerler, [1234.56, 12000, 950.5]);
    assert.equal(bul(vs, 'Gelir').ondalik, 2);
    assert.deepEqual(bul(vs, 'Oran').degerler, [0.125, 0.07, 1]);
    assert.equal(bul(vs, 'Oran').ondalik, 3);
    assert.deepEqual(bul(vs, 'Değişim').degerler, [-3.5, 2, 0]);
    assert.equal(bul(vs, 'Tarih').veriTuru, 'tarih');
    assert.deepEqual(bul(vs, 'Tarih').degerler, [utc(2024, 3, 15), utc(2023, 2, 1, 14, 30), utc(2023, 12, 31)]);
    assert.ok(vs.uyarilar.some((u) => /Oran.*yüzde/.test(u)));
    assert.equal(bilgi.ondalikAyirici, ',');
  });

  it('sekmeyle ayrılmış, BOM\'suz UTF-16LE', () => {
    const m = 'Grup\tDeğer\r\nA\t1,5\r\nB\t2,25\r\n';
    const { veriSeti: vs, bilgi } = csvOkuAyrintili(Buffer.from(m, 'utf16le'));
    assert.equal(bilgi.kodlama, 'utf-16le');
    assert.equal(bilgi.ayirici, '\t');
    assert.deepEqual(bul(vs, 'Değer').degerler, [1.5, 2.25]);
    assert.ok(vs.uyarilar.some((u) => /UTF-16/.test(u)));
  });

  it('windows-1254 baytları (Türkçe harfler) algılanır ve doğru çözülür', () => {
    const m = 'Ad;Şehir;Yaş\r\nAyşe;İstanbul;34\r\nIşık;Muğla;27,5\r\nÇağrı;Iğdır;\r\nÖzgür;Üsküdar;41\r\n';
    const { veriSeti: vs, bilgi } = csvOkuAyrintili(cp1254(m));
    modelSekli(vs);
    assert.equal(bilgi.kodlama, 'windows-1254');
    assert.deepEqual(vs.degiskenler.map((d) => d.ad), ['Ad', 'Şehir', 'Yaş']);
    assert.deepEqual(bul(vs, 'Ad').degerler, ['Ayşe', 'Işık', 'Çağrı', 'Özgür']);
    assert.deepEqual(bul(vs, 'Şehir').degerler, ['İstanbul', 'Muğla', 'Iğdır', 'Üsküdar']);
    assert.deepEqual(bul(vs, 'Yaş').degerler, [34, 27.5, null, 41]);
    assert.ok(vs.uyarilar.some((u) => /Windows-1254/.test(u)));
    // Kullanıcı kodlamayı zorlayabilir
    const zorla = csvOkuAyrintili(cp1254(m), { kodlama: 'windows-1254' });
    assert.ok(!zorla.veriSeti.uyarilar.some((u) => /kodlaması/.test(u)));
  });

  it('eksik değer simgeleri (varsayılan ve özel liste)', () => {
    const m = 'a;b;c\n1;NA;x\n2;.;N/A\n3;-;—\n4;#N/A;null\n5;#YOK;?\n6;#SAYI/0!;y\n7; ;nan\n8;9;z\n';
    const vs = csvMetniOku(m);
    assert.deepEqual(bul(vs, 'b').degerler, [null, null, null, null, null, null, null, 9]);
    assert.deepEqual(bul(vs, 'c').degerler, ['x', null, null, null, null, 'y', null, 'z']);
    const ozel = csvMetniOku('a;b\n1;99\n2;-\n3;4\n', { eksikDegerler: ['99'] });
    assert.deepEqual(bul(ozel, 'b').degerler, [null, '-', '4']);
  });

  it('başlık algılama: sayısal gövde, tümü metin, başlıksız, zorlama', () => {
    let r = csvOkuAyrintili(utf8('1;2\n3;4\n'));
    assert.equal(r.bilgi.baslik, false);
    assert.deepEqual(r.veriSeti.degiskenler.map((d) => d.ad), ['Değişken1', 'Değişken2']);
    assert.equal(r.veriSeti.satirSayisi, 2);
    r = csvOkuAyrintili(utf8('Ad,Şehir\nAli,Ankara\nVeli,İzmir\n'));
    assert.equal(r.bilgi.baslik, true);
    r = csvOkuAyrintili(utf8('Ali,Ankara\nVeli,Ankara\nAli,İzmir\n'));
    assert.equal(r.bilgi.baslik, false, 'ilk satırın değerleri gövdede tekrar ediyor');
    r = csvOkuAyrintili(utf8('Yaş;Kilo\n;70\n34;\n'));
    assert.equal(r.bilgi.baslik, true);
    assert.deepEqual(bul(r.veriSeti, 'Yaş').degerler, [null, 34]);
    r = csvOkuAyrintili(utf8('Yaş;Kilo\n30;70\n'), { baslik: false });
    assert.equal(r.veriSeti.satirSayisi, 2);
    assert.equal(r.veriSeti.degiskenler[0].veriTuru, 'metin', '"Yaş" sayı olmadığından sütun metin kalır (2 değerden 1\'i)');
    r = csvOkuAyrintili(utf8('10;20\n30;40\n'), { baslik: true });
    assert.deepEqual(r.veriSeti.degiskenler.map((d) => d.ad), ['10', '20']);
    // Baştaki açıklama satırı atlanır
    r = csvOkuAyrintili(utf8('Hasta listesi 2024\nAd;Yaş;Kilo\nAli;30;70\nVeli;40;80\n'));
    assert.equal(r.bilgi.baslik, true);
    assert.equal(r.bilgi.atlananSatirlar, 1);
    assert.deepEqual(r.veriSeti.degiskenler.map((d) => d.ad), ['Ad', 'Yaş', 'Kilo']);
    // Elle atlanacak satır sayısı
    r = csvOkuAyrintili(utf8('not\nnot2\nx;y\n1;2\n'), { atlanacakSatir: 2 });
    assert.deepEqual(r.veriSeti.degiskenler.map((d) => d.ad), ['x', 'y']);
  });

  it('yinelenen ve boş başlıklar benzersizleştirilir (Türkçe İ/ı)', () => {
    const vs = csvMetniOku('Yaş,Yaş,,yaş,İL,il,IŞIK,ışık\n1,2,3,4,5,6,7,8\n');
    modelSekli(vs);
    assert.deepEqual(vs.degiskenler.map((d) => d.ad), ['Yaş', 'Yaş_2', 'Değişken3', 'yaş_3', 'İL', 'il_2', 'IŞIK', 'ışık_2']);
    assert.ok(vs.uyarilar.some((u) => /Yinelenen sütun adları/.test(u)));
  });

  it('düzensiz satırlar: kısa satırlar boşla tamamlanır, fazla alan adsız sütun olur', () => {
    const { veriSeti: vs } = csvOkuAyrintili(utf8('a,b,c\n1,2,3\n4,5\n6,7,8,9\n'));
    modelSekli(vs);
    assert.deepEqual(vs.degiskenler.map((d) => d.ad), ['a', 'b', 'c', 'Değişken4']);
    assert.deepEqual(bul(vs, 'c').degerler, [3, null, 8]);
    assert.deepEqual(bul(vs, 'Değişken4').degerler, [null, null, 9]);
    assert.ok(vs.uyarilar.some((u) => /1 satırda alan sayısı beklenenden \(3\) az \(satır 3\)/.test(u)));
    assert.ok(vs.uyarilar.some((u) => /1 satırda başlıktakinden fazla alan var \(satır 4\)/.test(u)));
  });

  it('boş satır/sütun temizliği; sondaki ayırıcılar uyarı üretmez', () => {
    const { veriSeti: vs, bilgi } = csvOkuAyrintili(utf8('\n\na;;b;\n1;;2;\n\n;;;\n3;;4;\n\n\n'));
    assert.deepEqual(vs.degiskenler.map((d) => [d.ad, d.kaynakSutun]), [['a', 0], ['b', 2]]);
    assert.deepEqual(bul(vs, 'b').degerler, [2, 4]);
    assert.equal(bilgi.bosSatirlar, 2);
    assert.ok(vs.uyarilar.some((u) => /2 tamamen boş satır/.test(u)));
    assert.ok(!vs.uyarilar.some((u) => /alan sayısı/.test(u)));
  });

  it(`sayısal sütun kuralı: eksik olmayanların en az %${SAYISAL_ESIK * 100}'i sayı olmalı`, () => {
    const dokuzOn = Array.from({ length: 19 }, (_, i) => String(i + 1));
    const vs = csvMetniOku(['x;y', ...dokuzOn.map((v, i) => `${v};${i === 3 ? 'abc' : v}`), '20;yok'].join('\n'));
    const x = bul(vs, 'x');
    assert.equal(x.veriTuru, 'sayi');
    const y = bul(vs, 'y');
    assert.equal(y.veriTuru, 'metin', '18/20 = %90 sayı → metin');
    const vs2 = csvMetniOku(['x', ...dokuzOn, 'yok'].join('\n'));
    const x2 = bul(vs2, 'x');
    assert.equal(x2.veriTuru, 'sayi', '19/20 = %95 sayı → sayısal');
    assert.equal(x2.degerler[19], null);
    assert.ok(vs2.uyarilar.some((u) => /"x" sütunu sayısal kabul edildi; sayıya çevrilemeyen 1 değer eksik sayıldı: satır 21 \("yok"\)/.test(u)));
  });

  it('ondalık ayırıcı sütun sütun ve dosya geneli kanıtla seçilir', () => {
    // b sütununda yalnızca belirsiz "1,234" biçimi var; dosyadaki diğer sütun ondalık virgül kanıtı veriyor
    let vs = csvMetniOku('a;b\n12,5;1,234\n3,25;2,500\n');
    assert.deepEqual(bul(vs, 'b').degerler, [1.234, 2.5]);
    // Virgüllü CSV'de varsayılan nokta: tırnaklı "1,234" binlik ayırıcıdır
    vs = csvMetniOku('a,b\n1.5,"1,234"\n2.5,"2,500"\n');
    assert.deepEqual(bul(vs, 'b').degerler, [1234, 2500]);
    // Zorlama
    vs = csvMetniOku('a;b\n1.234;5\n', { ondalikAyirici: '.' });
    assert.deepEqual(bul(vs, 'a').degerler, [1.234]);
    // Karışık biçim: azınlık eksik sayılır ve uyarılır
    vs = csvMetniOku(['a', ...Array.from({ length: 25 }, (_, i) => `${i},5`), '7.5'].join('\n'));
    assert.equal(bul(vs, 'a').degerler[25], null);
    assert.ok(vs.uyarilar.some((u) => /"7.5"/.test(u)));
  });

  it('tarih sütunları: gün/ay sırası, ABD sırası uyarısı, geçersiz tarih', () => {
    let vs = csvMetniOku('t\n13/01/2024\n02/03/2024\n');
    assert.deepEqual(bul(vs, 't').degerler, [utc(2024, 1, 13), utc(2024, 3, 2)]);
    vs = csvMetniOku('t\n01/13/2024\n02/14/2024\n03/01/2024\n');
    assert.deepEqual(bul(vs, 't').degerler, [utc(2024, 1, 13), utc(2024, 2, 14), utc(2024, 3, 1)]);
    assert.ok(vs.uyarilar.some((u) => /ay\/gün\/yıl/.test(u)));
    const satirlar = ['t', ...Array.from({ length: 24 }, (_, i) => `${String(i + 1).padStart(2, '0')}.01.2024`), '31.02.2024'];
    vs = csvMetniOku(satirlar.join('\n'));
    const t = bul(vs, 't');
    assert.equal(t.veriTuru, 'tarih');
    assert.equal(t.degerler[24], null);
    assert.ok(vs.uyarilar.some((u) => /tarihe çevrilemeyen 1 değer/.test(u)));
    vs = csvMetniOku('t;x\n2024-03-15T10:00:00Z;1\n2024-03-16T10:00:00+03:00;2\n');
    assert.deepEqual(bul(vs, 't').degerler, [utc(2024, 3, 15, 10), utc(2024, 3, 16, 7)]);
  });

  it('hatalar: boş dosya, ikili içerik', async () => {
    await hataBekle(() => csvOku(new Uint8Array(0)), /boş/);
    await hataBekle(() => csvOku(utf8('\n\n  \n')), /boş|veri bulunamadı/);
    await hataBekle(() => csvOku(Uint8Array.from([0x41, 0x00, 0x42, 0x01, 0x02, 0x00, 0x00, 0x00, 0x43, 0x44])), /ikili|binary/);
  });

  it('başarım: 100 000 satır × 6 sütun CSV', () => {
    const satirlar = ['id;grup;yas;skor;tarih;not'];
    for (let i = 0; i < 100000; i++) satirlar.push(`${i + 1};${['A', 'B', 'Ç'][i % 3]};${20 + (i % 50)};${(i * 0.37).toFixed(2).replace('.', ',')};${String(1 + (i % 28)).padStart(2, '0')}.0${1 + (i % 9)}.2020;${i % 7 === 0 ? 'NA' : 'iyi'}`);
    const bayt = utf8(satirlar.join('\r\n'));
    const t0 = performance.now();
    const vs = csvOku(bayt);
    const sure = performance.now() - t0;
    assert.equal(vs.satirSayisi, 100000);
    assert.equal(bul(vs, 'skor').degerler[99999], Number((99999 * 0.37).toFixed(2)));
    assert.equal(bul(vs, 'tarih').veriTuru, 'tarih');
    assert.ok(sure < 5000, `CSV okuma ${sure.toFixed(0)} ms sürdü`);
  });
});

describe('yapıştırılan veri', () => {
  it('Excel panosu: sekme, ondalık virgül, tırnaklı çok satırlı hücre, sondaki satır sonu', () => {
    const vs = yapistirilanMetinOku('Ad\tPuan\tAçıklama\r\nAli\t12,5\t"iki\r\nsatır"\r\nVeli\t13\t"tırnak ""içi"""\r\n');
    modelSekli(vs);
    assert.equal(vs.kaynak, 'yapistir');
    assert.deepEqual(bul(vs, 'Puan').degerler, [12.5, 13]);
    assert.deepEqual(bul(vs, 'Açıklama').degerler, ['iki\r\nsatır', 'tırnak "içi"']);
  });

  it('Türkçe Excel görünen metni: binlik nokta, ondalık virgül; sekmesiz yapıştırma ayırıcı tahmini', () => {
    let vs = yapistirilanMetinOku('Tutar\n1.250\n2.500\n');
    assert.deepEqual(bul(vs, 'Tutar').degerler, [1250, 2500], 'belirsiz biçimde varsayılan ondalık virgül');
    vs = yapistirilanMetinOku('a;b\n1;2,5\n');
    assert.deepEqual(bul(vs, 'b').degerler, [2.5]);
    vs = yapistirilanMetinOku('a,b\n1,2.5\n');
    assert.deepEqual(bul(vs, 'b').degerler, [2.5]);
    assert.throws(() => yapistirilanMetinOku('   \n'), IstatistikHatasi);
  });
});

// ======================================================================= Excel

function ornekKitap(tarihBicimi = 'm/d/yy') {
  const s = (v) => ({ t: 's', v });
  const ws = { A1: s('Hasta Verileri 2024') };
  ['Ad', 'Yaş', 'Kilo', 'Tarih', 'Grup', 'Sigara'].forEach((h, i) => { ws[String.fromCharCode(65 + i) + '2'] = s(h); });
  Object.assign(ws, { A3: s('Çağrı'), B3: { t: 'n', v: 34 }, C3: { t: 'n', v: 72.5, z: '0.0' }, D3: { t: 'n', v: 45000, z: tarihBicimi }, E3: s('Kontrol'), F3: { t: 'b', v: true } });
  Object.assign(ws, { A4: s('Şule'), B4: { t: 'n', v: 51 }, C4: s('68,25'), D4: { t: 'n', v: 45001.5, z: tarihBicimi }, F4: { t: 'b', v: false } });
  Object.assign(ws, { A6: s('İlker'), C6: { t: 'n', v: 80 }, D6: s('15.03.2020'), E6: s('Deney'), F6: { t: 'e', v: 7 } });
  Object.assign(ws, { A7: s('Ilgın'), B7: { t: 'n', v: 35, f: 'B3+1' }, C7: { t: 'n', f: 'C3*2' }, D7: { t: 'n', v: 45002, z: tarihBicimi }, E7: s('Deney'), F7: { t: 'b', v: true } });
  ws['!ref'] = 'A1:H9';
  ws['!merges'] = [{ s: { r: 0, c: 0 }, e: { r: 0, c: 5 } }, { s: { r: 2, c: 4 }, e: { r: 3, c: 4 } }];
  const wb = XLSX.utils.book_new();
  XLSX.utils.book_append_sheet(wb, {}, 'Boş');
  XLSX.utils.book_append_sheet(wb, ws, 'Hastalar');
  XLSX.utils.book_append_sheet(wb, XLSX.utils.aoa_to_sheet([['x', 'yüzde'], [1, { t: 'n', v: 0.4567, z: '0.0%' }], [2, { t: 'n', v: 0.5, z: '0.0%' }]]), 'Özet');
  return wb;
}

function hastalarDogrula(vs) {
  modelSekli(vs);
  assert.equal(vs.kaynak, 'excel');
  assert.equal(vs.satirSayisi, 4);
  assert.deepEqual(vs.degiskenler.map((d) => [d.ad, d.veriTuru, d.kaynakSutun]), [
    ['Ad', 'metin', 0], ['Yaş', 'sayi', 1], ['Kilo', 'sayi', 2], ['Tarih', 'tarih', 3], ['Grup', 'metin', 4], ['Sigara', 'sayi', 5],
  ]);
  assert.deepEqual(bul(vs, 'Ad').degerler, ['Çağrı', 'Şule', 'İlker', 'Ilgın']);
  assert.deepEqual(bul(vs, 'Yaş').degerler, [34, 51, null, 35], 'formülün kayıtlı sonucu okunur');
  assert.deepEqual(bul(vs, 'Kilo').degerler, [72.5, 68.25, 80, null], 'metin olarak saklanan "68,25" sayıya çevrilir');
  assert.equal(bul(vs, 'Kilo').ondalik, 2);
  assert.deepEqual(bul(vs, 'Tarih').degerler, [utc(2023, 3, 15), utc(2023, 3, 16, 12), utc(2020, 3, 15), utc(2023, 3, 17)]);
  assert.deepEqual(bul(vs, 'Grup').degerler, ['Kontrol', 'Kontrol', 'Deney', 'Deney'], 'dikey birleştirilmiş hücre doldurulur');
  assert.deepEqual(bul(vs, 'Sigara').degerler, [1, 0, null, 1]);
  assert.deepEqual(bul(vs, 'Sigara').degerEtiketleri, { 0: 'Yanlış', 1: 'Doğru' });
  assert.ok(vs.uyarilar.some((u) => /açıklama satırı atlandı: "Hasta Verileri 2024"/.test(u)));
  assert.ok(vs.uyarilar.some((u) => /1 tamamen boş satır/.test(u)));
  assert.ok(vs.uyarilar.some((u) => /birleştirilmiş/.test(u)));
  assert.ok(vs.uyarilar.some((u) => /hata değeri/.test(u)));
}

describe('Excel okuma', () => {
  for (const tur of ['xlsx', 'xls', 'xlsb', 'ods']) {
    it(`${tur}: sayılar, metin, tarihler, boş satır, birleştirilmiş hücreler, hata ve formül hücreleri`, () => {
      const buf = XLSX.write(ornekKitap(), { type: 'array', bookType: tur });
      const kitap = excelAc(buf, { ad: 'hastalar' });
      assert.deepEqual(kitap.sayfalar, ['Boş', 'Hastalar', 'Özet']);
      assert.equal(kitap.varsayilanSayfa, 'Hastalar', 'boş ilk sayfa atlanır');
      const vs = kitap.oku();
      hastalarDogrula(vs);
      assert.equal(vs.ad, 'hastalar – Hastalar');
      if (tur === 'xlsx') assert.ok(vs.uyarilar.some((u) => /formül hücresinin hesaplanmış değeri/.test(u)));
    });
  }

  it('sayfa seçimi, yüzde biçimli hücre (ham değer, ondalık + 2), sayfa listesi', () => {
    const buf = XLSX.write(ornekKitap(), { type: 'array', bookType: 'xlsx' });
    assert.deepEqual(excelSayfalari(buf), ['Boş', 'Hastalar', 'Özet']);
    const vs = excelOku(buf, { sayfa: 'Özet' });
    assert.deepEqual(bul(vs, 'yüzde').degerler, [0.4567, 0.5]);
    assert.equal(bul(vs, 'yüzde').ondalik, 3);
    assert.deepEqual(excelOku(buf, { sayfa: 2 }).degiskenler.map((d) => d.ad), ['x', 'yüzde']);
    const { bilgi } = excelOkuAyrintili(buf, { sayfa: 'Hastalar' });
    assert.equal(bilgi.sayfa, 'Hastalar');
    assert.equal(bilgi.baslik, true);
    assert.equal(bilgi.atlananSatirlar, 1);
    assert.throws(() => excelOku(buf, { sayfa: 'Yok' }), /"Yok" adlı sayfa yok/);
    assert.throws(() => excelOku(buf, { sayfa: 'Boş' }), /boş/);
  });

  it('writexl (R) ile yazılmış .xlsx R değerleriyle aynı okunur', () => {
    const kitap = excelAc(dosya('iceaktar-writexl.xlsx'));
    assert.deepEqual(kitap.sayfalar, ['Veriler', 'İkinci Sayfa']);
    const vs = kitap.oku('Veriler');
    modelSekli(vs);
    const sayi = (a) => a.map((x) => (x === null ? null : Number(x)));
    assert.deepEqual(bul(vs, 'ad').degerler, R.xlsx.ad);
    assert.deepEqual(bul(vs, 'deger').degerler, sayi(R.xlsx.deger));
    assert.equal(bul(vs, 'tarih').veriTuru, 'tarih');
    assert.deepEqual(bul(vs, 'tarih').degerler, sayi(R.xlsx.tarih));
    assert.deepEqual(bul(vs, 'zaman').degerler, sayi(R.xlsx.zaman));
    assert.deepEqual(bul(vs, 'mantik').degerler, [1, 0, null, 1]);
  });

  it('bicimOndaligi', () => {
    assert.equal(bicimOndaligi('General'), null);
    assert.equal(bicimOndaligi('0'), 0);
    assert.equal(bicimOndaligi('0.00'), 2);
    assert.equal(bicimOndaligi('#,##0.0'), 1);
    assert.equal(bicimOndaligi('0.0%'), 3);
    assert.equal(bicimOndaligi('0%'), 2);
    assert.equal(bicimOndaligi('[Red]0.000;[Blue]-0.0'), 3);
    assert.equal(bicimOndaligi('"TL "#,##0.00'), 2);
    assert.equal(bicimOndaligi('0.00E+00'), null);
  });

  it('bozuk ya da boş Excel dosyası Türkçe hata verir', async () => {
    const buf = new Uint8Array(XLSX.write(ornekKitap(), { type: 'array', bookType: 'xlsx' }));
    await hataBekle(() => excelOku(buf.slice(0, Math.floor(buf.length / 2))), /Excel dosyası okunamadı/);
    await hataBekle(() => excelOku(new Uint8Array(0)), /boş/);
    const bosKitap = XLSX.utils.book_new();
    XLSX.utils.book_append_sheet(bosKitap, {}, 'S1');
    await hataBekle(() => excelOku(XLSX.write(bosKitap, { type: 'array', bookType: 'xlsx' })), /boş/);
  });
});

// ======================================================================= SPSS: haven karşılaştırması

function bicimOndalikBeklenen(format, tur) {
  if (tur === 'metin' || tur === 'tarih') return null;
  const m = /^[A-Z]+\d+(?:\.(\d+))?$/.exec(format);
  return m && m[1] ? Number(m[1]) : 0;
}

function havenIleKarsilastir(vs, bilgi, ref) {
  modelSekli(vs);
  assert.equal(vs.kaynak, 'sav');
  assert.equal(vs.satirSayisi, ref.satirSayisi);
  assert.deepEqual(vs.degiskenler.map((d) => d.ad), ref.degiskenler.map((d) => d.ad));
  ref.degiskenler.forEach((r, k) => {
    const d = vs.degiskenler[k];
    const ne = `${r.ad}`;
    assert.equal(d.kaynakSutun, k);
    assert.equal(d.etiket, r.etiket, `${ne} etiketi`);
    assert.equal(d.veriTuru, r.tur === 'sure' ? 'sayi' : r.tur, `${ne} türü`);
    assert.equal(bilgi.bicimler[d.ad], r.format, `${ne} SPSS biçimi`);
    assert.equal(d.ondalik, bicimOndalikBeklenen(r.format, d.veriTuru), `${ne} ondalık`);
    assert.equal(d.olcum, R.olcumler[r.ad], `${ne} ölçüm düzeyi`);
    // Değerler: sayılar bit düzeyinde aynı; haven'ın "" metni bizde null
    r.degerler.forEach((b, i) => {
      const g = d.degerler[i];
      if (b === null || b === '') assert.equal(g, null, `${ne}[${i}] eksik olmalı`);
      else if (d.veriTuru === 'metin') assert.equal(g, b, `${ne}[${i}]`);
      else assert.ok(Object.is(g, Number(b)) || g === Number(b), `${ne}[${i}]: ${g} ≠ ${b}`);
    });
    // Değer etiketleri
    if (r.degerEtiketleri === null) assert.equal(d.degerEtiketleri, null, `${ne} değer etiketi olmamalı`);
    else {
      const beklenen = Object.fromEntries(r.degerEtiketleri.map((e) => [d.veriTuru === 'metin' ? e.deger : String(Number(e.deger)), e.etiket]));
      assert.deepEqual({ ...d.degerEtiketleri }, beklenen, `${ne} değer etiketleri`);
    }
    // Kullanıcı tanımlı eksik değer tanımı
    if (r.eksikTekil === null && r.eksikAralik === null) assert.equal(d.eksikKodlar, null, `${ne} eksik tanımı olmamalı`);
    else {
      const tekil = (r.eksikTekil || []).map((x) => (d.veriTuru === 'metin' ? x : Number(x)));
      const aralik = r.eksikAralik ? r.eksikAralik.map((x) => (x === '-Inf' ? -Infinity : x === 'Inf' ? Infinity : Number(x))) : null;
      assert.deepEqual(d.eksikKodlar, { tekil, aralik }, `${ne} eksik tanımı`);
    }
    // eksikMi, haven'ın is.na() sonucuyla aynı (haven boş metni eksik saymaz, biz sayarız)
    r.eksik.forEach((b, i) => {
      assert.equal(eksikMi(d, d.degerler[i]), b || r.degerler[i] === '', `${ne}[${i}] eksikMi`);
    });
  });
}

describe('SPSS .sav okuma (haven ile karşılaştırma)', () => {
  const dosyalar = { none: 'iceaktar-ornek-none.sav', byte: 'iceaktar-ornek-byte.sav', zsav: 'iceaktar-ornek-zsav.zsav' };
  for (const [sikistirma, ad] of Object.entries(dosyalar)) {
    it(`${ad}: tüm değerler, etiketler, değer etiketleri, eksik tanımları ve ölçüm düzeyleri`, async () => {
      const { veriSeti, bilgi } = await savOkuAyrintili(dosya(ad), { ad: 'ornek' });
      havenIleKarsilastir(veriSeti, bilgi, R.ornek[sikistirma]);
      assert.equal(bilgi.sikistirma, { none: 'yok', byte: 'bayt', zsav: 'zlib' }[sikistirma]);
      assert.equal(bilgi.imza, sikistirma === 'zsav' ? '$FL3' : '$FL2');
      assert.equal(bilgi.kodlama, 'utf-8');
      assert.equal(bilgi.endian, 'little');
      assert.equal(veriSeti.ad, 'ornek');
      // Uzun ad (> 8 karakter), Türkçe karakterli ad, çok uzun metin (> 255 bayt)
      assert.equal(bilgi.sutunlar.kan_basinci_sistolik_olcumu.kisaAd.length <= 8, true);
      assert.equal(bilgi.sutunlar.uzun_metin.genislik > 255, true);
      assert.ok(veriSeti.degiskenler.some((d) => d.ad === 'Değişken_Türkçe_Ad_ıİşŞğĞ'));
      assert.ok(veriSeti.uyarilar.some((u) => /"sure".*saniye/.test(u)));
    });
  }

  it('üç sıkıştırma türü aynı veri setini verir', async () => {
    const [a, b, c] = await Promise.all(Object.values(dosyalar).map((f) => savOku(dosya(f))));
    assert.deepEqual(b, a);
    assert.deepEqual(c, a);
  });

  it('çok uzun metin parçaları doğru birleştirilir (255 bayt/segment)', async () => {
    const vs = await savOku(dosya('iceaktar-ornek-byte.sav'));
    const d = bul(vs, 'uzun_metin');
    const uzunlar = d.degerler.filter((s) => s && utf8(s).length > 255);
    assert.ok(uzunlar.length > 10);
    assert.equal(d.degerler[2], 'ş'.repeat(300));
  });

  it('başarım: 50 000 satırlık .sav 1 saniyeden kısa sürede ve R ile aynı okunur', async () => {
    const b = dosya('iceaktar-buyuk.sav');
    const t0 = performance.now();
    const vs = await savOku(b);
    const sure = performance.now() - t0;
    assert.ok(sure < 1000, `50 000 satır ${sure.toFixed(0)} ms sürdü`);
    const ref = R.buyuk;
    modelSekli(vs);
    assert.equal(vs.satirSayisi, ref.satirSayisi);
    assert.deepEqual(vs.degiskenler.map((d) => d.ad), ref.adlar);
    const topla = (ad) => bul(vs, ad).degerler.reduce((t, v) => (v === null ? t : t + v), 0);
    const yakinMi = (a, b, ne) => assert.ok(Math.abs(a - b) <= 1e-12 * Math.abs(b), `${ne}: ${a} ≠ ${b}`);
    yakinMi(topla('yas'), Number(ref.toplamYas), 'yaş toplamı');
    yakinMi(topla('skor'), Number(ref.toplamSkor), 'skor toplamı');
    yakinMi(topla('puan'), Number(ref.toplamPuan), 'puan toplamı');
    yakinMi(topla('grup'), Number(ref.toplamGrup), 'grup toplamı');
    yakinMi(topla('tarih'), Number(ref.toplamTarih), 'tarih toplamı');
    assert.equal(bul(vs, 'yas').degerler.filter((v) => v === null).length, ref.eksikYas);
    const frekans = {};
    for (const s of bul(vs, 'sehir').degerler) frekans[s] = (frekans[s] || 0) + 1;
    assert.deepEqual(frekans, ref.sehirFrekans);
    for (const r of ref.ornekSatirlar) {
      for (const ad of ['id', 'grup', 'yas', 'skor', 'cinsiyet', 'tarih', 'puan']) {
        assert.equal(bul(vs, ad).degerler[r.satir], r[ad] === null ? null : Number(r[ad]), `${ad}[${r.satir}]`);
      }
      assert.equal(bul(vs, 'sehir').degerler[r.satir], r.sehir);
    }
    assert.deepEqual(bul(vs, 'grup').degerEtiketleri, { 1: 'Kontrol', 2: 'Deney A', 3: 'Deney B' });
  });
});

// ======================================================================= SPSS: elle kurulan dosyalar

const ENDUSUK = -1.7976931348623155e308;
const SYSMIS = -Number.MAX_VALUE;
const spssSaniye = (ms) => ms / 1000 + SPSS_EPOCH_SANIYE;

/**
 * Test için küçük bir SPSS sistem dosyası yazıcısı (haven'ın üretmediği durumlar: büyük-endian, windows-1254,
 * belgeler, ağırlık, MRSETS, bilinmeyen alt türler, farklı sapma, alt tür 16 vaka sayısı).
 */
function savYaz(o) {
  const le = o.le ?? true;
  const kodla = o.kodla ?? utf8;
  const sapma = o.sapma ?? 100;
  const parcalar = [];
  const sayi32 = (v) => {
    const b = new Uint8Array(4);
    new DataView(b.buffer).setInt32(0, v, le);
    return b;
  };
  const sayi64 = (v) => {
    const b = new Uint8Array(8);
    new DataView(b.buffer).setFloat64(0, v, le);
    return b;
  };
  const tam64 = (v) => {
    const b = new Uint8Array(8);
    new DataView(b.buffer).setBigInt64(0, BigInt(v), le);
    return b;
  };
  const i32 = (v) => parcalar.push(sayi32(v));
  const f64 = (v) => parcalar.push(sayi64(v));
  const bayt = (b) => parcalar.push(b instanceof Uint8Array ? b : Uint8Array.from(b));
  const dolgulu = (b, n) => {
    const s = new Uint8Array(n).fill(0x20);
    s.set(b.subarray(0, n));
    return s;
  };
  const metin = (s, n) => dolgulu(kodla(s), n);
  const birimSay = (d) => (d.tip > 0 ? Math.ceil(d.tip / 8) : 1);
  const toplamBirim = o.degiskenler.reduce((t, d) => t + birimSay(d), 0);
  const uc = (v) => (v === -Infinity ? ENDUSUK : v === Infinity ? Number.MAX_VALUE : v);

  bayt(utf8('$FL2'));
  bayt(dolgulu(utf8('@(#) SPSS DATA FILE test'), 60));
  i32(2);
  i32(toplamBirim);
  i32(o.sikistirma ?? 0);
  i32(o.agirlik ?? 0);
  i32(o.vakaSayisiBaslik ?? o.vakalar.length);
  f64(sapma);
  bayt(utf8('02 Oct 26'));
  bayt(utf8('12:34:56'));
  bayt(metin(o.dosyaEtiketi ?? '', 64));
  bayt([0, 0, 0]);

  for (const d of o.degiskenler) {
    const e = d.eksik;
    let ne = 0;
    let ed = [];
    if (e && e.aralik) {
      ne = e.tekil && e.tekil.length ? -3 : -2;
      ed = [uc(e.aralik[0]), uc(e.aralik[1]), ...(e.tekil || [])];
    } else if (e) {
      ne = e.tekil.length;
      ed = e.tekil;
    }
    const [bt, bg, bo] = d.bicim ?? (d.tip > 0 ? [1, d.tip, 0] : [5, 8, 2]);
    const f = (bt << 16) | (bg << 8) | bo;
    i32(2); i32(d.tip); i32(d.etiket ? 1 : 0); i32(ne); i32(f); i32(f);
    bayt(dolgulu(utf8(d.ad), 8));
    if (d.etiket) {
      const eb = kodla(d.etiket);
      i32(eb.length);
      bayt(eb);
      bayt(new Uint8Array((4 - (eb.length % 4)) % 4));
    }
    for (const v of ed) bayt(typeof v === 'string' ? metin(v, 8) : sayi64(v));
    for (let k = 1; k < birimSay(d); k++) {
      i32(2); i32(-1); i32(0); i32(0); i32(0); i32(0);
      bayt(dolgulu(new Uint8Array(0), 8));
    }
  }
  let idx = 1;
  for (const d of o.degiskenler) {
    const buIdx = idx;
    idx += birimSay(d);
    if (!d.degerEtiketleri) continue;
    i32(3);
    i32(d.degerEtiketleri.length);
    for (const [v, et] of d.degerEtiketleri) {
      bayt(typeof v === 'string' ? metin(v, 8) : sayi64(v));
      const eb = kodla(et);
      const blok = new Uint8Array(Math.ceil((eb.length + 1) / 8) * 8).fill(0x20);
      blok[0] = eb.length;
      blok.set(eb, 1);
      bayt(blok);
    }
    i32(4); i32(1); i32(buIdx);
  }
  if (o.belgeler) {
    i32(6);
    i32(o.belgeler.length);
    for (const s of o.belgeler) bayt(metin(s, 80));
  }
  const uzanti = (alt, boyut, veri) => {
    i32(7); i32(alt); i32(boyut); i32(veri.length / boyut);
    bayt(veri);
  };
  const birlestir = (dizi) => {
    const t = new Uint8Array(dizi.reduce((a, b) => a + b.length, 0));
    let p = 0;
    for (const b of dizi) {
      t.set(b, p);
      p += b.length;
    }
    return t;
  };
  if (o.karakterKodu !== undefined) uzanti(3, 4, birlestir([1, 0, 0, -1, 1, o.sikistirma ?? 0, le ? 2 : 1, o.karakterKodu].map(sayi32)));
  uzanti(4, 8, birlestir([SYSMIS, Number.MAX_VALUE, ENDUSUK].map(sayi64)));
  if (o.mrsets) uzanti(7, 1, utf8('$soru=C 9 Sorular CINSIYET KOD\n'));
  if (o.olcumler) uzanti(11, 4, birlestir(o.degiskenler.flatMap((d, i) => [o.olcumler[i], 8, 0]).map(sayi32)));
  const uzunlar = o.degiskenler.filter((d) => d.uzunAd);
  if (uzunlar.length) uzanti(13, 1, kodla(uzunlar.map((d) => `${d.ad}=${d.uzunAd}`).join('\t')));
  if (o.vakaSayisi16) uzanti(16, 8, birlestir([tam64(1), tam64(o.vakalar.length)]));
  if (o.bilinmeyen) {
    uzanti(99, 1, Uint8Array.from({ length: 13 }, (_, i) => i));
    uzanti(18, 1, utf8("CINSIYET:$@Role('0'\n)"));
    uzanti(24, 1, utf8('<xml/>'));
    uzanti(5, 1, utf8('Set1= ID KOD'));
  }
  if (o.kodlamaAdi) uzanti(20, 1, utf8(o.kodlamaAdi));
  i32(999); i32(0);

  // Veri
  const birimler = (vaka) => {
    const b = [];
    o.degiskenler.forEach((d, k) => {
      const v = vaka[k];
      if (d.tip === 0) b.push({ sayi: v });
      else {
        const s = metin(v ?? '', birimSay(d) * 8);
        for (let j = 0; j < birimSay(d); j++) b.push({ metin: s.subarray(8 * j, 8 * j + 8) });
      }
    });
    return b;
  };
  if (!o.sikistirma) {
    for (const vaka of o.vakalar) for (const b of birimler(vaka)) bayt('sayi' in b ? sayi64(b.sayi ?? SYSMIS) : b.metin);
  } else {
    let kodlar = [];
    let hamlar = [];
    const bosalt = () => {
      while (kodlar.length < 8) kodlar.push(0);
      bayt(kodlar);
      for (const h of hamlar) bayt(h);
      kodlar = [];
      hamlar = [];
    };
    const kod = (k, ham) => {
      kodlar.push(k);
      if (ham) hamlar.push(ham);
      if (kodlar.length === 8) bosalt();
    };
    for (const vaka of o.vakalar) {
      for (const b of birimler(vaka)) {
        if ('sayi' in b) {
          const v = b.sayi;
          if (v === null) kod(255);
          else if (Number.isInteger(v) && v + sapma >= 1 && v + sapma <= 251 && !Object.is(v, -0)) kod(v + sapma);
          else kod(253, sayi64(v));
        } else if (b.metin.every((x) => x === 0x20)) kod(254);
        else kod(253, b.metin);
      }
    }
    if (o.eofKodu) kod(252);
    if (kodlar.length) bosalt();
  }
  return birlestir(parcalar);
}

const ELLE_DEGISKENLER = [
  { ad: 'ID', tip: 0, bicim: [5, 8, 0] },
  { ad: 'CINSIYET', uzunAd: 'Cinsiyet_Değişkeni', tip: 0, etiket: 'Cinsiyet (ğüşıöç İĞÜŞÖÇ)', bicim: [5, 3, 0], degerEtiketleri: [[1, 'Erkek'], [2, 'Kadın'], [9, 'Yanıtsız']], eksik: { tekil: [9] } },
  { ad: 'SEHIR', uzunAd: 'Şehir', tip: 12, etiket: 'Yaşadığı şehir', eksik: { tekil: ['YOK'] } },
  { ad: 'KOD', tip: 3, degerEtiketleri: [['A', 'Ağır'], ['B', 'Bölüm']], eksik: { tekil: ['Z', 'ZZ'] } },
  { ad: 'TARIH', tip: 0, bicim: [20, 11, 0] },
  { ad: 'ADATE', tip: 0, bicim: [23, 10, 0] },
  { ad: 'AGIRLIK', tip: 0, bicim: [5, 8, 3] },
  { ad: 'PUAN', tip: 0, bicim: [5, 6, 2], eksik: { aralik: [-Infinity, -1], tekil: [999] } },
  { ad: 'SURE', tip: 0, bicim: [21, 8, 0] },
];
const ELLE_VAKALAR = [
  [1, 1, 'İstanbul', 'A', spssSaniye(utc(2024, 3, 15)), spssSaniye(utc(1999, 12, 31)), 1.5, 12.25, 3600],
  [2, 2, 'Şanlıurfa', 'B', null, spssSaniye(utc(1582, 10, 14)), 0.5, -3, 59.5],
  [3, 9, '', 'Z', spssSaniye(utc(2000, 2, 29, 13, 45, 30)), null, 2, 999, null],
  [4, null, 'YOK', '', spssSaniye(utc(1900, 1, 1)), spssSaniye(utc(2026, 10, 2)), 1, null, 0],
  [-150, 251, 'Ağrı Dağı ç', 'ZZ', spssSaniye(utc(1970, 1, 1)), spssSaniye(utc(1969, 12, 31, 23, 59, 59)), 123.456, 1e10, 86399],
];
const ELLE_OLCUM = [0, 1, 1, 1, 3, 3, 3, 2, 3];

function elleBeklenen(vs) {
  modelSekli(vs);
  assert.equal(vs.satirSayisi, 5);
  assert.deepEqual(vs.degiskenler.map((d) => d.ad), ['ID', 'Cinsiyet_Değişkeni', 'Şehir', 'KOD', 'TARIH', 'ADATE', 'AGIRLIK', 'PUAN', 'SURE']);
  assert.deepEqual(vs.degiskenler.map((d) => d.veriTuru), ['sayi', 'sayi', 'metin', 'metin', 'tarih', 'tarih', 'sayi', 'sayi', 'sayi']);
  assert.deepEqual(vs.degiskenler.map((d) => d.olcum), [null, 'kategorik', 'kategorik', 'kategorik', 'surekli', 'surekli', 'surekli', 'sirali', 'surekli']);
  assert.deepEqual(vs.degiskenler.map((d) => d.ondalik), [0, 0, null, null, null, null, 3, 2, 0]);
  const c = bul(vs, 'Cinsiyet_Değişkeni');
  assert.equal(c.etiket, 'Cinsiyet (ğüşıöç İĞÜŞÖÇ)');
  assert.deepEqual(c.degerler, [1, 2, 9, null, 251]);
  assert.deepEqual({ ...c.degerEtiketleri }, { 1: 'Erkek', 2: 'Kadın', 9: 'Yanıtsız' });
  assert.deepEqual(c.eksikKodlar, { tekil: [9], aralik: null });
  assert.deepEqual(c.degerler.map((v, i) => etkinDeger(c, i)), [1, 2, null, null, 251]);
  const s = bul(vs, 'Şehir');
  assert.equal(s.etiket, 'Yaşadığı şehir');
  assert.deepEqual(s.degerler, ['İstanbul', 'Şanlıurfa', null, 'YOK', 'Ağrı Dağı ç']);
  assert.deepEqual(s.eksikKodlar, { tekil: ['YOK'], aralik: null });
  assert.equal(eksikMi(s, 'YOK'), true);
  const k = bul(vs, 'KOD');
  assert.deepEqual(k.degerler, ['A', 'B', 'Z', null, 'ZZ']);
  assert.deepEqual({ ...k.degerEtiketleri }, { A: 'Ağır', B: 'Bölüm' });
  assert.deepEqual(k.degerler.map((v) => eksikMi(k, v)), [false, false, true, true, true]);
  assert.deepEqual(bul(vs, 'ID').degerler, [1, 2, 3, 4, -150]);
  assert.deepEqual(bul(vs, 'TARIH').degerler, [utc(2024, 3, 15), null, utc(2000, 2, 29, 13, 45, 30), utc(1900, 1, 1), 0]);
  assert.deepEqual(bul(vs, 'ADATE').degerler, [utc(1999, 12, 31), utc(1582, 10, 14), null, utc(2026, 10, 2), -1000]);
  assert.deepEqual(bul(vs, 'AGIRLIK').degerler, [1.5, 0.5, 2, 1, 123.456]);
  const p = bul(vs, 'PUAN');
  assert.deepEqual(p.degerler, [12.25, -3, 999, null, 1e10]);
  assert.deepEqual(p.eksikKodlar, { tekil: [999], aralik: [-Infinity, -1] });
  assert.deepEqual(p.degerler.map((v) => eksikMi(p, v)), [false, true, true, true, false]);
  assert.deepEqual(bul(vs, 'SURE').degerler, [3600, 59.5, null, 0, 86399]);
}

describe('SPSS .sav okuma (elle kurulan dosyalar)', () => {
  const ortak = { degiskenler: ELLE_DEGISKENLER, vakalar: ELLE_VAKALAR, olcumler: ELLE_OLCUM, kodla: cp1254 };

  it('büyük-endian, sıkıştırmasız, windows-1254 (yalnız kod sayfası), belgeler, ağırlık, MRSETS, bilinmeyen alt türler, alt tür 16', async () => {
    const b = savYaz({
      ...ortak, le: false, karakterKodu: 1254, agirlik: 8, belgeler: ['Bu dosya test için üretildi.', 'İkinci satır: ğüşıöç'],
      mrsets: true, bilinmeyen: true, vakaSayisiBaslik: -1, vakaSayisi16: true, dosyaEtiketi: 'Örnek dosya etiketi',
    });
    const { veriSeti, bilgi } = await savOkuAyrintili(b);
    elleBeklenen(veriSeti);
    assert.equal(bilgi.endian, 'big');
    assert.equal(bilgi.kodlama, 'windows-1254');
    assert.equal(bilgi.kodlamaKaynagi, 'dosya (kod sayfası)');
    assert.equal(bilgi.agirlikDegiskeni, 'AGIRLIK');
    assert.equal(bilgi.dosyaEtiketi, 'Örnek dosya etiketi');
    assert.deepEqual(bilgi.belgeler, ['Bu dosya test için üretildi.', 'İkinci satır: ğüşıöç']);
    assert.equal(bilgi.olusturma, '02 Oct 26 12:34:56');
    assert.equal(bilgi.bicimler.TARIH, 'DATE11');
    assert.equal(bilgi.bicimler.ADATE, 'ADATE10');
    assert.ok(veriSeti.uyarilar.some((u) => /ağırlık değişkeni tanımlı \("AGIRLIK"\)/.test(u)));
    assert.ok(veriSeti.uyarilar.some((u) => /çoklu yanıt/.test(u)));
    assert.ok(veriSeti.uyarilar.some((u) => /"SURE".*saniye/.test(u)));
  });

  it('küçük-endian, bayt kodu sıkıştırmalı, kodlama kaydı (alt tür 20) kod sayfasından önceliklidir, 252 dosya sonu kodu', async () => {
    const b = savYaz({ ...ortak, sikistirma: 1, karakterKodu: 1252, kodlamaAdi: 'windows-1254', eofKodu: true });
    const { veriSeti, bilgi } = await savOkuAyrintili(b);
    elleBeklenen(veriSeti);
    assert.equal(bilgi.kodlamaKaynagi, 'dosya (kodlama kaydı)');
    assert.equal(bilgi.sikistirma, 'bayt');
    assert.equal(bilgi.agirlikDegiskeni, null);
  });

  it('farklı sapma (bias = 50), büyük-endian sıkıştırmalı, vaka sayısı bilinmiyor', async () => {
    const b = savYaz({ ...ortak, le: false, sikistirma: 1, sapma: 50, karakterKodu: 1254, vakaSayisiBaslik: -1 });
    elleBeklenen(await savOku(b));
  });

  it('kodlama bilgisi yoksa: Türkçe baytlar → windows-1254 (uyarıyla), geçerli UTF-8 → UTF-8; elle zorlama', async () => {
    let r = await savOkuAyrintili(savYaz({ ...ortak }));
    elleBeklenen(r.veriSeti);
    assert.equal(r.bilgi.kodlamaKaynagi, 'tahmin');
    assert.ok(r.veriSeti.uyarilar.some((u) => /kodlaması bilgisi yok/.test(u)));
    r = await savOkuAyrintili(savYaz({ ...ortak, kodla: utf8 }));
    assert.equal(r.bilgi.kodlama, 'utf-8');
    assert.equal(bul(r.veriSeti, 'Şehir').degerler[1], 'Şanlıurfa');
    r = await savOkuAyrintili(savYaz({ ...ortak, kodla: utf8, karakterKodu: 1254 }), { kodlama: 'utf-8' });
    assert.equal(r.bilgi.kodlamaKaynagi, 'kullanıcı');
    assert.equal(bul(r.veriSeti, 'Şehir').degerler[1], 'Şanlıurfa');
  });

  it('ölçüm düzeyi kaydı yoksa olcum null', async () => {
    const vs = await savOku(savYaz({ ...ortak, olcumler: undefined, karakterKodu: 1254 }));
    assert.ok(vs.degiskenler.every((d) => d.olcum === null));
  });
});

describe('SPSS .sav okuma: bozuk ve eksik dosyalar', () => {
  const tam = dosya('iceaktar-ornek-byte.sav');

  it('kesik başlık, kesik sözlük, kesik veri → Türkçe hata', async () => {
    await hataBekle(() => savOku(tam.slice(0, 100)), /SPSS dosyası bozuk ya da eksik/);
    await hataBekle(() => savOku(tam.slice(0, 700)), /SPSS dosyası bozuk ya da eksik/);
    await hataBekle(() => savOku(tam.slice(0, tam.length - 40)), /SPSS dosyası bozuk ya da eksik/);
    const yok = dosya('iceaktar-ornek-none.sav');
    await hataBekle(() => savOku(yok.slice(0, yok.length - 100)), /veri bölümü eksik/);
    const z = dosya('iceaktar-ornek-zsav.zsav');
    await hataBekle(() => savOku(z.slice(0, z.length - 30)), /SPSS dosyası bozuk ya da eksik/);
  });

  it('kesikKabul: okunabilen vakalar uyarıyla döner', async () => {
    const yok = dosya('iceaktar-ornek-none.sav');
    const vs = await savOku(yok.slice(0, yok.length - 100), { kesikKabul: true });
    assert.ok(vs.satirSayisi < 40 && vs.satirSayisi > 0);
    assert.ok(vs.uyarilar.some((u) => /veri bölümü eksik/.test(u)));
    modelSekli(vs);
    const vs2 = await savOku(tam.slice(0, tam.length - 40), { kesikKabul: true });
    assert.ok(vs2.satirSayisi < 40);
    modelSekli(vs2);
  });

  it('yanlış imza, rastgele bayt, bozuk zlib bloğu, boş dosya', async () => {
    const yanlis = tam.slice();
    yanlis.set(utf8('XXXX'), 0);
    await hataBekle(() => savOku(yanlis), /SPSS veri dosyası \(\.sav\) değil/);
    const rastgele = Uint8Array.from({ length: 2000 }, (_, i) => (i * 7919 + 13) % 256);
    rastgele.set(utf8('$FL2'), 0);
    await hataBekle(() => savOku(rastgele), /SPSS dosyası bozuk/);
    const z = dosya('iceaktar-ornek-zsav.zsav').slice();
    const { veriSeti } = await savOkuAyrintili(z);
    assert.equal(veriSeti.satirSayisi, 40);
    // Son zlib bloğunun içini boz (blok, 48 baytlık ZSAV sonlandırıcısından hemen önce biter)
    const bozuk = z.slice();
    for (let i = 0; i < 64; i++) bozuk[z.length - 48 - 400 + i] ^= 0x5a;
    await hataBekle(() => savOku(bozuk), /SPSS dosyası bozuk/);
    await hataBekle(() => savOku(new Uint8Array(0)), /boş/);
  });
});

// ======================================================================= dosyaOku

describe('dosyaOku (tek giriş)', () => {
  it('imza ve uzantıyla tür belirleme', () => {
    assert.equal(dosyaTuru('veri.dat', dosya('iceaktar-ornek-byte.sav')), 'sav', 'uzantı yanlış olsa da imza');
    assert.equal(dosyaTuru('veri.csv', dosya('iceaktar-writexl.xlsx')), 'excel');
    assert.equal(dosyaTuru('veri.xls', XLSX.write(ornekKitap(), { type: 'array', bookType: 'xls' })), 'excel');
    assert.equal(dosyaTuru('veri.txt', utf8('a;b\n1;2')), 'csv');
    assert.equal(dosyaTuru('veri', utf8('a;b\n1;2')), 'csv');
    assert.equal(dosyaTuru('veri.xyz', utf8('a;b\n1;2')), 'csv');
    assert.equal(dosyaTuru('veri.xyz', Buffer.from('a;b\n1;2', 'utf16le')), 'csv');
    assert.throws(() => dosyaTuru('veri.xyz', Uint8Array.from([1, 2, 3, 0, 5])), /desteklenmiyor/);
    assert.throws(() => dosyaTuru('veri.por', utf8('abc')), /taşınabilir/);
    assert.throws(() => dosyaTuru('veri.dta', utf8('abc')), /Stata/);
    assert.throws(() => dosyaTuru('rapor.pdf', utf8('%PDF-1.7')), /PDF/);
    assert.throws(() => dosyaTuru('veri.sav', utf8('a;b\n1;2')), /SPSS veri dosyası \(\.sav\) gibi görünmüyor/);
  });

  it('CSV, SPSS ve Excel aynı girişten okunur; ad dosya adından gelir', async () => {
    const csv = await dosyaOku('C:\\Veriler\\anket sonuçları.csv', dosya('iceaktar-r-csv2-utf8.csv').buffer);
    modelSekli(csv);
    assert.equal(csv.ad, 'anket sonuçları');
    assert.equal(csv.kaynak, 'csv');
    const sav = await dosyaOku('hastalar.sav', dosya('iceaktar-ornek-zsav.zsav'));
    assert.equal(sav.kaynak, 'sav');
    assert.equal(sav.ad, 'hastalar');
    const { veriSeti, bilgi } = await dosyaOkuAyrintili('x.sav', dosya('iceaktar-ornek-none.sav'));
    assert.equal(bilgi.tur, 'sav');
    assert.equal(veriSeti.satirSayisi, 40);
    await hataBekle(() => dosyaOku('bos.csv', new ArrayBuffer(0)), /boş/);
  });

  it('çok sayfalı Excel: sayfalar ve sayfaOku sayılamayan yardımcılar olarak eklenir, model şekli değişmez', async () => {
    const buf = XLSX.write(ornekKitap(), { type: 'array', bookType: 'xlsx' });
    const vs = await dosyaOku('klinik.xlsx', buf);
    modelSekli(vs);
    hastalarDogrula(vs);
    assert.equal(vs.ad, 'klinik – Hastalar');
    assert.deepEqual(vs.sayfalar, ['Boş', 'Hastalar', 'Özet']);
    const ozet = await vs.sayfaOku('Özet');
    modelSekli(ozet);
    assert.deepEqual(ozet.degiskenler.map((d) => d.ad), ['x', 'yüzde']);
    assert.equal(JSON.parse(JSON.stringify(vs)).sayfalar, undefined);
    const secili = await dosyaOku('klinik.xlsx', buf, { sayfa: 'Özet' });
    assert.equal(secili.sayfalar, undefined);
    assert.equal(secili.satirSayisi, 2);
  });
});
