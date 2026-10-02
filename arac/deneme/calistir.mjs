// Geliştirme aracı: bir veri dosyasında bir analizi çalıştırıp tabloları metin olarak yazdırır.
//   node arac/deneme/calistir.mjs <dosya.csv> '<istek JSON>' ['<ayarlar JSON>']
import fs from 'node:fs';
import { yapistirilanMetinOku } from '../../src/veri/index.js';
import { olcumTahmin } from '../../src/veri/tanima.js';
import { ayarlariBirlestir } from '../../src/karar/ayarlar.js';
import { bicimAyarlari } from '../../src/rapor/bicim.js';
import { tabloMetin } from '../../src/rapor/tablo.js';

const MODUL = {
  grup: ['grup', 'grupAnalizi'],
  betimsel: ['betimsel', 'betimselAnaliz'],
  eslestirilmis: ['eslestirilmis', 'eslestirilmisAnaliz'],
  korelasyon: ['korelasyon', 'korelasyonAnalizi'],
  capraz: ['capraz', 'caprazAnaliz'],
  tekorneklem: ['tekorneklem', 'tekOrneklemAnalizi'],
  regresyon: ['regresyon', 'regresyonAnalizi'],
  roc: ['roc', 'rocAnalizi'],
  tani: ['tani', 'taniAnalizi'],
  sagkalim: ['sagkalim', 'sagkalimAnalizi'],
  uyum: ['uyum', 'uyumAnalizi'],
  guvenirlik: ['guvenirlik', 'guvenirlikAnalizi'],
  faktor: ['faktor', 'faktorAnalizi'],
};

const [dosya, istekMetni, ayarMetni] = process.argv.slice(2);
const vs = await yapistirilanMetinOku(fs.readFileSync(dosya, 'utf8'));
const olcumler = {};
for (const d of vs.degiskenler) {
  const t = olcumTahmin(d);
  olcumler[d.ad] = t.olcum;
  if (t.siraliDuzeyler && !d.duzeySirasi) d.duzeySirasi = t.siraliDuzeyler;
}
const ayarlar = ayarlariBirlestir(ayarMetni ? JSON.parse(ayarMetni) : {});
const baglam = { olcumler, ayarlar, bicim: bicimAyarlari(ayarlar), satirMaskesi: null };
const istek = JSON.parse(istekMetni);
const [modulAdi, fonksiyon] = MODUL[istek.tur];
const m = await import(`../../src/karar/analizler/${modulAdi}.js`);
const t0 = Date.now();
const o = { yontemler: [], ...m[fonksiyon](istek.secim, vs, baglam) };
console.log(`# ${o.baslik}  (${Date.now() - t0} ms)`);
console.log('ölçümler:', Object.entries(olcumler).map(([k, v]) => `${k}=${v}`).join(' '));
let no = 0;
for (const b of o.bloklar) {
  if (b.tur === 'tablo') console.log(`\n${tabloMetin(b.tablo, ++no)}`);
  else if (b.tur === 'karar') {
    const adimlar = [...(b.adimlar || []), ...(b.alt || []).flatMap((x) => x.adimlar.map((y) => ({ ...y, baslik: `[${x.baslik}] ${y.baslik}` })))];
    for (const a of adimlar) console.log(`  - ${a.baslik}: ${a.metin}`);
  } else if (b.tur === 'grafik') console.log(`[grafik] ${b.baslik} (${b.svg.length} bayt${/NaN|undefined/.test(b.svg) ? ', NaN/undefined İÇERİYOR' : ''})`);
  else console.log(`[${b.tur}] ${b.metin}`);
}
console.log('yöntemler:', o.yontemler.join(', '));
