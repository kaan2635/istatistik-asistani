// Veri içe aktarma katmanının tek girişi: dosyaOku(ad, ArrayBuffer, secenekler).
// Dosya türü önce sihirli baytlardan (imza), sonra uzantıdan belirlenir.
import { IstatistikHatasi } from '../cekirdek/hata.js';
import { csvOkuAyrintili, yapistirilanMetinOku, csvMetniOku, csvAyristir, ayiriciBul } from './csv.js';
import { excelAc, excelOku, excelSayfalari } from './excel.js';
import { savOkuAyrintili, savOku } from './sav.js';
import { tablodanVeriSeti, sayiAyristir, tarihAyristir, VARSAYILAN_EKSIK_DEGERLER, SAYISAL_ESIK } from './tablo.js';
import { metinCoz, kodlamaAlgila } from './kodlama.js';

export * from './model.js';
export {
  csvOkuAyrintili, yapistirilanMetinOku, csvMetniOku, csvAyristir, ayiriciBul,
  excelAc, excelOku, excelSayfalari,
  savOkuAyrintili, savOku,
  tablodanVeriSeti, sayiAyristir, tarihAyristir, VARSAYILAN_EKSIK_DEGERLER, SAYISAL_ESIK,
  metinCoz, kodlamaAlgila,
};

const EXCEL_UZANTILARI = new Set(['xlsx', 'xlsm', 'xltx', 'xltm', 'xlsb', 'xls', 'xlt', 'ods', 'fods', 'xlw']);
const METIN_UZANTILARI = new Set(['csv', 'tsv', 'tab', 'txt', 'dat', 'prn', 'text']);
const DESTEKLENMEYEN = {
  por: 'SPSS taşınabilir (.por) dosyaları desteklenmiyor. Dosyayı SPSS\'te .sav olarak kaydedin.',
  dta: 'Stata (.dta) dosyaları desteklenmiyor. Dosyayı CSV ya da Excel olarak dışa aktarın.',
  sas7bdat: 'SAS dosyaları desteklenmiyor. Dosyayı CSV ya da Excel olarak dışa aktarın.',
  rds: 'R (.rds) dosyaları desteklenmiyor. R\'de write.csv() ya da haven::write_sav() ile dışa aktarın.',
  rdata: 'R (.RData) dosyaları desteklenmiyor. R\'de write.csv() ya da haven::write_sav() ile dışa aktarın.',
  numbers: 'Apple Numbers dosyaları desteklenmiyor. Numbers\'ta Excel ya da CSV olarak dışa aktarın.',
  pdf: 'PDF dosyalarından veri okunamaz. Veriyi Excel ya da CSV olarak hazırlayın.',
  docx: 'Word dosyalarından veri okunamaz. Tabloyu Excel\'e kopyalayıp kaydedin ya da doğrudan yapıştırın.',
  doc: 'Word dosyalarından veri okunamaz. Tabloyu Excel\'e kopyalayıp kaydedin ya da doğrudan yapıştırın.',
};

/** BOM'suz UTF-16 metin mi? (sıfır baytlar UTF-16'dan geliyorsa çözülen metinde denetim karakteri olmaz) */
function utf16MetinMi(ornek) {
  const k = kodlamaAlgila(ornek).kodlama;
  if (!k.startsWith('utf-16')) return false;
  const metin = metinCoz(ornek.subarray(0, ornek.length - (ornek.length % 2)), { kodlama: k }).metin;
  return !/[\u0000-\u0008\u000e-\u001f]/.test(metin);
}

function uzanti(ad) {
  const m = /\.([^./\\]+)$/.exec(String(ad || ''));
  return m ? m[1].toLowerCase() : '';
}

function adGovdesi(ad) {
  const s = String(ad || '').split(/[\\/]/).pop();
  const i = s.lastIndexOf('.');
  return i > 0 ? s.slice(0, i) : s;
}

function baytlar(veri) {
  if (veri instanceof Uint8Array) return veri;
  if (veri instanceof ArrayBuffer) return new Uint8Array(veri);
  if (ArrayBuffer.isView(veri)) return new Uint8Array(veri.buffer, veri.byteOffset, veri.byteLength);
  throw new IstatistikHatasi('Dosya içeriği okunamadı (ArrayBuffer bekleniyordu).');
}

/**
 * Dosya türünü belirler: 'sav' | 'excel' | 'csv'. İmza önceliklidir: "$FL2"/"$FL3" → SPSS; ZIP ("PK\3\4") ve
 * OLE2 (D0 CF 11 E0) → Excel/ODS; sonra uzantı. Bilinen ama desteklenmeyen biçimlerde Türkçe hata fırlatır.
 * @param {string} ad
 * @param {ArrayBuffer|Uint8Array} veri
 * @returns {'sav'|'excel'|'csv'}
 */
export function dosyaTuru(ad, veri) {
  const b = baytlar(veri);
  const u = uzanti(ad);
  if (b.length >= 4 && b[0] === 0x24 && b[1] === 0x46 && b[2] === 0x4c && (b[3] === 0x32 || b[3] === 0x33)) return 'sav';
  if (b.length >= 4 && b[0] === 0x50 && b[1] === 0x4b && b[2] === 0x03 && b[3] === 0x04) {
    if (DESTEKLENMEYEN[u] && u !== 'numbers') throw new IstatistikHatasi(DESTEKLENMEYEN[u]);
    return 'excel';
  }
  if (b.length >= 4 && b[0] === 0xd0 && b[1] === 0xcf && b[2] === 0x11 && b[3] === 0xe0) {
    if (u === 'doc') throw new IstatistikHatasi(DESTEKLENMEYEN.doc);
    return 'excel';
  }
  if (b.length >= 4 && b[0] === 0x25 && b[1] === 0x50 && b[2] === 0x44 && b[3] === 0x46) throw new IstatistikHatasi(DESTEKLENMEYEN.pdf);
  if (u === 'sav' || u === 'zsav') {
    throw new IstatistikHatasi('Dosya bir SPSS veri dosyası (.sav) gibi görünmüyor: dosya bozuk ya da farklı bir biçimde kaydedilmiş.');
  }
  if (DESTEKLENMEYEN[u]) throw new IstatistikHatasi(DESTEKLENMEYEN[u]);
  if (EXCEL_UZANTILARI.has(u)) {
    // Eski "xls" uzantılı HTML/XML tablolar ve .fods: SheetJS okuyabilir
    return 'excel';
  }
  if (METIN_UZANTILARI.has(u) || u === '') return 'csv';
  // Bilinmeyen uzantı: metin gibi görünüyorsa CSV dene
  const ornek = b.subarray(0, 4096);
  let ikili = 0;
  for (const x of ornek) if (x < 9) ikili++;
  if (ikili > 0 && !utf16MetinMi(ornek)) {
    throw new IstatistikHatasi(`".${u}" uzantılı dosyalar desteklenmiyor. CSV, Excel (.xlsx, .xls, .ods) ya da SPSS (.sav) dosyası seçin.`);
  }
  return 'csv';
}

/**
 * Dosyayı okur ve VeriSeti ile okuma bilgisini döndürür.
 * `bilgi.tur`: 'csv' | 'excel' | 'sav'; CSV için kodlama/ayırıcı/başlık kararı, Excel için sayfa ve sayfa listesi,
 * SPSS için sıkıştırma, kodlama, belgeler, ağırlık değişkeni vb.
 * @param {string} ad   dosya adı (uzantı ve veri seti adı için)
 * @param {ArrayBuffer|Uint8Array} veri
 * @param {Object} [secenekler]  türe özgü seçenekler (bkz. CsvSecenekleri, ExcelSecenekleri, SavSecenekleri)
 * @returns {Promise<{veriSeti: import('./model.js').VeriSeti, bilgi: object, kitap?: ReturnType<typeof excelAc>}>}
 */
export async function dosyaOkuAyrintili(ad, veri, secenekler = {}) {
  const b = baytlar(veri);
  if (b.length === 0) throw new IstatistikHatasi('Dosya boş: okunacak veri yok.');
  const tur = dosyaTuru(ad, b);
  const ayar = { ad: adGovdesi(ad), ...secenekler };
  if (tur === 'sav') {
    const { veriSeti, bilgi } = await savOkuAyrintili(b, ayar);
    return { veriSeti, bilgi: { tur, ...bilgi } };
  }
  if (tur === 'excel') {
    const kitap = excelAc(b, ayar);
    const { veriSeti, bilgi } = kitap.okuAyrintili(ayar.sayfa);
    return { veriSeti, bilgi: { tur, ...bilgi }, kitap };
  }
  const { veriSeti, bilgi } = csvOkuAyrintili(b, ayar);
  return { veriSeti, bilgi: { tur, ...bilgi } };
}

/**
 * Tek giriş noktası: dosya adını ve içeriğini (ArrayBuffer) alır, türüne göre (SPSS .sav/.zsav, Excel/ODS, CSV/TSV/TXT)
 * okur ve VeriSeti döndürür. Hatalarda Türkçe mesajlı IstatistikHatasi fırlatır.
 *
 * Birden çok sayfalı Excel dosyasında (secenekler.sayfa verilmemişse) içinde veri olan ilk görünür sayfa okunur;
 * dönen VeriSeti'ne arayüz için sayılamayan (non-enumerable) iki yardımcı eklenir: `sayfalar` (string[]) ve
 * `sayfaOku(sayfaAdi, secenekler?) → Promise<VeriSeti>`. Bunlar JSON'a/kopyaya girmez; veri modeli değişmez.
 * @param {string} ad
 * @param {ArrayBuffer|Uint8Array} veri
 * @param {Object} [secenekler]
 * @returns {Promise<import('./model.js').VeriSeti>}
 */
export async function dosyaOku(ad, veri, secenekler = {}) {
  const { veriSeti, kitap } = await dosyaOkuAyrintili(ad, veri, secenekler);
  if (kitap && kitap.sayfalar.length > 1 && secenekler.sayfa === undefined) {
    Object.defineProperty(veriSeti, 'sayfalar', { value: kitap.sayfalar.slice(), enumerable: false, configurable: true });
    Object.defineProperty(veriSeti, 'sayfaOku', {
      value: async (sayfa, ek = {}) => kitap.oku(sayfa, ek),
      enumerable: false,
      configurable: true,
    });
  }
  return veriSeti;
}
