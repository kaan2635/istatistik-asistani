// Karakter kodlaması algılama ve çözme. Türkçe Excel'in CSV çıktısı çoğunlukla windows-1254'tür;
// yeni dosyalar UTF-8 (BOM'lu/BOM'suz), bazı araçlar UTF-16 üretir. Algılama sırası:
// BOM → BOM'suz UTF-16 sezgisi → katı (fatal) UTF-8 denemesi → windows-1254.
import { IstatistikHatasi } from '../cekirdek/hata.js';

// windows-1254 (WHATWG Encoding Standard tablosu): 0x80–0x9F özel; 0xA0–0xFF Latin-1 ile aynı,
// yalnızca 0xD0 Ğ, 0xDD İ, 0xDE Ş, 0xF0 ğ, 0xFD ı, 0xFE ş farklı.
const CP1254_80 = [
  0x20ac, 0x0081, 0x201a, 0x0192, 0x201e, 0x2026, 0x2020, 0x2021, 0x02c6, 0x2030, 0x0160, 0x2039, 0x0152, 0x008d, 0x008e, 0x008f,
  0x0090, 0x2018, 0x2019, 0x201c, 0x201d, 0x2022, 0x2013, 0x2014, 0x02dc, 0x2122, 0x0161, 0x203a, 0x0153, 0x009d, 0x009e, 0x0178,
];
const CP1254_OZEL = { 0xd0: 0x011e, 0xdd: 0x0130, 0xde: 0x015e, 0xf0: 0x011f, 0xfd: 0x0131, 0xfe: 0x015f };

let cp1254Tablo = null;
function cp1254() {
  if (cp1254Tablo) return cp1254Tablo;
  const t = new Array(256);
  for (let b = 0; b < 256; b++) {
    let k = b;
    if (b >= 0x80 && b < 0xa0) k = CP1254_80[b - 0x80];
    else if (CP1254_OZEL[b]) k = CP1254_OZEL[b];
    t[b] = String.fromCharCode(k);
  }
  cp1254Tablo = t;
  return t;
}

function cp1254Coz(bytes) {
  const t = cp1254();
  let s = '';
  const PARCA = 8192;
  for (let i = 0; i < bytes.length; i += PARCA) {
    const son = Math.min(bytes.length, i + PARCA);
    const parca = new Array(son - i);
    for (let j = i; j < son; j++) parca[j - i] = t[bytes[j]];
    s += parca.join('');
  }
  return s;
}

/** Bir TextDecoder etiketini (ya da SPSS/IANA adını) bizim standart adımıza çevirir. */
export function kodlamaAdiDuzelt(ad) {
  const k = String(ad || '').trim().toLowerCase().replace(/_/g, '-');
  if (k === '' ) return null;
  if (k === 'utf8' || k === 'utf-8' || k === 'unicode-1-1-utf-8' || k === 'cp65001') return 'utf-8';
  if (['windows-1254', 'cp1254', 'x-cp1254', 'iso-8859-9', 'iso8859-9', 'iso88599', 'latin5', 'l5', 'turkish'].includes(k)) {
    return 'windows-1254';
  }
  if (k === 'utf-16' || k === 'utf-16le') return 'utf-16le';
  if (k === 'utf-16be') return 'utf-16be';
  if (/^(cp|windows-?)125[0-8]$/.test(k)) return `windows-${k.slice(-4)}`;
  return k;
}

/**
 * Verilen kodlama için bayt → metin çözücü üretir. windows-1254 kendi tablomuzla çözülür (her ortamda aynı sonuç);
 * diğerleri TextDecoder ile. Desteklenmeyen kodlamada IstatistikHatasi fırlatır.
 * @param {string} kodlama
 * @returns {(bytes: Uint8Array) => string}
 */
export function cozucuOlustur(kodlama) {
  const ad = kodlamaAdiDuzelt(kodlama);
  if (ad === 'windows-1254') return cp1254Coz;
  let td;
  try {
    td = new TextDecoder(ad, { ignoreBOM: true });
  } catch {
    throw new IstatistikHatasi(`Desteklenmeyen karakter kodlaması: ${kodlama}`);
  }
  return (b) => td.decode(b);
}

/** Kodlama bu ortamda çözülebiliyor mu? */
export function kodlamaDestekleniyor(kodlama) {
  try {
    cozucuOlustur(kodlama);
    return true;
  } catch {
    return false;
  }
}

/** Baytlar geçerli UTF-8 mi? (sondaki yarım kalmış çok baytlı karakter geçerli sayılır: kesilmiş dosya) */
export function gecerliUtf8Mi(bytes) {
  try {
    new TextDecoder('utf-8', { fatal: true }).decode(bytes, { stream: true });
    return true;
  } catch {
    return false;
  }
}

function asciiMi(bytes) {
  for (let i = 0; i < bytes.length; i++) if (bytes[i] >= 0x80) return false;
  return true;
}

/**
 * Bayt dizisinin kodlamasını algılar.
 * @param {Uint8Array} bytes
 * @returns {{kodlama: 'utf-8'|'utf-16le'|'utf-16be'|'windows-1254', bom: boolean, ascii: boolean}}
 */
export function kodlamaAlgila(bytes) {
  const n = bytes.length;
  if (n >= 3 && bytes[0] === 0xef && bytes[1] === 0xbb && bytes[2] === 0xbf) return { kodlama: 'utf-8', bom: true, ascii: false };
  if (n >= 2 && bytes[0] === 0xff && bytes[1] === 0xfe) return { kodlama: 'utf-16le', bom: true, ascii: false };
  if (n >= 2 && bytes[0] === 0xfe && bytes[1] === 0xff) return { kodlama: 'utf-16be', bom: true, ascii: false };
  // BOM'suz UTF-16: ASCII ağırlıklı metinde her iki bayttan biri sıfırdır.
  const ornek = Math.min(n - (n % 2), 4096);
  if (ornek >= 4) {
    let tekSifir = 0;
    let ciftSifir = 0;
    for (let i = 0; i < ornek; i += 2) {
      if (bytes[i] === 0) ciftSifir++;
      if (bytes[i + 1] === 0) tekSifir++;
    }
    const ciftler = ornek / 2;
    if (tekSifir > 0.4 * ciftler && ciftSifir < 0.05 * ciftler) return { kodlama: 'utf-16le', bom: false, ascii: false };
    if (ciftSifir > 0.4 * ciftler && tekSifir < 0.05 * ciftler) return { kodlama: 'utf-16be', bom: false, ascii: false };
  }
  if (asciiMi(bytes)) return { kodlama: 'utf-8', bom: false, ascii: true };
  if (gecerliUtf8Mi(bytes)) return { kodlama: 'utf-8', bom: false, ascii: false };
  return { kodlama: 'windows-1254', bom: false, ascii: false };
}

const KODLAMA_ADLARI = {
  'utf-8': 'UTF-8',
  'utf-16le': 'UTF-16 (little-endian)',
  'utf-16be': 'UTF-16 (big-endian)',
  'windows-1254': 'Windows-1254 (Türkçe)',
};

/** Kullanıcıya gösterilecek kodlama adı. */
export function kodlamaGorunenAd(kodlama) {
  return KODLAMA_ADLARI[kodlama] || kodlama;
}

/**
 * Baytları metne çevirir. `kodlama` verilmezse kodlamaAlgila() kullanılır. BOM metinden çıkarılır.
 * @param {Uint8Array|ArrayBuffer} veri
 * @param {{kodlama?: string}} [secenekler]
 * @returns {{metin: string, kodlama: string, bom: boolean}}
 */
export function metinCoz(veri, secenekler = {}) {
  const bytes = veri instanceof Uint8Array ? veri : new Uint8Array(veri);
  let kodlama;
  let bom = false;
  if (secenekler.kodlama && secenekler.kodlama !== 'otomatik') {
    kodlama = kodlamaAdiDuzelt(secenekler.kodlama);
    bom = (kodlama === 'utf-8' && bytes[0] === 0xef && bytes[1] === 0xbb && bytes[2] === 0xbf) ||
      (kodlama === 'utf-16le' && bytes[0] === 0xff && bytes[1] === 0xfe) ||
      (kodlama === 'utf-16be' && bytes[0] === 0xfe && bytes[1] === 0xff);
  } else {
    ({ kodlama, bom } = kodlamaAlgila(bytes));
  }
  const govde = bom ? bytes.subarray(kodlama === 'utf-8' ? 3 : 2) : bytes;
  let metin;
  if (kodlama === 'utf-8') {
    // Kesik dosyada sondaki yarım karakter U+FFFD olur; geri kalan katı çözülür.
    metin = new TextDecoder('utf-8', { ignoreBOM: true }).decode(govde);
  } else {
    metin = cozucuOlustur(kodlama)(govde);
  }
  return { metin, kodlama, bom };
}
