// Excel (.xlsx, .xlsm, .xlsb, .xls) ve OpenDocument (.ods) okuma. Ayrıştırma SheetJS ile yapılır; hücreler
// türleriyle (sayı, tarih, mantıksal, metin) tablo.js'e aktarılır, böylece CSV ile aynı başlık/tür kuralları geçerlidir.
// Tarihler: SheetJS (cellDates) Excel seri numarasını UTC tabanlı Date'e çevirir; saat dilimi kayması olmaz.
import { read } from 'xlsx';
import { IstatistikHatasi } from '../cekirdek/hata.js';
import { tablodanVeriSetiAyrintili } from './tablo.js';

function hucreAdresi(a) {
  const m = /^\$?([A-Z]+)\$?(\d+)$/i.exec(a);
  if (!m) return null;
  let c = 0;
  for (const ch of m[1].toUpperCase()) c = c * 26 + (ch.charCodeAt(0) - 64);
  return { r: Number(m[2]) - 1, c: c - 1 };
}

function aralikCoz(ref) {
  const [a, b] = String(ref).split(':');
  const s = hucreAdresi(a);
  const e = b ? hucreAdresi(b) : s;
  if (!s || !e) return null;
  return { s, e };
}

/**
 * Sayı biçim kodundan görünen ondalık basamak: "0.00" → 2, "#,##0.0" → 1, "0.0%" → 3 (değer oran olarak
 * tutulduğundan yüzde biçimi iki basamak ekler). "General"/bilimsel → null (değerden çıkarılır).
 */
export function bicimOndaligi(z) {
  if (typeof z !== 'string' || z === '' || /^general$/i.test(z)) return null;
  // İlk bölüm (pozitif sayılar); tırnaklı metin, [renk/koşul], kaçışlı karakterler atılır
  let bolum = '';
  let tirnak = false;
  for (let i = 0; i < z.length; i++) {
    const ch = z[i];
    if (ch === '"') tirnak = !tirnak;
    else if (tirnak) continue;
    else if (ch === '\\' || ch === '_' || ch === '*') i++;
    else if (ch === ';') break;
    else bolum += ch;
  }
  bolum = bolum.replace(/\[[^\]]*\]/g, '');
  if (/[eE][+-]/.test(bolum) || /^@$/.test(bolum.trim())) return null;
  const yuzde = bolum.includes('%') ? 2 : 0;
  const nokta = bolum.indexOf('.');
  if (nokta < 0) return /[0#?]/.test(bolum) ? yuzde : null;
  let k = 0;
  for (let i = nokta + 1; i < bolum.length && /[0#?]/.test(bolum[i]); i++) k++;
  return k + yuzde;
}

/** SheetJS'in kesirli gün → ms dönüşümündeki 1 ms'lik kesme hatasını tam saniyeye yuvarlar. */
function tarihMs(d) {
  const t = d instanceof Date ? d.getTime() : Number(d);
  if (!Number.isFinite(t)) return NaN;
  const sn = Math.round(t / 1000) * 1000;
  return Math.abs(t - sn) <= 1 ? sn : t;
}

function hucreCevir(c, sayac) {
  if (!c) return null;
  switch (c.t) {
    case 'z':
      return null;
    case 's':
    case 'str':
      return typeof c.v === 'string' ? c.v : c.v === undefined || c.v === null ? null : String(c.v);
    case 'n':
      if (typeof c.v !== 'number') {
        if (c.f) sayac.formulDegersiz++;
        return null;
      }
      return { t: 'sayi', v: c.v, w: c.w, d: bicimOndaligi(c.z) };
    case 'd': {
      const ms = tarihMs(c.v);
      return Number.isFinite(ms) ? { t: 'tarih', v: ms, w: c.w } : null;
    }
    case 'b':
      return { t: 'mantik', v: !!c.v, w: c.w };
    case 'e':
      if (c.v === undefined && c.f) sayac.formulDegersiz++;
      else sayac.hata++;
      return null;
    default:
      return c.v === undefined || c.v === null ? null : String(c.v);
  }
}

function calismaKitabiOku(veri) {
  const bytes = veri instanceof Uint8Array ? veri : new Uint8Array(veri);
  if (bytes.length === 0) throw new IstatistikHatasi('Dosya boş: okunacak veri yok.');
  try {
    return read(bytes, { type: 'array', cellDates: true, dense: true, cellNF: true, cellStyles: false, cellHTML: false, sheetStubs: false });
  } catch (e) {
    const m = String(e && e.message ? e.message : e);
    if (/password|encrypt|ECMA-376|RC4/i.test(m)) {
      throw new IstatistikHatasi('Excel dosyası parola korumalı. Excel\'de parolayı kaldırıp dosyayı yeniden kaydedin.', { ozgun: m });
    }
    throw new IstatistikHatasi('Excel dosyası okunamadı: dosya bozuk, eksik ya da desteklenmeyen bir biçimde.', { ozgun: m });
  }
}

/**
 * Bir çalışma sayfasını hücre tablosuna çevirir. Birleştirilmiş hücreler: bir satırın tek içeriği olan yatay
 * birleştirmeler (sayfa başlığı) dokunulmadan bırakılır (tablo.js açıklama satırı olarak atlar); diğer birleştirilmiş
 * aralıklar (ör. dikeyde birleştirilmiş grup adı) sol üst hücrenin değeriyle doldurulur.
 */
function sayfaTablosu(ws) {
  const sayac = { formulDegersiz: 0, hata: 0, birlesik: 0 };
  const aralik = ws && ws['!ref'] ? aralikCoz(ws['!ref']) : null;
  const veri = (ws && ws['!data']) || [];
  if (!aralik) return { satirlar: [], ofsetR: 0, ofsetC: 0, sayac };
  // !ref biçimlendirme yüzünden çok büyük olabilir; gerçek veri sınırına daralt
  const sonR = Math.min(aralik.e.r, veri.length - 1);
  let sonC = aralik.s.c - 1;
  for (let r = aralik.s.r; r <= sonR; r++) {
    const satir = veri[r];
    if (satir) {
      for (let c = Math.min(aralik.e.c, satir.length - 1); c > sonC; c--) {
        if (satir[c] && satir[c].t !== 'z') {
          sonC = c;
          break;
        }
      }
    }
  }
  const { s } = aralik;
  const satirlar = [];
  for (let r = s.r; r <= sonR; r++) {
    const kaynak = veri[r] || [];
    const satir = new Array(Math.max(0, sonC - s.c + 1)).fill(null);
    for (let c = s.c; c <= sonC; c++) satir[c - s.c] = hucreCevir(kaynak[c], sayac);
    satirlar.push(satir);
  }
  for (const m of ws['!merges'] || []) {
    const r0 = m.s.r - s.r;
    const c0 = m.s.c - s.c;
    if (r0 < 0 || r0 >= satirlar.length || c0 < 0) continue;
    const deger = satirlar[r0][c0];
    if (deger === null || deger === undefined) continue;
    const r1 = Math.min(m.e.r - s.r, satirlar.length - 1);
    const c1 = Math.min(m.e.c - s.c, Math.max(0, sonC - s.c));
    let yalniz = c1 > c0;
    for (let r = r0; r <= r1 && yalniz; r++) {
      for (let c = 0; c < satirlar[r].length; c++) {
        if ((c < c0 || c > c1 || r !== r0 || c !== c0) && satirlar[r][c] !== null && satirlar[r][c] !== undefined) {
          yalniz = false;
          break;
        }
      }
    }
    if (yalniz) continue;
    for (let r = r0; r <= r1; r++) for (let c = c0; c <= c1; c++) satirlar[r][c] = deger;
    sayac.birlesik++;
  }
  return { satirlar, ofsetR: s.r, ofsetC: s.c, sayac };
}

function sayfaGizliMi(wb, i) {
  const s = wb.Workbook && wb.Workbook.Sheets && wb.Workbook.Sheets[i];
  return !!(s && s.Hidden);
}

function sayfaDoluMu(ws) {
  return !!(ws && ws['!ref'] && (ws['!data'] || []).some((r) => r && r.some((c) => c && c.t !== 'z' && c.v !== undefined && c.v !== '')));
}

/**
 * @typedef {import('./tablo.js').TabloSecenekleri & {sayfa?: string|number}} ExcelSecenekleri
 */

/**
 * Çalışma kitabını bir kez ayrıştırır; sayfa listesi ve sayfa okuma işlevleri döndürür.
 * @param {ArrayBuffer|Uint8Array} veri
 * @param {{ad?: string}} [secenekler]
 * @returns {{sayfalar: string[], gizliSayfalar: string[], varsayilanSayfa: string,
 *   okuAyrintili: (sayfa?: string|number, secenekler?: ExcelSecenekleri) => {veriSeti: import('./model.js').VeriSeti, bilgi: object},
 *   oku: (sayfa?: string|number, secenekler?: ExcelSecenekleri) => import('./model.js').VeriSeti}}
 */
export function excelAc(veri, secenekler = {}) {
  const wb = calismaKitabiOku(veri);
  const sayfalar = (wb.SheetNames || []).slice();
  if (sayfalar.length === 0) throw new IstatistikHatasi('Excel dosyasında çalışma sayfası bulunamadı.');
  const gizliSayfalar = sayfalar.filter((_, i) => sayfaGizliMi(wb, i));
  const varsayilanSayfa =
    sayfalar.find((ad, i) => !sayfaGizliMi(wb, i) && sayfaDoluMu(wb.Sheets[ad])) ||
    sayfalar.find((ad) => sayfaDoluMu(wb.Sheets[ad])) ||
    sayfalar[0];

  function okuAyrintili(sayfa, ek = {}) {
    const ayar = { ...secenekler, ...ek };
    let ad = sayfa ?? ayar.sayfa;
    if (ad === undefined || ad === null || ad === '') ad = varsayilanSayfa;
    if (typeof ad === 'number') {
      if (!(ad >= 0 && ad < sayfalar.length)) throw new IstatistikHatasi(`Excel dosyasında ${ad + 1}. sayfa yok.`);
      ad = sayfalar[ad];
    }
    if (!sayfalar.includes(ad)) throw new IstatistikHatasi(`Excel dosyasında "${ad}" adlı sayfa yok.`);
    const { satirlar, ofsetR, ofsetC, sayac } = sayfaTablosu(wb.Sheets[ad]);
    if (satirlar.length === 0) throw new IstatistikHatasi(`"${ad}" sayfası boş.`);
    const uyarilar = [...(ayar.uyarilar || [])];
    if (sayac.birlesik) uyarilar.push(`${sayac.birlesik} birleştirilmiş hücre aralığı sol üst hücrenin değeriyle dolduruldu.`);
    if (sayac.hata) uyarilar.push(`${sayac.hata} hücrede Excel hata değeri (#SAYI/0!, #YOK vb.) vardı; eksik kabul edildi.`);
    if (sayac.formulDegersiz) {
      uyarilar.push(`${sayac.formulDegersiz} formül hücresinin hesaplanmış değeri dosyada kayıtlı değil; eksik kabul edildi. Dosyayı Excel'de açıp kaydederek değerleri hesaplatabilirsiniz.`);
    }
    const kitapAdi = String(ayar.ad ?? '');
    const { veriSeti, bilgi } = tablodanVeriSetiAyrintili(satirlar, {
      varsayilanAyirici: ',',
      ...ayar,
      ad: sayfalar.length > 1 ? (kitapAdi ? `${kitapAdi} – ${ad}` : ad) : kitapAdi,
      kaynak: 'excel',
      satirNumarasiOfseti: ofsetR + 1,
      sutunOfseti: ofsetC,
      uyarilar,
    });
    return { veriSeti, bilgi: { ...bilgi, sayfa: ad, sayfalar: sayfalar.slice() } };
  }

  return {
    sayfalar,
    gizliSayfalar,
    varsayilanSayfa,
    okuAyrintili,
    oku: (sayfa, ek) => okuAyrintili(sayfa, ek).veriSeti,
  };
}

/** Çalışma kitabındaki sayfa adları. */
export function excelSayfalari(veri) {
  return excelAc(veri).sayfalar;
}

/**
 * Excel/ODS dosyasından bir sayfayı okur (varsayılan: içinde veri olan ilk görünür sayfa).
 * @param {ArrayBuffer|Uint8Array} veri
 * @param {ExcelSecenekleri} [secenekler]
 */
export function excelOkuAyrintili(veri, secenekler = {}) {
  return excelAc(veri, secenekler).okuAyrintili(secenekler.sayfa, {});
}

/** Excel/ODS dosyasından bir sayfayı VeriSeti olarak okur. */
export function excelOku(veri, secenekler = {}) {
  return excelOkuAyrintili(veri, secenekler).veriSeti;
}
