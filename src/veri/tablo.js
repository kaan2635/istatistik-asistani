// Hücre tablosundan (satır × sütun) VeriSeti kurar. CSV, Excel ve yapıştırılan veri aynı yoldan geçer:
// boş satır/sütun temizliği → başlık/açıklama satırı ayıklama → başlık satırı algılama → sütun türü
// (sayı / tarih / metin) ve ondalık ayırıcı kararı → benzersiz adlar.
//
// Sütun türü kuralı (açık ve sabit): eksik olmayan hücrelerin en az %95'i (SAYISAL_ESIK) sayıya
// çevrilebiliyorsa sütun sayısaldır; çevrilemeyen azınlık eksik (null) yapılır ve uyarı yazılır.
// Değilse aynı eşikle tarih denenir; o da tutmazsa metin olur.
import { IstatistikHatasi } from '../cekirdek/hata.js';
import { adAnahtari, benzersizAd } from './model.js';

/** Eksik kabul edilen hücre metinleri (kırpıldıktan sonra birebir karşılaştırılır). Türkçe Excel hata kodları dahil. */
export const VARSAYILAN_EKSIK_DEGERLER = Object.freeze([
  '', 'NA', 'N/A', 'n/a', 'na', 'NaN', 'nan', 'NAN', '.', '-', '—', '–', '?', 'null', 'NULL', 'Null',
  '#N/A', '#DIV/0!', '#NULL!', '#VALUE!', '#REF!', '#NUM!', '#NAME?',
  '#YOK', '#SAYI/0!', '#BOŞ!', '#DEĞER!', '#BAŞV!', '#SAYI!', '#AD?',
]);

/** Bir sütunun sayısal (ya da tarih) sayılması için eksik olmayan hücreler içinde çevrilebilenlerin en az oranı. */
export const SAYISAL_ESIK = 0.95;

const UYARI_ORNEK = 5; // uyarılarda gösterilecek en fazla örnek

// ---------------------------------------------------------------- sayı ayrıştırma

// Ondalık nokta: binlik ayırıcı virgül, boşluk ya da kesme işareti. Ondalık virgül: binlik nokta ya da boşluk.
// Gruplu yazımda ilk grup 0 ile başlayamaz ("0,125" binlik gruplu olamaz → kesin ondalık virgül).
const RE_NOKTA = /^(\d+|[1-9]\d{0,2}(?:,\d{3})+|[1-9]\d{0,2}(?:[   ']\d{3})+)?(?:\.(\d*))?(?:[eE]([+-]?\d{1,3}))?$/;
const RE_VIRGUL = /^(\d+|[1-9]\d{0,2}(?:\.\d{3})+|[1-9]\d{0,2}(?:[   ]\d{3})+)?(?:,(\d*))?(?:[eE]([+-]?\d{1,3}))?$/;
const RE_TAMSAYI = /^[+-]?\d{1,15}$/;

let sonBasamak = 0;
let sonYuzde = false;

function bosluk(c) {
  return c === 32 || c === 9 || c === 0xa0 || c === 0x202f;
}

/**
 * Kırpılmış bir metni verilen ondalık ayırıcıya göre sayıya çevirir; çevrilemezse NaN.
 * Kabul edilenler: işaret (+, -, U+2212 eksi), binlik ayırıcılar, bilimsel gösterim ("1,5E-03"),
 * yüzde ("%12,5" ya da "12,5 %" → 0.125; Excel'in yüzde hücre değeriyle aynı). Sonuç, ondalık metnin
 * en yakın double karşılığıdır (Number() ile tek adımda dönüştürülür).
 * @param {string} s
 * @param {','|'.'} ayirici  ondalık ayırıcı
 * @returns {number}
 */
export function sayiAyristir(s, ayirici) {
  let i = 0;
  let n = s.length;
  if (n === 0) return NaN;
  sonYuzde = false;
  if (RE_TAMSAYI.test(s)) {
    sonBasamak = 0;
    return Number(s);
  }
  let yuzde = false;
  if (s.charCodeAt(0) === 37) {
    yuzde = true;
    i = 1;
    while (i < n && bosluk(s.charCodeAt(i))) i++;
  } else if (s.charCodeAt(n - 1) === 37) {
    yuzde = true;
    n--;
    while (n > i && bosluk(s.charCodeAt(n - 1))) n--;
  }
  let negatif = false;
  const c = s.charCodeAt(i);
  if (c === 43) i++;
  else if (c === 45 || c === 0x2212) {
    negatif = true;
    i++;
  }
  if (i >= n) return NaN;
  const m = (ayirici === ',' ? RE_VIRGUL : RE_NOKTA).exec(i === 0 && n === s.length ? s : s.slice(i, n));
  if (!m) return NaN;
  const tam = m[1] || '';
  const kesir = m[2] || '';
  if (tam === '' && kesir === '') return NaN;
  let us = m[3] ? parseInt(m[3], 10) : 0;
  if (yuzde) us -= 2;
  let metin = (negatif ? '-' : '') + (tam === '' ? '0' : tam.replace(/[^0-9]/g, '')) + (kesir ? '.' + kesir : '');
  if (us !== 0) metin += 'e' + us;
  sonBasamak = Math.max(0, kesir.length - us);
  sonYuzde = yuzde;
  return Number(metin);
}

/**
 * Bir sayının gösterim ondalık basamağı: 15 anlamlı basamağa yuvarlanmış en kısa yazımındaki
 * ondalık sayısı (0.1+0.2 → 1). En fazla 15.
 */
export function ondalikCikar(v) {
  if (!Number.isFinite(v) || Number.isInteger(v)) return 0;
  const s = String(Number(v.toPrecision(15)));
  const e = s.indexOf('e');
  const mant = e >= 0 ? s.slice(0, e) : s;
  const us = e >= 0 ? Number(s.slice(e + 1)) : 0;
  const nokta = mant.indexOf('.');
  const k = nokta < 0 ? 0 : mant.length - nokta - 1;
  return Math.min(15, Math.max(0, k - us));
}

// ---------------------------------------------------------------- tarih ayrıştırma

const RE_ISO = /^(\d{4})([-/.])(\d{1,2})\2(\d{1,2})(?:[T ](\d{1,2}):(\d{2})(?::(\d{2})(?:[.,](\d{1,9}))?)?)?\s*(Z|[+-]\d{2}(?::?\d{2})?)?$/i;
const RE_GAY = /^(\d{1,2})([./-])(\d{1,2})\2(\d{4})(?:[ T]+(\d{1,2}):(\d{2})(?::(\d{2})(?:[.,](\d{1,9}))?)?)?$/;

/** @typedef {{iso: boolean, ayirici: string, yil: number, b1: number, b2: number, sa: number, dk: number, sn: number, ms: number, ofset: number}} TarihParcalari */

/** Tarih benzeri metni parçalara ayırır (gün/ay sırası henüz çözülmemiş); değilse null. */
function tarihParcala(s) {
  const c = s.charCodeAt(0);
  if (c < 48 || c > 57) return null;
  let m = RE_ISO.exec(s);
  let iso = true;
  if (!m) {
    m = RE_GAY.exec(s);
    iso = false;
    if (!m) return null;
  }
  let ofset = 0;
  if (iso && m[9] && m[9].toUpperCase() !== 'Z') {
    const t = m[9].replace(':', '');
    ofset = (t[0] === '-' ? -1 : 1) * (Number(t.slice(1, 3)) * 60 + (t.length > 3 ? Number(t.slice(3, 5)) : 0));
  }
  const kesir = m[8] ? Math.round(Number('0.' + m[8]) * 1000) : 0;
  return {
    iso,
    ayirici: m[2],
    yil: Number(iso ? m[1] : m[4]),
    b1: Number(iso ? m[3] : m[1]),
    b2: Number(iso ? m[4] : m[3]),
    sa: m[5] ? Number(m[5]) : 0,
    dk: m[6] ? Number(m[6]) : 0,
    sn: m[7] ? Number(m[7]) : 0,
    ms: kesir,
    ofset,
  };
}

function ayinGunu(yil, ay) {
  if (ay === 2) return (yil % 4 === 0 && yil % 100 !== 0) || yil % 400 === 0 ? 29 : 28;
  return [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][ay - 1];
}

/** Proleptik Gregoryen takvimde UTC milisaniye (0–99 yılları dahil doğru). */
export function utcMs(yil, ay, gun, sa = 0, dk = 0, sn = 0, ms = 0) {
  const d = new Date(0);
  d.setUTCFullYear(yil, ay - 1, gun);
  d.setUTCHours(sa, dk, sn, ms);
  return d.getTime();
}

function parcadanMs(p, sira) {
  let ay;
  let gun;
  if (p.iso) {
    ay = p.b1;
    gun = p.b2;
  } else if (p.ayirici === '.' || sira !== 'ay-gun') {
    gun = p.b1;
    ay = p.b2;
  } else {
    ay = p.b1;
    gun = p.b2;
  }
  if (ay < 1 || ay > 12 || gun < 1 || gun > ayinGunu(p.yil, ay)) return NaN;
  if (p.sa > 23 || p.dk > 59 || p.sn > 59) return NaN;
  return utcMs(p.yil, ay, gun, p.sa, p.dk, p.sn, p.ms) - p.ofset * 60000;
}

/**
 * Tarih metnini UTC milisaniyeye çevirir; tarih değilse NaN. Biçimler: ISO "2024-03-15", "2024-03-15T14:30:00(Z|+03:00)",
 * Türkçe sıra "15.03.2024", "15/03/2024", "15-03-2024" (isteğe bağlı " 14:30[:00]"). Saat dilimi verilmemişse
 * duvar saati UTC kabul edilir. Eğik çizgili yazımda `sira` 'ay-gun' verilirse ABD sırası (03/15/2024) kullanılır.
 * @param {string} s
 * @param {'gun-ay'|'ay-gun'} [sira]
 */
export function tarihAyristir(s, sira = 'gun-ay') {
  const p = tarihParcala(String(s).trim());
  return p ? parcadanMs(p, sira) : NaN;
}

// ---------------------------------------------------------------- hücreler

/**
 * Tabloya girebilecek hücre: metin (çözümlenecek), sayı, mantıksal, Date (UTC anı), null/undefined (boş) ya da
 * türü önceden belli bir nesne (Excel): {t: 'sayi'|'tarih'|'mantik'|'metin', v, w?: görünen metin, d?: ondalık}.
 * @typedef {string|number|boolean|Date|null|undefined|{t: string, v: any, w?: string, d?: number|null}} Hucre
 */

const S_BOS = 0;
const S_METIN = 1;
const S_SAYI = 2;
const S_TARIH = 3;
const S_MANTIK = 4;
const S_EKSIK = 5;

function hucreBosMu(h) {
  if (h === null || h === undefined) return true;
  if (typeof h === 'string') {
    for (let i = 0; i < h.length; i++) if (!bosluk(h.charCodeAt(i)) && h.charCodeAt(i) !== 10 && h.charCodeAt(i) !== 13) return false;
    return true;
  }
  if (typeof h === 'number') return Number.isNaN(h);
  if (h instanceof Date) return Number.isNaN(h.getTime());
  if (typeof h === 'object') return h.v === null || h.v === undefined || (h.t === 'metin' && hucreBosMu(h.v));
  return false;
}

/** Hücrenin kaba sınıfı (başlık algılamada kullanılır). */
function hucreSinifi(h, eksikler) {
  if (hucreBosMu(h)) return S_BOS;
  if (typeof h === 'number') return S_SAYI;
  if (typeof h === 'boolean') return S_MANTIK;
  if (h instanceof Date) return S_TARIH;
  if (typeof h === 'object') {
    if (h.t === 'sayi') return S_SAYI;
    if (h.t === 'tarih') return S_TARIH;
    if (h.t === 'mantik') return S_MANTIK;
    return metinSinifi(String(h.v).trim(), eksikler);
  }
  return metinSinifi(String(h).trim(), eksikler);
}

function metinSinifi(s, eksikler) {
  if (eksikler.has(s)) return S_EKSIK;
  if (!Number.isNaN(sayiAyristir(s, '.')) || !Number.isNaN(sayiAyristir(s, ','))) return S_SAYI;
  if (tarihParcala(s)) return S_TARIH;
  return S_METIN;
}

function isoTarihMetni(ms) {
  if (!Number.isFinite(ms)) return '';
  const s = new Date(ms).toISOString();
  return s.endsWith('T00:00:00.000Z') ? s.slice(0, 10) : s.replace('.000Z', '').replace('T', ' ').replace('Z', '');
}

/** Hücrenin görünen metni (başlık adları ve metin sütunları için). */
export function hucreMetni(h) {
  if (h === null || h === undefined) return '';
  if (typeof h === 'string') return h.trim();
  if (typeof h === 'number') return Number.isFinite(h) ? String(h) : '';
  if (typeof h === 'boolean') return h ? 'Doğru' : 'Yanlış';
  if (h instanceof Date) return isoTarihMetni(h.getTime());
  if (typeof h === 'object') {
    if (typeof h.w === 'string' && h.w.trim() !== '') return h.w.trim();
    if (h.t === 'tarih') return isoTarihMetni(h.v);
    if (h.t === 'mantik') return h.v ? 'Doğru' : 'Yanlış';
    return h.v === null || h.v === undefined ? '' : String(h.v).trim();
  }
  return String(h).trim();
}

// ---------------------------------------------------------------- sütun çözümleme

function ornekListesi(liste) {
  const parca = liste.slice(0, UYARI_ORNEK).map(([satir, deger]) => `satır ${satir} ("${deger}")`);
  return parca.join(', ') + (liste.length > UYARI_ORNEK ? ` ve ${liste.length - UYARI_ORNEK} tane daha` : '');
}

/**
 * Bir sütunun ilk taraması: sınıflar, iki ondalık ayırıcı varsayımı altında çevrilebilen metin sayıları.
 * Kesin kanıt: yalnız bir ayırıcıyla çevrilebilen ya da iki ayırıcıyla farklı değer veren ("12,5", "1.234,5")
 * hücreler; "1,234" gibi iki yorumu da geçerli olanlar belirsizdir.
 */
function sutunTara(hucreler, eksikler) {
  const n = hucreler.length;
  const t = {
    sinif: new Uint8Array(n),
    metin: new Array(n),
    dolu: 0,
    sayiTipli: 0,
    tarihTipli: 0,
    mantik: 0,
    sayiN: 0,
    sayiV: 0,
    kesinN: 0,
    kesinV: 0,
    belirsiz: 0,
    tarihMetin: 0,
    gunAy: 0,
    ayGun: 0,
  };
  for (let i = 0; i < n; i++) {
    const h = hucreler[i];
    if (hucreBosMu(h)) continue;
    let s = null;
    if (typeof h === 'string') s = h.trim();
    else if (typeof h === 'number') {
      t.sinif[i] = S_SAYI;
      t.sayiTipli++;
      t.dolu++;
      continue;
    } else if (typeof h === 'boolean') {
      t.sinif[i] = S_MANTIK;
      t.mantik++;
      t.dolu++;
      continue;
    } else if (h instanceof Date) {
      t.sinif[i] = S_TARIH;
      t.tarihTipli++;
      t.dolu++;
      continue;
    } else if (typeof h === 'object') {
      if (h.t === 'sayi' || h.t === 'tarih' || h.t === 'mantik') {
        const sinif = h.t === 'sayi' ? S_SAYI : h.t === 'tarih' ? S_TARIH : S_MANTIK;
        t.sinif[i] = sinif;
        if (sinif === S_SAYI) t.sayiTipli++;
        else if (sinif === S_TARIH) t.tarihTipli++;
        else t.mantik++;
        t.dolu++;
        continue;
      }
      s = String(h.v).trim();
    } else s = String(h).trim();
    if (eksikler.has(s)) {
      t.sinif[i] = S_EKSIK;
      continue;
    }
    t.dolu++;
    t.sinif[i] = S_METIN;
    t.metin[i] = s;
    const vN = sayiAyristir(s, '.');
    const vV = sayiAyristir(s, ',');
    const okN = !Number.isNaN(vN);
    const okV = !Number.isNaN(vV);
    if (okN) t.sayiN++;
    if (okV) t.sayiV++;
    if (okN && !okV) t.kesinN++;
    else if (okV && !okN) t.kesinV++;
    else if (okN && okV && vN !== vV) t.belirsiz++;
    if (!okN && !okV) {
      const p = tarihParcala(s);
      if (p) {
        t.tarihMetin++;
        if (!p.iso && p.ayirici !== '.') {
          if (p.b1 > 12 && p.b2 <= 12) t.gunAy++;
          else if (p.b2 > 12 && p.b1 <= 12) t.ayGun++;
        }
      }
    }
  }
  return t;
}

function hucreSayisi(h) {
  if (typeof h === 'number') return h;
  if (typeof h === 'boolean') return h ? 1 : 0;
  if (typeof h === 'object' && h !== null) return h.t === 'mantik' ? (h.v ? 1 : 0) : h.v;
  return NaN;
}

function hucreOndalik(h) {
  if (typeof h === 'number') return ondalikCikar(h);
  if (typeof h === 'object' && h !== null && !(h instanceof Date)) {
    if (typeof h.d === 'number') return h.d;
    if (h.t === 'sayi') return ondalikCikar(h.v);
  }
  return 0;
}

function hucreTarihi(h) {
  if (h instanceof Date) return h.getTime();
  if (typeof h === 'object' && h !== null && h.t === 'tarih') return h.v instanceof Date ? h.v.getTime() : h.v;
  return NaN;
}

/**
 * Taranan sütunu türüne karar verip değerlere çevirir.
 * @returns {{veriTuru: 'sayi'|'tarih'|'metin', degerler: any[], ondalik: number|null, degerEtiketleri: object|null,
 *   ayirici: ','|'.'|null, uyarilar: string[]}}
 */
function sutunuCoz(ad, hucreler, t, ayar, satirNo) {
  const n = hucreler.length;
  const uyarilar = [];
  let ayirici = ayar.ondalikAyirici;
  if (ayirici !== ',' && ayirici !== '.') {
    if (t.kesinV > t.kesinN) ayirici = ',';
    else if (t.kesinN > t.kesinV) ayirici = '.';
    else ayirici = ayar.varsayilanAyirici;
  }
  const sayisal = t.sayiTipli + t.mantik + (ayirici === ',' ? t.sayiV : t.sayiN);
  const degerler = new Array(n);

  if (t.dolu === 0) {
    degerler.fill(null);
    if (n > 0) uyarilar.push(`"${ad}" sütununun tüm hücreleri boş ya da eksik.`);
    return { veriTuru: 'sayi', degerler, ondalik: null, degerEtiketleri: null, ayirici: null, uyarilar };
  }

  if (sayisal >= SAYISAL_ESIK * t.dolu) {
    let ondalik = 0;
    let yuzdeVar = false;
    const cevrilemeyen = [];
    for (let i = 0; i < n; i++) {
      const sinif = t.sinif[i];
      if (sinif === S_METIN) {
        const v = sayiAyristir(t.metin[i], ayirici);
        if (Number.isNaN(v)) {
          degerler[i] = null;
          cevrilemeyen.push([satirNo(i), t.metin[i]]);
        } else {
          degerler[i] = v;
          if (sonYuzde) yuzdeVar = true;
          if (sonBasamak > ondalik) ondalik = sonBasamak;
        }
      } else if (sinif === S_SAYI || sinif === S_MANTIK) {
        const h = hucreler[i];
        const v = hucreSayisi(h);
        degerler[i] = Number.isFinite(v) ? v : null;
        const d = hucreOndalik(h);
        if (d > ondalik) ondalik = d;
      } else if (sinif === S_TARIH) {
        degerler[i] = null;
        cevrilemeyen.push([satirNo(i), hucreMetni(hucreler[i])]);
      } else degerler[i] = null;
    }
    if (cevrilemeyen.length) {
      uyarilar.push(`"${ad}" sütunu sayısal kabul edildi; sayıya çevrilemeyen ${cevrilemeyen.length} değer eksik sayıldı: ${ornekListesi(cevrilemeyen)}.`);
    }
    if (yuzdeVar) uyarilar.push(`"${ad}" sütunundaki yüzde değerleri oran olarak okundu (%45 → 0,45).`);
    if (t.belirsiz > 0 && t.kesinN === 0 && t.kesinV === 0 && ayar.ondalikAyirici !== ',' && ayar.ondalikAyirici !== '.') {
      uyarilar.push(`"${ad}" sütununda ondalık ayırıcı belirsiz ("1,234" gibi); ${ayirici === ',' ? 'virgül' : 'nokta'} ondalık ayırıcı kabul edildi.`);
    }
    let degerEtiketleri = null;
    if (t.mantik > 0 && t.mantik === t.dolu) degerEtiketleri = { 0: 'Yanlış', 1: 'Doğru' };
    return { veriTuru: 'sayi', degerler, ondalik: Math.min(ondalik, 15), degerEtiketleri, ayirici, uyarilar };
  }

  if (t.tarihTipli + t.tarihMetin >= SAYISAL_ESIK * t.dolu) {
    let sira = ayar.tarihSirasi;
    if (sira !== 'gun-ay' && sira !== 'ay-gun') sira = t.ayGun > 0 && t.gunAy === 0 ? 'ay-gun' : 'gun-ay';
    const cevrilemeyen = [];
    for (let i = 0; i < n; i++) {
      const sinif = t.sinif[i];
      let v = NaN;
      if (sinif === S_TARIH) v = hucreTarihi(hucreler[i]);
      else if (sinif === S_METIN) v = tarihAyristir(t.metin[i], sira);
      else if (sinif === S_BOS || sinif === S_EKSIK) {
        degerler[i] = null;
        continue;
      }
      if (Number.isFinite(v)) degerler[i] = v;
      else {
        degerler[i] = null;
        cevrilemeyen.push([satirNo(i), sinif === S_METIN ? t.metin[i] : hucreMetni(hucreler[i])]);
      }
    }
    if (cevrilemeyen.length) {
      uyarilar.push(`"${ad}" sütunu tarih kabul edildi; tarihe çevrilemeyen ${cevrilemeyen.length} değer eksik sayıldı: ${ornekListesi(cevrilemeyen)}.`);
    }
    if (sira === 'ay-gun' && ayar.tarihSirasi !== 'ay-gun') {
      uyarilar.push(`"${ad}" sütunundaki tarihler ay/gün/yıl sırasında (ABD biçimi) okundu.`);
    }
    return { veriTuru: 'tarih', degerler, ondalik: null, degerEtiketleri: null, ayirici: null, uyarilar };
  }

  for (let i = 0; i < n; i++) {
    const sinif = t.sinif[i];
    if (sinif === S_BOS || sinif === S_EKSIK) degerler[i] = null;
    else if (sinif === S_METIN) degerler[i] = t.metin[i];
    else {
      const m = hucreMetni(hucreler[i]);
      degerler[i] = m === '' ? null : m;
    }
  }
  return { veriTuru: 'metin', degerler, ondalik: null, degerEtiketleri: null, ayirici: null, uyarilar };
}

// ---------------------------------------------------------------- başlık algılama

/**
 * İlk satır başlık mı? Kanıt sütun sütun toplanır: ilk hücresi metin, gövdesi çoğunlukla sayı/tarih olan sütun
 * başlık lehine; ilk hücresi de gövdesi de sayı/tarih olan sütun aleyhine sayılır. Eşitlikte (ör. tümü metin
 * sütunlar) ilk satırın tüm hücreleri dolu ve benzersizse ve değerleri sütunların çoğunda gövdede geçmiyorsa başlıktır.
 */
function baslikVarMi(satirlar, sutunlar, eksikler) {
  if (satirlar.length === 0) return false;
  const ilk = satirlar[0];
  const ornek = satirlar.slice(1, 201);
  let lehine = 0;
  let aleyhine = 0;
  let ilkDolu = 0;
  let ilkSayisal = 0;
  const metinSutunlari = [];
  for (const j of sutunlar) {
    const s0 = hucreSinifi(ilk[j], eksikler);
    if (s0 !== S_BOS) ilkDolu++;
    if (s0 === S_SAYI || s0 === S_TARIH || s0 === S_MANTIK) ilkSayisal++;
    let dolu = 0;
    let sayisal = 0;
    for (const r of ornek) {
      const s = hucreSinifi(r[j], eksikler);
      if (s === S_BOS || s === S_EKSIK) continue;
      dolu++;
      if (s === S_SAYI || s === S_TARIH || s === S_MANTIK) sayisal++;
    }
    const sayisalGovde = dolu > 0 && sayisal >= 0.5 * dolu;
    if (s0 === S_METIN) {
      if (sayisalGovde) lehine++;
      else metinSutunlari.push(j);
    } else if (s0 === S_SAYI || s0 === S_TARIH || s0 === S_MANTIK) {
      if (sayisalGovde || dolu === 0) aleyhine++;
    }
  }
  if (lehine !== aleyhine) return lehine > aleyhine;
  if (ilkSayisal > 0) return false;
  if (satirlar.length === 1) return ilkDolu > 0;
  if (ilkDolu < sutunlar.length || metinSutunlari.length === 0) return false;
  const anahtarlar = sutunlar.map((j) => adAnahtari(hucreMetni(ilk[j])));
  if (new Set(anahtarlar).size !== anahtarlar.length) return false;
  let govdedeGecen = 0;
  for (const j of metinSutunlari) {
    const a = adAnahtari(hucreMetni(ilk[j]));
    if (ornek.some((r) => adAnahtari(hucreMetni(r[j])) === a)) govdedeGecen++;
  }
  return govdedeGecen * 2 < metinSutunlari.length;
}

// ---------------------------------------------------------------- ana işlev

/**
 * @typedef {Object} TabloSecenekleri
 * @property {string} [ad]                      Veri seti adı
 * @property {import('./model.js').VeriKaynagi} [kaynak]
 * @property {'otomatik'|boolean} [baslik]       İlk (kalan) satır başlık mı? Varsayılan otomatik algılama.
 * @property {number} [atlanacakSatir]          Baştan atlanacak satır sayısı (başlık/açıklama satırları için)
 * @property {string[]} [eksikDegerler]         Eksik sayılacak metinler (varsayılan VARSAYILAN_EKSIK_DEGERLER)
 * @property {'otomatik'|','|'.'} [ondalikAyirici]  Sayılardaki ondalık ayırıcı; otomatikte sütun sütun karar verilir
 * @property {','|'.'} [varsayilanAyirici]      Kanıt yoksa (tüm değerler "1,234" gibi belirsizse) kullanılacak ayırıcı
 * @property {'otomatik'|'gun-ay'|'ay-gun'} [tarihSirasi]  Eğik çizgili tarihlerde gün/ay sırası
 * @property {boolean} [duzensizSatirUyarisi]   Alan sayısı farklı satırlar için uyarı (CSV)
 * @property {number} [satirNumarasiOfseti]     Uyarılardaki satır numarasına eklenecek değer (varsayılan 1)
 * @property {number} [sutunOfseti]             kaynakSutun'a eklenecek değer (Excel aralığı A'dan başlamıyorsa)
 * @property {string[]} [uyarilar]              Önceki adımlardan gelen uyarılar (başa eklenir)
 */

/**
 * Hücre tablosunu VeriSeti'ne çevirir.
 * @param {Hucre[][]} satirlar
 * @param {TabloSecenekleri} [secenekler]
 * @returns {{veriSeti: import('./model.js').VeriSeti, bilgi: {baslik: boolean, atlananSatirlar: number,
 *   bosSatirlar: number, ondalikAyirici: ','|'.'|null, sutunSayisi: number}}}
 */
export function tablodanVeriSetiAyrintili(satirlar, secenekler = {}) {
  const ayar = {
    ad: '',
    kaynak: 'yapistir',
    baslik: 'otomatik',
    atlanacakSatir: 0,
    eksikDegerler: VARSAYILAN_EKSIK_DEGERLER,
    ondalikAyirici: 'otomatik',
    varsayilanAyirici: ',',
    tarihSirasi: 'otomatik',
    duzensizSatirUyarisi: false,
    satirNumarasiOfseti: 1,
    sutunOfseti: 0,
    ...secenekler,
  };
  if (!Array.isArray(satirlar)) throw new IstatistikHatasi('Tablo verisi okunamadı.');
  const eksikler = new Set(ayar.eksikDegerler.map((s) => String(s).trim()));
  const uyarilar = [...(ayar.uyarilar || [])];

  // 1) Atlanacak satırlar ve boş satırlar
  const kayitlar = [];
  const atla = Math.max(0, ayar.atlanacakSatir | 0);
  for (let r = atla; r < satirlar.length; r++) {
    const satir = satirlar[r] || [];
    let son = -1;
    let doluSayisi = 0;
    for (let j = 0; j < satir.length; j++) {
      if (!hucreBosMu(satir[j])) {
        son = j;
        doluSayisi++;
      }
    }
    kayitlar.push({ satir, kaynak: r, etkinUzunluk: son + 1, doluSayisi });
  }
  let ilkDolu = kayitlar.findIndex((k) => k.doluSayisi > 0);
  if (ilkDolu < 0) throw new IstatistikHatasi('Tabloda veri bulunamadı: tüm hücreler boş.');
  let sonDolu = kayitlar.length - 1;
  while (kayitlar[sonDolu].doluSayisi === 0) sonDolu--;
  let ara = kayitlar.slice(ilkDolu, sonDolu + 1);
  const bosSatirlar = ara.filter((k) => k.doluSayisi === 0).length;
  ara = ara.filter((k) => k.doluSayisi > 0);
  if (bosSatirlar > 0) uyarilar.push(`Veri arasındaki ${bosSatirlar} tamamen boş satır atlandı.`);

  // 2) Baştaki başlık/açıklama satırları: tablo en az 3 sütunluyken tek dolu hücreli satırlar
  let genislik = 0;
  for (const k of ara) if (k.etkinUzunluk > genislik) genislik = k.etkinUzunluk;
  let aciklama = 0;
  if (ayar.atlanacakSatir === 0 && genislik >= 3) {
    while (aciklama < Math.min(5, ara.length - 1) && ara[aciklama].doluSayisi <= 1 && ara.slice(aciklama + 1).some((k) => k.doluSayisi >= 2)) {
      aciklama++;
    }
    if (aciklama > 0) {
      const metinler = ara.slice(0, aciklama).map((k) => hucreMetni(k.satir.find((h) => !hucreBosMu(h)))).filter(Boolean);
      uyarilar.push(`Tablonun üstündeki ${aciklama} açıklama satırı atlandı${metinler.length ? `: "${metinler.join('", "').slice(0, 120)}"` : ''}.`);
      ara = ara.slice(aciklama);
      genislik = 0;
      for (const k of ara) if (k.etkinUzunluk > genislik) genislik = k.etkinUzunluk;
    }
  }

  // 3) Tamamen boş sütunlar
  const doluSutun = new Uint8Array(genislik);
  for (const k of ara) for (let j = 0; j < k.etkinUzunluk; j++) if (!doluSutun[j] && !hucreBosMu(k.satir[j])) doluSutun[j] = 1;
  const sutunlar = [];
  for (let j = 0; j < genislik; j++) if (doluSutun[j]) sutunlar.push(j);
  const araBosSutun = genislik - sutunlar.length;
  if (araBosSutun > 0) uyarilar.push(`${araBosSutun} tamamen boş sütun atlandı.`);

  // 4) Başlık
  const satirListesi = ara.map((k) => k.satir);
  const baslikVar = ayar.baslik === true || (ayar.baslik !== false && baslikVarMi(satirListesi, sutunlar, eksikler));
  const govde = baslikVar ? ara.slice(1) : ara;

  // 5) Düzensiz satırlar (CSV): alan sayısı başlıktan/çoğunluktan farklı olanlar
  if (ayar.duzensizSatirUyarisi && govde.length > 0) {
    const sayac = new Map();
    for (const k of ara) sayac.set(k.satir.length, (sayac.get(k.satir.length) || 0) + 1);
    let mod = baslikVar ? ara[0].satir.length : 0;
    if (!baslikVar) {
      let enCok = -1;
      for (const [u, c] of sayac) if (c > enCok) [mod, enCok] = [u, c];
    }
    const kisa = [];
    const uzun = [];
    for (const k of govde) {
      if (k.etkinUzunluk > mod) uzun.push(k.kaynak + ayar.satirNumarasiOfseti);
      else if (k.satir.length < mod) kisa.push(k.kaynak + ayar.satirNumarasiOfseti);
    }
    const liste = (a) => a.slice(0, UYARI_ORNEK).join(', ') + (a.length > UYARI_ORNEK ? ' …' : '');
    if (kisa.length) uyarilar.push(`${kisa.length} satırda alan sayısı beklenenden (${mod}) az (satır ${liste(kisa)}); eksik alanlar boş kabul edildi.`);
    if (uzun.length) uyarilar.push(`${uzun.length} satırda başlıktakinden fazla alan var (satır ${liste(uzun)}); fazla alanlar adsız sütun olarak okundu.`);
  }

  // 6) Sütunlar: tarama → dosya geneli ondalık ayırıcı varsayılanı → çözüm
  const taramalar = [];
  const sutunHucreleri = [];
  let kesinN = 0;
  let kesinV = 0;
  for (const j of sutunlar) {
    const hucreler = new Array(govde.length);
    for (let i = 0; i < govde.length; i++) hucreler[i] = govde[i].satir[j];
    const t = sutunTara(hucreler, eksikler);
    kesinN += t.kesinN;
    kesinV += t.kesinV;
    taramalar.push(t);
    sutunHucreleri.push(hucreler);
  }
  const ozelAyar = { ...ayar };
  if (kesinV > kesinN) ozelAyar.varsayilanAyirici = ',';
  else if (kesinN > kesinV) ozelAyar.varsayilanAyirici = '.';

  const kullanilan = new Set();
  const degiskenler = [];
  const ayiricilar = new Set();
  const yenidenAdlanan = [];
  sutunlar.forEach((j, s) => {
    const kaynakSutun = j + ayar.sutunOfseti;
    const hamAd = baslikVar ? hucreMetni(ara[0].satir[j]).replace(/[\r\n\t]+/g, ' ').trim() : '';
    const ad = benzersizAd(hamAd, kullanilan, `Değişken${kaynakSutun + 1}`);
    if (hamAd !== '' && ad !== hamAd) yenidenAdlanan.push(`"${hamAd}" → "${ad}"`);
    kullanilan.add(adAnahtari(ad));
    const satirNo = (i) => govde[i].kaynak + ayar.satirNumarasiOfseti;
    const c = sutunuCoz(ad, sutunHucreleri[s], taramalar[s], ozelAyar, satirNo);
    if (c.ayirici) ayiricilar.add(c.ayirici);
    uyarilar.push(...c.uyarilar);
    degiskenler.push({
      ad,
      etiket: '',
      veriTuru: c.veriTuru,
      degerler: c.degerler,
      olcum: null,
      degerEtiketleri: c.degerEtiketleri,
      eksikKodlar: null,
      ondalik: c.ondalik,
      kaynakSutun,
    });
  });
  if (yenidenAdlanan.length) uyarilar.push(`Yinelenen sütun adları değiştirildi: ${yenidenAdlanan.join(', ')}.`);

  const veriSeti = {
    ad: String(ayar.ad),
    kaynak: ayar.kaynak,
    satirSayisi: govde.length,
    degiskenler,
    uyarilar,
  };
  return {
    veriSeti,
    bilgi: {
      baslik: baslikVar,
      atlananSatirlar: atla + aciklama,
      bosSatirlar,
      ondalikAyirici: ayiricilar.size === 1 ? [...ayiricilar][0] : ayiricilar.size > 1 ? 'karisik' : null,
      sutunSayisi: degiskenler.length,
    },
  };
}

/**
 * Hücre tablosunu VeriSeti'ne çevirir (bkz. tablodanVeriSetiAyrintili).
 * @param {Hucre[][]} satirlar
 * @param {TabloSecenekleri} [secenekler]
 * @returns {import('./model.js').VeriSeti}
 */
export function tablodanVeriSeti(satirlar, secenekler = {}) {
  return tablodanVeriSetiAyrintili(satirlar, secenekler).veriSeti;
}
