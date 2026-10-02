// SPSS sistem dosyası (.sav / .zsav) okuyucu. Bağımlılık yoktur; tarayıcıda ve Node 20+'da çalışır.
// Biçim, PSPP belgelerindeki "System File Format" tanımına göre uygulanmıştır:
//   başlık ($FL2/$FL3) → değişken kayıtları (2) → değer etiketleri (3+4) → belgeler (6) → uzantı kayıtları (7)
//   → sözlük sonu (999) → veri (sıkıştırmasız, bayt kodu sıkıştırmalı ya da ZSAV zlib blokları).
// ZSAV blokları DecompressionStream('deflate') ile açıldığından okuma eşzamansızdır (async).
import { IstatistikHatasi } from '../cekirdek/hata.js';
import { cozucuOlustur, kodlamaAdiDuzelt, kodlamaDestekleniyor, gecerliUtf8Mi } from './kodlama.js';
import { adAnahtari, benzersizAd } from './model.js';

/** 1582-10-14 (SPSS tarih başlangıcı) ile 1970-01-01 arasındaki saniye. */
export const SPSS_EPOCH_SANIYE = 12219379200;

const SYSMIS = -Number.MAX_VALUE; // 0xFFEFFFFFFFFFFFFF
const ENDUSUK = -1.7976931348623155e308; // LOWEST: 0xFFEFFFFFFFFFFFFE
const ENYUKSEK = Number.MAX_VALUE; // HIGHEST

/** SPSS biçim türü kodları (PSPP format.def). */
export const SPSS_BICIMLERI = Object.freeze({
  1: 'A', 2: 'AHEX', 3: 'COMMA', 4: 'DOLLAR', 5: 'F', 6: 'IB', 7: 'PIBHEX', 8: 'P', 9: 'PIB', 10: 'PK', 11: 'RB',
  12: 'RBHEX', 15: 'Z', 16: 'N', 17: 'E', 20: 'DATE', 21: 'TIME', 22: 'DATETIME', 23: 'ADATE', 24: 'JDATE',
  25: 'DTIME', 26: 'WKDAY', 27: 'MONTH', 28: 'MOYR', 29: 'QYR', 30: 'WKYR', 31: 'PCT', 32: 'DOT', 33: 'CCA',
  34: 'CCB', 35: 'CCC', 36: 'CCD', 37: 'CCE', 38: 'EDATE', 39: 'SDATE', 40: 'MTIME', 41: 'YMDHMS',
});
/** Değeri 1582-10-14'ten bu yana saniye olan biçimler → veriTuru 'tarih'. */
const TARIH_BICIMLERI = new Set([20, 22, 23, 24, 28, 29, 30, 38, 39, 41]);
/** Süre/saat biçimleri (saniye cinsinden süre; tarih değil) → veriTuru 'sayi' (saniye). */
const SURE_BICIMLERI = new Set([21, 25, 40]);

const OLCUM = { 1: 'kategorik', 2: 'sirali', 3: 'surekli' };

function bozuk(ayrinti) {
  return new IstatistikHatasi(`SPSS dosyası bozuk ya da eksik: ${ayrinti}.`);
}

class Okuyucu {
  constructor(bytes) {
    this.b = bytes;
    this.dv = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    this.p = 0;
    this.le = true;
  }
  gerekli(n, ne) {
    if (n < 0 || this.p + n > this.b.length) throw bozuk(`${ne || 'kayıt'} okunurken dosya sonuna gelindi (konum ${this.p})`);
  }
  i32(ne) {
    this.gerekli(4, ne);
    const v = this.dv.getInt32(this.p, this.le);
    this.p += 4;
    return v;
  }
  f64(ne) {
    this.gerekli(8, ne);
    const v = this.dv.getFloat64(this.p, this.le);
    this.p += 8;
    return v;
  }
  i64(ne) {
    this.gerekli(8, ne);
    const v = i64Oku(this.dv, this.p, this.le);
    this.p += 8;
    return v;
  }
  u8(ne) {
    this.gerekli(1, ne);
    return this.b[this.p++];
  }
  bayt(n, ne) {
    this.gerekli(n, ne);
    const s = this.b.subarray(this.p, this.p + n);
    this.p += n;
    return s;
  }
}

function i64Oku(dv, p, le) {
  const lo = dv.getUint32(p + (le ? 0 : 4), le);
  const hi = dv.getInt32(p + (le ? 4 : 0), le);
  return hi * 4294967296 + lo;
}

function ascii(bytes) {
  let s = '';
  for (let i = 0; i < bytes.length; i++) s += String.fromCharCode(bytes[i]);
  return s;
}

/** Sondaki boşluk ve NUL baytları atılmış uzunluk. */
function kirpikUzunluk(b, bas, uz) {
  let n = uz;
  while (n > 0 && (b[bas + n - 1] === 0x20 || b[bas + n - 1] === 0)) n--;
  return n;
}

function kodSayfasi(cp) {
  if (cp === 65001) return 'utf-8';
  if (cp === 1254 || cp === 28599) return 'windows-1254';
  if (cp >= 1250 && cp <= 1258) return `windows-${cp}`;
  if (cp >= 28591 && cp <= 28605) return `iso-8859-${cp - 28590}`;
  const diger = { 874: 'windows-874', 932: 'shift_jis', 936: 'gbk', 949: 'euc-kr', 950: 'big5', 20932: 'euc-jp', 20127: 'windows-1252', 866: 'ibm866', 54936: 'gb18030' };
  return diger[cp] || null;
}

async function zlibAc(bloklar, boyutlar) {
  if (typeof DecompressionStream === 'undefined' || typeof Response === 'undefined') {
    throw new IstatistikHatasi('Bu tarayıcı sıkıştırılmış SPSS (.zsav) dosyalarını açamıyor. Güncel bir tarayıcı kullanın ya da dosyayı SPSS\'te sıkıştırmasız .sav olarak kaydedin.');
  }
  const parcalar = [];
  let toplam = 0;
  for (const blok of bloklar) {
    let ab;
    try {
      const akis = new Blob([blok]).stream().pipeThrough(new DecompressionStream('deflate'));
      ab = await new Response(akis).arrayBuffer();
    } catch {
      throw bozuk('sıkıştırılmış (ZSAV) veri bloğu açılamadı');
    }
    if (boyutlar && ab.byteLength !== boyutlar[parcalar.length]) throw bozuk('sıkıştırılmış (ZSAV) veri bloğunun boyutu tutmuyor');
    parcalar.push(new Uint8Array(ab));
    toplam += ab.byteLength;
  }
  const sonuc = new Uint8Array(toplam);
  let p = 0;
  for (const c of parcalar) {
    sonuc.set(c, p);
    p += c.length;
  }
  return sonuc;
}

// ---------------------------------------------------------------- sözlük

function baslikOku(o) {
  o.gerekli(176, 'dosya başlığı');
  const imza = ascii(o.b.subarray(0, 4));
  if (imza !== '$FL2' && imza !== '$FL3') {
    throw new IstatistikHatasi('Bu dosya bir SPSS veri dosyası (.sav) değil ya da tanınmayan bir sürümde.');
  }
  const duzen = o.dv.getInt32(64, true);
  if (duzen === 2 || duzen === 3) o.le = true;
  else {
    const t = o.dv.getInt32(64, false);
    if (t === 2 || t === 3) o.le = false;
    else throw bozuk('dosya başlığındaki düzen kodu tanınmadı');
  }
  o.p = 4;
  const urun = o.bayt(60);
  o.p = 68;
  const birimSayisi = o.i32();
  const sikistirma = o.i32();
  const agirlikIndeksi = o.i32();
  const vakaSayisi = o.i32();
  const sapma = o.f64();
  const tarih = o.bayt(9);
  const saat = o.bayt(8);
  const etiket = o.bayt(64);
  o.p = 176;
  if (sikistirma < 0 || sikistirma > 2) throw bozuk(`bilinmeyen sıkıştırma türü (${sikistirma})`);
  if (sikistirma === 2 && imza !== '$FL3') throw bozuk('zlib sıkıştırması yalnızca .zsav ($FL3) dosyalarında olabilir');
  return { imza, urun, birimSayisi, sikistirma, agirlikIndeksi, vakaSayisi, sapma, tarih, saat, etiket };
}

function sozlukOku(o) {
  const kayitlar = []; // değişken kayıtları (devam kayıtları dahil)
  const degerEtiketKayitlari = [];
  const belgeler = [];
  const uzantilar = new Map(); // alt tür → [veri]
  for (let guvenlik = 0; ; guvenlik++) {
    if (guvenlik > 10_000_000) throw bozuk('sözlük sonlanmıyor');
    const tur = o.i32('kayıt türü');
    if (tur === 2) {
      const tip = o.i32();
      const etiketVar = o.i32();
      const eksikSayisi = o.i32();
      const yazdirma = o.i32();
      const yazma = o.i32();
      const ad = o.bayt(8, 'değişken adı');
      let etiket = null;
      if (etiketVar === 1) {
        const uz = o.i32('değişken etiketi');
        if (uz < 0 || uz > 65536) throw bozuk('geçersiz değişken etiketi uzunluğu');
        etiket = o.bayt(uz, 'değişken etiketi');
        o.bayt((4 - (uz % 4)) % 4);
      } else if (etiketVar !== 0) throw bozuk('geçersiz değişken kaydı');
      const eksik = [];
      const ne = Math.abs(eksikSayisi);
      if (ne > 3 || eksikSayisi === -1) throw bozuk('geçersiz eksik değer sayısı');
      for (let k = 0; k < ne; k++) eksik.push(o.bayt(8, 'eksik değer'));
      if (tip < -1 || tip > 255) throw bozuk('geçersiz değişken türü');
      kayitlar.push({ tip, ad, etiket, eksikSayisi, eksik, yazdirma, yazma, sozlukIndeksi: kayitlar.length + 1 });
    } else if (tur === 3) {
      const sayi = o.i32('değer etiketi');
      if (sayi < 0 || sayi > 1e7) throw bozuk('geçersiz değer etiketi sayısı');
      const etiketler = [];
      for (let k = 0; k < sayi; k++) {
        const deger = o.bayt(8, 'değer etiketi');
        const uz = o.u8('değer etiketi');
        const etiket = o.bayt(uz, 'değer etiketi');
        o.bayt((8 - ((uz + 1) % 8)) % 8);
        etiketler.push({ deger, etiket });
      }
      const t4 = o.i32('değer etiketi değişken listesi');
      if (t4 !== 4) throw bozuk('değer etiketi kaydından sonra değişken listesi (tür 4) bekleniyordu');
      const vs = o.i32();
      if (vs < 0 || vs > 1e7) throw bozuk('geçersiz değer etiketi değişken sayısı');
      const indeksler = [];
      for (let k = 0; k < vs; k++) indeksler.push(o.i32());
      degerEtiketKayitlari.push({ etiketler, indeksler });
    } else if (tur === 6) {
      const n = o.i32('belge kaydı');
      if (n < 0 || n > 1e6) throw bozuk('geçersiz belge satırı sayısı');
      for (let k = 0; k < n; k++) belgeler.push(o.bayt(80, 'belge kaydı'));
    } else if (tur === 7) {
      const alt = o.i32('uzantı kaydı');
      const boyut = o.i32();
      const adet = o.i32();
      if (boyut < 0 || adet < 0 || boyut * adet > o.b.length) throw bozuk(`geçersiz uzantı kaydı (alt tür ${alt})`);
      const veri = o.bayt(boyut * adet, `uzantı kaydı (alt tür ${alt})`);
      if (!uzantilar.has(alt)) uzantilar.set(alt, []);
      uzantilar.get(alt).push({ boyut, adet, veri });
    } else if (tur === 999) {
      o.i32();
      break;
    } else {
      throw bozuk(`tanınmayan sözlük kaydı türü (${tur}, konum ${o.p - 4})`);
    }
  }
  return { kayitlar, degerEtiketKayitlari, belgeler, uzantilar };
}

function metinKayitlari(uzantilar, alt, coz) {
  return (uzantilar.get(alt) || []).map((u) => coz(u.veri)).join('');
}

// ---------------------------------------------------------------- veri

/**
 * Birim (8 baytlık hücre) planı: her birim ya bir sayısal değişkenin değeri ya da bir metin değişkeninin
 * 8 baytlık parçasıdır. Çok uzun metinlerde (> 255) her parça (segment) 255 bayt veri taşır, segment sayısı
 * ceil(genişlik/252)'dir (PSPP ve ReadStat ile aynı).
 */
function birimPlani(degiskenler) {
  const tur = [];
  const deg = [];
  const hedef = [];
  const adet = [];
  degiskenler.forEach((d, k) => {
    if (!d.metin) {
      tur.push(0);
      deg.push(k);
      hedef.push(0);
      adet.push(8);
      return;
    }
    d.segmentler.forEach((seg, s) => {
      const son = s === d.segmentler.length - 1;
      const veriBayti = son ? Math.max(0, d.genislik - 255 * s) : 255;
      const birim = Math.ceil(seg.genislik / 8);
      for (let j = 0; j < birim; j++) {
        tur.push(1);
        deg.push(k);
        hedef.push(255 * s + 8 * j);
        adet.push(Math.max(0, Math.min(8, veriBayti - 8 * j)));
      }
    });
  });
  return { tur: Uint8Array.from(tur), deg: Int32Array.from(deg), hedef: Int32Array.from(hedef), adet: Uint8Array.from(adet) };
}

function metinCozucu(coz) {
  const onbellek = new Map();
  return (b, uz) => {
    const n = kirpikUzunluk(b, 0, uz);
    if (n === 0) return null;
    let asc = true;
    for (let i = 0; i < n; i++) {
      if (b[i] >= 0x80) {
        asc = false;
        break;
      }
    }
    let s;
    if (asc) {
      s = n < 4096 ? String.fromCharCode.apply(null, b.subarray(0, n)) : ascii(b.subarray(0, n));
    } else {
      const anahtar = ascii(b.subarray(0, n));
      s = onbellek.get(anahtar);
      if (s === undefined) {
        s = coz(b.slice(0, n));
        if (onbellek.size < 100000) onbellek.set(anahtar, s);
      }
    }
    if (s.indexOf('\0') >= 0) s = s.replace(/\0/g, '');
    return s === '' ? null : s;
  };
}

function veriOku(veri, bas, ctx) {
  const { plan, degiskenler, sikistirmali, sapma, sysmis, le, vakaSayisi, metinCoz, kesikKabul } = ctx;
  const U = plan.tur.length;
  const dv = new DataView(veri.buffer, veri.byteOffset, veri.byteLength);
  const sutunlar = degiskenler.map(() => []);
  const tamponlar = degiskenler.map((d) => (d.metin ? new Uint8Array(Math.max(8, d.segmentler.length * 256)) : null));
  const metinIndeksleri = [];
  degiskenler.forEach((d, k) => d.metin && metinIndeksleri.push(k));
  const hedefVaka = vakaSayisi >= 0 ? vakaSayisi : Infinity;
  let gecersizKod = 0;
  let n = 0;
  const vakaBitir = () => {
    for (const k of metinIndeksleri) {
      sutunlar[k].push(metinCoz(tamponlar[k], degiskenler[k].genislik));
      tamponlar[k].fill(0x20);
    }
    n++;
  };
  for (const k of metinIndeksleri) tamponlar[k].fill(0x20);
  // -0 → 0: bayt kodu sıkıştırması -0'ı 0 olarak yazar; sıkıştırmadan bağımsız sonuç için normalleştirilir
  const sayi = (v) => (v === sysmis || v === SYSMIS || Number.isNaN(v) ? null : v === 0 ? 0 : v);

  if (!sikistirmali) {
    const vakaBayt = U * 8;
    const mevcut = vakaBayt > 0 ? Math.floor((veri.length - bas) / vakaBayt) : 0;
    const artik = vakaBayt > 0 ? (veri.length - bas) % vakaBayt : 0;
    let okunacak = Math.min(mevcut, hedefVaka);
    if ((vakaSayisi >= 0 && mevcut < vakaSayisi) || (vakaSayisi < 0 && artik !== 0)) {
      if (!kesikKabul) throw bozuk(`veri bölümü eksik (${vakaSayisi >= 0 ? `${vakaSayisi} vaka bekleniyordu, ${mevcut} vaka var` : 'son vaka yarım kalmış'})`);
      okunacak = mevcut;
    }
    for (let v = 0; v < okunacak; v++) {
      const taban = bas + v * vakaBayt;
      for (let u = 0; u < U; u++) {
        const p = taban + 8 * u;
        if (plan.tur[u] === 0) sutunlar[plan.deg[u]].push(sayi(dv.getFloat64(p, le)));
        else {
          const a = plan.adet[u];
          if (a > 0) tamponlar[plan.deg[u]].set(veri.subarray(p, p + a), plan.hedef[u]);
        }
      }
      vakaBitir();
    }
    return { sutunlar, n, kesik: okunacak < Math.min(mevcut, hedefVaka) || (vakaSayisi >= 0 && okunacak < vakaSayisi), gecersizKod };
  }

  // Bayt kodu sıkıştırması: 8 komut baytlık blok, ardından 253 kodlarının ham 8 baytlık verileri.
  const son = veri.length;
  let komutPos = bas;
  let ki = 8;
  let dp = bas;
  let kesik = false;
  dongu: while (n < hedefVaka) {
    for (let u = 0; u < U; u++) {
      let op;
      for (;;) {
        if (ki === 8) {
          if (dp + 8 > son) {
            op = -1;
            break;
          }
          komutPos = dp;
          dp += 8;
          ki = 0;
        }
        op = veri[komutPos + ki++];
        if (op !== 0) break;
      }
      if (op === -1 || op === 252) {
        if (u === 0) break dongu;
        if (!kesikKabul) throw bozuk('son vaka yarım kalmış');
        kesik = true;
        break dongu;
      }
      const k = plan.deg[u];
      if (plan.tur[u] === 0) {
        if (op === 253) {
          if (dp + 8 > son) {
            if (!kesikKabul) throw bozuk('sıkıştırılmış veri yarım kalmış');
            kesik = true;
            break dongu;
          }
          sutunlar[k].push(sayi(dv.getFloat64(dp, le)));
          dp += 8;
        } else if (op === 255) sutunlar[k].push(null);
        else if (op === 254) {
          gecersizKod++;
          sutunlar[k].push(null);
        } else sutunlar[k].push(op - sapma);
      } else {
        const a = plan.adet[u];
        if (op === 253) {
          if (dp + 8 > son) {
            if (!kesikKabul) throw bozuk('sıkıştırılmış veri yarım kalmış');
            kesik = true;
            break dongu;
          }
          if (a > 0) tamponlar[k].set(veri.subarray(dp, dp + a), plan.hedef[u]);
          dp += 8;
        } else if (op !== 254) gecersizKod++;
        // 254: sekiz boşluk — tampon zaten boşlukla dolu
      }
    }
    vakaBitir();
  }
  if (vakaSayisi >= 0 && n < vakaSayisi && !kesik) {
    if (!kesikKabul) throw bozuk(`veri bölümü eksik (${vakaSayisi} vaka bekleniyordu, ${n} vaka okundu)`);
    kesik = true;
  }
  // Yarım kalan vakada eklenmiş değerleri geri al
  for (const s of sutunlar) if (s.length > n) s.length = n;
  return { sutunlar, n, kesik, gecersizKod };
}

/** Ondalık kısmı her zaman yazılan sayısal biçimler (F8.0 gibi). */
const ONDALIKLI_BICIMLER = new Set([3, 4, 5, 15, 16, 17, 31, 32, 33, 34, 35, 36, 37]);

/** SPSS biçim metni: "F8.2", "A20", "DATE11", "TIME8" (haven'ın format.spss özniteliğiyle aynı yazım). */
function bicimMetni(d) {
  if (d.metin) return `A${d.genislik}`;
  const g = (d.bicim >> 8) & 0xff;
  const o = d.ondalikBasamak;
  return `${d.bicimAdi}${g}${o > 0 || ONDALIKLI_BICIMLER.has(d.bicimTuru) ? `.${o}` : ''}`;
}

// ---------------------------------------------------------------- ana işlev

/**
 * @typedef {Object} SavSecenekleri
 * @property {string} [ad]          Veri seti adı
 * @property {string} [kodlama]     Metin kodlamasını zorla ('utf-8', 'windows-1254', ...); varsayılan dosyadan
 * @property {boolean} [kesikKabul] Veri bölümü eksik dosyada hata yerine okunabilen vakaları döndür (uyarıyla)
 */

/**
 * SPSS .sav/.zsav dosyasını okur.
 * Dönüşümler: SYSMIS → null; metinler sağdan kırpılır, boş metin → null; tarih biçimli (DATE, ADATE, EDATE,
 * SDATE, JDATE, DATETIME, YMDHMS, MOYR, QYR, WKYR) değişkenler 1970 tabanlı UTC milisaniyeye çevrilir
 * (haven::read_sav ile aynı an); TIME/DTIME/MTIME süre biçimli değişkenler saniye cinsinden sayı olarak kalır.
 * Kullanıcı tanımlı eksik değerler ham değer olarak durur, tanımları eksikKodlar alanına yazılır
 * (haven::read_sav(user_na = TRUE) gibi). Ölçüm düzeyi (scale/ordinal/nominal) olcum alanına aktarılır.
 * @param {ArrayBuffer|Uint8Array} girdi
 * @param {SavSecenekleri} [secenekler]
 * @returns {Promise<{veriSeti: import('./model.js').VeriSeti, bilgi: object}>}
 */
export async function savOkuAyrintili(girdi, secenekler = {}) {
  const bytes = girdi instanceof Uint8Array ? girdi : new Uint8Array(girdi);
  if (bytes.length === 0) throw new IstatistikHatasi('Dosya boş: okunacak veri yok.');
  const o = new Okuyucu(bytes);
  const baslik = baslikOku(o);
  const sozluk = sozlukOku(o);
  const { kayitlar, uzantilar } = sozluk;
  const uyarilar = [];
  const le = o.le;

  // --- Uzantı kayıtlarındaki sayısal bilgiler
  let sysmis = SYSMIS;
  let enYuksek = ENYUKSEK;
  let enDusuk = ENDUSUK;
  const f4 = (uzantilar.get(4) || [])[0];
  if (f4 && f4.boyut === 8 && f4.adet >= 3) {
    const dv = new DataView(f4.veri.buffer, f4.veri.byteOffset, f4.veri.byteLength);
    sysmis = dv.getFloat64(0, le);
    enYuksek = dv.getFloat64(8, le);
    enDusuk = dv.getFloat64(16, le);
  }
  let karakterKodu = null;
  const i3 = (uzantilar.get(3) || [])[0];
  if (i3 && i3.boyut === 4 && i3.adet >= 8) {
    karakterKodu = new DataView(i3.veri.buffer, i3.veri.byteOffset, i3.veri.byteLength).getInt32(28, le);
  }
  let vakaSayisi = baslik.vakaSayisi;
  const e16 = (uzantilar.get(16) || [])[0];
  if (e16 && e16.boyut === 8 && e16.adet === 2 && vakaSayisi < 0) {
    const v = i64Oku(new DataView(e16.veri.buffer, e16.veri.byteOffset, e16.veri.byteLength), 8, le);
    if (v >= 0 && v < 2 ** 40) vakaSayisi = v;
  }

  // --- Kodlama
  let kodlama = null;
  let kodlamaKaynagi = '';
  if (secenekler.kodlama && secenekler.kodlama !== 'otomatik') {
    kodlama = kodlamaAdiDuzelt(secenekler.kodlama);
    kodlamaKaynagi = 'kullanıcı';
  }
  if (!kodlama && uzantilar.has(20)) {
    const ad = kodlamaAdiDuzelt(ascii(uzantilar.get(20)[0].veri).replace(/\0+$/, ''));
    if (ad && kodlamaDestekleniyor(ad)) {
      kodlama = ad;
      kodlamaKaynagi = 'dosya (kodlama kaydı)';
    }
  }
  if (!kodlama && karakterKodu !== null) {
    const ad = kodSayfasi(karakterKodu);
    if (ad && kodlamaDestekleniyor(ad)) {
      kodlama = ad;
      kodlamaKaynagi = 'dosya (kod sayfası)';
    }
  }
  if (!kodlama) {
    // Eski dosya: sözlükteki metinler geçerli UTF-8 ise UTF-8, değilse Türkçe Windows kod sayfası
    const parcalar = [];
    for (const k of kayitlar) {
      parcalar.push(k.ad);
      if (k.etiket) parcalar.push(k.etiket);
    }
    for (const r of sozluk.degerEtiketKayitlari) for (const e of r.etiketler) parcalar.push(e.etiket);
    for (const alt of [13, 21]) for (const u of uzantilar.get(alt) || []) parcalar.push(u.veri);
    const toplam = parcalar.reduce((t, p) => t + p.length, 0);
    const hepsi = new Uint8Array(toplam);
    let p = 0;
    for (const x of parcalar) {
      hepsi.set(x, p);
      p += x.length;
    }
    const asc = hepsi.every((b) => b < 0x80);
    kodlama = !asc && gecerliUtf8Mi(hepsi) ? 'utf-8' : 'windows-1254';
    kodlamaKaynagi = 'tahmin';
    if (!asc) uyarilar.push(`Dosyada karakter kodlaması bilgisi yok; ${kodlama === 'utf-8' ? 'UTF-8' : 'Windows-1254 (Türkçe)'} varsayıldı. Türkçe harfler bozuksa kodlamayı elle seçin.`);
  }
  const cozHam = cozucuOlustur(kodlama);
  const coz = (b) => cozHam(b);
  const kirpCoz = (b) => coz(b.subarray(0, kirpikUzunluk(b, 0, b.length)));

  // --- Değişkenler: devam kayıtlarını (tip -1) atla, segmentleri topla
  const segmentler = [];
  for (let i = 0; i < kayitlar.length; i++) {
    const k = kayitlar[i];
    if (k.tip === -1) {
      if (segmentler.length === 0 || segmentler[segmentler.length - 1].tip === 0) throw bozuk('sayısal değişkenden sonra devam kaydı');
      segmentler[segmentler.length - 1].birim++;
      continue;
    }
    segmentler.push({ ...k, kisaAd: kirpCoz(k.ad), birim: 1, genislik: k.tip });
  }
  for (const s of segmentler) {
    if (s.tip > 0 && s.birim !== Math.ceil(s.tip / 8)) throw bozuk(`"${s.kisaAd}" metin değişkeninin devam kayıtları eksik`);
  }

  const uzunAdlar = new Map();
  for (const parca of metinKayitlari(uzantilar, 13, coz).split('\t')) {
    const e = parca.indexOf('=');
    if (e > 0) uzunAdlar.set(parca.slice(0, e), parca.slice(e + 1).replace(/\0+$/, ''));
  }
  const cokUzun = new Map();
  for (const parca of metinKayitlari(uzantilar, 14, coz).split(/[\0\t]+/)) {
    const e = parca.indexOf('=');
    if (e > 0) {
      const g = parseInt(parca.slice(e + 1), 10);
      if (g > 0) cokUzun.set(parca.slice(0, e), g);
    }
  }

  const degiskenler = [];
  for (let i = 0; i < segmentler.length; i++) {
    const s = segmentler[i];
    const genislik = s.tip > 0 && cokUzun.has(s.kisaAd) ? cokUzun.get(s.kisaAd) : s.tip;
    let segs = [s];
    if (s.tip > 0 && genislik > 255) {
      const ns = Math.ceil(genislik / 252);
      segs = segmentler.slice(i, i + ns);
      if (segs.length !== ns || segs.some((x) => x.tip <= 0)) throw bozuk(`"${s.kisaAd}" çok uzun metin değişkeninin parçaları eksik`);
      i += ns - 1;
    }
    degiskenler.push({
      ilk: s,
      kisaAd: s.kisaAd,
      ad: uzunAdlar.get(s.kisaAd) || s.kisaAd,
      metin: s.tip > 0,
      genislik: s.tip > 0 ? genislik : 0,
      segmentler: segs,
      bicim: s.yazdirma,
      etiket: s.etiket ? coz(s.etiket) : '',
      olcum: null,
      degerEtiketleri: null,
      eksikKodlar: null,
    });
  }
  const sozlukIndeksiyle = new Map(degiskenler.map((d) => [d.ilk.sozlukIndeksi, d]));
  const adla = new Map();
  for (const d of degiskenler) {
    adla.set(d.ad, d);
    if (!adla.has(d.kisaAd)) adla.set(d.kisaAd, d);
  }
  const adlaBul = (ad) => {
    if (adla.has(ad)) return adla.get(ad);
    const a = adAnahtari(ad);
    return degiskenler.find((d) => adAnahtari(d.ad) === a || adAnahtari(d.kisaAd) === a) || null;
  };

  // --- Biçim ve tür
  for (const d of degiskenler) {
    const tip = (d.bicim >> 16) & 0xff;
    d.bicimTuru = tip;
    d.bicimAdi = SPSS_BICIMLERI[tip] || `?${tip}`;
    d.ondalikBasamak = d.bicim & 0xff;
    d.tarih = !d.metin && TARIH_BICIMLERI.has(tip);
    d.sure = !d.metin && SURE_BICIMLERI.has(tip);
  }
  const sayiCevir = (d, v) => (d.tarih ? Math.round((v - SPSS_EPOCH_SANIYE) * 1000) : v);
  const sayiDegeri = (b) => new DataView(b.buffer, b.byteOffset, 8).getFloat64(0, le);

  // --- Eksik değerler (değişken kaydı)
  for (const d of degiskenler) {
    const k = d.ilk;
    if (k.eksikSayisi === 0) continue;
    if (d.metin) {
      d.eksikKodlar = { tekil: k.eksik.map((b) => kirpCoz(b)), aralik: null };
    } else {
      const v = k.eksik.map(sayiDegeri);
      const ucCevir = (x, alt) => (alt ? (x <= enDusuk || x === SYSMIS ? -Infinity : sayiCevir(d, x)) : x >= enYuksek ? Infinity : sayiCevir(d, x));
      if (k.eksikSayisi > 0) d.eksikKodlar = { tekil: v.map((x) => sayiCevir(d, x)), aralik: null };
      else d.eksikKodlar = { tekil: v.slice(2).map((x) => sayiCevir(d, x)), aralik: [ucCevir(v[0], true), ucCevir(v[1], false)] };
    }
  }

  // --- Görüntüleme parametreleri (alt tür 11): ölçüm düzeyi
  const e11 = (uzantilar.get(11) || [])[0];
  if (e11) {
    const dv = new DataView(e11.veri.buffer, e11.veri.byteOffset, e11.veri.byteLength);
    const deger = (i) => dv.getInt32(4 * i, le);
    const adet = e11.adet;
    const tumSegmentler = degiskenler.flatMap((d) => d.segmentler.map((s, j) => ({ d, ilk: j === 0 })));
    let hedefler = null;
    let adim = 0;
    if (adet === 3 * tumSegmentler.length || adet === 2 * tumSegmentler.length) {
      adim = adet / tumSegmentler.length;
      hedefler = tumSegmentler;
    } else if (adet === 3 * degiskenler.length || adet === 2 * degiskenler.length) {
      adim = adet / degiskenler.length;
      hedefler = degiskenler.map((d) => ({ d, ilk: true }));
    }
    if (hedefler) {
      hedefler.forEach((h, i) => {
        if (h.ilk) h.d.olcum = OLCUM[deger(i * adim)] || null;
      });
    } else uyarilar.push('Değişken görüntüleme bilgisi (ölçüm düzeyi) okunamadı; ölçüm düzeyleri boş bırakıldı.');
  }

  // --- Değer etiketleri (kayıt 3+4)
  for (const r of sozluk.degerEtiketKayitlari) {
    for (const idx of r.indeksler) {
      const d = sozlukIndeksiyle.get(idx);
      if (!d) continue;
      if (!d.degerEtiketleri) d.degerEtiketleri = {};
      for (const e of r.etiketler) {
        const anahtar = d.metin ? kirpCoz(e.deger) : String(sayiCevir(d, sayiDegeri(e.deger)));
        d.degerEtiketleri[anahtar] = coz(e.etiket);
      }
    }
  }

  // --- Uzun metin değer etiketleri (alt tür 21) ve eksik değerleri (alt tür 22)
  for (const u of uzantilar.get(21) || []) {
    const dv = new DataView(u.veri.buffer, u.veri.byteOffset, u.veri.byteLength);
    let p = 0;
    const L = u.veri.length;
    const i32 = () => {
      if (p + 4 > L) throw bozuk('uzun metin değer etiketi kaydı');
      const v = dv.getInt32(p, le);
      p += 4;
      return v;
    };
    const parca = (n) => {
      if (n < 0 || p + n > L) throw bozuk('uzun metin değer etiketi kaydı');
      const b = u.veri.subarray(p, p + n);
      p += n;
      return b;
    };
    while (p < L) {
      const ad = coz(parca(i32()));
      i32(); // genişlik
      const n = i32();
      const d = adlaBul(ad);
      const etiketler = d ? d.degerEtiketleri || {} : null;
      for (let k = 0; k < n; k++) {
        const deger = kirpCoz(parca(i32()));
        const etiket = coz(parca(i32()));
        if (etiketler) etiketler[deger] = etiket;
      }
      if (d) d.degerEtiketleri = etiketler;
    }
  }
  for (const u of uzantilar.get(22) || []) {
    const dv = new DataView(u.veri.buffer, u.veri.byteOffset, u.veri.byteLength);
    let p = 0;
    const L = u.veri.length;
    const kontrol = (n) => {
      if (n < 0 || p + n > L) throw bozuk('uzun metin eksik değer kaydı');
    };
    while (p < L) {
      kontrol(4);
      const adUz = dv.getInt32(p, le);
      p += 4;
      kontrol(adUz);
      const ad = coz(u.veri.subarray(p, p + adUz));
      p += adUz;
      kontrol(5);
      const n = u.veri[p];
      p += 1;
      const degerUz = dv.getInt32(p, le);
      p += 4;
      const tekil = [];
      for (let k = 0; k < n; k++) {
        kontrol(degerUz);
        tekil.push(kirpCoz(u.veri.subarray(p, p + degerUz)));
        p += degerUz;
      }
      const d = adlaBul(ad);
      if (d) d.eksikKodlar = { tekil, aralik: null };
    }
  }

  // --- Veri
  let veriBaytlari = bytes;
  let veriBasi = o.p;
  if (baslik.sikistirma === 2) {
    const z = new Okuyucu(bytes);
    z.le = le;
    z.p = o.p;
    const zBaslik = z.i64('ZSAV başlığı');
    const zSon = z.i64('ZSAV başlığı');
    const zSonUz = z.i64('ZSAV başlığı');
    if (zBaslik !== o.p || zSon < z.p || zSon + zSonUz > bytes.length || zSonUz < 24) throw bozuk('ZSAV veri başlığı geçersiz');
    z.p = zSon;
    z.i64(); // sapma (negatif)
    z.i64(); // sıfır
    z.i32(); // blok boyutu
    const blokSayisi = z.i32('ZSAV blok listesi');
    if (blokSayisi < 0 || 24 + 24 * blokSayisi > zSonUz) throw bozuk('ZSAV blok listesi geçersiz');
    const bloklar = [];
    const boyutlar = [];
    for (let k = 0; k < blokSayisi; k++) {
      z.i64(); // sıkıştırılmamış konum
      const sKonum = z.i64();
      boyutlar.push(z.i32()); // sıkıştırılmamış boyut
      const sBoyut = z.i32();
      if (sKonum < 0 || sBoyut < 0 || sKonum + sBoyut > bytes.length) throw bozuk('ZSAV bloğu dosya dışında');
      bloklar.push(bytes.subarray(sKonum, sKonum + sBoyut));
    }
    veriBaytlari = await zlibAc(bloklar, boyutlar);
    veriBasi = 0;
  }
  const plan = birimPlani(degiskenler);
  if (baslik.birimSayisi > 0 && baslik.birimSayisi !== plan.tur.length) {
    uyarilar.push(`Dosya başlığındaki vaka genişliği (${baslik.birimSayisi}) sözlükle uyuşmuyor (${plan.tur.length}); sözlük esas alındı.`);
  }
  const okunan = veriOku(veriBaytlari, veriBasi, {
    plan,
    degiskenler,
    sikistirmali: baslik.sikistirma !== 0,
    sapma: baslik.sapma,
    sysmis,
    le,
    vakaSayisi,
    metinCoz: metinCozucu(coz),
    kesikKabul: !!secenekler.kesikKabul,
  });
  if (okunan.kesik) uyarilar.push(`Dosyanın veri bölümü eksik: yalnızca ilk ${okunan.n} vaka okunabildi.`);
  if (okunan.gecersizKod) uyarilar.push(`Sıkıştırılmış veride ${okunan.gecersizKod} geçersiz kod vardı; ilgili hücreler eksik sayıldı.`);

  // --- Model
  const kullanilan = new Set();
  const sonuc = degiskenler.map((d, k) => {
    const ad = benzersizAd(d.ad, kullanilan, `Değişken${k + 1}`);
    kullanilan.add(adAnahtari(ad));
    let degerler = okunan.sutunlar[k];
    if (d.tarih) degerler = degerler.map((v) => (v === null ? null : sayiCevir(d, v)));
    if (d.sure) {
      uyarilar.push(`"${ad}" değişkeni SPSS'te süre/saat biçiminde (${d.bicimAdi}); değerler saniye cinsinden sayı olarak okundu.`);
    }
    return {
      ad,
      etiket: d.etiket,
      veriTuru: d.metin ? 'metin' : d.tarih ? 'tarih' : 'sayi',
      degerler,
      olcum: d.olcum,
      degerEtiketleri: d.degerEtiketleri && Object.keys(d.degerEtiketleri).length ? d.degerEtiketleri : null,
      eksikKodlar: d.eksikKodlar,
      ondalik: d.metin || d.tarih ? null : d.ondalikBasamak,
      kaynakSutun: k,
    };
  });

  const agirlik = baslik.agirlikIndeksi > 0 ? sozlukIndeksiyle.get(baslik.agirlikIndeksi) : null;
  const agirlikAdi = agirlik ? sonuc[degiskenler.indexOf(agirlik)].ad : null;
  if (agirlikAdi) {
    uyarilar.push(`Dosyada SPSS ağırlık değişkeni tanımlı ("${agirlikAdi}"). Analizler bu değişkenle ağırlıklandırılmadan yapılır; gerekiyorsa ağırlığı ayrıca uygulayın.`);
  }
  if (uzantilar.has(7) || uzantilar.has(19)) {
    uyarilar.push('Dosyadaki çoklu yanıt kümesi tanımları (MRSETS) okunmadı; değişkenler tek tek aktarıldı.');
  }

  const veriSeti = {
    ad: String(secenekler.ad ?? ''),
    kaynak: 'sav',
    satirSayisi: okunan.n,
    degiskenler: sonuc,
    uyarilar,
  };
  const bilgi = {
    imza: baslik.imza,
    urun: ascii(baslik.urun).replace(/[\0 ]+$/, ''),
    sikistirma: ['yok', 'bayt', 'zlib'][baslik.sikistirma],
    endian: le ? 'little' : 'big',
    kodlama,
    kodlamaKaynagi,
    karakterKodu,
    olusturma: `${ascii(baslik.tarih)} ${ascii(baslik.saat)}`.trim(),
    dosyaEtiketi: kirpCoz(baslik.etiket),
    belgeler: sozluk.belgeler.map((b) => kirpCoz(b)),
    agirlikDegiskeni: agirlikAdi,
    vakaSayisiBaslik: baslik.vakaSayisi,
    bicimler: Object.fromEntries(sonuc.map((s, k) => [s.ad, bicimMetni(degiskenler[k])])),
    sutunlar: Object.fromEntries(sonuc.map((s, k) => [s.ad, { kisaAd: degiskenler[k].kisaAd, genislik: degiskenler[k].genislik }])),
  };
  return { veriSeti, bilgi };
}

/**
 * SPSS .sav/.zsav dosyasını VeriSeti olarak okur (bkz. savOkuAyrintili).
 * @param {ArrayBuffer|Uint8Array} girdi
 * @param {SavSecenekleri} [secenekler]
 * @returns {Promise<import('./model.js').VeriSeti>}
 */
export async function savOku(girdi, secenekler = {}) {
  return (await savOkuAyrintili(girdi, secenekler)).veriSeti;
}
