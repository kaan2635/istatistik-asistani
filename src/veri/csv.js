// CSV / TSV / düz metin okuma (RFC 4180) ve Excel'den yapıştırılan veri.
// Türkçe Excel "CSV (virgülle ayrılmış)" çıktısı: ';' ayırıcı, ondalık virgül, windows-1254 kodlama.
import { IstatistikHatasi } from '../cekirdek/hata.js';
import { metinCoz, kodlamaGorunenAd } from './kodlama.js';
import { tablodanVeriSetiAyrintili, sayiAyristir } from './tablo.js';

/** Ayırıcı adayları, eşit kanıtta öncelik sırasıyla (sekme ve ';' veri içinde virgülden çok daha seyrek geçer). */
export const AYIRICI_ADAYLARI = Object.freeze(['\t', ';', '|', ',']);

const AYIRICI_ADLARI = { '\t': 'sekme', ';': 'noktalı virgül', '|': 'dikey çizgi', ',': 'virgül' };

/**
 * RFC 4180 CSV ayrıştırıcı: çift tırnaklı alanlar, tırnak içinde "" kaçışı, alan içinde satır sonu,
 * CRLF/LF/CR satır sonları. Hoşgörülü davranışlar: tırnaklı alandan sonra ayırıcıya kadar gelen metin alana
 * eklenir; alan başındaki boşluklardan sonra gelen tırnak alanı tırnaklı sayar; kapanmamış tırnak dosya sonuna
 * kadar tek alan olur (uyarı yazılır).
 * @param {string} metin
 * @param {string} [ayirici]   tek karakter
 * @param {{enFazlaSatir?: number, uyarilar?: string[]}} [secenekler]
 * @returns {string[][]}
 */
export function csvAyristir(metin, ayirici = ',', secenekler = {}) {
  if (typeof ayirici !== 'string' || ayirici.length !== 1) throw new IstatistikHatasi('Ayırıcı tek bir karakter olmalıdır.');
  const enFazla = secenekler.enFazlaSatir ?? Infinity;
  const A = ayirici.charCodeAt(0);
  const T = 34;
  const n = metin.length;
  const satirlar = [];
  let satir = [];
  let i = 0;
  let kapanmamis = false;
  while (i < n) {
    let c = metin.charCodeAt(i);
    // Alan başındaki boşluk + tırnak: ' "a b"' → tırnaklı alan
    if (c === 32 && A !== 32) {
      let k = i;
      while (k < n && metin.charCodeAt(k) === 32) k++;
      if (k < n && metin.charCodeAt(k) === T) {
        i = k;
        c = T;
      }
    }
    let alan;
    if (c === T) {
      i++;
      let bas = i;
      alan = '';
      for (;;) {
        const j = metin.indexOf('"', i);
        if (j < 0) {
          alan += metin.slice(bas);
          i = n;
          kapanmamis = true;
          break;
        }
        if (metin.charCodeAt(j + 1) === T) {
          alan += metin.slice(bas, j + 1);
          i = j + 2;
          bas = i;
          continue;
        }
        alan += metin.slice(bas, j);
        i = j + 1;
        break;
      }
      let k = i;
      while (k < n) {
        const d = metin.charCodeAt(k);
        if (d === A || d === 10 || d === 13) break;
        k++;
      }
      if (k > i) {
        const kalan = metin.slice(i, k);
        if (kalan.trim() !== '') alan += kalan;
      }
      i = k;
    } else {
      let j = i;
      while (j < n) {
        const d = metin.charCodeAt(j);
        if (d === A || d === 10 || d === 13) break;
        j++;
      }
      alan = metin.slice(i, j);
      i = j;
    }
    satir.push(alan);
    if (i >= n) break;
    c = metin.charCodeAt(i);
    if (c === A) {
      i++;
      if (i === n) satir.push('');
      continue;
    }
    i += c === 13 && metin.charCodeAt(i + 1) === 10 ? 2 : 1;
    satirlar.push(satir);
    satir = [];
    if (satirlar.length >= enFazla) return satirlar;
  }
  if (satir.length > 0) satirlar.push(satir);
  if (kapanmamis && secenekler.uyarilar) {
    secenekler.uyarilar.push('Dosyada kapanmamış bir tırnak işareti (") var; dosyanın sonuna kadar olan kısım tek hücre olarak okundu.');
  }
  return satirlar;
}

/**
 * Ayırıcıyı tahmin eder: her aday için ilk ~50 kayıt (tırnaklar dikkate alınarak) ayrıştırılır; boş olmayan
 * kayıtlar içinde en sık alan sayısı (mod) ve bu sayıya sahip kayıt oranı (tutarlılık) hesaplanır.
 * Mod ≥ 2 ve tutarlılık ≥ 0,8 olan adaylar uygundur; uygunlar arasında tutarlılığı en yüksek olanlardan
 * (0,1 içinde eşit sayılır) öncelik sırası sekme > ; > | > , ile seçilir. Uygun aday yoksa tek sütunlu veri
 * kabul edilir.
 * @param {string} metin
 * @returns {{ayirici: string, alanSayisi: number, tutarlilik: number}}
 */
export function ayiriciBul(metin) {
  const ornek = metin.length > 65536 ? metin.slice(0, 65536) : metin;
  const sonuclar = [];
  for (const aday of AYIRICI_ADAYLARI) {
    let kayitlar = csvAyristir(ornek, aday, { enFazlaSatir: 51 });
    if (ornek.length < metin.length && kayitlar.length > 1) kayitlar = kayitlar.slice(0, -1); // kesilmiş son kayıt
    kayitlar = kayitlar.filter((k) => k.length > 1 || (k[0] ?? '').trim() !== '');
    if (kayitlar.length === 0) continue;
    const sayac = new Map();
    for (const k of kayitlar) sayac.set(k.length, (sayac.get(k.length) || 0) + 1);
    let mod = 1;
    let modSayisi = 0;
    for (const [u, c] of sayac) if (c > modSayisi || (c === modSayisi && u > mod)) [mod, modSayisi] = [u, c];
    sonuclar.push({ ayirici: aday, alanSayisi: mod, tutarlilik: modSayisi / kayitlar.length });
  }
  // Tek sütunlu ondalık virgüllü veri ("Yaş\n12,5\n13,5"): virgül ayırıcı değil ondalık işaretidir
  const satirlar = ornek.split(/\r\n|\n|\r/).map((s) => s.trim()).filter(Boolean).slice(0, 51);
  const virgulluSayi = satirlar.filter((s) => s.includes(',') && !Number.isNaN(sayiAyristir(s.replace(/^"|"$/g, ''), ','))).length;
  const virgulluSatir = satirlar.filter((s) => s.includes(',')).length;
  const tekSutunVirgul = virgulluSatir > 0 && virgulluSayi >= 0.9 * virgulluSatir && !satirlar.some((s) => /[;\t|]/.test(s));
  const uygun = sonuclar.filter((s) => s.alanSayisi >= 2 && s.tutarlilik >= 0.8 && !(tekSutunVirgul && s.ayirici === ','));
  if (uygun.length === 0) {
    const enIyi = sonuclar.filter((s) => s.alanSayisi >= 2 && !(tekSutunVirgul && s.ayirici === ',')).sort((a, b) => b.tutarlilik - a.tutarlilik)[0];
    if (enIyi && enIyi.tutarlilik >= 0.5) return enIyi;
    // Tek sütun: metinde geçmeyen bir ayırıcıyla ayrıştırılır ki "12,5" bölünmesin
    const yok = AYIRICI_ADAYLARI.find((a) => !ornek.includes(a));
    return { ayirici: yok || ',', alanSayisi: 1, tutarlilik: 1 };
  }
  const enYuksek = Math.max(...uygun.map((s) => s.tutarlilik));
  return uygun.find((s) => s.tutarlilik >= enYuksek - 0.1);
}

/**
 * @typedef {import('./tablo.js').TabloSecenekleri & {ayirici?: 'otomatik'|string, kodlama?: 'otomatik'|string}} CsvSecenekleri
 */

/**
 * CSV metnini (çözülmüş) VeriSeti'ne çevirir; ayrıntılı bilgiyle.
 * @param {string} metin
 * @param {CsvSecenekleri} [secenekler]
 * @returns {{veriSeti: import('./model.js').VeriSeti, bilgi: object}}
 */
export function csvMetniOkuAyrintili(metin, secenekler = {}) {
  if (metin.trim() === '') throw new IstatistikHatasi('Dosya boş: okunacak veri yok.');
  const uyarilar = [...(secenekler.uyarilar || [])];
  let ayirici = secenekler.ayirici;
  let tutarlilik = null;
  let alanSayisi = null;
  if (!ayirici || ayirici === 'otomatik') {
    const b = ayiriciBul(metin);
    ayirici = b.ayirici;
    tutarlilik = b.tutarlilik;
    alanSayisi = b.alanSayisi;
  }
  const satirlar = csvAyristir(metin, ayirici, { uyarilar });
  // Virgül alan ayırıcıysa ondalık işareti noktadır; tek sütunlu veride Türkçe varsayılan (virgül) kullanılır
  const varsayilanAyirici = ayirici === ',' && alanSayisi !== 1 ? '.' : ',';
  const { veriSeti, bilgi } = tablodanVeriSetiAyrintili(satirlar, {
    kaynak: 'csv',
    varsayilanAyirici,
    duzensizSatirUyarisi: true,
    ...secenekler,
    uyarilar,
  });
  return { veriSeti, bilgi: { ...bilgi, ayirici, ayiriciAdi: AYIRICI_ADLARI[ayirici] || ayirici, ayiriciTutarliligi: tutarlilik } };
}

/** CSV metnini VeriSeti'ne çevirir. */
export function csvMetniOku(metin, secenekler = {}) {
  return csvMetniOkuAyrintili(metin, secenekler).veriSeti;
}

/**
 * CSV dosyasının baytlarını okur: kodlama algılama (UTF-8/BOM, UTF-16, windows-1254) → ayırıcı → tablo.
 * `bilgi.kodlama` algılanan kodlamadır; UTF-8 dışı kodlamalarda kullanıcıya uyarı da yazılır.
 * @param {ArrayBuffer|Uint8Array} veri
 * @param {CsvSecenekleri} [secenekler]
 * @returns {{veriSeti: import('./model.js').VeriSeti, bilgi: object}}
 */
export function csvOkuAyrintili(veri, secenekler = {}) {
  const bytes = veri instanceof Uint8Array ? veri : new Uint8Array(veri);
  if (bytes.length === 0) throw new IstatistikHatasi('Dosya boş: okunacak veri yok.');
  const { metin, kodlama, bom } = metinCoz(bytes, { kodlama: secenekler.kodlama });
  const uyarilar = [...(secenekler.uyarilar || [])];
  if (kodlama !== 'utf-8' && (!secenekler.kodlama || secenekler.kodlama === 'otomatik')) {
    uyarilar.push(`Dosyanın karakter kodlaması ${kodlamaGorunenAd(kodlama)} olarak algılandı. Türkçe karakterler bozuk görünüyorsa kodlamayı elle seçin.`);
  }
  if (metin.indexOf('\0') >= 0) {
    throw new IstatistikHatasi('Dosya metin (CSV) dosyası gibi görünmüyor; ikili (binary) veri içeriyor.');
  }
  const sonuc = csvMetniOkuAyrintili(metin, { ...secenekler, uyarilar });
  return { veriSeti: sonuc.veriSeti, bilgi: { ...sonuc.bilgi, kodlama, bom } };
}

/** CSV dosyası baytlarını VeriSeti'ne çevirir. */
export function csvOku(veri, secenekler = {}) {
  return csvOkuAyrintili(veri, secenekler).veriSeti;
}

/**
 * Excel'den/Google E-Tablolar'dan kopyalanıp yapıştırılan metni okur. Pano verisi sekmeyle ayrılmıştır
 * (hücre içinde satır sonu varsa hücre çift tırnaklıdır); sekme yoksa ayırıcı tahmin edilir.
 * Türkçe Excel görünen metni kopyaladığı için ondalık virgül varsayılandır (kanıt varsa sütun sütun düzeltilir).
 * @param {string} metin
 * @param {CsvSecenekleri} [secenekler]
 * @returns {import('./model.js').VeriSeti}
 */
export function yapistirilanMetinOku(metin, secenekler = {}) {
  const temiz = String(metin ?? '').replace(/^﻿/, '');
  if (temiz.trim() === '') throw new IstatistikHatasi('Yapıştırılan metin boş.');
  const ayirici = secenekler.ayirici && secenekler.ayirici !== 'otomatik' ? secenekler.ayirici : temiz.includes('\t') ? '\t' : 'otomatik';
  return csvMetniOkuAyrintili(temiz, {
    ad: 'Yapıştırılan veri',
    kaynak: 'yapistir',
    ...secenekler,
    ayirici,
  }).veriSeti;
}
