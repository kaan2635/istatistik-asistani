// Analizler için veri setinden değişken çekme yardımcıları. Eksik değerler (boş hücre ve kullanıcı tanımlı
// eksik kodlar) ile vaka filtresi burada uygulanır; analiz modülleri temiz dizilerle çalışır.

import { IstatistikHatasi } from '../cekirdek/hata.js';
import { degerEksikMi, duzeyleriBul } from '../veri/tanima.js';

export function degiskenBul(veriSeti, ad) {
  const d = veriSeti.degiskenler.find((x) => x.ad === ad);
  if (!d) throw new IstatistikHatasi(`"${ad}" adlı değişken bulunamadı.`);
  return d;
}

/** Değişkenin görünen adı: etiket varsa "ad (etiket)" değil, yalnızca etiket; yoksa ad */
export function gorunenAd(d) {
  return d.etiket && d.etiket.trim() ? d.etiket.trim() : d.ad;
}

/** Satır, filtre ve eksik değer açısından geçerli mi? */
function satirGecerli(baglam, i) {
  return !baglam.satirMaskesi || baglam.satirMaskesi[i] === 1;
}

/** i. satırdaki değer eksikse null, değilse değer */
export function deger(d, i) {
  const v = d.degerler[i];
  return degerEksikMi(d, v) ? null : v;
}

/** Sayısal değerler (eksikler atılır). Sayıya çevrilemeyen değer varsa hata. */
export function sayisalDizi(veriSeti, ad, baglam) {
  const d = degiskenBul(veriSeti, ad);
  const x = [];
  for (let i = 0; i < veriSeti.satirSayisi; i++) {
    if (!satirGecerli(baglam, i)) continue;
    const v = deger(d, i);
    if (v === null) continue;
    if (typeof v !== 'number') throw new IstatistikHatasi(`"${gorunenAd(d)}" sayısal olmayan değerler içeriyor ("${v}"). Değişken türünü kontrol edin.`);
    x.push(v);
  }
  return x;
}

/**
 * Kategorik değişkenin düzeyleri (sıralı, etiketli). Filtre uygulanmış veriye göre sayılır.
 * @returns {{deger, etiket, n}[]}
 */
export function duzeyler(veriSeti, ad, baglam) {
  const d = degiskenBul(veriSeti, ad);
  if (!baglam?.satirMaskesi) return duzeyleriBul(d);
  const kopya = { ...d, degerler: d.degerler.map((v, i) => (satirGecerli(baglam, i) ? v : null)) };
  return duzeyleriBul(kopya);
}

/**
 * Sayısal bağımlı değişkeni grup değişkeninin düzeylerine göre böler (her ikisi de eksik olmayan satırlar).
 * @returns {{gruplar:number[][], adlar:string[], duzeyler:any[], n:number, dislanan:number}}
 */
export function gruplaraAyir(veriSeti, bagimliAd, grupAd, baglam) {
  const y = degiskenBul(veriSeti, bagimliAd);
  const g = degiskenBul(veriSeti, grupAd);
  const dz = duzeyler(veriSeti, grupAd, baglam);
  const indeks = new Map(dz.map((x, k) => [String(x.deger), k]));
  const gruplar = dz.map(() => []);
  let dislanan = 0;
  for (let i = 0; i < veriSeti.satirSayisi; i++) {
    if (!satirGecerli(baglam, i)) continue;
    const gv = deger(g, i);
    const yv = deger(y, i);
    if (gv === null || yv === null) {
      dislanan++;
      continue;
    }
    if (typeof yv !== 'number') throw new IstatistikHatasi(`"${gorunenAd(y)}" sayısal olmayan değerler içeriyor ("${yv}").`);
    gruplar[indeks.get(String(gv))].push(yv);
  }
  // boş grupları çıkar
  const dolu = gruplar.map((x, k) => [x, k]).filter(([x]) => x.length > 0);
  return {
    gruplar: dolu.map(([x]) => x),
    adlar: dolu.map(([, k]) => dz[k].etiket),
    duzeyler: dolu.map(([, k]) => dz[k].deger),
    n: dolu.reduce((s, [x]) => s + x.length, 0),
    dislanan,
  };
}

/**
 * İki kategorik değişkenin çapraz tablosu (satır = değişken1, sütun = değişken2).
 * @returns {{tablo:number[][], satirlar:{deger,etiket}[], sutunlar:{deger,etiket}[], n:number, dislanan:number}}
 */
export function caprazTablo(veriSeti, satirAd, sutunAd, baglam) {
  const a = degiskenBul(veriSeti, satirAd);
  const b = degiskenBul(veriSeti, sutunAd);
  const sa = duzeyler(veriSeti, satirAd, baglam);
  const su = duzeyler(veriSeti, sutunAd, baglam);
  const ia = new Map(sa.map((x, k) => [String(x.deger), k]));
  const ib = new Map(su.map((x, k) => [String(x.deger), k]));
  const tablo = sa.map(() => su.map(() => 0));
  let n = 0;
  let dislanan = 0;
  for (let i = 0; i < veriSeti.satirSayisi; i++) {
    if (!satirGecerli(baglam, i)) continue;
    const av = deger(a, i);
    const bv = deger(b, i);
    if (av === null || bv === null) {
      dislanan++;
      continue;
    }
    tablo[ia.get(String(av))][ib.get(String(bv))]++;
    n++;
  }
  return { tablo, satirlar: sa, sutunlar: su, n, dislanan };
}

/**
 * Birden çok değişkenin, hepsinin dolu olduğu satırlardaki değerleri (liste bazında silme).
 * @returns {{matris:(number|string)[][], satirIndeksleri:number[], dislanan:number}} matris[i][j]: i. vaka, j. değişken
 */
export function tamVakalar(veriSeti, adlar, baglam, { sayisal = true } = {}) {
  const ds = adlar.map((ad) => degiskenBul(veriSeti, ad));
  const matris = [];
  const satirIndeksleri = [];
  let dislanan = 0;
  for (let i = 0; i < veriSeti.satirSayisi; i++) {
    if (!satirGecerli(baglam, i)) continue;
    const satir = [];
    let tam = true;
    for (const d of ds) {
      const v = deger(d, i);
      if (v === null) {
        tam = false;
        break;
      }
      if (sayisal && typeof v !== 'number') throw new IstatistikHatasi(`"${gorunenAd(d)}" sayısal olmayan değerler içeriyor ("${v}").`);
      satir.push(v);
    }
    if (tam) {
      matris.push(satir);
      satirIndeksleri.push(i);
    } else dislanan++;
  }
  return { matris, satirIndeksleri, dislanan };
}

/** Kategorik değişkenin satır değerleri (eksik = null), filtre uygulanmış satırlar için */
export function kategorikDizi(veriSeti, ad, baglam) {
  const d = degiskenBul(veriSeti, ad);
  const sonuc = [];
  for (let i = 0; i < veriSeti.satirSayisi; i++) {
    if (!satirGecerli(baglam, i)) continue;
    sonuc.push(deger(d, i));
  }
  return sonuc;
}

/** Bir değerin etiketini döndürür */
export function etiket(d, v) {
  if (v === null || v === undefined) return '';
  return d.degerEtiketleri?.[String(v)] ?? String(v);
}

/** Filtre sonrası analizdeki vaka sayısı */
export function etkinSatirSayisi(veriSeti, baglam) {
  if (!baglam?.satirMaskesi) return veriSeti.satirSayisi;
  let n = 0;
  for (let i = 0; i < veriSeti.satirSayisi; i++) if (baglam.satirMaskesi[i] === 1) n++;
  return n;
}
