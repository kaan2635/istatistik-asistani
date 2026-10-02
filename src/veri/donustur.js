// Yeni değişken üretme araçları: hesaplama, kategorilere ayırma, yeniden kodlama, ölçek puanı, tarih farkı.
// Hepsi yeni bir Degisken nesnesi döndürür; veri setine eklemek çağıranın işidir.

import { IstatistikHatasi } from '../cekirdek/hata.js';
import { yuzdelik } from '../cekirdek/betimsel.js';
import { ifadeDerle } from './ifade.js';
import { degerEksikMi } from './tanima.js';

function yeniDegisken(ad, degerler, ozellik = {}) {
  return {
    ad,
    etiket: '',
    veriTuru: 'sayi',
    degerler,
    olcum: null,
    degerEtiketleri: null,
    eksikKodlar: null,
    ondalik: null,
    kaynakSutun: -1,
    turetilmis: true,
    ...ozellik,
  };
}

export function adKullanilabilir(veriSeti, ad) {
  if (!ad || !ad.trim()) return 'Değişken adı boş olamaz.';
  if (veriSeti.degiskenler.some((d) => d.ad.toLocaleLowerCase('tr') === ad.trim().toLocaleLowerCase('tr'))) return `"${ad}" adında bir değişken zaten var.`;
  return null;
}

function etkinDegerAlici(veriSeti) {
  const harita = new Map(veriSeti.degiskenler.map((d) => [d.ad, d]));
  return (i) => (ad) => {
    const d = harita.get(ad);
    const v = d.degerler[i];
    if (degerEksikMi(d, v)) return null;
    return v;
  };
}

/** Formülle yeni değişken (SPSS COMPUTE) */
export function hesapla(veriSeti, ad, ifadeMetni) {
  const derlenmis = ifadeDerle(ifadeMetni, veriSeti.degiskenler.map((d) => d.ad));
  const al = etkinDegerAlici(veriSeti);
  const degerler = new Array(veriSeti.satirSayisi);
  let gecerli = 0;
  for (let i = 0; i < veriSeti.satirSayisi; i++) {
    const v = derlenmis.hesapla(al(i));
    degerler[i] = typeof v === 'number' && Number.isFinite(v) ? v : null;
    if (degerler[i] !== null) gecerli++;
  }
  if (gecerli === 0) throw new IstatistikHatasi('Formül hiçbir satırda geçerli bir sayı üretmedi. Değişken türlerini ve formülü kontrol edin.');
  return yeniDegisken(ad, degerler, { etiket: '', formul: ifadeMetni });
}

/**
 * Sürekli değişkeni kesim noktalarıyla kategorilere ayırır. Aralıklar [a, b) biçimindedir; son aralık üstten açıktır.
 * @param {number[]} kesimler artan sırada
 * @param {string[]} [etiketler] kesimler.length + 1 adet
 */
export function kategorize(veriSeti, kaynakAd, ad, kesimler, etiketler, ondalikAyrac = ',') {
  const d = veriSeti.degiskenler.find((x) => x.ad === kaynakAd);
  if (!d) throw new IstatistikHatasi('Kaynak değişken bulunamadı.');
  const k = [...new Set(kesimler)].sort((a, b) => a - b);
  if (!k.length) throw new IstatistikHatasi('En az bir kesim noktası girin.');
  const yaz = (x) => String(x).replace('.', ondalikAyrac);
  const otomatik = [`<${yaz(k[0])}`];
  for (let j = 1; j < k.length; j++) otomatik.push(`${yaz(k[j - 1])}–<${yaz(k[j])}`);
  otomatik.push(`≥${yaz(k[k.length - 1])}`);
  const ad2 = etiketler && etiketler.length === k.length + 1 ? etiketler : otomatik;
  const degerler = d.degerler.map((v) => {
    if (degerEksikMi(d, v) || typeof v !== 'number') return null;
    let s = 0;
    while (s < k.length && v >= k[s]) s++;
    return s + 1;
  });
  const degerEtiketleri = {};
  ad2.forEach((e, j) => {
    degerEtiketleri[String(j + 1)] = e;
  });
  return yeniDegisken(ad, degerler, { olcum: 'sirali', degerEtiketleri, etiket: `${kaynakAd} (gruplanmış)` });
}

/** Eşit sayıda gözlem içeren gruplar için kesim noktaları (medyan: 2, tersil: 3, kartil: 4) */
export function yuzdelikKesimleri(veriSeti, kaynakAd, grupSayisi, tip = 6) {
  const d = veriSeti.degiskenler.find((x) => x.ad === kaynakAd);
  const x = d.degerler.filter((v) => !degerEksikMi(d, v) && typeof v === 'number');
  const kesimler = [];
  for (let j = 1; j < grupSayisi; j++) kesimler.push(yuzdelik(x, j / grupSayisi, tip));
  return kesimler;
}

/**
 * Değerleri yeniden kodlar (kategori birleştirme). eslesme: eski değer (String) → yeni etiket. Eşleşmeyen değerler eksik olur
 * ya da `digerleriniKoru` true ise kendi etiketleriyle kalır.
 */
export function yenidenKodla(veriSeti, kaynakAd, ad, eslesme, { digerleriniKoru = true, sira = null } = {}) {
  const d = veriSeti.degiskenler.find((x) => x.ad === kaynakAd);
  if (!d) throw new IstatistikHatasi('Kaynak değişken bulunamadı.');
  const yeniEtiketler = [];
  const degerler = d.degerler.map((v) => {
    if (degerEksikMi(d, v)) return null;
    const anahtar = String(v);
    let yeni = eslesme[anahtar];
    if (yeni === undefined || yeni === null || String(yeni).trim() === '') {
      if (!digerleriniKoru) return null;
      yeni = d.degerEtiketleri?.[anahtar] ?? anahtar;
    }
    yeni = String(yeni).trim();
    if (!yeniEtiketler.includes(yeni)) yeniEtiketler.push(yeni);
    return yeni;
  });
  const duzeySirasi = sira ? sira.filter((e) => yeniEtiketler.includes(e)) : null;
  return yeniDegisken(ad, degerler, { veriTuru: 'metin', olcum: 'kategorik', duzeySirasi, etiket: `${kaynakAd} (yeniden kodlanmış)` });
}

/**
 * Ölçek puanı (madde toplamı ya da ortalaması), ters maddeler (min + maks - x) ile.
 * @param {{ters?:string[], min?:number, maks?:number, yontem?:'toplam'|'ortalama', enAzGecerli?:number}} s
 *   enAzGecerli: bu sayıdan az geçerli maddesi olan kişinin puanı eksik olur (varsayılan: tüm maddeler).
 */
export function olcekPuani(veriSeti, maddeler, ad, { ters = [], min = null, maks = null, yontem = 'toplam', enAzGecerli = null } = {}) {
  const ds = maddeler.map((m) => veriSeti.degiskenler.find((x) => x.ad === m));
  if (ds.some((x) => !x)) throw new IstatistikHatasi('Maddelerden biri bulunamadı.');
  if (ters.length && (min === null || maks === null)) throw new IstatistikHatasi('Ters maddeler için ölçeğin en küçük ve en büyük değerini girin.');
  const gerekli = enAzGecerli ?? maddeler.length;
  const degerler = new Array(veriSeti.satirSayisi);
  for (let i = 0; i < veriSeti.satirSayisi; i++) {
    const gecerli = [];
    ds.forEach((d) => {
      const v = d.degerler[i];
      if (degerEksikMi(d, v) || typeof v !== 'number') return;
      gecerli.push(ters.includes(d.ad) ? min + maks - v : v);
    });
    if (gecerli.length < gerekli || gecerli.length === 0) {
      degerler[i] = null;
      continue;
    }
    const ort = gecerli.reduce((a, b) => a + b, 0) / gecerli.length;
    // Toplam puanda eksik madde varsa ortalama × madde sayısı (orantılı toplam)
    degerler[i] = yontem === 'ortalama' ? ort : gecerli.length === maddeler.length ? gecerli.reduce((a, b) => a + b, 0) : ort * maddeler.length;
  }
  return yeniDegisken(ad, degerler, { olcum: 'surekli', etiket: `${yontem === 'ortalama' ? 'Ortalama' : 'Toplam'} puan (${maddeler.length} madde)` });
}

/** Ters kodlanmış madde (min + maks − x) */
export function tersKodla(veriSeti, kaynakAd, ad, min, maks) {
  const d = veriSeti.degiskenler.find((x) => x.ad === kaynakAd);
  const degerler = d.degerler.map((v) => (degerEksikMi(d, v) || typeof v !== 'number' ? null : min + maks - v));
  return yeniDegisken(ad, degerler, { olcum: d.olcum, etiket: `${kaynakAd} (ters)` });
}

const BIRIM_MS = { gun: 86400000, hafta: 7 * 86400000, ay: 30.4375 * 86400000, yil: 365.25 * 86400000 };

/** İki tarih arasındaki süre (bitiş − başlangıç) */
export function tarihFarki(veriSeti, baslangicAd, bitisAd, ad, birim = 'gun') {
  const a = veriSeti.degiskenler.find((x) => x.ad === baslangicAd);
  const b = veriSeti.degiskenler.find((x) => x.ad === bitisAd);
  if (!a || !b) throw new IstatistikHatasi('Tarih değişkenleri bulunamadı.');
  if (a.veriTuru !== 'tarih' || b.veriTuru !== 'tarih') throw new IstatistikHatasi('Her iki değişken de tarih türünde olmalı.');
  const degerler = a.degerler.map((v, i) => {
    const w = b.degerler[i];
    if (degerEksikMi(a, v) || degerEksikMi(b, w)) return null;
    return (w - v) / BIRIM_MS[birim];
  });
  return yeniDegisken(ad, degerler, { olcum: 'surekli', etiket: `${baslangicAd} → ${bitisAd} (${birim})` });
}
