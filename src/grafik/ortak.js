// Grafik altyapısı: seçenekler ve temalar, "güzel" eksen adımları, ölçekler, sayı biçimlendirme,
// metin genişliği tahmini ve satır kaydırma, SVG öğe üretimi (Tuval), eksen/gösterge/başlık yerleşimi.
//
// Her şey saftır; DOM kullanılmaz (Node testlerinde çalışır). Tüm biçimler öğelerin üzerinde açık
// niteliklerle yazılır (fill, stroke, font-family...); CSS sınıfı ve dış yazı tipi yoktur. Böylece aynı SVG
// uygulamada, indirilen .svg dosyasında, PNG'ye çevrildiğinde ve Word'e eklendiğinde aynı görünür.

/* =====================================================================
 * Seçenekler ve temalar
 * ===================================================================== */

/** Her grafiğin ortak seçenekleri ve varsayılanları */
export const VARSAYILAN_SECENEKLER = Object.freeze({
  genislik: 640,
  yukseklik: 420,
  baslik: '',
  xEtiket: '',
  yEtiket: '',
  ondalikAyrac: ',',
  /** Binlik ayırıcı; verilmezse ondalık ayırıcı ',' iken '.', '.' iken ','. '' gruplamayı kapatır. */
  binlikAyrac: undefined,
  yaziTipi: 'Arial, Helvetica, sans-serif',
  yaziBoyutu: 12,
  /** 'yayin' (siyah-gri, gri tonlu baskıda ayırt edilir) | 'renkli' (renk körlüğüne uygun palet) */
  tema: 'yayin',
  /** İsteğe bağlı seri renkleri; verilirse temanın seri renklerinin yerine geçer */
  renkler: undefined,
  /** 'ust' | 'sag' | 'yok' — gösterge her zaman çizim alanının dışında durur */
  gostergeKonumu: 'ust',
  /** Yardımcı ızgara çizgileri; verilmezse 'yayin' temasında kapalı, 'renkli' temasında açık */
  izgara: undefined,
  /** 1'i aşamayan değerlerde (r, p) baştaki sıfır: true → "0,45", false → ",45" (APA) */
  basSifir: true,
});

/** Renk körlüğüne uygun kategorik palet (8 renk; beyaz zeminde doğrulandı: komşu CVD ΔE ≥ 9,1) */
export const RENKLI_PALET = Object.freeze(['#2a78d6', '#eb6834', '#1baf7a', '#eda100', '#e87ba4', '#008300', '#4a3aa7', '#e34948']);

/** İşaret (nokta) şekilleri; seriler rengin yanında şekille de ayrılır */
export const SEKILLER = Object.freeze(['daire', 'kare', 'ucgen', 'elmas', 'ucgenAsagi', 'arti', 'carpi', 'yildiz']);

// Yayın teması dolguları: önce gri tonları, sonra 45°/135° tarama ve noktalı desenler (siyah kenarlı).
const YAYIN_DOLGULAR = [
  { dolgu: '#404040' },
  { dolgu: '#bfbfbf' },
  { dolgu: '#ffffff' },
  { dolgu: '#808080' },
  { dolgu: '#ffffff', desen: 'capraz45' },
  { dolgu: '#e6e6e6', desen: 'capraz135' },
  { dolgu: '#ffffff', desen: 'kafes' },
  { dolgu: '#ffffff', desen: 'nokta' },
  { dolgu: '#bfbfbf', desen: 'capraz45' },
  { dolgu: '#bfbfbf', desen: 'nokta' },
];
const YAYIN_KESIKLER = ['', '6 3', '2 2', '8 3 2 3', '12 4', '1 3', '8 2 2 2 2 2', '4 4'];

export const TEMALAR = Object.freeze({
  yayin: Object.freeze({
    ad: 'yayin',
    arkaPlan: '#ffffff',
    murekkep: '#000000',
    ikincil: '#000000',
    soluk: '#595959',
    eksen: '#000000',
    eksenKalinlik: 0.8,
    izgara: '#d9d9d9',
    izgaraVarsayilan: false,
    referans: '#595959',
    cizgiKalinlik: 1.5,
    bantRenk: '#000000',
    bantOpaklik: 0.13,
    tekDolgu: '#bfbfbf',
    tekRenk: '#000000',
    kutuDolgu: '#d9d9d9',
    kutuCizgi: '#000000',
    kenar: '#000000',
    kenarKalinlik: 0.75,
    noktaCizgi: '#000000',
    noktaDolgu: '#ffffff',
    noktaOpaklik: 1,
    // Gri tonlu ısı haritası mutlak değeri kodlar; işaret hücredeki sayıyla okunur.
    iraksak: ['#3a3a3a', '#ffffff', '#3a3a3a'],
  }),
  renkli: Object.freeze({
    ad: 'renkli',
    arkaPlan: '#ffffff',
    murekkep: '#0b0b0b',
    ikincil: '#3d3c39',
    soluk: '#6b6a65',
    eksen: '#6b6a65',
    eksenKalinlik: 0.8,
    izgara: '#e6e5e0',
    izgaraVarsayilan: true,
    referans: '#6b6a65',
    cizgiKalinlik: 2,
    bantRenk: '#2a78d6',
    bantOpaklik: 0.16,
    tekDolgu: '#2a78d6',
    tekRenk: '#1c5cab',
    kutuDolgu: '#cde2fb',
    kutuCizgi: '#1c5cab',
    kenar: '#ffffff',
    kenarKalinlik: 1,
    noktaCizgi: '#ffffff',
    noktaDolgu: '#2a78d6',
    noktaOpaklik: 0.85,
    // −1 kırmızı, 0 nötr gri, +1 mavi (corrplot geleneği: pozitif mavi)
    iraksak: ['#b8322f', '#f5f5f3', '#1c5cab'],
  }),
});

function yarimYuvarla(v) {
  return Math.round(v * 2) / 2;
}

/**
 * Kullanıcı seçeneklerini varsayılanlarla birleştirir, temayı ve yazı boyutlarını çözer.
 * ozel: grafiğe özgü varsayılanlar (ör. {yEtiket: 'Frekans'}).
 */
export function secenekHazirla(secenek = {}, ozel = {}) {
  const temiz = {};
  for (const [k, v] of Object.entries(secenek || {})) if (v !== undefined) temiz[k] = v;
  const s = { ...VARSAYILAN_SECENEKLER, ...ozel, ...temiz };
  const gen = Number(s.genislik);
  const yuk = Number(s.yukseklik);
  s.genislik = Number.isFinite(gen) && gen >= 100 ? Math.round(gen) : Math.max(100, VARSAYILAN_SECENEKLER.genislik);
  s.yukseklik = Number.isFinite(yuk) && yuk >= 80 ? Math.round(yuk) : Math.max(80, VARSAYILAN_SECENEKLER.yukseklik);
  const yb = Number.isFinite(Number(s.yaziBoyutu)) && Number(s.yaziBoyutu) >= 6 ? Number(s.yaziBoyutu) : 12;
  s.yaziBoyutu = yb;
  s.T = TEMALAR[s.tema] || TEMALAR.yayin;
  s.tema = s.T.ad;
  s.fs = {
    baslik: yarimYuvarla(yb * 1.17),
    eksen: yb,
    tik: yarimYuvarla(yb * 0.92),
    gosterge: yarimYuvarla(yb * 0.92),
    kucuk: yarimYuvarla(yb * 0.85),
  };
  s.ondalikAyrac = s.ondalikAyrac === '.' ? '.' : ',';
  s.binlik = s.binlikAyrac ?? (s.ondalikAyrac === ',' ? '.' : ',');
  s.izgara = s.izgara ?? s.T.izgaraVarsayilan;
  s.kenar = Math.round(yb * 0.9);
  if (!['ust', 'sag', 'yok'].includes(s.gostergeKonumu)) s.gostergeKonumu = 'ust';
  s.baslik = s.baslik == null ? '' : String(s.baslik);
  s.xEtiket = s.xEtiket == null ? '' : String(s.xEtiket);
  s.yEtiket = s.yEtiket == null ? '' : String(s.yEtiket);
  return s;
}

/**
 * i'nci serinin görsel stili. Renkli temada 8 renkten sonra renk + tarama deseni birleşimi kullanılır
 * (9. bir renk üretilmez). Yayın temasında gri tonları, desenler, kesikli çizgiler ve dolu/boş işaretler.
 */
export function seriStili(s, i) {
  const sekil = SEKILLER[i % SEKILLER.length];
  const ozel = Array.isArray(s.renkler) && s.renkler.length ? s.renkler[i % s.renkler.length] : null;
  if (s.T.ad === 'renkli' || ozel) {
    const renk = ozel || RENKLI_PALET[i % RENKLI_PALET.length];
    const tur = ozel ? 0 : Math.floor(i / RENKLI_PALET.length);
    return {
      renk,
      dolgu: renk,
      desen: tur >= 1 ? ['capraz45', 'capraz135', 'nokta'][(tur - 1) % 3] : null,
      desenRenk: '#ffffff',
      kesik: '',
      kalinlik: s.T.cizgiKalinlik,
      sekil,
      isaretDolu: true,
      kenar: s.T.ad === 'renkli' ? '#ffffff' : '#000000',
      kenarKalinlik: s.T.ad === 'renkli' ? 1 : 0.75,
    };
  }
  const d = YAYIN_DOLGULAR[i % YAYIN_DOLGULAR.length];
  return {
    renk: '#000000',
    dolgu: d.dolgu,
    desen: d.desen || null,
    desenRenk: '#000000',
    kesik: YAYIN_KESIKLER[i % YAYIN_KESIKLER.length],
    kalinlik: s.T.cizgiKalinlik,
    sekil,
    isaretDolu: i % 2 === 0,
    kenar: '#000000',
    kenarKalinlik: 0.75,
  };
}

/** Tek serili grafiklerin (histogram, frekans, kutu) stili */
export function tekSeriStili(s) {
  const ozel = Array.isArray(s.renkler) && s.renkler.length ? s.renkler[0] : null;
  return {
    renk: ozel || s.T.tekRenk,
    dolgu: ozel || s.T.tekDolgu,
    desen: null,
    kesik: '',
    kalinlik: s.T.cizgiKalinlik,
    sekil: 'daire',
    isaretDolu: true,
    kenar: s.T.kenar,
    kenarKalinlik: s.T.kenarKalinlik,
  };
}

/* =====================================================================
 * Renk yardımcıları
 * ===================================================================== */

function hexRgb(hex) {
  let h = String(hex).trim().replace('#', '');
  if (h.length === 3) h = h.split('').map((c) => c + c).join('');
  const n = parseInt(h.slice(0, 6), 16);
  if (!Number.isFinite(n) || h.length < 6) return [0, 0, 0];
  return [(n >> 16) & 255, (n >> 8) & 255, n & 255].map((v) => v / 255);
}

function rgbHex(rgb) {
  return '#' + rgb.map((v) => Math.round(Math.min(1, Math.max(0, v)) * 255).toString(16).padStart(2, '0')).join('');
}

const srgbDogrusal = (c) => (c <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4);
const dogrusalSrgb = (c) => (c <= 0.0031308 ? 12.92 * c : 1.055 * Math.max(0, c) ** (1 / 2.4) - 0.055);

function oklab(hex) {
  const [r, g, b] = hexRgb(hex).map(srgbDogrusal);
  const l = Math.cbrt(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b);
  const m = Math.cbrt(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b);
  const s = Math.cbrt(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b);
  return [
    0.2104542553 * l + 0.793617785 * m - 0.0040720468 * s,
    1.9779984951 * l - 2.428592205 * m + 0.4505937099 * s,
    0.0259040371 * l + 0.7827717662 * m - 0.808675766 * s,
  ];
}

function oklabHex([L, A, B]) {
  const l = (L + 0.3963377774 * A + 0.2158037573 * B) ** 3;
  const m = (L - 0.1055613458 * A - 0.0638541728 * B) ** 3;
  const s = (L - 0.0894841775 * A - 1.291485548 * B) ** 3;
  const rgb = [
    4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
    -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
    -0.0041960863 * l - 0.7034186147 * m + 1.707614701 * s,
  ];
  return rgbHex(rgb.map(dogrusalSrgb));
}

/** İki rengi algısal olarak düzgün OKLab uzayında karıştırır (t: 0 → a, 1 → b) */
export function renkKaristir(a, b, t) {
  const p = oklab(a);
  const q = oklab(b);
  const u = Math.min(1, Math.max(0, t));
  return oklabHex([0, 1, 2].map((i) => p[i] + (q[i] - p[i]) * u));
}

/** Göreli parlaklık (WCAG) */
export function parlaklik(hex) {
  const [r, g, b] = hexRgb(hex).map(srgbDogrusal);
  return 0.2126 * r + 0.7152 * g + 0.0722 * b;
}

/** Verilen dolgunun üzerinde okunaklı metin rengi (siyah ya da beyaz) */
export function metinRengi(dolgu) {
  return parlaklik(dolgu) > 0.32 ? '#000000' : '#ffffff';
}

/** −1…1 arasındaki değeri temanın ıraksak ölçeğinde renge çevirir */
export function iraksakRenk(s, v) {
  const [neg, orta, poz] = s.T.iraksak;
  const t = Math.max(-1, Math.min(1, v));
  return t < 0 ? renkKaristir(orta, neg, -t) : renkKaristir(orta, poz, t);
}

/* =====================================================================
 * Metin: kaçış, genişlik tahmini, kaydırma
 * ===================================================================== */

/** XML özel karakterlerini kaçırır */
export function kacis(metin) {
  return String(metin)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;');
}

// Helvetica/Arial karakter genişlikleri (1/1000 em), ASCII 32–126.
const ASCII_GENISLIK = [
  278, 278, 355, 556, 556, 889, 667, 191, 333, 333, 389, 584, 278, 333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556,
  556, 556, 278, 278, 584, 584, 584, 556, 1015, 667, 667, 722, 722, 667, 611, 778, 722, 278, 500, 667, 556, 833, 722, 778,
  667, 778, 722, 667, 611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556, 333, 556, 556, 500, 556, 556, 278, 556,
  556, 222, 222, 500, 222, 833, 556, 556, 556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500, 334, 260, 334, 584,
];
const OZEL_GENISLIK = {
  'ı': 278, '−': 584, '–': 556, '—': 1000, '…': 1000, '±': 584, '×': 584, '÷': 584, '²': 333, '³': 333, '≤': 549,
  '≥': 549, '≠': 549, '·': 278, '°': 400, '’': 222, '‘': 222, '“': 333, '”': 333, ' ': 278, ' ': 167,
  'χ': 549, 'α': 578, 'β': 575, 'μ': 576, 'σ': 603, 'Δ': 612, 'η': 556, 'ω': 690, 'ŷ': 500, '→': 1000, '←': 1000,
};
const AKSAN_TABANI = {
  'ç': 'c', 'Ç': 'C', 'ğ': 'g', 'Ğ': 'G', 'İ': 'I', 'ö': 'o', 'Ö': 'O', 'ş': 's', 'Ş': 'S', 'ü': 'u', 'Ü': 'U',
  'â': 'a', 'Â': 'A', 'î': 'i', 'Î': 'I', 'û': 'u', 'Û': 'U', 'é': 'e', 'É': 'E', 'è': 'e', 'ä': 'a', 'Ä': 'A',
  'à': 'a', 'á': 'a', 'ó': 'o', 'ñ': 'n', 'ë': 'e', 'ï': 'i',
};

function harfGenisligi(ch) {
  const k = ch.codePointAt(0);
  if (k >= 32 && k <= 126) return ASCII_GENISLIK[k - 32];
  if (OZEL_GENISLIK[ch] !== undefined) return OZEL_GENISLIK[ch];
  const taban = AKSAN_TABANI[ch];
  if (taban) return ASCII_GENISLIK[taban.codePointAt(0) - 32];
  return 556;
}

/**
 * Metnin piksel genişliği tahmini (Arial/Helvetica ölçüleri). Kalın yazı ~%7 daha geniştir.
 * icerik: metin ya da parça dizisi [{t, italik, kalin}] / string.
 */
export function metinGenisligi(icerik, boyut, kalin = false) {
  if (Array.isArray(icerik)) {
    return icerik.reduce((t, p) => t + (typeof p === 'string' ? metinGenisligi(p, boyut, kalin) : metinGenisligi(p.t, boyut, kalin || p.kalin)), 0);
  }
  let t = 0;
  for (const ch of String(icerik)) t += harfGenisligi(ch);
  return (t / 1000) * boyut * (kalin ? 1.07 : 1);
}

/** Metni verilen genişliğe sığacak biçimde sonuna "…" koyarak kısaltır */
export function kisalt(metin, maks, boyut, kalin = false) {
  const m = String(metin);
  if (metinGenisligi(m, boyut, kalin) <= maks) return m;
  const harfler = Array.from(m);
  let n = harfler.length;
  while (n > 1 && metinGenisligi(harfler.slice(0, n).join('').trimEnd() + '…', boyut, kalin) > maks) n--;
  return harfler.slice(0, n).join('').trimEnd() + '…';
}

/**
 * Metni sözcük sınırlarından satırlara böler. '\n' zorunlu satır başıdır.
 * secenek: {kalin=false, maksSatir=Infinity, bolme=true}. bolme=true iken sığmayan uzun sözcük tireyle bölünür;
 * false iken satır taşabilir (çağıran genişliği denetler). maksSatir aşılırsa son satır "…" ile kısaltılır.
 */
export function metniSar(metin, maks, boyut, secenek = {}) {
  const { kalin = false, maksSatir = Infinity, bolme = true } = secenek;
  const satirlar = [];
  for (const paragraf of String(metin ?? '').split('\n')) {
    const kelimeler = paragraf.split(/\s+/).filter(Boolean);
    if (!kelimeler.length) {
      satirlar.push('');
      continue;
    }
    let mevcut = '';
    for (let k of kelimeler) {
      if (bolme) {
        while (metinGenisligi(k, boyut, kalin) > maks && Array.from(k).length > 2) {
          const harf = Array.from(k);
          let i = harf.length - 1;
          while (i > 1 && metinGenisligi(harf.slice(0, i).join('') + '-', boyut, kalin) > maks) i--;
          if (mevcut) {
            satirlar.push(mevcut);
            mevcut = '';
          }
          satirlar.push(harf.slice(0, i).join('') + '-');
          k = harf.slice(i).join('');
        }
      }
      const aday = mevcut ? `${mevcut} ${k}` : k;
      if (!mevcut || metinGenisligi(aday, boyut, kalin) <= maks) mevcut = aday;
      else {
        satirlar.push(mevcut);
        mevcut = k;
      }
    }
    if (mevcut) satirlar.push(mevcut);
  }
  if (satirlar.length > maksSatir) {
    const kesik = satirlar.slice(0, maksSatir);
    kesik[maksSatir - 1] = kisalt(satirlar.slice(maksSatir - 1).join(' '), maks, boyut, kalin);
    return kesik;
  }
  return satirlar;
}

/** Bir satır dizisinin en geniş satırı */
export function enGenisSatir(satirlar, boyut, kalin = false) {
  return satirlar.reduce((m, t) => Math.max(m, metinGenisligi(t, boyut, kalin)), 0);
}

/**
 * "p = 0,012", "n = 25" gibi notlardaki istatistik simgelerini italik parçaya ayırır (APA).
 * Döndürür: parça dizisi [string | {t, italik:true}].
 */
export function simgeleriItalikYap(metin) {
  const parcalar = [];
  const re = /(?<![\p{L}\p{N}])(χ²|R²|p|n|N|r|R|t|F|z|U|H|W|M|d)(?=\s*[=<>≤≥])/gu;
  let son = 0;
  let m;
  const str = String(metin);
  while ((m = re.exec(str))) {
    if (m.index > son) parcalar.push(str.slice(son, m.index));
    parcalar.push({ t: m[1], italik: true });
    son = m.index + m[1].length;
  }
  if (son < str.length) parcalar.push(str.slice(son));
  return parcalar;
}

/* =====================================================================
 * Sayı biçimlendirme
 * ===================================================================== */

/**
 * Sayıyı sabit ondalıkla biçimler: ondalık ayırıcı s.ondalikAyrac (varsayılan ','), 5 ve daha çok basamaklı
 * tam kısımlar binlik ayırıcıyla gruplanır (10.000), eksi işareti tipografik "−" (U+2212) olur.
 * Sonlu olmayan değerler "–" döner.
 */
export function sayiBicimle(x, ondalik = 2, s = {}) {
  if (typeof x !== 'number' || !Number.isFinite(x)) return '–';
  const ayrac = s.ondalikAyrac === '.' ? '.' : ',';
  const binlik = s.binlik ?? s.binlikAyrac ?? (ayrac === ',' ? '.' : ',');
  const d = Math.max(0, Math.min(20, Math.round(ondalik)));
  const mutlak = Math.abs(x);
  if (mutlak >= 1e21) {
    const [m, e] = mutlak.toExponential(Math.min(d, 3)).split('e');
    return (x < 0 ? '−' : '') + m.replace('.', ayrac) + ' × 10^' + Number(e);
  }
  const str = mutlak.toFixed(d);
  const negatif = x < 0 && Number(str) !== 0;
  let [tam, kesir] = str.split('.');
  if (binlik && tam.length > 4) tam = tam.replace(/\B(?=(\d{3})+(?!\d))/g, binlik);
  return (negatif ? '−' : '') + tam + (kesir ? ayrac + kesir : '');
}

/** Bir adım değerini tam göstermek için gereken ondalık sayısı (0,25 → 2; 20 → 0) */
export function ondalikSayisi(v) {
  const m = Math.abs(v);
  if (!Number.isFinite(m) || m === 0) return 0;
  for (let d = 0; d <= 12; d++) {
    const k = m * 10 ** d;
    if (Math.abs(k - Math.round(k)) < 1e-7 * Math.max(1, k)) return d;
  }
  return 12;
}

/**
 * Özet değer (eğim, ortalama) için okunaklı biçim: |v| ≥ 1000 → 0 ondalık, ≥ 1 → 2 ondalık,
 * daha küçük değerlerde 3 anlamlı basamak (en çok 6 ondalık).
 */
export function ozetBicimle(v, s = {}) {
  if (!Number.isFinite(v)) return '–';
  const m = Math.abs(v);
  let d;
  if (m >= 1000) d = 0;
  else if (m >= 1 || m === 0) d = 2;
  else d = Math.min(6, 2 - Math.floor(Math.log10(m)));
  return sayiBicimle(v, d, s);
}

/** 1'i aşamayan değerlerde baştaki sıfırı isteğe göre kaldırır ("0,45" → ",45") */
export function basSifirUygula(metin, s = {}) {
  if (s.basSifir !== false) return metin;
  return metin.replace(/^(−?)0(?=[.,])/, '$1');
}

/** Yüzde: Türkçe kullanımda işaret önde ("%45,0"), '.' ayırıcıyla sonda ("45.0%") */
export function yuzdeBicimle(v, ondalik = 1, s = {}) {
  const sayi = sayiBicimle(v, ondalik, s);
  const onde = s.yuzdeOnde ?? s.ondalikAyrac !== '.';
  return onde ? `%${sayi}` : `${sayi}%`;
}

/** p değeri: p < 0,001 → "< 0,001"; aksi hâlde 3 ondalık */
export function pBicimle(p, s = {}) {
  if (!Number.isFinite(p)) return '–';
  if (p < 0.001) return '< ' + basSifirUygula(sayiBicimle(0.001, 3, s), s);
  return basSifirUygula(sayiBicimle(p, 3, s), s);
}

/** Anlamlılık yıldızları: * p < 0,05; ** p < 0,01; *** p < 0,001 */
export function yildizlar(p) {
  if (!Number.isFinite(p)) return '';
  if (p < 0.001) return '***';
  if (p < 0.01) return '**';
  if (p < 0.05) return '*';
  return '';
}

/* =====================================================================
 * Eksen adımları ve ölçekler
 * ===================================================================== */

/**
 * "Güzel" eksen adımları: adım 1, 2, 2,5 ya da 5 × 10^k; uçlar adımın katlarına genişletilir.
 * hedef: istenen aralık sayısı. secenek.tamsayi: adım en az 1 ve tam sayı (frekans eksenleri).
 * Döndürür: {adimlar, adim, alt, ust, ondalik}. Örnek: (0, 97, 5) → 0, 20, 40, 60, 80, 100.
 */
export function guzelAdimlar(min, max, hedef = 5, secenek = {}) {
  let a = Number(min);
  let b = Number(max);
  if (!Number.isFinite(a) || !Number.isFinite(b)) {
    a = 0;
    b = 1;
  }
  if (a > b) [a, b] = [b, a];
  if (b - a <= 1e-12 * Math.max(1, Math.abs(a), Math.abs(b))) {
    const d = a === 0 ? 1 : Math.abs(a) * 0.1;
    a -= d;
    b += d;
  }
  const h = Math.max(1, Math.round(hedef));
  const ham = (b - a) / h;
  const us = Math.floor(Math.log10(ham));
  let en = null;
  for (let e = us - 1; e <= us + 1; e++) {
    for (const m of [1, 2, 2.5, 5]) {
      const adim = m * 10 ** e;
      if (secenek.tamsayi && (adim < 1 || !Number.isInteger(Math.round(adim * 1e9) / 1e9))) continue;
      const ia = Math.floor(a / adim + 1e-9);
      const ib = Math.ceil(b / adim - 1e-9);
      const n = ib - ia;
      if (n < 1) continue;
      const kapsama = (b - a) / (n * adim);
      const puan = Math.abs(n - h) + (m === 2.5 ? 0.5 : 0) + (1 - kapsama) * 1.5;
      if (!en || puan < en.puan - 1e-9) en = { puan, adim, ia, ib };
    }
  }
  if (!en) {
    // tamsayi kısıtıyla aday kalmadı (çok dar aralık): adım 1
    const adim = 1;
    en = { adim, ia: Math.floor(a), ib: Math.max(Math.ceil(b), Math.floor(a) + 1) };
  }
  const ondalik = ondalikSayisi(en.adim);
  const adimlar = [];
  for (let i = en.ia; i <= en.ib; i++) adimlar.push(Number((i * en.adim).toFixed(Math.min(20, ondalik + 2))));
  return { adimlar, adim: en.adim, alt: adimlar[0], ust: adimlar[adimlar.length - 1], ondalik };
}

/**
 * Logaritmik eksen adımları (pozitif aralık). Geniş aralıkta 1×10^k, orta aralıkta 1-2-5 ya da 1-2-3-5,
 * dar aralıkta (maks/min < 2,5) doğrusal güzel adımlar kullanılır.
 * Döndürür: {adimlar, alt, ust}
 */
export function logAdimlar(min, max, hedef = 5) {
  let a = min;
  let b = max;
  if (!(a > 0) || !(b > 0) || !Number.isFinite(a) || !Number.isFinite(b)) return { adimlar: [0.1, 1, 10], alt: 0.1, ust: 10 };
  if (a > b) [a, b] = [b, a];
  if (b / a < 2.5) {
    const g = guzelAdimlar(a, b, Math.max(2, hedef - 1));
    if (g.alt > 0) return { adimlar: g.adimlar, alt: g.alt, ust: g.ust };
    const kat = g.adimlar.filter((v) => v > 0);
    return { adimlar: [a, ...kat.filter((v) => v > a)], alt: a, ust: g.ust };
  }
  const setler = [[1], [1, 2, 5], [1, 2, 3, 5], [1, 2, 3, 4, 5, 6, 7, 8, 9]];
  const veriGen = Math.log10(b / a);
  let en = null;
  for (const set of setler) {
    const adaylar = [];
    for (let e = Math.floor(Math.log10(a)) - 1; e <= Math.ceil(Math.log10(b)) + 1; e++) {
      for (const m of set) adaylar.push(Number((m * 10 ** e).toPrecision(12)));
    }
    adaylar.sort((p, q) => p - q);
    const alt = adaylar.filter((v) => v <= a * (1 + 1e-9)).pop();
    const ust = adaylar.find((v) => v >= b * (1 - 1e-9));
    if (alt === undefined || ust === undefined) continue;
    const adimlar = adaylar.filter((v) => v >= alt && v <= ust);
    const bosluk = (Math.log10(ust / alt) - veriGen) / Math.max(veriGen, 1e-9);
    const puan = Math.abs(adimlar.length - 1 - hedef) + bosluk * 2;
    if (!en || puan < en.puan - 1e-9) en = { puan, adimlar, alt, ust };
  }
  return { adimlar: en.adimlar, alt: en.alt, ust: en.ust };
}

/** Doğrusal ölçek: alan [d0, d1] → aralık [r0, r1]. Sıfır genişlikli alan ortalanır. */
export function dogrusalOlcek(d0, d1, r0, r1) {
  const fark = d1 - d0;
  const f = (v) => (fark === 0 ? (r0 + r1) / 2 : r0 + ((v - d0) / fark) * (r1 - r0));
  f.alan = [d0, d1];
  f.aralik = [r0, r1];
  return f;
}

/** Logaritmik ölçek (taban 10); alan pozitif olmalıdır */
export function logOlcek(d0, d1, r0, r1) {
  const l0 = Math.log10(d0);
  const l1 = Math.log10(d1);
  const f = (v) => (l1 === l0 ? (r0 + r1) / 2 : r0 + ((Math.log10(v) - l0) / (l1 - l0)) * (r1 - r0));
  f.alan = [d0, d1];
  f.aralik = [r0, r1];
  f.log = true;
  return f;
}

/** Tohumlu sözde rastgele sayı üreteci (mulberry32): titreşim (jitter) tekrarlanabilir olsun diye */
export function tohumluRastgele(tohum = 1) {
  let t = tohum >>> 0;
  return () => {
    t = (t + 0x6d2b79f5) >>> 0;
    let r = Math.imul(t ^ (t >>> 15), 1 | t);
    r = (r + Math.imul(r ^ (r >>> 7), 61 | r)) ^ r;
    return ((r ^ (r >>> 14)) >>> 0) / 4294967296;
  };
}

/* =====================================================================
 * SVG üretimi
 * ===================================================================== */

/** Koordinatı 2 ondalığa yuvarlar; sonlu olmayan değerde hata verir (SVG'ye NaN yazılmasın) */
export function yuv(v) {
  if (typeof v !== 'number' || !Number.isFinite(v)) throw new Error(`Grafik: geçersiz sayısal değer (${v})`);
  const r = Math.round(v * 100) / 100;
  return Object.is(r, -0) ? 0 : r;
}

function nitelikler(o) {
  let str = '';
  for (const [k, v] of Object.entries(o)) {
    if (v === undefined || v === null || v === false || v === '') continue;
    str += ` ${k}="${typeof v === 'number' ? yuv(v) : kacis(v)}"`;
  }
  return str;
}

/** SVG öğesi metni üretir; icerik verilmezse kendiliğinden kapanır */
export function el(ad, o = {}, icerik) {
  return icerik === undefined || icerik === null ? `<${ad}${nitelikler(o)}/>` : `<${ad}${nitelikler(o)}>${icerik}</${ad}>`;
}

/** Nokta dizisinden yol verisi ("M x y L x y ...") */
export function yolVerisi(noktalar, kapali = false) {
  if (!noktalar.length) return '';
  const d = noktalar.map(([x, y], i) => `${i ? 'L' : 'M'}${yuv(x)} ${yuv(y)}`).join('');
  return kapali ? d + 'Z' : d;
}

function fnv1a(str) {
  let h = 0x811c9dc5;
  for (let i = 0; i < str.length; i++) {
    h ^= str.charCodeAt(i);
    h = Math.imul(h, 0x01000193) >>> 0;
  }
  return h >>> 0;
}

const KIMLIK = '__GRAFIKKIMLIK__';

/**
 * SVG parçalarını toplayan çizim yüzeyi. Desen tanımlarının kimlikleri içerikten türetilen bir özetle
 * adlandırılır; böylece aynı sayfaya gömülen birden çok grafik birbirinin desenini ezmez ve çıktı
 * belirlenimci kalır.
 */
export class Tuval {
  constructor(s) {
    this.s = s;
    this.parcalar = [];
    this.tanimlar = new Map();
  }

  ekle(parca) {
    this.parcalar.push(parca);
    return this;
  }

  cizgi(x1, y1, x2, y2, o = {}) {
    return this.ekle(
      el('line', {
        x1,
        y1,
        x2,
        y2,
        stroke: o.renk ?? this.s.T.murekkep,
        'stroke-width': o.kalinlik ?? 1,
        'stroke-dasharray': o.kesik || undefined,
        'stroke-linecap': o.uc,
        'stroke-opacity': o.opaklik,
      }),
    );
  }

  dikdortgen(x, y, w, h, o = {}) {
    const x0 = Math.min(x, x + w);
    const y0 = Math.min(y, y + h);
    return this.ekle(
      el('rect', {
        x: x0,
        y: y0,
        width: Math.abs(w),
        height: Math.abs(h),
        fill: o.dolgu ?? 'none',
        'fill-opacity': o.opaklik,
        stroke: o.cizgi,
        'stroke-width': o.cizgi ? o.kalinlik ?? 1 : undefined,
        'stroke-dasharray': o.kesik || undefined,
        'shape-rendering': o.keskin ? 'crispEdges' : undefined,
      }),
    );
  }

  daire(cx, cy, r, o = {}) {
    return this.ekle(
      el('circle', {
        cx,
        cy,
        r,
        fill: o.dolgu ?? 'none',
        'fill-opacity': o.opaklik,
        stroke: o.cizgi,
        'stroke-width': o.cizgi ? o.kalinlik ?? 1 : undefined,
      }),
    );
  }

  yol(d, o = {}) {
    if (!d) return this;
    return this.ekle(
      el('path', {
        d,
        fill: o.dolgu ?? 'none',
        'fill-opacity': o.opaklik,
        stroke: o.renk,
        'stroke-width': o.renk ? o.kalinlik ?? 1 : undefined,
        'stroke-dasharray': o.kesik || undefined,
        'stroke-linejoin': o.renk ? o.birlesim ?? 'round' : undefined,
        'stroke-linecap': o.renk ? o.uc ?? 'butt' : undefined,
      }),
    );
  }

  /**
   * Metin. icerik: string ya da parça dizisi [string | {t, italik, kalin}].
   * o: {boyut, renk, hiza: 'bas'|'orta'|'son', kalin, italik, dondur (derece)}
   */
  metin(x, y, icerik, o = {}) {
    const s = this.s;
    const ic = Array.isArray(icerik)
      ? icerik
          .map((p) =>
            typeof p === 'string' ? kacis(p) : el('tspan', { 'font-style': p.italik ? 'italic' : undefined, 'font-weight': p.kalin ? 'bold' : undefined }, kacis(p.t)),
          )
          .join('')
      : kacis(icerik);
    if (!ic) return this;
    const hiza = o.hiza === 'orta' ? 'middle' : o.hiza === 'son' ? 'end' : undefined;
    return this.ekle(
      el(
        'text',
        {
          x,
          y,
          'font-family': s.yaziTipi,
          'font-size': o.boyut ?? s.fs.tik,
          fill: o.renk ?? s.T.murekkep,
          'text-anchor': hiza,
          'font-weight': o.kalin ? 'bold' : undefined,
          'font-style': o.italik ? 'italic' : undefined,
          transform: o.dondur ? `rotate(${yuv(o.dondur)} ${yuv(x)} ${yuv(y)})` : undefined,
        },
        ic,
      ),
    );
  }

  /** Çok satırlı metin; y ilk satırın taban çizgisidir */
  satirlar(x, y, satirlar, o = {}) {
    const boyut = o.boyut ?? this.s.fs.tik;
    const aralik = o.aralik ?? boyut * 1.2;
    satirlar.forEach((t, i) => this.metin(x, y + i * aralik, t, o));
    return this;
  }

  /** Dolgu değeri: desen varsa tanımını ekleyip url(#...) döndürür */
  dolgu(stil) {
    if (!stil.desen) return stil.dolgu;
    return this.desen(stil.desen, stil.dolgu, stil.desenRenk ?? '#000000');
  }

  desen(tur, zemin, cizgiRenk) {
    const anahtar = `${tur}|${zemin}|${cizgiRenk}`;
    if (!this.tanimlar.has(anahtar)) {
      const id = `${KIMLIK}-d${this.tanimlar.size}`;
      const k = 7;
      const cizgi = (x1, y1, x2, y2) => el('line', { x1, y1, x2, y2, stroke: cizgiRenk, 'stroke-width': 1.1 });
      let ic = el('rect', { x: 0, y: 0, width: k, height: k, fill: zemin });
      const t45 = cizgi(0, k, k, 0) + cizgi(-1, 1, 1, -1) + cizgi(k - 1, k + 1, k + 1, k - 1);
      const t135 = cizgi(0, 0, k, k) + cizgi(-1, k - 1, 1, k + 1) + cizgi(k - 1, -1, k + 1, 1);
      if (tur === 'capraz45') ic += t45;
      else if (tur === 'capraz135') ic += t135;
      else if (tur === 'kafes') ic += t45 + t135;
      else ic += el('circle', { cx: k / 2, cy: k / 2, r: 1.3, fill: cizgiRenk });
      this.tanimlar.set(anahtar, { id, metin: el('pattern', { id, patternUnits: 'userSpaceOnUse', width: k, height: k }, ic) });
    }
    return `url(#${this.tanimlar.get(anahtar).id})`;
  }

  /**
   * İşaret (veri noktası) çizer. sekil: daire, kare, ucgen, ucgenAsagi, elmas, arti, carpi, yildiz.
   * o: {dolgu, cizgi, kalinlik, opaklik}
   */
  isaret(x, y, sekil, r, o = {}) {
    const cz = o.cizgi;
    const kal = o.kalinlik ?? 1;
    const dol = o.dolgu ?? 'none';
    const stil = { dolgu: dol, renk: cz, kalinlik: kal, opaklik: o.opaklik };
    switch (sekil) {
      case 'kare': {
        const a = r * 0.9;
        return this.dikdortgen(x - a, y - a, 2 * a, 2 * a, { dolgu: dol, cizgi: cz, kalinlik: kal, opaklik: o.opaklik });
      }
      case 'ucgen':
        return this.yol(yolVerisi([[x, y - r * 1.2], [x + r * 1.1, y + r * 0.75], [x - r * 1.1, y + r * 0.75]], true), stil);
      case 'ucgenAsagi':
        return this.yol(yolVerisi([[x, y + r * 1.2], [x + r * 1.1, y - r * 0.75], [x - r * 1.1, y - r * 0.75]], true), stil);
      case 'elmas':
        return this.yol(yolVerisi([[x, y - r * 1.3], [x + r * 1.05, y], [x, y + r * 1.3], [x - r * 1.05, y]], true), stil);
      case 'arti': {
        const renk = cz && cz !== '#ffffff' ? cz : o.dolgu ?? this.s.T.murekkep;
        const a = r * 1.15;
        return this.yol(`M${yuv(x - a)} ${yuv(y)}H${yuv(x + a)}M${yuv(x)} ${yuv(y - a)}V${yuv(y + a)}`, { renk, kalinlik: Math.max(1.3, kal) });
      }
      case 'carpi': {
        const renk = cz && cz !== '#ffffff' ? cz : o.dolgu ?? this.s.T.murekkep;
        const a = r * 0.95;
        return this.yol(`M${yuv(x - a)} ${yuv(y - a)}L${yuv(x + a)} ${yuv(y + a)}M${yuv(x - a)} ${yuv(y + a)}L${yuv(x + a)} ${yuv(y - a)}`, {
          renk,
          kalinlik: Math.max(1.3, kal),
        });
      }
      case 'yildiz': {
        // Altı kollu yıldız işareti (SPSS uç değer simgesi ✱); yazı tipinden bağımsız çizilir.
        const renk = cz && cz !== '#ffffff' ? cz : o.dolgu ?? this.s.T.murekkep;
        const a = r * 1.15;
        const c30 = a * Math.cos(Math.PI / 6);
        const s30 = a * Math.sin(Math.PI / 6);
        const d =
          `M${yuv(x)} ${yuv(y - a)}V${yuv(y + a)}` +
          `M${yuv(x - c30)} ${yuv(y - s30)}L${yuv(x + c30)} ${yuv(y + s30)}` +
          `M${yuv(x - c30)} ${yuv(y + s30)}L${yuv(x + c30)} ${yuv(y - s30)}`;
        return this.yol(d, { renk, kalinlik: Math.max(1.2, kal), uc: 'round' });
      }
      default:
        return this.daire(x, y, r, { dolgu: dol, cizgi: cz, kalinlik: kal, opaklik: o.opaklik });
    }
  }

  /** Seri stiline göre işaret: dolu işaretler seri renginde, boş işaretler beyaz dolgulu */
  seriIsareti(x, y, stil, r = 4) {
    const cizgiSekli = ['arti', 'carpi', 'yildiz'].includes(stil.sekil);
    if (cizgiSekli) return this.isaret(x, y, stil.sekil, r, { cizgi: stil.renk, kalinlik: 1.4 });
    const renkli = this.s.T.ad === 'renkli' || stil.kenar === '#ffffff';
    if (stil.isaretDolu) {
      return this.isaret(x, y, stil.sekil, r, { dolgu: stil.renk, cizgi: renkli ? '#ffffff' : stil.renk, kalinlik: renkli ? 1 : 0.8 });
    }
    return this.isaret(x, y, stil.sekil, r, { dolgu: '#ffffff', cizgi: stil.renk, kalinlik: 1.2 });
  }

  /** Bitmiş SVG metni (kök öğe açık genişlik/yükseklik ve viewBox taşır) */
  svg() {
    const s = this.s;
    const W = s.genislik;
    const H = s.yukseklik;
    const tanim = [...this.tanimlar.values()].map((t) => t.metin).join('');
    let icerik = (tanim ? `<defs>${tanim}</defs>` : '') + this.parcalar.join('');
    if (tanim) {
      const ozet = 'g' + fnv1a(icerik).toString(36);
      icerik = icerik.split(KIMLIK).join(ozet);
    }
    const baslik = s.baslik ? `<title>${kacis(s.baslik)}</title>` : '';
    return (
      `<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}" role="img">` +
      baslik +
      el('rect', { x: 0, y: 0, width: W, height: H, fill: s.T.arkaPlan }) +
      icerik +
      '</svg>'
    );
  }
}

/* =====================================================================
 * Başlık, notlar ve gösterge
 * ===================================================================== */

const ANAHTAR = { dolgu: 12, cizgi: 26, isaret: 12, bant: 14 };

/**
 * Gösterge ölçümü ve çizimi.
 * ogeler: [{ad, tur: 'dolgu'|'cizgi'|'isaret'|'bant', stil}] (stil: seriStili çıktısı ya da benzeri)
 * dikey=true → tek sütun (sağ kenar); false → satırlara kayan yatay dizilim (üst).
 * Döndürür: {genislik, yukseklik, ciz(c, x, y)} — yatayda x orta noktadır, dikeyde sol kenar.
 */
export function gostergeOlc(s, ogeler, maksGenislik, dikey, baslik = '') {
  const fs = s.fs.gosterge;
  const satirH = fs * 1.25;
  const olculer = ogeler.map((o) => {
    const ag = ANAHTAR[o.tur] ?? 12;
    const satirlar = metniSar(o.ad, Math.max(30, maksGenislik - ag - 6), fs);
    const gen = ag + 6 + enGenisSatir(satirlar, fs);
    return { o, ag, satirlar, gen, yuk: Math.max(satirlar.length, 1) * satirH };
  });
  const baslikSatir = baslik ? metniSar(baslik, maksGenislik, fs, { kalin: true }) : [];
  const baslikH = baslikSatir.length * satirH;
  if (dikey) {
    const genislik = Math.max(enGenisSatir(baslikSatir, fs, true), ...olculer.map((m) => m.gen), 0);
    const yukseklik = baslikH + olculer.reduce((t, m) => t + m.yuk + 4, 0);
    return {
      genislik,
      yukseklik,
      ciz(c, x, y) {
        let yy = y;
        baslikSatir.forEach((t) => {
          c.metin(x, yy + fs * 0.9, t, { boyut: fs, kalin: true });
          yy += satirH;
        });
        for (const m of olculer) {
          anahtarCiz(c, m.o, x, yy + satirH / 2);
          c.satirlar(x + m.ag + 6, yy + satirH / 2 + fs * 0.35, m.satirlar, { boyut: fs, renk: s.T.murekkep, aralik: satirH });
          yy += m.yuk + 4;
        }
      },
    };
  }
  const bosluk = 16;
  const sira = [];
  let mevcut = { ogeler: [], gen: 0, yuk: satirH };
  for (const m of olculer) {
    const ek = (mevcut.ogeler.length ? bosluk : 0) + m.gen;
    if (mevcut.ogeler.length && mevcut.gen + ek > maksGenislik) {
      sira.push(mevcut);
      mevcut = { ogeler: [], gen: 0, yuk: satirH };
    }
    mevcut.gen += (mevcut.ogeler.length ? bosluk : 0) + m.gen;
    mevcut.yuk = Math.max(mevcut.yuk, m.yuk);
    mevcut.ogeler.push(m);
  }
  if (mevcut.ogeler.length) sira.push(mevcut);
  const yukseklik = baslikH + sira.reduce((t, r) => t + r.yuk, 0) + Math.max(0, sira.length - 1) * 3;
  return {
    genislik: Math.max(enGenisSatir(baslikSatir, fs, true), ...sira.map((r) => r.gen), 0),
    yukseklik,
    ciz(c, xOrta, y) {
      let yy = y;
      baslikSatir.forEach((t) => {
        c.metin(xOrta, yy + fs * 0.9, t, { boyut: fs, kalin: true, hiza: 'orta' });
        yy += satirH;
      });
      for (const r of sira) {
        let xx = xOrta - r.gen / 2;
        for (const m of r.ogeler) {
          anahtarCiz(c, m.o, xx, yy + satirH / 2);
          c.satirlar(xx + m.ag + 6, yy + satirH / 2 + fs * 0.35, m.satirlar, { boyut: fs, aralik: satirH });
          xx += m.gen + bosluk;
        }
        yy += r.yuk + 3;
      }
    },
  };
}

/** Gösterge anahtarı (renk/desen kutusu, çizgi örneği ya da işaret) */
export function anahtarCiz(c, o, x, y) {
  const s = c.s;
  const st = o.stil || {};
  if (o.tur === 'cizgi') {
    c.cizgi(x, y, x + ANAHTAR.cizgi, y, { renk: st.renk, kalinlik: st.kalinlik ?? s.T.cizgiKalinlik, kesik: st.kesik });
    if (st.sekil && o.isaretli !== false) c.seriIsareti(x + ANAHTAR.cizgi / 2, y, st, 3.5);
  } else if (o.tur === 'isaret') {
    if (o.isaretCiz) o.isaretCiz(c, x + 6, y);
    else c.seriIsareti(x + 6, y, st, 4);
  } else if (o.tur === 'bant') {
    c.dikdortgen(x, y - 6, ANAHTAR.bant, 12, { dolgu: st.renk ?? s.T.bantRenk, opaklik: st.opaklik ?? s.T.bantOpaklik });
  } else {
    c.dikdortgen(x, y - 6, 12, 12, { dolgu: c.dolgu(st), cizgi: s.T.ad === 'yayin' ? '#000000' : undefined, kalinlik: 0.75 });
  }
}

/**
 * Üst bölüm (başlık, üstteki gösterge, sağa yaslı notlar) ölçümü.
 * notlar: string ya da parça dizisi; string notlardaki istatistik simgeleri italikleştirilir.
 * Döndürür: {yukseklik, ciz(c, sag)} — sag: notların hizalanacağı sağ kenar.
 */
export function ustBolumOlc(s, gosterge, notlar = [], gostergeBasligi = '') {
  const W = s.genislik;
  const P = s.kenar;
  const kullanilir = W - 2 * P;
  const fs = s.fs;
  const baslik = s.baslik ? metniSar(s.baslik, kullanilir, fs.baslik, { kalin: true, maksSatir: 3 }) : [];
  const baslikH = baslik.length * fs.baslik * 1.25;
  const gost = gosterge && gosterge.length && s.gostergeKonumu === 'ust' ? gostergeOlc(s, gosterge, kullanilir, false, gostergeBasligi) : null;
  const notSatir = [];
  for (const n of notlar.filter((v) => v && (!Array.isArray(v) || v.length))) {
    if (Array.isArray(n)) notSatir.push(n);
    else for (const t of metniSar(n, kullanilir, fs.kucuk)) notSatir.push(simgeleriItalikYap(t));
  }
  const notH = notSatir.length * fs.kucuk * 1.3;
  let yukseklik = baslikH;
  if (gost) yukseklik += (yukseklik ? 6 : 0) + gost.yukseklik;
  if (notH) yukseklik += (yukseklik ? 4 : 0) + notH;
  return {
    yukseklik,
    ciz(c, sag) {
      let y = P;
      baslik.forEach((t) => {
        c.metin(W / 2, y + fs.baslik, t, { boyut: fs.baslik, kalin: true, hiza: 'orta' });
        y += fs.baslik * 1.25;
      });
      if (gost) {
        if (y > P) y += 6;
        gost.ciz(c, W / 2, y);
        y += gost.yukseklik;
      }
      if (notH) {
        if (y > P) y += 4;
        notSatir.forEach((n) => {
          c.metin(sag, y + fs.kucuk, n, { boyut: fs.kucuk, hiza: 'son', renk: s.T.ikincil });
          y += fs.kucuk * 1.3;
        });
      }
    },
  };
}

/** Veri yokken ya da çizilemezken başlık ve ortalanmış açıklama içeren grafik */
export function bosGrafik(s, mesaj = 'Grafik için yeterli veri yok.') {
  const c = new Tuval(s);
  const ust = ustBolumOlc(s, null, []);
  ust.ciz(c, s.genislik - s.kenar);
  const satirlar = metniSar(mesaj, s.genislik - 4 * s.kenar, s.fs.eksen);
  const y0 = s.yukseklik / 2 - ((satirlar.length - 1) * s.fs.eksen * 1.2) / 2;
  c.satirlar(s.genislik / 2, y0, satirlar, { boyut: s.fs.eksen, hiza: 'orta', renk: s.T.soluk });
  return c.svg();
}

/* =====================================================================
 * Eksenler ve kartezyen yerleşim
 * ===================================================================== */

const TIK = 5; // tik uzunluğu
const TIK_BOSLUK = 4; // tik ile etiket arası

function sinirla(v, a, b) {
  return Math.max(a, Math.min(b, v));
}

/**
 * Sayısal eksen hazırlığı: güzel adımlar, etiketler ve etiket çakışma denetimi.
 * e: {min, max, sifirDahil, tamsayi, log, sabitAlan:[a,b], adimlar:number[] (zorunlu tikler), bicim(v, ondalik)}
 */
export function sayisalEksenHazirla(s, e, yon, uzunluk) {
  const fs = s.fs.tik;
  let min = Number.isFinite(e.min) ? e.min : NaN;
  let max = Number.isFinite(e.max) ? e.max : NaN;
  if (!Number.isFinite(min) || !Number.isFinite(max)) {
    min = Number.isFinite(min) ? min : Number.isFinite(max) ? max : 0;
    max = Number.isFinite(max) ? max : min + 1;
  }
  if (e.sifirDahil) {
    min = Math.min(min, 0);
    max = Math.max(max, 0);
  }
  const log = !!e.log && min > 0;
  const bicim = e.bicim || ((v, d) => sayiBicimle(v, d, s));
  let hedef = yon === 'y' ? sinirla(Math.round(uzunluk / 45), 2, 8) : sinirla(Math.round(uzunluk / 70), 2, 10);
  let sonuc = null;
  for (; hedef >= 1; hedef--) {
    let adimlar;
    let alan;
    if (Array.isArray(e.adimlar) && e.adimlar.length) {
      adimlar = [...e.adimlar].filter(Number.isFinite).sort((p, q) => p - q);
      alan = e.sabitAlan ? [...e.sabitAlan] : [Math.min(min, adimlar[0]), Math.max(max, adimlar[adimlar.length - 1])];
    } else if (log) {
      const g = logAdimlar(min, max, hedef);
      adimlar = g.adimlar;
      alan = [g.alt, g.ust];
    } else if (e.sabitAlan) {
      const [a, b] = e.sabitAlan;
      const g = guzelAdimlar(a, b, hedef, { tamsayi: e.tamsayi });
      const tol = (b - a) * 1e-9;
      adimlar = g.adimlar.filter((v) => v >= a - tol && v <= b + tol);
      alan = [a, b];
    } else {
      const g = guzelAdimlar(min, max, hedef, { tamsayi: e.tamsayi });
      adimlar = g.adimlar;
      alan = [g.alt, g.ust];
    }
    if (alan[0] === alan[1]) alan = [alan[0] - 1, alan[1] + 1];
    const ondalik = log || e.adimlar ? Math.max(...adimlar.map(ondalikSayisi), 0) : ondalikSayisi(adimlar.length > 1 ? adimlar[1] - adimlar[0] : adimlar[0] || 1);
    const etiketler = adimlar.map((v) => (log ? bicim(v, ondalikSayisi(v)) : bicim(v, ondalik)));
    const olcek = log ? logOlcek(alan[0], alan[1], 0, uzunluk) : dogrusalOlcek(alan[0], alan[1], 0, uzunluk);
    const konum = adimlar.map((v) => olcek(v));
    const genislikler = etiketler.map((t) => metinGenisligi(t, fs));
    let goster = adimlar.map(() => true);
    const cakisma = (gst) => {
      let onceki = -1;
      for (let i = 0; i < adimlar.length; i++) {
        if (!gst[i]) continue;
        if (onceki >= 0) {
          const ara = Math.abs(konum[i] - konum[onceki]);
          const gerek = yon === 'x' ? (genislikler[i] + genislikler[onceki]) / 2 + 6 : fs * 1.25;
          if (ara < gerek) return true;
        }
        onceki = i;
      }
      return false;
    };
    let cakisiyor = cakisma(goster);
    if (cakisiyor && (e.adimlar || hedef <= 2)) {
      // Zorunlu tiklerde (ör. risk tablosu zamanları) etiketleri seyrelt
      for (let k = 2; k <= adimlar.length && cakisiyor; k++) {
        goster = adimlar.map((_, i) => i % k === 0);
        cakisiyor = cakisma(goster);
      }
    }
    sonuc = { tur: 'sayisal', alan, adimlar, etiketler, goster, log, genislikler };
    if (!cakisiyor || e.adimlar || hedef <= 2) break;
  }
  const maksW = Math.max(0, ...sonuc.genislikler.filter((_, i) => sonuc.goster[i]));
  sonuc.kalinlik = yon === 'y' ? TIK + TIK_BOSLUK + maksW + 2 : TIK + TIK_BOSLUK + fs + 2;
  const sonIdx = sonuc.adimlar.length - 1;
  sonuc.sagTasma = yon === 'x' && sonuc.goster[sonIdx] ? sonuc.genislikler[sonIdx] / 2 : 0;
  sonuc.solTasma = yon === 'x' && sonuc.goster[0] ? sonuc.genislikler[0] / 2 : 0;
  return sonuc;
}

/** Kategorik x ekseni: etiketler yatay sığar, sözcük sınırlarından sarılır ya da 45°/90° döndürülür */
function kategorikXHazirla(s, adlar, uzunluk) {
  const fs = s.fs.tik;
  const n = Math.max(1, adlar.length);
  const bant = uzunluk / n;
  const maks = Math.max(8, bant - 6);
  const satirH = fs * 1.2;
  const sarilmis = adlar.map((a) => metniSar(a, maks, fs, { bolme: false }));
  const sigdi = sarilmis.every((sat) => sat.length <= 3 && enGenisSatir(sat, fs) <= maks);
  if (sigdi) {
    const satir = Math.max(1, ...sarilmis.map((t) => t.length));
    return { tur: 'kategorik', mod: 'yatay', satirlar: sarilmis, kalinlik: TIK + TIK_BOSLUK + fs + (satir - 1) * satirH + 2, bant, n, sagTasma: 0, solTasma: 0 };
  }
  // Döndürülmüş etiketler: bandın dik yönde izin verdiği kadar satıra (en çok 3) dengeli sarılır;
  // yalnızca yine de sığmayanlar "…" ile kısaltılır (ayırt edici son ekler kaybolmasın diye önce sarma).
  const aci = bant >= fs * 1.5 ? 45 : 90;
  const satirAraligi = fs * 1.1;
  const dikGen = aci === 45 ? bant * Math.SQRT1_2 : bant;
  const maksUzunluk = Math.max(50, s.yukseklik * 0.28);
  const duz = adlar.map((a) => String(a).replace(/\s*\n\s*/g, ' '));
  const hamEnUzun = Math.max(0, ...duz.map((t) => metinGenisligi(t, fs)));
  const satirSayisi = sinirla(Math.ceil(hamEnUzun / (maksUzunluk * 0.85)), 1, sinirla(Math.floor(dikGen / satirAraligi), 1, 3));
  const hedefUzunluk = sinirla((hamEnUzun / satirSayisi) * 1.15, Math.min(60, maksUzunluk), maksUzunluk);
  const metinler = duz.map((t) => metniSar(t, hedefUzunluk, fs, { maksSatir: satirSayisi }));
  const genislikler = metinler.map((sat) => enGenisSatir(sat, fs));
  const enUzun = Math.max(0, ...genislikler);
  const enCokSatir = Math.max(1, ...metinler.map((sat) => sat.length));
  const sicrama = Math.max(1, Math.ceil((satirAraligi * enCokSatir) / dikGen));
  const goster = metinler.map((_, i) => i % sicrama === 0);
  const blok = (enCokSatir - 1) * satirAraligi;
  const dikey = aci === 45 ? (enUzun + fs * 0.5 + blok) * Math.SQRT1_2 + fs * 0.4 : enUzun;
  let solTasma = 0;
  if (aci === 45) genislikler.forEach((w, i) => (solTasma = Math.max(solTasma, w * Math.SQRT1_2 - (i + 0.5) * bant)));
  return { tur: 'kategorik', mod: 'donuk', aci, metinler, satirAraligi, goster, kalinlik: TIK + TIK_BOSLUK + dikey + 2, bant, n, sagTasma: 0, solTasma };
}

/** Kategorik y ekseni (yatay çubuk, orman grafiği): etiketler solda, sağa yaslı ve sarılmış */
function kategorikYHazirla(s, adlar, uzunluk) {
  const fs = s.fs.tik;
  const n = Math.max(1, adlar.length);
  const bant = uzunluk / n;
  const maksW = Math.max(50, s.genislik * 0.32);
  const maksSatir = sinirla(Math.floor((bant - 2) / (fs * 1.15)), 1, 3);
  const satirlar = adlar.map((a) => metniSar(a, maksW, fs, { maksSatir }));
  const w = Math.max(0, ...satirlar.map((t) => enGenisSatir(t, fs)));
  return { tur: 'kategorik', mod: 'sol', satirlar, kalinlik: TIK + TIK_BOSLUK + w + 2, bant, n, sagTasma: 0, solTasma: 0 };
}

function eksenHazirla(s, e, yon, uzunluk) {
  if (e.tur === 'kategorik') return yon === 'x' ? kategorikXHazirla(s, e.adlar, uzunluk) : kategorikYHazirla(s, e.adlar, uzunluk);
  return sayisalEksenHazirla(s, e, yon, uzunluk);
}

/**
 * Kartezyen grafiğin yerleşimini hesaplar ve eksenleri hazırlar.
 * g: {
 *   x, y: eksen tanımı — {tur:'sayisal', min, max, sifirDahil, tamsayi, log, sabitAlan, adimlar, bicim}
 *         ya da {tur:'kategorik', adlar}
 *   gosterge: [{ad, tur, stil}] | null, gostergeBasligi,
 *   notlar: üstte sağa yaslı kısa notlar, altNotlar: en altta sola yaslı dipnotlar,
 *   sagEk: sağda ayrılacak ek genişlik (çizgi etiketleri), altEk: x ekseni başlığının altında ek yükseklik
 *   solMin: sol kenar için en küçük değer (risk tablosu adları), kare: çizim alanı kare olsun
 * }
 * Döndürür: {sol, sag, ust, alt, gen, yuk, x, y, sx(v), sy(v), xMerkez(i), yMerkez(i), altBlokY,
 *            izgaraCiz(c), cerceveCiz(c)}
 */
export function kartezyenPlanla(s, g) {
  const W = s.genislik;
  const H = s.yukseklik;
  const P = s.kenar;
  const fs = s.fs;
  const ust = ustBolumOlc(s, g.gosterge, g.notlar || [], g.gostergeBasligi);
  const sagGosterge = g.gosterge && g.gosterge.length && s.gostergeKonumu === 'sag' ? gostergeOlc(s, g.gosterge, Math.max(70, W * 0.28), true, g.gostergeBasligi) : null;
  const xBaslik = s.xEtiket ? metniSar(s.xEtiket, W - 2 * P, fs.eksen, { maksSatir: 2 }) : [];
  const xBaslikH = xBaslik.length ? xBaslik.length * fs.eksen * 1.2 + 4 : 0;
  const altNotlar = (g.altNotlar || []).filter(Boolean).flatMap((n) => metniSar(n, W - 2 * P, fs.kucuk));
  const altNotH = altNotlar.length ? altNotlar.length * fs.kucuk * 1.3 + 6 : 0;
  const yBaslik = s.yEtiket ? metniSar(s.yEtiket, Math.max(60, H * 0.62), fs.eksen, { maksSatir: 2 }) : [];
  const yBaslikW = yBaslik.length ? yBaslik.length * fs.eksen * 1.2 + 6 : 0;
  const ustY = P + ust.yukseklik + (ust.yukseklik ? 10 : 0) + fs.tik * 0.6;
  const sagSinir = W - P - (sagGosterge ? sagGosterge.genislik + 16 : 0) - (g.sagEk || 0);
  const altEk = g.altEk || 0;
  let sol = P + yBaslikW + 40;
  let sag = sagSinir;
  let alt = H - P - altNotH - xBaslikH - altEk - (fs.tik + TIK + TIK_BOSLUK + 2);
  let xE;
  let yE;
  for (let tur = 0; tur < 3; tur++) {
    const gen = Math.max(30, sag - sol);
    const yuk = Math.max(30, alt - ustY);
    yE = eksenHazirla(s, g.y, 'y', yuk);
    xE = eksenHazirla(s, g.x, 'x', gen);
    sol = Math.max(P + yBaslikW + yE.kalinlik, P + xE.solTasma, g.solMin || 0);
    alt = H - P - altNotH - xBaslikH - altEk - xE.kalinlik;
    sag = Math.min(sagSinir, W - P - xE.sagTasma);
  }
  let gen = Math.max(30, sag - sol);
  let yuk = Math.max(30, alt - ustY);
  if (g.kare) {
    const k = Math.min(gen, yuk);
    sol += (gen - k) / 2;
    gen = k;
    yuk = k;
  }
  sag = sol + gen;
  alt = ustY + yuk;
  // Son uzunluklarla eksenleri yeniden hazırla (kenar boşlukları önceki turdan yeterlidir)
  yE = eksenHazirla(s, g.y, 'y', yuk);
  xE = eksenHazirla(s, g.x, 'x', gen);

  const sx =
    xE.tur === 'sayisal'
      ? (xE.log ? logOlcek : dogrusalOlcek)(xE.alan[0], xE.alan[1], sol, sag)
      : (v) => sol + (v + 0.5) * (gen / xE.n);
  const sy =
    yE.tur === 'sayisal'
      ? (yE.log ? logOlcek : dogrusalOlcek)(yE.alan[0], yE.alan[1], alt, ustY)
      : (v) => ustY + (v + 0.5) * (yuk / yE.n);
  const xBaslikY = alt + xE.kalinlik + fs.eksen;
  const altBlokY = alt + xE.kalinlik + xBaslikH + 4;

  const plan = {
    sol,
    sag,
    ust: ustY,
    alt,
    gen,
    yuk,
    x: xE,
    y: yE,
    sx,
    sy,
    xBant: xE.tur === 'kategorik' ? gen / xE.n : null,
    yBant: yE.tur === 'kategorik' ? yuk / yE.n : null,
    altBlokY,

    izgaraCiz(c) {
      if (!s.izgara) return;
      const T = s.T;
      // Varsayılan: yalnızca değer ekseninin ızgarası; saçılım türü grafikler 'xy' ister
      const yon = g.izgaraYonu || (yE.tur === 'sayisal' ? 'y' : 'x');
      if (yE.tur === 'sayisal' && yon.includes('y')) {
        yE.adimlar.forEach((v) => {
          const y = sy(v);
          if (Math.abs(y - alt) > 0.5) c.cizgi(sol, y, sag, y, { renk: T.izgara, kalinlik: 0.7 });
        });
      }
      if (xE.tur === 'sayisal' && yon.includes('x')) {
        xE.adimlar.forEach((v) => {
          const x = sx(v);
          if (Math.abs(x - sol) > 0.5) c.cizgi(x, ustY, x, alt, { renk: T.izgara, kalinlik: 0.7 });
        });
      }
    },

    cerceveCiz(c) {
      const T = s.T;
      const ek = { renk: T.eksen, kalinlik: T.eksenKalinlik };
      // y ekseni
      c.cizgi(sol, ustY, sol, alt, { ...ek, uc: 'square' });
      if (yE.tur === 'sayisal') {
        yE.adimlar.forEach((v, i) => {
          const y = sy(v);
          c.cizgi(sol - TIK, y, sol, y, ek);
          if (yE.goster[i]) c.metin(sol - TIK - TIK_BOSLUK, y + fs.tik * 0.35, yE.etiketler[i], { boyut: fs.tik, hiza: 'son', renk: T.ikincil });
        });
      } else {
        const bant = yuk / yE.n;
        yE.satirlar.forEach((sat, i) => {
          const y = ustY + (i + 0.5) * bant;
          c.cizgi(sol - TIK, y, sol, y, ek);
          const y0 = y - ((sat.length - 1) * fs.tik * 1.15) / 2 + fs.tik * 0.35;
          c.satirlar(sol - TIK - TIK_BOSLUK, y0, sat, { boyut: fs.tik, hiza: 'son', renk: T.ikincil, aralik: fs.tik * 1.15 });
        });
      }
      // x ekseni
      c.cizgi(sol, alt, sag, alt, { ...ek, uc: 'square' });
      if (xE.tur === 'sayisal') {
        xE.adimlar.forEach((v, i) => {
          const x = sx(v);
          c.cizgi(x, alt, x, alt + TIK, ek);
          if (xE.goster[i]) c.metin(x, alt + TIK + TIK_BOSLUK + fs.tik * 0.8, xE.etiketler[i], { boyut: fs.tik, hiza: 'orta', renk: T.ikincil });
        });
      } else {
        const bant = gen / xE.n;
        for (let i = 0; i < xE.n; i++) {
          const x = sol + (i + 0.5) * bant;
          c.cizgi(x, alt, x, alt + TIK, ek);
          if (xE.mod === 'yatay') {
            c.satirlar(x, alt + TIK + TIK_BOSLUK + fs.tik * 0.8, xE.satirlar[i] || [], { boyut: fs.tik, hiza: 'orta', renk: T.ikincil });
          } else if (xE.goster[i]) {
            // Satırlar metin yönüne dik olarak istiflenir ve blok tikin üzerinde ortalanır
            const y = alt + TIK + TIK_BOSLUK;
            const sat = xE.metinler[i];
            const a = xE.satirAraligi;
            sat.forEach((t, j) => {
              const kay = (j - (sat.length - 1) / 2) * a;
              if (xE.aci === 45) {
                const d = kay * Math.SQRT1_2;
                c.metin(x + fs.tik * 0.3 + d, y + fs.tik * 0.3 + d + Math.max(0, (sat.length - 1) / 2) * a * Math.SQRT1_2, t, { boyut: fs.tik, hiza: 'son', renk: T.ikincil, dondur: -45 });
              } else {
                c.metin(x + fs.tik * 0.35 + kay, y, t, { boyut: fs.tik, hiza: 'son', renk: T.ikincil, dondur: -90 });
              }
            });
          }
        }
      }
      // eksen başlıkları
      if (xBaslik.length) c.satirlar((sol + sag) / 2, xBaslikY, xBaslik, { boyut: fs.eksen, hiza: 'orta' });
      if (yBaslik.length) {
        const yOrta = (ustY + alt) / 2;
        // Başlık tik etiketlerinin hemen soluna yerleşir (sol kenar risk tablosu gibi nedenlerle genişlese de)
        const sonX = sol - yE.kalinlik - 4 - fs.eksen * 0.25;
        yBaslik.forEach((t, i) => {
          const x = sonX - (yBaslik.length - 1 - i) * fs.eksen * 1.2;
          c.metin(x, yOrta, t, { boyut: fs.eksen, hiza: 'orta', dondur: -90 });
        });
      }
      // üst bölüm, sağ gösterge, dipnotlar
      ust.ciz(c, sag);
      if (sagGosterge) sagGosterge.ciz(c, W - P - sagGosterge.genislik, ustY);
      if (altNotlar.length) {
        let y = H - P - altNotH + 6 + fs.kucuk;
        altNotlar.forEach((t) => {
          c.metin(P, y, simgeleriItalikYap(t), { boyut: fs.kucuk, renk: T.ikincil });
          y += fs.kucuk * 1.3;
        });
      }
    },
  };
  return plan;
}

/** Hata çubuğu: dikey çizgi ve uç başlıkları (yatay=true ise yatay) */
export function hataCubugu(c, merkez, alt, ust, baslikGen, o = {}, yatay = false) {
  const renk = o.renk ?? c.s.T.murekkep;
  const kal = o.kalinlik ?? 1;
  const b = baslikGen / 2;
  if (yatay) {
    c.cizgi(alt, merkez, ust, merkez, { renk, kalinlik: kal });
    if (b > 0) {
      c.cizgi(alt, merkez - b, alt, merkez + b, { renk, kalinlik: kal });
      c.cizgi(ust, merkez - b, ust, merkez + b, { renk, kalinlik: kal });
    }
    return;
  }
  c.cizgi(merkez, alt, merkez, ust, { renk, kalinlik: kal });
  if (b > 0) {
    c.cizgi(merkez - b, alt, merkez + b, alt, { renk, kalinlik: kal });
    c.cizgi(merkez - b, ust, merkez + b, ust, { renk, kalinlik: kal });
  }
}
