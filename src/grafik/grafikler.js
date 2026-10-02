// Yayın kalitesinde istatistik grafikleri.
//
// Her grafik saf bir fonksiyondur ve tek başına geçerli bir SVG metni döndürür:
//   <svg xmlns="http://www.w3.org/2000/svg" width=… height=… viewBox=…> … </svg>
// Tüm biçimler satır içidir (CSS sınıfı yok), bu yüzden SVG uygulamada, .svg/.png olarak ve Word'de aynı görünür.
//
// Ortak seçenekler (her grafikte): {genislik=640, yukseklik=420, baslik='', xEtiket='', yEtiket='',
//   ondalikAyrac=',', binlikAyrac, yaziTipi='Arial, Helvetica, sans-serif', yaziBoyutu=12,
//   tema='yayin'|'renkli', renkler?: string[], gostergeKonumu='ust'|'sag'|'yok', izgara?: boolean, basSifir=true}
// Ayrıntı: ortak.js → VARSAYILAN_SECENEKLER.

import { ortalama, standartSapma, sirali, yuzdelik, temizSayilar } from '../cekirdek/betimsel.js';
import { qnorm, qt, dnorm } from '../cekirdek/dagilim.js';
import {
  Tuval,
  basSifirUygula,
  bosGrafik,
  dogrusalOlcek,
  enGenisSatir,
  guzelAdimlar,
  hataCubugu,
  iraksakRenk,
  kartezyenPlanla,
  kisalt,
  logOlcek,
  metinGenisligi,
  metinRengi,
  metniSar,
  ozetBicimle,
  pBicimle,
  sayiBicimle,
  sayisalEksenHazirla,
  secenekHazirla,
  seriStili,
  simgeleriItalikYap,
  tekSeriStili,
  tohumluRastgele,
  ustBolumOlc,
  yildizlar,
  yolVerisi,
  yuzdeBicimle,
} from './ortak.js';

/* =====================================================================
 * Girdi yardımcıları
 * ===================================================================== */

function sayiDizisi(x) {
  if (!Array.isArray(x) && !ArrayBuffer.isView(x)) return [];
  return temizSayilar(Array.from(x));
}

/** number[] → [number[]]; number[][] → her grup temizlenir */
function grupla(gruplar) {
  if (!Array.isArray(gruplar) && !ArrayBuffer.isView(gruplar)) return [];
  const dizi = Array.from(gruplar);
  if (dizi.length && dizi.every((g) => !Array.isArray(g) && !ArrayBuffer.isView(g))) return [sayiDizisi(dizi)];
  return dizi.map((g) => sayiDizisi(g ?? []));
}

function adHazirla(adlar, n, onEk = 'Grup', tekseBos = true) {
  return Array.from({ length: n }, (_, i) => {
    const a = Array.isArray(adlar) ? adlar[i] : undefined;
    if (a !== undefined && a !== null && String(a) !== '') return String(a);
    return n === 1 && tekseBos ? '' : `${onEk} ${i + 1}`;
  });
}

function sinirlar(degerler) {
  let a = Infinity;
  let b = -Infinity;
  for (const v of degerler) {
    if (typeof v !== 'number' || !Number.isFinite(v)) continue;
    if (v < a) a = v;
    if (v > b) b = v;
  }
  return Number.isFinite(a) ? [a, b] : [NaN, NaN];
}

const sinirla = (v, a, b) => Math.max(a, Math.min(b, v));

/** Hata payı adı (dipnot için) */
function hataAciklamasi(hata, duzey, s) {
  if (hata === 'ss') return 'Hata çubukları ±1 standart sapmayı gösterir.';
  if (hata === 'sh') return 'Hata çubukları ±1 standart hatayı gösterir.';
  return `Hata çubukları ortalamanın ${yuzdeBicimle(duzey * 100, duzey * 100 === Math.round(duzey * 100) ? 0 : 1, s)} güven aralığını gösterir.`;
}

/** Ham gruptan ortalama ve hata aralığı (n < 2 iken aralık yoktur) */
function ortalamaVeHata(v, hata, duzey) {
  const n = v.length;
  const m = n ? ortalama(v) : NaN;
  if (n < 2) return { n, ortalama: m, alt: NaN, ust: NaN };
  const ss = standartSapma(v, m);
  let e;
  if (hata === 'ss') e = ss;
  else if (hata === 'sh') e = ss / Math.sqrt(n);
  else e = qt(1 - (1 - duzey) / 2, n - 1) * (ss / Math.sqrt(n));
  return { n, ortalama: m, alt: m - e, ust: m + e, ss };
}

/* =====================================================================
 * 1. Histogram
 * ===================================================================== */

/**
 * Histogram kutuları. Kutular soldan kapalıdır [a, b) (SPSS gibi); son kutu iki yandan kapalıdır.
 * kutuSayisi: 'otomatik' (Sturges ile Freedman–Diaconis'ten çok kutu vereni; numpy 'auto'), 'sturges',
 *   'fd' ya da sayı (min–maks arası tam o kadar eşit kutu). Otomatik seçimde sınırlar R'nin pretty()
 *   yaklaşımıyla "güzel" sayılara yuvarlanır. kutuGenisligi verilirse kutular onun katlarındadır.
 * Döndürür: {kenarlar: number[], sayilar: number[]}
 */
export function histogramKutulari(x, kutuSayisi = 'otomatik', kutuGenisligi) {
  const v = sayiDizisi(x);
  const n = v.length;
  if (!n) return { kenarlar: [], sayilar: [] };
  const s = sirali(v);
  const min = s[0];
  const max = s[n - 1];
  let kenarlar;
  if (max - min <= 1e-12 * Math.max(1, Math.abs(max))) {
    // Sabit veri: değeri ortalayan tek kutu
    const h = guzelAdimlar(0, min === 0 ? 1 : Math.abs(min) * 0.2, 2).adim;
    kenarlar = [min - h / 2, min + h / 2];
  } else if (Number.isFinite(kutuGenisligi) && kutuGenisligi > 0 && (max - min) / kutuGenisligi <= 1000) {
    const w = kutuGenisligi;
    const ilk = Math.floor(min / w + 1e-9);
    kenarlar = [];
    for (let i = ilk; kenarlar.length < 2 || kenarlar[kenarlar.length - 1] <= max; i++) kenarlar.push(Number((i * w).toPrecision(12)));
  } else if (typeof kutuSayisi === 'number' && Number.isFinite(kutuSayisi) && kutuSayisi >= 1) {
    const k = Math.min(1000, Math.round(kutuSayisi));
    kenarlar = Array.from({ length: k + 1 }, (_, i) => (i === k ? max : min + ((max - min) * i) / k));
  } else {
    const sturges = Math.ceil(Math.log2(n) + 1);
    const iqr = yuzdelik(s, 0.75, 7, true) - yuzdelik(s, 0.25, 7, true);
    const fd = iqr > 0 ? Math.ceil((max - min) / (2 * iqr * n ** (-1 / 3))) : 0;
    let k = kutuSayisi === 'sturges' ? sturges : kutuSayisi === 'fd' ? fd || sturges : Math.max(sturges, fd);
    k = sinirla(k, 1, 60);
    kenarlar = guzelAdimlar(min, max, k).adimlar;
  }
  const k = kenarlar.length - 1;
  const sayilar = new Array(k).fill(0);
  for (const deger of v) {
    let a = 0;
    let b = k - 1;
    while (a < b) {
      const o = (a + b + 1) >> 1;
      if (kenarlar[o] <= deger) a = o;
      else b = o - 1;
    }
    sayilar[a]++;
  }
  return { kenarlar, sayilar };
}

/**
 * Histogram, isteğe bağlı olarak frekansa ölçeklenmiş normal yoğunluk eğrisiyle (n · h · φ(x; x̄, s)).
 * @param {number[]} x Sayısal veri (eksik değerler atılır)
 * @param {object} [secenek] Ortak seçenekler ve {kutuSayisi='otomatik'|'sturges'|'fd'|number, kutuGenisligi?,
 *   normalEgri=true, istatistikNotu=true (üstte "Ort. = …; SS = …; n = …")}
 * @returns {string} SVG
 */
export function histogram(x, secenek = {}) {
  const s = secenekHazirla(secenek, { yEtiket: 'Frekans', kutuSayisi: 'otomatik', normalEgri: true, istatistikNotu: true });
  const v = sayiDizisi(x);
  if (!v.length) return bosGrafik(s);
  const { kenarlar, sayilar } = histogramKutulari(v, s.kutuSayisi, s.kutuGenisligi);
  const n = v.length;
  const m = ortalama(v);
  const ss = n > 1 ? standartSapma(v, m) : NaN;
  const k = sayilar.length;
  const h = (kenarlar[k] - kenarlar[0]) / k;
  const egri = !!s.normalEgri && ss > 0;
  let xmin = kenarlar[0];
  let xmax = kenarlar[k];
  if (egri) {
    // Eğri kuyrukları için eksen en çok bir kutu genişliği uzatılır (çarpık veride eksen boşa genişlemesin)
    xmin = Math.min(xmin, Math.max(m - 3 * ss, kenarlar[0] - h));
    xmax = Math.max(xmax, Math.min(m + 3 * ss, kenarlar[k] + h));
  }
  if (k === 1) {
    // Tek kutu (sabit veri): çubuk tüm alanı kaplamasın
    xmin = kenarlar[0] - 2 * h;
    xmax = kenarlar[1] + 2 * h;
  }
  const tepe = egri ? n * h * dnorm(m, m, ss) : 0;
  const ymax = Math.max(...sayilar, tepe);
  const notlar = s.istatistikNotu ? [`Ort. = ${ozetBicimle(m, s)}; SS = ${ozetBicimle(ss, s)}; n = ${sayiBicimle(n, 0, s)}`] : [];
  const plan = kartezyenPlanla(s, {
    x: { tur: 'sayisal', min: xmin, max: xmax },
    y: { tur: 'sayisal', min: 0, max: ymax, sifirDahil: true, tamsayi: true },
    notlar,
  });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  const st = tekSeriStili(s);
  for (let i = 0; i < k; i++) {
    if (!sayilar[i]) continue;
    const x0 = plan.sx(kenarlar[i]);
    const x1 = plan.sx(kenarlar[i + 1]);
    c.dikdortgen(x0, plan.sy(sayilar[i]), x1 - x0, plan.sy(0) - plan.sy(sayilar[i]), { dolgu: st.dolgu, cizgi: st.kenar, kalinlik: st.kenarKalinlik });
  }
  if (egri) {
    const [a, b] = plan.x.alan;
    const nokta = [];
    for (let i = 0; i <= 160; i++) {
      const t = a + ((b - a) * i) / 160;
      nokta.push([plan.sx(t), plan.sy(n * h * dnorm(t, m, ss))]);
    }
    c.yol(yolVerisi(nokta), { renk: s.T.murekkep, kalinlik: 1.5 });
  }
  plan.cerceveCiz(c);
  return c.svg();
}

/* =====================================================================
 * 2. Normal Q-Q grafiği
 * ===================================================================== */

const QQ_KONUM = { blom: 3 / 8, rankit: 0.5, tukey: 1 / 3, vanDerWaerden: 0 };

/**
 * Normal Q-Q konumları: p_i = (i − a) / (n + 1 − 2a), teorik nicelik z_i = Φ⁻¹(p_i).
 * konum: 'blom' (a = 3/8; SPSS varsayılanı), 'r' (R ppoints: n ≤ 10 iken 3/8, değilse 1/2),
 *   'rankit' (1/2), 'tukey' (1/3), 'vanDerWaerden' (0).
 * Döndürür: {teorik, gozlenen (sıralı), p}
 */
export function qqNoktalari(x, konum = 'blom') {
  const g = Array.from(sirali(sayiDizisi(x)));
  const n = g.length;
  const a = konum === 'r' ? (n <= 10 ? 3 / 8 : 0.5) : QQ_KONUM[konum] ?? 3 / 8;
  const p = g.map((_, i) => (i + 1 - a) / (n + 1 - 2 * a));
  return { teorik: p.map((q) => qnorm(q)), gozlenen: g, p };
}

/**
 * Çeyreklerden geçen referans doğrusu (R qqline: tip 7 çeyrekler).
 * Döndürür: {egim, kesim}
 */
export function qqDogrusu(gozlenen) {
  const g = sirali(sayiDizisi(gozlenen));
  if (!g.length) return { egim: NaN, kesim: NaN };
  const y1 = yuzdelik(g, 0.25, 7, true);
  const y2 = yuzdelik(g, 0.75, 7, true);
  const x1 = qnorm(0.25);
  const x2 = qnorm(0.75);
  const egim = (y2 - y1) / (x2 - x1);
  return { egim, kesim: y1 - egim * x1 };
}

/**
 * Normal Q-Q grafiği: gözlenen sıralı değerler teorik normal niceliklere karşı, çeyreklerden geçen
 * referans doğrusu ve isteğe bağlı noktasal %95 güven bandı (car::qqPlot yaklaşımı:
 * SH(z_i) = (eğim / φ(z_i)) · √(p_i(1 − p_i)/n)).
 * @param {number[]} x
 * @param {object} [secenek] Ortak seçenekler ve {konum='blom'|'r'|'rankit'|'tukey'|'vanDerWaerden',
 *   gaBandi=false, duzey=0.95, referansCizgisi=true}
 */
export function qqGrafigi(x, secenek = {}) {
  const s = secenekHazirla(secenek, {
    xEtiket: 'Teorik normal nicelikler',
    yEtiket: 'Gözlenen değerler',
    konum: 'blom',
    gaBandi: false,
    duzey: 0.95,
    referansCizgisi: true,
  });
  const { teorik, gozlenen, p } = qqNoktalari(x, s.konum);
  const n = gozlenen.length;
  if (!n) return bosGrafik(s);
  const { egim, kesim } = qqDogrusu(gozlenen);
  const cizgiVar = s.referansCizgisi && Number.isFinite(egim);
  const bant = [];
  if (s.gaBandi && cizgiVar && n >= 3) {
    const z = qnorm(1 - (1 - s.duzey) / 2);
    teorik.forEach((t, i) => {
      const sh = (Math.abs(egim) / dnorm(t)) * Math.sqrt((p[i] * (1 - p[i])) / n);
      const yh = kesim + egim * t;
      bant.push([t, yh - z * sh, yh + z * sh]);
    });
  }
  const [tmin, tmax] = sinirlar(teorik);
  const yDeg = [...gozlenen];
  if (cizgiVar) yDeg.push(kesim + egim * tmin, kesim + egim * tmax);
  bant.forEach((b) => yDeg.push(b[1], b[2]));
  const [ymin, ymax] = sinirlar(yDeg);
  const plan = kartezyenPlanla(s, { x: { tur: 'sayisal', min: tmin, max: tmax }, y: { tur: 'sayisal', min: ymin, max: ymax }, izgaraYonu: 'xy' });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  if (bant.length) {
    const ust = bant.map((b) => [plan.sx(b[0]), plan.sy(b[2])]);
    const alt = bant.map((b) => [plan.sx(b[0]), plan.sy(b[1])]).reverse();
    c.yol(yolVerisi([...ust, ...alt], true), { dolgu: '#000000', opaklik: s.T.ad === 'yayin' ? 0.12 : 0.1 });
  }
  if (cizgiVar) {
    const renk = s.T.ad === 'renkli' ? seriStili(s, 1).renk : s.T.murekkep;
    c.cizgi(plan.sx(tmin), plan.sy(kesim + egim * tmin), plan.sx(tmax), plan.sy(kesim + egim * tmax), { renk, kalinlik: 1.3 });
  }
  noktalariCiz(c, s, teorik.map((t, i) => [plan.sx(t), plan.sy(gozlenen[i])]), n);
  plan.cerceveCiz(c);
  return c.svg();
}

/** Tek serili nokta bulutu: yayın temasında boş daire, renkli temada beyaz halkalı dolu daire */
function noktalariCiz(c, s, noktalar, n, r) {
  const yaricap = r ?? (n > 1000 ? 2.2 : n > 300 ? 2.8 : 3.3);
  const ozel = Array.isArray(s.renkler) && s.renkler.length ? s.renkler[0] : null;
  for (const [x, y] of noktalar) {
    if (s.T.ad === 'yayin' && !ozel) c.daire(x, y, yaricap, { dolgu: '#ffffff', cizgi: '#000000', kalinlik: 0.9 });
    else c.daire(x, y, yaricap, { dolgu: ozel || s.T.noktaDolgu, opaklik: s.T.noktaOpaklik, cizgi: n <= 300 ? '#ffffff' : undefined, kalinlik: 0.8 });
  }
}

/* =====================================================================
 * 3. Kutu grafiği
 * ===================================================================== */

/**
 * Kutu grafiği özetleri. Kutu Q1–Q3, ortanca çizgisi; bıyıklar Q1 − 1,5·ÇAG ile Q3 + 1,5·ÇAG içindeki en uç
 * gözlemlere uzanır; 1,5–3 ÇAG arası aykırı (○), 3 ÇAG ötesi uç (✱) değerdir (SPSS EXAMINE kuralları).
 * tip: yüzdelik yöntemi — 6 (SPSS ağırlıklı ortalama, varsayılan), 7 (R/Excel) ya da 'tukey'
 * (Tukey menteşeleri; SPSS kutu grafiğinin çizimde kullandığı yöntem).
 * Döndürür: {n, q1, medyan, q3, ceyrekAcikligi, altBiyik, ustBiyik, aykirilar, ucDegerler, ortalama, min, max} | null
 */
export function kutuIstatistikleri(x, tip = 6) {
  const v = Array.from(sirali(sayiDizisi(x)));
  const n = v.length;
  if (!n) return null;
  let q1;
  let q3;
  const medyan = n % 2 ? v[(n - 1) / 2] : (v[n / 2 - 1] + v[n / 2]) / 2;
  const ortanca = (d) => (d.length % 2 ? d[(d.length - 1) / 2] : (d[d.length / 2 - 1] + d[d.length / 2]) / 2);
  if (tip === 'tukey') {
    q1 = ortanca(v.slice(0, Math.ceil(n / 2)));
    q3 = ortanca(v.slice(Math.floor(n / 2)));
  } else {
    q1 = yuzdelik(v, 0.25, tip, true);
    q3 = yuzdelik(v, 0.75, tip, true);
  }
  const iqr = q3 - q1;
  const tol = 1e-12 * Math.max(1, Math.abs(q1), Math.abs(q3));
  const icAlt = q1 - 1.5 * iqr - tol;
  const icUst = q3 + 1.5 * iqr + tol;
  const disAlt = q1 - 3 * iqr - tol;
  const disUst = q3 + 3 * iqr + tol;
  const ic = v.filter((d) => d >= icAlt && d <= icUst);
  const altBiyik = Math.min(ic.length ? ic[0] : q1, q1);
  const ustBiyik = Math.max(ic.length ? ic[ic.length - 1] : q3, q3);
  const aykirilar = v.filter((d) => (d < icAlt && d >= disAlt) || (d > icUst && d <= disUst));
  const ucDegerler = v.filter((d) => d < disAlt || d > disUst);
  return { n, q1, medyan, q3, ceyrekAcikligi: iqr, altBiyik, ustBiyik, aykirilar, ucDegerler, ortalama: ortalama(v), min: v[0], max: v[n - 1] };
}

/**
 * SPSS tarzı kutu grafiği.
 * @param {number[][]|number[]} gruplar Grupların ham değerleri (tek dizi tek grup sayılır)
 * @param {string[]} [adlar] Grup adları
 * @param {object} [secenek] Ortak seçenekler ve {aykiriGoster=true, noktalar=false (titreşimli ham noktalar),
 *   ortalamaGoster=false (+ işareti), yuzdelikTipi=6|7|'tukey', nGoster=false (adın altına n), aciklama=true
 *   (aykırı/uç simgeleri için gösterge)}
 */
export function kutuGrafigi(gruplar, adlar, secenek = {}) {
  const s = secenekHazirla(secenek, { aykiriGoster: true, noktalar: false, ortalamaGoster: false, yuzdelikTipi: 6, nGoster: false, aciklama: true });
  const G = grupla(gruplar);
  if (!G.length || G.every((g) => !g.length)) return bosGrafik(s);
  const ad = adHazirla(adlar, G.length);
  const ist = G.map((g) => kutuIstatistikleri(g, s.yuzdelikTipi));
  const etiketler = ad.map((a, i) => (s.nGoster ? `${a}${a ? '\n' : ''}(n = ${sayiBicimle(ist[i]?.n ?? 0, 0, s)})` : a));
  const yDeg = [];
  ist.forEach((t) => {
    if (!t) return;
    yDeg.push(t.altBiyik, t.ustBiyik, t.q1, t.q3);
    if (s.aykiriGoster || s.noktalar) yDeg.push(t.min, t.max);
    if (s.ortalamaGoster) yDeg.push(t.ortalama);
  });
  const [ymin, ymax] = sinirlar(yDeg);
  const T = s.T;
  const murekkep = T.ad === 'renkli' ? T.kutuCizgi : T.murekkep;
  const gosterge = [];
  if (s.aciklama) {
    const aykiriVar = s.aykiriGoster && ist.some((t) => t && t.aykirilar.length);
    const ucVar = s.aykiriGoster && ist.some((t) => t && t.ucDegerler.length);
    if (aykiriVar) gosterge.push({ ad: `Aykırı değer (> ${sayiBicimle(1.5, 1, s)} ÇAG)`, tur: 'isaret', isaretCiz: (c, x, y) => c.isaret(x, y, 'daire', 3.2, { dolgu: '#ffffff', cizgi: murekkep, kalinlik: 1 }) });
    if (ucVar) gosterge.push({ ad: 'Uç değer (> 3 ÇAG)', tur: 'isaret', isaretCiz: (c, x, y) => c.isaret(x, y, 'yildiz', 3.6, { cizgi: murekkep, kalinlik: 1.1 }) });
    if (s.ortalamaGoster) gosterge.push({ ad: 'Ortalama', tur: 'isaret', isaretCiz: (c, x, y) => c.isaret(x, y, 'arti', 4, { cizgi: murekkep, kalinlik: 1.4 }) });
  }
  const plan = kartezyenPlanla(s, { x: { tur: 'kategorik', adlar: etiketler }, y: { tur: 'sayisal', min: ymin, max: ymax }, gosterge });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  const bant = plan.xBant;
  const kutuGen = Math.max(4, Math.min(bant * 0.55, 72));
  ist.forEach((t, i) => {
    if (!t) return;
    const cx = plan.sx(i);
    const y = plan.sy;
    const cizgi = { renk: murekkep, kalinlik: 1 };
    // bıyıklar ve uç başlıkları
    c.cizgi(cx, y(t.altBiyik), cx, y(t.q1), cizgi);
    c.cizgi(cx, y(t.q3), cx, y(t.ustBiyik), cizgi);
    c.cizgi(cx - kutuGen / 4, y(t.altBiyik), cx + kutuGen / 4, y(t.altBiyik), cizgi);
    c.cizgi(cx - kutuGen / 4, y(t.ustBiyik), cx + kutuGen / 4, y(t.ustBiyik), cizgi);
    // kutu ve ortanca
    c.dikdortgen(cx - kutuGen / 2, y(t.q3), kutuGen, y(t.q1) - y(t.q3), { dolgu: Array.isArray(s.renkler) && s.renkler.length ? s.renkler[i % s.renkler.length] : T.kutuDolgu, cizgi: murekkep, kalinlik: 1 });
    c.cizgi(cx - kutuGen / 2, y(t.medyan), cx + kutuGen / 2, y(t.medyan), { renk: murekkep, kalinlik: 2.2 });
    const aykiriKume = new Set([...t.aykirilar, ...t.ucDegerler]);
    if (s.noktalar) {
      const rng = tohumluRastgele(7919 * (i + 1));
      for (const d of G[i]) {
        if (s.aykiriGoster && aykiriKume.has(d)) continue;
        c.daire(cx + (rng() - 0.5) * kutuGen * 0.7, y(d), 2.1, { dolgu: T.ad === 'renkli' ? '#1c5cab' : '#000000', opaklik: 0.45 });
      }
    }
    if (s.aykiriGoster) {
      const kullanilan = new Map();
      const kaydir = (py) => {
        const anahtar = Math.round(py / 3);
        const k = kullanilan.get(anahtar) || 0;
        kullanilan.set(anahtar, k + 1);
        return k === 0 ? 0 : (k % 2 ? 1 : -1) * Math.ceil(k / 2) * 7;
      };
      for (const d of t.aykirilar) {
        const py = y(d);
        c.isaret(cx + kaydir(py), py, 'daire', 3.2, { dolgu: '#ffffff', cizgi: murekkep, kalinlik: 1 });
      }
      for (const d of t.ucDegerler) {
        const py = y(d);
        c.isaret(cx + kaydir(py), py, 'yildiz', 3.6, { cizgi: murekkep, kalinlik: 1.1 });
      }
    }
    if (s.ortalamaGoster) c.isaret(cx, y(t.ortalama), 'arti', 4, { cizgi: murekkep, kalinlik: 1.4 });
  });
  plan.cerceveCiz(c);
  return c.svg();
}

/* =====================================================================
 * 4. Ortalama ve hata çubuğu grafiği
 * ===================================================================== */

/**
 * Ortalamalar ve hata çubukları (çubuk ya da nokta).
 * @param {number[][]|Array<{ortalama:number, alt?:number, ust?:number, hata?:number}>} veri
 *   Ham gruplar (ortalama ve hata içeride hesaplanır) ya da önceden hesaplanmış özetler
 *   ({ortalama, alt, ust} ya da simetrik {ortalama, hata}).
 * @param {string[]} [adlar]
 * @param {object} [secenek] Ortak seçenekler ve {hata='ga95'|'ss'|'sh', duzey=0.95, tip='cubuk'|'nokta',
 *   hataNotu=true (ham veride otomatik dipnot) ya da dipnot metni}
 */
export function ortalamaHataGrafigi(veri, adlar, secenek = {}) {
  const s = secenekHazirla(secenek, { hata: 'ga95', duzey: 0.95, tip: 'cubuk', hataNotu: true, yEtiket: 'Ortalama' });
  const dizi = Array.isArray(veri) ? veri : [];
  const ozetMi = dizi.length > 0 && dizi.every((d) => d && typeof d === 'object' && !Array.isArray(d) && !ArrayBuffer.isView(d));
  let ozetler;
  if (ozetMi) {
    ozetler = dizi.map((d) => {
      const m = Number(d.ortalama);
      let alt = Number(d.alt);
      let ust = Number(d.ust);
      if ((!Number.isFinite(alt) || !Number.isFinite(ust)) && Number.isFinite(Number(d.hata))) {
        alt = m - Number(d.hata);
        ust = m + Number(d.hata);
      }
      return { ortalama: m, alt, ust };
    });
  } else {
    ozetler = grupla(dizi).map((g) => ortalamaVeHata(g, s.hata, s.duzey));
  }
  if (!ozetler.length || ozetler.every((o) => !Number.isFinite(o.ortalama))) return bosGrafik(s);
  const ad = adHazirla(adlar, ozetler.length);
  const cubuk = s.tip !== 'nokta';
  const yDeg = ozetler.flatMap((o) => [o.ortalama, o.alt, o.ust]);
  const [ymin, ymax] = sinirlar(yDeg);
  const altNot = typeof s.hataNotu === 'string' ? s.hataNotu : s.hataNotu && !ozetMi ? hataAciklamasi(s.hata, s.duzey, s) : '';
  const plan = kartezyenPlanla(s, {
    x: { tur: 'kategorik', adlar: ad },
    y: { tur: 'sayisal', min: ymin, max: ymax, sifirDahil: cubuk },
    altNotlar: altNot ? [altNot] : [],
  });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  const st = tekSeriStili(s);
  const bant = plan.xBant;
  const cubukGen = Math.max(3, Math.min(bant * 0.6, 56));
  const y0 = plan.sy(sinirla(0, plan.y.alan[0], plan.y.alan[1]));
  ozetler.forEach((o, i) => {
    if (!Number.isFinite(o.ortalama)) return;
    const cx = plan.sx(i);
    const ym = plan.sy(o.ortalama);
    if (cubuk) c.dikdortgen(cx - cubukGen / 2, ym, cubukGen, y0 - ym, { dolgu: st.dolgu, cizgi: st.kenar, kalinlik: st.kenarKalinlik });
    if (Number.isFinite(o.alt) && Number.isFinite(o.ust)) {
      hataCubugu(c, cx, plan.sy(o.alt), plan.sy(o.ust), cubuk ? Math.min(cubukGen * 0.4, 16) : 12, { renk: s.T.murekkep, kalinlik: 1.1 });
    }
    if (!cubuk) c.isaret(cx, ym, 'daire', 4.2, { dolgu: s.T.ad === 'renkli' ? st.dolgu : '#000000', cizgi: s.T.ad === 'renkli' ? '#ffffff' : '#000000', kalinlik: 1 });
  });
  if (plan.y.alan[0] < 0 && plan.y.alan[1] > 0) c.cizgi(plan.sol, plan.sy(0), plan.sag, plan.sy(0), { renk: s.T.eksen, kalinlik: s.T.eksenKalinlik });
  plan.cerceveCiz(c);
  return c.svg();
}

/* =====================================================================
 * 5. Profil (tekrarlı ölçüm) grafiği
 * ===================================================================== */

/**
 * Tekrarlı ölçüm profil grafiği: her seri bir grup; x ekseninde ölçüm zamanları/koşullar, kaydırılmış
 * (dodge) hata çubuklarıyla ortalamalar.
 * @param {Array<{ad:string, noktalar:Array<{x:string|number, ortalama:number, alt?:number, ust?:number}>}>} seriler
 * @param {object} [secenek] Ortak seçenekler ve {hataNotu?: string (dipnot), gostergeBasligi?: string}
 */
export function profilGrafigi(seriler, secenek = {}) {
  const s = secenekHazirla(secenek, { yEtiket: 'Ortalama', hataNotu: '', gostergeBasligi: '' });
  const S = (Array.isArray(seriler) ? seriler : []).filter((r) => r && Array.isArray(r.noktalar));
  const kategoriler = [];
  const kimlik = new Map();
  S.forEach((r) =>
    r.noktalar.forEach((p) => {
      if (!p) return;
      const k = String(p.x ?? '');
      if (!kimlik.has(k)) {
        kimlik.set(k, kategoriler.length);
        kategoriler.push(k);
      }
    }),
  );
  const yDeg = S.flatMap((r) => r.noktalar.flatMap((p) => (p ? [Number(p.ortalama), Number(p.alt), Number(p.ust)] : [])));
  const [ymin, ymax] = sinirlar(yDeg);
  if (!kategoriler.length || !Number.isFinite(ymin)) return bosGrafik(s);
  const adlar = S.map((r, j) => (r.ad != null && String(r.ad) !== '' ? String(r.ad) : `Seri ${j + 1}`));
  const stiller = S.map((_, j) => (S.length === 1 ? { ...seriStili(s, 0), isaretDolu: true } : seriStili(s, j)));
  const gosterge = S.length >= 2 ? adlar.map((a, j) => ({ ad: a, tur: 'cizgi', stil: stiller[j] })) : null;
  const plan = kartezyenPlanla(s, {
    x: { tur: 'kategorik', adlar: kategoriler },
    y: { tur: 'sayisal', min: ymin, max: ymax },
    gosterge,
    gostergeBasligi: s.gostergeBasligi,
    altNotlar: s.hataNotu ? [s.hataNotu] : [],
  });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  const k = S.length;
  const kayma = k > 1 ? Math.min((plan.xBant * 0.4) / k, 10) : 0;
  const seriNoktalari = S.map((r, j) => {
    const dizi = new Array(kategoriler.length).fill(null);
    r.noktalar.forEach((p) => {
      if (!p || !Number.isFinite(Number(p.ortalama))) return;
      dizi[kimlik.get(String(p.x ?? ''))] = { m: Number(p.ortalama), alt: Number(p.alt), ust: Number(p.ust) };
    });
    return dizi.map((p, i) => (p ? { ...p, x: plan.sx(i) + (j - (k - 1) / 2) * kayma } : null));
  });
  seriNoktalari.forEach((dizi, j) => {
    const st = stiller[j];
    dizi.forEach((p) => {
      if (p && Number.isFinite(p.alt) && Number.isFinite(p.ust)) hataCubugu(c, p.x, plan.sy(p.alt), plan.sy(p.ust), 8, { renk: st.renk, kalinlik: 1 });
    });
  });
  seriNoktalari.forEach((dizi, j) => {
    const st = stiller[j];
    let parca = [];
    const bitir = () => {
      if (parca.length > 1) c.yol(yolVerisi(parca), { renk: st.renk, kalinlik: st.kalinlik, kesik: st.kesik });
      parca = [];
    };
    dizi.forEach((p) => {
      if (p) parca.push([p.x, plan.sy(p.m)]);
      else bitir();
    });
    bitir();
  });
  seriNoktalari.forEach((dizi, j) => dizi.forEach((p) => p && c.seriIsareti(p.x, plan.sy(p.m), stiller[j], 4)));
  plan.cerceveCiz(c);
  return c.svg();
}

/* =====================================================================
 * 6. Saçılım grafiği
 * ===================================================================== */

/**
 * En küçük kareler doğrusu ve ortalama yanıt için güven bandı:
 * ŷ ± t(1−α/2; n−2) · s · √(1/n + (x₀ − x̄)²/Sxx)  (R predict(lm, interval = 'confidence')).
 * Döndürür: {n, kesim, egim, r2, s, xOrt, sxx, bant(x0, duzey) → [alt, ust]} | null (n < 2 ya da Sxx = 0)
 */
export function olsDogrusu(x, y) {
  const n = Math.min(x.length, y.length);
  if (n < 2) return null;
  const xo = ortalama(x);
  const yo = ortalama(y);
  let sxx = 0;
  let sxy = 0;
  let syy = 0;
  for (let i = 0; i < n; i++) {
    const dx = x[i] - xo;
    const dy = y[i] - yo;
    sxx += dx * dx;
    sxy += dx * dy;
    syy += dy * dy;
  }
  if (!(sxx > 0)) return null;
  const egim = sxy / sxx;
  const kesim = yo - egim * xo;
  const ssr = Math.max(0, syy - egim * sxy);
  const sHata = n > 2 ? Math.sqrt(ssr / (n - 2)) : NaN;
  const r2 = syy > 0 ? 1 - ssr / syy : NaN;
  return {
    n,
    kesim,
    egim,
    r2,
    s: sHata,
    xOrt: xo,
    sxx,
    bant(x0, duzey = 0.95) {
      const yh = kesim + egim * x0;
      if (!(n > 2) || !Number.isFinite(sHata)) return [yh, yh];
      const e = qt(1 - (1 - duzey) / 2, n - 2) * sHata * Math.sqrt(1 / n + ((x0 - xo) ** 2) / sxx);
      return [yh - e, yh + e];
    },
  };
}

/**
 * Saçılım grafiği, isteğe bağlı OLS doğrusu ve ortalama için %95 güven bandıyla.
 * @param {number[]} x
 * @param {number[]} y
 * @param {object} [secenek] Ortak seçenekler ve {regresyonCizgisi=true|false|'grup' (grup başına doğru),
 *   gaBandi=true, duzey=0.95, gruplar?: Array<string|number> (x ile aynı uzunlukta grup etiketleri; şekil ve
 *   renkle ayrılır), denklemGoster=true ("y = a + bx; R² = …; n = …"), noktaBoyutu?: number}
 */
export function sacilimGrafigi(x, y, secenek = {}) {
  const s = secenekHazirla(secenek, { regresyonCizgisi: true, gaBandi: true, duzey: 0.95, denklemGoster: true, gostergeBasligi: '' });
  const xs = Array.isArray(x) || ArrayBuffer.isView(x) ? Array.from(x) : [];
  const ys = Array.isArray(y) || ArrayBuffer.isView(y) ? Array.from(y) : [];
  const gr = Array.isArray(s.gruplar) ? s.gruplar : null;
  const noktalar = [];
  for (let i = 0; i < Math.min(xs.length, ys.length); i++) {
    const a = xs[i];
    const b = ys[i];
    if (typeof a === 'number' && typeof b === 'number' && Number.isFinite(a) && Number.isFinite(b)) noktalar.push({ x: a, y: b, g: gr ? String(gr[i] ?? '') : '' });
  }
  if (!noktalar.length) return bosGrafik(s);
  const grupAdlari = [];
  if (gr) for (const p of noktalar) if (!grupAdlari.includes(p.g)) grupAdlari.push(p.g);
  const gruplu = grupAdlari.length > 0;
  const [xmin, xmax] = sinirlar(noktalar.map((p) => p.x));
  const yDeg = noktalar.map((p) => p.y);
  // Doğrular: genel ya da grup başına; yalnızca verinin x aralığında çizilir (dış değer biçme yok)
  const dogrular = [];
  const dogruEkle = (alt, stil, grupMu) => {
    const f = olsDogrusu(alt.map((p) => p.x), alt.map((p) => p.y));
    if (!f) return;
    const [a, b] = sinirlar(alt.map((p) => p.x));
    const orneklem = Array.from({ length: 61 }, (_, i) => a + ((b - a) * i) / 60);
    const bant = s.gaBandi && f.n > 2 ? orneklem.map((t) => [t, ...f.bant(t, s.duzey)]) : [];
    bant.forEach((v) => yDeg.push(v[1], v[2]));
    dogrular.push({ f, a, b, bant, stil, grupMu });
  };
  if (s.regresyonCizgisi === 'grup' && gruplu) grupAdlari.forEach((g, j) => dogruEkle(noktalar.filter((p) => p.g === g), seriStili(s, j), true));
  else if (s.regresyonCizgisi) dogruEkle(noktalar, null, false);
  const [ymin, ymax] = sinirlar(yDeg);
  const notlar = [];
  const genel = dogrular.find((d) => !d.grupMu);
  if (s.denklemGoster && genel) {
    const { kesim, egim, r2, n } = genel.f;
    const isaret = egim < 0 ? '−' : '+';
    notlar.push(
      simgeleriItalikYap(
        `y = ${ozetBicimle(kesim, s)} ${isaret} ${ozetBicimle(Math.abs(egim), s)}x; R² = ${basSifirUygula(sayiBicimle(r2, 3, s), s)}; n = ${sayiBicimle(n, 0, s)}`,
      ),
    );
  }
  const stiller = grupAdlari.map((_, j) => seriStili(s, j));
  const gosterge = gruplu ? grupAdlari.map((g, j) => ({ ad: g || '(boş)', tur: s.regresyonCizgisi === 'grup' ? 'cizgi' : 'isaret', stil: stiller[j] })) : null;
  const plan = kartezyenPlanla(s, {
    x: { tur: 'sayisal', min: xmin, max: xmax },
    y: { tur: 'sayisal', min: ymin, max: ymax },
    gosterge,
    gostergeBasligi: s.gostergeBasligi,
    notlar,
    izgaraYonu: 'xy',
  });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  for (const d of dogrular) {
    if (!d.bant.length) continue;
    const ust = d.bant.map((v) => [plan.sx(v[0]), plan.sy(v[2])]);
    const alt = d.bant.map((v) => [plan.sx(v[0]), plan.sy(v[1])]).reverse();
    const renk = d.stil && s.T.ad === 'renkli' ? d.stil.renk : '#000000';
    c.yol(yolVerisi([...ust, ...alt], true), { dolgu: renk, opaklik: d.stil && s.T.ad === 'renkli' ? 0.14 : 0.12 });
  }
  const n = noktalar.length;
  if (gruplu) {
    const r = s.noktaBoyutu ?? (n > 1000 ? 2.4 : n > 300 ? 3 : 3.8);
    noktalar.forEach((p) => c.seriIsareti(plan.sx(p.x), plan.sy(p.y), stiller[grupAdlari.indexOf(p.g)], r));
  } else {
    noktalariCiz(c, s, noktalar.map((p) => [plan.sx(p.x), plan.sy(p.y)]), n, s.noktaBoyutu);
  }
  for (const d of dogrular) {
    const renk = d.stil ? d.stil.renk : s.T.murekkep;
    c.cizgi(plan.sx(d.a), plan.sy(d.f.kesim + d.f.egim * d.a), plan.sx(d.b), plan.sy(d.f.kesim + d.f.egim * d.b), {
      renk,
      kalinlik: d.stil ? d.stil.kalinlik : 1.6,
      kesik: d.stil ? d.stil.kesik : undefined,
    });
  }
  plan.cerceveCiz(c);
  return c.svg();
}

/* =====================================================================
 * 7. Kaplan–Meier grafiği
 * ===================================================================== */

/** Sağdan sürekli basamak fonksiyonunun t anındaki değeri */
function basamakDegeri(adimlar, t, alan = 'sagkalim', bas = 1) {
  let v = bas;
  for (const a of adimlar) {
    if (a.zaman <= t) v = a[alan];
    else break;
  }
  return v;
}

/**
 * Kaplan–Meier sağkalım eğrileri: (0, 1)'den başlayan basamaklar, sansür çentikleri, isteğe bağlı güven
 * bandı (basamaklı gölge), eğrilerin altında hizalı risk tablosu ve not (ör. log-rank p).
 * @param {Array<{ad:string, adimlar:Array<{zaman:number, sagkalim:number, gaAlt?:number, gaUst?:number}>,
 *   sansurZamanlari?:number[]}>} egriler adimlar: olay zamanlarında yeni sağkalım değerleri (0–1)
 * @param {object} [secenek] Ortak seçenekler ve {gaGoster=false, yuzde=false (0–100 ölçeği),
 *   riskTablosu?: {zamanlar:number[], sayilar:{[ad]:number[]} | number[][]}, notMetni?: string,
 *   yMin=0 (y ekseni alt sınırı, olasılık ölçeğinde), sansurGoster=true}
 */
export function kaplanMeierGrafigi(egriler, secenek = {}) {
  const yuzde = !!secenek.yuzde;
  const s = secenekHazirla(secenek, {
    gaGoster: false,
    yuzde: false,
    notMetni: '',
    xEtiket: 'Zaman',
    yEtiket: yuzde ? 'Sağkalım (%)' : 'Sağkalım olasılığı',
    yMin: 0,
    sansurGoster: true,
    gostergeBasligi: '',
  });
  const E = (Array.isArray(egriler) ? egriler : [])
    .filter((e) => e && Array.isArray(e.adimlar))
    .map((e, j) => ({
      ad: e.ad != null && String(e.ad) !== '' ? String(e.ad) : `Eğri ${j + 1}`,
      adimlar: e.adimlar
        .filter((a) => a && Number.isFinite(a.zaman) && a.zaman >= 0 && Number.isFinite(a.sagkalim))
        .map((a) => ({
          zaman: a.zaman,
          sagkalim: sinirla(a.sagkalim, 0, 1),
          gaAlt: Number.isFinite(a.gaAlt) ? sinirla(a.gaAlt, 0, 1) : sinirla(a.sagkalim, 0, 1),
          gaUst: Number.isFinite(a.gaUst) ? sinirla(a.gaUst, 0, 1) : sinirla(a.sagkalim, 0, 1),
        }))
        .sort((p, q) => p.zaman - q.zaman),
      sansur: sayiDizisi(e.sansurZamanlari || []).filter((t) => t >= 0).sort((p, q) => p - q),
    }));
  if (!E.length) return bosGrafik(s);
  const k = yuzde ? 100 : 1;
  const risk = s.riskTablosu && Array.isArray(s.riskTablosu.zamanlar) ? s.riskTablosu : null;
  const riskZaman = risk ? sayiDizisi(risk.zamanlar) : [];
  const riskSatirlari = risk
    ? E.map((e, j) => {
        const kaynak = Array.isArray(risk.sayilar) ? risk.sayilar[j] : risk.sayilar?.[e.ad];
        return Array.isArray(kaynak) ? { ad: e.ad, j, sayilar: kaynak } : null;
      }).filter(Boolean)
    : [];
  let tMax = 0;
  E.forEach((e) => {
    e.adimlar.forEach((a) => (tMax = Math.max(tMax, a.zaman)));
    e.sansur.forEach((t) => (tMax = Math.max(tMax, t)));
  });
  riskZaman.forEach((t) => (tMax = Math.max(tMax, t)));
  if (!(tMax > 0)) tMax = 1;
  const stiller = E.map((_, j) => ({ ...seriStili(s, j), isaretDolu: true }));
  const gosterge = E.length >= 2 ? E.map((e, j) => ({ ad: e.ad, tur: 'cizgi', stil: stiller[j], isaretli: false })) : null;
  const fs = s.fs;
  const satirH = fs.tik * 1.45;
  const riskBaslikH = fs.kucuk * 1.6;
  const altEk = riskSatirlari.length ? riskBaslikH + riskSatirlari.length * satirH + 6 : 0;
  // Risk tablosundaki ilk sütunun (genellikle t = 0) sayıları adlarla çakışmasın
  const ilkSayiYarisi = riskSatirlari.length
    ? Math.max(...riskSatirlari.map((r) => metinGenisligi(sayiBicimle(Number.isFinite(r.sayilar[0]) ? r.sayilar[0] : 0, 0, s), fs.tik))) / 2
    : 0;
  const solMin = riskSatirlari.length
    ? s.kenar + 22 + Math.max(...riskSatirlari.map((r) => metinGenisligi(kisalt(r.ad, s.genislik * 0.25, fs.tik), fs.tik))) + ilkSayiYarisi + 10
    : 0;
  const yMin = sinirla(Number(s.yMin) || 0, 0, 0.95);
  // x tikleri: risk zamanları eşit aralıklıysa aynı adımla izlem sonuna kadar uzatılır; değilse güzel adımlar
  let xEksen = { tur: 'sayisal', min: 0, max: tMax, sifirDahil: true };
  if (riskZaman.length >= 2) {
    const rz = [...riskZaman].sort((a, b) => a - b);
    const adim = rz[1] - rz[0];
    const esit = adim > 0 && rz.every((t, i) => Math.abs(t - (rz[0] + i * adim)) < 1e-9 * Math.max(1, Math.abs(t)));
    if (esit) {
      const tikler = [...rz];
      while (tikler[tikler.length - 1] < tMax - 1e-9) tikler.push(Number((tikler[tikler.length - 1] + adim).toPrecision(12)));
      xEksen = { tur: 'sayisal', min: Math.min(0, rz[0]), max: tikler[tikler.length - 1], adimlar: tikler, sabitAlan: [Math.min(0, rz[0]), tikler[tikler.length - 1]] };
    } else {
      xEksen = { tur: 'sayisal', min: 0, max: Math.max(tMax, ...rz), sifirDahil: true };
    }
  }
  const plan = kartezyenPlanla(s, {
    x: xEksen,
    y: { tur: 'sayisal', min: yMin * k, max: k, sabitAlan: [yMin * k, k] },
    gosterge,
    gostergeBasligi: s.gostergeBasligi,
    notlar: s.notMetni ? [String(s.notMetni)] : [],
    altEk,
    solMin,
  });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  const xSon = plan.x.alan[1];
  const yAlt = yMin * k;
  const py = (v) => plan.sy(Math.max(yAlt, v * k));
  const egriBitis = (e) => Math.min(xSon, Math.max(e.adimlar.length ? e.adimlar[e.adimlar.length - 1].zaman : 0, e.sansur.length ? e.sansur[e.sansur.length - 1] : 0));
  const basamakYolu = (e, alan) => {
    const bas = e.adimlar.length && e.adimlar[0].zaman <= 0 ? e.adimlar[0][alan] : 1;
    let deger = bas;
    const p = [[0, deger]];
    for (const a of e.adimlar) {
      if (a.zaman <= 0 || a.zaman > xSon) continue;
      p.push([a.zaman, deger]);
      deger = a[alan];
      p.push([a.zaman, deger]);
    }
    const son = egriBitis(e);
    if (son > p[p.length - 1][0]) p.push([son, deger]);
    return p;
  };
  if (s.gaGoster) {
    E.forEach((e, j) => {
      const ust = basamakYolu(e, 'gaUst');
      const alt = basamakYolu(e, 'gaAlt').reverse();
      const renk = s.T.ad === 'renkli' || (Array.isArray(s.renkler) && s.renkler.length) ? stiller[j].renk : '#000000';
      c.yol(yolVerisi([...ust, ...alt].map(([t, v]) => [plan.sx(t), py(v)]), true), { dolgu: renk, opaklik: s.T.ad === 'renkli' ? 0.15 : 0.1 });
    });
  }
  E.forEach((e, j) => {
    const st = stiller[j];
    c.yol(yolVerisi(basamakYolu(e, 'sagkalim').map(([t, v]) => [plan.sx(t), py(v)])), { renk: st.renk, kalinlik: st.kalinlik, kesik: st.kesik, birlesim: 'miter', uc: 'butt' });
  });
  if (s.sansurGoster) {
    E.forEach((e, j) => {
      const st = stiller[j];
      for (const t of e.sansur) {
        if (t > xSon) continue;
        const x = plan.sx(t);
        const y = py(basamakDegeri(e.adimlar, t));
        c.cizgi(x, y - 4.5, x, y + 4.5, { renk: st.renk, kalinlik: 1.2 });
      }
    });
  }
  if (riskSatirlari.length) {
    let y = plan.altBlokY + fs.kucuk;
    c.metin(s.kenar, y, 'Risk altındaki sayı', { boyut: fs.kucuk, kalin: true });
    y += riskBaslikH - fs.kucuk;
    const gosterilen = riskZaman.map((t) => t >= plan.x.alan[0] - 1e-9 && t <= xSon + 1e-9);
    riskSatirlari.forEach((r) => {
      const ym = y + satirH / 2;
      const st = stiller[r.j];
      c.cizgi(s.kenar, ym, s.kenar + 16, ym, { renk: st.renk, kalinlik: st.kalinlik, kesik: st.kesik });
      c.metin(s.kenar + 22, ym + fs.tik * 0.35, kisalt(r.ad, s.genislik * 0.25, fs.tik), { boyut: fs.tik });
      let oncekiSag = -Infinity;
      riskZaman.forEach((t, i) => {
        const deger = r.sayilar[i];
        if (!gosterilen[i] || !Number.isFinite(deger)) return;
        const metin = sayiBicimle(deger, 0, s);
        const x = plan.sx(t);
        const w = metinGenisligi(metin, fs.tik);
        if (x - w / 2 < oncekiSag + 4) return; // çok yakın zamanlar: sayılar üst üste binmesin
        oncekiSag = x + w / 2;
        c.metin(x, ym + fs.tik * 0.35, metin, { boyut: fs.tik, hiza: 'orta', renk: s.T.ikincil });
      });
      y += satirH;
    });
  }
  plan.cerceveCiz(c);
  return c.svg();
}

/* =====================================================================
 * 8. ROC eğrisi
 * ===================================================================== */

/**
 * ROC eğrileri: kare çizim alanı, köşegen referans çizgisi, göstergede AUC.
 * @param {Array<{ad?:string, noktalar:Array<{fpr:number, tpr:number}>, auc?:number, aucGa?:[number, number]}>} egriler
 * @param {object} [secenek] Ortak seçenekler ve {optimum?: {fpr, tpr, etiket?} | Array<…> (işaretlenecek kesim
 *   noktası), gostergeKonumu='sag', kosegen=true}
 */
export function rocGrafigi(egriler, secenek = {}) {
  const s = secenekHazirla(secenek, { xEtiket: '1 − Özgüllük', yEtiket: 'Duyarlılık', gostergeKonumu: 'sag', kosegen: true });
  const E = (Array.isArray(egriler) ? egriler : [])
    .filter((e) => e && Array.isArray(e.noktalar))
    .map((e) => {
      const p = e.noktalar
        .filter((q) => q && Number.isFinite(q.fpr) && Number.isFinite(q.tpr))
        .map((q) => ({ fpr: sinirla(q.fpr, 0, 1), tpr: sinirla(q.tpr, 0, 1) }))
        .sort((a, b) => a.fpr - b.fpr || a.tpr - b.tpr);
      if (!p.length || p[0].fpr > 0 || p[0].tpr > 0) p.unshift({ fpr: 0, tpr: 0 });
      const son = p[p.length - 1];
      if (son.fpr < 1 || son.tpr < 1) p.push({ fpr: 1, tpr: 1 });
      return { ...e, noktalar: p };
    });
  if (!E.length) return bosGrafik(s);
  const etiket = (e, j) => {
    const parca = [];
    if (Number.isFinite(e.auc)) {
      let t = `AUC = ${basSifirUygula(sayiBicimle(e.auc, 3, s), s)}`;
      if (Array.isArray(e.aucGa) && e.aucGa.every(Number.isFinite)) {
        t += `\n${yuzdeBicimle(95, 0, s)} GA: ${basSifirUygula(sayiBicimle(e.aucGa[0], 3, s), s)}–${basSifirUygula(sayiBicimle(e.aucGa[1], 3, s), s)}`;
      }
      parca.push(t);
    }
    const ad = e.ad != null && String(e.ad) !== '' ? String(e.ad) : E.length > 1 ? `Eğri ${j + 1}` : '';
    if (ad && parca.length) return `${ad}\n${parca[0]}`;
    return ad || parca[0] || '';
  };
  const stiller = E.map((_, j) => ({ ...seriStili(s, j), isaretDolu: true }));
  const gosterge = E.map((e, j) => ({ ad: etiket(e, j), tur: 'cizgi', stil: stiller[j], isaretli: false })).filter((o) => o.ad);
  const adimlar = [0, 0.2, 0.4, 0.6, 0.8, 1];
  const plan = kartezyenPlanla(s, {
    x: { tur: 'sayisal', min: 0, max: 1, sabitAlan: [0, 1], adimlar },
    y: { tur: 'sayisal', min: 0, max: 1, sabitAlan: [0, 1], adimlar },
    gosterge: gosterge.length ? gosterge : null,
    kare: true,
    izgaraYonu: 'xy',
  });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  if (s.kosegen) c.cizgi(plan.sx(0), plan.sy(0), plan.sx(1), plan.sy(1), { renk: s.T.referans, kalinlik: 0.9, kesik: '4 3' });
  E.forEach((e, j) => {
    const st = stiller[j];
    c.yol(yolVerisi(e.noktalar.map((p) => [plan.sx(p.fpr), plan.sy(p.tpr)])), { renk: st.renk, kalinlik: st.kalinlik, kesik: st.kesik });
  });
  const optimumlar = s.optimum ? (Array.isArray(s.optimum) ? s.optimum : [s.optimum]) : [];
  for (const o of optimumlar) {
    if (!o || !Number.isFinite(o.fpr) || !Number.isFinite(o.tpr)) continue;
    const x = plan.sx(sinirla(o.fpr, 0, 1));
    const y = plan.sy(sinirla(o.tpr, 0, 1));
    c.isaret(x, y, 'daire', 4.2, { dolgu: s.T.murekkep, cizgi: '#ffffff', kalinlik: 1.2 });
    if (o.etiket) {
      const satirlar = metniSar(String(o.etiket), plan.gen * 0.6, s.fs.kucuk);
      const w = enGenisSatir(satirlar, s.fs.kucuk);
      const sagaSigar = x + 8 + w <= plan.sag;
      const tx = sagaSigar ? x + 8 : x - 8;
      const aralik = s.fs.kucuk * 1.2;
      let ty = y + 8 + s.fs.kucuk;
      if (ty + (satirlar.length - 1) * aralik > plan.alt - 4) ty = y - 8 - (satirlar.length - 1) * aralik;
      // Yarı saydam beyaz zemin: etiket eğri ya da köşegenle kesişse de okunur
      const kx = sagaSigar ? tx - 2 : tx - w - 2;
      c.dikdortgen(kx, ty - s.fs.kucuk * 0.95, w + 4, (satirlar.length - 1) * aralik + s.fs.kucuk * 1.3, { dolgu: '#ffffff', opaklik: 0.85 });
      c.satirlar(tx, ty, satirlar, { boyut: s.fs.kucuk, hiza: sagaSigar ? 'bas' : 'son', aralik });
    }
  }
  plan.cerceveCiz(c);
  return c.svg();
}

/* =====================================================================
 * 9. Bland–Altman grafiği
 * ===================================================================== */

/**
 * Bland–Altman uyum grafiği: ortalama fark (yanlılık) ve uyum sınırları yatay çizgiler, değerleri sağ kenarda.
 * Verilmeyen değerler farklardan hesaplanır: yanlılık = ort(d), sınırlar = yanlılık ± 1,96·SS(d).
 * @param {number[]} ortalamalar İki ölçümün ortalaması (x)
 * @param {number[]} farklar İki ölçümün farkı (y)
 * @param {object} [secenek] Ortak seçenekler ve {bias?, loaAlt?, loaUst?, carpan=1.96,
 *   gaCizgileri?: true (Bland & Altman 1999 yaklaşık %95 GA: SH(yanlılık) = s/√n, SH(sınır) = s·√(3/n))
 *     | {bias:[alt,ust], loaAlt:[alt,ust], loaUst:[alt,ust]}, sifirCizgisi=true}
 */
export function blandAltmanGrafigi(ortalamalar, farklar, secenek = {}) {
  const s = secenekHazirla(secenek, { xEtiket: 'İki ölçümün ortalaması', yEtiket: 'İki ölçüm arasındaki fark', carpan: 1.96, sifirCizgisi: true });
  const xs = Array.isArray(ortalamalar) || ArrayBuffer.isView(ortalamalar) ? Array.from(ortalamalar) : [];
  const ds = Array.isArray(farklar) || ArrayBuffer.isView(farklar) ? Array.from(farklar) : [];
  const nokta = [];
  for (let i = 0; i < Math.min(xs.length, ds.length); i++) if (Number.isFinite(xs[i]) && Number.isFinite(ds[i])) nokta.push([xs[i], ds[i]]);
  if (!nokta.length) return bosGrafik(s);
  const d = nokta.map((p) => p[1]);
  const n = d.length;
  const ss = n > 1 ? standartSapma(d) : NaN;
  const bias = Number.isFinite(s.bias) ? s.bias : ortalama(d);
  const loaAlt = Number.isFinite(s.loaAlt) ? s.loaAlt : bias - s.carpan * ss;
  const loaUst = Number.isFinite(s.loaUst) ? s.loaUst : bias + s.carpan * ss;
  let ga = null;
  if (s.gaCizgileri === true && n >= 3 && Number.isFinite(ss)) {
    const t = qt(0.975, n - 1);
    const shB = ss / Math.sqrt(n);
    const shL = ss * Math.sqrt(3 / n);
    ga = { bias: [bias - t * shB, bias + t * shB], loaAlt: [loaAlt - t * shL, loaAlt + t * shL], loaUst: [loaUst - t * shL, loaUst + t * shL] };
  } else if (s.gaCizgileri && typeof s.gaCizgileri === 'object') {
    ga = s.gaCizgileri;
  }
  const gaListesi = ga ? ['bias', 'loaAlt', 'loaUst'].map((k) => ga[k]).filter((a) => Array.isArray(a) && a.length === 2 && a.every(Number.isFinite)) : [];
  const cizgiler = [
    { deger: loaUst, metin: `+${sayiBicimle(s.carpan, 2, s)} SS: ${ozetBicimle(loaUst, s)}`, kesik: '6 3' },
    { deger: bias, metin: `Ortalama: ${ozetBicimle(bias, s)}`, kesik: '' },
    { deger: loaAlt, metin: `−${sayiBicimle(s.carpan, 2, s)} SS: ${ozetBicimle(loaAlt, s)}`, kesik: '6 3' },
  ].filter((c) => Number.isFinite(c.deger));
  const fs = s.fs.kucuk;
  const sagEk = Math.max(...cizgiler.map((c) => metinGenisligi(c.metin, fs))) + 10;
  const [xmin, xmax] = sinirlar(nokta.map((p) => p[0]));
  const [ymin, ymax] = sinirlar([...d, ...cizgiler.map((c) => c.deger), ...gaListesi.flat(), ...(s.sifirCizgisi ? [] : [])]);
  const plan = kartezyenPlanla(s, { x: { tur: 'sayisal', min: xmin, max: xmax }, y: { tur: 'sayisal', min: ymin, max: ymax }, sagEk, izgaraYonu: 'xy' });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  for (const [a, b] of gaListesi) c.dikdortgen(plan.sol, plan.sy(Math.max(a, b)), plan.gen, plan.sy(Math.min(a, b)) - plan.sy(Math.max(a, b)), { dolgu: '#000000', opaklik: 0.08 });
  if (s.sifirCizgisi && plan.y.alan[0] < 0 && plan.y.alan[1] > 0 && Math.abs(plan.sy(0) - plan.sy(bias)) > 1) {
    c.cizgi(plan.sol, plan.sy(0), plan.sag, plan.sy(0), { renk: s.T.referans, kalinlik: 0.7 });
  }
  noktalariCiz(c, s, nokta.map(([x, y]) => [plan.sx(x), plan.sy(y)]), n);
  cizgiler.forEach((l) => c.cizgi(plan.sol, plan.sy(l.deger), plan.sag, plan.sy(l.deger), { renk: s.T.murekkep, kalinlik: 1.2, kesik: l.kesik }));
  // Sağ kenar etiketleri: çakışırsa en az bir satır aralığıyla ayrılır
  const yer = cizgiler.map((l) => ({ ...l, y: plan.sy(l.deger) })).sort((a, b) => a.y - b.y);
  const aralik = fs * 1.2;
  for (let i = 1; i < yer.length; i++) if (yer[i].y - yer[i - 1].y < aralik) yer[i].y = yer[i - 1].y + aralik;
  const tasma = yer.length ? yer[yer.length - 1].y - (plan.alt + fs) : 0;
  if (tasma > 0) yer.forEach((l) => (l.y -= tasma));
  yer.forEach((l) => c.metin(plan.sag + 6, l.y + fs * 0.35, l.metin, { boyut: fs, renk: s.T.ikincil }));
  plan.cerceveCiz(c);
  return c.svg();
}

/* =====================================================================
 * 10. Çapraz tablo çubuk grafiği
 * ===================================================================== */

/**
 * Çapraz tablo için kümelenmiş ya da yığılmış (yüzdeyle %100) çubuklar; sığdığı yerde değer etiketleri.
 * x eksenindeki kategoriler yüzde tabanıdır: yuzdeTabani='sutun' → her sütun bir çubuk/küme ve satır
 * kategorileri seriler (göstergede); 'satir' → tersi.
 * @param {number[][]} tablo Frekanslar (satır × sütun)
 * @param {string[]} satirAdlari
 * @param {string[]} sutunAdlari
 * @param {object} [secenek] Ortak seçenekler ve {tip='kumelenmis'|'yigilmis', yuzde=true, yuzdeTabani='sutun'|'satir',
 *   degerEtiketi=true, gostergeBasligi?: string}
 */
export function caprazCubukGrafigi(tablo, satirAdlari, sutunAdlari, secenek = {}) {
  const yuzde = secenek.yuzde ?? true;
  const s = secenekHazirla(secenek, { tip: 'kumelenmis', yuzde: true, yuzdeTabani: 'sutun', degerEtiketi: true, yEtiket: yuzde ? 'Yüzde (%)' : 'Frekans', gostergeBasligi: '' });
  const M = (Array.isArray(tablo) ? tablo : []).filter((r) => Array.isArray(r) || ArrayBuffer.isView(r)).map((r) => Array.from(r, (v) => (Number.isFinite(v) && v > 0 ? v : 0)));
  const r = M.length;
  const k = r ? Math.max(...M.map((satir) => satir.length)) : 0;
  if (!r || !k) return bosGrafik(s);
  const satirTabani = s.yuzdeTabani === 'satir';
  const sAd = adHazirla(satirAdlari, r, 'Satır', false);
  const kAd = adHazirla(sutunAdlari, k, 'Sütun', false);
  const kategoriler = satirTabani ? sAd : kAd;
  const seriler = satirTabani ? kAd : sAd;
  const deger = (kat, ser) => (satirTabani ? M[kat][ser] ?? 0 : M[ser]?.[kat] ?? 0);
  const toplamlar = kategoriler.map((_, kat) => seriler.reduce((t, __, ser) => t + deger(kat, ser), 0));
  const v = (kat, ser) => (yuzde ? (toplamlar[kat] > 0 ? (100 * deger(kat, ser)) / toplamlar[kat] : 0) : deger(kat, ser));
  const yigilmis = s.tip === 'yigilmis';
  let ymax = 0;
  kategoriler.forEach((_, kat) => {
    if (yigilmis) ymax = Math.max(ymax, yuzde ? 100 : toplamlar[kat]);
    else seriler.forEach((__, ser) => (ymax = Math.max(ymax, v(kat, ser))));
  });
  if (!yigilmis && s.degerEtiketi) ymax *= 1.06;
  const yEksen = yigilmis && yuzde ? { tur: 'sayisal', min: 0, max: 100, sabitAlan: [0, 100] } : { tur: 'sayisal', min: 0, max: ymax || 1, sifirDahil: true, tamsayi: !yuzde };
  const stiller = seriler.map((_, j) => seriStili(s, j));
  const gosterge = seriler.map((ad, j) => ({ ad, tur: 'dolgu', stil: stiller[j] }));
  const plan = kartezyenPlanla(s, { x: { tur: 'kategorik', adlar: kategoriler }, y: yEksen, gosterge, gostergeBasligi: s.gostergeBasligi });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  const T = s.T;
  const kenar = T.ad === 'yayin' ? { cizgi: '#000000', kalinlik: 0.75 } : { cizgi: '#ffffff', kalinlik: 1 };
  const fsD = s.fs.kucuk;
  const etiketMetni = (deg) => (yuzde ? yuzdeBicimle(deg, 1, s) : sayiBicimle(deg, 0, s));
  const m = seriler.length;
  kategoriler.forEach((_, kat) => {
    const cx = plan.sx(kat);
    if (yigilmis) {
      const gen = Math.max(4, Math.min(plan.xBant * 0.6, 64));
      let birikim = 0;
      seriler.forEach((__, ser) => {
        const deg = v(kat, ser);
        if (!(deg > 0)) return;
        const y1 = plan.sy(birikim);
        const y2 = plan.sy(birikim + deg);
        const st = stiller[ser];
        c.dikdortgen(cx - gen / 2, y2, gen, y1 - y2, { dolgu: c.dolgu(st), ...kenar });
        if (s.degerEtiketi) {
          const t = etiketMetni(deg);
          const tw = metinGenisligi(t, fsD);
          if (y1 - y2 >= fsD + 3 && tw + 6 <= gen) {
            const ym = (y1 + y2) / 2;
            let renk = metinRengi(st.dolgu);
            if (st.desen) {
              c.dikdortgen(cx - tw / 2 - 2, ym - fsD * 0.6, tw + 4, fsD * 1.2, { dolgu: '#ffffff' });
              renk = '#000000';
            }
            c.metin(cx, ym + fsD * 0.35, t, { boyut: fsD, hiza: 'orta', renk });
          }
        }
        birikim += deg;
      });
    } else {
      const gen = Math.max(1.5, Math.min((plan.xBant * 0.8) / m, 48));
      const x0 = cx - (gen * m) / 2;
      seriler.forEach((__, ser) => {
        const deg = v(kat, ser);
        const st = stiller[ser];
        const x = x0 + ser * gen;
        if (deg > 0) c.dikdortgen(x, plan.sy(deg), gen, plan.sy(0) - plan.sy(deg), { dolgu: c.dolgu(st), ...kenar });
        if (s.degerEtiketi) {
          const t = etiketMetni(deg);
          if (metinGenisligi(t, fsD) + 2 <= gen) c.metin(x + gen / 2, plan.sy(deg) - 3, t, { boyut: fsD, hiza: 'orta', renk: T.ikincil });
        }
      });
    }
  });
  plan.cerceveCiz(c);
  return c.svg();
}

/* =====================================================================
 * 11. Frekans (çubuk) ve pasta grafiği
 * ===================================================================== */

/**
 * Tek değişkenin frekans çubuk grafiği.
 * @param {string[]} etiketler Kategori adları
 * @param {number[]} sayilar Frekanslar
 * @param {object} [secenek] Ortak seçenekler ve {yuzde=true (çubuk yüksekliği yüzde), yatay=false,
 *   degerEtiketi=true|'sayi'|'yuzde'|'ikisi'|false, sirala=false|'azalan'|'artan'}
 */
export function frekansGrafigi(etiketler, sayilar, secenek = {}) {
  const yuzde = secenek.yuzde ?? true;
  const yatay = !!secenek.yatay;
  const eksenAdi = yuzde ? 'Yüzde (%)' : 'Frekans';
  const s = secenekHazirla(secenek, { yuzde: true, yatay: false, degerEtiketi: true, sirala: false, ...(yatay ? { xEtiket: eksenAdi } : { yEtiket: eksenAdi }) });
  const say = Array.isArray(sayilar) || ArrayBuffer.isView(sayilar) ? Array.from(sayilar) : [];
  let ogeler = say.map((v, i) => ({ ad: Array.isArray(etiketler) && etiketler[i] != null ? String(etiketler[i]) : `Kategori ${i + 1}`, n: Number.isFinite(v) && v > 0 ? v : 0 }));
  if (!ogeler.length) return bosGrafik(s);
  if (s.sirala === 'azalan') ogeler = [...ogeler].sort((a, b) => b.n - a.n);
  else if (s.sirala === 'artan') ogeler = [...ogeler].sort((a, b) => a.n - b.n);
  const toplam = ogeler.reduce((t, o) => t + o.n, 0);
  const deger = (o) => (yuzde ? (toplam > 0 ? (100 * o.n) / toplam : 0) : o.n);
  const mod = s.degerEtiketi === true ? (yuzde ? 'yuzde' : 'sayi') : s.degerEtiketi;
  const etiketMetni = (o) => {
    const p = toplam > 0 ? (100 * o.n) / toplam : 0;
    if (mod === 'yuzde') return yuzdeBicimle(p, 1, s);
    if (mod === 'sayi') return sayiBicimle(o.n, ondalikSayisiFrekans(o.n), s);
    if (mod === 'ikisi') return `${sayiBicimle(o.n, ondalikSayisiFrekans(o.n), s)} (${yuzdeBicimle(p, 1, s)})`;
    return '';
  };
  const vmax = Math.max(...ogeler.map(deger));
  const fsD = s.fs.kucuk;
  const degerEksen = { tur: 'sayisal', min: 0, max: (vmax || 1) * (mod && !yatay ? 1.05 : 1), sifirDahil: true, tamsayi: !yuzde && ogeler.every((o) => Number.isInteger(o.n)) };
  const katEksen = { tur: 'kategorik', adlar: ogeler.map((o) => o.ad) };
  const sagEk = mod && yatay ? Math.max(...ogeler.map((o) => metinGenisligi(etiketMetni(o), fsD))) + 6 : 0;
  const plan = kartezyenPlanla(s, yatay ? { x: degerEksen, y: katEksen, sagEk } : { x: katEksen, y: degerEksen });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  const st = tekSeriStili(s);
  ogeler.forEach((o, i) => {
    const d = deger(o);
    const t = mod ? etiketMetni(o) : '';
    if (yatay) {
      const h = Math.max(2, Math.min(plan.yBant * 0.65, 30));
      const cy = plan.sy(i);
      const x0 = plan.sx(0);
      const x1 = plan.sx(d);
      if (d > 0) c.dikdortgen(x0, cy - h / 2, x1 - x0, h, { dolgu: st.dolgu, cizgi: st.kenar, kalinlik: st.kenarKalinlik });
      if (t && plan.yBant >= fsD * 1.05) c.metin(x1 + 4, cy + fsD * 0.35, t, { boyut: fsD, renk: s.T.ikincil });
    } else {
      const w = Math.max(2, Math.min(plan.xBant * 0.65, 60));
      const cx = plan.sx(i);
      const y1 = plan.sy(d);
      if (d > 0) c.dikdortgen(cx - w / 2, y1, w, plan.sy(0) - y1, { dolgu: st.dolgu, cizgi: st.kenar, kalinlik: st.kenarKalinlik });
      if (t && metinGenisligi(t, fsD) + 2 <= plan.xBant) c.metin(cx, y1 - 4, t, { boyut: fsD, hiza: 'orta', renk: s.T.ikincil });
    }
  });
  plan.cerceveCiz(c);
  return c.svg();
}

function ondalikSayisiFrekans(v) {
  return Number.isInteger(v) ? 0 : 2;
}

/**
 * Pasta grafiği: dilimler saat 12'den saat yönünde, doğrudan etiketler (ad + yüzde) kılavuz çizgileriyle
 * dışarıda; aynı taraftaki etiketler çakışmayacak biçimde dikeyde aralanır.
 * @param {string[]} etiketler
 * @param {number[]} sayilar
 * @param {object} [secenek] Ortak seçenekler ve {sayiGoster=false (yüzdenin yanında n), ondalik=1}
 */
export function pastaGrafigi(etiketler, sayilar, secenek = {}) {
  const s = secenekHazirla(secenek, { sayiGoster: false, ondalik: 1 });
  const say = Array.isArray(sayilar) || ArrayBuffer.isView(sayilar) ? Array.from(sayilar) : [];
  const ogeler = say
    .map((v, i) => ({ i, ad: Array.isArray(etiketler) && etiketler[i] != null ? String(etiketler[i]) : `Kategori ${i + 1}`, n: Number.isFinite(v) && v > 0 ? v : 0 }))
    .filter((o) => o.n > 0);
  const toplam = ogeler.reduce((t, o) => t + o.n, 0);
  if (!ogeler.length || !(toplam > 0)) return bosGrafik(s);
  const W = s.genislik;
  const H = s.yukseklik;
  const P = s.kenar;
  const fs = s.fs.tik;
  const ust = ustBolumOlc(s, null, []);
  const ustY = P + ust.yukseklik + (ust.yukseklik ? 10 : 0);
  const alanH = H - P - ustY;
  const etiketMaks = Math.max(60, W * 0.24);
  const etiketler2 = ogeler.map((o) => {
    const p = (100 * o.n) / toplam;
    const yuzdeT = yuzdeBicimle(p, s.ondalik, s) + (s.sayiGoster ? ` (n = ${sayiBicimle(o.n, ondalikSayisiFrekans(o.n), s)})` : '');
    const satirlar = [...metniSar(o.ad, etiketMaks, fs, { maksSatir: 2 }), yuzdeT];
    return { satirlar, gen: enGenisSatir(satirlar, fs), yuk: satirlar.length * fs * 1.15 };
  });
  const etiketGen = Math.max(...etiketler2.map((e) => e.gen));
  const r = Math.max(20, Math.min(alanH / 2 - fs * 1.5, W / 2 - P - etiketGen - 26));
  const cx = W / 2;
  const cy = ustY + alanH / 2;
  const c = new Tuval(s);
  const kenar = s.T.ad === 'yayin' ? { renk: '#000000', kalinlik: 0.75 } : { renk: '#ffffff', kalinlik: 1.5 };
  let aci = -Math.PI / 2;
  const dilimler = ogeler.map((o) => {
    const pay = o.n / toplam;
    const a0 = aci;
    const a1 = aci + pay * 2 * Math.PI;
    aci = a1;
    return { o, pay, a0, a1, orta: (a0 + a1) / 2 };
  });
  for (const d of dilimler) {
    const st = seriStili(s, d.o.i);
    const dolgu = c.dolgu(st);
    if (d.pay >= 0.99999) {
      c.daire(cx, cy, r, { dolgu, cizgi: kenar.renk, kalinlik: kenar.kalinlik });
      continue;
    }
    const x0 = cx + r * Math.cos(d.a0);
    const y0 = cy + r * Math.sin(d.a0);
    const x1 = cx + r * Math.cos(d.a1);
    const y1 = cy + r * Math.sin(d.a1);
    const buyuk = d.a1 - d.a0 > Math.PI ? 1 : 0;
    const yol = `M${round2(cx)} ${round2(cy)}L${round2(x0)} ${round2(y0)}A${round2(r)} ${round2(r)} 0 ${buyuk} 1 ${round2(x1)} ${round2(y1)}Z`;
    c.yol(yol, { dolgu, renk: kenar.renk, kalinlik: kenar.kalinlik, birlesim: 'round' });
  }
  // Etiket yerleşimi: sağ ve sol taraf ayrı ayrı yukarıdan aşağı sıralanır, çakışmalar itilir
  const yerlesim = dilimler.map((d, i) => ({
    i,
    sag: Math.cos(d.orta) >= 0,
    ax: cx + r * Math.cos(d.orta),
    ay: cy + r * Math.sin(d.orta),
    bx: cx + (r + 10) * Math.cos(d.orta),
    by: cy + (r + 10) * Math.sin(d.orta),
    y: cy + (r + 10) * Math.sin(d.orta),
    e: etiketler2[i],
  }));
  for (const taraf of [true, false]) {
    const grup = yerlesim.filter((p) => p.sag === taraf).sort((a, b) => a.y - b.y);
    for (let i = 1; i < grup.length; i++) {
      const enAz = grup[i - 1].y + grup[i - 1].e.yuk / 2 + grup[i].e.yuk / 2 + 2;
      if (grup[i].y < enAz) grup[i].y = enAz;
    }
    const altSinir = H - P;
    if (grup.length) {
      const son = grup[grup.length - 1];
      const tasma = son.y + son.e.yuk / 2 - altSinir;
      if (tasma > 0) {
        son.y -= tasma;
        for (let i = grup.length - 2; i >= 0; i--) {
          const enCok = grup[i + 1].y - grup[i + 1].e.yuk / 2 - grup[i].e.yuk / 2 - 2;
          if (grup[i].y > enCok) grup[i].y = enCok;
        }
      }
      const ilk = grup[0];
      const ustTasma = ustY - (ilk.y - ilk.e.yuk / 2);
      if (ustTasma > 0) grup.forEach((p) => (p.y += ustTasma));
    }
  }
  for (const p of yerlesim) {
    const lx = cx + (p.sag ? 1 : -1) * (r + 22);
    c.yol(yolVerisi([[p.ax, p.ay], [p.bx, p.by], [lx + (p.sag ? -4 : 4), p.y]]), { renk: s.T.soluk, kalinlik: 0.7 });
    const y0 = p.y - p.e.yuk / 2 + fs * 0.85;
    c.satirlar(lx, y0, p.e.satirlar, { boyut: fs, hiza: p.sag ? 'bas' : 'son', aralik: fs * 1.15 });
  }
  ust.ciz(c, W - P);
  return c.svg();
}

function round2(v) {
  return Math.round(v * 100) / 100;
}

/* =====================================================================
 * 12. Korelasyon ısı haritası
 * ===================================================================== */

/**
 * Korelasyon ısı haritası: −1…1 ıraksak ölçek (renkli: kırmızı–gri–mavi; yayın: |r| gri tonu), hücrede
 * değer ve anlamlılık yıldızları (* p < 0,05; ** p < 0,01; *** p < 0,001), sağda renk ölçeği.
 * @param {number[][]} r Kare korelasyon matrisi
 * @param {string[]} adlar Değişken adları
 * @param {object} [secenek] Ortak seçenekler ve {p?: number[][] (p değerleri matrisi), ucgen='tam'|'alt'|'ust',
 *   ondalik=2, degerGoster=true, renkOlcegi=true}
 */
export function korelasyonIsiHaritasi(r, adlar, secenek = {}) {
  const s = secenekHazirla(secenek, { ucgen: 'tam', ondalik: 2, degerGoster: true, renkOlcegi: true });
  const R = (Array.isArray(r) ? r : []).map((satir) => (Array.isArray(satir) || ArrayBuffer.isView(satir) ? Array.from(satir) : []));
  const k = R.length;
  if (!k) return bosGrafik(s);
  const P = Array.isArray(s.p) ? s.p : null;
  const ad = adHazirla(adlar, k, 'Değişken', false);
  const W = s.genislik;
  const H = s.yukseklik;
  const pad = s.kenar;
  const fs = s.fs.tik;
  const ust = ustBolumOlc(s, null, []);
  const ustY = pad + ust.yukseklik + (ust.yukseklik ? 10 : 0);
  const esik = (v, d) => basSifirUygula(sayiBicimle(v, d, s), s);
  const dipnot = P ? `* p < ${esik(0.05, 2)}; ** p < ${esik(0.01, 2)}; *** p < ${esik(0.001, 3)}` : '';
  const dipnotH = dipnot ? fs * 1.6 : 0;
  // Satır adları en çok 2 satıra sarılır (hücre yeterince yüksekse); sığmayan kısım '…' ile kısalır
  const solMaks = W * 0.24;
  const solAdHazirla = (iki) => ad.map((a) => metniSar(a, solMaks, fs, { maksSatir: iki ? 2 : 1 }));
  let solAd = solAdHazirla(true);
  const solW = Math.max(...solAd.map((sat) => enGenisSatir(sat, fs))) + 8;
  const olcekGen = s.renkOlcegi ? 14 + 6 + metinGenisligi(sayiBicimle(-0.5, 1, s), fs) + 16 : 0;
  // Alt etiketler önce döndürülmüş varsayılır; hücre yeterince genişse yataya geçilir.
  let altH = 0;
  let hucre = 0;
  let mod = 'donuk';
  // Sütun adları: uzunsa kısaltmadan önce iki satıra sarılır (ayırt edici son ekler kaybolmasın)
  const altSinir = Math.max(60, H * 0.26);
  const altAd = ad.map((a) => {
    const w = metinGenisligi(a, fs);
    return w <= altSinir ? [a] : metniSar(a, Math.min(altSinir, Math.max(altSinir * 0.55, w * 0.58)), fs, { maksSatir: 2 });
  });
  const enUzun = Math.max(...altAd.map((sat) => enGenisSatir(sat, fs)));
  const altSatir = Math.max(...altAd.map((sat) => sat.length));
  const tekSatir = ad.map((a) => [a]);
  for (let tur = 0; tur < 2; tur++) {
    altH = mod === 'yatay' ? fs * 1.6 : (enUzun + (altSatir - 1) * fs * 1.1) * Math.SQRT1_2 + fs + 8;
    const kullanW = W - 2 * pad - solW - olcekGen;
    const kullanH = H - ustY - pad - dipnotH - altH;
    hucre = Math.max(4, Math.min(kullanW / k, kullanH / k));
    if (tekSatir.every(([a]) => metinGenisligi(a, fs) + 4 <= hucre)) mod = 'yatay';
    else break;
  }
  if (hucre < fs * 2.4) solAd = solAdHazirla(false);
  const izgaraGen = hucre * k;
  const x0 = pad + solW + (W - 2 * pad - solW - olcekGen - izgaraGen) / 2;
  const y0 = ustY;
  const c = new Tuval(s);
  const ornek = `−0${s.ondalikAyrac}${'0'.repeat(s.ondalik)}${P ? '***' : ''}`;
  const fsHucre = Math.min(fs, (hucre - 4) / Math.max(1e-9, metinGenisligi(ornek, 1)), hucre * 0.42);
  const gorunur = (i, j) => s.ucgen === 'tam' || (s.ucgen === 'alt' ? j <= i : j >= i);
  for (let i = 0; i < k; i++) {
    for (let j = 0; j < k; j++) {
      if (!gorunur(i, j)) continue;
      const v = R[i][j];
      const sonlu = typeof v === 'number' && Number.isFinite(v);
      const dolgu = sonlu ? iraksakRenk(s, v) : '#ffffff';
      const x = x0 + j * hucre;
      const y = y0 + i * hucre;
      c.dikdortgen(x, y, hucre, hucre, { dolgu, cizgi: '#ffffff', kalinlik: hucre > 12 ? 1.5 : 0.5 });
      if (!s.degerGoster || fsHucre < 6.5) continue;
      let t;
      if (!sonlu) t = '–';
      else if (i === j && Math.abs(v - 1) < 1e-12) t = '1';
      else t = basSifirUygula(sayiBicimle(v, s.ondalik, s), s) + (P && i !== j ? yildizlar(P[i]?.[j]) : '');
      c.metin(x + hucre / 2, y + hucre / 2 + fsHucre * 0.35, t, { boyut: fsHucre, hiza: 'orta', renk: metinRengi(dolgu) });
    }
  }
  // Satır adları (sol) ve sütun adları (alt)
  solAd.forEach((sat, i) => {
    const y = y0 + (i + 0.5) * hucre - ((sat.length - 1) * fs * 1.1) / 2 + fs * 0.35;
    c.satirlar(x0 - 6, y, sat, { boyut: fs, hiza: 'son', renk: s.T.ikincil, aralik: fs * 1.1 });
  });
  const altY = y0 + izgaraGen + 6;
  const adim = mod === 'yatay' ? 1 : Math.max(1, Math.ceil((fs * 1.1) / (hucre * Math.SQRT1_2)));
  (mod === 'yatay' ? tekSatir : altAd).forEach((sat, j) => {
    const x = x0 + (j + 0.5) * hucre;
    if (mod === 'yatay') {
      c.metin(x, altY + fs * 0.85, sat[0], { boyut: fs, hiza: 'orta', renk: s.T.ikincil });
      return;
    }
    if (j % adim !== 0) return;
    // Döndürülmüş satırlar metne dik yönde istiflenir (eksen etiketleriyle aynı yöntem)
    sat.forEach((t, i) => {
      const d = (i - (sat.length - 1) / 2) * fs * 1.1 * Math.SQRT1_2;
      const ek = ((sat.length - 1) / 2) * fs * 1.1 * Math.SQRT1_2;
      c.metin(x + fs * 0.3 + d, altY + fs * 0.3 + d + ek, t, { boyut: fs, hiza: 'son', renk: s.T.ikincil, dondur: -45 });
    });
  });
  // Renk ölçeği
  if (s.renkOlcegi) {
    const ox = x0 + izgaraGen + 16;
    const oh = Math.min(izgaraGen, 220);
    const oy = y0 + (izgaraGen - oh) / 2;
    const parca = 40;
    for (let i = 0; i < parca; i++) {
      const v = 1 - (2 * (i + 0.5)) / parca;
      c.dikdortgen(ox, oy + (i * oh) / parca, 14, oh / parca + 0.4, { dolgu: iraksakRenk(s, v) });
    }
    c.dikdortgen(ox, oy, 14, oh, { cizgi: s.T.eksen, kalinlik: 0.6 });
    const olcek = dogrusalOlcek(1, -1, oy, oy + oh);
    for (const v of [1, 0.5, 0, -0.5, -1]) {
      const y = olcek(v);
      c.cizgi(ox + 14, y, ox + 18, y, { renk: s.T.eksen, kalinlik: 0.6 });
      c.metin(ox + 21, y + fs * 0.35, sayiBicimle(v, v === Math.round(v) ? 0 : 1, s), { boyut: fs, renk: s.T.ikincil });
    }
  }
  if (dipnot) c.metin(pad, H - pad, simgeleriItalikYap(dipnot), { boyut: s.fs.kucuk, renk: s.T.ikincil });
  ust.ciz(c, W - pad);
  return c.svg();
}

/* =====================================================================
 * 13. Orman (forest) grafiği
 * ===================================================================== */

/**
 * Orman grafiği: her satırda nokta tahmini (kare) ve güven aralığı, solda adlar, sağda "tahmin (alt–üst)"
 * ve isteğe bağlı p sütunu; referans çizgisi (log ölçekte 1, doğrusalda 0).
 * Log ölçek istenip pozitif olmayan değer varsa doğrusal ölçeğe geçilir.
 * @param {Array<{ad:string, tahmin?:number, alt?:number, ust?:number, p?:number, agirlik?:number,
 *   ozet?:boolean (elmas), baslik?:boolean (yalnızca kalın ad)}>} satirlar
 * @param {object} [secenek] Ortak seçenekler ve {logOlcek=true, referans?: number, etiket='OR (%95 GA)',
 *   solBaslik='', ondalik=2, yonEtiketleri?: [solYazi, sagYazi]}
 */
export function ormanGrafigi(satirlar, secenek = {}) {
  const ayr = secenek.ondalikAyrac === '.' ? '.' : ',';
  const s = secenekHazirla(secenek, { logOlcek: true, etiket: ayr === ',' ? 'OR (%95 GA)' : 'OR (95% GA)', solBaslik: '', ondalik: 2 });
  const R = (Array.isArray(satirlar) ? satirlar : [])
    .filter(Boolean)
    .map((r, i) => ({
      ad: r.ad != null ? String(r.ad).trim() : `Satır ${i + 1}`,
      // Alt grup girintisi: baştaki boşluklar ya da girinti düzeyi (SVG baştaki boşlukları yutar)
      girinti: (r.ad != null ? String(r.ad).match(/^\s*/)[0].length * 0.3 : 0) + (Number.isFinite(r.girinti) ? r.girinti : 0),
      tahmin: Number(r.tahmin),
      alt: Number(r.alt),
      ust: Number(r.ust),
      p: Number(r.p),
      agirlik: Number(r.agirlik),
      ozet: !!r.ozet,
      baslik: !!r.baslik,
    }));
  if (!R.length) return bosGrafik(s);
  R.forEach((r) => {
    r.var = !r.baslik && Number.isFinite(r.tahmin);
    r.gaVar = r.var && Number.isFinite(r.alt) && Number.isFinite(r.ust);
  });
  const degerler = R.filter((r) => r.var).flatMap((r) => (r.gaVar ? [r.tahmin, r.alt, r.ust] : [r.tahmin]));
  const log = !!s.logOlcek && degerler.length > 0 && degerler.every((v) => v > 0);
  const referans = Number.isFinite(s.referans) ? s.referans : log ? 1 : 0;
  const refGecerli = !log || referans > 0;
  const W = s.genislik;
  const pad = s.kenar;
  const fs = s.fs.tik;
  const f = (v) => sayiBicimle(v, s.ondalik, s);
  const tahminMetni = R.map((r) => (!r.var ? '' : r.gaVar ? `${f(r.tahmin)} (${f(Math.min(r.alt, r.ust))}–${f(Math.max(r.alt, r.ust))})` : f(r.tahmin)));
  const pVar = R.some((r) => Number.isFinite(r.p));
  const pMetni = R.map((r) => (Number.isFinite(r.p) ? pBicimle(r.p, s) : ''));
  const adW = Math.min(W * 0.32, Math.max(metinGenisligi(s.solBaslik, fs, true), ...R.map((r) => r.girinti * fs + metinGenisligi(r.ad, fs, r.baslik || r.ozet))));
  const tahminW = Math.max(metinGenisligi(s.etiket, fs, true), ...tahminMetni.map((t) => metinGenisligi(t, fs)));
  const pW = pVar ? Math.max(metinGenisligi('p', fs, true), ...pMetni.map((t) => metinGenisligi(t, fs))) : 0;
  const ust = ustBolumOlc(s, null, []);
  const basY = pad + ust.yukseklik + (ust.yukseklik ? 10 : 0);
  const sutunBaslikH = fs * 1.6;
  const xBaslik = s.xEtiket ? metniSar(s.xEtiket, W - 2 * pad, s.fs.eksen, { maksSatir: 2 }) : [];
  const yon = Array.isArray(s.yonEtiketleri) && s.yonEtiketleri.length === 2 ? s.yonEtiketleri.map(String) : null;
  const eksenH = 5 + 4 + fs + 4 + (yon ? s.fs.kucuk * 1.4 : 0) + (xBaslik.length ? xBaslik.length * s.fs.eksen * 1.2 + 4 : 0);
  const satirUst = basY + sutunBaslikH + 4;
  // Yükseklik verilmemişse satır sayısına göre hesaplanır (satır başına ~2,3 satır yüksekliği)
  if (secenek.yukseklik === undefined) s.yukseklik = Math.max(160, Math.round(satirUst + R.length * fs * 2.3 + eksenH + pad));
  const H = s.yukseklik;
  const satirAlt = H - pad - eksenH;
  const satirH = Math.max(6, (satirAlt - satirUst) / R.length);
  const fsSatir = Math.min(fs, satirH / 1.25);
  const sol = pad + adW + 14;
  const sag = W - pad - tahminW - 16 - (pVar ? pW + 14 : 0);
  // x ekseni
  const eksenDeg = [...degerler, ...(refGecerli ? [referans] : [])];
  const [dmin, dmax] = sinirlar(eksenDeg);
  const eksen = sayisalEksenHazirla(s, { tur: 'sayisal', min: dmin, max: dmax, log }, 'x', Math.max(40, sag - sol));
  const sx = (eksen.log ? logOlcek : dogrusalOlcek)(eksen.alan[0], eksen.alan[1], sol, sag);
  const c = new Tuval(s);
  // Sütun başlıkları
  const bY = basY + fs;
  if (s.solBaslik) c.metin(pad, bY, s.solBaslik, { boyut: fs, kalin: true });
  c.metin(W - pad - (pVar ? pW + 14 : 0), bY, s.etiket, { boyut: fs, kalin: true, hiza: 'son' });
  if (pVar) c.metin(W - pad, bY, [{ t: 'p', italik: true, kalin: true }], { boyut: fs, kalin: true, hiza: 'son' });
  c.cizgi(pad, basY + sutunBaslikH, W - pad, basY + sutunBaslikH, { renk: s.T.eksen, kalinlik: 0.6 });
  // Referans çizgisi
  if (refGecerli && referans >= eksen.alan[0] && referans <= eksen.alan[1]) {
    c.cizgi(sx(referans), satirUst - 2, sx(referans), satirAlt, { renk: s.T.referans, kalinlik: 0.9, kesik: '4 3' });
  }
  const isaretRenk = s.T.ad === 'renkli' ? (Array.isArray(s.renkler) && s.renkler[0]) || '#1c5cab' : '#000000';
  const maksAgirlik = Math.max(0, ...R.filter((r) => r.var && r.agirlik > 0).map((r) => r.agirlik));
  R.forEach((r, i) => {
    const ym = satirUst + (i + 0.5) * satirH;
    const kalin = r.baslik || r.ozet;
    const gx = r.girinti * fsSatir;
    c.metin(pad + gx, ym + fsSatir * 0.35, kisalt(r.ad, adW - gx, fsSatir, kalin), { boyut: fsSatir, kalin });
    if (!r.var) return;
    c.metin(W - pad - (pVar ? pW + 14 : 0), ym + fsSatir * 0.35, tahminMetni[i], { boyut: fsSatir, hiza: 'son', renk: s.T.murekkep });
    if (pMetni[i]) c.metin(W - pad, ym + fsSatir * 0.35, pMetni[i], { boyut: fsSatir, hiza: 'son', renk: s.T.murekkep });
    const xt = sx(r.tahmin);
    if (r.ozet && r.gaVar) {
      const h = Math.min(satirH * 0.35, 7);
      c.yol(yolVerisi([[sx(Math.min(r.alt, r.ust)), ym], [xt, ym - h], [sx(Math.max(r.alt, r.ust)), ym], [xt, ym + h]], true), { dolgu: isaretRenk, renk: isaretRenk, kalinlik: 0.8 });
      return;
    }
    if (r.gaVar) c.cizgi(sx(Math.min(r.alt, r.ust)), ym, sx(Math.max(r.alt, r.ust)), ym, { renk: s.T.murekkep, kalinlik: 1.2 });
    const boyut = maksAgirlik > 0 && r.agirlik > 0 ? 4 + 9 * Math.sqrt(r.agirlik / maksAgirlik) : 8;
    const kenar = Math.min(boyut, satirH * 0.8);
    c.dikdortgen(xt - kenar / 2, ym - kenar / 2, kenar, kenar, { dolgu: isaretRenk });
  });
  // Eksen
  const ek = { renk: s.T.eksen, kalinlik: s.T.eksenKalinlik };
  c.cizgi(sol, satirAlt, sag, satirAlt, ek);
  eksen.adimlar.forEach((v, i) => {
    const x = sx(v);
    c.cizgi(x, satirAlt, x, satirAlt + 5, ek);
    if (eksen.goster[i]) c.metin(x, satirAlt + 9 + fs * 0.8, eksen.etiketler[i], { boyut: fs, hiza: 'orta', renk: s.T.ikincil });
  });
  let y = satirAlt + 9 + fs + 4;
  if (yon && refGecerli) {
    const xr = sinirla(sx(referans), sol, sag);
    c.metin(xr - 6, y + s.fs.kucuk * 0.9, `← ${yon[0]}`, { boyut: s.fs.kucuk, hiza: 'son', renk: s.T.ikincil });
    c.metin(xr + 6, y + s.fs.kucuk * 0.9, `${yon[1]} →`, { boyut: s.fs.kucuk, renk: s.T.ikincil });
    y += s.fs.kucuk * 1.4;
  }
  if (xBaslik.length) c.satirlar((sol + sag) / 2, y + s.fs.eksen, xBaslik, { boyut: s.fs.eksen, hiza: 'orta' });
  ust.ciz(c, W - pad);
  return c.svg();
}

/* =====================================================================
 * 14. Yamaç (scree) grafiği
 * ===================================================================== */

/**
 * Yamaç grafiği: bileşen numarasına karşı özdeğerler, Kaiser ölçütü (özdeğer = 1) çizgisi ve isteğe bağlı
 * paralel analiz eğrisi.
 * @param {number[]} ozdegerler Büyükten küçüğe özdeğerler
 * @param {object} [secenek] Ortak seçenekler ve {paralelAnaliz?: number[] (rastgele veri özdeğerleri; ör. %95
 *   yüzdelik), kaiser=true}
 */
export function yamacGrafigi(ozdegerler, secenek = {}) {
  const s = secenekHazirla(secenek, { xEtiket: 'Bileşen numarası', yEtiket: 'Özdeğer', kaiser: true });
  const ev = (Array.isArray(ozdegerler) || ArrayBuffer.isView(ozdegerler) ? Array.from(ozdegerler) : []).map((v) => (Number.isFinite(v) ? v : NaN));
  const k = ev.length;
  if (!k || ev.every((v) => !Number.isFinite(v))) return bosGrafik(s);
  const pa = Array.isArray(s.paralelAnaliz) || ArrayBuffer.isView(s.paralelAnaliz) ? Array.from(s.paralelAnaliz).slice(0, Math.max(k, 0)).map((v) => (Number.isFinite(v) ? v : NaN)) : null;
  const tikler = k <= 20 ? Array.from({ length: k }, (_, i) => i + 1) : guzelAdimlar(1, k, 8, { tamsayi: true }).adimlar.filter((v) => v >= 1 && v <= k);
  if (k > 20 && !tikler.includes(1)) tikler.unshift(1);
  const yDeg = [...ev, ...(pa || []), ...(s.kaiser ? [1] : [])];
  const [ymin, ymax] = sinirlar(yDeg);
  const kaiserMetni = `Özdeğer = 1`;
  const sagEk = s.kaiser ? metinGenisligi(kaiserMetni, s.fs.kucuk) + 10 : 0;
  const st0 = { ...seriStili(s, 0), isaretDolu: true, sekil: 'daire', kesik: '' };
  const st1 = { ...seriStili(s, 1), isaretDolu: false, sekil: 'ucgen', kesik: '5 3' };
  const gosterge = pa ? [{ ad: 'Gözlenen özdeğerler', tur: 'cizgi', stil: st0 }, { ad: 'Paralel analiz', tur: 'cizgi', stil: st1 }] : null;
  const plan = kartezyenPlanla(s, {
    x: { tur: 'sayisal', min: 0.5, max: k + 0.5, sabitAlan: [0.5, k + 0.5], adimlar: tikler },
    y: { tur: 'sayisal', min: Math.min(0, ymin), max: ymax, sifirDahil: true },
    gosterge,
    sagEk,
  });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  if (s.kaiser && 1 >= plan.y.alan[0] && 1 <= plan.y.alan[1]) {
    c.cizgi(plan.sol, plan.sy(1), plan.sag, plan.sy(1), { renk: s.T.referans, kalinlik: 0.9, kesik: '4 3' });
    c.metin(plan.sag + 5, plan.sy(1) + s.fs.kucuk * 0.35, kaiserMetni, { boyut: s.fs.kucuk, renk: s.T.ikincil });
  }
  const seriCiz = (degerler, st, r) => {
    let parca = [];
    const bitir = () => {
      if (parca.length > 1) c.yol(yolVerisi(parca), { renk: st.renk, kalinlik: st.kalinlik, kesik: st.kesik });
      parca = [];
    };
    degerler.forEach((v, i) => (Number.isFinite(v) ? parca.push([plan.sx(i + 1), plan.sy(v)]) : bitir()));
    bitir();
    const isaretR = k > 40 ? Math.min(r, 2.5) : r;
    degerler.forEach((v, i) => Number.isFinite(v) && c.seriIsareti(plan.sx(i + 1), plan.sy(v), st, isaretR));
  };
  if (pa) seriCiz(pa, st1, 3.8);
  seriCiz(ev, st0, 4);
  plan.cerceveCiz(c);
  return c.svg();
}

/* =====================================================================
 * 15. Nokta (strip / arı sürüsü) grafiği
 * ===================================================================== */

/**
 * Arı sürüsü yerleşimi: her nokta, üst üste binmeyeceği en merkeze yakın yatay konuma yerleştirilir.
 * ys: piksel y değerleri; r: yarıçap; yariGen: izin verilen en büyük yatay sapma. Döndürür: x kaymaları.
 */
export function suruYerlesimi(ys, r, yariGen) {
  const n = ys.length;
  const sonuc = new Array(n).fill(0);
  if (n > 2000) {
    const rng = tohumluRastgele(n);
    return sonuc.map(() => (rng() - 0.5) * 2 * yariGen);
  }
  const d = 2 * r + 0.6;
  const sira = ys.map((y, i) => [y, i]).sort((a, b) => a[0] - b[0]);
  const yer = [];
  for (const [y, i] of sira) {
    const yakin = [];
    for (let j = yer.length - 1; j >= 0 && y - yer[j].y < d; j--) yakin.push(yer[j]);
    const adaylar = [0];
    for (const p of yakin) {
      const dx = Math.sqrt(Math.max(0, d * d - (p.y - y) ** 2));
      adaylar.push(p.x - dx, p.x + dx);
    }
    adaylar.sort((a, b) => Math.abs(a) - Math.abs(b) || a - b);
    let x = adaylar.find((cx) => yakin.every((p) => (p.x - cx) ** 2 + (p.y - y) ** 2 >= d * d - 1e-6));
    if (x === undefined) x = 0;
    yer.push({ x, y });
    sonuc[i] = x;
  }
  const maks = Math.max(0, ...sonuc.map(Math.abs));
  if (maks > yariGen && maks > 0) return sonuc.map((x) => (x * yariGen) / maks);
  return sonuc;
}

/**
 * Bireysel değerler (küçük örneklemler için): her grupta arı sürüsü noktaları, ortalama ya da ortanca çizgisi,
 * isteğe bağlı hata çubuğu.
 * @param {number[][]|number[]} gruplar
 * @param {string[]} [adlar]
 * @param {object} [secenek] Ortak seçenekler ve {merkez='ortalama'|'medyan'|'yok', hata?: 'ss'|'sh'|'ga95'
 *   (yalnız ortalama ile), duzey=0.95, noktaBoyutu?: number, aciklamaNotu=true}
 */
export function noktaGrafigi(gruplar, adlar, secenek = {}) {
  const s = secenekHazirla(secenek, { merkez: 'ortalama', hata: undefined, duzey: 0.95, aciklamaNotu: true });
  const G = grupla(gruplar);
  if (!G.length || G.every((g) => !g.length)) return bosGrafik(s);
  const ad = adHazirla(adlar, G.length);
  const merkezler = G.map((g) => {
    if (!g.length || s.merkez === 'yok') return null;
    if (s.merkez === 'medyan') return { m: kutuIstatistikleri(g).medyan };
    const o = s.hata ? ortalamaVeHata(g, s.hata, s.duzey) : { ortalama: ortalama(g) };
    return { m: o.ortalama, alt: o.alt, ust: o.ust };
  });
  const yDeg = [...G.flat(), ...merkezler.flatMap((m) => (m ? [m.m, m.alt, m.ust] : []))];
  const [ymin, ymax] = sinirlar(yDeg);
  const notlar = [];
  if (s.aciklamaNotu && s.merkez !== 'yok') {
    let t = `Yatay çizgi: ${s.merkez === 'medyan' ? 'ortanca' : 'ortalama'}.`;
    if (s.hata && s.merkez !== 'medyan') t += ' ' + hataAciklamasi(s.hata, s.duzey, s);
    notlar.push(t);
  }
  const plan = kartezyenPlanla(s, { x: { tur: 'kategorik', adlar: ad }, y: { tur: 'sayisal', min: ymin, max: ymax }, altNotlar: notlar });
  const c = new Tuval(s);
  plan.izgaraCiz(c);
  const maksN = Math.max(...G.map((g) => g.length));
  const r = s.noktaBoyutu ?? (maksN > 200 ? 2.2 : maksN > 60 ? 2.8 : 3.4);
  const yariGen = plan.xBant * 0.38;
  G.forEach((g, i) => {
    if (!g.length) return;
    const cx = plan.sx(i);
    const ys = g.map((v) => plan.sy(v));
    const dx = suruYerlesimi(ys, r, yariGen);
    const m = merkezler[i];
    const merkezVar = m && Number.isFinite(m.m);
    const w = Math.min(plan.xBant * 0.55, 64);
    // Sıra: hata çubuğu → noktalar → merkez çizgisi (noktalar ince çubuğun üstünde kalır)
    if (merkezVar && Number.isFinite(m.alt) && Number.isFinite(m.ust)) {
      hataCubugu(c, cx, plan.sy(m.alt), plan.sy(m.ust), Math.min(w * 0.35, 14), { renk: s.T.murekkep, kalinlik: 1.2 });
    }
    noktalariCiz(c, s, ys.map((y, j) => [cx + dx[j], y]), g.length, r);
    if (merkezVar) c.cizgi(cx - w / 2, plan.sy(m.m), cx + w / 2, plan.sy(m.m), { renk: s.T.murekkep, kalinlik: 2.2 });
  });
  plan.cerceveCiz(c);
  return c.svg();
}

/** noktaGrafigi ile aynı (strip grafiği adıyla) */
export const stripGrafigi = noktaGrafigi;
