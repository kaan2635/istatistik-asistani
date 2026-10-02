// Genel doğrusal model (tek bağımlı değişken): faktöriyel ANOVA ve ANCOVA.
// SPSS UNIANOVA / GLM "Tests of Between-Subjects Effects" tablosunun karşılığıdır.
//
// Parametrelendirme: faktörler toplamı sıfır (R contr.sum) kodlanır, kovaryatlar ham (merkezlenmemiş) girer.
// Bu kodlamada Tip III kareler toplamı SPSS'in Tip III'üyle ve R'de
// car::Anova(lm(..., contrasts = contr.sum), type = 3) ile aynıdır.
// Tip I/II kareler toplamları, sütunları istenen sırada işleyen Householder QR'ın "etki" vektöründen
// (Q'y) hesaplanır; model karşılaştırmasındaki çıkarma işleminden doğan hassasiyet kaybı olmaz.
// Tip III tek bir QR'dan Wald biçimiyle (b_T' [(X'X)^-1_TT]^-1 b_T) hesaplanır; tam ranklı modelde
// "terimi çıkar ve karşılaştır" ile matematiksel olarak aynıdır (car::linearHypothesis'in yaptığı da budur).

import { enKucukKareler, simetrikTers } from './matris.js';
import { pf, pt, qt } from './dagilim.js';
import { ortalama, standartSapma } from './betimsel.js';
import { IstatistikHatasi, gerekli } from './hata.js';

export const AYRAC = ' * ';

export const DUZELTME_YONTEMLERI = ['bonferroni', 'sidak', 'yok'];

// ---------------------------------------------------------------------------
// Ortak yardımcılar (tekrarli.js de kullanır)
// ---------------------------------------------------------------------------

/** Etiket eksik mi? (null, undefined, NaN, boş metin) */
export function etiketEksik(v) {
  if (v === null || v === undefined) return true;
  if (typeof v === 'number') return !Number.isFinite(v);
  if (typeof v === 'string') return v.trim() === '';
  return false;
}

/** Sonlu sayı mı? */
export function sayiMi(v) {
  return typeof v === 'number' && Number.isFinite(v);
}

/**
 * Faktör düzeylerini belirler. Varsayılan sıra SPSS gibi artan değer sırasıdır
 * (tüm etiketler sayısalsa sayısal, değilse Türkçe alfabetik). `sira` verilirse önce o sıra,
 * sonra listede olmayan gözlenmiş düzeyler (artan sırada) gelir.
 * @param {Array} etiketler eksikleri ayıklanmış etiketler
 * @param {Array} [sira] istenen düzey sırası
 * @returns {string[]}
 */
export function duzeyleriBelirle(etiketler, sira) {
  const tekil = [...new Set(etiketler.map((e) => String(e)))];
  const sayisal = tekil.every((e) => e.trim() !== '' && Number.isFinite(Number(e)));
  tekil.sort(sayisal ? (a, b) => Number(a) - Number(b) || a.localeCompare(b) : (a, b) => a.localeCompare(b, 'tr'));
  if (!sira) return tekil;
  const istenen = [];
  for (const s of sira) {
    const k = String(s);
    if (tekil.includes(k) && !istenen.includes(k)) istenen.push(k);
  }
  return istenen.concat(tekil.filter((k) => !istenen.includes(k)));
}

/**
 * Sütunları verilen sırada işleyen Householder QR. Önceki sütunların doğrusal bileşimi olan sütun
 * (kalan normu ilk normunun `tol` katından küçük) atlanır ve etkisi null olur (R lm()'deki dqrdc2 gibi).
 * `katsayiDondur` true ise kullanılan sütunlar için katsayılar ve (X'X)^-1 = R^-1 R^-T de döndürülür
 * (atlanan sütunlarda null; enKucukKareler ile aynı biçim).
 * @param {Float64Array[]} sutunlar sütun dizileri
 * @param {ArrayLike<number>} y
 * @returns {{etki:(number|null)[], rank:number, kalanKT:number, katsayi?:(number|null)[], XtXTers?:(number|null)[][]}}
 *   etki[j]: j. sütunun Q'y bileşeni
 */
export function ardisikQR(sutunlar, y, tol = 1e-7, katsayiDondur = false) {
  const n = y.length;
  const p = sutunlar.length;
  const A = sutunlar.map((s) => Float64Array.from(s));
  const b = Float64Array.from(y);
  const etki = new Array(p).fill(null);
  const Rsutun = [];
  let r = 0;
  for (let j = 0; j < p && r < n; j++) {
    const a = A[j];
    let ilk = 0;
    const kaynak = sutunlar[j];
    for (let i = 0; i < n; i++) ilk += kaynak[i] * kaynak[i];
    let s = 0;
    for (let i = r; i < n; i++) s += a[i] * a[i];
    const norm = Math.sqrt(s);
    if (norm === 0 || norm < tol * Math.sqrt(ilk)) continue;
    const alfa = a[r] > 0 ? -norm : norm;
    const v0 = a[r] - alfa;
    const vn2 = s - a[r] * a[r] + v0 * v0;
    const yansit = (x) => {
      let d = v0 * x[r];
      for (let i = r + 1; i < n; i++) d += a[i] * x[i];
      const f = (2 * d) / vn2;
      x[r] -= f * v0;
      for (let i = r + 1; i < n; i++) x[i] -= f * a[i];
    };
    for (let m = j + 1; m < p; m++) yansit(A[m]);
    yansit(b);
    etki[j] = b[r];
    if (katsayiDondur) {
      const kol = new Float64Array(r + 1);
      for (let i = 0; i < r; i++) kol[i] = a[i];
      kol[r] = alfa;
      Rsutun.push({ j, kol });
    }
    r++;
  }
  let kalanKT = 0;
  for (let i = r; i < n; i++) kalanKT += b[i] * b[i];
  if (!katsayiDondur) return { etki, rank: r, kalanKT };
  // R β = Q'y ve R^-1 (geri yerine koyma)
  const Ri = Array.from({ length: r }, () => new Float64Array(r));
  for (let i = r - 1; i >= 0; i--) {
    Ri[i][i] = 1 / Rsutun[i].kol[i];
    for (let k = i + 1; k < r; k++) {
      let s = 0;
      for (let m = i + 1; m <= k; m++) s -= Rsutun[m].kol[i] * Ri[m][k];
      Ri[i][k] = s / Rsutun[i].kol[i];
    }
  }
  const katsayi = new Array(p).fill(null);
  const XtXTers = Array.from({ length: p }, () => new Array(p).fill(null));
  for (let i = 0; i < r; i++) {
    let s = 0;
    for (let k = i; k < r; k++) s += Ri[i][k] * etki[Rsutun[k].j];
    katsayi[Rsutun[i].j] = s;
    for (let k = 0; k <= i; k++) {
      let v = 0;
      for (let m = i; m < r; m++) v += Ri[i][m] * Ri[k][m];
      XtXTers[Rsutun[i].j][Rsutun[k].j] = v;
      XtXTers[Rsutun[k].j][Rsutun[i].j] = v;
    }
  }
  return { etki, rank: r, kalanKT, katsayi, XtXTers };
}

/** Tip III (Wald) kareler toplamı: b_T' [(X'X)^-1_TT]^-1 b_T (car::linearHypothesis ile aynı) */
function waldKT(qr, sutunlar) {
  const b = sutunlar.map((j) => qr.katsayi[j]);
  const V = sutunlar.map((i) => sutunlar.map((j) => qr.XtXTers[i][j]));
  return kuadratik(b, simetrikTers(V));
}

/** v' A v */
export function kuadratik(v, A) {
  let t = 0;
  for (let i = 0; i < v.length; i++) {
    if (v[i] === 0) continue;
    let s = 0;
    for (let j = 0; j < v.length; j++) s += A[i][j] * v[j];
    t += v[i] * s;
  }
  return t;
}

/** u' A v */
export function ikiliForm(u, A, v) {
  let t = 0;
  for (let i = 0; i < u.length; i++) {
    if (u[i] === 0) continue;
    let s = 0;
    for (let j = 0; j < v.length; j++) s += A[i][j] * v[j];
    t += u[i] * s;
  }
  return t;
}

/** Šidák düzeltmesi 1-(1-p)^m, küçük p'de hassas biçimde */
function sidakP(p, m) {
  return Math.min(1, -Math.expm1(m * Math.log1p(-p)));
}

/**
 * Çoklu karşılaştırma düzeltmesi uygulanmış p ve iki yönlü güven aralığı için kritik t.
 * SPSS EMMEANS COMPARE ADJ(...) ve emmeans::pairs(adjust = ...) ile aynıdır:
 * bonferroni: p·m, t_{α/(2m)}; sidak: 1-(1-p)^m, t_{(1-(1-α)^{1/m})/2}; yok (LSD): düzeltmesiz.
 */
export function duzeltilmisKarsilastirma(t, sd, m, yontem, duzey) {
  const pHam = Math.min(1, 2 * pt(-Math.abs(t), sd));
  const alfa = 1 - duzey;
  let p = pHam;
  let a = alfa;
  if (m > 1 && yontem === 'bonferroni') {
    p = Math.min(1, pHam * m);
    a = alfa / m;
  } else if (m > 1 && yontem === 'sidak') {
    p = sidakP(pHam, m);
    a = -Math.expm1(Math.log1p(-alfa) / m);
  }
  return { p, pHam, tKritik: qt(a / 2, sd, false) };
}

export function duzeltmeDogrula(duzeltme) {
  gerekli(DUZELTME_YONTEMLERI.includes(duzeltme), `Bilinmeyen çoklu karşılaştırma düzeltmesi: ${duzeltme}. Seçenekler: bonferroni, sidak, yok.`);
}

export function duzeyDogrula(duzey) {
  gerekli(typeof duzey === 'number' && duzey > 0 && duzey < 1, 'Güven düzeyi 0 ile 1 arasında olmalıdır (ör. 0,95).');
}

/** Tüm düzey birleşimlerini üretir (ilk faktör en yavaş değişir) */
export function izgara(boyutlar) {
  const sonuc = [];
  const n = boyutlar.length;
  const idx = new Array(n).fill(0);
  if (boyutlar.some((k) => k === 0)) return sonuc;
  for (;;) {
    sonuc.push(idx.slice());
    let j = n - 1;
    while (j >= 0) {
      idx[j]++;
      if (idx[j] < boyutlar[j]) break;
      idx[j] = 0;
      j--;
    }
    if (j < 0) break;
  }
  return sonuc;
}

// contr.sum kodlaması: l < k-1 için e_l, son düzey için -1 vektörü
function toplamKodu(l, k) {
  const v = new Array(k - 1).fill(0);
  if (l === k - 1) v.fill(-1);
  else v[l] = 1;
  return v;
}

function terimAdi(adlar) {
  return adlar.join(AYRAC);
}

// ---------------------------------------------------------------------------
// Model kurulumu
// ---------------------------------------------------------------------------

// SPSS kapsama kuralı (GLM algoritmaları, Tip II): F1, F2'nin içinde ise (1) ikisi de aynı kovaryat
// etkisini içerir (ya da hiç içermez), (2) F2'de F1'den çok faktör vardır, (3) F1'in tüm faktörleri F2'de vardır.
// Kesişim tüm saf faktör etkilerinin içindedir; kovaryat içeren hiçbir etkinin içinde değildir.
function icerir(buyuk, kucuk) {
  if (kucuk.kesisim) return !buyuk.kesisim && buyuk.kovIdx.length === 0;
  if (buyuk.kesisim) return false;
  if (buyuk.kovIdx.length !== kucuk.kovIdx.length || buyuk.kovIdx.some((c, i) => c !== kucuk.kovIdx[i])) return false;
  if (buyuk.faktorIdx.length <= kucuk.faktorIdx.length) return false;
  return kucuk.faktorIdx.every((f) => buyuk.faktorIdx.includes(f));
}

function terimleriKur(faktorAdlari, kovAdlari, etkilesimler) {
  const terimler = [];
  const anahtarlar = new Set();
  const ekle = (faktorIdx, kovIdx, ad) => {
    const fi = faktorIdx.slice().sort((a, b) => a - b);
    const ci = kovIdx.slice().sort((a, b) => a - b);
    const anahtar = `${fi.join(',')}|${ci.join(',')}`;
    if (anahtarlar.has(anahtar)) return;
    anahtarlar.add(anahtar);
    terimler.push({ ad, faktorIdx: fi, kovIdx: ci });
  };
  kovAdlari.forEach((ad, c) => ekle([], [c], ad));
  faktorAdlari.forEach((ad, f) => ekle([f], [], ad));
  if (etkilesimler === 'tam') {
    const F = faktorAdlari.length;
    for (let boy = 2; boy <= F; boy++) {
      const secim = [];
      const uret = (bas) => {
        if (secim.length === boy) {
          ekle(secim, [], terimAdi(secim.map((f) => faktorAdlari[f])));
          return;
        }
        for (let f = bas; f < F; f++) {
          secim.push(f);
          uret(f + 1);
          secim.pop();
        }
      };
      uret(0);
    }
  } else if (Array.isArray(etkilesimler)) {
    for (const e of etkilesimler) {
      gerekli(Array.isArray(e) && e.length >= 2, 'Her etkileşim en az iki değişken adından oluşan bir dizi olmalıdır (ör. ["A", "B"]).');
      gerekli(new Set(e).size === e.length, `Etkileşimde aynı değişken iki kez geçiyor: ${e.join(AYRAC)}.`);
      const fi = [];
      const ci = [];
      for (const ad of e) {
        const f = faktorAdlari.indexOf(ad);
        const c = kovAdlari.indexOf(ad);
        gerekli(f >= 0 || c >= 0, `Etkileşimdeki '${ad}' değişkeni faktörler ya da kovaryatlar arasında yok.`);
        if (f >= 0) fi.push(f);
        else ci.push(c);
      }
      ekle(fi, ci, terimAdi(e));
    }
  } else {
    gerekli(etkilesimler === 'yok', "etkilesimler seçeneği 'tam', 'yok' ya da değişken adı dizilerinden oluşan bir dizi olmalıdır.");
  }
  return terimler;
}

/**
 * Veriyi ayıklar, düzeyleri belirler, terimleri ve tasarım matrisini kurar.
 * @private (tekrarli.js ve testler için dışa aktarılır)
 */
export function modelKur({ y, faktorler, kovaryatlar, etkilesimler, duzeySirasi = {}, ekTerimler = [] }) {
  gerekli(Array.isArray(y) || ArrayBuffer.isView(y), 'Bağımlı değişken (y) bir sayı dizisi olmalıdır.');
  const faktorAdlari = Object.keys(faktorler);
  const kovAdlari = Object.keys(kovaryatlar);
  gerekli(faktorAdlari.length >= 1, 'En az bir faktör (gruplama değişkeni) gerekir.');
  const nHam = y.length;
  for (const ad of faktorAdlari) {
    gerekli((Array.isArray(faktorler[ad]) || ArrayBuffer.isView(faktorler[ad])) && faktorler[ad].length === nHam, `'${ad}' faktörünün uzunluğu bağımlı değişkenle aynı olmalıdır.`);
  }
  for (const ad of kovAdlari) {
    gerekli((Array.isArray(kovaryatlar[ad]) || ArrayBuffer.isView(kovaryatlar[ad])) && kovaryatlar[ad].length === nHam, `'${ad}' kovaryatının uzunluğu bağımlı değişkenle aynı olmalıdır.`);
    gerekli(!faktorAdlari.includes(ad), `'${ad}' hem faktör hem kovaryat olarak verilmiş.`);
  }
  const tut = [];
  for (let i = 0; i < nHam; i++) {
    if (!sayiMi(y[i])) continue;
    if (faktorAdlari.some((ad) => etiketEksik(faktorler[ad][i]))) continue;
    if (kovAdlari.some((ad) => !sayiMi(kovaryatlar[ad][i]))) continue;
    tut.push(i);
  }
  const n = tut.length;
  const yv = Float64Array.from(tut.map((i) => y[i]));
  const duzeyler = faktorAdlari.map((ad) => duzeyleriBelirle(tut.map((i) => faktorler[ad][i]), duzeySirasi[ad]));
  faktorAdlari.forEach((ad, f) => {
    gerekli(duzeyler[f].length >= 2, `'${ad}' faktörünün (eksik değerler çıkarıldıktan sonra) en az iki düzeyi olmalıdır.`);
  });
  const faktorK = duzeyler.map((d) => d.length);
  const duzeyIdx = tut.map((i) => faktorAdlari.map((ad, f) => duzeyler[f].indexOf(String(faktorler[ad][i]))));
  const kovDeger = tut.map((i) => kovAdlari.map((ad) => kovaryatlar[ad][i]));
  const kovOrt = kovAdlari.map((_, c) => ortalama(kovDeger.map((s) => s[c])));

  const terimler = terimleriKur(faktorAdlari, kovAdlari, etkilesimler);
  const anaTerimSayisi = terimler.length;
  for (const e of ekTerimler) {
    const anahtar = `${e.faktorIdx.join(',')}|${e.kovIdx.join(',')}`;
    if (!terimler.some((t) => `${t.faktorIdx.join(',')}|${t.kovIdx.join(',')}` === anahtar)) terimler.push({ ...e, ek: true });
  }
  let p = 1;
  for (const t of terimler) {
    let k = 1;
    for (const f of t.faktorIdx) k *= faktorK[f] - 1;
    t.sutunlar = Array.from({ length: k }, (_, j) => p + j);
    p += k;
  }
  // tek gözlemin (ya da ızgara noktasının) tasarım satırı
  const satir = (di, kd) => {
    const x = new Array(p);
    x[0] = 1;
    for (const t of terimler) {
      let v = [1];
      for (const f of t.faktorIdx) {
        const c = toplamKodu(di[f], faktorK[f]);
        const yeni = [];
        for (const b of c) for (const a of v) yeni.push(a * b);
        v = yeni;
      }
      let carpan = 1;
      for (const c of t.kovIdx) carpan *= kd[c];
      for (let j = 0; j < v.length; j++) x[t.sutunlar[j]] = v[j] * carpan;
    }
    return x;
  };
  const sutunlar = Array.from({ length: p }, () => new Float64Array(n));
  for (let i = 0; i < n; i++) {
    const x = satir(duzeyIdx[i], kovDeger[i]);
    for (let j = 0; j < p; j++) sutunlar[j][i] = x[j];
  }
  return {
    faktorAdlari, kovAdlari, duzeyler, faktorK, terimler, anaTerimSayisi,
    n, nHam, tut, yv, duzeyIdx, kovDeger, kovOrt, sutunlar, p, satir,
  };
}

/** Bir terimin faktör düzey birleşimlerinden gözlenmeyenleri (boş hücreler) */
function bosHucreler(model, faktorIdx) {
  const boyut = faktorIdx.map((f) => model.faktorK[f]);
  const gorulen = new Set(model.duzeyIdx.map((d) => faktorIdx.map((f) => d[f]).join(',')));
  return izgara(boyut)
    .filter((h) => !gorulen.has(h.join(',')))
    .map((h) => Object.fromEntries(faktorIdx.map((f, j) => [model.faktorAdlari[f], model.duzeyler[f][h[j]]])));
}

// ---------------------------------------------------------------------------
// Kareler toplamları
// ---------------------------------------------------------------------------

function blokKT(model, digerSutunlar, hedefSutunlar) {
  const sira = [...digerSutunlar, ...hedefSutunlar];
  const r = ardisikQR(sira.map((j) => model.sutunlar[j]), model.yv);
  let KT = 0;
  let sd = 0;
  for (let j = digerSutunlar.length; j < sira.length; j++) {
    if (r.etki[j] !== null) {
      KT += r.etki[j] * r.etki[j];
      sd++;
    }
  }
  return { KT, sd, kalanKT: r.kalanKT, rank: r.rank };
}

/**
 * Terim kareler toplamları. Tip I: sıralı; Tip II: SPSS kapsama kuralına göre terimi içermeyen
 * tüm etkilere göre düzeltilmiş; Tip III: diğer tüm etkilere göre düzeltilmiş (toplamı sıfır kodlamayla).
 */
function terimKTlari(model, terimler, tip, qr) {
  const kesisim = { kesisim: true, sutunlar: [0] };
  const hepsi = [kesisim, ...terimler];
  const sonuc = new Map();
  if (tip === 1) {
    const sira = hepsi.flatMap((t) => t.sutunlar);
    const r = ardisikQR(sira.map((j) => model.sutunlar[j]), model.yv);
    let k = 0;
    for (const t of hepsi) {
      let KT = 0;
      let sd = 0;
      for (let j = 0; j < t.sutunlar.length; j++, k++) {
        if (r.etki[k] !== null) {
          KT += r.etki[k] ** 2;
          sd++;
        }
      }
      sonuc.set(t, { KT, sd });
    }
    return { kesisim: sonuc.get(kesisim), terimler: terimler.map((t) => sonuc.get(t)) };
  }
  if (tip === 3) {
    const hesapla3 = (t) => ({ KT: waldKT(qr, t.sutunlar), sd: t.sutunlar.length });
    return { kesisim: hesapla3(kesisim), terimler: terimler.map(hesapla3) };
  }
  const hesapla = (hedef) => {
    const diger = hepsi.filter((t) => t !== hedef && !icerir(t, hedef));
    return blokKT(model, diger.flatMap((t) => t.sutunlar), hedef.sutunlar);
  };
  return { kesisim: hesapla(kesisim), terimler: terimler.map(hesapla) };
}

function fSatiri(kaynak, tur, KT, sd, KOhata, sdHata, KThata) {
  const KO = KT / sd;
  const F = KO / KOhata;
  return { kaynak, tur, KT, sd, KO, F, p: pf(F, sd, sdHata, false), kismiEtaKare: KT / (KT + KThata) };
}

// ---------------------------------------------------------------------------
// Ana fonksiyon
// ---------------------------------------------------------------------------

/**
 * Faktöriyel ANOVA / ANCOVA (gruplar arası, tek bağımlı değişken).
 *
 * SPSS UNIANOVA karşılıkları: "Tests of Between-Subjects Effects" (anovaTablosu), "Levene's Test of
 * Equality of Error Variances" (levene), "Estimated Marginal Means" (marjinalOrtalamalar),
 * "Pairwise Comparisons" (ikiliKarsilastirmalar), EMMEANS ... COMPARE(A) "Univariate Tests" (basitEtkiler).
 * R doğrulaması: car::Anova(lm(..., contrasts = contr.sum), type = 3 | 2), anova() (Tip I),
 * car::leveneTest(center = mean), emmeans::emmeans / pairs(adjust = ...) / joint_tests(by = ...).
 *
 * - Tip III (varsayılan): toplamı sıfır kodlamada her terim diğer tüm terimlere göre düzeltilir (SPSS = car type 3).
 * - Tip II: SPSS kapsama kuralı (yukarıda). Faktör × kovaryat etkileşimi olmayan modellerde car type 2 ile aynıdır;
 *   faktör × kovaryat etkileşimi varsa SPSS'e uyulur (car'ın sonucu farklıdır).
 * - Tip I: terimler sırasıyla (kovaryatlar, ana etkiler, etkileşimler) eklenir (R anova()).
 * - Levene (SPSS "ortalamaya dayalı"): tüm faktörlerin tam çaprazlanmış hücreleri (+ varsa kovaryatlar ana etki olarak)
 *   modelinin artıklarının mutlak değerine hücreler üzerinden tek yönlü ANOVA. Kovaryat yoksa car::leveneTest(center = mean).
 * - Kestirilen marjinal ortalamalar: diğer faktörlerin düzeyleri üzerinden eşit ağırlıklı ortalama, kovaryatlar
 *   ortalamalarında (SPSS EMMEANS; R emmeans). Kovaryatlı modelde bunlar düzeltilmiş ortalamalardır.
 * - Boş hücre (modeldeki bir etkileşimin gözlemsiz hücresi) varsa Tip III tanımsız olduğundan hata verilir.
 *
 * @param {object} s
 * @param {number[]} s.y bağımlı değişken (eksikler null/NaN olabilir; liste bazında çıkarılır)
 * @param {Object<string, Array>} s.faktorler { ad: etiketler[] } (en az bir faktör)
 * @param {Object<string, number[]>} [s.kovaryatlar] { ad: sayılar[] }
 * @param {'tam'|'yok'|string[][]} [s.etkilesimler='tam'] tam faktöriyel, yalnız ana etkiler ya da özel liste
 *   (ör. [['A','B'], ['A','X']]; kovaryat içeren etkileşime izin verilir)
 * @param {1|2|3} [s.tip=3] kareler toplamı tipi
 * @param {number} [s.duzey=0.95] güven düzeyi
 * @param {'bonferroni'|'sidak'|'yok'} [s.duzeltme='bonferroni'] ikili karşılaştırma düzeltmesi ('yok' = SPSS LSD)
 * @param {Object<string, Array>} [s.duzeySirasi] faktör düzeylerinin sırası (varsayılan artan sıra)
 * @param {boolean} [s.egimHomojenligi=true] kovaryat varsa regresyon eğimlerinin homojenliği testi
 * @returns {object} alanlar: yontem, ad, tip, n, cikarilanGozlem, faktorler, kovaryatlar, terimler, anovaTablosu,
 *   R2, duzeltilmisR2, hataKO, hataSd, levene, betimsel, marjinalOrtalamalar, ikiliKarsilastirmalar, basitEtkiler,
 *   kovaryatKatsayilari, egimHomojenligi, dengeli, duzey, duzeltme, uyarilar
 */
export function faktoriyelAnova(s) {
  const {
    y, faktorler = {}, kovaryatlar = {}, etkilesimler = 'tam', tip = 3, duzey = 0.95,
    duzeltme = 'bonferroni', duzeySirasi = {}, egimHomojenligi = true,
  } = s;
  gerekli([1, 2, 3].includes(tip), 'Kareler toplamı tipi 1, 2 ya da 3 olmalıdır.');
  duzeyDogrula(duzey);
  duzeltmeDogrula(duzeltme);
  const uyarilar = [];
  const model = modelKur({ y, faktorler, kovaryatlar, etkilesimler, duzeySirasi });
  const { n, faktorAdlari, kovAdlari, duzeyler, faktorK, terimler, p } = model;

  // Boş hücre denetimi
  for (const t of terimler) {
    if (t.faktorIdx.length < 2) continue;
    const bos = bosHucreler(model, t.faktorIdx);
    if (bos.length > 0) {
      const ornek = bos.slice(0, 3).map((h) => Object.entries(h).map(([a, d]) => `${a}=${d}`).join(', ')).join('; ');
      throw new IstatistikHatasi(
        `'${t.ad}' etkileşiminde ${bos.length} boş hücre var (ör. ${ornek}). Boş hücreli tasarımda bu etkileşim kestirilemez; ` +
          'etkileşimi modelden çıkarın ya da seyrek düzeyleri birleştirin.',
        { bosHucreler: bos, terim: t.ad },
      );
    }
  }
  const tamBos = bosHucreler(model, faktorAdlari.map((_, f) => f));
  if (tamBos.length > 0) {
    uyarilar.push(`Faktörlerin ${tamBos.length} düzey birleşiminde hiç gözlem yok; marjinal ortalamalar bu hücreler için modelden kestirilmiştir.`);
  }

  gerekli(n > p, `Gözlem sayısı (${n}) modeldeki parametre sayısından (${p}) fazla olmalıdır; hata serbestlik derecesi sıfır.`);
  const tumSira = ardisikQR(model.sutunlar, model.yv, 1e-7, true);
  if (tumSira.rank < p) {
    throw new IstatistikHatasi(
      'Model matrisi tam ranklı değil: kovaryatlar arasında ya da kovaryatlarla faktörler arasında tam doğrusal bağımlılık var. Doğrusal bağımlı kovaryatlardan birini modelden çıkarın.',
    );
  }
  const yOrt = ortalama(model.yv);
  let SST = 0;
  let toplamKare = 0;
  for (const v of model.yv) {
    SST += (v - yOrt) ** 2;
    toplamKare += v * v;
  }
  const SSE = tumSira.kalanKT;
  const hataSd = n - p;
  gerekli(SSE > 1e-13 * Math.max(SST, 1e-300) && SST > 0, 'Hata kareler toplamı sıfır: bağımlı değişken hücreler içinde değişkenlik göstermiyor.');
  const MSE = SSE / hataSd;
  let SSM = 0;
  for (let j = 1; j < p; j++) SSM += tumSira.etki[j] ** 2;

  const kt = terimKTlari(model, terimler, tip, tumSira);
  const anovaTablosu = [];
  anovaTablosu.push(fSatiri('Düzeltilmiş Model', 'model', SSM, p - 1, MSE, hataSd, SSE));
  anovaTablosu.push(fSatiri('Kesişim', 'kesisim', kt.kesisim.KT, kt.kesisim.sd, MSE, hataSd, SSE));
  terimler.forEach((t, i) => {
    anovaTablosu.push(fSatiri(t.ad, 'terim', kt.terimler[i].KT, kt.terimler[i].sd, MSE, hataSd, SSE));
  });
  anovaTablosu.push({ kaynak: 'Hata', tur: 'hata', KT: SSE, sd: hataSd, KO: MSE });
  anovaTablosu.push({ kaynak: 'Toplam', tur: 'toplam', KT: toplamKare, sd: n });
  anovaTablosu.push({ kaynak: 'Düzeltilmiş Toplam', tur: 'duzeltilmisToplam', KT: SST, sd: n - 1 });

  const R2 = SSM / SST;
  const duzeltilmisR2 = 1 - (SSE / hataSd) / (SST / (n - 1));

  // Hücre sayıları, dengelilik
  const hucreSayilari = new Map();
  for (const d of model.duzeyIdx) hucreSayilari.set(d.join(','), (hucreSayilari.get(d.join(',')) || 0) + 1);
  const sayilar = [...hucreSayilari.values()];
  const dengeli = tamBos.length === 0 && sayilar.every((v) => v === sayilar[0]);
  if (!dengeli && tip === 1) uyarilar.push('Dengesiz tasarımda Tip I kareler toplamı terimlerin sırasına bağlıdır; SPSS varsayılanı Tip III\'tür.');
  if (sayilar.some((v) => v < 2)) uyarilar.push('Bazı hücrelerde yalnızca bir gözlem var; varyans homojenliği bu hücreler için değerlendirilemez.');

  const levene = leveneHesapla(model);
  const betimsel = betimselHesapla(model);

  // Kestirilen marjinal ortalamalar
  const V = tumSira.XtXTers;
  const beta = tumSira.katsayi;
  const tKritik = qt((1 - duzey) / 2, hataSd, false);
  const emmTerimleri = [
    ...faktorAdlari.map((a, f) => ({ fi: [f], ad: a })),
    ...terimler.filter((t) => t.faktorIdx.length >= 2 && t.kovIdx.length === 0).map((t) => ({ fi: t.faktorIdx, ad: t.ad })),
  ];
  const Lharitasi = new Map();
  const marjinalOrtalamalar = {};
  for (const { fi, ad } of emmTerimleri) {
    const Ls = marjinalL(model, fi);
    Lharitasi.set(fi.join(','), Ls);
    marjinalOrtalamalar[ad] = Ls.map(({ hucre, L }) => {
      const tahmin = L.reduce((t, v, j) => t + v * beta[j], 0);
      const SH = Math.sqrt(MSE * kuadratik(L, V));
      return {
        duzeyler: Object.fromEntries(fi.map((f, j) => [faktorAdlari[f], duzeyler[f][hucre[j]]])),
        ortalama: tahmin, SH, sd: hataSd, gaAlt: tahmin - tKritik * SH, gaUst: tahmin + tKritik * SH,
      };
    });
  }

  const karsilastir = (Ls, etiket, m) => {
    const sonuc = [];
    for (let i = 0; i < Ls.length; i++) {
      for (let j = i + 1; j < Ls.length; j++) {
        const L = Ls[i].L.map((v, k) => v - Ls[j].L[k]);
        const fark = L.reduce((t, v, k) => t + v * beta[k], 0);
        const SH = Math.sqrt(MSE * kuadratik(L, V));
        const t = fark / SH;
        const d = duzeltilmisKarsilastirma(t, hataSd, m, duzeltme, duzey);
        sonuc.push({
          duzey1: etiket(Ls[i]), duzey2: etiket(Ls[j]), fark, SH, t, sd: hataSd, p: d.p, pHam: d.pHam,
          gaAlt: fark - d.tKritik * SH, gaUst: fark + d.tKritik * SH,
        });
      }
    }
    return sonuc;
  };

  const ikiliKarsilastirmalar = {};
  faktorAdlari.forEach((ad, f) => {
    const Ls = Lharitasi.get(String(f));
    ikiliKarsilastirmalar[ad] = karsilastir(Ls, (h) => duzeyler[f][h.hucre[0]], (faktorK[f] * (faktorK[f] - 1)) / 2);
  });

  // Basit etkiler: iki faktörlü etkileşimlerde bir faktörün diğerinin her düzeyindeki etkisi
  const basitEtkiler = [];
  for (const t of terimler) {
    if (t.faktorIdx.length !== 2 || t.kovIdx.length !== 0) continue;
    const Ls = Lharitasi.get(t.faktorIdx.join(','));
    for (const [etkenSira, kosulSira] of [[0, 1], [1, 0]]) {
      const fE = t.faktorIdx[etkenSira];
      const fK = t.faktorIdx[kosulSira];
      for (let kd = 0; kd < faktorK[fK]; kd++) {
        const grup = Ls.filter((h) => h.hucre[kosulSira] === kd).sort((a, b) => a.hucre[etkenSira] - b.hucre[etkenSira]);
        const q = grup.length - 1;
        const Lm = grup.slice(0, q).map((h) => h.L.map((v, k) => v - grup[q].L[k]));
        const tahmin = Lm.map((L) => L.reduce((s2, v, k) => s2 + v * beta[k], 0));
        const C = Lm.map((Li) => Lm.map((Lj) => ikiliForm(Li, V, Lj)));
        const Ci = simetrikTers(C);
        const KT = kuadratik(tahmin, Ci);
        const F = KT / q / MSE;
        basitEtkiler.push({
          etkilesim: t.ad,
          etken: faktorAdlari[fE],
          kosul: { [faktorAdlari[fK]]: duzeyler[fK][kd] },
          KT, sd: q, KO: KT / q, F, hataSd, p: pf(F, q, hataSd, false), kismiEtaKare: KT / (KT + SSE),
          ikiliKarsilastirmalar: karsilastir(grup, (h) => duzeyler[fE][h.hucre[etkenSira]], (faktorK[fE] * (faktorK[fE] - 1)) / 2),
        });
      }
    }
  }

  // Kovaryat katsayıları (kovaryat başka bir terimde geçmiyorsa SPSS "Parameter Estimates" ile aynıdır)
  const kovaryatKatsayilari = [];
  terimler.forEach((t) => {
    if (t.faktorIdx.length !== 0 || t.kovIdx.length !== 1) return;
    const c = t.kovIdx[0];
    const baskaTerimde = terimler.some((u) => u !== t && u.kovIdx.includes(c));
    if (baskaTerimde) return;
    const j = t.sutunlar[0];
    const B = beta[j];
    const SH = Math.sqrt(MSE * V[j][j]);
    const tt = B / SH;
    kovaryatKatsayilari.push({
      ad: kovAdlari[c], B, SH, t: tt, sd: hataSd, p: Math.min(1, 2 * pt(-Math.abs(tt), hataSd)),
      gaAlt: B - tKritik * SH, gaUst: B + tKritik * SH,
    });
  });

  // Regresyon eğimlerinin homojenliği
  let egim = null;
  if (egimHomojenligi && kovAdlari.length > 0) {
    egim = egimHomojenligiHesapla(s, model, SSE, hataSd);
    if (egim && egim.toplu.p < 0.05) {
      uyarilar.push('Regresyon eğimleri gruplar arasında homojen değil (faktör × kovaryat etkileşimi anlamlı, p < 0,05); ANCOVA varsayımı sağlanmıyor.');
    }
  }
  if (levene && levene.p < 0.05) uyarilar.push('Levene testi anlamlı (p < 0,05): hata varyansları gruplar arasında homojen değil.');

  const birFaktor = faktorAdlari.length === 1;
  const ad = kovAdlari.length > 0
    ? (birFaktor ? 'Tek yönlü kovaryans analizi (ANCOVA)' : 'Faktöriyel kovaryans analizi (ANCOVA)')
    : birFaktor ? 'Tek yönlü varyans analizi (GLM)'
      : faktorAdlari.length === 2 ? 'İki yönlü varyans analizi' : `${faktorAdlari.length} yönlü varyans analizi`;

  return {
    yontem: kovAdlari.length > 0 ? 'ankova' : 'faktoriyel-anova',
    ad,
    tip,
    n,
    cikarilanGozlem: model.nHam - n,
    faktorler: faktorAdlari.map((a, f) => ({ ad: a, duzeyler: duzeyler[f] })),
    kovaryatlar: kovAdlari.map((a, c) => ({ ad: a, ortalama: model.kovOrt[c] })),
    terimler: terimler.map((t) => t.ad),
    anovaTablosu,
    R2,
    duzeltilmisR2,
    hataKO: MSE,
    hataSd,
    levene,
    betimsel,
    marjinalOrtalamalar,
    ikiliKarsilastirmalar,
    basitEtkiler,
    kovaryatKatsayilari,
    egimHomojenligi: egim,
    dengeli,
    duzey,
    duzeltme,
    uyarilar,
  };
}

/** Bir faktör kümesinin hücreleri için EMM katsayı vektörleri (diğer faktörler üzerinden eşit ağırlık, kovaryatlar ortalamada) */
function marjinalL(model, faktorIdx) {
  const { faktorK, p } = model;
  const tum = izgara(faktorK);
  const harita = new Map();
  for (const nokta of tum) {
    const hucre = faktorIdx.map((f) => nokta[f]);
    const anahtar = hucre.join(',');
    let kayit = harita.get(anahtar);
    if (!kayit) {
      kayit = { hucre, L: new Array(p).fill(0), sayi: 0 };
      harita.set(anahtar, kayit);
    }
    const x = model.satir(nokta, model.kovOrt);
    for (let j = 0; j < p; j++) kayit.L[j] += x[j];
    kayit.sayi++;
  }
  const sonuc = [...harita.values()];
  for (const k of sonuc) k.L = k.L.map((v) => v / k.sayi);
  // izgara sırası: ilk faktör en yavaş
  return sonuc.map(({ hucre, L }) => ({ hucre, L }));
}

/** Tek yönlü ANOVA F'si (gruplar: tamsayı indeksleri) */
function tekYonluF(z, grup, G) {
  const n = z.length;
  const top = new Array(G).fill(0);
  const say = new Array(G).fill(0);
  for (let i = 0; i < n; i++) {
    top[grup[i]] += z[i];
    say[grup[i]]++;
  }
  const ort = top.map((t, g) => t / say[g]);
  const genel = ortalama(z);
  let ara = 0;
  let ic = 0;
  for (let g = 0; g < G; g++) ara += say[g] * (ort[g] - genel) ** 2;
  for (let i = 0; i < n; i++) ic += (z[i] - ort[grup[i]]) ** 2;
  const sd1 = G - 1;
  const sd2 = n - G;
  const F = (ara / sd1) / (ic / sd2);
  return { F, sd1, sd2, ara, ic };
}

function leveneHesapla(model) {
  const { n, duzeyIdx, kovAdlari } = model;
  const anahtarlar = duzeyIdx.map((d) => d.join(','));
  const tekil = [...new Set(anahtarlar)];
  const G = tekil.length;
  const grup = anahtarlar.map((a) => tekil.indexOf(a));
  if (G < 2 || n - G < 1) return null;
  let artik;
  if (kovAdlari.length === 0) {
    const top = new Array(G).fill(0);
    const say = new Array(G).fill(0);
    model.yv.forEach((v, i) => {
      top[grup[i]] += v;
      say[grup[i]]++;
    });
    // hücre ortalamaları (iki geçişli)
    const ort = top.map((t, g) => t / say[g]);
    const duz = new Array(G).fill(0);
    model.yv.forEach((v, i) => {
      duz[grup[i]] += v - ort[grup[i]];
    });
    for (let g = 0; g < G; g++) ort[g] += duz[g] / say[g];
    artik = Array.from(model.yv, (v, i) => v - ort[grup[i]]);
  } else {
    // hücreler (gösterge kodlama) + kovaryatlar
    const sut = [];
    for (let g = 0; g < G; g++) sut.push(Float64Array.from(grup, (h) => (h === g ? 1 : 0)));
    kovAdlari.forEach((_, c) => sut.push(Float64Array.from(model.kovDeger, (s) => s[c])));
    const X = Array.from({ length: n }, (_, i) => sut.map((s) => s[i]));
    const e = enKucukKareler(X, Array.from(model.yv));
    artik = e.kalan;
  }
  const z = artik.map(Math.abs);
  const r = tekYonluF(z, grup, G);
  if (!(r.ic > 0)) return null;
  return {
    yontem: 'levene-ortalama',
    ad: 'Levene testi (ortalamaya dayalı)',
    istatistik: r.F,
    istatistikAdi: 'F',
    sd: [r.sd1, r.sd2],
    p: pf(r.F, r.sd1, r.sd2, false),
    hucreSayisi: G,
    kovaryatliArtik: kovAdlari.length > 0,
  };
}

function betimselOzet(degerler) {
  return {
    n: degerler.length,
    ortalama: ortalama(degerler),
    standartSapma: degerler.length > 1 ? standartSapma(degerler) : null,
  };
}

function betimselHesapla(model) {
  const { faktorAdlari, duzeyler, faktorK, duzeyIdx, yv } = model;
  const hucreler = [];
  for (const h of izgara(faktorK)) {
    const d = [];
    duzeyIdx.forEach((di, i) => {
      if (di.every((v, f) => v === h[f])) d.push(yv[i]);
    });
    if (d.length === 0) continue;
    hucreler.push({ duzeyler: Object.fromEntries(faktorAdlari.map((a, f) => [a, duzeyler[f][h[f]]])), ...betimselOzet(d) });
  }
  const faktorBazinda = {};
  faktorAdlari.forEach((a, f) => {
    faktorBazinda[a] = duzeyler[f].map((dz, l) => ({
      duzey: dz,
      ...betimselOzet(Array.from(yv).filter((_, i) => duzeyIdx[i][f] === l)),
    }));
  });
  return { hucreler, faktorBazinda, toplam: betimselOzet(Array.from(yv)) };
}

/**
 * Regresyon eğimlerinin homojenliği: modele her kovaryat için (modeldeki her saf faktör terimi) × kovaryat
 * etkileşimleri eklenir. Her eklenen terim için Tip III F (SPSS'te özel model "A, X, A*X" ile aynı) ve
 * eklenen tüm terimler için birleşik F (genişletilmiş modelle karşılaştırma) raporlanır.
 */
function egimHomojenligiHesapla(s, model, SSE0, sd0) {
  const ekTerimler = [];
  const faktorTerimleri = model.terimler.filter((t) => t.faktorIdx.length > 0 && t.kovIdx.length === 0);
  model.kovAdlari.forEach((kAd, c) => {
    for (const t of faktorTerimleri) {
      ekTerimler.push({ ad: `${t.ad}${AYRAC}${kAd}`, faktorIdx: t.faktorIdx.slice(), kovIdx: [c] });
    }
  });
  const genis = modelKur({
    y: s.y, faktorler: s.faktorler, kovaryatlar: s.kovaryatlar, etkilesimler: s.etkilesimler ?? 'tam',
    duzeySirasi: s.duzeySirasi ?? {}, ekTerimler,
  });
  const yeni = genis.terimler.filter((t) => t.ek);
  if (yeni.length === 0) return null;
  const hataSd = genis.n - genis.p;
  if (hataSd < 1) return null;
  const tum = ardisikQR(genis.sutunlar, genis.yv, 1e-7, true);
  if (tum.rank < genis.p) return null;
  const SSE = tum.kalanKT;
  const MSE = SSE / hataSd;
  const kt = terimKTlari(genis, genis.terimler, 3, tum);
  const tablo = [];
  genis.terimler.forEach((t, i) => {
    if (!t.ek) return;
    tablo.push(fSatiri(t.ad, 'terim', kt.terimler[i].KT, kt.terimler[i].sd, MSE, hataSd, SSE));
  });
  // eklenen terimler sütun sırasının sonunda: birleşik KT = bu sütunların QR etkilerinin kareler toplamı
  const blok = { KT: 0, sd: 0 };
  for (const t of yeni) {
    for (const j of t.sutunlar) {
      blok.KT += tum.etki[j] ** 2;
      blok.sd++;
    }
  }
  const F = blok.KT / blok.sd / MSE;
  return {
    tablo,
    toplu: { KT: blok.KT, sd: blok.sd, KO: blok.KT / blok.sd, F, hataSd, p: pf(F, blok.sd, hataSd, false) },
    hata: { KT: SSE, sd: hataSd, KO: MSE },
    ozgunModelHata: { KT: SSE0, sd: sd0 },
  };
}
