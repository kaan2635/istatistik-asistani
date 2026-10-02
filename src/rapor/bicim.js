// Sayı ve istatistik biçimlendirme. Tüm metin çıktısı buradan geçer; ondalık ayırıcı, basamak sayısı
// ve p değeri gösterimi kullanıcı ayarlarından gelir.

export const VARSAYILAN_BICIM = {
  ondalikAyrac: ',',
  basamak: 2,
  pBasamak: 3,
  pEsik: 0.001,
  pBasSifir: true, // true: "0,034" (Türkçe tezler), false: ",034" (APA)
  istatistikBasamak: 2,
  eksi: '-',
};

function ayrac(metin, b) {
  return b.ondalikAyrac === '.' ? metin : metin.replace('.', b.ondalikAyrac);
}

/** Sayıyı sabit basamakla yazar; -0 yazmaz. Sonlu değilse "–" döner. */
export function sayi(x, basamak, b = VARSAYILAN_BICIM) {
  if (typeof x !== 'number' || Number.isNaN(x)) return '–';
  if (x === Infinity) return '∞';
  if (x === -Infinity) return '-∞';
  const k = basamak ?? b.basamak;
  let metin = Math.abs(x) >= 1e15 ? x.toExponential(k) : x.toFixed(k);
  if (/^-0(\.0*)?$/.test(metin)) metin = metin.slice(1);
  if (metin.startsWith('-')) metin = b.eksi + metin.slice(1);
  return ayrac(metin, b);
}

/** Tamsayı (binlik ayırıcısız) */
export function tam(x) {
  if (x === null || x === undefined || Number.isNaN(x)) return '–';
  return String(Math.round(x));
}

/** Değere göre uygun basamak: büyük sayılarda az, küçüklerde çok basamak */
export function uyarlanmis(x, b = VARSAYILAN_BICIM) {
  if (x === null || x === undefined || !Number.isFinite(x)) return sayi(x, 0, b);
  const m = Math.abs(x);
  if (m === 0) return sayi(0, b.basamak, b);
  if (m >= 1000) return sayi(x, Math.min(b.basamak, 1), b);
  if (m < 0.01) return sayi(x, Math.max(b.basamak, 4), b);
  if (m < 0.1) return sayi(x, Math.max(b.basamak, 3), b);
  return sayi(x, b.basamak, b);
}

/** p değeri: "<0,001", "0,034", "1,000"; bas sıfır ayarına uyar */
export function p(deger, b = VARSAYILAN_BICIM) {
  if (deger === null || deger === undefined || Number.isNaN(deger)) return '–';
  let metin;
  if (deger < b.pEsik) metin = '<' + sayi(b.pEsik, b.pBasamak, b);
  else {
    metin = sayi(Math.min(1, deger), b.pBasamak, b);
    // yuvarlama eşiğe eşit sonuç verirse ("0,001" ama değer 0,00096 değil) olduğu gibi kalır
  }
  if (!b.pBasSifir) metin = metin.replace(/^(<)?0(?=[.,])/, '$1');
  return metin;
}

/** "p = 0,034" ya da "p < 0,001" */
export function pEsitlik(deger, b = VARSAYILAN_BICIM) {
  const m = p(deger, b);
  if (m.startsWith('<')) return `p ${'<'} ${m.slice(1)}`;
  return `p = ${m}`;
}

/** ortalama ± SS */
export function ortSS(ort, ss, basamak, b = VARSAYILAN_BICIM) {
  return `${sayi(ort, basamak, b)} ± ${sayi(ss, basamak, b)}`;
}

/** Aralık yazımı; negatif değer varsa karışıklığı önlemek için noktalı virgül kullanılır */
export function aralik(alt, ust, basamak, b = VARSAYILAN_BICIM) {
  const a = sayi(alt, basamak, b);
  const u = sayi(ust, basamak, b);
  if ((alt !== null && alt < 0) || (ust !== null && ust < 0)) return `${a}; ${u}`;
  return `${a}–${u}`;
}

/** medyan (alt–üst) */
export function medyanAralik(med, alt, ust, basamak, b = VARSAYILAN_BICIM) {
  return `${sayi(med, basamak, b)} (${aralik(alt, ust, basamak, b)})`;
}

/** n (%) */
export function nYuzde(n, toplam, basamak = 1, b = VARSAYILAN_BICIM) {
  if (!toplam) return `${n} (–)`;
  return `${n} (${sayi((100 * n) / toplam, basamak, b)})`;
}

export function yuzde(oran, basamak = 1, b = VARSAYILAN_BICIM) {
  return `${sayi(oran * 100, basamak, b)}%`;
}

/** Güven aralığı: her zaman "a; b" (negatif sınırlarla karışmaması ve tablolarda tutarlılık için) */
export function ga(alt, ust, basamak, b = VARSAYILAN_BICIM) {
  return `${sayi(alt, basamak, b)}; ${sayi(ust, basamak, b)}`;
}

/** Serbestlik derecesi: tamsayıysa tam, değilse iki basamak */
export function sd(deger, b = VARSAYILAN_BICIM) {
  if (Array.isArray(deger)) return deger.map((d) => sd(d, b)).join(', ');
  if (deger === null || deger === undefined) return '';
  if (Math.abs(deger - Math.round(deger)) < 1e-9) return String(Math.round(deger));
  return sayi(deger, 2, b);
}

/** Simge ve sd ile istatistik: "t(17,78) = -1,86" */
export function istatistik(simge, deger, serbestlik, b = VARSAYILAN_BICIM) {
  const s = serbestlik === null || serbestlik === undefined || (Array.isArray(serbestlik) && serbestlik.length === 0) ? '' : `(${sd(serbestlik, b)})`;
  return `${simge}${s} = ${sayi(deger, b.istatistikBasamak, b)}`;
}

/** HTML özel karakterlerini kaçırır */
export function kacir(metin) {
  return String(metin).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]);
}

/** Ayarlar nesnesinden biçim nesnesi */
export function bicimAyarlari(ayarlar = {}) {
  return {
    ...VARSAYILAN_BICIM,
    ondalikAyrac: ayarlar.ondalikAyrac ?? VARSAYILAN_BICIM.ondalikAyrac,
    basamak: ayarlar.basamak ?? VARSAYILAN_BICIM.basamak,
    pBasamak: ayarlar.pBasamak ?? VARSAYILAN_BICIM.pBasamak,
    pBasSifir: ayarlar.pBasSifir ?? VARSAYILAN_BICIM.pBasSifir,
  };
}
