// Model (tasarım) matrisi kurucu: regresyon ve GLM modülleri için ortak.
// Kategorik tahminciler gösterge (dummy, "treatment") kodlamasıyla açılır; eksik değerli satırlar
// liste bazında (listwise) atılır. R'nin model.frame + model.matrix (contr.treatment,
// drop.unused.levels = TRUE) davranışıyla aynıdır.

import { IstatistikHatasi } from './hata.js';

export const SABIT_ADI = '(Sabit)';

/** Eksik değer mi? null, undefined, NaN ve boş/boşluk dizesi eksik sayılır. */
export function eksikMi(v) {
  return v === null || v === undefined || (typeof v === 'number' && Number.isNaN(v)) || (typeof v === 'string' && v.trim() === '');
}

/** Sürekli değişken değerini sayıya çevirir (sayısal dizeler kabul edilir); geçersizse hata verir. */
export function sayiyaCevir(v, ad) {
  if (typeof v === 'number') {
    if (!Number.isFinite(v)) throw new IstatistikHatasi(`'${ad}' değişkeninde sonlu olmayan değer var (${v}).`);
    return v;
  }
  if (typeof v === 'boolean') return v ? 1 : 0;
  if (typeof v === 'string') {
    const s = Number(v.trim().replace(',', '.'));
    if (Number.isFinite(s)) return s;
  }
  throw new IstatistikHatasi(`'${ad}' sürekli değişkeninde sayısal olmayan değer var: '${v}'. Değişkeni kategorik olarak tanımlayın ya da değeri düzeltin.`);
}

/**
 * Düzeyleri sıralar: tümü sayıysa küçükten büyüğe (SPSS'teki sayısal kodlar gibi), değilse Türkçe alfabetik.
 * R'de karakter vektörlerinin sırası yerel ayara bağlıdır; kesin sıra gerekiyorsa `duzeyler` verin.
 */
export function duzeySirala(degerler) {
  const tekil = [];
  const gorulen = new Set();
  for (const v of degerler) {
    const k = String(v);
    if (!gorulen.has(k)) {
      gorulen.add(k);
      tekil.push(v);
    }
  }
  if (tekil.every((v) => typeof v === 'number')) return tekil.sort((a, b) => a - b);
  return tekil.sort((a, b) => String(a).localeCompare(String(b), 'tr'));
}

/**
 * Verilen dizilerin hiçbirinde eksik değer olmayan satırların indeksleri (listwise).
 * @param {Array<Array>} diziler
 * @returns {number[]}
 */
export function tamSatirlar(diziler) {
  if (!diziler.length) return [];
  const n = diziler[0].length;
  for (const d of diziler) {
    if (d.length !== n) throw new IstatistikHatasi('Değişkenlerin gözlem sayıları eşit değil.');
  }
  const satirlar = [];
  for (let i = 0; i < n; i++) {
    let tam = true;
    for (const d of diziler) {
      if (eksikMi(d[i])) {
        tam = false;
        break;
      }
    }
    if (tam) satirlar.push(i);
  }
  return satirlar;
}

/** Bir tahminci tanımını seçili satırlara indirger (diğer alanlar korunur). */
export function tahminciSec(t, satirlar) {
  return { ...t, degerler: satirlar.map((i) => t.degerler[i]) };
}

/**
 * Tasarım matrisi kurar.
 *
 * Kategorik değişkenler gösterge kodlamasıyla (k düzey → k-1 sütun) açılır; referans düzey sıfır satırıdır.
 * Varsayılan referans **ilk** düzeydir (R'nin contr.treatment'ı ve SPSS'in "Indicator, Reference: First" seçeneği).
 * SPSS LOGISTIC REGRESSION/GLM'nin kendi varsayılanı **son** kategoridir; aynı sonucu almak için `referans` açıkça verilmelidir.
 * Kullanılmayan düzeyler (eksik satırlar atıldıktan sonra hiç gözlemi kalmayan) R'deki gibi modelden çıkarılır.
 *
 * Etkileşimler: `etkilesimler: [['a','b'], ['a','b','c']]`. Her bileşen ana etki olarak da modelde bulunmalı
 * (3'lü etkileşimde alt 2'li etkileşimler de). Sütunlar, bileşen sütunlarının çarpımıdır; ilk bileşen en hızlı değişir
 * (R model.matrix sırası).
 *
 * @param {Array<{ad:string, tur:'surekli'|'kategorik', degerler:Array<number|string|null>, referans?:any, duzeyler?:Array}>} tahminciler
 * @param {{sabit?:boolean, etkilesimler?:string[][], ekDiziler?:Array<Array>}} [secenekler]
 *   ekDiziler: eksik değer kontrolüne katılacak ek diziler (ör. bağımlı değişken); süzülmüş halleri `ekler`de döner.
 * @returns {{X:number[][], sutunAdlari:string[], sutunlar:Array<{ad:string, terim:string|null, duzey:any}>,
 *   terimler:Array<{ad:string, tur:'surekli'|'kategorik'|'etkilesim', sutunlar:number[], duzeyler?:Array, referans?:any,
 *   frekanslar?:number[], bilesenler?:string[]}>, sabit:boolean, satirlar:number[], n:number, atilanSatir:number,
 *   ekler:Array<Array>, uyarilar:string[]}}
 */
export function modelMatrisi(tahminciler, secenekler = {}) {
  const { sabit = true, etkilesimler = [], ekDiziler = [] } = secenekler;
  if (!Array.isArray(tahminciler)) throw new IstatistikHatasi('Tahminci listesi geçersiz.');
  const uyarilar = [];
  const adlar = new Set();
  for (const t of tahminciler) {
    if (!t || typeof t.ad !== 'string' || !t.ad) throw new IstatistikHatasi('Her tahmincinin bir adı olmalıdır.');
    if (adlar.has(t.ad)) throw new IstatistikHatasi(`'${t.ad}' adlı tahminci birden fazla kez verildi.`);
    adlar.add(t.ad);
    if (t.tur !== 'surekli' && t.tur !== 'kategorik') throw new IstatistikHatasi(`'${t.ad}' için tür 'surekli' ya da 'kategorik' olmalıdır.`);
    if (!Array.isArray(t.degerler)) throw new IstatistikHatasi(`'${t.ad}' için değer dizisi yok.`);
  }
  const tumDiziler = [...tahminciler.map((t) => t.degerler), ...ekDiziler];
  const N = tumDiziler.length ? tumDiziler[0].length : 0;
  const satirlar = tamSatirlar(tumDiziler);
  const n = satirlar.length;
  if (tumDiziler.length && n === 0) throw new IstatistikHatasi('Eksik değerler çıkarıldıktan sonra hiç gözlem kalmadı.');
  if (n < N) uyarilar.push(`Eksik değer nedeniyle ${N - n} gözlem analiz dışı bırakıldı (liste bazında silme); analiz ${n} gözlemle yapıldı.`);

  // Her tahmincinin sütun blokları (n × k)
  const bloklar = new Map();
  const terimler = [];
  const sutunlar = [];
  const sutunlarX = [];
  if (sabit) {
    sutunlar.push({ ad: SABIT_ADI, terim: null, duzey: null });
    sutunlarX.push(new Array(n).fill(1));
  }
  for (const t of tahminciler) {
    const ham = satirlar.map((i) => t.degerler[i]);
    if (t.tur === 'surekli') {
      const x = ham.map((v) => sayiyaCevir(v, t.ad));
      const blok = { sutunlar: [x], adlar: [t.ad], duzeyler: [null] };
      bloklar.set(t.ad, blok);
      const indeks = [sutunlarX.length];
      sutunlarX.push(x);
      sutunlar.push({ ad: t.ad, terim: t.ad, duzey: null });
      terimler.push({ ad: t.ad, tur: 'surekli', sutunlar: indeks });
      continue;
    }
    // kategorik
    const anahtar = ham.map((v) => String(v));
    const mevcut = new Set(anahtar);
    let duzeyler;
    if (Array.isArray(t.duzeyler) && t.duzeyler.length) {
      const tanimli = new Set(t.duzeyler.map(String));
      for (const v of ham) {
        if (!tanimli.has(String(v))) throw new IstatistikHatasi(`'${t.ad}' değişkeninde tanımlı düzeyler dışında bir değer var: '${v}'.`);
      }
      const bos = t.duzeyler.filter((d) => !mevcut.has(String(d)));
      if (bos.length) uyarilar.push(`'${t.ad}' değişkeninin şu düzeylerinde gözlem yok, modelden çıkarıldı: ${bos.join(', ')}.`);
      duzeyler = t.duzeyler.filter((d) => mevcut.has(String(d)));
    } else {
      duzeyler = duzeySirala(ham);
    }
    if (duzeyler.length < 2) {
      throw new IstatistikHatasi(`'${t.ad}' değişkeninde yalnızca bir düzey var${n < N ? ' (eksik değerler çıkarıldıktan sonra)' : ''}; kategorik tahminci en az iki düzey gerektirir.`);
    }
    let referans = duzeyler[0];
    if (t.referans !== undefined && t.referans !== null) {
      const r = duzeyler.find((d) => String(d) === String(t.referans));
      if (r === undefined) throw new IstatistikHatasi(`'${t.ad}' için seçilen referans düzeyi ('${t.referans}') veride bulunmuyor.`);
      referans = r;
    }
    const refAnahtar = String(referans);
    const frekanslar = duzeyler.map((d) => {
      const k = String(d);
      let c = 0;
      for (const a of anahtar) if (a === k) c++;
      return c;
    });
    const blok = { sutunlar: [], adlar: [], duzeyler: [] };
    const indeks = [];
    for (const d of duzeyler) {
      const k = String(d);
      if (k === refAnahtar) continue;
      const x = anahtar.map((a) => (a === k ? 1 : 0));
      const ad = `${t.ad}=${d}`;
      blok.sutunlar.push(x);
      blok.adlar.push(ad);
      blok.duzeyler.push(d);
      indeks.push(sutunlarX.length);
      sutunlarX.push(x);
      sutunlar.push({ ad, terim: t.ad, duzey: d });
    }
    bloklar.set(t.ad, blok);
    terimler.push({ ad: t.ad, tur: 'kategorik', sutunlar: indeks, duzeyler, referans, frekanslar });
  }

  // Etkileşimler
  const etkilesimAnahtarlari = new Set();
  const anahtarOf = (bilesen) => [...bilesen].sort().join('\u0000');
  for (const e of etkilesimler) {
    if (!Array.isArray(e) || e.length < 2) throw new IstatistikHatasi('Etkileşim terimi en az iki değişken içermelidir.');
    if (new Set(e).size !== e.length) throw new IstatistikHatasi('Etkileşim teriminde aynı değişken iki kez geçiyor.');
    for (const ad of e) {
      if (!bloklar.has(ad)) throw new IstatistikHatasi(`Etkileşimdeki '${ad}' değişkeni modelde ana etki olarak bulunmalıdır.`);
    }
    etkilesimAnahtarlari.add(anahtarOf(e));
  }
  const etkilesimSirali = etkilesimler.map((e, i) => ({ e, i })).sort((a, b) => a.e.length - b.e.length || a.i - b.i);
  const eklenen = new Set();
  for (const { e } of etkilesimSirali) {
    const anahtar = anahtarOf(e);
    if (eklenen.has(anahtar)) throw new IstatistikHatasi(`'${e.join(' × ')}' etkileşimi birden fazla kez verildi.`);
    // Hiyerarşi: tüm alt etkileşimler (2..k-1'li) modelde olmalı
    if (e.length > 2) {
      const altKumeler = [];
      const k = e.length;
      for (let m = 1; m < (1 << k) - 1; m++) {
        const alt = e.filter((_, j) => m & (1 << j));
        if (alt.length >= 2) altKumeler.push(alt);
      }
      for (const alt of altKumeler) {
        if (!etkilesimAnahtarlari.has(anahtarOf(alt))) {
          throw new IstatistikHatasi(`'${e.join(' × ')}' etkileşimi için alt etkileşim '${alt.join(' × ')}' de modelde bulunmalıdır.`);
        }
      }
    }
    eklenen.add(anahtar);
    // Kartezyen çarpım; ilk bileşen en hızlı değişir
    let birikim = [{ x: new Array(n).fill(1), adlar: [] }];
    for (const ad of e) {
      const b = bloklar.get(ad);
      const yeni = [];
      for (let j = 0; j < b.sutunlar.length; j++) {
        for (const onceki of birikim) {
          const x = onceki.x.map((v, i) => v * b.sutunlar[j][i]);
          yeni.push({ x, adlar: [...onceki.adlar, b.adlar[j]] });
        }
      }
      birikim = yeni;
    }
    const terimAdi = e.join(' × ');
    const indeks = [];
    for (const s of birikim) {
      indeks.push(sutunlarX.length);
      sutunlarX.push(s.x);
      sutunlar.push({ ad: s.adlar.join(' × '), terim: terimAdi, duzey: null });
    }
    terimler.push({ ad: terimAdi, tur: 'etkilesim', sutunlar: indeks, bilesenler: e.slice() });
  }

  const p = sutunlarX.length;
  const X = new Array(n);
  for (let i = 0; i < n; i++) {
    const satir = new Array(p);
    for (let j = 0; j < p; j++) satir[j] = sutunlarX[j][i];
    X[i] = satir;
  }
  const ekler = ekDiziler.map((d) => satirlar.map((i) => d[i]));
  return {
    X,
    sutunAdlari: sutunlar.map((s) => s.ad),
    sutunlar,
    terimler,
    sabit,
    satirlar,
    n,
    atilanSatir: N - n,
    ekler,
    uyarilar,
  };
}
