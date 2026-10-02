// Değişken türü tanıma ve analiz önerileri.
// Dosyadan gelen ölçüm düzeyi (SPSS) varsa ona güvenilir; yoksa değerlerin yapısından ve değişken adından tahmin edilir.
// Tahminler kullanıcıya "kontrol edin" işaretiyle gösterilir; son karar kullanıcınındır.

/** Türkçe duyarlı küçük harfe çevirip aksanları sadeleştirir (karşılaştırma için) */
export function sadelestir(metin) {
  return String(metin)
    .toLocaleLowerCase('tr')
    .replace(/ı/g, 'i')
    .replace(/ş/g, 's')
    .replace(/ğ/g, 'g')
    .replace(/ü/g, 'u')
    .replace(/ö/g, 'o')
    .replace(/ç/g, 'c')
    .replace(/â/g, 'a')
    .replace(/î/g, 'i')
    .replace(/û/g, 'u')
    .replace(/\s+/g, ' ')
    .trim();
}

function eksikMi(d, v) {
  if (v === null || v === undefined || (typeof v === 'number' && Number.isNaN(v))) return true;
  const e = d.eksikKodlar;
  if (!e) return false;
  if (e.tekil && e.tekil.some((k) => k === v || String(k) === String(v))) return true;
  if (e.aralik && typeof v === 'number' && v >= e.aralik[0] && v <= e.aralik[1]) return true;
  return false;
}

/** Değişkenin geçerli (eksik olmayan) değerleri */
export function gecerliDegerler(d) {
  const sonuc = [];
  for (const v of d.degerler) if (!eksikMi(d, v)) sonuc.push(v);
  return sonuc;
}

export { eksikMi as degerEksikMi };

/** Hızlı özet: n, eksik, benzersiz değer sayısı, tamsayı mı, min/max, en sık değerler */
export function degiskenOzeti(d) {
  const gecerli = gecerliDegerler(d);
  const say = new Map();
  for (const v of gecerli) say.set(v, (say.get(v) || 0) + 1);
  const sayisal = d.veriTuru === 'sayi' || d.veriTuru === 'tarih';
  let tamsayi = sayisal;
  let min = Infinity;
  let max = -Infinity;
  if (sayisal) {
    for (const v of gecerli) {
      if (!Number.isInteger(v)) tamsayi = false;
      if (v < min) min = v;
      if (v > max) max = v;
    }
  }
  const enSik = [...say.entries()].sort((a, b) => b[1] - a[1]).slice(0, 10);
  return {
    n: gecerli.length,
    eksik: d.degerler.length - gecerli.length,
    benzersiz: say.size,
    tamsayi,
    min: sayisal && gecerli.length ? min : null,
    max: sayisal && gecerli.length ? max : null,
    enSik,
  };
}

// ---------- Sıralı ölçek sözlükleri ----------

const SIRALI_SOZLUKLER = [
  ['hic', 'nadiren', 'bazen', 'sik sik', 'her zaman'],
  ['hic', 'nadiren', 'bazen', 'genellikle', 'her zaman'],
  ['hic', 'az', 'orta', 'cok'],
  ['hic', 'biraz', 'orta', 'oldukca', 'cok'],
  ['yok', 'hafif', 'orta', 'siddetli', 'cok siddetli'],
  ['yok', 'hafif', 'orta', 'agir'],
  ['hafif', 'orta', 'agir'],
  ['hafif', 'orta', 'siddetli'],
  ['dusuk', 'orta', 'yuksek'],
  ['cok dusuk', 'dusuk', 'orta', 'yuksek', 'cok yuksek'],
  ['kesinlikle katilmiyorum', 'katilmiyorum', 'kararsizim', 'katiliyorum', 'kesinlikle katiliyorum'],
  ['hic katilmiyorum', 'katilmiyorum', 'kararsizim', 'katiliyorum', 'tamamen katiliyorum'],
  ['cok kotu', 'kotu', 'orta', 'iyi', 'cok iyi'],
  ['kotu', 'orta', 'iyi'],
  ['zayif', 'orta', 'iyi', 'cok iyi'],
  ['okur yazar degil', 'okur yazar', 'ilkokul', 'ortaokul', 'lise', 'universite', 'yuksek lisans', 'doktora'],
  ['okuryazar degil', 'okuryazar', 'ilkokul', 'ilkogretim', 'ortaokul', 'lise', 'on lisans', 'onlisans', 'lisans', 'universite', 'yuksekokul', 'yuksek lisans', 'lisansustu', 'doktora'],
  ['none', 'mild', 'moderate', 'severe'],
  ['low', 'medium', 'high'],
  ['low', 'moderate', 'high'],
  ['never', 'rarely', 'sometimes', 'often', 'always'],
  ['strongly disagree', 'disagree', 'neutral', 'agree', 'strongly agree'],
  ['poor', 'fair', 'good', 'very good', 'excellent'],
];

const ROMA = { i: 1, ii: 2, iii: 3, iv: 4, v: 5, vi: 6, vii: 7, viii: 8, ix: 9, x: 10 };

/**
 * Metin düzeylerinin sıralı olup olmadığını ve doğal sırasını bulur.
 * @returns {string[]|null} sıralı düzeyler ya da null
 */
export function siraliDuzeyBul(duzeyler) {
  const sade = duzeyler.map((d) => sadelestir(d).replace(/[-_.]/g, ' ').replace(/\s+/g, ' '));
  for (const sozluk of SIRALI_SOZLUKLER) {
    if (sade.every((s) => sozluk.includes(s))) {
      return duzeyler.slice().sort((a, b) => sozluk.indexOf(sade[duzeyler.indexOf(a)]) - sozluk.indexOf(sade[duzeyler.indexOf(b)]));
    }
  }
  // "Evre I", "Evre II" / "Grade 1" / "1. derece" / "I" gibi numaralı düzeyler
  const anahtarlar = sade.map((s) => {
    const m = s.match(/^(?:evre|stage|grade|derece|seviye|duzey|sinif|class|tip|type)?\s*([ivx]+|\d+)\s*(?:\.|\s)?\s*(?:evre|derece|seviye|duzey|sinif)?$/);
    if (!m) return null;
    const t = m[1];
    if (/^\d+$/.test(t)) return Number(t);
    return ROMA[t] ?? null;
  });
  if (anahtarlar.every((k) => k !== null) && new Set(anahtarlar).size === anahtarlar.length) {
    const sira = duzeyler.map((d, i) => [d, anahtarlar[i]]).sort((a, b) => a[1] - b[1]);
    return sira.map(([d]) => d);
  }
  return null;
}

// ---------- Ad ipuçları ----------

const KIMLIK_ADLARI = /^(id|no|sira ?no|sira|hasta ?no|hasta ?id|protokol( ?no)?|dosya ?no|tc( ?kimlik)?( ?no)?|kayit ?no|denek|subject|case ?id|olgu ?no|numara|#)$/;
const LIKERT_ADLARI = /(madde|soru|item|^s\d+$|^m\d+$|^q\d+$|^i\d+$|olcek|likert|skor|puan|evre|derece|grade|stage|seviye|duzey)/;
const GRUP_ADLARI = /(grup|group|kol|arm|tedavi|treatment|hasta|kontrol|olgu|case|cinsiyet|sex|gender|cins)/;

/**
 * Ölçüm düzeyi tahmini.
 * @returns {{olcum:'surekli'|'sirali'|'kategorik'|'kimlik'|'haric', guven:'yuksek'|'dusuk', gerekce:string, siraliDuzeyler?:string[]}}
 */
export function olcumTahmin(d) {
  const oz = degiskenOzeti(d);
  const ad = sadelestir(d.ad).replace(/[_\-.]+/g, ' ').trim();
  if (d.veriTuru === 'tarih') return { olcum: 'haric', guven: 'yuksek', gerekce: 'Tarih değişkeni; analizde doğrudan kullanılmaz (süre hesaplamak için "Yeni değişken" aracını kullanın).' };
  if (oz.n === 0) return { olcum: 'haric', guven: 'yuksek', gerekce: 'Tüm değerler eksik.' };

  // Dosyada ölçüm düzeyi tanımlıysa (SPSS) onu kullan
  if (d.olcum) {
    if (d.olcum === 'surekli' && d.degerEtiketleri && oz.benzersiz <= 10 && Object.keys(d.degerEtiketleri).length >= oz.benzersiz) {
      return { olcum: 'kategorik', guven: 'dusuk', gerekce: 'SPSS dosyasında "scale" tanımlı ama tüm değerlerin etiketi var; kategorik olabilir.' };
    }
    return { olcum: d.olcum, guven: 'yuksek', gerekce: 'Ölçüm düzeyi dosyada tanımlı.' };
  }

  if (oz.benzersiz === 1) return { olcum: 'haric', guven: 'yuksek', gerekce: 'Tek bir değer alıyor (sabit); analizde kullanılamaz.' };

  if (d.veriTuru === 'metin') {
    const duzeyler = [...new Set(gecerliDegerler(d))];
    if (KIMLIK_ADLARI.test(ad) || (oz.benzersiz === oz.n && oz.n > 10)) {
      return { olcum: 'kimlik', guven: KIMLIK_ADLARI.test(ad) ? 'yuksek' : 'dusuk', gerekce: 'Her satırda farklı bir değer var; kimlik ya da serbest metin olabilir.' };
    }
    if (oz.benzersiz > 30 && oz.benzersiz > oz.n * 0.5) return { olcum: 'haric', guven: 'dusuk', gerekce: 'Çok sayıda farklı metin değeri var (serbest metin).' };
    const sirali = siraliDuzeyBul(duzeyler);
    if (sirali) return { olcum: 'sirali', guven: 'yuksek', gerekce: `Düzeyler doğal bir sıra izliyor: ${sirali.join(' < ')}.`, siraliDuzeyler: sirali };
    return { olcum: 'kategorik', guven: 'yuksek', gerekce: `${oz.benzersiz} farklı kategori.` };
  }

  // Sayısal
  if (d.degerEtiketleri && Object.keys(d.degerEtiketleri).length >= 2) {
    const etiketler = Object.values(d.degerEtiketleri);
    const sirali = siraliDuzeyBul(etiketler);
    if (sirali) return { olcum: 'sirali', guven: 'yuksek', gerekce: 'Değer etiketleri sıralı bir ölçek gösteriyor.' };
    return { olcum: 'kategorik', guven: 'yuksek', gerekce: 'Değer etiketleri tanımlı (kodlanmış kategoriler).' };
  }
  if (KIMLIK_ADLARI.test(ad) && oz.tamsayi && oz.benzersiz >= oz.n * 0.95) {
    return { olcum: 'kimlik', guven: 'yuksek', gerekce: 'Ad ve değerler bir kimlik/sıra numarası gösteriyor.' };
  }
  if (oz.benzersiz === 2) return { olcum: 'kategorik', guven: 'yuksek', gerekce: 'İki farklı değer alıyor (ikili değişken).' };
  if (oz.tamsayi && oz.benzersiz <= 7) {
    if (LIKERT_ADLARI.test(ad)) return { olcum: 'sirali', guven: 'dusuk', gerekce: `${oz.benzersiz} farklı tamsayı değer; ad, sıralı bir ölçek (madde/puan/evre) gösteriyor. Kontrol edin.` };
    if (oz.benzersiz <= 5 || oz.n >= 20 * oz.benzersiz) {
      return { olcum: 'kategorik', guven: 'dusuk', gerekce: `Yalnızca ${oz.benzersiz} farklı tamsayı değer (${[...new Set(gecerliDegerler(d))].sort((a, b) => a - b).join(', ')}); kodlanmış kategori olabilir. Sayım ya da puan ise "Sürekli", sıralı ise "Sıralı" seçin.` };
    }
  }
  if (oz.tamsayi && oz.benzersiz <= 10 && LIKERT_ADLARI.test(ad)) {
    return { olcum: 'sirali', guven: 'dusuk', gerekce: 'Az sayıda tamsayı değer; sıralı ölçek olabilir.' };
  }
  return { olcum: 'surekli', guven: oz.benzersiz >= 10 ? 'yuksek' : 'dusuk', gerekce: `${oz.benzersiz} farklı sayısal değer.` };
}

/**
 * Kategorik/sıralı değişkenin düzeyleri, sırasıyla ve etiketleriyle.
 * @returns {{deger:(number|string), etiket:string, n:number}[]}
 */
export function duzeyleriBul(d, siraliDuzeyler = null) {
  const say = new Map();
  for (const v of gecerliDegerler(d)) say.set(v, (say.get(v) || 0) + 1);
  let degerler = [...say.keys()];
  if (Array.isArray(d.duzeySirasi) && d.duzeySirasi.length) {
    const sira = d.duzeySirasi.map(String);
    degerler.sort((a, b) => {
      const ia = sira.indexOf(String(a));
      const ib = sira.indexOf(String(b));
      return (ia < 0 ? 1e9 : ia) - (ib < 0 ? 1e9 : ib) || String(a).localeCompare(String(b), 'tr');
    });
  } else if (siraliDuzeyler) {
    degerler.sort((a, b) => siraliDuzeyler.indexOf(a) - siraliDuzeyler.indexOf(b));
  } else if (d.veriTuru === 'sayi') {
    degerler.sort((a, b) => a - b);
  } else {
    const sirali = siraliDuzeyBul(degerler);
    if (sirali) degerler = sirali;
    else degerler.sort((a, b) => String(a).localeCompare(String(b), 'tr'));
  }
  return degerler.map((v) => ({
    deger: v,
    etiket: d.degerEtiketleri?.[String(v)] ?? String(v),
    n: say.get(v),
  }));
}

// ---------- Analiz önerileri ----------

/** Grup değişkeni adayları, en olasıdan başlayarak */
export function grupAdaylari(veriSeti, olcumler) {
  const adaylar = [];
  for (const d of veriSeti.degiskenler) {
    const olcum = olcumler[d.ad];
    if (olcum !== 'kategorik' && olcum !== 'sirali') continue;
    const duzeyler = duzeyleriBul(d);
    if (duzeyler.length < 2 || duzeyler.length > 8) continue;
    const enKucuk = Math.min(...duzeyler.map((x) => x.n));
    if (enKucuk < 2) continue;
    let puan = 0;
    const ad = sadelestir(d.ad);
    if (GRUP_ADLARI.test(ad)) puan += 5;
    if (/^(grup|group|kol|arm)/.test(ad)) puan += 5;
    if (duzeyler.length <= 4) puan += 2;
    const ortalamaN = duzeyler.reduce((s, x) => s + x.n, 0) / duzeyler.length;
    puan += 2 * (enKucuk / ortalamaN); // dengeli gruplar
    if (olcum === 'sirali') puan -= 2;
    adaylar.push({ ad: d.ad, puan, duzeySayisi: duzeyler.length });
  }
  return adaylar.sort((a, b) => b.puan - a.puan);
}

const ZAMAN_KELIMELERI = {
  once: 0, oncesi: 0, on: 0, pre: 0, preop: 0, preoperatif: 0, baslangic: 0, bazal: 0, baseline: 0, giris: 0, ilk: 0, before: 0,
  intraop: 0.5, peroperatif: 0.5,
  sonra: 1, sonrasi: 1, son: 1, post: 1, postop: 1, postoperatif: 1, bitis: 1, cikis: 1, final: 1, after: 1, kontrol: 1, takip: 1,
};
const BIRIMLER = { saat: 1 / 24, sa: 1 / 24, s: 1 / 24, h: 1 / 24, hour: 1 / 24, dk: 1 / 1440, dakika: 1 / 1440, min: 1 / 1440, gun: 1, g: 1, d: 1, day: 1, hafta: 7, hf: 7, w: 7, week: 7, ay: 30, a: 30, m: 30, month: 30, yil: 365, y: 365, year: 365 };

function adParcala(ad) {
  return sadelestir(ad)
    .replace(/([a-z])(\d)/g, '$1 $2')
    .replace(/(\d)([a-z])/g, '$1 $2')
    .split(/[^a-z0-9]+/)
    .filter(Boolean);
}

/** Bir sütun adını (gövde imzası, zaman anahtarı) olarak çözümler; zaman belirteci yoksa null */
function zamanCozumle(ad) {
  const parcalar = adParcala(ad);
  const imza = [];
  let zaman = null;
  for (let i = 0; i < parcalar.length; i++) {
    const p = parcalar[i];
    if (/^\d+$/.test(p) && zaman === null) {
      let deger = Number(p);
      const sonraki = parcalar[i + 1];
      if (sonraki && sonraki in BIRIMLER) {
        deger *= BIRIMLER[sonraki];
        i++;
      }
      // "ay1", "hafta 2" gibi önce birim sonra sayı (tek harfli birimler burada sayılmaz: "S1" bir madde adıdır)
      const onceki = imza[imza.length - 1];
      if (onceki && onceki.length >= 2 && onceki in BIRIMLER && !(sonraki && sonraki in BIRIMLER)) {
        deger *= BIRIMLER[onceki];
        imza.pop();
      }
      zaman = deger;
      imza.push('#');
    } else if (p in ZAMAN_KELIMELERI && zaman === null) {
      zaman = ZAMAN_KELIMELERI[p];
      imza.push('#');
    } else if (/^t\d+$/.test(p) && zaman === null) {
      zaman = Number(p.slice(1));
      imza.push('#');
    } else {
      imza.push(p);
    }
  }
  if (zaman === null) return null;
  return { imza: imza.join('_'), zaman };
}

/**
 * Tekrarlı ölçüm (aynı değişkenin farklı zamanlarda ölçümü) ve ölçek maddesi kümelerini bulur.
 * @returns {{tekrarli: {ad:string, degiskenler:string[]}[], olcekMaddeleri: {ad:string, degiskenler:string[]}[]}}
 */
export function tekrarliOlcumAdaylari(veriSeti, olcumler) {
  const kumeler = new Map();
  for (const d of veriSeti.degiskenler) {
    const olcum = olcumler[d.ad];
    if (olcum === 'kimlik' || olcum === 'haric') continue;
    const z = zamanCozumle(d.ad);
    if (!z) continue;
    const anahtar = `${z.imza}|${olcum === 'surekli' ? 's' : 'k'}`;
    if (!kumeler.has(anahtar)) kumeler.set(anahtar, []);
    kumeler.get(anahtar).push({ ad: d.ad, zaman: z.zaman });
  }
  const tekrarli = [];
  const olcekMaddeleri = [];
  for (const [anahtar, uyeler] of kumeler) {
    if (uyeler.length < 2) continue;
    const zamanlar = new Set(uyeler.map((u) => u.zaman));
    if (zamanlar.size !== uyeler.length) continue;
    uyeler.sort((a, b) => a.zaman - b.zaman);
    const govde = anahtar.split('|')[0].replace(/_?#_?/g, ' ').trim() || uyeler[0].ad;
    // Görünen ad: ilk değişkenin adından zaman belirteci atılmış hali (özgün yazımıyla)
    const gorunen =
      uyeler[0].ad
        .replace(/[\s._-]*(\d+[\s._-]*(saat|sa|dk|gün|gun|hafta|ay|yıl|yil|h|d|w|m|y)?|t\d+|önce(si)?|once(si)?|sonra(sı|si)?|ön|on|son|pre(op)?|post(op)?|başlangıç|baslangic|bazal|baseline|giriş|giris|çıkış|cikis|final|kontrol|takip)$/i, '')
        .replace(/^(\d+[\s._-]*(ay|hafta|gün|gun|yıl|yil)?|t\d+|pre|post|önce|once|sonra)[\s._-]+/i, '')
        .trim() || govde;
    const kisaGovde = govde.replace(/\s/g, '').length <= 2;
    const maddeAdi = /^(madde|soru|item|s|m|q|i|md|sr)$/.test(govde.trim());
    if (uyeler.length >= 5 && (kisaGovde || maddeAdi)) {
      olcekMaddeleri.push({ ad: gorunen, degiskenler: uyeler.map((u) => u.ad) });
    } else if (uyeler.length <= 8) {
      tekrarli.push({ ad: gorunen, degiskenler: uyeler.map((u) => u.ad) });
    }
  }
  return { tekrarli, olcekMaddeleri };
}

const SURE_ADLARI = /(sure|time|takip|follow|survival|sagkalim|os|pfs|dfs|efs|ay|gun|month|day|yasam)/;
const OLAY_ADLARI = /(olay|event|status|durum|olum|exitus|mortalite|mortality|death|nuks|relaps|recurrence|progresyon|progression|vefat|sonlanim|outcome)/;

/** Sağkalım analizi için süre ve olay değişkeni adayları */
export function sagkalimAdaylari(veriSeti, olcumler) {
  const sureler = [];
  const olaylar = [];
  for (const d of veriSeti.degiskenler) {
    const ad = sadelestir(d.ad);
    const oz = degiskenOzeti(d);
    if (olcumler[d.ad] === 'surekli' && SURE_ADLARI.test(ad) && oz.min !== null && oz.min >= 0) sureler.push(d.ad);
    if ((olcumler[d.ad] === 'kategorik' || olcumler[d.ad] === 'sirali') && oz.benzersiz === 2 && OLAY_ADLARI.test(ad)) olaylar.push(d.ad);
  }
  return { sureler, olaylar };
}

const POZITIF_ADLAR = /^(var|evet|pozitif|positive|yes|hasta|olgu|vaka|case|1|true|dogru|olay|event|olum|olu|exitus|ex|nuks|relaps|komplikasyon|basarisiz|anormal|patolojik|malign|kotu|yuksek|mevcut|present)$/;

/**
 * İki kategorili bir değişkende "pozitif / olay / hasta" anlamındaki kategoriyi tahmin eder.
 * Ad eşleşmezse son düzey (0/1 kodlamada 1) döner.
 * @param {{deger, etiket}[]} duzeyler
 */
export function pozitifDuzeyTahmin(duzeyler) {
  if (!duzeyler.length) return null;
  const bulunan = duzeyler.find((x) => POZITIF_ADLAR.test(sadelestir(x.etiket)) || POZITIF_ADLAR.test(sadelestir(String(x.deger))));
  return (bulunan ?? duzeyler[duzeyler.length - 1]).deger;
}
