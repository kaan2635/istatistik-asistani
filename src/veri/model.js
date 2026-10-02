// Veri seti modeli: tüm içe aktarıcıların (CSV, Excel, SPSS .sav, yapıştırma) ürettiği ortak yapı.
// Diğer modüller bu şekle güvenir; alan eklemek/çıkarmak için önce bu dosyadaki JSDoc güncellenmelidir.
import { IstatistikHatasi } from '../cekirdek/hata.js';

/**
 * @typedef {'csv'|'excel'|'sav'|'yapistir'|'proje'} VeriKaynagi
 */

/**
 * SPSS kullanıcı tanımlı eksik değerleri ("user-missing").
 * `tekil`: tek tek eksik sayılan kodlar (sayısal değişkende sayı, metin değişkeninde sağdan kırpılmış metin).
 * `aralik`: [alt, üst] kapalı aralık (yalnızca sayısal); SPSS'teki LO/HI uçları -Infinity/Infinity olur.
 * @typedef {{ tekil: (number|string)[], aralik: [number, number]|null }} EksikKodlar
 */

/**
 * Bir sütun (değişken).
 * @typedef {Object} Degisken
 * @property {string} ad            Benzersiz, boş olmayan ad ("Yaş", "Yaş_2", boş başlık → "Değişken3").
 * @property {string} etiket        SPSS değişken etiketi; yoksa ''.
 * @property {'sayi'|'metin'|'tarih'} veriTuru
 * @property {(number|string|null)[]} degerler  'sayi' ve 'tarih' için sayı (tarih = 1970-01-01 UTC'den
 *   bu yana milisaniye; saat dilimi içermeyen "duvar saati" UTC kabul edilir), 'metin' için metin; null = eksik.
 *   Kullanıcı tanımlı eksik kodlar (ör. 99) null'a çevrilmez, ham değer olarak durur; bkz. eksikMi().
 * @property {'surekli'|'sirali'|'kategorik'|null} olcum  SPSS ölçüm düzeyi (scale/ordinal/nominal); bilinmiyorsa null.
 * @property {Record<string,string>|null} degerEtiketleri  Anahtar String(kod): {"1":"Erkek","2":"Kadın"}.
 * @property {EksikKodlar|null} eksikKodlar  SPSS kullanıcı tanımlı eksik değer tanımı.
 * @property {number|null} ondalik  Gösterim ondalık basamağı (SPSS biçiminden ya da veriden çıkarılır).
 * @property {number} kaynakSutun   Kaynak dosyadaki özgün sütun sırası (0'dan başlar).
 */

/**
 * @typedef {Object} VeriSeti
 * @property {string} ad
 * @property {VeriKaynagi} kaynak
 * @property {number} satirSayisi   Her değişkenin `degerler` uzunluğu.
 * @property {Degisken[]} degiskenler
 * @property {string[]} uyarilar    Kullanıcıya gösterilecek içe aktarma uyarıları (Türkçe).
 */

const KAYNAKLAR = new Set(['csv', 'excel', 'sav', 'yapistir', 'proje']);
const TURLER = new Set(['sayi', 'metin', 'tarih']);
const OLCUMLER = new Set(['surekli', 'sirali', 'kategorik']);

/**
 * Boş bir veri seti oluşturur.
 * @param {string} [ad]
 * @param {VeriKaynagi} [kaynak]
 * @returns {VeriSeti}
 */
export function bosVeriSeti(ad = '', kaynak = 'yapistir') {
  if (!KAYNAKLAR.has(kaynak)) throw new IstatistikHatasi(`Geçersiz veri kaynağı: ${kaynak}`);
  return { ad: String(ad), kaynak, satirSayisi: 0, degiskenler: [], uyarilar: [] };
}

/**
 * Eksik alanları varsayılanlarla doldurarak bir Degisken nesnesi kurar (yeni nesne döndürür).
 * @param {Partial<Degisken> & {ad: string, degerler: (number|string|null)[]}} o
 * @returns {Degisken}
 */
export function yeniDegisken(o) {
  const veriTuru = o.veriTuru ?? 'sayi';
  if (!TURLER.has(veriTuru)) throw new IstatistikHatasi(`Geçersiz veri türü: ${veriTuru}`);
  const olcum = o.olcum ?? null;
  if (olcum !== null && !OLCUMLER.has(olcum)) throw new IstatistikHatasi(`Geçersiz ölçüm düzeyi: ${olcum}`);
  return {
    ad: String(o.ad ?? ''),
    etiket: o.etiket ?? '',
    veriTuru,
    degerler: o.degerler ?? [],
    olcum,
    degerEtiketleri: o.degerEtiketleri ?? null,
    eksikKodlar: o.eksikKodlar ?? null,
    ondalik: o.ondalik ?? null,
    kaynakSutun: o.kaynakSutun ?? 0,
  };
}

/** Türkçe kurallarıyla büyük/küçük harf duyarsız karşılaştırma anahtarı (İ→i, I→ı). */
export function adAnahtari(ad) {
  return String(ad).trim().toLocaleLowerCase('tr');
}

/**
 * `istenen` adı, `kullanilan` kümesindekilerle (adAnahtari ile) çakışmayacak biçimde benzersizleştirir:
 * "Yaş" → "Yaş_2" → "Yaş_3". Boş ad → `bosAd`. Kümeyi değiştirmez.
 * @param {string} istenen
 * @param {Set<string>} kullanilan  adAnahtari() değerlerinden oluşan küme
 * @param {string} [bosAd]
 */
export function benzersizAd(istenen, kullanilan, bosAd = 'Değişken') {
  let temel = String(istenen ?? '').replace(/[\r\n\t]+/g, ' ').trim();
  if (temel === '') temel = bosAd;
  if (!kullanilan.has(adAnahtari(temel))) return temel;
  for (let k = 2; ; k++) {
    const aday = `${temel}_${k}`;
    if (!kullanilan.has(adAnahtari(aday))) return aday;
  }
}

/**
 * Veri setine değişken ekler. Girdiyi değiştirmez; yeni VeriSeti döndürür (diğer değişken nesneleri paylaşılır).
 * Ad çakışırsa "_2", "_3" eklenir. İlk değişkende satirSayisi belirlenir; sonrakiler aynı uzunlukta olmalıdır.
 * @param {VeriSeti} veriSeti
 * @param {Partial<Degisken> & {ad: string, degerler: (number|string|null)[]}} degisken
 * @returns {VeriSeti}
 */
export function degiskenEkle(veriSeti, degisken) {
  const d = yeniDegisken(degisken);
  const bos = veriSeti.degiskenler.length === 0;
  if (!bos && d.degerler.length !== veriSeti.satirSayisi) {
    throw new IstatistikHatasi(
      `"${d.ad}" değişkeninde ${d.degerler.length} değer var; veri setinde ${veriSeti.satirSayisi} satır bulunuyor.`,
    );
  }
  const kullanilan = new Set(veriSeti.degiskenler.map((v) => adAnahtari(v.ad)));
  d.ad = benzersizAd(d.ad, kullanilan, `Değişken${veriSeti.degiskenler.length + 1}`);
  if (degisken.kaynakSutun === undefined) d.kaynakSutun = veriSeti.degiskenler.length;
  return {
    ...veriSeti,
    satirSayisi: bos ? d.degerler.length : veriSeti.satirSayisi,
    degiskenler: [...veriSeti.degiskenler, d],
    uyarilar: [...veriSeti.uyarilar],
  };
}

function eksikKodKopya(e) {
  return e ? { tekil: [...e.tekil], aralik: e.aralik ? [e.aralik[0], e.aralik[1]] : null } : null;
}

/** Bir değişkenin derin kopyası. */
export function degiskenKopyala(d) {
  return {
    ...d,
    degerler: d.degerler.slice(),
    degerEtiketleri: d.degerEtiketleri ? { ...d.degerEtiketleri } : null,
    eksikKodlar: eksikKodKopya(d.eksikKodlar),
  };
}

/**
 * Veri setinin derin kopyası (değer dizileri dahil).
 * @param {VeriSeti} veriSeti
 * @returns {VeriSeti}
 */
export function kopyala(veriSeti) {
  return {
    ...veriSeti,
    degiskenler: veriSeti.degiskenler.map(degiskenKopyala),
    uyarilar: [...veriSeti.uyarilar],
  };
}

/**
 * Değer eksik mi? null/undefined/NaN ve SPSS kullanıcı tanımlı eksik kodlar (tekil kodlar ve aralık)
 * eksik sayılır. SPSS'te olduğu gibi metin kodları sağdaki boşluklar yok sayılarak birebir karşılaştırılır
 * (haven::read_sav(user_na = TRUE) sonrası is.na() ile aynı).
 * @param {Degisken} degisken
 * @param {number|string|null|undefined} deger
 */
export function eksikMi(degisken, deger) {
  if (deger === null || deger === undefined) return true;
  if (typeof deger === 'number' && Number.isNaN(deger)) return true;
  const e = degisken.eksikKodlar;
  if (!e) return false;
  if (typeof deger === 'number') {
    if (e.aralik && deger >= e.aralik[0] && deger <= e.aralik[1]) return true;
    for (const k of e.tekil) if (k === deger) return true;
    return false;
  }
  const s = String(deger).replace(/[ \0]+$/, '');
  for (const k of e.tekil) if (typeof k === 'string' && k.replace(/[ \0]+$/, '') === s) return true;
  return false;
}

/**
 * i. satırın analizde kullanılacak değeri: eksikse (null ya da kullanıcı tanımlı eksik) null, değilse değerin kendisi.
 * @param {Degisken} degisken
 * @param {number} i
 * @returns {number|string|null}
 */
export function etkinDeger(degisken, i) {
  const x = degisken.degerler[i];
  return eksikMi(degisken, x) ? null : x;
}

/** Tüm satırların etkin değerleri (kullanıcı tanımlı eksikler null'a çevrilmiş yeni dizi). */
export function etkinDegerler(degisken) {
  if (!degisken.eksikKodlar) return degisken.degerler.map((x) => (x === undefined || (typeof x === 'number' && Number.isNaN(x)) ? null : x));
  return degisken.degerler.map((x) => (eksikMi(degisken, x) ? null : x));
}
