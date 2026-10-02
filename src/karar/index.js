// Analiz yürütücüsü: bir istek (tür + seçimler) alır, ilgili analiz modülünü çalıştırır ve rapor öğesi döndürür.
//
// RaporOgesi: { baslik, bloklar: Blok[], yontemler: string[], hata?: string }
// Blok: {tur:'karar', baslik?, adimlar?, alt?:[{baslik, adimlar}]} | {tur:'tablo', tablo} | {tur:'grafik', baslik, svg, dosyaAdi}
//     | {tur:'satir', metin} | {tur:'not', metin, seviye?:'bilgi'|'uyari'} | {tur:'baslik', metin} | {tur:'metin', metin}

import { IstatistikHatasi } from '../cekirdek/hata.js';
import { betimselAnaliz } from './analizler/betimsel.js';
import { grupAnalizi } from './analizler/grup.js';
import { eslestirilmisAnaliz } from './analizler/eslestirilmis.js';
import { korelasyonAnalizi } from './analizler/korelasyon.js';
import { caprazAnaliz } from './analizler/capraz.js';
import { tekOrneklemAnalizi } from './analizler/tekorneklem.js';
import { regresyonAnalizi } from './analizler/regresyon.js';
import { rocAnalizi } from './analizler/roc.js';
import { taniAnalizi } from './analizler/tani.js';
import { sagkalimAnalizi } from './analizler/sagkalim.js';
import { uyumAnalizi } from './analizler/uyum.js';
import { guvenirlikAnalizi } from './analizler/guvenirlik.js';
import { faktorAnalizi } from './analizler/faktor.js';
import { gucAnalizi } from './analizler/guc.js';

const CALISTIRICILAR = {
  betimsel: betimselAnaliz,
  grup: grupAnalizi,
  tablo1: grupAnalizi,
  eslestirilmis: eslestirilmisAnaliz,
  korelasyon: korelasyonAnalizi,
  capraz: caprazAnaliz,
  tekorneklem: tekOrneklemAnalizi,
  regresyon: regresyonAnalizi,
  roc: rocAnalizi,
  tani: taniAnalizi,
  sagkalim: sagkalimAnalizi,
  uyum: uyumAnalizi,
  guvenirlik: guvenirlikAnalizi,
  faktor: faktorAnalizi,
  guc: gucAnalizi,
};

/**
 * @param {{tur:string, secim:object}} istek
 * @param {object} veriSeti
 * @param {{olcumler, ayarlar, bicim, satirMaskesi}} baglam
 * @returns {object} RaporOgesi
 */
export function analizCalistir(istek, veriSeti, baglam) {
  const f = CALISTIRICILAR[istek.tur];
  if (!f) return { baslik: 'Bilinmeyen analiz', bloklar: [], yontemler: [], hata: `Analiz türü tanınmadı: ${istek.tur}` };
  try {
    const sonuc = f(istek.secim || {}, veriSeti, baglam);
    return { yontemler: [], ...sonuc };
  } catch (e) {
    if (!(e instanceof IstatistikHatasi)) console.error(e);
    return {
      baslik: istek.baslik || 'Analiz yapılamadı',
      bloklar: [],
      yontemler: [],
      hata: e instanceof IstatistikHatasi ? e.message : `Beklenmeyen hata: ${e.message}`,
    };
  }
}
