// İki kategorili tanı testinin altın standarda göre performansı (2×2 tablo).

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import { taniTesti } from '../../cekirdek/tani.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim } from '../kurallar.js';
import { degiskenBul, deger, duzeyler, gorunenAd } from '../veri-erisim.js';
import { pozitifDuzeyTahmin } from '../../veri/tanima.js';
import { oranKatMetni, oranMetni } from './roc.js';
import { uyariBloklari } from './ortak.js';

export function taniAnalizi(secim, vs, baglam) {
  const { ayarlar, bicim: b } = baglam;
  const testD = degiskenBul(vs, secim.test);
  const refD = degiskenBul(vs, secim.referans);
  const tp0 = String(secim.testPozitif ?? pozitifDuzeyTahmin(duzeyler(vs, secim.test, baglam)));
  const rp0 = String(secim.referansPozitif ?? pozitifDuzeyTahmin(duzeyler(vs, secim.referans, baglam)));
  let tp = 0;
  let fp = 0;
  let fn = 0;
  let tn = 0;
  let dislanan = 0;
  for (let i = 0; i < vs.satirSayisi; i++) {
    if (baglam.satirMaskesi && !baglam.satirMaskesi[i]) continue;
    const t = deger(testD, i);
    const r = deger(refD, i);
    if (t === null || r === null) {
      dislanan++;
      continue;
    }
    const tPoz = String(t) === tp0;
    const rPoz = String(r) === rp0;
    if (tPoz && rPoz) tp++;
    else if (tPoz) fp++;
    else if (rPoz) fn++;
    else tn++;
  }
  if (tp + fp + fn + tn === 0) throw new IstatistikHatasi('Analiz edilecek vaka yok.');
  const prevalansGirdi = String(secim.prevalans ?? '').trim();
  const prevalans = prevalansGirdi ? Number(prevalansGirdi.replace(',', '.')) / 100 : null;
  if (prevalans !== null && !(prevalans > 0 && prevalans < 1)) throw new IstatistikHatasi('Prevalans 0 ile 100 arasında bir yüzde olmalı.');
  const s = taniTesti(tp, fp, fn, tn, { duzey: ayarlar.guvenDuzeyi, prevalans: prevalans ?? undefined });
  const gaY = Math.round(ayarlar.guvenDuzeyi * 100);
  const testAd = gorunenAd(testD);
  const refAd = gorunenAd(refD);
  const tEt = testD.degerEtiketleri?.[tp0] ?? tp0;
  const rEt = refD.degerEtiketleri?.[rp0] ?? rp0;
  const uyarilar = [...(s.uyarilar || [])];
  if (dislanan) uyarilar.push(`${dislanan} vaka eksik değer nedeniyle dışarıda bırakıldı.`);
  const bloklar = [
    { tur: 'karar', adimlar: [adim('Tasarım', `${testAd} ("${tEt}" pozitif), ${refAd} altın standardına ("${rEt}" hasta) göre değerlendirildi; n = ${tp + fp + fn + tn}.`), adim('Yöntem', 'Oranların güven aralıkları kesin (Clopper-Pearson), olabilirlik oranlarınınki log yöntemiyle hesaplandı.')] },
    {
      tur: 'tablo',
      tablo: {
        baslik: `${testAd} ile ${refAd} çapraz tablosu`,
        basliklar: [[hucre(testAd, { sol: true, satir: 2 }), hucre(refAd, { sutun: 2 }), hucre('Toplam', { satir: 2 })], [`${rEt} (hasta)`, 'Sağlam']],
        satirlar: [
          { hucreler: [hucre(`${tEt} (pozitif)`, { sol: true }), `${tp} (DP)`, `${fp} (YP)`, String(tp + fp)] },
          { hucreler: [hucre('Negatif', { sol: true }), `${fn} (YN)`, `${tn} (DN)`, String(fn + tn)] },
          { tur: 'ara', hucreler: [hucre('Toplam', { sol: true }), String(tp + fn), String(fp + tn), String(tp + fp + fn + tn)] },
        ],
        not: 'DP: doğru pozitif; YP: yanlış pozitif; YN: yanlış negatif; DN: doğru negatif.',
        dipnotlar: [],
      },
    },
  ];
  const satirlar = [
    ['Duyarlılık, %', oranMetni(s.duyarlilik, b)],
    ['Özgüllük, %', oranMetni(s.ozgulluk, b)],
    ['Pozitif prediktif değer, %', oranMetni(s.pozitifKestirimDegeri, b)],
    ['Negatif prediktif değer, %', oranMetni(s.negatifKestirimDegeri, b)],
    ['Doğruluk, %', oranMetni(s.dogruluk, b)],
    ['Pozitif olabilirlik oranı', oranKatMetni(s.pozitifOlabilirlikOrani, b)],
    ['Negatif olabilirlik oranı', oranKatMetni(s.negatifOlabilirlikOrani, b)],
    ['Tanısal odds oranı', oranKatMetni(s.tanisalOddsOrani, b)],
    ['Youden indeksi', oranKatMetni(s.youdenJ, b)],
    ['Örneklem prevalansı, %', oranMetni(s.prevalans, b)],
  ];
  if (s.duzeltilmis) {
    satirlar.push([`PPD (prevalans %${F.sayi(prevalans * 100, 1, b)}), %`, oranMetni(s.duzeltilmis.pozitifKestirimDegeri, b)]);
    satirlar.push([`NPD (prevalans %${F.sayi(prevalans * 100, 1, b)}), %`, oranMetni(s.duzeltilmis.negatifKestirimDegeri, b)]);
  }
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Tanısal performans ölçütleri',
      basliklar: [[hucre('Ölçüt', { sol: true }), `Değer (%${gaY} GA)`]],
      satirlar: satirlar.map(([a, v]) => ({ hucreler: [hucre(a, { sol: true }), v] })),
      not: s.kappa ? `Uyum (Cohen kappa) = ${F.sayi(s.kappa.deger, 3, b)}; ${F.pEsitlik(s.kappa.p, b)}.` : '',
      dipnotlar: [],
    },
  });
  bloklar.push({ tur: 'satir', metin: `Duyarlılık %${F.sayi(s.duyarlilik.deger * 100, 1, b)} (%${gaY} GA: ${F.sayi(s.duyarlilik.gaAlt * 100, 1, b)}; ${F.sayi(s.duyarlilik.gaUst * 100, 1, b)}), özgüllük %${F.sayi(s.ozgulluk.deger * 100, 1, b)} (%${gaY} GA: ${F.sayi(s.ozgulluk.gaAlt * 100, 1, b)}; ${F.sayi(s.ozgulluk.gaUst * 100, 1, b)})` });
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `Tanı testi: ${testAd} (altın standart: ${refAd})`, bloklar, yontemler: ['tani-testi'] };
}
