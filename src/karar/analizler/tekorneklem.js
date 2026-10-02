// Tek örneklem: ortalama/medyanın bilinen bir değerle, oranların beklenen oranlarla karşılaştırılması.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import * as P from '../../cekirdek/parametrik.js';
import * as NP from '../../cekirdek/nonparametrik.js';
import * as K from '../../cekirdek/kategorik.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim, normallikIfadesi, normallikKarari } from '../kurallar.js';
import { degiskenBul, duzeyler, gorunenAd, sayisalDizi } from '../veri-erisim.js';
import { etkiMetni, istatistikHucresi, pHucresi, surekliMetin, testSatiri, uyariBloklari } from './ortak.js';

function sayiCoz(m) {
  return Number(String(m).trim().replace(',', '.'));
}

export function tekOrneklemAnalizi(secim, vs, baglam) {
  const { ayarlar, bicim: b, olcumler } = baglam;
  const alfa = ayarlar.alfa;
  const d = degiskenBul(vs, secim.degisken);
  const etiketAd = gorunenAd(d);
  const olcum = olcumler[secim.degisken];
  const uyarilar = [];
  const yontemler = new Set();
  const bloklar = [];

  if (olcum !== 'kategorik') {
    const mu = sayiCoz(secim.deger);
    if (!Number.isFinite(mu)) throw new IstatistikHatasi('Referans değeri sayı olarak girin.');
    const x = sayisalDizi(vs, secim.degisken, baglam);
    if (x.length < 3) throw new IstatistikHatasi('En az 3 geçerli gözlem gerekir.');
    const adimlar = [adim('Tasarım', `${etiketAd} (n = ${x.length}) referans değer ${F.sayi(mu, undefined, b)} ile karşılaştırılıyor.`)];
    let s;
    let parametrik = false;
    if (olcum === 'surekli') {
      const nk = normallikKarari(x, ayarlar, b);
      if (nk.sw) yontemler.add('shapiro-wilk');
      adimlar.push(adim(`Normallik${nk.yontem ? ` (${nk.yontem})` : ''}`, `${nk.ozet} → ${normallikIfadesi(nk)}.`, nk.normal ? 'tamam' : 'uyari'));
      parametrik = nk.normal === true;
    } else adimlar.push(adim('Ölçek', 'Sıralı değişken → parametrik olmayan test.'));
    if (parametrik) {
      s = P.tekOrneklemT(x, { mu, duzey: ayarlar.guvenDuzeyi });
      yontemler.add('tek-orneklem-t');
      adimlar.push(adim('Seçilen test', 'Normal dağılım → tek örneklem t testi.', 'tamam'));
    } else {
      s = NP.wilcoxonIsaretliSira(x, null, { mu, duzey: ayarlar.guvenDuzeyi });
      yontemler.add('wilcoxon');
      adimlar.push(adim('Seçilen test', 'Normal dağılım varsayımı sağlanmadı → tek örneklem Wilcoxon işaretli sıralar testi (medyan karşılaştırması).', 'tamam'));
    }
    bloklar.push({ tur: 'karar', adimlar });
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: `${etiketAd}: ${F.sayi(mu, undefined, b)} ile karşılaştırma`,
        basliklar: [[hucre('Değişken', { sol: true }), 'n', parametrik ? 'Ort ± SS' : 'Medyan (Ç1–Ç3)', 'Test', 'İstatistik', 'p', 'Etki büyüklüğü']],
        satirlar: [{ hucreler: [hucre(etiketAd, { sol: true }), String(x.length), surekliMetin(x, parametrik ? 'ortss' : 'medyan-ceyrek', b, ayarlar.yuzdelikTipi), s.ad, istatistikHucresi(s, b), pHucresi(s.p, alfa, b), etkiMetni(s.etki, b) || '–'] }],
        not: parametrik ? `Ortalama farkı ${F.sayi(s.ortalamaFarki, undefined, b)} (%95 GA: ${F.ga(s.ga.alt, s.ga.ust, undefined, b)}).` : s.hodgesLehmann ? `Hodges-Lehmann sözde medyanı ${F.sayi(s.hodgesLehmann.tahmin, undefined, b)} (%95 GA: ${F.ga(s.hodgesLehmann.ga.alt, s.hodgesLehmann.ga.ust, undefined, b)}).` : '',
        dipnotlar: [],
      },
    });
    bloklar.push({ tur: 'satir', metin: testSatiri(s, b) });
    (s.uyarilar || []).forEach((u) => uyarilar.push(u));
  } else {
    const dz = duzeyler(vs, secim.degisken, baglam);
    if (dz.length < 2) throw new IstatistikHatasi('Değişkende en az iki kategori olmalı.');
    const n = dz.reduce((a, x) => a + x.n, 0);
    let oranlar = String(secim.oranlar || '').trim() ? String(secim.oranlar).split(/[;\s]+/).filter(Boolean).map(sayiCoz) : dz.map(() => 1 / dz.length);
    if (oranlar.length !== dz.length || oranlar.some((o) => !(o > 0))) throw new IstatistikHatasi(`Beklenen oranlar ${dz.length} adet pozitif sayı olmalı (kategori sırasıyla: ${dz.map((x) => x.etiket).join(', ')}).`);
    const toplam = oranlar.reduce((a, v) => a + v, 0);
    oranlar = oranlar.map((o) => o / toplam);
    const adimlar = [adim('Tasarım', `${etiketAd}: ${dz.length} kategori, n = ${n}; beklenen oranlar ${dz.map((x, i) => `${x.etiket} %${F.sayi(oranlar[i] * 100, 1, b)}`).join(', ')}.`)];
    let s;
    if (dz.length === 2) {
      s = K.binomTesti(dz[0].n, n, oranlar[0], { duzey: ayarlar.guvenDuzeyi });
      yontemler.add('binom');
      adimlar.push(adim('Seçilen test', 'İki kategori → kesin binom testi (Clopper-Pearson güven aralığıyla).', 'tamam'));
    } else {
      s = K.uyumIyiligiKiKare(dz.map((x) => x.n), oranlar);
      yontemler.add('uyum-iyiligi');
      const kucuk = oranlar.filter((o) => o * n < 5).length;
      adimlar.push(adim('Seçilen test', `${dz.length} kategori → ki-kare uyum iyiliği testi.${kucuk ? ` ${kucuk} kategoride beklenen değer 5'ten küçük; sonuç dikkatle yorumlanmalı.` : ''}`, kucuk ? 'uyari' : 'tamam'));
    }
    bloklar.push({ tur: 'karar', adimlar });
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: `${etiketAd}: gözlenen ve beklenen dağılım`,
        basliklar: [[hucre('Kategori', { sol: true }), 'Gözlenen n (%)', 'Beklenen n (%)']],
        satirlar: dz.map((x, i) => ({ hucreler: [hucre(x.etiket, { sol: true }), F.nYuzde(x.n, n, 1, b), `${F.sayi(oranlar[i] * n, 1, b)} (${F.sayi(oranlar[i] * 100, 1, b)})`] })),
        not: `${s.ad}: ${testSatiri(s, b)}${s.ga ? `; ${dz[0].etiket} oranı %${F.sayi(s.oran * 100, 1, b)} (%95 GA: ${F.ga(s.ga.alt * 100, s.ga.ust * 100, 1, b)})` : ''}.`,
        dipnotlar: [],
      },
    });
    bloklar.push({ tur: 'satir', metin: testSatiri(s, b) });
  }
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `${etiketAd}: tek örneklem karşılaştırması`, bloklar, yontemler: [...yontemler] };
}
