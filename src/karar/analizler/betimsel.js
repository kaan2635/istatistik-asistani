// Betimsel istatistikler: sürekli değişkenlerde özet ve normallik, kategorik değişkenlerde frekans tabloları.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import { betimsel } from '../../cekirdek/betimsel.js';
import { frekansGrafigi, histogram, qqGrafigi } from '../../grafik/grafikler.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { normallikIfadesi, normallikKarari } from '../kurallar.js';
import { degiskenBul, duzeyler, etkinSatirSayisi, gorunenAd, sayisalDizi } from '../veri-erisim.js';
import { grafikSecenek, uyariBloklari } from './ortak.js';

const GRAFIK_SINIRI = 12;

export function betimselAnaliz(secim, vs, baglam) {
  const { ayarlar, bicim: b, olcumler } = baglam;
  const degiskenler = (secim.degiskenler || []).filter((a) => ['surekli', 'sirali', 'kategorik'].includes(olcumler[a]));
  if (!degiskenler.length) throw new IstatistikHatasi('Betimsel analiz için uygun değişken seçilmedi.');
  const toplamVaka = etkinSatirSayisi(vs, baglam);
  const surekliSatirlar = [];
  const frekansSatirlari = [];
  const kararAlt = [];
  const grafikler = [];
  const uyarilar = [];
  const yontemler = new Set();
  const grafikSec = grafikSecenek(baglam);
  let siraliVar = false;

  for (const ad of degiskenler) {
    const d = degiskenBul(vs, ad);
    const etiketAd = gorunenAd(d);
    const olcum = olcumler[ad];
    const benzersiz = new Set(d.degerler.filter((v) => v !== null && v !== undefined)).size;
    const surekliGibi = olcum === 'surekli' || (olcum === 'sirali' && d.veriTuru === 'sayi' && benzersiz > 7);
    try {
      if (surekliGibi) {
        const x = sayisalDizi(vs, ad, baglam);
        if (!x.length) throw new IstatistikHatasi('geçerli değer yok');
        const s = betimsel(x, { yuzdelikTipi: ayarlar.yuzdelikTipi, duzey: ayarlar.guvenDuzeyi });
        const nk = olcum === 'surekli' ? normallikKarari(x, ayarlar, b) : null;
        if (nk) {
          if (nk.sw) yontemler.add('shapiro-wilk');
          if ((nk.yontem || '').includes('Kolmogorov')) yontemler.add('lilliefors');
          if ((nk.yontem || '').includes('ıklık')) yontemler.add('carpiklik-basiklik');
          kararAlt.push({ baslik: etiketAd, adimlar: [{ baslik: nk.yontem || 'Normallik', metin: `${nk.ozet} → ${normallikIfadesi(nk)}.`, durum: nk.normal ? 'tamam' : 'uyari' }] });
        }
        const normallikMetni = nk ? (nk.normal === null ? 'sınanamadı' : `${nk.normal ? 'Normal' : 'Normal değil'}${nk.sw ? ` (SW ${F.p(nk.sw.p, b)})` : nk.ks ? ` (KS ${F.p(nk.ks.p, b)})` : ''}`) : 'Sıralı';
        surekliSatirlar.push({
          hucreler: [
            hucre(etiketAd, { sol: true }),
            String(s.n),
            String(toplamVaka - s.n),
            s.n > 1 ? F.ortSS(s.ortalama, s.standartSapma, undefined, b) : F.sayi(s.ortalama, undefined, b),
            s.n > 1 ? F.ga(s.gaAlt, s.gaUst, undefined, b) : '–',
            F.medyanAralik(s.medyan, s.q1, s.q3, undefined, b),
            F.aralik(s.minimum, s.maksimum, undefined, b),
            F.sayi(s.carpiklik, 2, b),
            F.sayi(s.basiklik, 2, b),
            normallikMetni,
          ],
        });
        if (secim.grafik && grafikler.length < GRAFIK_SINIRI) {
          grafikler.push({ tur: 'grafik', baslik: `${etiketAd} dağılımı (histogram)`, svg: histogram(x, { ...grafikSec, xEtiket: etiketAd, yEtiket: 'Frekans', normalEgri: olcum === 'surekli' }), dosyaAdi: `${ad}-histogram` });
          if (olcum === 'surekli' && x.length >= 3) grafikler.push({ tur: 'grafik', baslik: `${etiketAd} normal Q-Q grafiği`, svg: qqGrafigi(x, { ...grafikSec, xEtiket: 'Beklenen normal değer', yEtiket: etiketAd }), dosyaAdi: `${ad}-qq` });
        }
      } else {
        const dz = duzeyler(vs, ad, baglam);
        const gecerli = dz.reduce((a, x) => a + x.n, 0);
        const eksik = toplamVaka - gecerli;
        if (olcum === 'sirali') siraliVar = true;
        frekansSatirlari.push({ tur: 'ara', hucreler: [hucre(`${etiketAd}${eksik ? ` (geçerli n = ${gecerli}, eksik = ${eksik})` : ''}`, { sol: true, sutun: 1 }), '', '', ''] });
        let kumulatif = 0;
        for (const x of dz) {
          kumulatif += x.n;
          frekansSatirlari.push({
            hucreler: [hucre(x.etiket, { girinti: true, sol: true }), String(x.n), F.sayi((100 * x.n) / gecerli, 1, b), olcum === 'sirali' ? F.sayi((100 * kumulatif) / gecerli, 1, b) : ''],
          });
        }
        if (secim.grafik && grafikler.length < GRAFIK_SINIRI) {
          grafikler.push({ tur: 'grafik', baslik: `${etiketAd} dağılımı`, svg: frekansGrafigi(dz.map((x) => x.etiket), dz.map((x) => x.n), { ...grafikSec, xEtiket: etiketAd, yEtiket: 'Yüzde', yuzde: true }), dosyaAdi: `${ad}-frekans` });
        }
      }
    } catch (e) {
      if (!(e instanceof IstatistikHatasi)) throw e;
      uyarilar.push(`${etiketAd}: ${e.message}.`);
    }
  }

  const bloklar = [];
  if (kararAlt.length) bloklar.push({ tur: 'karar', baslik: 'Normallik değerlendirmesi', alt: kararAlt });
  if (surekliSatirlar.length) {
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Sürekli değişkenlerin betimsel istatistikleri',
        basliklar: [[hucre('Değişken', { sol: true }), 'n', 'Eksik', 'Ort ± SS', `%${Math.round(ayarlar.guvenDuzeyi * 100)} GA`, 'Medyan (Ç1–Ç3)', 'Min–Maks', 'Çarpıklık', 'Basıklık', 'Normallik']],
        satirlar: surekliSatirlar,
        not: `Ort ± SS: ortalama ± standart sapma; GA: ortalamanın güven aralığı; Ç1–Ç3: birinci ve üçüncü çeyreklik; SW: Shapiro-Wilk, KS: Kolmogorov-Smirnov (Lilliefors) p değeri. Çarpıklık ve basıklık SPSS ile aynı biçimde (G1, G2) hesaplanmıştır.`,
        dipnotlar: [],
      },
    });
  }
  if (frekansSatirlari.length) {
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Kategorik değişkenlerin frekans dağılımı',
        basliklar: [[hucre('Değişken / kategori', { sol: true }), 'n', '%', siraliVar ? 'Birikimli %' : '']],
        satirlar: frekansSatirlari,
        not: 'Yüzdeler geçerli (eksik olmayan) gözlemler üzerinden hesaplanmıştır.',
        dipnotlar: [],
      },
    });
  }
  bloklar.push(...grafikler);
  if (secim.grafik && grafikler.length >= GRAFIK_SINIRI) bloklar.push({ tur: 'not', metin: `Grafik sayısı ${GRAFIK_SINIRI} ile sınırlandırıldı; diğer değişkenler için analizi ayrı çalıştırın.` });
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: degiskenler.length === 1 ? `${gorunenAd(degiskenBul(vs, degiskenler[0]))}: betimsel istatistikler` : 'Betimsel istatistikler', bloklar, yontemler: [...yontemler] };
}
