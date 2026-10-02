// İki kategorik değişkenin ilişkisi: çapraz tablo, ki-kare / Fisher, risk ölçüleri, Mantel-Haenszel.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import * as K from '../../cekirdek/kategorik.js';
import { caprazCubukGrafigi } from '../../grafik/grafikler.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim, kategorikTestSec } from '../kurallar.js';
import { caprazTablo, degiskenBul, duzeyler, gorunenAd } from '../veri-erisim.js';
import { bosSatirSutunAt, kategorikTestUygula } from './grup.js';
import { TEST_ADLARI, dene, grafikSecenek, pHucresi, testSatiri, uyariBloklari } from './ortak.js';

const KAT_YONTEM = { pearson: 'pearson-kikare', yates: 'yates-kikare', fisher: 'fisher', 'fisher-freeman-halton': 'fisher-freeman-halton' };

export function caprazAnaliz(secim, vs, baglam) {
  const { ayarlar, bicim: b, olcumler } = baglam;
  const alfa = ayarlar.alfa;
  if (!secim.satir || !secim.sutun) throw new IstatistikHatasi('Satır ve sütun değişkenlerini seçin.');
  const satirAd = gorunenAd(degiskenBul(vs, secim.satir));
  const sutunAd = gorunenAd(degiskenBul(vs, secim.sutun));
  const ct = caprazTablo(vs, secim.satir, secim.sutun, baglam);
  const { tablo, satirIdx, sutunIdx } = bosSatirSutunAt(ct.tablo);
  if (tablo.length < 2 || tablo[0].length < 2) throw new IstatistikHatasi('Çapraz tabloda en az iki dolu satır ve iki dolu sütun olmalı.');
  const satirlar = satirIdx.map((i) => ct.satirlar[i]);
  const sutunlar = sutunIdx.map((j) => ct.sutunlar[j]);
  const r = tablo.length;
  const c = tablo[0].length;
  const N = ct.n;
  const yontemler = new Set();
  const uyarilar = [];
  if (ct.dislanan) uyarilar.push(`${ct.dislanan} vaka değişkenlerden en az birinde eksik olduğu için analiz dışı bırakıldı.`);

  const ts = kategorikTestSec(tablo, ayarlar, b);
  const { sonuc, kiKare: kk } = kategorikTestUygula(tablo, ts.test, b);
  yontemler.add(KAT_YONTEM[ts.test]);
  const adimlar = [adim('Tasarım', `${satirAd} (${r} kategori) × ${sutunAd} (${c} kategori), n = ${N}.`), ...ts.adimlar];
  const siraliIkisi = olcumler[secim.satir] === 'sirali' && olcumler[secim.sutun] === 'sirali';
  if (siraliIkisi && kk) adimlar.push(adim('Ek', 'Her iki değişken de sıralı olduğundan doğrusal-doğrusal ilişki (trend) testi de raporlandı.'));
  const bloklar = [{ tur: 'karar', adimlar }];

  // Çapraz tablo: n, satır %, sütun %
  const satirTop = tablo.map((s) => s.reduce((a, v) => a + v, 0));
  const sutunTop = tablo[0].map((_, j) => tablo.reduce((a, s) => a + s[j], 0));
  const yuzdeTabani = ayarlar.kategorikYuzde === 'satir' ? 'satir' : 'sutun';
  const govde = [];
  satirlar.forEach((s, i) => {
    govde.push({
      hucreler: [
        hucre(s.etiket, { sol: true }),
        ...tablo[i].map((v, j) => F.nYuzde(v, yuzdeTabani === 'satir' ? satirTop[i] : sutunTop[j], 1, b)),
        F.nYuzde(satirTop[i], yuzdeTabani === 'satir' ? satirTop[i] : N, 1, b),
      ],
    });
  });
  govde.push({ tur: 'ara', hucreler: [hucre('Toplam', { sol: true }), ...sutunTop.map((v) => F.nYuzde(v, yuzdeTabani === 'satir' ? N : v, 1, b)), F.nYuzde(N, N, 1, b)] });
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: `${satirAd} ile ${sutunAd} çapraz tablosu`,
      basliklar: [
        [hucre(satirAd, { sol: true, satir: 2 }), hucre(sutunAd, { sutun: c }), hucre('Toplam', { satir: 2 })],
        sutunlar.map((s) => s.etiket),
      ],
      satirlar: govde,
      not: `Değerler n (%) olarak verilmiştir; yüzdeler ${yuzdeTabani === 'satir' ? 'satır' : 'sütun'} içindedir. ${TEST_ADLARI[ts.test]}: ${testSatiri(sonuc, b)}.`,
      dipnotlar: [],
    },
  });

  // Test özeti
  const testSatirlari = [{ hucreler: [hucre(TEST_ADLARI[ts.test], { sol: true }), Number.isFinite(sonuc.istatistik) ? F.sayi(sonuc.istatistik, 2, b) : '–', sonuc.sd ? F.sd(sonuc.sd, b) : '–', pHucresi(sonuc.p, alfa, b)] }];
  if (kk) {
    if (ts.test !== 'pearson') testSatirlari.push({ hucreler: [hucre('Pearson ki-kare', { sol: true }), F.sayi(kk.pearson.istatistik, 2, b), F.sd(kk.pearson.sd, b), F.p(kk.pearson.p, b)] });
    if (r === 2 && c === 2 && ts.test !== 'yates') testSatirlari.push({ hucreler: [hucre('Süreklilik (Yates) düzeltmeli', { sol: true }), F.sayi(kk.yates.istatistik, 2, b), F.sd(kk.yates.sd, b), F.p(kk.yates.p, b)] });
    testSatirlari.push({ hucreler: [hucre('Olabilirlik oranı (G²)', { sol: true }), F.sayi(kk.olabilirlikOrani.istatistik, 2, b), F.sd(kk.olabilirlikOrani.sd, b), F.p(kk.olabilirlikOrani.p, b)] });
    if (siraliIkisi || r === 2 || c === 2) testSatirlari.push({ hucreler: [hucre('Doğrusal-doğrusal ilişki', { sol: true }), F.sayi(kk.dogrusalIliski.istatistik, 2, b), '1', F.p(kk.dogrusalIliski.p, b)] });
    if (ts.test !== 'fisher' && r === 2 && c === 2) testSatirlari.push({ hucreler: [hucre("Fisher'in kesin testi", { sol: true }), '–', '–', F.p(kk.fisher.p, b)] });
  }
  const etkiMetin = kk ? (r === 2 && c === 2 ? `φ = ${F.sayi(kk.etkiler.phi, 3, b)}; ` : '') + `Cramér's V = ${F.sayi(kk.etkiler.cramerV, 3, b)}; kontenjans katsayısı = ${F.sayi(kk.etkiler.kontenjansKatsayisi, 3, b)}` : '';
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Bağımsızlık testleri',
      basliklar: [[hucre('Test', { sol: true }), 'Değer', 'sd', 'p']],
      satirlar: testSatirlari,
      not: `İlk satır karar kuralına göre raporlanması gereken testtir; diğerleri bilgi içindir. Etki büyüklüğü: ${etkiMetin}.`,
      dipnotlar: [],
    },
  });

  // 2×2: risk ölçüleri
  if (r === 2 && c === 2) {
    const ro = dene(() => K.oddsOrani(tablo, { duzey: ayarlar.guvenDuzeyi }), uyarilar);
    if (ro) {
      const g = (x) => `${F.sayi(x.tahmin, 2, b)} (${F.ga(x.ga.alt, x.ga.ust, 2, b)})`;
      bloklar.push({
        tur: 'tablo',
        tablo: {
          baslik: 'Risk ölçüleri (2×2 tablo)',
          basliklar: [[hucre('Ölçü', { sol: true }), `Değer (%${Math.round(ayarlar.guvenDuzeyi * 100)} GA)`]],
          satirlar: [
            { hucreler: [hucre('Odds oranı (OR)', { sol: true }), g(ro.oddsOrani)] },
            { hucreler: [hucre(`Göreli risk (${sutunlar[0].etiket})`, { sol: true }), g(ro.goreliRisk)] },
            { hucreler: [hucre(`Risk farkı (${sutunlar[0].etiket})`, { sol: true }), `${F.sayi(ro.riskFarki.tahmin * 100, 1, b)}% (${F.ga(ro.riskFarki.newcombe.alt * 100, ro.riskFarki.newcombe.ust * 100, 1, b)})`] },
          ],
          not: `Karşılaştırma: "${satirlar[0].etiket}" satırı, "${satirlar[1].etiket}" satırına göre; olay: "${sutunlar[0].etiket}" sütunu. Farklı bir referans için Değişkenler adımında kategori sırasını değiştirin. OR için Woolf, göreli risk için log, risk farkı için Newcombe güven aralığı.`,
          dipnotlar: [],
        },
      });
      (ro.uyarilar || []).forEach((u) => uyarilar.push(u));
    }
  }

  // r×c anlamlıysa: hangi hücreler farklı?
  if (kk && (r > 2 || c > 2) && (sonuc.p < alfa || ayarlar.postHocHerZaman)) {
    const so = dene(() => K.sutunOranKarsilastirma(tablo, { alfa }), uyarilar);
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Farkın kaynağı: düzeltilmiş standart artıklar',
        basliklar: [[hucre(satirAd, { sol: true }), ...sutunlar.map((s) => s.etiket)]],
        satirlar: satirlar.map((s, i) => ({
          hucreler: [
            hucre(s.etiket, { sol: true }),
            ...tablo[i].map((_, j) => {
              const z = kk.kalintilar.duzeltilmis[i][j];
              const harf = so?.satirlar?.[i]?.harfler?.[j] ?? '';
              return hucre(`${F.sayi(z, 2, b)}`, { anlamli: Math.abs(z) > 1.96, ust: harf || undefined });
            }),
          ],
        })),
        not: '|z| > 1,96 olan hücreler (kalın) beklenenden anlamlı olarak fazla (+) ya da az (−) gözlenmiştir. Üst simgeler: aynı satırda aynı harfi taşıyan sütunların oranları arasında anlamlı fark yoktur (sütun oranları z testi, Bonferroni düzeltmeli).',
        dipnotlar: [],
      },
    });
  }

  // Mantel-Haenszel (katmanlı 2×2)
  if (secim.katman) {
    const katmanAd = gorunenAd(degiskenBul(vs, secim.katman));
    const kd = duzeyler(vs, secim.katman, baglam);
    if (r !== 2 || c !== 2) uyarilar.push('Mantel-Haenszel testi yalnızca 2×2 tablolar için yapılır.');
    else {
      const tablolar = [];
      const katmanAdlari = [];
      for (const k of kd) {
        const maske = vs.degiskenler.find((d) => d.ad === secim.katman).degerler.map((v, i) => (String(v) === String(k.deger) && (!baglam.satirMaskesi || baglam.satirMaskesi[i]) ? 1 : 0));
        const alt = caprazTablo(vs, secim.satir, secim.sutun, { ...baglam, satirMaskesi: Uint8Array.from(maske) });
        const t = satirIdx.map((i) => sutunIdx.map((j) => alt.tablo[i]?.[j] ?? 0));
        if (t.flat().reduce((a, v) => a + v, 0) > 0) {
          tablolar.push(t);
          katmanAdlari.push(k.etiket);
        }
      }
      const mh = dene(() => K.mantelHaenszel(tablolar, { duzey: ayarlar.guvenDuzeyi }), uyarilar);
      if (mh) {
        yontemler.add('mantel-haenszel');
        const satirlarMH = katmanAdlari.map((ad, i) => {
          const o = dene(() => K.oddsOrani(tablolar[i]), []);
          return { hucreler: [hucre(ad, { sol: true }), o ? `${F.sayi(o.oddsOrani.tahmin, 2, b)} (${F.ga(o.oddsOrani.ga.alt, o.oddsOrani.ga.ust, 2, b)})` : '–'] };
        });
        satirlarMH.push({ tur: 'ara', hucreler: [hucre('Ortak OR (Mantel-Haenszel)', { sol: true }), `${F.sayi(mh.ortakOddsOrani?.tahmin, 2, b)} (${F.ga(mh.ortakOddsOrani?.ga?.alt, mh.ortakOddsOrani?.ga?.ust, 2, b)})`] });
        bloklar.push({
          tur: 'tablo',
          tablo: {
            baslik: `${katmanAd} katmanlarına göre odds oranları`,
            basliklar: [[hucre(katmanAd, { sol: true }), 'OR (%95 GA)']],
            satirlar: satirlarMH,
            not: `Mantel-Haenszel χ² = ${F.sayi(mh.istatistik, 2, b)}; ${F.pEsitlik(mh.p, b)}.${mh.breslowDay ? ` Breslow-Day homojenlik testi: ${F.pEsitlik(mh.breslowDay.p, b)}${mh.breslowDay.p < alfa ? ' (katmanlar arasında OR farklı; ortak OR yorumlanmamalı)' : ''}.` : ''}`,
            dipnotlar: [],
          },
        });
      }
    }
  }

  if (secim.grafik) {
    bloklar.push({
      tur: 'grafik',
      baslik: `${sutunAd} kategorilerine göre ${satirAd} dağılımı`,
      svg: caprazCubukGrafigi(tablo, satirlar.map((s) => s.etiket), sutunlar.map((s) => s.etiket), { ...grafikSecenek(baglam), tip: 'kumelenmis', yuzde: true, yuzdeTabani, xEtiket: sutunAd, yEtiket: 'Yüzde' }),
      dosyaAdi: `${secim.satir}-${secim.sutun}`,
    });
  }
  bloklar.push({ tur: 'baslik', metin: 'Raporlama satırı' });
  bloklar.push({ tur: 'satir', metin: testSatiri(sonuc, b) });
  (sonuc.uyarilar || []).forEach((u) => uyarilar.push(u));
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `${satirAd} ile ${sutunAd} arasındaki ilişki`, bloklar, yontemler: [...yontemler] };
}
