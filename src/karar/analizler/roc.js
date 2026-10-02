// ROC analizi: AUC (DeLong), en iyi kesim değeri (Youden), tanısal ölçütler ve belirteçlerin karşılaştırılması.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import { rocAnalizi as rocHesapla, rocKarsilastir } from '../../cekirdek/roc.js';
import { pDuzelt } from '../../cekirdek/duzeltme.js';
import { rocGrafigi } from '../../grafik/grafikler.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim } from '../kurallar.js';
import { degiskenBul, deger, duzeyler, gorunenAd } from '../veri-erisim.js';
import { pozitifDuzeyTahmin } from '../../veri/tanima.js';
import { dene, grafikSecenek, pHucresi, uyariBloklari } from './ortak.js';

function yorumAUC(auc) {
  if (auc >= 0.9) return 'mükemmel';
  if (auc >= 0.8) return 'iyi';
  if (auc >= 0.7) return 'orta';
  if (auc >= 0.6) return 'zayıf';
  return 'başarısız';
}

export function oranMetni(o, b) {
  if (!o || !Number.isFinite(o.deger)) return '–';
  return `${F.sayi(o.deger * 100, 1, b)} (${F.sayi(o.gaAlt * 100, 1, b)}; ${F.sayi(o.gaUst * 100, 1, b)})`;
}

export function oranKatMetni(o, b) {
  if (!o || !Number.isFinite(o.deger)) return '–';
  return `${F.sayi(o.deger, 2, b)} (${Number.isFinite(o.gaAlt) ? F.sayi(o.gaAlt, 2, b) : '–'}; ${Number.isFinite(o.gaUst) ? F.sayi(o.gaUst, 2, b) : '–'})`;
}

export function rocAnalizi(secim, vs, baglam) {
  const { ayarlar, bicim: b } = baglam;
  const alfa = ayarlar.alfa;
  const durumD = degiskenBul(vs, secim.durum);
  const pozitif = String(secim.pozitif ?? pozitifDuzeyTahmin(duzeyler(vs, secim.durum, baglam)));
  const pozitifEtiket = durumD.degerEtiketleri?.[pozitif] ?? pozitif;
  const belirtecler = [...new Set(secim.belirtecler || [])].filter((a) => a !== secim.durum);
  if (!belirtecler.length) throw new IstatistikHatasi('En az bir ölçüm seçin.');
  const uyarilar = [];
  const yontemler = new Set(['roc', 'delong', 'youden']);
  const sonuclar = [];
  const gaY = Math.round(ayarlar.guvenDuzeyi * 100);

  const veriAl = (ad) => {
    const d = degiskenBul(vs, ad);
    const x = [];
    const y = [];
    const satir = [];
    for (let i = 0; i < vs.satirSayisi; i++) {
      if (baglam.satirMaskesi && !baglam.satirMaskesi[i]) continue;
      const v = deger(d, i);
      const s = deger(durumD, i);
      if (typeof v !== 'number' || s === null) continue;
      x.push(v);
      y.push(String(s) === pozitif ? 1 : 0);
      satir.push(i);
    }
    return { x, y, satir };
  };

  for (const ad of belirtecler) {
    const etiket = gorunenAd(degiskenBul(vs, ad));
    const { x, y, satir } = veriAl(ad);
    const nH = y.filter((v) => v === 1).length;
    if (nH === 0 || nH === y.length) {
      uyarilar.push(`${etiket}: hasta ya da sağlam grupta hiç gözlem yok.`);
      continue;
    }
    const r = dene(() => rocHesapla(x, y, { yon: 'otomatik', duzey: ayarlar.guvenDuzeyi }), uyarilar, `${etiket}: `);
    if (r) sonuclar.push({ ad, etiket, r, x, y, satir });
  }
  if (!sonuclar.length) throw new IstatistikHatasi('ROC analizi yapılabilecek ölçüm yok.');

  const n = sonuclar[0].r.nHasta + sonuclar[0].r.nSaglam;
  const adimlar = [
    adim('Tasarım', `Gerçek durum: ${gorunenAd(durumD)}; "${pozitifEtiket}" pozitif (hasta) kabul edildi. ${sonuclar.map((s) => `${s.etiket}: ${s.r.nHasta} hasta, ${s.r.nSaglam} sağlam`).join('; ')}.`),
    adim('Yöntem', 'Eğri altında kalan alan (AUC) ve güven aralığı DeLong yöntemiyle hesaplandı; AUC = 0,5 hipotezi z testiyle sınandı. En iyi kesim değeri Youden indeksini (duyarlılık + özgüllük − 1) en büyük yapan değerdir; tanısal ölçütlerin güven aralıkları kesin (Clopper-Pearson), olabilirlik oranlarınınki log yöntemiyle hesaplandı.'),
  ];
  for (const s of sonuclar) {
    if (s.r.yon === 'kucuk-pozitif') adimlar.push(adim(`${s.etiket} yönü`, 'Hastalarda değerler daha düşük olduğundan küçük değerler pozitif kabul edildi (kesim değerinin altı hasta).', 'uyari'));
  }
  const bloklar = [{ tur: 'karar', adimlar }];

  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'ROC analizi sonuçları',
      basliklar: [[hucre('Ölçüm', { sol: true }), `AUC (%${gaY} GA)`, 'SH', 'p', 'Yorum', 'Kesim değeri', `Duyarlılık, % (%${gaY} GA)`, `Özgüllük, % (%${gaY} GA)`, 'Youden J']],
      satirlar: sonuclar.map(({ etiket, r }) => {
        const yo = r.enIyiYouden;
        const m = yo?.metrikler;
        return {
          hucreler: [
            hucre(etiket, { sol: true }),
            `${F.sayi(r.auc, 3, b)} (${F.ga(r.gaAlt, r.gaUst, 3, b)})`,
            F.sayi(r.sh, 3, b),
            pHucresi(r.pDeLong, alfa, b),
            yorumAUC(r.auc),
            yo ? `${r.yon === 'kucuk-pozitif' ? '≤' : '≥'} ${F.uyarlanmis(yo.esik, b)}` : '–',
            oranMetni(m?.duyarlilik, b),
            oranMetni(m?.ozgulluk, b),
            yo ? F.sayi(yo.youdenJ, 3, b) : '–',
          ],
        };
      }),
      not: `AUC: eğri altında kalan alan; SH: DeLong standart hatası; p: AUC = 0,5 hipotezi (DeLong, MedCalc ile aynı). SPSS'in sıfır hipotezi altındaki standart hatayla hesapladığı p: ${sonuclar.map(({ etiket, r }) => `${etiket} ${F.p(r.pH0, b)}`).join(', ')}. AUC yorumu: 0,9–1 mükemmel, 0,8–0,9 iyi, 0,7–0,8 orta, 0,6–0,7 zayıf.`,
      dipnotlar: [],
    },
  });

  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'En iyi kesim değerinde tanısal ölçütler',
      basliklar: [[hucre('Ölçüm', { sol: true }), 'Kesim', `PPD, % (%${gaY} GA)`, `NPD, % (%${gaY} GA)`, `+OO (%${gaY} GA)`, `−OO (%${gaY} GA)`, `Doğruluk, % (%${gaY} GA)`]],
      satirlar: sonuclar.map(({ etiket, r }) => {
        const m = r.enIyiYouden?.metrikler;
        return {
          hucreler: [
            hucre(etiket, { sol: true }),
            r.enIyiYouden ? `${r.yon === 'kucuk-pozitif' ? '≤' : '≥'} ${F.uyarlanmis(r.enIyiYouden.esik, b)}` : '–',
            oranMetni(m?.pozitifKestirimDegeri, b),
            oranMetni(m?.negatifKestirimDegeri, b),
            oranKatMetni(m?.pozitifOlabilirlikOrani, b),
            oranKatMetni(m?.negatifOlabilirlikOrani, b),
            oranMetni(m?.dogruluk, b),
          ],
        };
      }),
      not: `PPD/NPD: pozitif/negatif prediktif değer (örneklemdeki prevalans %${F.sayi((100 * sonuclar[0].r.nHasta) / n, 1, b)} için); +OO/−OO: pozitif/negatif olabilirlik oranı.`,
      dipnotlar: [],
    },
  });
  sonuclar.forEach(({ r }) => (r.enIyiYouden?.esitler?.length > 1 ? uyarilar.push(`Youden indeksi birden çok kesim değerinde aynı en büyük değeri alıyor (${r.enIyiYouden.esitler.map((e) => F.uyarlanmis(e.esik, b)).join(', ')}); ilki raporlandı.`) : null));

  // ---- Karşılaştırma ----
  const raporSatirlari = sonuclar.map(({ etiket, r }) => `${etiket}: AUC = ${F.sayi(r.auc, 3, b)} (%${gaY} GA: ${F.ga(r.gaAlt, r.gaUst, 3, b)}); ${F.pEsitlik(r.pDeLong, b)}`);
  if (sonuclar.length >= 2) {
    const satirlar = [];
    const ham = [];
    for (let i = 0; i < sonuclar.length; i++) {
      for (let j = i + 1; j < sonuclar.length; j++) {
        const A = sonuclar[i];
        const B = sonuclar[j];
        // her iki ölçümün de dolu olduğu ortak vakalar
        const ortak = A.satir.filter((s) => B.satir.includes(s));
        const xa = ortak.map((s) => A.x[A.satir.indexOf(s)]);
        const xb = ortak.map((s) => B.x[B.satir.indexOf(s)]);
        const yy = ortak.map((s) => A.y[A.satir.indexOf(s)]);
        const k = dene(() => rocKarsilastir(xa, xb, yy, { yon1: A.r.yon, yon2: B.r.yon, duzey: ayarlar.guvenDuzeyi }), uyarilar);
        if (!k) continue;
        ham.push(k.p);
        satirlar.push({ ad: `${A.etiket} – ${B.etiket}`, k, n: ortak.length });
      }
    }
    const duz = pDuzelt(ham, 'bonferroni');
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'AUC değerlerinin karşılaştırılması (DeLong testi)',
        basliklar: [[hucre('Karşılaştırma', { sol: true }), 'n', `AUC farkı (%${gaY} GA)`, 'z', 'p', ...(satirlar.length > 1 ? ['p (Bonferroni)'] : [])]],
        satirlar: satirlar.map((s, i) => ({ hucreler: [hucre(s.ad, { sol: true }), String(s.n), `${F.sayi(s.k.fark, 3, b)} (${F.ga(s.k.gaAlt, s.k.gaUst, 3, b)})`, F.sayi(s.k.istatistik, 2, b), pHucresi(s.k.p, alfa, b), ...(satirlar.length > 1 ? [pHucresi(duz[i], alfa, b)] : [])] })),
        not: 'Eşleştirilmiş (aynı hastalarda) ROC eğrileri DeLong yöntemiyle karşılaştırılmıştır; yalnızca iki ölçümün de dolu olduğu hastalar kullanılmıştır.',
        dipnotlar: [],
      },
    });
    satirlar.forEach((s) => raporSatirlari.push(`${s.ad}: AUC farkı = ${F.sayi(s.k.fark, 3, b)}; z = ${F.sayi(s.k.istatistik, 2, b)}; ${F.pEsitlik(s.k.p, b)}`));
  }

  if (secim.grafik) {
    bloklar.push({
      tur: 'grafik',
      baslik: `${sonuclar.length > 1 ? 'ROC eğrileri' : 'ROC eğrisi'} (${pozitifEtiket} tanısı)`,
      svg: rocGrafigi(
        sonuclar.map(({ etiket, r }) => ({ ad: etiket, noktalar: r.noktalar.map((p) => ({ fpr: 1 - p.ozgulluk, tpr: p.duyarlilik })), auc: r.auc, aucGa: [r.gaAlt, r.gaUst] })),
        { ...grafikSecenek(baglam), xEtiket: '1 − Özgüllük', yEtiket: 'Duyarlılık', optimum: sonuclar.map(({ r }) => (r.enIyiYouden ? { fpr: 1 - r.enIyiYouden.ozgulluk, tpr: r.enIyiYouden.duyarlilik } : null)).filter(Boolean) },
      ),
      dosyaAdi: 'roc',
    });
  }
  bloklar.push({ tur: 'baslik', metin: 'Raporlama satırları' });
  raporSatirlari.forEach((m) => bloklar.push({ tur: 'satir', metin: m }));
  sonuclar.forEach(({ r }) => (r.uyarilar || []).forEach((u) => uyarilar.push(u)));
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `ROC analizi: ${gorunenAd(durumD)} (${pozitifEtiket})`, bloklar, yontemler: [...yontemler] };
}
