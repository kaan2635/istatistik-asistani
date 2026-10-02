// Korelasyon analizi: korelasyon matrisi, ikili ayrıntılar, kısmi korelasyon.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import * as KO from '../../cekirdek/korelasyon.js';
import { korelasyonIsiHaritasi, sacilimGrafigi } from '../../grafik/grafikler.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim, normallikIfadesi, normallikKarari } from '../kurallar.js';
import { degiskenBul, deger, gorunenAd, sayisalDizi, tamVakalar } from '../veri-erisim.js';
import { dene, grafikSecenek, pHucresi, uyariBloklari } from './ortak.js';

const YONTEM_ADI = { pearson: 'Pearson korelasyonu', spearman: 'Spearman sıra korelasyonu', kendall: 'Kendall tau-b' };
const SEMBOL = { pearson: 'r', spearman: 'ρ', kendall: 'τ' };

function yildiz(p) {
  if (!Number.isFinite(p)) return '';
  if (p < 0.001) return '***';
  if (p < 0.01) return '**';
  if (p < 0.05) return '*';
  return '';
}

/** İki değişkenin ikisinin de dolu olduğu satırlar (filtre dahil) */
function ikiliVeri(vs, a, c, baglam) {
  const da = degiskenBul(vs, a);
  const dc = degiskenBul(vs, c);
  const x = [];
  const y = [];
  for (let i = 0; i < vs.satirSayisi; i++) {
    if (baglam.satirMaskesi && !baglam.satirMaskesi[i]) continue;
    const u = deger(da, i);
    const v = deger(dc, i);
    if (typeof u === 'number' && typeof v === 'number') {
      x.push(u);
      y.push(v);
    }
  }
  return { x, y };
}

export function korelasyonAnalizi(secim, vs, baglam) {
  const { ayarlar, bicim: b, olcumler } = baglam;
  const alfa = ayarlar.alfa;
  const adlar = [...new Set(secim.degiskenler || [])].filter((a) => ['surekli', 'sirali'].includes(olcumler[a]));
  if (adlar.length < 2) throw new IstatistikHatasi('Korelasyon için en az iki sürekli ya da sıralı değişken seçin.');
  const etiketler = adlar.map((a) => gorunenAd(degiskenBul(vs, a)));
  const kontrol = [...new Set(secim.kontrol || [])].filter((a) => !adlar.includes(a));
  const yontemler = new Set();
  const uyarilar = [];
  const adimlar = [];

  // Yöntem: tüm değişkenler için ortak
  let yontem = ayarlar.korelasyonYontemi ?? 'otomatik';
  if (yontem === 'otomatik') {
    if (adlar.some((a) => olcumler[a] === 'sirali')) {
      yontem = 'spearman';
      adimlar.push(adim('Yöntem', 'Değişkenlerden en az biri sıralı → Spearman sıra korelasyonu.', 'tamam'));
    } else {
      const satirlar = [];
      let hepsi = true;
      adlar.forEach((a, i) => {
        const nk = normallikKarari(sayisalDizi(vs, a, baglam), ayarlar, b);
        if (nk.sw) yontemler.add('shapiro-wilk');
        if (nk.normal !== true) hepsi = false;
        satirlar.push(`${etiketler[i]}: ${nk.ozet} → ${normallikIfadesi(nk)}`);
      });
      adimlar.push(adim('Normallik', satirlar.join('. ') + '.', hepsi ? 'tamam' : 'uyari'));
      yontem = hepsi ? 'pearson' : 'spearman';
      adimlar.push(adim('Yöntem', hepsi ? 'Tüm değişkenler normal dağılıyor → Pearson korelasyonu.' : 'En az bir değişken normal dağılmıyor → tüm çiftler için Spearman sıra korelasyonu (tutarlılık için tek yöntem).', 'tamam'));
    }
  } else adimlar.push(adim('Yöntem', `Ayarlarda ${YONTEM_ADI[yontem]} seçili.`));
  yontemler.add(yontem);

  const bloklar = [{ tur: 'karar', adimlar }];
  const s = SEMBOL[yontem];

  if (kontrol.length) {
    // ---- Kısmi korelasyon (Pearson tabanlı; Spearman için sıralar üzerinden) ----
    const kontrolEtiket = kontrol.map((a) => gorunenAd(degiskenBul(vs, a)));
    const tum = tamVakalar(vs, [...adlar, ...kontrol], baglam);
    if (tum.matris.length < kontrol.length + 4) throw new IstatistikHatasi('Kısmi korelasyon için yeterli tam vaka yok.');
    if (tum.dislanan) uyarilar.push(`${tum.dislanan} vaka eksik değer nedeniyle analiz dışı bırakıldı (liste bazında).`);
    yontemler.add('kismi-korelasyon');
    const satirlar = [];
    for (let i = 0; i < adlar.length; i++) {
      for (let j = i + 1; j < adlar.length; j++) {
        const sut = (k) => tum.matris.map((r) => r[k]);
        const kontroller = kontrol.map((_, m) => sut(adlar.length + m));
        const r = dene(() => KO.kismiKorelasyon(sut(i), sut(j), kontroller, { yontem, duzey: ayarlar.guvenDuzeyi }), uyarilar);
        if (!r) continue;
        satirlar.push({ hucreler: [hucre(`${etiketler[i]} – ${etiketler[j]}`, { sol: true }), F.sayi(r.r, 3, b), F.ga(r.ga?.alt, r.ga?.ust, 3, b), F.sd(r.sd, b), pHucresi(r.p, alfa, b)] });
      }
    }
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: `Kısmi korelasyonlar (kontrol: ${kontrolEtiket.join(', ')})`,
        basliklar: [[hucre('Değişken çifti', { sol: true }), 'Kısmi r', '%95 GA', 'sd', 'p']],
        satirlar,
        not: `n = ${tum.matris.length}. ${yontem === 'spearman' ? 'Kısmi korelasyon sıralar üzerinden (Spearman) hesaplanmıştır.' : ''}`,
        dipnotlar: [],
      },
    });
  }

  // ---- Korelasyon matrisi (ikili silme) ----
  const diziler = adlar.map((a) => {
    const d = degiskenBul(vs, a);
    const sonuc = [];
    for (let i = 0; i < vs.satirSayisi; i++) {
      if (baglam.satirMaskesi && !baglam.satirMaskesi[i]) continue;
      const v = deger(d, i);
      sonuc.push(typeof v === 'number' ? v : null);
    }
    return sonuc;
  });
  const M = KO.korelasyonMatrisi(diziler, { yontem, eksik: 'ikili' });
  const k = adlar.length;
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: `${YONTEM_ADI[yontem]} matrisi`,
      basliklar: [[hucre('Değişken', { sol: true }), ...adlar.map((_, j) => String(j + 1))]],
      satirlar: adlar.map((_, i) => ({
        hucreler: [hucre(`${i + 1}. ${etiketler[i]}`, { sol: true }), ...adlar.map((__, j) => (j < i ? `${F.sayi(M.r[i][j], 2, b)}${yildiz(M.p[i][j])}` : j === i ? '1' : ''))],
      })),
      not: `${s}: korelasyon katsayısı. * p < 0,05; ** p < 0,01; *** p < 0,001. Eksik değerler ikili olarak dışarıda bırakılmıştır.`.replace(/0,0/g, b.ondalikAyrac === '.' ? '0.0' : '0,0'),
      dipnotlar: [],
    },
  });

  // ---- İkili ayrıntılar ----
  const ayrinti = [];
  const raporSatirlari = [];
  const f = yontem === 'pearson' ? KO.pearson : yontem === 'spearman' ? KO.spearman : KO.kendallTauB;
  const ciftler = [];
  for (let i = 0; i < k; i++) for (let j = i + 1; j < k; j++) ciftler.push([i, j]);
  for (const [i, j] of ciftler.slice(0, 190)) {
    const { x, y } = ikiliVeri(vs, adlar[i], adlar[j], baglam);
    const r = dene(() => f(x, y, { duzey: ayarlar.guvenDuzeyi }), uyarilar, `${etiketler[i]} – ${etiketler[j]}: `);
    if (!r) continue;
    const deg = r.etki?.deger ?? r.r ?? r.rho ?? r.tau;
    ayrinti.push({ hucreler: [hucre(`${etiketler[i]} – ${etiketler[j]}`, { sol: true }), String(r.n), F.sayi(deg, 3, b), r.ga ? F.ga(r.ga.alt, r.ga.ust, 3, b) : '–', pHucresi(r.p, alfa, b), r.etki?.yorum ?? ''] });
    raporSatirlari.push(`${etiketler[i]} – ${etiketler[j]}: ${s}(${r.n - 2}) = ${F.sayi(deg, 2, b)}; ${F.pEsitlik(r.p, b)}`);
  }
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'İkili korelasyonlar ve güven aralıkları',
      basliklar: [[hucre('Değişken çifti', { sol: true }), 'n', s, `%${Math.round(ayarlar.guvenDuzeyi * 100)} GA`, 'p', 'Büyüklük']],
      satirlar: ayrinti,
      not: `Güven aralıkları Fisher z dönüşümüyle${yontem === 'spearman' ? ' (Bonett-Wright standart hatası)' : ''} hesaplanmıştır. Büyüklük yorumu Cohen (1988): 0,10 küçük, 0,30 orta, 0,50 büyük.`,
      dipnotlar: [],
    },
  });

  if (secim.grafik) {
    const g = grafikSecenek(baglam);
    if (k >= 3) bloklar.push({ tur: 'grafik', baslik: 'Korelasyon ısı haritası', svg: korelasyonIsiHaritasi(M.r, etiketler, { ...g, p: M.p }), dosyaAdi: 'korelasyon-isi-haritasi' });
    if (k <= 6) {
      for (const [i, j] of ciftler.slice(0, 15)) {
        const { x, y } = ikiliVeri(vs, adlar[i], adlar[j], baglam);
        if (x.length < 3) continue;
        bloklar.push({ tur: 'grafik', baslik: `${etiketler[i]} ile ${etiketler[j]} saçılım grafiği`, svg: sacilimGrafigi(x, y, { ...g, xEtiket: etiketler[i], yEtiket: etiketler[j], regresyonCizgisi: yontem === 'pearson', gaBandi: yontem === 'pearson' }), dosyaAdi: `${adlar[i]}-${adlar[j]}` });
      }
    }
  }
  bloklar.push({ tur: 'baslik', metin: 'Raporlama satırları' });
  raporSatirlari.forEach((m) => bloklar.push({ tur: 'satir', metin: m }));
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: k === 2 ? `${etiketler[0]} ile ${etiketler[1]} arasındaki korelasyon` : 'Korelasyon analizi', bloklar, yontemler: [...yontemler] };
}
