// Ölçek güvenirliği: Cronbach alfa, madde istatistikleri, yarıya bölme ve McDonald omega.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import { cronbachAlfa, mcdonaldOmega, splitHalf } from '../../cekirdek/guvenirlik.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim } from '../kurallar.js';
import { degiskenBul, gorunenAd, tamVakalar } from '../veri-erisim.js';
import { dene, uyariBloklari } from './ortak.js';

function yorumAlfa(a) {
  if (a >= 0.9) return 'mükemmel';
  if (a >= 0.8) return 'iyi';
  if (a >= 0.7) return 'kabul edilebilir';
  if (a >= 0.6) return 'şüpheli';
  if (a >= 0.5) return 'zayıf';
  return 'kabul edilemez';
}

export function guvenirlikAnalizi(secim, vs, baglam) {
  const { ayarlar, bicim: b } = baglam;
  const adlar = [...new Set(secim.degiskenler || [])];
  if (adlar.length < 2) throw new IstatistikHatasi('En az iki madde seçin.');
  const etiketler = adlar.map((a) => gorunenAd(degiskenBul(vs, a)));
  const tum = tamVakalar(vs, adlar, baglam);
  if (tum.matris.length < 3) throw new IstatistikHatasi('Tüm maddeleri yanıtlanmış en az 3 katılımcı gerekir.');
  const uyarilar = [];
  if (tum.dislanan) uyarilar.push(`${tum.dislanan} katılımcı en az bir maddeyi yanıtlamadığı için dışarıda bırakıldı (liste bazında silme).`);
  const r = cronbachAlfa(tum.matris, { duzey: ayarlar.guvenDuzeyi, maddeAdlari: etiketler });
  const yarim = adlar.length >= 2 ? dene(() => splitHalf(tum.matris), uyarilar) : null;
  const omega = adlar.length >= 3 ? dene(() => mcdonaldOmega(tum.matris, { maddeAdlari: etiketler }), uyarilar) : null;
  (r.uyarilar || []).forEach((u) => uyarilar.push(u));
  (omega?.uyarilar || []).forEach((u) => uyarilar.push(`Omega: ${u}`));
  const gaY = Math.round(ayarlar.guvenDuzeyi * 100);
  const negatif = r.maddeler.filter((m) => m.duzeltilmisMaddeToplamKorelasyonu < 0);
  const dusuk = r.maddeler.filter((m) => m.duzeltilmisMaddeToplamKorelasyonu >= 0 && m.duzeltilmisMaddeToplamKorelasyonu < 0.3);
  const artiran = r.maddeler.filter((m) => m.alfaSilinirse > r.alfa + 0.01);
  const adimlar = [
    adim('Tasarım', `${r.n} katılımcı, ${r.maddeSayisi} madde.`),
    adim('Yöntem', `İç tutarlılık Cronbach alfa (Feldt güven aralığıyla)${omega ? ', tek faktörlü modele dayalı McDonald omega' : ''} ve yarıya bölme (Spearman-Brown, Guttman) katsayılarıyla değerlendirildi; maddeler düzeltilmiş madde-toplam korelasyonu ve madde silindiğinde alfa değeriyle incelendi.`),
  ];
  if (negatif.length) adimlar.push(adim('Ters madde', `${negatif.map((m) => m.ad).join(', ')} maddesinin toplamla korelasyonu negatif; ters puanlanması gerekiyor olabilir. Ters puanlamadan alfa yorumlanmamalıdır.`, 'uyari'));
  if (dusuk.length) adimlar.push(adim('Düşük ayırt edicilik', `${dusuk.map((m) => m.ad).join(', ')}: düzeltilmiş madde-toplam korelasyonu 0,30'un altında.`, 'uyari'));

  const bloklar = [{ tur: 'karar', adimlar }];
  const ozetSatirlari = [
    [`Cronbach alfa (%${gaY} GA)`, `${F.sayi(r.alfa, 3, b)} (${F.ga(r.ga.alt, r.ga.ust, 3, b)})`],
    ['Standartlaştırılmış alfa', F.sayi(r.standartAlfa, 3, b)],
    ['Ortalama maddeler arası korelasyon', F.sayi(r.ortalamaMaddeArasiKorelasyon, 3, b)],
  ];
  if (omega) ozetSatirlari.push(['McDonald omega (ω)', F.sayi(omega.omega, 3, b)]);
  if (yarim) {
    ozetSatirlari.push(['Yarılar arası korelasyon', F.sayi(yarim.parcalarArasiKorelasyon, 3, b)]);
    ozetSatirlari.push(['Spearman-Brown katsayısı', F.sayi(yarim.parcalarArasiKorelasyon === undefined ? NaN : yarim.parca1.maddeSayisi === yarim.parca2.maddeSayisi ? yarim.spearmanBrownEsit : yarim.spearmanBrownEsitOlmayan, 3, b)]);
    ozetSatirlari.push(['Guttman iki yarı katsayısı', F.sayi(yarim.guttman, 3, b)]);
  }
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Güvenirlik katsayıları',
      basliklar: [[hucre('Katsayı', { sol: true }), 'Değer']],
      satirlar: ozetSatirlari.map(([a, v]) => ({ hucreler: [hucre(a, { sol: true }), v] })),
      not: `n = ${r.n}, madde sayısı = ${r.maddeSayisi}. Alfa yorumu (George ve Mallery): ${yorumAlfa(r.alfa)} (≥0,9 mükemmel, ≥0,8 iyi, ≥0,7 kabul edilebilir).${yarim ? ` Yarıya bölme: ilk ${yarim.parca1.maddeSayisi} madde ve son ${yarim.parca2.maddeSayisi} madde (SPSS ile aynı).` : ''}`,
      dipnotlar: [],
    },
  });
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Madde-toplam istatistikleri',
      basliklar: [[hucre('Madde', { sol: true }), 'Ort ± SS', 'Madde silinirse ölçek ortalaması', 'Madde silinirse ölçek varyansı', 'Düzeltilmiş madde-toplam r', 'Kareli çoklu korelasyon', 'Madde silinirse alfa']],
      satirlar: r.maddeler.map((m) => ({
        hucreler: [
          hucre(m.ad, { sol: true }),
          F.ortSS(m.ortalama, m.ss, 2, b),
          F.sayi(m.olcekOrtalamasiSilinirse, 2, b),
          F.sayi(m.olcekVaryansiSilinirse, 2, b),
          hucre(F.sayi(m.duzeltilmisMaddeToplamKorelasyonu, 3, b), { anlamli: m.duzeltilmisMaddeToplamKorelasyonu < 0.3 }),
          F.sayi(m.karesiAlinmisCokluKorelasyon, 3, b),
          hucre(F.sayi(m.alfaSilinirse, 3, b), { anlamli: m.alfaSilinirse > r.alfa + 0.01 }),
        ],
      })),
      not: `Kalın değerler: madde-toplam korelasyonu 0,30'un altında olan ya da silindiğinde alfayı belirgin artıran (${artiran.length ? artiran.map((m) => m.ad).join(', ') : 'yok'}) maddeler. Madde çıkarma kararı yalnızca istatistiğe değil, ölçeğin kuramsal yapısına da dayanmalıdır.`,
      dipnotlar: [],
    },
  });
  bloklar.push({ tur: 'satir', metin: `Cronbach α = ${F.sayi(r.alfa, 3, b)} (%${gaY} GA: ${F.ga(r.ga.alt, r.ga.ust, 3, b)})${omega ? `; McDonald ω = ${F.sayi(omega.omega, 3, b)}` : ''}` });
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `Ölçek güvenirliği (${adlar.length} madde)`, bloklar, yontemler: ['cronbach-alfa'] };
}
