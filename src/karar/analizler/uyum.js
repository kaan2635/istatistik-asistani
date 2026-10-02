// Ölçüm uyumu: kategorik ölçümlerde kappa, sürekli ölçümlerde ICC, Bland-Altman ve Lin uyum korelasyonu.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import * as U from '../../cekirdek/uyum.js';
import { blandAltmanGrafigi } from '../../grafik/grafikler.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim } from '../kurallar.js';
import { degiskenBul, duzeyler, gorunenAd, tamVakalar } from '../veri-erisim.js';
import { dene, grafikSecenek, pHucresi, uyariBloklari } from './ortak.js';

const MODEL_ADI = { 'tek-yonlu': 'tek yönlü rastgele', 'iki-yonlu-rastgele': 'iki yönlü rastgele', 'iki-yonlu-karma': 'iki yönlü karma' };

export function uyumAnalizi(secim, vs, baglam) {
  const { ayarlar, bicim: b, olcumler } = baglam;
  const alfa = ayarlar.alfa;
  const adlar = [...new Set(secim.degiskenler || [])];
  if (adlar.length < 2) throw new IstatistikHatasi('En az iki ölçüm sütunu seçin.');
  const etiketler = adlar.map((a) => gorunenAd(degiskenBul(vs, a)));
  const turler = new Set(adlar.map((a) => olcumler[a]));
  if (turler.size > 1) throw new IstatistikHatasi('Seçilen ölçümler aynı türde olmalı.');
  const tur = [...turler][0];
  const gaY = Math.round(ayarlar.guvenDuzeyi * 100);
  const uyarilar = [];
  const yontemler = new Set();
  const bloklar = [];
  const raporSatirlari = [];
  const kategorik = tur === 'kategorik' || (tur === 'sirali' && adlar.every((a) => degiskenBul(vs, a).veriTuru !== 'sayi' || duzeyler(vs, a, baglam).length <= 10));
  const tum = tamVakalar(vs, adlar, baglam, { sayisal: !kategorik });
  const n = tum.matris.length;
  if (n < 3) throw new IstatistikHatasi('Tüm ölçümleri dolu en az 3 vaka gerekir.');
  if (tum.dislanan) uyarilar.push(`${tum.dislanan} vaka en az bir ölçümü eksik olduğu için dışarıda bırakıldı.`);
  const adimlar = [adim('Tasarım', `${n} vaka, ${adlar.length} gözlemci/yöntem (${etiketler.join(', ')}); ölçüm türü: ${kategorik ? (tur === 'sirali' ? 'sıralı' : 'kategorik') : 'sürekli'}.`)];

  if (kategorik) {
    // Ortak düzey sırası
    const sira = [];
    for (const a of adlar) for (const x of duzeyler(vs, a, baglam)) if (!sira.some((s) => String(s) === String(x.deger))) sira.push(x.deger);
    if (adlar.length === 2) {
      const r1 = tum.matris.map((s) => s[0]);
      const r2 = tum.matris.map((s) => s[1]);
      const agirliklar = tur === 'sirali' ? ['yok', 'dogrusal', 'kuadratik'] : ['yok'];
      const sonuclar = agirliklar.map((ag) => ({ ag, r: dene(() => U.cohenKappa(r1, r2, { agirlik: ag, duzey: ayarlar.guvenDuzeyi, duzeyler: sira }), uyarilar) })).filter((x) => x.r);
      yontemler.add('cohen-kappa');
      if (tur === 'sirali') yontemler.add('agirlikli-kappa');
      adimlar.push(adim('Seçilen yöntem', tur === 'sirali' ? 'İki gözlemci, sıralı kategoriler → Cohen kappa ile birlikte ağırlıklı kappa (doğrusal ve kuadratik ağırlık); sıralı ölçümler için ağırlıklı kappa raporlanmalıdır.' : 'İki gözlemci, kategorik ölçüm → Cohen kappa.', 'tamam'));
      const ilk = sonuclar[0]?.r;
      if (ilk) {
        bloklar.push({
          tur: 'tablo',
          tablo: {
            baslik: `${etiketler[0]} × ${etiketler[1]} çapraz tablosu`,
            basliklar: [[hucre(`${etiketler[0]} \\ ${etiketler[1]}`, { sol: true }), ...ilk.duzeyler.map((d) => String(degiskenBul(vs, adlar[0]).degerEtiketleri?.[String(d)] ?? d))]],
            satirlar: ilk.tablo.map((s, i) => ({ hucreler: [hucre(String(degiskenBul(vs, adlar[0]).degerEtiketleri?.[String(ilk.duzeyler[i])] ?? ilk.duzeyler[i]), { sol: true }), ...s.map(String)] })),
            not: `Gözlenen uyum %${F.sayi(ilk.gozlenenUyum * 100, 1, b)}; şansa bağlı beklenen uyum %${F.sayi(ilk.beklenenUyum * 100, 1, b)}.`,
            dipnotlar: [],
          },
        });
      }
      bloklar.push({
        tur: 'tablo',
        tablo: {
          baslik: 'Gözlemciler arası uyum (kappa)',
          basliklar: [[hucre('Yöntem', { sol: true }), `κ (%${gaY} GA)`, 'SH', 'z', 'p', 'Yorum']],
          satirlar: sonuclar.map(({ r }) => ({ hucreler: [hucre(r.ad, { sol: true }), `${F.sayi(r.kappa, 3, b)} (${F.ga(r.ga.alt, r.ga.ust, 3, b)})`, F.sayi(r.sh, 3, b), F.sayi(r.istatistik, 2, b), pHucresi(r.p, alfa, b), r.yorum] })),
          not: 'SH: asimptotik standart hata (güven aralığı için); z ve p sıfır hipotezi (κ = 0) altındaki standart hatayla hesaplanmıştır. Yorum: Landis ve Koch (1977) — <0 zayıf, 0–0,20 önemsiz, 0,21–0,40 hafif, 0,41–0,60 orta, 0,61–0,80 iyi, 0,81–1 çok iyi.',
          dipnotlar: [],
        },
      });
      sonuclar.forEach(({ r }) => raporSatirlari.push(`${r.ad}: κ = ${F.sayi(r.kappa, 3, b)} (%${gaY} GA: ${F.ga(r.ga.alt, r.ga.ust, 3, b)}); ${F.pEsitlik(r.p, b)}`));
      sonuclar.forEach(({ r }) => (r.uyarilar || []).forEach((u) => uyarilar.push(u)));
    } else {
      const r = U.fleissKappa(tum.matris, { duzey: ayarlar.guvenDuzeyi });
      yontemler.add('fleiss-kappa');
      adimlar.push(adim('Seçilen yöntem', `${adlar.length} gözlemci, kategorik ölçüm → Fleiss kappa.${tur === 'sirali' ? ' (Fleiss kappa sıralamayı dikkate almaz; sürekliye yakın ölçümlerde ICC düşünülebilir.)' : ''}`, 'tamam'));
      bloklar.push({
        tur: 'tablo',
        tablo: {
          baslik: 'Gözlemciler arası uyum (Fleiss kappa)',
          basliklar: [[hucre('Kategori', { sol: true }), 'κ', 'SH', 'z', 'p']],
          satirlar: [
            ...r.kategoriler.map((k) => ({ hucreler: [hucre(String(k.kategori), { sol: true }), F.sayi(k.kappa, 3, b), F.sayi(k.sh, 3, b), F.sayi(k.z, 2, b), F.p(k.p, b)] })),
            { tur: 'ara', hucreler: [hucre('Genel', { sol: true }), `${F.sayi(r.kappa, 3, b)} (${F.ga(r.ga.alt, r.ga.ust, 3, b)})`, F.sayi(r.sh, 3, b), F.sayi(r.istatistik, 2, b), pHucresi(r.p, alfa, b)] },
          ],
          not: `Genel kappa için %${gaY} güven aralığı parantez içindedir. Yorum (Landis ve Koch): ${r.yorum}.`,
          dipnotlar: [],
        },
      });
      raporSatirlari.push(`Fleiss κ = ${F.sayi(r.kappa, 3, b)} (%${gaY} GA: ${F.ga(r.ga.alt, r.ga.ust, 3, b)}); ${F.pEsitlik(r.p, b)}`);
    }
  } else {
    // ---- Sürekli ölçümler ----
    const model = secim.iccModel || 'iki-yonlu-rastgele';
    const tip = secim.iccTip || 'uyum';
    const birim = secim.iccBirim || 'tek';
    const r = U.icc(tum.matris, { model, tip, birim, duzey: ayarlar.guvenDuzeyi });
    yontemler.add('icc');
    adimlar.push(
      adim(
        'Seçilen yöntem',
        `Sürekli ölçüm → sınıf içi korelasyon katsayısı: ${MODEL_ADI[model]} model, ${tip === 'uyum' ? 'mutlak uyum' : 'tutarlılık'}, ${birim === 'tek' ? 'tek ölçüm' : 'ortalama ölçüm'} (McGraw-Wong ${r.mcGrawWong}; Shrout-Fleiss ${r.shroutFleiss}).${adlar.length === 2 ? ' İki yöntem arasındaki sistematik fark ve uyum sınırları Bland-Altman analiziyle, uyum Lin katsayısıyla da değerlendirildi.' : ''}`,
        'tamam',
      ),
    );
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Sınıf içi korelasyon katsayısı (ICC)',
        basliklar: [[hucre('Birim', { sol: true }), `ICC (%${gaY} GA)`, 'F', 'sd', 'p', 'Yorum']],
        satirlar: r.satirlar.map((s) => ({ hucreler: [hucre(`${s.birim === 'tek' ? 'Tek ölçüm' : 'Ortalama ölçüm'} (${s.mcGrawWong})`, { sol: true, kalin: s.birim === birim }), `${F.sayi(s.icc, 3, b)} (${F.ga(s.gaAlt, s.gaUst, 3, b)})`, F.sayi(s.F, 2, b), `${F.sd(s.sd1, b)}, ${F.sd(s.sd2, b)}`, pHucresi(s.p, alfa, b), ''] })),
        not: `Model: ${MODEL_ADI[model]}, ${tip === 'uyum' ? 'mutlak uyum' : 'tutarlılık'} (SPSS Reliability "ICC" ile aynı tanım). Seçilen birim kalın. Yorum (Koo ve Li, 2016): <0,50 zayıf, 0,50–0,75 orta, 0,75–0,90 iyi, >0,90 mükemmel; seçilen ICC için: ${r.yorum}.`,
        dipnotlar: [],
      },
    });
    raporSatirlari.push(`ICC = ${F.sayi(r.icc, 3, b)} (%${gaY} GA: ${F.ga(r.ga.alt, r.ga.ust, 3, b)}); F(${F.sd(r.sd[0], b)}, ${F.sd(r.sd[1], b)}) = ${F.sayi(r.istatistik, 2, b)}; ${F.pEsitlik(r.p, b)}`);
    (r.uyarilar || []).forEach((u) => uyarilar.push(u));

    if (adlar.length === 2) {
      const x = tum.matris.map((s) => s[0]);
      const y = tum.matris.map((s) => s[1]);
      const ba = dene(() => U.blandAltman(x, y, { duzey: ayarlar.guvenDuzeyi }), uyarilar);
      const ccc = dene(() => U.linCCC(x, y, { duzey: ayarlar.guvenDuzeyi }), uyarilar);
      if (ba) {
        yontemler.add('bland-altman');
        const g = (a) => `${F.sayi(a[0], 2, b)}; ${F.sayi(a[1], 2, b)}`;
        bloklar.push({
          tur: 'tablo',
          tablo: {
            baslik: `Bland-Altman analizi (${etiketler[0]} − ${etiketler[1]})`,
            basliklar: [[hucre('Ölçüt', { sol: true }), 'Değer', `%${gaY} GA`]],
            satirlar: [
              { hucreler: [hucre('Ortalama fark (yanlılık)', { sol: true }), F.sayi(ba.bias, 2, b), F.ga(ba.biasGa.alt, ba.biasGa.ust, 2, b)] },
              { hucreler: [hucre('Farkların standart sapması', { sol: true }), F.sayi(ba.ssFark, 2, b), ''] },
              { hucreler: [hucre('Alt uyum sınırı', { sol: true }), F.sayi(ba.altSinir, 2, b), g(ba.altSinirGa.kesin)] },
              { hucreler: [hucre('Üst uyum sınırı', { sol: true }), F.sayi(ba.ustSinir, 2, b), g(ba.ustSinirGa.kesin)] },
              { hucreler: [hucre('Oransal yanlılık (eğim)', { sol: true }), `${F.sayi(ba.oransalYanlilik.egim, 3, b)} (${F.pEsitlik(ba.oransalYanlilik.p, b)})`, F.ga(ba.oransalYanlilik.egimGaAlt, ba.oransalYanlilik.egimGaUst, 3, b)] },
              { hucreler: [hucre('Uyum sınırları dışındaki ölçüm', { sol: true }), `${ba.disaridaSayi} (%${F.sayi(ba.disaridaYuzde, 1, b)})`, ''] },
            ],
            not: `Uyum sınırları: ortalama fark ± 1,96 × SS. Uyum sınırlarının güven aralıkları kesin yöntemle (Carkeet, 2015) hesaplanmıştır. Yanlılığın sıfırdan farkı: t(${ba.sd}) = ${F.sayi(ba.istatistik, 2, b)}; ${F.pEsitlik(ba.p, b)}. Oransal yanlılık: farkların ortalamaya göre doğrusal regresyonunun eğimi.`,
            dipnotlar: [],
          },
        });
        raporSatirlari.push(`Bland-Altman: yanlılık = ${F.sayi(ba.bias, 2, b)} (%${gaY} GA: ${F.ga(ba.biasGa.alt, ba.biasGa.ust, 2, b)}); uyum sınırları ${F.sayi(ba.altSinir, 2, b)} ile ${F.sayi(ba.ustSinir, 2, b)}`);
        if (secim.grafik) {
          bloklar.push({
            tur: 'grafik',
            baslik: `Bland-Altman grafiği: ${etiketler[0]} ile ${etiketler[1]}`,
            svg: blandAltmanGrafigi(ba.noktalar.map((p) => p.ortalama), ba.noktalar.map((p) => p.fark), { ...grafikSecenek(baglam), bias: ba.bias, loaAlt: ba.altSinir, loaUst: ba.ustSinir, xEtiket: 'İki ölçümün ortalaması', yEtiket: `Fark (${etiketler[0]} − ${etiketler[1]})` }),
            dosyaAdi: 'bland-altman',
          });
        }
      }
      if (ccc) {
        yontemler.add('lin-ccc');
        bloklar.push({
          tur: 'tablo',
          tablo: {
            baslik: 'Lin uyum korelasyon katsayısı',
            basliklar: [[hucre('Ölçüt', { sol: true }), 'Değer']],
            satirlar: [
              { hucreler: [hucre(`ρc (%${gaY} GA)`, { sol: true }), `${F.sayi(ccc.ccc, 3, b)} (${F.ga(ccc.ga.alt, ccc.ga.ust, 3, b)})`] },
              { hucreler: [hucre('Kesinlik (Pearson r)', { sol: true }), F.sayi(ccc.kesinlik, 3, b)] },
              { hucreler: [hucre('Doğruluk (Cb)', { sol: true }), F.sayi(ccc.dogruluk, 3, b)] },
            ],
            not: `ρc = r × Cb. Yorum: ${ccc.yorum}.`,
            dipnotlar: [],
          },
        });
        raporSatirlari.push(`Lin ρc = ${F.sayi(ccc.ccc, 3, b)} (%${gaY} GA: ${F.ga(ccc.ga.alt, ccc.ga.ust, 3, b)})`);
      }
    }
  }
  bloklar.unshift({ tur: 'karar', adimlar });
  bloklar.push({ tur: 'baslik', metin: 'Raporlama satırları' });
  raporSatirlari.forEach((m) => bloklar.push({ tur: 'satir', metin: m }));
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `Ölçüm uyumu: ${etiketler.join(', ')}`, bloklar, yontemler: [...yontemler] };
}
