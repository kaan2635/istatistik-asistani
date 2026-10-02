// Sağkalım analizi: Kaplan-Meier, log-rank ailesi testler, Cox regresyonu ve orantılı tehlikeler varsayımı.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import { ciftliLogRank, kaplanMeier, kaplanMeierGruplu, logRank } from '../../cekirdek/sagkalim.js';
import { coxPHVarsayimi, coxRegresyon } from '../../cekirdek/cox.js';
import { modelMatrisi } from '../../cekirdek/model.js';
import { kaplanMeierGrafigi, ormanGrafigi } from '../../grafik/grafikler.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim } from '../kurallar.js';
import { degiskenBul, deger, duzeyler, gorunenAd } from '../veri-erisim.js';
import { dene, grafikSecenek, pHucresi, uyariBloklari } from './ortak.js';
import { pozitifDuzeyTahmin } from '../../veri/tanima.js';

function guzelAdim(maks) {
  if (!(maks > 0)) return 1;
  const kaba = maks / 5;
  const us = 10 ** Math.floor(Math.log10(kaba));
  const m = kaba / us;
  return (m <= 1 ? 1 : m <= 2 ? 2 : m <= 2.5 ? 2.5 : m <= 5 ? 5 : 10) * us;
}

/** Tahminci tanımı (model matrisi için) */
export function tahminci(vs, ad, olcumler, satirlar, baglam) {
  const d = degiskenBul(vs, ad);
  const olcum = olcumler[ad];
  const sayisalSirali = olcum === 'sirali' && d.veriTuru === 'sayi';
  const tur = olcum === 'surekli' || sayisalSirali ? 'surekli' : 'kategorik';
  const degerler = satirlar.map((i) => deger(d, i));
  const t = { ad: gorunenAd(d), tur, degerler };
  if (tur === 'kategorik') {
    const dz = duzeyler(vs, ad, baglam);
    t.duzeyler = dz.map((x) => x.deger);
    t.referans = dz[0]?.deger;
    t.etiketler = Object.fromEntries(dz.map((x) => [String(x.deger), x.etiket]));
  }
  return t;
}

/** Model matrisi sütun adını okunur yapar: "evre=II" → "Evre: II (ref. I)" */
export function sutunEtiketi(mm, j, tahminciler) {
  const s = mm.sutunlar[j];
  const t = tahminciler.find((x) => x.ad === s.terim);
  if (!t || t.tur !== 'kategorik' || s.duzey === null || s.duzey === undefined) return mm.sutunAdlari[j];
  const et = (v) => t.etiketler?.[String(v)] ?? String(v);
  return `${t.ad}: ${et(s.duzey)} (ref. ${et(t.referans)})`;
}

export function sagkalimAnalizi(secim, vs, baglam) {
  const { ayarlar, bicim: b, olcumler } = baglam;
  const alfa = ayarlar.alfa;
  const sureD = degiskenBul(vs, secim.sure);
  const olayD = degiskenBul(vs, secim.olay);
  const grupD = secim.grup ? degiskenBul(vs, secim.grup) : null;
  const olayDuzeyi = String(secim.olayDuzeyi ?? pozitifDuzeyTahmin(duzeyler(vs, secim.olay, baglam)));
  const olayEtiketi = olayD.degerEtiketleri?.[olayDuzeyi] ?? olayDuzeyi;
  const satirlar = [];
  const sure = [];
  const olay = [];
  const grup = [];
  let dislanan = 0;
  let negatif = 0;
  for (let i = 0; i < vs.satirSayisi; i++) {
    if (baglam.satirMaskesi && !baglam.satirMaskesi[i]) continue;
    const t = deger(sureD, i);
    const o = deger(olayD, i);
    const g = grupD ? deger(grupD, i) : 'Tümü';
    if (typeof t !== 'number' || o === null || g === null) {
      dislanan++;
      continue;
    }
    if (t < 0) {
      negatif++;
      continue;
    }
    satirlar.push(i);
    sure.push(t);
    olay.push(String(o) === olayDuzeyi ? 1 : 0);
    grup.push(grupD ? String(g) : 'Tümü');
  }
  if (sure.length < 3) throw new IstatistikHatasi('Sağkalım analizi için yeterli gözlem yok.');
  const toplamOlay = olay.reduce((a, v) => a + v, 0);
  if (toplamOlay === 0) throw new IstatistikHatasi(`Hiç olay ("${olayEtiketi}") yok; sağkalım eğrisi hesaplanamaz. Olay kategorisini kontrol edin.`);
  const uyarilar = [];
  if (dislanan) uyarilar.push(`${dislanan} vaka süre, olay${grupD ? ' ya da grup' : ''} bilgisi eksik olduğu için dışarıda bırakıldı.`);
  if (negatif) uyarilar.push(`${negatif} vakada süre negatif olduğu için dışarıda bırakıldı; veriyi kontrol edin.`);
  const yontemler = new Set(['kaplan-meier']);
  const sureAd = gorunenAd(sureD);
  const grupAd = grupD ? gorunenAd(grupD) : null;
  const zamanlar = String(secim.zamanlar || '')
    .split(/[;\s]+/)
    .map((s) => Number(s.replace(',', '.')))
    .filter((x) => Number.isFinite(x) && x > 0);
  const gaY = Math.round(ayarlar.guvenDuzeyi * 100);
  const adimlar = [
    adim('Tasarım', `${sure.length} vaka, ${toplamOlay} olay ("${olayEtiketi}"), ${sure.length - toplamOlay} sansürlü gözlem; süre değişkeni: ${sureAd}.`),
    adim('Yöntem', `Sağkalım olasılıkları Kaplan-Meier yöntemiyle, standart hatalar Greenwood formülüyle, güven aralıkları log dönüşümüyle hesaplandı.${grupD ? ' Gruplar log-rank (Mantel-Cox) testiyle karşılaştırıldı; erken ve geç farklara duyarlı Breslow ve Tarone-Ware testleri de raporlandı.' : ''}`),
  ];

  // ---- Kaplan-Meier ----
  const grupSirasi = grupD ? duzeyler(vs, secim.grup, baglam).map((x) => String(x.deger)).filter((g) => grup.includes(g)) : ['Tümü'];
  const etiket = (g) => (grupD ? (grupD.degerEtiketleri?.[g] ?? g) : 'Tüm hastalar');
  const km = grupD ? kaplanMeierGruplu(sure, olay, grup, { duzey: ayarlar.guvenDuzeyi, gaTipi: 'log', zamanlar, duzeyler: grupSirasi }) : { gruplar: [{ grup: 'Tümü', ...kaplanMeier(sure, olay, { duzey: ayarlar.guvenDuzeyi, gaTipi: 'log', zamanlar }) }] };
  const kmSatirlari = km.gruplar.map((g) => {
    const med = g.medyan;
    const medMetin = med && Number.isFinite(med.deger) ? `${F.sayi(med.deger, 1, b)} (${Number.isFinite(med.gaAlt) ? F.sayi(med.gaAlt, 1, b) : '–'}; ${Number.isFinite(med.gaUst) ? F.sayi(med.gaUst, 1, b) : 'ulaşılmadı'})` : 'ulaşılmadı';
    const ko = g.kisitliOrtalama;
    return {
      hucreler: [
        hucre(etiket(g.grup), { sol: true }),
        String(g.n),
        F.nYuzde(g.olaySayisi, g.n, 1, b),
        medMetin,
        ko ? `${F.sayi(ko.deger, 1, b)} ± ${F.sayi(ko.sh, 1, b)}` : '–',
        ...zamanlar.map((t) => {
          const z = (g.zamanlarda || []).find((x) => x.zaman === t);
          return z ? `${F.sayi(z.sagkalim * 100, 1, b)} (${F.sayi(z.gaAlt * 100, 1, b)}; ${F.sayi(z.gaUst * 100, 1, b)})` : '–';
        }),
      ],
    };
  });
  const bloklar = [];
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: `Kaplan-Meier sağkalım özeti${grupAd ? ` (${grupAd})` : ''}`,
      basliklar: [[hucre(grupAd || '', { sol: true }), 'n', 'Olay, n (%)', `Medyan sağkalım (%${gaY} GA)`, 'Kısıtlı ortalama ± SH', ...zamanlar.map((t) => `${F.sayi(t, Number.isInteger(t) ? 0 : 1, b)}. zamanda sağkalım, % (%${gaY} GA)`)]],
      satirlar: kmSatirlari,
      not: `Süreler ${sureAd} biriminde. Medyan sağkalım: sağkalımın %50'nin altına indiği ilk zaman; "ulaşılmadı": izlem süresince %50'ye inmedi. Kısıtlı ortalama, ${F.sayi(km.gruplar[0].kisitliOrtalama?.ustSinir, 1, b)} zamanına kadar hesaplanmıştır.`,
      dipnotlar: [],
    },
  });

  // ---- Log-rank ve diğer testler ----
  const raporSatirlari = [];
  if (grupD && grupSirasi.length >= 2) {
    const testler = [
      ['log-rank', 'Log-rank (Mantel-Cox)'],
      ['breslow', 'Breslow (genelleştirilmiş Wilcoxon)'],
      ['tarone-ware', 'Tarone-Ware'],
    ].map(([agirlik, ad]) => ({ ad, r: dene(() => logRank(sure, olay, grup, { agirlik, duzeyler: grupSirasi }), uyarilar) }));
    yontemler.add('log-rank');
    const lr = testler[0].r;
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Sağkalım eğrilerinin karşılaştırılması',
        basliklar: [[hucre('Test', { sol: true }), 'χ²', 'sd', 'p']],
        satirlar: testler.filter((t) => t.r).map((t) => ({ hucreler: [hucre(t.ad, { sol: true }), F.sayi(t.r.istatistik, 2, b), F.sd(t.r.sd, b), pHucresi(t.r.p, alfa, b)] })),
        not: 'Log-rank testi tüm zamanlara eşit ağırlık verir; Breslow erken, Tarone-Ware ara dönemdeki farklara daha duyarlıdır. Raporlanması önerilen test log-rank testidir.',
        dipnotlar: [],
      },
    });
    if (lr) {
      raporSatirlari.push(`Log-rank: χ²(${F.sd(lr.sd, b)}) = ${F.sayi(lr.istatistik, 2, b)}; ${F.pEsitlik(lr.p, b)}`);
      bloklar.push({
        tur: 'tablo',
        tablo: {
          baslik: 'Gözlenen ve beklenen olay sayıları (log-rank)',
          basliklar: [[hucre(grupAd, { sol: true }), 'Gözlenen', 'Beklenen', 'Gözlenen/Beklenen']],
          satirlar: lr.gruplar.map((g) => ({ hucreler: [hucre(etiket(g.grup), { sol: true }), F.sayi(g.gozlenen, 0, b), F.sayi(g.beklenen, 2, b), F.sayi(g.gozlenen / g.beklenen, 2, b)] })),
          dipnotlar: [],
        },
      });
    }
    if (grupSirasi.length > 2 && lr && (lr.p < alfa || ayarlar.postHocHerZaman)) {
      const ci = dene(() => ciftliLogRank(sure, olay, grup, { pDuzeltme: 'bonferroni', duzeyler: grupSirasi }), uyarilar);
      if (ci) {
        yontemler.add('bonferroni');
        bloklar.push({
          tur: 'tablo',
          tablo: {
            baslik: 'İkili log-rank karşılaştırmaları',
            basliklar: [[hucre('Karşılaştırma', { sol: true }), 'χ²', 'p', 'p (Bonferroni)']],
            satirlar: ci.karsilastirmalar.map((c) => ({ hucreler: [hucre(`${etiket(c.grup1)} – ${etiket(c.grup2)}`, { sol: true }), F.sayi(c.istatistik, 2, b), F.p(c.p, b), pHucresi(c.pDuzeltilmis, alfa, b)] })),
            dipnotlar: [],
          },
        });
      }
    }
  }

  // ---- Cox regresyonu ----
  const kovaryatlar = [...new Set(secim.kovaryatlar || [])].filter((a) => a !== secim.sure && a !== secim.olay && ['surekli', 'sirali', 'kategorik'].includes(olcumler[a]));
  if (kovaryatlar.length) {
    yontemler.add('cox').add('schoenfeld');
    const tahminciler = kovaryatlar.map((a) => tahminci(vs, a, olcumler, satirlar, baglam));
    const tekli = [];
    for (const t of tahminciler) {
      const mm = dene(() => modelMatrisi([t], { sabit: false }), uyarilar, `${t.ad}: `);
      if (!mm) continue;
      const m = dene(() => coxRegresyon({ sure: mm.satirlar.map((i) => sure[i]), olay: mm.satirlar.map((i) => olay[i]), X: mm.X, adlar: mm.sutunAdlari, baglar: 'efron', duzey: ayarlar.guvenDuzeyi }), uyarilar, `${t.ad}: `);
      if (m) m.katsayilar.forEach((k, j) => tekli.push({ etiket: sutunEtiketi(mm, j, [t]), k }));
    }
    const mmCok = dene(() => modelMatrisi(tahminciler, { sabit: false }), uyarilar);
    let cok = null;
    if (mmCok) {
      cok = dene(() => coxRegresyon({ sure: mmCok.satirlar.map((i) => sure[i]), olay: mmCok.satirlar.map((i) => olay[i]), X: mmCok.X, adlar: mmCok.sutunAdlari, baglar: 'efron', duzey: ayarlar.guvenDuzeyi }), uyarilar);
      (mmCok.uyarilar || []).forEach((u) => uyarilar.push(u));
    }
    const hr = (k) => (k && Number.isFinite(k.HR) ? `${F.sayi(k.HR, 2, b)} (${F.ga(k.gaAlt, k.gaUst, 2, b)})` : '–');
    const satirlarCox = (cok?.katsayilar || tekli.map((x) => x.k)).map((_, j) => {
      const etiketJ = mmCok ? sutunEtiketi(mmCok, j, tahminciler) : tekli[j].etiket;
      const tk = tekli.find((x) => x.etiket === etiketJ)?.k;
      const ck = cok?.katsayilar[j];
      return { hucreler: [hucre(etiketJ, { sol: true }), hr(tk), tk ? pHucresi(tk.p, alfa, b) : '–', hr(ck), ck ? pHucresi(ck.p, alfa, b) : '–'] };
    });
    const olaySayisi = cok?.olaySayisi ?? toplamOlay;
    const parametre = mmCok ? mmCok.sutunAdlari.length : kovaryatlar.length;
    if (olaySayisi / parametre < 10) uyarilar.push(`Cox modelinde parametre başına olay sayısı ${F.sayi(olaySayisi / parametre, 1, b)} (önerilen ≥ 10); tahminler kararsız olabilir.`);
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Cox orantılı tehlikeler regresyonu',
        basliklar: [
          [hucre('Değişken', { sol: true, satir: 2 }), hucre('Tek değişkenli', { sutun: 2 }), hucre('Çok değişkenli', { sutun: 2 })],
          [`HR (%${gaY} GA)`, 'p', `HR (%${gaY} GA)`, 'p'],
        ],
        satirlar: satirlarCox,
        not: `HR: tehlike oranı (hazard ratio). Bağlı süreler için Efron yöntemi kullanılmıştır.${cok ? ` Çok değişkenli model: n = ${cok.n}, olay = ${cok.olaySayisi}; olabilirlik oranı χ²(${cok.modelTestleri.olabilirlikOrani.sd}) = ${F.sayi(cok.modelTestleri.olabilirlikOrani.istatistik, 2, b)}, ${F.pEsitlik(cok.modelTestleri.olabilirlikOrani.p, b)}; uyum indeksi (C) = ${F.sayi(cok.uyum.deger, 3, b)}.` : ''}`,
        dipnotlar: [],
      },
    });
    (cok?.uyarilar || []).forEach((u) => uyarilar.push(u));
    if (cok) {
      const ph = dene(() => coxPHVarsayimi(cok, { terimler: mmCok.terimler.map((t) => t.sutunlar), terimAdlari: tahminciler.map((t) => t.ad) }), uyarilar);
      if (ph) {
        const ihlal = ph.testler.filter((t) => t.p < alfa);
        bloklar.push({
          tur: 'tablo',
          tablo: {
            baslik: 'Orantılı tehlikeler varsayımı (Schoenfeld artıkları)',
            basliklar: [[hucre('Değişken', { sol: true }), 'χ²', 'sd', 'p']],
            satirlar: [...ph.testler, { ...ph.genel, ad: 'Genel' }].map((t, i, a) => ({ tur: i === a.length - 1 ? 'ara' : undefined, hucreler: [hucre(t.ad, { sol: true }), F.sayi(t.istatistik, 2, b), F.sd(t.sd, b), pHucresi(t.p, alfa, b)] })),
            not: ihlal.length ? `Varsayım ${ihlal.map((t) => t.ad).join(', ')} için sağlanmıyor (p < ${F.sayi(alfa, 2, b)}); bu değişkenler için tabakalandırma ya da zamana bağlı etki düşünülmelidir.` : 'Tüm değişkenler için orantılı tehlikeler varsayımı sağlanıyor.',
            dipnotlar: [],
          },
        });
        adimlar.push(adim('Cox varsayımı', ihlal.length ? `Orantılı tehlikeler varsayımı ${ihlal.map((t) => t.ad).join(', ')} için ihlal edildi.` : 'Orantılı tehlikeler varsayımı sağlandı (Schoenfeld artıkları testi).', ihlal.length ? 'uyari' : 'tamam'));
      }
      cok.katsayilar.forEach((k, j) => raporSatirlari.push(`${sutunEtiketi(mmCok, j, tahminciler)}: HR = ${F.sayi(k.HR, 2, b)} (%${gaY} GA: ${F.ga(k.gaAlt, k.gaUst, 2, b)}); ${F.pEsitlik(k.p, b)}`));
      if (secim.grafik) {
        bloklar.push({
          tur: 'grafik',
          baslik: 'Çok değişkenli Cox modeli: tehlike oranları',
          svg: ormanGrafigi(cok.katsayilar.map((k, j) => ({ ad: sutunEtiketi(mmCok, j, tahminciler), tahmin: k.HR, alt: k.gaAlt, ust: k.gaUst, p: k.p })), { ...grafikSecenek(baglam), logOlcek: true, referans: 1, etiket: `HR (%${gaY} GA)` }),
          dosyaAdi: 'cox-orman',
        });
      }
    }
  }

  bloklar.unshift({ tur: 'karar', baslik: 'Yöntem', adimlar });

  // ---- Kaplan-Meier grafiği ----
  if (secim.grafik) {
    const maks = Math.max(...sure);
    const adimBoyu = guzelAdim(maks);
    const riskZamanlari = [];
    for (let t = 0; t <= maks + 1e-9; t += adimBoyu) riskZamanlari.push(Math.round(t * 1e6) / 1e6);
    const egriler = km.gruplar.map((g) => ({
      ad: etiket(g.grup),
      adimlar: g.tablo.map((s) => ({ zaman: s.zaman, sagkalim: s.sagkalim, gaAlt: s.gaAlt, gaUst: s.gaUst })),
      sansurZamanlari: g.tablo.filter((s) => s.sansur > 0).map((s) => s.zaman),
    }));
    const sayilar = {};
    km.gruplar.forEach((g) => {
      const gs = sure.filter((_, i) => grup[i] === g.grup);
      sayilar[etiket(g.grup)] = riskZamanlari.map((t) => gs.filter((x) => x >= t).length);
    });
    const lr = raporSatirlari.find((s) => s.startsWith('Log-rank'));
    bloklar.push({
      tur: 'grafik',
      baslik: `Kaplan-Meier sağkalım eğrisi${grupAd ? ` (${grupAd})` : ''}`,
      svg: kaplanMeierGrafigi(egriler, { ...grafikSecenek(baglam), xEtiket: sureAd, yEtiket: 'Sağkalım olasılığı', gaGoster: !grupD, riskTablosu: { zamanlar: riskZamanlari, sayilar }, notMetni: lr ? lr.replace(': ', ' ') : '' }),
      dosyaAdi: 'kaplan-meier',
    });
  }
  bloklar.push({ tur: 'baslik', metin: 'Raporlama satırları' });
  raporSatirlari.forEach((m) => bloklar.push({ tur: 'satir', metin: m }));
  km.gruplar.forEach((g) => (g.uyarilar || []).forEach((u) => uyarilar.push(u)));
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `Sağkalım analizi${grupAd ? `: ${grupAd} gruplarına göre` : ''}`, bloklar, yontemler: [...yontemler] };
}
