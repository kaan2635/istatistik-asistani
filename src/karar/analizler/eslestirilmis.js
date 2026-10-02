// Eşleştirilmiş / tekrarlı ölçümler: aynı deneklerin farklı zaman ya da koşullardaki ölçümleri.
// İsteğe bağlı bir gruplar arası etkenle karma desen.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import { betimsel } from '../../cekirdek/betimsel.js';
import * as P from '../../cekirdek/parametrik.js';
import * as NP from '../../cekirdek/nonparametrik.js';
import * as K from '../../cekirdek/kategorik.js';
import { pDuzelt } from '../../cekirdek/duzeltme.js';
import { tekrarliOlcumAnova } from '../../cekirdek/tekrarli.js';
import { caprazCubukGrafigi, profilGrafigi } from '../../grafik/grafikler.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim, eslestirilmisTestSec, normallikIfadesi, normallikKarari } from '../kurallar.js';
import { degiskenBul, duzeyler, gorunenAd, tamVakalar } from '../veri-erisim.js';
import { grupAnalizi, surekliTestUygula } from './grup.js';
import { dene, etkiMetni, grafikSecenek, istatistikHucresi, pHucresi, surekliMetin, testSatiri, uyariBloklari } from './ortak.js';

const DUZELTME_ADI = { kuresellik: 'Küresellik varsayıldı', 'greenhouse-geisser': 'Greenhouse-Geisser', 'huynh-feldt': 'Huynh-Feldt', 'alt-sinir': 'Alt sınır' };

/** Sıralı metin düzeylerini 1..m koduna çevirir; sayısal değerleri olduğu gibi bırakır */
function kodlayici(vs, ad, baglam) {
  const d = degiskenBul(vs, ad);
  if (d.veriTuru === 'sayi') return (v) => v;
  const dz = duzeyler(vs, ad, baglam);
  const kod = new Map(dz.map((x, i) => [String(x.deger), i + 1]));
  return (v) => kod.get(String(v)) ?? null;
}

function sutun(matris, j) {
  return matris.map((s) => s[j]);
}

export function eslestirilmisAnaliz(secim, vs, baglam) {
  const { ayarlar, olcumler } = baglam;
  const adlar = secim.degiskenler || [];
  if (adlar.length < 2) throw new IstatistikHatasi('En az iki ölçüm seçin.');
  const turler = adlar.map((a) => olcumler[a]);
  const benzersizSayisi = (a) => new Set(degiskenBul(vs, a).degerler.filter((v) => v !== null && v !== undefined)).size;
  const kategorik = turler.every((t) => t === 'kategorik') || (turler.every((t) => t === 'sirali') && adlar.every((a) => benzersizSayisi(a) <= 2));
  if (kategorik) return kategorikEslestirilmis(secim, vs, baglam);
  if (turler.some((t) => t === 'kategorik')) throw new IstatistikHatasi('Ölçümler aynı türde olmalı: hepsi sürekli/sıralı ya da hepsi kategorik.');
  return surekliEslestirilmis(secim, vs, baglam, turler.includes('sirali') ? 'sirali' : 'surekli');
}

function surekliEslestirilmis(secim, vs, baglam, olcum) {
  const { ayarlar, bicim: b } = baglam;
  const alfa = ayarlar.alfa;
  const adlar = secim.degiskenler;
  const etiketler = adlar.map((a) => gorunenAd(degiskenBul(vs, a)));
  const grupAd = secim.grup || null;
  const tum = tamVakalar(vs, grupAd ? [...adlar, grupAd] : adlar, baglam, { sayisal: false });
  const kodlar = adlar.map((a) => kodlayici(vs, a, baglam));
  const matris = [];
  const grupDegerleri = [];
  for (const s of tum.matris) {
    const satir = adlar.map((_, j) => (olcum === 'sirali' ? kodlar[j](s[j]) : s[j]));
    if (satir.some((v) => typeof v !== 'number' || !Number.isFinite(v))) continue;
    matris.push(satir);
    if (grupAd) grupDegerleri.push(s[adlar.length]);
  }
  const n = matris.length;
  if (n < 3) throw new IstatistikHatasi(`Tüm ölçümleri dolu en az 3 denek gerekir (şu an ${n}).`);
  const k = adlar.length;
  const uyarilar = [];
  if (tum.dislanan > 0) uyarilar.push(`${tum.dislanan} denek en az bir ölçümü eksik olduğu için analiz dışı bırakıldı (liste bazında silme).`);
  const yontemler = new Set();
  const bloklar = [];
  const grafikSec = grafikSecenek(baglam);

  // ---- Gruplar arası etken (karma desen) ----
  let grupDuzeyleri = null;
  if (grupAd) {
    const dz = duzeyler(vs, grupAd, baglam).filter((x) => grupDegerleri.some((g) => String(g) === String(x.deger)));
    grupDuzeyleri = dz;
    if (dz.length < 2) throw new IstatistikHatasi('Grup değişkeninde ölçümleri tam olan en az iki grup olmalı.');
  }

  // ---- Test seçimi ----
  let testSec;
  if (grupAd) {
    const adimlar = [adim('Tasarım', `${n} denekte ${k} tekrarlı ölçüm (${etiketler.join(', ')}); gruplar arası etken: ${gorunenAd(degiskenBul(vs, grupAd))} (${grupDuzeyleri.map((g) => `${g.etiket}: n = ${grupDegerleri.filter((x) => String(x) === String(g.deger)).length}`).join('; ')}).`)];
    let hepsiNormal = olcum === 'surekli';
    if (olcum === 'surekli') {
      const satirlar = [];
      for (const g of grupDuzeyleri) {
        const alt = matris.filter((_, i) => String(grupDegerleri[i]) === String(g.deger));
        adlar.forEach((_, j) => {
          const nk = normallikKarari(sutun(alt, j), ayarlar, b);
          if (nk.normal !== true) hepsiNormal = false;
          satirlar.push(`${g.etiket} / ${etiketler[j]}: ${nk.ozet} → ${normallikIfadesi(nk)}`);
          if (nk.sw) yontemler.add('shapiro-wilk');
        });
      }
      adimlar.push(adim('Normallik (her grup ve ölçümde)', satirlar.join('. ') + '.', hepsiNormal ? 'tamam' : 'uyari'));
    } else adimlar.push(adim('Ölçek', 'Sıralı ölçek → parametrik olmayan testler.'));
    adimlar.push(
      adim(
        'Seçilen test',
        hepsiNormal
          ? 'Tüm hücreler normal dağılıyor → karma desen (iki yönlü, bir etkeni tekrarlı) varyans analizi; küresellik Mauchly testiyle, gruplarda varyans eşitliği Levene testiyle sınanır.'
          : 'Normal dağılım varsayımı sağlanmadı → karma desen ANOVA yerine: her grupta zamanlar arası değişim ' + (k === 2 ? 'Wilcoxon' : 'Friedman') + ' testiyle, her zamanda gruplar arası fark ' + (grupDuzeyleri.length === 2 ? 'Mann-Whitney U' : 'Kruskal-Wallis') + ' testiyle ve değişim miktarının (son − ilk ölçüm) gruplar arasında karşılaştırılmasıyla incelenir.',
        'tamam',
      ),
    );
    testSec = { parametrik: hepsiNormal, adimlar };
  } else {
    testSec = eslestirilmisTestSec(matris, etiketler, olcum, ayarlar, b);
    for (const nk of testSec.normallik || []) if (nk.sw) yontemler.add('shapiro-wilk');
  }
  bloklar.push({ tur: 'karar', adimlar: testSec.adimlar });

  // ---- Betimsel tablo ----
  const gosterim = testSec.parametrik ? 'ortss' : ayarlar.surekliGosterim === 'medyan-minmaks' ? 'medyan-minmaks' : 'medyan-ceyrek';
  const gosterimEtiketi = gosterim === 'ortss' ? 'Ort ± SS' : gosterim === 'medyan-minmaks' ? 'Medyan (min–maks)' : 'Medyan (Ç1–Ç3)';
  const gruplarArasi = grupDuzeyleri
    ? grupDuzeyleri.map((g) => ({ g, alt: matris.filter((_, i) => String(grupDegerleri[i]) === String(g.deger)) }))
    : null;
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: `Ölçümlerin betimsel istatistikleri (${gosterimEtiketi})`,
      basliklar: [[hucre('Ölçüm', { sol: true }), ...(gruplarArasi ? gruplarArasi.map(({ g, alt }) => `${g.etiket}\n(n = ${alt.length})`) : []), `${gruplarArasi ? 'Toplam' : 'Değer'}\n(n = ${n})`]],
      satirlar: adlar.map((_, j) => ({
        hucreler: [hucre(etiketler[j], { sol: true }), ...(gruplarArasi ? gruplarArasi.map(({ alt }) => surekliMetin(sutun(alt, j), gosterim, b, ayarlar.yuzdelikTipi)) : []), surekliMetin(sutun(matris, j), gosterim, b, ayarlar.yuzdelikTipi)],
      })),
      not: olcum === 'sirali' && adlar.some((a) => degiskenBul(vs, a).veriTuru !== 'sayi') ? 'Sıralı kategoriler sıra numaralarıyla (1, 2, …) kodlanarak özetlenmiştir.' : '',
      dipnotlar: [],
    },
  });

  const raporSatirlari = [];
  if (!grupAd && k === 2) {
    // ---- İki ölçüm ----
    const ilk = sutun(matris, 0);
    const son = sutun(matris, 1);
    let s;
    if (testSec.parametrik) {
      s = P.eslestirilmisT(son, ilk, { duzey: ayarlar.guvenDuzeyi });
      yontemler.add('eslestirilmis-t');
    } else {
      s = NP.wilcoxonIsaretliSira(son, ilk, { duzey: ayarlar.guvenDuzeyi });
      yontemler.add('wilcoxon');
    }
    const fark = testSec.parametrik
      ? `${F.sayi(s.ortalamaFarki, undefined, b)} (${F.ga(s.ga.alt, s.ga.ust, undefined, b)})`
      : s.hodgesLehmann
        ? `${F.sayi(s.hodgesLehmann.tahmin, undefined, b)} (${F.ga(s.hodgesLehmann.ga.alt, s.hodgesLehmann.ga.ust, undefined, b)})`
        : '–';
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: `${etiketler[1]} ile ${etiketler[0]} karşılaştırması`,
        basliklar: [[hucre('Test', { sol: true }), testSec.parametrik ? 'Ortalama fark (%95 GA)' : 'Hodges-Lehmann farkı (%95 GA)', 'İstatistik', 'p', 'Etki büyüklüğü']],
        satirlar: [{ hucreler: [hucre(s.ad, { sol: true }), fark, istatistikHucresi(s, b), pHucresi(s.p, alfa, b), etkiMetni(s.etki, b) || '–'] }],
        not: `Fark = ${etiketler[1]} − ${etiketler[0]}.${!testSec.parametrik && s.sifirFark ? ` Farkı sıfır olan ${s.sifirFark} denek Wilcoxon testinde dışarıda bırakıldı.` : ''}${!testSec.parametrik ? ` Negatif sıra: n = ${s.negatifSira?.n}, pozitif sıra: n = ${s.pozitifSira?.n}.` : ''}`,
        dipnotlar: [],
      },
    });
    raporSatirlari.push(testSatiri(s, b));
    (s.uyarilar || []).forEach((u) => uyarilar.push(u));
  } else if (!grupAd) {
    // ---- Üç ve daha çok ölçüm ----
    if (testSec.parametrik) {
      const r = tekrarliOlcumAnova({ veri: matris, duzeyAdlari: etiketler, faktorAdi: 'Zaman', duzey: ayarlar.guvenDuzeyi, duzeltme: 'bonferroni' });
      yontemler.add('tekrarli-anova').add('mauchly').add('bonferroni');
      rmAnovaTablolari(r, bloklar, raporSatirlari, b, alfa, null);
      (r.uyarilar || []).forEach((u) => uyarilar.push(u));
    } else {
      const fr = NP.friedman(matris);
      yontemler.add('friedman');
      bloklar.push({
        tur: 'tablo',
        tablo: {
          baslik: 'Friedman testi',
          basliklar: [[hucre('Ölçüm', { sol: true }), 'Ortalama sıra', 'Medyan']],
          satirlar: [
            ...etiketler.map((e, j) => ({ hucreler: [hucre(e, { sol: true }), F.sayi(fr.ortalamaSiralar[j], 2, b), F.sayi(fr.medyanlar[j], undefined, b)] })),
            { tur: 'ara', hucreler: [hucre(`χ²(${fr.sd}) = ${F.sayi(fr.istatistik, 2, b)}; ${F.pEsitlik(fr.p, b)}; Kendall W = ${F.sayi(fr.kendallW, 2, b)}`, { sol: true, sutun: 3 })] },
          ],
          dipnotlar: [],
        },
      });
      raporSatirlari.push(testSatiri(fr, b));
      if (fr.p < alfa || ayarlar.postHocHerZaman) {
        const ph = dene(() => NP.friedmanPostHoc(matris, { yontem: 'wilcoxon-bonferroni', duzeltme: ayarlar.parametrikOlmayanDuzeltme }), uyarilar);
        if (ph) {
          yontemler.add('wilcoxon').add('bonferroni');
          bloklar.push({
            tur: 'tablo',
            tablo: {
              baslik: 'İkili karşılaştırmalar (Wilcoxon işaretli sıralar testi)',
              basliklar: [[hucre('Karşılaştırma', { sol: true }), 'Z', 'p', `p (${ph.duzeltme === 'bonferroni' ? 'Bonferroni' : ph.duzeltme} düzeltmeli)`]],
              satirlar: ph.karsilastirmalar.map((c) => ({ hucreler: [hucre(`${etiketler[c.olcum2]} – ${etiketler[c.olcum1]}`, { sol: true }), F.sayi(c.z, 2, b), F.p(c.p, b), pHucresi(c.pDuzeltilmis, alfa, b)] })),
              dipnotlar: [],
            },
          });
        }
      }
      (fr.uyarilar || []).forEach((u) => uyarilar.push(u));
    }
  } else if (testSec.parametrik) {
    // ---- Karma desen ANOVA ----
    const r = tekrarliOlcumAnova({ veri: matris, duzeyAdlari: etiketler, faktorAdi: 'Zaman', gruplar: grupDegerleri.map(String), grupAdi: gorunenAd(degiskenBul(vs, grupAd)), grupSirasi: grupDuzeyleri.map((g) => String(g.deger)), duzey: ayarlar.guvenDuzeyi, duzeltme: 'bonferroni' });
    yontemler.add('karma-anova').add('mauchly').add('levene').add('bonferroni');
    rmAnovaTablolari(r, bloklar, raporSatirlari, b, alfa, grupDuzeyleri);
    (r.uyarilar || []).forEach((u) => uyarilar.push(u));
  } else {
    // ---- Karma desen, parametrik olmayan ----
    const icSatirlar = [];
    for (const { g, alt } of gruplarArasi) {
      if (alt.length < 3) {
        icSatirlar.push({ hucreler: [hucre(g.etiket, { sol: true }), 'yetersiz n', '–', '–'] });
        continue;
      }
      const s = k === 2 ? NP.wilcoxonIsaretliSira(sutun(alt, 1), sutun(alt, 0)) : NP.friedman(alt);
      yontemler.add(k === 2 ? 'wilcoxon' : 'friedman');
      icSatirlar.push({ hucreler: [hucre(g.etiket, { sol: true }), s.ad, istatistikHucresi(s, b), pHucresi(s.p, alfa, b)] });
      raporSatirlari.push(`${g.etiket} (zamanlar arası): ${testSatiri(s, b)}`);
    }
    bloklar.push({ tur: 'tablo', tablo: { baslik: 'Her grupta zamanlar arası değişim', basliklar: [[hucre('Grup', { sol: true }), 'Test', 'İstatistik', 'p']], satirlar: icSatirlar, dipnotlar: [] } });
    const arasiSatirlar = [];
    const zamanGruplari = adlar.map((_, j) => gruplarArasi.map(({ alt }) => sutun(alt, j)));
    const degisim = gruplarArasi.map(({ alt }) => alt.map((s) => s[k - 1] - s[0]));
    const yap = (etiket, gruplar) => {
      const id = gruplar.length === 2 ? 'mann-whitney-u' : 'kruskal-wallis';
      const s = surekliTestUygula(id, gruplar, gruplarArasi.map(({ g }) => g.etiket), ayarlar).sonuc;
      yontemler.add(id);
      arasiSatirlar.push({ hucreler: [hucre(etiket, { sol: true }), s.ad, istatistikHucresi(s, b), pHucresi(s.p, alfa, b)] });
      raporSatirlari.push(`${etiket} (gruplar arası): ${testSatiri(s, b)}`);
    };
    zamanGruplari.forEach((gr, j) => yap(etiketler[j], gr));
    yap(`Değişim (${etiketler[k - 1]} − ${etiketler[0]})`, degisim);
    bloklar.push({ tur: 'tablo', tablo: { baslik: 'Her ölçümde ve değişim miktarında gruplar arası karşılaştırma', basliklar: [[hucre('Ölçüm', { sol: true }), 'Test', 'İstatistik', 'p']], satirlar: arasiSatirlar, dipnotlar: [] } });
  }

  // ---- Grafik ----
  if (secim.grafik) {
    const nokta = (x) => {
      const d = betimsel(x, { yuzdelikTipi: ayarlar.yuzdelikTipi });
      return testSec.parametrik ? { ortalama: d.ortalama, alt: d.gaAlt, ust: d.gaUst } : { ortalama: d.medyan, alt: d.q1, ust: d.q3 };
    };
    const seriler = gruplarArasi
      ? gruplarArasi.map(({ g, alt }) => ({ ad: g.etiket, noktalar: etiketler.map((e, j) => ({ x: e, ...nokta(sutun(alt, j)) })) }))
      : [{ ad: 'Tüm denekler', noktalar: etiketler.map((e, j) => ({ x: e, ...nokta(sutun(matris, j)) })) }];
    bloklar.push({
      tur: 'grafik',
      baslik: `${etiketler.join(', ')}: zamana göre değişim`,
      svg: profilGrafigi(seriler, { ...grafikSec, xEtiket: 'Ölçüm', yEtiket: testSec.parametrik ? 'Ortalama (%95 GA)' : 'Medyan (Ç1–Ç3)', hataNotu: testSec.parametrik ? 'Hata çubukları: ortalamanın %95 güven aralığı' : 'Hata çubukları: birinci ve üçüncü çeyreklik' }),
      dosyaAdi: 'tekrarli-olcum',
    });
  }
  bloklar.push({ tur: 'baslik', metin: 'Raporlama satırları' });
  raporSatirlari.forEach((m) => bloklar.push({ tur: 'satir', metin: m }));
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `Tekrarlı ölçümler: ${etiketler.join(' → ')}${grupAd ? ` (${gorunenAd(degiskenBul(vs, grupAd))} gruplarında)` : ''}`, bloklar, yontemler: [...yontemler] };
}

/** Tekrarlı ölçümler / karma ANOVA sonuç tabloları */
function rmAnovaTablolari(r, bloklar, raporSatirlari, b, alfa, grupDuzeyleri) {
  const kur = r.kuresellik?.[0];
  const mauchlyVar = kur && Number.isFinite(kur.p) && kur.sd > 0;
  if (mauchlyVar) {
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Mauchly küresellik testi',
        basliklar: [[hucre('Etki', { sol: true }), "Mauchly W", 'χ²', 'sd', 'p', 'ε (GG)', 'ε (HF)', 'ε (alt sınır)']],
        satirlar: r.kuresellik.map((k) => ({
          hucreler: [hucre(k.etki, { sol: true }), F.sayi(k.W, 3, b), F.sayi(k.kiKare, 2, b), F.sd(k.sd, b), pHucresi(k.p, alfa, b), F.sayi(k.epsilon.greenhouseGeisser, 3, b), F.sayi(k.epsilon.huynhFeldt, 3, b), F.sayi(k.epsilon.altSinir, 3, b)],
        })),
        not: kur.p < alfa ? `Küresellik varsayımı sağlanmadı; ${kur.onerilenDuzeltme === 'greenhouse-geisser' ? 'ε(GG) < 0,75 olduğundan Greenhouse-Geisser' : 'ε(GG) ≥ 0,75 olduğundan Huynh-Feldt'} düzeltmesi uygulandı.` : 'Küresellik varsayımı sağlandı.',
        dipnotlar: [],
      },
    });
  }
  if (!mauchlyVar) bloklar.push({ tur: 'not', metin: 'İki ölçümde küresellik varsayımı kendiliğinden sağlanır; Mauchly testi gerekmez.' });
  const secilen = mauchlyVar && kur.p < alfa ? kur.onerilenDuzeltme : 'kuresellik';
  const satirlar = r.icEtkilerTablosu.filter((x) => x.duzeltme === secilen);
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: `Denek içi etkiler (${DUZELTME_ADI[secilen]})`,
      basliklar: [[hucre('Kaynak', { sol: true }), 'KT', 'sd', 'KO', 'F', 'p', 'Kısmi η²']],
      satirlar: satirlar.map((x) => ({
        hucreler: [hucre(x.kaynak, { sol: true }), F.sayi(x.KT, 2, b), F.sd(x.sd, b), F.sayi(x.KO, 2, b), x.tur === 'etki' ? F.sayi(x.F, 2, b) : '', x.tur === 'etki' ? pHucresi(x.p, alfa, b) : '', x.tur === 'etki' ? F.sayi(x.kismiEtaKare, 3, b) : ''],
      })),
      not: 'KT: kareler toplamı; sd: serbestlik derecesi; KO: kareler ortalaması.',
      dipnotlar: [],
    },
  });
  for (const x of satirlar.filter((s) => s.tur === 'etki')) raporSatirlari.push(`${x.kaynak}: F(${F.sd(x.sd, b)}, ${F.sd(satirlar.find((s) => s.tur === 'hata')?.sd, b)}) = ${F.sayi(x.F, 2, b)}; ${F.pEsitlik(x.p, b)}; kısmi η² = ${F.sayi(x.kismiEtaKare, 3, b)}`);
  if (grupDuzeyleri && r.gruplarArasiTablo) {
    const ga = r.gruplarArasiTablo.filter((x) => x.tur !== 'kesisim');
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Gruplar arası etkiler',
        basliklar: [[hucre('Kaynak', { sol: true }), 'KT', 'sd', 'KO', 'F', 'p', 'Kısmi η²']],
        satirlar: ga.map((x) => ({ hucreler: [hucre(x.kaynak, { sol: true }), F.sayi(x.KT, 2, b), F.sd(x.sd, b), F.sayi(x.KO, 2, b), Number.isFinite(x.F) ? F.sayi(x.F, 2, b) : '', Number.isFinite(x.p) ? pHucresi(x.p, alfa, b) : '', Number.isFinite(x.kismiEtaKare) ? F.sayi(x.kismiEtaKare, 3, b) : ''] })),
        dipnotlar: [],
      },
    });
    const g = ga.find((x) => Number.isFinite(x.F));
    const hata = ga.find((x) => !Number.isFinite(x.F));
    if (g) raporSatirlari.push(`${g.kaynak}: F(${F.sd(g.sd, b)}, ${F.sd(hata?.sd, b)}) = ${F.sayi(g.F, 2, b)}; ${F.pEsitlik(g.p, b)}; kısmi η² = ${F.sayi(g.kismiEtaKare, 3, b)}`);
    if (r.levene?.length) {
      bloklar.push({
        tur: 'tablo',
        tablo: {
          baslik: 'Her ölçümde varyans homojenliği (Levene)',
          basliklar: [[hucre('Ölçüm', { sol: true }), 'F', 'sd', 'p']],
          satirlar: r.levene.map((l) => ({ hucreler: [hucre(Object.values(l.duzeyler).join(' × '), { sol: true }), F.sayi(l.istatistik, 2, b), F.sd(l.sd, b), pHucresi(l.p, alfa, b)] })),
          dipnotlar: [],
        },
      });
    }
  }
  const ikili = r.ikiliKarsilastirmalar?.Zaman || Object.values(r.ikiliKarsilastirmalar || {})[0];
  if (ikili?.length) {
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Ölçümler arası ikili karşılaştırmalar (Bonferroni düzeltmeli)',
        basliklar: [[hucre('Karşılaştırma', { sol: true }), 'Ortalama fark', 'SH', 'p', '%95 GA']],
        satirlar: ikili.map((c) => ({ hucreler: [hucre(`${c.duzey2} – ${c.duzey1}`, { sol: true }), F.sayi(-c.fark, undefined, b), F.sayi(c.SH, undefined, b), pHucresi(c.p, alfa, b), F.ga(-c.gaUst, -c.gaAlt, undefined, b)] })),
        not: 'SH: standart hata. Fark = sonraki ölçüm − önceki ölçüm.',
        dipnotlar: [],
      },
    });
  }
}

/** Kategorik eşleştirilmiş ölçümler: McNemar, McNemar-Bowker, Cochran Q */
function kategorikEslestirilmis(secim, vs, baglam) {
  const { ayarlar, bicim: b } = baglam;
  const alfa = ayarlar.alfa;
  const adlar = secim.degiskenler;
  const etiketler = adlar.map((a) => gorunenAd(degiskenBul(vs, a)));
  const tum = tamVakalar(vs, adlar, baglam, { sayisal: false });
  const n = tum.matris.length;
  if (n < 2) throw new IstatistikHatasi('Tüm ölçümleri dolu yeterli denek yok.');
  const yontemler = new Set();
  const uyarilar = [];
  if (tum.dislanan) uyarilar.push(`${tum.dislanan} denek en az bir ölçümü eksik olduğu için analiz dışı bırakıldı.`);
  // Ortak düzeyler (tüm ölçümlerin birleşimi), ilk değişkenin düzey sırasıyla
  const sira = [];
  for (const a of adlar) for (const x of duzeyler(vs, a, baglam)) if (!sira.some((s) => String(s.deger) === String(x.deger))) sira.push(x);
  const idx = new Map(sira.map((x, i) => [String(x.deger), i]));
  const m = sira.length;
  const k = adlar.length;
  const bloklar = [];
  const raporSatirlari = [];
  const adimlar = [adim('Tasarım', `${n} denekte ${k} eşleştirilmiş kategorik ölçüm (${etiketler.join(', ')}); kategoriler: ${sira.map((x) => x.etiket).join(', ')}.`)];

  // Dağılım tablosu: kategori × ölçüm
  const sayim = sira.map(() => new Array(k).fill(0));
  for (const s of tum.matris) s.forEach((v, j) => sayim[idx.get(String(v))][j]++);
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Ölçümlere göre dağılım',
      basliklar: [[hucre('Kategori', { sol: true }), ...etiketler]],
      satirlar: sira.map((x, i) => ({ hucreler: [hucre(x.etiket, { sol: true }), ...sayim[i].map((c) => F.nYuzde(c, n, 1, b))] })),
      not: 'Değerler n (%) olarak verilmiştir.',
      dipnotlar: [],
    },
  });

  if (m < 2) throw new IstatistikHatasi('Ölçümlerde tek bir kategori var; karşılaştırma yapılamaz.');
  if (k === 2) {
    const tablo = sira.map(() => new Array(m).fill(0));
    for (const s of tum.matris) tablo[idx.get(String(s[0]))][idx.get(String(s[1]))]++;
    let s;
    if (m === 2) {
      s = K.mcnemar(tablo, { duzey: ayarlar.guvenDuzeyi });
      yontemler.add('mcnemar');
      adimlar.push(adim('Seçilen test', `İki kategorili iki eşleştirilmiş ölçüm → McNemar testi; uyumsuz çift sayısı ${s.b + s.c}${s.pYontemi === 'kesin' ? " (25'ten az olduğundan kesin binom p değeri)" : ' (süreklilik düzeltmeli ki-kare)'}.`, 'tamam'));
    } else {
      s = K.mcnemarBowker(tablo);
      yontemler.add('mcnemar-bowker');
      adimlar.push(adim('Seçilen test', `${m} kategorili iki eşleştirilmiş ölçüm → McNemar-Bowker simetri testi.`, 'tamam'));
    }
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: `${etiketler[0]} × ${etiketler[1]} çapraz tablosu`,
        basliklar: [[hucre(`${etiketler[0]} \\ ${etiketler[1]}`, { sol: true }), ...sira.map((x) => x.etiket), 'Toplam']],
        satirlar: [
          ...sira.map((x, i) => ({ hucreler: [hucre(x.etiket, { sol: true }), ...tablo[i].map(String), String(tablo[i].reduce((a, v) => a + v, 0))] })),
          { tur: 'ara', hucreler: [hucre(`${s.ad}: ${testSatiri(s, b)}`, { sol: true, sutun: m + 2 })] },
        ],
        dipnotlar: [],
      },
    });
    raporSatirlari.push(testSatiri(s, b));
    (s.uyarilar || []).forEach((u) => uyarilar.push(u));
  } else {
    if (m > 2) throw new IstatistikHatasi('Üç ve daha çok ölçümde kategorik karşılaştırma yalnızca iki kategorili (ör. Var/Yok) değişkenler için yapılabilir. Kategorileri birleştirin ya da değişkeni sıralı olarak tanımlayın.');
    const ikili = tum.matris.map((s) => s.map((v) => (idx.get(String(v)) === 1 ? 1 : 0)));
    const q = K.cochranQ(ikili);
    yontemler.add('cochran-q');
    adimlar.push(adim('Seçilen test', `İki kategorili ${k} eşleştirilmiş ölçüm → Cochran Q testi; anlamlıysa ikili karşılaştırmalar Bonferroni düzeltmeli McNemar testiyle.`, 'tamam'));
    bloklar.push({ tur: 'tablo', tablo: { baslik: "Cochran'ın Q testi", basliklar: [[hucre('Test', { sol: true }), 'Q', 'sd', 'p']], satirlar: [{ hucreler: [hucre(q.ad, { sol: true }), F.sayi(q.istatistik, 2, b), F.sd(q.sd, b), pHucresi(q.p, alfa, b)] }], dipnotlar: [] } });
    raporSatirlari.push(testSatiri(q, b));
    if (q.p < alfa || ayarlar.postHocHerZaman) {
      const satirlar = [];
      const ham = [];
      for (let i = 0; i < k; i++) {
        for (let j = i + 1; j < k; j++) {
          const t = [[0, 0], [0, 0]];
          for (const s of ikili) t[s[i]][s[j]]++;
          const r = K.mcnemar(t);
          ham.push(r.p);
          satirlar.push({ ad: `${etiketler[j]} – ${etiketler[i]}`, r });
        }
      }
      const duz = pDuzelt(ham, 'bonferroni');
      yontemler.add('mcnemar').add('bonferroni');
      bloklar.push({
        tur: 'tablo',
        tablo: {
          baslik: 'İkili karşılaştırmalar (McNemar, Bonferroni düzeltmeli)',
          basliklar: [[hucre('Karşılaştırma', { sol: true }), 'p', 'p (Bonferroni)']],
          satirlar: satirlar.map((s, i) => ({ hucreler: [hucre(s.ad, { sol: true }), F.p(s.r.p, b), pHucresi(duz[i], alfa, b)] })),
          dipnotlar: [],
        },
      });
    }
  }
  bloklar.unshift({ tur: 'karar', adimlar });
  if (secim.grafik) {
    bloklar.push({ tur: 'grafik', baslik: 'Ölçümlere göre kategori dağılımı', svg: caprazCubukGrafigi(sayim, sira.map((x) => x.etiket), etiketler, { ...grafikSecenek(baglam), tip: 'yigilmis', yuzde: true, yuzdeTabani: 'sutun', xEtiket: 'Ölçüm', yEtiket: 'Yüzde' }), dosyaAdi: 'eslestirilmis-kategorik' });
  }
  bloklar.push({ tur: 'baslik', metin: 'Raporlama satırı' });
  raporSatirlari.forEach((mt) => bloklar.push({ tur: 'satir', metin: mt }));
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `Eşleştirilmiş kategorik ölçümler: ${etiketler.join(' → ')}`, bloklar, yontemler: [...yontemler] };
}

export { grupAnalizi };
