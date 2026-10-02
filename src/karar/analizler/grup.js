// Bağımsız grupların karşılaştırılması ve "Tablo 1". Her değişken için test, değişkenin türüne,
// grupların dağılımına ve varyans homojenliğine göre seçilir.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import * as P from '../../cekirdek/parametrik.js';
import * as H from '../../cekirdek/posthoc.js';
import * as NP from '../../cekirdek/nonparametrik.js';
import * as K from '../../cekirdek/kategorik.js';
import { caprazCubukGrafigi, kutuGrafigi } from '../../grafik/grafikler.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim, gosterimSec, kategorikTestSec, surekliGrupTestiSec } from '../kurallar.js';
import { caprazTablo, degiskenBul, duzeyler, gorunenAd, gruplaraAyir } from '../veri-erisim.js';
import { GOSTERIM_ETIKETI, TEST_ADLARI, dene, etkiMetni, grafikSecenek, harfAtayici, istatistikHucresi, pHucresi, surekliMetin, testSatiri, uyariBloklari } from './ortak.js';

const NORMALLIK_YONTEMI = { 'Shapiro-Wilk': 'shapiro-wilk', 'Kolmogorov-Smirnov': 'lilliefors', Çarpıklık: 'carpiklik-basiklik' };

function normallikYontemleri(secim, yontemler) {
  for (const nk of secim.normallik || []) {
    for (const [anahtar, id] of Object.entries(NORMALLIK_YONTEMI)) if ((nk.yontem || '').includes(anahtar)) yontemler.add(id);
  }
}

/** Boş satır ve sütunları atar (test için) */
export function bosSatirSutunAt(tablo) {
  const satirlar = tablo.map((s, i) => [s, i]).filter(([s]) => s.some((v) => v > 0));
  const sutunIdx = tablo[0].map((_, j) => j).filter((j) => tablo.some((s) => s[j] > 0));
  return { tablo: satirlar.map(([s]) => sutunIdx.map((j) => s[j])), satirIdx: satirlar.map(([, i]) => i), sutunIdx };
}

/** Kategorik testi uygular; kiKare sonucunu etki büyüklüğü için her zaman hesaplar */
export function kategorikTestUygula(tablo, testId, b) {
  const kk = tablo.length >= 2 && tablo[0].length >= 2 ? K.kiKare(tablo) : null;
  let sonuc;
  if (testId === 'pearson') sonuc = { ...kk, yontem: 'pearson-ki-kare' };
  else if (testId === 'yates') sonuc = { ...kk, yontem: 'yates-ki-kare', ad: 'Yates düzeltmeli ki-kare testi', istatistik: kk.yates.istatistik, sd: kk.yates.sd, p: kk.yates.p };
  else sonuc = K.fisherKesin(tablo);
  const etki = kk ? (tablo.length === 2 && tablo[0].length === 2 ? { ad: 'Phi', sembol: 'φ', deger: kk.etkiler.phi } : { ad: "Cramér's V", sembol: 'V', deger: kk.etkiler.cramerV }) : null;
  return { sonuc: { ...sonuc, etki }, kiKare: kk };
}

const KAT_YONTEM = { pearson: 'pearson-kikare', yates: 'yates-kikare', fisher: 'fisher', 'fisher-freeman-halton': 'fisher-freeman-halton' };
const SUR_YONTEM = { 'student-t': 'student-t', 'welch-t': 'welch-t', 'mann-whitney-u': 'mann-whitney-u', anova: 'anova', 'welch-anova': 'welch-anova', 'kruskal-wallis': 'kruskal-wallis' };

/** Sürekli / sıralı-sayısal değişkende seçilen testi çalıştırır */
export function surekliTestUygula(testId, gruplar, adlar, ayarlar) {
  const duzey = ayarlar.guvenDuzeyi;
  switch (testId) {
    case 'student-t':
      return { sonuc: P.bagimsizT(gruplar[0], gruplar[1], { esitVaryans: true, duzey }) };
    case 'welch-t':
      return { sonuc: P.bagimsizT(gruplar[0], gruplar[1], { esitVaryans: false, duzey }) };
    case 'mann-whitney-u':
      return { sonuc: NP.mannWhitneyU(gruplar[0], gruplar[1], { duzey }) };
    case 'anova':
      return { sonuc: P.tekYonluAnova(gruplar, { duzey, adlar }) };
    case 'welch-anova': {
      const w = P.welchAnova(gruplar);
      const klasik = P.tekYonluAnova(gruplar, { duzey, adlar });
      return { sonuc: { ...w, etki: klasik.etki } };
    }
    case 'kruskal-wallis':
      return { sonuc: NP.kruskalWallis(gruplar) };
    default:
      throw new IstatistikHatasi(`Bilinmeyen test: ${testId}`);
  }
}

/** İkili karşılaştırma tablosu satırları */
export function postHocSatirlari(postHocId, gruplar, adlar, ayarlar, b) {
  const duzey = ayarlar.guvenDuzeyi;
  if (postHocId === 'dunn') {
    const d = NP.dunn(gruplar, { duzeltme: ayarlar.parametrikOlmayanDuzeltme });
    return {
      sonuc: d,
      satirlar: d.karsilastirmalar.map((c) => ({ ad: `${adlar[c.grup1]} – ${adlar[c.grup2]}`, deger: `z = ${F.sayi(c.z, 2, b)}`, p: c.pDuzeltilmis })),
      baslik: 'z',
    };
  }
  const f = postHocId === 'tukey' ? H.tukeyHSD : H.gamesHowell;
  const r = f(gruplar, { duzey, adlar });
  return {
    sonuc: r,
    satirlar: r.karsilastirmalar.map((c) => ({ ad: `${adlar[c.i]} – ${adlar[c.j]}`, deger: `${F.sayi(c.fark, 2, b)} (${F.sayi(c.gaAlt, 2, b)}; ${F.sayi(c.gaUst, 2, b)})`, p: c.p })),
    baslik: 'Ortalama farkı (%95 GA)',
  };
}

const POST_HOC_ADI = { tukey: 'Tukey HSD', 'games-howell': 'Games-Howell', dunn: 'Dunn testi' };

/** Sıralı değişkenin değerini düzey sırasına göre sayısal koda çevirir */
function siraKodlari(vs, ad, grupAd, baglam) {
  const dz = duzeyler(vs, ad, baglam);
  const kod = new Map(dz.map((x, i) => [String(x.deger), i + 1]));
  const y = degiskenBul(vs, ad);
  const kopya = { ...y, ad: `${ad}__sira`, veriTuru: 'sayi', degerler: y.degerler.map((v) => (v === null || v === undefined ? null : kod.has(String(v)) ? kod.get(String(v)) : null)), eksikKodlar: null };
  const gecici = { ...vs, degiskenler: [...vs.degiskenler, kopya] };
  return gruplaraAyir(gecici, kopya.ad, grupAd, baglam);
}

export function grupAnalizi(secim, vs, baglam) {
  const { ayarlar, bicim: b, olcumler } = baglam;
  const alfa = ayarlar.alfa;
  const grupD = degiskenBul(vs, secim.grup);
  const grupAdi = gorunenAd(grupD);
  const gDuzey = duzeyler(vs, secim.grup, baglam);
  if (gDuzey.length < 2) throw new IstatistikHatasi(`"${grupAdi}" değişkeninde en az iki grup olmalı.`);
  const toplamN = gDuzey.reduce((a, x) => a + x.n, 0);
  const harf = harfAtayici();
  const tabloSatirlari = [];
  const ayrinti = [];
  const kararAlt = [];
  const postHocTablolari = [];
  const raporSatirlari = [];
  const grafikler = [];
  const yontemler = new Set();
  const uyarilar = [];
  const gosterimler = new Set();
  let eksikVar = false;
  const degiskenler = (secim.degiskenler || []).filter((a) => a !== secim.grup && ['surekli', 'sirali', 'kategorik'].includes(olcumler[a]));
  if (!degiskenler.length) throw new IstatistikHatasi('Karşılaştırılacak uygun değişken yok.');
  const grafikSayisiSiniri = degiskenler.length <= 6 ? 6 : 10;
  const grafikSec = grafikSecenek(baglam);
  const altuste = ayarlar.kategorikYuzde === 'satir';

  for (const ad of degiskenler) {
    const d = degiskenBul(vs, ad);
    const etiketAd = gorunenAd(d);
    const olcum = olcumler[ad];
    const benzersiz = new Set(d.degerler.filter((v) => v !== null && v !== undefined)).size;
    const sayisalSirali = olcum === 'sirali' && d.veriTuru === 'sayi' && benzersiz > 7;
    const kategorikGoster = olcum === 'kategorik' || (olcum === 'sirali' && !sayisalSirali);
    const siraTesti = olcum === 'sirali' && ayarlar.siraliTest !== 'kikare';

    try {
      if (!kategorikGoster) {
        // ---- Sürekli (ya da çok düzeyli sayısal sıralı) ----
        const ay = gruplaraAyir(vs, ad, secim.grup, baglam);
        if (ay.dislanan > 0) eksikVar = true;
        if (ay.gruplar.length < 2) {
          // Yalnızca bir grupta ölçülmüş (ör. yalnız hastalarda): betimsel olarak göster, test yapma
          const gosterim = olcum === 'sirali' ? 'medyan-ceyrek' : 'ortss';
          gosterimler.add(gosterim);
          const hucreler = gDuzey.map((g) => {
            const k = ay.duzeyler.findIndex((v) => String(v) === String(g.deger));
            return k < 0 ? '–' : surekliMetin(ay.gruplar[k], gosterim, b, ayarlar.yuzdelikTipi);
          });
          tabloSatirlari.push({ hucreler: [hucre(`${etiketAd}, ${GOSTERIM_ETIKETI[gosterim]}`, { sol: true }), ...hucreler, surekliMetin(ay.gruplar.flat(), gosterim, b, ayarlar.yuzdelikTipi), '–'] });
          uyarilar.push(`${etiketAd} yalnızca bir grupta (${ay.adlar.join(', ') || '—'}) ölçülmüş; karşılaştırma yapılmadı, betimsel değerler verildi.`);
          continue;
        }
        const testSec = surekliGrupTestiSec(ay.gruplar, ay.adlar, olcum, ayarlar, b);
        normallikYontemleri(testSec, yontemler);
        if (testSec.levene) yontemler.add('levene');
        const { sonuc } = surekliTestUygula(testSec.test, ay.gruplar, ay.adlar, ayarlar);
        yontemler.add(SUR_YONTEM[testSec.test]);
        const gosterim = olcum === 'sirali' ? (ayarlar.surekliGosterim === 'medyan-minmaks' ? 'medyan-minmaks' : 'medyan-ceyrek') : gosterimSec(testSec.parametrik, ayarlar);
        gosterimler.add(gosterim);
        const hucreler = gDuzey.map((g) => {
          const k = ay.duzeyler.findIndex((v) => String(v) === String(g.deger));
          return k < 0 ? '–' : surekliMetin(ay.gruplar[k], gosterim, b, ayarlar.yuzdelikTipi);
        });
        const tum = ay.gruplar.flat();
        const testHarfi = harf.al(testSec.test);
        tabloSatirlari.push({ hucreler: [hucre(`${etiketAd}, ${GOSTERIM_ETIKETI[gosterim]}`, { sol: true }), ...hucreler, surekliMetin(tum, gosterim, b, ayarlar.yuzdelikTipi), pHucresi(sonuc.p, alfa, b, testHarfi)] });
        ayrinti.push([etiketAd, TEST_ADLARI[testSec.test], istatistikHucresi(sonuc, b), pHucresi(sonuc.p, alfa, b), etkiMetni(sonuc.etki, b) || '–']);
        raporSatirlari.push(`${etiketAd}: ${testSatiri(sonuc, b)}`);
        kararAlt.push({ baslik: etiketAd, adimlar: testSec.adimlar });
        (sonuc.uyarilar || []).forEach((u) => uyarilar.push(`${etiketAd}: ${u}`));
        if (testSec.postHoc && (sonuc.p < alfa || ayarlar.postHocHerZaman)) {
          const ph = dene(() => postHocSatirlari(testSec.postHoc, ay.gruplar, ay.adlar, ayarlar, b), uyarilar, `${etiketAd} ikili karşılaştırma: `);
          if (ph) {
            yontemler.add(testSec.postHoc);
            if (testSec.postHoc === 'dunn') yontemler.add('bonferroni');
            postHocTablolari.push({ etiketAd, ph, yontem: testSec.postHoc });
          }
        }
        if (secim.grafik && grafikler.length < grafikSayisiSiniri && (degiskenler.length <= 6 || sonuc.p < alfa)) {
          grafikler.push({ tur: 'grafik', baslik: `${grupAdi} değişkenine göre ${etiketAd}`, svg: kutuGrafigi(ay.gruplar, ay.adlar, { ...grafikSec, xEtiket: grupAdi, yEtiket: etiketAd, ortalamaGoster: testSec.parametrik }), dosyaAdi: `${ad}-${secim.grup}` });
        }
      } else {
        // ---- Kategorik (ya da az düzeyli sıralı) ----
        const ct = caprazTablo(vs, ad, secim.grup, baglam);
        if (ct.dislanan > 0) eksikVar = true;
        const { tablo: testTablosu } = bosSatirSutunAt(ct.tablo);
        const sutunToplam = ct.sutunlar.map((_, j) => ct.tablo.reduce((a, s) => a + s[j], 0));
        const duzeySatirlari = () =>
          ct.satirlar.forEach((sat, i) => {
            const satirToplam = ct.tablo[i].reduce((a, v) => a + v, 0);
            if (satirToplam === 0) return;
            const hucreler = gDuzey.map((g) => {
              const j = ct.sutunlar.findIndex((s) => String(s.deger) === String(g.deger));
              if (j < 0 || sutunToplam[j] === 0) return '–';
              return F.nYuzde(ct.tablo[i][j], altuste ? satirToplam : sutunToplam[j], 1, b);
            });
            tabloSatirlari.push({ hucreler: [hucre(sat.etiket, { girinti: true, sol: true }), ...hucreler, F.nYuzde(satirToplam, ct.n, 1, b), ''] });
          });
        if (!testTablosu.length || testTablosu[0].length < 2 || testTablosu.length < 2) {
          tabloSatirlari.push({ tur: 'ara', hucreler: [hucre(`${etiketAd}, n (%)`, { sol: true }), ...gDuzey.map(() => ''), '', '–'] });
          duzeySatirlari();
          uyarilar.push(
            testTablosu.length < 2
              ? `${etiketAd} tek bir kategoriden oluşuyor; karşılaştırma yapılmadı.`
              : `${etiketAd} yalnızca bir grupta ölçülmüş; karşılaştırma yapılmadı, dağılımı verildi.`,
          );
          continue;
        }
        let sonuc;
        let testAdi;
        let adimlar;
        let testKimlik;
        if (siraTesti) {
          const ay = siraKodlari(vs, ad, secim.grup, baglam);
          const testSec = surekliGrupTestiSec(ay.gruplar, ay.adlar, 'sirali', ayarlar, b);
          sonuc = surekliTestUygula(testSec.test, ay.gruplar, ay.adlar, ayarlar).sonuc;
          testKimlik = testSec.test;
          testAdi = TEST_ADLARI[testSec.test];
          adimlar = testSec.adimlar;
          yontemler.add(SUR_YONTEM[testSec.test]);
          if (testSec.postHoc && (sonuc.p < alfa || ayarlar.postHocHerZaman)) {
            const ph = dene(() => postHocSatirlari(testSec.postHoc, ay.gruplar, ay.adlar, ayarlar, b), uyarilar, `${etiketAd} ikili karşılaştırma: `);
            if (ph) {
              yontemler.add('dunn');
              postHocTablolari.push({ etiketAd, ph, yontem: 'dunn' });
            }
          }
        } else {
          const ts = kategorikTestSec(testTablosu, ayarlar, b);
          const u = kategorikTestUygula(testTablosu, ts.test, b);
          sonuc = u.sonuc;
          testKimlik = ts.test;
          testAdi = TEST_ADLARI[ts.test];
          adimlar = [adim('Tasarım', `${etiketAd} (${ct.satirlar.length} kategori) × ${grupAdi} (${ct.sutunlar.length} grup) çapraz tablosu, n = ${ct.n}.`), ...ts.adimlar];
          yontemler.add(KAT_YONTEM[ts.test]);
          if (sonuc.pYontemi === 'monte-carlo') uyarilar.push(`${etiketAd}: tablo kesin hesap için çok büyük olduğundan p değeri Monte Carlo benzetimiyle (${sonuc.B} tekrar) hesaplandı.`);
        }
        const testHarfi = harf.al(testKimlik);
        tabloSatirlari.push({ tur: 'ara', hucreler: [hucre(`${etiketAd}, n (%)`, { sol: true }), ...gDuzey.map(() => ''), '', pHucresi(sonuc.p, alfa, b, testHarfi)] });
        duzeySatirlari();
        ayrinti.push([etiketAd, testAdi, istatistikHucresi(sonuc, b), pHucresi(sonuc.p, alfa, b), etkiMetni(sonuc.etki, b, false) || '–']);
        raporSatirlari.push(`${etiketAd}: ${testSatiri(sonuc, b)}`);
        kararAlt.push({ baslik: etiketAd, adimlar });
        (sonuc.uyarilar || []).forEach((u) => uyarilar.push(`${etiketAd}: ${u}`));
        if (secim.grafik && grafikler.length < grafikSayisiSiniri && (degiskenler.length <= 6 || sonuc.p < alfa)) {
          grafikler.push({
            tur: 'grafik',
            baslik: `${grupAdi} değişkenine göre ${etiketAd} dağılımı`,
            svg: caprazCubukGrafigi(ct.tablo, ct.satirlar.map((s) => s.etiket), ct.sutunlar.map((s) => s.etiket), { ...grafikSec, tip: 'kumelenmis', yuzde: true, yuzdeTabani: altuste ? 'satir' : 'sutun', xEtiket: grupAdi, yEtiket: 'Yüzde' }),
            dosyaAdi: `${ad}-${secim.grup}`,
          });
        }
      }
    } catch (e) {
      if (!(e instanceof IstatistikHatasi)) throw e;
      tabloSatirlari.push({ hucreler: [hucre(etiketAd, { sol: true }), ...gDuzey.map(() => '–'), '–', '–'] });
      uyarilar.push(`${etiketAd}: analiz yapılamadı (${e.message}).`);
    }
  }

  const testAdlari = { ...TEST_ADLARI };
  const notParcalari = [];
  if (gosterimler.has('ortss') || gosterimler.has('ikisi')) notParcalari.push('ort ± SS: ortalama ± standart sapma');
  if (gosterimler.has('medyan-ceyrek') || gosterimler.has('ikisi')) notParcalari.push('Ç1–Ç3: birinci ve üçüncü çeyreklik');
  if (gosterimler.has('medyan-minmaks')) notParcalari.push('min–maks: en küçük ve en büyük değer');
  if (tabloSatirlari.some((s) => s.tur === 'ara')) notParcalari.push(`kategorik değişkenlerde yüzdeler ${altuste ? 'satır içindedir' : 'grup içindedir'}`);
  notParcalari.push(`kalın p değerleri p < ${F.sayi(alfa, 2, b)}`);
  if (eksikVar) notParcalari.push('eksik veri nedeniyle bazı değişkenlerde toplam n farklıdır');

  const tablo1 = {
    baslik: `${grupAdi} değişkenine göre değişkenlerin karşılaştırılması`,
    basliklar: [[hucre('Değişken', { sol: true }), ...gDuzey.map((g) => `${g.etiket}\n(n = ${g.n})`), `Toplam\n(n = ${toplamN})`, 'p']],
    satirlar: tabloSatirlari,
    not: notParcalari.join('; ') + '.',
    dipnotlar: harf.dipnotlar(testAdlari),
  };

  const bloklar = [];
  bloklar.push({ tur: 'karar', baslik: 'Neden bu testler? (her değişken için)', alt: kararAlt });
  bloklar.push({ tur: 'tablo', tablo: tablo1 });
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Test istatistikleri ve etki büyüklükleri',
      basliklar: [[hucre('Değişken', { sol: true }), 'Test', 'İstatistik', 'p', 'Etki büyüklüğü']],
      satirlar: ayrinti.map((s) => ({ hucreler: s })),
      not: "Etki büyüklükleri: d, Cohen d; r = |Z|/√N; η², eta kare; ε², epsilon kare; φ, phi; V, Cramér's V. Yorum için Cohen (1988) sınırları kullanılabilir.",
      dipnotlar: [],
    },
  });
  for (const { etiketAd, ph, yontem } of postHocTablolari) {
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: `${etiketAd}: ikili karşılaştırmalar (${POST_HOC_ADI[yontem]})`,
        basliklar: [[hucre('Karşılaştırma', { sol: true }), ph.baslik, yontem === 'dunn' ? `p (${ayarlar.parametrikOlmayanDuzeltme === 'bonferroni' ? 'Bonferroni' : ayarlar.parametrikOlmayanDuzeltme} düzeltmeli)` : 'p']],
        satirlar: ph.satirlar.map((s) => ({ hucreler: [s.ad, s.deger, pHucresi(s.p, alfa, b)] })),
        dipnotlar: [],
      },
    });
  }
  bloklar.push({ tur: 'baslik', metin: 'Raporlama satırları' });
  raporSatirlari.forEach((m) => bloklar.push({ tur: 'satir', metin: m }));
  bloklar.push(...grafikler);
  if (secim.grafik && degiskenler.length > 6) bloklar.push({ tur: 'not', metin: 'Değişken sayısı fazla olduğundan yalnızca anlamlı fark bulunan değişkenlerin grafikleri gösterildi.' });
  bloklar.push(...uyariBloklari(uyarilar));

  return {
    baslik: degiskenler.length === 1 ? `${gorunenAd(degiskenBul(vs, degiskenler[0]))}: ${grupAdi} değişkenine göre karşılaştırma` : `${grupAdi} değişkenine göre karşılaştırma`,
    bloklar,
    yontemler: [...yontemler],
  };
}
