// Regresyon: sonuç değişkeninin türüne göre doğrusal ya da ikili lojistik regresyon; tek ve çok değişkenli sonuçlar,
// isteğe bağlı tarama (p < eşik) ve adımsal seçim, varsayım denetimleri.

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import { adimsalRegresyon, dogrusalRegresyon } from '../../cekirdek/regresyon.js';
import { lojistikRegresyon, multinomialLojistik, ordinalLojistik } from '../../cekirdek/lojistik.js';
import { shapiroWilk } from '../../cekirdek/normallik.js';
import { ormanGrafigi, qqGrafigi, sacilimGrafigi } from '../../grafik/grafikler.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim } from '../kurallar.js';
import { degiskenBul, deger, duzeyler, gorunenAd } from '../veri-erisim.js';
import { dene, grafikSecenek, pHucresi, uyariBloklari } from './ortak.js';
import { tahminci } from './sagkalim.js';
import { pozitifDuzeyTahmin } from '../../veri/tanima.js';

/** Katsayı satırının okunur adı: "Evre: III (ref. I)" */
function katsayiEtiketi(k, tahminciler) {
  if (!k.terim) return k.ad;
  const t = tahminciler.find((x) => x.ad === k.terim);
  if (!t || t.tur !== 'kategorik' || k.duzey === null || k.duzey === undefined) return k.terim;
  const et = (v) => t.etiketler?.[String(v)] ?? String(v);
  return `${t.ad}: ${et(k.duzey)} (ref. ${et(t.referans)})`;
}

/** Bir terimin (değişkenin) tek değişkenli p'si: kategorikse çok sd'li terim testi */
function terimP(sonuc, terimAd, lojistik) {
  const tt = sonuc.terimTestleri?.find((x) => x.ad === terimAd);
  if (tt) return lojistik ? tt.lrP ?? tt.waldP : tt.p;
  const k = sonuc.katsayilar.find((x) => x.terim === terimAd);
  return k?.p ?? NaN;
}

export function regresyonAnalizi(secim, vs, baglam) {
  const { ayarlar, olcumler } = baglam;
  const bagimliD = degiskenBul(vs, secim.bagimli);
  const olcum = olcumler[secim.bagimli];
  const bagimsizlar = [...new Set(secim.bagimsizlar || [])].filter((a) => a !== secim.bagimli && ['surekli', 'sirali', 'kategorik'].includes(olcumler[a]));
  if (!bagimsizlar.length) throw new IstatistikHatasi('En az bir bağımsız değişken seçin.');
  // Filtreye uyan satırlar
  const satirlar = [];
  for (let i = 0; i < vs.satirSayisi; i++) if (!baglam.satirMaskesi || baglam.satirMaskesi[i]) satirlar.push(i);
  const tahminciler = bagimsizlar.map((a) => tahminci(vs, a, olcumler, satirlar, baglam));
  const yHam = satirlar.map((i) => deger(bagimliD, i));
  const benzersiz = new Set(yHam.filter((v) => v !== null)).size;
  if (olcum === 'surekli' || (olcum === 'sirali' && bagimliD.veriTuru === 'sayi' && benzersiz > 7)) return dogrusal(secim, vs, baglam, bagimliD, tahminciler, yHam);
  if (benzersiz === 2) return lojistik(secim, vs, baglam, bagimliD, tahminciler, yHam);
  if (benzersiz >= 3 && benzersiz <= 10) return cokKategorili(secim, vs, baglam, bagimliD, tahminciler, yHam, olcum === 'sirali');
  throw new IstatistikHatasi(`Bağımlı değişkenin ${benzersiz} kategorisi var. İkili lojistik regresyon için iki kategoriye indirin (Değişkenler → Yeniden kodla).`);
}

function taramaUygula(secim, tahminciler, tekliP, adimlar, b) {
  if (!secim.tarama || secim.tarama === 'hepsi') return tahminciler;
  const esik = Number(secim.tarama);
  const secilen = tahminciler.filter((t) => tekliP.get(t.ad) < esik);
  adimlar.push(adim('Değişken taraması', `Tek değişkenli analizde p < ${F.sayi(esik, 2, b)} olan ${secilen.length} değişken çok değişkenli modele alındı${secilen.length < tahminciler.length ? ` (alınmayanlar: ${tahminciler.filter((t) => !secilen.includes(t)).map((t) => t.ad).join(', ')})` : ''}.`, secilen.length ? 'tamam' : 'uyari'));
  return secilen;
}

function dogrusal(secim, vs, baglam, bagimliD, tahminciler, yHam) {
  const { ayarlar, bicim: b } = baglam;
  const alfa = ayarlar.alfa;
  const yAd = gorunenAd(bagimliD);
  const uyarilar = [];
  const yontemler = new Set(['dogrusal-regresyon']);
  const y = yHam.map((v) => (typeof v === 'number' ? v : null));
  const gaY = Math.round(ayarlar.guvenDuzeyi * 100);
  const adimlar = [adim('Model', `Bağımlı değişken (${yAd}) sürekli → en küçük kareler yöntemiyle doğrusal regresyon. Kategorik bağımsız değişkenler gösterge (dummy) değişkenlerle, ilk kategori referans alınarak modele girdi.`)];

  // Tek değişkenli
  const tekli = new Map();
  const tekliP = new Map();
  for (const t of tahminciler) {
    const r = dene(() => dogrusalRegresyon({ y, tahminciler: [t], duzey: ayarlar.guvenDuzeyi }), uyarilar, `${t.ad} (tek değişkenli): `);
    if (!r) continue;
    r.katsayilar.filter((k) => k.terim).forEach((k) => tekli.set(katsayiEtiketi(k, [t]), k));
    tekliP.set(t.ad, terimP(r, t.ad, false));
  }
  const modele = taramaUygula(secim, tahminciler, tekliP, adimlar, b);
  if (!modele.length) throw new IstatistikHatasi('Tarama ölçütünü sağlayan değişken yok; çok değişkenli model kurulamadı.');

  let model;
  let adimsal = null;
  if (secim.yontem === 'ileri' || secim.yontem === 'geri') {
    adimsal = adimsalRegresyon({ y, tahminciler: modele, yontem: secim.yontem, girisP: 0.05, cikisP: 0.1, duzey: ayarlar.guvenDuzeyi });
    model = adimsal.sonModel;
    adimlar.push(adim('Değişken seçimi', `${secim.yontem === 'ileri' ? 'İleriye doğru seçim' : 'Geriye doğru eleme'} (giriş p < 0,05, çıkış p > 0,10): modelde kalan değişkenler ${adimsal.secilenler.length ? adimsal.secilenler.join(', ') : 'yok'}.`, 'tamam'));
    if (!model) throw new IstatistikHatasi('Adımsal seçimde hiçbir değişken modele girmedi.');
  } else {
    model = dogrusalRegresyon({ y, tahminciler: modele, duzey: ayarlar.guvenDuzeyi });
  }
  (model.uyarilar || []).forEach((u) => uyarilar.push(u));
  const mo = model.modelOzeti;

  // Varsayımlar
  const t = model.tanilama;
  const artik = t.artiklar;
  const sw = artik.length >= 3 && artik.length <= 5000 ? dene(() => shapiroWilk(artik), []) : null;
  const bp = t.breuschPagan?.koenker;
  const vifler = model.katsayilar.filter((k) => Number.isFinite(k.VIF));
  const maksVIF = vifler.length ? Math.max(...vifler.map((k) => k.VIF)) : null;
  const cookSiniri = 4 / mo.n;
  const etkili = t.cookD.filter((d) => d > cookSiniri).length;
  const ucDeger = t.standartlastirilmisArtiklar.filter((z) => Math.abs(z) > 3).length;
  const varsayimSatirlari = [
    ['Artıkların normalliği (Shapiro-Wilk)', sw ? `W = ${F.sayi(sw.istatistik, 3, b)}; ${F.pEsitlik(sw.p, b)}` : '–', sw ? (sw.p > alfa ? 'sağlanıyor' : mo.n > 50 ? 'sağlanmıyor (büyük örneklemde katsayı testleri yine de güvenilirdir)' : 'sağlanmıyor') : '–'],
    ['Varyans homojenliği (Breusch-Pagan, Koenker)', bp ? `χ²(${bp.sd}) = ${F.sayi(bp.istatistik, 2, b)}; ${F.pEsitlik(bp.p, b)}` : '–', bp ? (bp.p > alfa ? 'sağlanıyor' : 'sağlanmıyor (dayanıklı standart hata düşünülmeli)') : '–'],
    ['Çoklu bağlantı (en büyük VIF)', maksVIF ? F.sayi(maksVIF, 2, b) : '–', maksVIF ? (maksVIF < 5 ? 'sorun yok' : maksVIF < 10 ? 'orta düzeyde' : 'ciddi') : '–'],
    ['Otokorelasyon (Durbin-Watson)', F.sayi(mo.durbinWatson, 2, b), mo.durbinWatson >= 1.5 && mo.durbinWatson <= 2.5 ? 'sorun yok' : 'olası otokorelasyon'],
    ['Uç değer (|standart artık| > 3)', String(ucDeger), ucDeger ? 'incelenmeli' : 'yok'],
    [`Etkili gözlem (Cook D > 4/n = ${F.sayi(cookSiniri, 3, b)})`, String(etkili), etkili ? 'incelenmeli' : 'yok'],
  ];
  adimlar.push(adim('Varsayımlar', varsayimSatirlari.map(([a, d, k]) => `${a}: ${d} → ${k}`).join('. ') + '.', varsayimSatirlari.every(([, , k]) => /sağlanıyor|sorun yok|yok$/.test(k)) ? 'tamam' : 'uyari'));

  const bloklar = [{ tur: 'karar', adimlar }];
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Model özeti',
      basliklar: [[hucre('n', { sol: true }), 'R', 'R²', 'Düzeltilmiş R²', 'Tahminin SH', 'F', 'sd', 'p']],
      satirlar: [{ hucreler: [hucre(String(mo.n), { sol: true }), F.sayi(mo.R, 3, b), F.sayi(mo.R2, 3, b), F.sayi(mo.duzeltilmisR2, 3, b), F.sayi(mo.tahminSH, 3, b), F.sayi(mo.F, 2, b), `${mo.sd1}, ${mo.sd2}`, pHucresi(mo.p, alfa, b)] }],
      dipnotlar: [],
    },
  });
  const B = (k) => (k && Number.isFinite(k.B) ? `${F.sayi(k.B, 3, b)} (${F.ga(k.gaAlt, k.gaUst, 3, b)})` : '–');
  const coklu = model.katsayilar.filter((k) => k.terim);
  const etiketler = [...new Set([...tekli.keys(), ...coklu.map((k) => katsayiEtiketi(k, tahminciler))])];
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: `${yAd} değişkenini yordayan etkenler (doğrusal regresyon)`,
      basliklar: [
        [hucre('Değişken', { sol: true, satir: 2 }), hucre('Tek değişkenli', { sutun: 2 }), hucre('Çok değişkenli', { sutun: 4 })],
        [`B (%${gaY} GA)`, 'p', `B (%${gaY} GA)`, 'β', 'p', 'VIF'],
      ],
      satirlar: [
        ...etiketler.map((e) => {
          const tk = tekli.get(e);
          const ck = coklu.find((k) => katsayiEtiketi(k, tahminciler) === e);
          return { hucreler: [hucre(e, { sol: true }), B(tk), tk ? pHucresi(tk.p, alfa, b) : '–', ck ? B(ck) : '–', ck && Number.isFinite(ck.beta) ? F.sayi(ck.beta, 3, b) : '–', ck ? pHucresi(ck.p, alfa, b) : '–', ck && Number.isFinite(ck.VIF) ? F.sayi(ck.VIF, 2, b) : '–'] };
        }),
        (() => {
          const s = model.katsayilar.find((k) => !k.terim);
          return { tur: 'ara', hucreler: [hucre('Sabit', { sol: true }), '', '', B(s), '', s ? pHucresi(s.p, alfa, b) : '–', ''] };
        })(),
      ],
      not: `B: standartlaştırılmamış katsayı; β: standartlaştırılmış katsayı; VIF: varyans şişme faktörü. Çok değişkenli model: R² = ${F.sayi(mo.R2, 3, b)}, düzeltilmiş R² = ${F.sayi(mo.duzeltilmisR2, 3, b)}. "–": değişken modelde yok.`,
      dipnotlar: [],
    },
  });
  if (model.terimTestleri?.some((tt) => tt.tur === 'kategorik' && tt.sd > 1)) {
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Çok kategorili değişkenlerin genel etkisi',
        basliklar: [[hucre('Değişken', { sol: true }), 'F', 'sd', 'p', 'Kısmi η²']],
        satirlar: model.terimTestleri.filter((tt) => tt.tur === 'kategorik' && tt.sd > 1).map((tt) => ({ hucreler: [hucre(tt.ad, { sol: true }), F.sayi(tt.F, 2, b), `${tt.sd}, ${mo.sd2}`, pHucresi(tt.p, alfa, b), F.sayi(tt.kismiEtaKare, 3, b)] })),
        dipnotlar: [],
      },
    });
  }
  if (adimsal) {
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Adımsal seçim adımları',
        basliklar: [[hucre('Adım', { sol: true }), 'İşlem', 'Değişken', 'p', 'R²', 'R² değişimi']],
        satirlar: adimsal.adimlar.map((a) => ({ hucreler: [hucre(String(a.adim), { sol: true }), a.islem === 'giris' ? 'girdi' : 'çıktı', a.terim, F.p(a.p, b), F.sayi(a.R2, 3, b), F.sayi(a.R2Degisim, 3, b)] })),
        dipnotlar: [],
      },
    });
  }
  bloklar.push({ tur: 'tablo', tablo: { baslik: 'Model varsayımlarının denetimi', basliklar: [[hucre('Varsayım', { sol: true }), 'Sonuç', 'Değerlendirme']], satirlar: varsayimSatirlari.map(([a, d, k]) => ({ hucreler: [hucre(a, { sol: true }), d, k] })), dipnotlar: [] } });
  if (secim.grafik) {
    const g = grafikSecenek(baglam);
    bloklar.push({ tur: 'grafik', baslik: 'Artıklar ve tahmin edilen değerler', svg: sacilimGrafigi(t.uyumlanan, t.standartlastirilmisArtiklar, { ...g, xEtiket: 'Tahmin edilen değer', yEtiket: 'Standartlaştırılmış artık', regresyonCizgisi: false, gaBandi: false, denklemGoster: false }), dosyaAdi: 'artik-tahmin' });
    bloklar.push({ tur: 'grafik', baslik: 'Artıkların normal Q-Q grafiği', svg: qqGrafigi(t.standartlastirilmisArtiklar, { ...g, xEtiket: 'Beklenen normal değer', yEtiket: 'Standartlaştırılmış artık' }), dosyaAdi: 'artik-qq' });
  }
  bloklar.push({ tur: 'baslik', metin: 'Raporlama satırları' });
  bloklar.push({ tur: 'satir', metin: `Model: F(${mo.sd1}, ${mo.sd2}) = ${F.sayi(mo.F, 2, b)}; ${F.pEsitlik(mo.p, b)}; R² = ${F.sayi(mo.R2, 3, b)}; düzeltilmiş R² = ${F.sayi(mo.duzeltilmisR2, 3, b)}` });
  coklu.forEach((k) => bloklar.push({ tur: 'satir', metin: `${katsayiEtiketi(k, tahminciler)}: B = ${F.sayi(k.B, 3, b)} (%${gaY} GA: ${F.ga(k.gaAlt, k.gaUst, 3, b)}); β = ${F.sayi(k.beta, 3, b)}; t = ${F.sayi(k.t, 2, b)}; ${F.pEsitlik(k.p, b)}` }));
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `Doğrusal regresyon: ${yAd}`, bloklar, yontemler: [...yontemler] };
}

function lojistik(secim, vs, baglam, bagimliD, tahminciler, yHam) {
  const { ayarlar, bicim: b } = baglam;
  const alfa = ayarlar.alfa;
  const yAd = gorunenAd(bagimliD);
  const uyarilar = [];
  const dz = duzeyler(vs, secim.bagimli, baglam);
  const olay = secim.olayDuzeyi !== undefined && dz.some((x) => String(x.deger) === String(secim.olayDuzeyi)) ? dz.find((x) => String(x.deger) === String(secim.olayDuzeyi)).deger : pozitifDuzeyTahmin(dz);
  const olayEtiket = bagimliD.degerEtiketleri?.[String(olay)] ?? String(olay);
  const digerDeger = dz.find((x) => String(x.deger) !== String(olay))?.deger;
  const digerEtiket = bagimliD.degerEtiketleri?.[String(digerDeger)] ?? String(digerDeger);
  const y = yHam.map((v) => (v === null ? null : String(v)));
  const gaY = Math.round(ayarlar.guvenDuzeyi * 100);
  const adimlar = [adim('Model', `Bağımlı değişken (${yAd}) iki kategorili → ikili lojistik regresyon; modellenen olay: "${olayEtiket}". Kategorik bağımsız değişkenlerde ilk kategori referanstır. Katsayılar Wald testiyle, model olabilirlik oranı (omnibus) testiyle sınandı.`)];

  const tekli = new Map();
  const tekliP = new Map();
  for (const t of tahminciler) {
    const r = dene(() => lojistikRegresyon({ y, olay: String(olay), tahminciler: [t], duzey: ayarlar.guvenDuzeyi, profilGA: false }), uyarilar, `${t.ad} (tek değişkenli): `);
    if (!r) continue;
    r.katsayilar.filter((k) => k.terim).forEach((k) => tekli.set(katsayiEtiketi(k, [t]), k));
    tekliP.set(t.ad, terimP(r, t.ad, true));
    (r.uyarilar || []).filter((u) => /ayrışma/i.test(u)).forEach((u) => uyarilar.push(`${t.ad} (tek değişkenli): ${u}`));
  }
  const modele = taramaUygula(secim, tahminciler, tekliP, adimlar, b);
  if (!modele.length) throw new IstatistikHatasi('Tarama ölçütünü sağlayan değişken yok; çok değişkenli model kurulamadı.');
  const m = lojistikRegresyon({ y, olay: String(olay), tahminciler: modele, duzey: ayarlar.guvenDuzeyi });
  (m.uyarilar || []).forEach((u) => uyarilar.push(u));
  const mo = m.modelOzeti;
  const hl = m.hosmerLemeshow;
  const yontemler = new Set(['lojistik-regresyon']);
  if (hl && Number.isFinite(hl.p)) yontemler.add('hosmer-lemeshow');
  const epvSorun = m.epv && m.epv.deger < 10;
  adimlar.push(
    adim(
      'Model uyumu',
      `Omnibus χ²(${mo.sd}) = ${F.sayi(mo.kiKare, 2, b)}, ${F.pEsitlik(mo.p, b)}; Nagelkerke R² = ${F.sayi(mo.nagelkerkeR2, 3, b)}; ${hl && Number.isFinite(hl.p) ? `Hosmer-Lemeshow χ²(${hl.sd}) = ${F.sayi(hl.istatistik, 2, b)}, ${F.pEsitlik(hl.p, b)} (${hl.p > alfa ? 'uyum iyi' : 'uyum zayıf'})` : 'Hosmer-Lemeshow hesaplanamadı'}; doğru sınıflama %${F.sayi(m.siniflama.dogruSiniflamaYuzdesi, 1, b)}; AUC = ${F.sayi(m.auc, 3, b)}.`,
      hl && hl.p > alfa ? 'tamam' : 'uyari',
    ),
  );
  if (m.epv) adimlar.push(adim('Örneklem yeterliliği', `Parametre başına olay sayısı (EPV) = ${F.sayi(m.epv.deger, 1, b)}${epvSorun ? " — 10'un altında; model aşırı uyum gösterebilir, değişken sayısını azaltmayı düşünün." : ' (≥ 10, yeterli).'}`, epvSorun ? 'uyari' : 'tamam'));
  if (m.ayrisma) adimlar.push(adim('Ayrışma', `${m.ayrisma.tur === 'tam' ? 'Tam' : 'Yarı'} ayrışma saptandı; ilgili katsayılar güvenilir değil.`, 'uyari'));

  const bloklar = [{ tur: 'karar', adimlar }];
  const or = (k) => (k?.ayrisma ? 'ayrışma' : k && Number.isFinite(k.OR) ? `${F.sayi(k.OR, 2, b)} (${F.ga(k.orGaAlt, k.orGaUst, 2, b)})` : '–');
  const coklu = m.katsayilar.filter((k) => k.terim);
  const etiketler = [...new Set([...tekli.keys(), ...coklu.map((k) => katsayiEtiketi(k, tahminciler))])];
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: `${yAd} (${olayEtiket}) ile ilişkili etkenler (lojistik regresyon)`,
      basliklar: [
        [hucre('Değişken', { sol: true, satir: 2 }), hucre('Tek değişkenli', { sutun: 2 }), hucre('Çok değişkenli', { sutun: 5 })],
        [`OR (%${gaY} GA)`, 'p', 'B', 'SH', 'Wald', `OR (%${gaY} GA)`, 'p'],
      ],
      satirlar: [
        ...etiketler.map((e) => {
          const tk = tekli.get(e);
          const ck = coklu.find((k) => katsayiEtiketi(k, tahminciler) === e);
          if (ck?.ayrisma) return { hucreler: [hucre(e, { sol: true }), or(tk), tk && Number.isFinite(tk.p) ? pHucresi(tk.p, alfa, b) : '–', hucre('ayrışma nedeniyle kestirilemedi', { sutun: 5 })] };
          return {
            hucreler: [
              hucre(e, { sol: true }),
              or(tk),
              tk && Number.isFinite(tk.p) ? pHucresi(tk.p, alfa, b) : '–',
              ck && Number.isFinite(ck.B) ? F.sayi(ck.B, 3, b) : '–',
              ck && Number.isFinite(ck.SH) ? F.sayi(ck.SH, 3, b) : '–',
              ck && Number.isFinite(ck.wald) ? F.sayi(ck.wald, 2, b) : '–',
              or(ck),
              ck && Number.isFinite(ck.p) ? pHucresi(ck.p, alfa, b) : '–',
            ],
          };
        }),
        (() => {
          const s = m.katsayilar.find((k) => !k.terim);
          return { tur: 'ara', hucreler: [hucre('Sabit', { sol: true }), '', '', s && Number.isFinite(s.B) ? F.sayi(s.B, 3, b) : '–', s && Number.isFinite(s.SH) ? F.sayi(s.SH, 3, b) : '–', s && Number.isFinite(s.wald) ? F.sayi(s.wald, 2, b) : '–', '', s && Number.isFinite(s.p) ? pHucresi(s.p, alfa, b) : '–'] };
        })(),
      ],
      not: `OR: olasılık (odds) oranı; GA: Wald güven aralığı (SPSS ile aynı). Çok değişkenli model: n = ${mo.n}, olay = ${m.olaySayisi}; −2 log olabilirlik = ${F.sayi(mo.ikiLL, 2, b)}; Cox-Snell R² = ${F.sayi(mo.coxSnellR2, 3, b)}; Nagelkerke R² = ${F.sayi(mo.nagelkerkeR2, 3, b)}. "–": değişken modelde yok.`,
      dipnotlar: [],
    },
  });
  if (m.terimTestleri?.some((tt) => tt.tur === 'kategorik' && tt.sd > 1)) {
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Çok kategorili değişkenlerin genel etkisi',
        basliklar: [[hucre('Değişken', { sol: true }), 'Wald', 'sd', 'p', 'Olabilirlik oranı χ²', 'p']],
        satirlar: m.terimTestleri.filter((tt) => tt.tur === 'kategorik' && tt.sd > 1).map((tt) => ({ hucreler: [hucre(tt.ad, { sol: true }), F.sayi(tt.wald, 2, b), String(tt.sd), pHucresi(tt.waldP, alfa, b), F.sayi(tt.lrKiKare, 2, b), pHucresi(tt.lrP, alfa, b)] })),
        dipnotlar: [],
      },
    });
  }
  const s = m.siniflama;
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: `Sınıflama tablosu (kesim değeri ${F.sayi(s.kesim, 2, b)})`,
      basliklar: [[hucre('Gözlenen', { sol: true, satir: 2 }), hucre('Tahmin edilen', { sutun: 2 }), hucre('Doğru %', { satir: 2 })], [digerEtiket, olayEtiket]],
      satirlar: [
        { hucreler: [hucre(digerEtiket, { sol: true }), String(s.tablo.dogruNegatif), String(s.tablo.yanlisPozitif), F.sayi(s.olmayanDogruYuzde, 1, b)] },
        { hucreler: [hucre(olayEtiket, { sol: true }), String(s.tablo.yanlisNegatif), String(s.tablo.dogruPozitif), F.sayi(s.olayDogruYuzde, 1, b)] },
        { tur: 'ara', hucreler: [hucre('Genel', { sol: true }), '', '', F.sayi(s.dogruSiniflamaYuzdesi, 1, b)] },
      ],
      not: `Yalnızca sabitli modelde doğru sınıflama %${F.sayi(m.blok0?.siniflama?.dogruSiniflamaYuzdesi, 1, b)}. Modelin ayırt ediciliği: AUC = ${F.sayi(m.auc, 3, b)}.`,
      dipnotlar: [],
    },
  });
  if (secim.grafik && coklu.some((k) => Number.isFinite(k.OR))) {
    bloklar.push({
      tur: 'grafik',
      baslik: 'Çok değişkenli modelde olasılık oranları',
      svg: ormanGrafigi(coklu.filter((k) => Number.isFinite(k.OR)).map((k) => ({ ad: katsayiEtiketi(k, tahminciler), tahmin: k.OR, alt: k.orGaAlt, ust: k.orGaUst, p: k.p })), { ...grafikSecenek(baglam), logOlcek: true, referans: 1 }),
      dosyaAdi: 'lojistik-orman',
    });
  }
  bloklar.push({ tur: 'baslik', metin: 'Raporlama satırları' });
  bloklar.push({ tur: 'satir', metin: `Model: χ²(${mo.sd}) = ${F.sayi(mo.kiKare, 2, b)}; ${F.pEsitlik(mo.p, b)}; Nagelkerke R² = ${F.sayi(mo.nagelkerkeR2, 3, b)}` });
  coklu.filter((k) => Number.isFinite(k.OR)).forEach((k) => bloklar.push({ tur: 'satir', metin: `${katsayiEtiketi(k, tahminciler)}: OR = ${F.sayi(k.OR, 2, b)} (%${gaY} GA: ${F.ga(k.orGaAlt, k.orGaUst, 2, b)}); ${F.pEsitlik(k.p, b)}` }));
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `Lojistik regresyon: ${yAd} (${olayEtiket})`, bloklar, yontemler: [...yontemler] };
}

/** Sıralı sonuç → ordinal (orantılı odds) lojistik; sırasız çok kategorili sonuç → multinomial lojistik regresyon */
function cokKategorili(secim, vs, baglam, bagimliD, tahminciler, yHam, sirali) {
  const { ayarlar, bicim: b } = baglam;
  const alfa = ayarlar.alfa;
  const yAd = gorunenAd(bagimliD);
  const uyarilar = [];
  const dz = duzeyler(vs, secim.bagimli, baglam);
  const et = (v) => bagimliD.degerEtiketleri?.[String(v)] ?? String(v);
  const y = yHam.map((v) => (v === null ? null : String(v)));
  const duzeyListesi = dz.map((x) => String(x.deger));
  const gaY = Math.round(ayarlar.guvenDuzeyi * 100);
  const or = (k) => (k?.ayrisma ? 'ayrışma' : k && Number.isFinite(k.OR) ? `${F.sayi(k.OR, 2, b)} (${F.ga(k.orGaAlt, k.orGaUst, 2, b)})` : '–');
  const sayiH = (v, k = 3) => (Number.isFinite(v) ? F.sayi(v, k, b) : '–');
  const bloklar = [];
  const adimlar = [];
  if (sirali) {
    const m = ordinalLojistik({ y, duzeyler: duzeyListesi, tahminciler, duzey: ayarlar.guvenDuzeyi });
    (m.uyarilar || []).forEach((u) => uyarilar.push(u));
    const mo = m.modelOzeti;
    const pc = m.paralelCizgiler;
    adimlar.push(adim('Model', `Bağımlı değişken (${yAd}) sıralı (${dz.map((x) => x.etiket).join(' < ')}) → orantılı odds (ordinal) lojistik regresyon. OR > 1, daha yüksek kategoride olma olasılığının arttığını gösterir.`));
    adimlar.push(
      adim(
        'Paralel çizgiler varsayımı',
        pc ? `χ²(${pc.sd}) = ${F.sayi(pc.kiKare, 2, b)}; ${F.pEsitlik(pc.p, b)} → ${pc.p > alfa ? 'sağlanıyor' : 'sağlanmıyor; multinomial lojistik regresyon düşünülmeli'}.` : 'Sınanamadı (bazı kategorilerde ayrışma ya da boş hücre).',
        pc && pc.p > alfa ? 'tamam' : 'uyari',
      ),
    );
    bloklar.push({ tur: 'karar', adimlar });
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: `${yAd} ile ilişkili etkenler (ordinal lojistik regresyon)`,
        basliklar: [[hucre('Değişken', { sol: true }), 'B', 'SH', 'Wald', 'p', `OR (%${gaY} GA)`]],
        satirlar: [
          ...m.katsayilar.map((k) => ({ hucreler: [hucre(katsayiEtiketi(k, tahminciler), { sol: true }), sayiH(k.B), sayiH(k.SH), sayiH(k.wald, 2), Number.isFinite(k.p) ? pHucresi(k.p, alfa, b) : '–', or(k)] })),
          ...m.esikler.map((e, i) => ({ tur: i === 0 ? 'ara' : undefined, hucreler: [hucre(`Eşik: ${e.ad.split('|').map(et).join(' | ')}`, { sol: true }), sayiH(e.B), sayiH(e.SH), sayiH(e.wald, 2), Number.isFinite(e.p) ? pHucresi(e.p, alfa, b) : '–', ''] })),
        ],
        not: `Model: −2LL = ${F.sayi(mo.ikiLL, 2, b)}; χ²(${mo.sd}) = ${F.sayi(mo.kiKare, 2, b)}, ${F.pEsitlik(mo.p, b)}; Nagelkerke R² = ${F.sayi(mo.nagelkerkeR2, 3, b)}. Parametreleştirme SPSS PLUM ve R polr ile aynıdır.`,
        dipnotlar: [],
      },
    });
    m.katsayilar.filter((k) => Number.isFinite(k.OR)).forEach((k) => bloklar.push({ tur: 'satir', metin: `${katsayiEtiketi(k, tahminciler)}: OR = ${F.sayi(k.OR, 2, b)} (%${gaY} GA: ${F.ga(k.orGaAlt, k.orGaUst, 2, b)}); ${F.pEsitlik(k.p, b)}` }));
  } else {
    const ref = duzeyListesi[0];
    const m = multinomialLojistik({ y, duzeyler: duzeyListesi, referansKategori: ref, tahminciler, duzey: ayarlar.guvenDuzeyi });
    (m.uyarilar || []).forEach((u) => uyarilar.push(u));
    const mo = m.modelOzeti;
    adimlar.push(adim('Model', `Bağımlı değişken (${yAd}) ${dz.length} kategorili ve sırasız → multinomial lojistik regresyon; referans kategori: ${et(ref)}. Her kategori referansla ayrı ayrı karşılaştırılır.`));
    bloklar.push({ tur: 'karar', adimlar });
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Değişkenlerin genel etkisi (olabilirlik oranı testleri)',
        basliklar: [[hucre('Değişken', { sol: true }), '−2LL (indirgenmiş)', 'χ²', 'sd', 'p']],
        satirlar: m.olabilirlikOraniTestleri.map((t) => ({ hucreler: [hucre(t.ad, { sol: true }), sayiH(t.ikiLLIndirgenmis, 2), sayiH(t.kiKare, 2), String(t.sd), pHucresi(t.p, alfa, b)] })),
        not: `Model: χ²(${mo.sd}) = ${F.sayi(mo.kiKare, 2, b)}, ${F.pEsitlik(mo.p, b)}; Nagelkerke R² = ${F.sayi(mo.nagelkerkeR2, 3, b)}.`,
        dipnotlar: [],
      },
    });
    for (const k of m.kategoriler) {
      bloklar.push({
        tur: 'tablo',
        tablo: {
          baslik: `${et(k.kategori)} (referans: ${et(ref)})`,
          basliklar: [[hucre('Değişken', { sol: true }), 'B', 'SH', 'Wald', 'p', `OR (%${gaY} GA)`]],
          satirlar: k.katsayilar.map((c) => ({ hucreler: [hucre(c.terim ? katsayiEtiketi(c, tahminciler) : 'Sabit', { sol: true }), sayiH(c.B), sayiH(c.SH), sayiH(c.wald, 2), Number.isFinite(c.p) ? pHucresi(c.p, alfa, b) : '–', c.terim ? or(c) : ''] })),
          dipnotlar: [],
        },
      });
    }
  }
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `${sirali ? 'Ordinal' : 'Multinomial'} lojistik regresyon: ${yAd}`, bloklar, yontemler: ['lojistik-regresyon'] };
}
