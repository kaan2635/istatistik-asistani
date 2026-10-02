// SVG grafik kütüphanesi testleri: biçim (iyi biçimli XML, boyutlar, NaN yok), eksen adımları, sayı biçimi,
// iç hesaplar (histogram kutuları, Q-Q konumları, kutu grafiği özetleri, OLS güven bandı) ve dışa aktarım yardımcıları.
// Çalıştırma: node --test test/grafik.test.js

import { test, describe } from 'node:test';
import assert from 'node:assert/strict';
import * as G from '../src/grafik/grafikler.js';
import {
  guzelAdimlar,
  logAdimlar,
  sayiBicimle,
  yuzdeBicimle,
  pBicimle,
  metniSar,
  metinGenisligi,
  kacis,
  sayisalEksenHazirla,
  secenekHazirla,
  ondalikSayisi,
  tohumluRastgele,
  simgeleriItalikYap,
  renkKaristir,
} from '../src/grafik/ortak.js';
import { svgBoyutlari, pngDpiAyarla } from '../src/grafik/disaAktar.js';
import { yakin } from './yardimci.js';

/* ---------------- Yardımcılar ---------------- */

/**
 * Küçük XML denetleyicisi: etiket dengesi, nitelik sözdizimi, yinelenen nitelik, metindeki kaçışsız < ve &.
 * Yalnızca bu kütüphanenin ürettiği alt küme için (yorum, CDATA, işlem talimatı yok).
 */
function xmlDogrula(xml) {
  const yigin = [];
  const etiketRe = /<(\/?)([A-Za-z][\w:.-]*)((?:\s+[A-Za-z_:][\w:.-]*="[^"<]*")*)\s*(\/?)>/g;
  const nitelikRe = /([A-Za-z_:][\w:.-]*)="([^"<]*)"/g;
  const varlikRe = /&(?!(?:amp|lt|gt|quot|apos|#\d+|#x[0-9a-fA-F]+);)/;
  let son = 0;
  let m;
  let sayi = 0;
  while ((m = etiketRe.exec(xml))) {
    const metin = xml.slice(son, m.index);
    assert.ok(!/[<>]/.test(metin), `kaçışsız < ya da > metinde: ${JSON.stringify(metin.slice(0, 80))}`);
    assert.ok(!varlikRe.test(metin), `kaçışsız & metinde: ${JSON.stringify(metin.slice(0, 80))}`);
    const [, kapanis, ad, nitelikler, kendiKapanan] = m;
    if (kapanis) {
      assert.equal(nitelikler.trim(), '', `kapanış etiketinde nitelik: ${ad}`);
      const ust = yigin.pop();
      assert.equal(ust, ad, `etiket dengesi bozuk: </${ad}> beklenen </${ust}>`);
    } else {
      const gorulen = new Set();
      let n;
      while ((n = nitelikRe.exec(nitelikler))) {
        assert.ok(!gorulen.has(n[1]), `yinelenen nitelik ${n[1]} <${ad}>`);
        gorulen.add(n[1]);
        assert.ok(!varlikRe.test(n[2]), `nitelikte kaçışsız &: ${n[1]}="${n[2]}"`);
      }
      if (!kendiKapanan) yigin.push(ad);
    }
    son = etiketRe.lastIndex;
    sayi++;
  }
  assert.equal(xml.slice(son).trim(), '', 'kök öğeden sonra fazladan içerik');
  assert.equal(yigin.length, 0, `kapanmamış etiketler: ${yigin.join(', ')}`);
  assert.equal((xml.match(/</g) || []).length, sayi, 'tanınmayan < dizisi');
}

/** Her grafik çıktısı için ortak denetimler */
function denetle(svg, gen = 640, yuk = 420, ad = '') {
  assert.equal(typeof svg, 'string', `${ad}: SVG metni değil`);
  assert.ok(svg.startsWith('<svg xmlns="http://www.w3.org/2000/svg"'), `${ad}: kök öğe`);
  xmlDogrula(svg);
  const kok = svg.slice(0, svg.indexOf('>') + 1);
  assert.match(kok, new RegExp(`\\swidth="${gen}"`), `${ad}: genişlik`);
  assert.match(kok, new RegExp(`\\sheight="${yuk}"`), `${ad}: yükseklik`);
  assert.match(kok, new RegExp(`\\sviewBox="0 0 ${gen} ${yuk}"`), `${ad}: viewBox`);
  assert.ok(!/NaN|undefined|Infinity|\bnull\b|\[object/.test(svg), `${ad}: geçersiz metin (NaN/undefined/Infinity/null)`);
  assert.ok(!/\sclass="/.test(svg), `${ad}: CSS sınıfı kullanılmamalı`);
  assert.ok(!/<style/.test(svg), `${ad}: <style> kullanılmamalı`);
  // Her metin öğesi yazı tipini ve boyutunu kendisi taşır
  for (const t of svg.match(/<text\b[^>]*>/g) || []) {
    assert.match(t, /font-family="/, `${ad}: metinde font-family yok`);
    assert.match(t, /font-size="/, `${ad}: metinde font-size yok`);
  }
  // Desen başvuruları tanımlı kimliklere gider, kimlikler tekildir
  const kimlikler = [...svg.matchAll(/\sid="([^"]+)"/g)].map((k) => k[1]);
  assert.equal(new Set(kimlikler).size, kimlikler.length, `${ad}: yinelenen id`);
  for (const r of svg.matchAll(/url\(#([^)]+)\)/g)) assert.ok(kimlikler.includes(r[1]), `${ad}: tanımsız başvuru #${r[1]}`);
  // Tüm koordinatlar kanvasın makul çevresinde
  for (const k of svg.matchAll(/\s(?:x|y|x1|x2|y1|y2|cx|cy)="(-?[\d.]+)"/g)) {
    const v = Number(k[1]);
    assert.ok(v > -50 && v < Math.max(gen, yuk) + 50, `${ad}: kanvas dışı koordinat ${k[0]}`);
  }
}

const rng0 = tohumluRastgele(42);
function normal(n, m = 0, s = 1, rng = rng0) {
  return Array.from({ length: n }, () => {
    const u = Math.max(1e-12, rng());
    const v = rng();
    return m + s * Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  });
}
const ort = (v) => v.reduce((a, b) => a + b, 0) / v.length;

const UZUN = [
  'Çok uzun Türkçe grup adı: İğdır, Şanlıurfa ve Çanakkale öğrencileri',
  'Öğretmenlik uygulaması sırasında ölçülen kaygı düzeyi (ön test)',
  'Ğ Ü Ş İ Ö Ç ğüşiöçı — üçüncü grup',
];

/** Uç durum senaryoları: her biri gruplar (number[][]) ve adlar */
const SENARYOLAR = {
  normal: { gruplar: [normal(40, 50, 10), normal(35, 55, 12), normal(30, 48, 9)], adlar: ['Kontrol', 'Deney A', 'Deney B'] },
  tekGrup: { gruplar: [normal(25, 10, 2)], adlar: ['Tek grup'] },
  onGrup: { gruplar: Array.from({ length: 10 }, (_, i) => normal(15, i * 3, 2)), adlar: Array.from({ length: 10 }, (_, i) => `Grup ${i + 1}`) },
  n1: { gruplar: [[7]], adlar: ['A'] },
  n2: { gruplar: [[3, 9], [4, 4.5]], adlar: ['A', 'B'] },
  sabit: { gruplar: [[5, 5, 5, 5, 5], [5, 5, 5]], adlar: ['Sabit 1', 'Sabit 2'] },
  negatif: { gruplar: [normal(20, -50, 10), normal(20, -10, 5)], adlar: ['Eksi 1', 'Eksi 2'] },
  buyuk: { gruplar: [normal(30, 1e6, 2e5), normal(30, 1.6e6, 3e5)], adlar: ['Gelir A', 'Gelir B'] },
  uzunEtiket: { gruplar: [normal(20, 50, 10), normal(20, 52, 10), normal(20, 47, 10)], adlar: UZUN },
};

/** Basit Kaplan–Meier adımları (yalnızca girdi üretmek için) */
function kmAdimlari(degerler) {
  const t = degerler.map((v) => Math.abs(v)).sort((a, b) => a - b);
  const n = t.length;
  const adimlar = [];
  const sansur = [];
  let S = 1;
  t.forEach((z, i) => {
    if (i % 4 === 3) {
      sansur.push(z);
      return;
    }
    S *= 1 - 1 / (n - i);
    adimlar.push({ zaman: z, sagkalim: S, gaAlt: Math.max(0, S - 0.1), gaUst: Math.min(1, S + 0.1) });
  });
  return { adimlar, sansur };
}

function rocNoktalari(poz, neg) {
  const esik = [...new Set([...poz, ...neg])].sort((a, b) => b - a);
  return [{ fpr: 0, tpr: 0 }, ...esik.map((e) => ({ fpr: neg.filter((v) => v >= e).length / neg.length, tpr: poz.filter((v) => v >= e).length / poz.length }))];
}

/** Her grafik için senaryodan girdi üreten bağdaştırıcılar */
const GRAFIKLER = {
  histogram: (v, o) => G.histogram(v.gruplar.flat(), o),
  histogramSturgesKutu: (v, o) => G.histogram(v.gruplar.flat(), { ...o, kutuSayisi: 'sturges', normalEgri: false }),
  histogramSabitKutu: (v, o) => G.histogram(v.gruplar.flat(), { ...o, kutuSayisi: 7 }),
  qqGrafigi: (v, o) => G.qqGrafigi(v.gruplar[0], { ...o, gaBandi: true }),
  kutuGrafigi: (v, o) => G.kutuGrafigi(v.gruplar, v.adlar, { ...o, ortalamaGoster: true, nGoster: true }),
  kutuNoktali: (v, o) => G.kutuGrafigi(v.gruplar, v.adlar, { ...o, noktalar: true, yuzdelikTipi: 'tukey' }),
  ortalamaHataCubuk: (v, o) => G.ortalamaHataGrafigi(v.gruplar, v.adlar, o),
  ortalamaHataNokta: (v, o) => G.ortalamaHataGrafigi(v.gruplar, v.adlar, { ...o, tip: 'nokta', hata: 'ss' }),
  ortalamaHataOzet: (v, o) =>
    G.ortalamaHataGrafigi(
      v.gruplar.map((g) => ({ ortalama: ort(g), alt: Math.min(...g), ust: Math.max(...g) })),
      v.adlar,
      { ...o, hata: 'sh' },
    ),
  profilGrafigi: (v, o) =>
    G.profilGrafigi(
      v.gruplar.map((g, j) => ({
        ad: v.adlar[j],
        noktalar: ['Ön test', 'Son test', 'İzleme'].map((x, k) => ({ x, ortalama: ort(g) + k * (j + 1), alt: ort(g) + k * (j + 1) - 1.5, ust: ort(g) + k * (j + 1) + 1.5 })),
      })),
      { ...o, hataNotu: 'Hata çubukları %95 GA.' },
    ),
  sacilimGrafigi: (v, o) => {
    const x = v.gruplar[0];
    return G.sacilimGrafigi(x, x.map((a, i) => 2 * a + (i % 3) - 1), o);
  },
  sacilimGruplu: (v, o) => {
    const x = v.gruplar.flat();
    const g = v.gruplar.flatMap((gr, j) => gr.map(() => v.adlar[j]));
    return G.sacilimGrafigi(x, x.map((a, i) => -a + (i % 5)), { ...o, gruplar: g, regresyonCizgisi: 'grup' });
  },
  kaplanMeierGrafigi: (v, o) => {
    const egriler = v.gruplar.map((g, j) => {
      const k = kmAdimlari(g);
      return { ad: v.adlar[j], adimlar: k.adimlar, sansurZamanlari: k.sansur };
    });
    const tMax = Math.max(...v.gruplar.flat().map(Math.abs), 1);
    const zamanlar = [0, tMax / 4, tMax / 2, (3 * tMax) / 4];
    const sayilar = Object.fromEntries(v.gruplar.map((g, j) => [v.adlar[j], zamanlar.map((z) => g.filter((d) => Math.abs(d) >= z).length)]));
    return G.kaplanMeierGrafigi(egriler, { ...o, gaGoster: true, notMetni: 'Log-rank p = 0,012', riskTablosu: { zamanlar, sayilar } });
  },
  kaplanMeierYuzde: (v, o) => {
    const k = kmAdimlari(v.gruplar[0]);
    return G.kaplanMeierGrafigi([{ ad: v.adlar[0], adimlar: k.adimlar, sansurZamanlari: k.sansur }], { ...o, yuzde: true, yMin: 0.2 });
  },
  rocGrafigi: (v, o) => {
    const poz = v.gruplar[0];
    const neg = v.gruplar[1] || v.gruplar[0].map((a) => a - 1);
    return G.rocGrafigi(
      [
        { ad: v.adlar[0], noktalar: rocNoktalari(poz, neg), auc: 0.81, aucGa: [0.72, 0.9] },
        { ad: 'İkinci model', noktalar: rocNoktalari(neg, poz), auc: 0.19 },
      ],
      { ...o, optimum: { fpr: 0.25, tpr: 0.75, etiket: 'Kesim = 12,5\nDuyarlılık 0,75' } },
    );
  },
  blandAltmanGrafigi: (v, o) => {
    const a = v.gruplar[0];
    return G.blandAltmanGrafigi(a, a.map((x, i) => x * 0.01 + (i % 4) - 1.5), { ...o, gaCizgileri: true });
  },
  caprazKumelenmis: (v, o) => {
    const tablo = v.gruplar.map((g, i) => [g.length, (i * 3) % 7 + 1, 2]);
    return G.caprazCubukGrafigi(tablo, v.adlar, ['Lisans', 'Yüksek lisans', 'Doktora'], o);
  },
  caprazYigilmis: (v, o) => {
    const tablo = v.gruplar.map((g, i) => [g.length, (i * 3) % 7 + 1, 0]);
    return G.caprazCubukGrafigi(tablo, v.adlar, UZUN, { ...o, tip: 'yigilmis', yuzdeTabani: 'satir' });
  },
  caprazSayi: (v, o) => G.caprazCubukGrafigi(v.gruplar.map((g) => [g.length, 3]), v.adlar, ['Evet', 'Hayır'], { ...o, yuzde: false }),
  frekansGrafigi: (v, o) => G.frekansGrafigi(v.adlar, v.gruplar.map((g) => g.length), o),
  frekansYatay: (v, o) => G.frekansGrafigi(v.adlar, v.gruplar.map((g) => g.length), { ...o, yatay: true, degerEtiketi: 'ikisi', sirala: 'azalan' }),
  frekansSayi: (v, o) => G.frekansGrafigi(v.adlar, v.gruplar.map((g) => g.length), { ...o, yuzde: false }),
  pastaGrafigi: (v, o) => G.pastaGrafigi(v.adlar, v.gruplar.map((g) => g.length), { ...o, sayiGoster: true }),
  korelasyonIsiHaritasi: (v, o) => {
    const k = v.gruplar.length;
    const r = Array.from({ length: k }, (_, i) => Array.from({ length: k }, (__, j) => (i === j ? 1 : Math.cos(i + j) * 0.9)));
    const p = r.map((s, i) => s.map((x, j) => (i === j ? 0 : Math.abs(x) > 0.6 ? 0.0004 : Math.abs(x) > 0.3 ? 0.02 : 0.4)));
    return G.korelasyonIsiHaritasi(r, v.adlar, { ...o, p });
  },
  korelasyonAlt: (v, o) => {
    const k = v.gruplar.length;
    const r = Array.from({ length: k }, (_, i) => Array.from({ length: k }, (__, j) => (i === j ? 1 : i === 0 ? NaN : -0.3)));
    return G.korelasyonIsiHaritasi(r, v.adlar, { ...o, ucgen: 'alt' });
  },
  ormanGrafigi: (v, o) =>
    G.ormanGrafigi(
      [
        { ad: 'Başlık satırı', baslik: true },
        ...v.gruplar.map((g, j) => {
          const m = ort(g);
          const e = g.length > 1 ? Math.abs(m) * 0.2 + 0.1 : 0;
          return { ad: `  ${v.adlar[j]}`, tahmin: m, alt: m - e, ust: m + e, p: 0.03 * (j + 1), agirlik: g.length };
        }),
        { ad: 'Genel', tahmin: 1.2, alt: 1.05, ust: 1.4, ozet: true },
      ],
      { ...o, yonEtiketleri: ['Kontrol lehine', 'Deney lehine'], xEtiket: 'Odds oranı' },
    ),
  ormanDogrusal: (v, o) =>
    G.ormanGrafigi(
      v.gruplar.map((g, j) => ({ ad: v.adlar[j], tahmin: ort(g), alt: Math.min(...g), ust: Math.max(...g) })),
      { ...o, logOlcek: false, etiket: 'Fark (%95 GA)' },
    ),
  yamacGrafigi: (v, o) => {
    const ev = v.gruplar.map((g) => Math.abs(ort(g)) / 10).sort((a, b) => b - a);
    return G.yamacGrafigi(ev, { ...o, paralelAnaliz: ev.map((_, i) => 1.5 - i * 0.1) });
  },
  noktaGrafigi: (v, o) => G.noktaGrafigi(v.gruplar, v.adlar, { ...o, hata: 'ga95' }),
  stripGrafigi: (v, o) => G.stripGrafigi(v.gruplar, v.adlar, { ...o, merkez: 'medyan' }),
};

/* ---------------- Tüm grafikler × tüm senaryolar ---------------- */

describe('tüm grafikler tüm senaryolarda geçerli SVG üretir', () => {
  for (const [grafikAdi, f] of Object.entries(GRAFIKLER)) {
    test(grafikAdi, () => {
      for (const [senaryo, veri] of Object.entries(SENARYOLAR)) {
        for (const tema of ['yayin', 'renkli']) {
          const ad = `${grafikAdi}/${senaryo}/${tema}`;
          const svg = f(veri, { tema, baslik: `${grafikAdi} — ${senaryo}`, xEtiket: 'X ekseni (birim)', yEtiket: 'Y ekseni (birim)' });
          // Orman grafiği yükseklik verilmezse satır sayısından hesaplar
          const yuk = Number(/\sheight="(\d+)"/.exec(svg)?.[1]);
          denetle(svg, 640, grafikAdi.startsWith('orman') ? yuk : 420, ad);
        }
      }
    });
  }
});

describe('boyut ve seçenekler', () => {
  test('istenen genişlik/yükseklik ve viewBox korunur (büyük ve küçük)', () => {
    const v = SENARYOLAR.normal;
    for (const [gen, yuk] of [
      [800, 500],
      [360, 260],
      [1200, 900],
    ]) {
      for (const [ad, f] of Object.entries(GRAFIKLER)) denetle(f(v, { genislik: gen, yukseklik: yuk }), gen, yuk, `${ad} ${gen}x${yuk}`);
    }
  });

  test('ondalık ayırıcı nokta seçeneği', () => {
    const svg = G.histogram([0.12, 0.25, 0.31, 0.45, 0.38], { ondalikAyrac: '.' });
    assert.match(svg, />\d+\.\d+</);
    const svgTr = G.histogram([0.12, 0.25, 0.31, 0.45, 0.38]);
    assert.match(svgTr, />\d+,\d+</);
  });

  test('özel renkler, yazı tipi ve boyutu satır içi uygulanır', () => {
    const svg = G.frekansGrafigi(['a', 'b'], [3, 4], { renkler: ['#123456'], yaziTipi: 'Times New Roman, serif', yaziBoyutu: 10 });
    assert.match(svg, /fill="#123456"/);
    assert.match(svg, /font-family="Times New Roman, serif"/);
    assert.ok(!/font-size="1[2-9]"/.test(svg.replace(/<title>.*<\/title>/, '')), 'yazı boyutu küçültülmeli');
  });

  test('Türkçe karakterler aynen yazılır, özel karakterler kaçırılır', () => {
    const svg = G.kutuGrafigi([[1, 2, 3], [4, 5, 6]], ['Çığ İşçi Ğğ Üü Öö Şş', '<b>&"x\'']);
    assert.ok(svg.includes('Çığ İşçi Ğğ Üü Öö Şş'));
    assert.ok(svg.includes('&lt;b&gt;&amp;&quot;x&#39;'));
    xmlDogrula(svg);
  });

  test('boş ya da geçersiz girdi açıklamalı boş grafik verir', () => {
    for (const svg of [
      G.histogram([]),
      G.histogram([NaN, null, undefined, Infinity]),
      G.qqGrafigi([]),
      G.kutuGrafigi([], []),
      G.ortalamaHataGrafigi([], []),
      G.profilGrafigi([]),
      G.sacilimGrafigi([], []),
      G.kaplanMeierGrafigi([]),
      G.rocGrafigi([]),
      G.blandAltmanGrafigi([], []),
      G.caprazCubukGrafigi([], [], []),
      G.frekansGrafigi([], []),
      G.pastaGrafigi(['a'], [0]),
      G.korelasyonIsiHaritasi([], []),
      G.ormanGrafigi([]),
      G.yamacGrafigi([]),
      G.noktaGrafigi([], []),
    ]) {
      denetle(svg);
      assert.match(svg, /yeterli veri yok/);
    }
  });

  test('aynı girdi aynı çıktıyı verir (belirlenimci; titreşim tohumlu)', () => {
    const v = SENARYOLAR.normal;
    assert.equal(G.kutuGrafigi(v.gruplar, v.adlar, { noktalar: true }), G.kutuGrafigi(v.gruplar, v.adlar, { noktalar: true }));
    assert.equal(G.noktaGrafigi(v.gruplar, v.adlar), G.noktaGrafigi(v.gruplar, v.adlar));
  });

  test('girdi dizileri değiştirilmez', () => {
    const g = [[3, 1, 2], [9, 7, 8]];
    const kopya = JSON.stringify(g);
    G.kutuGrafigi(g, ['a', 'b']);
    G.histogram(g[0]);
    G.qqGrafigi(g[1]);
    G.noktaGrafigi(g, ['a', 'b']);
    assert.equal(JSON.stringify(g), kopya);
  });

  test('desenli dolgular (çok seri) yayın temasında tanımlanır ve kimlikleri içerikten türer', () => {
    const tablo = Array.from({ length: 10 }, (_, i) => [i + 1, 10 - i]);
    const a = G.caprazCubukGrafigi(tablo, tablo.map((_, i) => `S${i}`), ['A', 'B'], { tip: 'yigilmis' });
    assert.match(a, /<pattern /);
    const b = G.caprazCubukGrafigi(tablo.map((r) => [r[0] + 1, r[1]]), tablo.map((_, i) => `S${i}`), ['A', 'B'], { tip: 'yigilmis' });
    const idA = /<pattern id="([^"]+)"/.exec(a)[1];
    const idB = /<pattern id="([^"]+)"/.exec(b)[1];
    assert.notEqual(idA, idB, 'farklı grafiklerin desen kimlikleri çakışmamalı');
  });

  test('renkli tema 8 renkten sonra yeni renk üretmez (renk + desen)', () => {
    const tablo = Array.from({ length: 10 }, (_, i) => [i + 1, 10 - i]);
    const svg = G.caprazCubukGrafigi(tablo, tablo.map((_, i) => `S${i}`), ['A', 'B'], { tema: 'renkli' });
    const dolgular = new Set([...svg.matchAll(/<rect[^>]*fill="(#[0-9a-f]{6})"[^>]*stroke="#ffffff"/g)].map((m) => m[1]));
    assert.ok(dolgular.size <= 8, `renk sayısı ${dolgular.size}`);
    assert.match(svg, /<pattern /);
  });

  test('ROC çizim alanı karedir', () => {
    const svg = G.rocGrafigi([{ ad: 'M', noktalar: [{ fpr: 0, tpr: 0 }, { fpr: 0.2, tpr: 0.7 }, { fpr: 1, tpr: 1 }], auc: 0.8 }]);
    const kosegen = /<line x1="([\d.]+)" y1="([\d.]+)" x2="([\d.]+)" y2="([\d.]+)"[^>]*stroke-dasharray="4 3"/.exec(svg);
    assert.ok(kosegen, 'köşegen çizgisi');
    const [x1, y1, x2, y2] = kosegen.slice(1).map(Number);
    yakin(x2 - x1, y1 - y2, 0, 0.02, 'kare');
    assert.match(svg, /AUC = 0,800/);
  });

  test('Kaplan–Meier (0, 1) noktasından başlar; risk tablosu ve not yazılır', () => {
    const svg = G.kaplanMeierGrafigi(
      [{ ad: 'A', adimlar: [{ zaman: 2, sagkalim: 0.8 }, { zaman: 5, sagkalim: 0.5 }], sansurZamanlari: [3, 7] }],
      { riskTablosu: { zamanlar: [0, 2, 4, 6], sayilar: { A: [10, 8, 6, 3] } }, notMetni: 'Log-rank p = 0,012' },
    );
    assert.match(svg, /Risk altındaki sayı/);
    assert.match(svg, /<tspan font-style="italic">p<\/tspan> = 0,012/);
    const yol = /<path d="M([\d.]+) ([\d.]+)L([\d.]+) ([\d.]+)/.exec(svg);
    assert.ok(yol);
    assert.equal(yol[2], yol[4], 'ilk parça yatay (S = 1)');
  });
});

/* ---------------- Eksen adımları ve biçim ---------------- */

describe('güzel eksen adımları', () => {
  test('0–97 → 0, 20, 40, 60, 80, 100', () => {
    assert.deepEqual(guzelAdimlar(0, 97, 5).adimlar, [0, 20, 40, 60, 80, 100]);
  });
  test('0–1 → 0,2 adım', () => {
    assert.deepEqual(guzelAdimlar(0, 1, 5).adimlar, [0, 0.2, 0.4, 0.6, 0.8, 1]);
  });
  test('negatif aralık ve kayan nokta artığı yok', () => {
    const g = guzelAdimlar(-3.2, 7.9, 5);
    assert.deepEqual(g.adimlar, [-4, -2, 0, 2, 4, 6, 8]);
    assert.deepEqual(guzelAdimlar(0.1, 0.7, 6).adimlar, [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7]);
  });
  test('adımlar her zaman 1, 2, 2,5, 5 × 10^k', () => {
    const rng = tohumluRastgele(5);
    for (let i = 0; i < 500; i++) {
      const a = (rng() - 0.5) * 10 ** Math.floor(rng() * 12 - 4);
      const b = a + rng() * 10 ** Math.floor(rng() * 10 - 3);
      const g = guzelAdimlar(a, b, 2 + Math.floor(rng() * 8));
      const m = g.adim / 10 ** Math.floor(Math.log10(g.adim) + 1e-9);
      assert.ok([1, 2, 2.5, 5].some((c) => Math.abs(m - c) < 1e-9), `adım ${g.adim}`);
      // Aralık, aralık genişliğinin 1e-9'u içinde kapsanır (kayan nokta artığı eksen uçlarını büyütmesin)
      assert.ok(g.alt <= a + 1e-9 * (b - a) && g.ust >= b - 1e-9 * (b - a), `aralık kapsanmalı: [${a}, ${b}] → [${g.alt}, ${g.ust}]`);
    }
  });
  test('sabit değer ve tamsayı kısıtı', () => {
    const g = guzelAdimlar(5, 5, 5);
    assert.ok(g.alt < 5 && g.ust > 5);
    assert.deepEqual(guzelAdimlar(0, 3, 8, { tamsayi: true }).adimlar, [0, 1, 2, 3]);
  });
  test('log adımları', () => {
    assert.deepEqual(logAdimlar(0.01, 120).adimlar, [0.01, 0.1, 1, 10, 100, 1000]);
    assert.ok(logAdimlar(0.3, 4.2).adimlar.includes(1));
    assert.deepEqual(logAdimlar(0.8, 1.3).adimlar, [0.8, 0.9, 1, 1.1, 1.2, 1.3]);
  });
  test('x ekseni etiketleri üst üste binmez (büyük sayılar, dar eksen)', () => {
    const s = secenekHazirla({});
    for (const [min, max, uzunluk] of [
      [0, 2.5e6, 180],
      [-1e9, 1e9, 220],
      [0.000123, 0.000987, 150],
      [0, 97, 600],
    ]) {
      const e = sayisalEksenHazirla(s, { tur: 'sayisal', min, max }, 'x', uzunluk);
      const olcek = (v) => ((v - e.alan[0]) / (e.alan[1] - e.alan[0])) * uzunluk;
      let onceki = null;
      e.adimlar.forEach((v, i) => {
        if (!e.goster[i]) return;
        const w = metinGenisligi(e.etiketler[i], s.fs.tik);
        const x = olcek(v);
        if (onceki) assert.ok(x - w / 2 >= onceki.x + onceki.w / 2, `çakışma: ${onceki.t} / ${e.etiketler[i]}`);
        onceki = { x, w, t: e.etiketler[i] };
      });
    }
  });
});

describe('sayı biçimlendirme', () => {
  test('ondalık virgül', () => {
    assert.equal(sayiBicimle(0.5, 1), '0,5');
    assert.equal(sayiBicimle(0.5, 1, { ondalikAyrac: '.' }), '0.5');
    assert.equal(sayiBicimle(-1.5, 1), '−1,5');
    assert.equal(sayiBicimle(-0.0001, 2), '0,00');
    assert.equal(sayiBicimle(1234, 0), '1234');
    assert.equal(sayiBicimle(12345.678, 2), '12.345,68');
    assert.equal(sayiBicimle(1e6, 0), '1.000.000');
    assert.equal(sayiBicimle(1e6, 0, { ondalikAyrac: '.' }), '1,000,000');
    assert.equal(sayiBicimle(NaN), '–');
  });
  test('yüzde, p ve ondalık sayısı', () => {
    assert.equal(yuzdeBicimle(45, 1), '%45,0');
    assert.equal(yuzdeBicimle(45, 1, { ondalikAyrac: '.' }), '45.0%');
    assert.equal(pBicimle(0.0004), '< 0,001');
    assert.equal(pBicimle(0.01234), '0,012');
    assert.equal(pBicimle(0.01234, { basSifir: false }), ',012');
    assert.equal(ondalikSayisi(0.25), 2);
    assert.equal(ondalikSayisi(2.5), 1);
    assert.equal(ondalikSayisi(20), 0);
  });
  test('metin kaydırma, genişlik ve italik simgeler', () => {
    const satir = metniSar('Kesinlikle katılmıyorum ve çok uzun bir açıklama', 80, 11);
    assert.ok(satir.length > 1);
    for (const t of satir) assert.ok(metinGenisligi(t, 11) <= 80 + 1e-9, t);
    assert.ok(metinGenisligi('WWW', 12) > metinGenisligi('iii', 12));
    assert.equal(metinGenisligi('ş', 12), metinGenisligi('s', 12));
    assert.deepEqual(simgeleriItalikYap('Log-rank p = 0,012; n = 5'), ['Log-rank ', { t: 'p', italik: true }, ' = 0,012; ', { t: 'n', italik: true }, ' = 5']);
    assert.equal(kacis('a<b & "c"'), 'a&lt;b &amp; &quot;c&quot;');
  });
  test('renk karıştırma uçları korur', () => {
    assert.equal(renkKaristir('#1c5cab', '#ffffff', 0), '#1c5cab');
    assert.equal(renkKaristir('#1c5cab', '#ffffff', 1), '#ffffff');
  });
});

/* ---------------- Grafik içi hesaplar ---------------- */

describe('grafik içi hesaplar', () => {
  test('histogram kutuları: toplam n, güzel sınırlar, sol kapalı', () => {
    const x = normal(100, 50, 10, tohumluRastgele(9));
    const { kenarlar, sayilar } = G.histogramKutulari(x);
    assert.equal(sayilar.reduce((a, b) => a + b, 0), 100);
    assert.ok(kenarlar[0] <= Math.min(...x) && kenarlar[kenarlar.length - 1] >= Math.max(...x));
    const h = kenarlar[1] - kenarlar[0];
    assert.ok([1, 2, 2.5, 5].some((c) => Math.abs(h / 10 ** Math.floor(Math.log10(h)) - c) < 1e-9));
    const k = G.histogramKutulari([0, 1, 2, 3, 4], 'sturges');
    assert.deepEqual(k.sayilar.reduce((a, b) => a + b, 0), 5);
    // Kenardaki değer sağdaki kutuya düşer ([a, b)); en büyük değer son kutuda
    const t = G.histogramKutulari([0, 10, 10, 20], 2);
    assert.deepEqual(t.kenarlar, [0, 10, 20]);
    assert.deepEqual(t.sayilar, [1, 3]);
    const g = G.histogramKutulari([1, 2, 3, 7], 'otomatik', 2);
    assert.deepEqual(g.kenarlar, [0, 2, 4, 6, 8]);
    assert.deepEqual(g.sayilar, [1, 2, 0, 1]);
    const sabit = G.histogramKutulari([5, 5, 5]);
    assert.equal(sabit.sayilar.length, 1);
    assert.ok(sabit.kenarlar[0] < 5 && sabit.kenarlar[1] > 5);
  });

  test('Q-Q konumları: Blom (SPSS) ve R ppoints', () => {
    const b = G.qqNoktalari([5, 1, 4, 2, 3]);
    assert.deepEqual(b.gozlenen, [1, 2, 3, 4, 5]);
    b.p.forEach((p, i) => yakin(p, (i + 1 - 0.375) / 5.25, 1e-12, 0, `blom ${i}`));
    // R: qnorm(ppoints(5)) → -1.17976, -0.49720, 0, 0.49720, 1.17976
    yakin(b.teorik[0], -1.179761118, 1e-8, 0, 'z1');
    yakin(b.teorik[2], 0, 0, 1e-15, 'z3');
    const r = G.qqNoktalari(Array.from({ length: 20 }, (_, i) => i), 'r');
    r.p.forEach((p, i) => yakin(p, (i + 1 - 0.5) / 20, 1e-12, 0, `ppoints ${i}`));
  });

  test('Q-Q referans doğrusu R qqline ile aynı', () => {
    // R: qqline(1:10) → eğim 4,5 / (2 · qnorm(0,75)) = 3,335855; kesim 5,5
    const d = G.qqDogrusu([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    yakin(d.egim, 4.5 / (2 * 0.6744897501960817), 1e-12, 0, 'eğim');
    yakin(d.kesim, 5.5, 1e-12, 1e-12, 'kesim');
  });

  test('kutu grafiği özetleri (SPSS kuralları)', () => {
    const k = G.kutuIstatistikleri([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 25, 100]);
    yakin(k.q1, 3.25, 1e-12, 0, 'Q1 (tip 6)');
    yakin(k.q3, 9.75, 1e-12, 0, 'Q3 (tip 6)');
    yakin(k.medyan, 6.5, 1e-12, 0, 'ortanca');
    assert.equal(k.ustBiyik, 10);
    assert.equal(k.altBiyik, 1);
    assert.deepEqual(k.aykirilar, [25]);
    assert.deepEqual(k.ucDegerler, [100]);
    const t = G.kutuIstatistikleri([1, 2, 3, 4, 5, 6, 7], 'tukey');
    assert.equal(t.q1, 2.5);
    assert.equal(t.q3, 5.5);
    const r7 = G.kutuIstatistikleri([1, 2, 3, 4, 5, 6, 7, 8], 7);
    yakin(r7.q1, 2.75, 1e-12, 0, 'Q1 tip 7');
    const sabit = G.kutuIstatistikleri([5, 5, 5, 5, 5, 5, 9]);
    assert.deepEqual(sabit.ucDegerler, [9]);
    assert.equal(G.kutuIstatistikleri([]), null);
  });

  test('OLS doğrusu ve ortalama için %95 güven bandı (R predict interval="confidence")', () => {
    const f = G.olsDogrusu([1, 2, 3, 4, 5], [2, 4, 5, 4, 5]);
    yakin(f.egim, 0.6, 1e-12, 0, 'eğim');
    yakin(f.kesim, 2.2, 1e-12, 0, 'kesim');
    yakin(f.r2, 0.6, 1e-12, 0, 'R²');
    const [a, b] = f.bant(3);
    // R: predict(lm(y ~ x), data.frame(x = 3), interval = "confidence") → 2,727021; 5,272979
    // t(0,975; 3) = 3,182446305284263; SH = √0,8 · √(1/5) = 0,4
    yakin(a, 4 - 3.182446305284263 * 0.4, 1e-10, 0, 'alt');
    yakin(b, 4 + 3.182446305284263 * 0.4, 1e-10, 0, 'üst');
    const [a1, b1] = f.bant(1);
    // R: x = 1 → fit 2.8, lwr 0.5952, upr 5.0048 (se = sqrt(0.8 * (1/5 + 4/10)))
    yakin(b1 - a1, 2 * 3.182446305 * Math.sqrt(0.8 * (0.2 + 0.4)), 1e-8, 0, 'genişlik x=1');
    assert.equal(G.olsDogrusu([1, 1, 1], [1, 2, 3]), null);
  });

  test('arı sürüsü yerleşiminde noktalar üst üste binmez', () => {
    const ys = [10, 10, 10, 10, 12, 13, 50, 51, 52, 52.5];
    const r = 3;
    const dx = G.suruYerlesimi(ys, r, 100);
    for (let i = 0; i < ys.length; i++) {
      for (let j = i + 1; j < ys.length; j++) {
        const d = Math.hypot(dx[i] - dx[j], ys[i] - ys[j]);
        assert.ok(d >= 2 * r - 1e-6, `binme ${i}-${j}: ${d}`);
      }
    }
    assert.equal(dx[6] === 0 || Math.abs(dx[6]) < 1e-9, true, 'yalnız nokta merkezde');
  });
});

/* ---------------- Dışa aktarım yardımcıları ---------------- */

function crc32(b) {
  let c = 0xffffffff;
  for (const v of b) {
    c ^= v;
    for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
  }
  return (c ^ 0xffffffff) >>> 0;
}

function obek(tur, veri) {
  const b = new Uint8Array(12 + veri.length);
  const dv = new DataView(b.buffer);
  dv.setUint32(0, veri.length);
  for (let i = 0; i < 4; i++) b[4 + i] = tur.charCodeAt(i);
  b.set(veri, 8);
  dv.setUint32(8 + veri.length, crc32(b.subarray(4, 8 + veri.length)));
  return b;
}

describe('dışa aktarım (saf yardımcılar)', () => {
  test('SVG boyutları okunur', () => {
    assert.deepEqual(svgBoyutlari(G.histogram([1, 2, 3], { genislik: 700, yukseklik: 300 })), { genislik: 700, yukseklik: 300 });
    assert.deepEqual(svgBoyutlari('<svg viewBox="0 0 50 40"></svg>'), { genislik: 50, yukseklik: 40 });
    assert.throws(() => svgBoyutlari('<div/>'));
  });

  test('PNG pHYs öbeği (DPI) eklenir ya da değiştirilir', () => {
    const imza = new Uint8Array([137, 80, 78, 71, 13, 10, 26, 10]);
    const ihdr = obek('IHDR', new Uint8Array([0, 0, 0, 1, 0, 0, 0, 1, 8, 2, 0, 0, 0]));
    const iend = obek('IEND', new Uint8Array(0));
    const eskiPhys = obek('pHYs', new Uint8Array([0, 0, 14, 195, 0, 0, 14, 195, 1]));
    for (const girdi of [[imza, ihdr, iend], [imza, ihdr, eskiPhys, iend]]) {
      const png = new Uint8Array(girdi.reduce((t, p) => t + p.length, 0));
      let k = 0;
      for (const p of girdi) {
        png.set(p, k);
        k += p.length;
      }
      const sonuc = pngDpiAyarla(png, 300);
      const metin = String.fromCharCode(...sonuc);
      assert.equal(metin.split('pHYs').length - 1, 1, 'tek pHYs');
      const i = metin.indexOf('pHYs');
      const dv = new DataView(sonuc.buffer, sonuc.byteOffset);
      assert.equal(dv.getUint32(i - 4), 9);
      assert.equal(dv.getUint32(i + 4), Math.round(300 / 0.0254));
      assert.equal(dv.getUint32(i + 8), Math.round(300 / 0.0254));
      assert.equal(sonuc[i + 12], 1);
      assert.equal(dv.getUint32(i + 13), crc32(sonuc.subarray(i, i + 13)), 'CRC');
      assert.ok(metin.indexOf('IHDR') < i && i < metin.indexOf('IEND'), 'öbek sırası');
    }
    assert.throws(() => pngDpiAyarla(new Uint8Array(40), 300));
  });

  test('tarayıcı işlevleri Node\'da içe aktarılırken çalışmaz, çağrılınca anlaşılır hata verir', async () => {
    const m = await import('../src/grafik/disaAktar.js');
    for (const ad of ['svgIndir', 'pngIndir', 'svgdenPngDataUrl', 'svgdenPngBlob']) assert.equal(typeof m[ad], 'function', ad);
    await assert.rejects(() => m.svgdenPngBlob('<svg width="10" height="10"></svg>'), /tarayıcı/);
  });
});
