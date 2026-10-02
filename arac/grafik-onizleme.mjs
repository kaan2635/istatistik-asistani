// Grafik önizlemesi: tüm grafikleri örnek verilerle iki temada çizer ve cikti/grafik-onizleme.html'e yazar.
// Çalıştırma: node arac/grafik-onizleme.mjs

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import * as G from '../src/grafik/grafikler.js';
import { tohumluRastgele } from '../src/grafik/ortak.js';

const KOK = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
let rng = tohumluRastgele(20261002);
const normal = (n, m = 0, s = 1) =>
  Array.from({ length: n }, () => {
    const u = Math.max(1e-12, rng());
    const v = rng();
    return m + s * Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  });
const ustel = (n, oran = 1) => Array.from({ length: n }, () => -Math.log(Math.max(1e-12, rng())) / oran);

/** Basit Kaplan–Meier (Greenwood GA, log-log yok) — yalnızca önizleme için */
function km(zaman, olay) {
  const sira = zaman.map((t, i) => [t, olay[i]]).sort((a, b) => a[0] - b[0]);
  let risk = sira.length;
  let S = 1;
  let gw = 0;
  const adimlar = [];
  const sansur = [];
  for (let i = 0; i < sira.length; ) {
    const t = sira[i][0];
    let d = 0;
    let c = 0;
    while (i < sira.length && sira[i][0] === t) {
      if (sira[i][1]) d++;
      else c++;
      i++;
    }
    if (d > 0) {
      S *= 1 - d / risk;
      gw += d / (risk * (risk - d) || 1);
      const se = S * Math.sqrt(gw);
      adimlar.push({ zaman: t, sagkalim: S, gaAlt: Math.max(0, S - 1.96 * se), gaUst: Math.min(1, S + 1.96 * se) });
    }
    if (c > 0) sansur.push(t);
    risk -= d + c;
  }
  return { adimlar, sansur };
}
function riskSayilari(zaman, noktalar) {
  return noktalar.map((t) => zaman.filter((z) => z >= t).length);
}
function roc(pozitif, negatif) {
  const esikler = [...new Set([...pozitif, ...negatif])].sort((a, b) => b - a);
  const noktalar = [{ fpr: 0, tpr: 0 }];
  for (const e of esikler) noktalar.push({ fpr: negatif.filter((v) => v >= e).length / negatif.length, tpr: pozitif.filter((v) => v >= e).length / pozitif.length });
  let auc = 0;
  for (const p of pozitif) for (const q of negatif) auc += p > q ? 1 : p === q ? 0.5 : 0;
  return { noktalar, auc: auc / (pozitif.length * negatif.length) };
}

const uzunAdlar = ['Kesinlikle katılmıyorum', 'Katılmıyorum', 'Kararsızım', 'Katılıyorum', 'Kesinlikle katılıyorum'];
const gruplar3 = [normal(30, 52, 9), normal(28, 58, 10).concat([95, 99, 12]), normal(35, 49, 8)];
const zA = ustel(60, 0.08).map((t) => Math.round(t * 10) / 10);
const oA = zA.map(() => (rng() < 0.75 ? 1 : 0));
const zB = ustel(55, 0.15).map((t) => Math.round(t * 10) / 10);
const oB = zB.map(() => (rng() < 0.8 ? 1 : 0));
const kmA = km(zA, oA);
const kmB = km(zB, oB);
const riskZ = [0, 10, 20, 30, 40];
const rocA = roc(normal(60, 1.2), normal(80, 0));
const rocB = roc(normal(60, 0.6), normal(80, 0));
const xs = normal(80, 50, 10);
const ys = xs.map((x) => 12 + 0.6 * x + normal(1, 0, 6)[0]);
const grupEt = xs.map((_, i) => ['Kadın', 'Erkek', 'Diğer'][i % 3]);
const r5 = [
  [1, 0.62, -0.35, 0.12, 0.48],
  [0.62, 1, -0.21, 0.05, 0.33],
  [-0.35, -0.21, 1, -0.71, -0.08],
  [0.12, 0.05, -0.71, 1, 0.27],
  [0.48, 0.33, -0.08, 0.27, 1],
];
const p5 = r5.map((s, i) => s.map((v, j) => (i === j ? 0 : Math.abs(v) > 0.6 ? 0.0004 : Math.abs(v) > 0.4 ? 0.004 : Math.abs(v) > 0.3 ? 0.03 : 0.4)));
const ozdeger = [4.21, 2.05, 1.32, 0.98, 0.71, 0.55, 0.43, 0.31, 0.25, 0.19];
const paralel = [1.62, 1.44, 1.31, 1.2, 1.1, 1.01, 0.93, 0.85, 0.77, 0.69];

const ornekler = [
  ['Histogram + normal eğri', (o) => G.histogram(normal(250, 50, 10), { ...o, xEtiket: 'Sınav puanı' })],
  ['Histogram (çarpık, Sturges)', (o) => G.histogram(ustel(120, 0.2), { ...o, kutuSayisi: 'sturges', xEtiket: 'Bekleme süresi (gün)' })],
  ['Normal Q-Q (GA bandı)', (o) => G.qqGrafigi(normal(60, 100, 15), { ...o, gaBandi: true })],
  ['Kutu grafiği (aykırı/uç)', (o) => G.kutuGrafigi(gruplar3, ['Kontrol', 'Deney A', 'Deney B'], { ...o, ortalamaGoster: true, yEtiket: 'Kaygı puanı', nGoster: true })],
  ['Kutu + ham noktalar', (o) => G.kutuGrafigi(gruplar3, ['Kontrol', 'Deney A', 'Deney B'], { ...o, noktalar: true, yEtiket: 'Kaygı puanı' })],
  ['Ortalama ± %95 GA (çubuk)', (o) => G.ortalamaHataGrafigi(gruplar3, ['Kontrol', 'Deney A', 'Deney B'], { ...o, yEtiket: 'Ortalama puan' })],
  ['Ortalama ± SS (nokta)', (o) => G.ortalamaHataGrafigi(gruplar3, ['Kontrol', 'Deney A', 'Deney B'], { ...o, tip: 'nokta', hata: 'ss' })],
  [
    'Profil grafiği',
    (o) =>
      G.profilGrafigi(
        [
          { ad: 'Deney', noktalar: [{ x: 'Ön test', ortalama: 42, alt: 39, ust: 45 }, { x: 'Son test', ortalama: 55, alt: 52, ust: 58 }, { x: 'İzleme', ortalama: 53, alt: 49.5, ust: 56.5 }] },
          { ad: 'Kontrol', noktalar: [{ x: 'Ön test', ortalama: 41, alt: 38, ust: 44 }, { x: 'Son test', ortalama: 44, alt: 41, ust: 47 }, { x: 'İzleme', ortalama: 43, alt: 40, ust: 46 }] },
        ],
        { ...o, hataNotu: 'Hata çubukları %95 güven aralığını gösterir.', yEtiket: 'Ortalama puan' },
      ),
  ],
  ['Saçılım + OLS + %95 GA', (o) => G.sacilimGrafigi(xs, ys, { ...o, xEtiket: 'Öz yeterlik', yEtiket: 'Akademik başarı' })],
  ['Saçılım (gruplu, grup doğruları)', (o) => G.sacilimGrafigi(xs, ys, { ...o, gruplar: grupEt, regresyonCizgisi: 'grup', gaBandi: false, xEtiket: 'Öz yeterlik', yEtiket: 'Akademik başarı' })],
  [
    'Kaplan–Meier + risk tablosu',
    (o) =>
      G.kaplanMeierGrafigi(
        [
          { ad: 'Standart tedavi', adimlar: kmA.adimlar, sansurZamanlari: kmA.sansur },
          { ad: 'Yeni tedavi', adimlar: kmB.adimlar, sansurZamanlari: kmB.sansur },
        ],
        {
          ...o,
          yukseklik: 480,
          gaGoster: true,
          notMetni: 'Log-rank p = 0,012',
          xEtiket: 'Süre (ay)',
          riskTablosu: { zamanlar: riskZ, sayilar: { 'Standart tedavi': riskSayilari(zA, riskZ), 'Yeni tedavi': riskSayilari(zB, riskZ) } },
        },
      ),
  ],
  [
    'ROC eğrileri',
    (o) =>
      G.rocGrafigi(
        [
          { ad: 'Model 1', ...rocA, aucGa: [rocA.auc - 0.06, Math.min(1, rocA.auc + 0.06)] },
          { ad: 'Model 2', ...rocB },
        ],
        { ...o, optimum: { fpr: 0.22, tpr: 0.74, etiket: 'Kesim = 12,5\nDuyarlılık 0,74; özgüllük 0,78' } },
      ),
  ],
  ['Bland–Altman', (o) => { const a = normal(50, 100, 15); const d = a.map(() => normal(1, 1.2, 3)[0]); return G.blandAltmanGrafigi(a, d, { ...o, gaCizgileri: true }); }],
  ['Çapraz tablo (kümelenmiş, %)', (o) => G.caprazCubukGrafigi([[30, 12, 8], [15, 25, 20], [5, 13, 32]], ['Düşük', 'Orta', 'Yüksek'], ['Lisans', 'Yüksek lisans', 'Doktora'], { ...o, gostergeBasligi: 'Kaygı düzeyi' })],
  ['Çapraz tablo (%100 yığılmış)', (o) => G.caprazCubukGrafigi([[30, 12, 8], [15, 25, 20], [5, 13, 32], [4, 6, 9], [2, 3, 11]], ['Hiç', 'Az', 'Orta', 'Çok', 'Pek çok'], ['Lisans', 'Yüksek lisans', 'Doktora'], { ...o, tip: 'yigilmis' })],
  ['Frekans (dikey, uzun etiket)', (o) => G.frekansGrafigi(uzunAdlar, [12, 25, 18, 60, 35], o)],
  ['Frekans (yatay, n ve %)', (o) => G.frekansGrafigi(uzunAdlar, [12, 25, 18, 60, 35], { ...o, yatay: true, degerEtiketi: 'ikisi', sirala: 'azalan' })],
  ['Pasta', (o) => G.pastaGrafigi(['Lisans', 'Yüksek lisans', 'Doktora', 'Doçent', 'Profesör', 'Diğer'], [120, 64, 38, 12, 9, 3], o)],
  ['Korelasyon ısı haritası', (o) => G.korelasyonIsiHaritasi(r5, ['Kaygı', 'Depresyon', 'Öz yeterlik', 'Yaşam doyumu', 'Stres'], { ...o, p: p5 })],
  ['Korelasyon (alt üçgen)', (o) => G.korelasyonIsiHaritasi(r5, ['Kaygı', 'Depresyon', 'Öz yeterlik', 'Yaşam doyumu', 'Stres'], { ...o, p: p5, ucgen: 'alt' })],
  [
    'Orman grafiği',
    (o) =>
      G.ormanGrafigi(
        [
          { ad: 'Yaş (her 10 yıl)', tahmin: 1.32, alt: 1.08, ust: 1.61, p: 0.006 },
          { ad: 'Cinsiyet (kadın)', tahmin: 0.84, alt: 0.6, ust: 1.18, p: 0.31 },
          { ad: 'Sigara', baslik: true },
          { ad: '  Eski içici', tahmin: 1.45, alt: 0.92, ust: 2.29, p: 0.11 },
          { ad: '  Halen içiyor', tahmin: 2.71, alt: 1.66, ust: 4.42, p: 0.0001 },
          { ad: 'Diyabet', tahmin: 1.9, alt: 1.21, ust: 2.98, p: 0.005 },
          { ad: 'Genel', tahmin: 1.52, alt: 1.22, ust: 1.89, ozet: true },
        ],
        { ...o, solBaslik: 'Değişken', xEtiket: 'Odds oranı (log ölçek)', yonEtiketleri: ['Daha düşük risk', 'Daha yüksek risk'] },
      ),
  ],
  ['Yamaç + paralel analiz', (o) => G.yamacGrafigi(ozdeger, { ...o, paralelAnaliz: paralel })],
  ['Nokta (arı sürüsü) ± SS', (o) => G.noktaGrafigi([normal(12, 20, 4), normal(15, 25, 5), normal(9, 18, 3)], ['Grup A', 'Grup B', 'Grup C'], { ...o, hata: 'ss', yEtiket: 'Süre (sn)' })],
  // Uç durumlar
  ['Uç: 10 grup kutu, uzun adlar', (o) => G.kutuGrafigi(Array.from({ length: 10 }, (_, i) => normal(20, i * 2, 3)), Array.from({ length: 10 }, (_, i) => `Çok uzun Türkçe grup adı ${i + 1}`), o)],
  ['Uç: tek gözlem histogram', (o) => G.histogram([5], o)],
  ['Uç: büyük değerler (1e6)', (o) => G.sacilimGrafigi(normal(40, 2e6, 4e5), normal(40, 5e6, 1e6), { ...o, xEtiket: 'Gelir (TL)', yEtiket: 'Harcama (TL)' })],
  ['Uç: 10 seri çapraz tablo', (o) => G.caprazCubukGrafigi(Array.from({ length: 10 }, (_, i) => [i + 3, 12 - i, 5 + (i % 3)]), Array.from({ length: 10 }, (_, i) => `Kategori ${i + 1}`), ['A', 'B', 'C'], { ...o, tip: 'yigilmis' })],
  ['Uç: negatif değerler', (o) => G.ortalamaHataGrafigi([normal(20, -5, 3), normal(20, 2, 3), normal(20, -1, 2)], ['Önce', 'Sonra', 'İzleme'], o)],
  ['Uç: uzun gösterge adları (çapraz)', (o) => G.caprazCubukGrafigi([[30, 12], [15, 25], [5, 13]], ['Öğretmenlik uygulaması sırasında yüksek kaygı yaşayanlar', 'Orta düzeyde kaygı bildiren öğrenciler', 'Düşük kaygı'], ['Devlet üniversitesi', 'Vakıf üniversitesi'], { ...o, baslik: 'Üniversite türüne göre kaygı düzeyi dağılımı: Türkiye örneklemi (2024–2025 eğitim-öğretim yılı)' })],
  ['Uç: uzun etiketli pasta', (o) => G.pastaGrafigi(uzunAdlar.map((a) => a + ' (öğretmen adayları)'), [3, 5, 40, 30, 22], o)],
  ['Uç: 10 değişkenli korelasyon, uzun adlar', (o) => { const k = 10; const r = Array.from({ length: k }, (_, i) => Array.from({ length: k }, (__, j) => (i === j ? 1 : Math.cos(i * 1.7 + j * 0.9) * 0.8))); return G.korelasyonIsiHaritasi(r, Array.from({ length: k }, (_, i) => `Ölçek alt boyutu ${i + 1}`), { ...o, p: r.map((s) => s.map((v) => (Math.abs(v) > 0.5 ? 0.001 : 0.2))) }); }],
  ['Uç: n = 2 kutu ve nokta', (o) => G.noktaGrafigi([[3, 9], [4, 4.5], [7]], ['A', 'B', 'C'], { ...o, hata: 'ss' })],
  ['Uç: KM tek eğri, %, risk tablosu düzensiz zamanlar', (o) => G.kaplanMeierGrafigi([{ ad: 'Tüm hastalar (Çukurova Üniversitesi kohortu)', adimlar: kmA.adimlar, sansurZamanlari: kmA.sansur }], { ...o, yuzde: true, riskTablosu: { zamanlar: [0, 7, 15, 36], sayilar: [riskSayilari(zA, [0, 7, 15, 36])] } })],
  ['Uç: veri yok', (o) => G.histogram([], { ...o, baslik: 'Boş veri' })],
];

const kart = (baslik, svg) => `<figure><figcaption>${baslik}</figcaption><div class="g">${svg}</div></figure>`;
let govde = '';
for (const [ad, f] of ornekler) {
  let yayin;
  let renkli;
  try {
    // Her iki tema aynı örnek veriyi görsün diye üreteç her çağrıdan önce aynı tohuma döner
    const tohum = Math.floor(rng() * 1e9);
    rng = tohumluRastgele(tohum);
    yayin = f({ tema: 'yayin' });
    rng = tohumluRastgele(tohum);
    renkli = f({ tema: 'renkli' });
  } catch (h) {
    yayin = renkli = `<pre style="color:#b00">${String(h.stack || h).replace(/</g, '&lt;')}</pre>`;
  }
  govde += `<section><h2>${ad}</h2><div class="cift">${kart('yayın', yayin)}${kart('renkli', renkli)}</div></section>\n`;
}

const html = `<!doctype html><html lang="tr"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Grafik önizleme</title>
<style>
body{margin:0;padding:16px;background:#f2f2f0;color:#111;font-family:system-ui,'Segoe UI',sans-serif}
h1{font-size:20px;margin:0 0 12px}h2{font-size:15px;margin:18px 0 8px}
.cift{display:flex;flex-wrap:wrap;gap:12px}figure{margin:0;background:#fff;border:1px solid #ddd;padding:6px}
figcaption{font-size:11px;color:#666;margin-bottom:4px}.g svg{display:block;max-width:100%;height:auto}
</style></head><body><h1>İstatistik Asistanı — grafik önizlemesi</h1>${govde}</body></html>`;

const hedef = path.join(KOK, 'cikti', 'grafik-onizleme.html');
fs.mkdirSync(path.dirname(hedef), { recursive: true });
fs.writeFileSync(hedef, html);
console.log(`Yazıldı: ${hedef}`);
