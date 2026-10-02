// 4. Rapor sekmesi: analiz sonuçları, "Neden bu test?" açıklamaları, tablo kopyalama ve dışa aktarım.

import { baglam, durum, istekSil, istekTasi, raporuTemizle, sekmeyeGit } from '../durum.js';
import { dosyaIndir, h, panoyaKopyala, simge, svgDugum } from '../dom.js';
import { bildirim, onayla, pencereAc } from '../pencere.js';
import { analizCalistir } from '../../karar/index.js';
import { YONTEMLER } from '../../karar/yontemler.js';
import { tabloHtml, tabloMetin, tabloWordHtml } from '../../rapor/tablo.js';
import { htmlRapor, metinRapor, numaralandir } from '../../rapor/belge.js';
import { docxOlustur } from '../../rapor/docx.js';
import { pngIndir, svgBoyutlari, svgIndir, svgdenPngBlob } from '../../grafik/disaAktar.js';
import { filtreMetni } from '../filtre.js';

function sonucAl(istek) {
  if (!durum.sonuclar.has(istek.id)) {
    durum.sonuclar.set(istek.id, analizCalistir(istek, durum.veriSeti, baglam()));
  }
  return durum.sonuclar.get(istek.id);
}

function kararBlogu(b) {
  const liste = (adimlar) =>
    h(
      'ol',
      {},
      adimlar.map((a) => h('li', { class: a.durum || '' }, h('span', { class: 'adim-baslik' }, `${a.baslik}: `), a.metin)),
    );
  return h(
    'details',
    { class: 'karar', open: durum.ayarlar.kararlarAcik !== false && !b.alt },
    h('summary', {}, b.baslik || 'Neden bu test?'),
    b.adimlar ? liste(b.adimlar) : null,
    (b.alt || []).map((alt) => h('div', {}, h('p', { style: 'margin:.5rem 0 0' }, h('strong', {}, alt.baslik)), liste(alt.adimlar))),
  );
}

function tabloBlogu(b) {
  const kap = h('div', { class: 'tablo-kap', html: tabloHtml(b.tablo, b.numara) });
  const eylem = h(
    'div',
    { class: 'satir yazdirma-gizle', style: 'margin-top:.35rem' },
    h(
      'button',
      {
        class: 'dugme kucuk',
        onclick: async () => {
          const ok = await panoyaKopyala(tabloWordHtml(b.tablo, b.numara), tabloMetin(b.tablo, b.numara));
          bildirim(ok ? 'Tablo kopyalandı. Word\'e yapıştırabilirsiniz.' : 'Kopyalanamadı; tabloyu seçip Ctrl+C ile kopyalayın.', { tur: ok ? 'tamam' : 'hata' });
        },
      },
      simge('kopyala', 14),
      'Word için kopyala',
    ),
    h(
      'button',
      {
        class: 'dugme kucuk hayalet',
        onclick: async () => {
          const ok = await panoyaKopyala(`<pre>${tabloMetin(b.tablo, b.numara)}</pre>`, tabloMetin(b.tablo, b.numara));
          bildirim(ok ? 'Tablo Excel için kopyalandı.' : 'Kopyalanamadı.', { tur: ok ? 'tamam' : 'hata' });
        },
      },
      'Excel için kopyala',
    ),
  );
  kap.appendChild(eylem);
  return kap;
}

function grafikBlogu(b) {
  const ad = (b.dosyaAdi || `sekil-${b.numara}`).replace(/[^\p{L}\p{N}_-]+/gu, '-');
  return h(
    'figure',
    { class: 'grafik-kap', style: 'margin-left:0;margin-right:0' },
    h('p', { class: 'tablo-baslik' }, h('span', { class: 'no' }, `Şekil ${b.numara}`), '. ', h('span', { class: 'ad' }, b.baslik)),
    h('div', { class: 'grafik' }, svgDugum(b.svg)),
    h(
      'div',
      { class: 'satir yazdirma-gizle', style: 'margin-top:.35rem' },
      h('button', { class: 'dugme kucuk', onclick: () => pngIndir(b.svg, `${ad}.png`, { olcek: 3 }) }, simge('indir', 14), 'PNG (yüksek çözünürlük)'),
      h('button', { class: 'dugme kucuk hayalet', onclick: () => svgIndir(b.svg, `${ad}.svg`) }, 'SVG'),
    ),
  );
}

function blokCiz(b) {
  switch (b.tur) {
    case 'karar':
      return kararBlogu(b);
    case 'tablo':
      return tabloBlogu(b);
    case 'grafik':
      return grafikBlogu(b);
    case 'satir':
      return h(
        'div',
        { class: 'raporlama-satiri' },
        h('span', {}, b.metin),
        h('button', { class: 'dugme kucuk hayalet yazdirma-gizle', title: 'Kopyala', onclick: async () => { await panoyaKopyala(b.metin, b.metin); bildirim('Kopyalandı.', { tur: 'tamam', sure: 1500 }); } }, simge('kopyala', 14)),
      );
    case 'not':
      return h('div', { class: `bilgi-kutusu ${b.seviye === 'uyari' ? 'uyari' : ''}`, style: 'margin:.5rem 0' }, simge(b.seviye === 'uyari' ? 'uyari' : 'bilgi', 16), h('div', { class: 'kucuk' }, b.metin));
    case 'baslik':
      return h('p', { class: 'alt-baslik' }, b.metin);
    case 'metin':
      return h('p', {}, b.metin);
    default:
      return null;
  }
}

function ogeKarti(istek, oge, sira, toplam) {
  return h(
    'section',
    { class: `kart rapor-ogesi${durum.sonEklenen === istek.id ? ' yeni' : ''}`, id: `oge-${istek.id}` },
    h(
      'div',
      { class: 'kart-baslik' },
      h('h3', {}, oge.baslik),
      h(
        'div',
        { class: 'oge-eylemler' },
        h('button', { class: 'dugme hayalet simge-dugme', title: 'Yukarı taşı', disabled: sira === 0, onclick: () => istekTasi(istek.id, -1) }, simge('yukari', 16)),
        h('button', { class: 'dugme hayalet simge-dugme', title: 'Aşağı taşı', disabled: sira === toplam - 1, onclick: () => istekTasi(istek.id, 1) }, simge('asagi', 16)),
        h('button', { class: 'dugme hayalet simge-dugme tehlike', title: 'Kaldır', onclick: () => istekSil(istek.id) }, simge('sil', 16)),
      ),
    ),
    oge.hata ? h('div', { class: 'bilgi-kutusu hata' }, simge('uyari'), h('div', {}, oge.hata)) : null,
    oge.bloklar.map(blokCiz),
  );
}

function raporBilgisi() {
  const vs = durum.veriSeti;
  const bilgi = [];
  if (vs) bilgi.push(`Veri: ${vs.ad || ''} (${vs.satirSayisi} vaka)`);
  const f = filtreMetni(durum.filtre);
  if (f) bilgi.push(`Filtre: ${f}`);
  bilgi.push(`Anlamlılık düzeyi α = ${String(durum.ayarlar.alfa).replace('.', durum.ayarlar.ondalikAyrac)}; tarih: ${new Date().toLocaleDateString('tr-TR')}`);
  return bilgi;
}

function tumOgeler() {
  return numaralandir(durum.istekler.map((i) => sonucAl(i)));
}

async function wordAktar() {
  bildirim('Word belgesi hazırlanıyor…', { sure: 2000 });
  try {
    const veri = await docxOlustur({
      baslik: 'İstatistiksel Analiz Raporu',
      bilgi: raporBilgisi(),
      ogeler: tumOgeler(),
      kararlariEkle: true,
      pngDonustur: async (svg) => {
        const blob = await svgdenPngBlob(svg, 3);
        const { genislik, yukseklik } = svgBoyutlari(svg);
        return { veri: new Uint8Array(await blob.arrayBuffer()), genislik, yukseklik };
      },
    });
    dosyaIndir(new Blob([veri], { type: 'application/vnd.openxmlformats-officedocument.wordprocessingml.document' }), 'istatistik-raporu.docx');
  } catch (e) {
    console.error(e);
    bildirim(`Word belgesi oluşturulamadı: ${e.message}`, { tur: 'hata' });
  }
}

function htmlAktar() {
  const css = [...document.querySelectorAll('style')].map((s) => s.textContent).join('\n');
  dosyaIndir(htmlRapor({ baslik: 'İstatistiksel Analiz Raporu', bilgi: raporBilgisi(), ogeler: tumOgeler(), css }), 'istatistik-raporu.html', 'text/html');
}

function yontemOzeti() {
  const kullanim = new Map();
  durum.istekler.forEach((i) => {
    const o = sonucAl(i);
    for (const y of o.yontemler || []) {
      if (!kullanim.has(y)) kullanim.set(y, []);
      kullanim.get(y).push(o.baslik);
    }
  });
  const satirlar = [...kullanim.entries()].map(([id, yerler]) => {
    const y = YONTEMLER[id] || { ad: id, amac: '', kaynak: '' };
    return h('tr', {}, h('td', {}, h('strong', {}, y.ad)), h('td', {}, y.amac), h('td', { class: 'kucuk' }, [...new Set(yerler)].join('; ')), h('td', { class: 'kucuk soluk' }, y.kaynak));
  });
  pencereAc({
    baslik: 'Kullanılan istatistiksel yöntemler',
    genis: true,
    govde: [
      h('p', { class: 'soluk' }, 'Bu rapordaki analizlerde kullanılan yöntemlerin listesi. Tezinizin ya da makalenizin "İstatistiksel analiz" bölümünü yazarken kontrol listesi olarak kullanabilirsiniz.'),
      satirlar.length
        ? h('div', { class: 'yardim' }, h('table', {}, h('thead', {}, h('tr', {}, h('th', {}, 'Yöntem'), h('th', {}, 'Amaç'), h('th', {}, 'Kullanıldığı analizler'), h('th', {}, 'Kaynak'))), h('tbody', {}, satirlar)))
        : h('p', {}, 'Henüz analiz yok.'),
      h('p', { class: 'kucuk soluk', style: 'margin-top:.75rem' }, `Ayarlar: anlamlılık düzeyi α = ${durum.ayarlar.alfa}; normallik değerlendirmesi: ${{ otomatik: 'otomatik (örneklem büyüklüğüne göre)', shapiro: 'Shapiro-Wilk', lilliefors: 'Kolmogorov-Smirnov (Lilliefors)', spss: 'n < 50 Shapiro-Wilk, n ≥ 50 Kolmogorov-Smirnov', 'carpiklik-basiklik': 'çarpıklık ve basıklık' }[durum.ayarlar.normallikYontemi]}.`),
    ],
  });
}

export function raporSekmesi(kap) {
  if (!durum.istekler.length) {
    kap.append(
      h(
        'section',
        { class: 'kart', style: 'text-align:center;padding:3rem 1rem' },
        h('div', { style: 'color:var(--ana)' }, simge('rapor', 40)),
        h('h2', {}, 'Rapor henüz boş'),
        h('p', { class: 'soluk' }, durum.veriSeti ? 'Analiz sekmesinden bir analiz seçin ya da otomatik analizi çalıştırın.' : 'Önce verinizi yükleyin.'),
        h('button', { class: 'dugme ana', onclick: () => sekmeyeGit(durum.veriSeti ? 'analiz' : 'veri') }, durum.veriSeti ? 'Analize git' : 'Veri yükle'),
      ),
    );
    return;
  }
  const arac = h(
    'section',
    { class: 'kart rapor-araclari' },
    h(
      'div',
      { class: 'satir' },
      h('h2', { style: 'margin:0' }, 'Rapor'),
      h('span', { class: 'etiket-kutu' }, `${durum.istekler.length} analiz`),
      h('span', { class: 'bosluk' }),
      h('button', { class: 'dugme ana', onclick: wordAktar }, simge('word'), 'Word\'e aktar'),
      h('button', { class: 'dugme', onclick: htmlAktar }, simge('indir'), 'HTML'),
      h('button', { class: 'dugme', onclick: () => window.print() }, simge('yazdir'), 'Yazdır / PDF'),
      h(
        'button',
        {
          class: 'dugme',
          onclick: async () => {
            const ok = await panoyaKopyala(metinRapor(tumOgeler()).replace(/\n/g, '<br>'), metinRapor(tumOgeler()));
            bildirim(ok ? 'Tüm tablolar düz metin olarak kopyalandı.' : 'Kopyalanamadı.', { tur: ok ? 'tamam' : 'hata' });
          },
        },
        simge('kopyala'),
        'Tümünü kopyala',
      ),
      h('button', { class: 'dugme', onclick: yontemOzeti }, simge('bilgi'), 'Kullanılan yöntemler'),
      h('button', { class: 'dugme hayalet tehlike', onclick: async () => { if (await onayla('Rapor temizlensin mi?', 'Tüm analiz sonuçları kaldırılacak.', { evet: 'Temizle', tehlike: true })) raporuTemizle(); } }, simge('sil'), 'Temizle'),
    ),
    filtreMetni(durum.filtre) ? h('div', { class: 'bilgi-kutusu uyari', style: 'margin-top:.75rem' }, simge('filtre'), h('div', { class: 'kucuk' }, `Filtre etkin: ${filtreMetni(durum.filtre)}`)) : null,
  );
  kap.append(arac);
  const liste = h('div', {});
  kap.append(liste);

  // Analizleri sırayla hesapla; her birinden sonra tarayıcıya nefes aldır
  const istekler = durum.istekler.slice();
  const yerTutucular = istekler.map((i) => {
    const el = h('section', { class: 'kart rapor-ogesi', id: `oge-${i.id}` }, h('div', { class: 'satir soluk' }, h('span', { class: 'yukleniyor' }), 'Hesaplanıyor…'));
    liste.appendChild(el);
    return el;
  });
  let tablo = 0;
  let sekil = 0;
  const hedef = durum.sonEklenen;
  durum.sonEklenen = null;
  const isle = (k) => {
    if (k >= istekler.length) return;
    const istek = istekler[k];
    if (!document.body.contains(yerTutucular[k])) return; // sekme değişti
    const ham = sonucAl(istek);
    const oge = { ...ham, bloklar: ham.bloklar.map((b) => (b.tur === 'tablo' ? { ...b, numara: ++tablo } : b.tur === 'grafik' ? { ...b, numara: ++sekil } : b)) };
    const kart = ogeKarti(istek, oge, k, istekler.length);
    if (istek.id === hedef) kart.classList.add('yeni');
    yerTutucular[k].replaceWith(kart);
    if (istek.id === hedef) setTimeout(() => kart.scrollIntoView({ behavior: 'smooth', block: 'start' }), 50);
    setTimeout(() => isle(k + 1), 0);
  };
  setTimeout(() => isle(0), 0);
}
