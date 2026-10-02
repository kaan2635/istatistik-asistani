// Rapor öğelerini belge sırasına göre numaralandırır (Tablo 1, 2…; Şekil 1, 2…) ve dışa aktarım biçimlerini üretir.

import { kacir } from './bicim.js';
import { tabloHtml, tabloMetin } from './tablo.js';

/** Bloklara tablo/şekil numarası ekler (kopya döner) */
export function numaralandir(ogeler) {
  let tablo = 0;
  let sekil = 0;
  return ogeler.map((o) => ({
    ...o,
    bloklar: (o.bloklar || []).map((b) => {
      if (b.tur === 'tablo') return { ...b, numara: ++tablo };
      if (b.tur === 'grafik') return { ...b, numara: ++sekil };
      return b;
    }),
  }));
}

function kararHtml(b) {
  const liste = (adimlar) => `<ol>${adimlar.map((a) => `<li class="${a.durum || ''}"><span class="adim-baslik">${kacir(a.baslik)}:</span> ${kacir(a.metin)}</li>`).join('')}</ol>`;
  let ic = b.adimlar ? liste(b.adimlar) : '';
  for (const alt of b.alt || []) ic += `<p style="margin:.5rem 0 0"><strong>${kacir(alt.baslik)}</strong></p>${liste(alt.adimlar)}`;
  return `<details class="karar" open><summary>${kacir(b.baslik || 'Neden bu test?')}</summary>${ic}</details>`;
}

/** Bağımsız (tek dosya) HTML raporu */
export function htmlRapor({ baslik, bilgi = [], ogeler, css }) {
  const parcalar = ogeler.map((o) => {
    const bloklar = o.bloklar
      .map((b) => {
        switch (b.tur) {
          case 'karar':
            return kararHtml(b);
          case 'tablo':
            return `<div class="tablo-kap">${tabloHtml(b.tablo, b.numara)}</div>`;
          case 'grafik':
            return `<figure class="grafik-kap"><p class="tablo-baslik"><span class="no">Şekil ${b.numara}</span>. <span class="ad">${kacir(b.baslik)}</span></p><div class="grafik">${b.svg}</div></figure>`;
          case 'satir':
            return `<p class="raporlama-satiri">${kacir(b.metin)}</p>`;
          case 'not':
            return `<p class="soluk kucuk">${kacir(b.metin)}</p>`;
          case 'baslik':
            return `<p class="alt-baslik">${kacir(b.metin)}</p>`;
          case 'metin':
            return `<p>${kacir(b.metin)}</p>`;
          default:
            return '';
        }
      })
      .join('');
    return `<section class="kart rapor-ogesi"><h3>${kacir(o.baslik)}</h3>${o.hata ? `<p class="hata-kutusu">${kacir(o.hata)}</p>` : ''}${bloklar}</section>`;
  });
  return `<!doctype html><html lang="tr"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>${kacir(baslik)}</title><style>${css}</style></head><body><main><h1>${kacir(baslik)}</h1>${bilgi.map((b) => `<p class="soluk kucuk">${kacir(b)}</p>`).join('')}${parcalar.join('')}</main></body></html>`;
}

/** Tüm tabloların düz metin hâli (panoya) */
export function metinRapor(ogeler) {
  const satirlar = [];
  for (const o of ogeler) {
    satirlar.push(o.baslik, '');
    for (const b of o.bloklar) {
      if (b.tur === 'tablo') satirlar.push(tabloMetin(b.tablo, b.numara), '');
      else if (b.tur === 'satir') satirlar.push(b.metin, '');
    }
  }
  return satirlar.join('\n');
}
