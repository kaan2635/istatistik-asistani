// Akademik tablo modeli ve çıktıları (ekran HTML'i, Word'e yapıştırılabilir HTML, sekmeyle ayrılmış metin).
//
// Tablo: { baslik, basliklar: Hucre[][], satirlar: {hucreler: Hucre[], tur?: 'grup-basligi'|'ara'}[], dipnotlar: string[], not?: string }
// Hucre: string | { metin, sol?, girinti?, kalin?, ust?, sutun?, satir?, anlamli? }

import { kacir } from './bicim.js';

/** Hücre metni: kaçırılır, satır sonları <br> olur */
function hucreIcerik(metin) {
  return kacir(metin).replace(/\n/g, '<br>');
}

export function hucre(metin, ozellik = {}) {
  return { metin: metin ?? '', ...ozellik };
}

function hucreNesne(h) {
  if (h === null || h === undefined) return { metin: '' };
  if (typeof h === 'string' || typeof h === 'number') return { metin: String(h) };
  return h;
}

/** Ekran için HTML (CSS sınıflarıyla) */
export function tabloHtml(t, numara) {
  const ust = t.basliklar
    .map((satir) => {
      const hucreler = satir
        .map((h0) => {
          const h = hucreNesne(h0);
          const ozn = `${h.sutun > 1 ? ` colspan="${h.sutun}"` : ''}${h.satir > 1 ? ` rowspan="${h.satir}"` : ''}${h.sol ? ' class="sol"' : ''}`;
          return `<th${ozn}>${hucreIcerik(h.metin)}${h.ust ? `<sup>${kacir(h.ust)}</sup>` : ''}</th>`;
        })
        .join('');
      return `<tr>${hucreler}</tr>`;
    })
    .join('');
  const govde = t.satirlar
    .map((s) => {
      const sinif = s.tur ? ` class="${s.tur}"` : '';
      const hucreler = s.hucreler
        .map((h0, j) => {
          const h = hucreNesne(h0);
          const siniflar = [];
          if (h.sol || (j === 0 && h.sol !== false)) siniflar.push('sol');
          if (h.girinti) siniflar.push('girinti');
          if (h.kalin || h.anlamli) siniflar.push('anlamli');
          const ozn = `${h.sutun > 1 ? ` colspan="${h.sutun}"` : ''}${h.satir > 1 ? ` rowspan="${h.satir}"` : ''}${siniflar.length ? ` class="${siniflar.join(' ')}"` : ''}`;
          return `<td${ozn}>${hucreIcerik(h.metin)}${h.ust ? `<sup>${kacir(h.ust)}</sup>` : ''}</td>`;
        })
        .join('');
      return `<tr${sinif}>${hucreler}</tr>`;
    })
    .join('');
  const baslik = `<p class="tablo-baslik"><span class="no">Tablo ${numara ?? ''}</span>${numara ? '. ' : ''}<span class="ad">${kacir(t.baslik)}</span></p>`;
  const notlar = dipnotHtml(t);
  return `${baslik}<table class="akademik"><thead>${ust}</thead><tbody>${govde}</tbody></table>${notlar}`;
}

function dipnotHtml(t) {
  const parcalar = [];
  if (t.not) parcalar.push(`<p><em>Not.</em> ${kacir(t.not)}</p>`);
  for (const d of t.dipnotlar || []) parcalar.push(`<p>${dipnotMetni(d)}</p>`);
  return parcalar.length ? `<div class="dipnot">${parcalar.join('')}</div>` : '';
}

/** "a: Mann-Whitney U testi" → üst simgeli dipnot */
function dipnotMetni(d) {
  const m = /^([a-z*†‡§]{1,2}):\s*(.*)$/.exec(d);
  if (m) return `<sup>${kacir(m[1])}</sup>${kacir(m[2])}`;
  return kacir(d);
}

const KENAR_KALIN = '1.5pt solid #000';
const KENAR_INCE = '0.75pt solid #000';

/**
 * Word'e yapıştırma için satır içi stilli HTML (APA tarzı: yalnızca yatay çizgiler).
 * @param {object} secenek yaziTipi, punto
 */
export function tabloWordHtml(t, numara, { yaziTipi = 'Times New Roman', punto = 11 } = {}) {
  const temel = `font-family:'${yaziTipi}',serif;font-size:${punto}pt;padding:2pt 6pt;vertical-align:top;`;
  const ust = t.basliklar
    .map((satir, i) => {
      const ilk = i === 0;
      const son = i === t.basliklar.length - 1;
      return `<tr>${satir
        .map((h0) => {
          const h = hucreNesne(h0);
          const stil = `${temel}font-weight:bold;text-align:${h.sol ? 'left' : 'center'};${ilk ? `border-top:${KENAR_KALIN};` : ''}${son || h.satir > 1 ? `border-bottom:${KENAR_INCE};` : ''}`;
          return `<th${h.sutun > 1 ? ` colspan="${h.sutun}"` : ''}${h.satir > 1 ? ` rowspan="${h.satir}"` : ''} style="${stil}">${hucreIcerik(h.metin)}${h.ust ? `<sup>${kacir(h.ust)}</sup>` : ''}</th>`;
        })
        .join('')}</tr>`;
    })
    .join('');
  const govde = t.satirlar
    .map((s, i) => {
      const son = i === t.satirlar.length - 1;
      return `<tr>${s.hucreler
        .map((h0, j) => {
          const h = hucreNesne(h0);
          const sol = h.sol || (j === 0 && h.sol !== false) || s.tur === 'grup-basligi';
          const stil = `${temel}text-align:${sol ? 'left' : 'center'};${h.girinti ? 'padding-left:14pt;' : ''}${h.kalin || h.anlamli || s.tur === 'grup-basligi' ? 'font-weight:bold;' : ''}${son ? `border-bottom:${KENAR_KALIN};` : ''}${s.tur === 'ara' ? `border-top:${KENAR_INCE};` : ''}`;
          return `<td${h.sutun > 1 ? ` colspan="${h.sutun}"` : ''}${h.satir > 1 ? ` rowspan="${h.satir}"` : ''} style="${stil}">${hucreIcerik(h.metin)}${h.ust ? `<sup>${kacir(h.ust)}</sup>` : ''}</td>`;
        })
        .join('')}</tr>`;
    })
    .join('');
  const baslikStil = `font-family:'${yaziTipi}',serif;font-size:${punto + 1}pt;margin:0 0 4pt 0;`;
  const baslik = `<p style="${baslikStil}"><b>Tablo ${numara ?? ''}</b>${numara ? '. ' : ''}<i>${kacir(t.baslik)}</i></p>`;
  const notStil = `font-family:'${yaziTipi}',serif;font-size:${punto - 1}pt;margin:2pt 0;`;
  const notlar = [t.not ? `<p style="${notStil}"><i>Not.</i> ${kacir(t.not)}</p>` : '', ...(t.dipnotlar || []).map((d) => `<p style="${notStil}">${dipnotMetni(d)}</p>`)].join('');
  return `${baslik}<table style="border-collapse:collapse;">${ust}${govde}</table>${notlar}`;
}

/** Sekmeyle ayrılmış düz metin (Excel'e yapıştırmak için) */
export function tabloMetin(t, numara) {
  const satirlar = [`Tablo ${numara ?? ''}. ${t.baslik}`];
  const yaz = (hucreler) => {
    const parcalar = [];
    for (const h0 of hucreler) {
      const h = hucreNesne(h0);
      parcalar.push(String(h.metin).replace(/\n/g, ' ') + (h.ust ? h.ust : ''));
      for (let k = 1; k < (h.sutun || 1); k++) parcalar.push('');
    }
    return parcalar.join('\t');
  };
  for (const s of t.basliklar) satirlar.push(yaz(s));
  for (const s of t.satirlar) satirlar.push(yaz(s.hucreler));
  if (t.not) satirlar.push(`Not. ${t.not}`);
  for (const d of t.dipnotlar || []) satirlar.push(d);
  return satirlar.join('\n');
}
