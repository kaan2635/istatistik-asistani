// Diyalog pencereleri ve bildirimler.

import { h, simge } from './dom.js';

/**
 * Modal pencere açar.
 * @param {{baslik:string, govde:Node|Node[], dugmeler?:{metin:string, ana?:boolean, tehlike?:boolean, tikla?:Function}[], genis?:boolean}} p
 * @returns {{kapat:Function, el:HTMLDialogElement}}
 */
export function pencereAc({ baslik, govde, dugmeler = [{ metin: 'Kapat' }], genis = false, kapaninca }) {
  const dlg = h('dialog', { class: 'pencere', style: genis ? { maxWidth: 'min(1100px, calc(100vw - 2rem))' } : null });
  const kapat = () => {
    dlg.close();
  };
  dlg.addEventListener('close', () => {
    dlg.remove();
    if (kapaninca) kapaninca();
  });
  const ust = h('div', { class: 'pencere-ust' }, h('h2', {}, baslik), h('span', { class: 'bosluk' }), h('button', { class: 'dugme hayalet simge-dugme', 'aria-label': 'Kapat', onclick: kapat }, simge('kapat')));
  const alt = h(
    'div',
    { class: 'pencere-alt' },
    dugmeler.map((d) =>
      h(
        'button',
        {
          class: `dugme${d.ana ? ' ana' : ''}${d.tehlike ? ' tehlike' : ''}`,
          onclick: async () => {
            if (d.tikla) {
              const sonuc = await d.tikla();
              if (sonuc === false) return; // pencere açık kalsın (ör. doğrulama hatası)
            }
            kapat();
          },
        },
        d.metin,
      ),
    ),
  );
  dlg.append(ust, h('div', { class: 'pencere-govde' }, govde), alt);
  document.body.appendChild(dlg);
  dlg.showModal();
  return { kapat, el: dlg };
}

/** Onay penceresi; Promise<boolean> */
export function onayla(baslik, metin, { evet = 'Evet', hayir = 'Vazgeç', tehlike = false } = {}) {
  return new Promise((coz) => {
    let sonuc = false;
    pencereAc({
      baslik,
      govde: h('p', {}, metin),
      dugmeler: [
        { metin: hayir },
        {
          metin: evet,
          ana: !tehlike,
          tehlike,
          tikla: () => {
            sonuc = true;
          },
        },
      ],
      kapaninca: () => coz(sonuc),
    });
  });
}

let bildirimKabi = null;

/** Kısa süreli bildirim */
export function bildirim(metin, { tur = 'bilgi', sure = 4000, eylem } = {}) {
  if (!bildirimKabi) {
    bildirimKabi = h('div', { id: 'bildirimler', role: 'status', 'aria-live': 'polite' });
    document.body.appendChild(bildirimKabi);
  }
  const el = h(
    'div',
    { class: `bildirim ${tur === 'hata' ? 'hata' : ''}` },
    simge(tur === 'hata' ? 'uyari' : tur === 'tamam' ? 'tamam' : 'bilgi'),
    h('span', {}, metin),
    eylem ? h('button', { class: 'dugme kucuk', onclick: () => { eylem.tikla(); el.remove(); } }, eylem.metin) : null,
  );
  bildirimKabi.appendChild(el);
  setTimeout(() => el.remove(), sure);
}
