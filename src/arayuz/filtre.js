// Vaka seçimi (filtre): yalnızca koşulu sağlayan satırlar analize girer. SPSS'teki "Select Cases" karşılığı.

import { durum, filtreAyarla } from './durum.js';
import { h } from './dom.js';
import { pencereAc } from './pencere.js';
import { degerEksikMi, duzeyleriBul } from '../veri/tanima.js';

export const ISLEMLER = {
  esit: '=',
  farkli: '≠',
  kucuk: '<',
  kucukEsit: '≤',
  buyuk: '>',
  buyukEsit: '≥',
  dolu: 'eksik değil',
};

function kosulSaglar(d, v, k) {
  if (k.islem === 'dolu') return !degerEksikMi(d, v);
  if (degerEksikMi(d, v)) return false;
  const hedef = typeof v === 'number' ? Number(k.deger) : String(k.deger);
  switch (k.islem) {
    case 'esit':
      return String(v) === String(k.deger);
    case 'farkli':
      return String(v) !== String(k.deger);
    case 'kucuk':
      return v < hedef;
    case 'kucukEsit':
      return v <= hedef;
    case 'buyuk':
      return v > hedef;
    case 'buyukEsit':
      return v >= hedef;
    default:
      return true;
  }
}

/** Filtre tanımından satır maskesi (1 = analize dahil) */
export function filtreMaskesi(filtre, veriSeti = durum.veriSeti) {
  if (!filtre || !filtre.kosullar?.length) return null;
  const n = veriSeti.satirSayisi;
  const maske = new Uint8Array(n);
  const ds = filtre.kosullar.map((k) => veriSeti.degiskenler.find((d) => d.ad === k.degisken));
  for (let i = 0; i < n; i++) {
    const sonuclar = filtre.kosullar.map((k, j) => (ds[j] ? kosulSaglar(ds[j], ds[j].degerler[i], k) : true));
    maske[i] = (filtre.baglac === 'veya' ? sonuclar.some(Boolean) : sonuclar.every(Boolean)) ? 1 : 0;
  }
  return maske;
}

export function filtreMetni(filtre, veriSeti = durum.veriSeti) {
  if (!filtre?.kosullar?.length) return '';
  return filtre.kosullar
    .map((k) => {
      const d = veriSeti.degiskenler.find((x) => x.ad === k.degisken);
      const etiket = d?.degerEtiketleri?.[String(k.deger)] ?? k.deger;
      return k.islem === 'dolu' ? `${k.degisken} eksik değil` : `${k.degisken} ${ISLEMLER[k.islem]} ${etiket}`;
    })
    .join(filtre.baglac === 'veya' ? ' veya ' : ' ve ');
}

/** Filtre düzenleme penceresi */
export function filtrePenceresi() {
  const vs = durum.veriSeti;
  const kosullar = (durum.filtre?.kosullar || []).map((k) => ({ ...k }));
  if (!kosullar.length) kosullar.push({ degisken: vs.degiskenler[0].ad, islem: 'esit', deger: '' });
  let baglac = durum.filtre?.baglac || 've';
  const liste = h('div', { class: 'yigin' });
  const sayac = h('p', { class: 'soluk kucuk' });

  const guncelSayi = () => {
    const m = filtreMaskesi({ kosullar, baglac }, vs);
    const n = m ? m.reduce((a, v) => a + v, 0) : vs.satirSayisi;
    sayac.textContent = `Koşulu sağlayan vaka: ${n} / ${vs.satirSayisi}`;
  };

  const ciz = () => {
    liste.replaceChildren();
    kosullar.forEach((k, idx) => {
      const d = vs.degiskenler.find((x) => x.ad === k.degisken);
      const kategorik = ['kategorik', 'sirali'].includes(durum.olcumler[k.degisken]);
      const degerGirdisi = kategorik
        ? h(
            'select',
            { onchange: (e) => { k.deger = e.target.value; guncelSayi(); } },
            h('option', { value: '' }, 'Seçin…'),
            duzeyleriBul(d).map((dz) => h('option', { value: String(dz.deger), selected: String(dz.deger) === String(k.deger) }, `${dz.etiket} (n = ${dz.n})`)),
          )
        : h('input', { type: 'text', value: k.deger, placeholder: 'değer', oninput: (e) => { k.deger = e.target.value.replace(',', '.'); guncelSayi(); } });
      liste.appendChild(
        h(
          'div',
          { class: 'satir' },
          h(
            'select',
            { onchange: (e) => { k.degisken = e.target.value; k.deger = ''; ciz(); guncelSayi(); } },
            vs.degiskenler.map((x) => h('option', { value: x.ad, selected: x.ad === k.degisken }, x.ad)),
          ),
          h(
            'select',
            { onchange: (e) => { k.islem = e.target.value; ciz(); guncelSayi(); } },
            Object.entries(ISLEMLER)
              .filter(([anahtar]) => !kategorik || ['esit', 'farkli', 'dolu'].includes(anahtar))
              .map(([anahtar, ad]) => h('option', { value: anahtar, selected: anahtar === k.islem }, ad)),
          ),
          k.islem !== 'dolu' ? degerGirdisi : null,
          h('button', { class: 'dugme hayalet kucuk', onclick: () => { kosullar.splice(idx, 1); ciz(); guncelSayi(); } }, 'Kaldır'),
        ),
      );
    });
  };
  ciz();
  guncelSayi();

  pencereAc({
    baslik: 'Vaka seçimi (filtre)',
    govde: [
      h('p', { class: 'soluk' }, 'Yalnızca koşulları sağlayan satırlar analizlere girer (ör. yalnızca kadın hastalar). Veriniz değişmez; filtreyi istediğiniz zaman kaldırabilirsiniz.'),
      liste,
      h(
        'div',
        { class: 'satir', style: 'margin-top:.75rem' },
        h('button', { class: 'dugme kucuk', onclick: () => { kosullar.push({ degisken: vs.degiskenler[0].ad, islem: 'esit', deger: '' }); ciz(); } }, '+ Koşul ekle'),
        h('span', { class: 'soluk kucuk' }, 'Koşullar arasında:'),
        h(
          'select',
          { onchange: (e) => { baglac = e.target.value; guncelSayi(); } },
          h('option', { value: 've', selected: baglac === 've' }, 'hepsi (ve)'),
          h('option', { value: 'veya', selected: baglac === 'veya' }, 'herhangi biri (veya)'),
        ),
      ),
      sayac,
    ],
    dugmeler: [
      { metin: 'Filtreyi kaldır', tikla: () => filtreAyarla(null, null) },
      {
        metin: 'Uygula',
        ana: true,
        tikla: () => {
          const gecerli = kosullar.filter((k) => k.islem === 'dolu' || String(k.deger).trim() !== '');
          const filtre = gecerli.length ? { kosullar: gecerli, baglac } : null;
          filtreAyarla(filtre, filtreMaskesi(filtre, vs));
        },
      },
    ],
  });
}
