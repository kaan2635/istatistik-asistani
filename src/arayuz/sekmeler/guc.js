// Güç analizi / örneklem büyüklüğü sekmesi. Veri gerektirmez.

import { baglam, istekEkle, sekmeyeGit } from '../durum.js';
import { h, simge } from '../dom.js';
import { bildirim } from '../pencere.js';
import { GUC_TESTLERI, gucAnalizi, gucFonksiyonlari as G } from '../../karar/analizler/guc.js';
import { tabloHtml } from '../../rapor/tablo.js';

const secim = { test: 'iki-bagimsiz-t', hesap: 'n', etki: '0,5', n: '', guc: '0,80', alfa: '0,05', alternatif: 'iki-yonlu', kayip: '' };

function alan(etiket, girdi, aciklama) {
  return h('div', { class: 'alan' }, h('label', {}, etiket), girdi, aciklama ? h('div', { class: 'aciklama' }, aciklama) : null);
}

function metinGirdisi(anahtar, yerTutucu = '') {
  return h('input', { type: 'text', value: secim[anahtar] ?? '', placeholder: yerTutucu, oninput: (e) => { secim[anahtar] = e.target.value; } });
}

function sayi(v) {
  return Number(String(v).trim().replace(',', '.'));
}

/** Etki büyüklüğü yardımcı hesaplayıcıları */
function yardimci(tur, etkiGirdisi) {
  const y = {};
  const g = (a, yt) => h('input', { type: 'text', placeholder: yt, style: 'width:5.5rem', oninput: (e) => { y[a] = e.target.value; } });
  const uygula = (deger) => {
    if (!Number.isFinite(deger)) {
      bildirim('Değerleri kontrol edin.', { tur: 'hata' });
      return;
    }
    secim.etki = String(Math.round(deger * 1000) / 1000).replace('.', ',');
    etkiGirdisi.value = secim.etki;
  };
  if (tur === 'ortalamalar') {
    return h(
      'details',
      { class: 'kucuk' },
      h('summary', {}, 'Ortalama ve standart sapmadan d hesapla'),
      h('div', { class: 'satir', style: 'margin-top:.4rem' }, 'Grup 1:', g('m1', 'ortalama'), g('s1', 'SS'), 'Grup 2:', g('m2', 'ortalama'), g('s2', 'SS'), h('button', { class: 'dugme kucuk', onclick: () => uygula(Math.abs(G.dHesapla(sayi(y.m1), sayi(y.s1), null, sayi(y.m2), sayi(y.s2), null))) }, 'Hesapla')),
      h('div', { class: 'aciklama' }, 'Önceki bir çalışma ya da pilot veriden alınabilir. d = |ort1 − ort2| / birleşik SS.'),
    );
  }
  if (tur === 'eta') {
    return h('details', { class: 'kucuk' }, h('summary', {}, 'η²\'den f hesapla'), h('div', { class: 'satir', style: 'margin-top:.4rem' }, 'η² =', g('eta', '0,06'), h('button', { class: 'dugme kucuk', onclick: () => uygula(G.fdenEta2(sayi(y.eta))) }, 'Hesapla')));
  }
  if (tur === 'r2') {
    return h('details', { class: 'kucuk' }, h('summary', {}, 'R²\'den f² hesapla'), h('div', { class: 'satir', style: 'margin-top:.4rem' }, 'R² =', g('r2', '0,13'), h('button', { class: 'dugme kucuk', onclick: () => uygula(G.f2denR2(sayi(y.r2))) }, 'Hesapla')));
  }
  return null;
}

export function gucSekmesi(kap) {
  const sonucKap = h('div', {});
  const form = h('div', { class: 'yigin' });

  const hesapla = () => {
    try {
      const o = gucAnalizi(secim, null, baglam());
      const tablo = o.bloklar.find((x) => x.tur === 'tablo');
      const satir = o.bloklar.find((x) => x.tur === 'satir');
      sonucKap.replaceChildren(
        h(
          'section',
          { class: 'kart' },
          h('div', { class: 'kart-baslik' }, simge('guc'), h('h3', {}, 'Sonuç'), h('span', { class: 'bosluk' }), h('button', { class: 'dugme', onclick: () => { istekEkle({ tur: 'guc', secim: { ...secim } }); bildirim('Rapora eklendi.', { tur: 'tamam', eylem: { metin: 'Rapora git', tikla: () => sekmeyeGit('rapor') } }); } }, simge('arti', 16), 'Rapora ekle')),
          h('div', { class: 'raporlama-satiri', style: 'font-size:1.05rem' }, satir.metin),
          h('div', { class: 'tablo-kap', html: tabloHtml(tablo.tablo, '') }),
          ...o.bloklar.filter((x) => x.tur === 'not').map((x) => h('div', { class: 'bilgi-kutusu uyari' }, simge('uyari'), h('div', { class: 'kucuk' }, x.metin))),
        ),
      );
    } catch (e) {
      sonucKap.replaceChildren(h('div', { class: 'bilgi-kutusu hata' }, simge('uyari'), h('div', {}, e.message)));
    }
  };

  const ciz = () => {
    const tanim = GUC_TESTLERI[secim.test];
    const etkiGirdisi = metinGirdisi('etki', tanim.etki.ornek);
    form.replaceChildren(
      alan(
        'Test türü',
        h(
          'select',
          {
            onchange: (e) => {
              secim.test = e.target.value;
              secim.etki = '';
              ciz();
            },
          },
          Object.entries(GUC_TESTLERI).map(([k, t]) => h('option', { value: k, selected: k === secim.test }, t.ad)),
        ),
      ),
      alan(
        'Ne hesaplansın?',
        h(
          'select',
          { onchange: (e) => { secim.hesap = e.target.value; ciz(); } },
          h('option', { value: 'n', selected: secim.hesap === 'n' }, 'Gerekli örneklem büyüklüğü'),
          h('option', { value: 'guc', selected: secim.hesap === 'guc' }, 'Mevcut örneklemin gücü'),
          h('option', { value: 'etki', selected: secim.hesap === 'etki' }, 'Saptanabilir en küçük etki'),
        ),
      ),
      h(
        'div',
        { class: 'izgara uc' },
        secim.hesap !== 'etki' ? alan(tanim.etki.ad, etkiGirdisi, `Örnek: ${tanim.etki.ornek}. Cohen (1988) sınırları ya da önceki çalışmalardan alınmalıdır.`) : null,
        secim.hesap !== 'n' ? alan(tanim.grupBasina ? 'Örneklem büyüklüğü (grup başına)' : 'Örneklem büyüklüğü (toplam)', metinGirdisi('n', 'ör. 50')) : null,
        secim.hesap !== 'guc' ? alan('Güç (1 − β)', metinGirdisi('guc', '0,80'), 'Genellikle 0,80 ya da 0,90.') : null,
        alan('Anlamlılık düzeyi (α)', metinGirdisi('alfa', '0,05')),
        tanim.yonYok
          ? null
          : alan(
              'Hipotez',
              h(
                'select',
                { onchange: (e) => { secim.alternatif = e.target.value; } },
                h('option', { value: 'iki-yonlu', selected: secim.alternatif === 'iki-yonlu' }, 'İki yönlü'),
                h('option', { value: 'buyuk', selected: secim.alternatif === 'buyuk' }, 'Tek yönlü'),
              ),
            ),
        ...(tanim.ekAlanlar || []).map((a) => {
          if (secim[a.anahtar] === undefined) secim[a.anahtar] = a.varsayilan;
          return alan(a.ad, metinGirdisi(a.anahtar, a.varsayilan));
        }),
        secim.hesap === 'n' ? alan('Kayıp payı (%)', metinGirdisi('kayip', 'ör. 10'), 'Takipten çıkma / yanıtsızlık için örneklem artırılır.') : null,
      ),
      secim.hesap !== 'etki' && tanim.etki.yardimci ? yardimci(tanim.etki.yardimci, etkiGirdisi) : null,
      tanim.not ? h('p', { class: 'kucuk soluk' }, tanim.not) : null,
      h('div', {}, h('button', { class: 'dugme ana buyuk', onclick: hesapla }, simge('guc'), 'Hesapla')),
    );
  };
  ciz();
  kap.append(
    h(
      'section',
      { class: 'kart' },
      h('div', { class: 'kart-baslik' }, simge('guc'), h('h2', {}, 'Örneklem büyüklüğü ve güç analizi')),
      h('p', { class: 'soluk' }, 'Çalışmanızı planlarken kaç katılımcı gerektiğini ya da mevcut örneklemin gücünü hesaplayın. Hesaplar G*Power ve R (pwr) ile aynı yöntemleri kullanır; veri yüklemeniz gerekmez.'),
      form,
    ),
    sonucKap,
  );
}
