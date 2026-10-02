// 3. Analiz sekmesi: tek tıkla otomatik analiz ve araştırma sorusuna göre analiz formları.

import { durum, istekEkle, sekmeyeGit } from '../durum.js';
import { h, simge } from '../dom.js';
import { bildirim } from '../pencere.js';
import { ANALIZ_TANIMLARI } from '../../karar/tanimlar.js';
import { duzeyleriBul, grupAdaylari, pozitifDuzeyTahmin, sagkalimAdaylari, tekrarliOlcumAdaylari } from '../../veri/tanima.js';

let acikAnaliz = null;

function degiskenler(turler, haric = []) {
  return durum.veriSeti.degiskenler.filter((d) => turler.includes(durum.olcumler[d.ad]) && !haric.includes(d.ad));
}

function degiskenEtiketi(d) {
  return d.etiket ? `${d.ad} — ${d.etiket}` : d.ad;
}

const TUR_KISA = { surekli: 'S', sirali: 'O', kategorik: 'K' };

/** Form alanları. Her alan {el, deger(), ayarla(v)} döndürür. */
function alanOlustur(alan, secim, yenile) {
  const uygun = () => degiskenler(alan.turler || ['surekli', 'sirali', 'kategorik'], (alan.haric || []).map((k) => secim[k]).flat().filter(Boolean));
  switch (alan.tur) {
    case 'tek': {
      const sec = h(
        'select',
        {
          onchange: (e) => {
            secim[alan.ad] = e.target.value || null;
            yenile();
          },
        },
        alan.istegeBagli ? h('option', { value: '' }, alan.bosMetin || '(yok)') : h('option', { value: '', disabled: true, selected: !secim[alan.ad] }, 'Seçin…'),
        uygun().map((d) => h('option', { value: d.ad, selected: secim[alan.ad] === d.ad }, `${degiskenEtiketi(d)} [${TUR_KISA[durum.olcumler[d.ad]] || ''}]`)),
      );
      return h('div', { class: 'alan' }, h('label', {}, alan.etiket), sec, alan.aciklama ? h('div', { class: 'aciklama' }, alan.aciklama) : null);
    }
    case 'cok': {
      if (!Array.isArray(secim[alan.ad])) secim[alan.ad] = [];
      const secili = secim[alan.ad];
      const liste = uygun();
      // seçili ama artık uygun olmayanları çıkar
      for (let i = secili.length - 1; i >= 0; i--) if (!liste.some((d) => d.ad === secili[i])) secili.splice(i, 1);
      const satirlar = liste.map((d) =>
        h(
          'label',
          { class: 'secim-satiri' },
          h('input', {
            type: 'checkbox',
            checked: secili.includes(d.ad),
            onchange: (e) => {
              if (e.target.checked) secili.push(d.ad);
              else secili.splice(secili.indexOf(d.ad), 1);
              if (alan.sirali) yenile();
            },
          }),
          h('span', { class: 'ad' }, degiskenEtiketi(d)),
          h('span', { class: `etiket-kutu ${durum.olcumler[d.ad]}` }, { surekli: 'Sürekli', sirali: 'Sıralı', kategorik: 'Kategorik' }[durum.olcumler[d.ad]]),
        ),
      );
      const tumu = h(
        'button',
        {
          class: 'dugme hayalet kucuk',
          type: 'button',
          onclick: () => {
            if (secili.length === liste.length) secili.length = 0;
            else {
              secili.length = 0;
              liste.forEach((d) => secili.push(d.ad));
            }
            yenile();
          },
        },
        secili.length === liste.length && liste.length ? 'Seçimi kaldır' : 'Tümünü seç',
      );
      const siraKutusu =
        alan.sirali && secili.length > 1
          ? h(
              'div',
              { class: 'kucuk', style: 'margin-top:.4rem' },
              h('span', { class: 'soluk' }, 'Ölçüm sırası: '),
              secili.map((ad, i) =>
                h(
                  'span',
                  { class: 'etiket-kutu', style: 'margin:.1rem' },
                  `${i + 1}. ${ad}`,
                  i > 0 ? h('button', { class: 'dugme hayalet kucuk', type: 'button', title: 'Öne al', style: 'padding:0 .2rem', onclick: () => { [secili[i - 1], secili[i]] = [secili[i], secili[i - 1]]; yenile(); } }, '‹') : null,
                ),
              ),
            )
          : null;
      return h(
        'div',
        { class: 'alan' },
        h('div', { class: 'satir' }, h('span', { class: 'alan-baslik' }, alan.etiket), h('span', { class: 'bosluk' }), liste.length ? tumu : null),
        liste.length ? h('div', { class: 'secim-listesi' }, satirlar) : h('div', { class: 'aciklama' }, 'Uygun türde değişken yok. Değişkenler adımında türleri kontrol edin.'),
        alan.aciklama ? h('div', { class: 'aciklama' }, alan.aciklama) : null,
        siraKutusu,
      );
    }
    case 'duzey': {
      const kaynak = secim[alan.kaynak];
      if (!kaynak) return h('div', { class: 'alan' }, h('label', {}, alan.etiket), h('div', { class: 'aciklama' }, 'Önce değişkeni seçin.'));
      const d = durum.veriSeti.degiskenler.find((x) => x.ad === kaynak);
      const dz = duzeyleriBul(d);
      if (secim[alan.ad] === undefined || !dz.some((x) => String(x.deger) === String(secim[alan.ad]))) secim[alan.ad] = String(pozitifDuzeyTahmin(dz) ?? '');
      return h(
        'div',
        { class: 'alan' },
        h('label', {}, alan.etiket),
        h('select', { onchange: (e) => { secim[alan.ad] = e.target.value; } }, dz.map((x) => h('option', { value: String(x.deger), selected: String(x.deger) === String(secim[alan.ad]) }, `${x.etiket} (n = ${x.n})`))),
        alan.aciklama ? h('div', { class: 'aciklama' }, alan.aciklama) : null,
      );
    }
    case 'onay': {
      if (secim[alan.ad] === undefined) secim[alan.ad] = alan.varsayilan ?? false;
      return h('label', { class: 'satir' }, h('input', { type: 'checkbox', checked: secim[alan.ad], onchange: (e) => { secim[alan.ad] = e.target.checked; if (alan.yenile) yenile(); } }), h('span', {}, alan.etiket), alan.aciklama ? h('span', { class: 'kucuk soluk' }, `— ${alan.aciklama}`) : null);
    }
    case 'secim': {
      if (secim[alan.ad] === undefined) secim[alan.ad] = alan.varsayilan ?? alan.secenekler[0][0];
      return h(
        'div',
        { class: 'alan' },
        h('label', {}, alan.etiket),
        h('select', { onchange: (e) => { secim[alan.ad] = e.target.value; if (alan.yenile) yenile(); } }, alan.secenekler.map(([v, ad]) => h('option', { value: v, selected: String(secim[alan.ad]) === String(v) }, ad))),
        alan.aciklama ? h('div', { class: 'aciklama' }, alan.aciklama) : null,
      );
    }
    case 'metin':
    case 'sayi': {
      if (secim[alan.ad] === undefined) secim[alan.ad] = alan.varsayilan ?? '';
      return h(
        'div',
        { class: 'alan' },
        h('label', {}, alan.etiket),
        h('input', { type: 'text', value: secim[alan.ad], placeholder: alan.yerTutucu || '', oninput: (e) => { secim[alan.ad] = e.target.value; } }),
        alan.aciklama ? h('div', { class: 'aciklama' }, alan.aciklama) : null,
      );
    }
    default:
      return null;
  }
}

function formCiz(tanim, kap, secim) {
  const govde = h('div', { class: 'yigin' });
  const ciz = () => {
    govde.replaceChildren(...tanim.alanlar.filter((a) => !a.gorunur || a.gorunur(secim, durum)).map((a) => alanOlustur(a, secim, ciz)));
  };
  ciz();
  const calistir = () => {
    const hata = tanim.dogrula ? tanim.dogrula(secim, durum) : null;
    if (hata) {
      bildirim(hata, { tur: 'hata', sure: 6000 });
      return;
    }
    const istekler = tanim.istekler ? tanim.istekler(secim, durum) : [{ tur: tanim.id, secim: JSON.parse(JSON.stringify(secim)) }];
    const eklenenler = istekler.map((i) => istekEkle(i));
    durum.sonEklenen = eklenenler[0]?.id ?? null;
    acikAnaliz = null;
    sekmeyeGit('rapor');
  };
  kap.append(
    h(
      'section',
      { class: 'kart', id: 'analiz-formu' },
      h('div', { class: 'kart-baslik' }, simge(tanim.simge), h('h2', {}, tanim.baslik), h('span', { class: 'bosluk' }), h('button', { class: 'dugme hayalet', onclick: () => { acikAnaliz = null; kap.replaceChildren(); analizSekmesi(kap); } }, 'Kapat')),
      h('p', { class: 'soluk' }, tanim.aciklama),
      govde,
      h('div', { class: 'satir', style: 'margin-top:1rem' }, h('button', { class: 'dugme ana buyuk', onclick: calistir }, simge('analiz'), 'Analiz et'), tanim.not ? h('span', { class: 'kucuk soluk' }, tanim.not) : null),
    ),
  );
  document.getElementById('analiz-formu')?.scrollIntoView({ behavior: 'smooth', block: 'start' });
}

function otomatikKart() {
  const vs = durum.veriSeti;
  const o = durum.olcumler;
  const gruplar = grupAdaylari(vs, o);
  const { tekrarli } = tekrarliOlcumAdaylari(vs, o);
  const sk = sagkalimAdaylari(vs, o);
  const surekliler = vs.degiskenler.filter((d) => o[d.ad] === 'surekli');
  const secim = { grup: gruplar[0]?.ad ?? '', betimsel: true, karsilastirma: true, tekrarli: tekrarli.length > 0, korelasyon: surekliler.length >= 2 && surekliler.length <= 20, sagkalim: sk.sureler.length > 0 && sk.olaylar.length > 0 };
  const grupSec = h(
    'select',
    { onchange: (e) => { secim.grup = e.target.value; } },
    h('option', { value: '' }, '(grup karşılaştırması yapma)'),
    vs.degiskenler.filter((d) => ['kategorik', 'sirali'].includes(o[d.ad])).map((d) => h('option', { value: d.ad, selected: d.ad === secim.grup }, d.ad)),
  );
  const onay = (anahtar, metin, devre = false) => h('label', { class: 'satir kucuk' }, h('input', { type: 'checkbox', checked: secim[anahtar], disabled: devre, onchange: (e) => { secim[anahtar] = e.target.checked; } }), metin);
  const calistir = () => {
    const ist = [];
    const analizEdilecek = vs.degiskenler.filter((d) => ['surekli', 'sirali', 'kategorik'].includes(o[d.ad])).map((d) => d.ad);
    if (secim.betimsel) ist.push({ tur: 'betimsel', secim: { degiskenler: analizEdilecek, grafik: false } });
    if (secim.grup && secim.karsilastirma) ist.push({ tur: 'grup', secim: { grup: secim.grup, degiskenler: analizEdilecek.filter((a) => a !== secim.grup), grafik: true } });
    if (secim.tekrarli) for (const t of tekrarli) ist.push({ tur: 'eslestirilmis', secim: { degiskenler: t.degiskenler, grup: secim.grup || null, grafik: true } });
    if (secim.korelasyon && surekliler.length >= 2) ist.push({ tur: 'korelasyon', secim: { degiskenler: surekliler.slice(0, 20).map((d) => d.ad), grafik: true } });
    if (secim.sagkalim && sk.sureler.length && sk.olaylar.length) {
      const olayD = vs.degiskenler.find((d) => d.ad === sk.olaylar[0]);
      const dz = duzeyleriBul(olayD);
      ist.push({ tur: 'sagkalim', secim: { sure: sk.sureler[0], olay: sk.olaylar[0], olayDuzeyi: String(pozitifDuzeyTahmin(dz)), grup: secim.grup || null, grafik: true } });
    }
    if (!ist.length) {
      bildirim('En az bir analiz seçin.', { tur: 'hata' });
      return;
    }
    const eklenenler = ist.map((i) => istekEkle(i));
    durum.sonEklenen = eklenenler[0].id;
    sekmeyeGit('rapor');
  };
  return h(
    'section',
    { class: 'kart otomatik-kart' },
    h('div', { class: 'kart-baslik' }, simge('sihir', 22), h('h2', {}, 'Otomatik analiz'), h('span', { class: 'etiket-kutu' }, 'önerilen başlangıç')),
    h('p', {}, 'Program verinizin yapısına göre temel analizlerin hepsini yapar: betimsel istatistikler, gruplar arası karşılaştırma tablosu (Tablo 1), tekrarlı ölçümler, korelasyonlar ve sağkalım. Her değişken için uygun test kendiliğinden seçilir ve nedeni rapora yazılır.'),
    h(
      'div',
      { class: 'izgara iki' },
      h('div', { class: 'alan' }, h('label', {}, 'Grup değişkeni (karşılaştırma için)'), grupSec, h('div', { class: 'aciklama' }, gruplar.length ? `Önerilen: ${gruplar[0].ad}` : 'Grup değişkeni bulunamadı; yalnızca betimsel analiz yapılır.')),
      h(
        'div',
        { class: 'yigin', style: 'gap:.3rem' },
        onay('betimsel', 'Tüm değişkenlerin betimsel istatistikleri'),
        onay('karsilastirma', 'Gruplara göre karşılaştırma (Tablo 1)'),
        onay('tekrarli', tekrarli.length ? `Tekrarlı ölçümler (${tekrarli.map((t) => t.ad).join(', ')})` : 'Tekrarlı ölçüm bulunamadı', !tekrarli.length),
        onay('korelasyon', `Sürekli değişkenler arası korelasyon (${surekliler.length} değişken)`, surekliler.length < 2),
        onay('sagkalim', sk.sureler.length && sk.olaylar.length ? `Sağkalım (${sk.sureler[0]}, ${sk.olaylar[0]})` : 'Sağkalım verisi bulunamadı', !(sk.sureler.length && sk.olaylar.length)),
      ),
    ),
    h('div', { class: 'satir', style: 'margin-top:1rem' }, h('button', { class: 'dugme ana buyuk', onclick: calistir }, simge('sihir'), 'Otomatik analiz yap')),
  );
}

export function analizSekmesi(kap) {
  if (durum.onDoldurma) {
    const { tur, secim } = durum.onDoldurma;
    durum.onDoldurma = null;
    acikAnaliz = { id: tur === 'tablo1' ? 'grup' : tur, secim: { ...secim } };
  }
  kap.append(
    otomatikKart(),
    h(
      'section',
      { class: 'kart' },
      h('div', { class: 'kart-baslik' }, simge('analiz'), h('h2', {}, 'Ne öğrenmek istiyorsunuz?')),
      h('p', { class: 'soluk' }, 'Araştırma sorunuza uyan kartı seçin, değişkenleri belirleyin. Hangi testin kullanılacağına program, verinizin dağılımına ve türüne bakarak karar verir.'),
      h(
        'div',
        { class: 'analiz-kartlari' },
        ANALIZ_TANIMLARI.map((t) =>
          h(
            'button',
            {
              class: 'analiz-karti',
              'aria-pressed': acikAnaliz?.id === t.id ? 'true' : 'false',
              onclick: () => {
                if (t.id === 'guc') {
                  sekmeyeGit('guc');
                  return;
                }
                acikAnaliz = { id: t.id, secim: {} };
                kap.replaceChildren();
                analizSekmesi(kap);
              },
            },
            h('span', { class: 'baslik' }, simge(t.simge), t.baslik),
            h('span', { class: 'kucuk' }, t.kisa),
            t.ornek ? h('span', { class: 'soru' }, t.ornek) : null,
          ),
        ),
      ),
    ),
  );
  if (acikAnaliz) {
    const t = ANALIZ_TANIMLARI.find((x) => x.id === acikAnaliz.id);
    if (t && t.alanlar) formCiz(t, kap, acikAnaliz.secim);
  }
}
