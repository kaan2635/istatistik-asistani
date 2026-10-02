// 2. Değişkenler sekmesi: ölçüm türlerini onaylama, değişken düzenleme, öneriler ve yeni değişken araçları.

import { baglam, degiskenGuncellendi, durum, olcumAyarla, sekmeyeGit, bildir } from '../durum.js';
import { h, simge } from '../dom.js';
import { bildirim, onayla, pencereAc } from '../pencere.js';
import { degiskenOzeti, duzeyleriBul, grupAdaylari, pozitifDuzeyTahmin, sagkalimAdaylari, tekrarliOlcumAdaylari } from '../../veri/tanima.js';
import * as T from '../../veri/donustur.js';
import { sayi } from '../../rapor/bicim.js';

export const OLCUM_ADLARI = {
  surekli: 'Sürekli (sayısal ölçüm)',
  sirali: 'Sıralı (ordinal)',
  kategorik: 'Kategorik (nominal)',
  kimlik: 'Kimlik / analiz dışı',
  haric: 'Analiz dışı',
};
const OLCUM_KISA = { surekli: 'Sürekli', sirali: 'Sıralı', kategorik: 'Kategorik', kimlik: 'Kimlik', haric: 'Dışarıda' };

function degerOzeti(d, olcum) {
  const oz = degiskenOzeti(d);
  const b = baglam().bicim;
  if (oz.n === 0) return 'Tüm değerler eksik';
  if (olcum === 'kategorik' || olcum === 'sirali') {
    const dz = duzeyleriBul(d);
    const ilk = dz.slice(0, 4).map((x) => `${x.etiket} (${x.n})`).join(', ');
    return dz.length > 4 ? `${ilk} … (${dz.length} düzey)` : ilk;
  }
  if (oz.min !== null) return `${sayi(oz.min, Number.isInteger(oz.min) ? 0 : 2, b)} – ${sayi(oz.max, Number.isInteger(oz.max) ? 0 : 2, b)}`;
  return `${oz.benzersiz} farklı değer`;
}

/** Analiz sekmesine ön doldurmayla geçiş */
function analizeGit(tur, secim) {
  durum.onDoldurma = { tur, secim };
  sekmeyeGit('analiz');
}

function oneriler() {
  const vs = durum.veriSeti;
  const o = durum.olcumler;
  const liste = [];
  const grup = grupAdaylari(vs, o)[0];
  if (grup) {
    const dz = duzeyleriBul(vs.degiskenler.find((d) => d.ad === grup.ad));
    liste.push(
      h(
        'div',
        { class: 'oneri' },
        simge('analiz'),
        h('div', { class: 'bosluk' }, h('strong', {}, 'Grup değişkeni olabilir: '), `${grup.ad} (${dz.map((x) => x.etiket).join(' / ')})`),
        h('button', { class: 'dugme kucuk ana', onclick: () => analizeGit('tablo1', { grup: grup.ad }) }, 'Tüm değişkenleri bu gruba göre karşılaştır'),
      ),
    );
  }
  const { tekrarli, olcekMaddeleri } = tekrarliOlcumAdaylari(vs, o);
  for (const t of tekrarli) {
    liste.push(
      h(
        'div',
        { class: 'oneri' },
        simge('yenile'),
        h('div', { class: 'bosluk' }, h('strong', {}, 'Tekrarlı ölçüm: '), t.degiskenler.join(' → ')),
        h('button', { class: 'dugme kucuk', onclick: () => analizeGit('eslestirilmis', { degiskenler: t.degiskenler, grup: grup?.ad ?? null }) }, 'Zaman içindeki değişimi analiz et'),
      ),
    );
  }
  for (const m of olcekMaddeleri) {
    liste.push(
      h(
        'div',
        { class: 'oneri' },
        simge('tablo'),
        h('div', { class: 'bosluk' }, h('strong', {}, 'Ölçek maddeleri: '), `${m.degiskenler[0]} – ${m.degiskenler[m.degiskenler.length - 1]} (${m.degiskenler.length} madde)`),
        h('button', { class: 'dugme kucuk', onclick: () => analizeGit('guvenirlik', { degiskenler: m.degiskenler }) }, 'Güvenirlik analizi'),
        h('button', { class: 'dugme kucuk', onclick: () => olcekPuaniPenceresi(m.degiskenler) }, 'Ölçek puanı hesapla'),
      ),
    );
  }
  const sk = sagkalimAdaylari(vs, o);
  if (sk.sureler.length && sk.olaylar.length) {
    liste.push(
      h(
        'div',
        { class: 'oneri' },
        simge('grafik'),
        h('div', { class: 'bosluk' }, h('strong', {}, 'Sağkalım verisi olabilir: '), `süre = ${sk.sureler[0]}, olay = ${sk.olaylar[0]}`),
        h('button', { class: 'dugme kucuk', onclick: () => analizeGit('sagkalim', { sure: sk.sureler[0], olay: sk.olaylar[0], olayDuzeyi: String(pozitifDuzeyTahmin(duzeyleriBul(vs.degiskenler.find((d) => d.ad === sk.olaylar[0])))), grup: grup?.ad ?? null }) }, 'Sağkalım analizi'),
      ),
    );
  }
  return liste;
}

export function degiskenlerSekmesi(kap) {
  const vs = durum.veriSeti;
  const kontrolSayisi = vs.degiskenler.filter((d) => durum.tahminler[d.ad]?.guven === 'dusuk').length;
  const satirlar = vs.degiskenler.map((d, idx) => {
    const olcum = durum.olcumler[d.ad];
    const tahmin = durum.tahminler[d.ad];
    const oz = degiskenOzeti(d);
    const kontrol = tahmin?.guven === 'dusuk';
    return h(
      'tr',
      { class: kontrol ? 'kontrol' : null },
      h('td', { class: 'soluk kucuk' }, idx + 1),
      h('td', {}, h('div', { class: 'kalin' }, d.ad), d.etiket ? h('div', { class: 'kucuk soluk' }, d.etiket) : null),
      h(
        'td',
        {},
        h(
          'select',
          { class: 'tur-sec', 'aria-label': `${d.ad} ölçüm türü`, onchange: (e) => olcumAyarla(d.ad, e.target.value) },
          Object.entries(OLCUM_ADLARI)
            .filter(([k]) => k !== 'haric' || olcum === 'haric')
            .map(([k, ad]) => h('option', { value: k, selected: k === olcum, disabled: k === 'surekli' && d.veriTuru === 'metin' }, ad)),
        ),
      ),
      h('td', {}, kontrol ? h('span', { class: 'etiket-kutu uyari', title: tahmin.gerekce }, simge('uyari', 12), 'Kontrol edin') : h('span', { class: `etiket-kutu ${olcum}`, title: tahmin?.gerekce || '' }, OLCUM_KISA[olcum])),
      h('td', { class: 'kucuk' }, `${oz.n}`, oz.eksik ? h('span', { class: 'soluk' }, ` (${oz.eksik} eksik)`) : null),
      h('td', { class: 'kucuk soluk', style: 'max-width:340px' }, degerOzeti(d, olcum), kontrol ? h('div', { class: 'kucuk', style: 'color:var(--uyari)' }, tahmin.gerekce) : null),
      h(
        'td',
        { style: 'white-space:nowrap' },
        h('button', { class: 'dugme kucuk', onclick: () => degiskenDuzenle(d) }, 'Düzenle'),
        d.turetilmis ? h('button', { class: 'dugme kucuk hayalet tehlike', title: 'Türetilmiş değişkeni sil', onclick: () => degiskenSil(d) }, simge('sil', 14)) : null,
      ),
    );
  });

  const oner = oneriler();
  kap.append(
    oner.length
      ? h('section', { class: 'kart' }, h('div', { class: 'kart-baslik' }, simge('sihir'), h('h2', {}, 'Verinizde tespit edilenler')), h('div', { class: 'oneri-liste' }, oner))
      : null,
    h(
      'section',
      { class: 'kart' },
      h(
        'div',
        { class: 'kart-baslik' },
        simge('degisken'),
        h('h2', {}, 'Değişkenler'),
        kontrolSayisi ? h('span', { class: 'etiket-kutu uyari' }, `${kontrolSayisi} değişkeni kontrol edin`) : h('span', { class: 'etiket-kutu' }, 'Türler otomatik belirlendi'),
        h('span', { class: 'bosluk' }),
        h('button', { class: 'dugme ana', onclick: () => sekmeyeGit('analiz') }, 'Analize geç', simge('ok')),
      ),
      h(
        'div',
        { class: 'bilgi-kutusu', style: 'margin-bottom:.75rem' },
        simge('bilgi'),
        h(
          'div',
          { class: 'kucuk' },
          'Program her değişkenin türünü değerlerinden tahmin etti. Test seçimi bu türlere göre yapılır, bu yüzden doğru olduklarından emin olun. ',
          h('strong', {}, 'Sürekli: '),
          'yaş, kilo, laboratuvar değeri gibi ölçümler. ',
          h('strong', {}, 'Sıralı: '),
          'evre, derece, Likert maddesi gibi sıralanabilen kategoriler. ',
          h('strong', {}, 'Kategorik: '),
          'cinsiyet, grup, kan grubu gibi sırasız kategoriler.',
        ),
      ),
      h('div', { class: 'satir', style: 'margin-bottom:.75rem' }, h('span', { class: 'kucuk soluk' }, 'Yeni değişken:'), araclarDugmeleri()),
      h('div', { style: 'overflow-x:auto' }, h('table', { class: 'liste' }, h('thead', {}, h('tr', {}, h('th', {}, '#'), h('th', {}, 'Değişken'), h('th', {}, 'Ölçüm türü'), h('th', {}, ''), h('th', {}, 'Geçerli n'), h('th', {}, 'Değerler'), h('th', {}, ''))), h('tbody', {}, satirlar))),
    ),
  );
}

function araclarDugmeleri() {
  return [
    h('button', { class: 'dugme kucuk', onclick: hesaplaPenceresi }, simge('arti', 14), 'Formülle hesapla'),
    h('button', { class: 'dugme kucuk', onclick: kategorizePenceresi }, 'Kategorilere ayır'),
    h('button', { class: 'dugme kucuk', onclick: yenidenKodlaPenceresi }, 'Yeniden kodla / birleştir'),
    h('button', { class: 'dugme kucuk', onclick: () => olcekPuaniPenceresi([]) }, 'Ölçek puanı'),
    durum.veriSeti.degiskenler.filter((d) => d.veriTuru === 'tarih').length >= 2 ? h('button', { class: 'dugme kucuk', onclick: tarihFarkiPenceresi }, 'Tarih farkı (süre)') : null,
  ];
}

function degiskenEkle(d, olcum) {
  durum.veriSeti.degiskenler.push(d);
  durum.olcumler[d.ad] = olcum ?? d.olcum ?? 'surekli';
  durum.tahminler[d.ad] = { olcum: durum.olcumler[d.ad], guven: 'yuksek', gerekce: 'Türetilmiş değişken.' };
  bildir('degisken');
  bildirim(`"${d.ad}" değişkeni eklendi.`, { tur: 'tamam' });
}

async function degiskenSil(d) {
  if (!(await onayla('Değişken silinsin mi?', `"${d.ad}" veri setinden kaldırılacak.`, { evet: 'Sil', tehlike: true }))) return;
  durum.veriSeti.degiskenler = durum.veriSeti.degiskenler.filter((x) => x !== d);
  delete durum.olcumler[d.ad];
  bildir('degisken');
}

function degiskenSecici(filtre, secili, etiket = 'Değişken') {
  const sec = h('select', {}, durum.veriSeti.degiskenler.filter((d) => filtre(durum.olcumler[d.ad], d)).map((d) => h('option', { value: d.ad, selected: d.ad === secili }, d.ad)));
  return { el: h('div', { class: 'alan' }, h('label', {}, etiket), sec), deger: () => sec.value, sec };
}

function adAlani(varsayilan) {
  const g = h('input', { type: 'text', value: varsayilan });
  return { el: h('div', { class: 'alan' }, h('label', {}, 'Yeni değişkenin adı'), g), deger: () => g.value.trim() };
}

/** Hata bildirir ve pencerenin açık kalması için false döner */
function hata(_alan, mesaj) {
  bildirim(mesaj, { tur: 'hata', sure: 6000 });
  return false;
}

function hesaplaPenceresi() {
  const ad = adAlani('');
  const formul = h('input', { type: 'text', placeholder: 'ör. kilo / (boy/100)^2', style: 'width:100%' });
  const olcum = h('select', {}, h('option', { value: 'surekli' }, OLCUM_ADLARI.surekli), h('option', { value: 'kategorik' }, OLCUM_ADLARI.kategorik), h('option', { value: 'sirali' }, OLCUM_ADLARI.sirali));
  pencereAc({
    baslik: 'Formülle yeni değişken hesapla',
    govde: h(
      'div',
      { class: 'yigin' },
      ad.el,
      h('div', { class: 'alan' }, h('label', {}, 'Formül'), formul, h('div', { class: 'aciklama' }, 'İşlemler: + − * / ^ ( ). Fonksiyonlar: ln, log10, exp, karekok, mutlak, yuvarla(x, basamak), ortalama(…), toplam(…), min(…), maks(…), eger(koşul, a, b). Karşılaştırma: < > <= >= == !=, bağlaçlar: ve, veya. Adında boşluk olan değişkeni [köşeli parantez] içinde yazın. Örnek: eger(yas >= 65, 1, 0)')),
      h('div', { class: 'alan' }, h('label', {}, 'Ölçüm türü'), olcum),
      h('div', { class: 'kucuk soluk' }, 'Değişkenler: ', durum.veriSeti.degiskenler.map((d) => d.ad).join(', ')),
    ),
    dugmeler: [
      { metin: 'Vazgeç' },
      {
        metin: 'Hesapla',
        ana: true,
        tikla: () => {
          const sorun = T.adKullanilabilir(durum.veriSeti, ad.deger());
          if (sorun) return hata(ad, sorun);
          try {
            degiskenEkle(T.hesapla(durum.veriSeti, ad.deger(), formul.value), olcum.value);
          } catch (e) {
            return hata(formul, e.message);
          }
        },
      },
    ],
  });
}

function kategorizePenceresi() {
  const kaynak = degiskenSecici((o) => o === 'surekli', null, 'Sürekli değişken');
  const ad = adAlani('');
  const yontem = h('select', {}, h('option', { value: 'ozel' }, 'Kendi kesim noktalarım'), h('option', { value: '2' }, 'Medyana göre 2 grup'), h('option', { value: '3' }, 'Tersiller (3 grup)'), h('option', { value: '4' }, 'Çeyreklikler (4 grup)'));
  const kesimler = h('input', { type: 'text', placeholder: 'ör. 40; 65 (noktalı virgülle ayırın)', style: 'width:100%' });
  const etiketler = h('input', { type: 'text', placeholder: 'isteğe bağlı: Genç; Orta yaş; Yaşlı', style: 'width:100%' });
  const bilgi = h('p', { class: 'kucuk soluk' }, 'Aralıklar alttan kapalı, üstten açıktır: 40 ve 65 kesimleri için gruplar <40, 40–<65, ≥65 olur.');
  yontem.addEventListener('change', () => {
    if (yontem.value !== 'ozel') {
      const k = T.yuzdelikKesimleri(durum.veriSeti, kaynak.deger(), Number(yontem.value), durum.ayarlar.yuzdelikTipi);
      kesimler.value = k.map((x) => String(Math.round(x * 1e6) / 1e6).replace('.', ',')).join('; ');
    }
  });
  kaynak.sec.addEventListener('change', () => {
    ad.el.querySelector('input').value = `${kaynak.deger()}_grup`;
    yontem.dispatchEvent(new Event('change'));
  });
  ad.el.querySelector('input').value = `${kaynak.deger()}_grup`;
  pencereAc({
    baslik: 'Sürekli değişkeni kategorilere ayır',
    govde: h('div', { class: 'yigin' }, kaynak.el, ad.el, h('div', { class: 'alan' }, h('label', {}, 'Yöntem'), yontem), h('div', { class: 'alan' }, h('label', {}, 'Kesim noktaları'), kesimler), h('div', { class: 'alan' }, h('label', {}, 'Grup etiketleri'), etiketler), bilgi),
    dugmeler: [
      { metin: 'Vazgeç' },
      {
        metin: 'Oluştur',
        ana: true,
        tikla: () => {
          const sorun = T.adKullanilabilir(durum.veriSeti, ad.deger());
          if (sorun) return hata(ad, sorun);
          const k = kesimler.value.split(';').map((s) => Number(s.trim().replace(',', '.'))).filter((x) => Number.isFinite(x));
          const e = etiketler.value.trim() ? etiketler.value.split(';').map((s) => s.trim()) : null;
          try {
            degiskenEkle(T.kategorize(durum.veriSeti, kaynak.deger(), ad.deger(), k, e, durum.ayarlar.ondalikAyrac), 'sirali');
          } catch (err) {
            return hata(kesimler, err.message);
          }
        },
      },
    ],
  });
}

function yenidenKodlaPenceresi() {
  const kaynak = degiskenSecici((o) => o === 'kategorik' || o === 'sirali', null, 'Kategorik değişken');
  const ad = adAlani('');
  const tablo = h('div', { class: 'yigin' });
  let girdiler = [];
  const ciz = () => {
    const d = durum.veriSeti.degiskenler.find((x) => x.ad === kaynak.deger());
    ad.el.querySelector('input').value = `${d.ad}_yeni`;
    const dz = duzeyleriBul(d);
    girdiler = dz.map((x) => ({ deger: x.deger, girdi: h('input', { type: 'text', value: x.etiket }) }));
    tablo.replaceChildren(
      h('p', { class: 'kucuk soluk' }, 'Birleştirmek istediğiniz kategorilere aynı yeni adı yazın. Boş bırakılan kategoriler eksik değer olur.'),
      h('table', { class: 'liste' }, h('thead', {}, h('tr', {}, h('th', {}, 'Mevcut kategori'), h('th', {}, 'n'), h('th', {}, 'Yeni kategori'))), h('tbody', {}, dz.map((x, i) => h('tr', {}, h('td', {}, x.etiket), h('td', {}, x.n), h('td', {}, girdiler[i].girdi))))),
    );
  };
  kaynak.sec.addEventListener('change', ciz);
  ciz();
  pencereAc({
    baslik: 'Yeniden kodla / kategorileri birleştir',
    govde: h('div', { class: 'yigin' }, kaynak.el, ad.el, tablo),
    dugmeler: [
      { metin: 'Vazgeç' },
      {
        metin: 'Oluştur',
        ana: true,
        tikla: () => {
          const sorun = T.adKullanilabilir(durum.veriSeti, ad.deger());
          if (sorun) return hata(ad, sorun);
          const eslesme = {};
          const sira = [];
          for (const g of girdiler) {
            const v = g.girdi.value.trim();
            eslesme[String(g.deger)] = v;
            if (v && !sira.includes(v)) sira.push(v);
          }
          const d = T.yenidenKodla(durum.veriSeti, kaynak.deger(), ad.deger(), eslesme, { digerleriniKoru: false, sira });
          degiskenEkle(d, durum.olcumler[kaynak.deger()] === 'sirali' ? 'sirali' : 'kategorik');
        },
      },
    ],
  });
}

export function olcekPuaniPenceresi(onSecili = []) {
  const adaylar = durum.veriSeti.degiskenler.filter((d) => ['surekli', 'sirali'].includes(durum.olcumler[d.ad]) && d.veriTuru === 'sayi');
  const secili = new Set(onSecili);
  const ters = new Set();
  const ad = adAlani('Toplam_puan');
  const yontem = h('select', {}, h('option', { value: 'toplam' }, 'Toplam puan'), h('option', { value: 'ortalama' }, 'Ortalama puan'));
  const min = h('input', { type: 'number', value: '1', style: 'width:5rem' });
  const maks = h('input', { type: 'number', value: '5', style: 'width:5rem' });
  const enAz = h('input', { type: 'number', value: '', placeholder: 'tümü', style: 'width:6rem' });
  const liste = h(
    'div',
    { class: 'secim-listesi' },
    adaylar.map((d) =>
      h(
        'div',
        { class: 'secim-satiri' },
        h('input', { type: 'checkbox', checked: secili.has(d.ad), onchange: (e) => (e.target.checked ? secili.add(d.ad) : secili.delete(d.ad)) }),
        h('span', { class: 'ad' }, d.ad),
        h('label', { class: 'kucuk soluk' }, h('input', { type: 'checkbox', onchange: (e) => (e.target.checked ? ters.add(d.ad) : ters.delete(d.ad)) }), ' ters'),
      ),
    ),
  );
  pencereAc({
    baslik: 'Ölçek puanı hesapla',
    govde: h(
      'div',
      { class: 'yigin' },
      ad.el,
      h('div', { class: 'alan' }, h('span', { class: 'alan-baslik' }, 'Maddeler (ters puanlanacakları işaretleyin)'), liste),
      h('div', { class: 'satir' }, h('div', { class: 'alan' }, h('label', {}, 'Yöntem'), yontem), h('div', { class: 'alan' }, h('label', {}, 'Ölçek en küçük'), min), h('div', { class: 'alan' }, h('label', {}, 'en büyük'), maks), h('div', { class: 'alan' }, h('label', {}, 'En az geçerli madde'), enAz)),
      h('p', { class: 'kucuk soluk' }, 'Ters maddeler (en küçük + en büyük − puan) ile çevrilir. Eksik maddesi olan kişinin puanı, "en az geçerli madde" sağlanıyorsa geçerli maddelerin ortalamasından hesaplanır; aksi halde eksik kalır.'),
    ),
    dugmeler: [
      { metin: 'Vazgeç' },
      {
        metin: 'Hesapla',
        ana: true,
        tikla: () => {
          const sorun = T.adKullanilabilir(durum.veriSeti, ad.deger());
          if (sorun) return hata(ad, sorun);
          if (secili.size < 2) return hata(null, 'En az iki madde seçin.');
          const maddeler = adaylar.map((d) => d.ad).filter((x) => secili.has(x));
          try {
            degiskenEkle(
              T.olcekPuani(durum.veriSeti, maddeler, ad.deger(), {
                ters: [...ters].filter((x) => secili.has(x)),
                min: Number(min.value),
                maks: Number(maks.value),
                yontem: yontem.value,
                enAzGecerli: enAz.value ? Number(enAz.value) : null,
              }),
              'surekli',
            );
          } catch (e) {
            return hata(null, e.message);
          }
        },
      },
    ],
  });
}

function tarihFarkiPenceresi() {
  const a = degiskenSecici((_, d) => d.veriTuru === 'tarih', null, 'Başlangıç tarihi');
  const b = degiskenSecici((_, d) => d.veriTuru === 'tarih', null, 'Bitiş tarihi');
  const ad = adAlani('Sure');
  const birim = h('select', {}, h('option', { value: 'gun' }, 'gün'), h('option', { value: 'hafta' }, 'hafta'), h('option', { value: 'ay' }, 'ay'), h('option', { value: 'yil' }, 'yıl'));
  pencereAc({
    baslik: 'İki tarih arasındaki süre',
    govde: h('div', { class: 'yigin' }, a.el, b.el, ad.el, h('div', { class: 'alan' }, h('label', {}, 'Birim'), birim)),
    dugmeler: [
      { metin: 'Vazgeç' },
      {
        metin: 'Hesapla',
        ana: true,
        tikla: () => {
          const sorun = T.adKullanilabilir(durum.veriSeti, ad.deger());
          if (sorun) return hata(ad, sorun);
          try {
            degiskenEkle(T.tarihFarki(durum.veriSeti, a.deger(), b.deger(), ad.deger(), birim.value), 'surekli');
          } catch (e) {
            return hata(null, e.message);
          }
        },
      },
    ],
  });
}

/** Değişken düzenleme: etiket, değer etiketleri, düzey sırası, eksik kodlar */
function degiskenDuzenle(d) {
  const olcum = durum.olcumler[d.ad];
  const etiket = h('input', { type: 'text', value: d.etiket || '', style: 'width:100%', placeholder: 'Tablolarda görünecek ad (ör. Vücut kitle indeksi, kg/m²)' });
  const eksik = h('input', { type: 'text', value: (d.eksikKodlar?.tekil || []).join('; '), placeholder: 'ör. 99; 999' });
  const kategorik = olcum === 'kategorik' || olcum === 'sirali';
  let dz = kategorik ? duzeyleriBul(d) : [];
  const etiketGirdileri = new Map();
  const duzeyKap = h('div', {});
  const duzeyCiz = () => {
    duzeyKap.replaceChildren(
      h('table', { class: 'liste' }, h('thead', {}, h('tr', {}, h('th', {}, 'Sıra'), h('th', {}, 'Değer'), h('th', {}, 'Etiket'), h('th', {}, 'n'), h('th', {}, ''))), h(
        'tbody',
        {},
        dz.map((x, i) => {
          if (!etiketGirdileri.has(String(x.deger))) etiketGirdileri.set(String(x.deger), h('input', { type: 'text', value: d.degerEtiketleri?.[String(x.deger)] ?? '', placeholder: String(x.deger) }));
          return h(
            'tr',
            {},
            h('td', { class: 'soluk' }, i + 1),
            h('td', {}, String(x.deger)),
            h('td', {}, etiketGirdileri.get(String(x.deger))),
            h('td', { class: 'soluk' }, x.n),
            h(
              'td',
              {},
              h('button', { class: 'dugme hayalet kucuk', disabled: i === 0, title: 'Yukarı', onclick: () => { [dz[i - 1], dz[i]] = [dz[i], dz[i - 1]]; duzeyCiz(); } }, simge('yukari', 14)),
              h('button', { class: 'dugme hayalet kucuk', disabled: i === dz.length - 1, title: 'Aşağı', onclick: () => { [dz[i + 1], dz[i]] = [dz[i], dz[i + 1]]; duzeyCiz(); } }, simge('asagi', 14)),
            ),
          );
        }),
      )),
    );
  };
  if (kategorik) duzeyCiz();
  pencereAc({
    baslik: `Değişken: ${d.ad}`,
    genis: true,
    govde: h(
      'div',
      { class: 'yigin' },
      h('div', { class: 'alan' }, h('label', {}, 'Etiket'), etiket, h('div', { class: 'aciklama' }, 'Etiket girilirse tablolarda değişken adı yerine görünür.')),
      h('div', { class: 'alan' }, h('label', {}, 'Eksik değer kodları'), eksik, h('div', { class: 'aciklama' }, 'Veride eksik anlamında kullanılan kodlar (ör. 99, 999). Boş hücreler zaten eksik sayılır.')),
      kategorik
        ? h('div', { class: 'alan' }, h('span', { class: 'alan-baslik' }, 'Kategoriler'), h('div', { class: 'aciklama' }, 'Kodlu veriler için etiket yazın (1 = Erkek gibi). Sıra, tablolardaki ve sıralı testlerdeki düzeni belirler; regresyonda ilk kategori referans alınır.'), duzeyKap)
        : null,
    ),
    dugmeler: [
      { metin: 'Vazgeç' },
      {
        metin: 'Kaydet',
        ana: true,
        tikla: () => {
          d.etiket = etiket.value.trim();
          const kodlar = eksik.value
            .split(/[;]/)
            .map((s) => s.trim())
            .filter(Boolean)
            .map((s) => (d.veriTuru === 'sayi' && Number.isFinite(Number(s.replace(',', '.'))) ? Number(s.replace(',', '.')) : s));
          d.eksikKodlar = kodlar.length || d.eksikKodlar?.aralik ? { tekil: kodlar, aralik: d.eksikKodlar?.aralik ?? null } : null;
          if (kategorik) {
            const yeni = { ...(d.degerEtiketleri || {}) };
            for (const [k, g] of etiketGirdileri) {
              if (g.value.trim()) yeni[k] = g.value.trim();
              else delete yeni[k];
            }
            d.degerEtiketleri = Object.keys(yeni).length ? yeni : null;
            d.duzeySirasi = dz.map((x) => x.deger);
          }
          degiskenGuncellendi();
        },
      },
    ],
  });
}
