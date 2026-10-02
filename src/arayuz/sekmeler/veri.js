// 1. Veri sekmesi: dosya yükleme, yapıştırma, örnek veri setleri ve veri önizleme.

import { durum, sekmeyeGit, veriYukle } from '../durum.js';
import { h, simge } from '../dom.js';
import { bildirim, onayla, pencereAc } from '../pencere.js';
import { filtreMetni, filtrePenceresi } from '../filtre.js';
import { dosyaOku, yapistirilanMetinOku } from '../../veri/index.js';
import { ORNEK_VERILER } from '../../veri/ornekler.js';
import { degerEksikMi } from '../../veri/tanima.js';

const SAYFA_BOYUTU = 100;
let sayfa = 0;

/** Dosyayı okuyup veri seti olarak yükler (sürükle-bırak ve dosya seçiminden çağrılır) */
export async function dosyaYukle(dosya) {
  if (durum.veriSeti && !(await onayla('Yeni veri yüklensin mi?', 'Mevcut veri ve rapordaki analizler kapatılacak. Gerekirse önce projeyi kaydedin.'))) return;
  if (dosya.size > 200 * 1024 * 1024) {
    bildirim('Dosya 200 MB\'tan büyük; tarayıcıda işlenemeyebilir.', { tur: 'hata' });
    return;
  }
  bildirim(`${dosya.name} okunuyor…`, { sure: 1500 });
  try {
    const tampon = await dosya.arrayBuffer();
    const sonuc = await dosyaOku(dosya.name, tampon);
    if (sonuc.sayfalar && sonuc.sayfalar.length > 1 && sonuc.sayfaOku) {
      sayfaSec(sonuc);
      return;
    }
    yukleVeGit(sonuc.veriSeti ?? sonuc, sonuc.sayfaOku ? sonuc : null);
  } catch (e) {
    console.error(e);
    bildirim(`Dosya okunamadı: ${e.message}`, { tur: 'hata', sure: 9000 });
  }
}

function yukleVeGit(veriSeti, calismaKitabi = null) {
  if (!veriSeti || !veriSeti.degiskenler?.length) {
    bildirim('Dosyada okunabilir sütun bulunamadı.', { tur: 'hata' });
    return;
  }
  sayfa = 0;
  veriYukle(veriSeti, { calismaKitabi });
  bildirim(`${veriSeti.satirSayisi} satır, ${veriSeti.degiskenler.length} değişken yüklendi. Değişken türlerini kontrol edin.`, { tur: 'tamam', sure: 5000 });
  sekmeyeGit('degiskenler');
}

function sayfaSec(kitap) {
  let secilen = kitap.sayfalar[0];
  pencereAc({
    baslik: 'Çalışma sayfası seçin',
    govde: [
      h('p', {}, 'Excel dosyasında birden fazla sayfa var. Verinizin bulunduğu sayfayı seçin.'),
      h(
        'div',
        { class: 'yigin' },
        kitap.sayfalar.map((ad, i) =>
          h('label', { class: 'secim-satiri' }, h('input', { type: 'radio', name: 'sayfa', checked: i === 0, onchange: () => { secilen = ad; } }), h('span', { class: 'ad' }, ad)),
        ),
      ),
    ],
    dugmeler: [
      { metin: 'Vazgeç' },
      {
        metin: 'Yükle',
        ana: true,
        tikla: async () => {
          try {
            yukleVeGit(await kitap.sayfaOku(secilen), kitap);
          } catch (e) {
            bildirim(`Sayfa okunamadı: ${e.message}`, { tur: 'hata' });
          }
        },
      },
    ],
  });
}

function yapistirPenceresi() {
  const alan = h('textarea', { placeholder: 'Excel\'de verinizi (başlık satırıyla birlikte) seçip kopyalayın, buraya yapıştırın.', style: 'min-height:16rem' });
  pencereAc({
    baslik: 'Veri yapıştır',
    govde: [h('p', { class: 'soluk' }, 'İlk satır değişken adları olmalı. Sekme, noktalı virgül ya da virgülle ayrılmış metin kabul edilir; Türkçe ondalık virgül otomatik tanınır.'), alan],
    dugmeler: [
      { metin: 'Vazgeç' },
      {
        metin: 'Yükle',
        ana: true,
        tikla: async () => {
          if (!alan.value.trim()) return false;
          if (durum.veriSeti && !(await onayla('Yeni veri yüklensin mi?', 'Mevcut veri ve rapor kapatılacak.'))) return false;
          try {
            const vs = await yapistirilanMetinOku(alan.value);
            vs.ad = vs.ad || 'Yapıştırılan veri';
            yukleVeGit(vs);
          } catch (e) {
            bildirim(`Veri okunamadı: ${e.message}`, { tur: 'hata' });
            return false;
          }
        },
      },
    ],
  });
}

async function ornekYukle(ornek) {
  if (durum.veriSeti && !(await onayla('Örnek veri yüklensin mi?', 'Mevcut veri ve rapor kapatılacak.'))) return;
  const vs = await yapistirilanMetinOku(ornek.csv);
  vs.ad = ornek.ad;
  vs.kaynak = 'ornek';
  yukleVeGit(vs);
}

function dosyaSecDugmesi(metin = 'Dosya seç', ana = true) {
  const girdi = h('input', { type: 'file', accept: '.csv,.tsv,.txt,.xlsx,.xls,.xlsm,.ods,.sav,.zsav', style: 'display:none' });
  girdi.addEventListener('change', () => {
    if (girdi.files[0]) dosyaYukle(girdi.files[0]);
    girdi.value = '';
  });
  return h('span', {}, girdi, h('button', { class: `dugme ${ana ? 'ana buyuk' : ''}`, onclick: () => girdi.click() }, simge('yukle'), metin));
}

function bosEkran(kap) {
  const surukle = h(
    'div',
    { class: 'surukle' },
    h('div', { class: 'buyuk-simge' }, simge('yukle', 44)),
    h('h2', {}, 'Verinizi yükleyin'),
    h('p', { class: 'soluk' }, 'Excel (.xlsx, .xls), CSV/metin ya da SPSS (.sav) dosyanızı buraya sürükleyin.'),
    h('div', { class: 'satir', style: 'justify-content:center;margin-top:1rem' }, dosyaSecDugmesi(), h('button', { class: 'dugme buyuk', onclick: yapistirPenceresi }, simge('kopyala'), 'Excel\'den yapıştır')),
    h('p', { class: 'kucuk soluk', style: 'margin-top:1rem' }, simge('kilit', 13), ' Dosyanız bilgisayarınızda okunur; hiçbir sunucuya yüklenmez. İnternet bağlantısı gerekmez.'),
  );
  surukle.addEventListener('dragenter', () => surukle.classList.add('uzerinde'));
  surukle.addEventListener('dragleave', (e) => {
    if (!surukle.contains(e.relatedTarget)) surukle.classList.remove('uzerinde');
  });
  surukle.addEventListener('drop', () => surukle.classList.remove('uzerinde'));

  kap.append(
    surukle,
    h(
      'div',
      { class: 'izgara iki', style: 'margin-top:1.25rem' },
      h(
        'section',
        { class: 'kart' },
        h('h3', {}, 'Örnek veri setiyle deneyin'),
        h('p', { class: 'soluk kucuk' }, 'Programın nasıl çalıştığını görmek için kurgusal bir veri seti yükleyin.'),
        h(
          'div',
          { class: 'ornek-liste' },
          ORNEK_VERILER.map((o) => h('button', { class: 'ornek', onclick: () => ornekYukle(o) }, h('strong', {}, o.ad), h('span', { class: 'kucuk soluk' }, o.aciklama))),
        ),
      ),
      h(
        'section',
        { class: 'kart' },
        h('h3', {}, 'Verinizi nasıl hazırlamalısınız?'),
        h(
          'ul',
          { class: 'kucuk', style: 'padding-left:1.1rem;margin:0' },
          h('li', {}, h('strong', {}, 'Her satır bir katılımcı'), ' (hasta, denek), her sütun bir değişken olmalı.'),
          h('li', {}, 'İlk satıra ', h('strong', {}, 'değişken adlarını'), ' yazın (ör. Yaş, Cinsiyet, Grup).'),
          h('li', {}, 'Kategorileri metin olarak (Erkek/Kadın) ya da kodla (1/2) girin. Kod kullanırsanız Değişkenler adımında etiket ekleyebilirsiniz.'),
          h('li', {}, 'Önce–sonra ölçümleri ayrı sütunlara yazın (ör. VAS_once, VAS_sonra). Program bunları tanır.'),
          h('li', {}, 'Boş hücreler eksik veri sayılır. "NA", "-" gibi ifadeler de eksik kabul edilir.'),
          h('li', {}, 'Birleştirilmiş hücre, ara toplam satırı ve açıklama satırı kullanmayın.'),
        ),
      ),
    ),
  );
}

function hucreMetni(d, v) {
  if (v === null || v === undefined) return '';
  if (d.veriTuru === 'tarih' && typeof v === 'number') {
    const t = new Date(v);
    return Number.isNaN(t.getTime()) ? String(v) : t.toISOString().slice(0, 10);
  }
  const etiket = d.degerEtiketleri?.[String(v)];
  if (etiket) return `${etiket}`;
  if (typeof v === 'number') return String(Math.round(v * 1e10) / 1e10).replace('.', durum.ayarlar.ondalikAyrac);
  return String(v);
}

function veriTablosu(vs) {
  const toplamSayfa = Math.max(1, Math.ceil(vs.satirSayisi / SAYFA_BOYUTU));
  sayfa = Math.min(sayfa, toplamSayfa - 1);
  const bas = sayfa * SAYFA_BOYUTU;
  const son = Math.min(vs.satirSayisi, bas + SAYFA_BOYUTU);
  const maske = durum.satirMaskesi;
  const tablo = h(
    'table',
    { class: 'veri' },
    h('thead', {}, h('tr', {}, h('th', { class: 'sira' }, '#'), vs.degiskenler.map((d) => h('th', { title: d.etiket || d.ad }, d.ad)))),
    h(
      'tbody',
      {},
      Array.from({ length: son - bas }, (_, k) => {
        const i = bas + k;
        return h(
          'tr',
          { class: maske && !maske[i] ? 'filtre-disi' : null },
          h('td', { class: 'sira' }, i + 1),
          vs.degiskenler.map((d) => {
            const v = d.degerler[i];
            const eksik = degerEksikMi(d, v);
            return h('td', { class: `${eksik ? 'eksik' : ''} ${typeof v === 'number' && !d.degerEtiketleri ? 'sayi' : ''}`.trim() || null }, eksik && (v === null || v === undefined) ? '' : hucreMetni(d, v));
          }),
        );
      }),
    ),
  );
  const gezinti = h(
    'div',
    { class: 'satir', style: 'margin-top:.5rem' },
    h('button', { class: 'dugme kucuk', disabled: sayfa === 0, onclick: () => { sayfa--; yenidenCiz(); } }, '‹ Önceki'),
    h('span', { class: 'kucuk soluk' }, `${bas + 1}–${son} / ${vs.satirSayisi} satır`),
    h('button', { class: 'dugme kucuk', disabled: sayfa >= toplamSayfa - 1, onclick: () => { sayfa++; yenidenCiz(); } }, 'Sonraki ›'),
  );
  return h('div', {}, h('div', { class: 'veri-kap' }, tablo), toplamSayfa > 1 ? gezinti : null);
}

let yenidenCiz = () => {};

export function veriSekmesi(kap) {
  yenidenCiz = () => {
    kap.replaceChildren();
    veriSekmesi(kap);
  };
  const vs = durum.veriSeti;
  if (!vs) {
    bosEkran(kap);
    return;
  }
  const filtre = filtreMetni(durum.filtre);
  const dahil = durum.satirMaskesi ? durum.satirMaskesi.reduce((a, v) => a + v, 0) : vs.satirSayisi;
  kap.append(
    h(
      'section',
      { class: 'kart' },
      h(
        'div',
        { class: 'kart-baslik' },
        simge('veri'),
        h('h2', {}, vs.ad || 'Veri'),
        h('span', { class: 'etiket-kutu' }, `${vs.satirSayisi} vaka × ${vs.degiskenler.length} değişken`),
        vs.kodlama ? h('span', { class: 'etiket-kutu' }, vs.kodlama) : null,
        h('span', { class: 'bosluk' }),
        h('button', { class: 'dugme', onclick: filtrePenceresi }, simge('filtre'), 'Vaka seçimi'),
        dosyaSecDugmesi('Başka veri yükle', false),
        h('button', { class: 'dugme ana', onclick: () => sekmeyeGit('degiskenler') }, 'Değişkenleri kontrol et', simge('ok')),
      ),
      filtre ? h('div', { class: 'bilgi-kutusu uyari', style: 'margin-bottom:.75rem' }, simge('filtre'), h('div', {}, h('strong', {}, 'Filtre etkin: '), `${filtre} — analizlere ${dahil} / ${vs.satirSayisi} vaka giriyor.`)) : null,
      (vs.uyarilar || []).length
        ? h('div', { class: 'bilgi-kutusu uyari', style: 'margin-bottom:.75rem' }, simge('uyari'), h('div', {}, h('strong', {}, 'Okuma sırasında dikkat edilmesi gerekenler'), h('ul', { style: 'margin:.25rem 0 0;padding-left:1.1rem' }, vs.uyarilar.map((u) => h('li', {}, u)))))
        : null,
      veriTablosu(vs),
    ),
  );
}
