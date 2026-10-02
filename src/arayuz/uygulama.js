// Uygulama kabuğu: üst çubuk, sekmeler ve sekme içeriklerinin çizimi.

import { abone, durum, projeyiSerilestir, projeyiYukle, sekmeyeGit } from './durum.js';
import { dosyaIndir, h, simge, temizle } from './dom.js';
import { bildirim, onayla } from './pencere.js';
import { ayarlarPenceresi } from './ayarlar.js';
import { veriSekmesi, dosyaYukle } from './sekmeler/veri.js';
import { degiskenlerSekmesi } from './sekmeler/degiskenler.js';
import { analizSekmesi } from './sekmeler/analiz.js';
import { raporSekmesi } from './sekmeler/rapor.js';
import { gucSekmesi } from './sekmeler/guc.js';
import { yardimSekmesi } from './sekmeler/yardim.js';
import { filtreMaskesi } from './filtre.js';

const SEKMELER = [
  { id: 'veri', ad: 'Veri', simge: 'veri', numara: 1, cizim: veriSekmesi },
  { id: 'degiskenler', ad: 'Değişkenler', simge: 'degisken', numara: 2, cizim: degiskenlerSekmesi, veriGerekir: true },
  { id: 'analiz', ad: 'Analiz', simge: 'analiz', numara: 3, cizim: analizSekmesi, veriGerekir: true },
  { id: 'rapor', ad: 'Rapor', simge: 'rapor', numara: 4, cizim: raporSekmesi },
  { id: 'guc', ad: 'Güç analizi', simge: 'guc', cizim: gucSekmesi },
  { id: 'yardim', ad: 'Yardım', simge: 'yardim', cizim: yardimSekmesi },
];

let icerik;
let sekmeCubugu;

export function baslat(kok) {
  temizle(kok);
  sekmeCubugu = h('nav', { class: 'sekmeler', 'aria-label': 'Bölümler' });
  const ust = h(
    'header',
    { class: 'ust' },
    h('div', { class: 'marka' }, h('span', { class: 'logo' }, simge('analiz', 18)), 'İstatistik Asistanı', h('span', { class: 'rozet', title: 'Tüm hesaplar tarayıcınızda yapılır; veriler hiçbir sunucuya gönderilmez.' }, simge('kilit', 13), 'Veriler bilgisayarınızda kalır')),
    sekmeCubugu,
    h(
      'div',
      { class: 'ust-eylemler' },
      h('button', { class: 'dugme hayalet', title: 'Projeyi kaydet (veri, ayarlar ve analizler)', onclick: projeKaydet }, simge('kaydet'), h('span', { class: 'etiket' }, 'Kaydet')),
      h('button', { class: 'dugme hayalet', title: 'Kayıtlı projeyi ya da yeni veri dosyasını aç', onclick: projeAc }, simge('ac'), h('span', { class: 'etiket' }, 'Aç')),
      h('button', { class: 'dugme hayalet simge-dugme', title: 'Ayarlar', 'aria-label': 'Ayarlar', onclick: ayarlarPenceresi }, simge('ayar')),
    ),
  );
  icerik = h('main', { id: 'icerik' });
  kok.append(ust, icerik, h('footer', { class: 'alt-bilgi' }, `İstatistik Asistanı ${typeof __SURUM__ !== 'undefined' ? __SURUM__ : ''} · Açık kaynak · Hesaplar R ile doğrulanmıştır · Sonuçları yorumlarken bir istatistikçiye danışmanız önerilir.`));

  abone((neler) => {
    sekmeCubuguCiz();
    if (neler.has('sekme') || neler.has('veri') || neler.has('degisken') || neler.has('ayar') || neler.has('rapor') || neler.has('filtre')) icerikCiz(neler);
  });
  sekmeCubuguCiz();
  icerikCiz(new Set(['sekme']));

  // Pencereye bırakılan dosyalar
  window.addEventListener('dragover', (e) => {
    if (e.dataTransfer && [...e.dataTransfer.types].includes('Files')) e.preventDefault();
  });
  window.addEventListener('drop', (e) => {
    if (!e.dataTransfer?.files?.length) return;
    e.preventDefault();
    dosyaYukle(e.dataTransfer.files[0]);
  });
  window.addEventListener('beforeunload', (e) => {
    if (durum.veriSeti) {
      e.preventDefault();
      e.returnValue = '';
    }
  });
}

function sekmeCubuguCiz() {
  temizle(sekmeCubugu);
  for (const s of SEKMELER) {
    const devreDisi = s.veriGerekir && !durum.veriSeti;
    sekmeCubugu.appendChild(
      h(
        'button',
        {
          class: 'sekme',
          'aria-current': durum.aktifSekme === s.id ? 'page' : null,
          disabled: devreDisi,
          title: devreDisi ? 'Önce veri yükleyin' : s.ad,
          onclick: () => sekmeyeGit(s.id),
        },
        s.numara ? h('span', { class: 'numara' }, s.numara) : simge(s.simge, 16),
        h('span', { class: 'etiket' }, s.ad),
        s.id === 'rapor' && durum.istekler.length ? h('span', { class: 'sayac' }, durum.istekler.length) : null,
      ),
    );
  }
}

let sonCizilen = null;

function icerikCiz(neler) {
  const s = SEKMELER.find((x) => x.id === durum.aktifSekme) || SEKMELER[0];
  // Aynı sekmede yalnızca ilgili değişiklik olduysa yeniden çiz
  if (sonCizilen === s.id && !neler.has('sekme') && s.id === 'guc') return;
  sonCizilen = s.id;
  const kaydirma = neler.has('sekme') ? 0 : window.scrollY;
  temizle(icerik);
  try {
    s.cizim(icerik, neler);
  } catch (e) {
    console.error(e);
    icerik.appendChild(h('div', { class: 'bilgi-kutusu hata' }, simge('uyari'), h('div', {}, h('strong', {}, 'Beklenmeyen hata: '), String(e.message || e))));
  }
  if (!neler.has('sekme')) window.scrollTo(0, kaydirma);
  else window.scrollTo(0, 0);
}

function projeKaydet() {
  if (!durum.veriSeti) {
    bildirim('Kaydedilecek veri yok.');
    return;
  }
  const ad = (durum.veriSeti.ad || 'proje').replace(/\.[^.]+$/, '');
  dosyaIndir(projeyiSerilestir(), `${ad}.istatistik.json`, 'application/json');
  bildirim('Proje kaydedildi. Dosya hasta verisi içeriyorsa güvenli bir yerde saklayın.', { tur: 'tamam' });
}

function projeAc() {
  const girdi = h('input', { type: 'file', accept: '.json,.csv,.tsv,.txt,.xlsx,.xls,.ods,.sav,.zsav', style: 'display:none' });
  girdi.addEventListener('change', async () => {
    const dosya = girdi.files[0];
    girdi.remove();
    if (!dosya) return;
    if (/\.json$/i.test(dosya.name)) {
      if (durum.veriSeti && !(await onayla('Proje açılsın mı?', 'Mevcut veri ve rapor kapatılacak.'))) return;
      try {
        projeyiYukle(await dosya.text(), filtreMaskesi);
        sekmeyeGit(durum.istekler.length ? 'rapor' : 'degiskenler');
        bildirim('Proje açıldı.', { tur: 'tamam' });
      } catch (e) {
        bildirim(`Proje açılamadı: ${e.message}`, { tur: 'hata', sure: 7000 });
      }
    } else {
      dosyaYukle(dosya);
    }
  });
  document.body.appendChild(girdi);
  girdi.click();
}
