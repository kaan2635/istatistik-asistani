// Küçük DOM yardımcıları: h() ile öğe oluşturma, simgeler, güvenli yerel depolama.

/**
 * Öğe oluşturur. props: class, style (nesne ya da metin), on* olay dinleyicileri, diğerleri öznitelik.
 * Çocuklar: düğüm, metin, dizi, null/false (atlanır).
 */
export function h(etiket, props = {}, ...cocuklar) {
  const el = document.createElement(etiket);
  for (const [k, v] of Object.entries(props || {})) {
    if (v === null || v === undefined || v === false) continue;
    if (k === 'class') el.className = v;
    else if (k === 'style' && typeof v === 'object') Object.assign(el.style, v);
    else if (k.startsWith('on') && typeof v === 'function') el.addEventListener(k.slice(2).toLowerCase(), v);
    else if (k === 'html') el.innerHTML = v;
    else if (k === 'value') el.value = v;
    else if (k === 'checked') el.checked = !!v;
    else if (k === 'selected') el.selected = !!v;
    else if (v === true) el.setAttribute(k, '');
    else el.setAttribute(k, v);
  }
  ekle(el, cocuklar);
  return el;
}

function ekle(el, cocuklar) {
  for (const c of cocuklar) {
    if (c === null || c === undefined || c === false) continue;
    if (Array.isArray(c)) ekle(el, c);
    else if (c instanceof Node) el.appendChild(c);
    else el.appendChild(document.createTextNode(String(c)));
  }
}

export function temizle(el) {
  while (el.firstChild) el.removeChild(el.firstChild);
  return el;
}

/** SVG metninden düğüm */
export function svgDugum(metin) {
  const kap = document.createElement('div');
  kap.innerHTML = metin;
  return kap.firstElementChild;
}

// Basit çizgi simgeler (24×24, currentColor)
const YOLLAR = {
  veri: '<path d="M4 6c0-1.7 3.6-3 8-3s8 1.3 8 3-3.6 3-8 3-8-1.3-8-3Z"/><path d="M4 6v6c0 1.7 3.6 3 8 3s8-1.3 8-3V6"/><path d="M4 12v6c0 1.7 3.6 3 8 3s8-1.3 8-3v-6"/>',
  degisken: '<path d="M4 6h16M4 12h16M4 18h10"/>',
  analiz: '<path d="M4 20V10M10 20V4M16 20v-8M22 20H2"/>',
  rapor: '<path d="M6 3h9l5 5v13H6z"/><path d="M14 3v6h6M9 13h8M9 17h6"/>',
  guc: '<path d="M12 3v3M12 18v3M3 12h3M18 12h3"/><circle cx="12" cy="12" r="5"/>',
  yardim: '<circle cx="12" cy="12" r="9"/><path d="M9.5 9a2.5 2.5 0 1 1 3.5 2.3c-.6.3-1 .9-1 1.6V14M12 17.5v.01"/>',
  ayar: '<circle cx="12" cy="12" r="3"/><path d="M19.4 15a1.7 1.7 0 0 0 .3 1.8l.1.1a2 2 0 1 1-2.8 2.8l-.1-.1a1.7 1.7 0 0 0-1.8-.3 1.7 1.7 0 0 0-1 1.5V21a2 2 0 1 1-4 0v-.1a1.7 1.7 0 0 0-1.1-1.5 1.7 1.7 0 0 0-1.8.3l-.1.1a2 2 0 1 1-2.8-2.8l.1-.1a1.7 1.7 0 0 0 .3-1.8 1.7 1.7 0 0 0-1.5-1H3a2 2 0 1 1 0-4h.1a1.7 1.7 0 0 0 1.5-1.1 1.7 1.7 0 0 0-.3-1.8l-.1-.1a2 2 0 1 1 2.8-2.8l.1.1a1.7 1.7 0 0 0 1.8.3H9a1.7 1.7 0 0 0 1-1.5V3a2 2 0 1 1 4 0v.1a1.7 1.7 0 0 0 1 1.5 1.7 1.7 0 0 0 1.8-.3l.1-.1a2 2 0 1 1 2.8 2.8l-.1.1a1.7 1.7 0 0 0-.3 1.8V9a1.7 1.7 0 0 0 1.5 1H21a2 2 0 1 1 0 4h-.1a1.7 1.7 0 0 0-1.5 1Z"/>',
  kaydet: '<path d="M5 3h11l3 3v15H5z"/><path d="M8 3v6h8V3M8 21v-7h8v7"/>',
  ac: '<path d="M3 7V5a2 2 0 0 1 2-2h4l2 3h8a2 2 0 0 1 2 2v1"/><path d="M3 7h18l-2 13H5z"/>',
  yukle: '<path d="M12 16V4M7 9l5-5 5 5"/><path d="M4 16v4h16v-4"/>',
  indir: '<path d="M12 4v12M7 11l5 5 5-5"/><path d="M4 20h16"/>',
  kopyala: '<rect x="8" y="8" width="12" height="12" rx="2"/><path d="M16 8V5a1 1 0 0 0-1-1H5a1 1 0 0 0-1 1v10a1 1 0 0 0 1 1h3"/>',
  sil: '<path d="M4 7h16M10 11v6M14 11v6M6 7l1 13h10l1-13M9 7V4h6v3"/>',
  yukari: '<path d="M12 19V5M5 12l7-7 7 7"/>',
  asagi: '<path d="M12 5v14M5 12l7 7 7-7"/>',
  arti: '<path d="M12 5v14M5 12h14"/>',
  kapat: '<path d="M6 6l12 12M18 6 6 18"/>',
  kilit: '<rect x="5" y="11" width="14" height="10" rx="2"/><path d="M8 11V7a4 4 0 1 1 8 0v4"/>',
  sihir: '<path d="M15 4V2M15 16v-2M8 9h2M20 9h2M17.8 11.8 19 13M17.8 6.2 19 5M12.2 6.2 11 5M3 21l9-9"/>',
  tamam: '<path d="M5 12l5 5 9-10"/>',
  uyari: '<path d="M12 3 2 21h20z"/><path d="M12 10v5M12 18v.01"/>',
  bilgi: '<circle cx="12" cy="12" r="9"/><path d="M12 11v6M12 7.5v.01"/>',
  tablo: '<rect x="3" y="4" width="18" height="16" rx="2"/><path d="M3 10h18M9 10v10"/>',
  grafik: '<path d="M3 3v18h18"/><path d="M7 15l4-4 3 3 5-6"/>',
  filtre: '<path d="M3 5h18l-7 8v6l-4 2v-8z"/>',
  yazdir: '<path d="M6 9V3h12v6"/><rect x="3" y="9" width="18" height="8" rx="2"/><path d="M6 14h12v7H6z"/>',
  word: '<path d="M6 3h9l5 5v13H6z"/><path d="M8.5 11l1.5 6 2-5 2 5 1.5-6"/>',
  yenile: '<path d="M20 11a8 8 0 1 0-2.3 5.7"/><path d="M20 4v7h-7"/>',
  ok: '<path d="M9 6l6 6-6 6"/>',
};

export function simge(ad, boyut = 18) {
  const span = document.createElement('span');
  span.className = 'simge';
  span.innerHTML = `<svg width="${boyut}" height="${boyut}" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">${YOLLAR[ad] || ''}</svg>`;
  return span;
}

/** Yerel depolama: özel pencerede ya da engelliyse sessizce başarısız olur */
export const depo = {
  oku(anahtar, varsayilan = null) {
    try {
      const v = localStorage.getItem(anahtar);
      return v === null ? varsayilan : JSON.parse(v);
    } catch {
      return varsayilan;
    }
  },
  yaz(anahtar, deger) {
    try {
      localStorage.setItem(anahtar, JSON.stringify(deger));
    } catch {
      /* yok say */
    }
  },
};

/** Dosya indirme */
export function dosyaIndir(icerik, dosyaAdi, tur = 'application/octet-stream') {
  const blob = icerik instanceof Blob ? icerik : new Blob([icerik], { type: tur });
  const url = URL.createObjectURL(blob);
  const a = h('a', { href: url, download: dosyaAdi, style: 'display:none' });
  document.body.appendChild(a);
  a.click();
  setTimeout(() => {
    URL.revokeObjectURL(url);
    a.remove();
  }, 2000);
}

/** Panoya HTML + düz metin kopyalama (Word'e tablo yapıştırmak için) */
export async function panoyaKopyala(html, metin) {
  try {
    if (navigator.clipboard && window.ClipboardItem) {
      await navigator.clipboard.write([
        new ClipboardItem({
          'text/html': new Blob([html], { type: 'text/html' }),
          'text/plain': new Blob([metin], { type: 'text/plain' }),
        }),
      ]);
      return true;
    }
  } catch {
    /* aşağıdaki yönteme düş */
  }
  // Eski yöntem: seçili bir öğeden kopyalama
  const kap = h('div', { contenteditable: 'true', style: 'position:fixed;left:-9999px;top:0', html });
  document.body.appendChild(kap);
  const aralik = document.createRange();
  aralik.selectNodeContents(kap);
  const secim = window.getSelection();
  secim.removeAllRanges();
  secim.addRange(aralik);
  let basarili = false;
  try {
    basarili = document.execCommand('copy');
  } catch {
    basarili = false;
  }
  secim.removeAllRanges();
  kap.remove();
  return basarili;
}

/** Benzersiz kimlik */
export function kimlik() {
  return Math.random().toString(36).slice(2, 10) + Date.now().toString(36).slice(-4);
}
