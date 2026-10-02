// Grafik dışa aktarımı (tarayıcı): SVG indirme, SVG → PNG (canvas) dönüştürme ve indirme.
//
// İçe aktarıldığında hiçbir tarayıcı API'si çağrılmaz; DOM'a yalnızca fonksiyonlar çalıştığında dokunulur.
// Bu yüzden modül Node'da da yüklenebilir ve saf yardımcılar (svgBoyutlari, pngDpiAyarla) test edilebilir.
//
// PNG'lere pHYs öbeği yazılır: Word ve diğer programlar görüntüyü fiziksel boyutuna göre yerleştirir.
// Örn. 640 px genişlikli grafik ölçek 3,125 ile 2000 px olur ve 300 DPI etiketiyle Word'de 640/96 inç
// (≈ 16,9 cm) görünür; ekrandaki boyutla aynı, baskıda keskin.

/** Ekran çözünürlüğü: SVG'nin 1 kullanıcı birimi = 1 CSS pikseli = 1/96 inç */
export const EKRAN_DPI = 96;

/** 300 DPI'a karşılık gelen ölçek */
export const BASKI_OLCEGI = 300 / EKRAN_DPI;

/**
 * SVG metninden genişlik ve yükseklik (kök öğenin width/height nitelikleri, yoksa viewBox).
 * @param {string} svg
 * @returns {{genislik:number, yukseklik:number}}
 */
export function svgBoyutlari(svg) {
  const kok = /<svg\b[^>]*>/i.exec(String(svg));
  if (!kok) throw new Error('Geçerli bir SVG değil.');
  const nitelik = (ad) => {
    const m = new RegExp(`\\s${ad}\\s*=\\s*"([^"]*)"`, 'i').exec(kok[0]);
    return m ? m[1] : null;
  };
  let gen = parseFloat(nitelik('width'));
  let yuk = parseFloat(nitelik('height'));
  if (!(gen > 0) || !(yuk > 0)) {
    const vb = (nitelik('viewBox') || '').trim().split(/[\s,]+/).map(Number);
    if (vb.length === 4 && vb[2] > 0 && vb[3] > 0) {
      gen = vb[2];
      yuk = vb[3];
    }
  }
  if (!(gen > 0) || !(yuk > 0)) throw new Error('SVG boyutları okunamadı.');
  return { genislik: gen, yukseklik: yuk };
}

let crcTablosu = null;
function crc32(baytlar, bas = 0, son = baytlar.length) {
  if (!crcTablosu) {
    crcTablosu = new Uint32Array(256);
    for (let n = 0; n < 256; n++) {
      let c = n;
      for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
      crcTablosu[n] = c >>> 0;
    }
  }
  let c = 0xffffffff;
  for (let i = bas; i < son; i++) c = crcTablosu[(c ^ baytlar[i]) & 0xff] ^ (c >>> 8);
  return (c ^ 0xffffffff) >>> 0;
}

const PNG_IMZA = [137, 80, 78, 71, 13, 10, 26, 10];

/**
 * PNG verisine çözünürlük (pHYs öbeği, metre başına piksel) yazar; varsa eskisini değiştirir. Saf fonksiyon.
 * @param {Uint8Array} png
 * @param {number} dpi
 * @returns {Uint8Array} yeni PNG
 */
export function pngDpiAyarla(png, dpi) {
  const b = png instanceof Uint8Array ? png : new Uint8Array(png);
  if (b.length < 33 || PNG_IMZA.some((v, i) => b[i] !== v)) throw new Error('Geçerli bir PNG değil.');
  const okuU32 = (i) => ((b[i] << 24) | (b[i + 1] << 16) | (b[i + 2] << 8) | b[i + 3]) >>> 0;
  // Öbekleri dolaş: IHDR'den sonra eklenecek, var olan pHYs atlanacak
  const parcalar = [b.subarray(0, 8)];
  let i = 8;
  let ihdrSonu = -1;
  while (i + 8 <= b.length) {
    const uzunluk = okuU32(i);
    const tur = String.fromCharCode(b[i + 4], b[i + 5], b[i + 6], b[i + 7]);
    const son = i + 12 + uzunluk;
    if (son > b.length) throw new Error('PNG öbeği bozuk.');
    if (tur !== 'pHYs') parcalar.push(b.subarray(i, son));
    if (tur === 'IHDR') ihdrSonu = parcalar.length;
    i = son;
    if (tur === 'IEND') break;
  }
  if (ihdrSonu < 0) throw new Error('PNG IHDR öbeği yok.');
  const ppm = Math.round(dpi / 0.0254);
  const phys = new Uint8Array(21);
  const yaz32 = (dizi, konum, v) => {
    dizi[konum] = (v >>> 24) & 255;
    dizi[konum + 1] = (v >>> 16) & 255;
    dizi[konum + 2] = (v >>> 8) & 255;
    dizi[konum + 3] = v & 255;
  };
  yaz32(phys, 0, 9);
  phys.set([112, 72, 89, 115], 4); // "pHYs"
  yaz32(phys, 8, ppm);
  yaz32(phys, 12, ppm);
  phys[16] = 1; // birim: metre
  yaz32(phys, 17, crc32(phys, 4, 17));
  parcalar.splice(ihdrSonu, 0, phys);
  const toplam = parcalar.reduce((t, p) => t + p.length, 0);
  const sonuc = new Uint8Array(toplam);
  let k = 0;
  for (const p of parcalar) {
    sonuc.set(p, k);
    k += p.length;
  }
  return sonuc;
}

function tarayiciGerekli() {
  if (typeof document === 'undefined' || typeof Image === 'undefined') {
    throw new Error('Bu işlem yalnızca tarayıcıda çalışır.');
  }
}

/** SVG metnini veri URL'sine çevirir (img src olarak kullanılabilir) */
export function svgDataUrl(svg) {
  return 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(String(svg));
}

function resimYukle(kaynak) {
  return new Promise((coz, reddet) => {
    const img = new Image();
    img.onload = () => coz(img);
    img.onerror = () => reddet(new Error('SVG görüntüye dönüştürülemedi.'));
    img.src = kaynak;
  });
}

/**
 * SVG → PNG Blob (canvas ile). Arka plan beyazdır; PNG'ye ölçeğe karşılık gelen DPI yazılır.
 * @param {string} svg
 * @param {number} [olcek=300/96] Piksel çarpanı (3,125 ≈ 300 DPI)
 * @param {{arkaPlan?: string}} [secenek]
 * @returns {Promise<Blob>}
 */
export async function svgdenPngBlob(svg, olcek = BASKI_OLCEGI, secenek = {}) {
  tarayiciGerekli();
  const { genislik, yukseklik } = svgBoyutlari(svg);
  const k = Number.isFinite(olcek) && olcek > 0 ? olcek : BASKI_OLCEGI;
  const img = await resimYukle(svgDataUrl(svg));
  const tuval = document.createElement('canvas');
  tuval.width = Math.max(1, Math.round(genislik * k));
  tuval.height = Math.max(1, Math.round(yukseklik * k));
  const ctx = tuval.getContext('2d');
  ctx.fillStyle = secenek.arkaPlan || '#ffffff';
  ctx.fillRect(0, 0, tuval.width, tuval.height);
  ctx.imageSmoothingEnabled = true;
  ctx.imageSmoothingQuality = 'high';
  ctx.drawImage(img, 0, 0, tuval.width, tuval.height);
  const hamBlob = await new Promise((coz, reddet) => tuval.toBlob((b) => (b ? coz(b) : reddet(new Error('PNG oluşturulamadı.'))), 'image/png'));
  const baytlar = new Uint8Array(await hamBlob.arrayBuffer());
  return new Blob([pngDpiAyarla(baytlar, EKRAN_DPI * k)], { type: 'image/png' });
}

/**
 * SVG → PNG veri URL'si ("data:image/png;base64,…").
 * @param {string} svg
 * @param {number} [olcek=300/96]
 * @returns {Promise<string>}
 */
export async function svgdenPngDataUrl(svg, olcek = BASKI_OLCEGI) {
  const blob = await svgdenPngBlob(svg, olcek);
  const baytlar = new Uint8Array(await blob.arrayBuffer());
  let ikili = '';
  const parca = 0x8000;
  for (let i = 0; i < baytlar.length; i += parca) ikili += String.fromCharCode.apply(null, baytlar.subarray(i, i + parca));
  return 'data:image/png;base64,' + btoa(ikili);
}

function dosyaAdi(ad, uzanti) {
  const temiz = String(ad || 'grafik')
    .replace(/[\\/:*?"<>|]+/g, '_')
    .trim() || 'grafik';
  return temiz.toLowerCase().endsWith(`.${uzanti}`) ? temiz : `${temiz}.${uzanti}`;
}

function blobIndir(blob, ad) {
  tarayiciGerekli();
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = ad;
  a.rel = 'noopener';
  a.style.display = 'none';
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 2000);
}

/**
 * SVG dosyası olarak indirir (UTF-8, XML bildirimiyle).
 * @param {string} svg
 * @param {string} [dosya='grafik.svg']
 */
export function svgIndir(svg, dosya = 'grafik.svg') {
  const metin = String(svg).startsWith('<?xml') ? String(svg) : `<?xml version="1.0" encoding="UTF-8"?>\n${svg}`;
  blobIndir(new Blob([metin], { type: 'image/svg+xml;charset=utf-8' }), dosyaAdi(dosya, 'svg'));
}

/**
 * PNG olarak indirir.
 * @param {string} svg
 * @param {string} [dosya='grafik.png']
 * @param {{olcek?: number}} [secenek] olcek varsayılanı 3 (≈ 288 DPI)
 * @returns {Promise<void>}
 */
export async function pngIndir(svg, dosya = 'grafik.png', secenek = {}) {
  const blob = await svgdenPngBlob(svg, secenek.olcek ?? 3);
  blobIndir(blob, dosyaAdi(dosya, 'png'));
}
