// Bağımlılıksız ZIP yazıcı (Word .docx dosyaları için). Tarayıcı destekliyorsa "deflate-raw" ile sıkıştırır,
// desteklemiyorsa sıkıştırmadan saklar (Word iki biçimi de açar).

const CRC_TABLOSU = (() => {
  const t = new Uint32Array(256);
  for (let n = 0; n < 256; n++) {
    let c = n;
    for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
    t[n] = c >>> 0;
  }
  return t;
})();

export function crc32(veri) {
  let c = 0xffffffff;
  for (let i = 0; i < veri.length; i++) c = CRC_TABLOSU[(c ^ veri[i]) & 0xff] ^ (c >>> 8);
  return (c ^ 0xffffffff) >>> 0;
}

async function sikistir(veri) {
  if (typeof CompressionStream === 'undefined') return null;
  try {
    const akis = new Blob([veri]).stream().pipeThrough(new CompressionStream('deflate-raw'));
    return new Uint8Array(await new Response(akis).arrayBuffer());
  } catch {
    return null;
  }
}

/**
 * @param {{ad:string, veri:Uint8Array|string}[]} dosyalar
 * @returns {Promise<Uint8Array>}
 */
export async function zipOlustur(dosyalar, { sikistirma = true } = {}) {
  const kodlayici = new TextEncoder();
  const parcalar = [];
  const merkez = [];
  let konum = 0;
  const simdi = new Date();
  const dosZaman = ((simdi.getHours() << 11) | (simdi.getMinutes() << 5) | (simdi.getSeconds() >> 1)) & 0xffff;
  const dosTarih = (((simdi.getFullYear() - 1980) << 9) | ((simdi.getMonth() + 1) << 5) | simdi.getDate()) & 0xffff;

  for (const d of dosyalar) {
    const ham = typeof d.veri === 'string' ? kodlayici.encode(d.veri) : d.veri;
    const adBayt = kodlayici.encode(d.ad);
    const crc = crc32(ham);
    let yontem = 0;
    let icerik = ham;
    if (sikistirma) {
      const s = await sikistir(ham);
      if (s && s.length < ham.length) {
        yontem = 8;
        icerik = s;
      }
    }
    const yerel = new DataView(new ArrayBuffer(30));
    yerel.setUint32(0, 0x04034b50, true);
    yerel.setUint16(4, 20, true);
    yerel.setUint16(6, 0x0800, true); // UTF-8 adlar
    yerel.setUint16(8, yontem, true);
    yerel.setUint16(10, dosZaman, true);
    yerel.setUint16(12, dosTarih, true);
    yerel.setUint32(14, crc, true);
    yerel.setUint32(18, icerik.length, true);
    yerel.setUint32(22, ham.length, true);
    yerel.setUint16(26, adBayt.length, true);
    yerel.setUint16(28, 0, true);
    parcalar.push(new Uint8Array(yerel.buffer), adBayt, icerik);

    const m = new DataView(new ArrayBuffer(46));
    m.setUint32(0, 0x02014b50, true);
    m.setUint16(4, 20, true);
    m.setUint16(6, 20, true);
    m.setUint16(8, 0x0800, true);
    m.setUint16(10, yontem, true);
    m.setUint16(12, dosZaman, true);
    m.setUint16(14, dosTarih, true);
    m.setUint32(16, crc, true);
    m.setUint32(20, icerik.length, true);
    m.setUint32(24, ham.length, true);
    m.setUint16(28, adBayt.length, true);
    m.setUint32(42, konum, true);
    merkez.push(new Uint8Array(m.buffer), adBayt);
    konum += 30 + adBayt.length + icerik.length;
  }
  const merkezBoyut = merkez.reduce((a, p) => a + p.length, 0);
  const son = new DataView(new ArrayBuffer(22));
  son.setUint32(0, 0x06054b50, true);
  son.setUint16(8, dosyalar.length, true);
  son.setUint16(10, dosyalar.length, true);
  son.setUint32(12, merkezBoyut, true);
  son.setUint32(16, konum, true);
  const tum = [...parcalar, ...merkez, new Uint8Array(son.buffer)];
  const toplam = tum.reduce((a, p) => a + p.length, 0);
  const cikti = new Uint8Array(toplam);
  let k = 0;
  for (const p of tum) {
    cikti.set(p, k);
    k += p.length;
  }
  return cikti;
}
