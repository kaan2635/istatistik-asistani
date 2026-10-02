// R referans değerleri üretici. R'nin WebAssembly sürümünü (webR) Node içinde çalıştırır;
// arac/r-referans/*.R betiklerinin her biri `sonuc` adlı bir liste tanımlar, bu liste
// test/referans/<ad>.json olarak kaydedilir. Testler bu dosyalarla karşılaştırma yapar.
//
// Kullanım:  npm run r-referans            (tüm betikler)
//            npm run r-referans -- dagilim  (yalnızca dagilim.R)
//
// Betiğin ilk satırlarında "# paketler: survival, pROC" yazılırsa bu paketler kurulur.
// Betik /cikti klasörüne dosya yazarsa bunlar test/veri altına kopyalanır.
// test/veri içindeki dosyalar R tarafında /veri klasöründe okunabilir.

const fs = require('node:fs');
const path = require('node:path');

const KOK = path.resolve(__dirname, '..', '..');
const BETIK_KLASORU = __dirname;
const REFERANS_KLASORU = path.join(KOK, 'test', 'referans');
const VERI_KLASORU = path.join(KOK, 'test', 'veri');

// webR, Windows'ta mutlak yolu import() ile yüklemeye çalışıyor; file:// URL'sine çevir.
function webrYamala() {
  const dosya = path.join(KOK, 'node_modules', 'webr', 'dist', 'webr-worker.js');
  const metin = fs.readFileSync(dosya, 'utf8');
  const eski = 'await import((await import("path")).default.resolve(e))';
  const yeni = 'await import((await import("url")).pathToFileURL((await import("path")).default.resolve(e)).href)';
  if (metin.includes(eski)) fs.writeFileSync(dosya, metin.split(eski).join(yeni));
}

async function main() {
  webrYamala();
  const { WebR } = require('webr');
  const secilen = process.argv.slice(2);
  const betikler = fs
    .readdirSync(BETIK_KLASORU)
    .filter((f) => f.endsWith('.R'))
    .filter((f) => secilen.length === 0 || secilen.includes(path.basename(f, '.R')));
  if (betikler.length === 0) {
    console.error('Betik bulunamadı.');
    process.exit(1);
  }
  const webR = new WebR({ interactive: false });
  await webR.init();
  const paketler = new Set(['jsonlite']);
  for (const b of betikler) {
    const ilk = fs.readFileSync(path.join(BETIK_KLASORU, b), 'utf8').split('\n').slice(0, 5).join('\n');
    const m = ilk.match(/#\s*paketler:\s*(.+)/);
    if (m) m[1].split(',').map((s) => s.trim()).filter(Boolean).forEach((p) => paketler.add(p));
  }
  console.log('Paketler kuruluyor:', [...paketler].join(', '));
  await webR.installPackages([...paketler], { quiet: true });

  await webR.FS.mkdir('/veri');
  await webR.FS.mkdir('/cikti');
  fs.mkdirSync(VERI_KLASORU, { recursive: true });
  fs.mkdirSync(REFERANS_KLASORU, { recursive: true });
  for (const f of fs.readdirSync(VERI_KLASORU)) {
    const tam = path.join(VERI_KLASORU, f);
    if (fs.statSync(tam).isFile()) await webR.FS.writeFile(`/veri/${f}`, new Uint8Array(fs.readFileSync(tam)));
  }

  for (const b of betikler) {
    const ad = path.basename(b, '.R');
    const kod = fs.readFileSync(path.join(BETIK_KLASORU, b), 'utf8');
    process.stdout.write(`${ad} ... `);
    const baslangic = Date.now();
    const sarmal = `local({
      ${kod}
      jsonlite::toJSON(sonuc, digits = NA, auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE)
    })`;
    try {
      const json = await webR.evalRString(sarmal);
      fs.writeFileSync(path.join(REFERANS_KLASORU, `${ad}.json`), JSON.stringify(JSON.parse(json), null, 1) + '\n');
      console.log(`tamam (${((Date.now() - baslangic) / 1000).toFixed(1)} sn)`);
    } catch (e) {
      console.log('HATA');
      console.error(String(e && e.message ? e.message : e).slice(0, 2000));
      process.exitCode = 1;
    }
  }
  // /cikti altına yazılan dosyaları test/veri'ye kopyala
  const ciktilar = await webR.FS.lookupPath('/cikti');
  for (const ad of Object.keys(ciktilar.contents || {})) {
    const veri = await webR.FS.readFile(`/cikti/${ad}`);
    fs.writeFileSync(path.join(VERI_KLASORU, ad), Buffer.from(veri));
    console.log('  veri:', ad);
  }
  await webR.close();
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
