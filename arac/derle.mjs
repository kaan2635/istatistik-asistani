// Uygulamayı tek bir HTML dosyasına derler (index.html). Dosya internet bağlantısı olmadan, çift tıklanarak da çalışır.
//   node arac/derle.mjs            → küçültülmüş üretim derlemesi
//   node arac/derle.mjs --gelistir → okunabilir derleme (hata ayıklama için)

import * as esbuild from 'esbuild';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const KOK = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');

export async function derle({ gelistir = false, cikti = path.join(KOK, 'index.html') } = {}) {
  const paket = JSON.parse(fs.readFileSync(path.join(KOK, 'package.json'), 'utf8'));
  const sonuc = await esbuild.build({
    entryPoints: [path.join(KOK, 'src', 'main.js')],
    bundle: true,
    format: 'iife',
    target: ['es2020'],
    minify: !gelistir,
    write: false,
    charset: 'utf8',
    legalComments: 'none',
    logLevel: 'silent',
    define: {
      __SURUM__: JSON.stringify(paket.version),
      __DERLEME_TARIHI__: JSON.stringify(new Date().toISOString().slice(0, 10)),
    },
  });
  // Satır içi betikte "</script" dizisi HTML ayrıştırıcısını erken kapatır; JS'de eşdeğeri olan "<\/script" yazılır.
  const js = sonuc.outputFiles[0].text.replace(/<\/script/gi, '<\\/script');
  const css = fs.readFileSync(path.join(KOK, 'src', 'stil.css'), 'utf8');
  const sablon = fs.readFileSync(path.join(KOK, 'src', 'index.html'), 'utf8');
  const html = sablon
    .replace('/*STIL*/', () => css)
    .replace('/*BETIK*/', () => js)
    .replace(/__SURUM__/g, () => paket.version);
  fs.writeFileSync(cikti, html);
  return { boyut: Buffer.byteLength(html), cikti };
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const gelistir = process.argv.includes('--gelistir');
  const { boyut, cikti } = await derle({ gelistir });
  console.log(`${path.relative(KOK, cikti)} yazıldı (${(boyut / 1024).toFixed(0)} KB)`);
}
