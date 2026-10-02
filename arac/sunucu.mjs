// Geliştirme sunucusu: her sayfa isteğinde uygulamayı yeniden derler ve sunar.
//   npm run dev   → http://localhost:5173

import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { derle } from './derle.mjs';

const KOK = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const PORT = Number(process.env.PORT) || 5173;
const GECICI = path.join(KOK, 'cikti', 'gelistirme.html');
fs.mkdirSync(path.dirname(GECICI), { recursive: true });

const TURLER = { '.html': 'text/html; charset=utf-8', '.csv': 'text/csv; charset=utf-8', '.sav': 'application/octet-stream', '.xlsx': 'application/octet-stream', '.json': 'application/json' };

http
  .createServer(async (istek, yanit) => {
    try {
      const url = new URL(istek.url, 'http://localhost');
      if (url.pathname === '/' || url.pathname === '/index.html') {
        await derle({ gelistir: true, cikti: GECICI });
        yanit.writeHead(200, { 'Content-Type': TURLER['.html'], 'Cache-Control': 'no-store' });
        yanit.end(fs.readFileSync(GECICI));
        return;
      }
      // Üretim derlemesini (kökteki index.html) olduğu gibi sunar
      if (url.pathname === '/uretim.html') {
        yanit.writeHead(200, { 'Content-Type': TURLER['.html'], 'Cache-Control': 'no-store' });
        yanit.end(fs.readFileSync(path.join(KOK, 'index.html')));
        return;
      }
      // test ve örnek veri dosyaları (tarayıcı testleri için)
      const yol = path.normalize(path.join(KOK, decodeURIComponent(url.pathname)));
      if (yol.startsWith(path.join(KOK, 'ornek-veriler')) || yol.startsWith(path.join(KOK, 'test', 'veri'))) {
        if (fs.existsSync(yol) && fs.statSync(yol).isFile()) {
          yanit.writeHead(200, { 'Content-Type': TURLER[path.extname(yol)] || 'application/octet-stream' });
          yanit.end(fs.readFileSync(yol));
          return;
        }
      }
      yanit.writeHead(404);
      yanit.end('Bulunamadı');
    } catch (e) {
      yanit.writeHead(500, { 'Content-Type': 'text/plain; charset=utf-8' });
      yanit.end(String(e && e.stack ? e.stack : e));
    }
  })
  .listen(PORT, () => console.log(`Geliştirme sunucusu: http://localhost:${PORT}`));
