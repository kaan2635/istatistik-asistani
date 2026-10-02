// Test yardımcıları: R referanslarını yükleme ve sayısal yakınlık karşılaştırması.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const KLASOR = path.dirname(fileURLToPath(import.meta.url));

export function referans(ad) {
  return JSON.parse(fs.readFileSync(path.join(KLASOR, 'referans', `${ad}.json`), 'utf8'));
}

export function veriDosyasi(ad) {
  return fs.readFileSync(path.join(KLASOR, 'veri', ad), 'utf8');
}

/** R'nin JSON'a null olarak yazdığı Inf değerlerini geri çevirir */
export function inf(x) {
  return x === null ? Infinity : x;
}

/**
 * gercek ile beklenen arasındaki fark, göreli ya da mutlak toleransın içinde mi?
 * Her iki tolerans da aşılırsa hata verir.
 */
export function yakin(gercek, beklenen, goreli = 1e-9, mutlak = 1e-14, mesaj = '') {
  if (beklenen === null || beklenen === undefined) {
    assert.ok(gercek === null || gercek === undefined || Number.isNaN(gercek), `${mesaj}: beklenen boş, gelen ${gercek}`);
    return;
  }
  if (typeof beklenen === 'string') {
    assert.equal(gercek, beklenen, mesaj);
    return;
  }
  if (!Number.isFinite(beklenen)) {
    assert.equal(gercek, beklenen, mesaj);
    return;
  }
  const fark = Math.abs(gercek - beklenen);
  const tamam = fark <= mutlak || fark <= goreli * Math.abs(beklenen);
  if (!tamam) {
    assert.fail(`${mesaj}: gelen ${gercek}, beklenen ${beklenen} (göreli fark ${(fark / Math.abs(beklenen)).toExponential(2)})`);
  }
}

/** Dizi/nesne karşılaştırması: sayıları yakin() ile, gerisini eşitlikle */
export function yakinDerin(gercek, beklenen, goreli = 1e-9, mutlak = 1e-14, mesaj = '') {
  if (Array.isArray(beklenen)) {
    assert.ok(Array.isArray(gercek) || ArrayBuffer.isView(gercek), `${mesaj}: dizi bekleniyordu`);
    assert.equal(gercek.length, beklenen.length, `${mesaj}: uzunluk`);
    beklenen.forEach((b, i) => yakinDerin(gercek[i], b, goreli, mutlak, `${mesaj}[${i}]`));
    return;
  }
  if (beklenen !== null && typeof beklenen === 'object') {
    for (const [k, v] of Object.entries(beklenen)) yakinDerin(gercek?.[k], v, goreli, mutlak, `${mesaj}.${k}`);
    return;
  }
  if (typeof beklenen === 'number' || beklenen === null) {
    yakin(gercek, beklenen, goreli, mutlak, mesaj);
    return;
  }
  assert.equal(gercek, beklenen, mesaj);
}
