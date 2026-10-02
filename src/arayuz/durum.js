// Uygulama durumu. Tek bir nesne; değişiklikler abonelere bildirilir.

import { ayarlariBirlestir } from '../karar/ayarlar.js';
import { bicimAyarlari } from '../rapor/bicim.js';
import { olcumTahmin } from '../veri/tanima.js';
import { depo, kimlik } from './dom.js';

const AYAR_ANAHTARI = 'istatistik-asistani-ayarlar';

export const durum = {
  veriSeti: null,
  olcumler: {},
  tahminler: {},
  ayarlar: ayarlariBirlestir(depo.oku(AYAR_ANAHTARI, {})),
  istekler: [],
  sonuclar: new Map(),
  filtre: null,
  satirMaskesi: null,
  aktifSekme: 'veri',
  calismaKitabi: null,
  sonEklenen: null,
};

const aboneler = new Set();

export function abone(fn) {
  aboneler.add(fn);
  return () => aboneler.delete(fn);
}

/** neler: 'veri' | 'degisken' | 'ayar' | 'rapor' | 'sekme' | 'filtre' */
export function bildir(...neler) {
  if (neler.some((n) => ['veri', 'degisken', 'ayar', 'filtre'].includes(n))) durum.sonuclar.clear();
  for (const fn of aboneler) fn(new Set(neler));
}

export function baglam() {
  return {
    olcumler: durum.olcumler,
    ayarlar: durum.ayarlar,
    bicim: bicimAyarlari(durum.ayarlar),
    satirMaskesi: durum.satirMaskesi,
  };
}

export function veriYukle(veriSeti, { calismaKitabi = null } = {}) {
  durum.veriSeti = veriSeti;
  durum.calismaKitabi = calismaKitabi;
  durum.olcumler = {};
  durum.tahminler = {};
  for (const d of veriSeti.degiskenler) {
    const t = olcumTahmin(d);
    durum.tahminler[d.ad] = t;
    durum.olcumler[d.ad] = t.olcum;
    if (t.siraliDuzeyler && !d.duzeySirasi) d.duzeySirasi = t.siraliDuzeyler;
  }
  durum.istekler = [];
  durum.filtre = null;
  durum.satirMaskesi = null;
  bildir('veri');
}

export function olcumAyarla(ad, olcum) {
  durum.olcumler[ad] = olcum;
  if (durum.tahminler[ad]) durum.tahminler[ad] = { ...durum.tahminler[ad], guven: 'yuksek', kullanici: true };
  bildir('degisken');
}

export function ayarDegistir(anahtar, deger) {
  durum.ayarlar = { ...durum.ayarlar, [anahtar]: deger };
  depo.yaz(AYAR_ANAHTARI, durum.ayarlar);
  bildir('ayar');
}

export function ayarlariSifirla() {
  durum.ayarlar = ayarlariBirlestir({});
  depo.yaz(AYAR_ANAHTARI, {});
  bildir('ayar');
}

export function istekEkle(istek) {
  const yeni = { id: kimlik(), ...istek };
  durum.istekler.push(yeni);
  durum.sonEklenen = yeni.id;
  bildir('rapor');
  return yeni;
}

export function istekSil(id) {
  durum.istekler = durum.istekler.filter((i) => i.id !== id);
  durum.sonuclar.delete(id);
  bildir('rapor');
}

export function istekTasi(id, yon) {
  const i = durum.istekler.findIndex((x) => x.id === id);
  const j = i + yon;
  if (i < 0 || j < 0 || j >= durum.istekler.length) return;
  const l = durum.istekler;
  [l[i], l[j]] = [l[j], l[i]];
  bildir('rapor');
}

export function raporuTemizle() {
  durum.istekler = [];
  durum.sonuclar.clear();
  bildir('rapor');
}

export function sekmeyeGit(sekme) {
  durum.aktifSekme = sekme;
  bildir('sekme');
}

/** Değişken üst verisi değişti (etiket, değer etiketleri, eksik kodlar, düzey sırası) */
export function degiskenGuncellendi() {
  bildir('degisken');
}

export function filtreAyarla(filtre, maske) {
  durum.filtre = filtre;
  durum.satirMaskesi = maske;
  bildir('filtre');
}

// ---------- Proje dosyası ----------

const PROJE_SURUMU = 1;

export function projeyiSerilestir() {
  return JSON.stringify({
    tur: 'istatistik-asistani-proje',
    surum: PROJE_SURUMU,
    kayitTarihi: new Date().toISOString(),
    veriSeti: durum.veriSeti,
    olcumler: durum.olcumler,
    ayarlar: durum.ayarlar,
    istekler: durum.istekler,
    filtre: durum.filtre,
  });
}

export function projeyiYukle(metin, maskeHesapla) {
  const p = JSON.parse(metin);
  if (p.tur !== 'istatistik-asistani-proje') throw new Error('Bu dosya bir İstatistik Asistanı proje dosyası değil.');
  durum.veriSeti = p.veriSeti;
  durum.olcumler = p.olcumler || {};
  durum.tahminler = {};
  for (const d of durum.veriSeti.degiskenler) {
    durum.tahminler[d.ad] = { ...olcumTahmin(d), guven: 'yuksek', kullanici: true };
    if (!durum.olcumler[d.ad]) durum.olcumler[d.ad] = durum.tahminler[d.ad].olcum;
  }
  durum.ayarlar = ayarlariBirlestir(p.ayarlar || {});
  durum.istekler = p.istekler || [];
  durum.filtre = p.filtre || null;
  durum.satirMaskesi = durum.filtre && maskeHesapla ? maskeHesapla(durum.filtre) : null;
  durum.calismaKitabi = null;
  bildir('veri', 'rapor');
}
