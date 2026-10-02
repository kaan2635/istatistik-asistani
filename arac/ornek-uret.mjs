// Kurgusal örnek veri setlerini üretir (tohumlu; her çalıştırmada aynı veri).
//   node arac/ornek-uret.mjs  →  ornek-veriler/*.csv ve src/veri/ornekler.js
// Veriler gerçek kişilere ait değildir; programın özelliklerini göstermek için tasarlanmıştır.

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const KOK = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');

function uretec(tohum) {
  let a = tohum >>> 0;
  const u = () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  const n = (ort = 0, ss = 1) => {
    let x = 0;
    while (x === 0) x = u();
    return ort + ss * Math.sqrt(-2 * Math.log(x)) * Math.cos(2 * Math.PI * u());
  };
  const b = (p) => u() < p;
  const sec = (liste, olasiliklar) => {
    const r = u();
    let t = 0;
    for (let i = 0; i < liste.length; i++) {
      t += olasiliklar[i];
      if (r < t) return liste[i];
    }
    return liste[liste.length - 1];
  };
  return { u, n, b, sec };
}

const yuvarla = (x, k = 0) => Math.round(x * 10 ** k) / 10 ** k;
const sinirla = (x, a, b) => Math.min(b, Math.max(a, x));

function csv(sutunlar, satirlar) {
  const kacir = (v) => {
    if (v === null || v === undefined) return '';
    const s = String(v);
    return /[",\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
  };
  return [sutunlar.join(','), ...satirlar.map((s) => sutunlar.map((c) => kacir(s[c])).join(','))].join('\n') + '\n';
}

function eksikEkle(r, satirlar, sutunlar, oran) {
  for (const s of satirlar) for (const c of sutunlar) if (r.u() < oran) s[c] = null;
}

// 1. Klinik çalışma (hasta–kontrol)
function klinik() {
  const r = uretec(20261002);
  const satirlar = [];
  for (let i = 0; i < 200; i++) {
    const hasta = i < 100;
    const yas = Math.round(sinirla(hasta ? r.n(58, 11) : r.n(54, 12), 18, 90));
    const erkek = r.b(hasta ? 0.56 : 0.48);
    const crp = yuvarla(Math.exp(hasta ? r.n(2.4, 0.9) : r.n(1.0, 0.7)), 1);
    const evre = hasta ? r.sec(['Evre I', 'Evre II', 'Evre III', 'Evre IV'], [0.25, 0.35, 0.28, 0.12]) : null;
    const lp = -4.2 + 0.035 * (yas - 55) + 0.45 * Math.log(crp) + (hasta ? 0.8 : 0) + (evre === 'Evre IV' ? 0.9 : evre === 'Evre III' ? 0.4 : 0);
    satirlar.push({
      Hasta_No: i + 1,
      Grup: hasta ? 'Hasta' : 'Kontrol',
      Yas: yas,
      Cinsiyet: erkek ? 'Erkek' : 'Kadın',
      VKI: yuvarla(hasta ? r.n(27.6, 4.1) : r.n(26.1, 3.8), 1),
      Sigara: r.b(hasta ? 0.44 : 0.3) ? 'Evet' : 'Hayır',
      Hipertansiyon: r.b(hasta ? 0.4 : 0.24) ? 'Var' : 'Yok',
      Diyabet: r.b(hasta ? 0.22 : 0.12) ? 'Var' : 'Yok',
      Kan_Grubu: r.sec(['A', 'B', 'AB', '0'], [0.42, 0.16, 0.08, 0.34]),
      CRP: crp,
      Hemoglobin: yuvarla(hasta ? r.n(12.8, 1.6) - (erkek ? -0.6 : 0.4) : r.n(13.9, 1.3) - (erkek ? -0.6 : 0.4), 1),
      Glukoz: Math.round(hasta ? r.n(118, 30) : r.n(97, 14)),
      Lokosit: yuvarla(Math.exp(hasta ? r.n(2.25, 0.3) : r.n(2.0, 0.22)), 1),
      Evre: evre,
      Yatis_Suresi: hasta ? Math.max(1, Math.round(Math.exp(r.n(1.8, 0.6)))) : null,
      Komplikasyon: r.u() < 1 / (1 + Math.exp(-lp)) ? 'Var' : 'Yok',
    });
  }
  eksikEkle(r, satirlar, ['VKI', 'CRP', 'Glukoz', 'Hemoglobin'], 0.02);
  return csv(Object.keys(satirlar[0]), satirlar);
}

// 2. Tedavi öncesi–sonrası (tekrarlı ölçüm)
function onceSonra() {
  const r = uretec(7);
  const satirlar = [];
  for (let i = 0; i < 48; i++) {
    const ilac = i % 2 === 0;
    const bas = sinirla(r.n(7.2, 1.2), 3, 10);
    const d1 = ilac ? r.n(2.4, 1.3) : r.n(0.9, 1.2);
    const d3 = ilac ? d1 + r.n(1.1, 0.9) : d1 + r.n(0.3, 0.9);
    const kilo = r.n(78, 12);
    const agriOnce = r.b(0.85);
    satirlar.push({
      Hasta_No: i + 1,
      Tedavi: ilac ? 'İlaç' : 'Plasebo',
      Yas: Math.round(r.n(46, 13)),
      Cinsiyet: r.b(0.5) ? 'Kadın' : 'Erkek',
      VAS_0: Math.round(bas),
      VAS_1ay: Math.round(sinirla(bas - d1, 0, 10)),
      VAS_3ay: Math.round(sinirla(bas - d3, 0, 10)),
      Kilo_once: yuvarla(kilo, 1),
      Kilo_sonra: yuvarla(kilo - (ilac ? r.n(2.1, 1.8) : r.n(0.4, 1.7)), 1),
      Agri_once: agriOnce ? 'Var' : 'Yok',
      Agri_sonra: agriOnce ? (r.b(ilac ? 0.35 : 0.75) ? 'Var' : 'Yok') : r.b(0.1) ? 'Var' : 'Yok',
    });
  }
  return csv(Object.keys(satirlar[0]), satirlar);
}

// 3. Ölçek (Likert), iki boyutlu, bir ters madde
function olcek() {
  const r = uretec(1234);
  const satirlar = [];
  const yuk = [0.85, 0.8, 0.75, -0.7, 0.7, 0.65, 0.8, 0.75, 0.75, 0.7, 0.65, 0.6];
  for (let i = 0; i < 250; i++) {
    const f1 = r.n();
    const f2 = 0.35 * f1 + Math.sqrt(1 - 0.35 ** 2) * r.n();
    const s = { Katilimci: i + 1, Cinsiyet: r.b(0.58) ? 'Kadın' : 'Erkek', Yas: Math.round(sinirla(r.n(34, 10), 18, 70)), Egitim: r.sec(['İlkokul', 'Lise', 'Üniversite', 'Lisansüstü'], [0.15, 0.35, 0.38, 0.12]) };
    for (let m = 0; m < 12; m++) {
      const f = m < 6 ? f1 : f2;
      s[`S${m + 1}`] = Math.round(sinirla(3 + 1.1 * yuk[m] * f + 0.75 * r.n(), 1, 5));
    }
    satirlar.push(s);
  }
  return csv(Object.keys(satirlar[0]), satirlar);
}

// 4. Sağkalım
function sagkalim() {
  const r = uretec(99);
  const satirlar = [];
  for (let i = 0; i < 160; i++) {
    const yeni = i % 2 === 1;
    const yas = Math.round(sinirla(r.n(61, 10), 30, 88));
    const evre = r.sec(['Evre I', 'Evre II', 'Evre III', 'Evre IV'], [0.2, 0.3, 0.3, 0.2]);
    const ek = { 'Evre I': 0, 'Evre II': 0.4, 'Evre III': 0.9, 'Evre IV': 1.5 }[evre];
    const tehlike = 0.012 * Math.exp((yeni ? Math.log(0.6) : 0) + 0.025 * (yas - 60) + ek);
    const olayZamani = -Math.log(r.u()) / tehlike;
    const sansur = Math.min(60, 6 + r.u() * 70);
    const sure = Math.min(olayZamani, sansur);
    satirlar.push({
      Hasta_No: i + 1,
      Tedavi: yeni ? 'Yeni' : 'Standart',
      Yas: yas,
      Cinsiyet: r.b(0.55) ? 'Erkek' : 'Kadın',
      Evre: evre,
      Sure_ay: yuvarla(Math.max(0.1, sure), 1),
      Olum: olayZamani <= sansur ? 'Var' : 'Yok',
    });
  }
  return csv(Object.keys(satirlar[0]), satirlar);
}

// 5. Tanı testi (ROC)
function tani() {
  const r = uretec(555);
  const satirlar = [];
  for (let i = 0; i < 200; i++) {
    const hasta = r.b(0.4);
    satirlar.push({
      Hasta_No: i + 1,
      Sepsis: hasta ? 'Var' : 'Yok',
      Yas: Math.round(sinirla(r.n(63, 15), 18, 95)),
      CRP: yuvarla(Math.exp(hasta ? r.n(4.4, 0.7) : r.n(3.7, 0.8)), 1),
      Prokalsitonin: yuvarla(Math.exp(hasta ? r.n(1.2, 0.9) : r.n(-0.8, 0.9)), 2),
      Laktat: yuvarla(Math.max(0.4, hasta ? r.n(3.0, 1.2) : r.n(2.0, 0.8)), 1),
      Hizli_Test: (hasta ? r.b(0.82) : r.b(0.12)) ? 'Pozitif' : 'Negatif',
    });
  }
  return csv(Object.keys(satirlar[0]), satirlar);
}

// 6. Gözlemciler arası uyum
function uyum() {
  const r = uretec(31);
  const satirlar = [];
  for (let i = 0; i < 60; i++) {
    const gercek = r.n(42, 8);
    const derece = r.sec([1, 2, 3, 4], [0.3, 0.35, 0.25, 0.1]);
    const kay = (d) => (r.b(0.78) ? d : sinirla(d + (r.b(0.5) ? 1 : -1), 1, 4));
    const tani1 = r.b(0.4);
    satirlar.push({
      Olgu: i + 1,
      Olcum_Gozlemci1: yuvarla(gercek + r.n(0, 2), 1),
      Olcum_Gozlemci2: yuvarla(gercek + 0.8 + r.n(0, 2.2), 1),
      Derece_Gozlemci1: derece,
      Derece_Gozlemci2: kay(derece),
      Tani_Gozlemci1: tani1 ? 'Var' : 'Yok',
      Tani_Gozlemci2: (r.b(0.88) ? tani1 : !tani1) ? 'Var' : 'Yok',
    });
  }
  return csv(Object.keys(satirlar[0]), satirlar);
}

const ORNEKLER = [
  { dosya: 'klinik-calisma.csv', ad: 'Klinik çalışma (hasta–kontrol)', aciklama: '200 kişi; grup karşılaştırması, Tablo 1, ki-kare/Fisher, lojistik regresyon.', uret: klinik },
  { dosya: 'once-sonra.csv', ad: 'Tedavi öncesi–sonrası', aciklama: '48 hasta, üç zamanda VAS; eşleştirilmiş testler, Friedman, McNemar, karma desen.', uret: onceSonra },
  { dosya: 'olcek.csv', ad: 'Ölçek uyarlama (Likert)', aciklama: '250 katılımcı, 12 madde; Cronbach alfa, faktör analizi.', uret: olcek },
  { dosya: 'sagkalim.csv', ad: 'Sağkalım çalışması', aciklama: '160 hasta; Kaplan-Meier, log-rank, Cox regresyonu.', uret: sagkalim },
  { dosya: 'tani-testi.csv', ad: 'Tanı testi (sepsis)', aciklama: '200 hasta; ROC analizi, kesim değeri, DeLong karşılaştırması, 2×2 tanı testi.', uret: tani },
  { dosya: 'gozlemci-uyumu.csv', ad: 'Gözlemciler arası uyum', aciklama: '60 olgu; ICC, Bland-Altman, kappa, ağırlıklı kappa.', uret: uyum },
];

fs.mkdirSync(path.join(KOK, 'ornek-veriler'), { recursive: true });
const modul = ['// Bu dosya arac/ornek-uret.mjs tarafından üretilir; elle düzenlemeyin.', 'export const ORNEK_VERILER = ['];
for (const o of ORNEKLER) {
  const metin = o.uret();
  fs.writeFileSync(path.join(KOK, 'ornek-veriler', o.dosya), metin);
  modul.push(`  { ad: ${JSON.stringify(o.ad)}, aciklama: ${JSON.stringify(o.aciklama)}, dosya: ${JSON.stringify(o.dosya)}, csv: ${JSON.stringify(metin)} },`);
  console.log(`${o.dosya}: ${metin.split('\n').length - 2} satır`);
}
modul.push('];', '');
fs.writeFileSync(path.join(KOK, 'src', 'veri', 'ornekler.js'), modul.join('\n'));
