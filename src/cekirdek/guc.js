// Güç (power) ve örneklem büyüklüğü analizi.
//
// Her fonksiyon {n, guc, etki} üçlüsünden hangisi verilmemişse (null/undefined) onu çözer; diğer ikisi verilmelidir.
// Hesaplar R'nin `pwr` paketiyle (dolayısıyla G*Power'ın aynı yöntemleriyle) birebir aynı formülleri kullanır.
// `pwr` bilinmeyeni uniroot'un varsayılan toleransıyla (~1.2e-4) çözer; burada kök Brent yöntemiyle ~1e-12 göreli
// doğrulukta bulunur. Bu yüzden kesirli çözümler pwr çıktısından 4. ondalıktan sonra farklılaşabilir; tam sayıya
// yuvarlanmış örneklem büyüklükleri ve gerçekleşen güç aynıdır (testler R'de uniroot(tol = 1e-12) ile doğrular).
//
// Örneklem büyüklüğü çözüldüğünde dönen alanlar:
//   nKesin   — denklemin kesirli çözümü
//   n        — yukarı yuvarlanmış tam sayı (gerekli en küçük tam sayı)
//   guc      — hedef güç (girdi)
//   gercekGuc— yuvarlanmış n ile ulaşılan güç (G*Power'daki "Actual power")
// Güç çözüldüğünde guc = gercekGuc = hesaplanan güç; etki çözüldüğünde etki.deger = bulunan etki büyüklüğü.
//
// alternatif: 'iki-yonlu' (varsayılan) | 'kucuk' | 'buyuk'  (R adları 'two.sided' | 'less' | 'greater' de kabul edilir)

import { pnorm, qnorm, qt, pnt, qf, pnf, qchisq, pnchisq } from './dagilim.js';
import { brent } from './sayisal.js';
import { IstatistikHatasi, gerekli } from './hata.js';

// ---------- Ortak yardımcılar ----------

const ALTERNATIFLER = {
  'iki-yonlu': 'iki-yonlu',
  'two.sided': 'iki-yonlu',
  kucuk: 'kucuk',
  less: 'kucuk',
  buyuk: 'buyuk',
  greater: 'buyuk',
};

function alternatifOku(a) {
  const s = ALTERNATIFLER[a ?? 'iki-yonlu'];
  if (!s) throw new IstatistikHatasi(`Bilinmeyen alternatif hipotez: "${a}". 'iki-yonlu', 'kucuk' ya da 'buyuk' olmalıdır.`);
  return s;
}

const bos = (v) => v === null || v === undefined;

function alfaDenetle(alfa) {
  gerekli(typeof alfa === 'number' && alfa > 0 && alfa < 1, 'Anlamlılık düzeyi (alfa) 0 ile 1 arasında olmalıdır.');
}

function gucDenetle(guc, alfa) {
  if (bos(guc)) return;
  gerekli(typeof guc === 'number' && guc > 0 && guc < 1, 'Güç 0 ile 1 arasında olmalıdır.');
  gerekli(guc > alfa, 'Hedef güç, anlamlılık düzeyinden (alfa) büyük olmalıdır.');
}

/** {n, guc, etki} içinden tam olarak birinin boş olduğunu denetler, boş olanın adını döndürür. */
function bilinmeyenBul(degerler) {
  const bosOlanlar = Object.entries(degerler).filter(([, v]) => bos(v)).map(([k]) => k);
  if (bosOlanlar.length !== 1) {
    throw new IstatistikHatasi(`Şunlardan tam olarak biri boş bırakılmalıdır (çözülecek bilinmeyen): ${Object.keys(degerler).join(', ')}.`);
  }
  return bosOlanlar[0];
}

/**
 * Artan g için g(x) = hedef çözümü. alt'tan başlayıp aralığı ikiye katlayarak kökü kuşatır, sonra Brent.
 * g(alt) >= hedef ise alt döner (sinirda = true).
 */
function artanCoz(g, hedef, alt, bas, ust, mesaj) {
  const h = (x) => g(x) - hedef;
  let a = alt;
  if (h(a) >= 0) return { x: alt, sinirda: true };
  let b = Math.min(Math.max(bas, alt + 1e-6), ust);
  let hb = h(b);
  let adim = 0;
  while (!(hb >= 0)) {
    if (b >= ust || adim++ > 300) throw new IstatistikHatasi(mesaj);
    a = b;
    b = Math.min(ust, alt + (b - alt) * 2);
    hb = h(b);
  }
  return { x: brent(h, a, b, 1e-12 * Math.max(1, Math.abs(a), Math.abs(b))), sinirda: false };
}

/** Azalan g için g(x) = hedef (x, alt'tan aşağı doğru aranır: alt > x > ust) */
function azalanCozAsagi(g, hedef, ust, bas, alt, mesaj) {
  // x = ust - y dönüşümüyle artan probleme çevir: G(y) = g(ust - y), y >= 0
  const r = artanCoz((y) => g(ust - y), hedef, 0, ust - bas, ust - alt, mesaj);
  return { x: ust - r.x, sinirda: r.sinirda };
}

/** Sabit aralıkta kök (fonksiyon iki uçta ters işaretli olmalı) */
function aralikCoz(g, hedef, a, b, mesaj) {
  const h = (x) => g(x) - hedef;
  const ha = h(a);
  const hb = h(b);
  if (ha === 0) return a;
  if (hb === 0) return b;
  if (!(ha * hb < 0)) throw new IstatistikHatasi(mesaj);
  return brent(h, a, b, 1e-13);
}

/**
 * Kesirli n'yi gerekli en küçük tam sayıya yuvarlar. Kayan nokta gürültüsüne karşı yuvarlanan n'de
 * gücün hedefe ulaştığı ayrıca denetlenir.
 */
function yukariYuvarla(nKesin, gucFn, hedef, enKucuk = 1) {
  let n = Math.ceil(nKesin - 1e-9 * Math.max(1, nKesin));
  const min = Math.ceil(enKucuk - 1e-8);
  if (n < min) n = min;
  for (let i = 0; i < 1000 && gucFn(n) < hedef - 1e-12; i++) n++;
  return n;
}

const tavan = (x) => Math.ceil(x - 1e-9 * Math.max(1, x));

/**
 * İki grup için yuvarlama: n1 = ⌈n1*⌉, n2 = ⌈oran·n1*⌉ (her grup ayrı yukarı yuvarlanır). Kayan nokta gürültüsü
 * nedeniyle güç hedefin altında kalırsa n1 artırılıp n2 = ⌈oran·n1⌉ alınır.
 */
function ikiGrupYuvarla(nKesin, oran, gucFn, hedef, enKucuk) {
  let n1 = Math.max(tavan(nKesin), Math.ceil(enKucuk - 1e-8));
  let n2 = oran === 1 ? n1 : tavan(oran * nKesin);
  for (let i = 0; i < 1000 && gucFn(n1, n2) < hedef - 1e-12; i++) {
    n1++;
    n2 = oran === 1 ? n1 : tavan(oran * n1);
  }
  return [n1, n2];
}

const ULASILAMAZ ='Bu girdilerle hedef güce ulaşılamıyor (örneklem 10⁹\'u aşıyor ya da etki yönü alternatif hipotezle çelişiyor).';
const ETKI_BULUNAMADI = 'Bu girdilerle hedef gücü sağlayan bir etki büyüklüğü bulunamadı.';

// ---------- t testleri ----------

/** t testinin gücü (sd ve merkezi olmama parametresi verilmiş). pwr::pwr.t.test ile aynı formül. */
function tGucuSdNcp(nu, ncp, alfa, alternatif) {
  if (alternatif === 'iki-yonlu') {
    const d = Math.abs(ncp);
    const qu = qt(alfa / 2, nu, false);
    return { guc: pnt(qu, nu, d, false) + pnt(-qu, nu, d, true), kritik: qu };
  }
  if (alternatif === 'kucuk') {
    const q = qt(alfa, nu, true);
    return { guc: pnt(q, nu, ncp, true), kritik: q };
  }
  const q = qt(alfa, nu, false);
  return { guc: pnt(q, nu, ncp, false), kritik: q };
}

/** tek/eşleştirilmiş: n gözlem; iki: n1, n2. are: Wilcoxon/Mann-Whitney için asimptotik göreli etkinlik çarpanı. */
function tGucuHesapla(tip, d, n1, n2, alfa, alternatif, are = 1) {
  let nu;
  let ncp;
  if (tip === 'iki') {
    const a = are * n1;
    const b = are * n2;
    nu = a + b - 2;
    ncp = d / Math.sqrt(1 / a + 1 / b);
  } else {
    const a = are * n1;
    nu = a - 1;
    ncp = Math.sqrt(a) * d;
  }
  return { ...tGucuSdNcp(nu, ncp, alfa, alternatif), sd: nu, ncp };
}

const T_ADLARI = {
  tek: { yontem: 'guc-t-tek', ad: 'Tek örneklem t testi' },
  eslestirilmis: { yontem: 'guc-t-eslestirilmis', ad: 'Eşleştirilmiş örneklemler t testi' },
  iki: { yontem: 'guc-t-iki', ad: 'Bağımsız iki örneklem t testi' },
};

/** Etki büyüklüğünü (d) çözer: güç, alternatif yönünde d ile artar. */
function dCoz(gucFn, hedef, alternatif) {
  if (alternatif === 'kucuk') return azalanCozAsagi(gucFn, hedef, 0, -0.5, -1e4, ETKI_BULUNAMADI).x;
  return artanCoz(gucFn, hedef, 0, 0.5, 1e4, ETKI_BULUNAMADI).x;
}

function tGenel(girdi, are, kimlik) {
  const { tip = 'iki', d, n, guc, alfa = 0.05, oran = 1 } = girdi;
  const alternatif = alternatifOku(girdi.alternatif);
  gerekli(['tek', 'iki', 'eslestirilmis'].includes(tip), "t testi tipi 'tek', 'iki' ya da 'eslestirilmis' olmalıdır.");
  alfaDenetle(alfa);
  gucDenetle(guc, alfa);
  gerekli(typeof oran === 'number' && oran > 0, 'Grup oranı (n2/n1) pozitif olmalıdır.');
  const bilinmeyen = bilinmeyenBul({ n, guc, etki: d });
  const iki = tip === 'iki';
  // pwr: n > 2 (iki örneklemde gruplar en az 2; tek örneklemde sd = n - 1 > 0). ARE uygulandığında etkin n için.
  const nAlt = (2 + 1e-10) / are;
  const fn = (nn, dd) => tGucuHesapla(tip, dd, nn, iki ? oran * nn : 0, alfa, alternatif, are);
  const sonuc = {
    yontem: kimlik.yontem,
    ad: kimlik.ad,
    analiz: 'güç',
    cozulen: bilinmeyen,
    alternatif,
    alfa,
    uyarilar: [],
  };
  let dd = d;
  let nKesin = n;
  let gucDeger = guc;
  if (bilinmeyen === 'n') {
    gerekli(d !== 0, 'Etki büyüklüğü sıfır olamaz.');
    const r = artanCoz((x) => fn(x, d).guc, guc, nAlt, nAlt + 8, 1e9, ULASILAMAZ);
    nKesin = r.x;
    if (r.sinirda) sonuc.uyarilar.push('Hedef güce en küçük örneklemle bile ulaşılıyor.');
  } else {
    gerekli(typeof n === 'number' && n * are > 2 - 1e-12, 'Örneklem büyüklüğü en az 2 olmalıdır.');
    if (bilinmeyen === 'guc') gucDeger = fn(n, d).guc;
    else dd = dCoz((x) => fn(n, x).guc, guc, alternatif);
  }
  if (alternatif === 'iki-yonlu') dd = Math.abs(dd);
  sonuc.etki = { ad: 'Cohen d', sembol: 'd', deger: dd };
  sonuc.guc = gucDeger;
  if (iki) {
    let n1;
    let n2;
    if (bilinmeyen === 'n') {
      [n1, n2] = ikiGrupYuvarla(nKesin, oran, (a, b) => tGucuHesapla(tip, dd, a, b, alfa, alternatif, are).guc, guc, nAlt);
    } else {
      n1 = n;
      n2 = oran * n;
    }
    const son = tGucuHesapla(tip, dd, n1, n2, alfa, alternatif, are);
    Object.assign(sonuc, {
      n: n1,
      nKesin,
      n1,
      n2,
      n1Kesin: nKesin,
      n2Kesin: oran * nKesin,
      oran,
      toplamN: n1 + n2,
      gercekGuc: bilinmeyen === 'n' ? son.guc : gucDeger,
      sd: son.sd,
      ncp: son.ncp,
      kritik: son.kritik,
    });
  } else {
    const nTam = bilinmeyen === 'n' ? yukariYuvarla(nKesin, (m) => fn(m, dd).guc, guc, nAlt) : n;
    const son = fn(nTam, dd);
    Object.assign(sonuc, {
      n: nTam,
      nKesin,
      toplamN: nTam,
      gercekGuc: bilinmeyen === 'n' ? son.guc : gucDeger,
      sd: son.sd,
      ncp: son.ncp,
      kritik: son.kritik,
    });
  }
  if (are !== 1) sonuc.are = are;
  return sonuc;
}

/**
 * t testleri için güç / örneklem büyüklüğü / etki büyüklüğü (merkezi olmayan t dağılımı ile kesin).
 * R: pwr::pwr.t.test(n, d, sig.level, power, type, alternative) — G*Power "t tests: Means" ile aynı.
 * tip 'iki' için `oran` (n2/n1, G*Power "Allocation ratio N2/N1") verilebilir: n = n1, n2 = oran·n1;
 * oran = 1 iken pwr.t.test(type = 'two.sample'), aksi halde pwr.t2n.test(n1, n2 = oran·n1) ile aynıdır.
 * Grup başına n çözüldüğünde n1 = ⌈n1*⌉, n2 = ⌈oran·n1*⌉.
 * 'iki-yonlu' gücü iki kuyruğu birlikte içerir (pwr ve G*Power gibi).
 * @param {{tip?:'tek'|'iki'|'eslestirilmis', d?:number|null, n?:number|null, guc?:number|null,
 *          alfa?:number, alternatif?:string, oran?:number}} girdi
 */
export function tTestGuc(girdi = {}) {
  const tip = girdi.tip ?? 'iki';
  return tGenel(girdi, 1, T_ADLARI[tip] ?? T_ADLARI.iki);
}

/**
 * Grup büyüklükleri farklı bağımsız iki örneklem t testi. R: pwr::pwr.t2n.test(n1, n2, d, ...).
 * n1, n2, d, guc içinden biri boş bırakılır.
 * @param {{d?:number|null, n1?:number|null, n2?:number|null, guc?:number|null, alfa?:number, alternatif?:string}} girdi
 */
export function tTestIkiNGuc(girdi = {}) {
  const { d, n1, n2, guc, alfa = 0.05 } = girdi;
  const alternatif = alternatifOku(girdi.alternatif);
  alfaDenetle(alfa);
  gucDenetle(guc, alfa);
  const bilinmeyen = bilinmeyenBul({ n1, n2, guc, etki: d });
  const fn = (a, b, dd) => tGucuHesapla('iki', dd, a, b, alfa, alternatif);
  const sonuc = { yontem: 'guc-t-iki', ad: 'Bağımsız iki örneklem t testi (farklı n)', analiz: 'güç', cozulen: bilinmeyen, alternatif, alfa, uyarilar: [] };
  for (const [ad, v] of [['n1', n1], ['n2', n2]]) {
    if (!bos(v)) gerekli(v >= 2, `${ad} en az 2 olmalıdır.`);
  }
  let dd = d;
  let a = n1;
  let b = n2;
  let gucDeger = guc;
  let kesin = null;
  if (bilinmeyen === 'n1' || bilinmeyen === 'n2') {
    gerekli(d !== 0, 'Etki büyüklüğü sıfır olamaz.');
    const g = bilinmeyen === 'n1' ? (x) => fn(x, n2, d).guc : (x) => fn(n1, x, d).guc;
    const r = artanCoz(g, guc, 2 + 1e-10, 10, 1e9, ULASILAMAZ);
    if (r.sinirda) sonuc.uyarilar.push('Hedef güce en küçük örneklemle bile ulaşılıyor.');
    kesin = r.x;
    const tam = yukariYuvarla(kesin, g, guc, 2);
    if (bilinmeyen === 'n1') a = tam;
    else b = tam;
  } else if (bilinmeyen === 'guc') {
    gucDeger = fn(n1, n2, d).guc;
  } else {
    dd = dCoz((x) => fn(n1, n2, x).guc, guc, alternatif);
  }
  if (alternatif === 'iki-yonlu') dd = Math.abs(dd);
  const son = fn(a, b, dd);
  return Object.assign(sonuc, {
    etki: { ad: 'Cohen d', sembol: 'd', deger: dd },
    guc: gucDeger,
    n1: a,
    n2: b,
    nKesin: kesin,
    n1Kesin: bilinmeyen === 'n1' ? kesin : n1,
    n2Kesin: bilinmeyen === 'n2' ? kesin : n2,
    toplamN: a + b,
    gercekGuc: kesin === null ? gucDeger : son.guc,
    sd: son.sd,
    ncp: son.ncp,
    kritik: son.kritik,
  });
}

// ---------- Parametrik olmayan testler (A.R.E. yöntemi) ----------

/** Asimptotik göreli etkinlik (Wilcoxon / Mann-Whitney'nin t testine göre), ebeveyn dağılıma göre */
export const ARE_DEGERLERI = Object.freeze({
  normal: 3 / Math.PI, // 0.9549
  lojistik: (Math.PI * Math.PI) / 9, // 1.0966
  laplace: 1.5,
  enKotu: 0.864, // Hodges-Lehmann alt sınırı (108/125); "%15 ekle" kuralı
});

function areOku(ebeveyn) {
  const e = ebeveyn ?? 'normal';
  const v = ARE_DEGERLERI[e];
  if (!v) throw new IstatistikHatasi(`Bilinmeyen ebeveyn dağılım: "${e}". 'normal', 'lojistik', 'laplace' ya da 'enKotu' olmalıdır.`);
  return v;
}

/**
 * Mann-Whitney U (Wilcoxon sıra toplamı) testi için güç / örneklem — A.R.E. yöntemi (G*Power "A.R.E." seçeneği).
 * Güç, örneklem büyüklükleri ARE ile çarpılmış t testinin gücüdür: sd = (n1+n2)·ARE − 2,
 * δ = d·√(n1·n2·ARE/(n1+n2)). Dolayısıyla kesirli çözüm n_MW = n_t / ARE.
 * ebeveyn: 'normal' (ARE = 3/π ≈ 0.955, varsayılan), 'lojistik' (π²/9), 'laplace' (1.5), 'enKotu' (0.864; en kötü
 * durum — t testi n'ini 0.864'e bölmek, yani ~%15 artırmak).
 * @param {{d?:number|null, n?:number|null, guc?:number|null, alfa?:number, alternatif?:string, oran?:number,
 *          ebeveyn?:'normal'|'lojistik'|'laplace'|'enKotu'}} girdi
 */
export function mannWhitneyGuc(girdi = {}) {
  const are = areOku(girdi.ebeveyn);
  const s = tGenel({ ...girdi, tip: 'iki' }, are, { yontem: 'guc-mann-whitney', ad: 'Mann-Whitney U testi (A.R.E.)' });
  s.ebeveyn = girdi.ebeveyn ?? 'normal';
  return s;
}

/**
 * Wilcoxon işaretli sıralar testi (tek örneklem / eşleştirilmiş) için güç / örneklem — A.R.E. yöntemi (G*Power).
 * sd = n·ARE − 1, δ = d·√(n·ARE). d: farkların ortalaması / farkların SS (dz).
 * @param {{d?:number|null, n?:number|null, guc?:number|null, alfa?:number, alternatif?:string,
 *          ebeveyn?:'normal'|'lojistik'|'laplace'|'enKotu'}} girdi
 */
export function wilcoxonGuc(girdi = {}) {
  const are = areOku(girdi.ebeveyn);
  const s = tGenel({ ...girdi, tip: 'eslestirilmis' }, are, { yontem: 'guc-wilcoxon', ad: 'Wilcoxon işaretli sıralar testi (A.R.E.)' });
  s.ebeveyn = girdi.ebeveyn ?? 'normal';
  return s;
}

// ---------- ANOVA, regresyon, ki-kare (merkezi olmayan F / ki-kare) ----------

function anovaGucHesapla(k, n, f, alfa) {
  const d1 = k - 1;
  const d2 = (n - 1) * k;
  const lambda = k * n * f * f;
  const kritik = qf(alfa, d1, d2, false);
  return { guc: pnf(kritik, d1, d2, lambda, false), kritik, sd: [d1, d2], ncp: lambda };
}

/** Pozitif etki büyüklüğü (f, f², w, h) çözümü: güç etkiyle artar, etki = 0'da güç = alfa. */
function pozitifEtkiCoz(gucFn, hedef, bas = 0.25) {
  return artanCoz(gucFn, hedef, 0, bas, 1e7, ETKI_BULUNAMADI).x;
}

/**
 * Tek yönlü (dengeli) ANOVA için güç / grup başına örneklem / etki (Cohen f).
 * R: pwr::pwr.anova.test(k, n, f, sig.level, power). λ = k·n·f², sd = (k−1, k(n−1)) — G*Power "ANOVA: Fixed effects,
 * omnibus, one-way" ile aynı (G*Power toplam N = k·n kullanır).
 * @param {{k:number, f?:number|null, n?:number|null, guc?:number|null, alfa?:number}} girdi n: grup başına
 */
export function anovaGuc(girdi = {}) {
  const { k, f, n, guc, alfa = 0.05 } = girdi;
  gerekli(Number.isFinite(k) && k >= 2, 'Grup sayısı (k) en az 2 olmalıdır.');
  alfaDenetle(alfa);
  gucDenetle(guc, alfa);
  if (!bos(f)) gerekli(f >= 0, 'Etki büyüklüğü f negatif olamaz.');
  const bilinmeyen = bilinmeyenBul({ n, guc, etki: f });
  const sonuc = { yontem: 'guc-anova', ad: 'Tek yönlü ANOVA', analiz: 'güç', cozulen: bilinmeyen, alfa, k, uyarilar: [] };
  let nKesin = n;
  let ff = f;
  let gucDeger = guc;
  if (bilinmeyen === 'n') {
    gerekli(f > 0, 'Etki büyüklüğü sıfır olamaz.');
    const r = artanCoz((x) => anovaGucHesapla(k, x, f, alfa).guc, guc, 2 + 1e-10, 10, 1e9, ULASILAMAZ);
    nKesin = r.x;
  } else {
    gerekli(n >= 2, 'Grup başına örneklem en az 2 olmalıdır.');
    if (bilinmeyen === 'guc') gucDeger = anovaGucHesapla(k, n, f, alfa).guc;
    else ff = pozitifEtkiCoz((x) => anovaGucHesapla(k, n, x, alfa).guc, guc);
  }
  const nTam = bilinmeyen === 'n' ? yukariYuvarla(nKesin, (m) => anovaGucHesapla(k, m, ff, alfa).guc, guc, 2) : n;
  const son = anovaGucHesapla(k, nTam, ff, alfa);
  return Object.assign(sonuc, {
    etki: { ad: 'Cohen f', sembol: 'f', deger: ff },
    guc: gucDeger,
    n: nTam,
    nKesin,
    toplamN: k * nTam,
    toplamNKesin: k * nKesin,
    gercekGuc: bilinmeyen === 'n' ? son.guc : gucDeger,
    sd: son.sd,
    ncp: son.ncp,
    kritik: son.kritik,
  });
}

function f2GucHesapla(u, v, f2, alfa) {
  const lambda = f2 * (u + v + 1);
  const kritik = qf(alfa, u, v, false);
  return { guc: pnf(kritik, u, v, lambda, false), kritik, sd: [u, v], ncp: lambda };
}

/**
 * Çoklu doğrusal regresyon (F testi; R² artışı ya da tüm model) için güç / örneklem / etki (Cohen f²).
 * R: pwr::pwr.f2.test(u, v, f2, sig.level, power); λ = f²·(u + v + 1) = f²·N — G*Power "Linear multiple regression:
 * Fixed model, R² deviation from zero / increase" ile aynı.
 * u: sınanan yordayıcı sayısı (pay sd), v: payda sd = N − p − 1 (p: modeldeki toplam yordayıcı; R² artışında
 * p = u + kontrol değişkenleri — bu durumda `v` ya da `N` ile birlikte `p` verin). N = v + p + 1.
 * @param {{u:number, v?:number|null, N?:number|null, p?:number, f2?:number|null, guc?:number|null, alfa?:number}} girdi
 */
export function regresyonGuc(girdi = {}) {
  const { u, f2, guc, alfa = 0.05 } = girdi;
  gerekli(Number.isFinite(u) && u >= 1, 'Sınanan yordayıcı sayısı (u) en az 1 olmalıdır.');
  const p = girdi.p ?? u;
  gerekli(p >= u, 'Toplam yordayıcı sayısı (p), sınanan yordayıcı sayısından (u) küçük olamaz.');
  let v = girdi.v;
  if (bos(v) && !bos(girdi.N)) v = girdi.N - p - 1;
  alfaDenetle(alfa);
  gucDenetle(guc, alfa);
  if (!bos(f2)) gerekli(f2 >= 0, 'Etki büyüklüğü f² negatif olamaz.');
  const bilinmeyen = bilinmeyenBul({ n: v, guc, etki: f2 });
  const sonuc = { yontem: 'guc-regresyon', ad: 'Çoklu doğrusal regresyon (F testi)', analiz: 'güç', cozulen: bilinmeyen, alfa, u, p, uyarilar: [] };
  let vKesin = v;
  let ff = f2;
  let gucDeger = guc;
  if (bilinmeyen === 'n') {
    gerekli(f2 > 0, 'Etki büyüklüğü sıfır olamaz.');
    vKesin = artanCoz((x) => f2GucHesapla(u, x, f2, alfa).guc, guc, 1 + 1e-10, 10, 1e9, ULASILAMAZ).x;
  } else {
    gerekli(v >= 1, 'Payda serbestlik derecesi (v = N − p − 1) en az 1 olmalıdır.');
    if (bilinmeyen === 'guc') gucDeger = f2GucHesapla(u, v, f2, alfa).guc;
    else ff = pozitifEtkiCoz((x) => f2GucHesapla(u, v, x, alfa).guc, guc, 0.1);
  }
  const vTam = bilinmeyen === 'n' ? yukariYuvarla(vKesin, (m) => f2GucHesapla(u, m, ff, alfa).guc, guc, 1) : v;
  const son = f2GucHesapla(u, vTam, ff, alfa);
  return Object.assign(sonuc, {
    etki: { ad: 'Cohen f²', sembol: 'f²', deger: ff },
    guc: gucDeger,
    v: vTam,
    vKesin,
    n: vTam + p + 1,
    nKesin: vKesin + p + 1,
    toplamN: vTam + p + 1,
    gercekGuc: bilinmeyen === 'n' ? son.guc : gucDeger,
    sd: son.sd,
    ncp: son.ncp,
    kritik: son.kritik,
  });
}

function kiKareGucHesapla(w, df, N, alfa) {
  const kritik = qchisq(alfa, df, false);
  const lambda = N * w * w;
  return { guc: pnchisq(kritik, df, lambda, false), kritik, sd: df, ncp: lambda };
}

/**
 * Ki-kare testleri (uyum iyiliği / bağımsızlık) için güç / toplam örneklem / etki (Cohen w).
 * R: pwr::pwr.chisq.test(w, N, df, sig.level, power); λ = N·w² — G*Power "χ² tests: Goodness-of-fit tests:
 * Contingency tables" ile aynı. Bağımsızlık testinde df = (r−1)(c−1).
 * @param {{w?:number|null, df:number, N?:number|null, guc?:number|null, alfa?:number}} girdi
 */
export function kiKareGuc(girdi = {}) {
  const { w, df, N, guc, alfa = 0.05 } = girdi;
  gerekli(Number.isFinite(df) && df >= 1, 'Serbestlik derecesi en az 1 olmalıdır.');
  alfaDenetle(alfa);
  gucDenetle(guc, alfa);
  if (!bos(w)) gerekli(w >= 0, 'Etki büyüklüğü w negatif olamaz.');
  const bilinmeyen = bilinmeyenBul({ n: N, guc, etki: w });
  const sonuc = { yontem: 'guc-ki-kare', ad: 'Ki-kare testi', analiz: 'güç', cozulen: bilinmeyen, alfa, uyarilar: [] };
  let nKesin = N;
  let ww = w;
  let gucDeger = guc;
  if (bilinmeyen === 'n') {
    gerekli(w > 0, 'Etki büyüklüğü sıfır olamaz.');
    nKesin = artanCoz((x) => kiKareGucHesapla(w, df, x, alfa).guc, guc, 1 + 1e-10, 10, 1e9, ULASILAMAZ).x;
  } else {
    gerekli(N >= 1, 'Örneklem büyüklüğü en az 1 olmalıdır.');
    if (bilinmeyen === 'guc') gucDeger = kiKareGucHesapla(w, df, N, alfa).guc;
    else ww = pozitifEtkiCoz((x) => kiKareGucHesapla(x, df, N, alfa).guc, guc, 0.3);
  }
  const nTam = bilinmeyen === 'n' ? yukariYuvarla(nKesin, (m) => kiKareGucHesapla(ww, df, m, alfa).guc, guc, 1) : N;
  const son = kiKareGucHesapla(ww, df, nTam, alfa);
  return Object.assign(sonuc, {
    etki: { ad: 'Cohen w', sembol: 'w', deger: ww },
    guc: gucDeger,
    N: nTam,
    n: nTam,
    nKesin,
    toplamN: nTam,
    gercekGuc: bilinmeyen === 'n' ? son.guc : gucDeger,
    sd: son.sd,
    ncp: son.ncp,
    kritik: son.kritik,
  });
}

// ---------- Korelasyon ----------

/** pwr::pwr.r.test'in yaklaşık gücü (Fisher z, r/(2(n−1)) yanlılık düzeltmeli) */
function korelasyonGucHesapla(r, n, alfa, alternatif) {
  const tek = (rr, a) => {
    const ttt = qt(a, n - 2, false);
    const rc = Math.sqrt((ttt * ttt) / (ttt * ttt + n - 2));
    const zr = Math.atanh(rr) + rr / (2 * (n - 1));
    const zrc = Math.atanh(rc);
    return { zr, zrc, rc };
  };
  if (alternatif === 'iki-yonlu') {
    const { zr, zrc, rc } = tek(Math.abs(r), alfa / 2);
    const s = Math.sqrt(n - 3);
    return { guc: pnorm((zr - zrc) * s) + pnorm((-zr - zrc) * s), kritik: rc };
  }
  const { zr, zrc, rc } = tek(alternatif === 'kucuk' ? -r : r, alfa);
  return { guc: pnorm((zr - zrc) * Math.sqrt(n - 3)), kritik: alternatif === 'kucuk' ? -rc : rc };
}

/**
 * Pearson korelasyonu (H0: ρ = 0) için güç / örneklem / etki (r).
 * R: pwr::pwr.r.test(n, r, sig.level, power, alternative) — Fisher z dönüşümüne dayalı yaklaşık yöntem.
 * Not: G*Power "Exact — Correlation: Bivariate normal model" kesin dağılımı kullanır; sonuçlar çoğunlukla
 * aynı tam sayı n'yi verir ama birkaç gözlem farklılaşabilir.
 * @param {{r?:number|null, n?:number|null, guc?:number|null, alfa?:number, alternatif?:string}} girdi
 */
export function korelasyonGuc(girdi = {}) {
  const { r, n, guc, alfa = 0.05 } = girdi;
  const alternatif = alternatifOku(girdi.alternatif);
  alfaDenetle(alfa);
  gucDenetle(guc, alfa);
  if (!bos(r)) gerekli(r > -1 && r < 1, 'Korelasyon katsayısı −1 ile 1 arasında olmalıdır.');
  const bilinmeyen = bilinmeyenBul({ n, guc, etki: r });
  const sonuc = { yontem: 'guc-korelasyon', ad: 'Pearson korelasyonu', analiz: 'güç', cozulen: bilinmeyen, alternatif, alfa, uyarilar: [] };
  let nKesin = n;
  let rr = r;
  let gucDeger = guc;
  if (bilinmeyen === 'n') {
    gerekli(r !== 0, 'Etki büyüklüğü sıfır olamaz.');
    nKesin = artanCoz((x) => korelasyonGucHesapla(r, x, alfa, alternatif).guc, guc, 4 + 1e-10, 12, 1e9, ULASILAMAZ).x;
  } else {
    gerekli(n >= 4, 'Örneklem büyüklüğü en az 4 olmalıdır.');
    if (bilinmeyen === 'guc') gucDeger = korelasyonGucHesapla(r, n, alfa, alternatif).guc;
    else {
      const g = (x) => korelasyonGucHesapla(x, n, alfa, alternatif).guc;
      if (alternatif === 'iki-yonlu') rr = aralikCoz(g, guc, 1e-10, 1 - 1e-10, ETKI_BULUNAMADI);
      else if (alternatif === 'buyuk') rr = aralikCoz(g, guc, -1 + 1e-10, 1 - 1e-10, ETKI_BULUNAMADI);
      else rr = aralikCoz((x) => -g(x), -guc, -1 + 1e-10, 1 - 1e-10, ETKI_BULUNAMADI);
    }
  }
  if (alternatif === 'iki-yonlu') rr = Math.abs(rr);
  const nTam = bilinmeyen === 'n' ? yukariYuvarla(nKesin, (m) => korelasyonGucHesapla(rr, m, alfa, alternatif).guc, guc, 4) : n;
  const son = korelasyonGucHesapla(rr, nTam, alfa, alternatif);
  return Object.assign(sonuc, {
    etki: { ad: 'Pearson r', sembol: 'r', deger: rr },
    guc: gucDeger,
    n: nTam,
    nKesin,
    toplamN: nTam,
    gercekGuc: bilinmeyen === 'n' ? son.guc : gucDeger,
    kritik: son.kritik,
  });
}

// ---------- Oranlar ----------

/** Arksinüs (Cohen h) yaklaşımıyla güç; ölçek = h'nin çarpılacağı √(etkin n) */
function hGucHesapla(h, olcek, alfa, alternatif) {
  const hs = h * olcek;
  if (alternatif === 'iki-yonlu') {
    const ah = Math.abs(h) * olcek;
    return pnorm(qnorm(alfa / 2, 0, 1, false) - ah, 0, 1, false) + pnorm(qnorm(alfa / 2) - ah);
  }
  if (alternatif === 'buyuk') return pnorm(qnorm(alfa, 0, 1, false) - hs, 0, 1, false);
  return pnorm(qnorm(alfa) - hs);
}

/** Klasik normal yaklaşım (H0 altında birleştirilmiş varyans; Fleiss, Levin & Paik 2003, 4.2), isteğe bağlı süreklilik düzeltmesi */
function normalOranGucHesapla(p1, p2, n1, k, alfa, alternatif, sureklilik) {
  // k = n2 / n1
  const pbar = (p1 + k * p2) / (1 + k);
  const sh0 = Math.sqrt(pbar * (1 - pbar) * (1 + 1 / k));
  const sh1 = Math.sqrt(p1 * (1 - p1) + (p2 * (1 - p2)) / k);
  let delta = alternatif === 'iki-yonlu' ? Math.abs(p1 - p2) : alternatif === 'buyuk' ? p1 - p2 : p2 - p1;
  if (sureklilik) delta -= (1 + 1 / k) / (2 * n1);
  const z = alternatif === 'iki-yonlu' ? qnorm(alfa / 2, 0, 1, false) : qnorm(alfa, 0, 1, false);
  return pnorm((delta * Math.sqrt(n1) - z * sh0) / sh1);
}

/**
 * Normal yaklaşımda n1'in kapalı formu (Fleiss): n1 = [z_α·√(p̄q̄(1+1/k)) + z_β·√(p1q1 + p2q2/k)]² / δ²;
 * süreklilik düzeltmesiyle n1' = n1/4 · [1 + √(1 + 2(k+1)/(k·n1·|δ|))]².
 */
function normalOranN(p1, p2, k, alfa, guc, alternatif, sureklilik) {
  const pbar = (p1 + k * p2) / (1 + k);
  const sh0 = Math.sqrt(pbar * (1 - pbar) * (1 + 1 / k));
  const sh1 = Math.sqrt(p1 * (1 - p1) + (p2 * (1 - p2)) / k);
  const delta = alternatif === 'iki-yonlu' ? Math.abs(p1 - p2) : alternatif === 'buyuk' ? p1 - p2 : p2 - p1;
  if (!(delta > 0)) throw new IstatistikHatasi(ULASILAMAZ);
  const za = alternatif === 'iki-yonlu' ? qnorm(alfa / 2, 0, 1, false) : qnorm(alfa, 0, 1, false);
  const zb = qnorm(guc);
  const n = ((za * sh0 + zb * sh1) / delta) ** 2;
  if (!sureklilik) return n;
  return (n / 4) * (1 + Math.sqrt(1 + (2 * (k + 1)) / (k * n * delta))) ** 2;
}

/** Cohen h = 2·asin(√p1) − 2·asin(√p2) (R: pwr::ES.h) */
export function hdenOranlar(p1, p2) {
  gerekli(p1 >= 0 && p1 <= 1 && p2 >= 0 && p2 <= 1, 'Oranlar 0 ile 1 arasında olmalıdır.');
  return 2 * Math.asin(Math.sqrt(p1)) - 2 * Math.asin(Math.sqrt(p2));
}

/**
 * Bağımsız iki oranın karşılaştırılması için güç / örneklem / etki.
 * yontem:
 *  'arcsin' (varsayılan): Cohen h ile arksinüs dönüşümü — R pwr::pwr.2p.test (n1 = n2) / pwr.2p2n.test (n1 ≠ n2).
 *     Güç: 1 − Φ(z_{α/2} − h√(n1n2/(n1+n2))) + Φ(−z_{α/2} − h√(·)).
 *  'normal': klasik normal yaklaşım, H0 altında birleştirilmiş varyans (Fleiss, Levin & Paik 2003):
 *     n1 = [z_{α/2}√(p̄q̄(1+1/k)) + z_β√(p1q1 + p2q2/k)]² / (p1−p2)², p̄ = (p1 + k·p2)/(1+k).
 *     k = 1 iken R power.prop.test(strict = FALSE) ile aynı (iki yönlü testte karşı kuyruk ihmal edilir).
 *  'normal-sureklilik': Fleiss süreklilik düzeltmesi: n1' = n1/4·[1 + √(1 + 2(k+1)/(k·n1·|p1−p2|))]²;
 *     güç: |p1−p2| yerine |p1−p2| − (1/n1 + 1/n2)/2 kullanılır (iki formül birbirinin tersidir).
 * n: 1. grubun büyüklüğü; n2 = oran·n (oran = n2/n1, varsayılan 1) ya da doğrudan `n2` verilebilir.
 * Etki: p1 ve p2 verilir. Çözülecek etki için p2 boş bırakılır (p1 verilir): 'buyuk' (H1: p1 > p2) için p2 < p1,
 * diğerlerinde p2 > p1 aranır. 'arcsin' yönteminde p1/p2 yerine doğrudan `h` de verilebilir; p1, p2 ve h boşsa h çözülür.
 * @param {{p1?:number|null, p2?:number|null, h?:number|null, n?:number|null, n2?:number|null, oran?:number,
 *          guc?:number|null, alfa?:number, alternatif?:string, yontem?:'arcsin'|'normal'|'normal-sureklilik'}} girdi
 */
export function ikiOranGuc(girdi = {}) {
  const { p1, p2, n, guc, alfa = 0.05 } = girdi;
  const yontem = girdi.yontem ?? 'arcsin';
  gerekli(['arcsin', 'normal', 'normal-sureklilik'].includes(yontem), "Yöntem 'arcsin', 'normal' ya da 'normal-sureklilik' olmalıdır.");
  const alternatif = alternatifOku(girdi.alternatif);
  alfaDenetle(alfa);
  gucDenetle(guc, alfa);
  let oran = girdi.oran ?? 1;
  if (!bos(girdi.n2)) {
    gerekli(!bos(n), 'n2 verildiğinde n (1. grup) de verilmelidir.');
    oran = girdi.n2 / n;
  }
  gerekli(oran > 0, 'Grup oranı (n2/n1) pozitif olmalıdır.');
  for (const p of [p1, p2]) if (!bos(p)) gerekli(p > 0 && p < 1, 'Oranlar 0 ile 1 arasında (uçlar hariç) olmalıdır.');
  const hDogrudan = yontem === 'arcsin' && bos(p1) && bos(p2);
  const etkiDegeri = hDogrudan ? girdi.h : bos(p1) || bos(p2) ? null : yontem === 'arcsin' ? hdenOranlar(p1, p2) : p1 - p2;
  if (!hDogrudan) gerekli(!bos(p1), 'p1 verilmelidir.');
  const bilinmeyen = bilinmeyenBul({ n, guc, etki: etkiDegeri });
  const sonuc = {
    yontem: `guc-iki-oran-${yontem}`,
    ad: yontem === 'arcsin' ? 'İki oranın karşılaştırılması (arksinüs, Cohen h)' : yontem === 'normal' ? 'İki oranın karşılaştırılması (normal yaklaşım)' : 'İki oranın karşılaştırılması (normal yaklaşım, süreklilik düzeltmeli)',
    analiz: 'güç',
    cozulen: bilinmeyen,
    alternatif,
    alfa,
    uyarilar: [],
  };
  // Güç fonksiyonu: (n1, p2 ya da h) → güç
  const gucFn = (n1, e) => {
    if (yontem === 'arcsin') {
      const h = hDogrudan ? e : hdenOranlar(p1, e);
      const n2 = oran * n1;
      return hGucHesapla(h, Math.sqrt((n1 * n2) / (n1 + n2)), alfa, alternatif);
    }
    return normalOranGucHesapla(p1, e, n1, oran, alfa, alternatif, yontem === 'normal-sureklilik');
  };
  const eGirdi = hDogrudan ? girdi.h : p2;
  let e = eGirdi;
  let nKesin = n;
  let gucDeger = guc;
  if (bilinmeyen === 'n') {
    gerekli(etkiDegeri !== 0, 'Etki büyüklüğü sıfır olamaz (p1 = p2).');
    if (yontem === 'arcsin') nKesin = artanCoz((x) => gucFn(x, e), guc, 2 + 1e-10, 10, 1e9, ULASILAMAZ).x;
    else nKesin = normalOranN(p1, p2, oran, alfa, guc, alternatif, yontem === 'normal-sureklilik');
  } else {
    gerekli(n >= 2, 'Grup büyüklüğü en az 2 olmalıdır.');
    if (bilinmeyen === 'guc') gucDeger = gucFn(n, e);
    else if (hDogrudan) e = dCoz((x) => gucFn(n, x), guc, alternatif);
    else if (alternatif === 'buyuk') e = aralikCoz((x) => -gucFn(n, x), -guc, 1e-12, p1 - 1e-12, ETKI_BULUNAMADI);
    else e = aralikCoz((x) => gucFn(n, x), guc, p1 + 1e-12, 1 - 1e-12, ETKI_BULUNAMADI);
  }
  if (hDogrudan && alternatif === 'iki-yonlu') e = Math.abs(e);
  let n1 = n;
  let n2 = bos(n) ? null : oran * n;
  if (bilinmeyen === 'n') {
    const gFn = (a, b) =>
      yontem === 'arcsin'
        ? hGucHesapla(hDogrudan ? e : hdenOranlar(p1, e), Math.sqrt((a * b) / (a + b)), alfa, alternatif)
        : normalOranGucHesapla(p1, e, a, b / a, alfa, alternatif, yontem === 'normal-sureklilik');
    [n1, n2] = ikiGrupYuvarla(nKesin, oran, gFn, guc, yontem === 'arcsin' ? 2 : 1);
  }
  const h = hDogrudan ? e : hdenOranlar(p1, e);
  const p2Son = hDogrudan ? null : e;
  const gercek = bilinmeyen === 'n'
    ? yontem === 'arcsin'
      ? hGucHesapla(h, Math.sqrt((n1 * n2) / (n1 + n2)), alfa, alternatif)
      : normalOranGucHesapla(p1, e, n1, n2 / n1, alfa, alternatif, yontem === 'normal-sureklilik')
    : gucDeger;
  return Object.assign(sonuc, {
    etki: yontem === 'arcsin' ? { ad: 'Cohen h', sembol: 'h', deger: h } : { ad: 'Oran farkı', sembol: 'p₁ − p₂', deger: p1 - e },
    p1: hDogrudan ? null : p1,
    p2: p2Son,
    h,
    guc: gucDeger,
    n: n1,
    nKesin,
    n1,
    n2,
    oran,
    toplamN: n1 + n2,
    gercekGuc: gercek,
  });
}

/**
 * Tek oranın sabit bir değerle (p0) karşılaştırılması — arksinüs (Cohen h) yaklaşımı.
 * R: pwr::pwr.p.test(h = ES.h(p, p0), n, sig.level, power, alternative). Güç h·√n ile hesaplanır.
 * Etki p ve p0 ile ya da doğrudan h ile verilir; çözülecek etki için h (ve p) boş bırakılır.
 * @param {{p?:number|null, p0?:number, h?:number|null, n?:number|null, guc?:number|null, alfa?:number, alternatif?:string}} girdi
 */
export function tekOranGuc(girdi = {}) {
  const { p, p0, n, guc, alfa = 0.05 } = girdi;
  const alternatif = alternatifOku(girdi.alternatif);
  alfaDenetle(alfa);
  gucDenetle(guc, alfa);
  let h = girdi.h;
  if (bos(h) && !bos(p)) {
    gerekli(!bos(p0), 'Karşılaştırılacak oran (p0) verilmelidir.');
    h = hdenOranlar(p, p0);
  }
  const bilinmeyen = bilinmeyenBul({ n, guc, etki: h });
  const sonuc = { yontem: 'guc-tek-oran', ad: 'Tek oran testi (arksinüs, Cohen h)', analiz: 'güç', cozulen: bilinmeyen, alternatif, alfa, uyarilar: [] };
  const fn = (m, hh) => hGucHesapla(hh, Math.sqrt(m), alfa, alternatif);
  let nKesin = n;
  let gucDeger = guc;
  if (bilinmeyen === 'n') {
    gerekli(h !== 0, 'Etki büyüklüğü sıfır olamaz.');
    nKesin = artanCoz((x) => fn(x, h), guc, 2 + 1e-10, 10, 1e9, ULASILAMAZ).x;
  } else {
    gerekli(n >= 1, 'Örneklem büyüklüğü en az 1 olmalıdır.');
    if (bilinmeyen === 'guc') gucDeger = fn(n, h);
    else h = dCoz((x) => fn(n, x), guc, alternatif);
  }
  if (alternatif === 'iki-yonlu') h = Math.abs(h);
  const nTam = bilinmeyen === 'n' ? yukariYuvarla(nKesin, (m) => fn(m, h), guc, 2) : n;
  let pCozum = p ?? null;
  if (bilinmeyen === 'etki' && !bos(p0)) {
    // h = 2asin√p − 2asin√p0 → p
    const aci = h / 2 + Math.asin(Math.sqrt(p0));
    pCozum = aci > 0 && aci < Math.PI / 2 ? Math.sin(aci) ** 2 : null;
  }
  return Object.assign(sonuc, {
    etki: { ad: 'Cohen h', sembol: 'h', deger: h },
    p: pCozum,
    p0: p0 ?? null,
    guc: gucDeger,
    n: nTam,
    nKesin,
    toplamN: nTam,
    gercekGuc: bilinmeyen === 'n' ? fn(nTam, h) : gucDeger,
  });
}

// ---------- Lojistik regresyon (Hsieh) ----------

function oddsOrandanP2(p1, or) {
  const o = (or * p1) / (1 - p1);
  return o / (1 + o);
}

/**
 * Lojistik regresyonda tek yordayıcı için güç / örneklem (Hsieh, Bloch & Larsen 1998; formüle dayalı).
 * tip 'ikili' (iki değerli yordayıcı): p1 = X = 0 grubunda olay olasılığı, p2 = X = 1 grubunda (ya da OR verilir),
 *   B = X = 1 olanların oranı. n = [z_α√(p̄q̄/B) + z_β√(p1q1 + p2q2(1−B)/B)]² / [(p1−p2)²(1−B)], p̄ = (1−B)p1 + B·p2.
 *   (R: powerMediation::SSizeLogisticBin / powerLogisticBin)
 * tip 'surekli' (normal dağılımlı yordayıcı): p1 = X'in ortalamasındaki olay olasılığı, OR = 1 SS artış için odds oranı.
 *   n = (z_α + z_β)² / [p1(1−p1)(ln OR)²]  (Hsieh 1998, eş. 1; R: powerMediation::SSizeLogisticCon / powerLogisticCon)
 * r2: yordayıcının modeldeki diğer değişkenlerle R²'si — n, 1/(1−R²) varyans şişme çarpanıyla büyütülür (Hsieh 1998).
 * İki yönlü testte z_α = z_{1−α/2}; karşı kuyruk ihmal edilir (kaynak formüller gibi).
 * Not: G*Power'ın varsayılan lojistik yöntemi (Demidenko 2007) farklıdır; G*Power'da "Hsieh et al. (1998)" seçilmelidir.
 * Çözülecek etki: OR (ikili tipte p2 de OR'dan türetilir); OR > 1 aranır ('kucuk' alternatifte OR < 1).
 * @param {{tip?:'ikili'|'surekli', p1:number, p2?:number|null, OR?:number|null, B?:number, r2?:number,
 *          n?:number|null, guc?:number|null, alfa?:number, alternatif?:string}} girdi
 */
export function lojistikGuc(girdi = {}) {
  const { tip = 'surekli', p1, n, guc, alfa = 0.05, r2 = 0 } = girdi;
  const B = girdi.B ?? 0.5;
  const alternatif = alternatifOku(girdi.alternatif);
  gerekli(tip === 'ikili' || tip === 'surekli', "Yordayıcı tipi 'ikili' ya da 'surekli' olmalıdır.");
  alfaDenetle(alfa);
  gucDenetle(guc, alfa);
  gerekli(p1 > 0 && p1 < 1, 'Olay olasılığı (p1) 0 ile 1 arasında olmalıdır.');
  gerekli(r2 >= 0 && r2 < 1, 'R² 0 ile 1 arasında olmalıdır.');
  if (tip === 'ikili') gerekli(B > 0 && B < 1, 'B (X = 1 olanların oranı) 0 ile 1 arasında olmalıdır.');
  let OR = girdi.OR;
  if (tip === 'ikili' && bos(OR) && !bos(girdi.p2)) {
    gerekli(girdi.p2 > 0 && girdi.p2 < 1, 'p2 0 ile 1 arasında olmalıdır.');
    OR = (girdi.p2 / (1 - girdi.p2)) / (p1 / (1 - p1));
  }
  if (!bos(OR)) gerekli(OR > 0, 'Odds oranı pozitif olmalıdır.');
  const bilinmeyen = bilinmeyenBul({ n, guc, etki: OR });
  const za = alternatif === 'iki-yonlu' ? qnorm(alfa / 2, 0, 1, false) : qnorm(alfa, 0, 1, false);
  const vif = 1 / (1 - r2);
  // ham (VIF'siz) n için güç
  const gucHam = (m, or) => {
    if (tip === 'surekli') {
      const b = Math.log(or);
      const etkin = alternatif === 'kucuk' ? -b : alternatif === 'buyuk' ? b : Math.abs(b);
      return pnorm(Math.sqrt(m * p1 * (1 - p1)) * etkin - za);
    }
    const p2 = oddsOrandanP2(p1, or);
    const pbar = (1 - B) * p1 + B * p2;
    const fark = alternatif === 'kucuk' ? p1 - p2 : alternatif === 'buyuk' ? p2 - p1 : Math.abs(p2 - p1);
    const a = za * Math.sqrt((pbar * (1 - pbar)) / B);
    const s = Math.sqrt(p1 * (1 - p1) + (p2 * (1 - p2) * (1 - B)) / B);
    return pnorm((fark * Math.sqrt(m * (1 - B)) - a) / s);
  };
  const gucFn = (m, or) => gucHam(m / vif, or);
  const nHam = (or) => {
    const zb = qnorm(guc);
    if (tip === 'surekli') return (za + zb) ** 2 / (p1 * (1 - p1) * Math.log(or) ** 2);
    const p2 = oddsOrandanP2(p1, or);
    const pbar = (1 - B) * p1 + B * p2;
    return (za * Math.sqrt((pbar * (1 - pbar)) / B) + zb * Math.sqrt(p1 * (1 - p1) + (p2 * (1 - p2) * (1 - B)) / B)) ** 2 / ((p1 - p2) ** 2 * (1 - B));
  };
  const sonuc = {
    yontem: `guc-lojistik-${tip}`,
    ad: tip === 'ikili' ? 'Lojistik regresyon, ikili yordayıcı (Hsieh 1998)' : 'Lojistik regresyon, sürekli yordayıcı (Hsieh 1998)',
    analiz: 'güç',
    cozulen: bilinmeyen,
    alternatif,
    alfa,
    uyarilar: [],
  };
  let nKesin = n;
  let gucDeger = guc;
  if (bilinmeyen === 'n') {
    gerekli(OR !== 1, 'Odds oranı 1 olamaz (etki yok).');
    const yonDogru = alternatif === 'iki-yonlu' || (alternatif === 'buyuk' ? OR > 1 : OR < 1);
    if (!yonDogru) throw new IstatistikHatasi(ULASILAMAZ);
    nKesin = nHam(OR) * vif;
  } else {
    gerekli(n > 0, 'Örneklem büyüklüğü pozitif olmalıdır.');
    if (bilinmeyen === 'guc') gucDeger = gucFn(n, OR);
    else {
      // ln OR üzerinde çöz
      const g = (x) => gucFn(n, Math.exp(x));
      OR = Math.exp(alternatif === 'kucuk' ? azalanCozAsagi(g, guc, 0, -0.5, -50, ETKI_BULUNAMADI).x : artanCoz(g, guc, 0, 0.5, 50, ETKI_BULUNAMADI).x);
    }
  }
  const nTam = bilinmeyen === 'n' ? yukariYuvarla(nKesin, (m) => gucFn(m, OR), guc, 1) : n;
  return Object.assign(sonuc, {
    tip,
    etki: { ad: 'Odds oranı', sembol: 'OR', deger: OR },
    p1,
    p2: tip === 'ikili' ? oddsOrandanP2(p1, OR) : null,
    B: tip === 'ikili' ? B : null,
    r2,
    guc: gucDeger,
    n: nTam,
    nKesin,
    toplamN: nTam,
    gercekGuc: bilinmeyen === 'n' ? gucFn(nTam, OR) : gucDeger,
  });
}

// ---------- Sağkalım (log-rank / Cox) ----------

/**
 * Sağkalım analizi (log-rank testi / Cox regresyonunda iki grup) için gerekli olay sayısı, toplam örneklem ve güç.
 * yontem 'schoenfeld' (varsayılan; Schoenfeld 1983): D = (z_α + z_β)² / [π(1−π)(ln HR)²], π = n1/(n1+n2).
 *   (R: gsDesign::nEvents ile aynı)
 * yontem 'freedman' (Freedman 1982): D = (z_α + z_β)²·[(1 + k·HR)/(1 − HR)]² / k, k = n1/n2.
 *   (R: powerSurvEpi::ssizeCT.default ile aynı)
 * HR = 1. grubun (deney/maruz) 2. gruba (kontrol) göre risk oranı. oran = n2/n1 (varsayılan 1).
 * Toplam örneklem = D / P(olay); olayOlasiligi tek sayı (genel olasılık) ya da [p1, p2] (gruplara göre; ağırlıklı
 * ortalaması alınır). Grup büyüklükleri ayrı ayrı yukarı yuvarlanır: n1 = ⌈D/(p1 + oran·p2)⌉, n2 = ⌈oran·D/(p1 + oran·p2)⌉.
 * Güç: Φ(√(D·π(1−π))·|ln HR| − z_α) (karşı kuyruk ihmal edilir). Güç hesabı için olaySayisi ya da N + olayOlasiligi verilir.
 * Etki çözülürken HR < 1 olan çözüm `etki.deger`, HR > 1 olan `hrUst` alanında döner.
 * @param {{hr?:number|null, olaySayisi?:number|null, N?:number|null, olayOlasiligi?:number|number[],
 *          oran?:number, guc?:number|null, alfa?:number, alternatif?:string, yontem?:'schoenfeld'|'freedman'}} girdi
 */
export function sagkalimGuc(girdi = {}) {
  const { hr, guc, alfa = 0.05, oran = 1 } = girdi;
  const yontem = girdi.yontem ?? 'schoenfeld';
  gerekli(yontem === 'schoenfeld' || yontem === 'freedman', "Yöntem 'schoenfeld' ya da 'freedman' olmalıdır.");
  const alternatif = alternatifOku(girdi.alternatif);
  alfaDenetle(alfa);
  gucDenetle(guc, alfa);
  gerekli(oran > 0, 'Grup oranı (n2/n1) pozitif olmalıdır.');
  if (!bos(hr)) gerekli(hr > 0, 'Risk oranı (HR) pozitif olmalıdır.');
  let P = null;
  let pOlay = null;
  if (!bos(girdi.olayOlasiligi)) {
    pOlay = Array.isArray(girdi.olayOlasiligi) ? girdi.olayOlasiligi : [girdi.olayOlasiligi, girdi.olayOlasiligi];
    gerekli(pOlay.length === 2 && pOlay.every((v) => v > 0 && v <= 1), 'Olay olasılığı 0 ile 1 arasında olmalıdır.');
    P = (pOlay[0] + oran * pOlay[1]) / (1 + oran);
  }
  let D = girdi.olaySayisi;
  if (bos(D) && !bos(girdi.N)) {
    gerekli(P !== null, 'Toplam örneklemden güç hesaplamak için olay olasılığı verilmelidir.');
    D = girdi.N * P;
  }
  const bilinmeyen = bilinmeyenBul({ n: D, guc, etki: hr });
  const pi = 1 / (1 + oran);
  const k = 1 / oran; // Freedman: n1/n2
  const za = alternatif === 'iki-yonlu' ? qnorm(alfa / 2, 0, 1, false) : qnorm(alfa, 0, 1, false);
  const etkinLog = (h) => {
    const b = Math.log(h);
    return alternatif === 'kucuk' ? -b : alternatif === 'buyuk' ? b : Math.abs(b);
  };
  const gucFn = (d, h) => {
    if (yontem === 'schoenfeld') return pnorm(Math.sqrt(d * pi * (1 - pi)) * etkinLog(h) - za);
    // Freedman: z_β = √(d·k)·|HR − 1|/(k·HR + 1) − z_α
    const isaret = alternatif === 'kucuk' ? -1 : alternatif === 'buyuk' ? 1 : null;
    const fark = isaret === null ? Math.abs(h - 1) : isaret * (h - 1);
    return pnorm((Math.sqrt(d * k) * fark) / (k * h + 1) - za);
  };
  const sonuc = {
    yontem: `guc-sagkalim-${yontem}`,
    ad: yontem === 'schoenfeld' ? 'Sağkalım analizi: log-rank / Cox (Schoenfeld)' : 'Sağkalım analizi: log-rank (Freedman)',
    analiz: 'güç',
    cozulen: bilinmeyen,
    alternatif,
    alfa,
    oran,
    uyarilar: [],
  };
  let DKesin = D;
  let h = hr;
  let gucDeger = guc;
  let hrUst = null;
  if (bilinmeyen === 'n') {
    gerekli(hr !== 1, 'Risk oranı 1 olamaz (etki yok).');
    const zb = qnorm(guc);
    if (etkinLog(hr) <= 0) throw new IstatistikHatasi(ULASILAMAZ);
    DKesin = yontem === 'schoenfeld' ? (za + zb) ** 2 / (pi * (1 - pi) * Math.log(hr) ** 2) : ((za + zb) ** 2 * ((1 + k * hr) / (1 - hr)) ** 2) / k;
  } else {
    gerekli(D > 0, 'Olay sayısı pozitif olmalıdır.');
    if (bilinmeyen === 'guc') gucDeger = gucFn(D, hr);
    else {
      const g = (x) => gucFn(D, Math.exp(x));
      // Freedman'da güç HR → 0 ya da HR → ∞ iken sınırlıdır; bir yönde çözüm olmayabilir.
      const dene = (f) => {
        try {
          return f();
        } catch (e) {
          if (e instanceof IstatistikHatasi) return null;
          throw e;
        }
      };
      const alt = alternatif !== 'buyuk' ? dene(() => azalanCozAsagi(g, guc, 0, -0.5, -50, ETKI_BULUNAMADI).x) : null;
      const ust = alternatif !== 'kucuk' ? dene(() => artanCoz(g, guc, 0, 0.5, 50, ETKI_BULUNAMADI).x) : null;
      if (alt === null && ust === null) throw new IstatistikHatasi(ETKI_BULUNAMADI);
      h = alt !== null ? Math.exp(alt) : Math.exp(ust);
      hrUst = alt !== null && ust !== null ? Math.exp(ust) : null;
    }
  }
  const DTam = bilinmeyen === 'n' ? yukariYuvarla(DKesin, (m) => gucFn(m, h), guc, 1) : D;
  Object.assign(sonuc, {
    etki: { ad: 'Risk oranı', sembol: 'HR', deger: h },
    hrUst,
    guc: gucDeger,
    olaySayisi: DTam,
    olaySayisiKesin: DKesin,
    gercekGuc: bilinmeyen === 'n' ? gucFn(DTam, h) : gucDeger,
    olayOlasiligi: P,
  });
  if (P !== null) {
    const payda = pOlay[0] + oran * pOlay[1];
    const n1Kesin = DKesin / payda;
    const n2Kesin = (oran * DKesin) / payda;
    const yuv = (x) => Math.ceil(x - 1e-9 * Math.max(1, x));
    const n1 = bilinmeyen === 'n' ? yuv(n1Kesin) : n1Kesin;
    const n2 = bilinmeyen === 'n' ? yuv(n2Kesin) : n2Kesin;
    Object.assign(sonuc, { n1, n2, n1Kesin, n2Kesin, toplamN: n1 + n2, nKesin: n1Kesin + n2Kesin, n: n1 + n2 });
  }
  return sonuc;
}

// ---------- Etki büyüklüğü yardımcıları ----------

/**
 * Cohen d: iki grup ortalaması farkı / birleştirilmiş SS.
 * n1 ve n2 verilirse sp = √[((n1−1)s1² + (n2−1)s2²)/(n1+n2−2)]; verilmezse sp = √[(s1² + s2²)/2] (G*Power "Determine").
 */
export function dHesapla(m1, s1, n1, m2, s2, n2) {
  gerekli(s1 > 0 && s2 > 0, 'Standart sapmalar pozitif olmalıdır.');
  let sp;
  if (bos(n1) || bos(n2)) sp = Math.sqrt((s1 * s1 + s2 * s2) / 2);
  else {
    gerekli(n1 + n2 > 2, 'Toplam örneklem 2\'den büyük olmalıdır.');
    sp = Math.sqrt(((n1 - 1) * s1 * s1 + (n2 - 1) * s2 * s2) / (n1 + n2 - 2));
  }
  return (m1 - m2) / sp;
}

/** Cohen f = √(η²/(1 − η²)) (kısmi η² için de aynı dönüşüm) */
export function fdenEta2(eta2) {
  gerekli(eta2 >= 0 && eta2 < 1, 'η² 0 ile 1 arasında olmalıdır.');
  return Math.sqrt(eta2 / (1 - eta2));
}

/**
 * Cohen w = √(Σ (p1 − p0)² / p0) (R: pwr::ES.w1). p0: H0 altındaki, p1: H1 altındaki hücre olasılıkları.
 */
export function wdenOranlar(p0, p1) {
  gerekli(Array.isArray(p0) && Array.isArray(p1) && p0.length === p1.length && p0.length >= 2, 'Olasılık dizileri aynı uzunlukta olmalıdır (en az 2).');
  let s = 0;
  for (let i = 0; i < p0.length; i++) {
    gerekli(p0[i] > 0, 'H0 olasılıkları pozitif olmalıdır.');
    s += (p1[i] - p0[i]) ** 2 / p0[i];
  }
  return Math.sqrt(s);
}

/**
 * Çapraz tablo olasılıklarından Cohen w (R: pwr::ES.w2): w = √(Σ (P_ij − P_i.P_.j)² / (P_i.P_.j)).
 * Tablo frekans ya da olasılık olabilir (toplama bölünür).
 */
export function wdenTablo(tablo) {
  let toplam = 0;
  for (const s of tablo) for (const v of s) toplam += v;
  gerekli(toplam > 0, 'Tablo toplamı pozitif olmalıdır.');
  const P = tablo.map((s) => s.map((v) => v / toplam));
  const sat = P.map((s) => s.reduce((a, b) => a + b, 0));
  const sut = P[0].map((_, j) => P.reduce((a, s) => a + s[j], 0));
  let w = 0;
  for (let i = 0; i < P.length; i++) {
    for (let j = 0; j < P[0].length; j++) {
      const e = sat[i] * sut[j];
      if (e > 0) w += (P[i][j] - e) ** 2 / e;
    }
  }
  return Math.sqrt(w);
}

/**
 * Cohen f² = R² / (1 − R²). İkinci argüman verilirse R² artışı için f² = (R²_tam − R²_indirgenmiş)/(1 − R²_tam).
 */
export function f2denR2(r2, r2Indirgenmis = null) {
  gerekli(r2 >= 0 && r2 < 1, 'R² 0 ile 1 arasında olmalıdır.');
  if (r2Indirgenmis === null || r2Indirgenmis === undefined) return r2 / (1 - r2);
  gerekli(r2Indirgenmis >= 0 && r2Indirgenmis <= r2, 'İndirgenmiş modelin R²\'si tam modelinkinden büyük olamaz.');
  return (r2 - r2Indirgenmis) / (1 - r2);
}
