// Varyans homojenliği testleri: Levene (ortalama / medyan = Brown-Forsythe), Bartlett, Fligner-Killeen
// ve iki varyansın oranı için F testi. Gruplar eksik değerlerden arındırılmış number[][] olarak verilir.

import { pchisq, pf, qf, qnorm } from './dagilim.js';
import { medyan, ortalama, siralar, toplam, varyans } from './betimsel.js';
import { IstatistikHatasi } from './hata.js';

function gruplariDenetle(gruplar, testAdi, enAzGrupBoyu = 1) {
  if (!Array.isArray(gruplar) || gruplar.length < 2) throw new IstatistikHatasi(`${testAdi} için en az 2 grup gerekir.`);
  for (const g of gruplar) {
    if (!g || g.length < enAzGrupBoyu) {
      throw new IstatistikHatasi(enAzGrupBoyu > 1 ? `${testAdi} için her grupta en az ${enAzGrupBoyu} gözlem gerekir.` : `${testAdi}: boş grup var.`);
    }
    for (const v of g) {
      if (typeof v !== 'number' || !Number.isFinite(v)) throw new IstatistikHatasi(`${testAdi}: veride eksik ya da sayısal olmayan değer var.`);
    }
  }
}

/** Tek yönlü ANOVA F'si (yalnızca istatistik; Levene için iç yardımcı) */
function anovaF(gruplar) {
  const k = gruplar.length;
  const N = gruplar.reduce((s, g) => s + g.length, 0);
  const ortalamalar = gruplar.map((g) => ortalama(g));
  const genel = ortalama(gruplar.flat());
  const KTa = toplam(gruplar.map((g, i) => g.length * (ortalamalar[i] - genel) ** 2));
  const KTi = toplam(gruplar.flatMap((g, i) => g.map((v) => (v - ortalamalar[i]) ** 2)));
  return { F: KTa / (k - 1) / (KTi / (N - k)), KTa, KTi, sd1: k - 1, sd2: N - k, N };
}

/**
 * Levene varyans homojenliği testi: |x − merkez| sapmaları üzerinde tek yönlü ANOVA.
 * merkez = 'ortalama' (varsayılan): SPSS "Based on Mean" = car::leveneTest(center = mean).
 * merkez = 'medyan': Brown-Forsythe (1974) uyarlaması = SPSS "Based on Median" = car::leveneTest(center = median)
 * (R'deki car paketinin varsayılanı budur; SPSS'inki ortalamadır).
 * @param {number[][]} gruplar
 * @param {{merkez?: 'ortalama'|'medyan'}} [secenek]
 */
export function levene(gruplar, secenek = {}) {
  const merkez = secenek.merkez ?? 'ortalama';
  if (merkez !== 'ortalama' && merkez !== 'medyan') throw new IstatistikHatasi(`Bilinmeyen Levene merkezi: ${merkez}`);
  gruplariDenetle(gruplar, 'Levene testi');
  const N = gruplar.reduce((s, g) => s + g.length, 0);
  if (N <= gruplar.length) throw new IstatistikHatasi('Levene testi için toplam gözlem sayısı grup sayısından büyük olmalıdır.');
  const sapmalar = gruplar.map((g) => {
    const c = merkez === 'ortalama' ? ortalama(g) : medyan(g);
    return g.map((v) => Math.abs(v - c));
  });
  const a = anovaF(sapmalar);
  let F = a.F;
  if (a.KTi === 0) {
    if (a.KTa === 0) throw new IstatistikHatasi('Levene testi hesaplanamıyor: tüm gruplarda sapmalar sabit.');
    F = Infinity;
  }
  return {
    yontem: merkez === 'ortalama' ? 'levene' : 'brown-forsythe-levene',
    ad: merkez === 'ortalama' ? 'Levene testi (ortalamaya göre)' : 'Levene testi (medyana göre, Brown-Forsythe)',
    istatistik: F,
    istatistikAdi: 'F',
    sd: [a.sd1, a.sd2],
    p: pf(F, a.sd1, a.sd2, false),
    merkez,
    n: N,
    uyarilar: [],
  };
}

/**
 * Bartlett varyans homojenliği testi (R bartlett.test ile aynı). Normallik varsayımına duyarlıdır.
 * Her grupta en az 2 gözlem ve sıfırdan büyük varyans gerekir.
 * @param {number[][]} gruplar
 */
export function bartlett(gruplar) {
  gruplariDenetle(gruplar, 'Bartlett testi', 2);
  const k = gruplar.length;
  const n = gruplar.map((g) => g.length - 1);
  const v = gruplar.map((g) => varyans(g));
  if (v.some((x) => !(x > 0))) throw new IstatistikHatasi('Bartlett testi: varyansı sıfır olan grup var.');
  const nToplam = n.reduce((s, x) => s + x, 0);
  const vToplam = toplam(n.map((ni, i) => ni * v[i])) / nToplam;
  const pay = nToplam * Math.log(vToplam) - toplam(n.map((ni, i) => ni * Math.log(v[i])));
  const payda = 1 + (toplam(n.map((ni) => 1 / ni)) - 1 / nToplam) / (3 * (k - 1));
  const K2 = pay / payda;
  return {
    yontem: 'bartlett',
    ad: 'Bartlett testi',
    istatistik: K2,
    istatistikAdi: 'χ²',
    sd: k - 1,
    p: pchisq(K2, k - 1, false),
    n: nToplam + k,
    uyarilar: [],
  };
}

/**
 * Fligner-Killeen (medyan merkezli) varyans homojenliği testi; R fligner.test ile aynı.
 * Normal olmayan dağılımlara karşı dayanıklıdır.
 * @param {number[][]} gruplar
 */
export function fligner(gruplar) {
  gruplariDenetle(gruplar, 'Fligner-Killeen testi');
  const k = gruplar.length;
  const merkezli = gruplar.flatMap((g) => {
    const m = medyan(g);
    return g.map((v) => Math.abs(v - m));
  });
  const n = merkezli.length;
  if (n < 2) throw new IstatistikHatasi('Fligner-Killeen testi için yeterli gözlem yok.');
  const { sira } = siralar(merkezli);
  const a = Array.from(sira, (r) => qnorm((1 + r / (n + 1)) / 2));
  const ort = ortalama(a);
  const ac = a.map((v) => v - ort);
  const s2 = toplam(ac.map((v) => v * v)) / (n - 1);
  if (!(s2 > 0)) throw new IstatistikHatasi('Fligner-Killeen testi hesaplanamıyor: tüm mutlak sapmalar eşit.');
  let bas = 0;
  let toplamPay = 0;
  for (const g of gruplar) {
    const parca = ac.slice(bas, bas + g.length);
    bas += g.length;
    const m = ortalama(parca);
    toplamPay += g.length * m * m;
  }
  const istatistik = toplamPay / s2;
  return {
    yontem: 'fligner-killeen',
    ad: 'Fligner-Killeen testi',
    istatistik,
    istatistikAdi: 'χ²',
    sd: k - 1,
    p: pchisq(istatistik, k - 1, false),
    n,
    uyarilar: [],
  };
}

/**
 * İki varyansın oranı için F testi (R var.test ile aynı). F = (s²x / s²y) / oran.
 * @param {number[]} x
 * @param {number[]} y
 * @param {{oran?:number, alternatif?:'iki'|'buyuk'|'kucuk', duzey?:number}} [secenek]
 */
export function varyansOraniF(x, y, secenek = {}) {
  const oran = secenek.oran ?? 1;
  const alternatif = secenek.alternatif ?? 'iki';
  const duzey = secenek.duzey ?? 0.95;
  gruplariDenetle([x, y], 'Varyans oranı F testi', 2);
  if (!(oran > 0) || !Number.isFinite(oran)) throw new IstatistikHatasi('Varyans oranı pozitif bir sayı olmalıdır.');
  if (!(duzey > 0 && duzey < 1)) throw new IstatistikHatasi('Güven düzeyi 0 ile 1 arasında olmalıdır.');
  const sd1 = x.length - 1;
  const sd2 = y.length - 1;
  const vx = varyans(x);
  const vy = varyans(y);
  if (!(vy > 0)) throw new IstatistikHatasi('İkinci grubun varyansı sıfır; varyans oranı hesaplanamaz.');
  const tahmin = vx / vy;
  const F = tahmin / oran;
  let p;
  let ga;
  if (alternatif === 'iki') {
    p = Math.min(1, 2 * Math.min(pf(F, sd1, sd2), pf(F, sd1, sd2, false)));
    const b = (1 - duzey) / 2;
    ga = { alt: tahmin / qf(1 - b, sd1, sd2), ust: tahmin / qf(b, sd1, sd2) };
  } else if (alternatif === 'buyuk') {
    p = pf(F, sd1, sd2, false);
    ga = { alt: tahmin / qf(duzey, sd1, sd2), ust: Infinity };
  } else if (alternatif === 'kucuk') {
    p = pf(F, sd1, sd2);
    ga = { alt: 0, ust: tahmin / qf(1 - duzey, sd1, sd2) };
  } else throw new IstatistikHatasi(`Bilinmeyen alternatif hipotez: ${alternatif}`);
  return {
    yontem: 'varyans-orani-f',
    ad: 'İki varyansın oranı için F testi',
    istatistik: F,
    istatistikAdi: 'F',
    sd: [sd1, sd2],
    p,
    alternatif,
    varyansOrani: tahmin,
    varyanslar: [vx, vy],
    ga: { duzey, alt: ga.alt, ust: ga.ust, neyin: 'varyans oranı' },
    n: x.length + y.length,
    uyarilar: [],
  };
}
