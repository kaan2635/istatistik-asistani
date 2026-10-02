// Tekrarlı ölçümler ANOVA'sı (denek içi faktörler) ve karma desen (bir gruplar arası faktör).
// SPSS GLM Repeated Measures çıktısının karşılığıdır: Mauchly küresellik testi ve epsilonlar,
// "Tests of Within-Subjects Effects", "Multivariate Tests", "Tests of Within-Subjects Contrasts",
// "Tests of Between-Subjects Effects", kestirilen marjinal ortalamalar ve ikili karşılaştırmalar.
//
// Yöntem (çok değişkenli yaklaşım): K ölçüm, her denek içi etki W için ortonormal kontrast matrisi M_W ile
// Z = Y·M_W'ye dönüştürülür (SPSS'in varsayılan polinom kontrastları; küresellik testi ve tüm F'ler ortonormal
// tabanın seçiminden bağımsızdır). Gruplar arası model toplamı sıfır kodlamalı hücre ortalamaları modelidir;
// hipotez SSCP matrisleri Tip III'tür (dengesiz gruplarda SPSS ve car::Anova(type = 3) ile aynı).

import { cholesky, carp, transpoz, simetrikOzdeger } from './matris.js';
import { pf, pchisq, qt } from './dagilim.js';
import { ortalama, standartSapma } from './betimsel.js';
import { gerekli } from './hata.js';
import {
  AYRAC, duzeyleriBelirle, etiketEksik, sayiMi, duzeltilmisKarsilastirma, duzeltmeDogrula, duzeyDogrula, izgara, kuadratik,
} from './glm.js';

const KONTRAST_ADLARI = ['Doğrusal', 'Karesel', 'Kübik'];

function kontrastAdi(derece) {
  return KONTRAST_ADLARI[derece - 1] ?? `${derece}. derece`;
}

/**
 * Ortonormal polinom kontrastları (R contr.poly, SPSS POLYNOMIAL): k × (k-1) matris.
 * Ayrık ortogonal polinomlar üç terimli yineleme (Stieltjes) ile kurulur, bir kez yeniden ortogonalleştirilir.
 * İşaret: en yüksek dereceli katsayı pozitif (R ile aynı).
 * @param {number} k düzey sayısı
 * @param {number[]} [skorlar] düzey değerleri (varsayılan 1..k, eşit aralıklı)
 */
export function polinomKontrastlari(k, skorlar) {
  const x0 = skorlar ? skorlar.slice() : Array.from({ length: k }, (_, i) => i + 1);
  const m = x0.reduce((a, b) => a + b, 0) / k;
  const x = x0.map((v) => v - m);
  const norm = (v) => Math.sqrt(v.reduce((a, b) => a + b * b, 0));
  const icc = (a, b) => a.reduce((t, v, i) => t + v * b[i], 0);
  const taban = [new Array(k).fill(1 / Math.sqrt(k))];
  let onceki = new Array(k).fill(0);
  let simdiki = new Array(k).fill(1);
  let oncekiNorm2 = 1;
  for (let j = 1; j < k; j++) {
    const n2 = icc(simdiki, simdiki);
    const a = icc(x.map((v, i) => v * simdiki[i]), simdiki) / n2;
    const b = j === 1 ? 0 : n2 / oncekiNorm2;
    let yeni = x.map((v, i) => (v - a) * simdiki[i] - b * onceki[i]);
    // yeniden ortogonalleştirme (yuvarlama birikimini giderir)
    for (const t of taban) {
      const d = icc(yeni, t);
      yeni = yeni.map((v, i) => v - d * t[i]);
    }
    const nn = norm(yeni);
    taban.push(yeni.map((v) => v / nn));
    onceki = simdiki;
    oncekiNorm2 = n2;
    simdiki = yeni;
  }
  return Array.from({ length: k }, (_, i) => taban.slice(1).map((t) => t[i]));
}

function kronecker(A, B) {
  const r1 = A.length;
  const c1 = A[0].length;
  const r2 = B.length;
  const c2 = B[0].length;
  const C = Array.from({ length: r1 * r2 }, () => new Array(c1 * c2).fill(0));
  for (let i1 = 0; i1 < r1; i1++) {
    for (let j1 = 0; j1 < c1; j1++) {
      for (let i2 = 0; i2 < r2; i2++) for (let j2 = 0; j2 < c2; j2++) C[i1 * r2 + i2][j1 * c2 + j2] = A[i1][j1] * B[i2][j2];
    }
  }
  return C;
}

function iz(A) {
  let t = 0;
  for (let i = 0; i < A.length; i++) t += A[i][i];
  return t;
}

function donustur(M, H) {
  return carp(transpoz(M), carp(H, M));
}

/**
 * Dört çok değişkenli test (SPSS "Multivariate Tests"; R summary.manova / car ile aynı F yaklaşımları).
 * @param {number[][]} H hipotez SSCP (p × p), @param {number[][]} E hata SSCP, @param {number} q hipotez sd, @param {number} sdHata
 */
export function cokDegiskenliTestler(H, E, q, sdHata) {
  const p = E.length;
  const L = cholesky(E);
  if (!L) return null;
  // L^-1
  const Li = Array.from({ length: p }, () => new Array(p).fill(0));
  for (let i = 0; i < p; i++) {
    Li[i][i] = 1 / L[i][i];
    for (let j = 0; j < i; j++) {
      let s = 0;
      for (let k = j; k < i; k++) s -= L[i][k] * Li[k][j];
      Li[i][j] = s / L[i][i];
    }
  }
  const A = carp(carp(Li, H), transpoz(Li));
  for (let i = 0; i < p; i++) for (let j = 0; j < i; j++) A[i][j] = A[j][i] = (A[i][j] + A[j][i]) / 2;
  const ozd = simetrikOzdeger(A).degerler.map((v) => Math.max(v, 0));
  const s = Math.min(p, q);
  const m = (Math.abs(p - q) - 1) / 2;
  const nn = (sdHata - p - 1) / 2;
  const satirlar = [];
  const ekle = (test, ad, deger, F, sd1, sd2, kismiEtaKare, kesin) => {
    satirlar.push({ test, ad, deger, F, sd1, sd2, p: pf(F, sd1, sd2, false), kismiEtaKare, kesin });
  };
  // Pillai
  const V = ozd.reduce((t, l) => t + l / (1 + l), 0);
  ekle('pillai', 'Pillai izi', V, ((2 * nn + s + 1) / (2 * m + s + 1)) * (V / (s - V)), s * (2 * m + s + 1), s * (2 * nn + s + 1), V / s, s === 1);
  // Wilks (Rao F)
  const lam = ozd.reduce((t, l) => t / (1 + l), 1);
  const t1 = sdHata - (p - q + 1) / 2;
  const t2 = (p * q - 2) / 4;
  const t3 = p * p + q * q - 5 > 0 ? Math.sqrt((p * p * q * q - 4) / (p * p + q * q - 5)) : 1;
  const sd2W = t1 * t3 - 2 * t2;
  const lamKok = lam ** (1 / t3);
  ekle('wilks', 'Wilks lambda', lam, ((1 - lamKok) / lamKok) * (sd2W / (p * q)), p * q, sd2W, 1 - lam ** (1 / s), Math.min(p, q) <= 2);
  // Hotelling-Lawley
  const T = ozd.reduce((t, l) => t + l, 0);
  const sd2H = 2 * (s * nn + 1);
  ekle('hotelling', 'Hotelling izi', T, (sd2H * T) / (s * s * (2 * m + s + 1)), s * (2 * m + s + 1), sd2H, T / s / (T / s + 1), s === 1);
  // Roy (üst sınır)
  const kok = Math.max(...ozd);
  const r1 = Math.max(p, q);
  const r2 = sdHata - r1 + q;
  ekle('roy', 'Roy en büyük kök', kok, (r2 * kok) / r1, r1, r2, kok / (1 + kok), s === 1);
  return satirlar;
}

/**
 * Mauchly küresellik testi ve epsilonlar (SPSS GLM Repeated Measures).
 * - W = |S| / (iz(S)/p)^p, χ² = -(sd - (2p²+p+2)/(6p))·ln W, sd_χ² = p(p+1)/2 - 1, p = P(χ² > ·) (SPSS).
 *   `pR`: R'nin mauchly.test/car'ın ikinci derece düzeltmeli p'si (w2 terimindeki "3·K" R'deki gibidir).
 * - Greenhouse-Geisser ε̂ = iz(S)² / (p·iz(S²)).
 * - Huynh-Feldt, SPSS (özgün formül): ε̃ = (N·p·ε̂ - 2) / (p·(N - g - p·ε̂)), N denek, g grup sayısı.
 *   Lecoutre (1991) düzeltmesi (R stats/car): ((sd+1)·p·ε̂ - 2) / (p·(sd - p·ε̂)), sd = N - g. g = 1 iken ikisi aynıdır.
 * - Alt sınır = 1/p.
 */
function kuresellikHesapla(EW, sdHata, N, G, K, hfFormulu) {
  const p = EW.length;
  const S = EW.map((r) => r.map((v) => v / sdHata));
  const izS = iz(S);
  let izS2 = 0;
  for (const r of S) for (const v of r) izS2 += v * v;
  const gg = izS2 > 0 ? (izS * izS) / (p * izS2) : 1;
  const hfSpss = (N * p * gg - 2) / (p * (N - G - p * gg));
  const hfLecoutre = ((sdHata + 1) * p * gg - 2) / (p * (sdHata - p * gg));
  const secilen = hfFormulu === 'lecoutre' ? hfLecoutre : hfSpss;
  const hf = Number.isFinite(secilen) && secilen > 0 ? Math.min(1, secilen) : 1;
  const epsilon = { greenhouseGeisser: gg, huynhFeldt: hf, altSinir: 1 / p, huynhFeldtSpss: hfSpss, huynhFeldtLecoutre: hfLecoutre };
  if (p === 1) return { W: 1, kiKare: 0, sd: 0, p: null, pR: null, epsilon };
  const L = cholesky(EW);
  if (!L || sdHata < p) return { W: null, kiKare: null, sd: (p * (p + 1)) / 2 - 1, p: null, pR: null, epsilon };
  let logDet = 0;
  for (let i = 0; i < p; i++) logDet += 2 * Math.log(L[i][i]);
  const logW = logDet - p * Math.log(iz(EW) / p);
  const rho = 1 - (2 * p * p + p + 2) / (6 * p * sdHata);
  const kiKare = -sdHata * rho * logW;
  const f = (p * (p + 1)) / 2 - 1;
  const p1 = pchisq(kiKare, f, false);
  const w2 = ((p + 2) * (p - 1) * (p - 2) * (2 * p ** 3 + 6 * p * p + 3 * K + 2)) / (288 * (sdHata * p * rho) ** 2);
  const pR = p1 + w2 * (pchisq(kiKare, f + 4, false) - p1);
  return { W: Math.exp(logW), kiKare, sd: f, p: p1, pR, epsilon };
}

const DUZELTME_SATIRLARI = [
  ['kuresellik', 'Küresellik varsayıldı'],
  ['greenhouse-geisser', 'Greenhouse-Geisser'],
  ['huynh-feldt', 'Huynh-Feldt'],
  ['alt-sinir', 'Alt sınır'],
];

/**
 * Tekrarlı ölçümler ANOVA'sı / karma desen ANOVA (SPSS GLM Repeated Measures, Tip III).
 *
 * R doğrulaması: car::Anova(lm(Y ~ grup, contrasts = contr.sum), idata, idesign, type = 3) ve summary(),
 * stats::mauchly.test, emmeans (mlm, mult.levs), car::linearHypothesis(P = ...), car::leveneTest(center = mean).
 *
 * Huynh-Feldt epsilonu varsayılan olarak SPSS'in kullandığı özgün formülle hesaplanır (bkz. kuresellikHesapla);
 * R'nin (stats::anova.mlm, car) Lecoutre düzeltmeli değeri `hfFormulu: 'lecoutre'` ile seçilebilir ve her
 * durumda `epsilon.huynhFeldtLecoutre` alanında verilir. Tek gruplu desende iki formül aynıdır.
 *
 * İkili karşılaştırmalar SPSS "Pairwise Comparisons" gibidir: her karşılaştırma kendi fark değişkeninin
 * (gruplar içi birleştirilmiş) varyansını kullanır; grupsuz desende Bonferroni düzeltmeli eşleştirilmiş t testine eşittir.
 *
 * @param {object} s
 * @param {number[][]} s.veri n × K ölçüm matrisi (satır = denek). Eksik değerli denekler liste bazında çıkarılır.
 * @param {string[]} [s.duzeyAdlari] tek denek içi faktörün düzey adları (varsayılan '1'..'K')
 * @param {string} [s.faktorAdi='Zaman'] tek denek içi faktörün adı
 * @param {{ad:string, duzeyler:string[]}[]} [s.icFaktorler] birden çok denek içi faktör (sütun sırası: ilk faktör en yavaş
 *   değişir, SPSS WSFACTORS gibi). Verilirse duzeyAdlari/faktorAdi yok sayılır.
 * @param {Array} [s.gruplar] her denek için gruplar arası faktör etiketi (karma desen)
 * @param {string} [s.grupAdi='Grup']
 * @param {Array} [s.grupSirasi] grup düzeylerinin sırası (varsayılan artan)
 * @param {number} [s.duzey=0.95] güven düzeyi
 * @param {'bonferroni'|'sidak'|'yok'} [s.duzeltme='bonferroni']
 * @param {'spss'|'lecoutre'} [s.hfFormulu='spss']
 * @returns {object} alanlar: yontem, ad, n, cikarilanGozlem, icFaktorler, grup, kuresellik, icEtkilerTablosu,
 *   cokDegiskenliTestler, icKontrastlar, gruplarArasiTablo, marjinalOrtalamalar, ikiliKarsilastirmalar,
 *   basitEtkiler, levene, betimsel, hfFormulu, duzey, duzeltme, uyarilar
 */
export function tekrarliOlcumAnova(s) {
  const {
    veri, duzeyAdlari, faktorAdi = 'Zaman', icFaktorler: icGirdi, gruplar, grupAdi = 'Grup', grupSirasi,
    duzey = 0.95, duzeltme = 'bonferroni', hfFormulu = 'spss',
  } = s;
  duzeyDogrula(duzey);
  duzeltmeDogrula(duzeltme);
  gerekli(hfFormulu === 'spss' || hfFormulu === 'lecoutre', "hfFormulu 'spss' ya da 'lecoutre' olmalıdır.");
  gerekli(Array.isArray(veri) && veri.length > 0 && veri.every((r) => Array.isArray(r) || ArrayBuffer.isView(r)), 'Veri, satırları denekler olan bir sayı matrisi olmalıdır.');
  const K = veri[0].length;
  gerekli(K >= 2, 'Tekrarlı ölçümler için en az iki ölçüm (sütun) gerekir.');
  gerekli(veri.every((r) => r.length === K), 'Tüm deneklerin ölçüm sayısı aynı olmalıdır.');

  let icFaktorler;
  if (icGirdi) {
    gerekli(Array.isArray(icGirdi) && icGirdi.length >= 1, 'icFaktorler bir dizi olmalıdır.');
    icFaktorler = icGirdi.map((f) => ({ ad: String(f.ad), duzeyler: f.duzeyler.map(String) }));
    gerekli(icFaktorler.every((f) => f.duzeyler.length >= 2), 'Her denek içi faktörün en az iki düzeyi olmalıdır.');
    const carpim = icFaktorler.reduce((t, f) => t * f.duzeyler.length, 1);
    gerekli(carpim === K, `Denek içi faktörlerin düzey sayılarının çarpımı (${carpim}) ölçüm sayısına (${K}) eşit olmalıdır.`);
    gerekli(new Set(icFaktorler.map((f) => f.ad)).size === icFaktorler.length, 'Denek içi faktör adları farklı olmalıdır.');
  } else {
    const adlar = duzeyAdlari ? duzeyAdlari.map(String) : Array.from({ length: K }, (_, j) => String(j + 1));
    gerekli(adlar.length === K, 'Düzey adlarının sayısı ölçüm sayısına eşit olmalıdır.');
    icFaktorler = [{ ad: faktorAdi, duzeyler: adlar }];
  }
  const icK = icFaktorler.map((f) => f.duzeyler.length);
  const sutunDuzey = izgara(icK); // j. sütunun her faktördeki düzey indeksi
  if (gruplar) {
    gerekli(Array.isArray(gruplar) && gruplar.length === veri.length, 'Grup etiketlerinin sayısı denek sayısına eşit olmalıdır.');
    gerekli(!icFaktorler.some((f) => f.ad === grupAdi), 'Grup adı denek içi faktör adlarından farklı olmalıdır.');
  }

  // Liste bazında silme
  const tut = [];
  veri.forEach((r, i) => {
    if (!Array.from(r).every(sayiMi)) return;
    if (gruplar && etiketEksik(gruplar[i])) return;
    tut.push(i);
  });
  const N = tut.length;
  const Y = tut.map((i) => Array.from(veri[i]));
  const uyarilar = [];
  const cikarilan = veri.length - N;
  if (cikarilan > 0) uyarilar.push(`Eksik ölçümü olan ${cikarilan} denek analizden çıkarıldı (liste bazında silme).`);

  let grupDuzeyleri = null;
  let grupIdx;
  if (gruplar) {
    grupDuzeyleri = duzeyleriBelirle(tut.map((i) => gruplar[i]), grupSirasi);
    gerekli(grupDuzeyleri.length >= 2, 'Gruplar arası faktörün (eksikler çıkarıldıktan sonra) en az iki düzeyi olmalıdır.');
    grupIdx = tut.map((i) => grupDuzeyleri.indexOf(String(gruplar[i])));
  } else {
    grupIdx = new Array(N).fill(0);
  }
  const G = grupDuzeyleri ? grupDuzeyleri.length : 1;
  const sdHata = N - G;
  gerekli(sdHata >= 1, gruplar
    ? 'Hata serbestlik derecesi sıfır: denek sayısı grup sayısından fazla olmalıdır.'
    : 'Tekrarlı ölçümler ANOVA\'sı için en az iki denek gerekir.');

  // Grup ortalamaları ve birleştirilmiş grup içi SSCP (E)
  const nG = new Array(G).fill(0);
  const yG = Array.from({ length: G }, () => new Array(K).fill(0));
  for (let i = 0; i < N; i++) {
    nG[grupIdx[i]]++;
    for (let j = 0; j < K; j++) yG[grupIdx[i]][j] += Y[i][j];
  }
  for (let g = 0; g < G; g++) for (let j = 0; j < K; j++) yG[g][j] /= nG[g];
  // ikinci geçiş düzeltmesi
  const duz = Array.from({ length: G }, () => new Array(K).fill(0));
  for (let i = 0; i < N; i++) for (let j = 0; j < K; j++) duz[grupIdx[i]][j] += Y[i][j] - yG[grupIdx[i]][j];
  for (let g = 0; g < G; g++) for (let j = 0; j < K; j++) yG[g][j] += duz[g][j] / nG[g];
  const E = Array.from({ length: K }, () => new Array(K).fill(0));
  for (let i = 0; i < N; i++) {
    const d = Y[i].map((v, j) => v - yG[grupIdx[i]][j]);
    for (let a = 0; a < K; a++) for (let b = a; b < K; b++) E[a][b] += d[a] * d[b];
  }
  for (let a = 0; a < K; a++) for (let b = 0; b < a; b++) E[a][b] = E[b][a];
  if (gruplar && nG.some((v) => v < 2)) uyarilar.push('Bazı gruplarda yalnızca bir denek var.');

  // Tip III hipotez SSCP'leri (K × K): kesişim (ağırlıksız ortalama) ve grup
  const muBar = new Array(K).fill(0);
  for (let g = 0; g < G; g++) for (let j = 0; j < K; j++) muBar[j] += yG[g][j] / G;
  const c = nG.reduce((t, v) => t + 1 / v, 0) / (G * G); // Var(ağırlıksız ortalama) / σ²
  const Hkesisim = muBar.map((a) => muBar.map((b) => (a * b) / c));
  let Hgrup = null;
  if (G > 1) {
    const genel = new Array(K).fill(0);
    for (let g = 0; g < G; g++) for (let j = 0; j < K; j++) genel[j] += (nG[g] * yG[g][j]) / N;
    Hgrup = Array.from({ length: K }, () => new Array(K).fill(0));
    for (let g = 0; g < G; g++) {
      const d = yG[g].map((v, j) => v - genel[j]);
      for (let a = 0; a < K; a++) for (let b = 0; b < K; b++) Hgrup[a][b] += nG[g] * d[a] * d[b];
    }
  }

  // Denek içi etkiler: faktörlerin boş olmayan alt kümeleri (derece, sonra sözlük sırası)
  const F = icFaktorler.length;
  const P = icK.map((k) => polinomKontrastlari(k));
  const U = icK.map((k) => Array.from({ length: k }, () => [1 / Math.sqrt(k)]));
  const etkiler = [];
  for (let boy = 1; boy <= F; boy++) {
    const secim = [];
    const uret = (bas) => {
      if (secim.length === boy) {
        let M = [[1]];
        for (let f = 0; f < F; f++) {
          M = kronecker(M, secim.includes(f) ? P[f] : U[f]);
        }
        let adlar = [''];
        for (const f of secim) {
          const yeni = [];
          for (const a of adlar) for (let d = 1; d < icK[f]; d++) yeni.push(a ? `${a} × ${kontrastAdi(d)}` : kontrastAdi(d));
          adlar = yeni;
        }
        etkiler.push({ faktorIdx: secim.slice(), ad: secim.map((f) => icFaktorler[f].ad).join(AYRAC), M, kontrastAdlari: adlar });
        return;
      }
      for (let f = bas; f < F; f++) {
        secim.push(f);
        uret(f + 1);
        secim.pop();
      }
    };
    uret(0);
  }

  const kuresellik = [];
  const icEtkilerTablosu = [];
  const cokDegiskenli = [];
  const icKontrastlar = [];
  let cokDegiskenliYok = false;
  for (const et of etkiler) {
    const M = et.M;
    const p = M[0].length;
    const EW = donustur(M, E);
    const kur = kuresellikHesapla(EW, sdHata, N, G, K, hfFormulu);
    const eps = kur.epsilon;
    let oneri = 'kuresellik';
    if (p > 1 && (kur.p === null || kur.p < 0.05)) oneri = eps.greenhouseGeisser < 0.75 ? 'greenhouse-geisser' : 'huynh-feldt';
    kuresellik.push({ etki: et.ad, ...kur, onerilenDuzeltme: oneri });
    if (p > 1 && kur.p !== null && kur.p < 0.05) {
      uyarilar.push(`'${et.ad}' için küresellik varsayımı sağlanmıyor (Mauchly p < 0,05); ${oneri === 'greenhouse-geisser' ? 'Greenhouse-Geisser' : 'Huynh-Feldt'} düzeltmesini raporlayın.`);
    }
    const KThata = iz(EW);
    gerekli(KThata > 1e-13 * iz(E), `'${et.ad}' için denek içi hata varyansı sıfır (tüm denekler ölçümler arasında aynı değişimi gösteriyor); F hesaplanamaz.`);
    const sdH = p * sdHata;
    const hipotezler = [[et.ad, Hkesisim, 1]];
    if (Hgrup) hipotezler.push([`${et.ad}${AYRAC}${grupAdi}`, Hgrup, G - 1]);
    for (const [kaynak, H, q] of hipotezler) {
      const HW = donustur(M, H);
      const KT = iz(HW);
      const sd = p * q;
      const Fd = (KT / sd) / (KThata / sdH);
      const epsDeger = { kuresellik: 1, 'greenhouse-geisser': eps.greenhouseGeisser, 'huynh-feldt': eps.huynhFeldt, 'alt-sinir': eps.altSinir };
      for (const [kod, duzAd] of DUZELTME_SATIRLARI) {
        const e = epsDeger[kod];
        icEtkilerTablosu.push({
          etki: et.ad, kaynak, tur: 'etki', duzeltme: kod, duzeltmeAdi: duzAd,
          KT, sd: sd * e, KO: KT / (sd * e), F: Fd, p: pf(Fd, sd * e, sdH * e, false), kismiEtaKare: KT / (KT + KThata), epsilon: e,
        });
      }
      const mv = sdHata >= p ? cokDegiskenliTestler(HW, EW, q, sdHata) : null;
      if (mv) for (const t of mv) cokDegiskenli.push({ kaynak, ...t });
      else cokDegiskenliYok = true;
      et.kontrastAdlari.forEach((kad, ci) => {
        const kt = HW[ci][ci];
        const Fk = (kt / q) / (EW[ci][ci] / sdHata);
        icKontrastlar.push({ etki: et.ad, kaynak, kontrast: kad, KT: kt, sd: q, KO: kt / q, F: Fk, p: pf(Fk, q, sdHata, false), kismiEtaKare: kt / (kt + EW[ci][ci]) });
      });
    }
    const hataAdi = `Hata(${et.ad})`;
    const epsDeger = { kuresellik: 1, 'greenhouse-geisser': eps.greenhouseGeisser, 'huynh-feldt': eps.huynhFeldt, 'alt-sinir': eps.altSinir };
    for (const [kod, duzAd] of DUZELTME_SATIRLARI) {
      const e = epsDeger[kod];
      icEtkilerTablosu.push({ etki: et.ad, kaynak: hataAdi, tur: 'hata', duzeltme: kod, duzeltmeAdi: duzAd, KT: KThata, sd: sdH * e, KO: KThata / (sdH * e), epsilon: e });
    }
    et.kontrastAdlari.forEach((kad, ci) => {
      icKontrastlar.push({ etki: et.ad, kaynak: hataAdi, kontrast: kad, KT: EW[ci][ci], sd: sdHata, KO: EW[ci][ci] / sdHata });
    });
  }
  if (cokDegiskenliYok) {
    uyarilar.push('Hata SSCP matrisi tekil (hata serbestlik derecesi denek içi kontrast sayısından küçük ya da ölçümler arasında tam doğrusal ilişki var); bazı çok değişkenli testler ve Mauchly testi hesaplanamadı.');
  }

  // Gruplar arası etkiler: ortalama dönüşümü (toplam/√K)
  const m0 = new Array(K).fill(1 / Math.sqrt(K));
  const KThataAra = kuadratik(m0, E);
  const KOhataAra = KThataAra / sdHata;
  const gruplarArasiTablo = [];
  const araSatir = (kaynak, tur, KT, sd) => {
    const Fd = KT / sd / KOhataAra;
    gruplarArasiTablo.push({ kaynak, tur, KT, sd, KO: KT / sd, F: Fd, p: pf(Fd, sd, sdHata, false), kismiEtaKare: KT / (KT + KThataAra) });
  };
  const m0mu = m0.reduce((t, v, j) => t + v * muBar[j], 0);
  araSatir('Kesişim', 'kesisim', (m0mu * m0mu) / c, 1);
  if (Hgrup) araSatir(grupAdi, 'terim', kuadratik(m0, Hgrup), G - 1);
  gruplarArasiTablo.push({ kaynak: 'Hata', tur: 'hata', KT: KThataAra, sd: sdHata, KO: KOhataAra });

  // ---- Kestirilen marjinal ortalamalar ve ikili karşılaştırmalar ----
  const tK = qt((1 - duzey) / 2, sdHata, false);
  const grupOrt = (w, g) => w.reduce((t, v, j) => t + v * yG[g][j], 0);
  const varW = (w) => kuadratik(w, E) / sdHata;
  const emmSatir = (duzeyler, tahmin, SH) => ({ duzeyler, ortalama: tahmin, SH, sd: sdHata, gaAlt: tahmin - tK * SH, gaUst: tahmin + tK * SH });
  const agirlik = (f, l) => sutunDuzey.map((d) => (d[f] === l ? icK[f] / K : 0));
  const ciftler = (sayi) => {
    const c2 = [];
    for (let i = 0; i < sayi; i++) for (let j = i + 1; j < sayi; j++) c2.push([i, j]);
    return c2;
  };
  const karsilastirma = (duzey1, duzey2, fark, SH, m) => {
    const t = fark / SH;
    const d = duzeltilmisKarsilastirma(t, sdHata, m, duzeltme, duzey);
    return { duzey1, duzey2, fark, SH, t, sd: sdHata, p: d.p, pHam: d.pHam, gaAlt: fark - d.tKritik * SH, gaUst: fark + d.tKritik * SH };
  };
  const ortTum = (w) => {
    let t = 0;
    for (let g = 0; g < G; g++) t += grupOrt(w, g);
    return t / G;
  };

  const marjinalOrtalamalar = {};
  const ikiliKarsilastirmalar = {};
  icFaktorler.forEach((fk, f) => {
    marjinalOrtalamalar[fk.ad] = fk.duzeyler.map((dz, l) => {
      const w = agirlik(f, l);
      return emmSatir({ [fk.ad]: dz }, ortTum(w), Math.sqrt(varW(w) * c));
    });
    const m = (icK[f] * (icK[f] - 1)) / 2;
    ikiliKarsilastirmalar[fk.ad] = ciftler(icK[f]).map(([i, j]) => {
      const w = agirlik(f, i).map((v, k) => v - agirlik(f, j)[k]);
      return karsilastirma(fk.duzeyler[i], fk.duzeyler[j], ortTum(w), Math.sqrt(varW(w) * c), m);
    });
  });
  if (F > 1) {
    const ad = icFaktorler.map((f) => f.ad).join(AYRAC);
    marjinalOrtalamalar[ad] = sutunDuzey.map((d, j) => {
      const w = new Array(K).fill(0);
      w[j] = 1;
      return emmSatir(Object.fromEntries(icFaktorler.map((fk, f) => [fk.ad, fk.duzeyler[d[f]]])), ortTum(w), Math.sqrt(varW(w) * c));
    });
  }

  let basitEtkiler = null;
  let levene = null;
  if (G > 1) {
    const wOrt = new Array(K).fill(1 / K);
    const s2 = varW(wOrt);
    marjinalOrtalamalar[grupAdi] = grupDuzeyleri.map((gd, g) => emmSatir({ [grupAdi]: gd }, grupOrt(wOrt, g), Math.sqrt(s2 / nG[g])));
    const mG = (G * (G - 1)) / 2;
    ikiliKarsilastirmalar[grupAdi] = ciftler(G).map(([a, b]) => karsilastirma(
      grupDuzeyleri[a], grupDuzeyleri[b], grupOrt(wOrt, a) - grupOrt(wOrt, b), Math.sqrt(s2 * (1 / nG[a] + 1 / nG[b])), mG,
    ));
    basitEtkiler = { grupHerDuzeyde: [], icFaktorHerGrupta: [] };
    icFaktorler.forEach((fk, f) => {
      const ad = `${grupAdi}${AYRAC}${fk.ad}`;
      marjinalOrtalamalar[ad] = [];
      for (let g = 0; g < G; g++) {
        for (let l = 0; l < icK[f]; l++) {
          const w = agirlik(f, l);
          marjinalOrtalamalar[ad].push(emmSatir({ [grupAdi]: grupDuzeyleri[g], [fk.ad]: fk.duzeyler[l] }, grupOrt(w, g), Math.sqrt(varW(w) / nG[g])));
        }
      }
      // Grupların her denek içi düzeydeki karşılaştırması (tek yönlü ANOVA F'si, birleştirilmiş grup içi hata)
      for (let l = 0; l < icK[f]; l++) {
        const w = agirlik(f, l);
        const ortlar = Array.from({ length: G }, (_, g) => grupOrt(w, g));
        const genel = ortlar.reduce((t, v, g) => t + nG[g] * v, 0) / N;
        const KT = ortlar.reduce((t, v, g) => t + nG[g] * (v - genel) ** 2, 0);
        const s2w = varW(w);
        const Fd = KT / (G - 1) / s2w;
        basitEtkiler.grupHerDuzeyde.push({
          etken: grupAdi, kosul: { [fk.ad]: fk.duzeyler[l] }, KT, sd: G - 1, KO: KT / (G - 1), F: Fd, hataSd: sdHata,
          p: pf(Fd, G - 1, sdHata, false), kismiEtaKare: KT / (KT + s2w * sdHata),
          ikiliKarsilastirmalar: ciftler(G).map(([a, b]) => karsilastirma(
            grupDuzeyleri[a], grupDuzeyleri[b], ortlar[a] - ortlar[b], Math.sqrt(s2w * (1 / nG[a] + 1 / nG[b])), mG,
          )),
        });
      }
      // Denek içi faktörün her gruptaki etkisi (çok değişkenli, q = 1) ve ikili karşılaştırmalar
      const Mf = etkiler.find((e) => e.faktorIdx.length === 1 && e.faktorIdx[0] === f).M;
      const EW = donustur(Mf, E);
      const mF = (icK[f] * (icK[f] - 1)) / 2;
      for (let g = 0; g < G; g++) {
        const z = Mf[0].map((_, ci) => Mf.reduce((t, r, j) => t + r[ci] * yG[g][j], 0));
        const H = z.map((a) => z.map((b) => nG[g] * a * b));
        basitEtkiler.icFaktorHerGrupta.push({
          etken: fk.ad,
          kosul: { [grupAdi]: grupDuzeyleri[g] },
          cokDegiskenli: sdHata >= Mf[0].length ? cokDegiskenliTestler(H, EW, 1, sdHata) : null,
          ikiliKarsilastirmalar: ciftler(icK[f]).map(([i, j]) => {
            const w = agirlik(f, i).map((v, k) => v - agirlik(f, j)[k]);
            return karsilastirma(fk.duzeyler[i], fk.duzeyler[j], grupOrt(w, g), Math.sqrt(varW(w) / nG[g]), mF);
          }),
        });
      }
    });
    // Her ölçüm için Levene testi (SPSS: ortalamaya dayalı)
    levene = sutunDuzey.map((d, j) => {
      const z = Y.map((r, i) => Math.abs(r[j] - yG[grupIdx[i]][j]));
      const zG = new Array(G).fill(0);
      for (let i = 0; i < N; i++) zG[grupIdx[i]] += z[i] / nG[grupIdx[i]];
      const zOrt = ortalama(z);
      let ara = 0;
      let ic = 0;
      for (let g = 0; g < G; g++) ara += nG[g] * (zG[g] - zOrt) ** 2;
      for (let i = 0; i < N; i++) ic += (z[i] - zG[grupIdx[i]]) ** 2;
      const Fd = (ara / (G - 1)) / (ic / sdHata);
      return {
        duzeyler: Object.fromEntries(icFaktorler.map((fk, f) => [fk.ad, fk.duzeyler[d[f]]])),
        istatistik: Fd, istatistikAdi: 'F', sd: [G - 1, sdHata], p: ic > 0 ? pf(Fd, G - 1, sdHata, false) : null,
      };
    });
  }

  // Betimsel istatistikler (her ölçüm × grup, SPSS "Descriptive Statistics")
  const betimsel = [];
  sutunDuzey.forEach((d, j) => {
    const duzeyler = Object.fromEntries(icFaktorler.map((fk, f) => [fk.ad, fk.duzeyler[d[f]]]));
    const ekle = (grup, degerler) => betimsel.push({
      duzeyler, grup, n: degerler.length, ortalama: ortalama(degerler), standartSapma: degerler.length > 1 ? standartSapma(degerler) : null,
    });
    if (G > 1) for (let g = 0; g < G; g++) ekle(grupDuzeyleri[g], Y.filter((_, i) => grupIdx[i] === g).map((r) => r[j]));
    ekle(null, Y.map((r) => r[j]));
  });

  return {
    yontem: G > 1 ? 'karma-anova' : 'tekrarli-olcum-anova',
    ad: G > 1 ? 'Karma desen varyans analizi (tekrarlı ölçümler)' : 'Tekrarlı ölçümler varyans analizi',
    n: N,
    cikarilanGozlem: cikarilan,
    icFaktorler,
    grup: G > 1 ? { ad: grupAdi, duzeyler: grupDuzeyleri, n: nG } : null,
    kuresellik,
    icEtkilerTablosu,
    cokDegiskenliTestler: cokDegiskenli,
    icKontrastlar,
    gruplarArasiTablo,
    marjinalOrtalamalar,
    ikiliKarsilastirmalar,
    basitEtkiler,
    levene,
    betimsel,
    hfFormulu,
    duzey,
    duzeltme,
    uyarilar,
  };
}

