// Cox orantılı hazardlar regresyonu ve orantılı hazard varsayımı testi.
//
// Algoritma R survival::coxph (coxfit6) ile adım adım aynıdır: başlangıç β = 0, Newton-Raphson, log-olabilirlik
// azalırsa adım yarılama, yakınsama ölçütü |1 − LL_eski/LL_yeni| ≤ 1e-9, en çok 20 yineleme, sütunlar ortalamadan
// arındırılıp ölçeklenir (sonuç değişmez, yalnızca sayısal kararlılık). Bağlı süreler Efron (R varsayılanı) ya da
// Breslow (SPSS'in kullandığı) yöntemiyle ele alınır. Orantılı hazard testi cox.zph(fit, transform = 'km')
// (survival ≥ 3.0 skor testi) ile aynıdır.

import { pchisq, pnorm, qnorm } from './dagilim.js';
import { IstatistikHatasi, gerekli } from './hata.js';
import { gruplariAyir, zamanDuzelt } from './sagkalim.js';

const EPS = 2.220446049250313e-16;
const TOLER_CHOL = EPS ** 0.75;

// ---------- survival'ın LDL' (cholesky2 / chsolve2 / chinv2) işlemleri ----------

/** Yerinde LDL' ayrıştırması; tekil sütunların köşegeni 0 yapılır. Döndürür: rank (negatif tanımlıysa −rank) */
function cholesky2(M, n, toler) {
  let nonneg = 1;
  let eps = 0;
  for (let i = 0; i < n; i++) {
    if (M[i][i] > eps) eps = M[i][i];
    for (let j = i + 1; j < n; j++) M[j][i] = M[i][j];
  }
  eps = eps === 0 ? toler : eps * toler;
  let rank = 0;
  for (let i = 0; i < n; i++) {
    const pivot = M[i][i];
    if (!Number.isFinite(pivot) || pivot < eps) {
      M[i][i] = 0;
      if (pivot < -8 * eps) nonneg = -1;
    } else {
      rank++;
      for (let j = i + 1; j < n; j++) {
        const temp = M[j][i] / pivot;
        M[j][i] = temp;
        M[j][j] -= temp * temp * pivot;
        for (let k = j + 1; k < n; k++) M[k][j] -= temp * M[k][i];
      }
    }
  }
  return rank * nonneg;
}

function chsolve2(M, n, y) {
  for (let i = 0; i < n; i++) {
    let temp = y[i];
    for (let j = 0; j < i; j++) temp -= y[j] * M[i][j];
    y[i] = temp;
  }
  for (let i = n - 1; i >= 0; i--) {
    if (M[i][i] === 0) y[i] = 0;
    else {
      let temp = y[i] / M[i][i];
      for (let j = i + 1; j < n; j++) temp -= y[j] * M[j][i];
      y[i] = temp;
    }
  }
}

/** cholesky2 sonrası tam tersi (simetrik) üretir; tekil satır/sütunlar 0 kalır */
function chinv2(M, n) {
  for (let i = 0; i < n; i++) {
    if (M[i][i] > 0) {
      M[i][i] = 1 / M[i][i];
      for (let j = i + 1; j < n; j++) {
        M[j][i] = -M[j][i];
        for (let k = 0; k < i; k++) M[j][k] += M[j][i] * M[i][k];
      }
    }
  }
  for (let i = 0; i < n; i++) {
    if (M[i][i] === 0) {
      for (let j = 0; j < i; j++) M[j][i] = 0;
      for (let j = i; j < n; j++) M[i][j] = 0;
    } else {
      for (let j = i + 1; j < n; j++) {
        const temp = M[j][i] * M[j][j];
        if (j !== i) M[i][j] = temp;
        for (let k = i; k < j; k++) M[i][k] += temp * M[j][k];
      }
    }
  }
  for (let i = 0; i < n; i++) for (let j = 0; j < i; j++) M[i][j] = M[j][i];
}

const kare = (n) => Array.from({ length: n }, () => new Array(n).fill(0));

// ---------- Veri hazırlığı ----------

function girdiHazirla({ sure, olay, X, adlar, tabaka }) {
  gerekli(Array.isArray(sure) || ArrayBuffer.isView(sure), 'Süre değişkeni bir dizi olmalıdır.');
  const n = sure.length;
  gerekli(n > 0, 'Cox regresyonu için veri yok.');
  for (const t of sure) gerekli(typeof t === 'number' && Number.isFinite(t) && t >= 0, 'Süre değişkeninde eksik, negatif ya da sayısal olmayan değer var.');
  gerekli(olay && olay.length === n, 'Süre ve olay değişkenlerinin uzunlukları eşit olmalıdır.');
  const d = new Int8Array(n);
  for (let i = 0; i < n; i++) {
    const v = olay[i];
    if (v === 1 || v === true) d[i] = 1;
    else if (v === 0 || v === false) d[i] = 0;
    else throw new IstatistikHatasi('Olay değişkeni yalnızca 1 (olay gerçekleşti) ve 0 (sansürlü) değerlerini almalıdır.');
  }
  gerekli(Array.isArray(X) && X.length === n, 'Açıklayıcı değişken matrisi (X) her gözlem için bir satır içermelidir.');
  const satirlar = X.map((s) => (Array.isArray(s) || ArrayBuffer.isView(s) ? Array.from(s) : [s]));
  const p = satirlar[0].length;
  gerekli(p >= 1, 'Cox regresyonu için en az bir açıklayıcı değişken gerekir.');
  for (const s of satirlar) {
    gerekli(s.length === p, 'X matrisinin tüm satırları aynı uzunlukta olmalıdır.');
    for (const v of s) gerekli(typeof v === 'number' && Number.isFinite(v), 'Açıklayıcı değişkenlerde eksik ya da sayısal olmayan değer var.');
  }
  const ad = adlar ? Array.from(adlar) : Array.from({ length: p }, (_, j) => `X${j + 1}`);
  gerekli(ad.length === p, 'Değişken adlarının sayısı X sütun sayısına eşit olmalıdır.');
  let tabakaIdx = null;
  if (tabaka) {
    gerekli(tabaka.length === n, 'Tabaka değişkeninin uzunluğu süre ile aynı olmalıdır.');
    tabakaIdx = gruplariAyir(tabaka).indeks;
  }
  const t = zamanDuzelt(sure);
  // R: order(strata, time)
  const sira = Array.from({ length: n }, (_, i) => i).sort(
    (a, b) => (tabakaIdx ? tabakaIdx[a] - tabakaIdx[b] : 0) || t[a] - t[b] || a - b,
  );
  // tabakaSonu[i] = 1: sıralı veride tabakanın son gözlemi (coxfit6 'strata')
  const tabakaSonu = new Int8Array(n);
  for (let i = 0; i < n; i++) {
    tabakaSonu[i] = i === n - 1 || (tabakaIdx && tabakaIdx[sira[i]] !== tabakaIdx[sira[i + 1]]) ? 1 : 0;
  }
  return { n, p, ad, t, d, X: satirlar, sira, tabakaSonu, tabakaIdx };
}

// Kısmi olabilirlik, skor ve bilgi matrisi (coxfit6 birikim döngüsü)
function birikim(st, sd, sx, tabakaSonu, beta, efron) {
  const n = st.length;
  const p = beta.length;
  let loglik = 0;
  const u = new Array(p).fill(0);
  const imat = kare(p);
  const a = new Array(p).fill(0);
  const a2 = new Array(p).fill(0);
  const cmat = kare(p);
  const cmat2 = kare(p);
  let denom = 0;
  let person = n - 1;
  while (person >= 0) {
    if (tabakaSonu[person] === 1) {
      denom = 0;
      a.fill(0);
      for (const s of cmat) s.fill(0);
    }
    const dtime = st[person];
    let ndead = 0;
    let deadwt = 0;
    let denom2 = 0;
    while (person >= 0 && st[person] === dtime) {
      const x = sx[person];
      let zbeta = 0;
      for (let i = 0; i < p; i++) zbeta += beta[i] * x[i];
      const risk = Math.exp(zbeta);
      if (sd[person] === 0) {
        denom += risk;
        for (let i = 0; i < p; i++) {
          a[i] += risk * x[i];
          for (let j = 0; j <= i; j++) cmat[i][j] += risk * x[i] * x[j];
        }
      } else {
        ndead++;
        deadwt += 1;
        denom2 += risk;
        loglik += zbeta;
        for (let i = 0; i < p; i++) {
          u[i] += x[i];
          a2[i] += risk * x[i];
          for (let j = 0; j <= i; j++) cmat2[i][j] += risk * x[i] * x[j];
        }
      }
      person--;
      if (person >= 0 && tabakaSonu[person] === 1) break;
    }
    if (ndead > 0) {
      if (!efron) {
        denom += denom2;
        loglik -= deadwt * Math.log(denom);
        for (let i = 0; i < p; i++) {
          a[i] += a2[i];
          const temp = a[i] / denom;
          u[i] -= deadwt * temp;
          for (let j = 0; j <= i; j++) {
            cmat[i][j] += cmat2[i][j];
            imat[j][i] += (deadwt * (cmat[i][j] - temp * a[j])) / denom;
          }
        }
      } else {
        const wtave = deadwt / ndead;
        for (let k = 0; k < ndead; k++) {
          denom += denom2 / ndead;
          loglik -= wtave * Math.log(denom);
          for (let i = 0; i < p; i++) {
            a[i] += a2[i] / ndead;
            const temp = a[i] / denom;
            u[i] -= wtave * temp;
            for (let j = 0; j <= i; j++) {
              cmat[i][j] += cmat2[i][j] / ndead;
              imat[j][i] += (wtave * (cmat[i][j] - temp * a[j])) / denom;
            }
          }
        }
      }
      a2.fill(0);
      for (const s of cmat2) s.fill(0);
    }
  }
  // coxfit6 yalnızca üst üçgeni doldurur; cholesky2 alt üçgeni oradan kopyalar
  return { loglik, u, imat };
}

// coxfit6: Newton-Raphson + adım yarılama
function coxfit6(g, efron, maksIt, eps) {
  const { n, p, X, sira, d, t, tabakaSonu } = g;
  const st = sira.map((i) => t[i]);
  const sd = Int8Array.from(sira, (i) => d[i]);
  const ortalama = new Array(p).fill(0);
  const olcek = new Array(p).fill(1);
  const sx = sira.map((i) => X[i].slice());
  for (let j = 0; j < p; j++) {
    // coxph(nocenter = c(-1, 0, 1)): yalnızca -1/0/1 değerli sütunlar ortalamadan arındırılmaz ve ölçeklenmez
    if (sx.every((s) => s[j] === -1 || s[j] === 0 || s[j] === 1)) continue;
    let s = 0;
    for (let i = 0; i < n; i++) s += sx[i][j];
    const m = s / n;
    ortalama[j] = m;
    let mut = 0;
    for (let i = 0; i < n; i++) {
      sx[i][j] -= m;
      mut += Math.abs(sx[i][j]);
    }
    const sc = mut > 0 ? n / mut : 1;
    olcek[j] = sc;
    for (let i = 0; i < n; i++) sx[i][j] *= sc;
  }
  let beta = new Array(p).fill(0);
  // 0. adım (başlangıç değerleri): skor testi buradan
  let b = birikim(st, sd, sx, tabakaSonu, beta, efron);
  const loglik0 = b.loglik;
  let enIyiLL = b.loglik; // R: loglik[1], şimdiye kadarki en iyi
  let imat = b.imat;
  let u = b.u;
  const a = b.u.slice();
  let flag = cholesky2(imat, p, TOLER_CHOL);
  chsolve2(imat, p, a);
  let skor = 0;
  for (let i = 0; i < p; i++) skor += b.u[i] * a[i];
  if (!Number.isFinite(loglik0)) throw new IstatistikHatasi('Başlangıç log-olabilirliği hesaplanamadı (taşma).');
  let yeniBeta = beta.map((v, i) => v + a[i]);
  let yarilama = 0;
  let iter;
  let yakinsadi = false;
  let sonLL = loglik0;
  let sonucBeta;
  for (iter = 1; iter <= maksIt; iter++) {
    b = birikim(st, sd, sx, tabakaSonu, yeniBeta, efron);
    const yeniLL = b.loglik;
    imat = b.imat;
    u = b.u;
    flag = cholesky2(imat, p, TOLER_CHOL);
    let sonlu = Number.isFinite(yeniLL);
    for (let i = 0; i < p && sonlu; i++) {
      if (!Number.isFinite(u[i])) sonlu = false;
      for (let j = 0; j < p; j++) if (!Number.isFinite(imat[i][j])) sonlu = false;
    }
    if (sonlu && Math.abs(1 - enIyiLL / yeniLL) <= eps) {
      yakinsadi = true;
      sonLL = yeniLL;
      sonucBeta = yeniBeta;
      if (yarilama) flag = -2;
      break;
    }
    if (!sonlu || yeniLL < enIyiLL) {
      // yakınsamıyor: adımı giderek daha çok küçült (R: halving++)
      yarilama++;
      yeniBeta = yeniBeta.map((v, i) => (v + yarilama * beta[i]) / (yarilama + 1));
    } else {
      yarilama = 0;
      enIyiLL = yeniLL;
      const adim = u.slice();
      chsolve2(imat, p, adim);
      beta = yeniBeta;
      yeniBeta = yeniBeta.map((v, i) => v + adim[i]);
    }
  }
  if (!yakinsadi) {
    // Yineleme sınırı: son iyi β'da yeniden hesaplanır; R burada bilgi matrisini ayrıştırmadan chinv2'ye verir
    // (sonuç köşegen 1/I_jj olur) — birebir uyum için aynen yapılır.
    iter = maksIt + 1;
    if (maksIt > 1) {
      b = birikim(st, sd, sx, tabakaSonu, beta, efron);
      imat = b.imat;
      u = b.u;
      sonLL = b.loglik;
    }
    sonucBeta = beta;
    flag = 1000;
  }
  chinv2(imat, p);
  const katsayi = sonucBeta.map((v, i) => v * olcek[i]);
  const skorSon = u.map((v, i) => v / olcek[i]);
  const V = kare(p);
  for (let i = 0; i < p; i++) for (let j = 0; j < p; j++) V[i][j] = imat[i][j] * olcek[i] * olcek[j];
  return { katsayi, V, loglik0, loglik: sonLL, skor, iter, yakinsadi, flag, u: skorSon, ortalama };
}

// Harrell C (survival::concordance, timewt = 'n'); SH sonsuz küçük çakı (infinitesimal jackknife) ile
function uyumIndeksi(t, d, eta, tabakaIdx) {
  const n = t.length;
  const gruplar = new Map();
  for (let i = 0; i < n; i++) {
    const k = tabakaIdx ? tabakaIdx[i] : 0;
    if (!gruplar.has(k)) gruplar.set(k, []);
    gruplar.get(k).push(i);
  }
  // I1/I2/I3: her gözlemin katıldığı uyumlu/uyumsuz/X-bağlı çift sayıları (Cox yönünde: kısa süre ↔ yüksek risk).
  // Karşılaştırılabilir çift: i olay, t_j > t_i ya da (t_j = t_i ve j sansürlü). Fenwick ağacıyla O(n log n).
  const I1 = new Float64Array(n);
  const I2 = new Float64Array(n);
  const I3 = new Float64Array(n);
  let uyumlu = 0;
  let uyumsuz = 0;
  let bagX = 0;
  let bagY = 0;
  let bagXY = 0;
  for (const idx of gruplar.values()) {
    const tekil = Array.from(new Set(idx.map((i) => eta[i]))).sort((a, b) => a - b);
    const sira = new Map(tekil.map((v, r) => [v, r]));
    const m = tekil.length;
    const r = new Map(idx.map((i) => [i, sira.get(eta[i])]));
    let agac = new Float64Array(m + 1);
    const ekle = (k) => {
      for (let q = k + 1; q <= m; q += q & -q) agac[q]++;
    };
    const onek = (k) => {
      // rank ≤ k olanların sayısı
      let s = 0;
      for (let q = k + 1; q > 0; q -= q & -q) s += agac[q];
      return s;
    };
    const say = (k) => {
      const alt = k > 0 ? onek(k - 1) : 0;
      const esit = onek(k) - alt;
      return { kucuk: alt, esit, buyuk: onek(m - 1) - alt - esit };
    };
    // Zaman blokları (artan)
    const sirali = idx.slice().sort((a, b) => t[a] - t[b] || a - b);
    const bloklar = [];
    for (let s = 0; s < sirali.length; ) {
      let e = s;
      while (e < sirali.length && t[sirali[e]] === t[sirali[s]]) e++;
      bloklar.push(sirali.slice(s, e));
      s = e;
    }
    // A: i (erken olay) rolü — süresi daha uzun olanlar + aynı anda sansürlüler ağaçta
    for (let b = bloklar.length - 1; b >= 0; b--) {
      const blok = bloklar[b];
      for (const j of blok) if (d[j] === 0) ekle(r.get(j));
      const olumler = blok.filter((i) => d[i] === 1);
      for (const i of olumler) {
        const c = say(r.get(i));
        uyumlu += c.kucuk;
        uyumsuz += c.buyuk;
        bagX += c.esit;
        I1[i] += c.kucuk;
        I2[i] += c.buyuk;
        I3[i] += c.esit;
      }
      // aynı anda ölenler: Y-bağlı çiftler
      const dd = olumler.length;
      if (dd > 1) {
        const esitler = new Map();
        for (const i of olumler) esitler.set(eta[i], (esitler.get(eta[i]) || 0) + 1);
        let xy = 0;
        for (const k of esitler.values()) xy += (k * (k - 1)) / 2;
        bagXY += xy;
        bagY += (dd * (dd - 1)) / 2 - xy;
      }
      for (const i of olumler) ekle(r.get(i));
    }
    // B: j (geç) rolü — süresi daha kısa olan olaylar ağaçta
    agac = new Float64Array(m + 1);
    for (const blok of bloklar) {
      const olumler = blok.filter((i) => d[i] === 1);
      for (const j of olumler) {
        const c = say(r.get(j));
        I1[j] += c.buyuk;
        I2[j] += c.kucuk;
        I3[j] += c.esit;
      }
      for (const i of olumler) ekle(r.get(i));
      for (const j of blok) {
        if (d[j] !== 0) continue;
        const c = say(r.get(j));
        I1[j] += c.buyuk;
        I2[j] += c.kucuk;
        I3[j] += c.esit;
      }
    }
  }
  const nCift = uyumlu + uyumsuz + bagX;
  if (nCift === 0) return { deger: null, sh: null, uyumlu, uyumsuz, bagliX: bagX, bagliY: bagY, bagliXY: bagXY };
  const somer = (uyumlu - uyumsuz) / nCift;
  let v = 0;
  for (let k = 0; k < n; k++) {
    const df = (I1[k] - I2[k] - (I1[k] + I2[k] + I3[k]) * somer) / (2 * nCift);
    v += df * df;
  }
  return { deger: (somer + 1) / 2, sh: Math.sqrt(v), uyumlu, uyumsuz, bagliX: bagX, bagliY: bagY, bagliXY: bagXY };
}

/**
 * Cox orantılı hazardlar regresyonu (R: coxph(Surv(sure, olay) ~ X, ties = baglar)).
 *
 * Kategorik değişkenler için gösterge (dummy) sütunları çağıran oluşturur; X tasarım matrisidir (sabit terim yok).
 * Bağlı süreler: 'efron' (R varsayılanı, daha doğru) ya da 'breslow' (SPSS COXREG'in kullandığı; SPSS ile
 * karşılaştırırken bunu seçin — SPSS'in yakınsama ölçütü farklı olduğundan son basamaklarda küçük farklar olabilir).
 *
 * Çıktı: katsayilar[{ad, B, SH, z, wald (= z², SPSS Wald), sd, p, HR (= Exp(B)), gaAlt, gaUst}] (HR için GA),
 * modelTestleri {olabilirlikOrani, wald, skor} (summary(coxph) ile aynı; skor testi = log-rank karşılığı),
 * logOlabilirlik {baslangic, son}, ikiLogOlabilirlik (−2LL, SPSS), uyum (Harrell C ve SH = summary()$concordance),
 * iterasyon, yakinsadi, kovaryans. Doğrusal bağımlı sütunların katsayısı null olur (R'de NA).
 * Olabilirlik sonsuza yakınsıyorsa (monoton olabilirlik, ayrışma) R'nin uyarısı verilir.
 *
 * @param {{sure:number[], olay:(0|1)[], X:number[][], adlar?:string[], baglar?:'efron'|'breslow', duzey?:number,
 *          tabaka?:Array, maksIterasyon?:number, eps?:number}} girdi
 */
export function coxRegresyon(girdi) {
  const { baglar = 'efron', duzey = 0.95, maksIterasyon = 20, eps = 1e-9 } = girdi;
  gerekli(baglar === 'efron' || baglar === 'breslow', "Bağ yöntemi 'efron' ya da 'breslow' olmalıdır.");
  gerekli(duzey > 0 && duzey < 1, 'Güven düzeyi 0 ile 1 arasında olmalıdır.');
  gerekli(Number.isInteger(maksIterasyon) && maksIterasyon >= 1, 'Yineleme sayısı en az 1 olmalıdır.');
  const g = girdiHazirla(girdi);
  const olaySayisi = g.d.reduce((a, b) => a + b, 0);
  if (olaySayisi === 0) throw new IstatistikHatasi('Hiç olay gözlenmediği için Cox regresyonu yapılamaz.');
  const f = coxfit6(g, baglar === 'efron', maksIterasyon, eps);
  const { p } = g;
  const uyarilar = [];
  const tekil = f.flag < p ? f.V.map((s, i) => s[i] === 0) : new Array(p).fill(false);
  if (tekil.some(Boolean)) {
    uyarilar.push(`Doğrusal bağımlılık nedeniyle şu değişkenler modelden çıkarıldı: ${g.ad.filter((_, i) => tekil[i]).join(', ')}.`);
  }
  // Sonsuz katsayı denetimi (coxph.fit)
  const lp = g.X.map((x) => {
    let s = 0;
    for (let j = 0; j < p; j++) if (!tekil[j]) s += (x[j] - f.ortalama[j]) * f.katsayi[j];
    return s;
  });
  const infs = new Array(p).fill(0);
  for (let j = 0; j < p; j++) {
    let s = 0;
    for (let i = 0; i < p; i++) s += f.u[i] * f.V[i][j];
    infs[j] = Math.abs(s);
  }
  if (maksIterasyon > 1) {
    if (!f.yakinsadi) {
      uyarilar.push('Yineleme sınırına ulaşıldı, model yakınsamadı.');
      if (Math.max(...lp) > 500 || infs.some((v) => !Number.isFinite(v))) uyarilar.push('Bir ya da daha fazla katsayı sonsuz olabilir.');
    } else {
      const sonsuz = [];
      for (let j = 0; j < p; j++) {
        if (!Number.isFinite(f.u[j]) || (infs[j] > eps && infs[j] > Math.sqrt(eps) * Math.abs(f.katsayi[j]))) sonsuz.push(g.ad[j]);
      }
      if (sonsuz.length) {
        uyarilar.push(
          `Log-olabilirlik şu değişkenlerin katsayısı yakınsamadan durdu; katsayı sonsuz olabilir (monoton olabilirlik / tam ayrışma): ${sonsuz.join(', ')}.`,
        );
      }
    }
  }
  const z = qnorm(1 - (1 - duzey) / 2);
  const katsayilar = g.ad.map((ad, j) => {
    if (tekil[j]) return { ad, B: null, SH: null, z: null, wald: null, sd: 1, p: null, HR: null, gaAlt: null, gaUst: null };
    const B = f.katsayi[j];
    const SH = Math.sqrt(f.V[j][j]);
    const zz = B / SH;
    return {
      ad,
      B,
      SH,
      z: zz,
      wald: zz * zz,
      sd: 1,
      p: 2 * pnorm(-Math.abs(zz)),
      HR: Math.exp(B),
      gaAlt: Math.exp(B - z * SH),
      gaUst: Math.exp(B + z * SH),
    };
  });
  // Wald testi: b' V⁻¹ b (tekil olmayan katsayılar)
  const ok = g.ad.map((_, j) => j).filter((j) => !tekil[j]);
  const sdModel = ok.length;
  const Vk = ok.map((i) => ok.map((j) => f.V[i][j]));
  const bk = ok.map((j) => f.katsayi[j]);
  cholesky2(Vk, sdModel, TOLER_CHOL);
  const cz = bk.slice();
  chsolve2(Vk, sdModel, cz);
  let wald = 0;
  for (let i = 0; i < sdModel; i++) wald += cz[i] * bk[i];
  const lr = 2 * (f.loglik - f.loglik0);
  const uyum = uyumIndeksi(g.t, g.d, lp, g.tabakaIdx);
  if (uyum.deger !== null) {
    uyum.gaAlt = Math.max(0, uyum.deger - z * uyum.sh);
    uyum.gaUst = Math.min(1, uyum.deger + z * uyum.sh);
  }
  const sonuc = {
    yontem: 'cox-regresyonu',
    ad: 'Cox orantılı hazardlar regresyonu',
    baglar,
    duzey,
    n: g.n,
    olaySayisi,
    katsayilar,
    modelTestleri: {
      olabilirlikOrani: { istatistik: lr, sd: sdModel, p: pchisq(lr, sdModel, false) },
      wald: { istatistik: wald, sd: sdModel, p: pchisq(wald, sdModel, false) },
      skor: { istatistik: f.skor, sd: sdModel, p: pchisq(f.skor, sdModel, false) },
    },
    logOlabilirlik: { baslangic: f.loglik0, son: f.loglik },
    ikiLogOlabilirlik: { baslangic: -2 * f.loglik0, son: -2 * f.loglik },
    uyum,
    iterasyon: f.iter,
    yakinsadi: f.yakinsadi,
    kovaryans: f.V.map((s, i) => s.map((v, j) => (tekil[i] || tekil[j] ? null : v))),
    uyarilar,
  };
  // cox.zph için gerekli girdiler (JSON'a yazılmaz)
  Object.defineProperty(sonuc, 'girdi', { value: { g, lp, katsayi: f.katsayi, tekil, V: f.V }, enumerable: false });
  return sonuc;
}

// ---------- Orantılı hazard varsayımı (cox.zph) ----------

// Havuzlanmış KM'ye göre 1 − S(t−) dönüşümü (cox.zph transform = 'km')
function kmDonusum(t, d) {
  const n = t.length;
  const sira = Array.from({ length: n }, (_, i) => i).sort((a, b) => t[a] - t[b] || a - b);
  const zamanlar = [];
  const S = [];
  let s = 1;
  let i = 0;
  while (i < n) {
    let j = i;
    let olum = 0;
    while (j < n && t[sira[j]] === t[sira[i]]) {
      olum += d[sira[j]];
      j++;
    }
    s *= (n - i - olum) / (n - i);
    zamanlar.push(t[sira[i]]);
    S.push(s);
    i = j;
  }
  // findInterval(times, km$time, left.open = TRUE): t'den küçük zaman sayısı
  return t.map((x) => {
    let lo = 0;
    let hi = zamanlar.length;
    while (lo < hi) {
      const m = (lo + hi) >> 1;
      if (zamanlar[m] < x) lo = m + 1;
      else hi = m;
    }
    return 1 - (lo === 0 ? 1 : S[lo - 1]);
  });
}

// Genel simetrik çözüm (Gauss, kısmi pivot)
function coz(A, b) {
  const n = b.length;
  const M = A.map((s, i) => [...s, b[i]]);
  for (let c = 0; c < n; c++) {
    let p = c;
    for (let r = c + 1; r < n; r++) if (Math.abs(M[r][c]) > Math.abs(M[p][c])) p = r;
    if (!(Math.abs(M[p][c]) > 0)) throw new IstatistikHatasi('Orantılı hazard testinde bilgi matrisi tekil.');
    [M[c], M[p]] = [M[p], M[c]];
    for (let r = c + 1; r < n; r++) {
      const f = M[r][c] / M[c][c];
      for (let s = c; s <= n; s++) M[r][s] -= f * M[c][s];
    }
  }
  const x = new Array(n).fill(0);
  for (let r = n - 1; r >= 0; r--) {
    let s = M[r][n];
    for (let c = r + 1; c < n; c++) s -= M[r][c] * x[c];
    x[r] = s / M[r][r];
  }
  return x;
}

function tersMatris(A) {
  const n = A.length;
  const T = kare(n);
  for (let j = 0; j < n; j++) {
    const e = new Array(n).fill(0);
    e[j] = 1;
    const s = coz(A, e);
    for (let i = 0; i < n; i++) T[i][j] = s[i];
  }
  return T;
}

/**
 * Orantılı hazard varsayımının Schoenfeld artıklarıyla sınanması (R: cox.zph(fit, transform = 'km', terms = TRUE)).
 * survival ≥ 3.0'daki skor testi: her terim için β sabitken x·g(t) etkileşiminin skor testi
 * (g(t) = 1 − KM(t−), olay zamanlarına göre ortalamadan arındırılmış) ve GENEL (GLOBAL) test.
 * Ayrıca grafik için ölçeklenmiş Schoenfeld artıkları (β + artık·V⁻¹·d, satırlar olay zamanlarına göre) döner.
 *
 * @param {object} model coxRegresyon() sonucu ya da coxRegresyon girdisi ({sure, olay, X, ...})
 * @param {{terimler?: number[][], terimAdlari?: string[]}} [secenek] birden çok sütunlu terimler
 *   (ör. bir kategorik değişkenin gösterge sütunları) için sütun indeks grupları; varsayılan her sütun ayrı terimdir.
 */
export function coxPHVarsayimi(model, secenek = {}) {
  const fit = model && model.girdi ? model : coxRegresyon(model);
  const { g, lp, katsayi, tekil } = fit.girdi;
  const efron = fit.baglar === 'efron';
  // Tekil (NA) katsayılar atılır
  const sutun = katsayi.map((_, j) => j).filter((j) => !tekil[j]);
  const nvar = sutun.length;
  const fcoef = sutun.map((j) => katsayi[j]);
  const adlar = sutun.map((j) => g.ad[j]);
  let terimler;
  let terimAdlari;
  if (secenek.terimler) {
    terimler = secenek.terimler.map((tr) => tr.map((j) => sutun.indexOf(j)).filter((j) => j >= 0)).filter((tr) => tr.length > 0);
    terimAdlari = secenek.terimAdlari ?? terimler.map((tr) => tr.map((j) => adlar[j]).join('+'));
  } else {
    terimler = sutun.map((_, j) => [j]);
    terimAdlari = adlar;
  }
  const { n, t, d, X, tabakaIdx } = g;
  const ttimes = kmDonusum(t, d);
  let ort = 0;
  let olaySay = 0;
  for (let i = 0; i < n; i++) if (d[i] === 1) {
    ort += ttimes[i];
    olaySay++;
  }
  ort /= olaySay;
  const gtime = ttimes.map((v) => v - ort);
  const strata = tabakaIdx ? Array.from(tabakaIdx) : new Array(n).fill(0);
  const sira = Array.from({ length: n }, (_, i) => i).sort((a, b) => strata[a] - strata[b] || t[a] - t[b] || a - b);
  const xm = X.map((s) => sutun.map((j) => s[j]));
  // zph1: kullanılan değişkenler (tabakada sabit değilse o tabakanın olay sayısı)
  const nTabaka = Math.max(...strata) + 1;
  const used = Array.from({ length: nTabaka }, () => new Array(nvar).fill(0));
  {
    let k = 0;
    let ndead = 0;
    let cs = strata[sira[0]];
    const kapat = (son) => {
      for (let j = 0; j < nvar; j++) {
        used[cs][j] = 0;
        for (let kk = k; kk < son; kk++) {
          if (xm[sira[kk]][j] !== xm[sira[k]][j]) {
            used[cs][j] = ndead;
            break;
          }
        }
      }
    };
    for (let i = 0; i < n; i++) {
      const pr = sira[i];
      if (cs === strata[pr]) ndead += d[pr];
      else {
        kapat(i);
        ndead = d[pr];
        k = i;
        cs = strata[pr];
      }
    }
    kapat(n);
  }
  // Ortalamadan arındırma (sayısal kararlılık)
  const xc = xm.map((s) => s.slice());
  for (let j = 0; j < nvar; j++) {
    let s = 0;
    for (let i = 0; i < n; i++) s += xc[i][j];
    s /= n;
    for (let i = 0; i < n; i++) xc[i][j] -= s;
  }
  const P = 2 * nvar;
  const u = new Array(P).fill(0);
  const imat = kare(P);
  const schoen = Array.from({ length: olaySay }, () => new Array(nvar).fill(0));
  const a = new Array(nvar).fill(0);
  const a2 = new Array(nvar).fill(0);
  const cmat = kare(nvar);
  const cmat2 = kare(nvar);
  let nevent = olaySay;
  let cstrat = -1;
  let denom = 0;
  let ip = n - 1;
  while (ip >= 0) {
    let person = sira[ip];
    if (strata[person] !== cstrat) {
      cstrat = strata[person];
      denom = 0;
      a.fill(0);
      for (const s of cmat) s.fill(0);
    }
    const dtime = t[person];
    const timewt = gtime[person];
    let ndead = 0;
    let deadwt = 0;
    let denom2 = 0;
    for (; ip >= 0; ip--) {
      person = sira[ip];
      if (t[person] !== dtime || strata[person] !== cstrat) break;
      const risk = Math.exp(lp[person]);
      const x = xc[person];
      if (d[person] === 0) {
        denom += risk;
        for (let i = 0; i < nvar; i++) {
          a[i] += risk * x[i];
          for (let j = 0; j <= i; j++) cmat[i][j] += risk * x[i] * x[j];
        }
      } else {
        ndead++;
        deadwt += 1;
        denom2 += risk;
        nevent--;
        for (let i = 0; i < nvar; i++) {
          schoen[nevent][i] = x[i];
          u[i] += x[i];
          u[i + nvar] += timewt * x[i];
          a2[i] += risk * x[i];
          for (let j = 0; j <= i; j++) cmat2[i][j] += risk * x[i] * x[j];
        }
      }
    }
    if (ndead > 0) {
      if (!efron) {
        denom += denom2;
        for (let i = 0; i < nvar; i++) {
          a[i] += a2[i];
          const temp2 = a[i] / denom;
          u[i] -= deadwt * temp2;
          u[i + nvar] -= timewt * deadwt * temp2;
          for (let j = 0; j <= i; j++) {
            cmat[i][j] += cmat2[i][j];
            const temp = (deadwt * (cmat[i][j] - temp2 * a[j])) / denom;
            imat[j][i] += temp;
            imat[j][i + nvar] += temp * timewt;
            imat[j + nvar][i + nvar] += temp * timewt * timewt;
          }
          for (let j = 0; j < ndead; j++) schoen[nevent + j][i] -= temp2;
        }
      } else {
        const wtave = deadwt / ndead;
        for (let i = 0; i < nvar; i++) {
          let tmean = 0;
          for (let k = 0; k < ndead; k++) {
            const temp = (k + 1) / ndead;
            tmean += (a[i] + a2[i] * temp) / (denom + denom2 * temp);
          }
          for (let j = 0; j < ndead; j++) schoen[nevent + j][i] -= tmean / ndead;
        }
        for (let k = 0; k < ndead; k++) {
          denom += denom2 / ndead;
          for (let i = 0; i < nvar; i++) {
            a[i] += a2[i] / ndead;
            const temp2 = a[i] / denom;
            u[i] -= wtave * temp2;
            u[i + nvar] -= timewt * wtave * temp2;
            for (let j = 0; j <= i; j++) {
              cmat[i][j] += cmat2[i][j] / ndead;
              const temp = (wtave * (cmat[i][j] - temp2 * a[j])) / denom;
              imat[j][i] += temp;
              imat[j][i + nvar] += timewt * temp;
              imat[j + nvar][i + nvar] += timewt * timewt * temp;
            }
          }
        }
      }
      a2.fill(0);
      for (const s of cmat2) s.fill(0);
    }
  }
  for (let i = 0; i < nvar; i++) {
    for (let j = 0; j < i; j++) {
      imat[i][j] = imat[j][i];
      imat[i][j + nvar] = imat[j][i + nvar];
      imat[i + nvar][j + nvar] = imat[j + nvar][i + nvar];
    }
  }
  for (let i = 0; i < nvar; i++) for (let j = 0; j < nvar; j++) imat[i + nvar][j] = imat[j][i + nvar];

  // Testler
  const testler = terimler.map((jj, ii) => {
    const kk = [...Array(nvar).keys(), ...jj.map((j) => j + nvar)];
    const im = kk.map((r) => kk.map((c) => imat[r][c]));
    const uu = [...new Array(nvar).fill(0), ...jj.map((j) => u[j + nvar])];
    const s = coz(im, uu);
    let ist = 0;
    for (let r = 0; r < uu.length; r++) ist += s[r] * uu[r];
    return { ad: terimAdlari[ii], istatistik: ist, sd: jj.length, p: pchisq(ist, jj.length, false) };
  });
  const ug = [...new Array(nvar).fill(0), ...u.slice(nvar)];
  const sg = coz(imat, ug);
  let genelIst = 0;
  for (let r = 0; r < P; r++) genelIst += sg[r] * ug[r];
  const genel = { ad: 'GENEL', istatistik: genelIst, sd: nvar, p: pchisq(genelIst, nvar, false) };

  // Ölçeklenmiş Schoenfeld artıkları
  const olayIdx = sira.filter((i) => d[i] === 1);
  const terimSayisi = terimler.length;
  let usedT = used.map((s) => s.slice());
  for (const jj of terimler) {
    if (jj.length > 1) {
      for (const s of usedT) if (jj.some((j) => s[j] === 0)) {
        const m = Math.max(...jj.map((j) => s[j]));
        for (const j of jj) s[j] = m;
      }
    }
  }
  const wtmat = kare(nvar);
  for (const s of usedT) for (let i = 0; i < nvar; i++) for (let j = 0; j < nvar; j++) wtmat[i][j] += Math.min(s[i], s[j]);
  let vmean = kare(nvar).map((s, i) => s.map((_, j) => imat[i][j] / (wtmat[i][j] === 0 ? 1 : wtmat[i][j])));
  let sresid = schoen.map((s) => s.slice());
  const cokSutunlu = terimler.some((tr) => tr.length > 1);
  if (cokSutunlu) {
    const temp = Array.from({ length: nvar }, () => new Array(terimSayisi).fill(0));
    terimler.forEach((jj, i) => {
      for (const j of jj) temp[j][i] = jj.length === 1 ? 1 : fcoef[j];
    });
    sresid = sresid.map((s) => temp[0].map((_, c) => s.reduce((acc, v, r) => acc + v * temp[r][c], 0)));
    const tv = temp[0].map((_, c) => vmean.map((sat) => sat.reduce((acc, v, r) => acc + v * temp[r][c], 0)));
    vmean = tv.map((sat) => temp[0].map((_, c) => sat.reduce((acc, v, r) => acc + v * temp[r][c], 0)));
    usedT = usedT.map((s) => terimler.map((jj) => s[jj[0]]));
  }
  // Tabaka başına: artık · vmean⁻¹ (R: sgrp <- rep(1:nrow(used), apply(used, 1, max)))
  let satir = 0;
  for (const s of usedT) {
    const adet = Math.max(...s);
    const k = s.map((v, i) => i).filter((i) => s[i] > 0);
    if (k.length > 0) {
      const vk = k.map((r) => k.map((c) => vmean[r][c]));
      for (let r = satir; r < satir + adet; r++) {
        const cz = coz(vk, k.map((c) => sresid[r][c]));
        const yeni = new Array(terimSayisi).fill(null);
        k.forEach((c, m) => (yeni[c] = cz[m]));
        sresid[r] = yeni;
      }
    }
    satir += adet;
  }
  const olcekli = sresid.map((s) =>
    terimler.map((jj, i) => (s[i] === null ? null : s[i] + (jj.length === 1 ? fcoef[jj[0]] : 1))),
  );
  return {
    yontem: 'cox-zph',
    ad: 'Orantılı hazard varsayımı (Schoenfeld artıkları, skor testi)',
    donusum: 'km',
    testler,
    genel,
    artiklar: {
      zaman: olayIdx.map((i) => t[i]),
      gZaman: olayIdx.map((i) => ttimes[i]),
      terimler: terimAdlari,
      olcekli,
    },
    varyans: tersMatris(vmean),
    uyarilar: testler.filter((x) => x.p < 0.05).map((x) => `${x.ad} için orantılı hazard varsayımı karşılanmıyor olabilir (p = ${x.p.toPrecision(3)}).`),
  };
}
