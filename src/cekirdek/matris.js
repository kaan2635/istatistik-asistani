// Doğrusal cebir: satır öncelikli dizi dizileri (number[][]) üzerinde çalışır.
// Regresyon, GLM ve tekrarlı ölçümler modülleri tarafından kullanılır.

import { IstatistikHatasi } from './hata.js';

export function sifir(r, c) {
  return Array.from({ length: r }, () => new Array(c).fill(0));
}

export function birim(n) {
  const I = sifir(n, n);
  for (let i = 0; i < n; i++) I[i][i] = 1;
  return I;
}

export function kopya(A) {
  return A.map((s) => s.slice());
}

export function transpoz(A) {
  const r = A.length;
  const c = r ? A[0].length : 0;
  const T = sifir(c, r);
  for (let i = 0; i < r; i++) for (let j = 0; j < c; j++) T[j][i] = A[i][j];
  return T;
}

export function carp(A, B) {
  const r = A.length;
  const k = B.length;
  const c = k ? B[0].length : 0;
  const C = sifir(r, c);
  for (let i = 0; i < r; i++) {
    const Ai = A[i];
    const Ci = C[i];
    for (let m = 0; m < k; m++) {
      const a = Ai[m];
      if (a === 0) continue;
      const Bm = B[m];
      for (let j = 0; j < c; j++) Ci[j] += a * Bm[j];
    }
  }
  return C;
}

export function carpVektor(A, v) {
  return A.map((s) => {
    let t = 0;
    for (let j = 0; j < v.length; j++) t += s[j] * v[j];
    return t;
  });
}

export function icCarpim(a, b) {
  let t = 0;
  for (let i = 0; i < a.length; i++) t += a[i] * b[i];
  return t;
}

/** X'X (simetrik) */
export function XtX(X, agirlik = null) {
  const n = X.length;
  const p = n ? X[0].length : 0;
  const M = sifir(p, p);
  for (let i = 0; i < n; i++) {
    const xi = X[i];
    const w = agirlik ? agirlik[i] : 1;
    for (let a = 0; a < p; a++) {
      const v = xi[a] * w;
      if (v === 0) continue;
      const Ma = M[a];
      for (let b = a; b < p; b++) Ma[b] += v * xi[b];
    }
  }
  for (let a = 0; a < p; a++) for (let b = 0; b < a; b++) M[a][b] = M[b][a];
  return M;
}

/** X'y */
export function Xty(X, y, agirlik = null) {
  const p = X.length ? X[0].length : 0;
  const v = new Array(p).fill(0);
  for (let i = 0; i < X.length; i++) {
    const w = agirlik ? agirlik[i] : 1;
    const xi = X[i];
    for (let a = 0; a < p; a++) v[a] += xi[a] * y[i] * w;
  }
  return v;
}

export function iz(A) {
  let t = 0;
  for (let i = 0; i < A.length; i++) t += A[i][i];
  return t;
}

/** Cholesky ayrıştırması A = L L'. Pozitif tanımlı değilse null döner. */
export function cholesky(A) {
  const n = A.length;
  const L = sifir(n, n);
  for (let i = 0; i < n; i++) {
    for (let j = 0; j <= i; j++) {
      let s = A[i][j];
      for (let k = 0; k < j; k++) s -= L[i][k] * L[j][k];
      if (i === j) {
        if (!(s > 0)) return null;
        L[i][i] = Math.sqrt(s);
      } else {
        L[i][j] = s / L[j][j];
      }
    }
  }
  return L;
}

/**
 * Kısmi pivotlu LU ayrıştırması. Tekil matris için null döner.
 * @returns {{LU:number[][], pivot:number[], isaret:number}|null}
 */
export function lu(A) {
  const n = A.length;
  const LU = kopya(A);
  const pivot = Array.from({ length: n }, (_, i) => i);
  let isaret = 1;
  let olcek = 0;
  for (const s of A) for (const v of s) olcek = Math.max(olcek, Math.abs(v));
  const esik = olcek * n * 1e-14;
  for (let k = 0; k < n; k++) {
    let maks = 0;
    let p = k;
    for (let i = k; i < n; i++) {
      const v = Math.abs(LU[i][k]);
      if (v > maks) {
        maks = v;
        p = i;
      }
    }
    if (maks <= esik) return null;
    if (p !== k) {
      [LU[p], LU[k]] = [LU[k], LU[p]];
      [pivot[p], pivot[k]] = [pivot[k], pivot[p]];
      isaret = -isaret;
    }
    for (let i = k + 1; i < n; i++) {
      LU[i][k] /= LU[k][k];
      const f = LU[i][k];
      if (f === 0) continue;
      for (let j = k + 1; j < n; j++) LU[i][j] -= f * LU[k][j];
    }
  }
  return { LU, pivot, isaret };
}

function luCoz({ LU, pivot }, b) {
  const n = LU.length;
  const x = pivot.map((p) => b[p]);
  for (let i = 0; i < n; i++) for (let j = 0; j < i; j++) x[i] -= LU[i][j] * x[j];
  for (let i = n - 1; i >= 0; i--) {
    for (let j = i + 1; j < n; j++) x[i] -= LU[i][j] * x[j];
    x[i] /= LU[i][i];
  }
  return x;
}

/** A x = b çözümü */
export function coz(A, b) {
  const d = lu(A);
  if (!d) throw new IstatistikHatasi('Matris tekil: değişkenler arasında tam doğrusal bağımlılık var.');
  return luCoz(d, b);
}

/** Ters matris (LU ile). Tekilse hata verir. */
export function ters(A) {
  const n = A.length;
  const d = lu(A);
  if (!d) throw new IstatistikHatasi('Matris tekil: değişkenler arasında tam doğrusal bağımlılık var.');
  const T = sifir(n, n);
  for (let j = 0; j < n; j++) {
    const e = new Array(n).fill(0);
    e[j] = 1;
    const sutun = luCoz(d, e);
    for (let i = 0; i < n; i++) T[i][j] = sutun[i];
  }
  return T;
}

/** Simetrik pozitif tanımlı matrisin tersi (Cholesky ile; değilse LU'ya düşer) */
export function simetrikTers(A) {
  const L = cholesky(A);
  if (!L) return ters(A);
  const n = A.length;
  // L^-1
  const Li = sifir(n, n);
  for (let i = 0; i < n; i++) {
    Li[i][i] = 1 / L[i][i];
    for (let j = 0; j < i; j++) {
      let s = 0;
      for (let k = j; k < i; k++) s -= L[i][k] * Li[k][j];
      Li[i][j] = s / L[i][i];
    }
  }
  // A^-1 = L^-T L^-1
  const T = sifir(n, n);
  for (let i = 0; i < n; i++) {
    for (let j = 0; j <= i; j++) {
      let s = 0;
      for (let k = i; k < n; k++) s += Li[k][i] * Li[k][j];
      T[i][j] = s;
      T[j][i] = s;
    }
  }
  return T;
}

export function determinant(A) {
  const d = lu(A);
  if (!d) return 0;
  let det = d.isaret;
  for (let i = 0; i < A.length; i++) det *= d.LU[i][i];
  return det;
}

/** log|det(A)|; tekilse -Infinity */
export function logDeterminant(A) {
  const d = lu(A);
  if (!d) return -Infinity;
  let s = 0;
  for (let i = 0; i < A.length; i++) s += Math.log(Math.abs(d.LU[i][i]));
  return s;
}

/**
 * Sütun pivotlu Householder QR ile en küçük kareler. Doğrusal bağımlı sütunları saptar (R'nin lm()'i gibi).
 * @returns {{katsayi:(number|null)[], rank:number, bagimli:number[], kalan:number[], uyum:number[], XtXTers:number[][]}}
 *   bagimli: diğer sütunların doğrusal bileşimi olduğu için modelden atılan sütun indeksleri.
 */
export function enKucukKareler(X, y, tol = 1e-7) {
  const n = X.length;
  const p = n ? X[0].length : 0;
  const A = kopya(X);
  const b = y.slice();
  const sira = Array.from({ length: p }, (_, j) => j);
  const normlar = new Array(p).fill(0);
  for (let j = 0; j < p; j++) {
    let s = 0;
    for (let i = 0; i < n; i++) s += A[i][j] ** 2;
    normlar[j] = Math.sqrt(s);
  }
  const ilkNorm = normlar.slice();
  let rank = 0;
  const diag = [];
  // R'nin dqrdc2 yaklaşımı: sütunlar sırayla işlenir; kalan normu ilk normunun tol katından küçükse sona atılır
  let son = p;
  let k = 0;
  while (k < son) {
    // k. sütunun kalan normu
    let s = 0;
    for (let i = k; i < n; i++) s += A[i][k] ** 2;
    const kalanNorm = Math.sqrt(s);
    if (kalanNorm < tol * Math.max(ilkNorm[sira[k]], 1e-300) || kalanNorm === 0) {
      // sütunu sona taşı
      for (let i = 0; i < n; i++) {
        const v = A[i][k];
        for (let j = k; j < p - 1; j++) A[i][j] = A[i][j + 1];
        A[i][p - 1] = v;
      }
      const sv = sira[k];
      for (let j = k; j < p - 1; j++) sira[j] = sira[j + 1];
      sira[p - 1] = sv;
      son--;
      continue;
    }
    // Householder yansıması
    const alfa = A[k][k] > 0 ? -kalanNorm : kalanNorm;
    const v = new Array(n).fill(0);
    for (let i = k; i < n; i++) v[i] = A[i][k];
    v[k] -= alfa;
    let vNorm2 = 0;
    for (let i = k; i < n; i++) vNorm2 += v[i] * v[i];
    if (vNorm2 > 0) {
      for (let j = k; j < p; j++) {
        let d = 0;
        for (let i = k; i < n; i++) d += v[i] * A[i][j];
        const f = (2 * d) / vNorm2;
        for (let i = k; i < n; i++) A[i][j] -= f * v[i];
      }
      let d = 0;
      for (let i = k; i < n; i++) d += v[i] * b[i];
      const f = (2 * d) / vNorm2;
      for (let i = k; i < n; i++) b[i] -= f * v[i];
    }
    diag.push(A[k][k]);
    rank++;
    k++;
  }
  // R β = Q'y (ilk rank satır)
  const beta = new Array(rank).fill(0);
  for (let i = rank - 1; i >= 0; i--) {
    let s = b[i];
    for (let j = i + 1; j < rank; j++) s -= A[i][j] * beta[j];
    beta[i] = s / A[i][i];
  }
  const katsayi = new Array(p).fill(null);
  for (let j = 0; j < rank; j++) katsayi[sira[j]] = beta[j];
  const bagimli = sira.slice(rank).sort((a, c) => a - c);
  // (R'R)^-1 → kullanılan sütunlar için (X'X)^-1
  const Rm = sifir(rank, rank);
  for (let i = 0; i < rank; i++) for (let j = i; j < rank; j++) Rm[i][j] = A[i][j];
  const Ri = sifir(rank, rank);
  for (let i = rank - 1; i >= 0; i--) {
    Ri[i][i] = 1 / Rm[i][i];
    for (let j = i + 1; j < rank; j++) {
      let s = 0;
      for (let m = i + 1; m <= j; m++) s -= Rm[i][m] * Ri[m][j];
      Ri[i][j] = s / Rm[i][i];
    }
  }
  const XtXTersKucuk = sifir(rank, rank);
  for (let i = 0; i < rank; i++) {
    for (let j = 0; j < rank; j++) {
      let s = 0;
      for (let m = Math.max(i, j); m < rank; m++) s += Ri[i][m] * Ri[j][m];
      XtXTersKucuk[i][j] = s;
    }
  }
  const XtXTers = sifir(p, p).map((s) => s.fill(null));
  for (let i = 0; i < rank; i++) for (let j = 0; j < rank; j++) XtXTers[sira[i]][sira[j]] = XtXTersKucuk[i][j];
  const uyum = X.map((s) => {
    let t = 0;
    for (let j = 0; j < p; j++) if (katsayi[j] !== null) t += s[j] * katsayi[j];
    return t;
  });
  const kalan = y.map((v, i) => v - uyum[i]);
  return { katsayi, rank, bagimli, kalan, uyum, XtXTers };
}

/**
 * Simetrik matrisin özdeğer ve özvektörleri (Jacobi dönüşümleri). Özdeğerler büyükten küçüğe sıralıdır.
 * @returns {{degerler:number[], vektorler:number[][]}} vektorler[i][k]: k. özvektörün i. bileşeni
 */
export function simetrikOzdeger(S) {
  const n = S.length;
  const A = kopya(S);
  const V = birim(n);
  for (let tur = 0; tur < 100; tur++) {
    let disToplam = 0;
    for (let i = 0; i < n; i++) for (let j = i + 1; j < n; j++) disToplam += A[i][j] ** 2;
    if (disToplam < 1e-30) break;
    for (let p = 0; p < n; p++) {
      for (let q = p + 1; q < n; q++) {
        if (Math.abs(A[p][q]) < 1e-300) continue;
        const theta = (A[q][q] - A[p][p]) / (2 * A[p][q]);
        const t = Math.sign(theta || 1) / (Math.abs(theta) + Math.sqrt(theta * theta + 1));
        const c = 1 / Math.sqrt(t * t + 1);
        const s = t * c;
        for (let k = 0; k < n; k++) {
          const akp = A[k][p];
          const akq = A[k][q];
          A[k][p] = c * akp - s * akq;
          A[k][q] = s * akp + c * akq;
        }
        for (let k = 0; k < n; k++) {
          const apk = A[p][k];
          const aqk = A[q][k];
          A[p][k] = c * apk - s * aqk;
          A[q][k] = s * apk + c * aqk;
        }
        for (let k = 0; k < n; k++) {
          const vkp = V[k][p];
          const vkq = V[k][q];
          V[k][p] = c * vkp - s * vkq;
          V[k][q] = s * vkp + c * vkq;
        }
      }
    }
  }
  const sira = Array.from({ length: n }, (_, i) => i).sort((a, b) => A[b][b] - A[a][a]);
  const degerler = sira.map((i) => A[i][i]);
  const vektorler = V.map((satir) => sira.map((i) => satir[i]));
  // İşaret standardı: her özvektörün en büyük mutlak bileşeni pozitif olsun
  for (let k = 0; k < n; k++) {
    let maks = 0;
    let isaret = 1;
    for (let i = 0; i < n; i++) {
      if (Math.abs(vektorler[i][k]) > maks) {
        maks = Math.abs(vektorler[i][k]);
        isaret = Math.sign(vektorler[i][k]) || 1;
      }
    }
    if (isaret < 0) for (let i = 0; i < n; i++) vektorler[i][k] = -vektorler[i][k];
  }
  return { degerler, vektorler };
}

/** Kovaryans matrisi (n-1 paydalı); veri[i][j]: i. gözlem, j. değişken */
export function kovaryansMatrisi(veri) {
  const n = veri.length;
  const p = veri[0].length;
  const ort = new Array(p).fill(0);
  for (const s of veri) for (let j = 0; j < p; j++) ort[j] += s[j] / n;
  const C = sifir(p, p);
  for (const s of veri) {
    for (let a = 0; a < p; a++) {
      const da = s[a] - ort[a];
      for (let b = a; b < p; b++) C[a][b] += da * (s[b] - ort[b]);
    }
  }
  for (let a = 0; a < p; a++) {
    for (let b = a; b < p; b++) {
      C[a][b] /= n - 1;
      C[b][a] = C[a][b];
    }
  }
  return C;
}

export function korelasyonMatrisi(veri) {
  const C = kovaryansMatrisi(veri);
  const p = C.length;
  const R = sifir(p, p);
  for (let a = 0; a < p; a++) for (let b = 0; b < p; b++) R[a][b] = C[a][b] / Math.sqrt(C[a][a] * C[b][b]);
  return R;
}
