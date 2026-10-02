// Özel matematik fonksiyonları ve sayısal yöntemler.
// Bu dosyadaki her şey dağılım fonksiyonlarının temelidir; doğruluk R ile test/dagilim.test.js içinde karşılaştırılır.

export const EPS = 2.220446049250313e-16;
export const LOG_SQRT_2PI = 0.9189385332046727; // log(sqrt(2*pi))
export const SQRT_2PI = 2.5066282746310002;
export const LN2 = Math.LN2;

// ---------- Gamma ----------

// Stirling serisinin katsayıları: B(2k) / (2k (2k-1)), k = 1..8
const STIRLING = [
  1 / 12,
  -1 / 360,
  1 / 1260,
  -1 / 1680,
  1 / 1188,
  -691 / 360360,
  1 / 156,
  -3617 / 122400,
];

/** log Γ(x), x > 0. Küçük x değerleri yineleme bağıntısıyla 15'in üstüne taşınır, sonra Stirling serisi kullanılır. */
export function lgamma(x) {
  if (Number.isNaN(x)) return NaN;
  if (x <= 0) {
    if (Number.isInteger(x)) return Infinity;
    // Yansıma: Γ(x)Γ(1-x) = π / sin(πx)
    return Math.log(Math.PI / Math.abs(Math.sin(Math.PI * x))) - lgamma(1 - x);
  }
  if (x === 1 || x === 2) return 0;
  let kaydirma = 0;
  if (x < 15) {
    let carpim = 1;
    while (x < 15) {
      carpim *= x;
      x += 1;
    }
    kaydirma = Math.log(carpim);
  }
  const x2 = 1 / (x * x);
  let seri = 0;
  for (let k = STIRLING.length - 1; k >= 0; k--) seri = seri * x2 + STIRLING[k];
  return (x - 0.5) * Math.log(x) - x + LOG_SQRT_2PI + seri / x - kaydirma;
}

const LOG_FAKTORIYEL = [0];
/** log(n!) — tamsayılar için önbellekli. */
export function logFaktoriyel(n) {
  if (n < 0) return NaN;
  if (n < 1000) {
    while (LOG_FAKTORIYEL.length <= n) {
      const i = LOG_FAKTORIYEL.length;
      LOG_FAKTORIYEL.push(lgamma(i + 1));
    }
    return LOG_FAKTORIYEL[n];
  }
  return lgamma(n + 1);
}

/** log C(n, k) */
export function logKombinasyon(n, k) {
  if (k < 0 || k > n) return -Infinity;
  return logFaktoriyel(n) - logFaktoriyel(k) - logFaktoriyel(n - k);
}

export function gamma(x) {
  if (x > 0 && x < 171) {
    if (Number.isInteger(x)) return Math.round(Math.exp(logFaktoriyel(x - 1)));
    return Math.exp(lgamma(x));
  }
  if (x >= 171) return Infinity;
  if (Number.isInteger(x)) return NaN;
  return Math.PI / (Math.sin(Math.PI * x) * gamma(1 - x));
}

/** Stirling düzeltme terimi: lgamma(x) - [(x - 0.5) ln x - x + ln √(2π)] */
export function stirlingDuzeltme(x) {
  if (x < 15) return lgamma(x) - ((x - 0.5) * Math.log(x) - x + LOG_SQRT_2PI);
  const x2 = 1 / (x * x);
  let seri = 0;
  for (let k = STIRLING.length - 1; k >= 0; k--) seri = seri * x2 + STIRLING[k];
  return seri / x;
}

/** log(1 + d) - d; küçük d için seri ile (iptal hatasını önler). */
export function log1pmx(d) {
  if (Math.abs(d) > 0.25) return Math.log1p(d) - d;
  // -d²/2 + d³/3 - d⁴/4 + ...
  let terim = d;
  let toplam = 0;
  for (let k = 2; k < 200; k++) {
    terim *= -d;
    const katki = terim / k;
    toplam += katki;
    if (Math.abs(katki) < Math.abs(toplam) * EPS * 0.5) break;
  }
  return toplam;
}

/** lgamma(a + b) - lgamma(a), a büyükken iptal hatası olmadan. */
export function lgammaFark(a, b) {
  if (a < 15) return lgamma(a + b) - lgamma(a);
  return (a - 0.5) * Math.log1p(b / a) + b * Math.log(a + b) - b + stirlingDuzeltme(a + b) - stirlingDuzeltme(a);
}

/** log B(a, b) — büyük parametrelerde Stirling açılımıyla hassas. */
export function lbeta(a, b) {
  if (a < b) [a, b] = [b, a]; // a >= b
  if (b >= 15) {
    const s = a + b;
    return (
      (a - 0.5) * Math.log(a / s) +
      (b - 0.5) * Math.log(b / s) -
      0.5 * Math.log(s) +
      LOG_SQRT_2PI +
      stirlingDuzeltme(a) +
      stirlingDuzeltme(b) -
      stirlingDuzeltme(s)
    );
  }
  if (a >= 15) return lgamma(b) - lgammaFark(a, b);
  return lgamma(a) + lgamma(b) - lgamma(a + b);
}

/** Digamma ψ(x), x > 0 (Cox regresyonunda ve bazı tahmin edicilerde kullanılır). */
export function digamma(x) {
  let sonuc = 0;
  while (x < 6) {
    sonuc -= 1 / x;
    x += 1;
  }
  const f = 1 / (x * x);
  const t = f * (-1 / 12 + f * (1 / 120 + f * (-1 / 252 + f * (1 / 240 + f * (-1 / 132 + f * (691 / 32760 + f * (-1 / 12)))))));
  return sonuc + Math.log(x) - 0.5 / x + t;
}

// ---------- Düzenlenmiş eksik gamma ----------

const MAKS_IT = 200000;
const KUCUK = 1e-300;

function gammaOnCarpan(a, x) {
  // x^a e^-x / Γ(a), log ölçekte. Büyük a için a ln(x/a) + (a - x) kısmı, x a'ya yakınsa log1pmx ile iptalsiz hesaplanır.
  if (a < 15) return a * Math.log(x) - x - lgamma(a);
  const d = (x - a) / a;
  const ana = Math.abs(d) < 0.5 ? a * log1pmx(d) : a * Math.log(x / a) + (a - x);
  return ana + 0.5 * Math.log(a) - LOG_SQRT_2PI - stirlingDuzeltme(a);
}

function gammaSeri(a, x) {
  let toplam = 1 / a;
  let terim = toplam;
  let ap = a;
  for (let n = 0; n < MAKS_IT; n++) {
    ap += 1;
    terim *= x / ap;
    toplam += terim;
    if (Math.abs(terim) < Math.abs(toplam) * EPS) break;
  }
  return toplam * Math.exp(gammaOnCarpan(a, x));
}

function gammaZincirKesir(a, x) {
  // Modifiye Lentz yöntemi, Q(a, x) için
  let b = x + 1 - a;
  let c = 1 / KUCUK;
  let d = 1 / b;
  let h = d;
  for (let i = 1; i < MAKS_IT; i++) {
    const an = -i * (i - a);
    b += 2;
    d = an * d + b;
    if (Math.abs(d) < KUCUK) d = KUCUK;
    c = b + an / c;
    if (Math.abs(c) < KUCUK) c = KUCUK;
    d = 1 / d;
    const del = d * c;
    h *= del;
    if (Math.abs(del - 1) < EPS) break;
  }
  return Math.exp(gammaOnCarpan(a, x)) * h;
}

/** Düzenlenmiş alt eksik gamma P(a, x) */
export function gammaP(a, x) {
  if (x <= 0) return 0;
  if (!Number.isFinite(x)) return 1;
  if (x < a + 1) return gammaSeri(a, x);
  return 1 - gammaZincirKesir(a, x);
}

/** Düzenlenmiş üst eksik gamma Q(a, x) = 1 - P(a, x); kuyrukta göreli doğruluğu korur. */
export function gammaQ(a, x) {
  if (x <= 0) return 1;
  if (!Number.isFinite(x)) return 0;
  if (x < a + 1) return 1 - gammaSeri(a, x);
  return gammaZincirKesir(a, x);
}

// ---------- Düzenlenmiş eksik beta ----------

function betaZincirKesir(x, a, b) {
  const qab = a + b;
  const qap = a + 1;
  const qam = a - 1;
  let c = 1;
  let d = 1 - (qab * x) / qap;
  if (Math.abs(d) < KUCUK) d = KUCUK;
  d = 1 / d;
  let h = d;
  for (let m = 1; m < MAKS_IT; m++) {
    const m2 = 2 * m;
    let aa = (m * (b - m) * x) / ((qam + m2) * (a + m2));
    d = 1 + aa * d;
    if (Math.abs(d) < KUCUK) d = KUCUK;
    c = 1 + aa / c;
    if (Math.abs(c) < KUCUK) c = KUCUK;
    d = 1 / d;
    h *= d * c;
    aa = (-(a + m) * (qab + m) * x) / ((a + m2) * (qap + m2));
    d = 1 + aa * d;
    if (Math.abs(d) < KUCUK) d = KUCUK;
    c = 1 + aa / c;
    if (Math.abs(c) < KUCUK) c = KUCUK;
    d = 1 / d;
    const del = d * c;
    h *= del;
    if (Math.abs(del - 1) < EPS) break;
  }
  return h;
}

/** log[x^a y^b / B(a, b)]; a ve b büyükken iptalsiz biçim kullanılır. */
export function betaOnCarpan(x, y, a, b) {
  // 1'e yakın argümanların logaritması, tümleyeni üzerinden log1p ile alınır
  const lnx = x <= 0.5 ? Math.log(x) : Math.log1p(-y);
  const lny = y <= 0.5 ? Math.log(y) : Math.log1p(-x);
  if (a < 15 || b < 15) return a * lnx + b * lny - lbeta(a, b);
  const s = a + b;
  const e = x <= 0.5 ? s * x - a : b - s * y;
  // a ln(xs/a) + b ln(ys/b) = a·log1pmx(e/a) + b·log1pmx(-e/b) (doğrusal terimler birbirini götürür)
  const terimA = Math.abs(e / a) < 0.5 ? a * log1pmx(e / a) : a * (lnx + Math.log(s / a)) - e;
  const terimB = Math.abs(e / b) < 0.5 ? b * log1pmx(-e / b) : b * (lny + Math.log(s / b)) + e;
  return (
    terimA +
    terimB +
    0.5 * Math.log((a * b) / s) -
    LOG_SQRT_2PI -
    stirlingDuzeltme(a) -
    stirlingDuzeltme(b) +
    stirlingDuzeltme(s)
  );
}

/**
 * Düzenlenmiş eksik beta I_x(a, b). y = 1 - x ayrıca verilebilir (kuyruklarda hassasiyet kaybını önler).
 * altKuyruk = false ise 1 - I_x(a, b) döner.
 */
export function ibeta(x, a, b, altKuyruk = true, y = 1 - x) {
  if (x <= 0) return altKuyruk ? 0 : 1;
  if (y <= 0) return altKuyruk ? 1 : 0;
  const logOn = betaOnCarpan(x, y, a, b);
  // Zincir kesir, x < (a+1)/(a+b+2) bölgesinde hızlı yakınsar; diğer bölgede simetri kullanılır.
  if (x < (a + 1) / (a + b + 2)) {
    const deger = (Math.exp(logOn) * betaZincirKesir(x, a, b)) / a;
    return altKuyruk ? deger : 1 - deger;
  }
  const deger = (Math.exp(logOn) * betaZincirKesir(y, b, a)) / b;
  return altKuyruk ? 1 - deger : deger;
}

// ---------- Kök bulma ----------

/**
 * Brent yöntemi (R'nin uniroot/zeroin'i ile aynı yaklaşım). f(a) ve f(b) zıt işaretli olmalı.
 */
export function brent(f, a, b, tol = 1e-13, maksIt = 500) {
  let fa = f(a);
  let fb = f(b);
  if (fa === 0) return a;
  if (fb === 0) return b;
  if (fa * fb > 0) throw new Error('brent: aralık kökü içermiyor');
  let c = a;
  let fc = fa;
  let d = b - a;
  let e = d;
  for (let it = 0; it < maksIt; it++) {
    if (fb * fc > 0) {
      c = a;
      fc = fa;
      d = b - a;
      e = d;
    }
    if (Math.abs(fc) < Math.abs(fb)) {
      a = b;
      b = c;
      c = a;
      fa = fb;
      fb = fc;
      fc = fa;
    }
    const tol1 = 2 * EPS * Math.abs(b) + 0.5 * tol;
    const xm = 0.5 * (c - b);
    if (Math.abs(xm) <= tol1 || fb === 0) return b;
    if (Math.abs(e) >= tol1 && Math.abs(fa) > Math.abs(fb)) {
      const s = fb / fa;
      let p;
      let q;
      if (a === c) {
        p = 2 * xm * s;
        q = 1 - s;
      } else {
        const qq = fa / fc;
        const r = fb / fc;
        p = s * (2 * xm * qq * (qq - r) - (b - a) * (r - 1));
        q = (qq - 1) * (r - 1) * (s - 1);
      }
      if (p > 0) q = -q;
      p = Math.abs(p);
      const min1 = 3 * xm * q - Math.abs(tol1 * q);
      const min2 = Math.abs(e * q);
      if (2 * p < Math.min(min1, min2)) {
        e = d;
        d = p / q;
      } else {
        d = xm;
        e = d;
      }
    } else {
      d = xm;
      e = d;
    }
    a = b;
    fa = fb;
    b += Math.abs(d) > tol1 ? d : xm > 0 ? tol1 : -tol1;
    fb = f(b);
  }
  return b;
}

/**
 * Artan bir g fonksiyonu için g(x) = hedef çözümü. Aralık bilinmiyorsa başlangıç noktasından genişletilerek bulunur.
 * alt/ust: tanım kümesi sınırları.
 */
export function tersCoz(g, hedef, baslangic, alt = -Infinity, ust = Infinity, tol = 1e-12) {
  const f = (x) => g(x) - hedef;
  let a = baslangic;
  let b = baslangic;
  let fa = f(a);
  if (fa === 0) return a;
  let adim = Math.max(Math.abs(baslangic) * 0.1, 0.5);
  if (fa < 0) {
    // kök sağda
    let fb = fa;
    let i = 0;
    while (fb < 0 && i++ < 2000) {
      a = b;
      b = Math.min(b + adim, ust);
      fb = f(b);
      if (b === ust && fb < 0) return ust;
      adim *= 2;
    }
  } else {
    let i = 0;
    let fb = fa;
    b = a;
    while (fb > 0 && i++ < 2000) {
      b = a;
      a = Math.max(a - adim, alt);
      fb = f(a);
      if (a === alt && fb > 0) return alt;
      adim *= 2;
    }
  }
  return brent(f, a, b, tol * Math.max(1, Math.abs(a), Math.abs(b)));
}

// ---------- Gauss-Legendre integrali ----------

const GL_ONBELLEK = new Map();

/** n noktalı Gauss-Legendre düğüm ve ağırlıkları ([-1, 1] aralığı). */
export function gaussLegendre(n) {
  if (GL_ONBELLEK.has(n)) return GL_ONBELLEK.get(n);
  const x = new Float64Array(n);
  const w = new Float64Array(n);
  const m = Math.floor((n + 1) / 2);
  for (let i = 0; i < m; i++) {
    let z = Math.cos((Math.PI * (i + 0.75)) / (n + 0.5));
    let pp = 0;
    for (let it = 0; it < 100; it++) {
      let p1 = 1;
      let p2 = 0;
      for (let j = 1; j <= n; j++) {
        const p3 = p2;
        p2 = p1;
        p1 = ((2 * j - 1) * z * p2 - (j - 1) * p3) / j;
      }
      pp = (n * (z * p1 - p2)) / (z * z - 1);
      const z1 = z;
      z = z1 - p1 / pp;
      if (Math.abs(z - z1) < 1e-15) break;
    }
    x[i] = -z;
    x[n - 1 - i] = z;
    w[i] = 2 / ((1 - z * z) * pp * pp);
    w[n - 1 - i] = w[i];
  }
  const sonuc = { x, w };
  GL_ONBELLEK.set(n, sonuc);
  return sonuc;
}

function glAralik(f, a, b, n = 20) {
  const { x, w } = gaussLegendre(n);
  const yari = (b - a) / 2;
  const orta = (a + b) / 2;
  let s = 0;
  for (let i = 0; i < n; i++) s += w[i] * f(orta + yari * x[i]);
  return s * yari;
}

/** Uyarlamalı Gauss-Legendre integrali: aralık, iki yarısının toplamı ile tutarlı olana kadar bölünür. */
export function integral(f, a, b, tol = 1e-12, derinlik = 0) {
  const tum = glAralik(f, a, b);
  const orta = (a + b) / 2;
  const sol = glAralik(f, a, orta);
  const sag = glAralik(f, orta, b);
  const iki = sol + sag;
  if (Math.abs(iki - tum) <= tol * Math.max(1, Math.abs(iki)) || derinlik > 40) return iki;
  return integral(f, a, orta, tol, derinlik + 1) + integral(f, orta, b, tol, derinlik + 1);
}

// ---------- Yardımcılar ----------

/** log(exp(a) + exp(b)) taşmasız */
export function logTopla(a, b) {
  if (a === -Infinity) return b;
  if (b === -Infinity) return a;
  const m = Math.max(a, b);
  return m + Math.log1p(Math.exp(-Math.abs(a - b)));
}

export function sinirla(x, alt, ust) {
  return Math.min(Math.max(x, alt), ust);
}
