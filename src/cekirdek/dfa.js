// Doğrulayıcı faktör analizi (DFA): basit yapılı ölçme modelleri (her gösterge tek faktöre yüklenir),
// en çok olabilirlik kestirimi. Varsayılanlar lavaan::cfa() ile aynıdır: işaretçi değişken yöntemi (ilk yük = 1),
// faktör kovaryansları serbest, beklenen bilgi matrisiyle standart hatalar, N paydalı örneklem kovaryansı
// (likelihood = "normal"), χ² = N·F_ML. Doğrulama: arac/r-referans/dfa.R (lavaan 0.6).

import { IstatistikHatasi, gerekli } from './hata.js';
import { sifir, carp, transpoz, simetrikTers, cholesky } from './matris.js';
import { pchisq, pnorm, pnchisq } from './dagilim.js';
import { brent } from './sayisal.js';
import { ozdegerAyristir } from './faktor.js';

// ---------------------------------------------------------------------------------------------
// Yardımcılar
// ---------------------------------------------------------------------------------------------

function nokta(a, b) {
  let t = 0;
  for (let i = 0; i < a.length; i++) t += a[i] * b[i];
  return t;
}

function matVek(A, v) {
  return A.map((s) => nokta(s, v));
}

function birimVektor(p, i) {
  const e = new Array(p).fill(0);
  e[i] = 1;
  return e;
}

/** log|Σ| ve Σ⁻¹ (Cholesky). Pozitif tanımlı değilse null. */
function logDetVeTers(S) {
  const L = cholesky(S);
  if (!L) return null;
  let ld = 0;
  for (let i = 0; i < S.length; i++) ld += 2 * Math.log(L[i][i]);
  return { logDet: ld, ters: simetrikTers(S) };
}

function pIkiYonlu(z) {
  return 2 * pnorm(-Math.abs(z));
}

// ---------------------------------------------------------------------------------------------
// Model kurulumu
// ---------------------------------------------------------------------------------------------

function modeliKur(secenek) {
  const { model, degiskenAdlari } = secenek;
  gerekli(model && typeof model === 'object' && !Array.isArray(model), 'DFA modeli {faktör: [göstergeler]} biçiminde bir nesne olmalıdır.');
  const faktorler = Object.keys(model);
  gerekli(faktorler.length >= 1, 'Modelde en az bir faktör olmalıdır.');
  gerekli(Array.isArray(degiskenAdlari), 'Değişken adları (degiskenAdlari) gerekir.');
  const gostergeler = [];
  const faktorunu = new Map();
  for (const f of faktorler) {
    const g = model[f];
    gerekli(Array.isArray(g) && g.length >= 2, `"${f}" faktörünün en az iki göstergesi olmalıdır.`);
    gerekli(!degiskenAdlari.includes(f), `Faktör adı "${f}" bir gözlenen değişkenle aynı olamaz.`);
    for (const ad of g) {
      gerekli(degiskenAdlari.includes(ad), `"${ad}" değişkeni veride bulunamadı.`);
      gerekli(!faktorunu.has(ad), `"${ad}" birden fazla faktöre yüklenmiş; çapraz yükler desteklenmiyor (basit yapı).`);
      faktorunu.set(ad, faktorler.indexOf(f));
      gostergeler.push(ad);
    }
  }
  const iliskili = (secenek.iliskiliHatalar ?? []).map((cift) => {
    gerekli(Array.isArray(cift) && cift.length === 2, 'İlişkili hatalar [değişken1, değişken2] çiftleri olarak verilmelidir.');
    const [a, b] = cift;
    gerekli(faktorunu.has(a) && faktorunu.has(b), `İlişkili hata çifti (${a}, ${b}) modeldeki göstergelerden olmalıdır.`);
    gerekli(a !== b, 'İlişkili hata çifti iki farklı değişkenden oluşmalıdır.');
    const i = gostergeler.indexOf(a);
    const j = gostergeler.indexOf(b);
    return i < j ? [i, j] : [j, i];
  });
  const anahtarlar = new Set(iliskili.map(([i, j]) => `${i},${j}`));
  gerekli(anahtarlar.size === iliskili.length, 'Aynı ilişkili hata çifti birden fazla kez verilmiş.');
  return { faktorler, gostergeler, faktorIndeksi: gostergeler.map((ad) => faktorunu.get(ad)), iliskili };
}

/** Örneklem kovaryansı (N paydalı) ve N */
function kovaryansHazirla(secenek, gostergeler) {
  const adlar = secenek.degiskenAdlari;
  const sira = gostergeler.map((ad) => adlar.indexOf(ad));
  const p = sira.length;
  if (Array.isArray(secenek.veri)) {
    const veri = secenek.veri;
    const n = veri.length;
    gerekli(n > p, `DFA için gözlem sayısı (${n}) gösterge sayısından (${p}) fazla olmalıdır.`);
    for (const s of veri) {
      gerekli(Array.isArray(s) && s.length === adlar.length, 'Veri satırlarındaki değer sayısı değişken adlarıyla aynı olmalıdır.');
      for (const j of sira) gerekli(Number.isFinite(s[j]), 'Veride eksik ya da sayı olmayan değer var; önce eksik değerleri ayıklayın.');
    }
    const ort = new Array(p).fill(0);
    for (const s of veri) sira.forEach((j, a) => (ort[a] += s[j] / n));
    const S = sifir(p, p);
    for (const s of veri) {
      for (let a = 0; a < p; a++) {
        const da = s[sira[a]] - ort[a];
        for (let b = a; b < p; b++) S[a][b] += da * (s[sira[b]] - ort[b]);
      }
    }
    for (let a = 0; a < p; a++) {
      for (let b = a; b < p; b++) {
        S[a][b] /= n;
        S[b][a] = S[a][b];
      }
    }
    return { S, n };
  }
  gerekli(Array.isArray(secenek.kovaryans), 'DFA için ham veri (veri) ya da kovaryans matrisi ve n gerekir.');
  const n = secenek.n;
  gerekli(Number.isFinite(n) && n > p, 'Kovaryans matrisi girdisinde örneklem büyüklüğü (n) gösterge sayısından büyük olmalıdır.');
  const K = secenek.kovaryans;
  gerekli(K.length === adlar.length && K.every((s) => Array.isArray(s) && s.length === adlar.length), 'Kovaryans matrisinin boyutu değişken adlarıyla uyumlu olmalıdır.');
  // lavaan sample.cov.rescale: (n − 1)/n ile N paydalı kovaryansa çevrilir
  const S = sira.map((i) =>
    sira.map((j) => {
      gerekli(Number.isFinite(K[i][j]) && Math.abs(K[i][j] - K[j][i]) <= 1e-8 * Math.max(1, Math.abs(K[i][j])), 'Kovaryans matrisi simetrik ve sayısal olmalıdır.');
      return (((K[i][j] + K[j][i]) / 2) * (n - 1)) / n;
    }),
  );
  return { S, n };
}

/**
 * Parametre listesi. Her parametre: {tur, i, j, f, g, serbest, deger}. tur:
 * 'yuk' (i: gösterge, f: faktör), 'faktorVaryans' (f), 'faktorKovaryans' (f, g), 'hataVaryans' (i), 'hataKovaryans' (i, j).
 */
function parametreleriKur(yapi, isaretci) {
  const { faktorler, gostergeler, faktorIndeksi, iliskili } = yapi;
  const prm = [];
  const ilkGosterge = faktorler.map((_, f) => faktorIndeksi.indexOf(f));
  gostergeler.forEach((_, i) => {
    const f = faktorIndeksi[i];
    prm.push({ tur: 'yuk', i, f, serbest: !(isaretci && ilkGosterge[f] === i), deger: 1 });
  });
  faktorler.forEach((_, f) => prm.push({ tur: 'faktorVaryans', f, serbest: isaretci, deger: 1 }));
  for (let f = 0; f < faktorler.length; f++) for (let g = f + 1; g < faktorler.length; g++) prm.push({ tur: 'faktorKovaryans', f, g, serbest: true, deger: 0 });
  gostergeler.forEach((_, i) => prm.push({ tur: 'hataVaryans', i, serbest: true, deger: 1 }));
  for (const [i, j] of iliskili) prm.push({ tur: 'hataKovaryans', i, j, serbest: true, deger: 0 });
  return prm;
}

/** Parametre değerlerinden Λ, Φ, Θ ve Σ = ΛΦΛ' + Θ */
function matrisler(prm, p, k) {
  const L = sifir(p, k);
  const Phi = sifir(k, k);
  const Th = sifir(p, p);
  for (const q of prm) {
    if (q.tur === 'yuk') L[q.i][q.f] = q.deger;
    else if (q.tur === 'faktorVaryans') Phi[q.f][q.f] = q.deger;
    else if (q.tur === 'faktorKovaryans') Phi[q.f][q.g] = Phi[q.g][q.f] = q.deger;
    else if (q.tur === 'hataVaryans') Th[q.i][q.i] = q.deger;
    else Th[q.i][q.j] = Th[q.j][q.i] = q.deger;
  }
  const LPhi = carp(L, Phi);
  const Sigma = carp(LPhi, transpoz(L)).map((s, a) => s.map((v, b) => v + Th[a][b]));
  return { L, Phi, Th, LPhi, Sigma };
}

/**
 * ∂Σ/∂θ = c (u v' + v u') gösterimi (düşük ranklı). u, v yoğun vektörler.
 * tur ve indeksler parametre nesnesinden; aday (sabit) parametreler için de kullanılır.
 */
function turevGosterimi(q, M, p) {
  switch (q.tur) {
    case 'yuk':
      return { u: birimVektor(p, q.i), v: M.LPhi.map((s) => s[q.f]), c: 1 };
    case 'faktorVaryans': {
      const a = M.L.map((s) => s[q.f]);
      return { u: a, v: a, c: 0.5 };
    }
    case 'faktorKovaryans':
      return { u: M.L.map((s) => s[q.f]), v: M.L.map((s) => s[q.g]), c: 1 };
    case 'hataVaryans': {
      const e = birimVektor(p, q.i);
      return { u: e, v: e, c: 0.5 };
    }
    default:
      return { u: birimVektor(p, q.i), v: birimVektor(p, q.j), c: 1 };
  }
}

/** tr(A D_k A D_l) = 2 c_k c_l [(v_k'A u_l)(v_l'A u_k) + (v_k'A v_l)(u_k'A u_l)] */
function bilgiElemani(dk, dl) {
  return 2 * dk.c * dl.c * (nokta(dk.Av, dl.u) * nokta(dl.v, dk.Au) + nokta(dk.Av, dl.v) * nokta(dk.Au, dl.u));
}

function turevleriHazirla(liste, M, A, p) {
  return liste.map((q) => {
    const d = turevGosterimi(q, M, p);
    d.Au = matVek(A, d.u);
    d.Av = d.u === d.v ? d.Au : matVek(A, d.v);
    return d;
  });
}

// ---------------------------------------------------------------------------------------------
// Kestirim
// ---------------------------------------------------------------------------------------------

function amac(S, logDetS, Sigma) {
  const r = logDetVeTers(Sigma);
  if (!r) return null;
  const p = S.length;
  let iz = 0;
  for (let a = 0; a < p; a++) for (let b = 0; b < p; b++) iz += S[a][b] * r.ters[b][a];
  return { F: r.logDet + iz - logDetS - p, A: r.ters, logDet: r.logDet, iz };
}

function baslangicDegerleri(prm, yapi, S, isaretci) {
  const { faktorler, faktorIndeksi } = yapi;
  const p = S.length;
  const sd = S.map((s, i) => Math.sqrt(s[i]));
  const yukStd = new Array(p).fill(0.7);
  // Her faktör için gösterge korelasyonlarının ilk temel bileşeni
  faktorler.forEach((_, f) => {
    const idx = faktorIndeksi.map((g, i) => (g === f ? i : -1)).filter((i) => i >= 0);
    const Rf = idx.map((a) => idx.map((b) => S[a][b] / (sd[a] * sd[b])));
    const { degerler, vektorler } = ozdegerAyristir(Rf);
    const isaret = vektorler[0][0] < 0 ? -1 : 1;
    idx.forEach((i, a) => {
      const l = isaret * vektorler[a][0] * Math.sqrt(Math.max(degerler[0], 1e-6));
      yukStd[i] = Math.max(-0.95, Math.min(0.95, l));
    });
  });
  const ilk = faktorler.map((_, f) => faktorIndeksi.indexOf(f));
  const ham = yukStd.map((l, i) => l * sd[i]);
  const faktorVar = faktorler.map((_, f) => (isaretci ? ham[ilk[f]] ** 2 : 1));
  const yuk = ham.map((v, i) => (isaretci ? v / ham[ilk[faktorIndeksi[i]]] : v));
  const korelasyon = (f, g) => {
    let t = 0;
    let say = 0;
    for (let i = 0; i < p; i++) {
      for (let j = 0; j < p; j++) {
        if (faktorIndeksi[i] === f && faktorIndeksi[j] === g) {
          t += S[i][j] / (sd[i] * sd[j] * yukStd[i] * yukStd[j]);
          say++;
        }
      }
    }
    return Math.max(-0.9, Math.min(0.9, t / say));
  };
  for (const q of prm) {
    if (q.tur === 'yuk') q.deger = yuk[q.i];
    else if (q.tur === 'faktorVaryans') q.deger = faktorVar[q.f];
    else if (q.tur === 'faktorKovaryans') q.deger = korelasyon(q.f, q.g) * Math.sqrt(faktorVar[q.f] * faktorVar[q.g]);
    else if (q.tur === 'hataVaryans') q.deger = Math.max(S[q.i][q.i] * (1 - yukStd[q.i] ** 2), 0.05 * S[q.i][q.i]);
    else q.deger = 0;
  }
  if (isaretci) for (const f of ilk) prm.find((q) => q.tur === 'yuk' && q.i === f).deger = 1;
}

/** Fisher puanlaması (beklenen bilgi) ve geri izlemeli satır araması ile F_ML'nin en küçüklenmesi */
function kestir(prm, S, k, maksIterasyon) {
  const p = S.length;
  const logDetS = logDetVeTers(S).logDet;
  const serbest = prm.filter((q) => q.serbest);
  const durum = () => {
    const M = matrisler(prm, p, k);
    const a = amac(S, logDetS, M.Sigma);
    return a ? { M, ...a } : null;
  };
  let d = durum();
  if (!d) throw new IstatistikHatasi('DFA başlangıç değerleri pozitif tanımlı bir kovaryans matrisi vermedi.');
  let it = 0;
  let yakinsadi = false;
  let gMaks = Infinity;
  for (; it < maksIterasyon; it++) {
    // ∂F/∂θ_k = tr(E ∂Σ/∂θ_k), E = Σ⁻¹ (Σ − S) Σ⁻¹
    const fark = d.M.Sigma.map((s, a) => s.map((v, b) => v - S[a][b]));
    const E = carp(carp(d.A, fark), d.A);
    const tur = turevleriHazirla(serbest, d.M, d.A, p);
    const g = tur.map((t) => 2 * t.c * nokta(t.u, matVek(E, t.v)));
    gMaks = Math.max(...g.map(Math.abs));
    if (gMaks < 1e-12) {
      yakinsadi = true;
      break;
    }
    const q = serbest.length;
    const H = sifir(q, q);
    for (let a = 0; a < q; a++) for (let b = a; b < q; b++) H[a][b] = H[b][a] = bilgiElemani(tur[a], tur[b]);
    const Hi = simetrikTers(H);
    const adim = matVek(Hi, g).map((v) => -v);
    const egim = nokta(g, adim);
    const eski = serbest.map((x) => x.deger);
    let t = 1;
    let yeni = null;
    for (let deneme = 0; deneme < 60; deneme++) {
      serbest.forEach((x, a) => (x.deger = eski[a] + t * adim[a]));
      const dd = durum();
      if (dd && dd.F <= d.F + 1e-4 * t * egim) {
        yeni = dd;
        break;
      }
      // yuvarlama bölgesi: F değişmiyorsa adımı kabul et (gradyan zaten çok küçük)
      if (dd && Math.abs(dd.F - d.F) <= 1e-14 * Math.max(1, Math.abs(d.F))) {
        yeni = dd;
        break;
      }
      t /= 2;
    }
    if (!yeni) {
      serbest.forEach((x, a) => (x.deger = eski[a]));
      yakinsadi = gMaks < 1e-7;
      break;
    }
    const degisim = Math.max(...adim.map((v, a) => Math.abs(t * v) / Math.max(1, Math.abs(eski[a]))));
    d = yeni;
    if (degisim < 1e-15) {
      yakinsadi = gMaks < 1e-7;
      break;
    }
  }
  return { d, iterasyon: it, yakinsadi, gradyanMaks: gMaks, logDetS };
}

// ---------------------------------------------------------------------------------------------
// Standartlaştırma (lavaan std.all)
// ---------------------------------------------------------------------------------------------

/**
 * std.all: yük λ·√φ_ff / √σ_ii, faktör kovaryansı φ_fg / √(φ_ff φ_gg), hata varyansı θ_ii / σ_ii,
 * hata kovaryansı θ_ij / √(θ_ii θ_jj) (lavaan: artık varyanslarla). σ_ii modelin öngördüğü varyanstır.
 */
function standartlastir(prm, p, k) {
  const M = matrisler(prm, p, k);
  const sig = M.Sigma.map((s, i) => s[i]);
  return prm.map((q) => {
    switch (q.tur) {
      case 'yuk':
        return (q.deger * Math.sqrt(M.Phi[q.f][q.f])) / Math.sqrt(sig[q.i]);
      case 'faktorVaryans':
        return 1;
      case 'faktorKovaryans':
        return q.deger / Math.sqrt(M.Phi[q.f][q.f] * M.Phi[q.g][q.g]);
      case 'hataVaryans':
        return q.deger / sig[q.i];
      default:
        return q.deger / Math.sqrt(M.Th[q.i][q.i] * M.Th[q.j][q.j]);
    }
  });
}

// ---------------------------------------------------------------------------------------------
// Ana fonksiyon
// ---------------------------------------------------------------------------------------------

/**
 * Doğrulayıcı faktör analizi (ML), lavaan::cfa() varsayılanlarıyla.
 *
 * @param {{veri?:number[][], kovaryans?:number[][], n?:number, model:Object<string,string[]>,
 *   degiskenAdlari:string[], iliskiliHatalar?:[string,string][], olcekleme?:'isaretci'|'faktor-varyansi',
 *   kestirim?:'ml', maksIterasyon?:number, modifikasyonSayisi?:number}} secenek
 *   veri: n × (değişken sayısı) ham veri (sütunlar degiskenAdlari sırasında); ya da kovaryans (n − 1 paydalı) ve n.
 *   olcekleme: 'isaretci' (lavaan varsayılanı std.lv = FALSE: her faktörün ilk göstergesinin yükü 1) ya da
 *   'faktor-varyansi' (std.lv = TRUE: faktör varyansları 1).
 * @returns {object} Alanlar: yukler, faktorVaryanslari, faktorKovaryanslari, faktorKorelasyonlari, hataVaryanslari,
 *   hataKovaryanslari (her satır {tahmin, SH, z, p, std, stdSH, stdZ, stdP}), R2, guvenirlik
 *   ({birlesikGuvenirlik, ortalamaAciklananVaryans}), uyum (χ², sd, p, CFI, TLI, NFI, RFI, IFI, RNI, PNFI,
 *   RMSEA ve %90 GA, RMSEA p (yakın uyum), SRMR, GFI, AGFI, PGFI, AIC, BIC, SABIC, ECVI, logL),
 *   modifikasyonIndeksleri (en büyük 10; mi, bdd = beklenen parametre değişimi, stdBdd), ortukKovaryans,
 *   ornekKovaryans, yakinsama, uyarilar.
 */
export function dogrulayiciFaktorAnalizi(secenek = {}) {
  const kestirim = secenek.kestirim ?? 'ml';
  gerekli(kestirim === 'ml', "Şimdilik yalnızca en çok olabilirlik ('ml') kestirimi destekleniyor.");
  const olcekleme = secenek.olcekleme ?? 'isaretci';
  gerekli(olcekleme === 'isaretci' || olcekleme === 'faktor-varyansi', "Ölçekleme 'isaretci' ya da 'faktor-varyansi' olmalıdır.");
  const isaretci = olcekleme === 'isaretci';
  const yapi = modeliKur(secenek);
  const { faktorler, gostergeler } = yapi;
  const p = gostergeler.length;
  const k = faktorler.length;
  const { S, n } = kovaryansHazirla(secenek, gostergeler);
  for (let i = 0; i < p; i++) gerekli(S[i][i] > 0, `"${gostergeler[i]}" değişkeninin varyansı sıfır.`);
  gerekli(cholesky(S) !== null, 'Örneklem kovaryans matrisi pozitif tanımlı değil (doğrusal bağımlı göstergeler ya da yetersiz gözlem).');
  const prm = parametreleriKur(yapi, isaretci);
  const serbest = prm.filter((q) => q.serbest);
  const npar = serbest.length;
  const momentSayisi = (p * (p + 1)) / 2;
  const sd = momentSayisi - npar;
  gerekli(sd >= 0, `Model tanımlanamıyor: serbest parametre sayısı (${npar}) gözlenen moment sayısından (${momentSayisi}) fazla.`);
  baslangicDegerleri(prm, yapi, S, isaretci);
  const uyarilar = [];
  const { d, iterasyon, yakinsadi, gradyanMaks, logDetS } = kestir(prm, S, k, secenek.maksIterasyon ?? 1000);
  if (!yakinsadi) uyarilar.push('Kestirim yakınsamadı; sonuçlar güvenilir değil. Modeli ve veriyi kontrol edin.');
  const M = d.M;

  // Standart hatalar: vcov = (2/N) H⁻¹ (beklenen bilgi)
  const tur = turevleriHazirla(serbest, M, d.A, p);
  const H = sifir(npar, npar);
  for (let a = 0; a < npar; a++) for (let b = a; b < npar; b++) H[a][b] = H[b][a] = bilgiElemani(tur[a], tur[b]);
  let Hi;
  try {
    Hi = simetrikTers(H);
  } catch {
    throw new IstatistikHatasi('Bilgi matrisi tekil: model tanımlanamıyor (identification).');
  }
  const V = Hi.map((s) => s.map((v) => (2 / n) * v));
  const se = V.map((s, a) => Math.sqrt(Math.max(s[a], 0)));
  serbest.forEach((q, a) => (q.SH = se[a]));

  // Standartlaştırılmış çözüm ve delta yöntemiyle standart hataları
  const std = standartlastir(prm, p, k);
  const J = std.map(() => new Array(npar).fill(0));
  serbest.forEach((q, a) => {
    const x0 = q.deger;
    const h = 1e-6 * Math.max(Math.abs(x0), 0.1);
    q.deger = x0 + h;
    const ust = standartlastir(prm, p, k);
    q.deger = x0 - h;
    const alt = standartlastir(prm, p, k);
    q.deger = x0;
    for (let r = 0; r < prm.length; r++) J[r][a] = (ust[r] - alt[r]) / (2 * h);
  });
  const stdSE = J.map((Jr) => {
    const VJ = matVek(V, Jr);
    return Math.sqrt(Math.max(nokta(Jr, VJ), 0));
  });
  const satir = (q, r) => {
    const tahmin = q.deger;
    const SH = q.serbest ? q.SH : null;
    const z = q.serbest ? tahmin / SH : null;
    const sSH = stdSE[r] > 1e-13 ? stdSE[r] : q.serbest ? stdSE[r] : null;
    const sZ = sSH ? std[r] / sSH : null;
    return {
      tahmin,
      SH,
      z,
      p: z === null ? null : pIkiYonlu(z),
      sabit: !q.serbest,
      std: std[r],
      stdSH: sSH,
      stdZ: sZ,
      stdP: sZ === null ? null : pIkiYonlu(sZ),
    };
  };
  const yukler = [];
  const faktorVaryanslari = [];
  const faktorKovaryanslari = [];
  const hataVaryanslari = [];
  const hataKovaryanslari = [];
  prm.forEach((q, r) => {
    const s = satir(q, r);
    if (q.tur === 'yuk') yukler.push({ faktor: faktorler[q.f], degisken: gostergeler[q.i], ...s });
    else if (q.tur === 'faktorVaryans') faktorVaryanslari.push({ faktor: faktorler[q.f], ...s });
    else if (q.tur === 'faktorKovaryans') faktorKovaryanslari.push({ faktor1: faktorler[q.f], faktor2: faktorler[q.g], ...s });
    else if (q.tur === 'hataVaryans') hataVaryanslari.push({ degisken: gostergeler[q.i], ...s });
    else hataKovaryanslari.push({ degisken1: gostergeler[q.i], degisken2: gostergeler[q.j], ...s });
  });
  const faktorKorelasyonlari = M.Phi.map((s, f) => s.map((v, g) => v / Math.sqrt(M.Phi[f][f] * M.Phi[g][g])));
  const sigma = M.Sigma.map((s, i) => s[i]);
  const R2 = gostergeler.map((ad, i) => ({ degisken: ad, deger: 1 - M.Th[i][i] / sigma[i] }));

  // Birleşik güvenirlik ve ortalama açıklanan varyans (standartlaştırılmış yüklerden; hata kovaryansları yok sayılır)
  const guvenirlik = faktorler.map((ad, f) => {
    const sat = yukler.filter((y) => y.faktor === ad);
    const toplam = sat.reduce((a, y) => a + y.std, 0);
    const kare = sat.reduce((a, y) => a + y.std ** 2, 0);
    const hata = sat.reduce((a, y) => a + hataVaryanslari.find((h) => h.degisken === y.degisken).std, 0);
    return { faktor: ad, birlesikGuvenirlik: toplam ** 2 / (toplam ** 2 + hata), ortalamaAciklananVaryans: kare / sat.length };
  });

  // Uyarılar: Heywood durumları
  hataVaryanslari.forEach((h) => {
    if (h.tahmin < 0) uyarilar.push(`"${h.degisken}" göstergesinin hata varyansı negatif (Heywood durumu).`);
  });
  faktorVaryanslari.forEach((v) => {
    if (v.tahmin <= 0) uyarilar.push(`"${v.faktor}" faktörünün varyansı negatif ya da sıfır.`);
  });
  faktorKovaryanslari.forEach((c) => {
    if (Math.abs(c.std) > 1) uyarilar.push(`"${c.faktor1}" ile "${c.faktor2}" arasındaki korelasyonun mutlak değeri 1'den büyük.`);
  });
  yukler.forEach((y) => {
    if (Math.abs(y.std) > 1) uyarilar.push(`"${y.degisken}" göstergesinin standartlaştırılmış yükü 1'den büyük (Heywood durumu).`);
  });

  // Uyum ölçütleri
  const chi = n * d.F;
  const pDeger = sd > 0 ? pchisq(chi, sd, false) : null;
  let temelLog = 0;
  for (let i = 0; i < p; i++) temelLog += Math.log(S[i][i]);
  const chiB = n * (temelLog - logDetS);
  const sdB = (p * (p - 1)) / 2;
  const cfi = 1 - Math.max(chi - sd, 0) / Math.max(chi - sd, chiB - sdB, 0);
  const tli = sd > 0 ? (chiB / sdB - chi / sd) / (chiB / sdB - 1) : 1;
  const nfi = (chiB - chi) / chiB;
  const rfi = sd > 0 ? (chiB / sdB - chi / sd) / (chiB / sdB) : null;
  const ifi = (chiB - chi) / (chiB - sd);
  const rni = 1 - (chi - sd) / (chiB - sdB);
  const pnfi = (sd / sdB) * nfi;
  const rmsea = sd > 0 ? Math.sqrt(Math.max((chi - sd) / (n * sd), 0)) : 0;
  let rmseaAlt = 0;
  let rmseaUst = 0;
  if (sd > 0) {
    const fAlt = (l) => pnchisq(chi, sd, l) - 0.95;
    const fUst = (l) => pnchisq(chi, sd, l) - 0.05;
    if (fAlt(0) >= 0) rmseaAlt = Math.sqrt(brent(fAlt, 0, chi, 1e-12) / (n * sd));
    const ustSinir = Math.max(n, 4 * chi);
    if (!(fUst(ustSinir) > 0) && fUst(0) >= 0) rmseaUst = Math.sqrt(brent(fUst, 0, ustSinir, 1e-12) / (n * sd));
  }
  const rmseaP = sd > 0 ? pnchisq(chi, sd, n * sd * 0.05 ** 2, false) : null;
  let srmrTop = 0;
  for (let i = 0; i < p; i++) for (let j = 0; j <= i; j++) srmrTop += ((S[i][j] - M.Sigma[i][j]) / Math.sqrt(S[i][i] * S[j][j])) ** 2;
  const srmr = Math.sqrt(srmrTop / momentSayisi);
  const AS = carp(d.A, S);
  let iz1 = 0;
  let iz2 = 0;
  for (let i = 0; i < p; i++) {
    for (let j = 0; j < p; j++) {
      const v = AS[i][j] * AS[j][i];
      iz2 += v;
      iz1 += v;
    }
    iz1 += 1 - 2 * AS[i][i];
  }
  const gfi = 1 - iz1 / iz2;
  const agfi = sd > 0 ? 1 - (momentSayisi / sd) * (1 - gfi) : null;
  const pgfi = (sd / momentSayisi) * gfi;
  const logL = (-n / 2) * (p * Math.log(2 * Math.PI) + d.logDet + d.iz);
  const logLDoymus = (-n / 2) * (p * Math.log(2 * Math.PI) + logDetS + p);
  const uyum = {
    kiKare: chi,
    sd,
    p: pDeger,
    kiKareSd: sd > 0 ? chi / sd : null,
    amacFonksiyonu: d.F,
    temelKiKare: chiB,
    temelSd: sdB,
    temelP: pchisq(chiB, sdB, false),
    CFI: cfi,
    TLI: tli,
    NNFI: tli,
    NFI: nfi,
    RFI: rfi,
    IFI: ifi,
    RNI: rni,
    PNFI: pnfi,
    RMSEA: rmsea,
    rmseaAlt,
    rmseaUst,
    rmseaP,
    SRMR: srmr,
    GFI: gfi,
    AGFI: agfi,
    PGFI: pgfi,
    logL,
    logLDoymus,
    parametreSayisi: npar,
    AIC: -2 * logL + 2 * npar,
    BIC: -2 * logL + npar * Math.log(n),
    SABIC: -2 * logL + npar * Math.log((n + 2) / 24),
    ECVI: chi / n + (2 * npar) / n,
  };
  if (sd === 0) uyarilar.push('Model tam tanımlı (serbestlik derecesi 0): uyum ölçütleri bilgi vermez.');

  // Modifikasyon indeksleri (skor testi, beklenen bilgi): MI = (N/2)·g_j² / (H_jj − H_jF H_FF⁻¹ H_Fj)
  const adaylar = [];
  for (let i = 0; i < p; i++) for (let f = 0; f < k; f++) if (yapi.faktorIndeksi[i] !== f) adaylar.push({ tur: 'yuk', i, f });
  const serbestKov = new Set(prm.filter((q) => q.tur === 'hataKovaryans').map((q) => `${q.i},${q.j}`));
  for (let i = 0; i < p; i++) for (let j = i + 1; j < p; j++) if (!serbestKov.has(`${i},${j}`)) adaylar.push({ tur: 'hataKovaryans', i, j });
  const Efark = carp(carp(d.A, M.Sigma.map((s, a) => s.map((v, b) => v - S[a][b]))), d.A);
  const adayTur = turevleriHazirla(adaylar, M, d.A, p);
  const mi = [];
  adaylar.forEach((q, a) => {
    const t = adayTur[a];
    const g = 2 * t.c * nokta(t.u, matVek(Efark, t.v));
    const hjF = tur.map((tf) => bilgiElemani(t, tf));
    const Vj = bilgiElemani(t, t) - nokta(hjF, matVek(Hi, hjF));
    if (!(Vj > 1e-12)) return;
    const deger = ((n / 2) * g * g) / Vj;
    const bdd = -g / Vj;
    let stdBdd;
    if (q.tur === 'yuk') stdBdd = (bdd * Math.sqrt(M.Phi[q.f][q.f])) / Math.sqrt(sigma[q.i]);
    else stdBdd = bdd / Math.sqrt(M.Th[q.i][q.i] * M.Th[q.j][q.j]);
    mi.push(
      q.tur === 'yuk'
        ? { tur: 'yuk', sol: faktorler[q.f], op: '=~', sag: gostergeler[q.i], mi: deger, bdd, stdBdd }
        : { tur: 'hataKovaryans', sol: gostergeler[q.i], op: '~~', sag: gostergeler[q.j], mi: deger, bdd, stdBdd },
    );
  });
  mi.sort((a, b) => b.mi - a.mi);

  return {
    yontem: 'dfa',
    ad: 'Doğrulayıcı faktör analizi',
    kestirim,
    olcekleme,
    n,
    degiskenler: gostergeler,
    faktorler,
    istatistik: chi,
    istatistikAdi: 'χ²',
    sd,
    p: pDeger,
    yukler,
    faktorVaryanslari,
    faktorKovaryanslari,
    faktorKorelasyonlari,
    hataVaryanslari,
    hataKovaryanslari,
    R2,
    guvenirlik,
    uyum,
    modifikasyonIndeksleri: mi.slice(0, secenek.modifikasyonSayisi ?? 10),
    ortukKovaryans: M.Sigma,
    ornekKovaryans: S,
    yakinsama: { iterasyon, yakinsadi, gradyanMaks },
    uyarilar,
  };
}
