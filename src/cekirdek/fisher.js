// Fisher-Freeman-Halton kesin testi (r × c tablolar).
//
// Kesin p: tablonun koşullu (satır ve sütun toplamları sabit) olasılığı gözlenen tablonunkinden küçük ya da eşit
// olan tüm tabloların olasılık toplamı (R fisher.test ile aynı tanım ve aynı eşitlik toleransı; aşağıya bakın).
// Algoritma: ağ (network) algoritması (Mehta & Patel 1983 yaklaşımı). Sütunlar sırayla işlenir; düğüm = kalan
// satır toplamlarının sıralı vektörü (satırlar gelecekte simetrik olduğundan). Her düğümde "geçmiş" log olasılık
// değerleri (aynı değerler birleştirilerek) tutulur. Bir alt düğüme geçerken geleceğin en büyük ve en küçük
// log olasılık sınırlarıyla, tüm tamamlanışları sayılacak yollar toplu eklenir, hiçbiri sayılmayacak yollar atılır;
// yalnızca belirsiz yollar ilerletilir. Sınırlar küçük tablolarda kesin (dinamik programlama), büyüklerde
// Lagrange/gevşetme sınırlarıdır. Süre ya da bellek bütçesi aşılırsa Monte Carlo'ya (Patefield 1981 algoritması,
// tohumlu üreteç) geçilir ve pYontemi 'monte-carlo' olarak raporlanır.

import { logFaktoriyel } from './sayisal.js';
import { IstatistikHatasi } from './hata.js';

// Eşit olasılıklı tabloların sayılması için tolerans (log ölçeği). R: 2 × 2 tablolarda d <= d_gozlenen·(1 + 1e-7);
// r × c tablolarda FEXACT log olasılığa TOL = 3.45254e-7 ekler. Aynı değerler kullanılır (R ile birebir uyum için).
const LOG_TOLERANS_2x2 = Math.log1p(1e-7);
const LOG_TOLERANS_RXC = 3.45254e-7;
const GUVENLIK = 1e-9; // sınırlar için yuvarlama güvenlik payı (log ölçeği)
const BIRLESTIRME = 1e-9; // aynı sayılan geçmiş değerleri birleştirme toleransı (log ölçeği)

class ButceAsildi extends Error {}

// ---------- Rastgele sayı üreteci ----------

/**
 * Tohumlu rastgele sayı üreteci (xoshiro128**, splitmix32 ile başlatılır). [0, 1) aralığında 53 bit hassas sayı döndürür.
 * @param {number} tohum
 * @returns {() => number}
 */
export function rastgeleUretec(tohum = 20240601) {
  let s = tohum >>> 0;
  const sm = () => {
    s = (s + 0x9e3779b9) >>> 0;
    let z = s;
    z = Math.imul(z ^ (z >>> 16), 0x85ebca6b);
    z = Math.imul(z ^ (z >>> 13), 0xc2b2ae35);
    return (z ^ (z >>> 16)) >>> 0;
  };
  let a = sm();
  let b = sm();
  let c = sm();
  let d = sm();
  if ((a | b | c | d) === 0) a = 1;
  const rotl = (x, k) => (x << k) | (x >>> (32 - k));
  const sonraki = () => {
    const sonuc = Math.imul(rotl(Math.imul(b, 5), 7), 9) >>> 0;
    const t = b << 9;
    c ^= a;
    d ^= b;
    b ^= c;
    a ^= d;
    c ^= t;
    d = rotl(d, 11);
    return sonuc;
  };
  return () => ((sonraki() >>> 5) * 67108864 + (sonraki() >>> 6)) / 9007199254740992;
}

function logFaktoriyelTablosu(N) {
  const LF = new Float64Array(N + 1);
  for (let i = 0; i <= N; i++) LF[i] = logFaktoriyel(i);
  return LF;
}

// ---------- Patefield (1981) rastgele tablo üretimi ----------

/** Hipergeometrik örnek: N'lik toplulukta K başarı, n çekiliş → başarı sayısı (moddan iki yöne ters dönüşüm). */
function hipergeometrikOrnek(N, K, n, U, LF) {
  const lo = Math.max(0, n - (N - K));
  const hi = Math.min(n, K);
  if (lo === hi) return lo;
  let mod = Math.floor(((n + 1) * (K + 1)) / (N + 2));
  if (mod < lo) mod = lo;
  if (mod > hi) mod = hi;
  const pm = Math.exp(LF[K] - LF[mod] - LF[K - mod] + LF[N - K] - LF[n - mod] - LF[N - K - n + mod] - (LF[N] - LF[n] - LF[N - n]));
  let kum = pm;
  if (U <= kum) return mod;
  let L = mod;
  let R = mod;
  let pL = pm;
  let pR = pm;
  for (;;) {
    const solVar = L > lo;
    const sagVar = R < hi;
    if (!solVar && !sagVar) return mod;
    const yeniL = solVar ? (pL * (L * (N - K - n + L))) / ((K - L + 1) * (n - L + 1)) : -1;
    const yeniR = sagVar ? (pR * ((K - R) * (n - R))) / ((R + 1) * (N - K - n + R + 1)) : -1;
    if (yeniL >= yeniR) {
      L--;
      pL = yeniL;
      kum += pL;
      if (U <= kum) return L;
    } else {
      R++;
      pR = yeniR;
      kum += pR;
      if (U <= kum) return R;
    }
  }
}

/**
 * Verilen satır ve sütun toplamlarıyla koşullu dağılımdan rastgele tablo (Patefield 1981, AS 159; R r2dtable ile aynı dağılım).
 * @param {number[]} satir satır toplamları
 * @param {number[]} sutun sütun toplamları
 * @param {() => number} rastgele [0,1) üreteci
 * @param {Float64Array} [LF] log faktöriyel tablosu
 * @returns {number[][]}
 */
export function rastgeleTablo(satir, sutun, rastgele, LF) {
  const N = satir.reduce((a, b) => a + b, 0);
  const lf = LF ?? logFaktoriyelTablosu(N);
  const duz = new Int32Array(satir.length * sutun.length);
  patefieldDoldur(satir, sutun, N, rastgele, lf, duz, new Int32Array(sutun.length));
  return Array.from({ length: satir.length }, (_, i) => Array.from(duz.subarray(i * sutun.length, (i + 1) * sutun.length)));
}

function patefieldDoldur(satir, sutun, N, rastgele, LF, cikti, kolKalan) {
  const r = satir.length;
  const c = sutun.length;
  for (let j = 0; j < c; j++) kolKalan[j] = sutun[j];
  let toplamKalan = N;
  for (let i = 0; i < r - 1; i++) {
    let satirKalan = satir[i];
    let topluluk = toplamKalan;
    for (let j = 0; j < c - 1; j++) {
      const t = satirKalan === 0 ? 0 : hipergeometrikOrnek(topluluk, kolKalan[j], satirKalan, rastgele(), LF);
      cikti[i * c + j] = t;
      topluluk -= kolKalan[j];
      kolKalan[j] -= t;
      satirKalan -= t;
    }
    cikti[i * c + c - 1] = satirKalan;
    kolKalan[c - 1] -= satirKalan;
    toplamKalan -= satir[i];
  }
  for (let j = 0; j < c; j++) cikti[(r - 1) * c + j] = kolKalan[j];
}

/**
 * Monte Carlo p (R fisher.test(simulate.p.value = TRUE) ile aynı tanım):
 * p = (1 + #{simüle tablo: -Σ log t! <= gözlenen / (1 + 64ε)}) / (B + 1).
 */
function monteCarloP(satir, sutun, gozlenenLogPay, B, tohum, LF) {
  const N = satir.reduce((a, b) => a + b, 0);
  const r = satir.length;
  const c = sutun.length;
  const rastgele = rastgeleUretec(tohum);
  const tablo = new Int32Array(r * c);
  const kolKalan = new Int32Array(c);
  const STAT = -gozlenenLogPay;
  const sinir = STAT / (1 + 64 * 2.220446049250313e-16);
  let say = 0;
  for (let b = 0; b < B; b++) {
    patefieldDoldur(satir, sutun, N, rastgele, LF, tablo, kolKalan);
    let s = 0;
    for (let i = 0; i < tablo.length; i++) s -= LF[tablo[i]];
    if (s <= sinir) say++;
  }
  const p = (1 + say) / (B + 1);
  return { p, sh: Math.sqrt((p * (1 - p)) / B), B, tohum, sayac: say };
}

// ---------- Ağ algoritması ----------

function aktar(t) {
  return t[0].map((_, j) => t.map((s) => s[j]));
}

/**
 * Ağ algoritmasıyla kesin p.
 * satirT: satır toplamları (anahtar boyutu, r <= c), sutunT: sütun toplamları (aşamalar), gozlenenLogF: Σ log C_j! - Σ log t_ij!
 */
function agKesinP(satirT, sutunT, gozlenenLogF, LF, butce) {
  const r = satirT.length;
  const c = sutunT.length;
  const N = satirT.reduce((a, b) => a + b, 0);
  const esik = gozlenenLogF + butce.logTolerans;
  let K = -LF[N];
  for (const R of satirT) K += LF[R];
  const C = sutunT;
  const taban = N + 1;
  const sayisalAnahtar = taban ** r < 2 ** 52;
  const anahtar = sayisalAnahtar
    ? (m) => {
        let k = 0;
        for (let i = r - 1; i >= 0; i--) k = k * taban + m[i];
        return k;
      }
    : (m) => m.join(',');

  // Kalan sütunlar için sonek toplamları
  const sonekLFC = new Float64Array(c + 1);
  const sonekClogC = new Float64Array(c + 1);
  for (let j = c - 1; j >= 0; j--) {
    sonekLFC[j] = sonekLFC[j + 1] + LF[C[j]];
    sonekClogC[j] = sonekClogC[j + 1] + (C[j] > 0 ? C[j] * Math.log(C[j]) : 0);
  }
  const kalanSutunlarAzalan = [];
  for (let k = 0; k <= c; k++) kalanSutunlarAzalan.push(C.slice(k).sort((a, b) => b - a));

  let islem = 0;
  const baslangic = Date.now();
  const kontrol = () => {
    if (++islem % 4096 === 0) {
      if (Date.now() - baslangic > butce.sure) throw new ButceAsildi('süre');
    }
    if (islem > butce.islem) throw new ButceAsildi('işlem');
  };

  // Kanonik sütun dağılımları: eşit kalan satır toplamlı satırlarda t artmayan; çokluk = permütasyon sayısı
  const faktoriyel = [1];
  for (let i = 1; i <= r; i++) faktoriyel.push(faktoriyel[i - 1] * i);
  // Not: dagilimlar iç içe çağrılabilir (sınır hesabı ileri geçişin ortasında yapılır); bu yüzden diziler yereldir.
  function dagilimlar(m, toplam, geriCagir) {
    const t = new Int32Array(r);
    const kalanKapasite = new Int32Array(r + 1);
    for (let i = r - 1; i >= 0; i--) kalanKapasite[i] = kalanKapasite[i + 1] + m[i];
    const ozyineli = (i, kalan) => {
      if (i === r - 1) {
        if (kalan > m[i]) return;
        if (i > 0 && m[i] === m[i - 1] && kalan > t[i - 1]) return;
        t[i] = kalan;
        // çokluk
        let cokluk = 1;
        let g = 0;
        while (g < r) {
          let h = g;
          while (h + 1 < r && m[h + 1] === m[g]) h++;
          if (h > g) {
            let pay = faktoriyel[h - g + 1];
            let a = g;
            while (a <= h) {
              let b = a;
              while (b + 1 <= h && t[b + 1] === t[a]) b++;
              pay /= faktoriyel[b - a + 1];
              a = b + 1;
            }
            cokluk *= pay;
          }
          g = h + 1;
        }
        geriCagir(t, cokluk);
        return;
      }
      let ust = Math.min(m[i], kalan);
      if (i > 0 && m[i] === m[i - 1]) ust = Math.min(ust, t[i - 1]);
      const enAz = Math.max(0, kalan - kalanKapasite[i + 1]);
      for (let v = ust; v >= enAz; v--) {
        t[i] = v;
        ozyineli(i + 1, kalan - v);
      }
    };
    ozyineli(0, toplam);
  }
  /** Çocuk düğüm m - t, azalan sırada cikti dizisine yazılır */
  function cocukHesapla(m, t, cikti) {
    for (let i = 0; i < r; i++) cikti[i] = m[i] - t[i];
    for (let i = 1; i < r; i++) {
      const v = cikti[i];
      let j = i - 1;
      while (j >= 0 && cikti[j] < v) {
        cikti[j + 1] = cikti[j];
        j--;
      }
      cikti[j + 1] = v;
    }
    return cikti;
  }

  // ----- Sınırlar -----
  // gelecek logF = Σ_{j>=k} log C_j! - Σ log t_ij!  (k. aşamadan sonraki tamamlanışlar)
  function sonAsamaDegeri(k, m) {
    let v = LF[C[k]];
    for (let i = 0; i < r; i++) v -= LF[m[i]];
    return v;
  }

  // Kesin sınırlar (küçük ağlarda): bellekli özyineleme
  let kesinSinirKullan = butce.kesinSinir !== false;
  const kesinBellek = Array.from({ length: c }, () => new Map());
  let kesinSayac = 0;
  function kesinSinir(k, m) {
    const ky = anahtar(m);
    const b = kesinBellek[k].get(ky);
    if (b) return b;
    let sonuc;
    if (k === c - 1) {
      const v = sonAsamaDegeri(k, m);
      sonuc = [v, v];
    } else {
      let mx = -Infinity;
      let mn = Infinity;
      const mKopya = Int32Array.from(m);
      const sonrakiler = [];
      dagilimlar(mKopya, C[k], (tt) => {
        if (++kesinSayac > butce.kesinSinirIslem) throw new ButceAsildi('kesin sınır');
        let artis = LF[C[k]];
        for (let i = 0; i < r; i++) artis -= LF[tt[i]];
        sonrakiler.push([artis, cocukHesapla(mKopya, tt, new Int32Array(r))]);
      });
      for (const [artis, ch] of sonrakiler) {
        const s = kesinSinir(k + 1, ch);
        if (artis + s[0] > mx) mx = artis + s[0];
        if (artis + s[1] < mn) mn = artis + s[1];
      }
      sonuc = [mx, mn];
    }
    kesinBellek[k].set(ky, sonuc);
    return sonuc;
  }

  // Gevşetme sınırları
  function gevsekSinir(k, m) {
    let M = 0;
    for (let i = 0; i < r; i++) M += m[i];
    // En olası tamamlanış için üst sınır: Lagrange alt sınırı (u_i + v_j = log(m_i C_j / M))
    let L = sonekClogC[k] - (M > 0 ? M * Math.log(M) : 0);
    for (let i = 0; i < r; i++) {
      const mi = m[i];
      if (mi === 0) continue;
      L += mi * Math.log(mi);
      for (let j = k; j < c; j++) {
        const Cj = C[j];
        const kap = Math.min(mi, Cj);
        const e = (mi * Cj) / M;
        const a = Math.log(e);
        const t0 = Math.min(kap, Math.floor(e));
        let enk = LF[t0] - a * t0;
        if (t0 > 0) enk = Math.min(enk, LF[t0 - 1] - a * (t0 - 1));
        if (t0 < kap) enk = Math.min(enk, LF[t0 + 1] - a * (t0 + 1));
        L += enk;
      }
    }
    const fMaks = sonekLFC[k] - L;
    // En az olası tamamlanış için alt sınır: Σ log t! üst sınırı (satır ya da sütun kısıtı gevşetilerek, açgözlü doldurma)
    let sutunG = 0;
    for (let j = k; j < c; j++) {
      let kalan = C[j];
      for (let i = 0; i < r && kalan > 0; i++) {
        const v = Math.min(m[i], kalan);
        sutunG += LF[v];
        kalan -= v;
      }
    }
    let satirG = 0;
    const kaps = kalanSutunlarAzalan[k];
    for (let i = 0; i < r; i++) {
      let kalan = m[i];
      for (let j = 0; j < kaps.length && kalan > 0; j++) {
        const v = Math.min(kaps[j], kalan);
        satirG += LF[v];
        kalan -= v;
      }
    }
    const fMin = sonekLFC[k] - Math.min(sutunG, satirG);
    return [fMaks, fMin];
  }

  function sinirlar(k, m) {
    let M = 0;
    let tot = 0;
    for (let i = 0; i < r; i++) {
      M += m[i];
      tot -= LF[m[i]];
    }
    tot += LF[M];
    if (k === c - 1) {
      const v = sonAsamaDegeri(k, m);
      return { fMaks: v, fMin: v, tot };
    }
    let s;
    if (kesinSinirKullan) {
      try {
        s = kesinSinir(k, m);
      } catch (e) {
        if (!(e instanceof ButceAsildi) || e.message !== 'kesin sınır') throw e;
        kesinSinirKullan = false;
        for (const b of kesinBellek) b.clear();
      }
    }
    if (!kesinSinirKullan) s = gevsekSinir(k, m);
    return { fMaks: s[0] + GUVENLIK, fMin: s[1] - GUVENLIK, tot };
  }

  // ----- İleri geçiş -----
  // Her düğüm, ebeveynlerinin sıralı geçmiş listelerinden kaydırılmış "koşular" (başvuru, kopya değil) tutar;
  // düğüm işlenirken koşular k-yollu birleştirmeyle tek sıralı listeye dönüştürülür (eşit değerler toplanır).
  let p = 0;
  let dugumler = [
    {
      m: Int32Array.from(satirT),
      kosular: [{ v: Float64Array.of(0), w: Float64Array.of(1), bas: 0, son: 1, artis: 0, cokluk: 1 }],
    },
  ];
  let dugumSayisi = 0;
  let depolanan = 0;
  for (let k = 0; k < c - 1; k++) {
    const sonraki = [];
    // Çocuk anahtarı → sınırlar ve (varsa) sonraki aşamadaki düğüm numarası
    const tablo = sayisalAnahtar ? new SayisalTablo() : new Map();
    const bMaks = [];
    const bMin = [];
    const bTot = [];
    const bDugum = [];
    const lfC = LF[C[k]];
    for (const dugum of dugumler) {
      dugumSayisi++;
      const { m } = dugum;
      const { v, w } = kosulariBirlestir(dugum.kosular);
      dugum.kosular = null;
      const L = v.length;
      const vmaks = v[L - 1];
      const onek = new Float64Array(L);
      let s = 0;
      for (let i = 0; i < L; i++) {
        s += w[i] * Math.exp(v[i] - vmaks);
        onek[i] = s;
      }
      // v'de x'ten küçük ya da eşit eleman sayısı
      const sayKucukEsit = (x) => {
        let lo = 0;
        let hi = L;
        while (lo < hi) {
          const o = (lo + hi) >> 1;
          if (v[o] <= x) lo = o + 1;
          else hi = o;
        }
        return lo;
      };
      if (k === c - 2) {
        // Son sütun zorunlu: tamamlanış tektir, sınır gerekmez (çocuğu sıralamaya da gerek yok)
        const lfSon = LF[C[c - 1]];
        dagilimlar(m, C[k], (tt, cokluk) => {
          kontrol();
          let f = lfC + lfSon;
          for (let i = 0; i < r; i++) f -= LF[tt[i]] + LF[m[i] - tt[i]];
          const i1 = sayKucukEsit(esik - f);
          if (i1 > 0) p += cokluk * Math.exp(K + f + vmaks) * onek[i1 - 1];
        });
        continue;
      }
      const ch = new Int32Array(r);
      dagilimlar(m, C[k], (tt, cokluk) => {
        kontrol();
        let artis = lfC;
        for (let i = 0; i < r; i++) artis -= LF[tt[i]];
        cocukHesapla(m, tt, ch);
        const ky = anahtar(ch);
        let idx = sayisalAnahtar ? tablo.bul(ky) : tablo.get(ky) ?? -1;
        if (idx < 0) {
          const b = sinirlar(k + 1, ch);
          idx = bMaks.length;
          bMaks.push(b.fMaks);
          bMin.push(b.fMin);
          bTot.push(b.tot);
          bDugum.push(-1);
          if (sayisalAnahtar) tablo.ekle(ky, idx);
          else tablo.set(ky, idx);
        }
        const altKesim = esik - artis - bMaks[idx];
        if (altKesim >= vmaks) {
          // tüm geçmiş değerler için tüm tamamlanışlar sayılır
          p += cokluk * Math.exp(K + artis + bTot[idx] + vmaks) * onek[L - 1];
          return;
        }
        const ustKesim = esik - artis - bMin[idx];
        if (ustKesim < v[0]) return; // hiçbiri sayılmaz
        const i1 = altKesim < v[0] ? 0 : sayKucukEsit(altKesim);
        if (i1 > 0) p += cokluk * Math.exp(K + artis + bTot[idx] + vmaks) * onek[i1 - 1];
        const i2 = ustKesim >= vmaks ? L : sayKucukEsit(ustKesim);
        if (i2 > i1) {
          let d = bDugum[idx];
          if (d < 0) {
            d = sonraki.length;
            sonraki.push({ m: Int32Array.from(ch), kosular: [] });
            bDugum[idx] = d;
          }
          sonraki[d].kosular.push({ v, w, bas: i1, son: i2, artis, cokluk });
          depolanan += i2 - i1;
          if (depolanan > butce.depo) throw new ButceAsildi('bellek');
        }
      });
    }
    dugumler = sonraki;
    depolanan = 0;
    if (dugumler.length === 0) break;
  }
  return { p: Math.min(1, p), dugumSayisi, sure: Date.now() - baslangic, kesinSinir: kesinSinirKullan };
}

/** Tamsayı değerli (< 2^52) sayısal anahtarlar için açık adresli karma tablo (Map'ten hızlı, kutulama yok). */
class SayisalTablo {
  constructor() {
    this.kap = 1 << 10;
    this.anahtar = new Float64Array(this.kap);
    this.deger = new Int32Array(this.kap).fill(-1);
    this.say = 0;
  }

  static karma(k) {
    const lo = k % 4294967296 | 0;
    const hi = (k / 4294967296) | 0;
    let h = Math.imul(lo ^ Math.imul(hi, 0x27d4eb2d), 0x9e3779b1);
    h ^= h >>> 15;
    h = Math.imul(h, 0x85ebca6b);
    return h ^ (h >>> 13);
  }

  bul(k) {
    const maske = this.kap - 1;
    let i = SayisalTablo.karma(k) & maske;
    for (;;) {
      const d = this.deger[i];
      if (d === -1) return -1;
      if (this.anahtar[i] === k) return d;
      i = (i + 1) & maske;
    }
  }

  ekle(k, v) {
    if (2 * (this.say + 1) > this.kap) this.buyut();
    const maske = this.kap - 1;
    let i = SayisalTablo.karma(k) & maske;
    while (this.deger[i] !== -1) i = (i + 1) & maske;
    this.anahtar[i] = k;
    this.deger[i] = v;
    this.say++;
  }

  buyut() {
    const eskiA = this.anahtar;
    const eskiD = this.deger;
    this.kap *= 2;
    this.anahtar = new Float64Array(this.kap);
    this.deger = new Int32Array(this.kap).fill(-1);
    this.say = 0;
    for (let i = 0; i < eskiD.length; i++) if (eskiD[i] !== -1) this.ekle(eskiA[i], eskiD[i]);
  }
}

/** Kaydırılmış sıralı koşuları tek sıralı listeye birleştirir; BIRLESTIRME toleransındaki değerlerin ağırlıkları toplanır. */
function kosulariBirlestir(kosular) {
  let toplam = 0;
  for (const k of kosular) toplam += k.son - k.bas;
  const v = new Float64Array(toplam);
  const w = new Float64Array(toplam);
  let L = 0;
  const K = kosular.length;
  if (K === 1) {
    const { v: kv, w: kw, bas, son, artis, cokluk } = kosular[0];
    for (let i = bas; i < son; i++) {
      const val = kv[i] + artis;
      if (L > 0 && val - v[L - 1] <= BIRLESTIRME) w[L - 1] += kw[i] * cokluk;
      else {
        v[L] = val;
        w[L] = kw[i] * cokluk;
        L++;
      }
    }
    return { v: v.subarray(0, L), w: w.subarray(0, L) };
  }
  // İkili yığın: her koşunun sıradaki değeri
  const konum = new Int32Array(K);
  const anahtar = new Float64Array(K);
  const yigin = new Int32Array(K);
  let ys = 0;
  const yukari = (i) => {
    const x = yigin[i];
    while (i > 0) {
      const ata = (i - 1) >> 1;
      if (anahtar[yigin[ata]] <= anahtar[x]) break;
      yigin[i] = yigin[ata];
      i = ata;
    }
    yigin[i] = x;
  };
  const asagi = (i) => {
    const x = yigin[i];
    for (;;) {
      let e = 2 * i + 1;
      if (e >= ys) break;
      if (e + 1 < ys && anahtar[yigin[e + 1]] < anahtar[yigin[e]]) e++;
      if (anahtar[yigin[e]] >= anahtar[x]) break;
      yigin[i] = yigin[e];
      i = e;
    }
    yigin[i] = x;
  };
  for (let q = 0; q < K; q++) {
    const kq = kosular[q];
    konum[q] = kq.bas;
    anahtar[q] = kq.v[kq.bas] + kq.artis;
    yigin[ys] = q;
    yukari(ys++);
  }
  while (ys > 0) {
    const q = yigin[0];
    const kq = kosular[q];
    const val = anahtar[q];
    const wt = kq.w[konum[q]] * kq.cokluk;
    if (L > 0 && val - v[L - 1] <= BIRLESTIRME) w[L - 1] += wt;
    else {
      v[L] = val;
      w[L] = wt;
      L++;
    }
    konum[q]++;
    if (konum[q] < kq.son) {
      anahtar[q] = kq.v[konum[q]] + kq.artis;
      asagi(0);
    } else {
      yigin[0] = yigin[--ys];
      if (ys > 0) asagi(0);
    }
  }
  return { v: v.subarray(0, L), w: w.subarray(0, L) };
}

/**
 * Fisher-Freeman-Halton kesin testi (r × c). 2 × 2 tablolar için de çalışır (kategorik.js'teki fisherKesin 2 × 2'yi
 * R'deki yöntemle ayrıca hesaplar).
 *
 * @param {number[][]} tablo negatif olmayan tamsayı frekanslar
 * @param {{yontem?:'otomatik'|'kesin'|'monte-carlo', zamanSiniri?:number, islemSiniri?:number, depoSiniri?:number,
 *   B?:number, tohum?:number, tolerans?:number, kesinSinir?:boolean}} secenek
 *   yontem 'otomatik': kesin hesap bütçeyi aşarsa (zamanSiniri varsayılan 5000 ms, islemSiniri 2e8 sütun dağılımı,
 *   depoSiniri bir aşamada 1.2e7 geçmiş değer) Monte Carlo; 'kesin': bütçe (600 sn) aşılırsa IstatistikHatasi;
 *   'monte-carlo': doğrudan Monte Carlo (B = 100000, tohum = 20240601 varsayılan; aynı tohum aynı sonucu verir).
 *   tolerans: eşit olasılık için göreli tolerans (varsayılan R ile aynı: 2 × 2'de 1e-7, r × c'de FEXACT'in 3.45254e-7'si).
 *   kesinSinir: gelecek sınırları dinamik programlamayla kesin hesapla (varsayılan yalnızca 2 satırlı tablolarda).
 * @returns {{p:number, pYontemi:'kesin'|'monte-carlo', sh?:number, B?:number, tohum?:number, sure:number,
 *   dugumSayisi?:number, uyarilar:string[]}} sh: Monte Carlo p'nin standart hatası √(p(1-p)/B)
 */
export function fisherRxC(tablo, secenek = {}) {
  const { yontem = 'otomatik', B = 100000, tohum = 20240601 } = secenek;
  if (!['otomatik', 'kesin', 'monte-carlo'].includes(yontem)) throw new IstatistikHatasi(`Bilinmeyen yöntem: ${yontem}`);
  if (!Array.isArray(tablo) || tablo.length === 0 || !Array.isArray(tablo[0])) throw new IstatistikHatasi('Tablo sayı dizilerinden oluşmalıdır.');
  const c0 = tablo[0].length;
  for (const s of tablo) {
    if (!Array.isArray(s) || s.length !== c0) throw new IstatistikHatasi('Tablonun tüm satırları aynı uzunlukta olmalıdır.');
    for (const v of s) {
      if (!Number.isInteger(v) || v < 0) throw new IstatistikHatasi('Fisher kesin testi için frekanslar negatif olmayan tamsayılar olmalıdır.');
    }
  }
  const uyarilar = [];
  // Boş satır ve sütunlar p değerini etkilemez
  let t = tablo.filter((s) => s.some((v) => v > 0));
  const doluSutun = c0 ? t[0]?.map((_, j) => t.some((s) => s[j] > 0)) ?? [] : [];
  t = t.map((s) => s.filter((_, j) => doluSutun[j]));
  const baslangic = Date.now();
  if (t.length < 2 || (t[0]?.length ?? 0) < 2) {
    return { p: 1, pYontemi: 'kesin', sure: 0, uyarilar: ['Gözlemi olan satır ya da sütun sayısı 2\'den az; p = 1.'] };
  }
  if (t.length > t[0].length) t = aktar(t);
  const satirT = t.map((s) => s.reduce((a, b) => a + b, 0));
  const sutunT = t[0].map((_, j) => t.reduce((a, s) => a + s[j], 0));
  const N = satirT.reduce((a, b) => a + b, 0);
  const LF = logFaktoriyelTablosu(N);
  let gozlenenLogF = 0;
  for (const C of sutunT) gozlenenLogF += LF[C];
  let gozlenenLogPay = 0; // Σ log t_ij!
  for (const s of t) for (const v of s) gozlenenLogPay += LF[v];
  gozlenenLogF -= gozlenenLogPay;

  if (yontem !== 'monte-carlo') {
    // Satırları azalan, sütunları (aşamaları) azalan sırala
    const sira = satirT.map((_, i) => i).sort((a, b) => satirT[b] - satirT[a]);
    const R = sira.map((i) => satirT[i]);
    const Cs = sutunT.slice().sort((a, b) => b - a);
    const butce = {
      sure: secenek.zamanSiniri ?? (yontem === 'kesin' ? 600000 : 5000),
      islem: secenek.islemSiniri ?? (yontem === 'kesin' ? 2e10 : 2e8),
      depo: secenek.depoSiniri ?? 1.2e7,
      kesinSinirIslem: secenek.kesinSinirIslem ?? 3e6,
      kesinSinir: secenek.kesinSinir ?? R.length === 2,
      logTolerans: secenek.tolerans !== undefined ? Math.log1p(secenek.tolerans) : R.length === 2 && Cs.length === 2 ? LOG_TOLERANS_2x2 : LOG_TOLERANS_RXC,
    };
    try {
      const s = agKesinP(R, Cs, gozlenenLogF, LF, butce);
      return { p: s.p, pYontemi: 'kesin', sure: Date.now() - baslangic, dugumSayisi: s.dugumSayisi, uyarilar };
    } catch (e) {
      if (!(e instanceof ButceAsildi)) throw e;
      if (yontem === 'kesin') {
        throw new IstatistikHatasi('Kesin Fisher testi hesap bütçesini aştı; Monte Carlo yöntemini kullanın.');
      }
      uyarilar.push(`Kesin hesap bu tablo için çok uzun sürdüğünden p değeri Monte Carlo benzetimiyle (B = ${B}) hesaplandı.`);
    }
  }
  const mc = monteCarloP(satirT, sutunT, gozlenenLogPay, B, tohum, LF);
  return { p: mc.p, pYontemi: 'monte-carlo', sh: mc.sh, B, tohum, sure: Date.now() - baslangic, uyarilar };
}
