// Çoklu karşılaştırma düzeltmeleri. R'nin p.adjust() fonksiyonuyla aynı sonuçları verir.

export const DUZELTME_ADLARI = {
  yok: 'Düzeltme yok',
  bonferroni: 'Bonferroni',
  holm: 'Holm',
  hochberg: 'Hochberg',
  hommel: 'Hommel',
  BH: 'Benjamini-Hochberg (FDR)',
  BY: 'Benjamini-Yekutieli',
  sidak: 'Šidák',
};

function sirala(p, azalan = false) {
  const o = p.map((_, i) => i).sort((a, b) => (azalan ? p[b] - p[a] : p[a] - p[b]) || a - b);
  return o;
}

/**
 * @param {number[]} p ham p değerleri (null/NaN olanlar olduğu gibi kalır)
 * @param {'yok'|'bonferroni'|'holm'|'hochberg'|'hommel'|'BH'|'BY'|'sidak'} yontem
 * @param {number} [m] toplam karşılaştırma sayısı (varsayılan: geçerli p sayısı)
 */
export function pDuzelt(p, yontem = 'holm', m) {
  const gecerliIdx = [];
  p.forEach((v, i) => {
    if (typeof v === 'number' && !Number.isNaN(v)) gecerliIdx.push(i);
  });
  const q = gecerliIdx.map((i) => p[i]);
  const n = m ?? q.length;
  let sonuc;
  if (n <= 1) sonuc = q.slice();
  else {
    switch (yontem) {
      case 'yok':
        sonuc = q.slice();
        break;
      case 'bonferroni':
        sonuc = q.map((v) => Math.min(1, v * n));
        break;
      case 'sidak':
        // 1 - (1 - p)^n, küçük p'de hassasiyet kaybı olmadan
        sonuc = q.map((v) => Math.min(1, -Math.expm1(n * Math.log1p(-v))));
        break;
      case 'holm': {
        const o = sirala(q);
        sonuc = new Array(q.length);
        let maks = 0;
        o.forEach((idx, i) => {
          maks = Math.max(maks, (n - i) * q[idx]);
          sonuc[idx] = Math.min(1, maks);
        });
        break;
      }
      case 'hochberg': {
        const o = sirala(q, true);
        sonuc = new Array(q.length);
        let min = Infinity;
        const L = q.length;
        o.forEach((idx, k) => {
          const i = L - k; // n:1
          min = Math.min(min, (n - i + 1) * q[idx]);
          sonuc[idx] = Math.min(1, min);
        });
        break;
      }
      case 'BH':
      case 'BY': {
        const o = sirala(q, true);
        sonuc = new Array(q.length);
        let min = Infinity;
        let c = 1;
        if (yontem === 'BY') {
          c = 0;
          for (let i = 1; i <= n; i++) c += 1 / i;
        }
        const L = q.length;
        o.forEach((idx, k) => {
          const i = L - k;
          min = Math.min(min, ((c * n) / i) * q[idx]);
          sonuc[idx] = Math.min(1, min);
        });
        break;
      }
      case 'hommel': {
        // R'deki algoritmanın birebir uyarlaması
        const L = q.length;
        const pp = q.slice();
        while (pp.length < n) pp.push(1);
        const N = pp.length;
        const o = sirala(pp);
        const ps = o.map((i) => pp[i]);
        let minIlk = Infinity;
        for (let i = 0; i < N; i++) minIlk = Math.min(minIlk, (N * ps[i]) / (i + 1));
        const qv = new Array(N).fill(minIlk);
        const pa = new Array(N).fill(minIlk);
        for (let mm = N - 1; mm >= 2; mm--) {
          const i1Son = N - mm + 1; // i1 = 1..N-m+1
          let q1 = Infinity;
          for (let k = 2; k <= mm; k++) {
            const idx = N - mm + k; // i2 = (N-m+2):N, 1 tabanlı
            q1 = Math.min(q1, (mm * ps[idx - 1]) / k);
          }
          for (let i = 1; i <= i1Son; i++) qv[i - 1] = Math.min(mm * ps[i - 1], q1);
          for (let i = N - mm + 2; i <= N; i++) qv[i - 1] = qv[N - mm];
          for (let i = 0; i < N; i++) pa[i] = Math.max(pa[i], qv[i]);
        }
        const sirali = pa.map((v, i) => Math.max(v, ps[i]));
        const geri = new Array(N);
        o.forEach((idx, i) => {
          geri[idx] = sirali[i];
        });
        sonuc = geri.slice(0, L);
        break;
      }
      default:
        throw new Error(`Bilinmeyen düzeltme yöntemi: ${yontem}`);
    }
  }
  const cikti = p.slice();
  gecerliIdx.forEach((i, k) => {
    cikti[i] = sonuc[k];
  });
  return cikti;
}
