// Yeni değişken hesaplamak için güvenli ifade çözümleyici (eval kullanılmaz).
// Örnekler:  kilo / (boy/100)^2      ln(CRP)      ortalama(S1, S2, S3)      eger(yas >= 65, 1, 0)
// Değişken adı boşluk ya da özel karakter içeriyorsa köşeli parantezle yazılır: [Sistolik KB]
// Eksik değer (null) aritmetikte yayılır; ortalama/toplam/min/maks eksikleri atlar (SPSS MEAN gibi).

import { IstatistikHatasi } from '../cekirdek/hata.js';

const FONKSIYONLAR = {
  ln: { n: 1, f: (x) => (x > 0 ? Math.log(x) : null) },
  log: { n: 1, f: (x) => (x > 0 ? Math.log(x) : null) },
  log10: { n: 1, f: (x) => (x > 0 ? Math.log10(x) : null) },
  log2: { n: 1, f: (x) => (x > 0 ? Math.log2(x) : null) },
  exp: { n: 1, f: (x) => Math.exp(x) },
  karekok: { n: 1, f: (x) => (x >= 0 ? Math.sqrt(x) : null) },
  sqrt: { n: 1, f: (x) => (x >= 0 ? Math.sqrt(x) : null) },
  mutlak: { n: 1, f: (x) => Math.abs(x) },
  abs: { n: 1, f: (x) => Math.abs(x) },
  yuvarla: { n: [1, 2], f: (x, k = 0) => Math.round(x * 10 ** k) / 10 ** k },
  round: { n: [1, 2], f: (x, k = 0) => Math.round(x * 10 ** k) / 10 ** k },
  tamsayi: { n: 1, f: (x) => Math.trunc(x) },
  kare: { n: 1, f: (x) => x * x },
};

// Eksik değerleri atlayan toplama fonksiyonları
const TOPLU = {
  ortalama: (d) => (d.length ? d.reduce((a, b) => a + b, 0) / d.length : null),
  mean: (d) => (d.length ? d.reduce((a, b) => a + b, 0) / d.length : null),
  toplam: (d) => (d.length ? d.reduce((a, b) => a + b, 0) : null),
  sum: (d) => (d.length ? d.reduce((a, b) => a + b, 0) : null),
  min: (d) => (d.length ? Math.min(...d) : null),
  maks: (d) => (d.length ? Math.max(...d) : null),
  max: (d) => (d.length ? Math.max(...d) : null),
  say: (d) => d.length,
};

function sozcukle(metin) {
  const t = [];
  let i = 0;
  while (i < metin.length) {
    const c = metin[i];
    if (/\s/.test(c)) {
      i++;
      continue;
    }
    if (/[0-9]/.test(c) || (c === '.' && /[0-9]/.test(metin[i + 1] || ''))) {
      let j = i;
      while (j < metin.length && /[0-9.]/.test(metin[j])) j++;
      if (/[eE]/.test(metin[j] || '') && /[-+0-9]/.test(metin[j + 1] || '')) {
        j++;
        if (/[-+]/.test(metin[j])) j++;
        while (j < metin.length && /[0-9]/.test(metin[j])) j++;
      }
      t.push({ tur: 'sayi', deger: Number(metin.slice(i, j)) });
      i = j;
      continue;
    }
    if (c === '[') {
      const j = metin.indexOf(']', i);
      if (j < 0) throw new IstatistikHatasi('Kapanmayan köşeli parantez.');
      t.push({ tur: 'ad', deger: metin.slice(i + 1, j) });
      i = j + 1;
      continue;
    }
    if (c === '"' || c === "'") {
      const j = metin.indexOf(c, i + 1);
      if (j < 0) throw new IstatistikHatasi('Kapanmayan tırnak.');
      t.push({ tur: 'metin', deger: metin.slice(i + 1, j) });
      i = j + 1;
      continue;
    }
    if (/[\p{L}_]/u.test(c)) {
      let j = i;
      while (j < metin.length && /[\p{L}\p{N}_.]/u.test(metin[j])) j++;
      t.push({ tur: 'ad', deger: metin.slice(i, j) });
      i = j;
      continue;
    }
    const iki = metin.slice(i, i + 2);
    if (['<=', '>=', '==', '!=', '<>', '&&', '||'].includes(iki)) {
      t.push({ tur: 'islec', deger: iki === '<>' ? '!=' : iki });
      i += 2;
      continue;
    }
    if ('+-*/^(),<>=!'.includes(c)) {
      t.push({ tur: 'islec', deger: c === '=' ? '==' : c });
      i++;
      continue;
    }
    throw new IstatistikHatasi(`Tanınmayan karakter: "${c}"`);
  }
  return t;
}

/**
 * İfadeyi derler.
 * @returns {{degiskenler:string[], hesapla:(degerAl:(ad:string)=>number|string|null)=>number|null}}
 */
export function ifadeDerle(metin, gecerliAdlar) {
  const t = sozcukle(metin);
  let k = 0;
  const kullanilan = new Set();
  const bak = () => t[k];
  const al = (beklenen) => {
    const s = t[k];
    if (!s || (beklenen && s.deger !== beklenen)) throw new IstatistikHatasi(beklenen ? `"${beklenen}" bekleniyordu.` : 'İfade eksik.');
    k++;
    return s;
  };

  // Öncelik: veya < ve < karşılaştırma < toplama < çarpma < tekli < üs < birincil
  function veya() {
    let sol = ve();
    while (bak()?.deger === '||' || (bak()?.tur === 'ad' && /^(veya|or)$/i.test(bak().deger))) {
      k++;
      const s = sol;
      const r = ve();
      sol = (g) => {
        const a = s(g);
        const b = r(g);
        if (a === null || b === null) return a === 1 || b === 1 ? 1 : null;
        return a || b ? 1 : 0;
      };
    }
    return sol;
  }
  function ve() {
    let sol = karsilastirma();
    while (bak()?.deger === '&&' || (bak()?.tur === 'ad' && /^(ve|and)$/i.test(bak().deger))) {
      k++;
      const s = sol;
      const r = karsilastirma();
      sol = (g) => {
        const a = s(g);
        const b = r(g);
        if (a === 0 || b === 0) return 0;
        if (a === null || b === null) return null;
        return 1;
      };
    }
    return sol;
  }
  function karsilastirma() {
    let sol = toplama();
    while (bak()?.tur === 'islec' && ['<', '>', '<=', '>=', '==', '!='].includes(bak().deger)) {
      const op = al().deger;
      const s = sol;
      const r = toplama();
      sol = (g) => {
        const a = s(g);
        const b = r(g);
        if (a === null || b === null) return null;
        switch (op) {
          case '<': return a < b ? 1 : 0;
          case '>': return a > b ? 1 : 0;
          case '<=': return a <= b ? 1 : 0;
          case '>=': return a >= b ? 1 : 0;
          case '==': return String(a) === String(b) ? 1 : 0;
          default: return String(a) !== String(b) ? 1 : 0;
        }
      };
    }
    return sol;
  }
  function toplama() {
    let sol = carpma();
    while (bak()?.tur === 'islec' && (bak().deger === '+' || bak().deger === '-')) {
      const op = al().deger;
      const s = sol;
      const r = carpma();
      sol = (g) => {
        const a = s(g);
        const b = r(g);
        if (a === null || b === null || typeof a !== 'number' || typeof b !== 'number') return null;
        return op === '+' ? a + b : a - b;
      };
    }
    return sol;
  }
  function carpma() {
    let sol = tekli();
    while (bak()?.tur === 'islec' && (bak().deger === '*' || bak().deger === '/')) {
      const op = al().deger;
      const s = sol;
      const r = tekli();
      sol = (g) => {
        const a = s(g);
        const b = r(g);
        if (a === null || b === null || typeof a !== 'number' || typeof b !== 'number') return null;
        if (op === '/') return b === 0 ? null : a / b;
        return a * b;
      };
    }
    return sol;
  }
  function tekli() {
    if (bak()?.deger === '-') {
      k++;
      const r = tekli();
      return (g) => {
        const a = r(g);
        return a === null || typeof a !== 'number' ? null : -a;
      };
    }
    if (bak()?.deger === '+') {
      k++;
      return tekli();
    }
    if (bak()?.deger === '!' || (bak()?.tur === 'ad' && /^(degil|not)$/i.test(bak().deger))) {
      k++;
      const r = tekli();
      return (g) => {
        const a = r(g);
        return a === null ? null : a ? 0 : 1;
      };
    }
    return us();
  }
  function us() {
    const taban = birincil();
    if (bak()?.deger === '^') {
      k++;
      const ust = tekli(); // sağdan birleşimli
      return (g) => {
        const a = taban(g);
        const b = ust(g);
        if (a === null || b === null) return null;
        const v = a ** b;
        return Number.isFinite(v) ? v : null;
      };
    }
    return taban;
  }
  function birincil() {
    const s = bak();
    if (!s) throw new IstatistikHatasi('İfade eksik.');
    if (s.tur === 'sayi') {
      k++;
      return () => s.deger;
    }
    if (s.tur === 'metin') {
      k++;
      return () => s.deger;
    }
    if (s.deger === '(') {
      k++;
      const ic = veya();
      al(')');
      return ic;
    }
    if (s.tur === 'ad') {
      k++;
      const ad = s.deger;
      const kucuk = ad.toLocaleLowerCase('tr');
      if (bak()?.deger === '(' && !gecerliAdlar.includes(ad)) {
        k++;
        const argumanlar = [];
        if (bak()?.deger !== ')') {
          argumanlar.push(veya());
          while (bak()?.deger === ',') {
            k++;
            argumanlar.push(veya());
          }
        }
        al(')');
        if (kucuk in TOPLU) {
          const f = TOPLU[kucuk];
          return (g) => f(argumanlar.map((a) => a(g)).filter((v) => typeof v === 'number' && Number.isFinite(v)));
        }
        if (kucuk === 'eger' || kucuk === 'if') {
          if (argumanlar.length !== 3) throw new IstatistikHatasi('eger(koşul, doğruysa, yanlışsa) üç bağımsız değişken alır.');
          return (g) => {
            const c = argumanlar[0](g);
            if (c === null) return null;
            return c ? argumanlar[1](g) : argumanlar[2](g);
          };
        }
        if (kucuk === 'eksik' || kucuk === 'missing') {
          return (g) => (argumanlar[0](g) === null ? 1 : 0);
        }
        const tanim = FONKSIYONLAR[kucuk];
        if (!tanim) throw new IstatistikHatasi(`Bilinmeyen fonksiyon: ${ad}`);
        const izinli = Array.isArray(tanim.n) ? tanim.n : [tanim.n];
        if (!izinli.includes(argumanlar.length)) throw new IstatistikHatasi(`${ad}() ${izinli.join(' ya da ')} bağımsız değişken alır.`);
        return (g) => {
          const v = argumanlar.map((a) => a(g));
          if (v.some((x) => x === null || typeof x !== 'number')) return null;
          const r = tanim.f(...v);
          return r === null || !Number.isFinite(r) ? null : r;
        };
      }
      if (!gecerliAdlar.includes(ad)) {
        const benzer = gecerliAdlar.find((x) => x.toLocaleLowerCase('tr') === kucuk);
        if (benzer) {
          kullanilan.add(benzer);
          return (g) => g(benzer);
        }
        throw new IstatistikHatasi(`"${ad}" adlı değişken yok. Adında boşluk varsa köşeli parantez kullanın: [${ad}]`);
      }
      kullanilan.add(ad);
      return (g) => g(ad);
    }
    throw new IstatistikHatasi(`Beklenmeyen simge: "${s.deger}"`);
  }

  const kok = veya();
  if (k < t.length) throw new IstatistikHatasi(`Fazladan simge: "${t[k].deger}"`);
  return { degiskenler: [...kullanilan], hesapla: kok };
}
