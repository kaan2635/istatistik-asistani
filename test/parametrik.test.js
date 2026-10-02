import { test } from 'node:test';
import assert from 'node:assert/strict';
import { bagimsizT, brownForsytheAnova, eslestirilmisT, tekOrneklemT, tekYonluAnova, welchAnova } from '../src/cekirdek/parametrik.js';
import { bartlett, fligner, levene, varyansOraniF } from '../src/cekirdek/varyans.js';
import { bonferroniPostHoc, dunnett, dunnettKritik, dunnettOlasilik, gamesHowell, lsd, phiHizli, scheffe, tamhaneT2, tukeyHSD } from '../src/cekirdek/posthoc.js';
import * as E from '../src/cekirdek/etki.js';
import { pf, pnf, pnorm, pnt, pt, qt } from '../src/cekirdek/dagilim.js';
import { integral } from '../src/cekirdek/sayisal.js';
import { IstatistikHatasi } from '../src/cekirdek/hata.js';
import { referans, yakin } from './yardimci.js';

const R = referans('parametrik');
const ALT = { 'two.sided': 'iki', greater: 'buyuk', less: 'kucuk' };

/** R'nin GA uçları: JSON'da ±Inf → null; alt uç null ise −∞, üst uç null ise +∞ */
function gaKarsilastir(ga, beklenen, goreli, mutlak, mesaj) {
  const [a, u] = beklenen;
  if (a === null) assert.equal(ga.alt, -Infinity, `${mesaj} alt`);
  else yakin(ga.alt, a, goreli, mutlak, `${mesaj} alt`);
  if (u === null) assert.equal(ga.ust, Infinity, `${mesaj} üst`);
  else yakin(ga.ust, u, goreli, mutlak, `${mesaj} üst`);
}

// ---------- t testleri ----------

test('bağımsız t testi: Student ve Welch (R t.test)', () => {
  for (const [ad, r] of Object.entries(R.ttestler)) {
    const duzey = ad === 'mtcars_am' ? 0.9 : 0.95;
    for (const [rAlt, alt] of Object.entries(ALT)) {
      for (const [tur, esit] of [['student', true], ['welch', false]]) {
        const b = r[`${tur}_${rAlt}`];
        const s = bagimsizT(r.x, r.y, { esitVaryans: esit, alternatif: alt, duzey });
        const m = `${ad} ${tur} ${rAlt}`;
        yakin(s.istatistik, b.t, 1e-12, 1e-14, `${m} t`);
        yakin(s.sd, b.df, 1e-12, 0, `${m} sd`);
        yakin(s.p, b.p, 1e-11, 1e-300, `${m} p`);
        gaKarsilastir(s.ga, b.ga, 1e-12, 1e-13, `${m} GA`);
        yakin(s.gruplar[0].ortalama, b.tahmin[0], 1e-14, 1e-14, `${m} x̄1`);
        yakin(s.gruplar[1].ortalama, b.tahmin[1], 1e-14, 1e-14, `${m} x̄2`);
        assert.equal(s.yontem, esit ? 'student-t' : 'welch-t');
      }
    }
    const s = bagimsizT(r.x, r.y, { duzey });
    const e = r.etki;
    yakin(s.etki.deger, e.d, 1e-12, 1e-14, `${ad} d`);
    // GA: R'de uniroot(tol = 1e-14) ile pt(ncp) tersine çevrildi; R'nin pnt'si 1e-12 düzeyinde doğru
    yakin(s.etki.gaAlt, e.dGA[0], 1e-9, 1e-11, `${ad} d GA alt`);
    yakin(s.etki.gaUst, e.dGA[1], 1e-9, 1e-11, `${ad} d GA üst`);
    // MBESS ci.smd optimize() ile çalışır (tol 1e-9 kare uzaklıkta) → yaklaşık 1e-5 doğruluk
    yakin(s.etki.gaAlt, e.mbess[0], 1e-4, 1e-5, `${ad} d GA alt (MBESS)`);
    yakin(s.etki.gaUst, e.mbess[1], 1e-4, 1e-5, `${ad} d GA üst (MBESS)`);
    yakin(s.hedgesG.deger, e.g, 1e-12, 1e-14, `${ad} g`);
    yakin(s.hedgesG.gaAlt, e.gGA[0], 1e-9, 1e-11, `${ad} g GA alt`);
    yakin(s.hedgesG.gaUst, e.gGA[1], 1e-9, 1e-11, `${ad} g GA üst`);
    yakin(s.glassDelta.deger, e.glass, 1e-12, 1e-14, `${ad} Glass Δ`);
    yakin(s.birlesikSS, e.sp, 1e-13, 0, `${ad} birleşik SS`);
    yakin(E.hedgesJ(r.x.length + r.y.length - 2), e.J, 1e-12, 0, `${ad} J`);
    // d ile Student t arasındaki dönüşüm
    const st = bagimsizT(r.x, r.y, { duzey });
    yakin(E.tdenD(st.istatistik, r.x.length, r.y.length), st.etki.deger, 1e-12, 1e-14, `${ad} t→d`);
  }
});

test('eşleştirilmiş t testi (R t.test(paired = TRUE))', () => {
  for (const [ad, r] of Object.entries(R.eslesikler)) {
    const duzey = ad === 'iris' ? 0.99 : 0.95;
    for (const [rAlt, alt] of Object.entries(ALT)) {
      const b = r[rAlt];
      const s = eslestirilmisT(r.x, r.y, { alternatif: alt, duzey });
      const m = `${ad} ${rAlt}`;
      yakin(s.istatistik, b.t, 1e-12, 1e-14, `${m} t`);
      assert.equal(s.sd, b.df);
      yakin(s.p, b.p, 1e-11, 1e-300, `${m} p`);
      gaKarsilastir(s.ga, b.ga, 1e-12, 1e-13, `${m} GA`);
      yakin(s.ortalamaFarki, b.tahmin, 1e-13, 1e-14, `${m} fark`);
    }
    const s = eslestirilmisT(r.x, r.y, { duzey });
    const e = r.etki;
    yakin(s.etki.deger, e.dz, 1e-12, 1e-14, `${ad} d_z`);
    yakin(s.hedgesG.deger, e.g, 1e-11, 1e-14, `${ad} g_z`);
    if (Math.abs(s.istatistik) < 37) {
      yakin(s.etki.gaAlt, e.dGA[0], 1e-9, 1e-11, `${ad} d_z GA alt`);
      yakin(s.etki.gaUst, e.dGA[1], 1e-9, 1e-11, `${ad} d_z GA üst`);
      yakin(s.hedgesG.gaAlt, e.gGA[0], 1e-9, 1e-11, `${ad} g_z GA alt`);
    } else {
      // |δ| > 37.62: R'nin pnt'si (AS 243) bu bölgede güvenilmez (MBESS de bunu uyarır); kendi tutarlılığıyla sınanır
      const k = Math.sqrt(s.n);
      const a = (1 - duzey) / 2;
      yakin(pnt(s.istatistik, s.sd, s.etki.gaAlt * k), 1 - a, 1e-10, 0, `${ad} d_z GA alt tutarlılık`);
      yakin(pnt(s.istatistik, s.sd, s.etki.gaUst * k), a, 1e-9, 0, `${ad} d_z GA üst tutarlılık`);
    }
    yakin(s.farklar.standartSapma, e.sdFark, 1e-13, 0, `${ad} SS fark`);
    yakin(s.farklar.standartHata, e.seFark, 1e-13, 0, `${ad} SH fark`);
    yakin(s.korelasyon.r, r.kor.r, 1e-12, 1e-14, `${ad} r`);
    yakin(s.korelasyon.p, r.kor.p, 1e-10, 1e-300, `${ad} r p`);
    yakin(E.tdenDz(s.istatistik, s.n), s.etki.deger, 1e-12, 1e-14, `${ad} t→d_z`);
  }
});

test('tek örneklem t testi (R t.test(mu))', () => {
  for (const [ad, r] of Object.entries(R.tekler)) {
    for (const [rAlt, alt] of Object.entries(ALT)) {
      const b = r[rAlt];
      const s = tekOrneklemT(r.x, { mu: r.mu, alternatif: alt });
      const m = `${ad} ${rAlt}`;
      yakin(s.istatistik, b.t, 1e-12, 1e-14, `${m} t`);
      yakin(s.p, b.p, 1e-11, 1e-300, `${m} p`);
      // R ortalamanın GA'sını verir; ga alanı ortalama farkınındır (SPSS)
      gaKarsilastir(s.ortalamaGA, b.ga, 1e-12, 1e-13, `${m} ortalama GA`);
      if (b.ga[0] !== null) yakin(s.ga.alt, b.ga[0] - r.mu, 1e-12, 1e-13, `${m} fark GA alt`);
    }
    const s = tekOrneklemT(r.x, { mu: r.mu });
    yakin(s.etki.deger, r.etki.d, 1e-12, 1e-14, `${ad} d`);
    if (r.x.length > 2) {
      yakin(s.etki.gaAlt, r.etki.dGA[0], 1e-9, 1e-11, `${ad} d GA alt`);
      yakin(s.etki.gaUst, r.etki.dGA[1], 1e-9, 1e-11, `${ad} d GA üst`);
      yakin(s.hedgesG.deger, r.etki.g, 1e-11, 1e-14, `${ad} g`);
    } else {
      // n = 2 (ν = 1): J(1) = 0 tanımsız düzeltme; R'nin pnt'si burada uyarı verir → kendi tutarlılığıyla sına
      assert.ok(Number.isNaN(s.hedgesG.deger));
      yakin(pnt(s.istatistik, 1, s.etki.gaAlt * Math.sqrt(2)), 0.975, 1e-9, 0, 'n = 2 GA alt tutarlılık');
      yakin(pnt(s.istatistik, 1, s.etki.gaUst * Math.sqrt(2)), 0.025, 1e-9, 0, 'n = 2 GA üst tutarlılık');
    }
  }
});

test('t testleri: hata durumları', () => {
  assert.throws(() => tekOrneklemT([5]), /en az 2/);
  assert.throws(() => tekOrneklemT([3, 3, 3]), /sabit/);
  assert.throws(() => tekOrneklemT([1, 2, NaN]), IstatistikHatasi);
  assert.throws(() => tekOrneklemT([1, 2], { alternatif: 'yanlis' }), /alternatif/);
  assert.throws(() => tekOrneklemT([1, 2], { duzey: 1.2 }), /Güven düzeyi/);
  assert.throws(() => bagimsizT([1], [2, 3]), /en az 2/);
  assert.throws(() => bagimsizT([2, 2], [2, 2, 2]), /sabit/);
  assert.throws(() => eslestirilmisT([1, 2, 3], [1, 2]), /eşit/);
  assert.throws(() => eslestirilmisT([1, 2, 3], [0, 1, 2]), /sabit/);
  // neredeyse sabit veri: hesaplanır (R t.test'in "essentially constant" eşiği 10·eps·|x̄|)
  const ys = tekOrneklemT([5, 5, 5, 5.000000001], { mu: 5 });
  yakin(ys.istatistik, 1, 1e-5, 0, 'neredeyse sabit t');
  // tek gruptaki sıfır varyans Welch'te sorun değildir
  const w = bagimsizT([1, 1, 1], [2, 3, 5], { esitVaryans: false });
  yakin(w.sd, 2, 1e-14, 0, 'Welch sd = n2 − 1');
});

// ---------- Gruplu testler ----------

// jsonlite tek elemanlı vektörleri skalere çevirir (k = 2'de tek karşılaştırma); diziye geri çevir
const dizi = (v) => (Array.isArray(v) ? v : [v]);
for (const r of Object.values(R.gruplar)) {
  for (const anahtar of ['tukey', 'lsd', 'bonferroni', 'scheffe', 'gh', 'ghRstatix', 't2']) {
    if (r[anahtar]) for (const [a, v] of Object.entries(r[anahtar])) r[anahtar][a] = dizi(v);
  }
  for (const d of r.dunnett) for (const alt of ['two.sided', 'greater', 'less']) for (const a of ['deney', 't', 'p', 'alt', 'ust']) d[alt][a] = dizi(d[alt][a]);
}
const gruplu = Object.entries(R.gruplar);

test('varyans homojenliği: Levene, Brown-Forsythe, Bartlett, Fligner (car, stats)', () => {
  for (const [ad, r] of gruplu) {
    const lv = levene(r.gruplar);
    yakin(lv.istatistik, r.levene.F, 1e-11, 1e-14, `${ad} Levene F`);
    assert.deepEqual(lv.sd, r.levene.df);
    yakin(lv.p, r.levene.p, 1e-10, 1e-300, `${ad} Levene p`);
    const lm = levene(r.gruplar, { merkez: 'medyan' });
    yakin(lm.istatistik, r.leveneMedyan.F, 1e-11, 1e-14, `${ad} BF-Levene F`);
    yakin(lm.p, r.leveneMedyan.p, 1e-10, 1e-300, `${ad} BF-Levene p`);
    if (r.bartlett) {
      const b = bartlett(r.gruplar);
      yakin(b.istatistik, r.bartlett.K2, 1e-11, 1e-14, `${ad} Bartlett`);
      yakin(b.p, r.bartlett.p, 1e-10, 1e-300, `${ad} Bartlett p`);
      assert.equal(b.sd, r.bartlett.df);
    }
    const f = fligner(r.gruplar);
    yakin(f.istatistik, r.fligner.X2, 1e-11, 1e-14, `${ad} Fligner`);
    yakin(f.p, r.fligner.p, 1e-10, 1e-300, `${ad} Fligner p`);
  }
  assert.throws(() => levene([[1, 2, 3]]), /en az 2 grup/);
  assert.throws(() => levene([[1, 2], []]), /boş grup/);
  assert.throws(() => levene([[1], [2]]), /grup sayısından/);
  assert.throws(() => levene([[1, 1], [2, 2]]), /sabit/);
  assert.throws(() => levene([[1, 2]], { merkez: 'mod' }), /merkez/);
  assert.throws(() => bartlett([[1, 2, 3], [4, 4, 4]]), /varyansı sıfır/);
  assert.throws(() => bartlett([[1, 2, 3], [4]]), /en az 2/);
});

test('tek yönlü ANOVA ve etki büyüklükleri (R aov, merkezi olmayan F)', () => {
  for (const [ad, r] of gruplu) {
    const a = r.anova;
    const s = tekYonluAnova(r.gruplar);
    const [ara, ic, top] = s.anovaTablosu;
    yakin(ara.KT, a.SSb, 1e-11, 1e-12, `${ad} KT ara`);
    yakin(ic.KT, a.SSw, 1e-11, 1e-12, `${ad} KT iç`);
    yakin(top.KT, a.SSb + a.SSw, 1e-11, 1e-12, `${ad} KT toplam`);
    assert.deepEqual([ara.sd, ic.sd, top.sd], [a.dfb, a.dfw, a.dfb + a.dfw]);
    yakin(ara.KO, a.MSb, 1e-11, 1e-12, `${ad} KO ara`);
    yakin(ic.KO, a.MSw, 1e-11, 1e-12, `${ad} KO iç`);
    yakin(s.istatistik, a.F, 1e-11, 1e-12, `${ad} F`);
    yakin(s.p, a.p, 1e-10, 1e-300, `${ad} p`);
    yakin(s.etaKare, a.eta2, 1e-11, 1e-14, `${ad} η²`);
    yakin(s.omegaKare, a.omega2, 1e-10, 1e-13, `${ad} ω²`);
    yakin(s.epsilonKare, a.epsilon2, 1e-10, 1e-13, `${ad} ε²`);
    // R'nin merkezi olmayan F'si (AS 226) 1e-9 mutlak hata payıyla çalışır; kök buna göre ~1e-6 göreli doğrulukta
    yakin(s.etki.gaAlt, a.eta2GA[0], 1e-6, 1e-9, `${ad} η² GA alt`);
    yakin(s.etki.gaUst, a.eta2GA[1], 1e-6, 1e-9, `${ad} η² GA üst`);
    // kendi tutarlılığı: sınırlar P(F ≤ f; λ) = 0.975 / 0.025'i sağlar
    const N = a.dfb + a.dfw + 1;
    const lAlt = (s.etki.gaAlt * N) / (1 - s.etki.gaAlt);
    const lUst = (s.etki.gaUst * N) / (1 - s.etki.gaUst);
    if (lAlt > 0) yakin(pnf(a.F, a.dfb, a.dfw, lAlt), 0.975, 1e-10, 0, `${ad} λ alt tutarlılık`);
    yakin(pnf(a.F, a.dfb, a.dfw, lUst), 0.025, 1e-9, 0, `${ad} λ üst tutarlılık`);
    yakin(E.fdenEtaKare(a.F, a.dfb, a.dfw), a.eta2, 1e-11, 0, `${ad} F→η²`);
    yakin(E.fdenOmegaKare(a.F, a.dfb, a.dfw), a.omega2, 1e-10, 1e-13, `${ad} F→ω²`);
    s.gruplar.forEach((g, i) => {
      yakin(g.ortalama, a.ort[i], 1e-13, 1e-13, `${ad} ort ${i}`);
      yakin(g.standartSapma, a.ss[i], 1e-12, 1e-14, `${ad} ss ${i}`);
      yakin(g.gaAlt, a.gaAlt[i], 1e-12, 1e-13, `${ad} GA alt ${i}`);
      yakin(g.gaUst, a.gaUst[i], 1e-12, 1e-13, `${ad} GA üst ${i}`);
    });
  }
  assert.throws(() => tekYonluAnova([[1, 2, 3]]), /en az 2 grup/);
  assert.throws(() => tekYonluAnova([[1, 1], [2, 2]]), /Grup içi varyans sıfır/);
  assert.throws(() => tekYonluAnova([[1], [2]]), /grup sayısından/);
  // tek gözlemli grup: hesaplanır, uyarı verir, o grubun SS'si null
  const t = tekYonluAnova([[1, 2, 3], [7], [4, 6]]);
  assert.equal(t.gruplar[1].standartSapma, null);
  assert.ok(t.uyarilar.length > 0);
});

test('Welch ve Brown-Forsythe ANOVA (oneway.test, onewaytests::bf.test)', () => {
  for (const [ad, r] of gruplu) {
    if (!r.welch) continue;
    const w = welchAnova(r.gruplar);
    yakin(w.istatistik, r.welch.F, 1e-11, 1e-14, `${ad} Welch F`);
    yakin(w.sd[1], r.welch.df[1], 1e-11, 0, `${ad} Welch sd2`);
    yakin(w.p, r.welch.p, 1e-10, 1e-300, `${ad} Welch p`);
    const b = brownForsytheAnova(r.gruplar);
    yakin(b.istatistik, r.bf.F, 1e-11, 1e-14, `${ad} BF F`);
    yakin(b.sd[1], r.bf.df[1], 1e-11, 0, `${ad} BF sd2`);
    yakin(b.p, r.bf.p, 1e-10, 1e-300, `${ad} BF p`);
    yakin(b.istatistik, r.bf.paketF, 1e-11, 1e-14, `${ad} BF F (onewaytests)`);
    yakin(b.sd[1], r.bf.paketDf[1], 1e-11, 0, `${ad} BF sd2 (onewaytests)`);
    yakin(b.p, r.bf.paketP, 1e-10, 1e-300, `${ad} BF p (onewaytests)`);
  }
  // k = 2'de Welch ANOVA F = Welch t²
  const x = R.ttestler.sleep.x;
  const y = R.ttestler.sleep.y;
  const w = welchAnova([x, y]);
  yakin(w.istatistik, bagimsizT(x, y, { esitVaryans: false }).istatistik ** 2, 1e-12, 0, 'Welch F = t²');
  assert.throws(() => welchAnova([[1, 2, 3], [5, 5, 5]]), /varyansı sıfır/);
  assert.throws(() => welchAnova([[1, 2, 3], [5]]), /en az 2/);
  assert.throws(() => brownForsytheAnova([[1, 1], [5, 5]]), /varyansı sıfır/);
});

test('varyans oranı F testi (R var.test)', () => {
  for (const [ad, r] of Object.entries(R.varyansF)) {
    for (const [rAlt, alt] of Object.entries(ALT)) {
      const b = r[rAlt];
      const s = varyansOraniF(r.x, r.y, { oran: r.ratio, alternatif: alt, duzey: r.conf });
      yakin(s.istatistik, b.F, 1e-12, 0, `${ad} ${rAlt} F`);
      assert.deepEqual(s.sd, b.df);
      // R 1 − pf ile hesaplar; bizimki üst kuyruğu doğrudan hesaplar
      yakin(s.p, b.p, 1e-10, 1e-15, `${ad} ${rAlt} p`);
      gaKarsilastir(s.ga, b.ga, 1e-11, 1e-14, `${ad} ${rAlt} GA`);
      yakin(s.varyansOrani, b.oran, 1e-12, 0, `${ad} oran`);
    }
  }
  assert.throws(() => varyansOraniF([1, 2, 3], [4, 4, 4]), /varyansı sıfır/);
  assert.throws(() => varyansOraniF([1], [4, 5]), /en az 2/);
});

// ---------- Post hoc ----------

/** R'nin ptukey'i ~1e-7 (sd < 5 iken ~1e-4) doğrulukta; qtukey benzer şekilde (dagilim.test.js) */
function tukeyTol(sd) {
  return sd < 5 ? { p: 2e-4, q: 1e-3 } : { p: 2e-7, q: 2e-6 };
}

function aralikKarsilastir(satir, alt, ust, goreliYari, m) {
  const yariR = (ust - alt) / 2;
  const yari = (satir.gaUst - satir.gaAlt) / 2;
  yakin((satir.gaAlt + satir.gaUst) / 2, (alt + ust) / 2, 1e-12, 1e-12, `${m} GA merkez`);
  yakin(yari, yariR, goreliYari, 0, `${m} GA yarı genişlik`);
}

test('Tukey HSD (R TukeyHSD)', () => {
  for (const [ad, r] of gruplu) {
    const s = tukeyHSD(r.gruplar);
    const tol = tukeyTol(s.sdHata);
    s.karsilastirmalar.forEach((c, i) => {
      const m = `${ad} (${c.i},${c.j})`;
      yakin(c.fark, r.tukey.fark[i], 1e-12, 1e-12, `${m} fark`);
      yakin(c.sh, r.lsd.se[i], 1e-12, 0, `${m} sh`);
      yakin(c.p, r.tukey.p[i], 0, tol.p, `${m} p`);
      aralikKarsilastir(c, r.tukey.alt[i], r.tukey.ust[i], tol.q, m);
    });
  }
  // k = 2: Tukey = Student t testi (kesin; ptukey(q, 2, ν) = P(|T_ν| ≤ q/√2))
  const x = R.ttestler.sleep.x;
  const y = R.ttestler.sleep.y;
  const tk = tukeyHSD([x, y]).karsilastirmalar[0];
  const st = bagimsizT(x, y);
  yakin(tk.p, st.p, 1e-10, 0, 'k = 2 p');
  yakin(tk.gaAlt, st.ga.alt, 1e-9, 0, 'k = 2 GA alt');
  assert.throws(() => tukeyHSD([[1, 2, 3]]), /en az 2 grup/);
  assert.throws(() => tukeyHSD([[1, 1], [3, 3]]), /varyans sıfır/);
});

test('Bonferroni, LSD, Scheffé (pairwise.t.test ve elle R)', () => {
  for (const [ad, r] of gruplu) {
    const adlar = r.gruplar.map((_, i) => `G${i}`);
    const L = lsd(r.gruplar, { adlar });
    const B = bonferroniPostHoc(r.gruplar);
    const S = scheffe(r.gruplar);
    L.karsilastirmalar.forEach((c, i) => {
      const m = `${ad} (${c.i},${c.j})`;
      assert.equal(c.grupI, `G${c.i}`);
      yakin(c.fark, r.lsd.fark[i], 1e-12, 1e-12, `${m} fark`);
      yakin(c.istatistik, r.lsd.t[i], 1e-11, 1e-13, `${m} t`);
      yakin(c.p, r.lsd.p[i], 1e-10, 1e-300, `${m} LSD p`);
      yakin(c.gaAlt, r.lsd.alt[i], 1e-12, 1e-12, `${m} LSD GA alt`);
      yakin(c.gaUst, r.lsd.ust[i], 1e-12, 1e-12, `${m} LSD GA üst`);
      const b = B.karsilastirmalar[i];
      yakin(b.p, r.bonferroni.p[i], 1e-10, 1e-300, `${m} Bonferroni p`);
      yakin(b.gaAlt, r.bonferroni.alt[i], 1e-12, 1e-12, `${m} Bonferroni GA alt`);
      yakin(b.gaUst, r.bonferroni.ust[i], 1e-12, 1e-12, `${m} Bonferroni GA üst`);
      const sc = S.karsilastirmalar[i];
      yakin(sc.istatistik, r.scheffe.F[i], 1e-11, 1e-13, `${m} Scheffé F`);
      yakin(sc.p, r.scheffe.p[i], 1e-10, 1e-300, `${m} Scheffé p`);
      yakin(sc.gaAlt, r.scheffe.alt[i], 1e-12, 1e-12, `${m} Scheffé GA alt`);
      yakin(sc.gaUst, r.scheffe.ust[i], 1e-12, 1e-12, `${m} Scheffé GA üst`);
    });
  }
});

test('Games-Howell (elle R + ptukey; rstatix::games_howell_test) ve Tamhane T2', () => {
  for (const [ad, r] of gruplu) {
    if (!r.gh) continue;
    const s = gamesHowell(r.gruplar);
    const t2 = tamhaneT2(r.gruplar);
    s.karsilastirmalar.forEach((c, i) => {
      const m = `${ad} (${c.i},${c.j})`;
      const tol = tukeyTol(c.sd);
      yakin(c.fark, r.gh.fark[i], 1e-12, 1e-12, `${m} fark`);
      yakin(c.sh, r.gh.se[i], 1e-12, 0, `${m} sh`);
      yakin(c.sd, r.gh.df[i], 1e-11, 0, `${m} sd`);
      yakin(c.istatistik, r.gh.q[i], 1e-11, 0, `${m} q`);
      yakin(c.p, r.gh.p[i], 0, tol.p, `${m} p`);
      aralikKarsilastir(c, r.gh.alt[i], r.gh.ust[i], tol.q, m);
      // rstatix: istatistik = |t| = q/√2, tahmin = x̄ⱼ − x̄ᵢ
      yakin(c.istatistik / Math.SQRT2, r.ghRstatix.statistic[i], 1e-11, 0, `${m} rstatix t`);
      yakin(c.sd, r.ghRstatix.df[i], 1e-11, 0, `${m} rstatix sd`);
      yakin(-c.fark, r.ghRstatix.est[i], 1e-12, 1e-12, `${m} rstatix tahmin`);
      aralikKarsilastir(c, -r.ghRstatix.high[i], -r.ghRstatix.low[i], tol.q, `${m} rstatix`);
      const t = t2.karsilastirmalar[i];
      yakin(t.istatistik, r.t2.t[i], 1e-11, 1e-13, `${m} T2 t`);
      yakin(t.p, r.t2.p[i], 1e-10, 1e-300, `${m} T2 p`);
      yakin(t.gaAlt, r.t2.alt[i], 1e-11, 1e-12, `${m} T2 GA alt`);
      yakin(t.gaUst, r.t2.ust[i], 1e-11, 1e-12, `${m} T2 GA üst`);
    });
  }
  assert.throws(() => gamesHowell([[1, 2, 3], [4]]), /en az 2/);
  assert.throws(() => gamesHowell([[1, 1], [3, 3], [4, 6]]), /varyansı sıfır/);
  assert.throws(() => tamhaneT2([[1, 2, 3]]), /en az 2 grup/);
});

// ---------- Dunnett ----------

/** Bağımsız kontrol: iki katlı uyarlamalı integral ve dagilim.pnorm ile P(max|Tᵢ| ≤ c) */
function dunnettYavas(c, lambdalar, nu, ikiYonlu) {
  const G = (u) =>
    integral(
      (z) => {
        let urun = Math.exp(-0.5 * z * z) / Math.sqrt(2 * Math.PI);
        for (const l of lambdalar) {
          const s = Math.sqrt(1 - l * l);
          urun *= ikiYonlu ? pnorm((u - l * z) / s) - pnorm((-u - l * z) / s) : pnorm((u - l * z) / s);
        }
        return urun;
      },
      -9,
      9,
      1e-13,
    );
  if (nu === Infinity) return G(c);
  // s = √(χ²_ν/ν) yoğunluğu, χ² üzerinden: E[G(c √(X/ν))]
  const logf = (x) => (nu / 2 - 1) * Math.log(x) - x / 2 - (nu / 2) * Math.LN2 - lgammaYerel(nu / 2);
  const ust = nu + 40 * Math.sqrt(2 * nu) + 200;
  const noktalar = [0, Math.max(nu / 4, 1e-9), nu / 2, nu, 2 * nu, 4 * nu + 20, ust];
  let t = 0;
  for (let i = 0; i < noktalar.length - 1; i++) t += integral((x) => (x > 0 ? G(c * Math.sqrt(x / nu)) * Math.exp(logf(x)) : 0), noktalar[i], noktalar[i + 1], 1e-11);
  return t;
}
function lgammaYerel(x) {
  // Lanczos (bağımsız gerçekleme)
  const g = 7;
  const k = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];
  x -= 1;
  let a = k[0];
  const t = x + g + 0.5;
  for (let i = 1; i < 9; i++) a += k[i] / (x + i);
  return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
}

test('Dunnett dağılımı: kesin integral (mvtnorm ve bağımsız uyarlamalı integral ile)', () => {
  for (const r of R.dunnettDagilim) {
    const nu = r.nu === null ? Infinity : r.nu;
    const p = dunnettOlasilik(r.c, r.lam, nu, r.iki);
    // mvtnorm (Genz-Bretz) rastgele kuasi-Monte Carlo: bildirdiği hata ~%99 güven sınırıdır
    yakin(p, r.p, 0, Math.max(3 * r.hata, 1e-12), `pmvt(c = ${r.c}, ν = ${nu})`);
  }
  // bağımsız gerçekleme ile 1e-10 düzeyinde uyum
  const durumlar = [
    [2.5, [Math.SQRT1_2, Math.SQRT1_2, Math.SQRT1_2], 20, true],
    [2.0, [Math.sqrt(100 / 103), Math.sqrt(2 / 5), Math.sqrt(50 / 53)], 150, true],
    [1.9, [Math.sqrt(3 / 7), Math.sqrt(30 / 34)], 3, true],
    [2.2, [0.3, 0.6, 0.9], 12, false],
    [2.4, [0.5, 0.7], Infinity, true],
  ];
  for (const [c, l, nu, iki] of durumlar) {
    yakin(dunnettOlasilik(c, l, nu, iki), dunnettYavas(c, l, nu, iki), 0, 2e-10, `bağımsız (c = ${c}, ν = ${nu})`);
  }
  // tek karşılaştırma: t dağılımına indirgenir (kesin)
  for (const nu of [1, 2, 5, 30, 1e5, Infinity]) {
    for (const c of [0.3, 1, 2.1, 4.5]) {
      yakin(dunnettOlasilik(c, [0.6], nu, true), 1 - 2 * pt(-c, nu), 0, 1e-12, `k = 1 iki yönlü (${c}, ${nu})`);
      yakin(dunnettOlasilik(c, [0.6], nu, false), pt(c, nu), 0, 1e-12, `k = 1 tek yönlü (${c}, ${nu})`);
    }
  }
  yakin(dunnettKritik(0.95, [0.6], 17), qt(0.975, 17), 1e-12, 0, 'k = 1 kritik değer');
  // hızlı Φ, R'nin pnorm_both'u ile aynı Cody katsayıları
  // (x < −30'da dagilim.pnorm'un kendi göreli hatası ~1e-13'tür)
  for (let x = -37; x <= 8; x += 0.37) yakin(phiHizli(x), pnorm(x), x < -30 ? 5e-13 : 1e-13, 1e-300, `Φ(${x})`);
});

test('Dunnett testi (R multcomp::glht, tek adımlı)', () => {
  for (const [ad, r] of gruplu) {
    for (const d of r.dunnett) {
      for (const [rAlt, alt] of Object.entries(ALT)) {
        const b = d[rAlt];
        const s = dunnett(r.gruplar, d.kontrol, { alternatif: alt });
        const m = `${ad} kontrol=${d.kontrol} ${rAlt}`;
        assert.deepEqual(s.karsilastirmalar.map((c) => c.i), b.deney, `${m} sıra`);
        // 2 karşılaştırmada mvtnorm kesin (iki değişkenli) algoritma kullanır; daha fazlasında
        // rastgele Genz-Bretz (bildirilen hata ≤ 1.2e-7); 1 karşılaştırmada kesin t
        const pTol = b.hata > 1e-12 ? Math.max(3 * b.hata, 1e-7) : 1e-9;
        // kritik değer: qmvt(ptol = 0.001) + 2'den fazla karşılaştırmada varsayılan Genz-Bretz (abseps = 1e-3)
        const cTol = b.hata > 1e-12 ? 3e-3 : 2e-4;
        s.karsilastirmalar.forEach((c, i) => {
          yakin(c.istatistik, b.t[i], 1e-11, 1e-13, `${m} t ${i}`);
          yakin(c.p, b.p[i], 0, pTol, `${m} p ${i}`);
          // multcomp'un eşzamanlı GA'sı qmvt(ptol = 0.001) ile bulunan kritik değere dayanır (~1e-4 göreli hata);
          // GA = fark ± c·sh olduğundan fark (merkez) sıkı, c·sh yarı genişliği gevşek karşılaştırılır
          const sh = c.sh;
          if (b.alt[i] !== null) {
            yakin(c.gaAlt + s.kritikDeger * sh, b.alt[i] + Math.abs(b.kritik) * sh, 1e-11, 1e-12, `${m} GA alt merkez ${i}`);
            yakin(c.gaAlt, b.alt[i], 0, cTol * s.kritikDeger * sh, `${m} GA alt ${i}`);
          } else assert.equal(c.gaAlt, -Infinity);
          if (b.ust[i] !== null) {
            yakin(c.gaUst - s.kritikDeger * sh, b.ust[i] - Math.abs(b.kritik) * sh, 1e-11, 1e-12, `${m} GA üst merkez ${i}`);
            yakin(c.gaUst, b.ust[i], 0, cTol * s.kritikDeger * sh, `${m} GA üst ${i}`);
          } else assert.equal(c.gaUst, Infinity);
        });
        // multcomp'un kritik değeri qmvt kök bulma toleransıyla (ptol = 0.001) sınırlı; tek yönlüde işaretli verilir.
        // Örn. PlantGrowth: multcomp 2.333538 → mvtnorm::pmvt (kesin iki değişkenli) 0.9500136; bizimki 2.333412 → 0.9500000.
        yakin(s.kritikDeger, Math.abs(b.kritik), cTol, 0, `${m} kritik`);
        const lam = s.karsilastirmalar.map((c) => Math.sqrt(s.gruplar[c.i].n / (s.gruplar[c.i].n + s.gruplar[c.j].n)));
        yakin(dunnettOlasilik(s.kritikDeger, lam, s.sdHata, alt === 'iki'), 0.95, 1e-11, 0, `${m} kritik tutarlılık`);
      }
    }
  }
  // k = 2: Dunnett = Student t testi
  const x = R.ttestler.sleep.x;
  const y = R.ttestler.sleep.y;
  const d = dunnett([x, y], 1).karsilastirmalar[0];
  yakin(d.p, bagimsizT(x, y).p, 1e-10, 0, 'k = 2 p');
  yakin(d.gaAlt, bagimsizT(x, y).ga.alt, 1e-10, 0, 'k = 2 GA');
  assert.throws(() => dunnett([[1, 2], [3, 4]], 2), /Kontrol/);
  assert.throws(() => dunnett([[1, 2], [3, 4]], 0, { alternatif: 'x' }), /alternatif/);
});

// ---------- Etki büyüklüğü yardımcıları ----------

test('etki: merkezi olmayan t ve F güven aralıkları, Hedges J', () => {
  for (const r of R.etkiNcp.t) {
    for (const [anahtar, duzey] of [['ga', 0.95], ['ga90', 0.9]]) {
      const g = E.ncpTGA(r.t, r.df, duzey);
      // R'nin pnt'si df = 3'te "tam hassasiyet sağlanamadı" uyarısı verir → orada 1e-7
      const tol = r.df < 5 ? 1e-7 : 1e-9;
      yakin(g.alt, r[anahtar][0], tol, 1e-9, `ncp t=${r.t} alt ${duzey}`);
      yakin(g.ust, r[anahtar][1], tol, 1e-9, `ncp t=${r.t} üst ${duzey}`);
      yakin(pnt(r.t, r.df, g.alt), 1 - (1 - duzey) / 2, 1e-11, 0, `t=${r.t} tutarlılık alt`);
      yakin(pnt(r.t, r.df, g.ust), (1 - duzey) / 2, 1e-10, 0, `t=${r.t} tutarlılık üst`);
    }
  }
  for (const r of R.etkiNcp.f) {
    for (const [anahtar, duzey] of [['ga', 0.95], ['ga90', 0.9]]) {
      const g = E.ncpFGA(r.F, r.df1, r.df2, duzey);
      yakin(g.alt, r[anahtar][0], 1e-6, 1e-8, `λ F=${r.F} alt ${duzey}`);
      yakin(g.ust, r[anahtar][1], 1e-6, 1e-8, `λ F=${r.F} üst ${duzey}`);
      if (g.alt > 0) yakin(pnf(r.F, r.df1, r.df2, g.alt), 1 - (1 - duzey) / 2, 1e-11, 0, `F=${r.F} tutarlılık alt`);
      else assert.ok(pf(r.F, r.df1, r.df2) <= 1 - (1 - duzey) / 2);
    }
  }
  // R tarafında J, lgamma farkıyla hesaplandı (büyük ν'de iptal hatası); bizimki lgammaFark ile iptalsizdir
  for (const r of R.etkiNcp.J) yakin(E.hedgesJ(r.df), r.J, r.df >= 100 ? 1e-9 : 1e-13, 0, `J(${r.df})`);
  assert.ok(Number.isNaN(E.hedgesJ(1)));
  // büyük ν'de J ≈ 1 − 3/(4ν − 1)
  yakin(E.hedgesJ(1e4), 1 - 3 / (4e4 - 1), 1e-8, 0, 'J yaklaşımı');
  assert.throws(() => E.ncpTGA(2, 10, 0), /Güven düzeyi/);
});

test('etki: dönüşümler ve Cohen (1988) yorumları', () => {
  yakin(E.tdenR(2, 16), 2 / Math.sqrt(20), 1e-15, 0, 't→r');
  yakin(E.kiKaredenCramerV(12, 100, 3, 4), Math.sqrt(12 / 200), 1e-15, 0, 'χ²→V');
  assert.ok(Number.isNaN(E.kiKaredenCramerV(1, 10, 1, 4)));
  yakin(E.zdenR(-2.5, 25), -0.5, 1e-15, 0, 'z→r');
  yakin(E.ddenR(0.8), 0.8 / Math.sqrt(4.64), 1e-15, 0, 'd→r');
  assert.deepEqual([0, 0.19, 0.2, 0.49, 0.5, 0.79, 0.8, -1.2].map(E.yorumD), ['önemsiz', 'önemsiz', 'küçük', 'küçük', 'orta', 'orta', 'büyük', 'büyük']);
  assert.deepEqual([0.05, 0.1, 0.3, -0.45, 0.5].map(E.yorumR), ['önemsiz', 'küçük', 'orta', 'orta', 'büyük']);
  assert.deepEqual([0.005, 0.01, 0.06, 0.1399, 0.14].map(E.yorumEtaKare), ['önemsiz', 'küçük', 'orta', 'orta', 'büyük']);
  assert.deepEqual([0.09, 0.1, 0.3, 0.5].map((v) => E.yorumCramerV(v, 1)), ['önemsiz', 'küçük', 'orta', 'büyük']);
  // sd* = 4: eşikler 0.05 / 0.15 / 0.25
  assert.deepEqual([0.04, 0.05, 0.15, 0.25].map((v) => E.yorumCramerV(v, 4)), ['önemsiz', 'küçük', 'orta', 'büyük']);
  assert.deepEqual([0.05, 0.1, 0.25, 0.4].map(E.yorumF), ['önemsiz', 'küçük', 'orta', 'büyük']);
  assert.equal(E.yorumD(NaN), null);
  assert.equal(E.yorumD(null), null);
});
