import { test } from 'node:test';
import * as D from '../src/cekirdek/dagilim.js';
import { digamma, lbeta, lgamma } from '../src/cekirdek/sayisal.js';
import { inf, referans, yakin } from './yardimci.js';

const R = referans('dagilim');

test('normal dağılım', () => {
  for (const r of R.normal) {
    yakin(D.pnorm(r.z), r.alt, 1e-12, 1e-320, `pnorm(${r.z})`);
    yakin(D.pnorm(r.z, 0, 1, false), r.ust, 1e-12, 1e-320, `pnorm üst(${r.z})`);
    yakin(D.dnorm(r.z), r.d, 1e-12, 1e-320, `dnorm(${r.z})`);
  }
  for (const r of R.qnormal) {
    yakin(D.qnorm(r.p), r.q, 1e-12, 1e-14, `qnorm(${r.p})`);
    yakin(D.qnorm(r.p, 0, 1, false), r.qust, 1e-12, 1e-14, `qnorm üst(${r.p})`);
  }
});

test('t dağılımı', () => {
  for (const r of R.t) {
    yakin(D.pt(r.t, r.df), r.alt, 1e-10, 1e-300, `pt(${r.t}, ${r.df})`);
    yakin(D.pt(r.t, r.df, false), r.ust, 1e-10, 1e-300, `pt üst(${r.t}, ${r.df})`);
    yakin(D.dt(r.t, r.df), r.d, 1e-10, 1e-300, `dt(${r.t}, ${r.df})`);
  }
  for (const r of R.qt) yakin(D.qt(r.p, r.df), r.q, 1e-10, 1e-12, `qt(${r.p}, ${r.df})`);
});

test('ki-kare dağılımı', () => {
  for (const r of R.kikare) {
    yakin(D.pchisq(r.x, r.df), r.alt, 1e-10, 1e-300, `pchisq(${r.x}, ${r.df})`);
    yakin(D.pchisq(r.x, r.df, false), r.ust, 1e-10, 1e-300, `pchisq üst(${r.x}, ${r.df})`);
    yakin(D.dchisq(r.x, r.df), r.d, 1e-10, 1e-300, `dchisq(${r.x}, ${r.df})`);
  }
  for (const r of R.qkikare) yakin(D.qchisq(r.p, r.df), r.q, 1e-10, 1e-12, `qchisq(${r.p}, ${r.df})`);
});

test('F dağılımı', () => {
  for (const r of R.f) {
    yakin(D.pf(r.f, r.d1, r.d2), r.alt, 1e-10, 1e-300, `pf(${r.f}, ${r.d1}, ${r.d2})`);
    yakin(D.pf(r.f, r.d1, r.d2, false), r.ust, 1e-10, 1e-300, `pf üst(${r.f}, ${r.d1}, ${r.d2})`);
    yakin(D.df_(r.f, r.d1, r.d2), r.d, 1e-9, 1e-300, `df(${r.f}, ${r.d1}, ${r.d2})`);
  }
  for (const r of R.qf) yakin(D.qf(r.p, r.d1, r.d2), r.q, 1e-10, 1e-12, `qf(${r.p}, ${r.d1}, ${r.d2})`);
});

test('beta dağılımı', () => {
  for (const r of R.beta) {
    yakin(D.pbeta(r.x, r.a, r.b), r.alt, 1e-10, 1e-300, `pbeta(${r.x}, ${r.a}, ${r.b})`);
    yakin(D.pbeta(r.x, r.a, r.b, false), r.ust, 1e-10, 1e-300, `pbeta üst(${r.x}, ${r.a}, ${r.b})`);
  }
  for (const r of R.qbeta) yakin(D.qbeta(r.p, r.a, r.b), r.q, 1e-9, 1e-14, `qbeta(${r.p}, ${r.a}, ${r.b})`);
});

test('kesikli dağılımlar', () => {
  for (const r of R.binom) {
    yakin(D.dbinom(r.x, r.n, r.p), r.d, 1e-11, 1e-300, `dbinom(${r.x}, ${r.n}, ${r.p})`);
    yakin(D.pbinom(r.x, r.n, r.p), r.alt, 1e-10, 1e-300, `pbinom(${r.x}, ${r.n}, ${r.p})`);
    yakin(D.pbinom(r.x, r.n, r.p, false), r.ust, 1e-10, 1e-300, `pbinom üst(${r.x}, ${r.n}, ${r.p})`);
  }
  for (const r of R.hiper) {
    yakin(D.dhyper(r.x, r.m, r.n, r.k), r.d, 1e-11, 1e-300, `dhyper(${r.x})`);
    yakin(D.phyper(r.x, r.m, r.n, r.k), r.alt, 1e-11, 1e-300, `phyper(${r.x})`);
    yakin(D.phyper(r.x, r.m, r.n, r.k, false), r.ust, 1e-11, 1e-300, `phyper üst(${r.x})`);
  }
  for (const r of R.pois) {
    yakin(D.dpois(r.x, r.lambda), r.d, 1e-11, 1e-300, `dpois(${r.x}, ${r.lambda})`);
    yakin(D.ppois(r.x, r.lambda), r.alt, 1e-10, 1e-300, `ppois(${r.x}, ${r.lambda})`);
    yakin(D.ppois(r.x, r.lambda, false), r.ust, 1e-10, 1e-300, `ppois üst(${r.x}, ${r.lambda})`);
  }
});

test('Tukey studentized range', () => {
  // R'nin ptukey algoritması (Copenhaver & Holland) yaklaşık 1e-7 doğrulukta; sd < 5 iken hata 1e-4'e çıkar.
  // Bu yüzden R ile karşılaştırma toleransı buna göre, kesin doğruluk ise aşağıdaki k = 2 testiyle sınanır.
  for (const r of R.tukey) {
    const tol = inf(r.df) < 5 ? 2e-4 : 2e-7;
    yakin(D.ptukey(r.q, r.k, inf(r.df)), r.p, 0, tol, `ptukey(${r.q}, ${r.k}, ${r.df})`);
  }
  for (const r of R.qtukey) yakin(D.qtukey(r.p, r.k, inf(r.df)), r.q, inf(r.df) < 5 ? 1e-3 : 1e-6, 0, `qtukey(${r.p}, ${r.k}, ${r.df})`);
});

test('Tukey: k = 2 için kesin değer (|t| dağılımı)', () => {
  // Q = |Z1 - Z2| / S  →  P(Q <= q) = P(|T_ν| <= q/√2)
  for (const df of [1, 2, 3, 5, 10, 60, 1000, Infinity]) {
    for (const q of [0.2, 0.5, 1, 2, 3, 5, 8, 12]) {
      const kesin = 1 - 2 * D.pt(-q / Math.SQRT2, df);
      yakin(D.ptukey(q, 2, df), kesin, 0, 1e-11, `ptukey(${q}, 2, ${df})`);
    }
  }
});

test('merkezi olmayan dağılımlar', () => {
  for (const r of R.nt) {
    yakin(D.pnt(r.t, r.df, r.ncp), r.alt, 1e-8, 1e-12, `pt(${r.t}, ${r.df}, ncp=${r.ncp})`);
    yakin(D.pnt(r.t, r.df, r.ncp, false), r.ust, 1e-8, 1e-12, `pt üst(${r.t}, ${r.df}, ncp=${r.ncp})`);
  }
  // R'nin merkezi olmayan beta algoritması (AS 226) 1e-9 hata payıyla çalışır
  for (const r of R.nf) {
    yakin(D.pnf(r.f, r.d1, r.d2, r.ncp), r.alt, 1e-12, 2e-9, `pf(${r.f}, ncp=${r.ncp})`);
    yakin(D.pnf(r.f, r.d1, r.d2, r.ncp, false), r.ust, 1e-12, 2e-9, `pf üst(${r.f}, ncp=${r.ncp})`);
  }
  for (const r of R.nkikare) {
    yakin(D.pnchisq(r.x, r.df, r.ncp), r.alt, 1e-9, 1e-14, `pchisq(${r.x}, ncp=${r.ncp})`);
    yakin(D.pnchisq(r.x, r.df, r.ncp, false), r.ust, 1e-9, 1e-14, `pchisq üst(${r.x}, ncp=${r.ncp})`);
  }
});

test('kesin sıra dağılımları', () => {
  for (const r of R.isaretli) {
    yakin(D.psignrank(r.v, r.n), r.alt, 1e-12, 1e-300, `psignrank(${r.v}, ${r.n})`);
    yakin(D.psignrank(r.v, r.n, false), r.ust, 1e-12, 1e-300, `psignrank üst(${r.v}, ${r.n})`);
  }
  for (const r of R.wilcox) {
    yakin(D.pwilcox(r.u, r.m, r.n), r.alt, 1e-12, 1e-300, `pwilcox(${r.u}, ${r.m}, ${r.n})`);
    yakin(D.pwilcox(r.u, r.m, r.n, false), r.ust, 1e-12, 1e-300, `pwilcox üst(${r.u}, ${r.m}, ${r.n})`);
  }
});

test('gamma ve beta fonksiyonları', () => {
  for (const r of R.gamma) {
    yakin(lgamma(r.x), r.lg, 1e-13, 1e-14, `lgamma(${r.x})`);
    yakin(digamma(r.x), r.dg, 1e-12, 1e-12, `digamma(${r.x})`);
  }
  for (const r of R.lbeta) yakin(lbeta(r.a, r.b), r.lb, 1e-12, 1e-13, `lbeta(${r.a}, ${r.b})`);
});
