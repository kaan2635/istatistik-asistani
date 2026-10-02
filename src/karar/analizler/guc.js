// Güç analizi ve örneklem büyüklüğü hesabı (G*Power / pwr ile aynı yöntemler).

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import * as G from '../../cekirdek/guc.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';

/** Test türleri: form alanları ve hesap fonksiyonu */
export const GUC_TESTLERI = {
  'iki-bagimsiz-t': {
    ad: 'İki bağımsız grubun ortalaması (t testi)',
    etki: { anahtar: 'd', ad: 'Cohen d', ornek: '0,5 (orta)', yardimci: 'ortalamalar' },
    ekAlanlar: [{ anahtar: 'oran', ad: 'Grup oranı (n2/n1)', varsayilan: '1' }],
    hesap: (g) => G.tTestGuc({ tip: 'iki', d: g.d, n: g.n, guc: g.guc, alfa: g.alfa, alternatif: g.alternatif, oran: g.oran ?? 1 }),
    grupBasina: true,
  },
  'eslestirilmis-t': {
    ad: 'Eşleştirilmiş ölçümler (önce–sonra, eşleştirilmiş t)',
    etki: { anahtar: 'd', ad: 'Cohen d_z (fark ortalaması / fark SS)', ornek: '0,5' },
    hesap: (g) => G.tTestGuc({ tip: 'eslestirilmis', d: g.d, n: g.n, guc: g.guc, alfa: g.alfa, alternatif: g.alternatif }),
  },
  'tek-orneklem-t': {
    ad: 'Tek örneklem ortalaması (t testi)',
    etki: { anahtar: 'd', ad: 'Cohen d', ornek: '0,5' },
    hesap: (g) => G.tTestGuc({ tip: 'tek', d: g.d, n: g.n, guc: g.guc, alfa: g.alfa, alternatif: g.alternatif }),
  },
  'mann-whitney': {
    ad: 'İki bağımsız grup, parametrik olmayan (Mann-Whitney U)',
    etki: { anahtar: 'd', ad: 'Cohen d', ornek: '0,5', yardimci: 'ortalamalar' },
    ekAlanlar: [{ anahtar: 'oran', ad: 'Grup oranı (n2/n1)', varsayilan: '1' }],
    hesap: (g) => G.mannWhitneyGuc({ d: g.d, n: g.n, guc: g.guc, alfa: g.alfa, alternatif: g.alternatif, oran: g.oran ?? 1 }),
    grupBasina: true,
    not: 'A.R.E. yöntemi (normal dağılım varsayımıyla, G*Power ile aynı).',
  },
  'wilcoxon': {
    ad: 'Eşleştirilmiş, parametrik olmayan (Wilcoxon)',
    etki: { anahtar: 'd', ad: 'Cohen d_z', ornek: '0,5' },
    hesap: (g) => G.wilcoxonGuc({ d: g.d, n: g.n, guc: g.guc, alfa: g.alfa, alternatif: g.alternatif }),
    not: 'A.R.E. yöntemi.',
  },
  anova: {
    ad: 'İkiden çok grubun ortalaması (tek yönlü ANOVA)',
    etki: { anahtar: 'f', ad: 'Cohen f', ornek: '0,25 (orta)', yardimci: 'eta' },
    ekAlanlar: [{ anahtar: 'k', ad: 'Grup sayısı', varsayilan: '3' }],
    hesap: (g) => G.anovaGuc({ k: g.k, f: g.f, n: g.n, guc: g.guc, alfa: g.alfa }),
    grupBasina: true,
  },
  'iki-oran': {
    ad: 'İki bağımsız oran (ki-kare / z testi)',
    etki: { anahtar: 'p2', ad: 'Grup 2 oranı (p2)', ornek: '0,30' },
    ekAlanlar: [
      { anahtar: 'p1', ad: 'Grup 1 oranı (p1)', varsayilan: '0,5' },
      { anahtar: 'oran', ad: 'Grup oranı (n2/n1)', varsayilan: '1' },
    ],
    hesap: (g) => G.ikiOranGuc({ p1: g.p1, p2: g.p2, n: g.n, oran: g.oran ?? 1, guc: g.guc, alfa: g.alfa, alternatif: g.alternatif, yontem: 'normal' }),
    grupBasina: true,
    not: 'Havuzlanmış varyanslı normal yaklaşım (Fleiss; R power.prop.test ile aynı).',
  },
  'tek-oran': {
    ad: 'Tek oranın beklenen değerle karşılaştırılması',
    etki: { anahtar: 'p', ad: 'Beklenen oran (p)', ornek: '0,65' },
    ekAlanlar: [{ anahtar: 'p0', ad: 'Karşılaştırılan oran (p0)', varsayilan: '0,5' }],
    hesap: (g) => G.tekOranGuc({ p: g.p, p0: g.p0, n: g.n, guc: g.guc, alfa: g.alfa, alternatif: g.alternatif }),
  },
  korelasyon: {
    ad: 'Korelasyon katsayısı',
    etki: { anahtar: 'r', ad: 'Beklenen korelasyon (r)', ornek: '0,3 (orta)' },
    hesap: (g) => G.korelasyonGuc({ r: g.r, n: g.n, guc: g.guc, alfa: g.alfa, alternatif: g.alternatif }),
  },
  'ki-kare': {
    ad: 'Ki-kare (r×c tablo, uyum iyiliği)',
    etki: { anahtar: 'w', ad: 'Cohen w', ornek: '0,3 (orta)' },
    ekAlanlar: [{ anahtar: 'df', ad: 'Serbestlik derecesi ((r−1)(c−1))', varsayilan: '1' }],
    hesap: (g) => G.kiKareGuc({ w: g.w, df: g.df, N: g.n, guc: g.guc, alfa: g.alfa }),
    yonYok: true,
  },
  regresyon: {
    ad: 'Çoklu doğrusal regresyon (R²)',
    etki: { anahtar: 'f2', ad: 'Cohen f²', ornek: '0,15 (orta)', yardimci: 'r2' },
    ekAlanlar: [{ anahtar: 'u', ad: 'Bağımsız değişken sayısı', varsayilan: '3' }],
    hesap: (g) => G.regresyonGuc({ u: g.u, N: g.n, f2: g.f2, guc: g.guc, alfa: g.alfa }),
    yonYok: true,
  },
  lojistik: {
    ad: 'Lojistik regresyon (bir sürekli değişken)',
    etki: { anahtar: 'OR', ad: 'Bir SS artış için olasılık oranı (OR)', ornek: '1,5' },
    ekAlanlar: [
      { anahtar: 'p1', ad: 'Ortalamada olay olasılığı (p1)', varsayilan: '0,2' },
      { anahtar: 'r2', ad: 'Diğer değişkenlerle R² (yoksa 0)', varsayilan: '0' },
    ],
    hesap: (g) => G.lojistikGuc({ tip: 'surekli', p1: g.p1, OR: g.OR, r2: g.r2 ?? 0, n: g.n, guc: g.guc, alfa: g.alfa, alternatif: g.alternatif }),
    not: 'Hsieh ve ark. (1998) yöntemi.',
  },
  sagkalim: {
    ad: 'Sağkalım (log-rank / Cox)',
    etki: { anahtar: 'hr', ad: 'Tehlike oranı (HR)', ornek: '0,6' },
    ekAlanlar: [
      { anahtar: 'olayOlasiligi', ad: 'Çalışma sonunda olay olasılığı', varsayilan: '0,5' },
      { anahtar: 'oran', ad: 'Grup oranı (n2/n1)', varsayilan: '1' },
    ],
    hesap: (g) => G.sagkalimGuc({ hr: g.hr, N: g.n, olayOlasiligi: g.olayOlasiligi, oran: g.oran ?? 1, guc: g.guc, alfa: g.alfa, alternatif: g.alternatif }),
    not: 'Schoenfeld (1983) yöntemi; önce gerekli olay sayısı, sonra olay olasılığıyla toplam hasta sayısı hesaplanır.',
  },
};

function sayi(v) {
  if (v === null || v === undefined || String(v).trim() === '') return null;
  const x = Number(String(v).trim().replace(',', '.'));
  return Number.isFinite(x) ? x : NaN;
}

/** Form seçimlerinden hesap; secim: {test, hesap:'n'|'guc'|'etki', etki, n, guc, alfa, alternatif, kayip, ...ek} */
export function gucHesapla(secim) {
  const tanim = GUC_TESTLERI[secim.test];
  if (!tanim) throw new IstatistikHatasi('Test türünü seçin.');
  const g = { alfa: sayi(secim.alfa) ?? 0.05, alternatif: secim.alternatif || 'iki-yonlu' };
  for (const a of tanim.ekAlanlar || []) {
    const v = sayi(secim[a.anahtar] ?? a.varsayilan);
    if (Number.isNaN(v)) throw new IstatistikHatasi(`${a.ad} sayı olmalı.`);
    g[a.anahtar] = v;
  }
  const hesap = secim.hesap || 'n';
  g[tanim.etki.anahtar] = hesap === 'etki' ? null : sayi(secim.etki);
  g.n = hesap === 'n' ? null : sayi(secim.n);
  g.guc = hesap === 'guc' ? null : sayi(secim.guc) ?? 0.8;
  if (hesap !== 'etki' && !Number.isFinite(g[tanim.etki.anahtar])) throw new IstatistikHatasi(`${tanim.etki.ad} değerini girin.`);
  if (hesap !== 'n' && !(g.n > 0)) throw new IstatistikHatasi('Örneklem büyüklüğünü girin.');
  if (hesap !== 'guc' && !(g.guc > 0 && g.guc < 1)) throw new IstatistikHatasi('Güç 0 ile 1 arasında olmalı (ör. 0,80).');
  if (!(g.alfa > 0 && g.alfa < 1)) throw new IstatistikHatasi('α 0 ile 1 arasında olmalı.');
  const r = tanim.hesap(g);
  return { tanim, girdi: g, r, hesap, kayip: sayi(secim.kayip) || 0 };
}

const ALT_ADI = { 'iki-yonlu': 'iki yönlü', kucuk: 'tek yönlü', buyuk: 'tek yönlü' };

/** Rapor öğesi (Güç sekmesinden "Rapora ekle" ile) */
export function gucAnalizi(secim, _vs, baglam) {
  const b = baglam?.bicim ?? F.VARSAYILAN_BICIM;
  const { tanim, girdi, r, hesap, kayip } = gucHesapla(secim);
  // Çözülen etki: oran testlerinde etki alanı oranın kendisidir (p2, p); diğerlerinde etki büyüklüğü
  const anahtar = tanim.etki.anahtar;
  const cozulen = anahtar === 'p2' ? r.p2 : anahtar === 'p' ? r.p : r.etki?.deger;
  const etkiDegeri = hesap === 'etki' ? cozulen : girdi[anahtar];
  const satirlar = [];
  const ekle = (a, v) => satirlar.push({ hucreler: [hucre(a, { sol: true }), v] });
  ekle('Test', tanim.ad);
  if (!tanim.yonYok) ekle('Hipotez', ALT_ADI[girdi.alternatif] || girdi.alternatif);
  ekle('Anlamlılık düzeyi (α)', F.sayi(girdi.alfa, 3, b));
  for (const a of tanim.ekAlanlar || []) ekle(a.ad, F.uyarlanmis(girdi[a.anahtar], b));
  ekle(tanim.etki.ad + (hesap === 'etki' ? ' (hesaplanan)' : ''), F.sayi(etkiDegeri, 3, b));
  ekle(`Güç (1 − β)${hesap === 'guc' ? ' (hesaplanan)' : ''}`, F.sayi(hesap === 'n' ? r.gercekGuc ?? r.guc : r.guc, 3, b));
  let ozet;
  if (r.olaySayisi !== undefined) ekle('Gerekli olay sayısı', String(r.olaySayisi));
  if (tanim.grupBasina && (r.n1 !== undefined || r.n !== undefined)) {
    const n1 = r.n1 ?? r.n;
    const n2 = r.n2 ?? r.n;
    ekle(`Grup başına n${hesap === 'n' ? ' (hesaplanan)' : ''}`, r.n1 !== undefined && r.n1 !== r.n2 ? `${n1} ve ${n2}` : String(r.n));
    ekle('Toplam n', String(r.toplamN ?? n1 + n2));
    ozet = r.toplamN ?? n1 + n2;
  } else {
    const n = r.toplamN ?? r.n;
    ekle(`Örneklem büyüklüğü${hesap === 'n' ? ' (hesaplanan)' : ''}`, String(n));
    ozet = n;
  }
  if (kayip > 0 && hesap === 'n') ekle(`Kayıp payı %${F.sayi(kayip, 0, b)} ile toplam`, String(Math.ceil(ozet / (1 - kayip / 100))));
  const satirMetni =
    hesap === 'n'
      ? `${tanim.ad}: α = ${F.sayi(girdi.alfa, 2, b)}, güç = %${F.sayi(girdi.guc * 100, 0, b)}, ${tanim.etki.ad.split(' (')[0]} = ${F.uyarlanmis(girdi[tanim.etki.anahtar], b)} için toplam ${ozet} katılımcı gerekir${kayip > 0 ? ` (%${F.sayi(kayip, 0, b)} kayıp payıyla ${Math.ceil(ozet / (1 - kayip / 100))})` : ''}.`
      : hesap === 'guc'
        ? `${tanim.ad}: n = ${F.tam(girdi.n)} ve ${tanim.etki.ad.split(' (')[0]} = ${F.uyarlanmis(girdi[tanim.etki.anahtar], b)} için güç = %${F.sayi(r.guc * 100, 1, b)}.`
        : `${tanim.ad}: n = ${F.tam(girdi.n)} ve güç = %${F.sayi(girdi.guc * 100, 0, b)} ile saptanabilir en küçük etki ${tanim.etki.ad.split(' (')[0]} = ${F.sayi(etkiDegeri, 3, b)}.`;
  return {
    baslik: `Güç analizi: ${tanim.ad}`,
    bloklar: [
      { tur: 'tablo', tablo: { baslik: 'Güç analizi / örneklem büyüklüğü', basliklar: [[hucre('Parametre', { sol: true }), 'Değer']], satirlar, not: `${tanim.not ? `${tanim.not} ` : ''}Hesaplar R pwr paketi ve G*Power ile aynı yöntemlerle yapılmıştır.`, dipnotlar: [] } },
      { tur: 'satir', metin: satirMetni },
      ...(r.uyarilar || []).map((u) => ({ tur: 'not', metin: u, seviye: 'uyari' })),
    ],
    yontemler: [],
  };
}

export { G as gucFonksiyonlari };
