// Faktör analizi: açımlayıcı (KMO, Bartlett, paralel analiz, döndürme) ve doğrulayıcı (uyum indeksleri, yükler).

import { IstatistikHatasi } from '../../cekirdek/hata.js';
import { faktorAnalizi as afaHesapla, kmoBartlett } from '../../cekirdek/faktor.js';
import { dogrulayiciFaktorAnalizi } from '../../cekirdek/dfa.js';
import { yamacGrafigi } from '../../grafik/grafikler.js';
import * as F from '../../rapor/bicim.js';
import { hucre } from '../../rapor/tablo.js';
import { adim } from '../kurallar.js';
import { degiskenBul, gorunenAd, tamVakalar } from '../veri-erisim.js';
import { grafikSecenek, pHucresi, uyariBloklari } from './ortak.js';
import { sadelestir } from '../../veri/tanima.js';

const YUK_ESIGI = 0.3;

export function faktorAnalizi(secim, vs, baglam) {
  const adlar = [...new Set(secim.degiskenler || [])];
  if (adlar.length < 3) throw new IstatistikHatasi('Faktör analizi için en az üç madde seçin.');
  const etiketler = adlar.map((a) => gorunenAd(degiskenBul(vs, a)));
  const tum = tamVakalar(vs, adlar, baglam);
  if (tum.matris.length < adlar.length + 2) throw new IstatistikHatasi(`Tam veri olan katılımcı sayısı (${tum.matris.length}) madde sayısına göre çok az.`);
  const uyarilar = [];
  if (tum.dislanan) uyarilar.push(`${tum.dislanan} katılımcı en az bir maddeyi yanıtlamadığı için dışarıda bırakıldı.`);
  if (tum.matris.length < 5 * adlar.length) uyarilar.push(`Katılımcı sayısı (${tum.matris.length}) madde sayısının 5 katından az; faktör çözümü kararsız olabilir (önerilen en az 5–10 katı ya da ≥ 200).`);
  return secim.tur === 'dfa' ? dfa(secim, adlar, etiketler, tum.matris, baglam, uyarilar) : afa(secim, adlar, etiketler, tum.matris, baglam, uyarilar);
}

function afa(secim, adlar, etiketler, X, baglam, uyarilar) {
  const { bicim: b } = baglam;
  const n = X.length;
  const bloklar = [];
  const kb = kmoBartlett(X);
  const fs = secim.faktorSayisi && /^\d+$/.test(String(secim.faktorSayisi)) ? Number(secim.faktorSayisi) : secim.faktorSayisi || 'kaiser';
  const r = afaHesapla(X, { cikarim: secim.cikarim || 'tbt', faktorSayisi: fs, dondurme: secim.dondurme || 'varimax', degiskenAdlari: etiketler });
  (kb.uyarilar || []).forEach((u) => uyarilar.push(u));
  (r.uyarilar || []).forEach((u) => uyarilar.push(u));
  const m = r.faktorSayisi;
  const adimlar = [
    adim('Tasarım', `${n} katılımcı, ${adlar.length} madde.`),
    adim('Uygunluk', `KMO = ${F.sayi(kb.kmo, 3, b)} (${kb.kmoYorum}); Bartlett küresellik testi χ²(${kb.sd}) = ${F.sayi(kb.istatistik, 2, b)}, ${F.pEsitlik(kb.p, b)}.${kb.kmo < 0.6 ? ' KMO 0,60\'ın altında: veri faktör analizi için uygun değil.' : ''}`, kb.kmo >= 0.6 && kb.p < 0.05 ? 'tamam' : 'uyari'),
    adim('Yöntem', `${r.cikarimAdi}; faktör sayısı ${fs === 'kaiser' ? 'özdeğeri 1\'den büyük faktörlere' : fs === 'paralel' ? 'paralel analize (Horn, %95 yüzdelik)' : 'kullanıcı seçimine'} göre ${m}; döndürme: ${r.dondurmeAdi}${r.kaiserNormalizasyonu && r.dondurme !== 'yok' ? ' (Kaiser normalizasyonu)' : ''}.`),
  ];
  const zayifMsa = kb.msa.map((v, i) => [etiketler[i], v]).filter(([, v]) => v < 0.5);
  if (zayifMsa.length) adimlar.push(adim('Madde uygunluğu', `MSA değeri 0,50'nin altında olan maddeler: ${zayifMsa.map(([a, v]) => `${a} (${F.sayi(v, 2, b)})`).join(', ')}; çıkarılması düşünülebilir.`, 'uyari'));
  bloklar.push({ tur: 'karar', adimlar });

  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'KMO ve Bartlett küresellik testi',
      basliklar: [[hucre('Ölçüt', { sol: true }), 'Değer']],
      satirlar: [
        { hucreler: [hucre('Kaiser-Meyer-Olkin örneklem yeterliliği', { sol: true }), `${F.sayi(kb.kmo, 3, b)} (${kb.kmoYorum})`] },
        { hucreler: [hucre('Bartlett testi χ²', { sol: true }), F.sayi(kb.istatistik, 2, b)] },
        { hucreler: [hucre('sd', { sol: true }), String(kb.sd)] },
        { hucreler: [hucre('p', { sol: true }), F.p(kb.p, b)] },
      ],
      dipnotlar: [],
    },
  });

  const gosterilen = r.toplamVaryans.slice(0, Math.max(m, Math.min(r.toplamVaryans.length, m + 3)));
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Açıklanan toplam varyans',
      basliklar: [
        [hucre('Faktör', { sol: true, satir: 2 }), hucre('Başlangıç özdeğerleri', { sutun: 3 }), hucre('Çıkarım kareler toplamı', { sutun: 3 }), hucre('Döndürme kareler toplamı', { sutun: r.egik ? 1 : 3 })],
        ['Toplam', '% varyans', 'Birikimli %', 'Toplam', '% varyans', 'Birikimli %', ...(r.egik ? ['Toplam'] : ['Toplam', '% varyans', 'Birikimli %'])],
      ],
      satirlar: gosterilen.map((t, i) => {
        const c = r.cikarimKareToplamlari[i];
        const d = r.donmeKareToplamlari?.[i];
        return {
          hucreler: [
            hucre(String(t.bilesen), { sol: true }),
            F.sayi(t.ozdeger, 3, b),
            F.sayi(t.varyansYuzdesi, 2, b),
            F.sayi(t.birikimliYuzde, 2, b),
            c ? F.sayi(c.toplam, 3, b) : '',
            c ? F.sayi(c.varyansYuzdesi, 2, b) : '',
            c ? F.sayi(c.birikimliYuzde, 2, b) : '',
            ...(r.egik ? [d ? F.sayi(d.toplam, 3, b) : ''] : [d ? F.sayi(d.toplam, 3, b) : '', d ? F.sayi(d.varyansYuzdesi, 2, b) : '', d ? F.sayi(d.birikimliYuzde, 2, b) : '']),
          ],
        };
      }),
      not: `${m} faktör, toplam varyansın %${F.sayi(r.cikarimKareToplamlari[m - 1]?.birikimliYuzde, 1, b)}'ini açıklıyor.${r.egik ? ' Eğik döndürmede faktörler ilişkili olduğundan döndürme sonrası kareler toplamı toplam varyansa eklenemez.' : ''}`,
      dipnotlar: [],
    },
  });

  if (r.paralelAnaliz) {
    const pa = r.paralelAnaliz;
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Paralel analiz',
        basliklar: [[hucre('Faktör', { sol: true }), 'Gerçek özdeğer', 'Rastgele ortalama', `Rastgele %${pa.yuzdelik} yüzdelik`, 'Karar']],
        satirlar: pa.gercek.slice(0, Math.min(pa.gercek.length, m + 3)).map((g, i) => ({ hucreler: [hucre(String(i + 1), { sol: true }), F.sayi(g, 3, b), F.sayi(pa.ortalama[i], 3, b), F.sayi(pa.yuzdelikDegerleri[i], 3, b), g > pa.yuzdelikDegerleri[i] ? 'Faktör' : '–'] })),
        not: `${pa.tekrar} rastgele veri seti; gerçek özdeğeri rastgele %${pa.yuzdelik} yüzdeliğinden büyük olan faktörler tutulur (önerilen: ${pa.onerilenFaktorSayisi}).`,
        dipnotlar: [],
      },
    });
  }

  // Yük tablosu: maddeleri en yüksek yüklendiği faktöre göre sırala
  const yukler = r.yukler;
  const sira = adlar.map((_, i) => i).sort((a, c) => {
    const fa = yukler[a].reduce((bi, v, j, d) => (Math.abs(v) > Math.abs(d[bi]) ? j : bi), 0);
    const fc = yukler[c].reduce((bi, v, j, d) => (Math.abs(v) > Math.abs(d[bi]) ? j : bi), 0);
    return fa - fc || Math.abs(yukler[c][fc]) - Math.abs(yukler[a][fa]);
  });
  const capraz = [];
  const yuksuz = [];
  for (let i = 0; i < adlar.length; i++) {
    const buyuk = yukler[i].map(Math.abs).filter((v) => v >= YUK_ESIGI).sort((x, y) => y - x);
    if (!buyuk.length) yuksuz.push(etiketler[i]);
    else if (buyuk.length > 1 && buyuk[0] - buyuk[1] < 0.1) capraz.push(etiketler[i]);
  }
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: `${r.dondurme === 'yok' ? 'Faktör yükleri' : r.egik ? 'Örüntü (pattern) matrisi' : 'Döndürülmüş faktör yükleri'}`,
      basliklar: [[hucre('Madde', { sol: true }), ...Array.from({ length: m }, (_, j) => `Faktör ${j + 1}`), 'Ortak varyans']],
      satirlar: sira.map((i) => ({
        hucreler: [hucre(etiketler[i], { sol: true }), ...yukler[i].map((v) => (Math.abs(v) >= YUK_ESIGI ? hucre(F.sayi(v, 3, b), { kalin: Math.abs(v) === Math.max(...yukler[i].map(Math.abs)) }) : '')), F.sayi(r.ortakliklar.cikarim[i], 3, b)],
      })),
      not: `Mutlak değeri ${F.sayi(YUK_ESIGI, 2, b)}'un altındaki yükler gösterilmemiştir; her maddenin en yüksek yükü kalındır. Artık korelasyonların %${F.sayi(r.artiklar.yuzde, 1, b)}'i 0,05'ten büyük (RMSR = ${F.sayi(r.artiklar.rmsr, 3, b)}).`,
      dipnotlar: [],
    },
  });
  if (capraz.length) uyarilar.push(`Binişik (iki faktöre yakın yüklenen) maddeler: ${capraz.join(', ')}. Yük farkı 0,10'dan küçük.`);
  if (yuksuz.length) uyarilar.push(`Hiçbir faktöre ${YUK_ESIGI.toString().replace('.', ',')} ve üzerinde yüklenmeyen maddeler: ${yuksuz.join(', ')}.`);
  const dusukOrtak = r.ortakliklar.cikarim.map((v, i) => [etiketler[i], v]).filter(([, v]) => v < 0.3);
  if (dusukOrtak.length) uyarilar.push(`Ortak varyansı 0,30'un altında olan maddeler: ${dusukOrtak.map(([a]) => a).join(', ')}.`);
  if (r.egik && m > 1) {
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'Faktörler arası korelasyonlar',
        basliklar: [[hucre('Faktör', { sol: true }), ...Array.from({ length: m }, (_, j) => String(j + 1))]],
        satirlar: r.faktorKorelasyonlari.map((s, i) => ({ hucreler: [hucre(String(i + 1), { sol: true }), ...s.map((v, j) => (j <= i ? F.sayi(v, 3, b) : ''))] })),
        dipnotlar: [],
      },
    });
  }
  if (secim.grafik !== false) {
    bloklar.push({ tur: 'grafik', baslik: 'Yamaç grafiği (scree plot)', svg: yamacGrafigi(r.ozdegerler, { ...grafikSecenek(baglam), paralelAnaliz: r.paralelAnaliz?.yuzdelikDegerleri, xEtiket: 'Faktör', yEtiket: 'Özdeğer' }), dosyaAdi: 'yamac-grafigi' });
  }
  bloklar.push({ tur: 'satir', metin: `KMO = ${F.sayi(kb.kmo, 3, b)}; Bartlett χ²(${kb.sd}) = ${F.sayi(kb.istatistik, 2, b)}, ${F.pEsitlik(kb.p, b)}; ${m} faktör toplam varyansın %${F.sayi(r.cikarimKareToplamlari[m - 1]?.birikimliYuzde, 1, b)}'ini açıklıyor.` });
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `Açımlayıcı faktör analizi (${adlar.length} madde)`, bloklar, yontemler: ['aciklayici-faktor'] };
}

/** "F1: S1, S2; F2: S3, S4" → {F1: [...], F2: [...]} (değişken adları/etiketleriyle eşleştirilir) */
export function modelCoz(metin, adlar, etiketler) {
  const model = {};
  const bul = (m) => {
    const s = sadelestir(m);
    const i = adlar.findIndex((a, k) => sadelestir(a) === s || sadelestir(etiketler[k]) === s);
    if (i < 0) throw new IstatistikHatasi(`Modeldeki "${m}" maddesi seçilen maddeler arasında yok.`);
    return adlar[i];
  };
  for (const parca of String(metin).split(/[;\n]+/)) {
    if (!parca.trim()) continue;
    const [ad, maddeler] = parca.split(':');
    if (!maddeler) throw new IstatistikHatasi(`"${parca.trim()}" satırında ":" yok. Biçim: Faktör: madde1, madde2`);
    const liste = maddeler.split(/[,\s]+/).filter(Boolean).map(bul);
    if (liste.length < 2) throw new IstatistikHatasi(`"${ad.trim()}" faktöründe en az iki madde olmalı.`);
    model[ad.trim()] = liste;
  }
  if (!Object.keys(model).length) throw new IstatistikHatasi('Model boş.');
  return model;
}

function dfa(secim, adlar, etiketler, X, baglam, uyarilar) {
  const { bicim: b, ayarlar } = baglam;
  const model = modelCoz(secim.model, adlar, etiketler);
  const kullanilan = [...new Set(Object.values(model).flat())];
  const sutunlar = kullanilan.map((a) => adlar.indexOf(a));
  const veri = X.map((s) => sutunlar.map((j) => s[j]));
  const adMap = Object.fromEntries(kullanilan.map((a) => [a, etiketler[adlar.indexOf(a)]]));
  const r = dogrulayiciFaktorAnalizi({ veri, degiskenAdlari: kullanilan, model, kestirim: 'ml' });
  (r.uyarilar || []).forEach((u) => uyarilar.push(u));
  const u = r.uyum;
  const k = (v, iyi, kabul, yon = '>=') => {
    if (!Number.isFinite(v)) return '–';
    const sagla = (e) => (yon === '>=' ? v >= e : v <= e);
    return sagla(iyi) ? 'iyi' : sagla(kabul) ? 'kabul edilebilir' : 'zayıf';
  };
  const adimlar = [
    adim('Tasarım', `${veri.length} katılımcı; ${Object.keys(model).length} faktör, ${kullanilan.length} madde (${Object.entries(model).map(([f, m]) => `${f}: ${m.length} madde`).join('; ')}).`),
    adim('Yöntem', 'En çok olabilirlik (ML) kestirimi; her faktörün ilk maddesinin yükü 1\'e sabitlendi (işaretçi değişken). Uyum Hu ve Bentler (1999) ölçütlerine göre değerlendirildi.'),
  ];
  const bloklar = [{ tur: 'karar', adimlar }];
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Model uyum indeksleri',
      basliklar: [[hucre('İndeks', { sol: true }), 'Değer', 'İyi uyum', 'Kabul edilebilir', 'Değerlendirme']],
      satirlar: [
        ['χ² (sd), p', `${F.sayi(u.kiKare, 2, b)} (${u.sd}), ${F.p(u.p, b)}`, 'p > 0,05', '–', u.p > 0.05 ? 'iyi' : 'büyük örneklemde beklenir'],
        ['χ²/sd', F.sayi(u.kiKareSd, 2, b), '≤ 3', '≤ 5', k(u.kiKareSd, 3, 5, '<=')],
        ['CFI', F.sayi(u.CFI, 3, b), '≥ 0,95', '≥ 0,90', k(u.CFI, 0.95, 0.9)],
        ['TLI', F.sayi(u.TLI, 3, b), '≥ 0,95', '≥ 0,90', k(u.TLI, 0.95, 0.9)],
        ['RMSEA (%90 GA)', `${F.sayi(u.RMSEA, 3, b)} (${F.ga(u.rmseaAlt, u.rmseaUst, 3, b)})`, '≤ 0,06', '≤ 0,08', k(u.RMSEA, 0.06, 0.08, '<=')],
        ['SRMR', F.sayi(u.SRMR, 3, b), '≤ 0,08', '≤ 0,10', k(u.SRMR, 0.08, 0.1, '<=')],
        ['GFI', F.sayi(u.GFI, 3, b), '≥ 0,95', '≥ 0,90', k(u.GFI, 0.95, 0.9)],
        ['AGFI', F.sayi(u.AGFI, 3, b), '≥ 0,90', '≥ 0,85', k(u.AGFI, 0.9, 0.85)],
        ['NFI', F.sayi(u.NFI, 3, b), '≥ 0,95', '≥ 0,90', k(u.NFI, 0.95, 0.9)],
        ['AIC / BIC', `${F.sayi(u.AIC, 1, b)} / ${F.sayi(u.BIC, 1, b)}`, 'karşılaştırmalı', '–', '–'],
      ].map((s) => ({ hucreler: [hucre(s[0], { sol: true }), ...s.slice(1)] })),
      not: 'Sınırlar: Hu ve Bentler (1999), Schermelleh-Engel ve ark. (2003). Uyum indeksleri birlikte değerlendirilmelidir.',
      dipnotlar: [],
    },
  });
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Faktör yükleri',
      basliklar: [[hucre('Faktör', { sol: true }), hucre('Madde', { sol: true }), 'Standart olmayan yük (SH)', 'Standart yük (λ)', 'z', 'p', 'R²']],
      satirlar: r.yukler.map((y) => ({
        hucreler: [hucre(y.faktor, { sol: true }), hucre(adMap[y.degisken] ?? y.degisken, { sol: true }), y.sabit ? `${F.sayi(y.tahmin, 3, b)} (sabit)` : `${F.sayi(y.tahmin, 3, b)} (${F.sayi(y.SH, 3, b)})`, hucre(F.sayi(y.std, 3, b), { anlamli: Math.abs(y.std) < 0.5 }), y.sabit ? '–' : F.sayi(y.z, 2, b), y.sabit ? '–' : pHucresi(y.p, ayarlar.alfa, b), F.sayi(r.R2.find((x) => x.degisken === y.degisken)?.deger, 3, b)],
      })),
      not: 'Standart yükü 0,50\'nin altında olanlar kalın. İşaretçi maddelerin yükü 1\'e sabitlendiği için bu maddelerde z ve p hesaplanmaz.',
      dipnotlar: [],
    },
  });
  bloklar.push({
    tur: 'tablo',
    tablo: {
      baslik: 'Birleşik güvenirlik ve açıklanan ortalama varyans',
      basliklar: [[hucre('Faktör', { sol: true }), 'Birleşik güvenirlik (CR)', 'Açıklanan ortalama varyans (AVE)', '√AVE']],
      satirlar: r.guvenirlik.map((g) => ({ hucreler: [hucre(g.faktor, { sol: true }), F.sayi(g.birlesikGuvenirlik, 3, b), F.sayi(g.ortalamaAciklananVaryans, 3, b), F.sayi(Math.sqrt(g.ortalamaAciklananVaryans), 3, b)] })),
      not: `CR ≥ 0,70 ve AVE ≥ 0,50 önerilir (Fornell ve Larcker, 1981). Ayırt edici geçerlik için her faktörün √AVE değeri, diğer faktörlerle korelasyonundan büyük olmalıdır.${r.faktorler.length > 1 ? ` Faktör korelasyonları: ${r.faktorKovaryanslari.map((c) => `${c.faktor1}–${c.faktor2} r = ${F.sayi(c.std, 3, b)}`).join('; ')}.` : ''}`,
      dipnotlar: [],
    },
  });
  if (r.modifikasyonIndeksleri?.length) {
    bloklar.push({
      tur: 'tablo',
      tablo: {
        baslik: 'En büyük modifikasyon indeksleri',
        basliklar: [[hucre('Öneri', { sol: true }), 'MI', 'Beklenen değişim (std)']],
        satirlar: r.modifikasyonIndeksleri.slice(0, 10).map((x) => ({ hucreler: [hucre(`${adMap[x.sol] ?? x.sol} ${x.op === '~~' ? '↔' : '→'} ${adMap[x.sag] ?? x.sag}${x.op === '~~' ? ' (hata kovaryansı)' : ' (çapraz yük)'}`, { sol: true }), F.sayi(x.mi, 2, b), F.sayi(x.stdBdd, 3, b)] })),
        not: 'Modifikasyonlar yalnızca kuramsal gerekçe varsa (ör. benzer ifadeli maddeler) eklenmelidir.',
        dipnotlar: [],
      },
    });
  }
  bloklar.push({ tur: 'satir', metin: `χ²(${u.sd}) = ${F.sayi(u.kiKare, 2, b)}, ${F.pEsitlik(u.p, b)}; χ²/sd = ${F.sayi(u.kiKareSd, 2, b)}; CFI = ${F.sayi(u.CFI, 3, b)}; TLI = ${F.sayi(u.TLI, 3, b)}; RMSEA = ${F.sayi(u.RMSEA, 3, b)} (%90 GA: ${F.ga(u.rmseaAlt, u.rmseaUst, 3, b)}); SRMR = ${F.sayi(u.SRMR, 3, b)}` });
  bloklar.push(...uyariBloklari(uyarilar));
  return { baslik: `Doğrulayıcı faktör analizi (${Object.keys(model).length} faktör)`, bloklar, yontemler: ['dogrulayici-faktor'] };
}
