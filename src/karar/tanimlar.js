// Analiz türlerinin tanımları: kart metinleri, form alanları ve doğrulama kuralları.
// Hesaplama src/karar/analizler/ altındaki modüllerdedir.

import { duzeyleriBul } from '../veri/tanima.js';

const SKO = ['surekli', 'sirali', 'kategorik'];

function duzeySayisi(durum, ad) {
  const d = durum.veriSeti.degiskenler.find((x) => x.ad === ad);
  return d ? duzeyleriBul(d).length : 0;
}

export const ANALIZ_TANIMLARI = [
  {
    id: 'grup',
    baslik: 'Grupları karşılaştır',
    simge: 'analiz',
    kisa: 'Bağımsız gruplar arasında fark var mı? (t testi, Mann-Whitney U, ANOVA, Kruskal-Wallis, ki-kare, Fisher)',
    ornek: '"Hasta ve kontrol gruplarında yaş, cinsiyet ve CRP farklı mı?"',
    aciklama: 'Her değişken için grupların normal dağılıp dağılmadığı ve varyansların eşitliği sınanır; uygun test seçilir. Sonuç, makalelerdeki "Tablo 1" düzeninde tek tabloda ve ayrıntılı olarak verilir.',
    alanlar: [
      { ad: 'grup', tur: 'tek', etiket: 'Grup değişkeni', turler: ['kategorik', 'sirali'], aciklama: '2–10 kategorili (ör. Grup, Cinsiyet, Evre).' },
      { ad: 'degiskenler', tur: 'cok', etiket: 'Karşılaştırılacak değişkenler', turler: SKO, haric: ['grup'] },
      { ad: 'grafik', tur: 'onay', etiket: 'Grafik ekle', varsayilan: true },
    ],
    dogrula: (s, durum) => {
      if (!s.grup) return 'Grup değişkenini seçin.';
      const k = duzeySayisi(durum, s.grup);
      if (k < 2) return 'Grup değişkeninin en az iki kategorisi olmalı.';
      if (k > 10) return `Grup değişkeninin ${k} kategorisi var; en fazla 10 desteklenir. Gerekirse kategorileri birleştirin.`;
      if (!s.degiskenler?.length) return 'En az bir değişken seçin.';
      return null;
    },
  },
  {
    id: 'eslestirilmis',
    baslik: 'Önce–sonra / tekrarlı ölçüm',
    simge: 'yenile',
    kisa: 'Aynı kişilerde zaman ya da koşula göre değişim var mı? (eşleştirilmiş t, Wilcoxon, tekrarlı ölçümler ANOVA, Friedman, McNemar, Cochran Q)',
    ornek: '"Tedavi öncesi, 1. ay ve 6. ayda ağrı puanı değişti mi?"',
    aciklama: 'Aynı deneklerin farklı zamanlarda ölçümlerini sırasıyla seçin. İsterseniz gruplar arasında değişimin farklı olup olmadığını da sınayabilirsiniz (karma desen).',
    alanlar: [
      { ad: 'degiskenler', tur: 'cok', sirali: true, etiket: 'Ölçümler (zaman sırasıyla)', turler: SKO, aciklama: 'En az iki ölçüm; hepsi aynı türde olmalı.' },
      { ad: 'grup', tur: 'tek', istegeBagli: true, bosMetin: '(grup yok)', etiket: 'Gruplar arası etken (isteğe bağlı)', turler: ['kategorik', 'sirali'], haric: ['degiskenler'] },
      { ad: 'grafik', tur: 'onay', etiket: 'Grafik ekle', varsayilan: true },
    ],
    dogrula: (s, durum) => {
      if (!s.degiskenler || s.degiskenler.length < 2) return 'En az iki ölçüm seçin.';
      const turler = new Set(s.degiskenler.map((a) => (durum.olcumler[a] === 'sirali' ? 'surekli' : durum.olcumler[a])));
      if (turler.size > 1) return 'Seçilen ölçümler aynı türde olmalı (hepsi sürekli ya da hepsi kategorik).';
      return null;
    },
  },
  {
    id: 'korelasyon',
    baslik: 'Korelasyon (ilişki)',
    simge: 'grafik',
    kisa: 'Sürekli ya da sıralı değişkenler birlikte artıp azalıyor mu? (Pearson, Spearman, Kendall, kısmi korelasyon)',
    ornek: '"Yaş ile sistolik kan basıncı arasında ilişki var mı?"',
    aciklama: 'İki ya da daha çok değişken seçin. Her çift için dağılıma göre Pearson ya da Spearman seçilir.',
    alanlar: [
      { ad: 'degiskenler', tur: 'cok', etiket: 'Değişkenler', turler: ['surekli', 'sirali'] },
      { ad: 'kontrol', tur: 'cok', etiket: 'Kontrol değişkenleri (kısmi korelasyon, isteğe bağlı)', turler: ['surekli', 'sirali'], haric: ['degiskenler'] },
      { ad: 'grafik', tur: 'onay', etiket: 'Saçılım grafikleri ekle (en fazla 6 değişken)', varsayilan: true },
    ],
    dogrula: (s) => (!s.degiskenler || s.degiskenler.length < 2 ? 'En az iki değişken seçin.' : null),
  },
  {
    id: 'capraz',
    baslik: 'Çapraz tablo (kategorik ilişki)',
    simge: 'tablo',
    kisa: 'İki kategorik değişken ilişkili mi? (ki-kare, Fisher, Fisher-Freeman-Halton; OR ve RR)',
    ornek: '"Sigara içme ile hipertansiyon arasında ilişki var mı?"',
    aciklama: 'Beklenen değerlere göre ki-kare ya da kesin test seçilir. 2×2 tablolarda olasılık oranı (OR) ve göreli risk (RR) güven aralıklarıyla verilir.',
    alanlar: [
      { ad: 'satir', tur: 'tek', etiket: 'Satır değişkeni (ör. risk etkeni)', turler: ['kategorik', 'sirali'] },
      { ad: 'sutun', tur: 'tek', etiket: 'Sütun değişkeni (ör. sonuç / grup)', turler: ['kategorik', 'sirali'], haric: ['satir'] },
      { ad: 'katman', tur: 'tek', istegeBagli: true, bosMetin: '(katman yok)', etiket: 'Katman (Mantel-Haenszel, isteğe bağlı)', turler: ['kategorik', 'sirali'], haric: ['satir', 'sutun'] },
      { ad: 'grafik', tur: 'onay', etiket: 'Grafik ekle', varsayilan: true },
    ],
    dogrula: (s) => (!s.satir || !s.sutun ? 'Satır ve sütun değişkenlerini seçin.' : null),
  },
  {
    id: 'betimsel',
    baslik: 'Betimsel istatistikler',
    simge: 'tablo',
    kisa: 'Ortalama, standart sapma, medyan, çeyreklikler, frekans ve yüzdeler; normallik testleri.',
    ornek: '"Katılımcıların yaş ve cinsiyet dağılımı nedir?"',
    aciklama: 'Sürekli değişkenler için özet istatistikler ve normallik değerlendirmesi, kategorik değişkenler için frekans tabloları üretilir.',
    alanlar: [
      { ad: 'degiskenler', tur: 'cok', etiket: 'Değişkenler', turler: SKO },
      { ad: 'grafik', tur: 'onay', etiket: 'Histogram, Q-Q ve frekans grafikleri ekle', varsayilan: false },
    ],
    dogrula: (s) => (!s.degiskenler?.length ? 'En az bir değişken seçin.' : null),
  },
  {
    id: 'tekorneklem',
    baslik: 'Bilinen bir değerle karşılaştır',
    simge: 'analiz',
    kisa: 'Ortalama/medyan bir referans değerden ya da oran beklenen orandan farklı mı? (tek örneklem t, Wilcoxon, binom, uyum iyiliği)',
    ornek: '"Hastaların ortalama HbA1c değeri %7\'den farklı mı?"',
    aciklama: 'Sürekli değişkende normal dağılıma göre tek örneklem t testi ya da Wilcoxon testi; kategorik değişkende binom testi ya da ki-kare uyum iyiliği testi uygulanır.',
    alanlar: [
      { ad: 'degisken', tur: 'tek', etiket: 'Değişken', turler: SKO },
      { ad: 'deger', tur: 'sayi', etiket: 'Referans değer (sürekli değişken için)', yerTutucu: 'ör. 7', gorunur: (s, d) => s.degisken && d.olcumler[s.degisken] !== 'kategorik' },
      { ad: 'oranlar', tur: 'metin', etiket: 'Beklenen oranlar (kategorik değişken için, kategori sırasıyla)', yerTutucu: 'ör. 0,5; 0,5 (boş bırakılırsa eşit)', gorunur: (s, d) => s.degisken && d.olcumler[s.degisken] === 'kategorik' },
    ],
    dogrula: (s, durum) => {
      if (!s.degisken) return 'Değişkeni seçin.';
      if (durum.olcumler[s.degisken] !== 'kategorik' && !Number.isFinite(Number(String(s.deger).replace(',', '.')))) return 'Referans değeri sayı olarak girin.';
      return null;
    },
  },
  {
    id: 'regresyon',
    baslik: 'Regresyon (etkileyen faktörler)',
    simge: 'grafik',
    kisa: 'Bir sonucu bağımsız olarak etkileyen değişkenler hangileri? (doğrusal, ikili / sıralı / çok kategorili lojistik regresyon)',
    ornek: '"Yaş, cinsiyet ve CRP\'den hangileri mortaliteyi bağımsız olarak öngörüyor?"',
    aciklama: 'Model türü bağımlı değişkene göre seçilir: sürekli → doğrusal, iki kategorili → ikili lojistik, sıralı → ordinal lojistik, sırasız çok kategorili → multinomial lojistik regresyon. Tek değişkenli ve çok değişkenli sonuçlar yan yana verilir; varsayımlar (çoklu bağlantı, artıkların dağılımı, Hosmer-Lemeshow vb.) denetlenir.',
    alanlar: [
      { ad: 'bagimli', tur: 'tek', etiket: 'Bağımlı (sonuç) değişken', turler: SKO },
      { ad: 'olayDuzeyi', tur: 'duzey', kaynak: 'bagimli', etiket: 'Olay / ilgilenilen kategori', aciklama: 'Lojistik regresyonda modellenen kategori (ör. Ölüm = Var).', gorunur: (s, d) => s.bagimli && d.olcumler[s.bagimli] !== 'surekli' && duzeySayisi(d, s.bagimli) === 2 },
      { ad: 'bagimsizlar', tur: 'cok', etiket: 'Bağımsız değişkenler', turler: SKO, haric: ['bagimli'] },
      { ad: 'tarama', tur: 'secim', etiket: 'Çok değişkenli modele alınacaklar', secenekler: [['hepsi', 'Seçilenlerin hepsi'], ['0.2', 'Tek değişkenli analizde p < 0,20 olanlar'], ['0.25', 'Tek değişkenli analizde p < 0,25 olanlar'], ['0.1', 'Tek değişkenli analizde p < 0,10 olanlar']], varsayilan: 'hepsi' },
      { ad: 'yontem', tur: 'secim', etiket: 'Değişken seçimi', secenekler: [['gir', 'Hepsini birlikte gir (Enter)'], ['geri', 'Geriye doğru eleme'], ['ileri', 'İleriye doğru seçim']], varsayilan: 'gir', gorunur: (s, d) => s.bagimli && d.olcumler[s.bagimli] === 'surekli' },
      { ad: 'grafik', tur: 'onay', etiket: 'Grafik ekle (orman grafiği / artık grafikleri)', varsayilan: true },
    ],
    dogrula: (s, durum) => {
      if (!s.bagimli) return 'Bağımlı değişkeni seçin.';
      if (!s.bagimsizlar?.length) return 'En az bir bağımsız değişken seçin.';
      if (durum.olcumler[s.bagimli] !== 'surekli' && duzeySayisi(durum, s.bagimli) > 10) return "Bağımlı değişkenin 10'dan fazla kategorisi var; kategorileri birleştirin ya da sürekli olarak tanımlayın.";
      return null;
    },
  },
  {
    id: 'roc',
    baslik: 'ROC analizi / tanı testi',
    simge: 'grafik',
    kisa: 'Bir ölçüm hastalığı ne kadar iyi ayırt ediyor? En iyi kesim değeri? (AUC, DeLong, Youden, duyarlılık, özgüllük)',
    ornek: '"Prokalsitonin sepsisi ayırt etmede CRP\'den daha iyi mi?"',
    aciklama: 'Her ölçüm için ROC eğrisi, eğri altında kalan alan (AUC) ve güven aralığı, Youden indeksine göre en iyi kesim değeri ve bu değerdeki tanısal ölçütler hesaplanır. Birden çok ölçüm DeLong testiyle karşılaştırılır.',
    alanlar: [
      { ad: 'durum', tur: 'tek', etiket: 'Gerçek durum (altın standart)', turler: ['kategorik', 'sirali'] },
      { ad: 'pozitif', tur: 'duzey', kaynak: 'durum', etiket: 'Hasta / pozitif kategori' },
      { ad: 'belirtecler', tur: 'cok', etiket: 'Tanısal ölçümler (belirteçler)', turler: ['surekli', 'sirali'], haric: ['durum'] },
      { ad: 'grafik', tur: 'onay', etiket: 'ROC eğrisi çiz', varsayilan: true },
    ],
    dogrula: (s, durum) => {
      if (!s.durum) return 'Gerçek durum değişkenini seçin.';
      if (duzeySayisi(durum, s.durum) !== 2) return 'Gerçek durum değişkeni tam iki kategorili olmalı.';
      if (!s.belirtecler?.length) return 'En az bir ölçüm seçin.';
      return null;
    },
  },
  {
    id: 'tani',
    baslik: 'Tanı testi 2×2 (duyarlılık, özgüllük)',
    simge: 'tablo',
    kisa: 'İki kategorili bir testin altın standarda göre duyarlılığı, özgüllüğü, PPD, NPD ve olabilirlik oranları.',
    ornek: '"Hızlı test sonucunun PCR\'a göre duyarlılığı nedir?"',
    aciklama: 'Test sonucu ve altın standart iki kategorili olmalıdır. Tüm ölçütler güven aralıklarıyla verilir.',
    alanlar: [
      { ad: 'test', tur: 'tek', etiket: 'Test sonucu', turler: ['kategorik', 'sirali'] },
      { ad: 'testPozitif', tur: 'duzey', kaynak: 'test', etiket: 'Testte pozitif kategori' },
      { ad: 'referans', tur: 'tek', etiket: 'Altın standart (gerçek durum)', turler: ['kategorik', 'sirali'], haric: ['test'] },
      { ad: 'referansPozitif', tur: 'duzey', kaynak: 'referans', etiket: 'Altın standartta hasta kategori' },
      { ad: 'prevalans', tur: 'sayi', etiket: 'Toplumdaki prevalans (isteğe bağlı, %)', yerTutucu: 'boş: örneklemdeki oran' },
    ],
    dogrula: (s, durum) => {
      if (!s.test || !s.referans) return 'Test ve altın standart değişkenlerini seçin.';
      if (duzeySayisi(durum, s.test) !== 2 || duzeySayisi(durum, s.referans) !== 2) return 'Her iki değişken de tam iki kategorili olmalı.';
      return null;
    },
  },
  {
    id: 'sagkalim',
    baslik: 'Sağkalım analizi',
    simge: 'grafik',
    kisa: 'Olaya kadar geçen süre gruplar arasında farklı mı? (Kaplan-Meier, log-rank, Cox regresyonu)',
    ornek: '"Yeni tedavi alanlarda sağkalım daha uzun mu?"',
    aciklama: 'Kaplan-Meier eğrileri, medyan sağkalım ve güven aralıkları, belirli zamanlardaki sağkalım oranları, log-rank (ve Breslow, Tarone-Ware) testleri; isteğe bağlı olarak Cox regresyonu ve orantılı tehlikeler varsayımı.',
    alanlar: [
      { ad: 'sure', tur: 'tek', etiket: 'Süre (takip süresi)', turler: ['surekli'] },
      { ad: 'olay', tur: 'tek', etiket: 'Olay değişkeni (ölüm, nüks…)', turler: ['kategorik', 'sirali'] },
      { ad: 'olayDuzeyi', tur: 'duzey', kaynak: 'olay', etiket: 'Olayı gösteren kategori', aciklama: 'Diğer kategori(ler) sansürlü kabul edilir.' },
      { ad: 'grup', tur: 'tek', istegeBagli: true, bosMetin: '(grup yok)', etiket: 'Grup (isteğe bağlı)', turler: ['kategorik', 'sirali'], haric: ['olay'] },
      { ad: 'zamanlar', tur: 'metin', etiket: 'Sağkalım oranı istenen zamanlar (isteğe bağlı)', yerTutucu: 'ör. 12; 24; 60' },
      { ad: 'kovaryatlar', tur: 'cok', etiket: 'Cox regresyonu için değişkenler (isteğe bağlı)', turler: SKO, haric: ['sure', 'olay'] },
      { ad: 'grafik', tur: 'onay', etiket: 'Kaplan-Meier eğrisi çiz', varsayilan: true },
    ],
    dogrula: (s, durum) => {
      if (!s.sure || !s.olay) return 'Süre ve olay değişkenlerini seçin.';
      if (duzeySayisi(durum, s.olay) < 2) return 'Olay değişkeninde en az iki kategori olmalı.';
      return null;
    },
  },
  {
    id: 'uyum',
    baslik: 'Ölçüm uyumu (gözlemciler / yöntemler)',
    simge: 'tamam',
    kisa: 'İki gözlemci ya da iki yöntem aynı sonucu veriyor mu? (Kappa, ICC, Bland-Altman, Lin uyum katsayısı)',
    ornek: '"İki radyoloğun ölçümleri birbiriyle uyumlu mu?"',
    aciklama: 'Kategorik ölçümlerde Cohen (iki gözlemci) ya da Fleiss (üç ve daha çok) kappa; sıralı ölçümlerde ağırlıklı kappa; sürekli ölçümlerde sınıf içi korelasyon katsayısı (ICC), Bland-Altman analizi ve Lin uyum korelasyonu hesaplanır.',
    alanlar: [
      { ad: 'degiskenler', tur: 'cok', etiket: 'Gözlemciler / yöntemler (her biri bir sütun)', turler: SKO },
      { ad: 'iccModel', tur: 'secim', etiket: 'ICC modeli (sürekli ölçümler)', secenekler: [['iki-yonlu-rastgele', 'İki yönlü rastgele (gözlemciler genellenecekse)'], ['iki-yonlu-karma', 'İki yönlü karma (yalnızca bu gözlemciler)'], ['tek-yonlu', 'Tek yönlü rastgele']], varsayilan: 'iki-yonlu-rastgele' },
      { ad: 'iccTip', tur: 'secim', etiket: 'ICC türü', secenekler: [['uyum', 'Mutlak uyum'], ['tutarlilik', 'Tutarlılık']], varsayilan: 'uyum' },
      { ad: 'iccBirim', tur: 'secim', etiket: 'ICC birimi', secenekler: [['tek', 'Tek ölçüm'], ['ortalama', 'Ortalama ölçüm']], varsayilan: 'tek' },
      { ad: 'grafik', tur: 'onay', etiket: 'Bland-Altman grafiği', varsayilan: true },
    ],
    dogrula: (s, durum) => {
      if (!s.degiskenler || s.degiskenler.length < 2) return 'En az iki ölçüm sütunu seçin.';
      const t = new Set(s.degiskenler.map((a) => durum.olcumler[a]));
      if (t.size > 1) return 'Seçilen ölçümler aynı türde olmalı.';
      return null;
    },
  },
  {
    id: 'guvenirlik',
    baslik: 'Ölçek güvenirliği (Cronbach alfa)',
    simge: 'tamam',
    kisa: 'Ölçek maddeleri tutarlı mı? (Cronbach alfa, madde-toplam korelasyonu, madde silinirse alfa, iki yarı güvenirliği)',
    ornek: '"Uyarladığımız ölçeğin iç tutarlılığı yeterli mi?"',
    aciklama: 'Maddeleri seçin. Ters puanlanan maddeleri önce "Değişkenler → Ölçek puanı" ya da "Formülle hesapla" ile çevirmeniz gerekir; program negatif madde-toplam korelasyonlarını ayrıca uyarır.',
    alanlar: [{ ad: 'degiskenler', tur: 'cok', etiket: 'Maddeler', turler: ['surekli', 'sirali'] }],
    dogrula: (s) => (!s.degiskenler || s.degiskenler.length < 2 ? 'En az iki madde seçin.' : null),
  },
  {
    id: 'faktor',
    baslik: 'Faktör analizi (yapı geçerliği)',
    simge: 'tablo',
    kisa: 'Açımlayıcı (KMO, Bartlett, paralel analiz, döndürme) ve doğrulayıcı faktör analizi (CFI, TLI, RMSEA, SRMR).',
    ornek: '"Ölçek maddeleri kaç boyutta toplanıyor?"',
    aciklama: 'Açımlayıcı faktör analizinde faktör sayısı özdeğer > 1 ya da paralel analizle belirlenebilir. Doğrulayıcı analiz için her faktörün maddelerini yazın.',
    alanlar: [
      { ad: 'degiskenler', tur: 'cok', etiket: 'Maddeler', turler: ['surekli', 'sirali'] },
      { ad: 'tur', tur: 'secim', etiket: 'Analiz', secenekler: [['afa', 'Açımlayıcı faktör analizi (AFA)'], ['dfa', 'Doğrulayıcı faktör analizi (DFA)']], varsayilan: 'afa', yenile: true },
      { ad: 'cikarim', tur: 'secim', etiket: 'Faktör çıkarma yöntemi', secenekler: [['tbt', 'Temel bileşenler'], ['ana-eksen', 'Temel eksen faktörleme'], ['ml', 'En çok olabilirlik']], varsayilan: 'tbt', gorunur: (s) => s.tur !== 'dfa' },
      { ad: 'faktorSayisi', tur: 'secim', etiket: 'Faktör sayısı', secenekler: [['kaiser', 'Özdeğeri 1\'den büyük olanlar'], ['paralel', 'Paralel analiz'], ['1', '1'], ['2', '2'], ['3', '3'], ['4', '4'], ['5', '5'], ['6', '6']], varsayilan: 'kaiser', gorunur: (s) => s.tur !== 'dfa' },
      { ad: 'dondurme', tur: 'secim', etiket: 'Döndürme', secenekler: [['varimax', 'Varimax (dik)'], ['promax', 'Promax (eğik)'], ['oblimin', 'Oblimin (eğik)'], ['yok', 'Döndürme yok']], varsayilan: 'varimax', gorunur: (s) => s.tur !== 'dfa' },
      { ad: 'model', tur: 'metin', etiket: 'Model (her satır bir faktör)', yerTutucu: 'F1: S1, S2, S3; F2: S4, S5, S6', aciklama: 'Faktörleri noktalı virgülle ayırın: Faktör adı, iki nokta, maddeler.', gorunur: (s) => s.tur === 'dfa' },
    ],
    dogrula: (s) => {
      if (!s.degiskenler || s.degiskenler.length < 3) return 'En az üç madde seçin.';
      if (s.tur === 'dfa' && !String(s.model || '').includes(':')) return 'Doğrulayıcı analiz için modeli "F1: S1, S2, S3; F2: S4, S5" biçiminde yazın.';
      return null;
    },
  },
  {
    id: 'guc',
    baslik: 'Örneklem büyüklüğü / güç',
    simge: 'guc',
    kisa: 'Çalışmanız için kaç kişi gerekir? Mevcut örneklemin gücü nedir? (G*Power ile aynı hesaplar)',
    ornek: '"d = 0,5 etkiyi %80 güçle saptamak için kaç hasta gerekir?"',
  },
];
