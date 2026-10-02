// Raporu Word belgesine (.docx, Office Open XML) dönüştürür. Tablolar APA düzeninde (yalnızca yatay çizgiler),
// grafikler PNG olarak gömülür. Bağımlılık yoktur; PNG dönüşümü çağıran tarafından sağlanır (tarayıcıda canvas).

import { zipOlustur } from './zip.js';

const NS =
  'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ' +
  'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ' +
  'xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" ' +
  'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ' +
  'xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"';

export function xmlKacir(s) {
  return String(s)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    // XML 1.0'da geçersiz kontrol karakterlerini at
    .replace(/[\u0000-\u0008\u000B\u000C\u000E-\u001F]/g, '');
}

/** Metin parçası: {metin, kalin, italik, ust, boyut} */
function run(p) {
  if (typeof p === 'string') p = { metin: p };
  const rpr = [];
  // Öğe sırası OOXML şemasına (CT_RPr) uygun olmalı: b, i, color, sz, szCs, vertAlign
  if (p.kalin) rpr.push('<w:b/>');
  if (p.italik) rpr.push('<w:i/>');
  if (p.renk) rpr.push(`<w:color w:val="${p.renk}"/>`);
  if (p.boyut) rpr.push(`<w:sz w:val="${Math.round(p.boyut * 2)}"/><w:szCs w:val="${Math.round(p.boyut * 2)}"/>`);
  if (p.ust) rpr.push('<w:vertAlign w:val="superscript"/>');
  const satirlar = String(p.metin).split('\n');
  return satirlar
    .map((s, i) => `<w:r>${rpr.length ? `<w:rPr>${rpr.join('')}</w:rPr>` : ''}${i > 0 ? '<w:br/>' : ''}<w:t xml:space="preserve">${xmlKacir(s)}</w:t></w:r>`)
    .join('');
}

function paragraf(parcalar, { stil, hizala, oncesi, sonrasi, tutsun } = {}) {
  const ppr = [];
  if (stil) ppr.push(`<w:pStyle w:val="${stil}"/>`);
  if (tutsun) ppr.push('<w:keepNext/>');
  if (oncesi !== undefined || sonrasi !== undefined) ppr.push(`<w:spacing w:before="${oncesi ?? 0}" w:after="${sonrasi ?? 0}"/>`);
  if (hizala) ppr.push(`<w:jc w:val="${hizala}"/>`);
  const govde = (Array.isArray(parcalar) ? parcalar : [parcalar]).map(run).join('');
  return `<w:p>${ppr.length ? `<w:pPr>${ppr.join('')}</w:pPr>` : ''}${govde}</w:p>`;
}

function hucreNesne(h) {
  if (h === null || h === undefined) return { metin: '' };
  if (typeof h === 'string' || typeof h === 'number') return { metin: String(h) };
  return h;
}

const KALIN = '<w:top w:val="single" w:sz="12" w:space="0" w:color="000000"/>';
const ALT_KALIN = '<w:bottom w:val="single" w:sz="12" w:space="0" w:color="000000"/>';
const ALT_INCE = '<w:bottom w:val="single" w:sz="6" w:space="0" w:color="000000"/>';
const UST_INCE = '<w:top w:val="single" w:sz="6" w:space="0" w:color="000000"/>';

/** Satırları ızgaraya yerleştirir (colspan/rowspan); her satır için [{hucre, sutun, devam}] döner */
function izgaraYerlestir(satirlar) {
  const dolu = []; // dolu[r][c] = {kaynak hücre, devam}
  const yerlesik = [];
  satirlar.forEach((satir, r) => {
    dolu[r] = dolu[r] || [];
    const liste = [];
    let c = 0;
    for (const h0 of satir) {
      const h = hucreNesne(h0);
      while (dolu[r][c]) {
        liste.push({ ...dolu[r][c], sutun: c });
        c += dolu[r][c].genislik;
      }
      const gen = h.sutun || 1;
      const yuk = h.satir || 1;
      liste.push({ hucre: h, sutun: c, genislik: gen, devam: false, birlesik: yuk > 1 });
      for (let rr = r + 1; rr < r + yuk; rr++) {
        dolu[rr] = dolu[rr] || [];
        dolu[rr][c] = { hucre: h, genislik: gen, devam: true, birlesik: true };
      }
      c += gen;
    }
    while (dolu[r][c]) {
      liste.push({ ...dolu[r][c], sutun: c });
      c += dolu[r][c].genislik;
    }
    yerlesik.push(liste);
  });
  return yerlesik;
}

function tabloXml(t, punto) {
  const baslikSatirlari = izgaraYerlestir(t.basliklar);
  const govdeSatirlari = izgaraYerlestir(t.satirlar.map((s) => s.hucreler));
  const sutunSayisi = Math.max(1, ...[...baslikSatirlari, ...govdeSatirlari].map((s) => s.reduce((a, x) => Math.max(a, x.sutun + x.genislik), 0)));
  const toplamGenislik = 9000;
  const ilkSutun = sutunSayisi > 1 ? Math.round(toplamGenislik * 0.3) : toplamGenislik;
  const digerSutun = sutunSayisi > 1 ? Math.round((toplamGenislik - ilkSutun) / (sutunSayisi - 1)) : 0;
  const izgara = Array.from({ length: sutunSayisi }, (_, i) => `<w:gridCol w:w="${i === 0 ? ilkSutun : digerSutun}"/>`).join('');

  const hucreXml = ({ hucre, genislik, devam, birlesik }, { baslik, sonBaslik, ilkBaslik, sonGovde, ara, grupBasligi, ilkSutunMu }) => {
    // tcBorders içinde önce üst, sonra alt kenar gelmelidir (şema sırası)
    const ust = baslik && ilkBaslik ? KALIN : ara ? UST_INCE : '';
    const alt = baslik && (sonBaslik || (birlesik && !devam)) ? ALT_INCE : !baslik && sonGovde ? ALT_KALIN : '';
    const kenar = [ust, alt].filter(Boolean);
    const tcpr = [];
    if (genislik > 1) tcpr.push(`<w:gridSpan w:val="${genislik}"/>`);
    if (birlesik) tcpr.push(devam ? '<w:vMerge/>' : '<w:vMerge w:val="restart"/>');
    if (kenar.length) tcpr.push(`<w:tcBorders>${kenar.join('')}</w:tcBorders>`);
    if (devam) return `<w:tc><w:tcPr>${tcpr.join('')}</w:tcPr><w:p/></w:tc>`;
    const sol = hucre.sol || (ilkSutunMu && hucre.sol !== false) || grupBasligi;
    const parcalar = [{ metin: hucre.metin, kalin: baslik || hucre.kalin || hucre.anlamli || grupBasligi, boyut: punto }];
    if (hucre.ust) parcalar.push({ metin: hucre.ust, ust: true, boyut: punto });
    const girinti = hucre.girinti ? '<w:ind w:left="284"/>' : '';
    const p = `<w:p><w:pPr><w:spacing w:before="20" w:after="20"/>${girinti}<w:jc w:val="${sol ? 'left' : 'center'}"/></w:pPr>${parcalar.map(run).join('')}</w:p>`;
    return `<w:tc>${tcpr.length ? `<w:tcPr>${tcpr.join('')}</w:tcPr>` : ''}${p}</w:tc>`;
  };

  const satirXml = [];
  baslikSatirlari.forEach((s, r) => {
    satirXml.push(
      `<w:tr><w:trPr><w:tblHeader/><w:cantSplit/></w:trPr>${s
        .map((x) => hucreXml(x, { baslik: true, ilkBaslik: r === 0, sonBaslik: r === baslikSatirlari.length - 1, ilkSutunMu: x.sutun === 0 }))
        .join('')}</w:tr>`,
    );
  });
  govdeSatirlari.forEach((s, r) => {
    const tur = t.satirlar[r].tur;
    satirXml.push(
      `<w:tr><w:trPr><w:cantSplit/></w:trPr>${s
        .map((x) => hucreXml(x, { sonGovde: r === govdeSatirlari.length - 1, ara: tur === 'ara', grupBasligi: tur === 'grup-basligi', ilkSutunMu: x.sutun === 0 }))
        .join('')}</w:tr>`,
    );
  });
  return `<w:tbl><w:tblPr><w:tblW w:w="0" w:type="auto"/><w:tblLayout w:type="autofit"/><w:tblCellMar><w:left w:w="85" w:type="dxa"/><w:right w:w="85" w:type="dxa"/></w:tblCellMar></w:tblPr><w:tblGrid>${izgara}</w:tblGrid>${satirXml.join('')}</w:tbl>`;
}

function dipnotParcalari(d, punto) {
  const m = /^([a-z*†‡§]{1,2}):\s*(.*)$/.exec(d);
  if (m) return [{ metin: m[1], ust: true, boyut: punto }, { metin: m[2], boyut: punto }];
  return [{ metin: d, boyut: punto }];
}

function resimXml(rId, kimlik, genislikPx, yukseklikPx) {
  const maksEmu = 5.9 * 914400;
  let cx = Math.round((genislikPx / 96) * 914400);
  let cy = Math.round((yukseklikPx / 96) * 914400);
  if (cx > maksEmu) {
    cy = Math.round((cy * maksEmu) / cx);
    cx = Math.round(maksEmu);
  }
  return (
    `<w:p><w:pPr><w:keepNext/><w:spacing w:before="0" w:after="60"/><w:jc w:val="center"/></w:pPr><w:r><w:drawing>` +
    `<wp:inline distT="0" distB="0" distL="0" distR="0"><wp:extent cx="${cx}" cy="${cy}"/><wp:docPr id="${kimlik}" name="Sekil ${kimlik}"/>` +
    '<wp:cNvGraphicFramePr><a:graphicFrameLocks noChangeAspect="1"/></wp:cNvGraphicFramePr>' +
    '<a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic>' +
    `<pic:nvPicPr><pic:cNvPr id="${kimlik}" name="sekil${kimlik}.png"/><pic:cNvPicPr/></pic:nvPicPr>` +
    `<pic:blipFill><a:blip r:embed="${rId}"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>` +
    `<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="${cx}" cy="${cy}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr>` +
    '</pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r></w:p>'
  );
}

function stillerXml(yaziTipi, punto) {
  const yt = xmlKacir(yaziTipi);
  return (
    `<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:styles ${NS}>` +
    `<w:docDefaults><w:rPrDefault><w:rPr><w:rFonts w:ascii="${yt}" w:hAnsi="${yt}" w:cs="${yt}" w:eastAsia="${yt}"/><w:sz w:val="${punto * 2}"/><w:szCs w:val="${punto * 2}"/><w:lang w:val="tr-TR"/></w:rPr></w:rPrDefault>` +
    '<w:pPrDefault><w:pPr><w:spacing w:after="120" w:line="276" w:lineRule="auto"/></w:pPr></w:pPrDefault></w:docDefaults>' +
    '<w:style w:type="paragraph" w:default="1" w:styleId="Normal"><w:name w:val="Normal"/><w:qFormat/></w:style>' +
    `<w:style w:type="paragraph" w:styleId="Heading1"><w:name w:val="heading 1"/><w:basedOn w:val="Normal"/><w:next w:val="Normal"/><w:qFormat/><w:pPr><w:keepNext/><w:spacing w:before="240" w:after="120"/><w:outlineLvl w:val="0"/></w:pPr><w:rPr><w:b/><w:sz w:val="${(punto + 4) * 2}"/></w:rPr></w:style>` +
    `<w:style w:type="paragraph" w:styleId="Heading2"><w:name w:val="heading 2"/><w:basedOn w:val="Normal"/><w:next w:val="Normal"/><w:qFormat/><w:pPr><w:keepNext/><w:spacing w:before="360" w:after="120"/><w:outlineLvl w:val="1"/></w:pPr><w:rPr><w:b/><w:sz w:val="${(punto + 1) * 2}"/></w:rPr></w:style>` +
    `<w:style w:type="paragraph" w:styleId="Heading3"><w:name w:val="heading 3"/><w:basedOn w:val="Normal"/><w:next w:val="Normal"/><w:qFormat/><w:pPr><w:keepNext/><w:spacing w:before="200" w:after="80"/><w:outlineLvl w:val="2"/></w:pPr><w:rPr><w:b/><w:i/></w:rPr></w:style>` +
    '</w:styles>'
  );
}

/**
 * Word belgesi üretir.
 * @param {object} p
 * @param {string} p.baslik
 * @param {string[]} [p.bilgi] başlık altındaki bilgi satırları
 * @param {{baslik:string, bloklar:object[]}[]} p.ogeler numaralandırılmış rapor öğeleri (rapor/belge.js)
 * @param {(svg:string)=>Promise<{veri:Uint8Array, genislik:number, yukseklik:number}>} [p.pngDonustur]
 * @param {boolean} [p.kararlariEkle] "Neden bu test?" adımları belgeye yazılsın mı
 */
export async function docxOlustur({ baslik, bilgi = [], ogeler, pngDonustur, yaziTipi = 'Times New Roman', punto = 12, kararlariEkle = true }) {
  const govde = [];
  const resimler = [];
  const tabloPunto = punto - 1;
  govde.push(paragraf([{ metin: baslik }], { stil: 'Heading1' }));
  for (const b of bilgi) govde.push(paragraf([{ metin: b, italik: true, boyut: punto - 2, renk: '555555' }]));

  for (const oge of ogeler) {
    govde.push(paragraf([{ metin: oge.baslik }], { stil: 'Heading2' }));
    for (const b of oge.bloklar) {
      if (b.tur === 'baslik') govde.push(paragraf([{ metin: b.metin }], { stil: 'Heading3' }));
      else if (b.tur === 'metin') govde.push(paragraf([{ metin: b.metin }]));
      else if (b.tur === 'not') govde.push(paragraf([{ metin: b.metin, italik: true, boyut: punto - 1 }]));
      else if (b.tur === 'satir') govde.push(paragraf([{ metin: b.metin }], { oncesi: 60, sonrasi: 60 }));
      else if (b.tur === 'karar' && kararlariEkle) {
        govde.push(paragraf([{ metin: b.baslik || 'Test seçimi', kalin: true, boyut: punto - 1 }], { tutsun: true, oncesi: 120, sonrasi: 40 }));
        const yaz = (adimlar) => adimlar.forEach((a) => govde.push(paragraf([{ metin: `• ${a.baslik}: `, kalin: true, boyut: punto - 2 }, { metin: a.metin, boyut: punto - 2 }], { oncesi: 0, sonrasi: 20 })));
        if (b.adimlar) yaz(b.adimlar);
        for (const alt of b.alt || []) {
          govde.push(paragraf([{ metin: alt.baslik, italik: true, boyut: punto - 2 }], { oncesi: 60, sonrasi: 20 }));
          yaz(alt.adimlar);
        }
      } else if (b.tur === 'tablo') {
        const t = b.tablo;
        govde.push(paragraf([{ metin: `Tablo ${b.numara}`, kalin: true }], { tutsun: true, oncesi: 240, sonrasi: 0 }));
        govde.push(paragraf([{ metin: t.baslik, italik: true }], { tutsun: true, oncesi: 0, sonrasi: 80 }));
        govde.push(tabloXml(t, tabloPunto));
        if (t.not) govde.push(paragraf([{ metin: 'Not. ', italik: true, boyut: tabloPunto }, { metin: t.not, boyut: tabloPunto }], { oncesi: 40, sonrasi: 0 }));
        for (const d of t.dipnotlar || []) govde.push(paragraf(dipnotParcalari(d, tabloPunto), { oncesi: 0, sonrasi: 0 }));
        govde.push(paragraf([''], { oncesi: 0, sonrasi: 120 }));
      } else if (b.tur === 'grafik' && pngDonustur) {
        try {
          const png = await pngDonustur(b.svg);
          const no = resimler.length + 1;
          const rId = `rIdResim${no}`;
          resimler.push({ ad: `media/sekil${no}.png`, veri: png.veri, rId });
          govde.push(paragraf([{ metin: `Şekil ${b.numara}`, kalin: true }], { tutsun: true, oncesi: 240, sonrasi: 0 }));
          govde.push(paragraf([{ metin: b.baslik, italik: true }], { tutsun: true, oncesi: 0, sonrasi: 80 }));
          govde.push(resimXml(rId, no, png.genislik, png.yukseklik));
        } catch {
          govde.push(paragraf([{ metin: `[Şekil ${b.numara} dönüştürülemedi]`, italik: true }]));
        }
      }
    }
  }
  govde.push('<w:sectPr><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:top="1418" w:right="1418" w:bottom="1418" w:left="1418" w:header="709" w:footer="709" w:gutter="0"/></w:sectPr>');

  const belge = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:document ${NS}><w:body>${govde.join('')}</w:body></w:document>`;
  const iliskiler =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
    '<Relationship Id="rIdStil" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>' +
    resimler.map((r) => `<Relationship Id="${r.rId}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="${r.ad}"/>`).join('') +
    '</Relationships>';
  const icerikTurleri =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">' +
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>' +
    '<Default Extension="xml" ContentType="application/xml"/>' +
    '<Default Extension="png" ContentType="image/png"/>' +
    '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>' +
    '<Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>' +
    '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>' +
    '<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>' +
    '</Types>';
  const kokIliskiler =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>' +
    '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>' +
    '<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>' +
    '</Relationships>';
  const tarih = new Date().toISOString().replace(/\.\d+Z$/, 'Z');
  const cekirdek =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' +
    `<dc:title>${xmlKacir(baslik)}</dc:title><dc:creator>İstatistik Asistanı</dc:creator>` +
    `<dcterms:created xsi:type="dcterms:W3CDTF">${tarih}</dcterms:created><dcterms:modified xsi:type="dcterms:W3CDTF">${tarih}</dcterms:modified></cp:coreProperties>`;
  const uygulama =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"><Application>İstatistik Asistanı</Application></Properties>';

  return zipOlustur([
    { ad: '[Content_Types].xml', veri: icerikTurleri },
    { ad: '_rels/.rels', veri: kokIliskiler },
    { ad: 'docProps/core.xml', veri: cekirdek },
    { ad: 'docProps/app.xml', veri: uygulama },
    { ad: 'word/document.xml', veri: belge },
    { ad: 'word/styles.xml', veri: stillerXml(yaziTipi, punto) },
    { ad: 'word/_rels/document.xml.rels', veri: iliskiler },
    ...resimler.map((r) => ({ ad: `word/${r.ad}`, veri: r.veri })),
  ]);
}
