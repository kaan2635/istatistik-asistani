// Hesaplanamayan durumlar için ortak hata sınıfı. Mesajlar kullanıcıya gösterilir, bu yüzden Türkçe ve anlaşılır olmalıdır.
export class IstatistikHatasi extends Error {
  constructor(mesaj, ayrinti = {}) {
    super(mesaj);
    this.name = 'IstatistikHatasi';
    this.ayrinti = ayrinti;
  }
}

export function gerekli(kosul, mesaj) {
  if (!kosul) throw new IstatistikHatasi(mesaj);
}
