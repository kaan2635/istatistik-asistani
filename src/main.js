// Giriş noktası.
import { baslat } from './arayuz/uygulama.js';
import { depo } from './arayuz/dom.js';

const tema = depo.oku('istatistik-asistani-tema', '');
if (tema) document.documentElement.setAttribute('data-tema', tema);

baslat(document.getElementById('uygulama'));
