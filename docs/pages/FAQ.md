---
layout: page
title: Preguntas frecuentes
EmuVer: 0.8
---
Lista de preguntas frecuentes. Bueno, lo de "frecuentes" es un decir, ya que
nadie ha preguntado nada todavía... XD XD.

## Sobre Emuteca ##

### Requisitos mínimos ###

Ehh... Seguro que el ordenador con el que lo piensas usar usar es mucho mejor
que el ordenador con el que lo estoy desarrollando XD XD XD.

Veamos, así a ojo:
  
S.O.
: Probado con **Win10**
: XP/Vista/7 NO PROBADOS, pero no debería haber problemas.
: Wine se ejecuta pero no he probado nada más. 
: Teóricamente se puede compilar en Linux y espero que no se necesiten realizar muchas adaptaciones (incluso las rutas de archivos se guardan en formato Linux bajo Windows)
: 95/98/ME quien sabe...
: **Versiones anteriores de Emuteca (0.7)**: Probabas con XP/Vista/7. Dan problemas con Win10 y **hay que actualizar en 7zip** que va incluido por una versión que funcione correctamente.

Procesador
: **Pentium III** (y no me extrañaría que funcionara en un 386), aunque algunas operaciones en sistemas con más de 10.000 juegos y 1.500 familas tardan un rato en un incluso en un procesador de 2GHz. Y a la hora de descomprimir algo de un archivo comprimido de 300 megas o más también tarda un ratillo.

RAM 
: Unos 8 MiB + Iconos + Imágenes + Textos del juego seleccionado... = **~128MiB**. Para lo que se necesita RAM de verdad es para la compresión/descompresión de los archivos 7z (+1 GiB). Y por supuesto los requisitos para mover los emuladores son superiores.

Disco Duro
: ~2MiB + información del sistema = **8-16MiB**... pero esto solo es por parte de Emuteca. Si contamos todo: juegos, emuladores, imágenes, vídeos, música... 1 PiB puede venir justo...

### Definiciones ###

Antes que nada, esteblezcamos unos conceptos para entender a que me refiero. No deben diferir mucho en lo que comunmente significan en el contexto de los emuladores, pero sirven para aclarar dudas:

Sistema
: Se refiere a una videoconsola, ordenador u otro aparato físico que es emulado. También se puede referir a un conjunto de estos muy estrechamente relacionados (No merece la pena separar los Amstrad CPC en sus versiones 446, 646, 6128, los Plus, etc.) o por el contrario símplemente tan diversos que el emulador correspondiente es el que encarga de normalizarlos (MAME, Pinballs). En verdad tampoco tiene que ser "físico" pudiendo ser un Sistema Operativo como MS-DOS o Windows; o incluso ser un motor (Doom, OpenBOR, Unreal).

Emulador
: Programa que simula el funcionamiento de uno o varios sistemas. Pero en también es el programa motor dónde se pueden ejecutar mods o juegos (Doom, OpenBOR)

Juego
: Fichero (o varios fichero) con el programa para el sistema emulado. Ojo que no tiene que ser un "juego", puede ser una aplicación (sobre todo en ordenadores). Realmente, dependiendo del contexto, en verdad se trataría del soporte (Disco, CD, Cartucho), o del fichero principal que define ese soporte.

Familia o Grupo
: Conjunto de versiones del mismo juego. Además es usado para establecer algunos datos comunes y otros valores por defecto.

Zip
: Cualquier archivo comprimido, independientemente del tipo (rar, 7z, gz, etc.). Es que "archivo comprimido" es muy largo y puede hacerse muy repetitivo XD; también usaré "7z" indistintamente.

Una que intentaré evitar:

ROM
: _Read Only Memory_, aunque posiblemente si aparece sea usado de forma equivalente a "juego".

### ¿Cuál es el objetivo de Emuteca? ###

El objetivo inicial y final de Emuteca, se podría resumir en:

* Se tiene un juego comprimido en un .7z y el emulador del sistema no reconoce los 7z.
* Pues la misión de Emuteca consiste en listar el juego; y que cuando se quiera jugar, lo descomprima (si es necesario), ejecute el emulador y abra el juego recién descomprimido.
* Y al terminar de jugar: Borre el fichero recién descomprimido

Una vez cumplido esto (ya desde la versión 0.01pre), todo lo demás se pueden
considerar adornos:

* Gestionar los distintos sistemas y emuladores.
* Gestionar los datos de los juegos y agruparlos por familias.
* Mostrar la información multimedia de un juego/familia: Imágenes, Textos y si algún día llega... vídeos y música.
* Otras ayudas para hacer algunas tareas más sencillas.

De todas formas en la página _[¿Porque...](Why-Emuteca)_ se explica con más detalle las cosas por las que fue creado Emuteca.

### ¿En que se basa para funcionar? ###

La filosofía de Emuteca consiste en agrupar las distintas variaciones (o versiones) del mismo juego bajo el mismo nombre común (lo que denomino ''familia'').

Dicha familia además de agrupar juegos, define unos datos por defecto
(desarrollador, año, etiquetas) y el nombre de los fichero por defecto
para las imágenes de los juegos que la componen. Esto no impide (siempre y
cuando el nombre del archivo del juego, excluyendo los paréntesis, difiera
con el nombre de archivo usado por la familia) que los juegos tengan sus
propias imágenes y otros fichero.

Como se puede ver esto es muy similar a la filosofía del MAME y sus Front-End...
pero con una diferencia clara, la familia NO es un juego en si misma. Siendo
más explicitos, en el sistema "Recreativa" el juego padre de MAME es un juego
como otro cualquiera en Emuteca (aunque la familia tendrá su nombre y usará sus
archivos multimedia)

### Limitaciones ###

Esta es una serie de limitaciones conocidas (o puestas a proposito), sobre el
funcionamiento del programa:

* Un juego debe ser un fichero, lo que no impide que este pueda ser un archivo comprimido en si mismo (p.e. MAME); o que aunque sean varios fichero solo uno sea el principal (el que abre el emulador) y se necesite descomprimir todos los demás archivos comprimidos (por ejemplo, un zip con imagen de CD con .cue + .bin + .mp3 + .sub).
* Un juego no puede ser una carpeta/directorio (el único uso que conozco así es MAME y no es muy común usarlo de esta manera)
* El CRC32 o e SHA1 (para Importar/Exportar) se hace sobre el fichero en sí, no sobre los datos reales del juego (no extrae las cabeceras u otra información de los distintos formatos de archivos)
* Solo un nivel de compresión, es decir, si la ROM está dentro de un .zip dentro de un .7z, no se listará/descomprimirá/extraerá, sino que solo se podra listar el zip.

## Sistemas, Emuladores y formatos soportados ##

### ¿Qué sistemas soporta? ###

Uhm..., bueno pues contestando de forma rápida y mal: Todos.

Al no tener ninguna lista de sistemas específicos codificada internamente,
ni da soporte a características especiales para ninguno, se puede crear
cualquier sistema nuevo sin ningún problema y tan solo hay que configurarlo
(aunque puede ser complicado).

Como ejemplos extremos del funcionamiento aunque no son prácticos:

* Un sistema en el que define como extensiones "exe", y un emulador cuyo campo con el ejecutable está vacío (o apuntando a cmd.com/command.com), y se convierte en un Front-End de tus juegos de Windows XD XD. (Aunque posiblemente esto [sea mejor hacerlo de otra manera](v0_7-System-Examples#Ejecutables_Windows))
* Un sistema que usen la extensión "txt" y el emulador sea el Bloc de Notas.
* Comprimir los zip de MAME en un 7z por familias (no se gana mucho la verdad); configurar el sistema para que las extensiones sean "zip" y marcar la opción de descoprimir todos los archivos.

### ¿Qué emuladores soporta? ###

Emuteca ejecuta los emuladores desde la línea de comandos. Para poder cargar los juegos directamente el emulador debe soportar hacerlo desde ahí.

Por lo demás... cualquier emulador es válido, e incluso si no se define ninguna ruta a un ejecutable, Emuteca intenta abrir los archivos con el programa por defecto del sistema.

### ¿Que formatos soporta? ###

Parte de esta información se puede encontrar en la caja de diálogo "Acerca de Emuteca..."

{% capture imagefile -%}{{- site.baseurl -}}/img/Dialogs/AboutBox.png{%- endcapture -%}
{%- include image.html file=imagefile caption="Sobre... Emuteca" %}

Primeramente hay que realizar una diferencia; entre los que Emuteca soporta y lo que está configurado. 

Por ejemplo, Emuteca internamente soporta los siguientes tipos de imagen según su extensión para poder mostrarlas: png, xpm, bpm, cur, ico, icns, jpeg, jpg, jpe, jfif, tif, tiff, gif, pbm, pgm, ppm.

Realmente esta lista está recortada en el archivo GUI.ini a: jpg, png.

Esto es util para reducir el número de búsquedas cuando se buscan las imágenes relacionas con un juego o grupo. Siendo configurable según las necesidades

Sin embargo, para elegir la imagen de un sistema o los iconos por defecto se pueden elegir un archivo de las extensiones soportadas.

Hecha esta aclaración:

Imágenes
: Soporta: png, xpm, bpm, cur, ico, icns, jpeg, jpg, jpe, jfif, tif, tiff, gif, pbm, pgm, ppm
: Preconfigurado: jpg, png.

Textos
: Soporta: Texto plano (UTF8), 
: Preconfiguradas: txt

Música
: Realmente se usa [MPlayer](http://www.mplayerhq.hu) para reproducir la música así que las lista es muy grande 😁
: Preconfiguradas: mp3, ogg

Vídeos
: Obviamente también se usa [MPlayer](http://www.mplayerhq.hu). 
: Preconfiguradas: avi, mkv, mp4, mpg

Comprimidos
: Se usa [7zip](https://www.7-zip.org/)...
: Preconfiguradas:  7z, cb7, cbr, cbz, rar, zip
: A diferencia de los anteriores, esta configuración pertenece a Emuteca.ini, puesto que pertenece al núcleo de Emuteca y no al GUI.

## Cosas puntuales sobre sistemas ##

Aquí indicaré cosas puntuales sobre sistemas que probablemente serán movidas a sus recpectivas páginas.

### Game Boy Advance ###

#### Juegos: Meet the Robinsons y Petz Vet ####

*NOTA: Desde la versión 0.8, la GBA está preconfigurada por defecto para usar SHA1. Por tanto si se usa la configuración por defecto esto ya no afecta.*

Extraído de la última versión de GoodGBA:

> CRC32 mode is highly discouraged in this release. Two games:
> "Meet the Robinsons" and "Petz Vet" have the same CRC32 value for their copier
> fixed versions as for the true unhacked copy. You must use SHA1 mode or you
> will be missing two games no matter what you do.

Explicación: Dice que usar el modo CRC32 en esa version del GoodGBA (3.xx) está
altamente desaprobado. Dos juegos: Meet the Robinsons y Petz Vet tienen el mismo
CRC32 (tanto las versiones hackeadas como las originales). Debes usar el modo
SHA1 o no serán renombrados.

Si Emuteca usa CRC32 para reconocer los juegos para la identificación de los juegos de GBA durante la exportación e importación uno de los juegos sería sobreescrito con el otro. Ninguno desaparecerá, no afectando a los archivos y estarán los dos bajo el mismo nombre.

En versiones anteriores donde con F5 se reescaneaban os directorios causa el mismo efecto, ya que exporta e importa los datos existentes automáticamente.

Esto se puede arreglar renombrando el juego que está mal con el editor, ya que simplemente ahí solo se guarda y lee el texto que hay escrito en la lista (hasta que se se vuelvan a importar o reescanear el directorio datos, claro).

En un futuro no muy lejano, tengo pensada la posibilidad de poder re-renombrar
y agrupar y organizar los fichero (similar a lo que hace Goodxxx + GoodMerge).
Pero no me iba a basar en los CRC32 u otro sistema similar, sino en la
estructura creada por el usuario en el árbol de juegos del sistema, así que
esto no es un problema en este sentido (al menos de forma directa).

### Recreativa (MAME) ###

#### Ficheros comprimidos ####

MAME, sus variantes y otros emuladores tienen varias formas de buscar un juego
(y sus ROMS).
  * En una carpeta con el nombre clave del juego.
  * En un zip o 7z con dicho nombre.

Como se describe en las limitaciones del programa, los juegos no pueden ser una
carpeta por tanto los juegos deben estar según el segundo método, que es el más habitual de todas formas.

Por otra parte, también el método de búsqueda de dichos emuladores permite
que se den los siguientes casos:

No Merged
: Todos los juegos (padres e hijos) tienen todas las ROM necesarias para poder ser usado de forma independiente, cada una en su zip correspondiente; esto significa que muchos ficheros de las ROM están repetidas varias veces entre los archivos.
Merged / Split (el más común)
: En el archivo del juego padre se encuentran todas sus ROM, pero en los juegos hijo solo se encuentras aquellas ROM propias o que difieren de la del padre.
Merged / No Split
: Todas las ROMS de un juego padre y sus hijos en un mismo zip (normalmente, el del padre aunque MAME hace una búsqueda más profunda y pueden estar en cualquiera de sus hijos). Es decir en un mismo archivo esta el juego principal y todas sus versiones.

En verdad, a no ser que se use un gestor de ROMS para MAME posiblemente el caos
sea mayor, como ROMs del padre en un zip de un hijo. Además desde hace bastante
también las BIOS de las recreativas se encuentran de forma separada.

Como ya se ha dicho, para Emuteca un juego es un fichero, por tanto si se
tienen _Merged / No Split_ tan solo se listarán los juegos padre, mientras que
con los otros dos modos no debería haber problema.

Resumiendo: Tienen que estar en zip y _Merged / Split_ o _No Merged_.

#### Juegos sin ROMS ####
Esto está sacado de la información de un script para hacer una base de datos
con los nombres de los juegos para el MAME en Perl (que no me acuerdo dónde
lo encontre).

Resulta que hay una serie de juegos que no necesitan ningun tipo de ROM
adicional respecto a sus juegos padre que son:
  * calspeda.
  * candance.
  * galpanib.
  * gauntd24.
  * jdreddb.
  * natodefa.
  * vaportrp.
  * wotwc.
Cuando se usa un gestor de ROMS (por ejemplo: ClrMAMEPro) estos distribuyen los
ficheros donde corresponda e incluso borran los anteriores ya que se quedarían
sus ficheros vacios, sin embargo Emuteca necesita que estos ficheros zip estén
creados (aunque ocupen 0 bytes) para poder listarlos.

Algo similar ocurriría con algunos forks del MAME que incluyen el Pong (que no
tiene ROMS), así que si usas alguna de esas versiones también deberías crear el
archivo pong.zip vacio para que lo liste Emuteca.

## Cosas puntuales sobre emuladores ##

No tengo objetivo de explicar la configuración de todas y cada una de las
configuraciones de los sistemas o los emuladores, pero si indicaré algunos
problemas o algunas cosas complicadas, que no tienen una solución clara o se
trata de un comportamiento extraño inexperado.

Algunas cosas serán movidas a sus respectivas páginas.

### Kega Fusion ###

Joder, con lo bueno que es este emulador...

#### Al salir del emulador... ####

Pueden pasar dos cosas.

  * **Se muestra un cuadro indicando que se producido el error 1** y no se guarda el tiempo jugado: Pues... el problema es que parece ser que cuando Kega se cierra correctamente devuelve un código de salida con valor 1 en vez de 0 espera Emuteca y como es habitual. Por tanto, muestra el código del error y al creer que ha ocurrido un error no añade el tiempo jugado. Asegurate de indicar en la configuración del emulador que ese es el código de salida para cuando se cierra.
  * **Emuteca se queda minimizada y no responde**: Si se ejecuta con el parámetro `-fullscreen` al pulsar `Esc` se cierra el emulador, pero por algún motivo desconocido Emuteca no detecta que el proceso haya finalizado y continúa esperando.

### NeorageX vX.XX ###

#### No ejecuta el juego directamente ####

Lógico, este emulador en ninguna de sus versiones soporta parámetros de la
línea de comandos... :P y poco provecho se le puede sacar con Emuteca.

De todas formas, MAME y otros emuladores de recreativas están lo
suficientemente optimizados para emular este sistema... aunque NeoRage pueda
ejecutar algunos juegos que los otros no pueden.

### Snes9X ###

#### Se cuelga al cargar un juego ####

¿Lo tienes configurado para que se inicie a pantalla completa?

Misteriosamente,
cuando se le manda a este emulador que cargue un juego desde la línea de
comandos y está configurado para que se inicie a pantalla completa, el emulador
se cuelga al iniciarse.

La solución es hacer que siempre se inicie en ventana
(ya sea editando su archivo de configuracion donde pone fullscreen = true y
cambiarlo por false) o abriendo el emulador (sin cargar ningún juego, con la
opción de ejecutar el emulador solo), pulsado ALT+RETORNO para pasarlo a
ventana y saliendo.

### Visual Pinball ###

#### Hacer que ejecute directamente la mesa ####

Este emulador acepta parámetros por la línea de comandos pero no están muy
documentados además de ser algo raros.

Si se le pasa un .vpt como parámetro la mesa pinball la abre con el editor,
sin embargo para poder hacer que se ejecute la mesa directamente hay que
escribir el la casilla de edición de parámetros:
> -play -"%ROM%"
Atención al guión antes del nombre de la mesa; si no lo hay, no funciona.

## ¿Versión para Linux? ##

Pues... en principio tenía pensado hacer el programa con Lazarus, incluso las
primerísimas versiones están hechas con él, pero como el componente principal
del programa que es la lista de juegos (VirtualTreeView) me daba muchos
problemas y no está completamente adaptado tuve que hacerlo con Delphi...

También comencé a usar otras cosas que no existían en Lazarus así que se
complió el volver a este compildor...

Sin embargo, a partir de Emuteca 0.5, separé la gestión de los juegos respecto
del formulario principal... y fui haciendo cositas...

Con la version 0.6 seguí probando cosas... y el aviso de Google sobre el virus
que afecta a los Delphi hizo que finalmente volviera a usar Lazarus para la
versión 0.7.

Durante una temporada probé con CodeThypon y finalmente volví a Lazarus... Y cuando comencé a reescribir la versión 0.8...

{% capture imagefile -%}{{- site.baseurl -}}/img/TestLinux.png{%- endcapture -%}
{%- include image.html file=imagefile caption="Test tempranero con Linux" %}

Pero esto no quiere decir que ahora sea fácilmente portable, ya que estoy seguro de que
uso bastante código específico de Windows.

## Otros ##

### ¿Dónde... ###

La verdad es que no sé porque pongo esta sección, todo se encuentra con
cualquier buscador en un momentín.

#### ... puedo encontrar juegos y ROMS? ####

Tan solo puedo contestar:

* Recreativa:
  * http://mamedev.org/roms/
  * [Gaelco](http://gaelco.com/pages/hablando/frhablan.htm) - El enlace "DESCARGAR MEMORIAS MAME" permite descargar el juego "World Rally" de forma legal.
* Game Boy Color:
  * [Daikatana de la página de John Romero](http://rome.ro/games_daikatana.htm)
* ZX Spectrum:
  * [World of Spectrum](http://www.worldofspectrum.org/)

Para las demás mejor enterarse bien de las leyes de tu país...

#### ... puedo encontrar emuladores? ####

Algunas páginas con noticias sobre emuladores son:

* [Emulatronia](http://www.emulatronia.com/) (Aunque algo abandonada...)
* [El dominio de Zophar](http://www.zophar.net/) (inglés)
* [Emulator Zone](http://www.emulator-zone.com/) (inglés)

#### ... puedo encontrar imágenes ya hechas para los juegos? ####

* En general:
  * [QuickPlay Frontend](http://www.quickplayfrontend.com/). La gente que usaba (y sigue usando) QuickPlay tras ser abandonado y no ha tenido ganadas de crearse su propio Front-End para solucionar esos pequeños errorcillos que lo apartaban de la perfección XD. Es una broma, yo también lo usaba y por una tontería creé Emuteca. Me bajé el código fuente... y ví que vivían dragones ahí (no quiero decir que el código sea malo, puesto que Emuteca lo tendrá peor; pero decidí comenzar otro front-end desde cero en vez de modificarlo). Esta es la razón por la que no me he ofrecido a ayudar a arreglar los fallos.
  * [The Video Game Museum](http://www.vgmuseum.com/)
* MAME:
  * [MAME World](http://mameworld.info/) (Inglés) En la columna derecha, sección "Art Files", además justo encima tiene los enlaces a los archivos auxiliares de este emulador (mameinfo, cheat, history, etc.)
  * [EMU France](http://www.emu-france.com/) (Francés) Tan solo hay que buscar un poco, y aunque no se sepa francés se llega de forma fácil.

#### ... puedo encontrar melodías ya hechas para los juegos? ####

* [Video Game Music Archive](http://www.vgmusic.com/) (Inglés) Con melodías en formato MIDI (hay tanto intentos por mantenerse fiel al original, como remixes y arreglos).
* [Zophar](http://www.zophar.net/music.html) (Inglés) En el formato original directamente extraidas de los juegos.

Por otra parte:

* [Overclocked Remix](http://ocremix.org/)

#### ... puedo encontrar más cosas interesantes? ####
* [EmuMovies](http://www.emumovies.com/) (Inglés) Videos de previsualización de los juegos.

* [Replacement Docs](http://www.replacementdocs.com) (Inglés) Escaneados de los manuales de instrucciones, lamentablemente son PDF y no se pueden poner en Emuteca :/
* [MAMEXT](http://www.mamext.net/) - Fork de MAMEUI preparado para usar más imágenes, videos, manuales y demás. Lo interesante es que dichos archivos se pueden descargar de esa misma página.

* [GSA](http://www.gsarchives.net) (Inglés) Sprites de videojuegos (Aunque para muchos iconos creo que sería mejor reusar los de MAME)
* [The Spriters Resource](http://www.spriters-resource.com/) Otros pocos más
* [Sprite Database](http://sdb.drshnaps.com/) Y más...

* [Games Database](http://www.gamesdbase.com/) De todo un poco.

Si te gustan las partidas jugadas rápidamente...

* [TASVideos](http://tasvideos.org/) Estas son hechas con emulador y usando sus ventajas (poder pausar, manejar la suerte, etc.)
* [Speed Demos Archive](http://speeddemosarchive.com/) Tiene partidas de este tipo "legales", realizadas por un ser humano en tiempo real.

Y si crees que eres el mejor en un juego...

* [Twin Galaxies](http://www.twingalaxies.com/) - Podríamos llamarlo "el registro del Libro Guinness de los videojuegos".