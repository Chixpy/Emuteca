---
layout: page
title: Sistemas
EmuVer: 0.8
---

Cada sistema es un mundo, y como tal para su emulación se requieren un tratamiento y estructura distinto para cada uno de ellos, lo cuál dificulta la tarea de hacer una generalización sencilla a través de un interfaz para gestionarlos todos como es la intención de Emuteca.

No es lo mismo la configuración necesaria para una consola que usa cartuchos, que para los arcades de MAME, o un ordenador que usa cassettes o disquetes. Por tanto, voy comentar cual creo que es la mejor forma de tratar cada tipo de sistema para su uso con Emuteca.

En esta página se intentará explicar el funcionamiento genéral de Emuteca. Para más detalles sobre las particularidades de cada tipo de sistema puedes visitar las siguientes páginas:

  - [Sistemas de cartuchos](Systems/Cart-Config): Atari 2600, NES, MegaDrive, etc.
  - [Sistemas de disco o cassete](Systems/Disk-Config): Spectrum, Commodore 64, etc.
  - [Sistemas de disco óptico](Systems/CD-Config): PSX, Dreamcast, PS2, Wii, etc.
  - [Juegos de Windows](Systems/Windows-Config). (Aplicable a otros "sistemas" que realmente ejecutables de Windows, como Game&Watch de Madrigal)
  - [Arcade (MAME)](Systems/Arcade-Config): Ciertamente es aplicable, en parte, a Final Burn Alpha y otros emuladores.
  - [Mapas de Doom](Systems/Doom-Config): Aplicable también sus derivados Doom II, Heretic, Hexxen y Strife.
  - [Mapas de Quake](Systems/Quake-Config): Quake tiene una forma diferente de funcionar de Doom. 
  - [Mapas de Duke Nukem 3D](Systems/DukeNukem3D-Config).
  - [Adobe Flash Player](Systems/Flash-Config).

Esbozos e ideas no desarrolladas:

  - [DOSBox](Systems/DOSBox-Config)
  - [Visual Pinball](Systems/VisualPinball-Config)
  - [Acorn Archimedes](Systems/Archimedes-Config)

## Emuladores

Aunque esta pagina sea para explicar las generalidades de los sistemas, antes vamos a tocar un poco el concepto de los emuladores.

Lo normal es que se cree un emulador en Emuteca para cada programa diferente que se vaya a usar. De forma general, se configura el ejecutable con los parámetros que hay que pasarle para que autoarranque el juego. Sin embargo, se nos puede plantear dos problemas:

  - Que el programa soporte varios sistemas: Mednafen, Retroarch, MAME, etc.
    - Si el emulador autodetecta que sistema o núcleo debe usar, se podría configurar de la forma simple habitual. Por ejemplo, Mednafen hace buen trabajo aunque es mejor indicárselo explícitamente. 
    - Si no, hay que configurar el sistema y emulador para usar el parámetro apropiado y el emulador use el núcleo correspondiente del sistema. Por ejemplo, Retroarch (y además hay que elegir núcleo específico).
    - En caso más extremos, hay que configurar el emulador como si de dos emuladores distintos se tratase. En MAME, las recreativas son diferentes al resto de sistemas
  - Que requiera unos parámetros diferentes para cada tipo de soporte diferente que tiene el sistema emulado (Cassette, disco, CD, cartucho, ...).
    - Entonces se debe configurar el emulador para que use los parámetros apropiados dependiendo de la extensión del archivo del juego.
  
Y para colmo, es probable que ambas situaciones se produzcan simultáneamente.

Respecto a como deben estar organizados los emuladores en el disco duro, no deberia haber problemas. Tan solo la recomendación de tener todos organizardos en subcarpetas de una carpeta común.

## Sistemas

En general, y para todos los sistemas, lo más recomendable es tener separados los programas emuladores respecto a los datos y juegos de los sistemas. De esta forma se pueden varios emuladores para un mismo sistema de forma independiente que puedan acceder a esos datos y juegos.

También hay que entender que algunos *sistemas* en realidad son *motores* (Doom, Pinball, OpenBOR, etc.), que realmente no son sistemas físicos.

## Juegos / Programas (Software de los sistemas)

Antes de nada, un *juego* no tiene porqué ser un *juego* 😵; los términos más correctos serían *Software* o *Programa* del sistema emulado; ya que se trata de programas para usar en el sistema emulado. De hecho en los sistemas que son realmente *motores* tampoco son programas 😵🤬, sino que son ficheros de datos para su funcionamiento.

Internamente Emuteca identifica un juego con un archivo *principal*, no se pueden añadir *carpetas* propiamente dichas.

Esto es válido para la mayoría de sistemas habituales en las que un juego es un archivo, aunque en otros puede darse el caso de que un juegos sean varias *caras de disco* o se requieran una serie de archivos adicionales de datos junto al archivo principal. Las particularidades de cada caso son tratadas en los enlaces iniciales de esta misma página.

Por otra parte, para la importación de los datos de los juegos se pueden usar varias formas de identificación:

  - La más común usar el SHA1 del archivo principal, si el archivo es consistente y no modificable (ROM).
  - En en caso de los CD y DVD el tamaño del archivo puede hacer que obtener el SHA1 sea muy lento y por ahora se recomienda usar CRC32.
  - En caso de que el SHA1 no sea consistente, también se puede usar el nombre del archivo si el sistema tiene una forma bastante establecida en el nombrado (p.ej. MAME) o un identificador.
  - Otros casos puede ser más complicados (por ejemplo, imágenes de discos duros) ya que pueden ser grandes y modificables.

## Grupos

El concepto de *Grupo* es muy similar al usado por *MAME* y sus *parents*. Aunque no es exactamente igual puesto que un *grupo*, a diferencia de un *padre*, no es un juego en si mismo.

Al igual que hace GoodMerge, agrupa todas las versiones y derivados de un mismo juego.

Este sistema es usado sobre todo en la búsqueda de material relacionado de un juego. Por ejemplo, cuando no se encuentra específicamente la imagen de un juego en concreto, se pasa a buscar la del grupo al que pertenece.

