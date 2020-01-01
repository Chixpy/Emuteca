---
layout: page
title: Configuración sistemas de cartuchos
EmuVer: 0.8
---
En esta página vamos a tratar como configurar aquellos sistema basados en cartuchos.

Se trata del tipo de sistema típico, básico y más natural para el que está adaptado Emuteca.

Una versión de un juego es solo un fichero, sin más complicaciones. Además dicho fichero no es modificable (ROM). 

No confundir con [sistemas que usan cassete o disco](Disk-Config), en ese tipo de sistemas una versión de un juego es uno o varios discos y además cada disco puede tener varias caras. Y para rizar el rizo pueden ser modificables. Tampoco es aplicable a CD/DVD.
  
A no ser que el emulador del sistema tenga alguna restricción, no debería haber ningún problema (luego se indicarán para algunos posibles soluciones a algunas limitaciones).

La **organización de los juegos** en el disco duro realmente da igual; lo ejecutará con el emulador correspondiente (si es necesario, descomprimiendo el fichero con anterioridad y borrándolo después).

En cierto modo, tener los juegos comprimidos por grupos (*merged*) es tan sólo una recomendación.

Para la **exportación/importación de datos** es posible usar SHA1 por su consistencia.

## Requisitos ##

* Emulador.
* Software para el sistema.

## Detalles a tener en cuenta ##

* Hay que asegurarse de configurar el emulador para que las **partidas guardadas lo hagan en una carpeta diferente a dónde se encuentre el juego**, porque la carpeta temporal se borrará. Normalmente se suele hacer una carpeta dentro del propio emulador, pero a veces varios emuladores son compatibles entre sí y posiblemente sea mejor usar la carpeta "Saves" dentro del sistema (Ojo, solo lo que son partidas guardadas dentro del juego, no los estados del emulador llamados "savestates"). En un futuro puede que añada la posibilidad de renombrar estos archivos (ya que muchos emuladores leen esta memoria en base al nombre del archivo de la ROM, muy similar a lo que hace Emuteca con las imágenes).
* En algunos sistemas se tiene que los emuladores tienen una lista de juegos propia (N64) o que directamente no tiene un parámetro para ejecutar el juego desde la línea de comandos (WinAPE usándolo para los cartuchos de Amstrad GX4000). Este caso, lo mejor es configurar que el archivo temporal dónde se descomprimirán sea una carpeta conocida, para ese fin es la carpeta "Temp" del sistema. En el caso de la N64 también habrá que configurar dicha carpeta como directorio base para que lo liste.
* Si el sistema tiene diferentes extensiones, mejor dicho formatos de archivos equivalentes, lo mejor es atenerse a uno solo, pero esto es un consejo.