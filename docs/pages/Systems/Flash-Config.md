---
layout: page
title: Configuración de Adobe Flash
EmuVer: 0.8
---

En este documento se indicará como se puede usar Emuteca para lanzar los distintos programas de Flash Player.

Los programas en Adobe Flash se pueden tener de dos formas:

  - Autoejecutables: Es decir un ejecutable .exe que se ejecuta directamente.
  - Archivos .swf: Que es el formato nativo de este tipo de software. 

## Autoejecutables ##

En caso de los autoejecutables lo mejor y más sencillo es tratarlos como programas de  Windows.

En caso de que se prefiera los SWF, existen varias utilidades para extraer los swf de los autoejecutables.

## Archivos .swf

Existen varios programas capaces de reproducir este tipo de archivos sin necesidad del plugin del navegador y desde 2020 Adobe dejó de continuar el producto y ningún navegador actual lo soporta. Algunos de estos programas son Irfanview y VLC entre otros.

De forma oficial, Adobe tenía Flash Player Projector que dejó de poder descargarse en la página oficial desde mediados de 2020, siendo la última versión la 32. Por suerte se puede encontrar en Archive.org:

  - [Subido directamente](https://archive.org/details/adobe-flash-player-projector)
  - [Version archivada de la página oficial de descarga](https://web.archive.org/web/20210714135833/https://www.adobe.com/support/flashplayer/debug_downloads.html)

Por lo demás, en la parte de la gestión no se diferencia de un [sistema de cartuchos normal](Cart-Config).

## Observaciones

### Comprimir los archivos .swf 

No merece la pena comprimirlos individualmente, ni siquiera con 7z. Los .swf son directamente archivos comprimidos con zlib (7Zip los puede abrir y reabrir el fichero interno) y por tanto no gana casi nada de espacio. 

De la misma forma, TAMPOCO se gana espacio al agrupar varias versiones dentro de un mismo archivo puesto que los ficheros son totalmente diferentes por esa compresión. Ya que una pequeña variación entre versiones al comprimirse puede resultar en archivos muy diferentes.

Sin embargo, no es mala idea agrupar las versiones en un archivo comprimido NO SÓLIDO.

### No todos los .swf "funcionan" de esta forma 

Según esté programado el .swf, puede impedir su correcto funcionamiento con programas externos a modo de protección. Las razones puede ser variadas:

  - Que esté programado para funcionar en un servidor concreto.
  - Que busque algún recurso en internet y éste ya no se encuentre disponible. A veces aunque no lo encuentre permite continuar (sobre todo si es publicidad).
    - También puede suceder que .swf sea tan solo realmente un frame para cargar otro .swf.
  - Que necesite algún parámetro en la url del archivo. Esto realmente no es problema porque Flash Player Projector permite pasar los parámetros como de una url se tratara "archivo.swf?param1&param2=valor2", pero hay que saber que parámetros espera y el valor...  
  
De esta forma aquellos .swf que no funcionan por estas razones son marcados como BadDumps en Emuteca.

Existe un proyecto llamado [FlashPoint Archive](https://flashpointarchive.org/) encargado de preservar todos estos archivos y hackearles para que continúen funcionando, a base de método que van mucho más allá del propósito de Emuteca. De hecho, desde el punto de vista de Emuteca, FlashPoint es un emulador.



