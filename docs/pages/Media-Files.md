---
layout: page
title: Ficheros de Medios
EmuVer: 0.8
---

Emuteca, además de listar, agrupar y ejecutar emuladores con sus juegos, también permite visualizar y reproducir contenido relacionado con el software como pueden ser imágenes, manuales, textos, vídeos o música.

Realmente, excepto el soporte para archivos comprimidos, la capacidad de visualizar y reproducir este tipo de contenido relacionado es dependiente de la interfaz (EmutecaGUI por el momento) puesto que el núcleo (EmutecaCore) tan solo se encarga de buscar y poner a su disposicion los ficheros encontrados.


## Repositorios

Para algunos sistemas existen unos repositorios creados para ser usados directamente con Emuteca (si se usan las carpetas que se crearían por defecto con [el botón correspondiente](Dialogs/Auto-Config-Folders)). La verdad es que por el momento distan mucho de estar completos, optimizados o totalmente asignados a los grupos y juegos.

De forma general, y en caso de no estar actualizada esta lista la lista, puedes verlos en los [Repositorios de Chixpy en GitHub](https://github.com/Chixpy?tab=repositories&q=ETKRes&type=&language=&sort=name)

  - [Amstrad CPC](https://github.com/Chixpy/ETKRes-CPC)
  - [Amstrad GX4000](https://github.com/Chixpy/ETKRes-GX4000)
  - [Atari 2600](https://github.com/Chixpy/ETKRes-2600)
  - [Atari 5200](https://github.com/Chixpy/ETKRes-5200)
  - [Atari 7800](https://github.com/Chixpy/ETKRes-7800)
  - [Atari Jaguar](https://github.com/Chixpy/ETKRes-Jaguar)
  - [Atari Lynx](https://github.com/Chixpy/ETKRes-Lynx)
  - [Bandai WonderSwan](https://github.com/Chixpy/ETKRes-WonderSwan)
  - [Bally Astrocade](https://github.com/Chixpy/ETKRes-Astrocade)
  - [Microsoft MSDOS](https://github.com/Chixpy/ETKRes-MSDOS)
  - [Misc - Arcade (MAME)](https://github.com/Chixpy/ETKRes-Arcade) (*ATENCIÓN: Hay que configurar a mano los directorios, al ser diferentes del resto de sistemas*)
  - [NEC Turbografx 16 / PC Engine](https://github.com/Chixpy/ETKRes-TG16)
  - [Nintendo GameBoy](https://github.com/Chixpy/ETKRes-GameBoy)
  - [Nintendo GameBoy Advance](https://github.com/Chixpy/ETKRes-GBA)
  - [Nintendo NES](https://github.com/Chixpy/ETKRes-NES)
  - [Nintendo SNES](https://github.com/Chixpy/ETKRes-SNES)
  - [Nintendo Virtual Boy](https://github.com/Chixpy/ETKRes-VirtualBoy)
  - [Sega GameGear](https://github.com/Chixpy/ETKRes-GameGear) 
  - [Sega Master System](https://github.com/Chixpy/ETKRes-MasterSystem)
  - [Sega MegaDrive / Genesis](https://github.com/Chixpy/ETKRes-MegaDrive)
  - [Sega SG1000](https://github.com/Chixpy/ETKRes-SG1000)  
  - [SNK Neo Geo Pocket](https://github.com/Chixpy/ETKRes-NGPocket)
  - [Sinclair ZX Spectrum](https://github.com/Chixpy/ETKRes-ZXSpectrum)

Hay dos formas de usarlos:

  - Clonando los repositorios con Git (o cualquier Front-End) en las carpetas base de cada sistema y así pudiendo actualizarlo de forma sencilla (renombrando y borrando archivos de forma automática).
  - Bajando el estado actual del repositorio en un zip y descomprimirlo (sin la carpeta base con el nombre del reprositorio) en las carpeta base de cada sistema.
    - En la descripción del cada repositorio hay un enlace, para poder bajarlo directamente.
    - En caso de no funcionar el enlace directo anterior se haría, pulsando sobre el botón `code` y en el menú desplegable elegir `Download zip`.

## Funcionamiento de Emuteca con los archivos asociados

### Formatos soportados y activados

Emuteca GUI soporta multitud de formatos para los distintos tipos de archivos. De forma práctica, tan solo están activados los más útiles ya que afecta a la velocidad a la hora de buscar. Aunque se pueden activar más editando el archivo `GUI.ini` es preferible convertir los archivos a estos formatos:

  - **Imagen**: `jpg`, `png`
    - Soportados: `bmp`, `cur`, `gif`, `icns`, `ico`, `jfif`, `jpe`, `jpeg`, `jpg`, `pbm`, `pgm`, `png`, `ppm`, `tif`, `tiff`, `xpm`
  - **Texto**: `txt`
    - Soportado: Archivos de texto plano
  - **Video**: `avi`, `mkv`, `mp4`, `mpg`
    - Soportado: Originalmente los soportados por mPlayer, pero el componente ha dejado de funcionar.
  - **Música**: `mp3`, `ogg`
    - Soportado: Originalmente los soportados por mPlayer, pero el componente ha dejado de funcionar.
    
A partir de la version 0.8.0.154 se ha desactivado la búsqueda de los ficheros dentro de archivos comprimidos. Aunque se tiene pensado que en un futuro sea configurable.

De todas formas, no era recomendable usar estos ficheros dentro de archivos comprimidos puesto que (excepto los `txt`) ya son archivos con algún tipo de compresión por si mismo y tan solo se ganaría espacio de framentación interna (espacio no ocupado en los bloques del disco duro).

En caso de usarse, entonces también era recomendable usar un formato que no sea sólido para evitar la necesidad de descomprimir todos los datos anteriores antes de llegar al archivo buscado.

### Funcionamiento general de Emuteca

De forma general, la búsqueda de cualquier tipo de ficheros multimedia para cada juego se realiza de la siguiente forma, parando la búsqueda en cuanto encuentra algo:

  - 1º Todos los ficheros de los formatos activados dentro de una carpeta con la clave de ordenación como nombre.
    - `MediaFolder\SoftSortTitle\*.ext`.
  - 2º Sólo un archivo que tenga como nombre la clave de ordenación y una de las extensiones activadas.
    - `MediaFolder\*.ext`
    - Es decir, si hay un `jpg` y un `png` con el mismo nombre solo selecionanará un archivo.
  - 3º Repetir los pasos anteriores pero con la clave de ordenación del grupo al que pertenece el juego.

Para iconos y logos tan solo seleccionará un solo fichero aunque haya varios dentro de una carpeta en el paso 1.

### Recomendaciones

Para aprovechar el funcionamiento de Emuteca se indican una serie de recomendaciones para cada tipo de archivo

#### Imágenes

Dependiendo del tipo de imagen se tiene unas normas generales.

##### Capturas de pantalla jugando, Pantallas de título

De forma general para cualquier captura de pantalla del juego se deberían aplicar las siguientes normas:

  - Capturas en la resolución nativa del sistema y sin corregir la relación de aspecto.  
    - Esto puede ser algo complicado en sistemas *analógicos* (Atari 2600) y pseudosistemas en los que se puede cambiar la resolución (Quake)
    - Lamentablemente muchos sistemas no tienen una resolución interna 4:3 aunque se visualizaran en ese tipo de pantallas. 
    - Automatizar la corrección de aspecto puede ser ciertamente complicado... [El formato `png` tiene un bonito parámetro pero no se usa](http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.8)    
  - Sin marquesinas ni bordes externos no dibujables.  
    - No es lo mismo que eliminar lar partes vacías que pudiera haber.
    - Por ejemplo: Esto incluye los bordes de Super GameBoy.    
  - No usar ningún tipo de filtro o suavizado. Tecnicamente al no reescalar la imagen no se hace uso de ello...
  - Usar un formato sin pérdida... En resumen, usar el formato `png`

##### Frontal, Traseras, Análisis, Anuncios, etc.

Esto es aplicable a todas las **imágenes escaneadas** de revistas y **fotografías**:

  - Formato `jpg` y que la calidad que sea bastante aceptable si es posible.  
    - El término *calidad* depende del compresor.    
  - El lado más largo que sea de 2048px como máximo  
    - Respetar la relación de aspecto en caso de ener que hacer más pequeña la imagen.
    - Si la imagen de por sí es más pequeña, no hacerla más grande.
    - Por supuesto, es preferible cortar primero y redimensionar después.    
  - Intentar no reguardar la imagen varias veces. Al ser un formato con pérdida se va deteriorando la calidad.  
    - Hay transformaciones que es posible realizar de forma especial sin que la calidad se vea afectada.
    - Se puede tolerar que la imagen no esté totalmente recta.

Para material del juego:

  - Frontal, lomo y trasera de la caja: Recortados de forma separada y sin ningún tipo de borde.
  - Media (Cartucho, CD, Disquete, etc.): Escaneado o fotografía del frontal del soporte medio (no solo la etiqueta) y sin bordes a ser posible.

Para textos de revistas y anuncios:

  - Recortar la imagen al texto e imágenes del artículo con un pequeño borde.

Para mapas y guías:

  - Si es una guía o mapa de revista: Lo mismo que los artículos, recortar al texto e imágenes.
  - Si es un mapa digital, hecho con capturas de pantalla o similar, y que no está escaneado de una revista: Usar `png` (si no era un `jpg` ya) y mantener como está. Mantener el tamaño incluso si excede los 2048px.

##### Iconos y Logos

Emuteca tan solo usa una imagen de cada tipo para cada grupo o juego. Y normalmente son extraidos a mano:

  - Iconos: Principalmente son extraidos de capturas de pantalla en su resolución original. Y suelen ser frames del protagonista, icanos de vida o algún otro icono reconocible.
    - Algunos sistemas tienen sus propios iconosy es mejor extraerlos.
  - Logos: Normalmente extraidos de pantallas de título o menús principales en su resolución original..
  
No extraerlos de fotografías o escaneados.

  - Usar el formato `png` con fondo transparente.
  - Tamaño original (sin redimensionar a un tamaño fijo), sin filtros ni bordes vacíos (ni alrededor, ni para hacerlo cuadrado). Emuteca redimensiona y centra de forma automática.
  - En caso de que los "píxeles" del la imagen sean 2x2, 3x3, etc. redimensionar para que sean de 1x1.
  - Una vez extraidos se añade un borde de gris medio y semitransparente (RGBA: 128, 128, 128, 128 / #80808080)


[Emuteca](https://github.com/chixpy/emuteca) incluye en su distribución la herramienta [ETKIconBorder](https://github.com/Chixpy/Emuteca/blob/master/bin/Tools/ETKIconBorder.exe). Que es un simple editor de imágenes para recortar, extraer, poner fondo transparente y aplicar un filtro que añade el borde automáticamente.

Alternativamente también incluye un script para GIMP que añade el borde semitransparente una vez se ha creado el fondo transparente... Pero es muuuuuuyyyy lento.

#### Textos 

Esto es simple, para que se vea en una TextBox:
  - Texto plano.
  - UTF 8 sin cabecera.
  - Línea vacía entre párrafos.
  - Sin saltos de línea dentro de los párrafos. El TextBox tiene activado el WordWrap
  - Es aceptable usar MarkDown
  
#### Vídeos 

Que sean a resolución y framerate nativo, si puede ser.

#### Música 

Que no tengan mucha pérdida de calidad.
