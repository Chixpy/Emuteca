---
layout: page
title: Lista de cambios
EmuVer: 0.8
---

Si quieres ver la evolución visual del programa puedes ir a la [galería de imágenes](Screenshots)

## 0.8.0.XX

Para un futuro lejano... [Aunque si la quieres compilar...](https://github.com/Chixpy/Emuteca/wiki/v0_8-Building)

La versión que está en desarrollo actualmente vuelve estar comenzada desde cero (otra vez..). Tampoco se quiere añadir muchas más cosas de las que tiene la versión 0.7.

En principio comenzó a ser desarrollada en CodeTyphon, pero posteriormente portada a Lazarus limpio y se necesita intalar un par de librerías externas.

  - [VirualTreeView para Lazarus](https://github.com/blikblum/VirtualTreeView-Lazarus.git), la rama _lazarus_v5_. Que hay que instalar en el propio Lazarus. Posiblemente si durante el desarrollo se actualiza Lazarus se pase a usar la versión 6 ya que esta requiere actualmente la versión Trunk de FPC. (Aunque implicará rehacer todo el código de dichos componentes)
  - Y hasta la version 0.8.0.155: [TMPlayerControl](http://wiki.freepascal.org/TMPlayerControl). También ha de instalarse en el propio Lazarus. Pero parece que ha dejado de funcionar.

Ambos componentes se instalan a través de Administrador de Paquetes en Línea del propio Lazarus.
  
Se ha revisado sobre todo la estructura interna con los siguientes objetivos:

  - [X] Mayor rapidez a la hora de cargar y listar los juegos.
  - [X] Hacer las tareas largas que no interfieren en el funcionamiento del usuario en segundo plano.
  - [X] Poder listar juegos de todos los sistemas simultáneamente.
  - [X] Independizar el funcionamiento interno respecto a la interfaz.
    - Con posibilidad de desarrollar un entorno gráfico (p. ej. con SDL) aparte del "editor" (¿Para la versión 1.0, tal vez? ).
  - [X] Mayor modularización para reusar de forma rápida los distintos componentes. 
  - [X] Poder filtrar los juegos en base a etiquetas o listas de juegos; similar al estilo CatVer.ini de MAME.
  - [X] Buscar una forma para poder simplificar los emuladores multisistema de tal forma que se pueda añadir los parámetros que identifican el núcleo a usar (MAME/Retroarch/Mednafen/¿Higan?)

Bueno, pues la verdad es que ya está y tan solo hay que pulirlo cara al usuario, aunque se están añadiendo alguna que otra funcionalidad.  

Una de las ideas barajadas y probadas había sido poder agrupar juegos de distintos sistemas... Por ejemplo: Mario Bros. de Arcade y NES estuvieran en el mismo grupo... pero no lo veo claro y sería lioso.

### Interno 

  - Al igual que la versión anterior, las listas de juegos se van cargando según se necesiten. Pero una vez cargadas se quedan en memoria y la siguiente vez que se vuelva a un sistema ya cargado no hará falta esperar. Por otra parte la velocidad de carga de la lista de juegos ha aumentado bastante a unos 100.000 juegos por minuto. Cuando he actualizado mi ordenador a un i7 me carga 300.000 por minuto
  - Básicamente todos los sistemas que usaban CRC32 y no sean CD/DVD para exportar/importar datos de los juegos pasan a usar SHA1 por defecto.
  - El SHA1 de los juegos en los sistema se calcula en un proceso paralelo una vez añadidos. Esto acelera el poder añadir juegos en masa y poder interactuar con ellos pero impedirá importar/exportar los datos de los juegos hasta que el proceso haya terminado (si usan SHA1 como identificador obviamente).
  - Cambiada, otra vez, la forma de guardar los datos de los juegos y las bases de datos para importar:
    - Dos ficheros: uno para los juegos y otro para los grupos de cada sistema.
    - Por defecto en la carpeta `Systems` (antes se guardaban en `Data`).
    - Formato CVS (Comma Separated Values), aunque los grupos tienen la extensión .egl. Incluso de pueden abrir con cualquier hoja de cálculo, con su cabecera y todo (aunque no lo recomiendo... porque pueden estropear los datos numéricos). De hecho se ha creado una utilidad (ETKDBEditor) para editarlos, aunque es muy básica.
    - A cambio es mucho más rápida su lectura y escritura (unos 15 segundos para 50000 juegos y 15000 grupos, creo que me repito...).
    - Ya no se usan .INI, para las bases de datos para exportar/importar. Sino CSV también, editable también con ETKDBEditor.
    - Ahora también se pueden hacer importaciones parciales, dicho de otro modo cambiar solo los valores de un campo dejando los otros como están (´@´ para señalar que no se debe modificar ese campo).
    - Al exportar se mantiene el contenido del archivo anterior que no se haya modificado.
  - Tras muchas pruebas y cambios de perspectiva, al final la estructura interna de organización no ha variado mucho, aunque está más cohesionada (por ejemplo, desde un juego se puede acceder al grupo y sistema al que pertenece).
  - Mejorado el soporte de los emuladores multisistema, añadiendo parámetros avanzados para:
    - Poner un parámetro que puede configurar el sistema que lo ejecute.
    - Poner paramétros que dependen de la extensión de la ROM (y diferenciar cassettes, cartuchos o discos)
    - Poner parámetros que se pueden configurar en cada juego (nº de mapa en Doom, o la orden necesaria dentro del emulador para poder autoarrancar el juego si lo soporta el emulador desde la línea de commandos).
  - Soporte, a ver si de manera definitiva, de nombres de archivo con caracteres que no son ANSI al descomprimir y ejecutar.
  - Ahora los juegos se pueden filtrar internamente, en vez de tener que hacerlo el GUI usado.

### GUI 

  - Los iconos de los grupos y juegos ya no se buscan bajo demanda según se necesitan. Ahora esta búsqueda se realiza en un hilo paralelo en segundo plano con los juegos listados (independiente del sistema o si están filtrados).
  - Se ha quitado el buscar ficheros multimedia que estén dentro de un archivo comprimido. Era lento, no tenía mucho sentido porque los archivos multimedia ya tienen compression por si mismos y además entraban en conflicto las dos formas en que se buscaban.
    - `NombreJuego.ext`
    - `NombreJuego\XXX.ext`
  - ~~¡Vídeos y Música! usando MPlayer como motor.~~ No, va ser que todavía no, el componente parece que ha dejado de funcionar.
  - El panel de imágenes y textos es en cierto modo configurable, pudiendo añadir los subpaneles que se deseen. 
  - Se muestran las estadísticas del Sistema y del Emulador.
  - El árbol de etiquetas ya funciona y filtra los juegos. Se pueden añadir y quitar grupos, seleccionando la etiqueta y pulsando botón derecho sobre el grupo. Cuando se marcan varias etiquetas se muestran los juegos que están en todas las etiquetas.
    - La idea es añadir a los juegos etiquetas estilo [UVList](https://www.uvlist.net/), con la perspectiva, ambientación, localizaciones, etc.
    - Como ejemplo, varios .ini de MAME han sido convertidos. Si bien no estoy de acuerdo en como se usan por ejemplo los de géneros (por eso se encuentran aparte), los de sagas de videojuegos pueden ser muy útiles.
    - Esto es otra odisea para rellenar con todos los juegos de los demás sistemas.
  - De forma general se han estandarizado los formularios y frames para guardar su posición y configuración.

### Listado de juegos 

  - Cada grupo muestra el icono del sistema al que pertenece. Como se pueden listar todos los sistema simultánemente, se necesita para poder diferenciar los que tienen mismo nombre.
  - Además cada sistema puede tener un icono propio para usar por defecto para su software.
  - Posibilidad de crear grupos nuevos en el editor de juegos (de hecho, ahora editor de grupos y de juegos son cosas distintas; aunque el primero está incluido en el segundo).
  - Si un grupo sólo contiene un juego, el juego es mostrado directamente en la lista.
  - Cuando la lista de juegos está activa, con ´CTRL´ + ´+´ y ´CTRL´ + ´-´ se puede cambiar el tamaño de la fuente de la lista (cambiando también la altura de las filas).
  - La búsqueda de juegos se debe hacer pulsando el botón "Buscar". Es posiblemente la parte más lenta y que salte continuamente puede ser molesto.

### Barra de progreso

  - Ahora con el botón derecho se puede configurar la frecuencia de actualización. Si se actualiza menos se gana un poco de velocidad

### Media Manager

  - Arreglados unos pequeños bugs.
  
 ### Gestor de Scripts

  - Bastantes scripts para realizar diversas tareas con los datos de Emuteca o manejo de ficheros y conversión de bases de datos al formato de Emuteca.
  
### Utilidades adicionales

  - ***ETKDBEditor***: Un simple editor de bases de datos de tanto de juegos como de grupos.
  - ***ETKIconBorder***: Un sencillo programa para crear iconos y logos. Centrado más en la creación de transparencias y añadir bordes a imágenes que en ser un programa de dibujo propiamente dicho.
  - ***ETKPDF2CBX***: Interfaz para extraer páginas de revistas escaneadas en PDF, mediante PDFImages (o PDFToPNG como último recurso). Originalmente es parte de Comicteca para crear CBX con las imágenes; pero aunque se haya quitado el soporte de archivos comprimidos en Emuteca, puede extraerlas en una carpeta sin comprimir.
  - ***ETKMagCut***: Programa para recortar artículos de las revistas escaneadas (es decir, normalmente para usar después de haber extraído las imágenes con ETKPDF2CBX); puesto que para Emuteca interesa recortar el artículo, noticia, guía, mapa, etc. que corresponda el juego únicamente en vez de páginas completas.

## Emuteca 0.7.0.XX Beta

[Descarga](https://github.com/Chixpy/Emuteca/releases/tag/v0.7.0.59-beta)

No pasó de Beta... El ejecutable es de depuración y al salir muestra un mensaje con la memoria usada (aunque ponga "error", no se trata de uno como tal a no ser que tenga mucho texto y salgan muchos mensajes seguidos).

La versión 0.7.0 fue rehecha casi desde cero con Lazarus, y posteriormente con CodeTyphon, aunque también he hecho algunos experimentillos con wxDevCPP (sí, C++) y Delphi 2009.

Se perdieron cosas por el camino debido a cosas específicas de Delphi en Windows como el reproducir música o vídeos de forma interna (aunque tenía pesado la forma de suplirlo con MPlayer).

El código principal es bastante funcional, lista juegos y grupos/familia, descomprime y ejecuta el emulador correspondiente. Pero faltan retoques por aquí y por allí...

### Interno

  - Supuestamente, todo debería funcionar en UTF8.
  - **Gestor de script**, posibilita la creación y ejecución de scripts en Pascal (gracias a [Pascal Script](http://www.remobjects.com/ps.aspx)) para manejar los datos de los juegos y listas, entre otras cosas. Todas las opciones que había en el menú herramientas (renombrado, parsear TOSEC, etc.) serán convertidas a scripts.
  - Separación más clara entre juego y familia/grupo (antes eran incluso lo mismo...). Cambios en las propiedades de ambas clases.
  - Búsqueda de ficheros alternativa en la que un zip o carpeta contiene varias imágenes (estilo actual como lo hace MAME, para entenderlo mejor). Además se han añadido las siguiente extensiones: CBR/CBZ/CB7.

### Formulario principal

  - Debido al Gestor de Scripts, las opciones chapuceras (en el buen sentido de la palabra chapuza) del menú utilidades están siendo convertidas en scripts.
  - Añadida una barra de progreso. Grande y en todo el medio de la ventana para que te enteres bien de que Emuteca está haciendo algo. (Además sirve para impedir que se toque algo en el formulario sin necesidad de desactivarlo o incluso para poder cancelar la operación)
  - Añadido el archivo `Search.ini`, por el que se pueden añadir buscadores de forma personalizada para buscar un sistema, emulador o juego en internet.
  - Ahora se pueden cambiar las propiedades de la fuente de texto de la lista de juegos. en verdad tan solo era un experimentillo, pero lo dejaré por el momento.

### Gestor de sistemas

  - Directorio de juegos siempre recursivo (no encuentro ninguna razón para que no lo sea); aunque realmente en vez quitar la opción, la oculto y sigue funcionando. Hay que editar el archivo de configuración de los sistemas.
  - Quitada la opcion **No buscar en comprimidos**. Por ejemplo, para MAME con poner "zip" entre las extensiones del sistema es suficiente para que no busque "dentro" de ese tipo de archivos.
  - A cada sistema se le puede asignar una imagen de fondo para la lista.

### Gestor emuladores

  - Añadida posibilidad de añadir un icono y una imagen para el emulador.

## 20100524 - Emuteca 0.6.2.38

~~Descarga~~

Versión realizada con Delphi 7.

En esta versión mi mayor propósito era arreglar la transparencia de fondo en los PNG y a ver si conseguía hacer que los .ico se redimensionen y se vean como debe.

  - Cambiada la librería de gráficos TNGmages por las librería de JVCL `pngimage` (dentro de la carpeta `devtools/MakePNG`). **Por fin se ven transparente como debe**, a cambio **ya no reconoce los .mng** (mejor usar .mpg en su carpeta correspondiente) ni los .jng (formato casi nada usado) entre otros.
  - El gestor multimedia renombraba mal los ficheros si se asignaban a un juego (no quitaba los paréntesis ni corchetes).

## 20100208 - Emuteca 0.6.1.36

~~Descarga~~

Tan solo son unos arreglos de fallos evidentes y rápidos que se colaron
en la anterior versión:

  - Fallo en el gestor multimedia en el que tenía repetido una parte del código (Lo raro es que lo he usado intensivamente y no había visto el fallo)
  - Fallo al leer del fichero de configuración las extensiones de los vídeos demo y la música demo, y por tanto no se mostraban como debían en el gestor multimedia (Aunque no se veía por el fallo anterior...).
  - Al exportar ya no se exporta los datos de cuantas veces se ha jugado y demás.
  - Organizado el orden de los campos cuando se pulsa la tecla tabulador.
  - Los vídeos se adaptan al tamaño del cuadro dónde se muestran... (eso sí, no se guarda la relación de aspecto del vídeo...)
  - Añadido un mensajito en el gestor multimedia para avisar que si hay muchos archivos dentro de los zip puede que taaaaaaarde demasiado.
  - Indica correctamente cuando un emulador ha devuelto un error. Pero claro, tan solo es un número al que no le doy sentido. Al menos es mejor que el error que daba antes que era por exceso de rango XD.

## 20100202 - Emuteca 0.6.0.35

~~Descarga~~

Antes de nada, empecemos con los problemas conocidos...

### Problemas conocidos

  - La importación de bases de datos creadas con versiones anteriores no funciona (ahora son en inglés los campos).
  - ~~Debido a un pequeño error al exportar los datos también se exporta el tiempo jugado, el número de veces y la última vez que se jugó (y ahora mismo no puedo recompilarlo...)~~ Arreglado 0.6.1.36
  - Lo de siempre, los png con transparencia de fondo se ve en negro.
  - Los iconos (.ico) no se redimendionan (por tanto, quedan feos en la lista de juegos, que junto a lo anterior...).
  - Los vídeos no se adaptan al tamaño del panel dónde se reproducen.
  - Lentitud (¡más aún!), pero esta vez al seleccionar un juego. Sobre todo si los archivos de imágenes, iconos, etc. están repartidos entre muchos archivos comprimidos, son muy grandes o hay muchos archivos dentro del un archivo comprimido.
  - Si el emulador devuelve un código de error, Emuteca salta un mensaje de error que no tiene nada que ver...
  - Las últimas versiones de Mozilla (FireFox/SeaMonkey/Etc.) al arrastrar una imagen, devuelven un .bmp ¡que no existe!, así que ahora es mejor usar "Copiar imagen" y pulsar con el botón derecho en la imagen de Emuteca para seleccionar "Pegar imagen"

De éstas hay algunas cosas que me traen por la calle de la amargura (lo de los .ico y .png), otras que requieren otro cambio en la estructura (la lentitud) y otras que son triviales (lo de las bases de datos exportadas).

### General

  - Fichero de configuración (Emuteca.ini) para establecer las imágenes, directorios y demás opciones de configuración. Es un archivo diferente al usado para guardar la posición de la ventana para poder reestablecer de forma sencilla ambas cosa (borrando sus respectivos ficheros) sin que una cosa afecte a la otra.
  - Ahora los textos por defecto están en inglés en vez de en castellano, con posibildad de traducirlos de forma sencilla (no sé si es muy amigo del UTF-8 ...). Para traducirlo a otro idioma tan sólo hay que poner el nombre del archivo para el idioma en el archivo de configuración. En caso de no existir una traducción ya hecha, se puede indicar un fichero inexistente e irá añadiendo las cadenas que va necesitando o copiar la versión en inglés para traducirla.
  - Ahora la resolución objetivo es 800x600 en vez de 640x480, es decir que intento encajar todo en un formulario de esas dimensiones.

### Formulario principal

  - Muestra los iconos de cada juego... (pero claro... si son .ico no se redimensionan y si son png los muestra con fondo negro...)
  - Veamos que hacemos con las imágenes y el vídeo... Añadido **Reproductor multimedia**, es lo que viene con Delphi y parece ser que tiene problemas con algunos MP3 y OGG. Por el momento solo está preparado (auto)reproducir una canción/vídeo por juego, y en formatos comunes. (Realmente, solo he probado MIDI y MPG..., pero puedes cambiar en Emuteca.ini las extensiones reconocidas)
  - Ahora se muestra la imagen del sistema, así como el fichero de texto con la información sobre él. Se puede arrastrar o pegar una imagen para asignarla.
  - Añadidas 3 columnas nuevas en el listado: Número de veces ejecutado, fecha de la última vez y el tiempo jugado.
  - Para las imágenes y texto se crean las pestañas que sean necesarias y se puede cambiar el título de la pestaña
  - Solo se descomprime y carga la imagen activa; y los iconos de los juegos visibles (en un intento de hacer que vaya más rápido).

### Sistema/Gestor sistemas

  - Ahora se puede tener el número de directorios que se quiera para imágenes, textos, música o vídeo.

### Emuladores/Gestor emuladores

  - Ahora se puede elegir desde que directorio se quiere ejecutar el emulador, antes siempre se ejecutaba desde el directorio del ejecutable del emulador.

### Interno

  - Actualizado VirtualTreeView a la versión 5.0.0, aunque ahora lo actualizo mediante SVN y por tanto puede que cuando termine use una versión intermedia más actual.
  - ~~De la misma forma actualizo mediante SVN el JCL y JVCL.~~ No uso esa versión porque se ha estropeado lo de para acceder a los archivos comprimidos y los botones de los editbox para buscar ficheros.
  - Actualizado a 7zip 9.XX (Si compilo con las últimas versiones de JCL y JVCL, reconoce más formatos comprimidos)
  - Un objeto que lee la configuración es pasado a los distintos formularios para leer las opciones de configuración a los distintos formularios.
  - Los formularios se traducen al crearse.

### Otros

  - Dejo de compilarlo con el Pentium III a 1Ghz, 192Mb y XP/ME; y a partir de ahora uso un T4200 2.10Ghz, 4Gb y Vista (Argh, :P). No es por dar envidia :P, sino para advertir que el cambio cualitativo puede implicar que descuide la optimización y rapidez de algunas cosas. Sigo usando Delphi 7 (por el momento me gustaría mantener la compatibiidad con Win98).

## 20090814 - Emuteca 0.5.0.22

~~Descarga~~

### Formulario principal

  - Quitada la imagen "miniatura" del icono (en principio la quiero poner dentro del árbol)
  - Se guarda el estado de la ventana (mediate JEDI) y los paneles.
  - Menus contextuales por aquí y por allá...
  - Se puede asignar una imagen que está copiada en el portapapeles (actuando de la misma forma que cuando se arrastra el fichero sobre la imagen). La opción está en el menú contextual de la imagen principal.
  - **Las imágenes de los juegos pueden estar en un archivo comprimido** (o varios :P), funciona en cualquiera de los formatos comprimidos que se reconocen, el nombre del archivo da igual ya que se busca en todos los archivos comprimidos que se encuentren, lo que ya no podría predecir es el orden en el que se recorren... así que es mejor tener solo uno. Tendiendo preferencia el siguiente orden:
    - 1º Imagen descomprimida del juego
    - 2º Imagen comprimida del juego
    - 3º Imagen descomprimida de la familia
    - 4º Imagen comprimida de la familia

#### Panel búsqueda

  - En la caja de búsqueda ahora se puede seleccionar si se quiere buscar en el nombre de los juegos, familias, desarrollador, palabras clave, año y versión.

#### Panel editor

  - La casilla "Familia" es ahora un ComboBox.
  - Quitado el botón de guardar la lista, no me terminaba de convencer...

#### Panel textos

  - Cuando se pulsa el botón guardar solo guarda el texto activo y no los tres como ocurría antes.

#### Utilidades

  - La opción _Cambiar " - " por ": "_ cambiada por _Cambiar " - " por "; "_ (el punto y coma da muchos menos problemas que los dos puntos)
  - Cuando se usa la utilidad de hacer que el nombre de las familias sea en base al directorio (o nombre del zip) en el que se encuentran, si el nombre tiene paréntesis estos se sustituirán por "`_`", en vez de "-".
  - Relacionada con lo anterior... Se ha añadido otra utilidad que extrae automáticamente el año y el desarrollador del nombre de la familia que tenga un formato "`Nombre _Año_ _Desarrollador_`" (y lo quita del nombre de la familia). No es la panacea y puede extraer algunos mal (sobretodo si no tienen ese formato)... :/ Solo lo hace con las familias, no con los juegos (para que así tengas que repasarlo XD XD, aunque con darle a grabar en el editor de datos a la familia modificando a sus hijos se cambian)

### Gestor sistemas

  - Ya se guardan y se leen los emuladores alternativos para dicho sistema. Aunque por ahora no son usados, la idea es poder ejecutar el juego con un emulador alternativo.

### Gestor multimedia

  - Mejoras internas (No creo que se noten mucho).
  - Tecla Intro (en un ListBox o ListView) = Botón Asignar.
  - Tecla Supr (en un ListBox de archivos) = Botón Borrar.
  - Añadido RichEdit para previsualizar los textos.
  - Un panel indica las dimesiones de la imagen original o el número de líneas del archivo de texto. El número de líneas del texto es despues del WordWrap automático así que la verdad no es muy útil XD XD.

### Interno

  - Sucumbí a la tentación y ahora uso las librerías JVCL del proyecto JEDI y de aquí vienen directamente algunas mejoras de forma directa al usar sus componentes. Pero esto ha impactado claramente en el tamaño del ejecutable y consumo de memoria.
  - Actualizado el componente VirtualTreeView a la versión 4.7.0 (Ouch, ya hay otra versión más nueva)
  - Reestructurada la forma en que se almacenan los juegos, siendo esto "independiente" del árbol (a costa de complicarme la vida y ralentizar mucho cuando se cambia de sistema). Ahora el árbol puede agrupar por año, desarrollador, directorio/fichero comprimido y palabras clave.
  - Ahora directamente se comprueban todos los formátos gráficos que soporta Delphi + paquetes instalados a la hora de compilar, es decir: **¡Soporta gif!** (Una cosa que estaba entre las cosas que no iba a arreglar, pero eso es gracias a JEDI), otras extensiones añadidas son .ani (iconos animados), .wmf/.emf (dibujos vectoriales), .jng (jpg con transparencia), .jpe/.jpeg/.jfif (extensiones alternativas .jpg), etc. El orden de preferencia en caso de haber dos fichero con mismo nombre pero distinta extensión es basicamente el mismo de antes: .mng, .png, .gif, .jpg, ..., .bmp, .ani, .ico
  - Soporte para un montón de tipos de archivos comprimidos (los que soporta [7-Zip](http://www.7-zip.org). Dejo de usar la unidad SevenZip.PAS (de Dominic Dumée), para pasar a usar lo que tiene ya implementado JEDI.

### Otros

  - ~~Compilado con Delphi 2006... ejem... he vuelto a instalarme Win XP... Así que he rehecho todas la ventanas y formularios desde cero, por tanto algunas cosillas visuales han cambiado. Y el código es copipegado del anterior pero repasado y corregido en su totalidad. Supongo que el programa tampoco funcionará ya en Win9X/ME... T\_T.~~ Ejem.. ejem... vuelto a compilar con Delphi 7 XD XD XD aunque es verdad que he rehecho todos los formularios y repasado todo el código. Así que han cambiado ligeramente algunas cosillas visualmente y algunos conceptos.
  - Subido a GoogleCode -> Licencia GNU GPL 3 (que es la que más conozco sus entresijos... bueno, la 2.1 en verdad). Aunque mi intención, posiblemente consista en usar la [Licencia Pública de la Unión Europea 1.1](http://ec.europa.eu/idabc/en/document/7330) (que no está soportada por GoogleCode) pero debo leérmela con detenimiento.
  - Uso de [JADD - DelphiDoc](http://delphidoc.sourceforge.net/) para la documentación de las unidades, clases y demás cosas internas. También tengo intención de usarlo para la ayuda del programa. Existen varias programas similares como [PasDoc](http://pasdoc.sipsolutions.net/) y [DelphiCodeToDoc](http://dephicodetodoc.sourceforge.net/) pero no me terminan de convencer.

## 20090129 - Emuteca 0.4.1.18

~~Descarga~~

En principio quería que esta fuera la 0.5.0.XX y estaba a la espera de inspirarme para hacer una cosilla (exactamente lo destripar los archivos history.dat para el MAME) para subirla, pero me embarcado en repasar el código, podríamos decir copiar y pegarlo de nuevo XD, y posiblemente quite/cambie cosas como CRC32 (incluso las muchas cosas supuestamente consistentes, en verdad no lo son...), usar iconos Tango (más que nada para que tengan la misma apariencia, porque los que hay cada uno son de su padre y su madre), a ver si hago que el menú de enlaces sea configurable desde fuera del código (en un archivo de texto normalito... posiblemente con un nuevo formulario para hacerlo... ejem, ¿más sencillo?), guardar la configuración de la ventana (ay, que fácil era esto con el JVCL del proyecto JEDI... pero no lo uso), poder leer las imágenes y ficheros dentro de un zip (estilo MAMEUI, solo para zip ya que los 7z tardarían bastante en extraer la imagen si hay muchas), etc...

  - Actualizado el FastMM 4.92 (el gestor de memoria) es una cosa interna, así que no debería preocupar mucho. Lo mismo con la dll para descomprimir los "7z" y "zip".
  - Unas cuantas opciones nuevas en el menú enlaces: Emulatronia, Screenchot Archive, 1Emulation, MAME, etc.
  - Añadida la posibilidad de **leer animaciones .mng** (aunque eso lo tenía ya la nueva librería de gráficos que uso, como se puede suponer por la dll) en las imágenes, teniendo preferencia esta extensión. No lo he probado mucho... pero parece correcto. En caso de haber varios archivos con el mismo nombre y distinta extensión ("juego.png", "juego.mng", "juego.jpg") el orden de preferencia es mng, png, jpg, bmp y finalmente ico.
  - La búsqueda muestra también aquellos título de juegos que coinciden con la cadena buscada pero el nombre de la familia, no. Es decir, **busca juegos en vez de familias** como hacía antes. Además he quitado el botón de buscar para que se busque mientras se introduce texto. No lo hice antes porque pensaba que se iba a ralentizar mucho... (bueno y lo hace, si hay unos 10.000 juegos con un PIII)
  - Intercambiado el orden de un par de columnas por defecto (se podían y pueden reordenar arrastrando la cabecera a una nueva posición e incluso ocultar si se pulsa con el botón derecho sobre la cabecera, pero por el momento no se guardan los cambios realizados ahí).
  - **Arreglado el problema de arrastrar una imagen desde Firefox a la imagen principal del juego seleccionado**... pero FF algunas veces al arrastrar indica una ruta a un fichero que no existe o no se corresponde con la imagen, otras que el archivo está en uso... pero en este último caso solo lo intento copiar y no debería haber problema ¿¿¿O\_o???. IE no permite arrastrar imágenes a no ser que se pulse botón derecho/Mostrar imagen (parece que funciona correctamente) y Opera no tengo ganas de mirarlo pero de entrada parece que no deja arrastra imágenes.
  - **Al pulsar la tecla RETORNO en los campos del editor graba los cambios**, (Usa TAB para pasar de campo a campo sin grabar los cambios en la lista)
  - Nueva opción en el menú ultilidades: **Reorganizar familias según la estructura de los ficheros**, es decir agrupa aquellos juegos que se encuentran dentro del mismo directorio o fichero comprimido bajo la misma familia. El nombre de la familia será el nombre del fichero comprimido (sin extensión) o directorio. Esto es útil para archivos comprimidos con RoomMerge
  - **Optimizado un poco más el gestor multimedia**. Ahora en las listas no aparecen los juegos o familias cuyo nombre de fichero es igual al usado por otra familia o juego añadido anteriormente a la lista. Esto reduce bastante los listados si se tienen muchas versiones de juegos en los que su nombre de fichero solo se diferencia por el contenido entre paréntesis. Añadido un poco de actividad mientras compara archivos, familias y juegos para que se vea que no se ha colgado :P.

## 20081003 - Emuteca 0.4.0.14

  - **¡Ayuda!** No, no es que la necesite. Sino que he creado un archivo HTML donde explico más detalladamente el funcionamiento para liar y confundir más a la gente. ¿Porqué no lo hago en un .chm y pongo ayuda contextual? Os dejo elegir la respuesta: "No me da la gana" o "Es muy pesado hacerlo"
  - **¡Búsqueda!** El panel de búsqueda ya funciona, por el momento solo busca en el nombre de las familias pero algo es algo...
  - Añadidas barras de desplazamiento vertical a las cajas de texto (que fallo más tonto :P) y centrado del texto para ver que tal...
  - Cambiada la forma en que se realiza la búsqueda de archivos multimedia de los juegos, ahora se ignora todo lo que se encuentra detrás de "(" o "[". Por ejemplo, si se tiene:
```
      NombreFamilia - Blanco - Compañía - Año - Etiqueta - FicheroFamilia
        Nombre juego - Versión - Compañía - Año - Etiquetas - FichJuego (vers.).ext
```
Antes se buscaba "FichJuego (vers.)" y luego "FicheroFamilia", ahora lo que busca es "FichJuego" y luego "FicheroFamilia", tiene como problema que si distintas versiones tienen el mismo nombre si se le quitan los paréntesis solo pueden usar una imagen, pero así no hay que tener repetidas un montón de imágenes.
  - Mejorado un poco el gestor multimedia otra vez.
  - Ya funciona la **posibilidad de definir en cada uno de los sistemas el directorio donde descomprimir los ficheros**. Tras cerrar el emulador "intenta" borrar todos los ficheros descomprimidos.
  - **Añadido el menu _Juego_** (aunque antes se podía hacer manualmente pulsando botón derecho sobre la barra de herramientas para configurarlo) porque le he puesto dos opciones más: Elegir un juego aleatorio del sistema y elegir un juego aleatorio de entre todos los sistemas.
  - Una minucia interna para **poder añadir como sistema a Windows/MS-DOS**, es decir poder configurar un sistema en el que se busquen los .exe y el emulador sea uno en el que no se define ejecutable dejando el campo vacío y como parámetro %ROM%. Es decir, que ejecute directamente los programas (.exe, .com, .bat, etc.). Posiblemente funcione tambien con otras extensiones asociadas por el sistema operativo abriendo el archivo con el correspondiente programa (no he comprobado esto último y si no funciona tampoco lo voy a mirar mucho...).
  - Añadido **un botón** al editor de información de juegos y familias, **para grabar el árbol de juegos al fichero**. Así no hay que cambiar de sistema, abrir algún gestor o cerrar el programa para que se graben los datos del sistema actual al fichero.
  - Nueva opción del menú Emulador, para **poder ejecutar el emulador sin parámetros**. Es decir sin juego.
  - Añadido un **nuevo menú _Enlaces_**, donde pondré algunos enlaces a páginas como VGMuseum y VGMusic donde se pueden conseguir imágenes y musica relacionada con los juegos. No es nada automático y solo abre la página principal, así que las tendrás que bajar a mano :P. Lo ideal es que se pudieran arrastrar las imágenes directamente desde el navegador a la imagen principal del programa, como se puede hacer desde el explorador de ficheros (que por eso lo puse en un principio), pero creo que así no funciona.
  - He quitado la opción de mover los artículos al principio en los títulos de los juegos. No me terminaba de convencer... y mejor que estén ordenados a mano esos casos... De hecho, la opción a sido sustituida por una que realiza el efecto contrario... Es decir, pone **los artículos al final del título** respetando los subtítulos (estilo: "Zorro, El" o "Zorro 2, El: La saga continúa")

## 20080617 - Emuteca 0.3.1.12

  - Mejorado un poco más el gestor multimedia, añadiendo unos botones para mover archivos y arreglando algunos errorcillos.

## 20080617 - Emuteca 0.3.0.11

  - Añadida **ayuda emergente** a un montón de sitios (no todos).
  - Varias utilidades creadas:
    - Añadida opción para **convertir en minúsculas los nombres claves/crc32 de un sistema**. Bastante útil en Win9X/ME para cuando se quieren importar los nombres de los juegos de MAME (los usa en minúscula) mientras que Win9X automáticamente puede que trate los archivos como si estuvieran en mayúscula si cumplen el formato 8.3. Por ejemplo un caso:
      - 1º Se configura el sistema de recreativas (Juegos de MAME)
      - 2º Se actualiza la lista
      - 3º Se crea la base de datos con el propio MAME
      - 4º Se intenta importar... y no funcionará porque en Emuteca la clave está en mayúsculas y en la base de datos en minúsculas. Por tanto habrá que usar esta opción antes para que funcione bien.
    - ~~Opción para pasar los artículos ", The"; ", La"; ", El" al principio del nombre del juego (Tengo pensado hacer que sea posible ordenar los juegos ignorando los artículos y una opción que se puedan quitar o poner del principio), pero para ello mejor que estos se encuentren al principio.~~ Borrada y sutituida posteriormente por una que hace el efecto contrario.
    - **Utilidad para convertir " - " en ": "**. Para los nombres de los juegos o familias. Para cuando se lee por primera vez un directorio de archivo y no se tiene una base de datos.
    - **Opción para renombrar todos los nombres de fichero de las familias, en base a su nombre real**.
  - **¡Gestor multimedia creado!** Aunque dista mucho de funcionar a la perfección.
  - **¡¡YA NO PIERDE MEMORIA!!** (O al menos eso me dice el nuevo gestor de memoria) La culpa la tenía la librería que usaba para leer los archivos PNG y la he cambiado por otra, problema: No reconoce GIFS (La otra era una bestia parda en formatos de ficheros, pero aun así también fallaba con los GIF). Otra cosilla es que reconoce la transparencia y otro formato interesante MNG... pero pone fondo negro por defecto y queda algo feo.
  - Interno: Le he instalado el gestor de memoria FastMM478, en teoría hace operaciones de memoria más rápido; pero lo que me interesa es que me informa de la memoria no liberada por el programa...
  - Modificada la forma en que funciona el editor de datos, ahora es necesario pulsar el botón para guardar los cambios. (Y bueno... realmente hay que hacer una acción que haga que se guarde el árbol de juegos en el fichero, como cambiar de sistema o salir)
  - **Se puede arrastrar un fichero de imagen (.png, .jpg, etc.) encima de la imagen del juego** directamente para añadirla o cambiar dicha imagen. No me convence mucho...
  - Añadido un poco de actividad al importar listas, crear base datos de MAME, etc... para que se vea que no se ha colgado (a costa de que tarde más)
  - ¡¡Añadida opción para **extraer nombres y organización de MAME**!! :D Lamentablemente es bastante lento y no extrae ni la compañia, ni el año. :/ Pero se puede indicar el .edb anterior para importar esos datos de nuevo ;)
  - Iconos en el gestor de emuladores, aunque todavía le tengo que cambiar el estilo a los botones, que tiene el antiguo.
  - Añadido la cantidad de juegos de cada sistema y el número de familias. No estoy seguro si esto ralentiza mucho...
  - Gestort sistemas y Gestor emuladores: Ordenación alfabética de los sistema y emuladores (solo al abrir la ventana, no al añadir un nuevo elemento)
  - Gestort sistemas: El directorio "Texto\Informacion" cambiado por "Texto\Información"

## 20080529 - Emuteca 0.2.0.8

  - Opción "No extraer ficheros" solo activa si se tiene marcado no usar CRC32. No tiene sentido usar el CRC32 si se buscan ZIPs. Además he quitado la ayuda de que opciones son para los distintos tipos de ROM. (Ya lo pondré en la ayuda emergente)
  - Al importar datos de un sistema, se actualiza la combobox de compañías.
  - Cuando un sistema se tiene la opción "extraer todo", **en teoria ya borra todos los fichero extraidos tras ser usados** (no lo he probado fondo...)
  - Quitado el formulario "Sobre..." porque no tenía ganas de importarlo de la anterior versión (así además no me tengo que preocupar de actualizarlo a mano, tal y como lo tenía) XD.
  - **Guarda los datos al refrescar la lista**, para luego volver a recuperarlos sin perden la información de los archivos que no fueron modificados.
  - Añadido **icono del sistema** en la parte superior (32x32 si es formato .ico)
  - Cambiado de lugar el editor de los juego, apareciendo ahora a la izquierda, en vez de abajo.
  - Gestor sistemas: Añadido botón para crear y configurar automáticamente los directorios de imágenes, texto, sonido y vídeo.
  - Algunas imágenes más para los botones
  - En vez de compilarlo con Delphi 2006, ahora esta siendo escrito con **Delphi 7** porque en Win98 no funciona Delphi 2006.
  - Menús y barras de herramientas configurables. Aunque la verdad es que no hace falta para nada...)
  - Cambiados algunos atajos del teclado

## 20080413 - Emuteca 0.1.2.6 

  - Añadido este archivo. (Edición posterior: Se refiere al archivo Cambios.txt que ahora está en este HTML)
  - Cambiado la forma de numeración de versión del programa, ahora se corresponde con la interna del archivo.
  - **Quitado _Modo MAME_**: Para cambiarlo por las siguientes opciones y tener mejor control:
    - **No buscar dentro de ZIP (o 7z)**: No busca archivos con las extensiones asociadas dentro de los ficheros comprimidos (aunque sí las busca si no están comprimidas) y añade los propios zip/7z. Por ejemplo para usar con las ROMS de Recreativa.
    - **Extraer todos los archivos de los ficheros comprimidos**: En vez de extraer solo el fichero del juego seleccionado extrae todos los ficheros del ZIP. Para usar con las imágenes de CD (ISO, CUE) o los Pinball
    - **No usar CRC32** para el reconocimiento de los juegos a través de las bases de datos o al exportarlos. Por ejemplo recreativas (casi todos los emuladores usan su nombre de fichero de MAME como identificador) o las imágenes de CD.
  - Arreglado el mensaje de la primera vez que se inicia.
  - Quitado que se quite de la bandeja del sistema y se oculte en la barra de tarea (al lado del reloj) al minimizar. A ver si así se soluciona lo de las ventanas que aparecen debajo de otras existentes.
  - **Liberación de bastante memoria física al minimizar antes de ejecutar un emulador**, antes minimizaba pero no liberaba memoria, aunque se libera bastante más minimizando pulsando el botón... T\_T
  - Quitado cutre parche al leer y guardar la lista de juegos, por un error que tuve de conceptos sobre funcionamiento del componente.
  - Ahora se auto expande el árbol al pulsar sobre una familia (es una chorrada que ha llevado 0,5 segundos porque es una opción pero queda más bonito y completo este archivo)
  - Opción para **exportar la lista de juegos en otros formatos** como puede ser HTML, CVS, texto plano, texto plano UTF-16 o RTF (Esto lo tenía el VirtualTreeView)
  - Arreglado fallo al cambiar el nombre de la familia en el editor, editando el nombre de la familia directamente (no se producía cambiando la familia de un juego).

## 20080406 - Emuteca 0.01pre 

  - Versión inicial que hace lo que debe: **Descomprimir los 7z o zip y ejecutar el emulador correspondiente**.
  - **Gestor de Sistemas** básico y **Gestor de Emuladores**.
  - **Importación y Exportación de datos** de los juegos.
  - Dos modos de tipo de archivos de juegos:
    - Modo normal: Busca todos los archivos de las extensiones especificadas dentro de los zip y 7z. Y los descomprime en el momento de ejecutar. La base de datos de juegos los reconoce por CRC32 de los archivos.
    - Modo MAME: Toma los comprimidos como juegos en sí. No descomprime nada. La bases de datos los reconoce por el nombre del archivo.
  - **Agrupación por familias de juegos**, creación automática y borrado de dichas cuando se necesita.
  - Asociación y **muestra de contenido multimedia** relacionado con los juegos: 6 imagenes (1 icono) y 3 archivos de texto (que pueden ser RTF)
  - Editor básico de datos de juegos.