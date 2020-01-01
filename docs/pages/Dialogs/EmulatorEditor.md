---
layout: page
title: Editor de Emuladores
EmuVer: 0.8
---

El Editor de Emuladores es el cuadro de diálogo donde se pueden configurar los distintos emuladores.

El editor se divide en dos pestañas:
 
* La primera para la configuración básica con las opciones comunes. 
* La segunda para la configuración avanzada de emuladores multisistema, parámetros que dependen del tipo de archivo y parámetros extra provenientes de la ROM.

## Botones ##

El editor incluye una barra de botones... con un sólo botón que sirve para abrir la carpeta dónde se encuentra el emulador, si la ruta del ejecutable está definida.

Por el momento, nada más.

## Pestaña Básica ## 

{% capture imagefile -%}{{- site.baseurl -}}/img/Dialogs/EmulatorEditor1.png{%- endcapture -%}
{%- include image.html file=imagefile caption="Editor de Emuladores, pestaña Básica" %}


Las opciones que se pueden configurar son las siguientes:

* Nombre del emulador.
* Desarrollador: Para dar crédito a sus desarrolladores.
* Página web del proyecto, con un botón para abrir el enlace en un navegador
* **La ruta del ejecutable**: Si el emulador que vas a utilizar es uno de los que ya vienen predefinidos con Emuteca, posiblemente sea el único valor que hay que configurar para que todo funcione. Te puedes ayudar del botón para buscarlo a través de un cuadro de diálogo.
* La carpeta de trabajo: Consiste en la carpeta desde dónde se va a ejecutar el emulador. Si bien se puede definir manualmente se pueden usar las siguientes variables que se pueden añadir automáticamente con el botón a la izquierda de la caja de edición:
  * `%EMUDIR%`: Es la más común, con ella se define con propia carpeta dónde se encuentra el ejecutable automáticamente.
  * `%ROMDIR%`: Mucho menos común, sirve para usar la carpeta dónde se encuentre la ROM. El ejemplo más claro de su uso es para los juegos de Windows.
  * `%CURRENTDIR%`: Realmente no se usa y está solo por completitud. Y define el directorio actual donde se encuentre Emuteca. Dicho de otra forma, el directorio base de Emuteca.
* Los parámetros que hay que pasar al emulador para que ejecute la ROM, también tiene sus propias variables. Como recordatorio indicar que posiblemente sea necesario entrecomillar, con dobles comillas `"`. Las variables que se pueden usar y también se pueden añadir con el botón a la izquierda de la caja de edición:
  * `%ROM%` = Ruta completa de la ROM, con unidad, directorio, nombre del fichero y extensión. El más común de usar: `"%ROM%"`.
  * `%ROMNAME%` = Nombre del fichero CON extensión.
  * `%ROMNAMENOEXT%` = Nombre del fichero SIN extensión.
  * `%ROMEXT%` = Extensión de la ROM.
  * `%SYSID%` = Párametro avanzado obtenido del sistema de la ROM. Usado habitualmente en los emuladores multisistema para elegir el núcleo a usar con la ROM.
  * `%EXTPARAM%` = Párametro avanzado en base a la extensión de la ROM. Para emuladores que necesitan distintos parámetros dependiendo del tipo de fichero que sea la ROM.
  * `%EXTRA%` = Párametros avanzados definidos en la propia ROM, normalmente para que arranque directamente. Por ejemplo, para definir el mapa en elos niveles de Doom o la orden para cargar el juego (si el emulador soporta pasarlo como parámetro)
* El icono del emulador.
  
 ## Pestaña Avanzada ## 

{% capture imagefile -%}{{- site.baseurl -}}/img/Dialogs/EmulatorEditor2.png{%- endcapture -%}
{%- include image.html file=imagefile caption="Editor de Emuladores, pestaña Avanzada" %}

Esta pestaña sirve para configurar los parárametros más avanzados para los emuladores multisistema, parámetros que dependen tipo de archivo de la ROM y los parámetros que dependen directamente de la ROM.

* Parámetro para emulador multisitema
  * Identificador del emulador: Se trata de un identificador usado para que el sistema pueda definir el núcleo a usar en el emulador.
  * Formato del parámetros: Formato a usar para añadir el parámetro del núcleo para el emulador. La variable `%SYSID%` es usada para sustuirla por el valor ofrecido por el sistema.
* Parámetros según la extesión: Aquí se definen los parámetros del emulador que dependen del tipo de archivo de la ROM. Se pueden definir varias líneas con un formato que consiste en `<ext>[,<ext>]=<parámetros>`
  * _<ext>_ es la extensión de los ficheros, sin el punto. Se puede hacer una lista separada por comas. Si la extensión contiene espacios se puede entrecomillar con comillas dobles (`"`).
  * Tras la lista de extensiones, un signo igual (`=`).
  * Luego van los parámetros a utilizar con dichas extensiones, aquí se pueden usar las mismas variables que el los parámetros de la pestaña básica.
* Parámetros extra de la ROM: Parámetros a usar con los valores extra de los ROM si están definidos. Además de poder usar las variables de los parámetros de la pestaña básica, se puede usar `%n%` (_n_ = un número desde 0), para ser sustituida por el valor que se encuentre en la línea correspondiente de la lista que tiene la ROM (La numeración comienza en 0).

 