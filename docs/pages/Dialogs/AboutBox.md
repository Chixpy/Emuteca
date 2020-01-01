---
layout: page
title: Sobre...
EmuVer: 0.8
---
La ventana *Sobre... Emuteca* muestra información sobre el programa y otros datos.

{% capture imagefile -%}{{- site.baseurl -}}/img/Dialogs/AboutBox.png{%- endcapture -%}
{%- include image.html file=imagefile caption="Sobre... Emuteca" %}

En esta ventana, en la parte superior se puede comprobar la versión de Emuteca
GUI, las versiones internas del producto y del fichero ejecutable.

En especial mencionar que el último número de la versión es el número de veces
que el programa ha sido construido y no se reiniciará a cero en cada versión.

Posteriormente se muestra la información sobre la compilación:

* Sistema para el que ha sido compilado y número de bits.
* Versiones de Free Pascal y LCL de Lázarus.
* Widget Set usado
* Fecha y hora de la compilación

En el siguiente cuadro muestra las extensiones de imágenes reconocidas y soportadas por el programa. Pero no confundir con los extensiones que se usaran a la hora de buscar imágenes, ya que eso se define a parte en el fichero de configuración (entre otras cosas para reducir la lista y agilizar el proceso).

Finalmente en el cuadro de texto, se muestran datos internos del núcleo de Emuteca:

* Número de Sistemas (tanto los activos como el total).
* Número de Emuladores (idem).
* Número de grupos cargados en memoria (tanto los usados y el total).
* Número de juegos cargados en memoria (Aunque el número de visibles y total debería ser el mismo).

Respecto a los grupos y los juegos depende directamente de los sistema cargados (que es diferente al número de sistemas activos) ya que lo hacen dinámicamente según se necesite.

Además en el apartado de información de la interfaz se muestra:

* La cantidad de iconos cargados de grupos y juegos (se cargan automáticamente según se necesiten para la lista de juegos).
* La cantidad de iconos de zonas.
* Y el número de iconos cargado para las distintas versiones.
