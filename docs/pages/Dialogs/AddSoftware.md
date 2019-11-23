---
layout: page
title: Añadir software
EmuVer: 0.8
---
La ventana *Añadir software* sirve para añadir juegos a la lista de forma individual, posiblemente sea usada sólo de forma esporádica y puntual.

{% capture imagefile -%}{{- site.baseurl -}}/img/Dialogs/AddSoftware.png{%- endcapture -%}
{%- include image.html file=imagefile caption="Añadir software" %}

Para ello se selecciona el sistema, y mostrará la lista de extensiones soportadas por dicho sistema.

Entonces podremos seleccionar un fichero, que tenga una extensión soportada o de fichero comprimido.

Si una vez elegido el fichero se trata de un archivo comprimido, se puede marcar la casilla correspondiente para poder elegir uno de los ficheros que este contiene dentro.

Al elegir un fichero Emuteca además comprueba si el fichero ya se encuentra en la lista de dicho sistema, mostrando un aviso al respecto.

Con esto ya se puede añadir el fichero seleccionado y será añadido.

En la parte derecha se encuentra el [Editor de Software](Dialogs/SoftwareEditor) donde se puede editar la información del juego. Aunque si se desea se puede editar posteriormente una vez está añadido a la lista.

