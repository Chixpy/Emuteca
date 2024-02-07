---
layout: page
title: Configuración sistemas de cartuchos
EmuVer: 0.8
---

En este documento se indicará como se puede usar Emuteca para lanzar los distintos mapas de Quake y sus derivados. La idea de manejar este sistema es muy similar a la usada para los [mapas de Doom](v0_8-Doom-Config), aunque difiere en algunos detalles.

## Requisitos

  * Un *source port* del motor de Quake, por ejemplo QuakeSpam.
  * Los .pak (o los archivos extraidos) de la versión registrada de Quake (o derivado). 

## Recomendaciones

El problema con el que nos encontramos es que los mapas han de estar en un sitio muy particular: *id1/maps* en el mismo directorio dónde se encuentren los .pak principales. Si se tienen los archivos extraidos, ese directorio existe y se encuentran los mapas originales del juego liando un poco más el asunto...

Bien..., pues la opción más recomendable, por increible que parezca, consiste en tener dos copias de los archivos de la versión registrada.

  * Una junto al motor de Quake. Así además de tener una copia de seguridad, se puede ejecutar el emulador sólo para jugar al Quake de forma normal.
  * La otra en otro directorio (dentro del directorio base del sistema) para usarla con los mapas de usuario. Aunque hay que diferenciar como se van tener los mapas.
    * Si se van a tener en formato comprimido: En una carpeta temporal y hacer que se descompriman en la subcarpeta correspondiente. 
    * Si se van a tener en formato descomprimidos: Si se van a tener descomprimidos se puede tener en la carpeta del software y meter los mapas en la subcarpeta *id1/maps* directamente.
  
Además hay que tener en cuenta que hay que adaptar a mano la línea de parámetros de emulador para que apunte como directorio base al directorio de la copia de los archivos de Quake.

### Detalles a tener en cuenta

Lo habitual es encontrar los mapas de Quake .bsp junto a un .txt dónde se incluye la información del mapa. Se puede definir el directorio para los textos de información el mismo dónde están los juegos... pero lo recomendable sería mover dichos archivos a un directorio dedicado a ello como en otros sistemas (*Text/Info*).

Bueno, pues por el momento solo se han comentado *mapas* en general. ¿Que pasa con los otros tipos de archivos que modifican otras cosas? Bueno, pues no se ha investigado asunto... y también hay que mirar lo de los mods, bots, ... etc, etc.
