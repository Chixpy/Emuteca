---
layout: page
title: Configuración mapas de Doom
EmuVer: 0.8
---

En este documento se indicará como se puede usar Emuteca para lanzar los distintos mapas de Doom y sus derivados (Doom II, Heretic, Hexen, Strife, etc.).

## Requisitos

  - Un *source port* del motor de Doom, por ejemplo QZDoom.
  - El .wad de la versión registrada del juego del que queremos jugar los mapas. Normalmente en el directorio dónde se encuentre el motor de Doom. Si no se tiene el correspondiente archivo para cada juegos, no se podrán cargar los mapas de usuario.

## Recomendaciones

Aunque el mismo "emulador" sirva para jugar a los mapas de todos los juegos derivados de Doom. Se necesita ejecutar con un parámetro diferente (el .wad pricipal) y por tanto hay que tener configurado un emulador diferente para cada juego cambiando dicho parámetro. 

No es necesario tener varias copias por separado del ejecutable, pudiendo configurar dichos emuladores a un mismo ejecutable.

En general, todos los motores de Doom reconocen los mismos parámetros básicos que tenía el Doom original. Aunque Emuteca incluya como ejemplo la configuración necesaria para ZDoom, realmente se puede asignar cualquier ejecutable de otro motor de Doom (por ejemplo, Risen3D o QZDoom). Y a no ser que se quieran tener varios motores, no hace falta crear otro nuevos y copiar su configuración.

De la misma forma y por organización, también es recomendable tener configurado un sistema diferente para los mapas de cada por cada juego y tratarlo, en general, de la misma forma que un sistema normal... Excepto un detalle a tener en cuenta que se desarrolla en la siguiente sección.

## Detalles a tener en cuenta

### Número de mapa

Emuteca no es un front-end dedicado en exclusiva a este tipo de "sistemas", como puede ser DoomLauncher. 

Por tanto, hay que tener en cuenta es que los mapas de usuario sustituyen uno o más mapas del juego en sí, pudiendo ser el primero o el último. Y realmente para ejecutar directamente el mapa cargado además hay que indicarle el mapa sustituido para empezar a jugar directamente a él.

Por fin, en Emuteca, ya se pueden definir parámetros adicionales para cada "software" (mapa) que se pueden añadir a los parámetros del emulador (motor de Doom) y de esta forma comience en el mapa correspondiente.

De todas formas, en caso de que no tenga el parámetro definido o sea erróneo; la forma de saltar al número de mapa correspondiente se hace tecleando los siguientes trucos:

  - Doom / Doom II: `idclev##`
  - Heretic: `engage##`
  - Hexen: `visit##`
  - Strife: `rift##`

Por otro lado, lo habitual es encontrar los mapas de Doom .wad junto a un .txt dónde se incluye la información del mapa. Se puede definir que el directorio para los textos de información sea el mismo dónde están los juegos... pero lo recomendable sería mover dichos archivos a un directorio dedicado a ello como en otros sistemas (*Text/Info*).

### .wad con sonidos o texturas

Bueno, pues por el momento solo se han comentado *mapas* en general... ¿Que pasa con los otros tipos de wads que modifican otras cosas? 

Bueno, se pueden cargar y aquellos que modifican sprites o músicas; y deberían funcionar. La única limitación es que Emuteca solo va a cargar un .wad y no va aglutinar varios simultáneamente (a no ser que se ponga a mano en la configuración del emulador)