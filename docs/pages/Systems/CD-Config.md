---
layout: page
title: Configuración sistemas de cartuchos
EmuVer: 0.8
---

En esta página vamos a tratar como configurar aquellos sistema basados en CDs, DVD, BluRay u otro sistema de almacenamiento de gran capacidad *no modificable* de forma genérica. Para simplificar, se usará el término "disco" para referirnos a ellos.

Aunque en principio debería ser tan sencillo como el uso de cartuchos (por no ser modificables), lidiar con ellos en Emuteca puede ser convertirse en algo complicado si no se tiene claro su uso, organización y funcionamiento.


## Problemas

Con los sistemas que usan esta forma de almacenamiento se nos plantean varios problemas:

  - Existen bastantes formatos genéricos (.bin, .mdf, .iso, etc.) que se pueden usar para guardar una misma copia del contenido. Habitualmente se les llama "archivos imagen".
  - El tamaño de los archivos imagen hace que sea muy lento obtener un hash (como CRC32) identificativo de ellos y su descompresión cuando se usan.
  - Realmente cada archivo suele ser la copia de una pista del disco, y por tanto si son varios archivos para las distintas pistas (datos, músicas, subcanal, etc.) suele ser necesario usar un archivo índice (.cue, .mds, .toc) que indican la posición de cada pista. Siendo habitual tener que usar estos archivos índice con el emulador y no los archivos imagen.
  - Estos archivos índice no son nada consistentes en CRC32 ya que dependen del nombre de los archivos imagen asociado... Y realmente lo que Emuteca usa para identificar los juegos son los propios archivo imagen.
  - De igual forma hay que tener en cuenta que los detalles de [sistemas que usan cartuchos](Cart-Config) también son aplicables a este tipo de sistemas.
  - Para finalizar... un mismo juego puede consistir en varios discos distintos (sobre todo si son CD).


### Formatos de los ficheros

Como suele ser habitual lo mejor es atenerse a un mismo tipo de formato para los ficheros.

A diferencia de los cartuchos, que suelen tener un formato propio para cada sistema, en esta ocasión los formatos usados suelen ser genéricos para el medio físico que representan.

Realmente cada tipo de fichero tiene sus ventajas y desventajas, que hay que valorar a la hora de decantarse por un tipo o por otro.

### El tamaño
  
### Archivos índice

Una de las primeras cosas que hay que entender, es que los archivos índice hacen referencia a los otros archivos de los que se compone el disco. 

Es decir, si un disco se compone de un .cue y varios .bin, .mp3 o .sub; renombrar algún .bin hará que deje de funcionar si no se ha actualizado el .cue en consecuencia. Y por tanto el checksum de estos ficheros depende del nombre de los .bin. 

Lo mismo sucede con .mdf y .mds (y estos peor el renombrado porque no se pueden actualizar manualmente).

Esto hace que no se puedan usar para identificar mediante estos fichero, pero son los necesarios para pasar al emulador correspondiente.

Por suerte, el formato .iso suele ser un único archivo (aunque puede ir acompañando a un .cue). Por desgracia, no soporta todos los formatos de este tipo de soporte.


### Identificación de juegos

Puesto que no se puede usar el fichero índice para identificar el juego en base a su checksum, la única alternativa que queda para hacerlo antes de tener que hacerlo de forma manual es usar los fichero de datos como tal (.bin, .mdf, etc.).

Realmente, debido a una serie de factores esto tampoco es garantía, ya que un mismo disco ripeado en diferentes programas o lectores puede que resulte en los mismos exactos ficheros. Pero hay que darlo como válido.

Al usar este tipo de ficheros, y como normalmente se corresponden con una pista, implica que Emuteca reconocerá cada una de las pistas como una versión "diferente" del juego. Dicho de otra forma, listará todas las pistas que compongan el disco (excepto .mp3 y sub).


### Detalles similares a los cartuchos

Por no repetir y no tener que copiar cualquier modificación; es mucho más sencillo que vayas a la página dónde se describen los [sistemas que usan cartuchos](Cart-Config) para ver las cosas que hay que tener en cuenta que también son aplicables a los sistemas que usan este tipo de soporte.

### Varios discos

## Resumen

Resumiendo la forma recomendada de gestionar este tipo de sistemas es la siguiente:

  - Un solo tipo de formato de archivos si es posible
  - 
