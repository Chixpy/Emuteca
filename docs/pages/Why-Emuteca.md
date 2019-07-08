---
layout: default
title: ¿Por qué...
---

# ¿Por qué Emuteca? #

Cuando se crea un programa alternativo a otros que ya existen de características
similares se tiene que tener unos motivos o características que provoquen la
creación y diferencien el nuevo proyecto.

En esta página pasamos a describir las características por las que se creó este
proyecto que no estaban implementadas (al menos de forma conjunta) en otro
Gestor de juegos para emuladores.

## Ejecutar juegos que estaban comprimidos en un 7z (y rar) ##

La principal razón de Emuteca cuando se comenzó a desarrollar fue la de poder
ejecutar juegos que se encontraban comprimidos con GoodMerge en 7z en emuladores
que no soporta(ban) que estuvieran así. Por aquel entonces pocos Front-End lo
soportaban, pero con el paso del tiempo el formato 7z se ha ido popularizando,
y cada vez más programas y emuladores soportan este tipo de archivo comprimido
directamente.

{% capture imagefile %}{{- site.baseurl -}}/img/WhyEmuteca/Inside7z.png{% endcapture %}
{% include image.html file=imagefile caption="ROM dentro de un 7z" %}

Aún así, a pesar que esta característica inicial esta siendo alcanzada por otros
proyectos; Emuteca permite un tratamiento particular para cada sistema:

* Extraer tan solo el archivo de la ROM que se vaya a usar, que es la forma más
  común para sistemas de cartuchos.
* Extraer todos los fichero del archivo comprimido en el que se encuentra la 
  ROM, que es útil si el software se compone de varios ficheros (Como ejemplo
  pueden ser las dos caras de una cinta, varios discos, el BIN+CUE de los CD,
  o cualquier juego de Windows o MSDOS).
* Que el propio archivo comprimido sea tratado como una ROM, principalmente
  para MAME y similares.

Además para cada sistema se puede definir una carpeta para la extracción y 
tener localizado dónde se han extraido los ficheros. Esto es útil para el 
segundo caso cuando se quiere (o el juego te pide) cambiar de cara o disco; 
y también tiene su utilidad para aquellos emuladores que no pueden cargar 
directamente las ROMs a través de la línea de comandos.

## Identificar los los juegos ##

A diferencia de TOSEC, NoIntro, GoodTools y similares; Emuteca identifica los juegos de forma semántica. No tiene inicialmente la misión de _renombrar_ archivos de forma única; sino que cada variante del juego tiene campos donde guardar sus propiedades intrínsecas.

![Propiedades de una ROM]({{- site.baseurl -}}/img/WhyEmuteca/ROMProperties.png)

La identificación se puede realizar mediante SHA1 (lo habitual con ROMs consistentes), CRC32 (en desuso, aunque útil para archivos muy grandes), nombre del fichero (MAME) o un identificador arbitrario.

La bases de datos son sencillas y editables incluso con un programa de hojas de calculo.

La verdad es que esto se trata de un trabajo titánico en si mismo. Así que a través de un script se pueden extraer e intentar analizar los .dat de TOSEC para convertilos y juntarlos al formato usado por Emuteca

## Agrupar juegos y sus versiones ##

La forma más sencilla de entender esto es pensar en una fusión entre MAME y GoodMerge.

Un grupo es el equivalente a la ROM padre de MAME, con la salvedad de que el grupo en sí no es ninguna versión del juego. Explicandolo gráficamente:

En MAME:

* Pacman (set 3) _<- Es la ROM padre_
  * Pacman (set 2) _<- Una versión_ 
  * Pacman (set 1) _<- Otra versión_
  
En Emuteca se traduce como:

* Pacman <- Es el grupo
  * Pacman (set 1) _<- Una versión_
  * Pacman (set 2) _<- Otra versión_ 
  * Pacman (set 3) _<- Otra versión_
  
![Grupo de Pang en recreativa]({{- site.baseurl -}}/img/WhyEmuteca/EmutecaGroup.png)
  
De la misma forma que en otros sistema un grupo agruparia las distintas versiones, regiones, discos, etc. tal y como hace GoodMerge.

NOTA: Esto solamente consiste en como trata Emuteca las diferentes agrupaciones de versiones, en el caso de MAME y los ficheros que usa obviamente hay que seguir sus reglas para que funcione correctamente.

Por otra parte los grupos tienen sus propias propiedades intrínsecas, como pueden ser el desarrollador y la fecha de programación.

Además los grupos definen un nombre de fichero común para los archivos auxiliares (imágenes, textos, música y vídeos) en caso de que no se encuentre la versión específica para cada versión. En el caso concreto de MAME se puede usar su identificador (en caso de ser un grupo, el identificador del juego padre) para sacar provecho de esta característica. Pero se desarrollara más adelante.

## Búsqueda de imágenes y otros medios en base al grupo al que pertence el juego ##

La relación de grupo-versiones, además permite usarla a la hora de buscar archivos relacionados. De esta forma si no se encuentra una imagen específica para un juego concreto (versión), la búsqueda se repite con el grupo al que pertenece la versión. Haciendo la siguiente secuencia.

1. Realiza una búsqueda en base al nombre del fichero de la versión (sin paréntesis)y las extensiones asociadas.
2. Si no se encuentra nada: Repite la búsqueda con el valor de la propiedad *MediaFilName* del grupo al que pertenece.

Esto además tiene la ventaja de que se reduce el número de imágenes (y otros fichero) comunes  para todas las versiones de un juego y a la vez se pueden usar ficheros específicos para alguna version concreta.

NOTA: Realmente exite una limitación respecto a esto: si el nombre del fichero de la versión es igual que el nombre de fichero usado por el grupo la búsqueda obviamente dará los mismos resultados (de hecho, Emuteca lo comprueba inicialmente si coinciden para ahorrarse una búsqueda).

## Poder tener varias imágenes (u otros ficheros) de cada tipo para cada juego o grupo ##

¿Qué pasa si un grupo o versión ha tenido dos carátulas diferentes? ¿O se quieren tener varias capturas de pantalla? ¿O se tienen varias canciones de la banda sonora del juego? ¿Y si los manuales están escaneados como un cómic (una imagen por cada página)?

Emuteca permite que por cada categoría de ficheros se puedan asignar múltiples ficheros a cada grupo o versión mediante un archivo comprimido o una carpeta.

La búsqueda se realiza añadiendo lo que encuentre de la siguiente forma:

1. Fichero con el nombre y las extensiones soportadas usado por la versión o grupo.
2. Todos los fichero con las extensiones soportadas que se encuentren en una carpeta con el nombre.
3. Si no se encuentra ninguna coincidencia, busca en todos los ficheros que se encuentren en un comprimido con ese nombre.

Si se alcanza el tercer paso los ficheros son descomprimidos en una carpeta temporal.

![Varias imágenes asignadas al manual de un juego]({{- site.baseurl -}}/img/WhyEmuteca/ManyImages.png)

NOTA: En versiones anteriores de Emuteca, también tenían la posibilidad de buscar los ficheros dentro de un archivo comprimido que contuviera todos los fichero de todos los juegos (como soporta MAME). Pero se eliminó por ser muy lento si se usa conjuntamente con lo anterior y creaba conflictos con el tercer paso.

POR DESARROLLAR: Añadir la posibilidad de que las imágenes se muestren en secuencia (slideshow).

POR DESARROLLAR: Guardar en caché la lista de archivos encontrados, y evitar realizar las misma búsquedas.

## Poder definir distintos tipos de imágenes para cada sistema ##

Esto es sencillo: Las recreativas pueden tener asociadas imágenes de los muebles, y los juegos de NES carátulas y contraportadas.

Dicho de otro modo, para cada sistema se puede definir grupos específicos y arbitrarios de ficheros (imagenes, textos, música y vídeos) de forma independiente. 

## Poder listar TODOS los juegos de TODOS los sistemas simultáneamente ##

Así como suena, obviamente puede que tarde un poco en cargar todas las lista de juegos si no se han cargado antes (¿unos 30 segundos para 200.000 versiones con un i7?)

![Arkanoid en distintos sistemas]({{- site.baseurl -}}/img/WhyEmuteca/AllSystems.png)

## El núcleo es independiente del GUI ##

El núcleo de Emuteca es independiente del GUI. Por el momento tan solo está desarrollado el front-end con estilo ventanas y que además sirve como editor.

Pero nada impide crear una aplicación SDL que use la base editada en el GUI... (En un futuro)