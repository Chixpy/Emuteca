---
layout: page
title: Configurar carpetas automáticamente
EmuVer: 0.8
---

Esta página explicará de forma resumida el propósito que tienen las carpetas creadas con el botón para autoconfigurar un sistema en el Editor de Sistemas.

![Imagen del botón para atoconfigurar un sistema](img/current/AutoConfigFolders.png)

## Qué hace ese botón ##

Ese botón es para facilitar la configuración de un sistema. Una vez se tiene seleccionada una carpeta base del sistema, si se pulsa dicho botón creará un árbol de carpetas dentro de el y las autoasignará para que se usen para texto, imágenes, vídeos y música.

Esto evita la tediosa tarea de estar creando carpetas y asignandolas después para cada sistema. Usando la estructura por defecto, para algunos sistemas (p.e. Arcade) tal vez no sean del todo aplicables pero en general ayuda con la tarea.

## Carpetas por defecto ##

Si no se ha cambiado la configuración por defecto, este es el listado de carpetas que se crean al usar el botón de autoconfiguración.

* **BIOS**: Carpeta para tener una copia de seguridad de las BIOS o datos del sistema usados por los emuladores.
* **Image**: Carpeta donde están las subcarpetas de imágenes. Las carpetas creadas automáticamente se añaden a las carpetas de imágenes del sistema. Excepto la de iconos que se asigna a su lugar correspondiente.
  * **Ad**: Anuncios escaneado en la prensa.
  * **Back**: Carátulas traseras de la caja del juego.
  * **Front**: Carátulas frontales de la caja del juego.
  * **Icon**: Icono para la lista de juegos.
  * **Logo**: Logo del juego.
  * **In game**: Imágenes mientras se juega.
  * **Other**: Otro tipo de imágenes: Noticias en la prensa que no son análisis, FanArt, etc.
  * **Manual**: Manuales escaneados (en .cbX)
  * **Map**: Imágenes con mapas y guías en la prensa del juego.
  * **Media**: Imágenes del soporte físico del juego, discos, cassette, CD, DVD, etc.
  * **Review**: Análisis escaneados en la prensa con nota y valoraciones.
  * **Spine**: Laterales de la caja / Logotipos
  * **Title**: Imágenes de título.
* **Music**: Carpeta donde están las subcarpetas de música. Las carpetas creadas automáticamente se añaden a las carpetas de música del sistema. 
  * **Autoplay**: Música para reproducir en el Front-End al seleccionar un juego. (Por el momento esto no es funcional)
  * **OST**: Banda sonora del juego
* **Soft**: Carpeta para guardar los juegos/software.
* **Save**: Carpeta para los guardados intrísecos del sistema. Más que nada está pensados para las tarjetas de memoria de la PSX/PS2. Pero se puede usar para la NVRAM, SRAM, etc. si los emuladores usados son compatibles entre sí.
* **Temp**: Para usar de forma temporal o otros usos variados.
* **Text**: Carpeta donde están las subcarpetas de textos. Las carpetas creadas automáticamente se añaden a las carpetas de textos del sistema.
  * **Info**: Información general del juego.
  * **Cheat**: Anotaciones con trucos o cheats.
  * **Credit**: Listas de créditos.
  * **Manual**: Manuales transcritos en formato texto
  * **Note**: Notas personales sobre los juegos.
* **Tool**: Carpeta para guardar las utilidades relacionadas con el sistema.
* **Video**: Carpeta donde están las subcarpetas de vídeos. Las carpetas creadas automáticamente se añaden a las carpetas de vídeos del sistema. 
  * **Autoplay**: Vídeo para reproducir en el Front-End al seleccionar un juego. (Por el momento esto no es funcional)
  * **TAS**: Para guardar TAS 
  * **Speedrun**: Para Speedruns

## Cambiar la estructura por defecto ##

La estructura por defecto se puede cambiar editando el archivo `Data/SysFolders.cvs`. (Bueno, realmente es ese archivo si no se ha modificado *Emuteca.ini* ni *GUI.ini*).

Al ser un CSV, se puede abrir con editor de texto o una hoja de cálculo; asegurándonos que al guardar los valores con espacios o comas quedan entrecomillados.

La primera línea es ignorada, ya que tan sólo es una cabecera.

La estructura de las siguientes, son tres valores separados por comas:

1. Nombre de la carpeta: Si se quieren crear subcarpetas tan solo hay que escribir la ruta relativa, *Carpeta/Subcarpeta/Subsubcarpeta*.
2. Clave del tipo de carpeta donde asignarla: Un caracter que indica el tipo de contenido que tendra esa carpeta y dónde asignarla. Puede ser:
  * **Vacío**: Crea la carpeta, pero no asignar a ningún sitio.
  * **c**: Carpeta de iconos. Solo puede haber una carpeta de este tipo.
  * **i**: Se añadirá a las carpetas de imágenes del sistema.
  * **l**: Carpeta de logos. Solo puede haber una carpeta de este tipo.
  * **m**: Se añadirá a las carpetas de música del sistema.
  * **t**: Se añadirá a las carpetas de textos del sistema.
  * **v**: Se añadirá a las carpetas de vídeos del sistema.
3. Título de la carpeta: Nombre que se mostrará para las imágenes de esa carpeta.

Es recomendable que todos los campos estén entrecomillados con dobles comillas ("), aunque solo es requerido si el campo tiene espacios o comas.
  