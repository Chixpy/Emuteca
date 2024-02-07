---
layout: page
title: Primeros pasos
EmuVer: 0.8
---

***¡Esto esta incompleto!***

Esta página es una miniguía para cagaprisas que quieren probar el programa de
forma rápida y con el mínimo esfuerzo en la configuración para que funcione.

Por tanto se explicará de forma rápida como hacer funcionar y la filosofía de 

## Instalación de Emuteca

Bueno, realmente lo primero que hay que hacer es [descargar Emuteca](https://github.com/Chixpy/Emuteca/releases). 

Una vez descargado la instalación de Emuteca consiste simplemente en descomprimir el fichero .7z en la carpeta que se prefiera. La carpeta debe ser editable por el programa ya que Emuteca creará y modificará ficheros dentro de dicha carpeta.

Para más detalles: [Instalación](v0_8-Installation)

## Gestor de Sistemas

En la primer ejecución (o cuando no haya ningún sistema activo), el primer formulario que se presentará será el Gestor de Sistemas. De esta forma se podrán activar y configurar inicialmente los sistemas que se quieran tener.

De todas formas, se puede cambiar en cualquier momento a través del mencionado Gestor de Sistemas en el menú principal.

Así que este formulario activa al menos uno de los sistemas, marcando la casilla correspondiente en el listado de la izquierda. Usaremos como ejemplo "Nintendo - NES". Por el momento, y como ejemplo, no hace falta configurar nada más; los sistema predefinidos ya vienen con el listado de extensiones preconfiguradas.

## Añadir juegos

Una vez está activado al menos un sistema. Lo siguiente sería añadir juegos a sistema seleccionado.

Para ello, se selecciona ´Archivo/Añadir.../Añadir juegos de una carpeta...´.

Se presentará un nuevo formulario, en que tan solo hay que elegir el sistema (que previamente debe estar activado) y la carpeta que se quiere escanear. Además hay otras opciones, si buscar en subdirectorios o dentro de archivos comprimidos, además para seleccionar el modo en que se agruparan los archivos y qué hacer en caso de que el archivo ya esté añadido.

Una vez se acepte, el programa buscará los archivos; dependiendo de la cantidad y otros factores puede tardar un rato, pero no suele ser mucho.

Cuando termine, muy posiblemente ya aparezca un listado de juegos al seleccionar el sistema al que se han añadido.

El siguiente paso consistiría... en importar los datos de los juegos. Pero en muchos sistemas que usan SHA1, para poder hacerlo de forma completa hay que esperar a que se calcule el SHA1 y esto puede tardar mucho tiempo (sobre todo si los juegos están comprimidos en 7z, pueden ser hasta horas). Así que se explicará después.

Por suerte, esto se realiza en segundo plano y mientras se pueden hacer otras cosas como seguir configurando o jugar; incluso salir de Emuteca y el proceso será retomado al volver a ejecutarla.

## Gestor de Emuladores ##

Por el momento ya tenemos un sistema y un listado de juegos (independientemente de si ya se han importado los datos de los juegos). Falta lo más importante configurar un emulador y asignarlo a un sistema.

Para ello hay que abrir el Gestor de Emuladores, y a al igual que el Gestor de Sistemas, activar los emuladores que se vayan a usar. Además de eso también hay que configurarlos seleccionando la ruta del ejecutable correspondiente. (IMPORTANTE: Recuerda pulsar `Aceptar` para guardar los cambios antes de cambiar de emulador o salir del gestor). Las demás opciones ya vienen preconfiguradas.

Normalmente, los sistemas ya tienen asignados sus correspondiente emuladores; sino habría que volver al Gestor de Sistemas para seleccionar para cada sistema los emuladores correspondientes.

En este momento... ¡¡A JUGAR!!