---
layout: page
title: Instalación
EmuVer: 0.7
---

***Esto es una copia de las instrucciones de instalación de la versión 0.7***

***Aunque mayormente es válido su contenido, falta por pegar un repaso***

---

En esta página se describen los procesos de instalación, desinstalación y actualización de Emuteca. Así como los ficheros y carpetas relacionados con el programa.

## Instalación

La instalación de Emuteca consiste simplemente en descomprimir el fichero .7z en la carpeta que se prefiera. La carpeta debe ser editable por el programa ya que Emuteca creará y modificará ficheros dentro de dicha carpeta.

No hay versión con instalador, así que tienes la libertad de hacer un acceso directo al programa dónde más te guste.

Si no sabes como descomprimir un archivo .7z ~~cómprate una máquina de escribir~~ prueba a usar [7-zip](http://www.7-zip.org), [PeaZip](http://peazip.sourceforge.net) o WinRar para descomprimirlo.

Si no sabes hacer un acceso directo, ahora sí, cómprate una máquina de escribir y déjame en paz.

## Desinstalación

Para desinstalar el programa simplemente borra la carpeta dónde _instalaste_ Emuteca... Aseguraté de que no guardaste ahí algo importate que quisieras mantener como por ejemplo los datos exportados de los sistemas.

El programa por sí mismo no _debería_ haber creado ningún archivo fuera de su directorio durante su uso (excepto de forma temporal o se le indique explicitamente para otras cosas). Tampoco modifica el registro de Windows.

## Actualización

Los archivos de datos de la versión 0.8 (lista de juegos, versiones y bases de datos de importación) son incompatibles con las versiones anteriores.

Por tanto, no se pueden usar los ficheros de dichas versiones. Sin embargo, los formatos de fichero usados para esta versión puede que sean casi definitivos.

## Ficheros

Esto es una descripción de los distintos ficheros que se pueden encontrar en la carpeta del programa.

  - **Data**: Directorio donde se guardan las listas de juegos añadidos a Emuteca.
    - ***.egl**: Lista de versiones de juegos de cada sistema. Formato CVS que se puede abrir con una hoja de cálculo.
    - ***.cvs**: Lista de versiones de juegos de cada sistema. Formato CVS.  
  - **Config**: Directorio dónde se guardan las configuraciones.
    - **Emulators.ini**: Archivo donde se guarda la lista de emuladores y sus configuraciones.
    - **Systems.ini**: Fichero donde se guarda la lista de sistemas y sus configuraciones.  
  - **Databases**
    - ***.egl** y ***.cvs**: Bases de datos para importar la información de los juegos para los diferentes sistemas.
  - **Help**: Directorio dónde habrá algo de ayuda y sobre la licencia, supongo.
  - **Images**: Directorio con las imágenes usadas por Emuteca.
    - **Default**: Iconos e imágenes por defecto para juegos, sistemas y emuladores.
    - **Flag**: Banderas para los iconos de la zona. Lee el archivo _readme.txt_ para obtener más información sobre ellos.
    - **Icons**: Directorio donde poner los iconos para las barras de herramientas y menús. Lee el archivo _Iconos.txt_ para tener más información sobre ellos.
    - **VerInfo**: Imagenes para los flags de los juegos: Volcado verificado o incorrecto, hackeado, traducido, etc.
  - **locales**: Directorio dónde se encuentran las traducciones de la interfaz.
  - **Scripts**: Directorio dónde se encuentran los scripts, para su respectivo gestor.
  - **LazEmuteca.exe**: Un fichero que no sirve para nada, lo puedes borrar ;P
  - **`*`.ini**: Archivos para configurar los diferentes.