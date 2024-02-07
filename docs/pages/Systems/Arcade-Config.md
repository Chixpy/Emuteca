---
layout: page
title: Configuración sistemas de arcade
EmuVer: 0.8
---

En este documento se indicará como configurar el sistema para arcades, centrándonos en  MAME. Puesto que los demás emuladores de recreativas usan un sistema similar ciertamente también es aplicable.

## Configuración básica 

### Requisitos 

Usando el sistema preconfigurado *Misc: Arcade*, para configurar este sistema hay que hacer tener los siguientes requisitos:

  - **ROMS comprimidas en archivos** (zip o 7z no solido). No se pueden añadir estando  descomprimidos; es decir, estando en carpetas (excepto los CHD).
  - **ROMS en formato *Split* o *No Merge*.** Es necesario que cada juego tenga su archivo por separado. En caso de estar *Merged*, Emuteca solo incluirá los juegos *padre*.
  - **MAME debe estar configurado** (para que conozca la ubicación de la carpeta de ROMS) y funcionar sin más.

Las BIOS, si se desea, pueden estar en una carpeta aparte (obviamente, con MAME configurado para que use dicha carpeta también) para que no se añadan a Emuteca.

Básicamente, esta es la forma más común que se tiene generalmente. Está bien pegar un repaso a las ROMs con [ClearMAMEPro](https://mamedev.emulab.it/clrmamepro/) o alguno similar para asegurarse que todo está correctamente.

### Añadiendo juegos

Una vez cumplido lo anterior, para añadir los juegos a Emuteca:

  - No es necesario hacerlo lo primero, pero ahorramos tiempo si configuramos y activamos primero MAME en el *Gestor de Emuladores*.
  - Hay que tener activo el sistema *Misc: Arcade* en el Gestor de Sistemas.![Imagen de la configuración del sistema Misc: Arcade](img/0_8/MiscArcadeSysMng.png)
    - Entre las opciones es importante asegurarse de tener el nombre del fichero como identificador.
    - Por defecto está puesta solo la extensión zip, si se tienen en 7z hay que añadirlo también.
    - Se puede aprovechar a configurar los diretorios de iconos, imágenes, textos y demás.
  - Ir a *Software/Añadir/Escanear archivos de carpeta...* ![Imagen del formulario para añadir desde una carpeta para Misc: Arcade](img/0_8/MiscArcadeAddFolder.png)
    - Seleccionar el Sistema _Misc: Arcade_.
    - Seleccionar la carpeta donde se encuentran las ROMs.
    - Desactivar *Buscar en subcarpetas*. (No es esencial pero puede ahorrar tiempo si se tienen muchas subcarpetas con CHDs)
    - Activar *No buscar en archivos comprimidos* (No es esencial, ya si la extensión zip/7z está reconocida por el sistema entonces no va a intentar buscar dentro de esos archivos)
    - El agrupamiento automático es indiferente puesto que lo vamos a corregir con la base de datos en el siguiente paso.
  - Como el listado de los archivos queda muy feo, lo siguiente consiste en importar la base de datos de MAME que hay en la carpeta *databases*...![Imagen del formulario para importar datos Misc: Arcade](img/0_8/MiscArcadeImportData.png)
    - Como se usa como clave el nombre de los ficheros usados por MAME. Se puede importar directamente sin tener que esperar a que se generen todos los SHA1. 😊
    - Sin embargo, si la base de datos no se corresponde con la versión de las ROMs usada es recomendable crear una nueva base de datos con el ejecutable de MAME que se tenga, tal y como se explica más adelante.

### Detalles a tener en cuenta 

En Emuteca no existe el concepto de *juego padre* como en MAME; sino que son grupos de juegos con sus distintas versiones. Por tanto, la ROM padre se lista como una versión más sin destacarla sobre las demás.

Por otra parte, Emuteca NO realiza un escaneo profundo de los juegos y sus archivos. Es decir, no comprueba si las ROMs están completas o tienen todas las dependencias necesarias para funcionar. Al igual que el resto de sistemas, se limita a añadir los archivos que encuentre que tengan las extensiones soportadas por el sistema.

Emuteca, desde la versión 0.8, **NO** soporta la búsqueda de imagenes, texto y demás multimedia que estén dentro de un único zip y puede ser bastante común que se tenga de esta forma (p.ej., *snap\snap.zip*). Esto es así por ser muy lento si se usa junto a los otros métodos de búsqueda que tiene Emuteca. Por tanto estos deben estar descomprimidos, pudiendo estar cada juego en carpetas separadas (que es como guarda las capturas MAME por defecto).

También respecto a esto, a diferencia de otros Front-End, Emuteca soporta asignar varias imágenes tanto a un grupo (juego) como a una versión. Esto hace que en algunos packs de imágenes, vamos a poner por ejemlo *Bosses*, las imágenes que pueden corresponder a un juego (y todas sus versiones) estén repartidos entre todas las versiones hijo (un *boss* por hijo), cuando en Emuteca se pueden asignar todas a un mismo juego.

### Crear una base de datos específica para importar 

#### Método antiguo 

  - ***NOTA:*** Ya se pueden extraer los datos de MAME a partir del XML generado con ´-listXML´. A su vez es descargable directamente de la [página de descargas de MAME](https://www.mamedev.org/release.html) sin necesidad de usar la línea de comandos. Así que este método no se recomienda usarlo

En principio Emuteca incluye una base de datos de MAME que muy posiblemente se quede anticuada, y por tanto con el transcurso de las versiones algunos juegos han ido cambiando el identificador, puede que falten o no coincidan correctamente.

Para ello se puede usar el script *MAMEImport.pas* en el *Script Manager*:

![Imagen del Script Manager](img/0_8/MiscArcadeScriptMng.png)

Sin embargo, para poder usarse, primeramente hay que usar el ejecutable de MAME para generar un par de ficheros con los datos específicos de dicha versión. Para ello, desde la consola se deben ejecutar los siguientes comando para generarlos.

  - ´<mame.exe> -listfull > MAMEfull.txt´
  - ´<mame.exe> -listclones > MAMEclones.txt´

Y con esto se generarán los dos ficheros que te pedirá el script al ser ejecutado. También se te preguntará por un archivo de salida que será el que deba ser usado para la importación de los datos de los juegos.

