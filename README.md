![Emuteca 0.8 preview](../../wiki/img/curr/GameManager.png)

## English 

**Emuteca** is an emulator front-end for any system, like [QuickPlay](http://www.quickplayfrontend.com/) (or [here in GitHub](https://github.com/tonywoode/quickPlay)). Now, it's in English; and it can be translated to any language (I think so...).


Features:

  - Support for ROMs inside a `7z`, `zip` or `rar`, and it can be configured for any other format that [7zip](http://www.7-zip.org/) can handle. And then extract the ROM (or all files) before launching the emulator. Alternatively, you can use the compressed file itself as a ROM; for example, while you are using MAME.
  - Game versions are grouped together.
  - Launch any emulator from command line and some are preconfigured as example, but you can add many as you want. 
    - Some emulators don't support launch a game from command line but you can use Emuteca to decompress the ROM before lauching the emulator or only for management purposes.
  - **System Manager** lets enable, configure and add any system. It can create a folder structure (from a base folder) for each system where you can put related system files and autoconfiguring it.
  - **Emulator Manager** lets configure the command line for the emulators. Many of them are preconfigured and only is needed to set their `.exe` path.
  - **Media Manager** lets rename media files and assign them to a group or a game.
  - **Script Manager** lets create and run scripts, in [Pascal Script](https://wiki.lazarus.freepascal.org/Pascal_Script). They can be for general purporse or manipulate Emuteca's data.
    - **WARNING 1**: To create a TStringList, `CreateStringList` function must be used. If normal constructor is used it created a memory leak on object destruction.
    - **WARNING 2**: As all executable code, it can be used to do harmful things.
  - Includes some external tools (in `Tools` folder):  
    - **ETKDBEditor**: A very basic database editor of `.cvs` and `.egl`.
    - **ETKIconBorder**: To make icons and logos, and add a semitransparent border.
    - **ETKPDF2CBX**: A frontend to extract pages from PDF with `pdfimages` or `pdftopng`. (Actually this tool is from [Comicteca](https://github.com/Chixpy/Comicteca))
    - **ETKMagCut**: To extract magazine texts.
  - It's in English by default, but can be translated with any `.po` editor.

Sorry, there is not more documentation in English; if you want, you can help me to translate the documentation to any languaje.

## Spanish

**Emuteca** es un *Front-End* para emuladores de cualquier sistema, originalmente basado en [QuickPlay](http://www.quickplayfrontend.com/) ([también en GitHub](https://github.com/tonywoode/quickPlay)). 

Entre sus características están:

  - Soporte de ROM en 7z, zip o rar, y se puede configurar para soportar todos los formatos que soporta [7zip](http://www.7-zip.org/). Y extraer la ROM (o todos los ficheros) antes de lanzar el emulador. Alternativamente se usar el propio archivo comprimido como ROM; por ejemplo para MAME.
  - Las versiones de un juegos son agrupadas.
  - Lanza caulquier emulador desde la línea de comandos y algunos están preconfigurados como ejemplo, aunque se pueden añadir los que se quiera.
    - Algunos emuladores no soportan ejecutar un juego desde la línea de comandos, pero Emuteca puede ser útil para descomprimirlo en una carpeta conocida antes de lanzar el emulador o con el propósito de organizarlo. 
  - El **Gestor de Sistemas** permite activar, configurar y añadir cualquier sistema. Con él puedes crear una estructura de carpetas (desde una carpeta base) dónde poner los ficheros relativos a cada sistema, autoconfigurándolos en el proceso.
  - El **Gestor de Emuladores** permite configurar la línea de comandos de los emuladores. Bastantes están preconfigurados y sólo es necesario configurar la ruta de su executable.
  - El **Gestor de Medio** para renombrar ficheros y asignarlo a un grupo o a un juego.
  - El **Gestor de Scripts** para crear y ejecutar scripts, en [Pascal Script](https://wiki.lazarus.freepascal.org/Pascal_Script). Estos pueden ser de propósito general o manipular los datos de Emuteca.
    - **ATENCIÓN 1**: Para crear una TStringList, se debe usar la función `CreateStringList`. Si se usa el constructor normal, se creará un *leak* de memoria en su destrucción.
    - **ATENCIÓN 2**: Cómo to código ejecutable, puede usar para hacer cosas dañinas.
  - Se incluyen algunas utilidades externas (en la carpeta `Tools`):  
    - **ETKDBEditor**: Un editor muy básico de bases de datos para `.cvs` and `.egl`.
    - **ETKIconBorder**: Para crear iconos y logos, y añadir un borde semitransparente.
    - **ETKPDF2CBX**: Un *Front-End* para extraer páginas de PDF con `pdfimages` or `pdftopng`. (Realmente esta utilidad es de [Comicteca](https://github.com/Chixpy/Comicteca))
    - **ETKMagCut**: Para extraer textos de las revistas.
  - Por defecto está en Inglés, pero puede ser traducido con cualquier editor de `.po`. Y además ya está traducido al español.

Toda la demás documentación está en español en su [página web](https://chixpy.github.io/Emuteca) mientras que la [wiki en GitHub](../../wiki) tiene información más técnica.

## Noticias 

  - 20240131: Subida la versión 0.8.0.155
  - 20240110: Actualizado a Lazarus 3.0. Aunque se pueden guardar los proyectos en modo compatibilidad, los guardo en el nuevo formato.
  - 20230809: ¡WOW! Hace mucho tiempo que no actualizo esto y ha habido muchas mejoras en todo este tiempo:
    - Se han añadido [varios programas auxiliares](https://github.com/Chixpy/Emuteca/tree/master/bin/Tools): ETKDBEditor, ETKIconBorder, ETKPDF2CBX y ETKMagCut. Aunque hay que mejorar la integración con EmutecaGUI.
    - Una [página web propia](https://chixpy.github.io/Emuteca/)
    - [Repositorios auxiliares](https://chixpy.github.io/Emuteca/pages/Media-Files.html)para poder bajar imagenes, textos y demás ficheros para los sistemas.
  - 20190617: Lazarus 2.0 y soporte para elegir núcleo en emuladores multisistema.
  - 20171024: Volviendo a usar Lazarus en vez de CodeTyphon
  - 2016XXXX: Retomando un poco esto. Y continúa en 2017...
  - 20160814: Arreglando un poco las imágenes de la wiki
  - 20150316: Movido a GitHub, GoogleCode cierra :-(
  - 20120705: Subo una nueva versión beta: 0.7.0.56. Lo hago antes de acometer uno cambios significativos en la búsqueda de imágenes (complicando más el código pero facilitando su uso al fusionar ambos modos de búsqueda) e intentar mejorar la gestión de los Scripts.
  - 20120704: ¡Nuevo icono!... Bueno la verdad es que es bastante cutre... A ver si dentro de poco compilo una nueva versión esta vez beta, de la versión 0.7.XX...
  - 20120320: He cambiado el repositorio de SVN a Git, la verdad es que me da igual uno que otro y realmente es para ver como funciona.
  - 20111213: Hace más de año y medio que parece que no hay actividad puesto que no he subido nuevas versiones de Emuteca, pero la verdad es que sí que la ha habido y la versión 0.7.X está siendo desarrollada pasito a pasito. La [Lista de cambios](../../wiki/Changes-List) podrás ver las diferencias entre la versión 0.6 y la 0.7.
  - 20111008: Hace mucho que no escribo, la versión de Lazarus sigue en desarrollo y cumple su funcion, aunque le faltan por implementar cosillas que tenía Delphi tiene varios añadidos. Sin embargo, desde esta fecha indicar que no usaré Lazarus puro sino [CodeTyphon](http://www.pilotlogic.com/sitejoom/index.php/codetyphon) y además se han eliminado las librerías JEDI para manejar .7z y van a ser reemplazadas por llamadas directas a 7z.exe y 7zG.exe...
  - 20100524: Subida la versión de la rama 0.6.2.38, ([Lista de cambios](../../wiki/Changes-List)), pero se resumen en la palabra **transparencia**.
  - 20100410: Google me ha avisado de que hay un problema con las descargas, y he encontrado que se encuentra infectado con un curioso virus que afecta a los compiladores de Delphi. Y del que pondré más información posteriormente. Por el momento he borrado todas las descargas.
  - 20100202: Versión 0.6.1.36. Que arregla algunos fallitos respecto a la anterior versión. ([Lista de cambios](../../wiki/Changes-List))
  - 20100202: Lanzada la versión 0.6.0.35. ([Lista de cambios](../../wiki/Changes-List))
  - 20090814: Lanzada la versión 0.5.0.22. ([Lista de cambios](../../wiki/Changes-List))
