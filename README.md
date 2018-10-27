![Emuteca 0.7.X.X](../../wiki/img/0_7/GameManager%200_7_0_53.png)

[More screenshots / Más imágenes](../../wiki/Screenshots)

Emuteca 0.8 preview...

![Emuteca 0.8 preview](../../wiki/img/0_8/GameManager%200_8_0_119.png)

# English #
_v0.8 is being developed from scratch_; v0.7 information follows:

**Emuteca** is an emulator front-end for any system, like [QuickPlay](http://www.quickplayfrontend.com/) (or [here in GitHub](https://github.com/tonywoode/quickPlay)). Now, it's in English; and it can be translated to any language (I think so...).

It's not complete, but it's stable and usable... :-|

Features:
  * Support for ROMs inside a 7z, zip, rar, and any other format that [7zip](http://www.7-zip.org/) can handle; and extract the ROM (or all files) before launching the emulator. Alternatively, you can use the compressed file itself as a ROM; for example, while you are using MAME.
  * Group games by families or groups. In many system a ''family'' means different versions of a game; while in others it's better use families to handle the different files which belong to the same game (computer games with many discs).
  * It launchs any emulator from command line; althought only some are preconfigured as example, you can add many as you want. Some emulators don't support launch a game from command line but you can use Emuteca to decompress the ROM before lauching the emulator or only for management purposes.
  * You can add, edit, delete or disable any system with the System Manager.
  * Emulator Manager lets you to configure the command line for the emulators.
  * It has a Media Manager to rename media files and assign them to a family or a game.
  * It's in English by default, but can be translated.
  * In 0.6.X versions, created with Delphi... You can assign a video or/and a music file to a game and it can be autoplayed when selecting the game (actually, only MPEG and MIDI were tested). Someday in 0.7.X this feature will return...
  * Other little things that you may discover XD.
Some things, like video and music folders (except _demo_ folders in 0.6.X), are useless...

Although video and music previews are dropped in 0.7.X and it's main goal is port from Delphi to Lazarus, this new version had some new features:
  * Script Manager to execute Pascal scripts to do some hard work with the game list. There are some example scripts:
    * Extract data from TOSEC or GoodXXX filenames.
    * Change ' - ' title delimiter to ': '
    * Group games that are in the same archive (7z,zip,etc.) or folder (after compressed with GoodMerge)
    * Merge in the same archive (7z) all games in a group.
    * ...and some others.
    * **WARNING 1**: Scripts that create a TStringlist generate a memory leak (TStringList.Free don't work as expected O\_o ).
    * **WARNING 2**: As all executable code, it can be used to do dangerous things.
  * Game media files (images, texts, etc.) search is more flexible.
  * Icons for perfect dumps, zones, bad dumps, etc.

## Traslation ##
If you want to traslate Emuteca 0.6.XX to another language or fix any misspelled sentence:
  1. Run Emuteca at least once (this will create some config files, and the translation file of the strings used). If you want, open any other window like System Manager, Emulator Manager, etc... this action will add their strings to the traslation file too.
  1. The default file with the strings is "i18n\en.lng" and you only need to edit this file. With Emuteca is included only es.lng for Spanish, but at first run "en.lng" will be created.
  1. If you want, you can copy or rename the file and change Emuteca.ini accordily... (and send me the translation XD to include it in next version)

In version 0.7.55 and later Emuteca is translated by .po/.mo files. So, you must translate using a .po editor a file in ´locale´ subfolder; and save it with the name `Emuteca.xx.po`. `xx` is the 2 characters code of your system language (it can be `xx_XX`, too). Language can be changed by [command line](../../wiki/CommandLine).

Sorry, not more documentation in English; if you want, you can help me to translate the documentation to any languaje.

# Spanish #
_La versión 0.8 está siendo desarrollada desde cero_; a continuación información de la versión 0.7.

No es una traducción literal de lo anterior, pero como todas las páginas de la wiki están en español... [¡recórretelas!](../../wiki)

Emuteca es un interfaz (front-end) de emuladores de cualquier sistema para Windows, estilo QuickPlay. Ahora por defecto está en inglés, pero incluyo la traducción al español. Para usarlo edita el archivo "Emuteca.ini" y cambia "`File=en.lng`" por "`File=es.lng`".

Entre sus características están:
  * Soporte de ROM en 7z, zip, rar, en definitiva todos los formatos que soporta [7zip](http://www.7-zip.org/) (alternativamente también se puede hacer que los propios archivos comprimidos sean la ROM en sí, por ejemplo con MAME)
  * Agrupación de juegos por familias, y agrupar los listados por desarrollador, año y palabras clave
  * Soporta básicamente cualquier emulador que permita ejecutar juegos desde la línea de comandos.
  * Tiene gestor de sistemas para añadir, modificar y eliminar la configuración de sistemas.
  * Gestor emuladores donde definir los parámetros de la línea de comandos para ejecutar directamente el juego.
  * Gestor de archivos multimedia para renombrar los archivos correspondientes semiautomáticamente.
  * En las versiones 0.6.X que fueron compiladas con Delphi, se puede reproducir un vídeo o música asignada al juego seleccionado (solo lo he probado con MPEGs y MIDIs). Ya veremos como lo soluciono para Lazarus...

## Noticias ##
  * 20171024: Volviendo a usar Lazarus en vez de CodeTyphon
  * 2016XXXX: Retomando un poco esto. Y continúa en 2017...
  * 20160814: Arreglando un poco las imágenes de la wiki
  * 20150316: Movido a GitHub, GoogleCode cierra :-(
  * 20120705: Subo una nueva versión beta: 0.7.0.56. Lo hago antes de acometer uno cambios significativos en la búsqueda de imágenes (complicando más el código pero facilitando su uso al fusionar ambos modos de búsqueda) e intentar mejorar la gestión de los Scripts.
  * 20120704: ¡Nuevo icono!... Bueno la verdad es que es bastante cutre... A ver si dentro de poco compilo una nueva versión esta vez beta, de la versión 0.7.XX...
  * 20120320: He cambiado el repositorio de SVN a Git, la verdad es que me da igual uno que otro y realmente es para ver como funciona.
  * 20111213: Hace más de año y medio que parece que no hay actividad puesto que no he subido nuevas versiones de Emuteca, pero la verdad es que sí que la ha habido y la versión 0.7.X está siendo desarrollada pasito a pasito. La [Lista de cambios](../../wiki/Changes-List) podrás ver las diferencias entre la versión 0.6 y la 0.7.
  * 20111008: Hace mucho que no escribo, la versión de Lazarus sigue en desarrollo y cumple su funcion, aunque le faltan por implementar cosillas que tenía Delphi tiene varios añadidos. Sin embargo, desde esta fecha indicar que no usaré Lazarus puro sino [CodeTyphon](http://www.pilotlogic.com/sitejoom/index.php/codetyphon) y además se han eliminado las librerías JEDI para manejar .7z y van a ser reemplazadas por llamadas directas a 7z.exe y 7zG.exe...
  * 20100524: Subida la versión de la rama 0.6.2.38, ([Lista de cambios](../../wiki/Changes-List)), pero se resumen en la palabra **transparencia**.
  * 20100410: Google me ha avisado de que hay un problema con las descargas, y he encontrado que se encuentra infectado con un curioso virus que afecta a los compiladores de Delphi. Y del que pondré más información posteriormente. Por el momento he borrado todas las descargas.
  * 20100202: Versión 0.6.1.36. Que arregla algunos fallitos respecto a la anterior versión. ([Lista de cambios](../../wiki/Changes-List))
  * 20100202:Lanzada la versión 0.6.0.35. ([Lista de cambios](../../wiki/Changes-List))
  * 20090814:Lanzada la versión 0.5.0.22. ([Lista de cambios](../../wiki/Changes-List))
