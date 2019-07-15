---
layout: default
title: Cuadros de diálogo
---

Los cuadros de diálogo diálogo de Emuteca intentan ser lo más sencillos y autoexplicativos posibles.

Además la forma en que han sido programados permite que se puedan mostrar en su ventana independiente o incrustados dentro de otros cuadros de diálogo. Por ejemplo el [Gestor de Emuladores](Dialogs/EmulatorManager) incluye el [Editor de Emuladores](Dialogs/EmulatorEditor) que este a su vez está dividido en otros dos cuadros de diálogo incrustados para las opciones básicas y avanzadas.

Esto hace que haya cuadros de diálogo que por el momento solo se usan incrustados o solo con ventana propia; pero para el programa funcionan de la misma forma. De hecho, internamente hay una jerarquía más compleja de *frames* para la reutilización de componentes.

## Propiedades de cuadros de diálogo ##

Para más información sobre las propiedades y funcionamiento de algún cuadro de diálogo específico ve a su pantalla correspondiente:

* [Añadir Software](Dialogs/AddSoftware)
* [Añadir Carpeta](Dialogs/AddFolder)
* [Editor de Software](Dialogs/SoftwareEditor)
* [Importar datos del Software](Dialogs/ImportSoftData)
* [Exportar datos del Software](Dialogs/ExportSoftData)
* [Gestor de Sistemas](Dialogs/SystemManager)
* [Editor de Sistemas](Dialogs/SystemEditor)
* [Gestor de Emuladores](Dialogs/EmulatorManager)
* [Editor de Emuladores](Dialogs/EmulatorEditor)
* [Gestor de Multimedia](Dialogs/MediaManager)
* [Gestor de Scripts](Dialogs/ScriptsManager)

## Las ventanas en general ##

Cuando los cuadros de diálogo se muestran en una ventana, en la parte inferior pueden aparecer dos botones y un CheckBox:

{% capture imagefile -%}{{- site.baseurl -}}/img/Dialogs/AddFolder.png{%- endcapture -%}
{%- include image.html file=imagefile caption="Ventana de cuadro de diálogo" %}

Botón Aceptar
: Como es evidente, acepta y guarda los cambios realizados.
Botón Cancelar
: Cancela los cambios sin guardar.
CheckBox Cerrar
: Hace que se cierre la ventana al pulsar los botones anteriores.
: A veces puede ser útil querer guardar los cambios sin querer cerrar la ventana. Por ejemplo, si se quieren importar los datos de varios sistema sin tener que volver a abrir el cuadro de diálogo varias veces.
: Cuando está desactivado, el botón Aceptar guarda los cambios y por tanto el botón Cancelar no cancelará los cambios ya guardados.

## Cuadros incrustados ##

Los cuadros incrustados ya no tienen la CheckBox para cerrarlos; y pueden no tener tampoco los botones si el componente donde están controla en cascada cuando se aceptan o cancelan los cambios.

{% capture imagefile -%}{{- site.baseurl -}}/img/Dialogs/AddSoftware.png{%- endcapture -%}
{%- include image.html file=imagefile caption="Editor de Software incrustado en Añadir software" %}

En la imagen el [Editor de Software](Dialogs/SoftwareEditor) está incrustado en la ventana de [Añadir Software](Dialogs/AddSoftware) pero solo se muestran los botones del *Editor de Software* para aceptar todos los cambios.

{% capture imagefile -%}{{- site.baseurl -}}/img/Dialogs/AddSoftware.png{%- endcapture -%}
{%- include image.html file=imagefile caption="Editor de Software incrustado en Añadir software" %}

Sin embargo, en el [Gestor de Sistemas](Dialogs/SystemManager) que tiene incrustado el [Editor de Sistemas](Dialogs/SystemEditor):

{% capture imagefile -%}{{- site.baseurl -}}/img/Dialogs/SystemManager.png{%- endcapture -%}
{%- include image.html file=imagefile caption="Editor de Sistemas incrustado en Gestor de Sistemas" %}

Se puede ver que el [Editor de Sistemas](Dialogs/SystemEditor) tiene sus propios botones para guardar o cancelar los cambios del sistema seleccionado. Y además recuerda que si quieres guardar los cambios debes hacerlo expresamente antes de seleccionar otro sistema. 