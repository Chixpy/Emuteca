---
layout: page
title: Notas varias
EmuVer: 0.8
---

Aquí hay alguna cosilla aleatoría sobre el programa y funcionamiento antes de ponerlo dónde le correspondería.

## Títulos, identificadores y claves de ordenación ##

Ficheros que contienen varios juegos y compilaciones:
  * Hay que diferenciar si realmente se trata de un HUB como tal o simplemente juegos puestos sin más. Si los juegos ni siquiera tienen un menú donde seleccionarlos y son pocos listarlos como tal. Separados por ' + ' (¿Posiblemente '; ' sea mejor?)
  * A Emuteca le interesan los juegos en sí, y si se encuentran en una compilación física pero cada juego es un archivo diferente serían la versión incluida en dicha compilación.
  * Si un fichero contiene varios juegos, los grupos contienen los juegos ordenados alfabéticamente mientras que el título del software puede representar el orden dentro del fichero (por ejemplo en cintas de cassete).
  
Títulos:
  * Título del juego... difícil ¿eh?
  * En caso de incongruencia elegir el título más completo con subtítulo.
  * Respetar el formato de numeración usado.
  * En el lenguaje original y caracteres latinos.
  * Subtítulos separados por ': '.
  
Identificador:
  * Título transliterado con carácteres latinos.
  * Sin artículos iniciales, en los subtítulos tampoco.
  
Clave de ordenación:
  * Es el identificador con los número de partes normalizado a números arábigos.
  * Sustituciones de caracteres en nombres de fichero no permitidos por Windows.
  * NO pueden terminar en punto '.', sustituirlo por '_'; Windows da algunos problemas si las carpetas terminan en punto '.'
  * ': ' → ' - '
  * '&' → ' and '
  
## MAME ##

Si se ejecuta a pantalla completa a veces no termina de salir programa, quedándose colgado sin más y con la pantalla en negro. Se ve el ratón y se puede interactuar con los elementos que debieran estar en pantalla pero es completamente a ciegas.

Como Emuteca está esperando a que termine MAME, no se puede interactuar con el programa.

En Win10 era posible, aunque muy difícil hacerlo, abrir el administrador de tareas y terminar la ejecución de MAME. En Win11, realmente lo veo imposible a no ser que se tengan dos pantallas.

Estoy pensando en ejecutar los emuladores en hilos aparte para poder interactuar con Emuteca cuando se está ejecutando un Emulador.