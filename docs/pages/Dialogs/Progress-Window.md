---
layout: page
title: Ventana de Progreso
EmuVer: 0.8
---

La Ventana de Progreso se muestra automáticamente cuando Emuteca está realizando una tarea que puede tardar un rato.

La verdad es que es simplemente eso, para mostrar como va el progreso de la tarea, pero tan solo comentar algunos detalles:

  - **Algunas acciones son cancelables**. Por ejemplo el añadir juegos nuevos; mientras que otras no lo son (cargar o guardar un sistema) para asegurar la integridad de los datos.
  - **El tiempo estimado, es eso estimado**. Una simple fórmula de cuanto ha progresado en tanto tiempo y calcula cuanto de tardará si se sigue al mismo ritmo.
  - Si no se actualiza, se debe a que algún paso toma bastante tiempo en realizarse. Por ejemplo, obtener la lista de archivos de un directorio al añadir juegos.
  - Haciendo **click con el botón derecho del ratón** sobre ella, se abre un menú con el que se puede cambiar la frecuencia de actualización de la ventana de progreso. A menor tiempo, más dinámica parece y responde antes al abrir el menú o cancelar, pero puede influir más en el tiempo que tarde en realizarse la tarea. Tener abierto el menú se pausa la tarea.

![Ventana de Progreso 0.8.0.120](img/0_8/ProgressBar%200_8_0_120.png)
