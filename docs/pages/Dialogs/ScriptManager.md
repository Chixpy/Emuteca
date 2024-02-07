---
layout: page
title: Title
EmuVer: 0.8
---

El Gestor de Scripts es la ventana donde se puede programar mediante Pascal Script pequeños programas para realizar tareas de forma rápida y automática.

La forma más fácil de conocer los diferentes métodos, funciones y procedimientos añadidos es ejecutar los diferentes Scripts de la carpeta Test.

Además de todo lo añadido en Emuteca (y CHXPas) están añadidos todos los ficheros de importación incluidos con el propio Pascal Script. Que tienen todo lo básico de Object Pascal.

## Observaciones

  - Las instancias de TStringList se realizan con `CreateStringList`. Por alguna razón Pascal Script no importa el constructor.
  - `uses uUnidad;` no funciona pese a haber activado que sea soportado. La alternativa por el momento es usar `{I "uArchivo.pas"}`