---
layout: page
title: Configuración sistemas de cartuchos
EmuVer: 0.8
---
## RPCEmu ##

Según parece RPCEmu no soporta cargar imágenes de disto a traves de la línea de comandos.

Sin embargo, en http://acorn.revivalteam.de/?site=ArcGB; que es una GameBase para este sistema usan un ejecutable llamado ´RPCEmuCaller.exe´; que se puede descargar junta a la base de datos http://acorn.revivalteam.de/ArcGB/Wocki's%20Acorn%20GameBase.zip

Pero no lo he conseguido hacer funcionar, según el script para usar con GameBase sería

'''
If GameType CONTAINS(ZIP)
	Run_Program(%dbpath%\RPCEmuCaller.exe||%gbgamepath% %emupath% "0"||WAIT)
ElseIf GameType CONTAINS(ADF)
	Run_Program(%dbpath%\RPCEmuCaller.exe||%gamepath%\%gamefile% %emupath% "0"||WAIT)
End If
'''

## Arculator ##

Arculator es el emulador para juegos

De la misma forma Arculator parece que necesita que se edite un .cfg.

'''
If GameType CONTAINS(ZIP)
	Set_CFG_Value(%emupath%\arc.cfg||disc_name_0||%gbgamepath%)
ElseIf GameType CONTAINS(ADF)
	Set_CFG_Value(%emupath%\arc.cfg||disc_name_0||%gamepath%\%gamefile%)
End If
Run_Emulator()
'''

Que se podría solucionar con un .bat que escriba dicho archivo

'''
::### set paths ###
set DiskName0Path=%1 %2 %3 %4 %5 %6 %7

::### delete arc.cfg ###
del arc.cfg

::### write arc.cfg ###
echo disc_name_0 = %DiskName0Path% > arc.cfg
(
echo no_borders = 0
echo stereo = 1
echo rom_set = 2
echo fdc_type = 0
echo fast_disc = 1
echo hardware_blit = 1
echo double_scan = 1
echo first_fullscreen = 1
echo full_borders = 0
echo hires = 0
echo fpa = 0
echo cpu_type = 0
echo mem_size = 4096
echo sound_enable = 1
echo limit_speed = 1
) >> arc.cfg

::### run emulator ###
start Arculator.exe
'''

## Solución intermedia ##

Hacer que Emuteca descomprima la imagen del disco en un directorio definido por el usuario y cómodo (en el editor del sistema, en *Diretorio de Trabajo*) para abrirlo con el propio emulador.