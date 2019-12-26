---
layout: page
title: Emuladores
EmuVer: 0.8
---

## Instalando PinMAME ##
Here's how to setup PinMAME and VPinMAME in QuickPlay:

1. Go to http://pinmame.retrogames.com and download the following:

Visual PinMAME binary
PinMAMEW binary
PinMAME Sound Samples
2. Extract PinMAME binary into your desired directory

3. Extract PinMAMEW binary into same directory as PinMAME binary

4. Move downloaded samples into the samples subdirectory of where you extracted PinMAME binary to. (When you extract PinMAME binary there should be a new samples directory. This is where you place the samples [keep them zipped])

5. Go to http://www.vpforums....bles/tables.php to download some required files for VPinMAME. You will need:

Font collection
VBS Scripts
6. Extract the VBS Scripts into the same directory as your PinMAME binary (same directory as VPinMAME.exe and PinMAME.exe)

7. Extract the Fonts Collection file into C:\Windows\Fonts

8. You'll need to add VPinMAME as an emulator to your QuickPlay emulator list.

9. Once VPinMAME is added, go to Emulators --> Emulator Management and go to PinMAME (or whatever you named your new pinball emulator)

10. On the line for Parameters enter: VPinball.exe /Play -"%ROMFILENAME%"

11. Click OK to close the Emulator Setup window.